//! Tab file state

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::files;
use crate::menu::Menu;
use crate::ProjectData;

use compiler::path::SourcePathBuf;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

extern crate fltk;
use fltk::enums::Color;
use fltk::group::Flex;
use fltk::prelude::{GroupExt, WidgetExt};

pub trait Tab {
    fn widget(&self) -> &fltk::group::Flex;
    fn widget_mut(&mut self) -> &mut fltk::group::Flex;

    fn file_type(&self) -> FileType;
}

// Used for the "there are unsaved changes" dialog box
#[derive(Debug, Clone, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum FileType {
    Project,
    SoundEffects,
    Song(ItemId),
}

impl FileType {
    fn can_save_as(&self) -> bool {
        match self {
            Self::Project => false,
            Self::SoundEffects => true,
            Self::Song(_) => true,
        }
    }
}

mod file_state {
    use super::{FileType, Flex, Tab};
    use fltk::prelude::WidgetExt;
    use std::path::{Path, PathBuf};

    pub struct TabFileState {
        file_type: FileType,

        tabs: Vec<(Option<String>, Flex)>,

        // absolute file path
        path: Option<PathBuf>,
        file_name: Option<String>,

        is_unsaved: bool,
    }

    pub fn new_tab_label(ft: &FileType) -> &'static str {
        match ft {
            FileType::Project => "New Project",
            FileType::SoundEffects => "New Sound Effect",
            FileType::Song(_) => "New Song",
        }
    }

    impl TabFileState {
        pub fn new(file_type: FileType) -> Self {
            TabFileState {
                file_type,
                tabs: Vec::new(),
                path: None,
                file_name: None,
                is_unsaved: false,
            }
        }

        pub fn add_tab(&mut self, tab: &dyn Tab, label: Option<String>) {
            let tab_widget = tab.widget().clone();

            match self.tabs.iter_mut().find(|(_, w)| w.is_same(&tab_widget)) {
                Some((tab_label, _widget)) => *tab_label = label,
                None => {
                    self.tabs.push((label, tab_widget));
                    self.update_labels();
                }
            }
        }

        pub fn first_widget(&self) -> Option<&fltk::group::Flex> {
            self.tabs.first().map(|(_, w)| w)
        }

        pub fn set_path(&mut self, path: PathBuf) {
            assert!(path.is_absolute());

            self.file_name = path.file_name().map(|s| s.to_string_lossy().to_string());
            self.path = Some(path);

            self.update_labels();
        }

        pub fn path(&self) -> Option<&Path> {
            self.path.as_deref()
        }

        pub fn file_name(&self) -> Option<&str> {
            self.file_name.as_deref()
        }

        pub fn is_unsaved(&self) -> bool {
            self.is_unsaved
        }

        pub fn mark_unsaved(&mut self) {
            if !self.is_unsaved {
                self.is_unsaved = true;

                self.update_labels();
            }
        }

        pub fn mark_saved(&mut self) {
            if self.is_unsaved {
                self.is_unsaved = false;

                self.update_labels();
            }
        }

        pub fn update_labels(&mut self) {
            for (label, tab_widget) in &mut self.tabs {
                let label = label
                    .as_deref()
                    .or(self.file_name.as_deref())
                    .unwrap_or_else(|| new_tab_label(&self.file_type));

                if self.is_unsaved {
                    let new_label = ["*", label, "  "].concat();
                    tab_widget.set_label(&new_label);
                } else {
                    let new_label = [label, "  "].concat();
                    tab_widget.set_label(&new_label);
                }
            }
        }
    }
}
use file_state::{new_tab_label, TabFileState};

pub enum SaveType {
    Save,
    SaveAs,
}

pub enum SaveResult {
    None,
    Saved,
    Renamed(SourcePathBuf),
}

impl SaveResult {
    pub fn is_saved(&self) -> bool {
        match self {
            Self::None => false,
            Self::Saved => true,
            Self::Renamed(_) => true,
        }
    }
}

pub struct TabManager {
    tabs_widget: fltk::group::Tabs,
    main_menu: Menu,

    tabs_list: Vec<(fltk::group::Flex, FileType)>,
    selected_file: Option<FileType>,
    file_states: HashMap<FileType, TabFileState>,
}

impl TabManager {
    pub fn new(tabs_widget: fltk::group::Tabs, main_menu: Menu) -> Self {
        Self {
            tabs_widget,
            main_menu,
            tabs_list: Vec::new(),
            selected_file: None,
            file_states: HashMap::new(),
        }
    }

    pub fn add_or_modify(&mut self, t: &dyn Tab, path: Option<PathBuf>, label: Option<&str>) {
        let ft = t.file_type();

        let state = self
            .file_states
            .entry(ft.clone())
            .or_insert(TabFileState::new(ft.clone()));

        if let Some(p) = path {
            state.set_path(p);
        }
        state.add_tab(t, label.map(String::from));

        let tab_widget = t.widget();
        let tab_exists = self.tabs_list.iter().any(|t| t.0.is_same(tab_widget));
        if !tab_exists {
            let mut tab_widget = tab_widget.clone();
            self.add_widget(&mut tab_widget, label);

            self.tabs_list.push((tab_widget, t.file_type()));
        }
    }

    pub fn add_widget(&mut self, w: &mut fltk::group::Flex, label: Option<&str>) {
        if self.tabs_widget.find(w) >= self.tabs_widget.children() {
            w.set_margins(3, 5, 3, 3);
            self.tabs_widget.add(w);
            self.tabs_widget.auto_layout();

            if let Some(label) = label {
                let new_label = [label, " "].concat();
                w.set_label(&new_label);
            }
        }
    }

    pub fn remove_tab(&mut self, t: &dyn Tab) {
        let ft = t.file_type();
        let widget = t.widget();

        // Remove widget from tabs_list
        self.tabs_list
            .retain(|(tl_flex, _)| !tl_flex.is_same(widget));

        // Only remove file_state if FileType no-longer exists
        let ft_exists = self.tabs_list.iter().any(|t| t.1 == ft);
        if !ft_exists {
            self.file_states.remove(&ft);
        }
        self.tabs_widget.remove(widget);

        self.selected_tab_changed();

        self.tabs_widget.redraw();
        self.tabs_widget.redraw_label();
    }

    pub fn set_tab_label_color(&mut self, t: &mut dyn Tab, valid: bool) {
        let w = t.widget_mut();
        let c = match valid {
            true => Color::Foreground,
            false => Color::Red,
        };

        if w.label_color() != c {
            w.set_label_color(c);
            if self.tabs_widget.value().is_some_and(|v| v.is_same(w)) {
                self.tabs_widget.set_label_color(c);
            }
            self.tabs_widget.redraw();
        }
    }

    pub fn set_selected_tab(&mut self, t: &dyn Tab) {
        let _ = self.tabs_widget.set_value(t.widget());
        self.selected_tab_changed();
    }

    pub fn selected_tab_changed(&mut self) {
        let tab_widget = self.tabs_widget.value();

        if let Some(tw) = &tab_widget {
            self.tabs_widget.set_label_color(tw.label_color());
        }

        self.selected_file_changed(
            tab_widget
                .and_then(|widget| self.tabs_list.iter().find(|t| t.0.is_same(&widget)))
                .map(|t| t.1.clone()),
        );
    }

    fn selected_file_changed(&mut self, ft: Option<FileType>) {
        self.selected_file = ft;
        self.update_save_menu();
        self.main_menu.tab_changed(&self.selected_file);
    }

    pub fn update_save_menu(&mut self) {
        let state = self
            .selected_file
            .as_ref()
            .and_then(|ft| self.file_states.get(ft));

        let can_save = state.is_some();
        let can_save_as = self.selected_file.as_ref().is_some_and(|s| s.can_save_as());

        self.main_menu.update_save_menus(can_save, can_save_as);
    }

    pub fn selected_widget(&self) -> Option<impl GroupExt> {
        self.tabs_widget.value()
    }

    pub fn selected_file(&self) -> Option<FileType> {
        self.selected_file.clone()
    }

    pub fn get_file_name(&self, ft: &FileType) -> Option<&str> {
        self.file_states.get(ft).and_then(|s| s.file_name())
    }

    pub fn get_file_name_or_new_type<'a>(&'a self, ft: &FileType) -> &'a str {
        self.file_states
            .get(ft)
            .and_then(|s| s.file_name())
            .unwrap_or_else(|| new_tab_label(ft))
    }

    pub fn selected_file_name(&self) -> Option<&str> {
        self.selected_file
            .as_ref()
            .and_then(|ft| self.get_file_name(ft))
    }

    pub fn project_file_name(&self) -> Option<&str> {
        self.get_file_name(&FileType::Project)
    }

    pub fn find_file(&self, p: &Path) -> Option<FileType> {
        let p = Some(p);
        self.file_states
            .iter()
            .find(|i| i.1.path() == p)
            .map(|i| i.0.clone())
    }

    pub fn is_unsaved(&self, ft: &FileType) -> bool {
        self.file_states
            .get(ft)
            .map(|s| s.is_unsaved())
            .unwrap_or(false)
    }

    pub fn unsaved_tabs(&self) -> Vec<FileType> {
        let mut v: Vec<FileType> = self
            .file_states
            .iter()
            .filter(|(_ft, state)| state.is_unsaved())
            .map(|(ft, _state)| ft.clone())
            .collect();

        v.sort();
        v.dedup();
        v
    }

    pub fn mark_unsaved(&mut self, ft: FileType) {
        if let Some(state) = self.file_states.get_mut(&ft) {
            state.mark_unsaved();
            self.tabs_widget.auto_layout();
        }
    }

    pub fn save_tab(
        &mut self,
        ft: FileType,
        save_type: SaveType,
        data: &impl files::Serializer,
        pd: &ProjectData,
    ) -> SaveResult {
        let state = match self.file_states.get_mut(&ft) {
            Some(s) => s,
            None => return SaveResult::None,
        };

        match (save_type, state.path()) {
            (SaveType::Save, Some(path)) => {
                if files::save_data(data, path) {
                    state.mark_saved();
                    self.tabs_widget.auto_layout();
                    // Must redraw tab_widget as the saved tab is now smaller
                    self.tabs_widget.redraw();

                    self.update_save_menu();

                    SaveResult::Saved
                } else {
                    SaveResult::None
                }
            }
            (SaveType::SaveAs, _) | (SaveType::Save, None) => {
                // Show tab so the user know what is being saved in SaveAllAndQuit
                if let Some(w) = state.first_widget() {
                    let _ = self.tabs_widget.set_value(w);
                }

                match files::save_data_with_save_as_dialog(data, state.path(), pd) {
                    Some(p) => {
                        state.set_path(p.full_path);
                        state.mark_saved();
                        self.tabs_widget.auto_layout();
                        // Must redraw tab_widget as the saved tab is now smaller
                        self.tabs_widget.redraw();

                        self.update_save_menu();

                        SaveResult::Renamed(p.source_path)
                    }
                    None => SaveResult::None,
                }
            }
        }
    }
}
