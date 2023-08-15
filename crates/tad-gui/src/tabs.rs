//! Tab file state

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::files;
use crate::{Menu, Message, ProjectData};

use std::collections::HashMap;
use std::path::PathBuf;

extern crate fltk;
use fltk::dialog;
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
    use std::path::{Path, PathBuf};

    use super::{Flex, Tab};
    use fltk::prelude::WidgetExt;

    pub struct TabFileState {
        tabs: Vec<(String, Flex)>,

        path: Option<PathBuf>,
        file_name: Option<String>,

        is_unsaved: bool,
    }

    impl TabFileState {
        pub fn new() -> Self {
            TabFileState {
                tabs: Vec::new(),
                path: None,
                file_name: None,
                is_unsaved: false,
            }
        }

        pub fn add_tab(&mut self, tab: &dyn Tab) {
            let tab_widget = tab.widget().clone();
            let label = tab_widget.label().trim_start_matches('*').to_string();

            match self.tabs.iter_mut().find(|(_, w)| w.is_same(&tab_widget)) {
                Some((tab_label, _widget)) => *tab_label = label,
                None => self.tabs.push((label, tab_widget)),
            }
        }

        pub fn first_widget(&self) -> Option<&fltk::group::Flex> {
            self.tabs.first().map(|(_, w)| w)
        }

        pub fn set_path(&mut self, path: PathBuf) {
            assert!(path.is_absolute());

            self.file_name = path.file_name().map(|s| s.to_string_lossy().to_string());
            self.path = Some(path);
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

                for (label, tab_widget) in &mut self.tabs {
                    let new_label = ["*", label].concat();
                    tab_widget.set_label(&new_label);

                    // Must redraw the Tabs widget otherwise the new label is not visible
                    if let Some(mut w) = tab_widget.parent() {
                        w.redraw();
                    }
                }
            }
        }

        pub fn mark_saved(&mut self) {
            if self.is_unsaved {
                self.is_unsaved = false;

                for (label, tab_widget) in &mut self.tabs {
                    tab_widget.set_label(label);

                    // Must redraw the Tabs widget otherwise the new label is not visible
                    if let Some(mut w) = tab_widget.parent() {
                        w.redraw_label();
                    }
                }
            }
        }
    }
}
use file_state::TabFileState;

pub enum SaveType {
    Save,
    SaveAs,
}

pub enum SaveResult {
    None,
    Saved,
    Renamed { pf_path: PathBuf },
}

impl SaveResult {
    pub fn is_saved(&self) -> bool {
        match self {
            Self::None => false,
            Self::Saved => true,
            Self::Renamed { .. } => true,
        }
    }
}

pub struct TabManager {
    tabs_widget: fltk::group::Tabs,
    tabs_list: Vec<(fltk::group::Flex, FileType)>,
    selected_file: Option<FileType>,
    file_states: HashMap<FileType, TabFileState>,
}

impl TabManager {
    pub fn new(tabs_widget: fltk::group::Tabs) -> Self {
        Self {
            tabs_widget,
            tabs_list: Vec::new(),
            selected_file: None,
            file_states: HashMap::new(),
        }
    }

    pub fn add_or_modify(&mut self, t: &dyn Tab, path: Option<PathBuf>) {
        let ft = t.file_type();

        let state = self.file_states.entry(ft).or_insert(TabFileState::new());

        if let Some(p) = path {
            state.set_path(p);
        }
        state.add_tab(t);

        let tab_widget = t.widget();
        let tab_exists = self.tabs_list.iter().any(|t| t.0.is_same(tab_widget));
        if !tab_exists {
            let mut tab_widget = tab_widget.clone();
            self.add_widget(&mut tab_widget);
            self.tabs_list.push((tab_widget, t.file_type()));
        }
    }

    pub fn add_widget(&mut self, w: &mut fltk::group::Flex) {
        if self.tabs_widget.find(w) >= self.tabs_widget.children() {
            w.set_margins(3, 5, 3, 3);
            self.tabs_widget.add(w);
            self.tabs_widget.auto_layout();
        }
    }

    pub fn selected_tab_changed(&mut self, menu: &mut Menu) {
        let tab_widget = self.tabs_widget.value();

        self.selected_file = tab_widget
            .and_then(|widget| self.tabs_list.iter().find(|t| t.0.is_same(&widget)))
            .map(|t| t.1.clone());

        let state = self
            .selected_file
            .as_ref()
            .and_then(|ft| self.file_states.get(ft));

        match state.and_then(TabFileState::file_name) {
            Some(file_name) => {
                menu.save.set_label(&format!("&Save {}", file_name));
                menu.save.activate();
            }
            None => {
                menu.save.set_label("&Save");
                menu.save.deactivate();
            }
        }

        if self.selected_file.as_ref().is_some_and(|s| s.can_save_as()) {
            menu.save_as.activate();
        } else {
            menu.save_as.deactivate();
        }
    }

    pub fn selected_file(&self) -> Option<FileType> {
        self.selected_file.clone()
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
                        state.set_path(p.path);
                        state.mark_saved();
                        SaveResult::Renamed { pf_path: p.pf_path }
                    }
                    None => SaveResult::None,
                }
            }
        }
    }
}

pub fn quit_with_unsaved_files_dialog(unsaved: Vec<FileType>, sender: fltk::app::Sender<Message>) {
    dialog::message_title("Unsaved changes");
    let choice = dialog::choice2_default(
        &format!(
            "Save changes to {} unsaved files before closing?",
            unsaved.len()
        ),
        "Cancel",
        "Save",
        "Quit without saving",
    );
    match choice {
        Some(1) => sender.send(Message::SaveAllAndQuit(unsaved)),
        Some(2) => sender.send(Message::ForceQuit),
        _ => (),
    }
}
