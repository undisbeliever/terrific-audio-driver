//! Tab file state

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::Menu;
use crate::Message;

use std::collections::HashMap;

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

mod file_state {
    use super::{Flex, Tab};
    use fltk::prelude::WidgetExt;

    pub(crate) struct TabFileState {
        tabs: Vec<(String, Flex)>,

        file_name: Option<String>,

        is_unsaved: bool,
    }

    impl TabFileState {
        pub fn new() -> Self {
            TabFileState {
                tabs: Vec::new(),
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

        // Returns `None` if the file has not been saved.
        pub fn file_name(&self) -> Option<&str> {
            self.file_name.as_deref()
        }

        pub fn set_file_name(&mut self, file_name: Option<String>) {
            self.file_name = file_name;
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

    pub fn add_or_modify(&mut self, t: &dyn Tab, file_name: Option<String>) {
        let ft = t.file_type();

        let state = self.file_states.entry(ft).or_insert(TabFileState::new());

        state.set_file_name(file_name);
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
    }

    pub fn selected_file(&self) -> Option<FileType> {
        self.selected_file.clone()
    }

    pub fn is_unsaved(&self, ft: &FileType) -> bool {
        self.file_states.get(ft).is_some_and(|s| s.is_unsaved())
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

    pub fn mark_saved(&mut self, ft: FileType) {
        if let Some(state) = self.file_states.get_mut(&ft) {
            state.mark_saved();
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
        "Quit without saving",
        "Save",
        "Cancel",
    );
    match choice {
        Some(0) => sender.send(Message::ForceQuit),
        Some(1) => sender.send(Message::SaveAllAndQuit(unsaved)),
        _ => (),
    }
}
