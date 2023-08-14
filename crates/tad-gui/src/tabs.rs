//! Tab file state

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::Message;

extern crate fltk;
use fltk::{dialog, group::Flex, prelude::WidgetExt};

pub trait Tab {
    fn widget(&self) -> &fltk::group::Flex;
    fn widget_mut(&mut self) -> &mut fltk::group::Flex;

    fn file_type(&self) -> FileType;

    fn file_state(&self) -> &TabFileState;
    fn file_state_mut(&mut self) -> &mut TabFileState;
}

// Used for the "there are unsaved changes" dialog box
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum FileType {
    Project,
    SoundEffects,
    Song(ItemId),
}

pub struct TabFileState {
    tab_widget: Flex,

    label: String,
    is_unsaved: bool,
}

#[allow(dead_code)]
impl TabFileState {
    pub fn new(tab_widget: Flex) -> Self {
        TabFileState {
            label: tab_widget.label().trim_start_matches('*').to_string(),
            is_unsaved: false,
            tab_widget,
        }
    }

    pub fn is_unsaved(&self) -> bool {
        self.is_unsaved
    }

    pub fn mark_unsaved(&mut self) {
        if !self.is_unsaved {
            self.is_unsaved = true;

            let new_label = ["*", &self.label].concat();
            self.tab_widget.set_label(&new_label);

            // Must redraw the Tabs widget otherwise the new label is not visible
            if let Some(mut w) = self.tab_widget.parent() {
                w.redraw();
            }
        }
    }

    pub fn mark_saved(&mut self) {
        if self.is_unsaved {
            self.is_unsaved = false;

            self.tab_widget.set_label(&self.label);

            // Must redraw the Tabs widget otherwise the new label is not visible
            if let Some(mut w) = self.tab_widget.parent() {
                w.redraw_label();
            }
        }
    }
}

pub fn unsaved_tabs<'a>(tabs: impl Iterator<Item = &'a dyn Tab>) -> Vec<FileType> {
    let mut v: Vec<FileType> = tabs
        .filter(|t| t.file_state().is_unsaved())
        .map(|t| t.file_type())
        .collect();

    v.sort();
    v.dedup();
    v
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
        Some(1) => (), // ::TODO send SaveMultiple(unsaved) message::
        _ => (),
    }
}
