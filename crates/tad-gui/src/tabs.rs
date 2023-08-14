//! Tab file state

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

extern crate fltk;
use fltk::{group::Flex, prelude::WidgetExt};

pub trait Tab {
    fn widget(&mut self) -> &mut fltk::group::Flex;

    fn file_state(&self) -> &TabFileState;
    fn file_state_mut(&mut self) -> &mut TabFileState;
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
