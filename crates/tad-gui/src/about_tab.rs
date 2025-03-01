//! About Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::{ch_units_to_width, input_height, label_center, label_top_center};
use crate::GuiMessage;

extern crate fltk;
use fltk::app;
use fltk::button::Button;
use fltk::enums::{CallbackReason, CallbackTrigger, Font};
use fltk::frame::Frame;
use fltk::group::{Flex, Tabs};
use fltk::prelude::{GroupExt, WidgetExt};

const VERSION_STR: &str = concat!["Version ", env!("CARGO_PKG_VERSION")];

const COPYRIGHT_TEXT: &str = "Copyright © 2023 Marcus Rowe";

pub struct AboutTab {
    parent: Tabs,

    group: Flex,

    no_project_group: Flex,
}

fn no_project_button_group(sender: app::Sender<GuiMessage>) -> Flex {
    let mut flex = Flex::default().row();

    let button_width = ch_units_to_width(&flex, 15);

    Frame::default();

    let mut button = |label: &str, f: fn() -> GuiMessage| {
        let mut b = Button::default().with_label(label);
        b.set_callback({
            let s = sender.clone();
            move |_w| s.send(f())
        });
        flex.fixed(&b, button_width);
        b
    };

    button("New Project", || GuiMessage::NewProject);
    button("Open Project", || GuiMessage::OpenProject);

    Frame::default();

    flex.end();

    flex
}

impl AboutTab {
    pub fn new(parent: Tabs, sender: app::Sender<GuiMessage>) -> Self {
        let mut group = Flex::default().column();
        group.set_label("About ");

        // ::TODO show a logo::
        let mut title = label_center("terrific audio driver");
        title.set_label_font(Font::CourierBold);
        title.set_label_size(group.label_size() * 4);
        group.fixed(&title, input_height(&title));

        let version = label_center(VERSION_STR);
        group.fixed(&version, input_height(&version));

        let copyright = label_top_center(COPYRIGHT_TEXT);
        group.fixed(&copyright, input_height(&copyright));

        let no_project_group = no_project_button_group(sender);
        group.fixed(&no_project_group, input_height(&no_project_group));

        let padding = Frame::default();
        group.fixed(&padding, 12);

        group.end();

        // Remove about_tab from parent if the user closed the tab.
        group.set_callback({
            let mut tabs = parent.clone();
            move |w| {
                if app::callback_reason() == CallbackReason::Closed {
                    tabs.remove(w);
                    // Update selected tab and main after the about tab is closed
                    tabs.do_callback();
                }
            }
        });

        Self {
            parent,
            group,
            no_project_group,
        }
    }

    pub fn project_loaded(&mut self) {
        self.no_project_group.hide();
        self.group.layout();

        // Allow the user to close the about tab
        self.group.set_trigger(CallbackTrigger::Closed);

        self.parent.remove(&self.group);
    }

    pub fn show(&mut self) {
        if self.group.parent().is_none() {
            self.parent.add(&self.group);
        }
        let _ = self.parent.set_value(&self.group);

        // Fixes a blank window when opening the about tab
        self.parent.auto_layout();
    }
}
