//! Menu Bar

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::Message;

extern crate fltk;
use fltk::enums::Shortcut;
use fltk::menu;
use fltk::prelude::{MenuExt, WidgetExt};

// I cannot store `menu::MenuItem` entries in `Menu`, whenever I open a file dialog all future
// updates to the file menu stop working.
//
// Looking at the examples in the `fltk-rs` repository they use `find_item()` to edit menus,
// without saving the `menu::MenuItem`, so that is what I'll do.
//
// Unfortunately, changing a menu item's label changes its path, so I cannot include the filename
// in the Save menu item (ie "Save sound_effects.txt").

const NEW_MML_FILE: &str = "&File/New MML File";
const OPEN_MML_FILE: &str = "&File/Open MML File";
const SAVE: &str = "&File/&Save";
const SAVE_AS: &str = "&File/Save As";
const SAVE_ALL: &str = "&File/Save &All";

const QUIT: &str = "&File/&Quit";

#[derive(Clone)]
pub struct Menu {
    menu_bar: fltk::menu::MenuBar,
}

impl Menu {
    pub fn new(sender: fltk::app::Sender<Message>) -> Self {
        let mut menu_bar = fltk::menu::MenuBar::default();
        menu_bar.set_frame(fltk::enums::FrameType::FlatBox);

        let mut add = |label, shortcut, flags, f: fn() -> Message| -> menu::MenuItem {
            let index = menu_bar.add(label, shortcut, flags, {
                let s = sender.clone();
                move |_: &mut fltk::menu::MenuBar| s.send(f())
            });

            menu_bar.at(index).unwrap()
        };

        add(
            NEW_MML_FILE,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::NewMmlFile,
        );

        add(
            OPEN_MML_FILE,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::OpenMmlFile,
        );

        add(
            SAVE,
            Shortcut::Ctrl | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveSelectedTab,
        );
        add(
            SAVE_AS,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::SaveSelectedTabAs,
        );
        add(
            SAVE_ALL,
            Shortcut::Ctrl | Shortcut::Shift | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveAllUnsaved,
        );
        add(QUIT, Shortcut::None, fltk::menu::MenuFlag::Normal, || {
            Message::QuitRequested
        });

        Menu { menu_bar }
    }

    pub fn menu_bar(&self) -> &menu::MenuBar {
        &self.menu_bar
    }

    fn activate(&mut self, path: &str) {
        if let Some(mut m) = self.menu_bar.find_item(path) {
            m.activate();
        }
    }

    fn deactivate(&mut self, path: &str) {
        if let Some(mut m) = self.menu_bar.find_item(path) {
            m.activate();
        }
    }

    fn set_active(&mut self, path: &str, active: bool) {
        if let Some(mut m) = self.menu_bar.find_item(path) {
            if active {
                m.activate();
            } else {
                m.deactivate();
            }
        }
    }

    pub fn deactivate_project_items(&mut self) {
        self.deactivate(NEW_MML_FILE);
        self.deactivate(OPEN_MML_FILE);

        self.deactivate(SAVE);
        self.deactivate(SAVE_AS);
        self.deactivate(SAVE_ALL);
    }

    pub fn project_loaded(&mut self) {
        self.activate(NEW_MML_FILE);
        self.activate(OPEN_MML_FILE);

        self.activate(SAVE_ALL);
    }

    pub fn update_save_menus(&mut self, can_save: bool, can_save_as: bool) {
        // I cannot update the save MenuItem label as that also changes the MenuItem's path

        self.set_active(SAVE, can_save);
        self.set_active(SAVE_AS, can_save && can_save_as);
    }
}
