//! Menu Bar

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::Message;

extern crate fltk;
use fltk::enums::Shortcut;
use fltk::menu;
use fltk::prelude::{MenuExt, WidgetExt};

pub struct Menu {
    menu_bar: fltk::menu::MenuBar,

    new_mml_file: menu::MenuItem,
    open_mml_file: menu::MenuItem,

    save: menu::MenuItem,
    save_as: menu::MenuItem,
    save_all: menu::MenuItem,
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

        let new_mml_file = add(
            "&File/New MML File",
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::NewMmlFile,
        );

        let open_mml_file = add(
            "&File/Open MML File",
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::OpenMmlFile,
        );

        let save = add(
            "&File/&Save",
            Shortcut::Ctrl | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveSelectedTab,
        );
        let save_as = add(
            "&File/Save As",
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::SaveSelectedTabAs,
        );
        let save_all = add(
            "&File/Save &All",
            Shortcut::Ctrl | Shortcut::Shift | 's',
            fltk::menu::MenuFlag::Normal,
            || Message::SaveAllUnsaved,
        );
        add(
            "&File/&Quit",
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || Message::QuitRequested,
        );

        Menu {
            menu_bar,
            new_mml_file,
            open_mml_file,
            save,
            save_as,
            save_all,
        }
    }

    pub fn menu_bar(&self) -> &menu::MenuBar {
        &self.menu_bar
    }

    pub fn deactivate(&mut self) {
        self.new_mml_file.deactivate();
        self.open_mml_file.deactivate();

        self.save.deactivate();
        self.save_as.deactivate();
        self.save_all.deactivate();
    }

    pub fn project_loaded(&mut self) {
        self.new_mml_file.activate();
        self.open_mml_file.activate();

        self.save_all.activate();
    }

    pub fn update_save_items(&mut self, save_file_name: Option<&str>, can_save_as: bool) {
        match save_file_name {
            Some(file_name) => {
                self.save.set_label(&format!("&Save {}", file_name));
                self.save.activate();
            }
            None => {
                self.save.set_label("&Save");
                self.save.deactivate();
            }
        }

        if can_save_as {
            self.save_as.activate();
        } else {
            self.save_as.deactivate();
        }
    }
}
