//! Menu Bar

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::{AudioMessage, DEFAULT_AUDIO_MODE};
use crate::tabs::FileType;
use crate::GuiMessage;

use std::sync::mpsc;

extern crate fltk;
use compiler::driver_constants::AudioMode;
use fltk::enums::{Key, Shortcut};
use fltk::menu::{self, MenuFlag};
use fltk::prelude::MenuExt;

// I cannot store `menu::MenuItem` entries in `Menu`, whenever I open a file dialog all future
// updates to the file menu stop working.
//
// Looking at the examples in the `fltk-rs` repository they use `find_item()` to edit menus,
// without saving the `menu::MenuItem`, so that is what I'll do.
//
// Unfortunately, changing a menu item's label changes its path, so I cannot include the filename
// in the Save menu item (ie "Save sound_effects.txt").

const NEW_MML_FILE: &str = "&File/&New MML File";
const OPEN_MML_FILE: &str = "&File/&Open MML File";
const SAVE: &str = "&File/&Save";
const SAVE_AS: &str = "&File/Save As";
const SAVE_ALL: &str = "&File/Save &All";

const CLEAR_CACHE_AND_REBUILD: &str = "&File/&Clear sample cache and rebuild";
const EXPORT_SPC: &str = "&File/&Export song to .spc";

const AUDIO_SFX_WINDOW: &str = "&Audio/Sound &Effects";
const DRIVER_STATE_WINDOW: &str = "&Audio/&Driver State";
const AUDIO_STOP: &str = "&Audio/&Stop Audio";

const AUDIO_MONO: &str = "&Audio/&Mono";
const AUDIO_STEREO: &str = "Audio/&S&tereo";
const AUDIO_SURROUND: &str = "&Audio/S&urround";

const SHOW_HELP_SYNTAX: &str = "&Help/&Syntax";
const SHOW_LICENSES: &str = "&Help/&Licencing Information";
const SHOW_ABOUT_TAB: &str = "&Help/&About";

const QUIT: &str = "&File/&Quit";

#[derive(Clone)]
pub struct Menu {
    menu_bar: fltk::menu::MenuBar,
}

impl Menu {
    pub fn new(
        sender: fltk::app::Sender<GuiMessage>,
        audio_sender: mpsc::Sender<AudioMessage>,
    ) -> Self {
        let mut menu_bar = fltk::menu::MenuBar::default();
        let mut menu_bar2 = menu_bar.clone();

        let mut add = |label, shortcut, flags, f: fn() -> GuiMessage| -> menu::MenuItem {
            let index = menu_bar.add(label, shortcut, flags, {
                let s = sender.clone();
                move |_: &mut fltk::menu::MenuBar| s.send(f())
            });

            menu_bar.at(index).unwrap()
        };

        let mut add_audio = |label, shortcut, flags, f: fn() -> AudioMessage| {
            menu_bar2.add(label, shortcut, flags, {
                let s = audio_sender.clone();
                move |_: &mut fltk::menu::MenuBar| {
                    s.send(f()).ok();
                }
            });
        };

        add(
            NEW_MML_FILE,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::NewMmlFile,
        );

        add(
            OPEN_MML_FILE,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::OpenMmlFile,
        );

        add(
            SAVE,
            Shortcut::Ctrl | 's',
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::SaveSelectedTab,
        );
        add(
            SAVE_AS,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::SaveSelectedTabAs,
        );
        add(
            SAVE_ALL,
            Shortcut::Ctrl | Shortcut::Shift | 's',
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::SaveAllUnsaved,
        );
        add(
            CLEAR_CACHE_AND_REBUILD,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ClearSampleCacheAndRebuild,
        );
        add(
            EXPORT_SPC,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ExportCurrentTabToSpcFile,
        );
        add(QUIT, Shortcut::None, fltk::menu::MenuFlag::Normal, || {
            GuiMessage::QuitRequested
        });

        add(
            AUDIO_SFX_WINDOW,
            Shortcut::from_key(Key::F10),
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ToggleSfxWindow,
        );

        add(
            DRIVER_STATE_WINDOW,
            Shortcut::from_key(Key::F11),
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ToggleDriverStateWindow,
        );

        add_audio(
            AUDIO_STOP,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || AudioMessage::StopAndClose,
        );
        add_audio(
            AUDIO_MONO,
            Shortcut::None,
            fltk::menu::MenuFlag::Radio,
            || AudioMessage::SetAudioMode(AudioMode::Mono),
        );
        add_audio(
            AUDIO_STEREO,
            Shortcut::None,
            fltk::menu::MenuFlag::Radio,
            || AudioMessage::SetAudioMode(AudioMode::Stereo),
        );

        const _: () = assert!(matches!(DEFAULT_AUDIO_MODE, AudioMode::Surround));
        add_audio(
            AUDIO_SURROUND,
            Shortcut::None,
            fltk::menu::MenuFlag::Radio | MenuFlag::Value,
            || AudioMessage::SetAudioMode(AudioMode::Surround),
        );

        add(
            SHOW_HELP_SYNTAX,
            Shortcut::from_key(Key::F1),
            fltk::menu::MenuFlag::Toggle,
            || GuiMessage::ShowOrHideHelpSyntax,
        );
        add(
            SHOW_LICENSES,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ShowLicensesDialog,
        );
        add(
            SHOW_ABOUT_TAB,
            Shortcut::None,
            fltk::menu::MenuFlag::Normal,
            || GuiMessage::ShowAboutTab,
        );

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
            m.deactivate();
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

        self.deactivate(CLEAR_CACHE_AND_REBUILD);
        self.deactivate(EXPORT_SPC);

        self.deactivate(AUDIO_SFX_WINDOW);
    }

    pub fn is_help_syntax_checked(&self) -> bool {
        self.menu_bar
            .find_item(SHOW_HELP_SYNTAX)
            .map(|m| m.value())
            .unwrap_or(false)
    }

    pub fn project_loaded(&mut self) {
        self.activate(NEW_MML_FILE);
        self.activate(OPEN_MML_FILE);

        self.activate(SAVE_ALL);
        self.activate(CLEAR_CACHE_AND_REBUILD);

        self.activate(AUDIO_SFX_WINDOW);
    }

    pub fn update_save_menus(&mut self, can_save: bool, can_save_as: bool) {
        // I cannot update the save MenuItem label as that also changes the MenuItem's path

        self.set_active(SAVE, can_save);
        self.set_active(SAVE_AS, can_save && can_save_as);
    }

    pub fn tab_changed(&mut self, tab: &Option<FileType>) {
        let is_song = matches!(&tab, Some(FileType::Song(_)));

        self.set_active(EXPORT_SPC, is_song);
    }
}
