//! Custom dialogs

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    compiler_thread::ItemId,
    helpers::{ch_units_to_width, input_height},
    tabs::{FileType, TabManager},
    GuiMessage,
};

use std::{cell::RefCell, rc::Rc};

use fltk::{
    button::{Button, ReturnButton},
    enums::Align,
    frame::Frame,
    prelude::{DisplayExt, GroupExt, WidgetBase, WidgetExt, WindowExt},
    text::{TextBuffer, TextDisplay, WrapMode},
    window::Window,
};

struct UnsavedTabDialog {
    window: Window,
    message: Frame,
    save: ReturnButton,

    song_id: Option<ItemId>,
    sender: fltk::app::Sender<GuiMessage>,
}

impl UnsavedTabDialog {
    fn new(sender: fltk::app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut window = Window::default().with_label("Unsaved changes");

        let h = input_height(&window);
        let ch = ch_units_to_width(&window, 1);
        let pad = ch;

        let win_width = ch * 65;

        window.set_size(win_width, 2 * h + 3 * pad);

        let message = Frame::new(ch, pad, 8 * ch, h, None).with_align(Align::Inside | Align::Left);

        let x = win_width - (45 * ch + 3 * pad);
        let y = h + pad * 2;
        let mut force_close = Button::new(x, y, 25 * ch, h, "Close tab without saving");
        let mut save = ReturnButton::new(0, 0, 10 * ch, h, "Save").right_of(&force_close, pad);
        let mut cancel = Button::new(0, 0, 10 * ch, h, "Cancel").right_of(&save, pad);

        window.end();

        window.make_modal(true);

        let out = Rc::new(RefCell::new(Self {
            window,
            message,
            save: save.clone(),
            song_id: None,
            sender,
        }));

        // Set song_id to None when closed
        out.borrow_mut().window.set_callback({
            let s = out.clone();
            move |_| {
                let mut s = s.borrow_mut();
                s.song_id = None;
                s.window.hide();
            }
        });

        cancel.set_callback({
            let s = out.clone();
            move |_| {
                let mut s = s.borrow_mut();
                s.song_id = None;
                s.window.hide();
            }
        });

        save.set_callback({
            let s = out.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    if let Some(song_id) = s.song_id {
                        s.song_id = None;
                        s.window.hide();
                        s.sender.send(GuiMessage::SaveAndCloseSongTab(song_id));
                    }
                }
            }
        });

        force_close.set_callback({
            let s = out.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    if let Some(song_id) = s.song_id {
                        s.song_id = None;
                        s.window.hide();
                        s.sender.send(GuiMessage::ForceCloseSongTab(song_id));
                    }
                }
            }
        });

        out
    }

    fn show(&mut self, song_id: ItemId, file_name: Option<&str>) {
        match file_name {
            Some(f) => self
                .message
                .set_label(&format!("Save changes to {} before closing?", f)),
            None => self.message.set_label("Save changes before closing?"),
        };
        let _ = self.save.take_focus();
        self.song_id = Some(song_id);
        self.window.show();
    }
}

struct QuitWithUnsavedFilesDialog {
    window: Window,
    message: Frame,
    file_buffer: TextBuffer,
    file_display: TextDisplay,
    save_all: ReturnButton,

    unsaved: Option<Vec<FileType>>,
    sender: fltk::app::Sender<GuiMessage>,
}

impl QuitWithUnsavedFilesDialog {
    fn new(sender: fltk::app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut window = Window::default().with_label("Unsaved changes");

        let h = input_height(&window);
        let ch = ch_units_to_width(&window, 1);
        let pad = ch;

        let win_width = ch * 65;

        window.set_size(win_width, 5 * h + 4 * pad);

        let message = Frame::new(ch, pad, 8 * ch, h, None).with_align(Align::Inside | Align::Left);

        let file_buffer = TextBuffer::default();
        let mut file_display = TextDisplay::new(ch, pad + h, win_width - 2 * pad, 3 * h, None);
        file_display.wrap_mode(WrapMode::AtBounds, 0);
        file_display.set_buffer(file_buffer.clone());

        let x = win_width - (45 * ch + 3 * pad);
        let y = 4 * h + pad * 3;
        let mut force_quit = Button::new(x, y, 21 * ch, h, "Quit without saving");
        let mut save_all =
            ReturnButton::new(0, 0, 13 * ch, h, "Save all").right_of(&force_quit, pad);
        let mut cancel = Button::new(0, 0, 10 * ch, h, "Cancel").right_of(&save_all, pad);

        window.end();

        window.make_modal(true);

        let out = Rc::new(RefCell::new(Self {
            window,
            message,
            file_buffer,
            file_display,
            save_all: save_all.clone(),
            unsaved: None,
            sender,
        }));

        // Set song_id to None when closed
        out.borrow_mut().window.set_callback({
            let s = out.clone();
            move |_| {
                let mut s = s.borrow_mut();
                s.unsaved = None;
                s.window.hide();
            }
        });

        cancel.set_callback({
            let s = out.clone();
            move |_| {
                let mut s = s.borrow_mut();
                s.unsaved = None;
                s.window.hide();
            }
        });

        save_all.set_callback({
            let s = out.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    let unsaved = std::mem::take(&mut s.unsaved);

                    if let Some(unsaved) = unsaved {
                        s.window.hide();
                        s.sender.send(GuiMessage::SaveAllAndQuit(unsaved));
                    }
                }
            }
        });

        force_quit.set_callback({
            let s = out.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    if s.unsaved.is_some() {
                        s.unsaved = None;
                        s.window.hide();
                        s.sender.send(GuiMessage::ForceQuit);
                    }
                }
            }
        });

        out
    }

    fn show(&mut self, unsaved: Vec<FileType>, tab_manager: &TabManager) {
        self.unsaved = None;

        match unsaved.len() {
            0 => return,
            1 => self
                .message
                .set_label("Save changes to 1 unsaved file before closing?"),
            2.. => self.message.set_label(&format!(
                "Save changes to {} unsaved files before closing?",
                unsaved.len()
            )),
        }

        let file_list = unsaved
            .iter()
            .map(|ft| tab_manager.get_file_name_or_new_type(ft))
            .collect::<Vec<&str>>()
            .join("\n");
        self.file_buffer.set_text(&file_list);
        self.file_display.scroll(0, 0);

        self.unsaved = Some(unsaved);

        let _ = self.save_all.take_focus();
        self.window.show();
    }
}

// Unsaved dialogs are long-lived as fltk-rs cannot decrement a widget reference counter in
// rust objects passed to cfltk.
struct DialogsInner {
    unsaved_tabs: Rc<RefCell<UnsavedTabDialog>>,
    quit_with_unsaved_files: Rc<RefCell<QuitWithUnsavedFilesDialog>>,
}

#[derive(Clone)]
pub struct Dialogs(Rc<RefCell<DialogsInner>>);

impl Dialogs {
    pub fn new(sender: fltk::app::Sender<GuiMessage>) -> Self {
        Self(Rc::new(RefCell::new(DialogsInner {
            unsaved_tabs: UnsavedTabDialog::new(sender),
            quit_with_unsaved_files: QuitWithUnsavedFilesDialog::new(sender),
        })))
    }

    pub fn close_unsaved_tab(&mut self, song_id: ItemId, file_name: Option<&str>) {
        self.0
            .borrow_mut()
            .unsaved_tabs
            .borrow_mut()
            .show(song_id, file_name);
    }

    pub fn quit_with_unsaved_changes(&mut self, unsaved: Vec<FileType>, tab_manager: &TabManager) {
        self.0
            .borrow_mut()
            .quit_with_unsaved_files
            .borrow_mut()
            .show(unsaved, tab_manager);
    }
}
