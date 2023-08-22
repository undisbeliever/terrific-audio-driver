//! Song Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, SongError, SongOutput};
use crate::helpers::*;
use crate::mml_editor::MmlEditor;
use crate::tabs::{FileType, Tab};
use crate::Message;

use compiler::data::TextFile;
use compiler::errors::MmlCompileErrors;

use fltk::app;
use fltk::button::Button;
use fltk::enums::{Color, Font};
use fltk::group::{Flex, Pack, PackType};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

use std::cell::RefCell;
use std::rc::Rc;

pub fn blank_mml_file() -> TextFile {
    TextFile {
        path: None,
        file_name: "new song.mml".to_owned(),
        contents: String::new(),
    }
}

pub struct State {
    sender: app::Sender<Message>,

    song_id: ItemId,

    editor: MmlEditor,

    console: TextDisplay,
    console_buffer: TextBuffer,

    errors: Option<MmlCompileErrors>,
}

pub struct SongTab {
    state: Rc<RefCell<State>>,

    song_id: ItemId,

    group: Flex,
}

impl Tab for SongTab {
    fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_mut(&mut self) -> &mut Flex {
        &mut self.group
    }

    fn file_type(&self) -> FileType {
        FileType::Song(self.song_id.clone())
    }
}

impl SongTab {
    pub fn new(song_id: ItemId, mml_file: &TextFile, sender: app::Sender<Message>) -> Self {
        let mut group = Flex::default_fill().column();

        let button_size = ch_units_to_width(&group, 4);

        let main_toolbar = Pack::default().with_type(PackType::Horizontal);
        group.fixed(&main_toolbar, button_size);

        let button = |label: &str, tooltip: &str| {
            let mut b = Button::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(tooltip);
            b
        };

        let mut compile_button = button("C", "compile song");

        main_toolbar.end();

        let mut editor = MmlEditor::new();
        editor.set_text_size(editor.widget().text_size() * 12 / 10);
        editor.set_text(&mml_file.contents);

        let mut console = TextDisplay::default();
        group.fixed(&console, input_height(&console) * 4);

        group.end();

        let console_buffer = TextBuffer::default();
        console.set_text_font(Font::Courier);
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        let state = Rc::new(RefCell::from(State {
            sender,
            song_id: song_id.clone(),
            editor,

            console,
            console_buffer,

            errors: None,
        }));

        compile_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.commit_song_if_changed()
                }
            }
        });

        state.borrow_mut().editor.set_unfocus_callback({
            let s = state.clone();
            move || {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.commit_song_if_changed();
                }
            }
        });

        Self {
            state,
            song_id,

            group,
        }
    }

    pub fn contents(&self) -> String {
        self.state.borrow().editor.text()
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.set_compiler_output(co);
        }
    }
}

impl State {
    fn commit_song_if_changed(&mut self) {
        if let Some(text) = self.editor.text_if_changed() {
            // The SongChanged message will set the unsaved flag.
            // It must not be sent if the MML text is unchanged.
            self.sender
                .send(Message::SongChanged(self.song_id.clone(), text));

            self.editor.clear_changed();
        }
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        match co {
            None => {
                self.console_buffer.set_text("");
                self.errors = None;
            }
            Some(Ok(size)) => {
                // ::TODO add more information::
                let text = format!("MML compiled successfully: {} bytes", size);
                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Foreground);
                self.errors = None;
            }
            Some(Err(e)) => {
                let text = match &e {
                    SongError::Dependency => e.to_string(),
                    SongError::Mml(e) => e.multiline_display().to_string(),
                    SongError::Song(e) => e.to_string(),
                };

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);

                self.errors = match e {
                    SongError::Dependency => None,
                    SongError::Mml(e) => Some(e),
                    SongError::Song(_) => None,
                }
            }
        }
    }
}
