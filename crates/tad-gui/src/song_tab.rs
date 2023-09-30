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

use compiler::song_duration_string;
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

        let mut compile_button = button("C", "Compile song");
        let mut play_button = button("@>", "Play song");
        let mut pause_resume_button = button("@||", "Pause/Resume song");

        main_toolbar.end();

        let mut editor = MmlEditor::new();
        editor.set_text_size(editor.widget().text_size() * 12 / 10);
        editor.set_text(&mml_file.contents);

        let mut console = TextDisplay::default();
        group.fixed(&console, input_height(&console) * 5);

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
                if let Ok(s) = s.try_borrow() {
                    s.compile_song();
                }
            }
        });

        play_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(s) = s.try_borrow() {
                    s.play_song();
                }
            }
        });
        pause_resume_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(s) = s.try_borrow() {
                    s.pause_resume();
                }
            }
        });

        {
            let mut s = state.borrow_mut();

            s.editor.set_changed_callback({
                let s = state.clone();
                move || {
                    if let Ok(s) = s.try_borrow() {
                        s.song_changed();
                    }
                }
            });
        }

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
    fn song_changed(&self) {
        self.sender.send(Message::SongChanged(
            self.song_id.clone(),
            self.editor.text(),
        ));
    }

    fn compile_song(&self) {
        self.sender.send(Message::RecompileSong(
            self.song_id.clone(),
            self.editor.text(),
        ));
    }

    fn play_song(&self) {
        self.sender
            .send(Message::PlaySong(self.song_id.clone(), self.editor.text()));
    }

    fn pause_resume(&self) {
        self.sender
            .send(Message::PauseResumeAudio(self.song_id.clone()));
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        match co {
            None => {
                self.console_buffer.set_text("");
                self.errors = None;
            }
            Some(Ok(o)) => {
                let text = format!(
                    "MML compiled successfully: {} bytes (+{} echo buffer bytes)\n\nDuration: {}\n{}",
                    o.data_size,
                    o.echo_buffer.buffer_size(),
                    song_duration_string(o.duration),
                    o.tick_count_table
                );
                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Foreground);
                self.errors = None;
            }
            Some(Err(e)) => {
                let text = match &e {
                    SongError::Dependency => e.to_string(),
                    SongError::Mml(e) => e.multiline_display().to_string(),
                    SongError::Song(e) => e.to_string(),
                    SongError::TooLarge(e) => e.multiline_display().to_string(),
                };

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);

                self.errors = match e {
                    SongError::Dependency => None,
                    SongError::Mml(e) => Some(e),
                    SongError::Song(_) => None,
                    SongError::TooLarge { .. } => None,
                };
            }
        }
        self.editor.highlight_errors(self.errors.as_ref());
    }
}
