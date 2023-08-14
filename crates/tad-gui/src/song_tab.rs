//! Song Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, SongError, SongOutput};
use crate::helpers::*;
use crate::tabs::{Tab, TabFileState};
use crate::Message;

use compiler::data::TextFile;
use compiler::errors::MmlCompileErrors;

use fltk::app;
use fltk::button::Button;
use fltk::enums::{Color, Event, Font};
use fltk::group::{Flex, Pack, PackType};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, TextEditor, WrapMode};

use std::cell::RefCell;
use std::rc::Rc;

pub struct State {
    sender: app::Sender<Message>,

    song_id: ItemId,

    editor: TextEditor,
    buffer: TextBuffer,

    console: TextDisplay,
    console_buffer: TextBuffer,

    errors: Option<MmlCompileErrors>,
}

pub struct SongTab {
    state: Rc<RefCell<State>>,

    group: Flex,
    file_state: TabFileState,
}

impl Tab for SongTab {
    fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_mut(&mut self) -> &mut Flex {
        &mut self.group
    }

    fn file_state(&self) -> &TabFileState {
        &self.file_state
    }

    fn file_state_mut(&mut self) -> &mut TabFileState {
        &mut self.file_state
    }
}

impl SongTab {
    pub fn new(song_id: ItemId, mml_file: &TextFile, sender: app::Sender<Message>) -> Self {
        let mut group = Flex::default_fill()
            .with_label(&mml_file.file_name)
            .column();

        let file_state = TabFileState::new(group.clone());

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

        let mut buffer = TextBuffer::default();
        buffer.can_undo(true);
        buffer.set_tab_distance(4);
        buffer.set_text(&mml_file.contents);

        let mut editor = TextEditor::default();
        editor.set_linenumber_width(ch_units_to_width(&editor, 4));
        editor.set_text_font(Font::Courier);
        editor.set_buffer(buffer.clone());

        let mut console = TextDisplay::default();
        group.fixed(&console, input_height(&console) * 5);

        group.end();

        let console_buffer = TextBuffer::default();
        console.set_text_font(Font::Courier);
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        let state = Rc::new(RefCell::from(State {
            sender,
            song_id,
            editor: editor.clone(),
            buffer: buffer.clone(),

            console,
            console_buffer,

            errors: None,
        }));

        compile_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.commit_song()
                }
            }
        });

        editor.handle({
            let s = state.clone();
            move |_widget, ev| match ev {
                Event::Unfocus => {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        s.commit_song_if_changed();
                    }
                    false
                }
                _ => false,
            }
        });

        Self {
            state,
            file_state,
            group,
        }
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.set_compiler_output(co);
        }
    }
}

impl State {
    fn commit_song_if_changed(&mut self) {
        if self.editor.changed() {
            self.commit_song();
        }
    }

    fn commit_song(&mut self) {
        self.sender.send(Message::SongChanged(
            self.song_id.clone(),
            self.buffer.text(),
        ));

        self.editor.clear_changed();
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
