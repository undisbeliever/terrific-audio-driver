//! Song Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::AudioMonitorData;
use crate::compiler_thread::{ItemId, SongError, SongOutput};
use crate::helpers::*;
use crate::mml_editor::{CompiledEditorData, MmlEditor, TextErrorRef, TextFormat};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use compiler::data::TextFile;
use compiler::errors::MmlCompileErrors;
use compiler::mml::{ChannelId, MmlTickCountTable};
use compiler::songs::{song_duration_string, SongData};

use compiler::time::TickCounter;
use fltk::app::{self, event_key};
use fltk::button::Button;
use fltk::enums::{CallbackReason, CallbackTrigger, Color, Event, Font, Key};
use fltk::group::{Flex, Pack, PackType};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

pub fn blank_mml_file() -> TextFile {
    TextFile {
        path: None,
        file_name: "new song.mml".to_owned(),
        contents: String::new(),
    }
}

pub struct State {
    sender: app::Sender<GuiMessage>,

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

    new_file: bool,
}

impl Tab for SongTab {
    fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_mut(&mut self) -> &mut Flex {
        &mut self.group
    }

    fn file_type(&self) -> FileType {
        FileType::Song(self.song_id)
    }
}

impl SongTab {
    pub fn new(song_id: ItemId, mml_file: &TextFile, sender: app::Sender<GuiMessage>) -> Self {
        let mut group = Flex::default_fill().column();

        let new_file = mml_file.path.is_none();

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

        // ::TODO add custom icons::

        // NOTE: toolbar shortcuts are handled by the `group.handle()` callback below
        let mut compile_button = button("C", "Compile song");
        let mut play_button = button("@>", "Play song from the beginning (F5)");
        let mut play_at_line_start_button = button("@>|", "Play from line start (F6)");
        let mut play_at_cursor_button = button("@>[]", "Play from cursor (F7)");
        let mut pause_resume_button = button("@||", "Pause/Resume song (F8)");

        main_toolbar.end();

        let mut editor = MmlEditor::new(&mml_file.contents, TextFormat::Mml);

        editor.set_text_size(editor.widget().text_size() * 12 / 10);

        let mut console = TextDisplay::default();
        group.fixed(&console, input_height(&console) * 5);

        group.add(editor.status_bar());
        group.fixed(editor.status_bar(), input_height(editor.status_bar()));

        group.end();

        let console_buffer = TextBuffer::default();
        console.set_text_font(Font::Courier);
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        group.set_trigger(CallbackTrigger::Closed);
        group.set_callback({
            #[allow(clippy::clone_on_copy)]
            let song_id = song_id.clone();
            let sender = sender.clone();
            move |_| {
                if app::callback_reason() == CallbackReason::Closed {
                    sender.send(GuiMessage::RequestCloseSongTab(song_id))
                }
            }
        });

        let state = Rc::new(RefCell::from(State {
            sender,
            song_id,
            editor,

            console,
            console_buffer,

            errors: None,
        }));

        // Handle toolbar shortcut keys
        // This is done here so focus is not stolen from the editor.
        group.handle({
            let s = state.clone();
            move |_widget, ev| match ev {
                Event::KeyDown => match event_key() {
                    Key::F5 => {
                        s.borrow_mut().play_song();
                        true
                    }
                    Key::F6 => {
                        s.borrow_mut().play_song_at_line_start();
                        true
                    }
                    Key::F7 => {
                        s.borrow_mut().play_song_at_cursor();
                        true
                    }
                    Key::F8 => {
                        s.borrow_mut().pause_resume();
                        true
                    }
                    _ => false,
                },
                _ => false,
            }
        });

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
        play_at_cursor_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(s) = s.try_borrow() {
                    s.play_song_at_cursor();
                }
            }
        });
        play_at_line_start_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(s) = s.try_borrow() {
                    s.play_song_at_line_start();
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
                move |buffer| {
                    if let Ok(s) = s.try_borrow() {
                        s.song_changed(buffer.text());
                    }
                }
            });
        }

        Self {
            state,
            song_id,

            group,

            new_file,
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

    pub fn is_new_file(&self) -> bool {
        self.new_file
    }

    pub fn clear_new_file_flag(&mut self) {
        self.new_file = false;
    }

    pub fn audio_thread_started_song(&mut self, song_data: Arc<SongData>) {
        self.state
            .borrow_mut()
            .editor
            .audio_thread_started_song(song_data);
    }

    pub fn monitor_timer_elapsed(&mut self, mon: AudioMonitorData) {
        self.state.borrow_mut().editor.update_note_tracking(mon);
    }
}

impl State {
    fn song_changed(&self, text: String) {
        self.sender
            .send(GuiMessage::SongChanged(self.song_id, text));
    }

    fn compile_song(&self) {
        self.sender
            .send(GuiMessage::RecompileSong(self.song_id, self.editor.text()));
    }

    fn play_song(&self) {
        self.sender
            .send(GuiMessage::PlaySong(self.song_id, self.editor.text(), None));
    }

    fn play_song_tick_counter(&self, cursor: Option<(ChannelId, TickCounter)>) {
        if let Some((ChannelId::Channel(_), tc)) = cursor {
            self.sender.send(GuiMessage::PlaySong(
                self.song_id,
                self.editor.text(),
                Some(tc),
            ));
        }
    }

    fn play_song_at_cursor(&self) {
        self.play_song_tick_counter(self.editor.cursor_tick_counter());
    }

    fn play_song_at_line_start(&self) {
        self.play_song_tick_counter(self.editor.cursor_tick_counter_line_start());
    }

    fn pause_resume(&self) {
        self.sender.send(GuiMessage::PauseResumeAudio(self.song_id));
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        match co {
            None => {
                self.editor.clear_compiled_data();

                self.console_buffer.set_text("");
                self.errors = None;
            }
            Some(Ok(sd)) => {
                let text = format!(
                    "MML compiled successfully: {} bytes (+{} echo buffer bytes)\n\nDuration: {}\n{}",
                    sd.data().len(),
                    sd.metadata().echo_buffer.edl.buffer_size(),
                    song_duration_string(sd.duration()),
                    MmlTickCountTable(&sd),
                );
                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Foreground);
                self.errors = None;

                self.editor.set_compiled_data(CompiledEditorData::Song(sd));
            }
            Some(Err(e)) => {
                self.editor.clear_compiled_data();

                let text = match &e {
                    SongError::Dependency => e.to_string(),
                    SongError::Song(e) => e.multiline_display().to_string(),
                    SongError::TooLarge(e) => e.multiline_display().to_string(),
                };

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);

                self.errors = match e {
                    SongError::Dependency => None,
                    SongError::Song(compiler::errors::SongError::MmlError(e)) => Some(e),
                    SongError::Song(_) => None,
                    SongError::TooLarge { .. } => None,
                };
            }
        }

        self.editor
            .highlight_errors(self.errors.as_ref().map(TextErrorRef::Song));
    }
}
