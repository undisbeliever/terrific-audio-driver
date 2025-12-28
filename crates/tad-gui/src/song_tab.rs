//! Song Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::{AudioMonitorData, MusicChannelsMask};
use crate::compiler_thread::{ItemId, SongError, SongOutput};
use crate::helpers::*;
use crate::menu::EditAction;
use crate::mml_editor::{CompiledEditorData, MmlEditor, TextErrorRef, TextFormat};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use compiler::data::TextFile;
use compiler::driver_constants::N_MUSIC_CHANNELS;
use compiler::errors::{MmlCompileErrors, MmlPrefixError};
use compiler::identifier::{ChannelId, MusicChannelIndex};
use compiler::mml::MmlTickCountTable;
use compiler::songs::{song_duration_string, SongData};
use compiler::time::TickCounter;

use fltk::app;
use fltk::button::{Button, ToggleButton};
use fltk::enums::{CallbackReason, CallbackTrigger, Color, Event, Font, Key};
use fltk::group::{Flex, Pack, PackType};
use fltk::input::{Input, IntInput};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};
use fltk::widget::Widget;

use std::cell::RefCell;
use std::cmp::min;
use std::rc::Rc;
use std::sync::Arc;

const MAX_START_TICKS: u32 = 2_000_000;

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

    group: Flex,

    song_start_ticks: IntInput,

    prev_channel_mask: MusicChannelsMask,
    channel_buttons: [ToggleButton; N_MUSIC_CHANNELS],

    sub_prefix_button: ToggleButton,
    sub_prefix_flex: Flex,
    sub_prefix: Input,

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

        let spacing = ch_units_to_width(&group, 1);
        let button_size = ch_units_to_width(&group, 4);

        let main_toolbar = Pack::default().with_type(PackType::Horizontal);
        group.fixed(&main_toolbar, button_size);

        let spacer = |width: i32| {
            Widget::default().with_size(width, button_size);
        };

        let button = |label: &str, tooltip: &str| {
            let mut b = Button::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(tooltip);
            b
        };

        let toggle_button = |label: &str, tooltip: &str| {
            let mut b = ToggleButton::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(tooltip);
            b
        };

        // NOTE: toolbar shortcuts are handled by the `group.handle()` callback below
        let mut compile_button = button("C", "Compile song");
        compile_button.set_label_font(Font::ScreenBold);
        spacer(spacing);

        let mut song_start_ticks = IntInput::default().with_size(button_size * 2, 0);
        song_start_ticks.set_value("0");
        song_start_ticks.set_tooltip("Song start position (in ticks)");

        let mut zero_start_ticks = button("0", "Zero start ticks");
        let mut set_start_ticks = button("set", "Set start ticks to cursor (F9)\nShift+Click (Shift+F9) will also mute the other channels");
        spacer(spacing);

        let mut play_button = button(
            "@play",
            "Play song (F5)\nShift+click (Shift+F5) to play song from the beginning",
        );
        let mut play_at_line_start_button = button("@play_line", "Play from line start (F6)");
        let mut play_at_cursor_button = button("@play_cursor", "Play from cursor (F7)");
        let mut play_channel_line_start_button =
            button("@play_c_line", "Play channel from line start (Shift F6)");
        let mut play_channel_cursor_button =
            button("@play_c_cursor", "Play channel from cursor (Shift F7)");
        let mut pause_resume_button = button("@pause", "Pause/Resume song (F8)");
        spacer(spacing * 2);

        let sub_prefix_button =
            toggle_button("!", "Temporary MML to run when previewing a subroutine");
        spacer(spacing * 2);

        let mut enable_all_button = button("All", "Enable all channels (Ctrl `)");
        spacer(spacing);

        let channel_buttons = std::array::from_fn(|i| {
            let channel = char::from(b'A' + i as u8);

            let mut b = ToggleButton::default()
                .with_size(button_size, button_size)
                .with_label(&channel.to_string());
            b.set_tooltip(&format!(
                "Toggle channel {} (Ctrl {})\nShift-click to select only channel {} (Ctrl+Shift {})",
                channel,
                i + 1,
                channel,
                i + 1
            ));
            b
        });
        spacer(spacing * 2);
        let mut sfx_button = button("SFX", "Open play sound effect window");

        main_toolbar.end();

        let mut sub_prefix_flex = Flex::default().row();
        group.fixed(&sub_prefix_flex, input_height(&sub_prefix_flex));
        sub_prefix_flex.hide();

        let l = label("! preview prefix: ");
        sub_prefix_flex.fixed(&l, ch_units_to_width(&l, 15));
        let mut sub_prefix = Input::default();
        sub_prefix.set_text_font(Font::Courier);

        sub_prefix_flex.end();

        let mut editor = MmlEditor::new(&mml_file.contents, TextFormat::Mml, &mut group);

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

        editor.take_focus();

        let state = Rc::new(RefCell::from(State {
            sender,
            song_id,
            group: group.clone(),
            song_start_ticks,
            prev_channel_mask: MusicChannelsMask::ALL,
            channel_buttons,
            sub_prefix_button,
            sub_prefix_flex,
            sub_prefix,
            editor,
            console,
            console_buffer,
            errors: None,
        }));

        group.set_trigger(CallbackTrigger::Closed);
        group.set_callback({
            let s = state.clone();
            move |_| {
                if app::callback_reason() == CallbackReason::Closed {
                    if let Ok(s) = s.try_borrow() {
                        s.sender.send(GuiMessage::RequestCloseSongTab(s.song_id))
                    }
                }
            }
        });

        // Handle toolbar shortcut keys
        // This is done here so focus is not stolen from the editor.
        group.handle({
            let s = state.clone();
            move |_widget, ev| match ev {
                Event::KeyDown => match app::event_key() {
                    Key::F9 => {
                        s.borrow_mut()
                            .set_start_ticks_clicked(app::is_event_shift());
                        true
                    }
                    Key::F5 => {
                        let mut s = s.borrow_mut();
                        match app::is_event_shift() {
                            false => s.play_song(),
                            true => s.play_song_start(),
                        }
                        true
                    }
                    Key::F6 => {
                        s.borrow_mut()
                            .play_song_at_line_start(app::is_event_shift());
                        true
                    }
                    Key::F7 => {
                        s.borrow_mut().play_song_at_cursor(app::is_event_shift());
                        true
                    }
                    Key::F8 => {
                        s.borrow_mut().pause_resume();
                        true
                    }
                    k => {
                        if app::is_event_ctrl() {
                            const BACKTICK: i32 = b'`' as i32;
                            const FIRST_CHANNEL: i32 = b'1' as i32;
                            const LAST_CHANNEL: i32 = FIRST_CHANNEL + N_MUSIC_CHANNELS as i32 - 1;

                            match k.bits() {
                                BACKTICK => {
                                    s.borrow_mut().enable_all_channels();
                                    true
                                }
                                k @ FIRST_CHANNEL..=LAST_CHANNEL => {
                                    let c = usize::try_from(k - FIRST_CHANNEL).unwrap();
                                    let mut s = s.borrow_mut();
                                    match app::is_event_shift() {
                                        false => s.toggle_channel(c),
                                        true => s.set_only_one_channel_index(c),
                                    }
                                    true
                                }
                                _ => false,
                            }
                        } else {
                            false
                        }
                    }
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

        zero_start_ticks.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.song_start_ticks.set_value("0");
                }
            }
        });

        set_start_ticks.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.set_start_ticks_clicked(app::is_event_shift());
                }
            }
        });

        play_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    match app::is_event_shift() {
                        false => s.play_song(),
                        true => s.play_song_start(),
                    }
                }
            }
        });
        play_at_cursor_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_song_at_cursor(false);
                }
            }
        });
        play_at_line_start_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_song_at_line_start(false);
                }
            }
        });
        play_channel_cursor_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_song_at_cursor(true);
                }
            }
        });
        play_channel_line_start_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.play_song_at_line_start(true);
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

        enable_all_button.set_callback({
            let s = state.clone();
            move |_| {
                if let Ok(mut s) = s.try_borrow_mut() {
                    s.enable_all_channels();
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

            s.editor.set_cursor_changed_callback({
                let s = state.clone();
                move |index| {
                    if let Ok(s) = s.try_borrow() {
                        s.on_cursor_moved(index);
                    }
                }
            });

            for (i, b) in &mut s.channel_buttons.iter_mut().enumerate() {
                b.set_value(true);

                b.set_callback({
                    let s = state.clone();
                    move |_| {
                        if let Ok(mut s) = s.try_borrow_mut() {
                            match app::is_event_shift() {
                                false => s.channel_button_clicked(),
                                true => s.set_only_one_channel_index(i),
                            }
                        }
                    }
                });
            }

            s.sub_prefix_button.set_callback({
                let s = state.clone();
                move |_| {
                    if let Ok(mut s) = s.try_borrow_mut() {
                        match s.sub_prefix_button.value() {
                            true => {
                                s.sub_prefix_flex.show();
                                let _ = s.sub_prefix_flex.take_focus();
                            }
                            false => s.sub_prefix_flex.hide(),
                        }
                        s.group.layout();
                    }
                }
            });

            s.sub_prefix.set_trigger(CallbackTrigger::Changed);
            s.sub_prefix.set_callback(State::on_sub_prefix_changed);
        }

        sfx_button.set_callback({
            let s = sender;
            move |_| {
                s.send(GuiMessage::ToggleSfxWindow);
            }
        });

        Self {
            state,
            song_id,

            group,

            new_file,
        }
    }

    pub fn reuse_tab(&mut self, new_id: ItemId, mml_file: &TextFile) {
        let mut s = self.state.borrow_mut();

        self.song_id = new_id;
        self.new_file = mml_file.path.is_none();

        s.song_id = new_id;
        s.set_compiler_output(None);
        s.editor.set_text(&mml_file.contents);

        s.sub_prefix_button.set(false);
        s.sub_prefix_flex.hide();
        s.sub_prefix.set_value("");

        s.editor.scroll_to_top();
        s.editor.take_focus();

        s.console.scroll(0, 0);

        s.prev_channel_mask = MusicChannelsMask::ALL;
        s.update_channel_buttons(MusicChannelsMask::ALL);

        s.group.layout();
    }

    pub fn contents(&self) -> String {
        self.state.borrow().editor.text()
    }

    pub fn set_compiler_output(&mut self, co: Option<SongOutput>) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.set_compiler_output(co);
        }
    }

    pub fn set_song_prefix_result(&mut self, r: Result<(), MmlPrefixError>) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.set_song_prefix_result(r);
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

    pub fn monitor_timer_elapsed(&mut self, mon: &AudioMonitorData) {
        self.state.borrow_mut().editor.update_note_tracking(mon);
    }

    pub fn clear_note_tracking(&mut self) {
        self.state.borrow_mut().editor.clear_note_tracking();
    }

    pub fn cursor_index(&self) -> Option<u32> {
        self.state.borrow().editor.cursor_index()
    }

    pub fn edit_action(&mut self, action: EditAction) {
        self.state.borrow_mut().editor.edit_action(action)
    }
}

impl State {
    fn on_cursor_moved(&self, cursor_index: u32) {
        self.sender.send(GuiMessage::SongCursorMoved {
            id: self.song_id,
            cursor_index,
        });
    }

    fn song_changed(&self, text: String) {
        self.sender.send(GuiMessage::SongChanged {
            id: self.song_id,
            mml: text,
            cursor_index: self.editor.cursor_index(),
        });
    }

    fn compile_song(&self) {
        self.sender.send(GuiMessage::RecompileSong {
            id: self.song_id,
            mml: self.editor.text(),
            cursor_index: self.editor.cursor_index(),
        });
    }

    fn set_start_ticks_clicked(&mut self, shift_pressed: bool) {
        if let Some((c, t)) = self.editor.cursor_tick_counter() {
            match c {
                ChannelId::Channel(c) => {
                    if shift_pressed {
                        self.set_only_one_channel(c);
                    }

                    let t = min(t.value(), MAX_START_TICKS);
                    self.song_start_ticks.set_value(&t.to_string());
                }
                ChannelId::SoundEffect => (),
                ChannelId::Subroutine(_) => (),
                ChannelId::MmlPrefix => (),
            }
        }
    }

    fn play_song(&mut self) {
        let ticks: u32 = match self.song_start_ticks.value().parse() {
            Ok(t) if t <= MAX_START_TICKS => t,
            Ok(_) => {
                self.song_start_ticks
                    .set_value(&MAX_START_TICKS.to_string());
                MAX_START_TICKS
            }
            Err(_) => {
                self.song_start_ticks.set_value("0");
                0
            }
        };

        self.update_channel_buttons(self.prev_channel_mask);

        self.sender.send(GuiMessage::PlaySong(
            self.song_id,
            self.editor.text(),
            TickCounter::new(ticks),
            self.prev_channel_mask,
        ));
    }

    fn play_song_start(&mut self) {
        self.update_channel_buttons(self.prev_channel_mask);

        self.sender.send(GuiMessage::PlaySong(
            self.song_id,
            self.editor.text(),
            TickCounter::new(0),
            self.prev_channel_mask,
        ));
    }

    fn play_song_tick_counter(
        &mut self,
        cursor: Option<(ChannelId, TickCounter)>,
        mute_other_channels: bool,
    ) {
        match cursor {
            Some((ChannelId::Channel(c), ticks)) => {
                let channels_mask = match mute_other_channels {
                    true => MusicChannelsMask::only_one_channel(c),
                    false => self.prev_channel_mask,
                };
                self.update_channel_buttons(channels_mask);

                self.sender.send(GuiMessage::PlaySong(
                    self.song_id,
                    self.editor.text(),
                    ticks,
                    channels_mask,
                ));
            }
            Some((ChannelId::Subroutine(si), ticks)) => {
                let prefix = match self.sub_prefix_button.value() {
                    true => Some(self.sub_prefix.value()),
                    false => None,
                };

                self.sender.send(GuiMessage::PlaySongSubroutine(
                    self.song_id,
                    self.editor.text(),
                    prefix,
                    si,
                    ticks,
                ));
            }
            _ => (),
        }
    }

    fn play_song_at_cursor(&mut self, mute_other_channels: bool) {
        self.play_song_tick_counter(self.editor.cursor_tick_counter(), mute_other_channels);
    }

    fn play_song_at_line_start(&mut self, mute_other_channels: bool) {
        self.play_song_tick_counter(
            self.editor.cursor_tick_counter_line_start(),
            mute_other_channels,
        );
    }

    fn pause_resume(&self) {
        self.sender.send(GuiMessage::PauseResumeAudio(self.song_id));
    }

    /// NOTE: will not modify `self.prev_channel_mask`
    fn update_channel_buttons(&mut self, mask: MusicChannelsMask) {
        for (i, b) in self.channel_buttons.iter_mut().enumerate() {
            b.set_value(mask.0 & (1u8 << i) != 0);
        }
    }

    fn set_only_one_channel(&mut self, c: MusicChannelIndex) {
        self.prev_channel_mask = MusicChannelsMask::only_one_channel(c);
        self.update_channel_buttons(self.prev_channel_mask);
    }

    fn set_only_one_channel_index(&mut self, i: usize) {
        self.prev_channel_mask = MusicChannelsMask::only_one_channel_index(i);
        self.update_channel_buttons(self.prev_channel_mask);
    }

    fn enable_all_channels(&mut self) {
        self.prev_channel_mask = MusicChannelsMask::ALL;
        for b in &mut self.channel_buttons {
            b.set_value(true);
        }
        self.sender.send(GuiMessage::SetMusicChannels(
            self.song_id,
            MusicChannelsMask::ALL,
        ));
    }

    fn toggle_channel(&mut self, channel_index: usize) {
        let b = &mut self.channel_buttons[channel_index];

        b.toggle(!b.is_toggled());
        self.channel_button_clicked();
    }

    fn channel_button_clicked(&mut self) {
        let mut channel_mask = 0x00;
        for (i, b) in self.channel_buttons.iter().enumerate() {
            if b.is_set() {
                channel_mask |= 1u8 << i;
            }
        }
        let channel_mask = MusicChannelsMask(channel_mask);

        self.sender
            .send(GuiMessage::SetMusicChannels(self.song_id, channel_mask));

        self.prev_channel_mask = channel_mask;
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
                    sd.metadata().echo_buffer.buffer_size(),
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
                    SongError::Dependency => "dependency error".to_string(),
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

    fn set_song_prefix_result(&mut self, r: Result<(), MmlPrefixError>) {
        match r {
            Ok(()) => {
                self.sub_prefix.set_text_color(Color::Foreground);
                self.sub_prefix.redraw();
            }
            Err(e) => {
                self.sub_prefix.set_text_color(Color::Red);
                self.sub_prefix.redraw();

                let text = e.multiline_display().to_string();

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);
            }
        }
    }

    fn on_sub_prefix_changed(sub_prefix: &mut Input) {
        if sub_prefix.text_color() == Color::Red {
            sub_prefix.set_text_color(Color::Foreground);
            sub_prefix.redraw();
        }
    }
}
