//! MML Syntax Highlighting

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::AudioMonitorData;
use crate::helpers::ch_units_to_width;

use compiler::driver_constants::N_MUSIC_CHANNELS;
use compiler::errors::{
    MmlChannelError, MmlCompileErrors, SfxSubroutineErrors, SoundEffectError, SoundEffectErrorList,
};
use compiler::mml::command_parser::Quantization;
use compiler::mml::{ChannelId, CursorTracker, FIRST_MUSIC_CHANNEL, LAST_MUSIC_CHANNEL};
use compiler::songs::{BytecodePos, SongBcTracking, SongData};
use compiler::sound_effects::{CompiledSfxSubroutines, CompiledSoundEffect};
use compiler::time::{TickCounter, ZenLen, DEFAULT_ZENLEN};
use compiler::FilePosRange;

use std::cell::RefCell;
use std::cmp::{min, Ordering};
use std::fmt::Write;
use std::rc::Rc;
use std::sync::Arc;

extern crate fltk;
use fltk::enums::{Align, Color, Event, Font, FrameType};
use fltk::frame::Frame;
use fltk::prelude::{DisplayExt, WidgetBase, WidgetExt};
use fltk::text::{StyleTableEntryExt, TextAttr, TextBuffer, TextEditor, WrapMode};

pub enum TextErrorRef<'a> {
    Song(&'a MmlCompileErrors),
    SfxSubroutines(&'a SfxSubroutineErrors),
    SoundEffect(&'a SoundEffectError),
}

#[derive(Clone, Copy)]
pub enum TextFormat {
    Mml,
    Bytecode,
}

pub enum CompiledEditorData {
    Song(Arc<SongData>),
    SfxSubroutines(Arc<CompiledSfxSubroutines>),
    SoundEffect(Arc<CompiledSoundEffect>),
}

impl CompiledEditorData {
    fn cursor_tracker(&self) -> Option<&CursorTracker> {
        match self {
            Self::Song(d) => d.tracking().map(|t| &t.cursor_tracker),
            Self::SfxSubroutines(d) => Some(d.cursor_tracker()),
            Self::SoundEffect(d) => d.cursor_tracker(),
        }
    }

    fn zenlen(&self) -> ZenLen {
        match self {
            Self::Song(sd) => sd.metadata().mml_settings.zenlen,
            Self::SfxSubroutines(_) => DEFAULT_ZENLEN,
            Self::SoundEffect(_) => DEFAULT_ZENLEN,
        }
    }
}

// I cannot remove the `buffer_modified` callback from TextBuffer (`TextBuffer::remove_modify_callback()` takes a `FnMut` argument).
// Instead I add a new callback to each buffer as it is created.
// Wrapping `TextBuffer` in `EditorBuffer` ensures the `buffer_modified` callback is set correctly.
//
// `EditorBuffer` must only be used in the MmlEditor instance that created it.
pub struct EditorBuffer {
    text_buffer: TextBuffer,
    format: TextFormat,
}

impl EditorBuffer {
    pub fn text(&self) -> String {
        self.text_buffer.text()
    }
}

pub struct MmlEditorState {
    widget: TextEditor,
    status_bar: Frame,
    buffer: Rc<RefCell<EditorBuffer>>,
    style_buffer: TextBuffer,

    style_vec: Vec<u8>,

    changed_callback: Box<dyn Fn(&EditorBuffer) + 'static>,
    cursor_changed_callback: Box<dyn Fn(u32) + 'static>,

    compiled_data: Option<CompiledEditorData>,
    playing_song_notes_valid: bool,
    tracking_notes: bool,
    note_tracking_state: [NoteTrackingState; N_MUSIC_CHANNELS],
    subroutine_tracking_state: [NoteTrackingState; N_MUSIC_CHANNELS],

    prev_cursor_index: Option<u32>,

    errors_in_style_buffer: bool,
}

pub struct MmlEditor {
    widget: TextEditor,
    status_bar: Frame,

    state: Rc<RefCell<MmlEditorState>>,
}

impl MmlEditor {
    pub fn new(text: &str, format: TextFormat) -> Self {
        let mut text_buffer = TextBuffer::default();
        text_buffer.set_text(text);
        text_buffer.set_tab_distance(4);
        text_buffer.can_undo(true);

        let mut style_buffer = TextBuffer::default();
        style_buffer.can_undo(false);

        let mut widget = TextEditor::default();
        widget.set_buffer(text_buffer.clone());

        widget.wrap_mode(WrapMode::AtBounds, 0);
        widget.set_linenumber_width(ch_units_to_width(&widget, 4));
        widget.set_text_font(Font::Courier);

        widget.set_highlight_data_ext(
            style_buffer.clone(),
            highlight_data(&MML_COLORS, widget.text_size()),
        );

        let mut status_bar = Frame::default();
        status_bar.set_frame(FrameType::DownBox);
        status_bar.set_align(Align::Inside | Align::Left);

        let state = Rc::new(RefCell::from(MmlEditorState {
            widget: widget.clone(),
            status_bar: status_bar.clone(),

            buffer: Rc::new(RefCell::from(EditorBuffer {
                format,
                text_buffer: text_buffer.clone(),
            })),
            style_buffer,
            style_vec: Vec::new(),

            playing_song_notes_valid: false,
            tracking_notes: false,
            note_tracking_state: Default::default(),
            subroutine_tracking_state: Default::default(),

            prev_cursor_index: None,
            compiled_data: None,

            changed_callback: Box::from(Self::blank_callback),
            cursor_changed_callback: Box::from(Self::blank_cursor_callback),

            errors_in_style_buffer: false,
        }));

        state.borrow_mut().redraw_style();

        widget.handle({
            let s = state.clone();
            move |_, ev| match ev {
                // The `TextEditor::insert_position()` does not point to the new position when `ev` is `KeyDown`.
                // This adds lag to the status-bar when the user holds down an arrow-key.
                // Not updating the status bar when an arrow key is held down is worse then a laggy status bar.
                Event::KeyDown | Event::KeyUp | Event::Released => {
                    if let Ok(mut state) = s.try_borrow_mut() {
                        state.check_if_cursor_moved();
                    }
                    false
                }
                _ => false,
            }
        });

        text_buffer.add_modify_callback({
            let s = state.clone();
            move |a, b, c, d, e| {
                s.borrow_mut().buffer_modified(a, b, c, d, e);
            }
        });

        Self {
            state,
            widget,
            status_bar,
        }
    }

    pub fn new_buffer(&mut self, text: &str, format: TextFormat) -> Rc<RefCell<EditorBuffer>> {
        let mut text_buffer = TextBuffer::default();
        text_buffer.set_text(text);
        text_buffer.set_tab_distance(4);
        text_buffer.can_undo(true);

        text_buffer.add_modify_callback({
            let s = self.state.clone();
            move |a, b, c, d, e| {
                s.borrow_mut().buffer_modified(a, b, c, d, e);
            }
        });

        let buf = Rc::new(RefCell::from(EditorBuffer {
            text_buffer,
            format,
        }));

        self.state.borrow_mut().set_buffer(buf.clone());

        buf
    }

    pub fn set_format(&mut self, format: TextFormat) {
        let mut s = self.state.borrow_mut();
        s.buffer.borrow_mut().format = format;
        s.redraw_style();
    }

    pub fn set_buffer(&mut self, buf: Rc<RefCell<EditorBuffer>>) {
        self.state.borrow_mut().set_buffer(buf);
    }

    pub fn buffer(&self) -> Rc<RefCell<EditorBuffer>> {
        self.state.borrow().buffer.clone()
    }

    pub fn cursor_index(&self) -> Option<u32> {
        self.widget.insert_position().try_into().ok()
    }

    pub fn text(&self) -> String {
        self.state.borrow().buffer.borrow().text()
    }

    pub fn set_text(&mut self, txt: &str) {
        // Make a copy of the TextBuffer to prevent a `BorrowError` panic in `MmlEditorState::buffer_modified()`.
        let mut tb = self.state.borrow().buffer.borrow().text_buffer.clone();
        tb.set_text(txt)
    }

    pub fn activate(&mut self) {
        self.widget.activate();
    }

    pub fn deactivate(&mut self) {
        self.widget.deactivate();
    }

    pub fn set_text_size(&mut self, text_size: i32) {
        self.widget.set_text_font(Font::Courier);
        self.widget.set_text_size(text_size);

        self.widget.set_highlight_data_ext(
            self.widget.style_buffer(),
            highlight_data(&MML_COLORS, text_size),
        );
    }

    pub fn scroll_to_top(&mut self) {
        self.widget.set_insert_position(0);
        self.widget.scroll(0, 0);
    }

    pub fn take_focus(&mut self) {
        let _ = self.widget.take_focus();
    }

    // Safety: editing `self` in the callback will cause a `BorrowMutError` panic.
    pub fn set_changed_callback(&mut self, f: impl Fn(&EditorBuffer) + 'static) {
        self.state.borrow_mut().changed_callback = Box::from(f);
    }

    pub fn set_cursor_changed_callback(&mut self, f: impl Fn(u32) + 'static) {
        self.state.borrow_mut().cursor_changed_callback = Box::from(f);
    }

    pub fn move_cursor_to_line_end(&mut self, line_no: u32) {
        self.state.borrow_mut().move_cursor_to_line_end(line_no);
    }

    pub fn highlight_errors(&mut self, errors: Option<TextErrorRef>) {
        match errors {
            Some(e) => self.state.borrow_mut().highlight_errors(e),
            None => self.state.borrow_mut().remove_errors(),
        }
    }

    pub fn widget(&self) -> &TextEditor {
        &self.widget
    }

    pub fn status_bar(&self) -> &Frame {
        &self.status_bar
    }

    pub fn cursor_tick_counter(&self) -> Option<(ChannelId, TickCounter)> {
        self.state.borrow().cursor_tick_counter()
    }

    pub fn cursor_tick_counter_line_start(&self) -> Option<(ChannelId, TickCounter)> {
        self.state.borrow().cursor_tick_counter_line_start()
    }

    fn blank_callback(_: &EditorBuffer) {}

    fn blank_cursor_callback(_: u32) {}

    pub fn audio_thread_started_song(&mut self, song_data: Arc<SongData>) {
        self.state.borrow_mut().song_started(song_data);
    }

    pub fn set_compiled_data(&mut self, data: CompiledEditorData) {
        let mut s = self.state.borrow_mut();
        s.set_compiled_data(Some(data));
        s.update_statusbar();
    }

    pub fn clear_compiled_data(&mut self) {
        let mut s = self.state.borrow_mut();
        s.set_compiled_data(None);
        s.update_statusbar();
    }

    pub fn update_note_tracking(&mut self, mon: &AudioMonitorData) {
        self.state.borrow_mut().update_note_tracking(mon);
    }

    pub fn clear_note_tracking(&mut self) {
        self.state.borrow_mut().clear_note_tracking();
    }
}

impl MmlEditorState {
    fn set_buffer(&mut self, buf: Rc<RefCell<EditorBuffer>>) {
        self.buffer = buf;

        self.widget
            .set_buffer(self.buffer.borrow().text_buffer.clone());

        self.redraw_style();
        self.update_statusbar();
    }

    fn move_cursor_to_line_end(&mut self, line_no: u32) {
        let nth_newline = match line_no.saturating_sub(1).try_into() {
            Ok(i) => i,
            Err(_) => return,
        };

        let text_buffer = &mut self.buffer.borrow_mut().text_buffer;

        // Fl_Text_Buffer::skip_lines() is not available in fltk-rs, manually skipping lines instead.
        let text = text_buffer.text();

        let i = text
            .bytes()
            .enumerate()
            .filter(|(_, c)| *c == b'\n')
            .nth(nth_newline)
            .map(|(i, _)| i)
            .unwrap_or(text.len() - 1);

        if let Ok(i) = i.try_into() {
            text_buffer.remove_selection();

            self.widget.set_insert_position(i);
            self.widget.show_insert_position();
            let _ = self.widget.take_focus();
        }
    }

    fn redraw_style(&mut self) {
        let editor_buffer = &self.buffer.borrow();

        let len = editor_buffer.text_buffer.length().max(0);

        self.style_vec
            .resize(len.try_into().unwrap(), Style::Unknown.to_u8_char());

        let changed = Self::update_style_vec(
            &mut self.style_vec,
            0,
            len,
            &editor_buffer.text_buffer,
            editor_buffer.format,
        );

        self.style_buffer.set_text(changed);
    }

    fn buffer_modified(
        &mut self,
        pos: i32,
        n_inserted: i32,
        n_deleted: i32,
        _n_restyled: i32,
        _deleted_text: &str,
    ) {
        let pos_usize = match usize::try_from(pos) {
            Ok(u) => u,
            Err(_) => return,
        };
        let n_inserted_usize = match usize::try_from(n_inserted) {
            Ok(u) => u,
            Err(_) => return,
        };
        let n_deleted_usize = match usize::try_from(n_deleted) {
            Ok(u) => u,
            Err(_) => return,
        };

        if n_inserted_usize == 0 && n_deleted_usize == 0 {
            return;
        }

        self.resize_style_vec(pos_usize, n_inserted_usize, n_deleted_usize);

        let editor_buffer = &self.buffer.borrow();
        let text_buffer = &editor_buffer.text_buffer;

        let to_style_start = pos;
        let to_style_end = match text_buffer.find_char_forward(pos + n_inserted, '\n') {
            Some(i) => i + 1,
            None => text_buffer.length(),
        };

        let changed = Self::update_style_vec(
            &mut self.style_vec,
            to_style_start,
            to_style_end,
            text_buffer,
            editor_buffer.format,
        );

        if self.playing_song_notes_valid {
            self.playing_song_notes_valid = false;

            let style = std::str::from_utf8(&self.style_vec).unwrap();
            self.style_buffer.set_text(style);
        } else {
            self.style_buffer.replace(
                to_style_start,
                to_style_end + n_deleted - n_inserted,
                changed,
            );
        }

        (self.changed_callback)(editor_buffer);
    }

    fn resize_style_vec(&mut self, pos: usize, n_inserted: usize, n_deleted: usize) {
        if n_inserted > n_deleted {
            // Add characters to the style buffer
            let to_insert = n_inserted - n_deleted;

            // ::ANNOY no insert multiple in Vec::
            assert!(to_insert > 0);
            self.style_vec.splice(
                pos..pos,
                std::iter::repeat(Style::Unknown.to_u8_char()).take(to_insert),
            );
        }
        if n_deleted > n_inserted {
            // Remove characters
            let to_delete = n_deleted - n_inserted;
            self.style_vec.drain(pos..(pos + to_delete));
        }
    }

    fn update_style_vec<'a>(
        style_vec: &'a mut [u8],
        start: i32,
        end: i32,
        text_buffer: &TextBuffer,
        format: TextFormat,
    ) -> &'a str {
        let start_usize = usize::try_from(start).unwrap();
        let end_usize = usize::try_from(end).unwrap();

        let range = start_usize..end_usize;

        let prev_style_char = start_usize.checked_sub(1).map(|i| style_vec[i]);
        let to_style = &mut style_vec[range];

        match text_buffer.text_range(start, end) {
            Some(s) => match format {
                TextFormat::Mml => Self::populate_mml_style(to_style, &s, prev_style_char),
                TextFormat::Bytecode => Self::populate_bc_style(to_style, &s, prev_style_char),
            },
            None => {
                to_style.fill(Style::Unknown.to_u8_char());
            }
        }

        std::str::from_utf8(to_style).unwrap()
    }

    fn populate_mml_style(style_vec: &mut [u8], text: &str, prev_style: Option<u8>) {
        assert_eq!(style_vec.len(), text.len());

        let mut current = prev_style
            .map(Style::from_u8_char)
            .unwrap_or(Style::NewLine);

        for (c, s) in std::iter::zip(text.bytes(), style_vec.iter_mut()) {
            current = next_style_mml(current, c);
            *s = current.to_u8_char();
        }
    }

    fn populate_bc_style(style_vec: &mut [u8], text: &str, prev_style: Option<u8>) {
        assert_eq!(style_vec.len(), text.len());

        let mut current = prev_style
            .map(Style::from_u8_char)
            .unwrap_or(Style::NewLine);

        for (c, s) in std::iter::zip(text.bytes(), style_vec.iter_mut()) {
            current = next_style_bc(current, c);
            *s = current.to_u8_char();
        }
    }

    fn highlight_errors(&mut self, errors: TextErrorRef) {
        let mut style_vec = self.style_vec.clone();

        let mut min_index = usize::MAX;
        let mut max_index = 0;

        let mut highlight_error = |pos: &FilePosRange, s: Style| {
            if let Ok(start) = usize::try_from(pos.index_start()) {
                if let Ok(end) = usize::try_from(pos.index_end()) {
                    if let Some(slice) = style_vec.get_mut(start..end) {
                        slice.fill(s.to_u8_char());
                        if start < min_index {
                            min_index = start;
                        }
                        if end > max_index {
                            max_index = end
                        }
                    }
                }
            }
        };

        match errors {
            TextErrorRef::Song(errors) => {
                for e in &errors.line_errors {
                    highlight_error(&e.0, Style::Error);
                }

                let mut parse_channel_error = |e: &MmlChannelError| {
                    for e in &e.errors {
                        highlight_error(&e.0, Style::Error);
                    }
                };

                for e in &errors.subroutine_errors {
                    parse_channel_error(e);
                }
                for e in &errors.channel_errors {
                    parse_channel_error(e);
                }
            }
            TextErrorRef::SfxSubroutines(errors) => match errors {
                SfxSubroutineErrors::LineErrors(errors) => {
                    for e in errors {
                        highlight_error(&e.0, Style::Error);
                    }
                }
                SfxSubroutineErrors::SubroutineErrors(errors) => {
                    for cerror in errors {
                        for e in &cerror.errors {
                            highlight_error(&e.0, Style::Error);
                        }
                    }
                }
                SfxSubroutineErrors::TooManySfxSubroutines(_) => (),
            },
            TextErrorRef::SoundEffect(errors) => match &errors.errors {
                SoundEffectErrorList::BytecodeErrors(errors) => {
                    for e in errors {
                        highlight_error(&e.0, Style::BytecodeError);
                    }
                }
                SoundEffectErrorList::MmlLineErrors(errors) => {
                    for e in errors {
                        highlight_error(&e.0, Style::Error);
                    }
                }
                SoundEffectErrorList::MmlErrors(errors) => {
                    for e in errors {
                        highlight_error(&e.0, Style::Error);
                    }
                }
            },
        }

        let style = std::str::from_utf8(&style_vec).unwrap();

        self.style_buffer.set_text(style);

        // Redraw the entire widget, required for the new style to be immediately visible.
        // NOTE: Fl_Text_Display::redisplay_range() is not available.
        self.widget.redraw();

        self.errors_in_style_buffer = true;
    }

    fn remove_errors(&mut self) {
        if self.errors_in_style_buffer {
            let style = std::str::from_utf8(&self.style_vec).unwrap();

            self.style_buffer.set_text(style);

            // Redraw the entire widget, required for the new style to be immediately visible.
            // NOTE: Fl_Text_Display::redisplay_range() is not available.
            self.widget.redraw();

            self.errors_in_style_buffer = false;
        }
    }

    fn set_compiled_data(&mut self, data: Option<CompiledEditorData>) {
        self.compiled_data = data;

        self.clear_note_tracking();

        self.prev_cursor_index = None;
    }

    fn clear_note_tracking(&mut self) {
        if self.tracking_notes {
            self.tracking_notes = false;
            self.note_tracking_state = Default::default();

            let style = std::str::from_utf8(&self.style_vec).unwrap();
            self.style_buffer.set_text(style);

            // Redraw the entire widget, required for the new style to be immediately visible.
            // NOTE: Fl_Text_Display::redisplay_range() is not available.
            self.widget.redraw();
        }
    }

    fn song_started(&mut self, song_data: Arc<SongData>) {
        self.set_compiled_data(Some(CompiledEditorData::Song(song_data)));
        self.playing_song_notes_valid = true;
    }

    fn update_note_tracking(&mut self, mon: &AudioMonitorData) {
        if !self.playing_song_notes_valid {
            return;
        }

        self.tracking_notes = true;

        let mut changed = false;
        if let Some(CompiledEditorData::Song(song_data)) = &self.compiled_data {
            if let Some(ntd) = song_data.tracking() {
                for (i, nts) in self.note_tracking_state.iter_mut().enumerate() {
                    let voice_pos = mon.voice_instruction_ptrs[i];

                    changed |= nts.update(&mut self.style_buffer, ntd, voice_pos, &self.style_vec);
                }

                for (i, sts) in self.subroutine_tracking_state.iter_mut().enumerate() {
                    let voice_pos = mon.voice_instruction_ptrs[i];
                    let return_pos = mon.voice_return_inst_ptrs[i];

                    // Only show return_pos if voice_pos is in a subroutine
                    let return_pos = match voice_pos {
                        Some(_) => return_pos,
                        _ => None,
                    };

                    changed |= sts.update(&mut self.style_buffer, ntd, return_pos, &self.style_vec);
                }
            }
        }

        // Redraw the entire widget, required for the new style to be immediately visible.
        // NOTE: Fl_Text_Display::redisplay_range() is not available.
        if changed {
            self.widget.redraw();
        }
    }

    fn cursor_index(&self) -> Option<u32> {
        self.widget.insert_position().try_into().ok()
    }

    fn cursor_tick_counter(&self) -> Option<(ChannelId, TickCounter)> {
        self.compiled_data
            .as_ref()?
            .cursor_tracker()?
            .find(self.cursor_index()?)
            .map(|(channel_id, c)| (channel_id, c.ticks.ticks))
    }

    fn cursor_tick_counter_line_start(&self) -> Option<(ChannelId, TickCounter)> {
        self.compiled_data
            .as_ref()?
            .cursor_tracker()?
            .find_line_start(self.cursor_index()?)
            .map(|(channel_id, c)| (channel_id, c.ticks.ticks))
    }

    fn check_if_cursor_moved(&mut self) {
        let ci = self.cursor_index();
        if self.prev_cursor_index != ci {
            if let Some(ci) = ci {
                self._update_statusbar(Some(ci));

                (self.cursor_changed_callback)(ci);
            }
        }
    }

    fn update_statusbar(&mut self) {
        let ci = self.cursor_index();
        self._update_statusbar(ci);
    }

    fn _update_statusbar(&mut self, cursor_index: Option<u32>) {
        self.prev_cursor_index = cursor_index;

        let (cursor_index, compiled_data) = match (cursor_index, &self.compiled_data) {
            (Some(ci), Some(sd)) => (ci, sd),
            _ => {
                self.status_bar.set_label("");
                return;
            }
        };

        match compiled_data
            .cursor_tracker()
            .and_then(|t| t.find(cursor_index))
        {
            Some((channel_id, c)) => {
                let mut s = String::with_capacity(64);

                match channel_id {
                    ChannelId::Channel(c) => {
                        let _ = write!(s, "{} ", c);
                    }
                    ChannelId::Subroutine(si) => match &compiled_data {
                        CompiledEditorData::Song(sd) => {
                            if let Some(subroutine) = sd.subroutines().get(usize::from(si)) {
                                let _ = write!(s, "!{} ", subroutine.identifier.as_str());
                            }
                        }
                        CompiledEditorData::SfxSubroutines(sfx_data) => {
                            if let Some(subroutine) = sfx_data.subroutines().get(usize::from(si)) {
                                let _ = write!(s, "!{} ", subroutine.identifier.as_str());
                            }
                        }
                        CompiledEditorData::SoundEffect(_) => (),
                    },
                    ChannelId::SoundEffect => (),
                    ChannelId::MmlPrefix => (),
                };

                let ticks = c.ticks.ticks.value();
                let in_loop = if c.ticks.in_loop { "+" } else { "" };

                let octave = c.state.octave.as_u8();

                let _ = write!(s, "{ticks}{in_loop} ticks  o{octave}");

                {
                    let dl = &c.state.default_length;
                    if !dl.length_in_ticks() {
                        let _ = write!(s, "  l{}", dl.length());
                        for _ in 0..dl.number_of_dots() {
                            s.push('.');
                        }
                    } else {
                        let _ = write!(s, "  l%{}", dl.length());
                        for _ in 0..dl.number_of_dots() {
                            s.push('.');
                        }
                    }
                }

                let so = c.state.semitone_offset;
                match so.cmp(&0) {
                    Ordering::Greater => {
                        let _ = write!(s, "  _M+{}", c.state.semitone_offset);
                    }
                    Ordering::Less => {
                        let _ = write!(s, "  _M{}", c.state.semitone_offset);
                    }
                    Ordering::Equal => {}
                }

                if !c.state.keyoff_enabled {
                    let _ = write!(s, "  K0");
                }

                if let Some(q) = c.state.quantize {
                    let q = q.as_u8();
                    if q % Quantization::FINE_QUANTIZATION_SCALE == 0 {
                        let _ = write!(s, "  Q{}", q / Quantization::FINE_QUANTIZATION_SCALE);
                    } else {
                        let _ = write!(s, "  Q%{}", q);
                    }
                }

                if c.state.zenlen != compiled_data.zenlen() {
                    let _ = write!(s, "  ZenLen: {}", c.state.zenlen.as_u8());
                }

                self.status_bar.set_label(&s);
            }
            None => {
                self.status_bar.set_label("");
            }
        }
    }
}

pub struct NotePos {
    buffer_pos: i32,
    index: usize,
}

#[derive(Default)]
pub struct NoteTrackingState {
    song_ptr: Option<u16>,
    prev_pos: Option<NotePos>,
}

impl NoteTrackingState {
    fn update(
        &mut self,
        style_buffer: &mut TextBuffer,
        tracking_data: &SongBcTracking,
        voice_pos: Option<u16>,
        style_vec: &Vec<u8>,
    ) -> bool {
        if voice_pos == self.song_ptr {
            return false;
        }
        self.song_ptr = voice_pos;

        let new_pos = match voice_pos {
            Some(vp) => self
                .prev_pos
                .as_ref()
                .and_then(|prev| Self::search_next_few_items(&tracking_data.bytecode, prev, vp))
                .or_else(|| Self::binary_search(&tracking_data.bytecode, vp)),
            None => None,
        };

        let old_buffer_pos = self.prev_pos.as_ref().map(|p| p.buffer_pos);
        let new_buffer_pos = new_pos.as_ref().map(|p| p.buffer_pos);

        self.prev_pos = new_pos;

        if new_buffer_pos != old_buffer_pos {
            if let Some(old_bp) = old_buffer_pos {
                Self::remove_note_lighlight(style_buffer, style_vec, old_bp);
            }
            if let Some(new_bp) = new_buffer_pos {
                Self::highlight_note(style_buffer, new_bp);
            }
            true
        } else {
            false
        }
    }

    fn binary_search(bc_tracking: &[BytecodePos], voice_pos: u16) -> Option<NotePos> {
        let index = match bc_tracking.binary_search_by_key(&voice_pos, |b| b.bc_end_pos) {
            Ok(i) => i,
            Err(i) => i,
        };
        let buffer_pos = bc_tracking
            .get(index)
            .and_then(|b| b.char_index.try_into().ok());

        buffer_pos.map(|buffer_pos| NotePos { index, buffer_pos })
    }

    // Search the next few items in ChannelBcTracking for the voice_pos.
    fn search_next_few_items(
        bc_tracking: &[BytecodePos],
        prev_pos: &NotePos,
        voice_pos: u16,
    ) -> Option<NotePos> {
        const TO_SEARCH: usize = 6;

        let starting_index = prev_pos.index;
        let to_find_range = starting_index..min(starting_index + TO_SEARCH, bc_tracking.len());
        let to_find = bc_tracking.get(to_find_range)?;

        if voice_pos < to_find.first().unwrap().bc_end_pos
            || voice_pos > to_find.last().unwrap().bc_end_pos
        {
            return None;
        }

        to_find
            .iter()
            .enumerate()
            .find(|(_i, b)| b.bc_end_pos >= voice_pos)
            .map(|(i, b)| NotePos {
                index: starting_index + i,
                buffer_pos: b.char_index.try_into().unwrap_or(0),
            })
    }

    #[allow(clippy::ptr_arg)]
    fn remove_note_lighlight(style_buffer: &mut TextBuffer, style_vec: &Vec<u8>, buffer_pos: i32) {
        if let Ok(i) = usize::try_from(buffer_pos) {
            if let Some(&s) = style_vec.get(i) {
                if let Ok(s) = std::str::from_utf8(&[s]) {
                    style_buffer.replace(buffer_pos, buffer_pos + 1, s);
                }
            }
        }
    }

    fn highlight_note(style_buffer: &mut TextBuffer, buffer_pos: i32) {
        if buffer_pos >= 0 && buffer_pos < style_buffer.length() {
            style_buffer.replace(buffer_pos, buffer_pos + 1, Style::NOTE_TRACKER_STR);
        }
    }
}

struct MmlColors {
    normal: Color,
    comments: Color,
    metadata: Color,
    metadata_values: Color,

    instruments: Color,
    instrument_hints: Color,
    subroutines: Color,

    channel_names: Color,
    invalid: Color,
    mml_bc_asm: Color,

    error_bg: Color,
    bytecode_error: Color,

    unknown: Color,

    tracker_bg: Color,
}

const MML_COLORS: MmlColors = MmlColors {
    normal: Color::Foreground,
    comments: Color::Blue,
    metadata: Color::DarkRed,
    metadata_values: Color::DarkRed,
    instruments: Color::DarkGreen,
    // ::TODO slightly different green for instrument hint::
    instrument_hints: Color::DarkGreen,
    mml_bc_asm: Color::DarkGreen,

    subroutines: Color::from_rgb(0xcc, 0x55, 0x00), // hsl(25, 100, 40)

    channel_names: Color::DarkMagenta,

    invalid: Color::Red,
    error_bg: Color::Red,
    bytecode_error: Color::Red,

    unknown: Color::Green,

    tracker_bg: Color::from_rgb(0xbb, 0x98, 0xcd), // hsl(280, 35, 70)
};

#[derive(Copy, Clone, Eq, PartialEq)]
enum Style {
    // If this style is edited YOU MUST ALSO edit `Style::from_u8_char()` and `highlight_data()`
    Normal,
    NewLine,

    Comment,

    MetaData,
    MetaDataValue,

    InstrumentLineDefinition,
    InstrumentLine,

    Instrument,
    InstrumentNumber,
    InstrumentName,

    SubroutineDefinition,
    Subroutine,
    SubroutineNumber,
    SubroutineName,

    ChannelName,
    InvalidLine,

    SlashAsm0,
    SlashAsm1,
    SlashAsm2,
    SlashAsm3,

    BytecodeAsm,
    BytecodeAsmComment,
    BytecodeAsmSeparator,

    InstrumentHintQuestionMark,
    InstrumentHint,
    InstrumentHintNumber,
    InstrumentHintName,

    Error,
    BytecodeError,

    NoteTracking,

    Unknown,
}

impl Style {
    const fn from_u8_char(c: u8) -> Style {
        match c {
            b'A' => Style::Normal,
            b'B' => Style::NewLine,

            b'C' => Style::Comment,

            b'D' => Style::MetaData,
            b'E' => Style::MetaDataValue,

            b'F' => Style::InstrumentLineDefinition,
            b'G' => Style::InstrumentLine,

            b'H' => Style::Instrument,
            b'I' => Style::InstrumentNumber,
            b'J' => Style::InstrumentName,

            b'K' => Style::SubroutineDefinition,
            b'L' => Style::Subroutine,
            b'M' => Style::SubroutineNumber,
            b'N' => Style::SubroutineName,

            b'O' => Style::ChannelName,
            b'P' => Style::InvalidLine,

            b'Q' => Style::SlashAsm0,
            b'R' => Style::SlashAsm1,
            b'S' => Style::SlashAsm2,
            b'T' => Style::SlashAsm3,

            b'U' => Style::BytecodeAsm,
            b'V' => Style::BytecodeAsmComment,
            b'W' => Style::BytecodeAsmSeparator,

            b'X' => Style::InstrumentHintQuestionMark,
            b'Y' => Style::InstrumentHint,
            b'Z' => Style::InstrumentHintNumber,
            b'[' => Style::InstrumentHintName,

            b'\\' => Style::Error,
            b']' => Style::BytecodeError,

            // MUST edit `NOTE_TRACKER_STR` when this character changes
            b'^' => Style::NoteTracking,

            _ => Style::Unknown,
        }
    }

    const NOTE_TRACKER_STR: &'static str = "^";

    const fn to_u8_char(self) -> u8 {
        b'A' + (self as u8)
    }
}

fn highlight_data(mml_colors: &MmlColors, font_size: i32) -> Vec<StyleTableEntryExt> {
    let courier = |c| StyleTableEntryExt {
        font: Font::Courier,
        size: font_size,
        color: c,
        ..StyleTableEntryExt::default()
    };
    let courier_bold = |c| StyleTableEntryExt {
        font: Font::CourierBold,
        size: font_size,
        color: c,
        ..StyleTableEntryExt::default()
    };
    let bg_bold = |bg| StyleTableEntryExt {
        font: Font::CourierBold,
        size: font_size,
        color: mml_colors.normal,
        bgcolor: bg,
        attr: TextAttr::BgColor,
    };

    vec![
        courier(mml_colors.normal),
        courier(mml_colors.normal),
        courier(mml_colors.comments),
        courier_bold(mml_colors.metadata),
        courier(mml_colors.metadata_values),
        courier_bold(mml_colors.instruments),
        courier(mml_colors.normal),
        courier_bold(mml_colors.instruments),
        courier(mml_colors.instruments),
        courier(mml_colors.instruments),
        courier_bold(mml_colors.subroutines),
        courier_bold(mml_colors.subroutines),
        courier(mml_colors.subroutines),
        courier(mml_colors.subroutines),
        courier_bold(mml_colors.channel_names),
        courier(mml_colors.invalid),
        courier(mml_colors.normal),
        courier(mml_colors.normal),
        courier(mml_colors.normal),
        courier(mml_colors.normal),
        courier(mml_colors.mml_bc_asm),
        courier(mml_colors.comments),
        courier(mml_colors.normal),
        courier_bold(mml_colors.instrument_hints),
        courier_bold(mml_colors.instrument_hints),
        courier(mml_colors.instrument_hints),
        courier(mml_colors.instrument_hints),
        bg_bold(mml_colors.error_bg),
        courier_bold(mml_colors.bytecode_error),
        bg_bold(mml_colors.tracker_bg),
        courier(mml_colors.unknown),
    ]
}

fn next_style_mml(current: Style, c: u8) -> Style {
    const FIRST_CHANNEL: u8 = FIRST_MUSIC_CHANNEL as u8;
    const LAST_CHANNEL: u8 = LAST_MUSIC_CHANNEL as u8;

    if c == b'\n' {
        return match current {
            Style::BytecodeAsm => Style::BytecodeAsm,
            Style::BytecodeAsmComment => Style::BytecodeAsm,
            _ => Style::NewLine,
        };
    }
    if c == b';' {
        return match current {
            Style::BytecodeAsm => Style::BytecodeAsmComment,
            Style::BytecodeAsmComment => Style::BytecodeAsmComment,
            _ => Style::Comment,
        };
    }

    match current {
        Style::NewLine => match c {
            b'#' => Style::MetaData,
            b'@' => Style::InstrumentLineDefinition,
            b'!' => Style::SubroutineDefinition,
            FIRST_CHANNEL..=LAST_CHANNEL => Style::ChannelName,
            _ => Style::InvalidLine,
        },

        Style::Comment => Style::Comment,

        Style::MetaData => match c {
            b' ' => Style::MetaDataValue,
            _ => Style::MetaData,
        },
        Style::MetaDataValue => Style::MetaDataValue,

        Style::InstrumentLineDefinition => match c {
            b' ' => Style::InstrumentLine,
            _ => Style::InstrumentLineDefinition,
        },
        Style::InstrumentLine => Style::InstrumentLine,

        Style::SubroutineDefinition => match c {
            b' ' => Style::Normal,
            _ => Style::SubroutineDefinition,
        },

        Style::ChannelName => match c {
            FIRST_CHANNEL..=LAST_CHANNEL => Style::ChannelName,
            b' ' => Style::Normal,
            _ => Style::InvalidLine,
        },

        Style::InvalidLine => Style::InvalidLine,

        Style::Normal => channel_or_subroutine_style(c),
        Style::Unknown => channel_or_subroutine_style(c),

        Style::Instrument => match c {
            c if c.is_ascii_digit() => Style::InstrumentNumber,
            _ => Style::InstrumentName,
        },
        Style::InstrumentNumber => match c {
            c if c.is_ascii_digit() => Style::InstrumentNumber,
            _ => channel_or_subroutine_style(c),
        },
        Style::InstrumentName => match c {
            b' ' => Style::Normal,
            _ => Style::InstrumentName,
        },

        Style::InstrumentHintQuestionMark => match c {
            b'@' => Style::InstrumentHint,
            _ => Style::InstrumentName,
        },
        Style::InstrumentHint => match c {
            c if c.is_ascii_digit() => Style::InstrumentHintNumber,
            _ => Style::InstrumentHintName,
        },
        Style::InstrumentHintNumber => match c {
            c if c.is_ascii_digit() => Style::InstrumentHintNumber,
            _ => channel_or_subroutine_style(c),
        },
        Style::InstrumentHintName => match c {
            b' ' => Style::Normal,
            _ => Style::InstrumentHintName,
        },

        Style::Subroutine => match c {
            c if c.is_ascii_digit() => Style::SubroutineNumber,
            _ => Style::SubroutineName,
        },
        Style::SubroutineNumber => match c {
            c if c.is_ascii_digit() => Style::SubroutineNumber,
            _ => channel_or_subroutine_style(c),
        },
        Style::SubroutineName => match c {
            b' ' => Style::Normal,
            _ => Style::SubroutineName,
        },

        Style::SlashAsm0 => match c {
            b'a' => Style::SlashAsm1,
            _ => Style::Normal,
        },
        Style::SlashAsm1 => match c {
            b's' => Style::SlashAsm2,
            _ => Style::Normal,
        },
        Style::SlashAsm2 => match c {
            b'm' => Style::SlashAsm3,
            _ => Style::Normal,
        },
        Style::SlashAsm3 => match c {
            b'{' => Style::BytecodeAsm,
            c if c.is_ascii_whitespace() => Style::SlashAsm3,
            _ => Style::Normal,
        },

        Style::BytecodeAsm | Style::BytecodeAsmSeparator => match c {
            b'}' => Style::Normal,
            b';' => Style::BytecodeAsmComment,
            b'|' => Style::BytecodeAsmSeparator,
            _ => Style::BytecodeAsm,
        },

        Style::BytecodeAsmComment => match c {
            b'\n' => Style::BytecodeAsm,
            _ => Style::BytecodeAsmComment,
        },

        Style::Error => Style::Error,
        Style::BytecodeError => Style::Error,
        Style::NoteTracking => Style::Error,
    }
}

fn channel_or_subroutine_style(c: u8) -> Style {
    match c {
        b'@' => Style::Instrument,
        b'?' => Style::InstrumentHintQuestionMark,
        b'!' => Style::Subroutine,
        b'\\' => Style::SlashAsm0,
        _ => Style::Normal,
    }
}

fn next_style_bc(current: Style, c: u8) -> Style {
    match c {
        b'\n' => Style::NewLine,
        b';' => Style::Comment,
        _ => match current {
            Style::NewLine => Style::Normal,
            _ => current,
        },
    }
}
