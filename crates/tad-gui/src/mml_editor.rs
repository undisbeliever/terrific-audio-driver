//! MML Syntax Highlighting

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::AudioMonitorData;
use crate::helpers::ch_units_to_width;

use compiler::driver_constants::N_MUSIC_CHANNELS;
use compiler::errors::{MmlChannelError, MmlCompileErrors};
use compiler::mml::{FIRST_MUSIC_CHANNEL, LAST_MUSIC_CHANNEL};
use compiler::songs::{ChannelBcTracking, SongBcTracking, SongData};
use compiler::FilePosRange;

use std::cell::RefCell;
use std::cmp::min;
use std::rc::Rc;
use std::sync::Arc;

extern crate fltk;
use fltk::enums::{Color, Font};
use fltk::prelude::{DisplayExt, WidgetExt};
use fltk::text::{StyleTableEntryExt, TextAttr, TextBuffer, TextEditor, WrapMode};

pub struct MmlEditorState {
    widget: TextEditor,
    text_buffer: TextBuffer,
    style_buffer: TextBuffer,

    style_vec: Vec<u8>,

    changed_callback: Box<dyn Fn() + 'static>,

    song_data: Option<Arc<SongData>>,
    note_tracking_state: [NoteTrackingState; N_MUSIC_CHANNELS],

    errors_in_style_buffer: bool,
}

pub struct MmlEditor {
    widget: TextEditor,
    text_buffer: TextBuffer,

    state: Rc<RefCell<MmlEditorState>>,
}

impl MmlEditor {
    pub fn new() -> Self {
        let mut text_buffer = TextBuffer::default();
        text_buffer.set_tab_distance(4);
        text_buffer.can_undo(true);

        let mut style_buffer = TextBuffer::default();
        style_buffer.can_undo(false);

        let mut widget = TextEditor::default();
        widget.wrap_mode(WrapMode::AtBounds, 0);
        widget.set_linenumber_width(ch_units_to_width(&widget, 4));
        widget.set_text_font(Font::Courier);
        widget.set_buffer(text_buffer.clone());

        widget.set_highlight_data_ext(
            style_buffer.clone(),
            highlight_data(&MML_COLORS, widget.text_size()),
        );

        let state = Rc::new(RefCell::from(MmlEditorState {
            widget: widget.clone(),
            text_buffer: text_buffer.clone(),
            style_buffer,
            style_vec: Vec::new(),

            note_tracking_state: Default::default(),
            song_data: None,

            changed_callback: Box::from(Self::blank_callback),

            errors_in_style_buffer: false,
        }));

        text_buffer.add_modify_callback({
            let s = state.clone();
            move |a, b, c, d, e| {
                s.borrow_mut().buffer_modified(a, b, c, d, e);
            }
        });

        Self {
            state,
            text_buffer,
            widget,
        }
    }

    pub fn set_text_size(&mut self, text_size: i32) {
        self.widget.set_text_font(Font::Courier);
        self.widget.set_text_size(text_size);

        self.widget.set_highlight_data_ext(
            self.widget.style_buffer(),
            highlight_data(&MML_COLORS, text_size),
        );
    }

    pub fn set_changed_callback(&mut self, f: impl Fn() + 'static) {
        self.state.borrow_mut().changed_callback = Box::from(f);
    }

    pub fn highlight_errors(&mut self, errors: Option<&MmlCompileErrors>) {
        match errors {
            Some(e) => self.state.borrow_mut().highlight_errors(e),
            None => self.state.borrow_mut().remove_errors(),
        }
    }

    pub fn widget(&self) -> &TextEditor {
        &self.widget
    }

    pub fn text(&self) -> String {
        self.text_buffer.text()
    }

    pub fn set_text(&mut self, text: &str) {
        self.text_buffer.set_text(text);
    }

    fn blank_callback() {}

    pub fn set_song_data(&mut self, song_data: Arc<SongData>) {
        self.state.borrow_mut().set_song_data(Some(song_data));
    }

    pub fn update_note_tracking(&mut self, mon: AudioMonitorData) {
        self.state.borrow_mut().update_note_tracking(mon);
    }
}

impl MmlEditorState {
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

        let to_style_start = pos;
        let to_style_end = match self.text_buffer.find_char_forward(pos + n_inserted, '\n') {
            Some(i) => i + 1,
            None => self.text_buffer.length(),
        };

        let changed = Self::update_style_vec(
            &mut self.style_vec,
            to_style_start,
            to_style_end,
            &self.text_buffer,
        );

        if self.song_data.is_none() {
            self.style_buffer.replace(
                to_style_start,
                to_style_end + n_deleted - n_inserted,
                changed,
            );
        } else {
            self.set_song_data(None);
        }

        (self.changed_callback)();
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
    ) -> &'a str {
        let start_usize = usize::try_from(start).unwrap();
        let end_usize = usize::try_from(end).unwrap();

        let range = start_usize..end_usize;

        let prev_style_char = start_usize.checked_sub(1).map(|i| style_vec[i]);
        let to_style = &mut style_vec[range];

        match text_buffer.text_range(start, end) {
            Some(s) => {
                Self::populate_style(to_style, &s, prev_style_char);
            }
            None => {
                to_style.fill(Style::Unknown.to_u8_char());
            }
        }

        std::str::from_utf8(to_style).unwrap()
    }

    fn populate_style(style_vec: &mut [u8], text: &str, prev_style: Option<u8>) {
        assert_eq!(style_vec.len(), text.bytes().len());

        let mut current = prev_style
            .map(Style::from_u8_char)
            .unwrap_or(Style::NewLine);

        for (c, s) in std::iter::zip(text.bytes(), style_vec.iter_mut()) {
            current = next_style(current, c);
            *s = current.to_u8_char();
        }
    }

    fn highlight_errors(&mut self, errors: &MmlCompileErrors) {
        let mut style_vec = self.style_vec.clone();

        let mut min_index = usize::MAX;
        let mut max_index = 0;

        let mut highlight_error = |pos: &FilePosRange| {
            if let Ok(start) = usize::try_from(pos.index_start()) {
                if let Ok(end) = usize::try_from(pos.index_end()) {
                    if let Some(slice) = style_vec.get_mut(start..end) {
                        slice.fill(Style::Error.to_u8_char());
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

        for e in &errors.line_errors {
            highlight_error(&e.0);
        }

        let mut parse_channel_error = |e: &MmlChannelError| {
            for e in &e.errors {
                highlight_error(&e.0);
            }
        };

        for e in &errors.subroutine_errors {
            parse_channel_error(e);
        }
        for e in &errors.channel_errors {
            parse_channel_error(e);
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

    fn set_song_data(&mut self, song_data: Option<Arc<SongData>>) {
        self.song_data = song_data;
        self.note_tracking_state = Default::default();

        let style = std::str::from_utf8(&self.style_vec).unwrap();
        self.style_buffer.set_text(style);
    }

    fn update_note_tracking(&mut self, mon: AudioMonitorData) {
        let mut changed = false;
        if let Some(song_data) = &self.song_data {
            if let Some(ntd) = &song_data.tracking() {
                for i in 0..self.note_tracking_state.len() {
                    let nts = &mut self.note_tracking_state[i];
                    let voice_pos = mon.voice_instruction_ptrs[i];

                    changed |=
                        nts.update(&mut self.style_buffer, ntd, i, voice_pos, &self.style_vec);
                }
            }
        }

        // Redraw the entire widget, required for the new style to be immediately visible.
        // NOTE: Fl_Text_Display::redisplay_range() is not available.
        if changed {
            self.widget.redraw();
        }
    }
}

pub struct NotePos {
    buffer_pos: i32,
    subroutine: Option<usize>,
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
        channel_index: usize,
        voice_pos: Option<u16>,
        style_vec: &Vec<u8>,
    ) -> bool {
        if voice_pos == self.song_ptr {
            return false;
        }
        self.song_ptr = voice_pos;

        let vc = match &tracking_data.channels[channel_index] {
            Some(vc) => vc,
            None => return false,
        };

        let new_pos = match voice_pos {
            Some(vp) => self
                .prev_pos
                .as_ref()
                .and_then(|prev| Self::search_prev_pos(vc, &tracking_data.subroutines, prev, vp))
                .or_else(|| Self::search_all(vc, &tracking_data.subroutines, vp)),
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

    fn search_prev_pos(
        channel_tracking: &ChannelBcTracking,
        subroutines: &[ChannelBcTracking],
        prev_pos: &NotePos,
        voice_pos: u16,
    ) -> Option<NotePos> {
        let c = match prev_pos.subroutine {
            Some(i) => match subroutines.get(i) {
                Some(t) => t,
                None => return None,
            },
            None => channel_tracking,
        };

        Self::search_next_few_items(prev_pos.subroutine, c, prev_pos.index, voice_pos)
            .or_else(|| Self::binary_search_channel(prev_pos.subroutine, c, voice_pos))
    }

    fn search_all(
        channel_tracking: &ChannelBcTracking,
        subroutines: &[ChannelBcTracking],
        voice_pos: u16,
    ) -> Option<NotePos> {
        if let Some(nts) = Self::binary_search_channel(None, channel_tracking, voice_pos) {
            Some(nts)
        } else {
            subroutines
                .iter()
                .enumerate()
                .find_map(|(i, s)| Self::binary_search_channel(Some(i), s, voice_pos))
        }
    }

    fn binary_search_channel(
        subroutine_index: Option<usize>,
        c: &ChannelBcTracking,
        voice_pos: u16,
    ) -> Option<NotePos> {
        if c.range.contains(&voice_pos) {
            let channel_pos = voice_pos.saturating_sub(c.range.start).saturating_sub(2);
            let index = match c.bytecodes.binary_search_by_key(&channel_pos, |b| b.bc_pos) {
                Ok(i) => i,
                Err(i) => i,
            };
            let buffer_pos = c
                .bytecodes
                .get(index)
                .and_then(|b| b.char_index.try_into().ok());

            buffer_pos.map(|buffer_pos| NotePos {
                subroutine: subroutine_index,
                index,
                buffer_pos,
            })
        } else {
            None
        }
    }

    // Search the next few items in ChannelBcTracking for the voice_pos.
    fn search_next_few_items(
        subroutine_index: Option<usize>,
        c: &ChannelBcTracking,
        starting_index: usize,
        voice_pos: u16,
    ) -> Option<NotePos> {
        const TO_SEARCH: usize = 6;

        let to_find_range = starting_index..min(starting_index + TO_SEARCH, c.bytecodes.len());
        let to_find = match c.bytecodes.get(to_find_range) {
            Some(s) => s,
            None => return None,
        };

        let channel_pos = voice_pos.saturating_sub(c.range.start).saturating_sub(2);

        if channel_pos < to_find.first().unwrap().bc_pos
            || channel_pos > to_find.last().unwrap().bc_pos
        {
            return None;
        }

        to_find
            .iter()
            .enumerate()
            .find(|(_i, b)| b.bc_pos >= channel_pos)
            .map(|(i, b)| NotePos {
                subroutine: subroutine_index,
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
    subroutines: Color,

    channel_names: Color,
    invalid: Color,

    error_bg: Color,

    unknown: Color,

    tracker_bg: Color,
}

const MML_COLORS: MmlColors = MmlColors {
    normal: Color::Foreground,
    comments: Color::Blue,
    metadata: Color::DarkRed,
    metadata_values: Color::DarkRed,
    instruments: Color::DarkGreen,

    subroutines: Color::from_rgb(0xcc, 0x55, 0x00), // hsl(25, 100, 40)

    channel_names: Color::DarkMagenta,

    invalid: Color::Red,
    error_bg: Color::Red,

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

    Error,

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

            b'Q' => Style::Error,

            b'R' => Style::NoteTracking,

            _ => Style::Unknown,
        }
    }

    const NOTE_TRACKER_STR: &str = "R";

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
        bg_bold(mml_colors.error_bg),
        bg_bold(mml_colors.tracker_bg),
        courier(mml_colors.unknown),
    ]
}

fn next_style(current: Style, c: u8) -> Style {
    const FIRST_CHANNEL: u8 = FIRST_MUSIC_CHANNEL as u8;
    const LAST_CHANNEL: u8 = LAST_MUSIC_CHANNEL as u8;

    if c == b'\n' {
        return Style::NewLine;
    }
    if c == b';' {
        return Style::Comment;
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

        Style::Error => Style::Error,
        Style::NoteTracking => Style::Error,
    }
}

fn channel_or_subroutine_style(c: u8) -> Style {
    match c {
        b'@' => Style::Instrument,
        b'!' => Style::Subroutine,
        _ => Style::Normal,
    }
}
