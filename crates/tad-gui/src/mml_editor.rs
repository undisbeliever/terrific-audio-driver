//! MML Syntax Highlighting

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::ch_units_to_width;

use compiler::mml::{FIRST_MUSIC_CHANNEL, LAST_MUSIC_CHANNEL};

use std::cell::RefCell;
use std::rc::Rc;

extern crate fltk;
use fltk::enums::{Color, Font};
use fltk::prelude::DisplayExt;
use fltk::text::{StyleTableEntryExt, TextBuffer, TextEditor, WrapMode};

pub struct MmlEditorState {
    text_buffer: TextBuffer,
    style_buffer: TextBuffer,

    style_vec: Vec<u8>,

    changed_callback: Box<dyn Fn() + 'static>,
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
            text_buffer: text_buffer.clone(),
            style_buffer,
            style_vec: Vec::new(),
            changed_callback: Box::from(Self::blank_callback),
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

        self.style_buffer.replace(
            to_style_start,
            to_style_end + n_deleted - n_inserted,
            changed,
        );

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
}

struct MmlColors {
    normal: Color,
    comments: Color,
    metadata: Color,
    metadata_values: Color,

    instruments: Color,
    subroutines: Color,

    channel_names: Color,
    errors: Color,

    unknown: Color,
}

const MML_COLORS: MmlColors = MmlColors {
    normal: Color::Foreground,
    comments: Color::Blue,
    metadata: Color::DarkRed,
    metadata_values: Color::DarkRed,
    instruments: Color::DarkGreen,

    subroutines: Color::from_rgb(0xcc, 0x55, 0x00), // hsl(25, 100, 40)

    channel_names: Color::DarkMagenta,
    errors: Color::Red,

    unknown: Color::Green,
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

            _ => Style::Unknown,
        }
    }

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
        courier(mml_colors.errors),
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
    }
}

fn channel_or_subroutine_style(c: u8) -> Style {
    match c {
        b'@' => Style::Instrument,
        b'!' => Style::Subroutine,
        _ => Style::Normal,
    }
}
