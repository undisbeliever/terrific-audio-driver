//! Input widget helpers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::notes::Note;
use compiler::project::{BlockNumber, SampleNumber};
use compiler::{envelope::Adsr, envelope::Gain, identifier::Name, notes::Octave};

use fltk::button::CheckButton;
use fltk::enums::{Align, Event, Key};
use fltk::frame::Frame;
use fltk::group::Flex;
use fltk::input::{FloatInput, Input, IntInput};
use fltk::prelude::{GroupExt, InputExt, WidgetExt};

use std::path::PathBuf;

pub fn label(s: &str) -> Frame {
    Frame::default()
        .with_label(s)
        .with_align(Align::Inside | Align::Left)
}

pub fn label_packed(s: &str) -> Frame {
    let mut l = label(s);

    fltk::draw::set_font(l.label_font(), l.label_size());

    let draw_width = fltk::draw::width(s);
    let draw_height = fltk::draw::height();

    let draw_width = if !draw_width.is_nan() {
        draw_width as i32
    } else {
        100
    };

    l.set_size(draw_width, draw_height + draw_height / 2);

    l
}

pub fn label_center(s: &str) -> Frame {
    Frame::default()
        .with_label(s)
        .with_align(Align::Inside | Align::Center)
}

pub fn label_top_center(s: &str) -> Frame {
    Frame::default()
        .with_label(s)
        .with_align(Align::Inside | Align::Top | Align::Center)
}

pub fn is_input_done_event(e: Event) -> bool {
    match e {
        Event::KeyDown => fltk::app::event_key() == Key::Enter,
        Event::Unfocus => true,
        _ => false,
    }
}

pub fn select_all_and_take_focus(input: &mut Input) {
    let _ = input.set_position(0);
    let _ = input.set_mark(input.value().len().try_into().unwrap_or(i32::MAX));
    let _ = input.take_focus();
}

pub trait SetActive {
    fn set_active(&mut self, active: bool);
}

impl<W> SetActive for W
where
    W: WidgetExt,
{
    fn set_active(&mut self, active: bool) {
        if active {
            self.activate();
        } else {
            self.deactivate();
        }
    }
}

pub trait InputHelper
where
    Self: Sized,
    Self::Widget: InputExt,
{
    type Widget;

    fn parse(s: String) -> Option<Self>;
    fn set_widget_value(w: &mut Self::Widget, value: &Self);

    fn read_or_reset(w: &mut Self::Widget, old_value: &Self) -> Option<Self> {
        match Self::parse(w.value()) {
            Some(v) => Some(v),
            None => {
                Self::set_widget_value(w, old_value);
                None
            }
        }
    }
}

impl InputHelper for String {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        Some(s)
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(value);
    }

    fn read_or_reset(w: &mut Self::Widget, old_value: &Self) -> Option<Self> {
        let _ = old_value;
        Some(w.value())
    }
}

impl InputHelper for Name {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        Name::try_new_lossy(s)
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(value.as_str());
    }
}

impl InputHelper for PathBuf {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        if !s.is_empty() {
            Some(s.into())
        } else {
            None
        }
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_string_lossy());
    }
}

impl InputHelper for f64 {
    type Widget = FloatInput;

    fn parse(s: String) -> Option<Self> {
        s.parse().ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_string());
    }
}

impl InputHelper for Octave {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        s.parse::<u32>().ok()?.try_into().ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.as_u8().to_string());
    }
}

impl InputHelper for Note {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        Note::parse_bytecode_argument(&s).ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_bytecode_argument());
    }
}

impl InputHelper for usize {
    type Widget = IntInput;

    fn parse(s: String) -> Option<Self> {
        s.parse().ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_string())
    }
}

impl InputHelper for Adsr {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        s.parse().ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_gui_string())
    }
}

impl InputHelper for Gain {
    type Widget = Input;

    fn parse(s: String) -> Option<Self> {
        s.parse().ok()
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.to_gui_string())
    }
}

impl InputHelper for SampleNumber {
    type Widget = IntInput;

    fn parse(s: String) -> Option<Self> {
        s.parse::<usize>().ok().map(Self)
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.0.to_string())
    }
}

impl InputHelper for BlockNumber {
    type Widget = IntInput;

    fn parse(s: String) -> Option<Self> {
        s.parse::<usize>().ok().map(Self)
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        w.set_value(&value.0.to_string())
    }
}

impl<T> InputHelper for Option<T>
where
    T: InputHelper,
{
    type Widget = T::Widget;

    fn parse(s: String) -> Option<Self> {
        if s.is_empty() {
            Some(None)
        } else {
            T::parse(s).map(Some)
        }
    }

    fn set_widget_value(w: &mut Self::Widget, value: &Self) {
        match value {
            Some(v) => T::set_widget_value(w, v),
            None => w.set_value(""),
        }
    }
}

pub fn ch_units_to_width(w: &impl WidgetExt, ch_units: i32) -> i32 {
    fltk::draw::set_font(w.label_font(), w.label_size());

    let ch_width = fltk::draw::width("0");

    if !ch_width.is_nan() {
        (ch_width.clamp(4.0, 50.0) as i32) * ch_units
    } else {
        ch_units * 20
    }
}

pub fn input_height(w: &impl WidgetExt) -> i32 {
    fltk::draw::set_font(w.label_font(), w.label_size());
    let min_line_spacing = fltk::draw::height();
    min_line_spacing + min_line_spacing / 2
}

pub struct InputForm {
    group: Flex,

    row_height: i32,
    left_column_width: i32,
    n_rows: i32,
}

impl InputForm {
    pub fn new(ch_units: i32) -> Self {
        let group = Flex::default().column();

        let left_column_width = ch_units_to_width(&group, ch_units);
        let row_height = input_height(&group);

        Self {
            group,
            left_column_width,
            row_height,
            n_rows: 0,
        }
    }

    /// returns (Flex, form height)
    pub fn end(self) -> (Flex, i32) {
        let form_height = self.row_height * self.n_rows + self.group.pad() * (self.n_rows - 1);

        self.group.end();

        (self.group, form_height)
    }

    #[allow(dead_code)]
    pub fn add_checkbox_left(&mut self, text: &str) -> CheckButton {
        let w = CheckButton::default().with_label(text);
        self.group.fixed(&w, self.row_height);

        self.n_rows += 1;

        w
    }

    #[allow(dead_code)]
    pub fn add_checkbox_right(&mut self, text: &str) -> CheckButton {
        let mut r = Flex::default().row();
        self.group.fixed(&r, self.row_height);

        let f = Frame::default();
        r.fixed(&f, self.left_column_width);

        let w = CheckButton::default().with_label(text);
        self.group.fixed(&w, self.row_height);

        r.end();

        self.n_rows += 1;

        w
    }

    pub fn add_input<T>(&mut self, text: &str) -> T
    where
        T: WidgetExt + Default,
    {
        let mut r = Flex::default().row();
        self.group.fixed(&r, self.row_height);

        let l = label(text);
        r.fixed(&l, self.left_column_width);

        let w = T::default();

        r.end();

        self.n_rows += 1;

        w
    }
}
