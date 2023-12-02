//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CombineSamplesError, InstrumentOutput, ItemId, PlaySampleArgs};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, ListAction, ListButtons, ListEditor, ListEditorTable, ListMessage,
    ListState, TableCompilerOutput, TableMapping,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use compiler::data::{self, Instrument, LoopSetting};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::errors::ValueError;
use compiler::notes::{Note, Octave, PitchChar, STARTING_OCTAVE};
use compiler::path::SourcePathBuf;
use compiler::samples::{BRR_EXTENSION, WAV_EXTENSION};

use fltk::button::{Button, RadioRoundButton};
use fltk::frame::Frame;
use fltk::misc::Spinner;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::{Align, Color, Event};
use fltk::group::{Flex, Group};
use fltk::input::{FloatInput, Input, IntInput};
use fltk::menu::Choice;
use fltk::output::Output;
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

#[derive(Clone, Copy)]
enum LoopChoice {
    None = 0,
    OverrideBrrLoopPoint = 1,
    LoopWithFilter = 2,
    LoopResetFilter = 3,
    DupeBlockHack = 4,
}
impl LoopChoice {
    const CHOICES: &'static str = concat![
        "&None",
        "|&Override BRR Loop Point",
        "|&Loop With Filter",
        "|Loop &Resets Filter",
        "|&Dupe Block Hack"
    ];

    fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::None,
            1 => Self::OverrideBrrLoopPoint,
            2 => Self::LoopWithFilter,
            3 => Self::LoopResetFilter,
            4 => Self::DupeBlockHack,

            _ => Self::None,
        }
    }

    fn to_i32(self) -> i32 {
        self as i32
    }
}

#[derive(Clone, Copy)]
enum EnvelopeChoice {
    Adsr = 0,
    Gain = 1,
}
impl EnvelopeChoice {
    const CHOICES: &'static str = "ADSR|GAIN";

    fn read_widget(c: &Choice) -> Option<EnvelopeChoice> {
        match c.value() {
            0 => Some(EnvelopeChoice::Adsr),
            1 => Some(EnvelopeChoice::Gain),
            _ => None,
        }
    }

    fn to_i32(self) -> i32 {
        self as i32
    }
}

const DEFAULT_ADSR: Adsr = match Adsr::try_new(12, 2, 2, 15) {
    Ok(a) => a,
    Err(_) => panic!("Invalid ADSR"),
};
const DEFAULT_GAIN: Gain = Gain::new(127);

fn blank_instrument() -> Instrument {
    Instrument {
        name: "name".parse().unwrap(),
        source: SourcePathBuf::default(),
        freq: 500.0,
        loop_setting: LoopSetting::None,
        first_octave: STARTING_OCTAVE,
        last_octave: STARTING_OCTAVE,
        envelope: Envelope::Adsr(DEFAULT_ADSR),
        comment: None,
    }
}

struct InstrumentMapping;

impl TableMapping for InstrumentMapping {
    type DataType = data::Instrument;
    type RowType = RowWithStatus<SimpleRow<1>>;

    const CAN_CLONE: bool = true;

    fn type_name() -> &'static str {
        "instrument"
    }

    fn headers() -> Vec<String> {
        vec!["Instruments".to_owned()]
    }

    fn add_clicked() -> GuiMessage {
        GuiMessage::Instrument(ListMessage::Add(blank_instrument()))
    }

    fn to_message(lm: ListMessage<data::Instrument>) -> GuiMessage {
        GuiMessage::Instrument(lm)
    }

    fn new_row(i: &Instrument) -> Self::RowType {
        RowWithStatus::new_unchecked(SimpleRow::new([i.name.as_str().to_string()]))
    }

    fn edit_row(r: &mut Self::RowType, i: &Instrument) -> bool {
        r.columns.edit_column(0, i.name.as_str())
    }
}

impl TableCompilerOutput for InstrumentMapping {
    type CompilerOutputType = InstrumentOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<InstrumentOutput>) -> bool {
        r.set_status_optional_result(co)
    }
}

#[derive(Debug, Eq, PartialEq)]
enum SourceFileType {
    Unknown,
    Wav,
    Brr,
}

pub struct InstrumentEditor {
    group: Flex,

    sender: app::Sender<GuiMessage>,

    selected_index: Option<usize>,
    data: Instrument,

    source_file_type: SourceFileType,

    name: Input,
    source: Output,
    freq: FloatInput,
    loop_choice: Choice,
    loop_setting: IntInput,
    first_octave: IntInput,
    last_octave: IntInput,
    envelope_choice: Choice,
    envelope_value: Input,
    comment: Input,

    prev_adsr: String,
    prev_gain: String,
}

impl InstrumentEditor {
    fn new(sender: app::Sender<GuiMessage>) -> (Rc<RefCell<InstrumentEditor>>, i32) {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);
        let freq = form.add_input::<FloatInput>("Frequency:");
        let loop_settings = form.add_two_inputs::<Choice, IntInput>("Loop:", 25);
        let first_octave = form.add_input::<IntInput>("First octave:");
        let last_octave = form.add_input::<IntInput>("Last octave:");
        let envelope = form.add_two_inputs::<Choice, Input>("Envelope:", 12);
        let comment = form.add_input::<Input>("Comment:");

        let form_height = 9 * form.row_height();
        let group = form.take_group_end();

        let (source, mut source_button) = source;
        let (loop_choice, loop_setting) = loop_settings;
        let (envelope_choice, envelope_value) = envelope;

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_index: None,
            data: blank_instrument(),
            source_file_type: SourceFileType::Unknown,
            name,
            source,
            freq,
            loop_choice,
            loop_setting,
            first_octave,
            last_octave,
            envelope_choice,
            envelope_value,
            comment,
            prev_adsr: DEFAULT_ADSR.to_gui_string(),
            prev_gain: DEFAULT_GAIN.to_gui_string(),
        }));

        {
            let mut editor = out.borrow_mut();

            editor.loop_choice.add_choice(LoopChoice::CHOICES);
            editor.envelope_choice.add_choice(EnvelopeChoice::CHOICES);

            editor.disable_editor();

            macro_rules! add_callbacks {
                ($name:ident) => {
                    let _: &dyn InputExt = &editor.$name;
                    editor.$name.handle({
                        let s = out.clone();
                        move |_widget, ev| Self::widget_event_handler(&s, ev)
                    });
                };
            }
            add_callbacks!(name);
            add_callbacks!(source);
            add_callbacks!(freq);
            add_callbacks!(loop_setting);
            add_callbacks!(first_octave);
            add_callbacks!(last_octave);
            add_callbacks!(envelope_value);
            add_callbacks!(comment);

            editor.loop_choice.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().loop_choice_changed()
            });

            editor.envelope_choice.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().envelope_choice_changed()
            });

            source_button.set_label("...");
            source_button.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().source_button_clicked()
            });
        }
        (out, form_height)
    }

    fn widget_event_handler(s: &Rc<RefCell<InstrumentEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
    }

    fn source_button_clicked(&mut self) {
        if let Some(index) = self.selected_index {
            self.sender
                .send(GuiMessage::OpenInstrumentSampleDialog(index));
        }
    }

    fn on_finished_editing(&mut self) {
        if let Some(new_data) = self.read_or_reset() {
            self.send_edit_message(new_data);
        }
    }

    fn send_edit_message(&self, data: Instrument) {
        if let Some(index) = self.selected_index {
            self.sender
                .send(GuiMessage::Instrument(ListMessage::ItemEdited(index, data)));
        }
    }

    fn read_or_reset(&mut self) -> Option<Instrument> {
        #[allow(clippy::question_mark)]
        if self.selected_index.is_none() {
            return None;
        }

        let old = &self.data;

        macro_rules! read_or_reset {
            ($field:ident) => {
                let $field = InputHelper::read_or_reset(&mut self.$field, &old.$field);
            };
        }
        read_or_reset!(name);
        read_or_reset!(freq);
        read_or_reset!(first_octave);
        read_or_reset!(last_octave);
        read_or_reset!(comment);

        let loop_setting = self.read_or_reset_loop_setting();
        let envelope = self.read_or_reset_envelope();

        Some(Instrument {
            name: name?,
            freq: freq?,
            loop_setting: loop_setting?,
            first_octave: first_octave?,
            last_octave: last_octave?,
            envelope: envelope?,
            comment: comment?,

            // must be last (after the ?'s)
            source: self.data.source.clone(),
        })
    }

    fn reset_loop_setting_widget(&mut self, choice: LoopChoice) {
        let w = &mut self.loop_setting;

        match choice {
            LoopChoice::None => {
                w.set_value("");
                w.deactivate();
            }

            LoopChoice::OverrideBrrLoopPoint
            | LoopChoice::LoopWithFilter
            | LoopChoice::LoopResetFilter => {
                let lp = match self.data.loop_setting {
                    LoopSetting::OverrideBrrLoopPoint(lp) => lp,
                    LoopSetting::LoopWithFilter(lp) => lp,
                    LoopSetting::LoopResetFilter(lp) => lp,
                    LoopSetting::DupeBlockHack(_) => 0,
                    LoopSetting::None => 0,
                };
                w.set_value(&lp.to_string());
                w.activate();
            }

            LoopChoice::DupeBlockHack => {
                let bc = match self.data.loop_setting {
                    LoopSetting::DupeBlockHack(dbh) => dbh,
                    _ => 2,
                };
                w.set_value(&bc.to_string());
                w.activate();
            }
        }
    }

    fn loop_choice_changed(&mut self) {
        let choice = LoopChoice::read_widget(&self.loop_choice);

        self.reset_loop_setting_widget(choice);

        self.on_finished_editing();
    }

    fn read_or_reset_loop_setting(&mut self) -> Option<LoopSetting> {
        let choice = LoopChoice::read_widget(&self.loop_choice);
        let value = self.loop_setting.value().parse().ok();

        let value = match choice {
            LoopChoice::None => Some(LoopSetting::None),
            LoopChoice::OverrideBrrLoopPoint => value.map(LoopSetting::OverrideBrrLoopPoint),
            LoopChoice::LoopWithFilter => value.map(LoopSetting::LoopWithFilter),
            LoopChoice::LoopResetFilter => value.map(LoopSetting::LoopResetFilter),
            LoopChoice::DupeBlockHack => value.map(LoopSetting::DupeBlockHack),
        };

        if value.is_none() {
            self.reset_loop_setting_widget(choice);
        }
        value
    }

    fn envelope_choice_changed(&mut self) {
        let new_value = match EnvelopeChoice::read_widget(&self.envelope_choice) {
            Some(EnvelopeChoice::Adsr) => &self.prev_adsr,
            Some(EnvelopeChoice::Gain) => &self.prev_adsr,
            None => "",
        };

        let w = &mut self.envelope_value;

        w.set_value(new_value);

        // Select all
        let _ = w.set_position(0);
        let _ = w.set_mark(i32::MAX);

        let _ = w.take_focus();
    }

    fn read_or_reset_envelope(&mut self) -> Option<Envelope> {
        let value = self.envelope_value.value();

        match EnvelopeChoice::read_widget(&self.envelope_choice) {
            Some(EnvelopeChoice::Adsr) => match InputHelper::parse(value.clone()) {
                Some(adsr) => {
                    self.prev_adsr = value;
                    Some(Envelope::Adsr(adsr))
                }
                None => {
                    self.envelope_value.set_value(&self.prev_adsr);
                    None
                }
            },
            Some(EnvelopeChoice::Gain) => match InputHelper::parse(value.clone()) {
                Some(gain) => {
                    self.prev_gain = value;
                    Some(Envelope::Gain(gain))
                }
                None => {
                    self.envelope_value.set_value(&self.prev_gain);
                    None
                }
            },
            None => None,
        }
    }

    fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.freq.set_value("");
        self.first_octave.set_value("");
        self.last_octave.set_value("");

        self.loop_choice.set_value(-1);
        self.loop_setting.set_value("");

        self.envelope_choice.set_value(-1);
        self.envelope_value.set_value("");

        self.selected_index = None;
    }

    fn set_data(&mut self, index: usize, data: &Instrument) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
        set_widget!(freq);
        set_widget!(first_octave);
        set_widget!(last_octave);
        set_widget!(comment);

        self.source.set_value(data.source.as_str());

        let (lc, lv) = match data.loop_setting {
            LoopSetting::None => (LoopChoice::None, None),
            LoopSetting::OverrideBrrLoopPoint(lp) => (LoopChoice::OverrideBrrLoopPoint, Some(lp)),
            LoopSetting::LoopWithFilter(lp) => (LoopChoice::LoopWithFilter, Some(lp)),
            LoopSetting::LoopResetFilter(lp) => (LoopChoice::LoopResetFilter, Some(lp)),
            LoopSetting::DupeBlockHack(dbh) => (LoopChoice::DupeBlockHack, Some(dbh)),
        };
        self.loop_choice.set_value(lc.to_i32());

        match lv {
            Some(v) => {
                self.loop_setting.set_value(&v.to_string());
                self.loop_setting.activate();
            }
            None => {
                self.loop_setting.set_value("");
                self.loop_setting.deactivate();
            }
        }

        self.update_source_file_type(&data.source);

        match data.envelope {
            Envelope::Adsr(adsr) => {
                self.envelope_choice
                    .set_value(EnvelopeChoice::Adsr.to_i32());

                InputHelper::set_widget_value(&mut self.envelope_value, &adsr);
                self.prev_adsr = self.envelope_value.value();
            }
            Envelope::Gain(gain) => {
                self.envelope_choice
                    .set_value(EnvelopeChoice::Gain.to_i32());

                InputHelper::set_widget_value(&mut self.envelope_value, &gain);
                self.prev_gain = self.envelope_value.value();
            }
        }

        self.selected_index = Some(index);
        self.data = data.clone();

        self.group.activate();
    }

    fn update_source_file_type(&mut self, source: &SourcePathBuf) {
        let sft = match source.extension() {
            Some(WAV_EXTENSION) => SourceFileType::Wav,
            Some(BRR_EXTENSION) => SourceFileType::Brr,
            _ => SourceFileType::Unknown,
        };

        if self.source_file_type != sft {
            self.source_file_type = sft;
            self.update_loop_choices();
        }
    }

    fn update_loop_choices(&mut self) {
        macro_rules! update_choices {
            ($($choice:ident),*) => {
                $(
                    let can_use = can_use_loop_setting(LoopChoice::$choice, &self.source_file_type);

                    if let Some(mut m) = self.loop_choice.at(LoopChoice::$choice.to_i32()) {
                        if can_use {
                            m.activate();
                        }
                        else {
                            m.deactivate()
                        }
                    }
                )*
            };
        }

        update_choices!(
            None,
            OverrideBrrLoopPoint,
            LoopWithFilter,
            LoopResetFilter,
            DupeBlockHack
        );
    }

    fn list_edited(&mut self, action: &ListAction<Instrument>) {
        if let ListAction::Edit(index, data) = action {
            if self.selected_index == Some(*index) {
                // Update name as the name deduplicator may have changed it.
                if self.data.name != data.name {
                    self.data.name = data.name.clone();
                    InputHelper::set_widget_value(&mut self.name, &self.data.name);
                }

                // Update source as it may have been changed by `open_instrument_sample_dialog()`
                if self.data.source != data.source {
                    self.data.source = data.source.clone();
                    self.source.set_value(data.source.as_str());
                    self.update_source_file_type(&data.source);
                }
            }
        }
    }
}

pub struct TestSampleWidget {
    selected_index: Option<usize>,
    selected_id: Option<ItemId>,

    sender: app::Sender<GuiMessage>,

    group: Group,

    octave: Spinner,
    note_length: Spinner,

    default_envelope: RadioRoundButton,
    adsr_envelope: RadioRoundButton,
    gain_envelope: RadioRoundButton,

    adsr_a: Spinner,
    adsr_d: Spinner,
    adsr_sl: Spinner,
    adsr_sr: Spinner,

    gain: Spinner,
}

impl TestSampleWidget {
    const KEYS: [(i32, &'static str); 12] = [
        (0, "C"),
        (1, ""),
        (2, "D"),
        (3, ""),
        (4, "E"),
        (6, "F"),
        (7, ""),
        (8, "G"),
        (9, ""),
        (10, "A"),
        (11, ""),
        (12, "B"),
    ];

    fn new(sender: app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut group = Group::default();
        group.make_resizable(false);

        let line_height = ch_units_to_width(&group, 3);

        let widget_width = ch_units_to_width(&group, 66);
        let widget_height = line_height * 6;

        group.set_size(widget_width, widget_height);

        let key_width = ch_units_to_width(&group, 5);
        let key_height = line_height * 3;
        let key_group_width = key_width * 7;

        let key_group = Group::new(0, 0, key_group_width, key_height * 2, None);

        let mut key_buttons: Vec<Button> = Vec::with_capacity(Self::KEYS.len());

        for (x, label) in Self::KEYS {
            let x = x * key_width / 2;
            let y = i32::from(!label.is_empty()) * key_height;

            let mut b = Button::new(x, y, key_width, key_height, None);
            if !label.is_empty() {
                b.set_color(Color::BackGround2);
                b.set_label_color(Color::Foreground);
                b.set_label(label);
            } else {
                b.set_color(Color::Foreground);
            }
            key_buttons.push(b);
        }

        key_group.end();

        let options_width = ch_units_to_width(&group, 30);
        let options_x = widget_width - options_width;
        let options_group = Group::new(options_x, 0, options_width, line_height * 7, None);

        let pos = |row, n_cols, col| -> (i32, i32, i32, i32) {
            assert!(col < n_cols);

            let spacing = (options_width - 2) / n_cols;
            let w = spacing - 2;
            let h = line_height;
            let x = options_x + spacing * col + 2;
            let y = row * h;

            (x, y, w, h)
        };

        let radio = |row, n_cols, col, label: &'static str| {
            let (x, y, w, h) = pos(row, n_cols, col);
            RadioRoundButton::new(x, y, w, h, Some(label))
        };

        let spinner =
            |row, n_cols, col, label: &'static str, tooltip: &str, min: u8, max: u8, value: u8| {
                let (x, y, w, h) = pos(row, n_cols, col);
                let mut c = Spinner::new(x, y, w, h, Some(label));
                if !tooltip.is_empty() {
                    c.set_tooltip(tooltip);
                }
                c.set_align(Align::Top);
                c.set_range(min.into(), max.into());
                c.set_value(value.into());
                c.set_step(1.0);
                c
            };

        let octave = spinner(1, 2, 0, "Octave", "", Octave::MIN, Octave::MAX, 4);
        let mut note_length = spinner(1, 2, 1, "Note Length", "", 2, 255, u8::MAX);
        note_length.set_maximum(1000.0);

        let default_envelope = radio(3, 3, 0, "Default");
        let adsr_envelope = radio(3, 3, 1, "ADSR");
        let gain_envelope = radio(3, 3, 2, "GAIN");

        let adsr_a = spinner(5, 4, 0, "A", "Attack", 0, Adsr::ATTACK_MAX, 12);
        let adsr_d = spinner(5, 4, 1, "D", "Decay", 0, Adsr::DECAY_MAX, 2);
        #[rustfmt::skip]
        let adsr_sl = spinner(5, 4, 2, "SL", "Sustain Level", 0, Adsr::SUSTAIN_LEVEL_MAX, 2);
        let adsr_sr = spinner(5, 4, 3, "SR", "Sustain Rate", 0, Adsr::SUSTAIN_RATE_MAX, 16);

        let gain = spinner(5, 2, 0, "GAIN", "", 0, u8::MAX, 127);

        options_group.end();

        group.end();

        let out = Rc::from(RefCell::new(Self {
            selected_index: None,
            selected_id: None,
            sender,
            group,

            octave,
            note_length,

            default_envelope,
            adsr_envelope,
            gain_envelope,

            adsr_a,
            adsr_d,
            adsr_sl,
            adsr_sr,

            gain,
        }));

        {
            let mut widget = out.borrow_mut();

            let set_envelope_callback = |w: &mut RadioRoundButton| {
                w.set_callback({
                    let state = out.clone();
                    move |_w| {
                        if let Ok(mut s) = state.try_borrow_mut() {
                            s.on_envelope_changed();
                        }
                    }
                });
            };
            set_envelope_callback(&mut widget.default_envelope);
            set_envelope_callback(&mut widget.adsr_envelope);
            set_envelope_callback(&mut widget.gain_envelope);

            widget.clear_selected();
            widget.on_envelope_changed();
        }

        for (i, button) in key_buttons.iter_mut().enumerate() {
            button.set_callback({
                let state = out.clone();
                let i = u8::try_from(i).unwrap();
                let pitch = PitchChar::try_from(i).unwrap();
                move |_w| {
                    if let Ok(s) = state.try_borrow() {
                        let _ = s.on_key_pressed(pitch);
                    }
                }
            });
        }

        out
    }

    fn clear_selected(&mut self) {
        self.selected_index = None;
        self.selected_id = None;
        self.group.deactivate();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, inst: &Instrument) {
        self.selected_index = Some(index);
        self.selected_id = Some(id);
        self.group.activate();
        self.update_octave_range(inst);
    }

    fn instrument_changed(&mut self, index: usize, inst: &Instrument) {
        if self.selected_index == Some(index) {
            self.update_octave_range(inst);
        }
    }

    fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    fn update_octave_range(&mut self, inst: &Instrument) {
        let first = inst.first_octave.as_u8();
        let last = inst.last_octave.as_u8();

        let min = std::cmp::min(first, last);
        let max = std::cmp::max(first, last);

        self.octave.set_range(min.into(), max.into());

        let v = self.octave.value() as u8;
        self.octave.set_value(v.clamp(min, max).into());
    }

    fn hide_adsr(&mut self) {
        self.adsr_a.hide();
        self.adsr_d.hide();
        self.adsr_sl.hide();
        self.adsr_sr.hide();
    }

    fn on_envelope_changed(&mut self) {
        if self.adsr_envelope.is_toggled() {
            self.adsr_a.show();
            self.adsr_d.show();
            self.adsr_sl.show();
            self.adsr_sr.show();

            self.gain.hide();
        } else if self.gain_envelope.is_toggled() {
            self.hide_adsr();
            self.gain.show();
        } else {
            self.default_envelope.set_value(true);
            self.hide_adsr();
            self.gain.hide();
        }
    }

    fn get_envelope(&self) -> Result<Option<Envelope>, ValueError> {
        if self.adsr_envelope.is_toggled() {
            let adsr = Adsr::try_new(
                self.adsr_a.value() as u8,
                self.adsr_d.value() as u8,
                self.adsr_sl.value() as u8,
                self.adsr_sr.value() as u8,
            )?;
            Ok(Some(Envelope::Adsr(adsr)))
        } else if self.gain_envelope.is_toggled() {
            let gain = Gain::new(self.gain.value() as u8);
            Ok(Some(Envelope::Gain(gain)))
        } else {
            Ok(None)
        }
    }

    fn on_key_pressed(&self, pitch: PitchChar) -> Result<(), ValueError> {
        if let Some(id) = self.selected_id {
            let envelope = self.get_envelope()?;
            let octave = Octave::try_from(self.octave.value() as u32)?;
            let note = Note::from_pitch_and_octave(pitch, octave)?;

            self.sender.send(GuiMessage::PlaySample(
                id,
                PlaySampleArgs {
                    note,
                    note_length: self.note_length.value() as u32,
                    envelope,
                },
            ));
        }

        Ok(())
    }
}

pub struct SamplesTab {
    group: Flex,

    inst_table: ListEditorTable<InstrumentMapping>,

    instrument_editor: Rc<RefCell<InstrumentEditor>>,
    test_sample_widget: Rc<RefCell<TestSampleWidget>>,

    console: TextDisplay,
    console_buffer: TextBuffer,
}

impl Tab for SamplesTab {
    fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_mut(&mut self) -> &mut Flex {
        &mut self.group
    }

    fn file_type(&self) -> FileType {
        FileType::Project
    }
}

impl SamplesTab {
    pub fn new(
        instruments: &impl ListState<Item = data::Instrument>,
        sender: app::Sender<GuiMessage>,
    ) -> Self {
        let mut group = Flex::default_fill().row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut inst_table = ListEditorTable::new_with_data(instruments, sender.clone());

        let button_height = inst_table.button_height();
        sidebar.fixed(&inst_table.list_buttons().pack, button_height);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let (instrument_editor, ie_height) = InstrumentEditor::new(sender.clone());
        main_group.fixed(&instrument_editor.borrow().group, ie_height + group.pad());

        let test_sample_widget = {
            let mut ts_flex = Flex::default().row();
            Frame::default();
            let test_sample_widget = TestSampleWidget::new(sender);
            Frame::default();
            ts_flex.end();

            {
                let ts_group = &test_sample_widget.borrow().group;
                ts_flex.fixed(ts_group, ts_group.width());
                main_group.fixed(&ts_flex, ts_group.height());
            }

            test_sample_widget
        };

        let mut console = TextDisplay::default();
        main_group.fixed(&console, button_height * 4);

        main_group.end();
        group.end();

        let console_buffer = TextBuffer::default();
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        Self {
            group,
            inst_table,
            instrument_editor,
            test_sample_widget,
            console,
            console_buffer,
        }
    }

    pub fn set_combine_result(&mut self, r: &Result<usize, CombineSamplesError>) {
        match r {
            Ok(_) => self.group.set_label_color(Color::Foreground),
            Err(_) => self.group.set_label_color(Color::Red),
        }
        self.group.redraw_label();
    }
}

impl ListEditor<Instrument> for SamplesTab {
    fn list_buttons(&mut self) -> &mut ListButtons {
        self.inst_table.list_buttons()
    }

    fn list_edited(&mut self, action: &ListAction<Instrument>) {
        self.inst_table.list_edited(action);
        self.instrument_editor.borrow_mut().list_edited(action);

        if let ListAction::Edit(index, inst) = action {
            self.test_sample_widget
                .borrow_mut()
                .instrument_changed(*index, inst);
        }
    }

    fn clear_selected(&mut self) {
        self.inst_table.clear_selected();
        self.instrument_editor.borrow_mut().disable_editor();
        self.test_sample_widget.borrow_mut().clear_selected();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, inst: &Instrument) {
        self.inst_table.set_selected(index, id, inst);
        self.instrument_editor.borrow_mut().set_data(index, inst);
        self.test_sample_widget
            .borrow_mut()
            .set_selected(index, id, inst);
    }
}

impl CompilerOutputGui<InstrumentOutput> for SamplesTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<InstrumentOutput>) {
        self.inst_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<InstrumentOutput>) {
        self.test_sample_widget
            .borrow_mut()
            .set_active(matches!(compiler_output, Some(Ok(_))));

        match compiler_output {
            None => self.console_buffer.set_text(""),
            Some(Ok(o)) => {
                self.console_buffer
                    .set_text(&format!("BRR Sample size: {} bytes", o));
                self.console.set_text_color(Color::Foreground);
            }
            Some(Err(errors)) => {
                let mut text = "ERROR:".to_owned();

                if let Some(e) = &errors.brr_error {
                    text += &format!("\n\t{}", e)
                }
                if let Some(e) = &errors.pitch_error {
                    text += &format!("\n\t{}", e)
                }

                self.console_buffer.set_text(&text);
                self.console.set_text_color(Color::Red);
                self.console.scroll(0, 0);
            }
        }
    }
}

const fn can_use_loop_setting(l: LoopChoice, sft: &SourceFileType) -> bool {
    match l {
        LoopChoice::None => !matches!(sft, SourceFileType::Unknown),
        LoopChoice::OverrideBrrLoopPoint => matches!(sft, SourceFileType::Brr),
        LoopChoice::LoopWithFilter => matches!(sft, SourceFileType::Wav),
        LoopChoice::LoopResetFilter => matches!(sft, SourceFileType::Wav),
        LoopChoice::DupeBlockHack => matches!(sft, SourceFileType::Wav),
    }
}
