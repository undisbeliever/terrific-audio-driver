//! Loop Point widget

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::{is_input_done_event, InputForm, InputHelper};

use compiler::data::LoopSetting;
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::path::SourcePathBuf;
use compiler::samples::{BRR_EXTENSION, WAV_EXTENSION};

use std::cell::RefCell;
use std::rc::Rc;

use fltk::input::{Input, IntInput};
use fltk::menu::Choice;
use fltk::prelude::*;

#[derive(Debug, Eq, PartialEq)]
pub enum SourceFileType {
    Unknown,
    Wav,
    Brr,
}

impl SourceFileType {
    pub fn from_source(source: &SourcePathBuf) -> Self {
        match source.extension() {
            Some(WAV_EXTENSION) => SourceFileType::Wav,
            Some(BRR_EXTENSION) => SourceFileType::Brr,
            _ => SourceFileType::Unknown,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum LoopChoice {
    None = 0,
    OverrideBrrLoopPoint = 1,
    LoopWithFilter = 2,
    LoopResetFilter = 3,
    DupeBlockHack = 4,
}
impl LoopChoice {
    pub const CHOICES: &'static str = concat![
        "&None",
        "|&Override BRR Loop Point",
        "|&Loop With Filter",
        "|Loop &Resets Filter",
        "|&Dupe Block Hack"
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::None,
            1 => Self::OverrideBrrLoopPoint,
            2 => Self::LoopWithFilter,
            3 => Self::LoopResetFilter,
            4 => Self::DupeBlockHack,

            _ => Self::None,
        }
    }

    pub fn to_i32(self) -> i32 {
        self as i32
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

#[derive(Clone, Copy)]
enum EnvelopeChoice {
    Adsr = 0,
    Gain = 1,
}
impl EnvelopeChoice {
    pub const CHOICES: &'static str = "ADSR|GAIN";

    pub fn read_widget(c: &Choice) -> Option<EnvelopeChoice> {
        match c.value() {
            0 => Some(EnvelopeChoice::Adsr),
            1 => Some(EnvelopeChoice::Gain),
            _ => None,
        }
    }

    pub fn to_i32(self) -> i32 {
        self as i32
    }
}

pub const DEFAULT_ADSR: Adsr = match Adsr::try_new(12, 2, 2, 15) {
    Ok(a) => a,
    Err(_) => panic!("Invalid ADSR"),
};
pub const DEFAULT_GAIN: Gain = Gain::new(127);

pub const DEFAULT_ENVELOPE: Envelope = Envelope::Adsr(DEFAULT_ADSR);

pub trait SampleWidgetEditor {
    fn loop_settings(&self) -> &LoopSetting;

    fn on_finished_editing(&mut self);
}

pub struct LoopSettingWidget {
    source_file_type: SourceFileType,
    choice: Choice,
    argument: IntInput,
}

impl LoopSettingWidget {
    pub fn new(form: &mut InputForm) -> Self {
        let loop_settings = form.add_two_inputs::<Choice, IntInput>("Loop:", 25);
        let (mut loop_choice, loop_setting) = loop_settings;

        loop_choice.add_choice(LoopChoice::CHOICES);

        Self {
            source_file_type: SourceFileType::Unknown,
            choice: loop_choice,
            argument: loop_setting,
        }
    }

    pub fn set_editor<E: SampleWidgetEditor + 'static>(&mut self, editor: Rc<RefCell<E>>) {
        self.choice.set_callback({
            let mut argument = self.argument.clone();
            let editor = editor.clone();
            move |choice| {
                let mut e = editor.borrow_mut();
                Self::on_loop_choice_changed(choice, &mut argument, e.loop_settings());
                e.on_finished_editing();
            }
        });

        self.argument.handle({
            move |_, ev| {
                if is_input_done_event(ev) {
                    editor.borrow_mut().on_finished_editing();
                }
                false
            }
        });
    }

    pub fn read_or_reset(&mut self, ls: &LoopSetting) -> Option<LoopSetting> {
        let choice = LoopChoice::read_widget(&self.choice);
        let value = self.argument.value().parse().ok();

        let value = match choice {
            LoopChoice::None => Some(LoopSetting::None),
            LoopChoice::OverrideBrrLoopPoint => value.map(LoopSetting::OverrideBrrLoopPoint),
            LoopChoice::LoopWithFilter => value.map(LoopSetting::LoopWithFilter),
            LoopChoice::LoopResetFilter => value.map(LoopSetting::LoopResetFilter),
            LoopChoice::DupeBlockHack => value.map(LoopSetting::DupeBlockHack),
        };

        if value.is_none() {
            self.set_value(ls);
        }
        value
    }

    fn on_loop_choice_changed(choice: &Choice, w: &mut IntInput, ls: &LoopSetting) {
        let choice = LoopChoice::read_widget(choice);
        match choice {
            LoopChoice::None => {
                w.set_value("");
                w.deactivate();
            }
            LoopChoice::OverrideBrrLoopPoint
            | LoopChoice::LoopWithFilter
            | LoopChoice::LoopResetFilter => {
                if !ls.samples_argument() {
                    w.set_value("0");
                    w.activate();
                }
            }
            LoopChoice::DupeBlockHack => {
                if !ls.is_dupe_block_hack() {
                    w.set_value("2");
                    w.activate();
                }
            }
        }
    }

    pub fn clear_value(&mut self) {
        self.choice.set_value(-1);
        self.argument.set_value("");
    }

    pub fn set_value(&mut self, ls: &LoopSetting) {
        let (lc, lv) = match ls {
            LoopSetting::None => (LoopChoice::None, None),
            LoopSetting::OverrideBrrLoopPoint(lp) => (LoopChoice::OverrideBrrLoopPoint, Some(lp)),
            LoopSetting::LoopWithFilter(lp) => (LoopChoice::LoopWithFilter, Some(lp)),
            LoopSetting::LoopResetFilter(lp) => (LoopChoice::LoopResetFilter, Some(lp)),
            LoopSetting::DupeBlockHack(dbh) => (LoopChoice::DupeBlockHack, Some(dbh)),
        };
        self.choice.set_value(lc.to_i32());

        match lv {
            Some(v) => {
                self.argument.set_value(&v.to_string());
                self.argument.activate();
            }
            None => {
                self.argument.set_value("");
                self.argument.deactivate();
            }
        }
    }

    pub fn update_loop_choices(&mut self, sft: SourceFileType) {
        macro_rules! update_choices {
            ($($choice:ident),*) => {
                $(
                    let can_use = can_use_loop_setting(LoopChoice::$choice, &sft);

                    if let Some(mut m) = self.choice.at(LoopChoice::$choice.to_i32()) {
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

        if self.source_file_type != sft {
            update_choices!(
                None,
                OverrideBrrLoopPoint,
                LoopWithFilter,
                LoopResetFilter,
                DupeBlockHack
            );

            self.source_file_type = sft;
        }
    }
}

struct SampleEnvelopeWidgetState {
    prev_adsr: String,
    prev_gain: String,
}
pub struct SampleEnvelopeWidget {
    choice: Choice,
    argument: Input,

    state: Rc<RefCell<SampleEnvelopeWidgetState>>,
}

impl SampleEnvelopeWidget {
    pub fn new(form: &mut InputForm) -> Self {
        let (mut choice, argument) = form.add_two_inputs::<Choice, Input>("Envelope:", 12);

        choice.add_choice(EnvelopeChoice::CHOICES);

        Self {
            choice,
            argument,
            state: Rc::new(RefCell::new(SampleEnvelopeWidgetState {
                prev_adsr: DEFAULT_ADSR.to_gui_string(),
                prev_gain: DEFAULT_GAIN.to_gui_string(),
            })),
        }
    }

    pub fn set_editor<E: SampleWidgetEditor + 'static>(&mut self, editor: Rc<RefCell<E>>) {
        self.choice.set_callback({
            let mut argument = self.argument.clone();
            let state = self.state.clone();
            let editor = editor.clone();
            move |choice| {
                let mut e = editor.borrow_mut();
                Self::on_choice_changed(choice, &mut argument, &state.borrow());
                e.on_finished_editing();
            }
        });

        self.argument.handle({
            move |_, ev| {
                if is_input_done_event(ev) {
                    editor.borrow_mut().on_finished_editing();
                }
                false
            }
        });
    }

    pub fn read_or_reset(&mut self) -> Option<Envelope> {
        let value = self.argument.value();

        match EnvelopeChoice::read_widget(&self.choice) {
            Some(EnvelopeChoice::Adsr) => match InputHelper::parse(value.clone()) {
                Some(adsr) => {
                    self.state.borrow_mut().prev_adsr = value;
                    Some(Envelope::Adsr(adsr))
                }
                None => {
                    self.argument.set_value(&self.state.borrow().prev_adsr);
                    None
                }
            },
            Some(EnvelopeChoice::Gain) => match InputHelper::parse(value.clone()) {
                Some(gain) => {
                    self.state.borrow_mut().prev_gain = value;
                    Some(Envelope::Gain(gain))
                }
                None => {
                    self.argument.set_value(&self.state.borrow().prev_gain);
                    None
                }
            },
            None => None,
        }
    }

    pub fn clear_value(&mut self) {
        self.choice.set_value(-1);
        self.argument.set_value("");
    }

    pub fn set_value(&mut self, envelope: &Envelope) {
        match envelope {
            Envelope::Adsr(adsr) => {
                self.choice.set_value(EnvelopeChoice::Adsr.to_i32());
                InputHelper::set_widget_value(&mut self.argument, adsr);

                self.state.borrow_mut().prev_adsr = self.argument.value();
            }
            Envelope::Gain(gain) => {
                self.choice.set_value(EnvelopeChoice::Gain.to_i32());
                InputHelper::set_widget_value(&mut self.argument, gain);

                self.state.borrow_mut().prev_gain = self.argument.value();
            }
        }
    }

    fn on_choice_changed(choice: &Choice, argument: &mut Input, state: &SampleEnvelopeWidgetState) {
        let new_value = match EnvelopeChoice::read_widget(choice) {
            Some(EnvelopeChoice::Adsr) => &state.prev_adsr,
            Some(EnvelopeChoice::Gain) => &state.prev_adsr,
            None => "",
        };

        argument.set_value(new_value);

        // Select all
        let _ = argument.set_position(0);
        let _ = argument.set_mark(i32::MAX);

        let _ = argument.take_focus();
    }
}
