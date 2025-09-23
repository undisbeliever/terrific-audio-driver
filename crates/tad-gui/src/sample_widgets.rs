//! Loop Point widget

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::{is_input_done_event, InputForm, InputHelper};

use compiler::data::{self, BlockNumber, BrrEvaluator, LoopSetting, SampleNumber};
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
pub enum LoopTypeChoice {
    None = 0,
    OverrideBrrLoopPoint = 1,
    Loop = 2,
    DupeBlockHack = 3,
}
impl LoopTypeChoice {
    pub const CHOICES: &'static str = concat![
        "&None",
        "|&Override BRR Loop Point",
        "|&Loop",
        "|&Dupe Block Hack"
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::None,
            1 => Self::OverrideBrrLoopPoint,
            2 => Self::Loop,
            3 => Self::DupeBlockHack,

            _ => Self::None,
        }
    }

    pub const fn to_i32(self) -> i32 {
        self as i32
    }
}

const fn can_use_loop_setting(l: LoopTypeChoice, sft: &SourceFileType) -> bool {
    match l {
        LoopTypeChoice::None => !matches!(sft, SourceFileType::Unknown),
        LoopTypeChoice::OverrideBrrLoopPoint => matches!(sft, SourceFileType::Brr),
        LoopTypeChoice::Loop => matches!(sft, SourceFileType::Wav),
        LoopTypeChoice::DupeBlockHack => matches!(sft, SourceFileType::Wav),
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum LoopFilterChoice {
    ResetFilter = 0,
    Auto = 1,
    Filter1 = 2,
    Filter2 = 3,
    Filter3 = 4,
}
impl LoopFilterChoice {
    pub const CHOICES: &'static str = concat![
        "&Reset filter",
        "|&Auto",
        "|&BRR Filter &1",
        "|&BRR Filter &2",
        "|&BRR Filter &3",
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::ResetFilter,
            1 => Self::Auto,
            2 => Self::Filter1,
            3 => Self::Filter2,
            4 => Self::Filter3,

            _ => Self::Auto,
        }
    }

    pub const fn to_i32(self) -> i32 {
        self as i32
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

#[derive(Clone, Copy, PartialEq)]
pub enum BrrEvaluatorChoice {
    Default = 0,
    SquaredError = 1,
    SquaredErrorAvoidGaussianOverflow = 2,
}
impl BrrEvaluatorChoice {
    const _DEFAULT_MATCHES: () = assert!(matches!(
        brr::DEFAULT_EVALUATOR,
        brr::Evaluator::SquaredErrorAvoidGaussianOverflow
    ));

    pub const CHOICES: &'static str = concat![
        "&Default (avoid Gaussian overflow)",
        "|&Squared Error",
        "|&Avoid Gaussian Overflow (Squared Error)",
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::Default,
            1 => Self::SquaredError,
            2 => Self::SquaredErrorAvoidGaussianOverflow,

            _ => Self::Default,
        }
    }

    pub const fn to_i32(self) -> i32 {
        self as i32
    }

    pub fn to_data(self) -> BrrEvaluator {
        match self {
            Self::Default => BrrEvaluator::Default,
            Self::SquaredError => BrrEvaluator::SquaredError,
            Self::SquaredErrorAvoidGaussianOverflow => {
                BrrEvaluator::SquaredErrorAvoidGaussianOverflow
            }
        }
    }

    pub fn from_data(e: BrrEvaluator) -> Self {
        match e {
            BrrEvaluator::Default => Self::Default,
            BrrEvaluator::SquaredError => Self::SquaredError,
            BrrEvaluator::SquaredErrorAvoidGaussianOverflow => {
                Self::SquaredErrorAvoidGaussianOverflow
            }
        }
    }
}

pub struct BrrSettingsWidget {
    source_file_type: SourceFileType,
    loop_type: Choice,
    loop_filter: Choice,
    loop_argument: IntInput,
    evaluator: Choice,
}

impl BrrSettingsWidget {
    pub fn new(form: &mut InputForm) -> Self {
        let loop_settings = form.add_three_inputs::<Choice, Choice, IntInput>("Loop:", 25, 15);
        let (mut loop_type, mut loop_filter, loop_argument) = loop_settings;

        loop_type.set_tooltip("Loop type");
        loop_type.add_choice(LoopTypeChoice::CHOICES);

        loop_filter.set_tooltip("BRR Filter at loop point");
        loop_filter.add_choice(LoopFilterChoice::CHOICES);

        let mut evaluator = form.add_input::<Choice>("Evaluator:");
        evaluator.set_tooltip("wav2brr evaluator to find the best BRR filters and nibbles");
        evaluator.add_choice(BrrEvaluatorChoice::CHOICES);

        Self {
            source_file_type: SourceFileType::Unknown,
            loop_type,
            loop_filter,
            loop_argument,
            evaluator,
        }
    }

    pub fn set_editor<E: SampleWidgetEditor + 'static>(&mut self, editor: Rc<RefCell<E>>) {
        self.loop_type.set_callback({
            let mut filter = self.loop_filter.clone();
            let mut loop_argument = self.loop_argument.clone();
            let editor = editor.clone();
            move |choice| {
                let mut e = editor.borrow_mut();
                Self::on_loop_type_changed(
                    choice,
                    &mut filter,
                    &mut loop_argument,
                    e.loop_settings(),
                );
                e.on_finished_editing();
            }
        });

        let choice_cb = {
            let editor = editor.clone();
            move |_: &mut Choice| {
                editor.borrow_mut().on_finished_editing();
            }
        };

        self.loop_filter.set_callback(choice_cb.clone());
        self.evaluator.set_callback(choice_cb);

        self.loop_argument.handle({
            move |_, ev| {
                if is_input_done_event(ev) {
                    editor.borrow_mut().on_finished_editing();
                }
                false
            }
        });
    }

    pub fn read_or_reset(&mut self, ls: &LoopSetting) -> (Option<LoopSetting>, BrrEvaluator) {
        type LT = LoopTypeChoice;
        type LF = LoopFilterChoice;

        let loop_type = LoopTypeChoice::read_widget(&self.loop_type);
        let loop_filter = LoopFilterChoice::read_widget(&self.loop_filter);

        let arg = self.loop_argument.value().parse().ok();

        let loop_setting = match (loop_type, loop_filter, arg) {
            (LT::None, _, _) => Some(LoopSetting::None),
            (LT::OverrideBrrLoopPoint, _, Some(i)) => {
                Some(LoopSetting::OverrideBrrLoopPoint(SampleNumber(i)))
            }

            (LT::Loop, LF::ResetFilter, Some(i)) => {
                Some(LoopSetting::LoopResetFilter(SampleNumber(i)))
            }
            (LT::Loop, LF::Auto, Some(i)) => Some(LoopSetting::LoopWithFilter(SampleNumber(i))),
            (LT::Loop, LF::Filter1, Some(i)) => Some(LoopSetting::LoopFilter1(SampleNumber(i))),
            (LT::Loop, LF::Filter2, Some(i)) => Some(LoopSetting::LoopFilter2(SampleNumber(i))),
            (LT::Loop, LF::Filter3, Some(i)) => Some(LoopSetting::LoopFilter3(SampleNumber(i))),

            (LT::DupeBlockHack, LF::ResetFilter, _) => None,
            (LT::DupeBlockHack, LF::Auto, Some(i)) => {
                Some(LoopSetting::DupeBlockHack(BlockNumber(i)))
            }
            (LT::DupeBlockHack, LF::Filter1, Some(i)) => {
                Some(LoopSetting::DupeBlockHackFilter1(BlockNumber(i)))
            }
            (LT::DupeBlockHack, LF::Filter2, Some(i)) => {
                Some(LoopSetting::DupeBlockHackFilter2(BlockNumber(i)))
            }
            (LT::DupeBlockHack, LF::Filter3, Some(i)) => {
                Some(LoopSetting::DupeBlockHackFilter3(BlockNumber(i)))
            }

            (_, _, None) => None,
        };

        if loop_setting.is_none() {
            self.set_loop_setting_value(ls);
        }

        let evaluator = BrrEvaluatorChoice::read_widget(&self.evaluator).to_data();

        (loop_setting, evaluator)
    }

    fn on_loop_type_changed(
        choice: &Choice,
        filter: &mut Choice,
        argument: &mut IntInput,
        ls: &LoopSetting,
    ) {
        let choice = LoopTypeChoice::read_widget(choice);
        match choice {
            LoopTypeChoice::None => {
                argument.set_value("");
                argument.deactivate();
            }
            LoopTypeChoice::OverrideBrrLoopPoint | LoopTypeChoice::Loop => {
                if !ls.samples_argument() {
                    argument.set_value("0");
                    argument.activate();
                }
            }
            LoopTypeChoice::DupeBlockHack => {
                if !ls.is_dupe_block_hack() {
                    argument.set_value("2");
                    argument.activate();
                }
                if ls == &LoopSetting::None {
                    filter.set_value(LoopFilterChoice::Auto.to_i32());
                }
            }
        }
        Self::update_loop_filter_choice(choice, filter);
    }

    fn update_loop_filter_choice(choice: LoopTypeChoice, loop_filter: &mut Choice) {
        match choice {
            LoopTypeChoice::None | LoopTypeChoice::OverrideBrrLoopPoint => {
                loop_filter.set_value(-1);
                loop_filter.deactivate();
            }
            LoopTypeChoice::Loop => {
                loop_filter
                    .at(LoopFilterChoice::ResetFilter.to_i32())
                    .unwrap()
                    .activate();
                if loop_filter.value() < 0 {
                    loop_filter.set_value(LoopFilterChoice::ResetFilter.to_i32());
                }
                loop_filter.activate();
            }
            LoopTypeChoice::DupeBlockHack => {
                loop_filter
                    .at(LoopFilterChoice::ResetFilter.to_i32())
                    .unwrap()
                    .deactivate();

                let lfc = loop_filter.value();
                if lfc < 0 || lfc <= LoopFilterChoice::ResetFilter.to_i32() {
                    loop_filter.set_value(LoopFilterChoice::Auto.to_i32());
                }
                loop_filter.activate();
            }
        }
    }

    pub fn clear_value(&mut self) {
        self.loop_type.set_value(-1);
        self.loop_argument.set_value("");
        self.evaluator.set_value(-1);
    }

    pub fn set_loop_setting_value(&mut self, ls: &LoopSetting) {
        type LS = LoopSetting;
        type LT = LoopTypeChoice;
        type LF = LoopFilterChoice;

        let (lc, lf, arg) = match ls {
            LS::None => (LT::None, None, None),
            LS::OverrideBrrLoopPoint(sn) => (LT::OverrideBrrLoopPoint, None, Some(sn.0)),
            LS::LoopResetFilter(sn) => (LT::Loop, Some(LF::ResetFilter), Some(sn.0)),
            LS::LoopWithFilter(sn) => (LT::Loop, Some(LF::Auto), Some(sn.0)),
            LS::LoopFilter1(sn) => (LT::Loop, Some(LF::Filter1), Some(sn.0)),
            LS::LoopFilter2(sn) => (LT::Loop, Some(LF::Filter2), Some(sn.0)),
            LS::LoopFilter3(sn) => (LT::Loop, Some(LF::Filter3), Some(sn.0)),
            LS::DupeBlockHack(bn) => (LT::DupeBlockHack, Some(LF::Auto), Some(bn.0)),
            LS::DupeBlockHackFilter1(bn) => (LT::DupeBlockHack, Some(LF::Filter1), Some(bn.0)),
            LS::DupeBlockHackFilter2(bn) => (LT::DupeBlockHack, Some(LF::Filter2), Some(bn.0)),
            LS::DupeBlockHackFilter3(bn) => (LT::DupeBlockHack, Some(LF::Filter3), Some(bn.0)),
        };
        self.loop_type.set_value(lc.to_i32());

        Self::update_loop_filter_choice(lc, &mut self.loop_filter);

        match lf {
            Some(fc) => {
                self.loop_filter.set_value(fc.to_i32());
                self.loop_filter.activate();
            }
            None => {
                self.loop_filter.set_value(-1);
                self.loop_filter.deactivate();
            }
        }

        match arg {
            Some(v) => {
                self.loop_argument.set_value(&v.to_string());
                self.loop_argument.activate();
            }
            None => {
                self.loop_argument.set_value("");
                self.loop_argument.deactivate();
            }
        }
    }

    pub fn set_value(&mut self, ls: &LoopSetting, e: data::BrrEvaluator) {
        self.set_loop_setting_value(ls);

        self.evaluator
            .set_value(BrrEvaluatorChoice::from_data(e).to_i32());
    }

    pub fn update_loop_type_choice(&mut self, sft: SourceFileType) {
        macro_rules! update_choices {
            ($($choice:ident),*) => {
                $(
                    let can_use = can_use_loop_setting(LoopTypeChoice::$choice, &sft);

                    if let Some(mut m) = self.loop_type.at(LoopTypeChoice::$choice.to_i32()) {
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
            update_choices!(None, OverrideBrrLoopPoint, Loop, DupeBlockHack);

            match sft {
                SourceFileType::Unknown | SourceFileType::Brr => self.evaluator.deactivate(),
                SourceFileType::Wav => self.evaluator.activate(),
            }

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
