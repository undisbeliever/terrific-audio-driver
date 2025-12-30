//! Loop Point widget

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::{is_input_done_event, InputForm};

use compiler::data::{self, BlockNumber, BrrEvaluator, LoopSetting, SampleNumber};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::path::SourcePathBuf;
use compiler::samples::{BRR_EXTENSION, WAV_EXTENSION};
use fltk::button::RadioRoundButton;

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

pub struct SampleEnvelopeWidget {
    adsr_button: RadioRoundButton,
    adsr_input: Input,
    prev_adsr_value: String,
    prev_adsr: Adsr,

    gain_button: RadioRoundButton,
    gain_input: Input,
    prev_gain_value: String,
    prev_gain: Gain,
}

impl SampleEnvelopeWidget {
    pub fn new(form: &mut InputForm) -> Self {
        let nrg = form.add_group("Envelope:", 1);

        let pad = nrg.pad();
        let w = nrg.ch_width(12);
        let w2 = nrg.ch_width(2);
        let h = nrg.row_height();

        let x = nrg.left_column_width() + pad;

        let adsr_button = RadioRoundButton::new(x, 0, w, h, Some("ADSR"));
        let x = x + pad + w;
        let adsr_input = Input::new(x, 0, w, h, None);
        let x = x + pad + w + w2;

        let gain_button = RadioRoundButton::new(x, 0, w, h, Some("GAIN"));
        let x = x + pad + w;
        let gain_input = Input::new(x, 0, w, h, None);

        nrg.end();

        Self {
            adsr_button,
            adsr_input,
            prev_adsr: DEFAULT_ADSR,
            prev_adsr_value: DEFAULT_ADSR.to_gui_string(),

            gain_button,
            gain_input,
            prev_gain: DEFAULT_GAIN,
            prev_gain_value: DEFAULT_GAIN.to_gui_string(),
        }
    }

    pub fn set_editor<E: SampleWidgetEditor + 'static>(&mut self, editor: Rc<RefCell<E>>) {
        self.adsr_button.set_callback({
            let mut adsr_input = self.adsr_input.clone();
            let mut gain_input = self.gain_input.clone();
            let editor = editor.clone();
            move |_| {
                gain_input.deactivate();
                adsr_input.activate();
                editor.borrow_mut().on_finished_editing();
            }
        });

        self.gain_button.set_callback({
            let mut adsr_input = self.adsr_input.clone();
            let mut gain_input = self.gain_input.clone();
            let editor = editor.clone();
            move |_| {
                adsr_input.deactivate();
                gain_input.activate();
                editor.borrow_mut().on_finished_editing();
            }
        });

        self.adsr_input.handle({
            let editor = editor.clone();
            move |_, ev| {
                if is_input_done_event(ev) {
                    editor.borrow_mut().on_finished_editing();
                }
                false
            }
        });

        self.gain_input.handle({
            let editor = editor;
            move |_, ev| {
                if is_input_done_event(ev) {
                    editor.borrow_mut().on_finished_editing();
                }
                false
            }
        });
    }

    // Always returns an Envelope.
    // I need to set the envelope if the envelope type changes.
    pub fn read_or_reset(&mut self) -> Envelope {
        if self.adsr_button.is_toggled() {
            let value = self.adsr_input.value();
            match value.parse() {
                Ok(adsr) => {
                    self.prev_adsr = adsr;
                    self.prev_adsr_value = value;
                    Envelope::Adsr(adsr)
                }
                Err(_) => {
                    self.adsr_input.set_value(&self.prev_adsr_value);
                    Envelope::Adsr(self.prev_adsr)
                }
            }
        } else {
            let value = self.gain_input.value();
            match value.parse() {
                Ok(gain) => {
                    self.prev_gain = gain;
                    self.prev_gain_value = value;
                    Envelope::Gain(gain)
                }
                Err(_) => {
                    self.gain_input.set_value(&self.prev_gain_value);
                    Envelope::Gain(self.prev_gain)
                }
            }
        }
    }

    pub fn clear_value(&mut self) {
        self.adsr_input.set_value("");
        self.gain_input.set_value("");
    }

    pub fn set_value(&mut self, envelope: Envelope) {
        match envelope {
            Envelope::Adsr(adsr) => {
                self.prev_adsr = adsr;
                self.prev_adsr_value = adsr.to_gui_string();

                self.adsr_button.toggle(true);
                self.adsr_input.set_value(&self.prev_adsr_value);
                self.adsr_input.activate();

                self.gain_button.toggle(false);
                self.gain_input.set_value("");
                self.gain_input.deactivate();
            }
            Envelope::Gain(gain) => {
                self.prev_gain = gain;
                self.prev_gain_value = gain.to_gui_string();

                self.gain_button.toggle(true);
                self.gain_input.set_value(&self.prev_gain_value);
                self.gain_input.activate();

                self.adsr_button.toggle(false);
                self.adsr_input.set_value("");
                self.adsr_input.deactivate();
            }
        }
    }
}
