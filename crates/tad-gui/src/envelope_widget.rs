//! Envelope widget

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::*;

use compiler::envelope::{Adsr, Envelope, Gain, GainMode};

use std::cell::RefCell;
use std::rc::Rc;

use compiler::errors::ValueError;
use fltk::button::RadioRoundButton;
use fltk::enums::Align;
use fltk::group::Group;
use fltk::menu::Choice;
use fltk::misc::Spinner;
use fltk::prelude::*;

// Only using the gain modes that output audio on key-on.
const GAIN_MODE_CHOICES: &str = concat!["Fixed", "|Linear Increase", "|Bent Increase"];
const GAIN_MODE_DEFAULT_VALUE: i32 = 0;

pub fn read_gain_mode_choice(c: &Choice) -> GainMode {
    match c.value() {
        0 => GainMode::Fixed,
        1 => GainMode::LinearIncrease,
        2 => GainMode::BentIncrease,
        _ => GainMode::Fixed,
    }
}

struct EnvelopeWidgetState {
    default_envelope: RadioRoundButton,
    adsr_envelope: RadioRoundButton,
    gain_envelope: RadioRoundButton,

    adsr_a: Spinner,
    adsr_d: Spinner,
    adsr_sl: Spinner,
    adsr_sr: Spinner,

    gain_mode: Choice,
    gain: Spinner,
}

pub struct EnvelopeWidget {
    #[allow(dead_code)]
    group: Group,
    state: Rc<RefCell<EnvelopeWidgetState>>,
}

impl EnvelopeWidget {
    pub fn new(x: i32, y: i32, widget_width: i32) -> Self {
        let mut group = Group::new(x, y, widget_width, 0, None);
        group.make_resizable(false);

        let line_height = ch_units_to_width(&group, 3);
        group.set_size(widget_width, line_height * 4);

        let pos = |row, n_cols, col, col_span| -> (i32, i32, i32, i32) {
            assert!(col < n_cols);

            let spacing = (widget_width - 2) / n_cols;
            let w = spacing * col_span - 2;
            let h = line_height;
            let x = x + spacing * col + 2;
            let y = y + row * h;

            (x, y, w, h)
        };

        let radio = |row, n_cols, col, label: &'static str| {
            let (x, y, w, h) = pos(row, n_cols, col, 1);
            RadioRoundButton::new(x, y, w, h, Some(label))
        };

        let spinner =
            |row, n_cols, col, label: &'static str, tooltip: &str, min: u8, max: u8, value: u8| {
                let (x, y, w, h) = pos(row, n_cols, col, 1);
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
        let choice = |row, n_cols, col, col_span, label: &'static str| {
            let (x, y, w, h) = pos(row, n_cols, col, col_span);
            let mut c = Choice::new(x, y, w, h, label);
            c.set_align(Align::Top);
            c
        };

        let default_envelope = radio(0, 3, 0, "Default");
        let adsr_envelope = radio(0, 3, 1, "ADSR");
        let gain_envelope = radio(0, 3, 2, "GAIN");

        let adsr_a = spinner(2, 4, 0, "A", "Attack", 0, Adsr::ATTACK_MAX, 12);
        let adsr_d = spinner(2, 4, 1, "D", "Decay", 0, Adsr::DECAY_MAX, 2);
        #[rustfmt::skip]
        let adsr_sl = spinner(2, 4, 2, "SL", "Sustain Level", 0, Adsr::SUSTAIN_LEVEL_MAX, 2);
        let adsr_sr = spinner(2, 4, 3, "SR", "Sustain Rate", 0, Adsr::SUSTAIN_RATE_MAX, 16);

        let gain_mode = choice(2, 4, 0, 3, "GAIN Mode");
        let gain = spinner(2, 4, 3, "GAIN", "", 0, 127, 127);

        group.end();

        let state = Rc::from(RefCell::new(EnvelopeWidgetState {
            default_envelope,
            adsr_envelope,
            gain_envelope,

            adsr_a,
            adsr_d,
            adsr_sl,
            adsr_sr,

            gain_mode,
            gain,
        }));

        {
            let mut s = state.borrow_mut();

            let set_envelope_callback = |w: &mut RadioRoundButton| {
                w.set_callback({
                    let state = state.clone();
                    move |_w| {
                        if let Ok(mut s) = state.try_borrow_mut() {
                            s.on_envelope_changed();
                        }
                    }
                });
            };
            set_envelope_callback(&mut s.default_envelope);
            set_envelope_callback(&mut s.adsr_envelope);
            set_envelope_callback(&mut s.gain_envelope);

            s.gain_mode.add_choice(GAIN_MODE_CHOICES);
            s.gain_mode.set_value(GAIN_MODE_DEFAULT_VALUE);

            s.gain_mode.set_callback({
                let state = state.clone();
                move |_| {
                    if let Ok(mut s) = state.try_borrow_mut() {
                        s.on_gain_mode_changed()
                    }
                }
            });

            s.on_envelope_changed();
            s.on_gain_mode_changed();
        }

        Self { group, state }
    }

    pub fn get_envelope(&self) -> Result<Option<Envelope>, ValueError> {
        self.state.borrow().get_envelope()
    }
}

impl EnvelopeWidgetState {
    fn hide_adsr(&mut self) {
        self.adsr_a.hide();
        self.adsr_d.hide();
        self.adsr_sl.hide();
        self.adsr_sr.hide();
    }

    fn hide_gain(&mut self) {
        self.gain_mode.hide();
        self.gain.hide();
    }

    fn on_envelope_changed(&mut self) {
        if self.adsr_envelope.is_toggled() {
            self.adsr_a.show();
            self.adsr_d.show();
            self.adsr_sl.show();
            self.adsr_sr.show();

            self.hide_gain();
        } else if self.gain_envelope.is_toggled() {
            self.gain_mode.show();
            self.gain.show();

            self.hide_adsr();
        } else {
            self.default_envelope.set_value(true);
            self.hide_adsr();
            self.hide_gain();
        }
    }

    fn on_gain_mode_changed(&mut self) {
        let max = read_gain_mode_choice(&self.gain_mode).max_value();
        let old_max = self.gain.maximum() as u32;

        if old_max != u32::from(max) {
            let old_value = self.gain.value();
            let new_value = old_value / f64::from(old_max) * f64::from(max);

            let max = max.into();
            self.gain.set_maximum(max);
            self.gain.set_value(new_value.clamp(0.0, max).round());
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
            let mode = read_gain_mode_choice(&self.gain_mode);
            let gain = Gain::from_mode_and_value(mode, self.gain.value() as u32)?;

            Ok(Some(Envelope::Gain(gain)))
        } else {
            Ok(None)
        }
    }
}
