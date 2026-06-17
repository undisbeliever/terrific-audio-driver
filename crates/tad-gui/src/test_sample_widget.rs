//! Sample test widgets (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, PlaySampleArgs, SampleOutput};
use crate::helpers::*;
use crate::GuiMessage;

use compiler::envelope::{Adsr, Envelope, Gain, GainMode};
use compiler::errors::ValueError;
use compiler::notes::{Note, Octave, PitchSemitoneIndex};
use compiler::project::{BrrSample, BrrSamplePitches};

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::button::Button;
use fltk::button::RadioRoundButton;
use fltk::enums::{Align, Color};
use fltk::group::{Flex, Group, Wizard};
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

struct TestInstrumentWidget {
    selected_id: Option<ItemId>,

    sender: app::Sender<GuiMessage>,

    group: Group,

    octave: Spinner,
    note_length: Spinner,
    envelope: EnvelopeWidget,
}

impl TestInstrumentWidget {
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
        let mut group = Group::default().size_of_parent();
        group.make_resizable(false);

        let line_height = ch_units_to_width(&group, 3);

        let widget_width = ch_units_to_width(&group, 66);
        let widget_height = line_height * 6;

        group.set_size(widget_width, widget_height);

        let key_width = ch_units_to_width(&group, 5);
        let key_height = line_height * 5 / 2;
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

        let octave = spinner(
            1,
            2,
            0,
            "Octave",
            "",
            Octave::MIN.as_u8(),
            Octave::MAX.as_u8(),
            4,
        );
        let mut note_length = spinner(1, 2, 1, "Note Length", "", 2, 255, u8::MAX);
        note_length.set_maximum(1000.0);

        let envelope = EnvelopeWidget::new(options_x, line_height * 3, options_width);

        options_group.end();

        group.end();

        let out = Rc::from(RefCell::new(Self {
            selected_id: None,
            sender,
            group,

            octave,
            note_length,
            envelope,
        }));

        {
            let mut widget = out.borrow_mut();

            widget.clear_selected();
        }

        for (i, button) in key_buttons.iter_mut().enumerate() {
            button.set_callback({
                let state = out.clone();
                let i = u8::try_from(i).unwrap();
                let pitch = PitchSemitoneIndex::try_from(i).unwrap();
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
        self.selected_id = None;
        self.group.deactivate();
    }

    fn set_selected(&mut self, id: ItemId) {
        self.selected_id = Some(id);
        self.group.activate();
    }

    fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    fn on_key_pressed(&self, pitch: PitchSemitoneIndex) -> Result<(), ValueError> {
        if let Some(id) = self.selected_id {
            let envelope = self.envelope.get_envelope()?;
            let octave = Octave::try_from(self.octave.value() as u32)?;
            let note = Note::from_pitch_and_octave(pitch, octave)?;

            self.sender.send(GuiMessage::PlayInstrument(
                id,
                PlaySampleArgs {
                    note,
                    note_length: self.note_length.value() as u16,
                    envelope,
                },
            ));
        }

        Ok(())
    }
}

struct TestSampleWidget {
    selected_id: Option<ItemId>,

    sender: app::Sender<GuiMessage>,

    group: Group,

    buttons: [Button; 12],

    note_length: Spinner,
    envelope: EnvelopeWidget,
}

impl TestSampleWidget {
    fn new(sender: app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut group = Group::default();
        group.make_resizable(false);

        let button_size = ch_units_to_width(&group, 5);

        let options_x = 6 * button_size;
        let options_y = 0;
        let options_width = ch_units_to_width(&group, 30);
        let line_height = ch_units_to_width(&group, 3);

        group.set_size(options_x + options_width, button_size * 4);

        let buttons = std::array::from_fn(|i| {
            let i = i32::try_from(i).unwrap();
            let x = (i % 4 + 1) * button_size;
            let y = (i / 4) * button_size;

            let mut b = Button::new(x, y, button_size, button_size, None);
            b.set_label(&format!("{}", i));

            b
        });

        let mut note_length = Spinner::new(
            options_x + options_width / 2,
            options_y,
            options_width / 2,
            line_height,
            "Note Length",
        );
        note_length.set_range(2.0, 1000.0);
        note_length.set_value(125.0); // 1 second
        note_length.set_step(1.0);

        let envelope = EnvelopeWidget::new(options_x, options_y + line_height * 2, options_width);

        let out = Rc::from(RefCell::new(Self {
            selected_id: None,
            sender,
            group,
            note_length,
            buttons,
            envelope,
        }));

        for (i, b) in out.borrow_mut().buttons.iter_mut().enumerate() {
            b.set_callback({
                let widget = out.clone();
                let note = Note::from_note_id_usize(i).unwrap();
                move |_| {
                    if let Ok(w) = widget.try_borrow() {
                        w.send_play_sample_message(note);
                    }
                }
            })
        }

        out
    }

    fn clear_selected(&mut self) {
        self.selected_id = None;
        self.group.deactivate();
    }

    fn set_selected(&mut self, id: ItemId, n_sample_rates: usize) {
        self.selected_id = Some(id);
        self.group.activate();
        self.update_buttons(n_sample_rates);
    }

    fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    fn update_buttons(&mut self, n_sample_rates: usize) {
        for (i, b) in self.buttons.iter_mut().enumerate() {
            b.set_active(i < n_sample_rates);
        }
    }

    fn send_play_sample_message(&self, note: Note) {
        if let Some(id) = self.selected_id {
            if let Ok(envelope) = self.envelope.get_envelope() {
                self.sender.send(GuiMessage::PlaySample(
                    id,
                    PlaySampleArgs {
                        note,
                        note_length: self.note_length.value() as u16,
                        envelope,
                    },
                ))
            }
        }
    }
}

pub struct TestBrrSampleWidget {
    wizard: Wizard,
    notes: Rc<RefCell<TestInstrumentWidget>>,
    sample_rates: Rc<RefCell<TestSampleWidget>>,
}

impl TestBrrSampleWidget {
    // Must be placed inside Pack or Flex for the widget to be in the right place
    pub fn new(parent: &mut Flex, sender: app::Sender<GuiMessage>) -> Self {
        let width = ch_units_to_width(parent, 50);
        let height = ch_units_to_width(parent, 5 * 3);

        let mut wizard = Wizard::new(0, 0, width, height, None);
        wizard.set_frame(fltk::enums::FrameType::NoBox);

        let notes = TestInstrumentWidget::new(sender);
        let sample_rates = TestSampleWidget::new(sender);

        wizard.end();

        parent.fixed(&wizard, height);

        Self {
            wizard,
            notes,
            sample_rates,
        }
    }

    pub fn clear_selected(&mut self) {
        self.notes.borrow_mut().clear_selected();
        self.sample_rates.borrow_mut().clear_selected();
        self.wizard.deactivate();
    }

    pub fn item_edited(&mut self, id: ItemId, data: &BrrSample) {
        match &data.pitches {
            Some(BrrSamplePitches::Octaves { .. }) | Some(BrrSamplePitches::Notes { .. }) => {
                let mut w = self.notes.borrow_mut();
                w.set_selected(id);
                self.wizard.set_current_widget(&w.group);
                self.wizard.activate();
            }
            Some(BrrSamplePitches::SampleRates { sample_rates }) => {
                let mut w = self.sample_rates.borrow_mut();
                w.set_selected(id, sample_rates.len());
                self.wizard.set_current_widget(&w.group);
                self.wizard.activate();
            }
            None => {
                let w = self.notes.borrow();
                self.wizard.set_current_widget(&w.group);
                self.wizard.deactivate();
            }
        }
    }

    pub fn compiler_output_changed(&mut self, compiler_output: Option<&SampleOutput>) {
        let a = compiler_output.is_some_and(|co| co.is_ok());

        self.notes.borrow_mut().set_active(a);
        self.sample_rates.borrow_mut().set_active(a);
    }
}
