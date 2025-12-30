//! Instrument editor (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{InstrumentOutput, ItemId, PlaySampleArgs};
use crate::envelope_widget::EnvelopeWidget;
use crate::helpers::*;
use crate::list_editor::{ListMessage, TableCompilerOutput, TableMapping};
use crate::sample_widgets::{
    BrrSettingsWidget, SampleEnvelopeWidget, SampleWidgetEditor, SourceFileType, DEFAULT_ENVELOPE,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::GuiMessage;

use compiler::data::{self, Instrument, InstrumentNoteRange, LoopSetting};
use compiler::errors::ValueError;
use compiler::notes::{Note, Octave, PitchSemitoneIndex, STARTING_OCTAVE};
use compiler::path::SourcePathBuf;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::button::{Button, CheckButton, RadioRoundButton};
use fltk::enums::{Align, CallbackTrigger, Color, Event};
use fltk::group::{Flex, Group};
use fltk::input::{FloatInput, Input, IntInput};
use fltk::misc::Spinner;
use fltk::output::Output;
use fltk::prelude::*;

fn blank_instrument() -> Instrument {
    Instrument {
        name: "name".parse().unwrap(),
        source: SourcePathBuf::default(),
        freq: 500.0,
        loop_setting: LoopSetting::None,
        evaluator: Default::default(),
        ignore_gaussian_overflow: false,
        note_range: data::InstrumentNoteRange::Octave {
            first: STARTING_OCTAVE,
            last: STARTING_OCTAVE,
        },
        envelope: DEFAULT_ENVELOPE,
        comment: None,
    }
}

pub struct InstrumentMapping;

impl TableMapping for InstrumentMapping {
    type DataType = data::Instrument;
    type RowType = RowWithStatus<SimpleRow<1>>;

    const CAN_CLONE: bool = true;
    const CAN_EDIT: bool = false;

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

    fn user_changes_selection() -> Option<GuiMessage> {
        Some(GuiMessage::UserChangedSelectedInstrument)
    }
}

impl TableCompilerOutput for InstrumentMapping {
    type CompilerOutputType = InstrumentOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<InstrumentOutput>) -> bool {
        r.set_status_optional_result(co)
    }
}

pub struct InstrumentEditor {
    group: Flex,

    sender: app::Sender<GuiMessage>,

    selected_id: Option<ItemId>,
    data: Instrument,

    name: Input,
    source: Output,
    freq: FloatInput,
    wavelength: FloatInput,
    brr_settings: BrrSettingsWidget,
    ignore_gaussian_overflow: CheckButton,
    octave_range: RadioRoundButton,
    first_octave: IntInput,
    last_octave: IntInput,
    note_range: RadioRoundButton,
    first_note: Input,
    last_note: Input,
    envelope: SampleEnvelopeWidget,
    comment: Input,
}

impl InstrumentEditor {
    pub fn new(sender: app::Sender<GuiMessage>) -> (Rc<RefCell<InstrumentEditor>>, i32) {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);

        let mut freq_flex = form.flex_row("Frequency:");
        let freq = FloatInput::default();
        let w_label = label_packed("Hz   Wavelength: ");
        freq_flex.fixed(&w_label, w_label.w());
        let wavelength = FloatInput::default();
        let mut analyse_button = Button::default();
        freq_flex.fixed(&analyse_button, ch_units_to_width(&freq_flex, 5));
        freq_flex.end();

        let brr_settings = BrrSettingsWidget::new(&mut form);
        let ignore_gaussian_overflow = form.add_checkbox_right("Ignore Gaussian overflow");

        let nrg = form.add_group("Note range:", 2);
        let pad = nrg.pad();
        let w = nrg.ch_width(12);
        let h = nrg.row_height();
        let x1 = nrg.left_column_width() + pad;
        let x2 = x1 + pad + w;
        let x3 = x2 + pad + w + nrg.ch_width(2);

        let octave_range = RadioRoundButton::new(x1, 0, w, h, Some("Octaves"));
        let first_octave = IntInput::new(x2, 0, w, h, None);
        let last_octave = IntInput::new(x3, 0, w, h, Some("- "));

        let y = h + pad;
        let note_range = RadioRoundButton::new(x1, y, w, h, Some("Notes"));
        let first_note = Input::new(x2, y, w, h, None);
        let last_note = Input::new(x3, y, w, h, Some("- "));

        nrg.end();

        let envelope = SampleEnvelopeWidget::new(&mut form);
        let comment = form.add_input::<Input>("Comment:");

        let (group, form_height) = form.end();

        let (source, mut source_button) = source;

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_id: None,
            data: blank_instrument(),
            name,
            source,
            freq,
            wavelength,
            brr_settings,
            ignore_gaussian_overflow,
            octave_range,
            first_octave,
            last_octave,
            note_range,
            first_note,
            last_note,
            envelope,
            comment,
        }));

        {
            let mut editor = out.borrow_mut();

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
            add_callbacks!(wavelength);
            add_callbacks!(first_octave);
            add_callbacks!(last_octave);
            add_callbacks!(first_note);
            add_callbacks!(last_note);
            add_callbacks!(comment);

            editor.wavelength.set_trigger(CallbackTrigger::Changed);
            editor.wavelength.set_callback({
                let s = out.clone();
                move |_widget| {
                    s.borrow_mut().on_wavelength_changed();
                }
            });

            editor.brr_settings.set_editor(out.clone());
            editor.envelope.set_editor(out.clone());

            editor.ignore_gaussian_overflow.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().on_finished_editing()
            });

            let set_note_range_callback = |w: &mut RadioRoundButton| {
                w.set_callback({
                    let editor = out.clone();
                    move |_w| {
                        if let Ok(mut e) = editor.try_borrow_mut() {
                            e.on_finished_editing();
                        }
                    }
                })
            };
            set_note_range_callback(&mut editor.octave_range);
            set_note_range_callback(&mut editor.note_range);

            source_button.set_label("...");
            source_button.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().source_button_clicked()
            });

            analyse_button.set_label("...");
            analyse_button.set_tooltip("Analyse Sample");
            analyse_button.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().analyse_button_clicked()
            });
        }
        (out, form_height)
    }

    pub fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_event_handler(s: &Rc<RefCell<InstrumentEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
    }

    fn on_wavelength_changed(&mut self) {
        if self.selected_id.is_some() {
            if let Ok(w) = self.wavelength.value().parse() {
                self.data.set_wavelength(w);
                InputHelper::set_widget_value(&mut self.freq, &self.data.freq);
            }
        }
    }

    fn source_button_clicked(&mut self) {
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::OpenInstrumentSampleDialog(id));
        }
    }

    fn analyse_button_clicked(&mut self) {
        if let Some(id) = self.selected_id {
            self.sender
                .send(GuiMessage::OpenAnalyseInstrumentDialog(id));
        }
    }

    fn on_finished_editing(&mut self) {
        if let Some(new_data) = self.read_or_reset() {
            self.send_edit_message(new_data);
        }
    }

    fn send_edit_message(&self, data: Instrument) {
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::EditInstrument(id, data));
        }
    }

    fn read_or_reset(&mut self) -> Option<Instrument> {
        #[allow(clippy::question_mark)]
        if self.selected_id.is_none() {
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
        read_or_reset!(comment);

        let note_range = if self.octave_range.is_toggled() {
            let (first, last) = match old.note_range {
                InstrumentNoteRange::Octave { first, last } => (first, last),
                InstrumentNoteRange::Note { first, last } => (first.octave(), last.octave()),
            };

            let new_first = InputHelper::read_or_reset(&mut self.first_octave, &first);
            let new_last = InputHelper::read_or_reset(&mut self.last_octave, &last);

            match (new_first, new_last) {
                (Some(first), Some(last)) => {
                    Some(data::InstrumentNoteRange::Octave { first, last })
                }
                _ => None,
            }
        } else if self.note_range.is_toggled() {
            let (first, last) = match old.note_range {
                InstrumentNoteRange::Note { first, last } => (first, last),
                InstrumentNoteRange::Octave { first, last } => (
                    Note::first_note_for_octave(first),
                    Note::last_note_for_octave(last),
                ),
            };

            let new_first = InputHelper::read_or_reset(&mut self.first_note, &first);
            let new_last = InputHelper::read_or_reset(&mut self.last_note, &last);

            match (new_first, new_last) {
                (Some(first), Some(last)) => Some(data::InstrumentNoteRange::Note { first, last }),
                _ => None,
            }
        } else {
            None
        };

        let (loop_setting, evaluator) = self.brr_settings.read_or_reset(&self.data.loop_setting);
        let ignore_gaussian_overflow = self.ignore_gaussian_overflow.value();
        let envelope = self.envelope.read_or_reset();

        Some(Instrument {
            name: name?,
            freq: freq?,
            loop_setting: loop_setting?,
            evaluator,
            ignore_gaussian_overflow,
            note_range: note_range?,
            envelope,
            comment: comment?,

            // must be last (after the ?'s)
            source: self.data.source.clone(),
        })
    }

    pub fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.freq.set_value("");
        self.wavelength.set_value("");
        self.ignore_gaussian_overflow.clear();
        self.octave_range.set_value(false);
        self.first_octave.set_value("");
        self.last_octave.set_value("");
        self.note_range.set_value(false);
        self.first_note.set_value("");
        self.last_note.set_value("");
        self.brr_settings.clear_value();
        self.envelope.clear_value();

        self.selected_id = None;
    }

    fn set_data_update_widget(&mut self, data: &Instrument) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
        set_widget!(freq);

        // Silly hack to only show f64 precision if required
        let w = data.wavelength();
        if w.fract().abs() < 0.00001 {
            self.wavelength.set_value(&format!("{}", w as i64));
        } else {
            self.wavelength.set_value(&format!("{:.2}", w));
        }

        match data.note_range {
            data::InstrumentNoteRange::Octave { first, last } => {
                self.octave_range.set_value(true);
                self.note_range.set_value(false);

                self.first_octave.activate();
                InputHelper::set_widget_value(&mut self.first_octave, &first);
                self.last_octave.activate();
                InputHelper::set_widget_value(&mut self.last_octave, &last);

                self.first_note.deactivate();
                InputHelper::set_widget_value(
                    &mut self.first_note,
                    &Note::first_note_for_octave(first),
                );
                self.last_note.deactivate();
                InputHelper::set_widget_value(
                    &mut self.last_note,
                    &Note::last_note_for_octave(last),
                );
            }
            data::InstrumentNoteRange::Note { first, last } => {
                self.note_range.set_value(true);
                self.octave_range.set_value(false);

                self.first_note.activate();
                InputHelper::set_widget_value(&mut self.first_note, &first);
                self.last_note.activate();
                InputHelper::set_widget_value(&mut self.last_note, &last);

                self.first_octave.deactivate();
                InputHelper::set_widget_value(&mut self.first_octave, &first.octave());
                self.last_octave.deactivate();
                InputHelper::set_widget_value(&mut self.last_octave, &last.octave());
            }
        }
        set_widget!(comment);
        self.source.set_value(data.source.as_str());
        self.brr_settings
            .set_value(&data.loop_setting, data.evaluator);
        self.ignore_gaussian_overflow
            .set_value(data.ignore_gaussian_overflow);
        self.envelope.set_value(data.envelope);

        self.brr_settings
            .update_loop_type_choice(SourceFileType::from_source(&data.source));

        self.data = data.clone();

        self.group.activate();
    }

    pub fn set_selected(&mut self, id: ItemId, value: &Instrument) {
        self.selected_id = Some(id);
        self.set_data_update_widget(value);
    }

    pub fn selected_id(&self) -> Option<ItemId> {
        self.selected_id
    }

    pub fn item_edited(&mut self, id: ItemId, value: &Instrument) {
        if self.selected_id == Some(id) {
            self.set_data_update_widget(value);
        }
    }
}

impl SampleWidgetEditor for InstrumentEditor {
    fn on_finished_editing(&mut self) {
        if let Some(new_data) = self.read_or_reset() {
            self.data = new_data.clone();
            self.send_edit_message(new_data);
        }
    }

    fn loop_settings(&self) -> &LoopSetting {
        &self.data.loop_setting
    }
}

pub struct TestInstrumentWidget {
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

    pub fn new(sender: app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut group = Group::default();
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

    pub fn widget(&self) -> &Group {
        &self.group
    }

    pub fn clear_selected(&mut self) {
        self.selected_id = None;
        self.group.deactivate();
    }

    pub fn set_selected(&mut self, id: ItemId) {
        self.selected_id = Some(id);
        self.group.activate();
    }

    pub fn set_active(&mut self, active: bool) {
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
