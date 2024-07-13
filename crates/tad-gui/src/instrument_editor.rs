//! Instrument editor (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{InstrumentOutput, ItemId, PlaySampleArgs};
use crate::envelope_widget::EnvelopeWidget;
use crate::helpers::*;
use crate::list_editor::{ListMessage, TableCompilerOutput, TableMapping};
use crate::sample_widgets::{
    LoopSettingWidget, SampleEnvelopeWidget, SampleWidgetEditor, SourceFileType, DEFAULT_ENVELOPE,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::GuiMessage;

use compiler::data::{self, Instrument, LoopSetting};
use compiler::driver_constants::MAX_INSTRUMENTS_AND_SAMPLES;
use compiler::errors::ValueError;
use compiler::notes::{Note, Octave, PitchChar, STARTING_OCTAVE};
use compiler::path::SourcePathBuf;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::button::Button;
use fltk::enums::{Align, Color, Event};
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
        first_octave: STARTING_OCTAVE,
        last_octave: STARTING_OCTAVE,
        envelope: DEFAULT_ENVELOPE,
        comment: None,
    }
}

pub struct InstrumentMapping;

impl TableMapping for InstrumentMapping {
    type DataType = data::Instrument;
    type RowType = RowWithStatus<SimpleRow<1>>;

    const MAX_SIZE: usize = MAX_INSTRUMENTS_AND_SAMPLES;

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
    loop_setting: LoopSettingWidget,
    first_octave: IntInput,
    last_octave: IntInput,
    envelope: SampleEnvelopeWidget,
    comment: Input,
}

impl InstrumentEditor {
    pub fn new(sender: app::Sender<GuiMessage>) -> (Rc<RefCell<InstrumentEditor>>, i32) {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);
        let freq = form.add_two_inputs_right::<FloatInput, Button>("Frequency:", 5);
        let loop_setting = LoopSettingWidget::new(&mut form);
        let first_octave = form.add_input::<IntInput>("First octave:");
        let last_octave = form.add_input::<IntInput>("Last octave:");
        let envelope = SampleEnvelopeWidget::new(&mut form);
        let comment = form.add_input::<Input>("Comment:");

        let form_height = 9 * form.row_height();
        let group = form.take_group_end();

        let (source, mut source_button) = source;
        let (freq, mut analyse_button) = freq;

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_id: None,
            data: blank_instrument(),
            name,
            source,
            freq,
            loop_setting,
            first_octave,
            last_octave,
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
            add_callbacks!(first_octave);
            add_callbacks!(last_octave);
            add_callbacks!(comment);

            editor.loop_setting.set_editor(out.clone());
            editor.envelope.set_editor(out.clone());

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
            })
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
        read_or_reset!(first_octave);
        read_or_reset!(last_octave);
        read_or_reset!(comment);

        let loop_setting = self.loop_setting.read_or_reset(&self.data.loop_setting);
        let envelope = self.envelope.read_or_reset();

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

    pub fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.freq.set_value("");
        self.first_octave.set_value("");
        self.last_octave.set_value("");
        self.loop_setting.clear_value();
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
        set_widget!(first_octave);
        set_widget!(last_octave);
        set_widget!(comment);
        self.source.set_value(data.source.as_str());
        self.loop_setting.set_value(&data.loop_setting);
        self.envelope.set_value(&data.envelope);

        self.loop_setting
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

    fn on_key_pressed(&self, pitch: PitchChar) -> Result<(), ValueError> {
        if let Some(id) = self.selected_id {
            let envelope = self.envelope.get_envelope()?;
            let octave = Octave::try_from(self.octave.value() as u32)?;
            let note = Note::from_pitch_and_octave(pitch, octave)?;

            self.sender.send(GuiMessage::PlayInstrument(
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
