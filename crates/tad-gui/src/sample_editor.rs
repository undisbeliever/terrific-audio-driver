//! Sample editor (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, PlaySampleArgs, SampleOutput};
use crate::envelope_widget::EnvelopeWidget;
use crate::helpers::*;
use crate::list_editor::{ListAction, ListMessage, TableCompilerOutput, TableMapping};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::GuiMessage;

use crate::samples_tab::{
    can_use_loop_setting, EnvelopeChoice, LoopChoice, SourceFileType, DEFAULT_ADSR, DEFAULT_GAIN,
};

use compiler::data::{self, LoopSetting, Sample};
use compiler::envelope::Envelope;
use compiler::notes::Note;
use compiler::path::SourcePathBuf;
use fltk::group::{Flex, Group};
use fltk::misc::Spinner;

use std::cell::RefCell;
use std::fmt::Write;
use std::rc::Rc;

use fltk::app;
use fltk::button::Button;
use fltk::enums::Event;
use fltk::input::{Input, IntInput};
use fltk::menu::Choice;
use fltk::output::Output;
use fltk::prelude::*;

fn blank_sample() -> data::Sample {
    data::Sample {
        name: "name".parse().unwrap(),
        source: SourcePathBuf::default(),
        loop_setting: LoopSetting::None,
        sample_rates: Vec::new(),
        envelope: Envelope::Adsr(DEFAULT_ADSR),
        comment: None,
    }
}

pub struct SampleMapping;

impl TableMapping for SampleMapping {
    type DataType = data::Sample;
    type RowType = RowWithStatus<SimpleRow<1>>;

    const CAN_CLONE: bool = true;
    const CAN_EDIT: bool = false;

    fn type_name() -> &'static str {
        "sample"
    }

    fn headers() -> Vec<String> {
        vec!["Samples".to_owned()]
    }

    fn add_clicked() -> GuiMessage {
        GuiMessage::Sample(ListMessage::Add(blank_sample()))
    }

    fn to_message(lm: ListMessage<data::Sample>) -> GuiMessage {
        GuiMessage::Sample(lm)
    }

    fn new_row(i: &data::Sample) -> Self::RowType {
        RowWithStatus::new_unchecked(SimpleRow::new([i.name.as_str().to_string()]))
    }

    fn edit_row(r: &mut Self::RowType, i: &data::Sample) -> bool {
        r.columns.edit_column(0, i.name.as_str())
    }
}

impl TableCompilerOutput for SampleMapping {
    type CompilerOutputType = SampleOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<SampleOutput>) -> bool {
        r.set_status_optional_result(co)
    }
}

pub struct SampleEditor {
    group: Flex,

    sender: app::Sender<GuiMessage>,

    selected_index: Option<usize>,
    data: Sample,

    source_file_type: SourceFileType,

    name: Input,
    source: Output,
    loop_choice: Choice,
    loop_setting: IntInput,
    sample_rates: Input,
    envelope_choice: Choice,
    envelope_value: Input,
    comment: Input,

    prev_adsr: String,
    prev_gain: String,
}

impl SampleEditor {
    pub fn new(sender: app::Sender<GuiMessage>) -> (Rc<RefCell<SampleEditor>>, i32) {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);
        let loop_settings = form.add_two_inputs::<Choice, IntInput>("Loop:", 25);
        let sample_rates = form.add_input::<Input>("Sample Rates:");
        let envelope = form.add_two_inputs::<Choice, Input>("Envelope:", 12);
        let comment = form.add_input::<Input>("Comment:");

        let form_height = 7 * form.row_height();
        let group = form.take_group_end();

        let (source, mut source_button) = source;
        let (loop_choice, loop_setting) = loop_settings;
        let (envelope_choice, envelope_value) = envelope;

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_index: None,
            data: blank_sample(),
            source_file_type: SourceFileType::Unknown,
            name,
            source,
            loop_setting,
            loop_choice,
            sample_rates,
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
            add_callbacks!(loop_setting);
            add_callbacks!(sample_rates);
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

    pub fn widget(&self) -> &Flex {
        &self.group
    }

    fn widget_event_handler(s: &Rc<RefCell<SampleEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
    }

    fn source_button_clicked(&mut self) {
        if let Some(index) = self.selected_index {
            self.sender.send(GuiMessage::OpenSampleSampleDialog(index));
        }
    }

    fn on_finished_editing(&mut self) {
        if let Some(new_data) = self.read_or_reset() {
            self.send_edit_message(new_data);
        }
    }

    fn send_edit_message(&self, data: Sample) {
        if let Some(index) = self.selected_index {
            self.sender
                .send(GuiMessage::Sample(ListMessage::ItemEdited(index, data)));
        }
    }

    fn read_or_reset(&mut self) -> Option<Sample> {
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
        read_or_reset!(comment);

        let loop_setting = self.read_or_reset_loop_setting();
        let envelope = self.read_or_reset_envelope();
        let sample_rates = self.read_or_reset_sample_rates();

        Some(Sample {
            name: name?,
            loop_setting: loop_setting?,
            sample_rates: sample_rates?,
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

    fn read_or_reset_sample_rates(&mut self) -> Option<Vec<u32>> {
        let mut out = Vec::new();

        for s in self.sample_rates.value().split_whitespace() {
            match s.parse() {
                Ok(s) => out.push(s),
                Err(_) => {
                    let s = Self::sample_rates_string(&self.data.sample_rates);
                    self.sample_rates.set_value(&s);
                    return None;
                }
            }
        }

        Some(out)
    }

    fn sample_rates_string(sample_rates: &[u32]) -> String {
        let mut out = String::new();

        let mut sample_rates = sample_rates.iter();
        if let Some(s) = sample_rates.next() {
            let _ = write!(out, "{}", s);
        }
        for s in sample_rates {
            let _ = write!(out, " {}", s);
        }

        out
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

    pub fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");

        self.loop_choice.set_value(-1);
        self.loop_setting.set_value("");

        self.sample_rates.set_value("");

        self.envelope_choice.set_value(-1);
        self.envelope_value.set_value("");

        self.selected_index = None;
    }

    pub fn set_data(&mut self, index: usize, data: &Sample) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
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

        self.sample_rates
            .set_value(&Self::sample_rates_string(&data.sample_rates));

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
        let sft = SourceFileType::from_source(source);

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

    pub fn list_edited(&mut self, action: &ListAction<Sample>) {
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
    selected_id: Option<ItemId>,
    selected_index: Option<usize>,

    sender: app::Sender<GuiMessage>,

    group: Group,

    buttons: [Button; 12],

    note_length: Spinner,
    envelope: EnvelopeWidget,
}

impl TestSampleWidget {
    pub fn new(sender: app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut group = Group::default();
        group.make_resizable(false);

        let button_size = ch_units_to_width(&group, 5);

        let options_x = 6 * button_size;
        let options_y = button_size;
        let options_width = ch_units_to_width(&group, 30);
        let line_height = ch_units_to_width(&group, 3);

        group.set_size(options_x + options_width, button_size * 4);

        let buttons = std::array::from_fn(|i| {
            let i = i32::try_from(i).unwrap();
            let x = (i % 4 + 1) * button_size;
            let y = (i / 4 + 1) * button_size;

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
            selected_index: None,
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

    pub fn clear_selected(&mut self) {
        self.selected_id = None;
        self.selected_index = None;
        self.group.deactivate();
    }

    pub fn set_selected(&mut self, index: usize, id: ItemId, data: &Sample) {
        self.selected_id = Some(id);
        self.selected_index = Some(index);
        self.group.activate();
        self.update_buttons(data);
    }

    pub fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    pub fn list_edited(&mut self, action: &ListAction<Sample>) {
        if let ListAction::Edit(index, data) = action {
            if self.selected_index == Some(*index) {
                self.update_buttons(data);
            }
        }
    }

    fn update_buttons(&mut self, data: &Sample) {
        let n_sample_rates = data.sample_rates.len();
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
                        note_length: self.note_length.value() as u32,
                        envelope,
                    },
                ))
            }
        }
    }
}