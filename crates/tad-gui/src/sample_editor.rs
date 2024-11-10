//! Sample editor (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, PlaySampleArgs, SampleOutput};
use crate::envelope_widget::EnvelopeWidget;
use crate::helpers::*;
use crate::list_editor::{ListMessage, TableCompilerOutput, TableMapping};
use crate::sample_widgets::{
    BrrSettingsWidget, SampleEnvelopeWidget, SampleWidgetEditor, SourceFileType, DEFAULT_ENVELOPE,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::GuiMessage;

use compiler::data::{self, LoopSetting, Sample};
use compiler::notes::Note;
use compiler::path::SourcePathBuf;
use fltk::group::{Flex, Group};
use fltk::misc::Spinner;

use std::cell::RefCell;
use std::fmt::Write;
use std::rc::Rc;

use fltk::app;
use fltk::button::{Button, CheckButton};
use fltk::enums::Event;
use fltk::input::Input;
use fltk::output::Output;
use fltk::prelude::*;

fn blank_sample() -> data::Sample {
    data::Sample {
        name: "name".parse().unwrap(),
        source: SourcePathBuf::default(),
        loop_setting: LoopSetting::None,
        evaluator: Default::default(),
        ignore_gaussian_overflow: false,
        sample_rates: Vec::new(),
        envelope: DEFAULT_ENVELOPE,
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

    fn user_changes_selection() -> Option<GuiMessage> {
        Some(GuiMessage::UserChangedSelectedSample)
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

    selected_id: Option<ItemId>,
    data: Sample,

    name: Input,
    source: Output,
    brr_settings: BrrSettingsWidget,
    ignore_gaussian_overflow: CheckButton,
    sample_rates: Input,
    envelope: SampleEnvelopeWidget,
    comment: Input,
}

impl SampleEditor {
    pub fn new(sender: app::Sender<GuiMessage>) -> (Rc<RefCell<SampleEditor>>, i32) {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);
        let brr_settings = BrrSettingsWidget::new(&mut form);
        let ignore_gaussian_overflow = form.add_checkbox_right("Ignore Gaussian overflow");
        let mut analyse_button = form.add_input::<Button>("");
        let sample_rates = form.add_input::<Input>("Sample Rates:");
        let envelope = SampleEnvelopeWidget::new(&mut form);
        let comment = form.add_input::<Input>("Comment:");

        let form_height = 9 * form.row_height();
        let group = form.take_group_end();

        let (source, mut source_button) = source;

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_id: None,
            data: blank_sample(),
            name,
            source,
            brr_settings,
            ignore_gaussian_overflow,
            sample_rates,
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
            add_callbacks!(sample_rates);
            add_callbacks!(comment);

            editor.brr_settings.set_editor(out.clone());
            editor.envelope.set_editor(out.clone());

            editor.ignore_gaussian_overflow.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().on_finished_editing()
            });

            source_button.set_label("...");
            source_button.set_callback({
                let s = out.clone();
                move |_widget| s.borrow_mut().source_button_clicked()
            });

            analyse_button.set_label("Analyse Sample");
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

    fn widget_event_handler(s: &Rc<RefCell<SampleEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
    }

    fn source_button_clicked(&mut self) {
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::OpenSampleSampleDialog(id));
        }
    }

    fn analyse_button_clicked(&mut self) {
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::OpenAnalyseSampleDialog(id));
        }
    }

    fn send_edit_message(&self, data: Sample) {
        if let Some(id) = self.selected_id {
            self.sender.send(GuiMessage::EditSample(id, data));
        }
    }

    fn read_or_reset(&mut self) -> Option<Sample> {
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
        read_or_reset!(comment);

        let (loop_setting, evaluator) = self.brr_settings.read_or_reset(&self.data.loop_setting);
        let ignore_gaussian_overflow = self.ignore_gaussian_overflow.value();
        let envelope = self.envelope.read_or_reset();
        let sample_rates = self.read_or_reset_sample_rates();

        Some(Sample {
            name: name?,
            loop_setting: loop_setting?,
            evaluator,
            ignore_gaussian_overflow,
            sample_rates: sample_rates?,
            envelope: envelope?,
            comment: comment?,

            // must be last (after the ?'s)
            source: self.data.source.clone(),
        })
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

    pub fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.brr_settings.clear_value();
        self.ignore_gaussian_overflow.clear();
        self.sample_rates.set_value("");
        self.envelope.clear_value();

        self.selected_id = None;
    }

    fn set_data_update_widget(&mut self, data: &Sample) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
        set_widget!(comment);

        self.source.set_value(data.source.as_str());
        self.brr_settings
            .set_value(&data.loop_setting, data.evaluator);
        self.envelope.set_value(&data.envelope);

        self.sample_rates
            .set_value(&Self::sample_rates_string(&data.sample_rates));

        self.brr_settings
            .update_loop_type_choice(SourceFileType::from_source(&data.source));

        self.ignore_gaussian_overflow
            .set_value(data.ignore_gaussian_overflow);

        self.data = data.clone();

        self.group.activate();
    }

    pub fn set_selected(&mut self, id: ItemId, value: &Sample) {
        self.selected_id = Some(id);
        self.set_data_update_widget(value);
    }

    pub fn selected_id(&self) -> Option<ItemId> {
        self.selected_id
    }

    pub fn item_edited(&mut self, id: ItemId, value: &Sample) {
        if self.selected_id == Some(id) {
            self.set_data_update_widget(value);
        }
    }
}

impl SampleWidgetEditor for SampleEditor {
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

pub struct TestSampleWidget {
    selected_id: Option<ItemId>,

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
        self.group.deactivate();
    }

    pub fn set_selected(&mut self, id: ItemId, data: &Sample) {
        self.selected_id = Some(id);
        self.group.activate();
        self.update_buttons(data);
    }

    pub fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    pub fn item_edited(&mut self, id: ItemId, data: &Sample) {
        if self.selected_id == Some(id) {
            self.update_buttons(data);
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
