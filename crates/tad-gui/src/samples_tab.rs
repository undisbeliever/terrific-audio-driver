//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CombineSamplesError, InstrumentOutput, ItemId};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, ListAction, ListButtons, ListEditor, ListEditorTable, ListMessage,
    ListState, TableCompilerOutput, TableMapping,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::Message;

use compiler::data::{self, Instrument, LoopSetting};
use compiler::path::SourcePathBuf;
use compiler::samples::{BRR_EXTENSION, WAV_EXTENSION};
use compiler::{Adsr, Envelope, Gain, STARTING_OCTAVE};
use fltk::button::Button;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::{Color, Event};
use fltk::group::Flex;
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
    const CHOICES: &str = concat![
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
    const CHOICES: &str = "ADSR|GAIN";

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

    fn add_clicked() -> Message {
        Message::Instrument(ListMessage::Add(blank_instrument()))
    }

    fn to_message(lm: ListMessage<data::Instrument>) -> Message {
        Message::Instrument(lm)
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

    sender: app::Sender<Message>,

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
    fn new(sender: app::Sender<Message>) -> Rc<RefCell<InstrumentEditor>> {
        let mut form = InputForm::new(15);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_two_inputs_right::<Output, Button>("Source:", 5);
        let freq = form.add_input::<FloatInput>("Frequency:");
        let loop_settings = form.add_two_inputs::<Choice, IntInput>("Loop:", 25);
        let first_octave = form.add_input::<IntInput>("First octave:");
        let last_octave = form.add_input::<IntInput>("Last octave:");
        let envelope = form.add_two_inputs::<Choice, Input>("Envelope:", 12);
        let comment = form.add_input::<Input>("Comment:");

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
        out
    }

    fn widget_event_handler(s: &Rc<RefCell<InstrumentEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
    }

    fn source_button_clicked(&mut self) {
        if let Some(index) = self.selected_index {
            self.sender.send(Message::OpenInstrumentSampleDialog(index));
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
                .send(Message::Instrument(ListMessage::ItemEdited(index, data)));
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
                let lp = loop_setting_to_loop_point(&self.data.loop_setting);
                w.set_value(&lp.to_string());
                w.activate();
            }

            LoopChoice::DupeBlockHack => {
                let bc = loop_setting_to_block_count(&self.data.loop_setting);
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

pub struct SamplesTab {
    group: Flex,

    inst_table: ListEditorTable<InstrumentMapping>,

    instrument_editor: Rc<RefCell<InstrumentEditor>>,

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
        sender: app::Sender<Message>,
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

        let instrument_editor = InstrumentEditor::new(sender);

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
    }

    fn clear_selected(&mut self) {
        self.inst_table.clear_selected();
        self.instrument_editor.borrow_mut().disable_editor();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, inst: &Instrument) {
        self.inst_table.set_selected(index, id, inst);

        self.instrument_editor.borrow_mut().set_data(index, inst);
    }
}

impl CompilerOutputGui<InstrumentOutput> for SamplesTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<InstrumentOutput>) {
        self.inst_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<InstrumentOutput>) {
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

                // ::TODO highlight invalid inputs::
            }
        }
    }
}

fn loop_setting_to_loop_point(l: &LoopSetting) -> usize {
    match *l {
        LoopSetting::None => 0,
        LoopSetting::OverrideBrrLoopPoint(lp) => lp,
        LoopSetting::LoopWithFilter(lp) => lp,
        LoopSetting::LoopResetFilter(lp) => lp,
        LoopSetting::DupeBlockHack(dbh) => dbh.checked_mul(16).unwrap_or(0),
    }
}

fn loop_setting_to_block_count(l: &LoopSetting) -> usize {
    match *l {
        LoopSetting::None => 0,

        LoopSetting::OverrideBrrLoopPoint(lp)
        | LoopSetting::LoopWithFilter(lp)
        | LoopSetting::LoopResetFilter(lp) => lp.checked_div(16).unwrap_or(0),

        LoopSetting::DupeBlockHack(dbh) => dbh,
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
