//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CombineSamplesError, InstrumentOutput};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, ListAction, ListButtons, ListEditor, ListEditorTable, ListMessage,
    ListState, TableCompilerOutput, TableMapping,
};
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::Message;

use compiler::data::{self, Instrument};
use compiler::STARTING_OCTAVE;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use fltk::app;
use fltk::button::CheckButton;
use fltk::enums::{Color, Event};
use fltk::group::Flex;
use fltk::input::{FloatInput, Input, IntInput};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

fn blank_instrument() -> Instrument {
    Instrument {
        name: "name".parse().unwrap(),
        source: PathBuf::new(),
        freq: 500.0,
        looping: true,
        loop_point: None,
        dupe_block_hack: None,
        loop_resets_filter: false,
        first_octave: STARTING_OCTAVE,
        last_octave: STARTING_OCTAVE,
        adsr: None,
        gain: None,
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

pub struct InstrumentEditor {
    group: Flex,

    sender: app::Sender<Message>,

    selected_index: Option<usize>,
    data: Instrument,

    name: Input,
    source: Input,
    freq: FloatInput,
    looping: CheckButton,
    loop_point: IntInput,
    dupe_block_hack: IntInput,
    loop_resets_filter: CheckButton,
    first_octave: IntInput,
    last_octave: IntInput,
    adsr: Input,
    gain: Input,
    comment: Input,
}

impl InstrumentEditor {
    fn new(sender: app::Sender<Message>) -> Rc<RefCell<InstrumentEditor>> {
        let mut form = InputForm::new(18);

        let name = form.add_input::<Input>("Name:");
        let source = form.add_input::<Input>("Source:");
        let freq = form.add_input::<FloatInput>("Frequency:");
        let looping = form.add_checkbox_left("Looping");
        let loop_point = form.add_input::<IntInput>("Loop Point:");
        let dupe_block_hack = form.add_input::<IntInput>("Dupe block hack:");
        let loop_resets_filter = form.add_checkbox_right("Loop resets filter");
        let first_octave = form.add_input::<IntInput>("First octave:");
        let last_octave = form.add_input::<IntInput>("Last octave:");
        let adsr = form.add_input::<Input>("Adsr:");
        let gain = form.add_input::<Input>("Gain:");
        let comment = form.add_input::<Input>("Comment:");

        let group = form.take_group_end();

        let out = Rc::from(RefCell::new(Self {
            group,
            sender,
            selected_index: None,
            data: blank_instrument(),
            name,
            source,
            freq,
            looping,
            loop_point,
            dupe_block_hack,
            loop_resets_filter,
            first_octave,
            last_octave,
            adsr,
            gain,
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
            add_callbacks!(loop_point);
            add_callbacks!(dupe_block_hack);
            add_callbacks!(first_octave);
            add_callbacks!(last_octave);
            add_callbacks!(adsr);
            add_callbacks!(gain);
            add_callbacks!(comment);

            let add_cb_callback = |w: &mut CheckButton| {
                w.set_callback({
                    let s = out.clone();
                    move |_widget| s.borrow_mut().on_finished_editing()
                });
            };
            add_cb_callback(&mut editor.looping);
            add_cb_callback(&mut editor.loop_resets_filter);
        }
        out
    }

    fn widget_event_handler(s: &Rc<RefCell<InstrumentEditor>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing();
        }
        false
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
        read_or_reset!(source);
        read_or_reset!(freq);
        read_or_reset!(loop_point);
        read_or_reset!(dupe_block_hack);
        read_or_reset!(first_octave);
        read_or_reset!(last_octave);
        read_or_reset!(adsr);
        read_or_reset!(gain);
        read_or_reset!(comment);

        let looping = self.looping.value();
        let loop_resets_filter = self.loop_resets_filter.value();

        Some(Instrument {
            name: name?,
            source: source?,

            freq: freq?,
            looping,
            loop_point: loop_point?,
            dupe_block_hack: dupe_block_hack?,
            loop_resets_filter,
            first_octave: first_octave?,
            last_octave: last_octave?,
            adsr: adsr?,
            gain: gain?,
            comment: comment?,
        })
    }

    fn disable_editor(&mut self) {
        self.group.deactivate();

        self.name.set_value("");
        self.source.set_value("");
        self.freq.set_value("");
        self.looping.clear();
        self.loop_point.set_value("");
        self.dupe_block_hack.set_value("");
        self.loop_resets_filter.clear();
        self.first_octave.set_value("");
        self.last_octave.set_value("");
        self.adsr.set_value("");
        self.gain.set_value("");

        self.selected_index = None;
    }

    fn set_data(&mut self, index: usize, data: &Instrument) {
        macro_rules! set_widget {
            ($name:ident) => {
                InputHelper::set_widget_value(&mut self.$name, &data.$name);
            };
        }

        set_widget!(name);
        set_widget!(source);
        set_widget!(freq);
        self.looping.set(data.looping);
        set_widget!(loop_point);
        set_widget!(dupe_block_hack);
        self.loop_resets_filter.set(data.loop_resets_filter);
        set_widget!(first_octave);
        set_widget!(last_octave);
        set_widget!(adsr);
        set_widget!(gain);
        set_widget!(comment);

        self.selected_index = Some(index);
        self.data = data.clone();

        self.group.activate();
    }

    fn list_edited(&mut self, action: &ListAction<Instrument>) {
        // Update name as the name deduplicator may have changed it.
        if let ListAction::Edit(index, data) = action {
            if self.selected_index == Some(*index) && self.data.name != data.name {
                self.data.name = data.name.clone();
                InputHelper::set_widget_value(&mut self.name, &self.data.name);
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

    fn set_selected(&mut self, index: usize, inst: &Instrument) {
        self.inst_table.set_selected(index, inst);

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
                if let Some(e) = &errors.envelope_error {
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
