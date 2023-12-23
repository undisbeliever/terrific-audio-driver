//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CombineSamplesError, InstrumentOutput, ItemId};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, ListAction, ListButtons, ListEditor, ListEditorTable, ListState,
};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use crate::instrument_editor::{InstrumentEditor, InstrumentMapping, TestInstrumentWidget};

use compiler::data::{self, Instrument};
use compiler::envelope::{Adsr, Gain};
use compiler::path::SourcePathBuf;
use compiler::samples::{BRR_EXTENSION, WAV_EXTENSION};

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::Color;
use fltk::frame::Frame;
use fltk::group::Flex;
use fltk::menu::Choice;
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

#[derive(Clone, Copy)]
pub enum LoopChoice {
    None = 0,
    OverrideBrrLoopPoint = 1,
    LoopWithFilter = 2,
    LoopResetFilter = 3,
    DupeBlockHack = 4,
}
impl LoopChoice {
    pub const CHOICES: &'static str = concat![
        "&None",
        "|&Override BRR Loop Point",
        "|&Loop With Filter",
        "|Loop &Resets Filter",
        "|&Dupe Block Hack"
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::None,
            1 => Self::OverrideBrrLoopPoint,
            2 => Self::LoopWithFilter,
            3 => Self::LoopResetFilter,
            4 => Self::DupeBlockHack,

            _ => Self::None,
        }
    }

    pub fn to_i32(self) -> i32 {
        self as i32
    }
}

#[derive(Clone, Copy)]
pub enum EnvelopeChoice {
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

pub const fn can_use_loop_setting(l: LoopChoice, sft: &SourceFileType) -> bool {
    match l {
        LoopChoice::None => !matches!(sft, SourceFileType::Unknown),
        LoopChoice::OverrideBrrLoopPoint => matches!(sft, SourceFileType::Brr),
        LoopChoice::LoopWithFilter => matches!(sft, SourceFileType::Wav),
        LoopChoice::LoopResetFilter => matches!(sft, SourceFileType::Wav),
        LoopChoice::DupeBlockHack => matches!(sft, SourceFileType::Wav),
    }
}

pub const DEFAULT_ADSR: Adsr = match Adsr::try_new(12, 2, 2, 15) {
    Ok(a) => a,
    Err(_) => panic!("Invalid ADSR"),
};
pub const DEFAULT_GAIN: Gain = Gain::new(127);

pub struct SamplesTab {
    group: Flex,

    inst_table: ListEditorTable<InstrumentMapping>,

    instrument_editor: Rc<RefCell<InstrumentEditor>>,
    test_instrument_widget: Rc<RefCell<TestInstrumentWidget>>,

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
        sender: app::Sender<GuiMessage>,
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

        let (instrument_editor, ie_height) = InstrumentEditor::new(sender.clone());
        main_group.fixed(instrument_editor.borrow().widget(), ie_height + group.pad());

        let test_instrument_widget = {
            let mut ts_flex = Flex::default().row();
            Frame::default();
            let test_instrument_widget = TestInstrumentWidget::new(sender);
            Frame::default();
            ts_flex.end();

            {
                let tsw = test_instrument_widget.borrow();
                let ts_group = tsw.widget();
                ts_flex.fixed(ts_group, ts_group.width());
                main_group.fixed(&ts_flex, ts_group.height());
            }

            test_instrument_widget
        };

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
            test_instrument_widget,
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
        self.test_instrument_widget.borrow_mut().clear_selected();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, inst: &Instrument) {
        self.inst_table.set_selected(index, id, inst);
        self.instrument_editor.borrow_mut().set_data(index, inst);
        self.test_instrument_widget.borrow_mut().set_selected(id);
    }
}

impl CompilerOutputGui<InstrumentOutput> for SamplesTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<InstrumentOutput>) {
        self.inst_table.set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<InstrumentOutput>) {
        self.test_instrument_widget
            .borrow_mut()
            .set_active(matches!(compiler_output, Some(Ok(_))));

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
            }
        }
    }
}
