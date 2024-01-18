//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CombineSamplesError, InstrumentOutput, ItemId, SampleOutput};
use crate::helpers::*;
use crate::list_editor::{
    CompilerOutputGui, ListAction, ListButtons, ListEditor, ListEditorTable, ListState,
};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use crate::instrument_editor::{InstrumentEditor, InstrumentMapping, TestInstrumentWidget};
use crate::sample_editor::{SampleEditor, SampleMapping, TestSampleWidget};

use compiler::data::{self, Instrument};

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::Color;
use fltk::frame::Frame;
use fltk::group::{Flex, Wizard};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

#[derive(PartialEq)]
enum EditorType {
    Instrument,
    Sample,
}

pub struct SamplesTab {
    group: Flex,

    selected_editor: EditorType,

    inst_table: ListEditorTable<InstrumentMapping>,
    sample_table: ListEditorTable<SampleMapping>,

    editor_wizard: Wizard,

    instrument_group: Flex,
    instrument_editor: Rc<RefCell<InstrumentEditor>>,
    test_instrument_widget: Rc<RefCell<TestInstrumentWidget>>,

    sample_group: Flex,
    sample_editor: Rc<RefCell<SampleEditor>>,
    test_sample_widget: Rc<RefCell<TestSampleWidget>>,

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
        samples: &impl ListState<Item = data::Sample>,
        sender: app::Sender<GuiMessage>,
    ) -> Self {
        let mut group = Flex::default_fill().row();

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut inst_table = ListEditorTable::new_with_data(instruments, sender.clone());

        let button_height = inst_table.button_height();
        sidebar.fixed(&inst_table.list_buttons().pack, button_height);

        let mut sample_table = ListEditorTable::new_with_data(samples, sender.clone());
        sidebar.fixed(&sample_table.list_buttons().pack, button_height);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let editor_wizard = Wizard::default().with_size(400, 400);

        let mut instrument_group = Flex::default().column().size_of_parent();

        let (instrument_editor, se_height) = InstrumentEditor::new(sender.clone());
        instrument_group.fixed(instrument_editor.borrow().widget(), se_height + group.pad());

        let test_instrument_widget = {
            let mut ts_flex = Flex::default().row();
            Frame::default();
            let test_instrument_widget = TestInstrumentWidget::new(sender.clone());
            Frame::default();
            ts_flex.end();

            {
                let tsw = test_instrument_widget.borrow();
                let ts_group = tsw.widget();
                ts_flex.fixed(ts_group, ts_group.width());
                instrument_group.fixed(&ts_flex, ts_group.height());
            }

            test_instrument_widget
        };

        instrument_group.end();

        let mut sample_group = Flex::default().column().size_of_parent();

        let (sample_editor, se_height) = SampleEditor::new(sender.clone());
        sample_group.fixed(sample_editor.borrow().widget(), se_height + group.pad());

        let test_sample_widget = TestSampleWidget::new(sender.clone());

        sample_group.end();

        editor_wizard.end();

        let mut console = TextDisplay::default();
        main_group.fixed(&console, button_height * 4);

        main_group.end();
        group.end();

        let console_buffer = TextBuffer::default();
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        Self {
            group,
            selected_editor: EditorType::Instrument,
            inst_table,
            sample_table,
            editor_wizard,
            instrument_group,
            instrument_editor,
            test_instrument_widget,
            sample_group,
            sample_editor,
            test_sample_widget,
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
        self.editor_wizard
            .set_current_widget(&self.instrument_group);

        self.selected_editor = EditorType::Instrument;

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
            None => {
                if self.selected_editor == EditorType::Instrument {
                    self.console_buffer.set_text("")
                }
            }
            Some(Ok(o)) => {
                self.console_buffer
                    .set_text(&format!("BRR Sample size: {} bytes", o.0));
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

impl ListEditor<data::Sample> for SamplesTab {
    fn list_buttons(&mut self) -> &mut ListButtons {
        self.sample_table.list_buttons()
    }

    fn list_edited(&mut self, action: &ListAction<data::Sample>) {
        self.sample_table.list_edited(action);
        self.sample_editor.borrow_mut().list_edited(action);
        self.test_sample_widget.borrow_mut().list_edited(action);
    }

    fn clear_selected(&mut self) {
        self.sample_table.clear_selected();

        self.sample_editor.borrow_mut().disable_editor();
        self.test_sample_widget.borrow_mut().clear_selected();
    }

    fn set_selected(&mut self, index: usize, id: ItemId, sample: &data::Sample) {
        self.editor_wizard.set_current_widget(&self.sample_group);

        self.selected_editor = EditorType::Sample;

        self.sample_table.set_selected(index, id, sample);
        self.sample_editor.borrow_mut().set_data(index, sample);
        self.test_sample_widget
            .borrow_mut()
            .set_selected(index, id, sample);
    }
}

impl CompilerOutputGui<SampleOutput> for SamplesTab {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<SampleOutput>) {
        self.sample_table
            .set_compiler_output(index, compiler_output);
    }

    fn set_selected_compiler_output(&mut self, compiler_output: &Option<SampleOutput>) {
        self.test_sample_widget
            .borrow_mut()
            .set_active(matches!(compiler_output, Some(Ok(_))));

        match compiler_output {
            None => {
                if self.selected_editor == EditorType::Sample {
                    self.console_buffer.set_text("")
                }
            }
            Some(Ok(o)) => {
                self.console_buffer
                    .set_text(&format!("BRR Sample size: {} bytes", o.0));
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
