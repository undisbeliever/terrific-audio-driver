//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{
    CadOutput, CombineSamplesError, InstrumentOutput, ItemId, SampleOutput,
};
use crate::list_editor::{
    tables_for_list_pair, ListAction, ListEditorTable, ListWithCompilerOutput,
    ListWithCompilerOutputEditor,
};
use crate::sample_sizes_widget::SampleSizesWidget;
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;
use crate::{helpers::*, InstrumentsAndSamplesData};

use crate::instrument_editor::{InstrumentEditor, InstrumentMapping, TestInstrumentWidget};
use crate::sample_editor::{SampleEditor, SampleMapping, TestSampleWidget};

use compiler::data::{self, Instrument};
use compiler::songs::SongAramSize;
use fltk::button::Button;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::Color;
use fltk::frame::Frame;
use fltk::group::{Flex, Wizard};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

#[derive(PartialEq)]
enum SelectedEditor {
    CombinedSamplesResult,
    Instrument(ItemId),
    Sample(ItemId),
}

pub struct SamplesTab {
    group: Flex,

    selected_editor: SelectedEditor,
    cad_output: CadOutput,

    sample_sizes_button: Button,
    inst_table: ListEditorTable<InstrumentMapping>,
    sample_table: ListEditorTable<SampleMapping>,

    editor_wizard: Wizard,

    sample_sizes_group: Flex,
    sample_sizes_widget: Rc<RefCell<SampleSizesWidget>>,

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
        instruments_and_samples: &InstrumentsAndSamplesData,
        sender: app::Sender<GuiMessage>,
    ) -> Self {
        let mut group = Flex::default_fill().row();
        let margin = ch_units_to_width(&group, 1) / 2;

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut sample_sizes_button = Button::default().with_label("Unchecked");
        sample_sizes_button.set_tooltip("Show sample sizes");
        sidebar.fixed(&sample_sizes_button, ch_units_to_width(&sidebar, 5));

        let (inst_table, sample_table) =
            tables_for_list_pair(&mut sidebar, sender, instruments_and_samples);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let editor_wizard = Wizard::default().with_size(400, 400);

        let mut sample_sizes_group = Flex::default().column().size_of_parent();
        sample_sizes_group.set_margin(margin);
        let sample_sizes_widget =
            SampleSizesWidget::new(&mut sample_sizes_group, instruments_and_samples);
        sample_sizes_group.end();

        let mut instrument_group = Flex::default().column().size_of_parent();
        instrument_group.set_margin(margin);

        let (instrument_editor, se_height) = InstrumentEditor::new(sender);
        instrument_group.fixed(instrument_editor.borrow().widget(), se_height + group.pad());

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
                instrument_group.fixed(&ts_flex, ts_group.height());
            }

            test_instrument_widget
        };

        instrument_group.end();

        let mut sample_group = Flex::default().column().size_of_parent();
        sample_group.set_margin(margin);

        let (sample_editor, se_height) = SampleEditor::new(sender);
        sample_group.fixed(sample_editor.borrow().widget(), se_height + group.pad());

        let test_sample_widget = TestSampleWidget::new(sender);

        sample_group.end();

        editor_wizard.end();

        let mut console = TextDisplay::default();
        main_group.fixed(&console, ch_units_to_width(&console, 8));

        main_group.end();
        group.end();

        let console_buffer = TextBuffer::default();
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        sample_sizes_button.set_callback({
            let s = sender;
            move |_| {
                s.send(GuiMessage::ShowSampleSizes);
            }
        });

        Self {
            group,
            selected_editor: SelectedEditor::CombinedSamplesResult,
            cad_output: CadOutput::None,
            sample_sizes_button,
            sample_sizes_group,
            sample_sizes_widget,
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

    pub fn set_common_audio_data(&mut self, cad: CadOutput) {
        self.cad_output = cad;

        match &self.cad_output {
            CadOutput::None => {
                self.sample_sizes_button.set_label("Sample sizes");
                self.sample_sizes_button.set_label_color(Color::Foreground);
            }
            CadOutput::Err(e) => {
                self.sample_sizes_button.set_label_color(Color::Red);
                match e {
                    CombineSamplesError::IndividualErrors {
                        n_instrument_errors,
                        n_sample_errors,
                    } => {
                        let n_errors = n_instrument_errors + n_sample_errors;
                        if n_errors == 1 {
                            self.sample_sizes_button.set_label("1 error");
                        } else {
                            self.sample_sizes_button
                                .set_label(&format!("{n_errors} errors"));
                        }
                    }
                    CombineSamplesError::CombineError(..) => {
                        self.sample_sizes_button.set_label("Combine samples error")
                    }
                    CombineSamplesError::CommonAudioData(..) => self
                        .sample_sizes_button
                        .set_label("Common audio data error"),
                    CombineSamplesError::UniqueNamesError(..) => {
                        self.sample_sizes_button.set_label("Unique names error")
                    }
                }
            }
            CadOutput::NoSfx(..) | CadOutput::SfxBuffer(..) | CadOutput::WithSfx(..) => {
                self.sample_sizes_button.set_label("All OK");
                self.sample_sizes_button.set_label_color(Color::Foreground);
            }
        }

        if matches!(self.selected_editor, SelectedEditor::CombinedSamplesResult) {
            self.sample_sizes_widget
                .borrow_mut()
                .cad_changed(&self.cad_output);
        }

        match &self.selected_editor {
            SelectedEditor::CombinedSamplesResult => self.update_sample_sizes_widget_and_console(),
            SelectedEditor::Instrument(_) => (),
            SelectedEditor::Sample(_) => (),
        }
    }

    pub fn set_largest_song(&mut self, s: SongAramSize) {
        self.sample_sizes_widget.borrow_mut().set_largest_song(s);
    }

    pub fn show_sample_sizes_widget(&mut self) {
        self.inst_table.clear_selected_row();
        self.sample_table.clear_selected_row();

        self.sample_editor.borrow_mut().disable_editor();
        self.test_sample_widget.borrow_mut().clear_selected();

        if !matches!(self.selected_editor, SelectedEditor::CombinedSamplesResult) {
            self.selected_editor = SelectedEditor::CombinedSamplesResult;
            self.editor_wizard
                .set_current_widget(&self.sample_sizes_group);

            self.update_sample_sizes_widget_and_console();
        }
    }

    fn update_sample_sizes_widget_and_console(&mut self) {
        self.sample_sizes_widget
            .borrow_mut()
            .cad_changed(&self.cad_output);

        match &self.cad_output {
            CadOutput::None
            | &CadOutput::NoSfx(..)
            | CadOutput::SfxBuffer(..)
            | CadOutput::WithSfx(..) => {
                self.console_buffer.set_text("");
            }
            CadOutput::Err(e) => {
                self.console_buffer.set_text(&e.to_string());
                self.console.set_text_color(Color::Red);
                self.console.scroll(0, 0);
            }
        }
    }

    fn selected_instrument_output_changed(&mut self, compiler_output: &Option<InstrumentOutput>) {
        self.test_instrument_widget
            .borrow_mut()
            .set_active(matches!(compiler_output, Some(Ok(_))));

        match compiler_output {
            None => {
                self.console_buffer.set_text("");
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

    fn selected_sample_output_changed(&mut self, compiler_output: &Option<SampleOutput>) {
        self.test_sample_widget
            .borrow_mut()
            .set_active(matches!(compiler_output, Some(Ok(_))));

        match compiler_output {
            None => {
                self.console_buffer.set_text("");
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

impl ListWithCompilerOutputEditor<Instrument, InstrumentOutput> for SamplesTab {
    type TableMapping = InstrumentMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping> {
        &mut self.inst_table
    }

    fn list_edited(&mut self, action: &ListAction<Instrument>) {
        self.sample_sizes_widget
            .borrow_mut()
            .instrument_edited(action);
    }

    fn item_edited(&mut self, id: ItemId, value: &Instrument) {
        self.instrument_editor.borrow_mut().item_edited(id, value)
    }

    fn set_compiler_output(
        &mut self,
        index: usize,
        id: ItemId,
        compiler_output: &Option<InstrumentOutput>,
    ) {
        self.sample_sizes_widget
            .borrow_mut()
            .instrument_compiled(index, compiler_output);

        let sel_id = self.instrument_editor.borrow().selected_id();
        if sel_id == Some(id) {
            self.selected_instrument_output_changed(compiler_output);
        }
    }

    fn selected_item_changed(
        &mut self,
        instruments: &ListWithCompilerOutput<Instrument, InstrumentOutput>,
    ) {
        match instruments.get_selected_row(&self.inst_table) {
            Some((id, inst, co)) => {
                self.sample_table.clear_selected_row();

                if self.selected_editor != SelectedEditor::Instrument(id) {
                    self.selected_editor = SelectedEditor::Instrument(id);

                    self.instrument_editor.borrow_mut().set_selected(id, inst);
                    self.test_instrument_widget.borrow_mut().set_selected(id);

                    self.editor_wizard
                        .set_current_widget(&self.instrument_group);

                    self.selected_instrument_output_changed(co);
                }
            }
            None => {
                if matches!(self.selected_editor, SelectedEditor::Instrument(_)) {
                    self.show_sample_sizes_widget();
                }
            }
        }
    }
}

impl ListWithCompilerOutputEditor<data::Sample, SampleOutput> for SamplesTab {
    type TableMapping = SampleMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping> {
        &mut self.sample_table
    }

    fn list_edited(&mut self, action: &ListAction<data::Sample>) {
        self.sample_sizes_widget.borrow_mut().sample_edited(action);
    }

    fn item_edited(&mut self, id: ItemId, value: &data::Sample) {
        self.sample_editor.borrow_mut().item_edited(id, value);
        self.test_sample_widget.borrow_mut().item_edited(id, value);
    }

    fn set_compiler_output(
        &mut self,
        index: usize,
        id: ItemId,
        compiler_output: &Option<SampleOutput>,
    ) {
        self.sample_sizes_widget
            .borrow_mut()
            .sample_compiled(index, compiler_output);

        let selected_id = self.sample_editor.borrow().selected_id();
        if selected_id == Some(id) {
            self.selected_sample_output_changed(compiler_output);
        }
    }

    fn selected_item_changed(
        &mut self,
        samples: &ListWithCompilerOutput<data::Sample, SampleOutput>,
    ) {
        match samples.get_selected_row(&self.sample_table) {
            Some((id, sample, co)) => {
                self.inst_table.clear_selected_row();

                if self.selected_editor != SelectedEditor::Sample(id) {
                    self.selected_editor = SelectedEditor::Sample(id);

                    self.sample_editor.borrow_mut().set_selected(id, sample);
                    self.test_sample_widget
                        .borrow_mut()
                        .set_selected(id, sample);

                    self.editor_wizard.set_current_widget(&self.sample_group);

                    self.selected_sample_output_changed(co);
                }
            }
            None => {
                if matches!(self.selected_editor, SelectedEditor::Sample(_)) {
                    self.show_sample_sizes_widget();
                }
            }
        }
    }
}
