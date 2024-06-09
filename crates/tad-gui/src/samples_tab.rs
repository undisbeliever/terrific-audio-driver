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
enum EditorType {
    CombinedSamplesResult,
    Instrument,
    Sample,
}

pub struct SamplesTab {
    group: Flex,

    selected_editor: EditorType,
    combined_samples: Option<Result<usize, CombineSamplesError>>,

    show_combined_samples_button: Button,
    inst_table: ListEditorTable<InstrumentMapping>,
    sample_table: ListEditorTable<SampleMapping>,

    editor_wizard: Wizard,

    combined_samples_widget: Frame,

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
        let margin = ch_units_to_width(&group, 1) / 2;

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut show_combined_samples_button = Button::default().with_label("Unchecked");
        sidebar.fixed(
            &show_combined_samples_button,
            ch_units_to_width(&sidebar, 5),
        );

        let mut inst_table = ListEditorTable::new_with_data(instruments, sender.clone());

        let button_height = inst_table.button_height();
        sidebar.fixed(&inst_table.list_buttons().pack, button_height);

        let mut sample_table = ListEditorTable::new_with_data(samples, sender.clone());
        sidebar.fixed(&sample_table.list_buttons().pack, button_height);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let editor_wizard = Wizard::default().with_size(400, 400);

        let combined_samples_widget = label("::TODO add graph and/or table of sample sizes::");

        let mut instrument_group = Flex::default().column().size_of_parent();
        instrument_group.set_margin(margin);

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
        sample_group.set_margin(margin);

        let (sample_editor, se_height) = SampleEditor::new(sender.clone());
        sample_group.fixed(sample_editor.borrow().widget(), se_height + group.pad());

        let test_sample_widget = TestSampleWidget::new(sender.clone());

        sample_group.end();

        editor_wizard.end();

        let mut console = TextDisplay::default();
        main_group.fixed(&console, button_height * 3);

        main_group.end();
        group.end();

        let console_buffer = TextBuffer::default();
        console.set_buffer(console_buffer.clone());
        console.wrap_mode(WrapMode::AtBounds, 0);

        show_combined_samples_button.set_callback({
            let sender = sender.clone();
            move |_| {
                sender.send(GuiMessage::ShowSamplesResult);
            }
        });

        Self {
            group,
            selected_editor: EditorType::CombinedSamplesResult,
            combined_samples: None,
            show_combined_samples_button,
            combined_samples_widget,
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

    pub fn set_combined_samples(&mut self, r: Result<usize, CombineSamplesError>) {
        let label_color = match r {
            Ok(_) => Color::Foreground,
            Err(_) => Color::Red,
        };
        if self.group.label_color() != label_color {
            self.group.set_label_color(label_color);
            self.group.redraw_label();
        }

        match &r {
            Ok(_) => {
                self.show_combined_samples_button.set_label("All OK");
                self.show_combined_samples_button
                    .set_label_color(Color::Foreground);
            }
            Err(e) => {
                self.show_combined_samples_button
                    .set_label_color(Color::Red);
                match e {
                    CombineSamplesError::IndividualErrors {
                        n_instrument_errors,
                        n_sample_errors,
                    } => {
                        let n_errors = n_instrument_errors + n_sample_errors;
                        if n_errors == 1 {
                            self.show_combined_samples_button.set_label("1 error");
                        } else {
                            self.show_combined_samples_button
                                .set_label(&format!("{n_errors} errors"));
                        }
                    }
                    CombineSamplesError::CombineError(..) => self
                        .show_combined_samples_button
                        .set_label("Combine samples error"),
                    CombineSamplesError::CommonAudioData(..) => self
                        .show_combined_samples_button
                        .set_label("Common audio data error"),
                    CombineSamplesError::UniqueNamesError(..) => self
                        .show_combined_samples_button
                        .set_label("Unique names error"),
                }
            }
        };

        self.combined_samples = Some(r);

        match &self.selected_editor {
            EditorType::CombinedSamplesResult => self.update_combined_samples_widget_and_console(),
            EditorType::Instrument => (),
            EditorType::Sample => (),
        }
    }

    pub fn show_combined_samples_widget(&mut self) {
        self.selected_editor = EditorType::CombinedSamplesResult;
        self.editor_wizard
            .set_current_widget(&self.combined_samples_widget);
        self.update_combined_samples_widget_and_console();
    }

    fn update_combined_samples_widget_and_console(&mut self) {
        match &self.combined_samples {
            None => {
                self.console_buffer.set_text("");
                self.console.set_text_color(Color::Foreground);
                self.console.scroll(0, 0);
            }
            Some(Ok(size)) => {
                self.console_buffer
                    .set_text(&format!("Total size: {} bytes", size));
                self.console.set_text_color(Color::Foreground);
                self.console.scroll(0, 0);
            }
            Some(Err(e)) => {
                self.console_buffer.set_text(&e.to_string());
                self.console.set_text_color(Color::Red);
                self.console.scroll(0, 0);
            }
        }
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

        if matches!(self.selected_editor, EditorType::Instrument) {
            self.show_combined_samples_widget();
        }
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

        if matches!(self.selected_editor, EditorType::Sample) {
            self.show_combined_samples_widget();
        }
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
