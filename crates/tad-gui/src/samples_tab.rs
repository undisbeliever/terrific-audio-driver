//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CadOutput, CombineSamplesError, ItemId, SampleOutput};
use crate::helpers::*;
use crate::list_editor::{
    ListAction, ListEditorTable, ListMessage, ListWithCompilerOutput, ListWithCompilerOutputEditor,
    TableCompilerOutput, TableMapping,
};
use crate::sample_sizes_widget::SampleSizesWidget;
use crate::sample_widgets::DEFAULT_ENVELOPE;
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use compiler::project;
use compiler::songs::SongAramSize;
use fltk::button::Button;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::enums::Color;
use fltk::group::{Flex, Wizard};
use fltk::prelude::*;
use fltk::text::{TextBuffer, TextDisplay, WrapMode};

fn blank_sample() -> project::BrrSample {
    project::BrrSample {
        name: "name".parse().unwrap(),
        source: project::BrrSampleSource::WaveFile(Default::default()),
        ignore_gaussian_overflow: Default::default(),
        pitches: None,
        envelope: DEFAULT_ENVELOPE,
        comment: String::new(),
    }
}

pub struct SampleMapping;

impl TableMapping for SampleMapping {
    type DataType = project::BrrSample;
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
        GuiMessage::BrrSample(ListMessage::Add(blank_sample()))
    }

    fn to_message(lm: ListMessage<project::BrrSample>) -> GuiMessage {
        GuiMessage::BrrSample(lm)
    }

    fn new_row(i: &project::BrrSample) -> Self::RowType {
        RowWithStatus::new_unchecked(SimpleRow::new([i.name.as_str().to_string()]))
    }

    fn edit_row(r: &mut Self::RowType, i: &project::BrrSample) -> bool {
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

#[derive(PartialEq)]
enum SelectedEditor {
    CombinedSamplesResult,
}

pub struct SamplesTab {
    group: Flex,

    selected_editor: SelectedEditor,
    cad_output: CadOutput,

    sample_sizes_button: Button,
    table: ListEditorTable<SampleMapping>,

    editor_wizard: Wizard,

    sample_sizes_group: Flex,
    sample_sizes_widget: Rc<RefCell<SampleSizesWidget>>,

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
        brr_samples: &ListWithCompilerOutput<project::BrrSample, SampleOutput>,
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

        let table = ListEditorTable::new_with_data(&mut sidebar, brr_samples, sender);

        sidebar.end();

        let mut main_group = Flex::default().column();

        let editor_wizard = Wizard::default().with_size(400, 400);

        let mut sample_sizes_group = Flex::default().column().size_of_parent();
        sample_sizes_group.set_margin(margin);
        let sample_sizes_widget = SampleSizesWidget::new(&mut sample_sizes_group, brr_samples);
        sample_sizes_group.end();

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
            table,
            editor_wizard,
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
                    &CombineSamplesError::IndividualErrors(n_errors) => {
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
        }
    }

    pub fn set_largest_song(&mut self, s: SongAramSize) {
        self.sample_sizes_widget.borrow_mut().set_largest_song(s);
    }

    pub fn show_sample_sizes_widget(&mut self) {
        self.table.clear_selected_row();

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
}

impl ListWithCompilerOutputEditor<project::BrrSample, SampleOutput> for SamplesTab {
    type TableMapping = SampleMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping> {
        &mut self.table
    }

    fn list_edited(&mut self, action: &ListAction<project::BrrSample>) {
        self.sample_sizes_widget.borrow_mut().sample_edited(action);
    }

    fn item_edited(&mut self, _id: ItemId, _value: &project::BrrSample) {}

    fn set_compiler_output(
        &mut self,
        index: usize,
        _id: ItemId,
        compiler_output: &Option<SampleOutput>,
    ) {
        self.sample_sizes_widget
            .borrow_mut()
            .sample_compiled(index, compiler_output);
    }

    fn selected_item_changed(
        &mut self,
        samples: &ListWithCompilerOutput<project::BrrSample, SampleOutput>,
    ) {
        match samples.get_selected_row(&self.table) {
            Some(_) => {}
            None => {
                self.show_sample_sizes_widget();
            }
        }
    }
}
