//! Samples Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CadOutput, CombineSamplesError, ItemId, SampleOutput, ToCompiler};
use crate::drag_and_drop::DragDropFileHandler;
use crate::helpers::*;
use crate::list_editor::{
    ListAction, ListEditorTable, ListMessage, ListWithCompilerOutput, ListWithCompilerOutputEditor,
    TableCompilerOutput, TableMapping,
};
use crate::sample_analyser::{SampleAnalyserWidget, SampleAnalysis};
use crate::sample_sizes_widget::SampleSizesWidget;
use crate::tables::{RowWithStatus, SimpleRow};
use crate::tabs::{FileType, Tab};
use crate::test_sample_widget::TestBrrSampleWidget;
use crate::GuiMessage;

use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::identifier::Name;
use compiler::notes::Note;
use compiler::path::SourcePathBuf;
use compiler::pitch_table::default_octaves_for_tuning_frequency;
use compiler::project::{
    self, BrrEncoderSettings, BrrEvaluator, BrrLoopFilter, BrrSample, BrrSamplePitches,
    BrrSampleSource, BrrSource, SampleNumber, SampleTuning, WaveSource,
};
use compiler::songs::SongAramSize;
use fltk::enums::Event;
use fltk::input::{FloatInput, InputType};
use fltk::output::Output;

use std::cell::RefCell;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;
use std::sync::mpsc;

use fltk::{
    app,
    button::{Button, CheckButton, RadioRoundButton},
    enums::{Align, Color, FrameType},
    frame::Frame,
    group::{Flex, Group, Pack, PackType, Scroll, ScrollType, Wizard},
    input::{Input, IntInput},
    menu::Choice,
    prelude::*,
    text::{TextBuffer, TextDisplay, TextEditor, WrapMode},
};

const DEFAULT_ADSR: Adsr = match Adsr::try_new(12, 2, 2, 15) {
    Ok(a) => a,
    Err(_) => panic!("Invalid ADSR"),
};
const DEFAULT_GAIN: Gain = Gain::new(127);

const DEFAULT_ENVELOPE: Envelope = Envelope::Adsr(DEFAULT_ADSR);

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

pub fn new_sample_from_file(source: SourcePathBuf, tuning_freq: Option<f64>) -> project::BrrSample {
    project::BrrSample {
        name: source
            .file_stem_string()
            .map(|s| Name::new_lossy(s.into()))
            .unwrap_or_else(|| "name".parse().unwrap()),
        source: BrrSampleSource::new_from_source(source),
        ignore_gaussian_overflow: Default::default(),
        pitches: tuning_freq.map(|f| {
            let range = default_octaves_for_tuning_frequency(f);
            BrrSamplePitches::Octaves {
                tuning: SampleTuning::Frequency(f),
                first: *range.start(),
                last: *range.end(),
            }
        }),
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
    SampleEditor,
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

    sample_editor: Rc<RefCell<BrrSampleEditor>>,

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
        compiler_sender: mpsc::Sender<ToCompiler>,
    ) -> Self {
        let mut group = Flex::default_fill().row();
        let margin = ch_units_to_width(&group, 1) / 2;

        // Sidebar
        let mut sidebar = Flex::default().column();
        group.fixed(&sidebar, ch_units_to_width(&sidebar, 30));

        let mut sample_sizes_button = Button::default().with_label("Unchecked");
        sample_sizes_button.set_tooltip("Show sample sizes");
        sidebar.fixed(&sample_sizes_button, ch_units_to_width(&sidebar, 5));

        let mut table = ListEditorTable::new_with_data(&mut sidebar, brr_samples, sender);

        table.add_button_to_start(
            "@fileopen",
            "Open new sample",
            |a| a.can_add(),
            || GuiMessage::OpenNewSampleDialog,
        );

        sidebar.end();

        let mut main_group = Flex::default().column();

        let editor_wizard = Wizard::default().with_size(400, 400);

        let mut sample_sizes_group = Flex::default().column().size_of_parent();
        sample_sizes_group.set_margin(margin);
        let sample_sizes_widget = SampleSizesWidget::new(&mut sample_sizes_group, brr_samples);
        sample_sizes_group.end();

        let sample_editor = BrrSampleEditor::new(sender, compiler_sender);

        editor_wizard.end();

        let mut console = TextDisplay::default();

        let h = input_height(&group);
        main_group.fixed(&console, h * 5 / 2);

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

        // Must be `sidebar`, cannot be `group`.
        //
        // When I add drag+drop onto `group` the `name` Input takes over the drop event.
        // and it ends up overriding the name instead of creating a new sample.
        DragDropFileHandler::add_to_widget(&mut sidebar, sender, GuiMessage::DragAndDropSampleFile);

        Self {
            group,
            selected_editor: SelectedEditor::CombinedSamplesResult,
            cad_output: CadOutput::None,
            sample_sizes_button,
            sample_sizes_group,
            sample_sizes_widget,
            sample_editor,
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
            SelectedEditor::SampleEditor => (),
        }
    }

    pub fn set_largest_song(&mut self, s: SongAramSize) {
        self.sample_sizes_widget.borrow_mut().set_largest_song(s);
    }

    fn show_sample_editor(&mut self) {
        self.selected_editor = SelectedEditor::SampleEditor;
        let _ = self.editor_wizard.set_index(1);
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

    fn update_compiler_output(&mut self, co: &Option<SampleOutput>) {
        match co {
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

    pub fn analysis_from_compiler_thread(
        &mut self,
        sample_id: ItemId,
        analysis: Option<SampleAnalysis>,
    ) {
        self.sample_editor
            .borrow_mut()
            .sample_analyser
            .analysis_from_compiler_thread(sample_id, analysis);
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

    fn item_edited(&mut self, id: ItemId, value: &project::BrrSample) {
        let mut e = self.sample_editor.borrow_mut();
        if e.is_id_selected(id) {
            e.set_data_and_update_widget(id, value);
        }
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

        if self.sample_editor.borrow().is_id_selected(id) {
            self.sample_editor
                .borrow_mut()
                .selected_compiler_output_changed(compiler_output.as_ref());

            self.update_compiler_output(compiler_output);
        }
    }

    fn selected_item_changed(
        &mut self,
        samples: &ListWithCompilerOutput<project::BrrSample, SampleOutput>,
    ) {
        match samples.get_selected_row(&self.table) {
            Some((item_id, s, co)) => {
                {
                    let mut e = self.sample_editor.borrow_mut();

                    e.set_data_and_update_widget(item_id, s);
                    e.selected_compiler_output_changed(co.as_ref());
                }

                self.show_sample_editor();

                self.update_compiler_output(co);
            }
            None => {
                self.sample_editor.borrow_mut().clear_data();

                self.show_sample_sizes_widget();
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum LoopFilterChoice {
    ResetFilter = 0,
    Auto = 1,
    Filter1 = 2,
    Filter2 = 3,
    Filter3 = 4,
}
impl LoopFilterChoice {
    pub const CHOICES: &'static str = concat![
        "&Reset filter",
        "|&Auto",
        "|BRR filter &1",
        "|BRR filter &2",
        "|BRR filter &3",
    ];

    pub fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::ResetFilter,
            1 => Self::Auto,
            2 => Self::Filter1,
            3 => Self::Filter2,
            4 => Self::Filter3,

            _ => Self::ResetFilter,
        }
    }

    pub const fn to_i32(self) -> i32 {
        self as i32
    }

    pub fn from_data(lf: BrrLoopFilter) -> Self {
        match lf {
            BrrLoopFilter::Reset => Self::ResetFilter,
            BrrLoopFilter::Auto => Self::Auto,
            BrrLoopFilter::Filter1 => Self::Filter1,
            BrrLoopFilter::Filter2 => Self::Filter2,
            BrrLoopFilter::Filter3 => Self::Filter3,
        }
    }

    pub fn to_data(self) -> BrrLoopFilter {
        match self {
            Self::ResetFilter => BrrLoopFilter::Reset,
            Self::Auto => BrrLoopFilter::Auto,
            Self::Filter1 => BrrLoopFilter::Filter1,
            Self::Filter2 => BrrLoopFilter::Filter2,
            Self::Filter3 => BrrLoopFilter::Filter3,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum BrrEvaluatorChoice {
    Default = 0,
    SquaredError = 1,
    SquaredErrorAvoidGaussianOverflow = 2,
}
impl BrrEvaluatorChoice {
    const _DEFAULT_MATCHES: () = assert!(matches!(
        brr::DEFAULT_EVALUATOR,
        brr::Evaluator::SquaredErrorAvoidGaussianOverflow
    ));

    const CHOICES: &'static str = concat![
        "&Default (avoid Gaussian overflow)",
        "|&Squared Error",
        "|&Avoid Gaussian Overflow (Squared Error)",
    ];

    fn read_widget(c: &Choice) -> Self {
        match c.value() {
            0 => Self::Default,
            1 => Self::SquaredError,
            2 => Self::SquaredErrorAvoidGaussianOverflow,

            _ => Self::Default,
        }
    }

    fn to_i32(self) -> i32 {
        self as i32
    }

    fn to_data(self) -> BrrEvaluator {
        match self {
            Self::Default => BrrEvaluator::Default,
            Self::SquaredError => BrrEvaluator::SquaredError,
            Self::SquaredErrorAvoidGaussianOverflow => {
                BrrEvaluator::SquaredErrorAvoidGaussianOverflow
            }
        }
    }

    fn from_data(e: BrrEvaluator) -> Self {
        match e {
            BrrEvaluator::Default => Self::Default,
            BrrEvaluator::SquaredError => Self::SquaredError,
            BrrEvaluator::SquaredErrorAvoidGaussianOverflow => {
                Self::SquaredErrorAvoidGaussianOverflow
            }
        }
    }
}

struct BrrEncoderSettingsEditor {
    group: Group,

    looping_cb: CheckButton,
    loop_point: IntInput,
    loop_filter: Choice,
    dupe_block_hack: IntInput,

    evaluator: Choice,
    ignore_gaussian_overflow: CheckButton,
}

impl BrrEncoderSettingsEditor {
    fn disable(&mut self, ignore_gaussian_overflow: bool) {
        self.group.deactivate();

        self.looping_cb.clear();
        self.loop_point.deactivate();
        self.loop_filter.deactivate();
        self.dupe_block_hack.deactivate();

        self.loop_point.set_value("");
        self.loop_filter.set_value(-1);
        self.dupe_block_hack.set_value("");
        self.evaluator.set_value(-1);
        self.ignore_gaussian_overflow.set(ignore_gaussian_overflow);
    }

    fn update(&mut self, value: &BrrEncoderSettings, ignore_gaussian_overflow: bool) {
        self.group.activate();

        let looping = value.loop_point.is_some()
            || value.loop_filter.is_some()
            || value.dupe_block_hack.is_some();

        self.looping_cb.set(looping);

        self.loop_point.set_active(looping);
        self.loop_filter.set_active(looping);
        self.dupe_block_hack.set_active(looping);

        InputHelper::set_widget_value(&mut self.loop_point, &value.loop_point);
        self.loop_filter.set_value(match value.loop_filter {
            Some(lf) => LoopFilterChoice::from_data(lf).to_i32(),
            None => -1,
        });
        InputHelper::set_widget_value(&mut self.dupe_block_hack, &value.dupe_block_hack);

        self.evaluator
            .set_value(BrrEvaluatorChoice::from_data(value.evaluator).to_i32());

        self.ignore_gaussian_overflow.set(ignore_gaussian_overflow);
    }

    fn read_or_reset(&mut self, old: &BrrEncoderSettings) -> Option<BrrEncoderSettings> {
        let evaluator = BrrEvaluatorChoice::read_widget(&self.evaluator).to_data();

        match self.looping_cb.value() {
            true => {
                let loop_point = InputHelper::read_or_reset(&mut self.loop_point, &old.loop_point);

                let mut loop_filter = LoopFilterChoice::read_widget(&self.loop_filter).to_data();

                let dupe_block_hack =
                    InputHelper::read_or_reset(&mut self.dupe_block_hack, &old.dupe_block_hack);

                if dupe_block_hack.flatten().is_some_and(|b| b.0 > 0)
                    && loop_filter == BrrLoopFilter::Reset
                {
                    self.loop_filter.set_value(LoopFilterChoice::Auto.to_i32());
                    loop_filter = BrrLoopFilter::Auto;
                }

                Some(BrrEncoderSettings {
                    evaluator,
                    loop_point: Some(loop_point?.unwrap_or(SampleNumber(0))),
                    loop_filter: Some(loop_filter),
                    dupe_block_hack: dupe_block_hack?.filter(|dbh| dbh.0 != 0),
                })
            }
            false => Some(BrrEncoderSettings {
                evaluator,
                loop_point: None,
                loop_filter: None,
                dupe_block_hack: None,
            }),
        }
    }

    fn on_loop_filter_edited(&mut self) {
        if self.loop_filter.value() == LoopFilterChoice::ResetFilter.to_i32() {
            self.dupe_block_hack.set_value("");
        }
    }
}

struct BrrSourceEditor {
    group: Group,

    looping_cb: CheckButton,
    override_loop_point: IntInput,

    ignore_gaussian_overflow: CheckButton,
}

impl BrrSourceEditor {
    fn disable(&mut self, ignore_gaussian_overflow: bool) {
        self.group.deactivate();

        self.looping_cb.clear();
        self.override_loop_point.deactivate();

        self.override_loop_point.set_value("");

        self.ignore_gaussian_overflow.set(ignore_gaussian_overflow);
    }

    fn update(&mut self, s: &BrrSource, ignore_gaussian_overflow: bool) {
        self.group.activate();

        let override_lp = s.loop_point.is_some();

        self.looping_cb.set(override_lp);
        self.override_loop_point.set_active(override_lp);

        InputHelper::set_widget_value(&mut self.override_loop_point, &s.loop_point);
        self.ignore_gaussian_overflow.set(ignore_gaussian_overflow);
    }

    fn read_or_reset(&mut self, old: &BrrSource) -> Option<BrrSampleSource> {
        match self.looping_cb.value() {
            true => {
                let loop_point =
                    InputHelper::read_or_reset(&mut self.override_loop_point, &old.loop_point);

                Some(BrrSampleSource::BrrFile(BrrSource {
                    loop_point: Some(loop_point?.unwrap_or(SampleNumber(0))),

                    // Must be last
                    source: old.source.clone(),
                }))
            }
            false => {
                Some(BrrSampleSource::BrrFile(BrrSource {
                    loop_point: None,
                    // Must be last
                    source: old.source.clone(),
                }))
            }
        }
    }
}

struct BrrSampleSourceEditor {
    source_fn: Output,
    open_source: Button,

    wav_source_rb: RadioRoundButton,
    brr_source_rb: RadioRoundButton,

    wav_settings: BrrEncoderSettingsEditor,
    brr_settings: BrrSourceEditor,

    // Used to restore the BRR encoder settings when switching
    // the source type from Wave -> BRR -> Wave
    old_brr_encoder_settings: HashMap<ItemId, BrrEncoderSettings>,
}

impl BrrSampleSourceEditor {
    fn clear(&mut self) {
        self.wav_source_rb.clear();
        self.brr_source_rb.clear();

        self.source_fn.set_value("");

        self.wav_settings.disable(false);
        self.wav_settings.group.show();

        self.brr_settings.disable(false);
        self.brr_settings.group.hide();
    }

    fn update(&mut self, id: ItemId, source: &BrrSampleSource, ignore_gaussian_overflow: bool) {
        match source {
            BrrSampleSource::WaveFile(w) => {
                self.wav_source_rb.set(true);
                self.brr_source_rb.clear();

                self.source_fn.set_value(w.source.as_str());

                self.wav_settings
                    .update(&w.settings, ignore_gaussian_overflow);

                self.wav_settings.group.show();
                self.brr_settings.group.hide();

                // Have to store them here as the "open sample file" dialog
                // can erase the BrrEncoderSettings.
                self.old_brr_encoder_settings.insert(id, w.settings.clone());
            }
            BrrSampleSource::BrrFile(b) => {
                self.brr_source_rb.set(true);
                self.wav_source_rb.clear();

                self.source_fn.set_value(b.source.as_str());

                self.brr_settings.update(b, ignore_gaussian_overflow);

                self.wav_settings.group.hide();
                self.brr_settings.group.show();
            }
        }
    }

    fn read_or_reset(&mut self, old: &BrrSampleSource) -> (Option<BrrSampleSource>, bool) {
        match old {
            BrrSampleSource::WaveFile(old) => (
                self.wav_settings
                    .read_or_reset(&old.settings)
                    .map(|settings| {
                        BrrSampleSource::WaveFile(project::WaveSource {
                            settings,
                            source: old.source.clone(),
                        })
                    }),
                self.wav_settings.ignore_gaussian_overflow.value(),
            ),
            BrrSampleSource::BrrFile(old) => (
                self.brr_settings.read_or_reset(old),
                self.brr_settings.ignore_gaussian_overflow.value(),
            ),
        }
    }

    fn source_type_changed(&self, id: ItemId, old: &BrrSampleSource) -> Option<BrrSampleSource> {
        match (self.wav_source_rb.value(), self.brr_source_rb.value()) {
            (true, false) => match old {
                BrrSampleSource::WaveFile(_) => None,
                BrrSampleSource::BrrFile(b) => match self.old_brr_encoder_settings.get(&id) {
                    Some(s) => Some(BrrSampleSource::WaveFile(WaveSource {
                        source: b.source.clone(),
                        settings: s.clone(),
                    })),
                    None => Some(BrrSampleSource::WaveFile(old.to_wave_source())),
                },
            },
            (false, true) => Some(BrrSampleSource::BrrFile(old.to_brr_source())),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
enum EditTuningField {
    TuningFrequency,
    TuningWavelength,
}

struct OldPitchValues {
    tuning: SampleTuning,
    first_note: Note,
    last_note: Note,
    sample_rates: Vec<u32>,
}

impl Default for OldPitchValues {
    fn default() -> Self {
        Self {
            tuning: SampleTuning::Frequency(500.0),
            first_note: Note::C3,
            last_note: Note::B6,
            sample_rates: Vec::new(),
        }
    }
}

struct PitchesEditor {
    octaves_rb: RadioRoundButton,
    notes_rb: RadioRoundButton,
    sample_rates_rb: RadioRoundButton,

    tuning_group: Group,

    tuning_frequency: FloatInput,
    tuning_wavelength: FloatInput,

    range_from: Input,
    range_to: Input,

    sample_rates: TextEditor,
    sample_rates_buffer: TextBuffer,

    // Used to restore the pitch fields when changing the pitch type
    // from Octaves/Notes to Sample-Rates and back again.
    old_values: HashMap<ItemId, OldPitchValues>,
}

impl PitchesEditor {
    fn clear(&mut self) {
        self.octaves_rb.clear();
        self.notes_rb.clear();
        self.sample_rates_rb.clear();

        self.tuning_group.hide();
        self.sample_rates.hide();
    }

    fn update(&mut self, value: &Option<BrrSamplePitches>) {
        // Always clear changed field (even when using sample-rates)
        self.tuning_frequency.clear_changed();
        self.tuning_wavelength.clear_changed();

        match value {
            Some(BrrSamplePitches::Octaves {
                tuning,
                first,
                last,
            }) => {
                self.octaves_rb.set(true);
                self.notes_rb.clear();
                self.sample_rates_rb.clear();

                self.range_from.set_type(InputType::Int);
                self.range_to.set_type(InputType::Int);

                self.update_tuning(tuning);

                InputHelper::set_widget_value(&mut self.range_from, first);
                InputHelper::set_widget_value(&mut self.range_to, last);

                self.sample_rates.hide();
                self.tuning_group.show();
            }
            Some(BrrSamplePitches::Notes {
                tuning,
                first,
                last,
            }) => {
                self.notes_rb.set(true);
                self.octaves_rb.clear();
                self.sample_rates_rb.clear();

                self.range_from.set_type(InputType::Normal);
                self.range_to.set_type(InputType::Normal);

                self.update_tuning(tuning);

                InputHelper::set_widget_value(&mut self.range_from, first);
                InputHelper::set_widget_value(&mut self.range_to, last);

                self.sample_rates.hide();
                self.tuning_group.show();
            }

            Some(BrrSamplePitches::SampleRates { sample_rates }) => {
                self.sample_rates_rb.set(true);
                self.octaves_rb.clear();
                self.notes_rb.clear();

                self.sample_rates_buffer
                    .set_text(&Self::sample_rates_string(sample_rates));

                self.tuning_group.hide();
                self.sample_rates.show();
            }

            None => self.clear(),
        }
    }

    fn update_tuning(&mut self, tuning: &SampleTuning) {
        InputHelper::set_widget_value(&mut self.tuning_frequency, &tuning.frequency());
        InputHelper::set_widget_value(&mut self.tuning_wavelength, &tuning.wavelength());
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

    fn read_or_reset(
        &mut self,
        old: &Option<BrrSamplePitches>,
        edit_type: Option<EditTuningField>,
    ) -> Option<Option<BrrSamplePitches>> {
        match old {
            Some(BrrSamplePitches::Octaves {
                tuning,
                first,
                last,
            }) => {
                let tuning = self.read_or_reset_tuning(tuning, edit_type);
                let first = InputHelper::read_or_reset(&mut self.range_from, first);
                let last = InputHelper::read_or_reset(&mut self.range_to, last);

                Some(Some(BrrSamplePitches::Octaves {
                    tuning: tuning?,
                    first: first?,
                    last: last?,
                }))
            }
            Some(BrrSamplePitches::Notes {
                tuning,
                first,
                last,
            }) => {
                let tuning = self.read_or_reset_tuning(tuning, edit_type);
                let first = InputHelper::read_or_reset(&mut self.range_from, first);
                let last = InputHelper::read_or_reset(&mut self.range_to, last);

                Some(Some(BrrSamplePitches::Notes {
                    tuning: tuning?,
                    first: first?,
                    last: last?,
                }))
            }
            Some(BrrSamplePitches::SampleRates { sample_rates }) => {
                self.read_or_reset_sample_rates(sample_rates)
            }
            None => Some(None),
        }
    }

    fn read_or_reset_tuning(
        &mut self,
        old: &SampleTuning,
        edit_type: Option<EditTuningField>,
    ) -> Option<SampleTuning> {
        match edit_type {
            Some(EditTuningField::TuningFrequency) => {
                if self.tuning_frequency.changed() {
                    InputHelper::read_or_reset(&mut self.tuning_frequency, &old.frequency())
                        .map(SampleTuning::Frequency)
                } else {
                    Some(old.clone())
                }
            }
            Some(EditTuningField::TuningWavelength) => {
                if self.tuning_wavelength.changed() {
                    InputHelper::read_or_reset(&mut self.tuning_wavelength, &old.wavelength())
                        .map(SampleTuning::Wavelength)
                } else {
                    Some(old.clone())
                }
            }
            None => Some(old.clone()),
        }
    }

    fn read_or_reset_sample_rates(&mut self, old: &[u32]) -> Option<Option<BrrSamplePitches>> {
        let mut out = Vec::new();

        for s in self.sample_rates_buffer.text().split_whitespace() {
            match s.parse() {
                Ok(s) => out.push(s),
                Err(_) => {
                    self.sample_rates_buffer
                        .set_text(&Self::sample_rates_string(old));
                    return None;
                }
            }
        }

        Some(Some(BrrSamplePitches::SampleRates { sample_rates: out }))
    }

    fn pitches_type_changed(
        &mut self,
        id: ItemId,
        old: &Option<BrrSamplePitches>,
    ) -> Option<BrrSamplePitches> {
        let old_values = self.old_values.entry(id).or_default();

        match old {
            Some(BrrSamplePitches::Octaves {
                tuning,
                first,
                last,
            }) => {
                old_values.tuning = tuning.clone();
                old_values.first_note = Note::first_note_for_octave(*first);
                old_values.last_note = Note::last_note_for_octave(*last);
            }
            Some(BrrSamplePitches::Notes {
                tuning,
                first,
                last,
            }) => {
                old_values.tuning = tuning.clone();
                old_values.first_note = *first;
                old_values.last_note = *last;
            }
            Some(BrrSamplePitches::SampleRates { sample_rates }) => {
                old_values.sample_rates = sample_rates.clone();
            }
            None => (),
        }

        match (
            self.octaves_rb.value(),
            self.notes_rb.value(),
            self.sample_rates_rb.value(),
        ) {
            (true, false, false) => Some(BrrSamplePitches::Octaves {
                tuning: old_values.tuning.clone(),
                first: old_values.first_note.octave(),
                last: old_values.last_note.octave(),
            }),
            (false, true, false) => Some(BrrSamplePitches::Notes {
                tuning: old_values.tuning.clone(),
                first: old_values.first_note,
                last: old_values.last_note,
            }),
            (false, false, true) => Some(BrrSamplePitches::SampleRates {
                sample_rates: old_values.sample_rates.clone(),
            }),
            _ => None,
        }
    }
}

struct OldEnvelopeValues {
    adsr: Adsr,
    gain: Gain,
}

impl Default for OldEnvelopeValues {
    fn default() -> Self {
        Self {
            adsr: DEFAULT_ADSR,
            gain: DEFAULT_GAIN,
        }
    }
}

struct EnvelopeEditor {
    adsr_rb: RadioRoundButton,
    adsr: Input,
    gain_rb: RadioRoundButton,
    gain: Input,

    // Used to restore previous ADSR or GAIN values
    // when changing envelope type.
    old_values: HashMap<ItemId, OldEnvelopeValues>,
}

impl EnvelopeEditor {
    fn clear(&mut self) {
        self.adsr_rb.clear();
        self.gain_rb.clear();

        self.adsr.set_value("");
        self.gain.set_value("");
    }

    fn update(&mut self, value: &Envelope) {
        match value {
            Envelope::Adsr(adsr) => {
                self.adsr_rb.set(true);
                self.gain_rb.clear();

                InputHelper::set_widget_value(&mut self.adsr, adsr);
                self.adsr.activate();

                self.gain.set_value("");
                self.gain.deactivate();
            }
            Envelope::Gain(gain) => {
                self.gain_rb.set(true);
                self.adsr_rb.clear();

                InputHelper::set_widget_value(&mut self.gain, gain);
                self.gain.activate();

                self.adsr.set_value("");
                self.adsr.deactivate();
            }
        }
    }

    fn read_or_reset(&mut self, old: &Envelope) -> Envelope {
        match self.gain_rb.value() {
            false => Envelope::Adsr(match old {
                Envelope::Adsr(adsr) => {
                    InputHelper::read_or_reset(&mut self.adsr, adsr).unwrap_or(*adsr)
                }
                Envelope::Gain(_) => DEFAULT_ADSR,
            }),
            true => Envelope::Gain(match old {
                Envelope::Gain(gain) => {
                    InputHelper::read_or_reset(&mut self.gain, gain).unwrap_or(*gain)
                }
                Envelope::Adsr(_) => DEFAULT_GAIN,
            }),
        }
    }

    fn envelope_type_changed(&mut self, id: ItemId, old: &Envelope) -> Envelope {
        let old_value = self.old_values.entry(id).or_default();

        match old {
            Envelope::Adsr(adsr) => old_value.adsr = *adsr,
            Envelope::Gain(gain) => old_value.gain = *gain,
        }

        match self.gain_rb.value() {
            false => Envelope::Adsr(old_value.adsr),
            true => Envelope::Gain(old_value.gain),
        }
    }
}

struct BrrSampleEditor {
    sender: app::Sender<GuiMessage>,
    data: Option<(ItemId, BrrSample)>,

    scroll: Scroll,
    scrollgroup: Group,

    name: Input,
    sample_source: BrrSampleSourceEditor,

    pitches: PitchesEditor,
    envelope: EnvelopeEditor,

    comment_buffer: TextBuffer,
    comment: TextEditor,

    sample_analyser: SampleAnalyserWidget,
    test_sample_widget: TestBrrSampleWidget,
}

impl BrrSampleEditor {
    fn new(
        sender: app::Sender<GuiMessage>,
        compiler_sender: mpsc::Sender<ToCompiler>,
    ) -> Rc<RefCell<Self>> {
        let scroll = Scroll::default()
            .with_type(ScrollType::Vertical)
            .size_of_parent();

        let h = input_height(&scroll);
        let s = 3; // spacing
        let r = h + s;

        let c0 = ch_units_to_width(&scroll, 1);
        let c1 = ch_units_to_width(&scroll, 21);
        let c2 = ch_units_to_width(&scroll, 30);
        let c3 = ch_units_to_width(&scroll, 65 - 5);
        let c4 = ch_units_to_width(&scroll, 65);
        let c5 = ch_units_to_width(&scroll, 66);

        let outer_w = c5 - c1;
        let inner_w = c4 - c1;

        let editor_width = c5 + c0;
        let editor_height = 22 * r;

        let flex_height = 18 * r;

        let mut scrollgroup = Group::default().with_size(editor_width, r * 40 + s);
        scrollgroup.make_resizable(false);

        let editor_group = Group::new(0, 0, editor_width, editor_height, None);

        let name = Input::new(c1, 0, outer_w, h, "Name: ");

        let sample_source = {
            let y = 2 * r;
            let radio_w = ch_units_to_width(&scroll, 12);

            let mut g = Group::new(c0, y - 2 * s, c5 - c0, 8 * r + 2 * s, "Sample:");
            g.set_frame(FrameType::EngravedBox);
            g.set_align(Align::TopLeft);

            let source_fn = Output::new(c1, y, c3 - c1 - s, h, "Filename: ");
            let open_source = Button::new(c3, y, c4 - c3, h, "...");

            let p = Pack::new(c1, y + r, inner_w, h, None).with_type(PackType::Horizontal);
            let wav_source_rb = RadioRoundButton::new(0, 0, radio_w, h, "Wave file");
            let brr_source_rb = RadioRoundButton::new(0, 0, radio_w, h, "BRR file");
            p.end();

            let wav_settings = {
                let y = y + 2 * r;
                let g = Group::new(0, y, c4 - c0, 6 * r, None);

                let looping_cb = CheckButton::new(c1, y, c4 - c1, h, "Looping sample");
                let mut loop_point = IntInput::new(c2, y + r, c4 - c2, h, "Loop point: ");
                loop_point.set_tooltip("Loop point in samples (must be a multiple of 16)");
                let mut loop_filter = Choice::new(c2, y + 2 * r, c4 - c2, h, "Loop filter: ");
                loop_filter.set_tooltip("BRR Filter to use at the loop point");
                loop_filter.add_choice(LoopFilterChoice::CHOICES);

                let mut dupe_block_hack =
                    IntInput::new(c2, y + 3 * r, c4 - c2, h, "Dupe block hack: ");
                dupe_block_hack.set_tooltip(concat![
                    "Number of blocks to duplicate at the end of the sample.\n",
                    "May improve the quality of small looping samples.\n",
                    "Can add low-frequency oscillation or noise to the sample.",
                ]);

                let mut evaluator = Choice::new(c1, y + 4 * r, inner_w, h, "Evaluator: ");
                evaluator
                    .set_tooltip("wav2brr evaluator to use when scoring BRR filters and nibbles");
                evaluator.add_choice(BrrEvaluatorChoice::CHOICES);

                let mut ignore_gaussian_overflow =
                    CheckButton::new(c1, y + 5 * r, c4 - c1, h, "Ignore Gaussian overflow");
                ignore_gaussian_overflow.set_tooltip(
                    "Allow glitch samples that can overflow the Gaussian interpolator",
                );

                g.end();

                BrrEncoderSettingsEditor {
                    group: g,
                    looping_cb,
                    loop_point,
                    loop_filter,
                    dupe_block_hack,
                    evaluator,
                    ignore_gaussian_overflow,
                }
            };

            let brr_settings = {
                let y = y + 2 * r;
                let mut g = Group::new(0, y, c4 - c0, 3 * r, None);

                let override_loop_cb = CheckButton::new(c1, y, c4 - c1, h, "Override loop point");
                let loop_point = IntInput::new(c2, y + r, c4 - c2, h, "Loop point: ");

                let ignore_gaussian_overflow =
                    CheckButton::new(c1, y + 2 * r, c4 - c1, h, "Ignore Gaussian overflow");

                g.end();
                g.hide();

                BrrSourceEditor {
                    group: g,
                    looping_cb: override_loop_cb,
                    override_loop_point: loop_point,
                    ignore_gaussian_overflow,
                }
            };

            g.end();

            BrrSampleSourceEditor {
                source_fn,
                open_source,
                wav_source_rb,
                brr_source_rb,
                wav_settings,
                brr_settings,

                old_brr_encoder_settings: HashMap::new(),
            }
        };

        let pitches = {
            let y = 11 * r;
            let radio_w = ch_units_to_width(&scroll, 15);

            let mut g = Group::new(c0, y - 2 * s, c5 - c0, 4 * r + 3 * s, "Pitches:");
            g.set_frame(FrameType::EngravedBox);
            g.set_align(Align::TopLeft);

            let p = Pack::new(c0 * 2, y, c4 - c0 * 2, h, None).with_type(PackType::Horizontal);
            let octaves_rb = RadioRoundButton::new(0, 0, radio_w, h, "Octaves");
            let notes_rb = RadioRoundButton::new(0, 0, radio_w, h, "Notes");
            let sample_rates_rb = RadioRoundButton::new(0, 0, radio_w, h, "Sample Rates");
            p.end();

            let label_w = ch_units_to_width(&scroll, 8);
            let input_w = c4 - c1 - label_w;

            let tuning_group = Group::new(c0, y + r, c5 - c0, 3 * h, None);

            let mut tuning_frequency = FloatInput::new(c1, y + r, input_w, h, "Tuning frequency: ");
            // Without `\n`, the tooltip wraps at the word rate (sample \n rate)
            tuning_frequency
                .set_tooltip("Frequency of the sample when played at\na 32000Hz sample rate");
            Frame::new(c4 - label_w, y + r, label_w, h, " Hz")
                .with_align(Align::Inside | Align::Left);

            let tuning_wavelength =
                FloatInput::new(c1, y + 2 * r, input_w, h, "Tuning wavelength: ");
            Frame::new(c4 - label_w, y + 2 * r, label_w, h, " samples")
                .with_align(Align::Inside | Align::Left);

            let to_label = ch_units_to_width(&scroll, 3) + s;
            let range_w = (input_w - to_label) / 2;

            let range_from = Input::new(c1, y + 3 * r, range_w, h, "Range: ");
            let range_to = Input::new(c1 + input_w - range_w, y + 3 * r, range_w, h, "to ");

            tuning_group.end();

            let sample_rates_buffer = TextBuffer::default();
            let mut sample_rates =
                TextEditor::new(c1, y + r, inner_w, h * 2 + h / 2, "Sample Rates: ")
                    .with_align(Align::Left);
            sample_rates.set_buffer(sample_rates_buffer.clone());
            sample_rates.set_tab_nav(true);
            sample_rates.wrap_mode(WrapMode::AtBounds, 0);
            sample_rates.hide();

            g.end();

            PitchesEditor {
                octaves_rb,
                notes_rb,
                sample_rates_rb,
                tuning_group,
                tuning_frequency,
                tuning_wavelength,
                range_from,
                range_to,
                sample_rates,
                sample_rates_buffer,

                old_values: HashMap::new(),
            }
        };

        let envelope = {
            let y = 16 * r;
            let rb_w = ch_units_to_width(&scroll, 10);
            let rb_x = c1 - rb_w;

            let mut g = Group::new(c0, y - 2 * s, c5 - c0, 2 * r + 3 * s, "Envelope:");
            g.set_frame(FrameType::EngravedBox);
            g.set_align(Align::TopLeft);

            let adsr_rb = RadioRoundButton::new(rb_x, y, rb_w, h, "ADSR");
            let adsr = Input::new(c1, y, inner_w, h, None);

            let gain_rb = RadioRoundButton::new(rb_x, y + r, rb_w, h, "GAIN");
            let gain = Input::new(c1, y + r, inner_w, h, None);

            g.end();

            EnvelopeEditor {
                adsr_rb,
                adsr,
                gain_rb,
                gain,

                old_values: HashMap::new(),
            }
        };

        let comment_buffer = TextBuffer::default();
        let mut comment = TextEditor::new(c0, r * 19, c5 - c0, h * 3, "Comment:");
        comment.set_buffer(comment_buffer.clone());
        comment.set_align(Align::TopLeft);
        comment.set_tab_nav(true);
        comment.wrap_mode(WrapMode::AtBounds, 0);

        editor_group.end();

        let mut flex = Flex::new(0, editor_height, editor_width, flex_height, None).column();
        flex.set_margin(c0);
        flex.set_pad(c0 / 2);

        let test_sample_widget = TestBrrSampleWidget::new(&mut flex, sender);

        let sample_analyser =
            SampleAnalyserWidget::new(&mut flex, editor_width - c0 * 2, sender, compiler_sender);

        flex.end();

        scrollgroup.end();

        scroll.end();

        let out = Rc::new(RefCell::new(Self {
            sender,

            data: None,

            scroll,
            scrollgroup,
            name,
            sample_source,
            pitches,
            envelope,
            comment_buffer,
            comment,

            sample_analyser,
            test_sample_widget,
        }));

        {
            let mut editor = out.borrow_mut();

            editor.clear_data();

            let in_callback = |w: &mut Input| {
                w.handle({
                    let e = out.clone();
                    move |_widget, ev| Self::widget_event_handler(&e, ev)
                })
            };
            let it_callback = |w: &mut IntInput| {
                w.handle({
                    let e = out.clone();
                    move |_widget, ev| Self::widget_event_handler(&e, ev)
                })
            };
            let tf_callback = |w: &mut FloatInput, field: EditTuningField| {
                w.handle({
                    let e = out.clone();
                    move |_widget, ev| Self::tuning_widget_event_handler(&e, ev, field)
                })
            };
            let ed_callback = |w: &mut TextEditor| {
                w.handle({
                    let e = out.clone();
                    move |_widget, ev| Self::widget_event_handler(&e, ev)
                })
            };
            let cb_callback = |cb: &mut CheckButton| {
                cb.set_callback({
                    let e = out.clone();
                    move |_widget| e.borrow_mut().on_finished_editing(None)
                })
            };
            let ch_callback = |w: &mut Choice| {
                w.set_callback({
                    let e = out.clone();
                    move |_widget| e.borrow_mut().on_finished_editing(None)
                })
            };

            in_callback(&mut editor.name);
            cb_callback(&mut editor.sample_source.wav_settings.looping_cb);
            it_callback(&mut editor.sample_source.wav_settings.loop_point);
            editor.sample_source.wav_settings.loop_filter.set_callback({
                let o = out.clone();
                move |_| o.borrow_mut().on_loop_filter_edited()
            });
            it_callback(&mut editor.sample_source.wav_settings.dupe_block_hack);
            ch_callback(&mut editor.sample_source.wav_settings.evaluator);
            cb_callback(&mut editor.sample_source.wav_settings.ignore_gaussian_overflow);

            cb_callback(&mut editor.sample_source.brr_settings.looping_cb);
            it_callback(&mut editor.sample_source.brr_settings.override_loop_point);
            cb_callback(&mut editor.sample_source.brr_settings.ignore_gaussian_overflow);

            tf_callback(
                &mut editor.pitches.tuning_frequency,
                EditTuningField::TuningFrequency,
            );
            tf_callback(
                &mut editor.pitches.tuning_wavelength,
                EditTuningField::TuningWavelength,
            );
            in_callback(&mut editor.pitches.range_from);
            in_callback(&mut editor.pitches.range_to);
            ed_callback(&mut editor.pitches.sample_rates);

            in_callback(&mut editor.envelope.adsr);
            in_callback(&mut editor.envelope.gain);

            ed_callback(&mut editor.comment);

            editor.sample_source.open_source.set_callback({
                let e = out.clone();
                move |_| e.borrow().on_source_button_clicked()
            });

            let cb = {
                let e = out.clone();
                move |_: &mut RadioRoundButton| e.borrow().on_source_type_changed()
            };
            editor.sample_source.wav_source_rb.set_callback(cb.clone());
            editor.sample_source.brr_source_rb.set_callback(cb);

            let cb = {
                let e = out.clone();
                move |_: &mut RadioRoundButton| e.borrow_mut().on_pitches_type_changed()
            };
            editor.pitches.octaves_rb.set_callback(cb.clone());
            editor.pitches.notes_rb.set_callback(cb.clone());
            editor.pitches.sample_rates_rb.set_callback(cb);

            let cb = {
                let e = out.clone();
                move |_: &mut RadioRoundButton| e.borrow_mut().on_envelope_type_changed()
            };
            editor.envelope.adsr_rb.set_callback(cb.clone());
            editor.envelope.gain_rb.set_callback(cb.clone());

            // Resize callback to move flex to the right of editor_group if the window is wide enough.
            {
                let mut scrollgroup = editor.scrollgroup.clone();
                let mut flex = flex;
                let editor_group = editor_group;

                let scrollbar_width = editor.scroll.scrollbar().width();

                let orig_flex_width = editor_width;
                let original_scroll_height = scrollgroup.height();

                editor.scroll.resize_callback({
                    move |scroll, _x, _y, sw, sh| {
                        if sw > orig_flex_width * 2 {
                            let x = editor_group.x() + editor_width;
                            let y = editor_group.y();

                            let w = if sh <= editor_height {
                                sw - scrollbar_width
                            } else {
                                sw
                            };
                            let h = max(sh, editor_height);

                            scrollgroup.set_size(w, h);
                            flex.resize(x, y, w - editor_width, h);

                            if scroll.yposition() + sh > editor_height {
                                scroll.scroll_to(0, 0);
                            }
                        } else {
                            let x = editor_group.x();
                            let y = editor_group.y() + editor_height;
                            let w = sw - scrollbar_width;

                            scrollgroup.set_size(w, original_scroll_height);
                            flex.resize(x, y, w, flex_height);
                        }
                    }
                });
            }
        }
        out
    }

    fn is_id_selected(&self, id: ItemId) -> bool {
        self.data.as_ref().is_some_and(|(sid, _)| id == *sid)
    }

    fn clear_data(&mut self) {
        self.data = None;

        self.name.set_value("");
        self.sample_source.clear();
        self.pitches.clear();
        self.envelope.clear();
        self.comment_buffer.set_text("");

        self.sample_analyser.clear_and_deactivate();
        self.test_sample_widget.clear_selected();

        self.scrollgroup.deactivate();
    }

    fn set_data_and_update_widget(&mut self, id: ItemId, data: &BrrSample) {
        self.data = Some((id, data.clone()));

        self.name.set_value(data.name.as_str());

        self.sample_source
            .update(id, &data.source, data.ignore_gaussian_overflow);
        self.pitches.update(&data.pitches);
        self.envelope.update(&data.envelope);

        self.comment_buffer.set_text(&data.comment);

        self.sample_analyser.selected_sample_edited(id, data);

        self.test_sample_widget.item_edited(id, data);

        self.scrollgroup.activate();
    }

    fn selected_compiler_output_changed(&mut self, co: Option<&SampleOutput>) {
        self.test_sample_widget.compiler_output_changed(co);
    }

    fn read_or_reset(&mut self, edit_type: Option<EditTuningField>) -> Option<BrrSample> {
        let (_, old) = self.data.as_ref()?;

        let name = InputHelper::read_or_reset(&mut self.name, &old.name);
        let (source, ignore_gaussian_overflow) = self.sample_source.read_or_reset(&old.source);
        let pitches = self.pitches.read_or_reset(&old.pitches, edit_type);
        let envelope = self.envelope.read_or_reset(&old.envelope);

        Some(BrrSample {
            name: name?,
            source: source?,
            pitches: pitches?,
            envelope,
            ignore_gaussian_overflow,
            comment: self.comment_buffer.text(),
        })
    }

    fn on_finished_editing(&mut self, edit_type: Option<EditTuningField>) {
        if let Some(new_data) = self.read_or_reset(edit_type) {
            if let Some((id, data)) = &mut self.data {
                *data = new_data.clone();
                self.sender.send(GuiMessage::EditBrrSample(*id, new_data));
            }
        }
    }

    fn on_loop_filter_edited(&mut self) {
        self.sample_source.wav_settings.on_loop_filter_edited();
        self.on_finished_editing(None);
    }

    fn widget_event_handler(s: &Rc<RefCell<Self>>, ev: Event) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing(None);
        }
        false
    }

    fn tuning_widget_event_handler(
        s: &Rc<RefCell<Self>>,
        ev: Event,
        field: EditTuningField,
    ) -> bool {
        if is_input_done_event(ev) {
            s.borrow_mut().on_finished_editing(Some(field));
        }
        false
    }

    fn on_source_button_clicked(&self) {
        if let Some((id, _)) = &self.data {
            self.sender.send(GuiMessage::OpenSampleFileDialog(*id));
        }
    }

    fn on_source_type_changed(&self) {
        if let Some((id, data)) = &self.data {
            if let Some(source) = self.sample_source.source_type_changed(*id, &data.source) {
                let new_data = BrrSample {
                    source,
                    ..data.clone()
                };
                self.sender.send(GuiMessage::EditBrrSample(*id, new_data));
            }
        }
    }

    fn on_pitches_type_changed(&mut self) {
        if let Some((id, data)) = &self.data {
            if let Some(p) = self.pitches.pitches_type_changed(*id, &data.pitches) {
                let new_data = BrrSample {
                    pitches: Some(p),
                    ..data.clone()
                };
                self.sender.send(GuiMessage::EditBrrSample(*id, new_data));
            }
        }
    }

    fn on_envelope_type_changed(&mut self) {
        if let Some((id, data)) = &self.data {
            let new_data = BrrSample {
                envelope: self.envelope.envelope_type_changed(*id, &data.envelope),
                ..data.clone()
            };
            self.sender.send(GuiMessage::EditBrrSample(*id, new_data));
        }
    }
}

pub fn sample_with_new_tuning_frequency(sample: &project::BrrSample, frequency: f64) -> BrrSample {
    let pitches = match sample.pitches {
        Some(BrrSamplePitches::Octaves {
            tuning: _,
            first,
            last,
        }) => BrrSamplePitches::Octaves {
            tuning: SampleTuning::Frequency(frequency),
            first,
            last,
        },
        Some(BrrSamplePitches::Notes {
            tuning: _,
            first,
            last,
        }) => BrrSamplePitches::Notes {
            tuning: SampleTuning::Frequency(frequency),
            first,
            last,
        },
        Some(BrrSamplePitches::SampleRates { .. }) | None => {
            let range = default_octaves_for_tuning_frequency(frequency);

            BrrSamplePitches::Octaves {
                tuning: SampleTuning::Frequency(frequency),
                first: *range.start(),
                last: *range.end(),
            }
        }
    };

    BrrSample {
        pitches: Some(pitches),
        ..sample.clone()
    }
}
