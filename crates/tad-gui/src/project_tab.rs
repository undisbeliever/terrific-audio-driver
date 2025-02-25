//! Project Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CadOutput, ShortSongError};
use crate::list_editor::{
    ListEditorTable, ListMessage, ListWithCompilerOutputEditor, TableAction, TableCompilerOutput,
    TableMapping,
};
use crate::sfx_export_order::SfxExportOrderEditor;
use crate::tables::{RowWithStatus, TableEvent, TableRow};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;
use crate::{helpers::*, ProjectData};

use compiler::common_audio_data::CommonAudioData;
use compiler::data::Name;
use compiler::data::{self, DefaultSfxFlags};
use compiler::path::SourcePathBuf;
use compiler::songs::{song_duration_string, SongAramSize, SongData};

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use fltk::button::CheckButton;
use fltk::enums::{Align, Color};
use fltk::group::Flex;
use fltk::output::Output;
use fltk::prelude::*;
use fltk::{app, draw};

pub struct SongRow {
    name: String,
    filename: String,
    duration: String,
    data_size: String,
}

impl TableRow for SongRow {
    const N_COLUMNS: i32 = 4;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32) {
        match col {
            0 => draw::draw_text2(self.name.as_str(), x, y, w, h, Align::Left),
            1 => draw::draw_text2(&self.filename, x, y, w, h, Align::Left),
            2 => draw::draw_text2(&self.duration, x, y, w, h, Align::Right),
            3 => draw::draw_text2(&self.data_size, x, y, w, h, Align::Right),
            _ => (),
        }
    }

    fn value(&self, col: i32) -> Option<&str> {
        match col {
            0 => Some(&self.name),
            _ => None,
        }
    }
}

pub struct SongMapping;
impl TableMapping for SongMapping {
    type DataType = data::Song;
    type RowType = RowWithStatus<SongRow>;

    const CAN_CLONE: bool = false;
    const CAN_EDIT: bool = true;

    fn type_name() -> &'static str {
        "song"
    }

    fn headers() -> Vec<String> {
        vec![
            "Song Name".to_owned(),
            "Filename".to_owned(),
            "Duration".to_owned(),
            "Data size".to_owned(),
        ]
    }

    fn add_clicked() -> GuiMessage {
        GuiMessage::AddSongToProjectDialog
    }

    fn to_message(lm: ListMessage<data::Song>) -> GuiMessage {
        GuiMessage::EditProjectSongs(lm)
    }

    fn new_row(song: &data::Song) -> Self::RowType {
        RowWithStatus::new_unchecked(SongRow {
            name: song.name.as_str().to_owned(),
            filename: song.source.as_str().to_owned(),
            duration: String::new(),
            data_size: String::new(),
        })
    }

    fn edit_row(r: &mut Self::RowType, song: &data::Song) -> bool {
        let mut edited = false;

        let write_if_changed = |dest: &mut String, src: &str| {
            if dest != src {
                src.clone_into(dest);
                true
            } else {
                false
            }
        };

        edited |= write_if_changed(&mut r.columns.name, song.name.as_str());
        edited |= write_if_changed(&mut r.columns.filename, song.source.as_str());

        edited
    }

    fn table_event(event: TableEvent, row: usize, col: i32) -> TableAction {
        match event {
            TableEvent::Enter | TableEvent::EditorRequested | TableEvent::CellClicked => {
                if col == 0 {
                    TableAction::OpenEditor
                } else {
                    TableAction::None
                }
            }
            TableEvent::DoubleClick => {
                if col != 0 {
                    TableAction::Send(GuiMessage::OpenSongTab(row))
                } else {
                    TableAction::None
                }
            }
        }
    }

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<GuiMessage> {
        match col {
            0 => Name::try_new_lossy(value).map(|name| GuiMessage::SetProjectSongName(index, name)),
            _ => None,
        }
    }
}

impl TableCompilerOutput for SongMapping {
    type CompilerOutputType = Result<Arc<SongData>, ShortSongError>;

    fn set_row_state(r: &mut Self::RowType, co: &Option<Self::CompilerOutputType>) -> bool {
        let (duration, data_size) = match co {
            None => (String::new(), String::new()),
            Some(Ok(song_data)) => {
                let dur = song_duration_string(song_data.duration());
                let ds = format!("{} bytes", song_data.data().len());
                (dur, ds)
            }
            Some(Err(e)) => (String::new(), e.to_string()),
        };

        r.set_status_optional_result(co);

        r.columns.duration = duration;
        r.columns.data_size = data_size;

        true
    }
}

pub struct ProjectTab {
    group: Flex,

    pub sfx_export_order: SfxExportOrderEditor,

    pub song_table: ListEditorTable<SongMapping>,

    sound_effects_file: Output,

    pub memory_stats: MemoryStats,
}

impl Tab for ProjectTab {
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

impl ProjectTab {
    pub fn new(data: &ProjectData, sender: app::Sender<GuiMessage>) -> Self {
        let mut group = Flex::default_fill().row();

        let mut sfx_sidebar = Flex::default().column();
        group.fixed(&sfx_sidebar, ch_units_to_width(&sfx_sidebar, 30));

        let sfx_export_order =
            SfxExportOrderEditor::new(&mut sfx_sidebar, &data.sfx_export_order, sender.clone());

        DefaultSfxFlagsWidget::new(&mut sfx_sidebar, data.default_sfx_flags, sender.clone());

        sfx_sidebar.end();

        let mut right = Flex::default().column();

        let song_table = ListEditorTable::new_with_data(&mut right, &data.project_songs, sender);

        let mut sfx_file_flex = Flex::default().row();
        right.fixed(&sfx_file_flex, input_height(&sfx_file_flex));

        let sfx_file_label = label("Sound Effects File: ");
        sfx_file_flex.fixed(&sfx_file_label, ch_units_to_width(&sfx_file_label, 18));

        let mut sound_effects_file = Output::default();
        sound_effects_file.set_color(Color::Background);
        if let Some(p) = &data.sound_effects_file {
            sound_effects_file.set_value(p.as_str());
        }

        sfx_file_flex.end();

        let memory_stats = MemoryStats::new(&mut right, 18);

        right.end();

        group.end();

        Self {
            group,
            sfx_export_order,
            song_table,
            sound_effects_file,
            memory_stats,
        }
    }

    pub fn sfx_file_changed(&mut self, source: &SourcePathBuf) {
        self.sound_effects_file.set_value(source.as_str());
    }
}

impl ListWithCompilerOutputEditor<data::Song, Result<Arc<SongData>, ShortSongError>>
    for ProjectTab
{
    type TableMapping = SongMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping> {
        &mut self.song_table
    }
}

struct DefaultSfxFlagsWidget {
    sender: app::Sender<GuiMessage>,
    one_channel: CheckButton,
    interruptible: CheckButton,
}

impl DefaultSfxFlagsWidget {
    fn new(
        parent: &mut Flex,
        sfx_flags: DefaultSfxFlags,
        sender: app::Sender<GuiMessage>,
    ) -> Rc<RefCell<DefaultSfxFlagsWidget>> {
        let height = input_height(parent);

        let l = label("Default SFX Flags:");
        parent.fixed(&l, height);

        let mut group = Flex::default().column();
        group.set_margins(ch_units_to_width(&group, 5), 0, 0, 0);
        parent.fixed(&group, height * 2);

        let mut add_checkbox = |label, value| {
            let mut b = CheckButton::default().with_label(label);
            parent.fixed(&b, height);
            b.set_value(value);
            b
        };

        let out = Rc::new(RefCell::new(DefaultSfxFlagsWidget {
            sender,
            one_channel: add_checkbox("One Channel", sfx_flags.one_channel),
            interruptible: add_checkbox("Interruptible", sfx_flags.interruptible),
        }));

        group.end();

        {
            let mut o = out.borrow_mut();

            let set_callback = |b: &mut CheckButton| {
                let state = out.clone();
                b.set_callback(move |_| {
                    state.borrow().flags_changed();
                });
            };
            set_callback(&mut o.one_channel);
            set_callback(&mut o.interruptible);
        }

        out
    }

    fn flags_changed(&self) {
        self.sender
            .send(GuiMessage::DefaultSfxFlagChanged(DefaultSfxFlags {
                one_channel: self.one_channel.value(),
                interruptible: self.interruptible.value(),
            }))
    }
}

pub struct MemoryStats {
    samples_out: Output,
    sfx_out: Output,
    largest_song_out: Output,
    free_space_out: Output,

    samples_size: usize,
    n_missing_sfx: usize,
    sfx_data_size: usize,

    largest_song_size: usize,
}

impl MemoryStats {
    const DRIVER_SIZE: usize = compiler::driver_constants::addresses::COMMON_DATA as usize;
    const AUDIO_RAM_SIZE: usize = compiler::driver_constants::AUDIO_RAM_SIZE;

    fn new(parent: &mut Flex, width_ch_units: i32) -> Self {
        let mut form = InputForm::new(width_ch_units);

        let mut driver_out = form.add_input::<Output>("Audio Driver:");
        Self::output_bytes(&mut driver_out, Self::DRIVER_SIZE);
        driver_out.set_color(Color::Background);

        let mut samples_out = form.add_input::<Output>("Samples:");
        samples_out.set_color(Color::Background);

        let mut sfx_out = form.add_input::<Output>("Sound effects:");
        sfx_out.set_color(Color::Background);

        let mut largest_song_out = form.add_input::<Output>("Largest song:");
        largest_song_out.set_color(Color::Background);

        let mut free_space_out = form.add_input::<Output>("Free space:");
        free_space_out.set_color(Color::Background);

        let (group, form_height) = form.end();
        parent.fixed(&group, form_height);

        Self {
            samples_out,
            sfx_out,
            largest_song_out,
            free_space_out,

            samples_size: 0,
            n_missing_sfx: 0,
            sfx_data_size: 0,
            largest_song_size: 0,
        }
    }

    fn output_bytes(o: &mut Output, size: usize) {
        o.set_value(&format!("{} bytes", size));
        o.set_text_color(Color::Foreground);
    }

    fn show_sfx_error_message(&mut self) {
        if self.n_missing_sfx == 0 {
            self.sfx_out.set_value("ERROR");
        } else {
            self.sfx_out.set_value(&format!(
                "ERROR: missing {} sound effects",
                self.n_missing_sfx
            ));
        }
        self.sfx_out.set_text_color(Color::Red);
    }

    fn update_free_space(&mut self) {
        let common_data = self.samples_size + self.sfx_data_size;
        // Add 1 if odd (loader can only transfer a multiple of 2 bytes)
        let common_data = common_data + (common_data % 2);

        let aram_used = Self::DRIVER_SIZE + common_data + self.largest_song_size;

        if aram_used <= Self::AUDIO_RAM_SIZE {
            Self::output_bytes(&mut self.free_space_out, Self::AUDIO_RAM_SIZE - aram_used);
        } else {
            if aram_used - self.largest_song_size < Self::AUDIO_RAM_SIZE {
                self.free_space_out
                    .set_value("ERROR: Largest song cannot fit in Audio-RAM");
            } else {
                self.free_space_out
                    .set_value("ERROR: Common audio data cannot fit in Audio-RAM");
            }
            self.free_space_out.set_text_color(Color::Red);
        }
    }

    pub fn cad_output_changed(&mut self, cad_output: &CadOutput) {
        let mut valid_samples = |cad: &CommonAudioData| {
            self.samples_size = cad.instruments_and_samples_size().into();
            Self::output_bytes(&mut self.samples_out, self.samples_size);
        };

        match cad_output {
            CadOutput::None => {
                self.samples_size = 0;
                self.samples_out.set_value("");
            }
            CadOutput::Err(_) => {
                self.samples_size = 0;
                self.samples_out.set_value("ERROR");
                self.samples_out.set_text_color(Color::Red);

                self.sfx_data_size = 0;
                self.sfx_out.set_value("ERROR");
                self.sfx_out.set_text_color(Color::Red);
            }
            CadOutput::NoSfx(cad) => {
                valid_samples(&cad.0);

                self.sfx_data_size = 0;
                self.show_sfx_error_message();
            }
            CadOutput::SfxBuffer(cad) => {
                valid_samples(cad.0.common_data());

                self.sfx_data_size = 0;
                self.show_sfx_error_message();
            }
            CadOutput::WithSfx(cad) => {
                valid_samples(&cad.common_audio_data);

                self.sfx_data_size = cad.common_audio_data.sfx_data_and_tables_range().len();
                Self::output_bytes(&mut self.sfx_out, self.sfx_data_size);
            }
        }
        self.update_free_space();
    }

    pub fn set_n_missing_sfx(&mut self, s: usize) {
        self.n_missing_sfx = s;
        if s > 0 {
            self.show_sfx_error_message();
        }
    }

    pub fn set_largest_song(&mut self, s: &SongAramSize) {
        let s = s.total_size();

        if s != self.largest_song_size {
            self.largest_song_size = s;
            Self::output_bytes(&mut self.largest_song_out, s);
            self.update_free_space();
        }
    }
}
