//! Project Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CadOutput, SongOutput};
use crate::helpers::*;
use crate::list_editor::{
    ListEditor, ListEditorTable, ListMessage, ListState, TableAction, TableCompilerOutput,
    TableMapping,
};
use crate::tables::{RowWithStatus, SimpleRow, TableEvent};
use crate::tabs::{FileType, Tab};
use crate::GuiMessage;

use compiler::common_audio_data::CommonAudioData;
use compiler::data;
use compiler::data::Name;
use compiler::path::SourcePathBuf;
use compiler::songs::{song_duration_string, SongAramSize};

use fltk::app;
use fltk::enums::Color;
use fltk::frame::Frame;
use fltk::group::Flex;
use fltk::output::Output;
use fltk::prelude::*;

pub struct SfxExportOrderMapping;
impl TableMapping for SfxExportOrderMapping {
    type DataType = data::Name;
    type RowType = SimpleRow<1>;

    const CAN_CLONE: bool = true;
    const CAN_EDIT: bool = true;

    fn type_name() -> &'static str {
        "sound effect"
    }

    fn headers() -> Vec<String> {
        vec!["Sound Effect Export Order".to_owned()]
    }

    fn add_clicked() -> GuiMessage {
        GuiMessage::EditSfxExportOrder(ListMessage::Add("name".to_owned().try_into().unwrap()))
    }

    fn to_message(lm: ListMessage<data::Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(lm)
    }

    fn new_row(sfx_name: &data::Name) -> Self::RowType {
        SimpleRow::new([sfx_name.as_str().to_string()])
    }

    fn edit_row(r: &mut Self::RowType, sfx_name: &data::Name) -> bool {
        r.edit_column(0, sfx_name.as_str())
    }

    fn table_event(event: TableEvent, _row: usize, _col: i32) -> TableAction {
        match event {
            TableEvent::Enter | TableEvent::EditorRequested | TableEvent::CellClicked => {
                TableAction::OpenEditor
            }
            TableEvent::DoubleClick => TableAction::None,
        }
    }

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<GuiMessage> {
        match col {
            0 => Name::try_new_lossy(value)
                .map(|name| GuiMessage::EditSfxExportOrder(ListMessage::ItemEdited(index, name))),
            _ => None,
        }
    }
}

pub struct SongMapping;
impl TableMapping for SongMapping {
    type DataType = data::Song;
    type RowType = RowWithStatus<SimpleRow<4>>;

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
        RowWithStatus::new_unchecked(SimpleRow::new([
            song.name.as_str().to_string(),
            song.source.as_str().to_string(),
            String::new(),
            String::new(),
        ]))
    }

    fn edit_row(r: &mut Self::RowType, song: &data::Song) -> bool {
        let mut edited = false;

        let filename = song.source.as_str().to_string();

        edited |= r.columns.edit_column(0, song.name.as_str());
        edited |= r.columns.edit_column(1, filename.as_ref());

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
    type CompilerOutputType = SongOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<SongOutput>) -> bool {
        let (duration, data_size) = match co {
            None => (String::new(), String::new()),
            Some(Ok(song_data)) => {
                let dur = song_duration_string(song_data.duration());
                let ds = format!("{} bytes", song_data.data().len());
                (dur, ds)
            }
            Some(Err(e)) => (String::new(), e.to_string()),
        };

        let mut edited = false;

        edited |= r.set_status_optional_result(co);
        edited |= r.columns.edit_column_string(2, duration);

        edited |= r.columns.edit_column_string(3, data_size);

        edited
    }
}

pub struct ProjectTab {
    group: Flex,

    pub sfx_export_order_table: ListEditorTable<SfxExportOrderMapping>,
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
    pub fn new(
        sfx_list: &impl ListState<Item = Name>,
        song_list: &impl ListState<Item = data::Song>,
        sfx_source_path: Option<&SourcePathBuf>,
        sender: app::Sender<GuiMessage>,
    ) -> Self {
        let mut group = Flex::default_fill().column();

        let mut sfx_flex = Flex::default().row();
        group.fixed(&sfx_flex, input_height(&sfx_flex));

        let sfx_file_label = label("Sound Effects File: ");
        sfx_flex.fixed(&sfx_file_label, ch_units_to_width(&sfx_file_label, 18));

        let mut sound_effects_file = Output::default();
        sound_effects_file.set_color(Color::Background);
        if let Some(p) = sfx_source_path {
            sound_effects_file.set_value(p.as_str());
        }

        sfx_flex.end();

        let sfx_flex_padding = Frame::default();
        group.fixed(&sfx_flex_padding, 8);

        let mut left_right = Flex::default().row();

        let mut left = Flex::default().column();
        left_right.fixed(&left, ch_units_to_width(&left, 30));

        let mut sfx_export_order_table = ListEditorTable::new_with_data(sfx_list, sender.clone());

        let button_height = sfx_export_order_table.button_height();
        left.fixed(&sfx_export_order_table.list_buttons().pack, button_height);

        left.end();

        let mut right = Flex::default().column();

        let mut song_table = ListEditorTable::new_with_data(song_list, sender);

        let button_height = song_table.button_height();
        right.fixed(&song_table.list_buttons().pack, button_height);

        right.end();
        left_right.end();

        let memory_stats = MemoryStats::new(&mut group, 18);

        group.end();

        Self {
            group,
            sfx_export_order_table,
            song_table,
            sound_effects_file,
            memory_stats,
        }
    }

    pub fn sfx_file_changed(&mut self, source: &SourcePathBuf) {
        self.sound_effects_file.set_value(source.as_str());
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

        let group = form.take_group_end();
        parent.fixed(&group, (input_height(&group) + group.pad()) * 5);

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

    fn update_sfx_out(&mut self) {
        if self.n_missing_sfx == 0 {
            Self::output_bytes(&mut self.sfx_out, self.sfx_data_size);
        } else {
            self.sfx_out.set_value(&format!(
                "ERROR: missing {} sound effects",
                self.n_missing_sfx
            ));
            self.sfx_out.set_text_color(Color::Red);
        }
    }

    pub fn samples_compiled(&mut self, cad_output: &CadOutput) {
        let mut valid = |cad: &CommonAudioData| {
            let size = cad.data().len() - cad.sfx_size_incl_addr_table();
            self.samples_size = size;
            Self::output_bytes(&mut self.samples_out, size);
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
            }
            CadOutput::NoSfx(cad, _) => {
                valid(&cad.0);
            }
            CadOutput::WithSfx(cad, _) => {
                valid(&cad.common_audio_data);
            }
        }
        self.update_free_space();
    }

    pub fn set_n_missing_sfx(&mut self, s: usize) {
        self.n_missing_sfx = s;
        self.update_sfx_out();
    }

    pub fn set_sfx_data_size(&mut self, s: usize) {
        self.sfx_data_size = s;
        self.update_sfx_out();
        self.update_free_space();
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
