//! Project Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::SongOutput;
use crate::helpers::*;
use crate::list_editor::{
    ListEditor, ListEditorTable, ListMessage, ListState, TableAction, TableCompilerOutput,
    TableMapping,
};
use crate::tables::{RowWithStatus, SimpleRow, TableEvent};
use crate::tabs::{FileType, Tab};
use crate::Message;

use std::path::Path;

use compiler::data;
use compiler::data::Name;

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

    fn add_clicked() -> Message {
        Message::EditSfxExportOrder(ListMessage::Add("name".to_owned().try_into().unwrap()))
    }

    fn to_message(lm: ListMessage<data::Name>) -> Message {
        Message::EditSfxExportOrder(lm)
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

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<Message> {
        match col {
            0 => Name::try_new_lossy(value)
                .map(|name| Message::EditSfxExportOrder(ListMessage::ItemEdited(index, name))),
            _ => None,
        }
    }
}

pub struct SongMapping;
impl TableMapping for SongMapping {
    type DataType = data::Song;
    type RowType = RowWithStatus<SimpleRow<3>>;

    const CAN_CLONE: bool = false;
    const CAN_EDIT: bool = true;

    fn type_name() -> &'static str {
        "song"
    }

    fn headers() -> Vec<String> {
        vec![
            "Song Name".to_owned(),
            "Filename".to_owned(),
            "Data size".to_owned(),
        ]
    }

    fn add_clicked() -> Message {
        Message::AddSongToProjectDialog
    }

    fn to_message(lm: ListMessage<data::Song>) -> Message {
        Message::EditProjectSongs(lm)
    }

    fn new_row(song: &data::Song) -> Self::RowType {
        RowWithStatus::new_unchecked(SimpleRow::new([
            song.name.as_str().to_string(),
            song.source.to_string_lossy().to_string(),
            String::new(),
        ]))
    }

    fn edit_row(r: &mut Self::RowType, song: &data::Song) -> bool {
        let mut edited = false;

        let filename = song.source.to_string_lossy();

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
                    TableAction::Send(Message::OpenSongTab(row))
                } else {
                    TableAction::None
                }
            }
        }
    }

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<Message> {
        match col {
            0 => Name::try_new_lossy(value).map(|name| Message::SetProjectSongName(index, name)),
            _ => None,
        }
    }
}

impl TableCompilerOutput for SongMapping {
    type CompilerOutputType = SongOutput;

    fn set_row_state(r: &mut Self::RowType, co: &Option<SongOutput>) -> bool {
        let s = match co {
            None => String::new(),
            Some(Ok(size)) => format!("{} bytes", size),
            Some(Err(e)) => e.to_string(),
        };

        let mut edited = false;

        edited |= r.set_status_optional_result(co);
        edited |= r.columns.edit_column_string(2, s);

        edited
    }
}

pub struct ProjectTab {
    group: Flex,

    pub sfx_export_order_table: ListEditorTable<SfxExportOrderMapping>,
    pub song_table: ListEditorTable<SongMapping>,

    sound_effects_file: Output,
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
        sfx_pf_path: Option<&Path>,
        sender: app::Sender<Message>,
    ) -> Self {
        let mut group = Flex::default_fill().column();

        let mut sfx_flex = Flex::default().row();
        group.fixed(&sfx_flex, input_height(&sfx_flex));

        let sfx_file_label = label("Sound Effects File: ");
        sfx_flex.fixed(&sfx_file_label, ch_units_to_width(&sfx_file_label, 18));

        let mut sound_effects_file = Output::default();
        sound_effects_file.set_color(Color::Background);
        if let Some(p) = sfx_pf_path {
            sound_effects_file.set_value(&p.display().to_string());
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
        group.end();

        Self {
            group,
            sfx_export_order_table,
            song_table,
            sound_effects_file,
        }
    }

    pub fn sfx_file_changed(&mut self, pf_path: &Path) {
        self.sound_effects_file
            .set_value(&pf_path.display().to_string());
    }
}
