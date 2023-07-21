//! Project Tab

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::helpers::*;
use crate::list_editor::{ListEditor, ListEditorTable, ListMessage, TableAction, TableMapping};
use crate::tables::{SingleColumnRow, TableEvent, TwoColumnsRow};
use crate::Message;
use crate::Tab;

use compiler::data;
use compiler::data::Name;

use fltk::app;
use fltk::group::Flex;
use fltk::prelude::*;

pub struct SfxExportOrderMapping;
impl TableMapping for SfxExportOrderMapping {
    type DataType = data::Name;
    type RowType = SingleColumnRow;

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

    fn new_row(sfx_name: &data::Name) -> SingleColumnRow {
        SingleColumnRow(sfx_name.as_str().to_string())
    }

    fn edit_row(r: &mut SingleColumnRow, sfx_name: &data::Name) -> bool {
        if r.0 != sfx_name.as_str() {
            r.0 = sfx_name.as_str().to_owned();
            true
        } else {
            false
        }
    }

    fn table_event(event: TableEvent, _row: usize, _col: i32) -> TableAction {
        match event {
            TableEvent::Enter | TableEvent::EditorRequested | TableEvent::CellClicked => {
                TableAction::OpenEditor
            }
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
    type RowType = TwoColumnsRow;

    const CAN_CLONE: bool = false;
    const CAN_EDIT: bool = true;

    fn type_name() -> &'static str {
        "song"
    }

    fn headers() -> Vec<String> {
        vec!["Song Name".to_owned(), "Filename".to_owned()]
    }

    fn add_clicked() -> Message {
        Message::AddSongToProjectDialog
    }

    fn to_message(lm: ListMessage<data::Song>) -> Message {
        Message::EditProjectSongs(lm)
    }

    fn new_row(song: &data::Song) -> TwoColumnsRow {
        TwoColumnsRow(
            song.name.as_str().to_string(),
            song.source.to_string_lossy().to_string(),
        )
    }

    fn edit_row(r: &mut TwoColumnsRow, song: &data::Song) -> bool {
        let mut edited = false;

        if r.0 != song.name.as_str() {
            r.0 = song.name.as_str().to_string();
            edited = true;
        }

        let s = song.source.to_string_lossy();
        if r.1 != s {
            r.1 = s.to_string();
            edited = true;
        }

        edited
    }

    fn table_event(event: TableEvent, _row: usize, col: i32) -> TableAction {
        match event {
            TableEvent::Enter | TableEvent::EditorRequested | TableEvent::CellClicked => {
                if col == 0 {
                    TableAction::OpenEditor
                } else {
                    // ::TODO open file dialog?::
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

pub struct ProjectTab {
    group: Flex,

    pub sfx_export_order_table: ListEditorTable<SfxExportOrderMapping>,
    pub song_table: ListEditorTable<SongMapping>,
}

impl Tab for ProjectTab {
    fn widget(&mut self) -> &mut Flex {
        &mut self.group
    }
}

impl ProjectTab {
    pub fn new(project: &data::Project, sender: app::Sender<Message>) -> Self {
        let mut group = Flex::default_fill().with_label("Project").row();

        let mut left = Flex::default().column();
        group.fixed(&left, ch_units_to_width(&left, 30));

        let mut sfx_export_order_table =
            ListEditorTable::new(&project.sound_effects, sender.clone());

        let button_height = sfx_export_order_table.button_height();
        left.fixed(&sfx_export_order_table.list_buttons().pack, button_height);

        left.end();

        let mut right = Flex::default().column();

        let mut song_table = ListEditorTable::new(&project.songs, sender);

        let button_height = song_table.button_height();
        right.fixed(&song_table.list_buttons().pack, button_height);

        right.end();

        group.end();

        Self {
            group,
            sfx_export_order_table,
            song_table,
        }
    }
}
