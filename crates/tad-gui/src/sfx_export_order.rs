//! Sound Effect Export Order

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::Range;

use crate::list_editor::{
    LaVec, ListAction, ListEditor, ListEditorTable, ListMessage, TableAction, TableMapping,
};
use crate::names::{deduplicate_item_name, deduplicate_names, NameDeduplicator};
use crate::tables::{SimpleRow, TableEvent};
use crate::GuiMessage;

use compiler::data::Name;
use compiler::driver_constants::MAX_SOUND_EFFECTS;
use compiler::sound_effects::SfxExportOrder;

use fltk::app::Sender;
use fltk::group::Flex;

#[derive(Debug)]
pub struct SfxId(u8);

impl SfxId {
    pub fn value(&self) -> u8 {
        self.0
    }
}

#[derive(Debug)]
pub struct SfxExportOrderAction {
    action: ListAction<Name>,
    low_priority_index: usize,
}

#[derive(Debug, Clone)]
pub struct GuiSfxExportOrder {
    export_order: LaVec<Name>,
    low_priority_index: usize,
}

impl Default for GuiSfxExportOrder {
    fn default() -> Self {
        Self {
            low_priority_index: 0,
            export_order: LaVec::new(),
        }
    }
}

impl GuiSfxExportOrder {
    pub fn new_lossy(eo: Vec<Name>, lp_eo: Vec<Name>) -> (Self, usize) {
        let low_priority_index = eo.len();

        let export_order = [eo, lp_eo].concat();
        let (export_order, n_renamed) = deduplicate_names(export_order);

        (
            Self {
                export_order: LaVec::from_vec(export_order.into_vec()),
                low_priority_index,
            },
            n_renamed,
        )
    }

    pub fn process(&mut self, action: &SfxExportOrderAction) {
        self.export_order.process(&action.action);
        self.low_priority_index = action.low_priority_index;
    }

    pub fn can_add_one(&self) -> bool {
        self.export_order.len() < MAX_SOUND_EFFECTS
    }

    pub fn normal_priority_sfx(&self) -> &[Name] {
        &self.export_order[0..self.low_priority_index]
    }

    pub fn low_priority_sfx(&self) -> &[Name] {
        &self.export_order[self.low_priority_index..]
    }
}

impl SfxExportOrder for GuiSfxExportOrder {
    fn n_sound_effects(&self) -> usize {
        self.export_order.len()
    }

    fn export_order(&self) -> &[Name] {
        &self.export_order
    }

    fn low_priority_index(&self) -> usize {
        self.low_priority_index
    }
}

impl GuiSfxExportOrder {
    pub fn sfx_id(&self, index: usize) -> Option<SfxId> {
        if index < self.export_order.len() {
            index.try_into().ok().map(SfxId)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum SfxExportOrderMessage {
    NormalPriority(ListMessage<Name>),
    LowPriority(ListMessage<Name>),
}

trait SfxEoMapping {
    const TYPE_NAME: &'static str;
    const HEADER: &'static str;

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::NormalPriority(lm))
    }
}

struct NormalSfxEoMapping;
impl SfxEoMapping for NormalSfxEoMapping {
    const TYPE_NAME: &'static str = "sound effect";
    const HEADER: &'static str = "Low Priority SFX Export Order";

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::NormalPriority(lm))
    }
}

struct LowPrioritySfxEoMapping;
impl SfxEoMapping for LowPrioritySfxEoMapping {
    const TYPE_NAME: &'static str = "sound effect";
    const HEADER: &'static str = "Sound Effect Export Order";

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::LowPriority(lm))
    }
}

impl<T> TableMapping for T
where
    T: SfxEoMapping,
{
    type DataType = Name;
    type RowType = SimpleRow<1>;

    const CAN_CLONE: bool = true;
    const CAN_EDIT: bool = true;

    fn type_name() -> &'static str {
        T::TYPE_NAME
    }

    fn headers() -> Vec<String> {
        vec![T::HEADER.to_owned()]
    }

    fn to_message(lm: ListMessage<Name>) -> GuiMessage {
        T::to_gui_message(lm)
    }

    fn add_clicked() -> GuiMessage {
        Self::to_gui_message(ListMessage::Add("name".to_owned().try_into().unwrap()))
    }

    fn new_row(sfx_name: &Name) -> Self::RowType {
        SimpleRow::new([sfx_name.as_str().to_string()])
    }

    fn edit_row(r: &mut Self::RowType, sfx_name: &Name) -> bool {
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
                .map(|name| T::to_gui_message(ListMessage::ItemEdited(index, name))),
            _ => None,
        }
    }
}

pub struct SfxExportOrderEditor {
    normal_priority: ListEditorTable<NormalSfxEoMapping>,
    low_priority: ListEditorTable<LowPrioritySfxEoMapping>,
}

impl SfxExportOrderEditor {
    pub fn new(
        parent: &mut Flex,
        sfx_export_order: &GuiSfxExportOrder,
        sender: Sender<GuiMessage>,
    ) -> Self {
        // ::TODO add a button to move SFX between low and high priorities::
        let mut normal_priority =
            ListEditorTable::new_from_slice(sfx_export_order.normal_priority_sfx(), sender.clone());
        let mut low_priority =
            ListEditorTable::new_from_slice(sfx_export_order.low_priority_sfx(), sender.clone());

        let button_height = normal_priority.button_height();
        parent.fixed(&normal_priority.list_buttons().pack, button_height);
        parent.fixed(&low_priority.list_buttons().pack, button_height);

        Self::clear_table_selection(&mut normal_priority, sfx_export_order);
        Self::clear_table_selection(&mut low_priority, sfx_export_order);

        Self {
            normal_priority,
            low_priority,
        }
    }

    fn clear_table_selection<T>(table: &mut ListEditorTable<T>, data: &GuiSfxExportOrder)
    where
        T: TableMapping + 'static,
        T::DataType: NameDeduplicator,
    {
        table.clear_selected();
        table.list_buttons().selected_clear(data.can_add_one());
    }

    fn update_table_selection<T>(
        table: &mut ListEditorTable<T>,
        index: usize,
        data: &GuiSfxExportOrder,
        range: &Range<usize>,
    ) where
        T: TableMapping + 'static,
        T::DataType: NameDeduplicator,
    {
        table.set_selected_row(index);
        table
            .list_buttons()
            .selected_changed(index, range.len(), data.can_add_one())
    }

    fn process_move<T>(
        table: &mut ListEditorTable<T>,
        data: &GuiSfxExportOrder,
        range: Range<usize>,
        from: usize,
        to: usize,
    ) -> Option<(ListAction<Name>, isize)>
    where
        T: TableMapping<DataType = Name> + 'static,
        T::DataType: NameDeduplicator,
    {
        assert_ne!(from, to);

        let eo_offset = range.start;

        table.list_edited(&ListAction::Move(from, to));
        Self::update_table_selection(table, to, data, &range);

        Some((ListAction::Move(from + eo_offset, to + range.start), 0))
    }

    fn process_list_message<T>(
        m: ListMessage<Name>,
        table: &mut ListEditorTable<T>,
        data: &GuiSfxExportOrder,
        range: Range<usize>,
    ) -> Option<(ListAction<Name>, isize)>
    where
        T: TableMapping<DataType = Name> + 'static,
        T::DataType: NameDeduplicator,
    {
        let eo_offset = range.start;
        let selected = table.selected_row().unwrap_or(usize::MAX);
        let slice = &data.export_order[range.clone()];

        let deduplicate_name =
            |name, index| match deduplicate_item_name(&name, data.export_order(), index) {
                Some(n) => n,
                None => name,
            };

        match m {
            ListMessage::ClearSelection => {
                Self::clear_table_selection(table, data);
                None
            }

            ListMessage::ItemSelected(i) => {
                match slice.get(i) {
                    Some(_) => {
                        Self::update_table_selection(table, i, data, &range);
                    }
                    None => table.clear_selected(),
                }
                None
            }

            ListMessage::ItemEdited(index, new_name) => match slice.get(index) {
                Some(name) if name != &new_name => {
                    let new_name = deduplicate_name(new_name, Some(index));

                    table.list_edited(&ListAction::Edit(index, new_name.clone()));

                    Some((ListAction::Edit(index + eo_offset, new_name), 0))
                }
                _ => None,
            },

            ListMessage::Add(name) | ListMessage::AddWithItemId(_, name) => {
                if data.can_add_one() {
                    let i = slice.len();
                    let name = deduplicate_name(name, None);

                    table.list_edited(&ListAction::Add(i, name.clone()));
                    Self::update_table_selection(table, i, data, &range);

                    table.open_editor(i, 0);

                    Some((ListAction::Add(i + eo_offset, name), 1))
                } else {
                    None
                }
            }

            ListMessage::CloneSelected => match (slice.get(selected), data.can_add_one()) {
                (Some(name), true) => {
                    let i = selected + 1;
                    let name = deduplicate_name(name.clone(), None);

                    table.list_edited(&ListAction::Add(i, name.clone()));
                    Self::update_table_selection(table, i, data, &range);

                    Some((ListAction::Add(i + eo_offset, name), 1))
                }
                _ => None,
            },

            ListMessage::RemoveSelected => {
                if selected < slice.len() {
                    table.list_edited(&ListAction::Remove(selected));
                    Self::clear_table_selection(table, data);

                    Some((ListAction::Remove(selected + eo_offset), -1))
                } else {
                    None
                }
            }
            ListMessage::MoveSelectedToTop => {
                if selected > 0 && selected < slice.len() {
                    Self::process_move(table, data, range, selected, 0)
                } else {
                    None
                }
            }
            ListMessage::MoveSelectedUp => {
                if selected > 0 && selected < slice.len() {
                    Self::process_move(table, data, range, selected, selected - 1)
                } else {
                    None
                }
            }
            ListMessage::MoveSelectedDown => {
                if selected + 1 < slice.len() {
                    Self::process_move(table, data, range, selected, selected + 1)
                } else {
                    None
                }
            }
            ListMessage::MoveSelectedToBottom => {
                if selected + 1 < slice.len() {
                    Self::process_move(table, data, range, selected, slice.len() - 1)
                } else {
                    None
                }
            }

            // Not supported
            ListMessage::AddMultiple(..) => None,
        }
    }

    pub fn process(
        &mut self,
        m: SfxExportOrderMessage,
        data: &mut GuiSfxExportOrder,
    ) -> Option<SfxExportOrderAction> {
        let a = match m {
            SfxExportOrderMessage::NormalPriority(m) => {
                self.low_priority.clear_selected();

                let (action, size_delta) = Self::process_list_message(
                    m,
                    &mut self.normal_priority,
                    data,
                    0..data.low_priority_index,
                )?;

                SfxExportOrderAction {
                    action,
                    low_priority_index: data
                        .low_priority_index
                        .checked_add_signed(size_delta)
                        .unwrap(),
                }
            }
            SfxExportOrderMessage::LowPriority(m) => {
                self.normal_priority.clear_selected();

                let (action, _size_delta) = Self::process_list_message(
                    m,
                    &mut self.low_priority,
                    data,
                    data.low_priority_index..data.export_order.len(),
                )?;

                SfxExportOrderAction {
                    action,
                    low_priority_index: data.low_priority_index,
                }
            }
        };

        data.process(&a);

        Some(a)
    }
}
