//! Sound Effect Export Order

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::Range;

use crate::list_editor::{
    LaVec, ListAction, ListEditorTable, ListMessage, TableAction, TableMapping,
};
use crate::names::{deduplicate_item_name, deduplicate_names};
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
    n_high_priority_sfx: usize,
    low_priority_index: usize,
}

#[derive(Clone, Copy, PartialEq)]
enum Priority {
    High,
    Normal,
    Low,
}

#[derive(Debug, Clone)]
pub struct GuiSfxExportOrder {
    export_order: LaVec<Name>,
    n_high_priority_sfx: usize,
    low_priority_index: usize,
}

impl Default for GuiSfxExportOrder {
    fn default() -> Self {
        Self {
            export_order: LaVec::new(),
            n_high_priority_sfx: 0,
            low_priority_index: 0,
        }
    }
}

impl GuiSfxExportOrder {
    pub fn new_lossy(hp_eo: Vec<Name>, eo: Vec<Name>, lp_eo: Vec<Name>) -> (Self, usize) {
        let n_high_priority_sfx = hp_eo.len();
        let low_priority_index = hp_eo.len() + eo.len();

        let export_order = [hp_eo, eo, lp_eo].concat();
        let (export_order, n_renamed) = deduplicate_names(export_order);

        (
            Self {
                export_order: LaVec::from_vec(export_order.into_vec()),
                n_high_priority_sfx,
                low_priority_index,
            },
            n_renamed,
        )
    }

    pub fn process(&mut self, action: &SfxExportOrderAction) {
        self.export_order.process(&action.action);
        self.n_high_priority_sfx = action.n_high_priority_sfx;
        self.low_priority_index = action.low_priority_index;

        assert!(self.low_priority_index <= self.export_order.len());
    }

    pub fn can_add_one(&self) -> bool {
        self.export_order.len() < MAX_SOUND_EFFECTS
    }

    pub fn high_priority_range(&self) -> Range<usize> {
        0..self.n_high_priority_sfx
    }

    pub fn normal_priority_range(&self) -> Range<usize> {
        self.n_high_priority_sfx..self.low_priority_index
    }

    pub fn low_priority_range(&self) -> Range<usize> {
        self.low_priority_index..self.export_order.len()
    }

    fn priority_range(&self, p: Priority) -> Range<usize> {
        match p {
            Priority::High => self.high_priority_range(),
            Priority::Normal => self.normal_priority_range(),
            Priority::Low => self.low_priority_range(),
        }
    }

    pub fn high_priority_sfx(&self) -> &[Name] {
        &self.export_order[..self.n_high_priority_sfx]
    }

    pub fn normal_priority_sfx(&self) -> &[Name] {
        &self.export_order[self.normal_priority_range()]
    }

    pub fn low_priority_sfx(&self) -> &[Name] {
        &self.export_order[self.low_priority_index..]
    }

    fn table_max_sizes(&self) -> (usize, usize, usize) {
        let n_high = self.n_high_priority_sfx;
        let n_normal = self.normal_priority_range().len();
        let n_low = self.low_priority_range().len();

        (
            MAX_SOUND_EFFECTS.saturating_sub(n_normal + n_low),
            MAX_SOUND_EFFECTS.saturating_sub(n_high + n_low),
            MAX_SOUND_EFFECTS.saturating_sub(n_high + n_normal),
        )
    }
}

impl SfxExportOrder for GuiSfxExportOrder {
    fn n_sound_effects(&self) -> usize {
        self.export_order.len()
    }

    fn export_order(&self) -> &[Name] {
        &self.export_order
    }

    fn n_high_priority_sfx(&self) -> usize {
        self.n_high_priority_sfx
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
    High(ListMessage<Name>),
    Normal(ListMessage<Name>),
    Low(ListMessage<Name>),
    MoveFromHighToNormal(usize),
    MoveFromHighToLow(usize),
    MoveFromNormalToHigh(usize),
    MoveFromNormalToLow(usize),
    MoveFromLowToHigh(usize),
    MoveFromLowToNormal(usize),
}

pub trait SfxEoMapping {
    const TYPE_NAME: &'static str;
    const HEADER: &'static str;

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage;
}

struct HighPrioritySfxEoMapping;
impl SfxEoMapping for HighPrioritySfxEoMapping {
    const TYPE_NAME: &'static str = "sound effect";
    const HEADER: &'static str = "High Priority SFX Export Order";

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::High(lm))
    }
}

struct NormalSfxEoMapping;
impl SfxEoMapping for NormalSfxEoMapping {
    const TYPE_NAME: &'static str = "sound effect";
    const HEADER: &'static str = "Sound Effect Export Order";

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::Normal(lm))
    }
}

struct LowPrioritySfxEoMapping;
impl SfxEoMapping for LowPrioritySfxEoMapping {
    const TYPE_NAME: &'static str = "sound effect";
    const HEADER: &'static str = "Low Priority SFX Export Order";

    fn to_gui_message(lm: ListMessage<Name>) -> GuiMessage {
        GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::Low(lm))
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
    high_priority: ListEditorTable<HighPrioritySfxEoMapping>,
    normal_priority: ListEditorTable<NormalSfxEoMapping>,
    low_priority: ListEditorTable<LowPrioritySfxEoMapping>,
}

impl SfxExportOrderEditor {
    pub fn new(
        parent: &mut Flex,
        sfx_export_order: &GuiSfxExportOrder,
        sender: Sender<GuiMessage>,
    ) -> Self {
        let (max_high, max_normal, max_low) = sfx_export_order.table_max_sizes();

        let mut high_priority = ListEditorTable::new_from_slice(
            parent,
            sfx_export_order.high_priority_sfx(),
            max_high,
            sender,
        );
        let mut normal_priority = ListEditorTable::new_from_slice(
            parent,
            sfx_export_order.normal_priority_sfx(),
            max_normal,
            sender,
        );
        let mut low_priority = ListEditorTable::new_from_slice(
            parent,
            sfx_export_order.low_priority_sfx(),
            max_low,
            sender,
        );

        high_priority.add_sel_button(
            "N",
            "Move to normal priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromHighToNormal(i)),
        );
        high_priority.add_sel_button(
            "L",
            "Move to low priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromHighToLow(i)),
        );

        normal_priority.add_sel_button(
            "H",
            "Move to high priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromNormalToHigh(i)),
        );
        normal_priority.add_sel_button(
            "L",
            "Move to low priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromNormalToLow(i)),
        );

        low_priority.add_sel_button(
            "H",
            "Move to high priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromLowToHigh(i)),
        );
        low_priority.add_sel_button(
            "N",
            "Move to normal priority",
            |a| a.selected.is_some(),
            |i| GuiMessage::EditSfxExportOrder(SfxExportOrderMessage::MoveFromLowToNormal(i)),
        );

        Self {
            high_priority,
            normal_priority,
            low_priority,
        }
    }

    fn process_move<T>(
        table: &mut ListEditorTable<T>,
        range: Range<usize>,
        from: usize,
        to: usize,
    ) -> Option<(ListAction<Name>, isize)>
    where
        T: SfxEoMapping,
    {
        assert_ne!(from, to);

        let eo_offset = range.start;

        table.sfx_eo_edited(&ListAction::Move(from, to));
        table.set_selected_row(to);

        Some((ListAction::Move(from + eo_offset, to + range.start), 0))
    }

    fn process_list_message<T>(
        m: ListMessage<Name>,
        table: &mut ListEditorTable<T>,
        data: &GuiSfxExportOrder,
        range: Range<usize>,
    ) -> Option<(ListAction<Name>, isize)>
    where
        T: SfxEoMapping,
    {
        let eo_offset = range.start;
        let slice = &data.export_order[range.clone()];

        let deduplicate_name =
            |name, index| match deduplicate_item_name(&name, data.export_order(), index) {
                Some(n) => n,
                None => name,
            };

        match m {
            ListMessage::ItemEdited(index, new_name) => match slice.get(index) {
                Some(name) if name != &new_name => {
                    let new_name = deduplicate_name(new_name, Some(index));

                    table.sfx_eo_edited(&ListAction::Edit(index, new_name.clone()));

                    Some((ListAction::Edit(index + eo_offset, new_name), 0))
                }
                _ => None,
            },

            ListMessage::Add(name) | ListMessage::AddWithItemId(_, name) => {
                if data.can_add_one() {
                    let i = slice.len();
                    let name = deduplicate_name(name, None);

                    table.sfx_eo_edited(&ListAction::Add(i, name.clone()));
                    table.set_selected_row(i);

                    table.open_editor(i, 0);

                    Some((ListAction::Add(i + eo_offset, name), 1))
                } else {
                    None
                }
            }

            ListMessage::Clone(index) => match (slice.get(index), data.can_add_one()) {
                (Some(name), true) => {
                    let i = index + 1;
                    let name = deduplicate_name(name.clone(), None);

                    table.sfx_eo_edited(&ListAction::Add(i, name.clone()));
                    table.set_selected_row(i);

                    Some((ListAction::Add(i + eo_offset, name), 1))
                }
                _ => None,
            },

            ListMessage::Remove(index) => {
                if index < slice.len() {
                    table.sfx_eo_edited(&ListAction::Remove(index));
                    table.set_selected_row(index);

                    Some((ListAction::Remove(index + eo_offset), -1))
                } else {
                    None
                }
            }
            ListMessage::MoveToTop(index) => {
                if index > 0 && index < slice.len() {
                    Self::process_move(table, range, index, 0)
                } else {
                    None
                }
            }
            ListMessage::MoveUp(index) => {
                if index > 0 && index < slice.len() {
                    Self::process_move(table, range, index, index - 1)
                } else {
                    None
                }
            }
            ListMessage::MoveDown(index) => {
                if index + 1 < slice.len() {
                    Self::process_move(table, range, index, index + 1)
                } else {
                    None
                }
            }
            ListMessage::MoveToBottom(index) => {
                if index + 1 < slice.len() {
                    Self::process_move(table, range, index, slice.len() - 1)
                } else {
                    None
                }
            }

            // Not supported
            ListMessage::AddMultiple(..) => None,
        }
    }

    fn move_between_priorities<M1, M2>(
        data: &GuiSfxExportOrder,
        index: usize,
        from: Priority,
        from_table: &mut ListEditorTable<M1>,
        to: Priority,
        to_table: &mut ListEditorTable<M2>,
    ) -> Option<SfxExportOrderAction>
    where
        M1: SfxEoMapping,
        M2: SfxEoMapping,
    {
        let from_range = data.priority_range(from);
        let to_range = data.priority_range(to);

        let sfx_list = &data.export_order[from_range.clone()];

        let sfx = sfx_list.get(index)?;
        from_table.sfx_eo_edited(&ListAction::Remove(index));

        let from_index = index + from_range.start;

        let to_index = if from_index < to_range.start {
            to_table.sfx_eo_edited(&ListAction::Add(0, sfx.clone()));
            to_range.start - 1
        } else {
            to_table.sfx_eo_edited(&ListAction::Add(to_range.len(), sfx.clone()));
            to_range.end
        };

        Some(SfxExportOrderAction {
            action: ListAction::Move(from_index, to_index),
            n_high_priority_sfx: if from == Priority::High {
                data.n_high_priority_sfx - 1
            } else if to == Priority::High {
                data.n_high_priority_sfx + 1
            } else {
                data.n_high_priority_sfx
            },
            low_priority_index: if from == Priority::Low {
                data.low_priority_index + 1
            } else if to == Priority::Low {
                data.low_priority_index - 1
            } else {
                data.low_priority_index
            },
        })
    }

    pub fn process(
        &mut self,
        m: SfxExportOrderMessage,
        data: &mut GuiSfxExportOrder,
    ) -> Option<SfxExportOrderAction> {
        let a = match m {
            SfxExportOrderMessage::High(m) => {
                self.normal_priority.clear_selected_row();
                self.low_priority.clear_selected_row();

                let (action, size_delta) = Self::process_list_message(
                    m,
                    &mut self.high_priority,
                    data,
                    data.high_priority_range(),
                )?;

                SfxExportOrderAction {
                    action,
                    n_high_priority_sfx: data
                        .n_high_priority_sfx
                        .checked_add_signed(size_delta)
                        .unwrap(),
                    low_priority_index: data
                        .low_priority_index
                        .checked_add_signed(size_delta)
                        .unwrap(),
                }
            }
            SfxExportOrderMessage::Normal(m) => {
                self.high_priority.clear_selected_row();
                self.low_priority.clear_selected_row();

                let (action, size_delta) = Self::process_list_message(
                    m,
                    &mut self.normal_priority,
                    data,
                    data.normal_priority_range(),
                )?;

                SfxExportOrderAction {
                    action,
                    n_high_priority_sfx: data.n_high_priority_sfx,
                    low_priority_index: data
                        .low_priority_index
                        .checked_add_signed(size_delta)
                        .unwrap(),
                }
            }
            SfxExportOrderMessage::Low(m) => {
                self.high_priority.clear_selected_row();
                self.normal_priority.clear_selected_row();

                let (action, _size_delta) = Self::process_list_message(
                    m,
                    &mut self.low_priority,
                    data,
                    data.low_priority_range(),
                )?;

                SfxExportOrderAction {
                    action,
                    n_high_priority_sfx: data.n_high_priority_sfx,
                    low_priority_index: data.low_priority_index,
                }
            }
            SfxExportOrderMessage::MoveFromHighToNormal(index) => Self::move_between_priorities(
                data,
                index,
                Priority::High,
                &mut self.high_priority,
                Priority::Normal,
                &mut self.normal_priority,
            )?,
            SfxExportOrderMessage::MoveFromHighToLow(index) => Self::move_between_priorities(
                data,
                index,
                Priority::High,
                &mut self.high_priority,
                Priority::Low,
                &mut self.low_priority,
            )?,
            SfxExportOrderMessage::MoveFromNormalToHigh(index) => Self::move_between_priorities(
                data,
                index,
                Priority::Normal,
                &mut self.normal_priority,
                Priority::High,
                &mut self.high_priority,
            )?,
            SfxExportOrderMessage::MoveFromNormalToLow(index) => Self::move_between_priorities(
                data,
                index,
                Priority::Normal,
                &mut self.normal_priority,
                Priority::Low,
                &mut self.low_priority,
            )?,
            SfxExportOrderMessage::MoveFromLowToHigh(index) => Self::move_between_priorities(
                data,
                index,
                Priority::Low,
                &mut self.low_priority,
                Priority::High,
                &mut self.high_priority,
            )?,
            SfxExportOrderMessage::MoveFromLowToNormal(index) => Self::move_between_priorities(
                data,
                index,
                Priority::Low,
                &mut self.low_priority,
                Priority::Normal,
                &mut self.normal_priority,
            )?,
        };

        data.process(&a);

        let (max_high, max_normal, max_low) = data.table_max_sizes();
        self.high_priority.set_max_size(max_high);
        self.normal_priority.set_max_size(max_normal);
        self.low_priority.set_max_size(max_low);

        Some(a)
    }
}
