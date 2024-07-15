//! List Editors

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{self, ItemChanged, ItemId};
use crate::helpers::{ch_units_to_width, SetActive};
use crate::names::{deduplicate_item_name_inplace, DeduplicatedNameVec, NameGetter, NameSetter};
use crate::GuiMessage;
use crate::{sfx_export_order, tables};

use compiler::data::Name;
use fltk::button::Button;
use fltk::group::{Pack, PackType};
use fltk::prelude::{GroupExt, WidgetExt};

use std::cell::RefCell;
use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

// A ListMessage MUST ONLY be called once per frame
// (to prevent a potential infinite `ListMessage::ItemSelected` loop)
#[derive(Debug)]
pub enum ListMessage<T> {
    // The `SelectedItemEdited` message has been removed because clicking on a TrTable list item
    // will cause the `ItemSelected` message to be sent before the `SelectedItemEdited` event.
    ItemEdited(usize, T),

    Add(T),
    AddMultiple(Vec<T>),
    Clone(usize),
    Remove(usize),
    MoveToTop(usize),
    MoveUp(usize),
    MoveDown(usize),
    MoveToBottom(usize),

    // Only adds the item if the list does not contain ItemId.
    AddWithItemId(ItemId, T),
}

#[derive(Debug)]
pub enum ListAction<T> {
    None,
    Add(usize, T),
    AddMultiple(usize, Vec<T>),
    Remove(usize),
    Edit(usize, T),
    Move(usize, usize),
}

impl<T> ListAction<T> {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

/// Preforms the ListAction on a list
///
/// #Panics
/// Panics if an index is out-of-bounds.  For safety, ListAction should be the only thing that modifies `list`.
pub fn process_list_action<T>(list: &mut Vec<T>, action: &ListAction<T>)
where
    T: Clone,
{
    process_list_action_map(
        list,
        action,
        // add
        |item| item.clone(),
        // edit
        |item, new_value| *item = new_value.clone(),
    );
}

/// Preforms the ListAction on a list not of type T.
///
/// #Panics
/// Panics if an index is out-of-bounds.  For safety, ListAction should be the only thing that modifies `list`.
pub fn process_list_action_map<T, U>(
    list: &mut Vec<U>,
    action: &ListAction<T>,
    add: impl Fn(&T) -> U,
    edit: impl FnOnce(&mut U, &T),
) {
    match action {
        ListAction::None => (),
        ListAction::Add(i, item) => list.insert(*i, add(item)),
        ListAction::AddMultiple(i, items) => {
            list.splice(i..i, items.iter().map(add));
        }
        ListAction::Remove(i) => {
            list.remove(*i);
        }
        ListAction::Edit(i, item) => edit(&mut list[*i], item),
        &ListAction::Move(from, to) => move_list_item(list, from, to),
    }
}

/// Move list item from index `from` to index `to`.
/// #Panics
/// Panics if `from` or `to` are out of bounds.
fn move_list_item<T>(list: &mut [T], from: usize, to: usize) {
    if from < to {
        // Move down
        for i in from..to {
            list.swap(i, i + 1);
        }
    } else {
        // Move up
        for i in (to + 1..=from).rev() {
            list.swap(i, i - 1);
        }
    }
}

/// A `Vec` that can only be resized or reordered by a `ListAction<T>`
#[derive(Debug, Default, Clone)]
pub struct LaVec<T>(Vec<T>);

impl<T> LaVec<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn from_vec(v: Vec<T>) -> Self {
        Self(v)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.0.get_mut(index)
    }

    #[allow(dead_code)]
    pub fn process(&mut self, action: &ListAction<T>)
    where
        T: Clone,
    {
        process_list_action(&mut self.0, action);
    }

    pub fn process_map<U>(
        &mut self,
        action: &ListAction<U>,
        add: impl Fn(&U) -> T,
        edit: impl FnOnce(&mut T, &U),
    ) {
        process_list_action_map(&mut self.0, action, add, edit);
    }
}

impl<T> Deref for LaVec<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.0
    }
}

pub trait ListState
where
    Self::Item: Clone + PartialEq<Self::Item> + NameGetter + NameSetter,
{
    type Item;

    fn len(&self) -> usize;
    fn can_add(&self) -> bool;
    fn can_add_multiple(&self, count: usize) -> bool;
    fn item_iter(&self) -> impl Iterator<Item = &Self::Item>;
}

pub trait CompilerOutput {
    fn is_valid(&self) -> bool;
}

impl<T, E> CompilerOutput for Result<T, E> {
    fn is_valid(&self) -> bool {
        self.is_ok()
    }
}

pub struct ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameGetter + NameSetter,
    O: CompilerOutput,
{
    max_size: usize,
    list: Vec<(ItemId, T)>,
    compiler_output: Vec<Option<O>>,
    error_set: HashSet<ItemId>,
}

pub trait ListWithCompilerOutputEditor<T, O>
where
    T: Clone + PartialEq<T> + NameGetter + NameSetter,
    O: CompilerOutput,
    Self::TableMapping:
        TableMapping<DataType = T> + TableCompilerOutput<CompilerOutputType = O> + 'static,
{
    type TableMapping;

    fn table_mut(&mut self) -> &mut ListEditorTable<Self::TableMapping>;

    // Called by ListWithCompilerOutput when the list's size or item order changes.
    fn selected_item_changed(&mut self, _list: &ListWithCompilerOutput<T, O>) {}

    fn list_edited(&mut self, _action: &ListAction<T>) {}
    fn item_edited(&mut self, _id: ItemId, _value: &T) {}
    fn set_compiler_output(&mut self, _index: usize, _id: ItemId, _compiler_output: &Option<O>) {}
}

impl<T, O> ListState for ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameGetter + NameSetter,
    O: CompilerOutput,
{
    type Item = T;

    fn len(&self) -> usize {
        self.list.len()
    }

    fn can_add(&self) -> bool {
        self.list.len() < self.max_size
    }

    fn can_add_multiple(&self, count: usize) -> bool {
        self.list.len() + count <= self.max_size
    }

    fn item_iter(&self) -> impl Iterator<Item = &T> {
        self.list.iter().map(|(_id, item)| item)
    }
}

impl<T, O> ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameGetter + NameSetter,
    T: NameGetter + NameSetter,
    O: CompilerOutput,
{
    pub fn new(list: DeduplicatedNameVec<T>, max_size: usize) -> Self {
        let list: Vec<(ItemId, T)> = list
            .into_vec()
            .into_iter()
            .map(|i| (ItemId::new(), i))
            .collect();

        // Compiler output is not cloneable
        let compiler_output = std::iter::repeat_with(|| None).take(list.len()).collect();

        Self {
            max_size,
            list,
            compiler_output,
            error_set: HashSet::new(),
        }
    }

    pub fn all_valid(&self) -> bool {
        self.error_set.is_empty()
    }

    pub fn set_compiler_output(
        &mut self,
        id: ItemId,
        co: O,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) {
        match co.is_valid() {
            true => self.error_set.remove(&id),
            false => self.error_set.insert(id),
        };

        let co = Some(co);

        if let Some(index) = self.id_to_index(id) {
            editor.table_mut().set_compiler_output(index, &co);
            editor.set_compiler_output(index, id, &co);

            if let Some(co_item) = self.compiler_output.get_mut(index) {
                *co_item = co;
            }
        }
    }

    fn edit_item_index(
        &mut self,
        index: usize,
        mut new_value: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        if let Some((id, old_value)) = self.list.get(index) {
            if *old_value != new_value {
                let id = *id;

                if old_value.name() != new_value.name() {
                    deduplicate_item_name_inplace(&mut new_value, &self.list, Some(index));
                }

                self.list[index].1 = new_value.clone();
                self.compiler_output[index] = None;

                let action = ListAction::Edit(index, new_value);
                editor.table_mut().list_edited(&action);
                editor.list_edited(&action);

                if let ListAction::Edit(_, item) = action {
                    editor.item_edited(id, &item);

                    (true, Some(ItemChanged::AddedOrEdited(id, item)))
                } else {
                    panic!();
                }
            } else {
                (false, None)
            }
        } else {
            (false, None)
        }
    }

    fn add_item(
        &mut self,
        id: Option<ItemId>,
        index: usize,
        mut item: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        if self.can_add() && index <= self.list.len() {
            let id = id.unwrap_or_else(ItemId::new);

            deduplicate_item_name_inplace(&mut item, &self.list, None);

            self.list.insert(index, (id, item.clone()));
            self.compiler_output.insert(index, None);

            let action = ListAction::Add(index, item);
            editor.table_mut().list_edited(&action);
            editor.list_edited(&action);
            editor.selected_item_changed(self);

            if let ListAction::Add(_, item) = action {
                (true, Some(ItemChanged::AddedOrEdited(id, item)))
            } else {
                panic!();
            }
        } else {
            (false, None)
        }
    }

    fn add_multiple_items(
        &mut self,
        index: usize,
        mut items: Vec<T>,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        if self.can_add_multiple(items.len()) && index <= items.len() {
            let mut new_items = Vec::with_capacity(items.len());

            // Must add and deduplicate items one at a time to ensure names are unique.
            for item in &mut items {
                deduplicate_item_name_inplace(item, &self.list, None);

                let i = index + new_items.len();
                let item_with_id = (ItemId::new(), item.clone());

                self.list.insert(i, item_with_id.clone());
                self.compiler_output.insert(i, None);

                new_items.push(item_with_id);
            }

            let action = ListAction::AddMultiple(index, items);
            editor.table_mut().list_edited(&action);
            editor.list_edited(&action);
            editor.selected_item_changed(self);

            (true, Some(ItemChanged::MultipleAddedOrEdited(new_items)))
        } else {
            (false, None)
        }
    }

    fn remove_item(
        &mut self,
        index: usize,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        if let Some((id, _)) = self.list.get(index) {
            let id = *id;

            self.list.remove(index);
            self.compiler_output.remove(index);

            let action = ListAction::Remove(index);
            editor.table_mut().list_edited(&action);
            editor.list_edited(&action);
            editor.selected_item_changed(self);

            (true, Some(ItemChanged::Removed(id)))
        } else {
            (false, None)
        }
    }

    fn move_item(
        &mut self,
        from: usize,
        to: usize,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        let range = 0..self.list.len();
        if range.contains(&from) && range.contains(&to) && from != to {
            move_list_item(&mut self.list, from, to);
            move_list_item(&mut self.compiler_output, from, to);

            let action = ListAction::Move(from, to);
            editor.table_mut().list_edited(&action);
            editor.list_edited(&action);
            editor.selected_item_changed(self);

            (true, None)
        } else {
            (false, None)
        }
    }

    #[must_use]
    pub fn process(
        &mut self,
        m: ListMessage<T>,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        match m {
            ListMessage::ItemEdited(index, new_value) => {
                self.edit_item_index(index, new_value, editor)
            }
            ListMessage::Add(item) => self.add_item(None, self.list.len(), item, editor),
            ListMessage::AddWithItemId(id, item) => {
                if !self.contains_id(id) {
                    self.add_item(Some(id), self.list.len(), item, editor)
                } else {
                    (false, None)
                }
            }
            ListMessage::AddMultiple(items) => {
                self.add_multiple_items(self.list.len(), items, editor)
            }
            ListMessage::Clone(index) => match self.list.get(index) {
                Some(item) => self.add_item(None, index + 1, item.1.clone(), editor),
                None => (false, None),
            },
            ListMessage::Remove(index) => self.remove_item(index, editor),
            ListMessage::MoveToTop(index) => self.move_item(index, 0, editor),
            ListMessage::MoveUp(index) => {
                if index > 0 {
                    self.move_item(index, index - 1, editor)
                } else {
                    (false, None)
                }
            }
            ListMessage::MoveDown(index) => self.move_item(index, index + 1, editor),
            ListMessage::MoveToBottom(index) => {
                let len = self.list.len();
                if len > 0 {
                    self.move_item(index, len - 1, editor)
                } else {
                    (false, None)
                }
            }
        }
    }

    #[must_use]
    pub fn edit_item(
        &mut self,
        id: ItemId,
        new_value: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        match self.id_to_index(id) {
            Some(index) => self.edit_item_index(index, new_value, editor),
            None => (false, None),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &(ItemId, T)> {
        self.list.iter()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.list.get(index).map(|(_id, item)| item)
    }

    pub fn get_with_id(&self, index: usize) -> Option<(ItemId, &T)> {
        self.list.get(index).map(|(id, item)| (*id, item))
    }

    fn id_to_index(&self, id: ItemId) -> Option<usize> {
        self.list.iter().position(|i| i.0 == id)
    }

    fn contains_id(&self, id: ItemId) -> bool {
        self.list.iter().any(|i| i.0 == id)
    }

    pub fn get_id(&self, id: ItemId) -> Option<(usize, &T)> {
        self.list
            .iter()
            .enumerate()
            .find(|(_, i)| i.0 == id)
            .map(|(id, i)| (id, &i.1))
    }

    // Assumes `table` is in sync with `self`.
    pub fn get_selected_row<M>(
        &self,
        table: &ListEditorTable<M>,
    ) -> Option<(ItemId, &T, &Option<O>)>
    where
        M: TableMapping<DataType = T>,
    {
        let i = table.selected_row()?;
        let item = self.list.get(i)?;
        let co = self.compiler_output.get(i)?;

        Some((item.0, &item.1, co))
    }

    #[must_use]
    pub fn replace_all_vec(&self) -> compiler_thread::ReplaceAllVec<T> {
        compiler_thread::ReplaceAllVec::new(self.list.clone())
    }
}

pub struct ListPairWithCompilerOutputs<T1, O1, T2, O2>
where
    T1: Clone + PartialEq<T1> + NameGetter + NameSetter,
    O1: CompilerOutput,
    T2: Clone + PartialEq<T2> + NameGetter + NameSetter,
    O2: CompilerOutput,
{
    list1: ListWithCompilerOutput<T1, O1>,
    list2: ListWithCompilerOutput<T2, O2>,
}

#[inline]
fn list_pair_process<T1, O1, T2, O2, Editor>(
    m: ListMessage<T1>,
    list1: &mut ListWithCompilerOutput<T1, O1>,
    list2: &mut ListWithCompilerOutput<T2, O2>,
    editor: &mut Editor,
) -> (bool, Option<ItemChanged<T1>>)
where
    T1: Clone + PartialEq<T1> + NameGetter + NameSetter + std::fmt::Debug,
    O1: CompilerOutput,
    T2: Clone + PartialEq<T2> + NameGetter + NameSetter + std::fmt::Debug,
    O2: CompilerOutput,
    Editor: ListWithCompilerOutputEditor<T1, O1> + ListWithCompilerOutputEditor<T2, O2>,
{
    let can_add = |count: usize| -> bool {
        debug_assert!(list1.max_size == list2.max_size);

        let max_size = list1.max_size;
        let len = list1.len() + list2.len();

        len + count <= max_size
    };
    let can_do_message = match &m {
        ListMessage::Add(..) => can_add(1),
        ListMessage::AddWithItemId(..) => can_add(1),
        ListMessage::AddMultiple(vec) => can_add(vec.len()),
        ListMessage::Clone(..) => can_add(1),

        ListMessage::ItemEdited(..)
        | ListMessage::Remove(..)
        | ListMessage::MoveToTop(..)
        | ListMessage::MoveUp(..)
        | ListMessage::MoveDown(..)
        | ListMessage::MoveToBottom(..) => true,
    };

    if can_do_message {
        // ::TODO deduplicate name::
        list1.process(m, editor)
    } else {
        (false, None)
    }
}

impl<T1, O1, T2, O2> ListPairWithCompilerOutputs<T1, O1, T2, O2>
where
    T1: Clone + PartialEq<T1> + NameGetter + NameSetter + std::fmt::Debug,
    O1: CompilerOutput,
    T2: Clone + PartialEq<T2> + NameGetter + NameSetter + std::fmt::Debug,
    O2: CompilerOutput,
{
    pub fn new(
        list1: DeduplicatedNameVec<T1>,
        list2: DeduplicatedNameVec<T2>,
        max_size: usize,
    ) -> Self {
        let list1 = ListWithCompilerOutput::new(list1, max_size);
        let list2 = ListWithCompilerOutput::new(list2, max_size);

        Self { list1, list2 }
    }

    pub fn list1(&self) -> &ListWithCompilerOutput<T1, O1> {
        &self.list1
    }
    pub fn list2(&self) -> &ListWithCompilerOutput<T2, O2> {
        &self.list2
    }

    #[must_use]
    pub fn process1<Editor>(
        &mut self,
        m: ListMessage<T1>,
        editor: &mut Editor,
    ) -> (bool, Option<ItemChanged<T1>>)
    where
        Editor: ListWithCompilerOutputEditor<T1, O1> + ListWithCompilerOutputEditor<T2, O2>,
    {
        list_pair_process(m, &mut self.list1, &mut self.list2, editor)
    }

    #[must_use]
    pub fn process2<Editor>(
        &mut self,
        m: ListMessage<T2>,
        editor: &mut Editor,
    ) -> (bool, Option<ItemChanged<T2>>)
    where
        Editor: ListWithCompilerOutputEditor<T1, O1> + ListWithCompilerOutputEditor<T2, O2>,
    {
        list_pair_process(m, &mut self.list2, &mut self.list1, editor)
    }

    #[must_use]
    pub fn edit_item1(
        &mut self,
        id: ItemId,
        new_value: T1,
        editor: &mut impl ListWithCompilerOutputEditor<T1, O1>,
    ) -> (bool, Option<ItemChanged<T1>>) {
        self.list1.edit_item(id, new_value, editor)
    }

    #[must_use]
    pub fn edit_item2(
        &mut self,
        id: ItemId,
        new_value: T2,
        editor: &mut impl ListWithCompilerOutputEditor<T2, O2>,
    ) -> (bool, Option<ItemChanged<T2>>) {
        self.list2.edit_item(id, new_value, editor)
    }

    pub fn set_compiler_output1(
        &mut self,
        id: ItemId,
        co: O1,
        editor: &mut impl ListWithCompilerOutputEditor<T1, O1>,
    ) {
        self.list1.set_compiler_output(id, co, editor)
    }

    pub fn set_compiler_output2(
        &mut self,
        id: ItemId,
        co: O2,
        editor: &mut impl ListWithCompilerOutputEditor<T2, O2>,
    ) {
        self.list2.set_compiler_output(id, co, editor)
    }
}

pub struct ListButtons {
    pub pack: Pack,

    pub max_size: usize,

    pub add: Button,
    pub clone: Option<Button>,
    pub remove: Button,
    pub move_top: Button,
    pub move_up: Button,
    pub move_down: Button,
    pub move_bottom: Button,
}

impl ListButtons {
    pub fn new(type_name: &str, max_size: usize, show_clone: bool) -> Self {
        let mut pack = Pack::default().with_type(PackType::Horizontal);

        let button_label_size = pack.label_size() * 8 / 10;
        pack.set_label_size(button_label_size);

        let button_size = ch_units_to_width(&pack, 4);

        let button = |label: &str, tooltip: String| {
            let mut b = Button::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(&tooltip);
            b.set_label_size(button_label_size);
            b
        };

        let add = button("@add", format!("Add {}", type_name));
        let clone = if show_clone {
            Some(button("@clone", format!("Clone {}", type_name)))
        } else {
            None
        };
        let remove = button("@remove", format!("Remove {}", type_name));
        let move_top = button("@top", format!("Move {} to top", type_name));
        let move_up = button("@up", format!("Move {} up", type_name));
        let move_down = button("@down", format!("Move {} down", type_name));
        let move_bottom = button("@bottom", format!("Move {} to bottom", type_name));

        pack.end();

        let mut out = Self {
            pack,
            max_size,
            add,
            clone,
            remove,
            move_top,
            move_up,
            move_down,
            move_bottom,
        };
        out.deactivate_all();
        out
    }

    fn selected_changed(&mut self, index: usize, list_len: usize) {
        let can_add = list_len < self.max_size;

        self.add.set_active(can_add);

        if let Some(c) = &mut self.clone {
            c.set_active(can_add);
        }
        self.remove.activate();

        if index > 0 {
            self.move_top.activate();
            self.move_up.activate();
        } else {
            self.move_top.deactivate();
            self.move_up.deactivate();
        }

        if index + 1 < list_len {
            self.move_down.activate();
            self.move_bottom.activate();
        } else {
            self.move_down.deactivate();
            self.move_bottom.deactivate();
        }
    }

    fn selected_clear(&mut self, list_len: usize) {
        let can_add = list_len < self.max_size;

        self.add.set_active(can_add);

        if let Some(c) = &mut self.clone {
            c.deactivate();
        }
        self.remove.deactivate();
        self.move_top.deactivate();
        self.move_up.deactivate();
        self.move_down.deactivate();
        self.move_bottom.deactivate();
    }

    pub fn update_buttons(&mut self, selected: Option<usize>, list_len: usize) {
        match selected {
            Some(i) => self.selected_changed(i, list_len),
            None => self.selected_clear(list_len),
        }
    }

    pub fn deactivate_all(&mut self) {
        self.add.deactivate();

        if let Some(c) = &mut self.clone {
            c.deactivate();
        }
        self.remove.deactivate();
        self.move_top.deactivate();
        self.move_up.deactivate();
        self.move_down.deactivate();
        self.move_bottom.deactivate();
    }
}

pub enum TableAction {
    None,
    OpenEditor,
    Send(GuiMessage),
}

pub trait TableMapping
where
    Self::DataType: Sized + Clone + std::cmp::PartialEq<Self::DataType>,
    Self::RowType: tables::TableRow + 'static,
{
    type DataType;
    type RowType;

    const MAX_SIZE: usize;

    const CAN_CLONE: bool;
    const CAN_EDIT: bool;

    fn headers() -> Vec<String>;
    fn type_name() -> &'static str;

    fn add_clicked() -> GuiMessage;
    fn to_message(lm: ListMessage<Self::DataType>) -> GuiMessage;

    fn new_row(d: &Self::DataType) -> Self::RowType;
    fn edit_row(r: &mut Self::RowType, d: &Self::DataType) -> bool;

    fn table_event(event: tables::TableEvent, row: usize, col: i32) -> TableAction {
        let _ = (event, row, col);
        TableAction::None
    }

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<GuiMessage> {
        let _ = (index, col, value);
        None
    }

    fn user_changes_selection() -> Option<GuiMessage> {
        None
    }
}

pub trait TableCompilerOutput
where
    Self: TableMapping,
{
    type CompilerOutputType;

    fn set_row_state(r: &mut Self::RowType, co: &Option<Self::CompilerOutputType>) -> bool;
}

pub struct ListEditorTable<T>
where
    T: TableMapping,
{
    list_buttons_pack: Pack,

    // Must store list_buttons in a separate Rc to prevent a BorrowMutError in set_selection_changed_callback
    list_buttons: Rc<RefCell<ListButtons>>,

    table: Rc<RefCell<tables::TrTable<T::RowType>>>,
}

impl<T> ListEditorTable<T>
where
    T: TableMapping,
{
    pub fn new(sender: fltk::app::Sender<GuiMessage>) -> Self {
        let list_buttons = Rc::new(RefCell::new(ListButtons::new(
            T::type_name(),
            T::MAX_SIZE,
            T::CAN_CLONE,
        )));
        let table = Rc::new(RefCell::new(tables::TrTable::new(T::headers())));

        let mut t = table.borrow_mut();
        let mut lb = list_buttons.borrow_mut();

        t.set_selection_changed_callback({
            let s = sender.clone();
            let list_buttons = list_buttons.clone();
            move |selected, n_rows, user_selection| {
                list_buttons.borrow_mut().update_buttons(selected, n_rows);

                if user_selection {
                    if let Some(m) = T::user_changes_selection() {
                        s.send(m)
                    }
                }
            }
        });

        if T::CAN_EDIT {
            t.enable_cell_editing({
                // Commit edited value
                let s = sender.clone();
                move |index, col, value| {
                    if let Some(m) = T::commit_edited_value(index, col, value) {
                        s.send(m);
                    }
                }
            });
        }

        t.set_callback({
            let s = sender.clone();
            move |ev, row, col| match T::table_event(ev, row, col) {
                TableAction::None => false,
                TableAction::OpenEditor => true,
                TableAction::Send(m) => {
                    s.send(m);
                    false
                }
            }
        });

        lb.add.set_callback({
            let s = sender.clone();
            move |_| s.send(T::add_clicked())
        });
        if let Some(b) = &mut lb.clone {
            b.set_callback({
                let s = sender.clone();
                let table = table.clone();
                move |_| {
                    if let Some(i) = table.borrow().selected_row() {
                        s.send(T::to_message(ListMessage::Clone(i)))
                    }
                }
            });
        }
        lb.remove.set_callback({
            let s = sender.clone();
            let table = table.clone();
            move |_| {
                if let Some(i) = table.borrow().selected_row() {
                    s.send(T::to_message(ListMessage::Remove(i)))
                }
            }
        });
        lb.move_top.set_callback({
            let s = sender.clone();
            let table = table.clone();
            move |_| {
                if let Some(i) = table.borrow().selected_row() {
                    s.send(T::to_message(ListMessage::MoveToTop(i)))
                }
            }
        });
        lb.move_up.set_callback({
            let s = sender.clone();
            let table = table.clone();
            move |_| {
                if let Some(i) = table.borrow().selected_row() {
                    s.send(T::to_message(ListMessage::MoveUp(i)))
                }
            }
        });
        lb.move_down.set_callback({
            let s = sender.clone();
            let table = table.clone();
            move |_| {
                if let Some(i) = table.borrow().selected_row() {
                    s.send(T::to_message(ListMessage::MoveDown(i)))
                }
            }
        });
        lb.move_bottom.set_callback({
            let s = sender;
            let table = table.clone();
            move |_| {
                if let Some(i) = table.borrow().selected_row() {
                    s.send(T::to_message(ListMessage::MoveToBottom(i)))
                }
            }
        });
        Self::update_list_buttons(&mut lb, &t);

        let list_buttons_pack = lb.pack.clone();

        drop(lb);
        drop(t);

        Self {
            list_buttons_pack,
            list_buttons,
            table,
        }
    }

    pub fn new_from_slice(data: &[T::DataType], sender: fltk::app::Sender<GuiMessage>) -> Self {
        let out = Self::new(sender);

        {
            let mut t = out.table.borrow_mut();
            let mut lb = out.list_buttons.borrow_mut();

            t.edit_table(|v| v.extend(data.iter().map(T::new_row)));

            Self::update_list_buttons(&mut lb, &t);
        }

        out
    }

    pub fn new_with_data(
        state: &impl ListState<Item = T::DataType>,
        sender: fltk::app::Sender<GuiMessage>,
    ) -> Self {
        let mut out = Self::new(sender);
        out.replace(state);
        out
    }

    pub fn replace(&mut self, state: &impl ListState<Item = T::DataType>) {
        let mut t = self.table.borrow_mut();
        let mut lb = self.list_buttons.borrow_mut();

        t.edit_table(|v| {
            *v = state.item_iter().map(T::new_row).collect();
        });
        t.clear_selected();

        Self::update_list_buttons(&mut lb, &t);
    }

    fn update_list_buttons(list_buttons: &mut ListButtons, table: &tables::TrTable<T::RowType>) {
        list_buttons.update_buttons(table.selected_row(), table.n_rows());
    }

    pub fn button_height(&self) -> i32 {
        self.list_buttons.borrow().add.height()
    }

    pub fn list_buttons_pack(&self) -> &Pack {
        &self.list_buttons_pack
    }

    pub fn selected_row(&self) -> Option<usize> {
        self.table.borrow().selected_row()
    }

    pub fn set_selected_row(&mut self, index: usize) {
        self.table.borrow_mut().set_selected(index);
    }

    pub fn clear_selected_row(&mut self) {
        self.table.borrow_mut().clear_selected();
    }

    pub fn open_editor(&mut self, index: usize, col: i32) {
        self.table.borrow_mut().open_editor(index, col);
    }
}

impl<T> ListEditorTable<T>
where
    T: TableMapping,
{
    fn list_edited(&mut self, action: &ListAction<T::DataType>) {
        let mut table = self.table.borrow_mut();

        match action {
            ListAction::None => (),
            ListAction::Edit(index, value) => {
                table.edit_row(*index, |d| -> bool { T::edit_row(d, value) });
            }
            a => table.edit_table(|table_vec| {
                process_list_action_map(table_vec, a, T::new_row, {
                    |row, new_value| {
                        T::edit_row(row, new_value);
                    }
                })
            }),
        }

        match action {
            ListAction::None => (),
            ListAction::Edit(..) => (),
            ListAction::Add(index, _) => table.force_set_selected(*index),
            ListAction::AddMultiple(..) => table.force_clear_selected(),
            ListAction::Remove(index) => {
                if table.selected_row() == Some(*index) {
                    table.force_clear_selected();
                }
            }
            ListAction::Move(from, to) => {
                if table.selected_row() == Some(*from) {
                    table.force_set_selected(*to);
                }
            }
        }
    }
}

impl<T> ListEditorTable<T>
where
    T: TableMapping + TableCompilerOutput,
{
    fn set_compiler_output(
        &mut self,
        index: usize,
        compiler_output: &Option<T::CompilerOutputType>,
    ) {
        self.table.borrow_mut().edit_row(index, |row| -> bool {
            T::set_row_state(row, compiler_output)
        });
    }
}

impl<T> ListEditorTable<T>
where
    T: sfx_export_order::SfxEoMapping,
{
    // Must only be called by SfxExportOrderEditor
    pub fn sfx_eo_edited(&mut self, action: &ListAction<Name>) {
        self.list_edited(action);
    }
}
