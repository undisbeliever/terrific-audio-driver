//! List Editors

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{self, ItemChanged, ItemId};
use crate::helpers::ch_units_to_width;
use crate::names::{
    deduplicate_name_iter, DeduplicatedNameVec, NameGetter, NameSetter, TwoDeduplicatedNameVecs,
};
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

// CAUTION: A ListMessage SHOULD ONLY be called once per frame.
#[derive(Debug)]
pub enum ListMessage<T> {
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

impl<T> ListMessage<T> {
    fn may_resize_list(&self) -> bool {
        match self {
            Self::ItemEdited(..) => false,

            Self::Add(..)
            | Self::AddMultiple(..)
            | Self::AddWithItemId(..)
            | Self::Clone(..)
            | Self::Remove(..) => true,

            Self::MoveToTop(..)
            | Self::MoveUp(..)
            | Self::MoveDown(..)
            | Self::MoveToBottom(..) => false,
        }
    }
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

trait OtherList {
    fn len(&self) -> usize;

    fn chain_name_iter<'a>(
        &'a self,
        main: impl Iterator<Item = &'a Name>,
    ) -> impl Iterator<Item = &'a Name>;
}

impl OtherList for () {
    fn len(&self) -> usize {
        0
    }

    fn chain_name_iter<'a>(
        &'a self,
        main: impl Iterator<Item = &'a Name>,
    ) -> impl Iterator<Item = &'a Name> {
        main
    }
}

impl<T, O> OtherList for &ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameGetter + NameSetter,
    O: CompilerOutput,
{
    fn len(&self) -> usize {
        ListWithCompilerOutput::len(self)
    }

    fn chain_name_iter<'a>(
        &'a self,
        main: impl Iterator<Item = &'a Name>,
    ) -> impl Iterator<Item = &'a Name> {
        main.chain(self.iter().map(NameGetter::name))
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

    fn deduplicate_item_name_inplace(
        &self,
        item: &mut T,
        other_list: &impl OtherList,
        index: Option<usize>,
    ) {
        let name_iter = other_list.chain_name_iter(self.list.iter().map(NameGetter::name));

        if let Some(new_name) = deduplicate_name_iter(item.name(), name_iter, index) {
            item.set_name(new_name)
        }
    }

    fn edit_item_index(
        &mut self,
        other_list: impl OtherList,
        index: usize,
        mut new_value: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        if let Some((id, old_value)) = self.list.get(index) {
            if *old_value != new_value {
                let id = *id;

                if old_value.name() != new_value.name() {
                    self.deduplicate_item_name_inplace(&mut new_value, &other_list, Some(index));
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
        other_list: impl OtherList,
        id: Option<ItemId>,
        index: usize,
        mut item: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        let can_add = self.list.len() + other_list.len() < self.max_size;

        if can_add && index <= self.list.len() {
            let id = id.unwrap_or_else(ItemId::new);

            self.deduplicate_item_name_inplace(&mut item, &other_list, None);

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
        other_list: impl OtherList,
        index: usize,
        mut items: Vec<T>,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        let can_add = self.list.len() + other_list.len() + items.len() <= self.max_size;

        if can_add && index <= items.len() {
            let mut new_items = Vec::with_capacity(items.len());

            // Must add and deduplicate items one at a time to ensure names are unique.
            for item in &mut items {
                self.deduplicate_item_name_inplace(item, &other_list, None);

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

    #[inline]
    fn process_(
        &mut self,
        other_list: impl OtherList,
        m: ListMessage<T>,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        match m {
            ListMessage::ItemEdited(index, new_value) => {
                self.edit_item_index(other_list, index, new_value, editor)
            }
            ListMessage::Add(item) => {
                self.add_item(other_list, None, self.list.len(), item, editor)
            }
            ListMessage::AddWithItemId(id, item) => {
                if !self.contains_id(id) {
                    self.add_item(other_list, Some(id), self.list.len(), item, editor)
                } else {
                    (false, None)
                }
            }
            ListMessage::AddMultiple(items) => {
                self.add_multiple_items(other_list, self.list.len(), items, editor)
            }
            ListMessage::Clone(index) => match self.list.get(index) {
                Some(item) => self.add_item(other_list, None, index + 1, item.1.clone(), editor),
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

    #[inline]
    fn edit_item_(
        &mut self,
        other_list: impl OtherList,
        id: ItemId,
        new_value: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        match self.id_to_index(id) {
            Some(index) => self.edit_item_index(other_list, index, new_value, editor),
            None => (false, None),
        }
    }

    #[must_use]
    pub fn process(
        &mut self,
        m: ListMessage<T>,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        self.process_((), m, editor)
    }

    #[must_use]
    pub fn edit_item(
        &mut self,
        id: ItemId,
        new_value: T,
        editor: &mut impl ListWithCompilerOutputEditor<T, O>,
    ) -> (bool, Option<ItemChanged<T>>) {
        self.edit_item_((), id, new_value, editor)
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

impl<T1, O1, T2, O2> ListPairWithCompilerOutputs<T1, O1, T2, O2>
where
    T1: Clone + PartialEq<T1> + NameGetter + NameSetter + std::fmt::Debug,
    O1: CompilerOutput,
    T2: Clone + PartialEq<T2> + NameGetter + NameSetter + std::fmt::Debug,
    O2: CompilerOutput,
{
    pub fn new(lists: TwoDeduplicatedNameVecs<T1, T2>, max_size: usize) -> Self {
        let (list1, list2) = lists.into_tuple();

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
        let may_resize_list = m.may_resize_list();

        let out = self.list1.process_(&self.list2, m, editor);

        if may_resize_list {
            // Assumes list1 and list2 have the same max_size
            ListWithCompilerOutputEditor::<T2, O2>::table_mut(editor)
                .set_max_size(self.list1.max_size.saturating_sub(self.list1.len()));
        }

        out
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
        let may_resize_list = m.may_resize_list();

        let out = self.list2.process_(&self.list1, m, editor);

        if may_resize_list {
            // Assumes list1 and list2 have the same max_size
            ListWithCompilerOutputEditor::<T1, O1>::table_mut(editor)
                .set_max_size(self.list2.max_size.saturating_sub(self.list2.len()));
        }

        out
    }

    #[must_use]
    pub fn edit_item1(
        &mut self,
        id: ItemId,
        new_value: T1,
        editor: &mut impl ListWithCompilerOutputEditor<T1, O1>,
    ) -> (bool, Option<ItemChanged<T1>>) {
        self.list1.edit_item_(&self.list2, id, new_value, editor)
    }

    #[must_use]
    pub fn edit_item2(
        &mut self,
        id: ItemId,
        new_value: T2,
        editor: &mut impl ListWithCompilerOutputEditor<T2, O2>,
    ) -> (bool, Option<ItemChanged<T2>>) {
        self.list2.edit_item_(&self.list1, id, new_value, editor)
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

/// Arguments to the ListEditorTableButtons update callback
pub struct UpdateButtonArgs {
    pub max_len: usize,
    pub list_len: usize,
    pub selected: Option<usize>,
}

/// Callback used to enable or disable ListEditorTableButtons
type UpdateButtonCallback = Box<dyn Fn(&UpdateButtonArgs) -> bool>;

struct ListEditorTableButtons {
    pack: Pack,

    button_size: i32,
    label_size: i32,

    max_size: usize,
    buttons: Vec<(Button, UpdateButtonCallback)>,
}

impl ListEditorTableButtons {
    fn new(max_size: usize) -> Self {
        let mut pack = Pack::default().with_type(PackType::Horizontal);
        pack.end();

        let label_size = pack.label_size() * 8 / 10;
        pack.set_label_size(label_size);

        Self {
            button_size: ch_units_to_width(&pack, 4),
            label_size,
            max_size,
            buttons: Vec::new(),

            pack,
        }
    }

    fn add_button(
        &mut self,
        label: &str,
        tooltip: &str,
        update_cb: UpdateButtonCallback,
        callback: impl FnMut(&mut Button) + 'static,
        list_len: usize,
        selected: Option<usize>,
    ) {
        let mut b = Button::default()
            .with_size(self.button_size, self.button_size)
            .with_label(label);
        b.set_tooltip(tooltip);
        b.set_label_size(self.label_size);

        let sel_args = UpdateButtonArgs {
            max_len: self.max_size,
            list_len,
            selected,
        };
        match update_cb(&sel_args) {
            true => b.activate(),
            false => b.deactivate(),
        }

        b.set_callback(callback);

        self.pack.add(&b);

        self.buttons.push((b, update_cb));
    }

    fn set_max_size_and_update_buttons(
        &mut self,
        max_size: usize,
        selected: Option<usize>,
        list_len: usize,
    ) {
        if self.max_size != max_size {
            self.max_size = max_size;
            self.update_buttons(selected, list_len);
        }
    }

    fn update_buttons(&mut self, selected: Option<usize>, list_len: usize) {
        let sel_args = UpdateButtonArgs {
            max_len: self.max_size,
            list_len,
            selected,
        };

        for (b, cb) in &mut self.buttons {
            match cb(&sel_args) {
                true => b.activate(),
                false => b.deactivate(),
            }
        }
    }
}

pub struct ListEditorTable<T>
where
    T: TableMapping,
{
    sender: fltk::app::Sender<GuiMessage>,

    list_buttons_pack: Pack,

    // Must store list_buttons in a separate Rc to prevent a BorrowMutError in set_selection_changed_callback
    list_buttons: Rc<RefCell<ListEditorTableButtons>>,

    table: Rc<RefCell<tables::TrTable<T::RowType>>>,
}

impl<T> ListEditorTable<T>
where
    T: TableMapping,
{
    pub fn new(sender: fltk::app::Sender<GuiMessage>) -> Self {
        let list_buttons = Rc::new(RefCell::new(ListEditorTableButtons::new(T::MAX_SIZE)));
        let table = Rc::new(RefCell::new(tables::TrTable::new(T::headers())));

        {
            let mut t = table.borrow_mut();

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
        }

        let list_buttons_pack = list_buttons.borrow().pack.clone();

        let mut out = Self {
            sender,
            list_buttons_pack,
            list_buttons,
            table,
        };

        let type_name = T::type_name();

        out.add_button(
            "@add",
            &format!("Add {type_name}"),
            |a| a.list_len < a.max_len,
            || T::add_clicked(),
        );
        if T::CAN_CLONE {
            out.add_sel_button(
                "@clone",
                &format!("Clone {type_name}"),
                |a| a.selected.is_some() && a.list_len < a.max_len,
                |index| T::to_message(ListMessage::Clone(index)),
            );
        }
        out.add_sel_button(
            "@remove",
            &format!("Remove {type_name}"),
            |a| a.selected.is_some(),
            |index| T::to_message(ListMessage::Remove(index)),
        );
        out.add_sel_button(
            "@top",
            &format!("Move {type_name} to top"),
            |a| a.selected.is_some_and(|i| i > 0),
            |index| T::to_message(ListMessage::MoveToTop(index)),
        );
        out.add_sel_button(
            "@up",
            &format!("Move {type_name} up"),
            |a| a.selected.is_some_and(|i| i > 0),
            |index| T::to_message(ListMessage::MoveUp(index)),
        );
        out.add_sel_button(
            "@down",
            &format!("Move {type_name} down"),
            |a| a.selected.is_some_and(|i| i + 1 < a.list_len),
            |index| T::to_message(ListMessage::MoveDown(index)),
        );
        out.add_sel_button(
            "@bottom",
            &format!("Move {type_name} to bottom"),
            |a| a.selected.is_some_and(|i| i + 1 < a.list_len),
            |index| T::to_message(ListMessage::MoveToBottom(index)),
        );

        out
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

    pub fn add_button(
        &mut self,
        label: &str,
        tooltip: &str,
        update_cb: impl Fn(&UpdateButtonArgs) -> bool + 'static,
        cb: impl Fn() -> GuiMessage + 'static,
    ) {
        let t = self.table.borrow();
        let mut lb = self.list_buttons.borrow_mut();

        lb.add_button(
            label,
            tooltip,
            Box::new(update_cb),
            {
                let s = self.sender.clone();
                move |_| s.send(cb())
            },
            t.n_rows(),
            t.selected_row(),
        );
    }

    pub fn add_sel_button(
        &mut self,
        label: &str,
        tooltip: &str,
        update_cb: impl Fn(&UpdateButtonArgs) -> bool + 'static,
        cb: impl Fn(usize) -> GuiMessage + 'static,
    ) {
        let t = self.table.borrow();
        let mut lb = self.list_buttons.borrow_mut();

        lb.add_button(
            label,
            tooltip,
            Box::new(update_cb),
            {
                let s = self.sender.clone();
                let t = self.table.clone();
                move |_| {
                    if let Some(i) = t.borrow().selected_row() {
                        s.send(cb(i))
                    }
                }
            },
            t.n_rows(),
            t.selected_row(),
        );
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

    pub fn set_max_size(&mut self, max_size: usize) {
        let t = self.table.borrow_mut();
        let mut lb = self.list_buttons.borrow_mut();

        lb.set_max_size_and_update_buttons(max_size, t.selected_row(), t.n_rows());
    }

    fn update_list_buttons(
        list_buttons: &mut ListEditorTableButtons,
        table: &tables::TrTable<T::RowType>,
    ) {
        list_buttons.update_buttons(table.selected_row(), table.n_rows());
    }

    pub fn button_height(&self) -> i32 {
        self.list_buttons.borrow().button_size
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
