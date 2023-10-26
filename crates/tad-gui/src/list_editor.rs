//! List Editors

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemChanged, ItemId};
use crate::helpers::SetActive;
use crate::names::{DeduplicatedNameVec, NameDeduplicator};
use crate::tables;
use crate::Message;

use fltk::button::Button;
use fltk::group::{Pack, PackType};
use fltk::prelude::{GroupExt, WidgetExt};

use std::ops::Deref;

// A ListMessage MUST ONLY be called once per frame
// (to prevent a potential infinite `ListMessage::ItemSelected` loop)
#[derive(Debug)]
pub enum ListMessage<T> {
    ClearSelection,
    ItemSelected(usize),

    // The `SelectedItemEdited` message has been removed because clicking on a TrTable list item
    // will cause the `ItemSelected` message to be sent before the `SelectedItemEdited` event.
    ItemEdited(usize, T),

    Add(T),
    AddMultiple(Vec<T>),
    CloneSelected,
    RemoveSelected,
    MoveSelectedToTop,
    MoveSelectedUp,
    MoveSelectedDown,
    MoveSelectedToBottom,
}

pub trait ListEditor<T> {
    // Called once at the start of the event
    fn list_buttons(&mut self) -> &mut ListButtons;

    fn list_edited(&mut self, action: &ListAction<T>);

    fn clear_selected(&mut self);
    fn set_selected(&mut self, index: usize, id: ItemId, value: &T);
}

pub trait CompilerOutputGui<T> {
    fn set_compiler_output(&mut self, index: usize, compiler_output: &Option<T>);
    fn set_selected_compiler_output(&mut self, compiler_output: &Option<T>);
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
        &ListAction::Move(from, to) => {
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
    }
}

/// A `Vec` that can only be resized or reordered by a `ListAction<T>`
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

pub struct ListData<T>
where
    T: Clone + PartialEq<T>,
{
    max_size: usize,
    list: LaVec<(ItemId, T)>,
}

impl<T> ListData<T>
where
    T: Clone + std::cmp::PartialEq<T>,
    T: NameDeduplicator,
{
    pub fn new(list: DeduplicatedNameVec<T>, max_size: usize) -> Self {
        let list = LaVec::from_vec(
            list.into_vec()
                .into_iter()
                .map(|i| (ItemId::new(), i))
                .collect(),
        );

        Self { max_size, list }
    }

    pub fn item_iter(&self) -> impl Iterator<Item = &T> {
        self.list.iter().map(|(_id, item)| item)
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.list.get(index).map(|(_id, item)| item)
    }

    pub fn get_with_id(&self, index: usize) -> Option<(ItemId, &T)> {
        self.list.get(index).map(|(id, item)| (*id, item))
    }

    pub fn can_add(&self) -> bool {
        self.list.len() < self.max_size
    }

    pub fn can_add_multiple(&self, count: usize) -> bool {
        self.list.len() + count <= self.max_size
    }

    fn id_to_index(&self, id: ItemId) -> Option<usize> {
        self.list.iter().position(|i| i.0 == id)
    }

    pub fn get_id(&self, id: ItemId) -> Option<(usize, &T)> {
        self.list
            .iter()
            .enumerate()
            .find(|(_, i)| i.0 == id)
            .map(|(id, i)| (id, &i.1))
    }

    #[must_use]
    fn replace_all_message(&self) -> ItemChanged<T> {
        ItemChanged::ReplaceAll(self.list.to_owned())
    }

    #[must_use]
    fn process(
        &mut self,
        m: ListMessage<T>,
        selected: Option<usize>,
    ) -> (ListAction<T>, Option<ItemChanged<T>>) {
        let update_list = |list: &mut LaVec<(ItemId, T)>, a: &ListAction<T>| {
            list.process_map(
                a,
                // new
                |v: &T| (ItemId::new(), v.clone()),
                // edit
                |e, v: &T| e.1 = v.clone(),
            );
        };

        match m {
            ListMessage::ClearSelection | ListMessage::ItemSelected(_) => (ListAction::None, None),

            ListMessage::ItemEdited(index, mut new_value) => {
                if let Some((id, item)) = self.list.get(index) {
                    if *item != new_value {
                        if NameDeduplicator::test_name_changed(item, &new_value) {
                            NameDeduplicator::dedupe_name(&mut new_value, &self.list, Some(index));
                        }

                        let c_message = ItemChanged::AddedOrEdited(*id, new_value.clone());

                        let action = ListAction::Edit(index, new_value);
                        update_list(&mut self.list, &action);
                        (action, Some(c_message))
                    } else {
                        (ListAction::None, None)
                    }
                } else {
                    (ListAction::None, None)
                }
            }

            ListMessage::Add(mut item) => {
                if self.can_add() {
                    NameDeduplicator::dedupe_name(&mut item, &self.list, None);

                    let i = self.list.len();
                    let action = ListAction::Add(i, item);
                    update_list(&mut self.list, &action);
                    let c_message = self
                        .list
                        .get(i)
                        .map(|(id, item)| ItemChanged::AddedOrEdited(*id, item.clone()));

                    (action, c_message)
                } else {
                    (ListAction::None, None)
                }
            }

            ListMessage::AddMultiple(mut items) => {
                if self.can_add_multiple(items.len()) {
                    let old_size = self.list.len();

                    let mut new_items_with_id = Vec::with_capacity(items.len());

                    // Must add and deduplicate items one at a time to ensure names are unique.
                    for item in &mut items {
                        NameDeduplicator::dedupe_name(item, &self.list, None);

                        let i = self.list.len();
                        let action = ListAction::Add(i, item.clone());
                        update_list(&mut self.list, &action);

                        if let Some(c) = self.list.get(i) {
                            new_items_with_id.push(c.clone());
                        }
                    }

                    let action = ListAction::AddMultiple(old_size, items);
                    let c_message = Some(ItemChanged::MultipleAddedOrEdited(new_items_with_id));

                    (action, c_message)
                } else {
                    (ListAction::None, None)
                }
            }

            lm => {
                // These list actions use the currently selected index
                let sel_index = match selected {
                    Some(i) if i < self.list.len() => i,
                    _ => return (ListAction::None, None),
                };

                match lm {
                    ListMessage::ClearSelection
                    | ListMessage::ItemSelected(_)
                    | ListMessage::ItemEdited(_, _)
                    | ListMessage::Add(_)
                    | ListMessage::AddMultiple(_) => (ListAction::None, None),

                    ListMessage::CloneSelected => {
                        if let (Some(item), true) = (self.get(sel_index), self.can_add()) {
                            let mut item = item.clone();
                            NameDeduplicator::dedupe_name(&mut item, &self.list, None);

                            let i = sel_index + 1;
                            let action = ListAction::Add(i, item);

                            update_list(&mut self.list, &action);
                            let c_message = self
                                .list
                                .get(i)
                                .map(|(id, item)| ItemChanged::AddedOrEdited(*id, item.clone()));

                            (action, c_message)
                        } else {
                            (ListAction::None, None)
                        }
                    }
                    ListMessage::RemoveSelected => {
                        let item_id = self.list[sel_index].0;
                        let c_message = ItemChanged::Removed(item_id);

                        let action = ListAction::Remove(sel_index);
                        update_list(&mut self.list, &action);

                        (action, Some(c_message))
                    }
                    ListMessage::MoveSelectedToTop => {
                        if sel_index > 0 {
                            let action = ListAction::Move(sel_index, 0);
                            update_list(&mut self.list, &action);

                            (action, None)
                        } else {
                            (ListAction::None, None)
                        }
                    }
                    ListMessage::MoveSelectedUp => {
                        if sel_index > 0 && sel_index < self.list.len() {
                            let action = ListAction::Move(sel_index, sel_index - 1);
                            update_list(&mut self.list, &action);

                            (action, None)
                        } else {
                            (ListAction::None, None)
                        }
                    }
                    ListMessage::MoveSelectedDown => {
                        if sel_index + 1 < self.list.len() {
                            let action = ListAction::Move(sel_index, sel_index + 1);
                            update_list(&mut self.list, &action);

                            (action, None)
                        } else {
                            (ListAction::None, None)
                        }
                    }
                    ListMessage::MoveSelectedToBottom => {
                        if sel_index + 1 < self.list.len() {
                            let action = ListAction::Move(sel_index, self.list.len() - 1);
                            update_list(&mut self.list, &action);

                            (action, None)
                        } else {
                            (ListAction::None, None)
                        }
                    }
                }
            }
        }
    }
}

pub trait ListState
where
    Self::Item: Clone + PartialEq<Self::Item> + NameDeduplicator,
{
    type Item;

    fn selected(&self) -> Option<usize>;
    fn list(&self) -> &ListData<Self::Item>;
    fn replace_all_message(&self) -> ItemChanged<Self::Item>;
}

pub struct ListWithSelection<T>
where
    T: Clone + PartialEq<T>,
{
    list: ListData<T>,
    selected: Option<usize>,
}

impl<T> ListState for ListWithSelection<T>
where
    T: Clone + PartialEq<T> + NameDeduplicator,
{
    type Item = T;

    fn selected(&self) -> Option<usize> {
        self.selected
    }

    fn list(&self) -> &ListData<T> {
        &self.list
    }

    fn replace_all_message(&self) -> ItemChanged<T> {
        self.list.replace_all_message()
    }
}

impl<T> ListWithSelection<T>
where
    T: Clone + PartialEq<T>,
    T: NameDeduplicator,
{
    pub fn new(list: DeduplicatedNameVec<T>, max_size: usize) -> Self {
        Self {
            list: ListData::new(list, max_size),
            selected: None,
        }
    }

    #[must_use]
    pub fn process(
        &mut self,
        m: ListMessage<T>,
        editor: &mut impl ListEditor<T>,
    ) -> (ListAction<T>, Option<ItemChanged<T>>) {
        match m {
            ListMessage::ClearSelection => {
                self.clear_selection(editor);
                (ListAction::None, None)
            }
            ListMessage::ItemSelected(index) => {
                self.set_selected(index, editor);
                (ListAction::None, None)
            }

            m => {
                let (action, c) = self.list.process(m, self.selected);
                self.process_action(&action, editor);
                (action, c)
            }
        }
    }

    fn process_action(&mut self, action: &ListAction<T>, editor: &mut impl ListEditor<T>) {
        editor.list_edited(action);
        match action {
            ListAction::Add(index, _) => {
                self.set_selected(*index, editor);
            }
            ListAction::AddMultiple(index, _) => {
                self.set_selected(*index, editor);
            }
            ListAction::Remove(index) => {
                if self.selected == Some(*index) {
                    self.clear_selection(editor);
                }
            }
            ListAction::Move(from, to) => {
                if self.selected == Some(*from) {
                    self.set_selected(*to, editor);
                }
            }
            ListAction::None => (),
            ListAction::Edit(_, _) => (),
        }
    }

    fn set_selected(&mut self, index: usize, editor: &mut impl ListEditor<T>) {
        match self.list.get_with_id(index) {
            Some((id, item)) => {
                self.selected = Some(index);
                editor.set_selected(index, id, item);
                editor.list_buttons().selected_changed(
                    index,
                    self.list.len(),
                    self.list().can_add(),
                );
            }
            None => {
                self.clear_selection(editor);
            }
        };
    }

    fn clear_selection(&mut self, editor: &mut impl ListEditor<T>) {
        self.selected = None;
        editor.clear_selected();
        editor.list_buttons().selected_clear(self.list.can_add());
    }
}

pub struct ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameDeduplicator,
{
    list: ListData<T>,
    compiler_output: Vec<Option<O>>,
    selected: Option<usize>,
}

impl<T, O> ListState for ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameDeduplicator,
{
    type Item = T;

    fn selected(&self) -> Option<usize> {
        self.selected
    }

    fn list(&self) -> &ListData<T> {
        &self.list
    }

    fn replace_all_message(&self) -> ItemChanged<T> {
        self.list.replace_all_message()
    }
}

impl<T, O> ListWithCompilerOutput<T, O>
where
    T: Clone + PartialEq<T> + NameDeduplicator,
{
    pub fn new(list: DeduplicatedNameVec<T>, max_size: usize) -> Self {
        let list = ListData::new(list, max_size);

        // Compiler output is not cloneable
        let compiler_output = std::iter::repeat_with(|| None).take(list.len()).collect();

        Self {
            list,
            compiler_output,
            selected: None,
        }
    }

    pub fn set_compiler_output(
        &mut self,
        id: ItemId,
        co: O,
        editor: &mut impl CompilerOutputGui<O>,
    ) {
        let co = Some(co);

        if let Some(index) = self.list.id_to_index(id) {
            editor.set_compiler_output(index, &co);

            if self.selected == Some(index) {
                editor.set_selected_compiler_output(&co);
            }
            if let Some(co_item) = self.compiler_output.get_mut(index) {
                *co_item = co;
            }
        }
    }

    #[must_use]
    pub fn process<Editor>(
        &mut self,
        m: ListMessage<T>,
        editor: &mut Editor,
    ) -> (ListAction<T>, Option<ItemChanged<T>>)
    where
        Editor: ListEditor<T> + CompilerOutputGui<O>,
    {
        match m {
            ListMessage::ClearSelection => {
                self.clear_selection(editor);
                (ListAction::None, None)
            }
            ListMessage::ItemSelected(index) => {
                self.set_selected(index, editor);
                (ListAction::None, None)
            }

            m => {
                let (action, c) = self.list.process(m, self.selected);
                self.process_action(&action, editor);
                (action, c)
            }
        }
    }

    fn process_action<Editor>(&mut self, action: &ListAction<T>, editor: &mut Editor)
    where
        Editor: ListEditor<T> + CompilerOutputGui<O>,
    {
        process_list_action_map(
            &mut self.compiler_output,
            action,
            // new
            |_| None,
            // edit
            |e, _| *e = None,
        );

        editor.list_edited(action);
        match action {
            ListAction::None => (),

            ListAction::Add(index, _) => {
                self.set_selected(*index, editor);
            }
            ListAction::AddMultiple(index, _) => {
                self.set_selected(*index, editor);
            }
            ListAction::Remove(index) => {
                if self.selected == Some(*index) {
                    self.clear_selection(editor);
                }
            }
            ListAction::Move(from, to) => {
                if self.selected == Some(*from) {
                    self.set_selected(*to, editor);
                }
            }
            ListAction::Edit(index, _) => {
                if let Some(co) = self.compiler_output.get_mut(*index) {
                    // Clear stored compiler output when an item is edited
                    *co = None;
                }
                // `editor.set_compiler_output` and `editor.set_selected_compiler_output`
                // are not called here to prevent an annoying flash in tables and the Sound Effect console.
            }
        }
    }

    fn set_selected<Editor>(&mut self, index: usize, editor: &mut Editor)
    where
        Editor: ListEditor<T> + CompilerOutputGui<O>,
    {
        match self.list.get_with_id(index) {
            Some((id, item)) => {
                self.selected = Some(index);
                editor.set_selected(index, id, item);

                let co = self.compiler_output.get(index).unwrap_or(&None);
                editor.set_selected_compiler_output(co);

                editor.list_buttons().selected_changed(
                    index,
                    self.list.len(),
                    self.list().can_add(),
                );
            }
            None => {
                self.clear_selection(editor);
            }
        };
    }

    fn clear_selection<Editor>(&mut self, editor: &mut Editor)
    where
        Editor: ListEditor<T> + CompilerOutputGui<O>,
    {
        self.selected = None;
        editor.clear_selected();
        editor.set_selected_compiler_output(&None);
        editor.list_buttons().selected_clear(self.list.can_add());
    }
}

pub fn update_compiler_output<CO, T>(
    id: ItemId,
    compiler_output: &Option<CO>,
    list: &ListData<T>,
    editor: &mut impl CompilerOutputGui<CO>,
) where
    T: Clone + PartialEq<T> + NameDeduplicator,
{
    if let Some(index) = list.id_to_index(id) {
        editor.set_compiler_output(index, compiler_output);
    }
}

pub struct ListButtons {
    pub pack: Pack,

    pub add: Button,
    pub clone: Option<Button>,
    pub remove: Button,
    pub move_top: Button,
    pub move_up: Button,
    pub move_down: Button,
    pub move_bottom: Button,
}

impl ListButtons {
    pub fn new(type_name: &str, show_clone: bool) -> Self {
        let pack = Pack::default().with_type(PackType::Horizontal);

        let button_size = pack.label_size() + 10;

        let button = |label: &str, tooltip: String| {
            let mut b = Button::default()
                .with_size(button_size, button_size)
                .with_label(label);
            b.set_tooltip(&tooltip);
            b
        };

        let add = button("@#+", format!("Add {}", type_name));
        let clone = if show_clone {
            Some(button("@#-9+", format!("Clone {}", type_name)))
        } else {
            None
        };
        let remove = button("@#line", format!("Remove {}", type_name));
        let move_top = button("@#8>|", format!("Move {} to top", type_name));
        let move_up = button("@#8>", format!("Move {} up", type_name));
        let move_down = button("@#2>", format!("Move {} down", type_name));
        let move_bottom = button("@#2>|", format!("Move {} to bottom", type_name));

        pack.end();

        let mut out = Self {
            pack,
            add,
            clone,
            remove,
            move_top,
            move_up,
            move_down,
            move_bottom,
        };
        out.selected_clear(false);
        out
    }

    pub fn update(&mut self, state: &impl ListState) {
        match state.selected() {
            Some(i) => self.selected_changed(i, state.list().len(), state.list().can_add()),
            None => self.selected_clear(state.list().can_add()),
        }
    }

    fn selected_changed(&mut self, index: usize, list_len: usize, can_add: bool) {
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

    fn selected_clear(&mut self, can_add: bool) {
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
}

pub enum TableAction {
    None,
    OpenEditor,
    Send(Message),
}

pub trait TableMapping
where
    Self::DataType: Sized + Clone + std::cmp::PartialEq<Self::DataType>,
    Self::RowType: tables::TableRow + 'static,
{
    type DataType;
    type RowType;

    const CAN_CLONE: bool;
    const CAN_EDIT: bool = false;

    fn headers() -> Vec<String>;
    fn type_name() -> &'static str;

    fn add_clicked() -> Message;
    fn to_message(lm: ListMessage<Self::DataType>) -> Message;

    fn new_row(d: &Self::DataType) -> Self::RowType;
    fn edit_row(r: &mut Self::RowType, d: &Self::DataType) -> bool;

    fn table_event(event: tables::TableEvent, row: usize, col: i32) -> TableAction {
        let _ = (event, row, col);
        TableAction::None
    }

    fn commit_edited_value(index: usize, col: i32, value: String) -> Option<Message> {
        let _ = (index, col, value);
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
    list_buttons: ListButtons,
    table: tables::TrTable<T::RowType>,
}

impl<T> ListEditorTable<T>
where
    T: TableMapping,
    T::DataType: NameDeduplicator,
{
    pub fn new(sender: fltk::app::Sender<Message>) -> Self {
        let mut list_buttons = ListButtons::new(T::type_name(), T::CAN_CLONE);
        let mut table = tables::TrTable::new(T::headers());

        table.set_selection_changed_callback({
            let s = sender.clone();
            move |i| match i {
                Some(i) => s.send(T::to_message(ListMessage::ItemSelected(i))),
                None => s.send(T::to_message(ListMessage::ClearSelection)),
            }
        });

        if T::CAN_EDIT {
            table.enable_cell_editing({
                // Commit edited value
                let s = sender.clone();
                move |index, col, value| {
                    if let Some(m) = T::commit_edited_value(index, col, value) {
                        s.send(m);
                    }
                }
            });
        }

        table.set_callback({
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

        list_buttons.add.set_callback({
            let s = sender.clone();
            move |_| s.send(T::add_clicked())
        });
        if let Some(b) = &mut list_buttons.clone {
            b.set_callback({
                let s = sender.clone();
                move |_| s.send(T::to_message(ListMessage::CloneSelected))
            });
        }
        list_buttons.remove.set_callback({
            let s = sender.clone();
            move |_| s.send(T::to_message(ListMessage::RemoveSelected))
        });
        list_buttons.move_top.set_callback({
            let s = sender.clone();
            move |_| s.send(T::to_message(ListMessage::MoveSelectedToTop))
        });
        list_buttons.move_up.set_callback({
            let s = sender.clone();
            move |_| s.send(T::to_message(ListMessage::MoveSelectedUp))
        });
        list_buttons.move_down.set_callback({
            let s = sender.clone();
            move |_| s.send(T::to_message(ListMessage::MoveSelectedDown))
        });
        list_buttons.move_bottom.set_callback({
            let s = sender;
            move |_| s.send(T::to_message(ListMessage::MoveSelectedToBottom))
        });

        Self {
            list_buttons,
            table,
        }
    }

    pub fn new_with_data(
        state: &impl ListState<Item = T::DataType>,
        sender: fltk::app::Sender<Message>,
    ) -> Self {
        let mut out = Self::new(sender);
        out.replace(state);
        out
    }

    pub fn replace(&mut self, state: &impl ListState<Item = T::DataType>) {
        self.table.edit_table(|v| {
            *v = state.list().item_iter().map(T::new_row).collect();
        });
        self.list_buttons.update(state);

        match state.selected() {
            Some(i) => self.table.set_selected(i),
            None => self.table.clear_selected(),
        }
    }

    pub fn button_height(&self) -> i32 {
        self.list_buttons.add.height()
    }
}

impl<T> ListEditor<T::DataType> for ListEditorTable<T>
where
    T: TableMapping + 'static,
    T::DataType: Clone,
{
    fn list_buttons(&mut self) -> &mut ListButtons {
        &mut self.list_buttons
    }

    fn list_edited(&mut self, action: &ListAction<T::DataType>) {
        match action {
            ListAction::None => (),
            ListAction::Edit(index, value) => {
                self.table
                    .edit_row(*index, |d| -> bool { T::edit_row(d, value) });
            }
            a => self.table.edit_table(|table_vec| {
                process_list_action_map(table_vec, a, T::new_row, |row, new_value| {
                    T::edit_row(row, new_value);
                })
            }),
        }
    }

    fn clear_selected(&mut self) {
        self.table.clear_selected();
    }

    fn set_selected(&mut self, index: usize, _: ItemId, _: &T::DataType) {
        self.table.set_selected(index);
    }
}

impl<T> CompilerOutputGui<T::CompilerOutputType> for ListEditorTable<T>
where
    T: TableMapping + TableCompilerOutput + 'static,
{
    fn set_selected_compiler_output(&mut self, _: &Option<T::CompilerOutputType>) {}

    fn set_compiler_output(
        &mut self,
        index: usize,
        compiler_output: &Option<T::CompilerOutputType>,
    ) {
        self.table.edit_row(index, |row| -> bool {
            T::set_row_state(row, compiler_output)
        });
    }
}
