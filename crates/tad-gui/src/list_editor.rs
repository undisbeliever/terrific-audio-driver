//! List Editors

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::ItemId;
use crate::helpers::SetActive;
use crate::tables;
use crate::Message;

use fltk::button::Button;
use fltk::group::{Pack, PackType};
use fltk::prelude::{GroupExt, WidgetExt};

use std::ops::Deref;

#[derive(Debug)]
pub enum ListMessage<T> {
    ClearSelection,
    ItemSelected(usize),

    // The `SelectedItemEdited` message has been removed because clicking on a TrTable list item
    // will cause the `ItemSelected` message to be sent before the `SelectedItemEdited` event.
    ItemEdited(usize, T),

    Add(T),
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
    fn set_selected(&mut self, index: usize, value: &T);
}

#[derive(Debug)]
pub enum ListAction<T> {
    None,
    Add(usize, T),
    Remove(usize),
    Edit(usize, T),
    Move(usize, usize),
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
    add: impl FnOnce(&T) -> U,
    edit: impl FnOnce(&mut U, &T),
) {
    match action {
        ListAction::None => (),
        ListAction::Add(i, item) => list.insert(*i, add(item)),
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

#[allow(dead_code)]
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

    pub fn process(&mut self, action: &ListAction<T>)
    where
        T: Clone,
    {
        process_list_action(&mut self.0, action);
    }

    pub fn process_map<U>(
        &mut self,
        action: &ListAction<U>,
        add: impl FnOnce(&U) -> T,
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

pub struct ListState<T>
where
    T: Clone + PartialEq<T>,
{
    max_size: usize,
    list: LaVec<(ItemId, T)>,
    selected: Option<usize>,
}

impl<T> ListState<T>
where
    T: Clone + std::cmp::PartialEq<T>,
{
    pub fn new(list: Vec<T>, max_size: usize) -> Self {
        Self {
            max_size,
            list: LaVec::from_vec(list.into_iter().map(|i| (ItemId::new(), i)).collect()),
            selected: None,
        }
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

    pub fn selected(&self) -> Option<usize> {
        self.selected
    }

    pub fn can_add(&self) -> bool {
        self.list.len() < self.max_size
    }

    fn set_selected(&mut self, index: usize, editor: &mut impl ListEditor<T>) {
        match self.list.get(index) {
            Some((_id, item)) => {
                self.selected = Some(index);
                editor.set_selected(index, item);
                editor
                    .list_buttons()
                    .selected_changed(index, self.list.len(), self.can_add());
            }
            None => {
                self.clear_selection(editor);
            }
        };
    }

    fn clear_selection(&mut self, editor: &mut impl ListEditor<T>) {
        self.selected = None;
        editor.clear_selected();
        editor.list_buttons().selected_clear(self.can_add());
    }

    pub fn process(&mut self, m: ListMessage<T>, editor: &mut impl ListEditor<T>) -> ListAction<T> {
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
            ListMessage::ClearSelection => {
                self.clear_selection(editor);
                ListAction::None
            }
            ListMessage::ItemSelected(index) => {
                self.set_selected(index, editor);
                ListAction::None
            }

            ListMessage::ItemEdited(index, new_value) => {
                if let Some((_id, item)) = self.list.get(index) {
                    if *item != new_value {
                        let action = ListAction::Edit(index, new_value);
                        update_list(&mut self.list, &action);
                        editor.list_edited(&action);

                        action
                    } else {
                        ListAction::None
                    }
                } else {
                    ListAction::None
                }
            }

            ListMessage::Add(item) => {
                if self.can_add() {
                    let i = self.list.len();
                    let action = ListAction::Add(i, item);
                    update_list(&mut self.list, &action);
                    editor.list_edited(&action);

                    self.set_selected(i, editor);

                    action
                } else {
                    ListAction::None
                }
            }

            lm => {
                // These list actions use the currently selected index
                let sel_index = match self.selected {
                    Some(i) if i < self.list.len() => i,
                    _ => return ListAction::None,
                };

                match lm {
                    ListMessage::ClearSelection
                    | ListMessage::ItemSelected(_)
                    | ListMessage::ItemEdited(_, _)
                    | ListMessage::Add(_) => ListAction::None,

                    ListMessage::CloneSelected => {
                        if let (Some(item), true) = (self.get(sel_index), self.can_add()) {
                            let i = sel_index + 1;
                            let action = ListAction::Add(i, item.clone());
                            update_list(&mut self.list, &action);
                            editor.list_edited(&action);

                            self.set_selected(i, editor);

                            action
                        } else {
                            ListAction::None
                        }
                    }
                    ListMessage::RemoveSelected => {
                        let action = ListAction::Remove(sel_index);
                        update_list(&mut self.list, &action);
                        editor.list_edited(&action);

                        self.clear_selection(editor);

                        action
                    }
                    ListMessage::MoveSelectedToTop => {
                        if sel_index > 0 {
                            let action = ListAction::Move(sel_index, 0);
                            update_list(&mut self.list, &action);
                            editor.list_edited(&action);

                            self.set_selected(0, editor);

                            action
                        } else {
                            ListAction::None
                        }
                    }
                    ListMessage::MoveSelectedUp => {
                        if sel_index > 0 && sel_index < self.list.len() {
                            let action = ListAction::Move(sel_index, sel_index - 1);
                            update_list(&mut self.list, &action);
                            editor.list_edited(&action);

                            self.set_selected(sel_index - 1, editor);

                            action
                        } else {
                            ListAction::None
                        }
                    }
                    ListMessage::MoveSelectedDown => {
                        if sel_index + 1 < self.list.len() {
                            let action = ListAction::Move(sel_index, sel_index + 1);
                            update_list(&mut self.list, &action);
                            editor.list_edited(&action);

                            self.set_selected(sel_index + 1, editor);

                            action
                        } else {
                            ListAction::None
                        }
                    }
                    ListMessage::MoveSelectedToBottom => {
                        if sel_index + 1 < self.list.len() {
                            let action = ListAction::Move(sel_index, self.list.len() - 1);
                            update_list(&mut self.list, &action);
                            editor.list_edited(&action);

                            self.set_selected(self.list.len() - 1, editor);

                            action
                        } else {
                            ListAction::None
                        }
                    }
                }
            }
        }
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

    pub fn update<T>(&mut self, state: &ListState<T>)
    where
        T: Clone + PartialEq<T>,
    {
        match state.selected() {
            Some(i) => self.selected_changed(i, state.list.len(), state.can_add()),
            None => self.selected_clear(state.can_add()),
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

// ::TODO is this macro required?::
#[allow(unused_macros)]
macro_rules! create_list_button_callbacks {
    ($lb:expr, $msg:ident, $sender:expr, $new_fn:path) => {
        $lb.add.set_callback({
            let s: fltk::app::Sender<Message> = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::Add($new_fn())))
        });

        create_list_button_callbacks!($lb, $msg, $sender);
    };
    ($lb:expr, $msg:ident, $sender:expr) => {
        if let Some(b) = &mut $lb.clone {
            b.set_callback({
                let s: fltk::app::Sender<Message> = $sender.clone();
                move |_| s.send(Message::$msg(ListMessage::CloneSelected))
            });
        }
        $lb.remove.set_callback({
            let s = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::RemoveSelected))
        });
        $lb.move_top.set_callback({
            let s = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::MoveSelectedToTop))
        });
        $lb.move_up.set_callback({
            let s = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::MoveSelectedUp))
        });
        $lb.move_down.set_callback({
            let s = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::MoveSelectedDown))
        });
        $lb.move_bottom.set_callback({
            let s = $sender.clone();
            move |_| s.send(Message::$msg(ListMessage::MoveSelectedToBottom))
        });
    };
}
// ::TODO is this macro required?::
#[allow(unused_imports)]
pub(crate) use create_list_button_callbacks;

pub struct IndexAndData<T> {
    pub index: Option<usize>,
    pub data: T,
}

impl<T> IndexAndData<T> {
    pub fn new(index: Option<usize>, data: T) -> Self {
        Self { index, data }
    }
}

macro_rules! create_list_item_edited_input_handler {
    ($widget:ident, $struct_name:path, $field:ident, $sender:ident, $msg:ident, $data:ident) => {
        $widget.handle({
            let s: fltk::app::Sender<Message> = $sender.clone();
            let data_rc: std::rc::Rc<std::cell::RefCell<IndexAndData<$struct_name>>> =
                $data.clone();
            move |widget, ev| {
                if is_input_done_event(ev) {
                    let id = data_rc.borrow();

                    if let Some(index) = id.index {
                        if let Some(v) = InputHelper::parse(widget.value()) {
                            if v != id.data.$field {
                                let mut d = id.data.clone();
                                d.$field = v;
                                s.send(Message::$msg(ListMessage::ItemEdited(index, d)));
                            }
                        } else {
                            InputHelper::set_widget_value(widget, &id.data.$field);
                        }
                    }
                }

                // Always propagate focus/enter events
                false
            }
        });
    };
}
pub(crate) use create_list_item_edited_input_handler;

macro_rules! create_list_item_edited_checkbox_handler {
    ($widget:ident, $struct_name:path, $field:ident, $sender:ident, $msg:ident, $data:ident) => {
        $widget.set_callback({
            let s: fltk::app::Sender<Message> = $sender.clone();
            let data_rc: std::rc::Rc<std::cell::RefCell<IndexAndData<$struct_name>>> =
                $data.clone();
            move |widget: &mut fltk::button::CheckButton| {
                let id = data_rc.borrow();
                if let Some(index) = id.index {
                    let b: bool = widget.is_checked();
                    if b != id.data.$field {
                        let mut d = id.data.clone();
                        d.$field = b;
                        s.send(Message::$msg(ListMessage::ItemEdited(index, d)));
                    }
                }
            }
        });
    };
}
pub(crate) use create_list_item_edited_checkbox_handler;

#[allow(dead_code)]
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
        state: &ListState<T::DataType>,
        sender: fltk::app::Sender<Message>,
    ) -> Self {
        let mut out = Self::new(sender);
        out.replace(state);
        out
    }

    pub fn replace(&mut self, state: &ListState<T::DataType>) {
        self.table.edit_table(|v| {
            *v = state.item_iter().map(T::new_row).collect();
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

    fn set_selected(&mut self, index: usize, _: &T::DataType) {
        self.table.set_selected(index);
    }
}
