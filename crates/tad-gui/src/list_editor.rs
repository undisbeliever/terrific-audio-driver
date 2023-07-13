//! List Editors

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use fltk::button::Button;
use fltk::group::{Pack, PackType};
use fltk::prelude::{GroupExt, WidgetExt};

#[derive(Debug)]
pub enum ListMessage<T> {
    ClearSelection,
    ItemSelected(usize),
    SelectedItemEdited(T),

    Add(T),
    CloneSelected,
    RemoveSelected,
    MoveSelectedToTop,
    MoveSelectedUp,
    MoveSelectedDown,
    MoveSelectedToBottom,
}

pub trait ListEditor<T> {
    fn list_buttons(&mut self) -> &mut ListButtons;

    fn list_changed(&mut self, list: &[T]);
    fn clear_selected(&mut self);
    fn set_selected(&mut self, index: usize, value: &T);
    fn item_changed(&mut self, index: usize, value: &T);
}

#[derive(Default)]
pub struct ListState {
    selected: Option<usize>,
    // ::TODO add max list size::
}

impl<T> ListMessage<T>
where
    T: Clone,
{
    #[allow(clippy::ptr_arg)]
    fn set_selected(
        index: usize,
        state: &mut ListState,
        list: &Vec<T>,
        editor: &mut impl ListEditor<T>,
    ) {
        match list.get(index) {
            Some(item) => {
                state.selected = Some(index);
                editor.set_selected(index, item);
                editor.list_buttons().selected_changed(index, list.len());
            }
            None => {
                Self::clear_selection(state, editor);
            }
        };
    }

    pub fn clear_selection(state: &mut ListState, editor: &mut impl ListEditor<T>) {
        state.selected = None;
        editor.clear_selected();
        editor.list_buttons().selected_clear();
    }

    pub fn process(
        self,
        state: &mut ListState,
        list: &mut Vec<T>,
        editor: &mut impl ListEditor<T>,
    ) {
        match self {
            ListMessage::ClearSelection => {
                Self::clear_selection(state, editor);
            }
            ListMessage::ItemSelected(index) => {
                Self::set_selected(index, state, list, editor);
            }

            ListMessage::SelectedItemEdited(new_value) => {
                if let Some(i) = state.selected {
                    if let Some(item) = list.get_mut(i) {
                        *item = new_value;
                        editor.item_changed(i, item);
                    }
                }
            }
            ListMessage::Add(item) => {
                let i = list.len();
                list.push(item);
                editor.list_changed(list);

                Self::set_selected(i, state, list, editor);
            }
            ListMessage::CloneSelected => {
                if let Some(i) = state.selected {
                    if let Some(item) = list.get(i) {
                        list.insert(i, item.clone());
                        editor.list_changed(list);

                        Self::set_selected(i + 1, state, list, editor);
                    }
                }
            }
            ListMessage::RemoveSelected => {
                if let Some(i) = state.selected {
                    if i < list.len() {
                        list.remove(i);
                        editor.list_changed(list);
                    }
                }
                Self::clear_selection(state, editor);
            }
            ListMessage::MoveSelectedToTop => {
                if let Some(i) = state.selected {
                    if i != 0 {
                        for j in (1..=i).rev() {
                            list.swap(j, j - 1);
                        }

                        editor.list_changed(list);
                        Self::set_selected(0, state, list, editor);
                    }
                }
            }
            ListMessage::MoveSelectedUp => {
                if let Some(i) = state.selected {
                    if i > 0 && i < list.len() {
                        list.swap(i, i - 1);
                        editor.list_changed(list);
                        Self::set_selected(i - 1, state, list, editor);
                    }
                }
            }
            ListMessage::MoveSelectedDown => {
                if let Some(i) = state.selected {
                    if i + 1 < list.len() {
                        list.swap(i, i + 1);
                        editor.list_changed(list);
                        Self::set_selected(i + 1, state, list, editor);
                    }
                }
            }
            ListMessage::MoveSelectedToBottom => {
                if let Some(i) = state.selected {
                    if i + 1 < list.len() {
                        let last_index = list.len() - 1;
                        for j in i..last_index {
                            list.swap(j, j + 1);
                        }

                        editor.list_changed(list);
                        Self::set_selected(last_index, state, list, editor);
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
        out.selected_clear();
        out
    }

    // ::TODO add max list size::

    fn selected_changed(&mut self, index: usize, list_len: usize) {
        if let Some(c) = &mut self.clone {
            c.activate();
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

    fn selected_clear(&mut self) {
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
pub(crate) use create_list_button_callbacks;

macro_rules! create_list_item_edited_input_handler {
    ($widget:ident, $struct_name:path, $field:ident, $sender:ident, $msg:ident, $data:ident) => {
        $widget.handle({
            let s: fltk::app::Sender<Message> = $sender.clone();
            let data_rc: std::rc::Rc<std::cell::RefCell<$struct_name>> = $data.clone();
            move |widget, ev| {
                if is_input_done_event(ev) {
                    let mut data = data_rc.borrow_mut();

                    if let Some(v) = InputHelper::parse(widget.value()) {
                        if v != data.$field {
                            data.$field = v;
                            s.send(Message::$msg(ListMessage::SelectedItemEdited(data.clone())));
                        }
                    }
                    InputHelper::set_widget_value(widget, &data.$field);
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
            let data_rc: std::rc::Rc<std::cell::RefCell<$struct_name>> = $data.clone();
            move |widget: &mut fltk::button::CheckButton| {
                let mut data = data_rc.borrow_mut();
                let b: bool = widget.is_checked();
                if b != data.$field {
                    data.$field = b;
                    s.send(Message::$msg(ListMessage::SelectedItemEdited(data.clone())));
                }
            }
        });
    };
}
pub(crate) use create_list_item_edited_checkbox_handler;
