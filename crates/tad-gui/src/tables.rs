//! custom fltk table

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::cell::RefCell;
use std::cmp::max;
use std::rc::Rc;

extern crate fltk;
use fltk::app::{self, event_key};
use fltk::draw;
use fltk::enums::{Align, Color, Cursor, Event, Font, FrameType, Key};
use fltk::prelude::{GroupExt, InputExt, TableExt, WidgetBase, WidgetExt};
use fltk::table::TableContext;

const SPACEBAR_KEY: Key = Key::from_char(' ');

/// Padding to the left/right of each cell.
const CELL_X_PAD: i32 = 3;

pub trait TableRow {
    const N_COLUMNS: i32;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32);

    fn value(&self, col: i32) -> Option<&str>;
}

pub struct TableState<T>
where
    T: TableRow,
{
    table: fltk::table::Table,

    headers: Vec<String>,

    font: Font,
    font_size: i32,

    data: Vec<T>,

    edit_widget: Option<fltk::input::Input>,

    // The position of the currently edited cell (if any)
    // This value will be out-of-bounds if the editor is not active.
    editing_col: i32,
    editing_row: i32,
}

#[derive(Debug)]
pub enum TableEvent {
    CellClicked,
    Spacebar,
    Enter,
}

// TableRow Table
pub struct TrTable<T>
where
    T: TableRow,
{
    table: fltk::table::Table,

    state: Rc<RefCell<TableState<T>>>,
}

impl<T> TrTable<T>
where
    T: TableRow + 'static,
{
    pub fn new(headers: Vec<String>) -> Self {
        let mut table = fltk::table::Table::default();

        table.set_tab_cell_nav(true);

        table.set_color(Color::Background2);
        table.set_cols(T::N_COLUMNS);

        if !headers.is_empty() {
            table.set_col_header(true);
        }

        let state = Rc::from(RefCell::from(TableState {
            table: table.clone(),
            headers,
            font: table.label_font(),
            font_size: table.label_size(),
            data: Vec::new(),
            edit_widget: None,
            editing_col: -1,
            editing_row: -1,
        }));

        table.draw_cell({
            let state = state.clone();
            move |_t, ctx, row, col, x, y, w, h| {
                if let Ok(mut s) = state.try_borrow_mut() {
                    s.draw_cell(ctx, row, col, x, y, w, h);
                }
            }
        });

        table.resize_callback({
            move |table, _x, _y, w, _h| {
                // ::TODO adjustable table columns and user adjustable columns::
                if T::N_COLUMNS > 0 {
                    let w = (w - table.scrollbar().width() - 3) / T::N_COLUMNS;
                    let w = max(30, w);
                    if w != table.col_width(0) {
                        table.set_col_width_all(w);
                    }
                }
            }
        });

        table.end();

        Self { table, state }
    }

    /// Create a cell editing widget and enable cell editing.
    ///
    /// # Callbacks
    /// `commit_value`: Called when the editing is completed with the value of the input widget.
    ///
    /// # Panics
    /// This function will panic if `enable_cell_editing()` is called twice.
    pub fn enable_cell_editing(&mut self, commit_value: impl Fn(usize, i32, String) + 'static) {
        let mut state = self.state.borrow_mut();

        if state.edit_widget.is_some() {
            panic!("enable_cell_editing() can only be called once");
        }

        state.editing_col = -1;
        state.editing_row = -1;

        let mut widget = fltk::input::Input::default();
        widget.hide();

        self.table.add(&widget);

        widget.handle({
            let state = self.state.clone();
            move |widget, ev| {
                let commit = |and_then: fn(&mut TableState<T>)| {
                    if let Ok(mut s) = state.try_borrow_mut() {
                        if widget.changed() {
                            if let Ok(index) = usize::try_from(s.editing_row) {
                                commit_value(index, s.editing_col, widget.value());
                            }
                        }
                        and_then(&mut s);
                    }
                };

                match ev {
                    Event::Unfocus => {
                        commit(TableState::hide_table_editor);
                        // Continue propagating the unfocus event
                        false
                    }
                    Event::KeyDown => match event_key() {
                        Key::Escape => {
                            if let Ok(mut s) = state.try_borrow_mut() {
                                s.hide_table_editor();
                            }
                            true
                        }
                        Key::Enter => {
                            commit(TableState::hide_table_editor);
                            true
                        }
                        Key::Tab => {
                            commit(|s| {
                                if app::is_event_shift() {
                                    s.open_previous_editor();
                                } else {
                                    s.open_next_editor();
                                }
                            });
                            // Propagate tab navigation to the table widget
                            false
                        }
                        _ => false,
                    },
                    _ => false,
                }
            }
        });

        state.edit_widget = Some(widget)
    }

    /// Callback: `f(event, index, col) -> bool`.
    /// If `f` returns true, the cell editor will be opened.
    pub fn set_callback(&mut self, f: impl Fn(TableEvent, usize, i32) -> bool + 'static) {
        self.table.handle({
            let state = self.state.clone();
            move |table, ev| {
                let f = |te| -> bool {
                    match usize::try_from(table.callback_row()) {
                        Ok(index) => f(te, index, table.callback_col()),
                        Err(_) => false,
                    }
                };

                let open_editor = match ev {
                    Event::Released => {
                        // Only process events if a cell was clicked
                        if let Some((TableContext::Cell, _, _, _)) = table.cursor2rowcol() {
                            f(TableEvent::CellClicked)
                        } else {
                            let _ = table.take_focus();
                            return false;
                        }
                    }
                    Event::KeyDown => match app::event_key() {
                        Key::Enter => f(TableEvent::Enter),
                        SPACEBAR_KEY => f(TableEvent::Spacebar),
                        _ => return false,
                    },
                    _ => return false,
                };

                if open_editor {
                    if let Ok(mut s) = state.try_borrow_mut() {
                        s.open_editor(table.callback_row(), table.callback_col());
                    }
                }
                true
            }
        });
    }

    pub fn set_selection_changed_callback(&mut self, f: impl Fn(Option<usize>) + 'static) {
        // ::TODO confirm this works correctly::
        self.table.set_callback(move |table| {
            let (row_top, _, row_bottom, _) = table.get_selection();

            f(usize::try_from(row_top).ok());

            // ::TODO handle multiple selections::
            if row_bottom != row_top {
                table.set_selection(row_top, 0, row_top, T::N_COLUMNS);
            }
        });
    }

    pub fn clear_selected(&mut self) {
        self.table.unset_selection();
    }

    pub fn set_selected(&mut self, row_index: usize) {
        match row_index.try_into() {
            Ok(r) => {
                self.table.set_selection(r, 0, r, T::N_COLUMNS);
            }
            Err(_) => self.table.unset_selection(),
        }
    }

    pub fn edit_row(&mut self, index: usize, f: impl Fn(&mut T) -> bool) {
        let mut state = self.state.borrow_mut();
        if let Some(d) = state.data.get_mut(index) {
            let edited = f(d);

            if edited {
                self.table.redraw();
            }
        }
    }

    pub fn edit_table(&mut self, f: impl Fn(&mut Vec<T>)) {
        let mut state = self.state.borrow_mut();
        f(&mut state.data);

        let n_rows = state.data.len().try_into().unwrap_or(0);
        self.table.set_rows(n_rows);
    }
}

impl<T> TableState<T>
where
    T: TableRow,
{
    #[allow(clippy::too_many_arguments)]
    fn draw_cell(&mut self, ctx: TableContext, row: i32, col: i32, x: i32, y: i32, w: i32, h: i32) {
        match ctx {
            TableContext::StartPage => draw::set_font(self.font, self.font_size),
            TableContext::ColHeader => {
                draw::push_clip(x, y, w, h);
                draw::draw_box(FrameType::ThinUpBox, x, y, w, h, Color::FrameDefault);

                if let Ok(col_index) = usize::try_from(col) {
                    if let Some(c) = self.headers.get(col_index) {
                        draw::set_draw_color(Color::Foreground);
                        draw::set_font(self.font, self.font_size);
                        draw::draw_text2(c, x, y, w, h, Align::Center);
                    }
                }
                draw::pop_clip();
            }
            TableContext::RowHeader => {
                draw::push_clip(x, y, w, h);
                draw::draw_box(FrameType::ThinUpBox, x, y, w, h, Color::FrameDefault);
                draw::pop_clip();
            }
            TableContext::Cell => {
                // Do not draw a cell that is being edited
                if row == self.editing_row && col == self.editing_col {
                    return;
                }

                draw::push_clip(x, y, w, h);

                let selected = self.table.is_selected(row, col);

                let bg_color = if selected {
                    Color::Selection
                } else {
                    Color::Background2
                };
                let fg_color = if selected {
                    Color::Background2
                } else {
                    Color::Foreground
                };

                draw::set_draw_color(bg_color);
                draw::draw_rectf(x, y, w, h);

                if let Ok(row_index) = usize::try_from(row) {
                    if let Some(table_row) = self.data.get(row_index) {
                        if w > 2 * CELL_X_PAD {
                            draw::set_draw_color(fg_color);
                            draw::set_font(self.font, self.font_size);

                            table_row.draw_cell(col, x + CELL_X_PAD, y, w - CELL_X_PAD * 2, h);
                        }
                    }
                }

                draw::pop_clip();
            }

            TableContext::RcResize => {
                // Table column/row resized
                if self.editing_row >= 0 && self.editing_col >= 0 {
                    if let Some((x, y, w, h)) =
                        self.table
                            .find_cell(TableContext::Cell, self.editing_row, self.editing_col)
                    {
                        if let Some(widget) = &mut self.edit_widget {
                            widget.resize(x, y, w, h);
                            widget.redraw();
                        }
                    }
                }
            }

            _ => (),
        }
    }

    fn open_previous_editor(&mut self) {
        if self.editing_col > 0 {
            // ::TODO how do I skip non-editable columns::
            self.open_editor(self.editing_row, self.editing_col - 1);
        } else if self.editing_row > 0 {
            self.open_editor(self.editing_row - 1, 0);
        }
    }

    fn open_next_editor(&mut self) {
        if self.editing_row >= 0 {
            if self.editing_col == T::N_COLUMNS - 1 {
                // ::TODO how do I skip non-editable columns::
                self.open_editor(self.editing_row + 1, 0);
            } else {
                self.open_editor(self.editing_row, self.editing_col + 1);
            }
        }
    }

    fn open_editor(&mut self, row: i32, col: i32) {
        let widget = match &mut self.edit_widget {
            Some(ew) => ew,
            None => return,
        };

        self.table.set_selection(row, 0, row, T::N_COLUMNS);

        if let Ok(index) = usize::try_from(row) {
            if let Some(row_data) = self.data.get(index) {
                if let Some(v) = row_data.value(col) {
                    widget.set_value(v);
                    widget.clear_changed();
                    if let Some((x, y, w, h)) = self.table.find_cell(TableContext::Cell, row, col) {
                        self.editing_row = row;
                        self.editing_col = col;

                        // Fixes a bug where quickly clicking on cells will not redraw the
                        // previous edited cell (leaving the old editor on screen).
                        self.table.redraw();

                        // Select all
                        let _ = widget.set_position(0);
                        let _ = widget.set_mark(i32::MAX);

                        widget.resize(x, y, w, h);
                        widget.show();
                        let _ = widget.take_focus();
                        widget.redraw();
                    }
                }
            }
        }
    }

    fn hide_table_editor(&mut self) {
        if let Some(widget) = &mut self.edit_widget {
            widget.set_value("");
            widget.hide();

            // Fixes a bug where the mouse cursor disappears
            if let Some(mut w) = widget.window() {
                w.set_cursor(Cursor::Default);
            }
        }

        // Reenable draw_cell on this cell (edited cells are not drawn).
        self.editing_col = -1;
        self.editing_row = -1;

        let _ = self.table.take_focus();
    }
}

pub struct SingleColumnRow(pub String);

impl TableRow for SingleColumnRow {
    const N_COLUMNS: i32 = 1;

    fn draw_cell(&self, _col: i32, x: i32, y: i32, w: i32, h: i32) {
        draw::draw_text2(&self.0, x, y, w, h, Align::Left);
    }

    fn value(&self, col: i32) -> Option<&str> {
        match col {
            0 => Some(&self.0),
            _ => None,
        }
    }
}

pub struct TwoColumnsRow(pub String, pub String);

impl TableRow for TwoColumnsRow {
    const N_COLUMNS: i32 = 2;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32) {
        match col {
            0 => draw::draw_text2(&self.0, x, y, w, h, Align::Left),
            1 => draw::draw_text2(&self.1, x, y, w, h, Align::Left),
            _ => (),
        }
    }

    fn value(&self, col: i32) -> Option<&str> {
        match col {
            0 => Some(&self.0),
            1 => Some(&self.1),
            _ => None,
        }
    }
}
