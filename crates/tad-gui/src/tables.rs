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

    fn default_bg_color() -> Color {
        Color::Background2
    }

    // Output: (bg_color, fg_color)
    fn row_colors(&self, selected: bool) -> (Color, Color) {
        if !selected {
            (Color::Background2, Color::Foreground)
        } else {
            (Color::Selection, Color::Background2)
        }
    }

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

    callback: Box<dyn Fn(TableEvent, usize, i32) -> bool>,
    /// The bool argument is:
    ///  * true if the user caused the change in selection
    ///    (ie, by clicking with the cursor or opening the editor)
    ///  * false if the change is selction was caused by `set_selected()` or `clear_selected()`.
    row_selected_callback: Box<dyn Fn(Option<usize>, bool)>,

    edit_widget: Option<fltk::input::Input>,

    // The currently selected cell.
    //
    // Manually tracking the selected cell:
    //
    //  * Fixes a desync between the cursor and the last edited cell.
    //    Opening the editor with the space-bar, pressing tab (to advance to the next cell and open
    //    a new editor), closing the editor and pressing space-bar again would not edit the
    //    expected cell (the cell before the selected cell would be edited instead).
    //
    //  * Fixes an bug where navigating a two-column table with the arrow keys would get stuck on
    //    column 1, never selecting column 0 when left is pressed.
    sel_row: i32,
    sel_col: i32,

    // The position of the currently edited cell (if any)
    // This value will be out-of-bounds if the editor is not active.
    editing_row: i32,
    editing_col: i32,
}

#[derive(Debug)]
pub enum TableEvent {
    EditorRequested,
    CellClicked,
    DoubleClick,
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
            callback: Box::from(blank_callback),
            row_selected_callback: Box::from(blank_sel_changed_callback),
            edit_widget: None,
            sel_row: -1,
            sel_col: 0,
            editing_row: -1,
            editing_col: -1,
        }));

        table.draw_cell({
            let state = state.clone();
            move |_t, ctx, row, col, x, y, w, h| {
                if let Ok(mut s) = state.try_borrow_mut() {
                    s.draw_cell(ctx, row, col, x, y, w, h);
                }
            }
        });

        table.handle({
            let state = state.clone();
            move |table, ev| TableState::handle_events(table, ev, &state)
        });

        // This callback occurs when an FLTK event (click or keybaord navigation) has changed the selection.
        table.set_callback({
            let state = state.clone();
            move |_table| {
                if let Ok(mut s) = state.try_borrow_mut() {
                    s.fltk_changed_selection();
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
                            true
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
        self.state.borrow_mut().callback = Box::from(f);
    }

    /// Callback `f(index, user_selection);`
    pub fn set_selection_changed_callback(&mut self, f: impl Fn(Option<usize>, bool) + 'static) {
        self.state.borrow_mut().row_selected_callback = Box::from(f);
    }

    pub fn clear_selected(&mut self) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            s.unset_selection(false);
        }
    }

    pub fn set_selected(&mut self, row_index: usize) {
        if let Ok(mut s) = self.state.try_borrow_mut() {
            match row_index.try_into() {
                Ok(r) => s.set_sel_row(r, false),
                Err(_) => s.unset_selection(false),
            }
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

    pub fn show(&mut self) {
        self.table.show();
    }

    pub fn hide(&mut self) {
        self.table.hide();
    }
}

fn blank_callback(_: TableEvent, _: usize, _: i32) -> bool {
    false
}

fn blank_sel_changed_callback(_: Option<usize>, _: bool) {}

impl<T> TableState<T>
where
    T: TableRow,
{
    fn handle_events(
        table: &mut fltk::table::Table,
        ev: fltk::enums::Event,
        state: &Rc<RefCell<TableState<T>>>,
    ) -> bool {
        match ev {
            Event::Push => {
                if let Ok(mut s) = state.try_borrow_mut() {
                    if let Some((TableContext::Cell, row, col, _)) = table.cursor2rowcol() {
                        s.set_selection(row, col, true);
                    } else {
                        s.unset_selection(true);
                    }
                }
                let _ = table.take_focus();
                true
            }
            Event::Released => {
                if app::event_is_click() {
                    // Only process events if a cell was clicked
                    if let Some((TableContext::Cell, row, col, _)) = table.cursor2rowcol() {
                        if let Ok(mut s) = state.try_borrow_mut() {
                            s.set_selection(row, col, true);

                            let e = match fltk::app::event_clicks() {
                                false => TableEvent::CellClicked,
                                true => TableEvent::DoubleClick,
                            };
                            s.do_callback(e);
                        }
                    }
                }
                true
            }
            Event::Drag => {
                if let Ok(mut s) = state.try_borrow_mut() {
                    if let Some((TableContext::Cell, row, col, _)) = table.cursor2rowcol() {
                        s.set_selection(row, col, true);
                    }
                }
                true
            }
            Event::KeyDown => {
                if let Ok(mut s) = state.try_borrow_mut() {
                    match app::event_key() {
                        Key::Enter => s.do_callback(TableEvent::Enter),
                        SPACEBAR_KEY => s.do_callback(TableEvent::EditorRequested),

                        // Update the selected column when left or right is pressed.
                        // I tried to use `Table::callback` to update the column, but `col` got
                        // stuck on the second column of a two column table.
                        Key::Left => {
                            let c = s.sel_col;
                            s.set_sel_col(c - 1);
                        }
                        Key::Right => {
                            let c = s.sel_col;
                            s.set_sel_col(c + 1);
                        }
                        _ => (),
                    }
                }
                true
            }
            _ => false,
        }
    }

    fn do_callback(&mut self, e: TableEvent) {
        self.do_callback_at(e, self.sel_row, self.sel_col);
    }

    fn do_callback_at(&mut self, e: TableEvent, row: i32, col: i32) {
        if let Ok(index) = usize::try_from(row) {
            let open_editor = (self.callback)(e, index, col);
            if open_editor {
                self.open_editor(row, col);
            }
        }
    }

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

                let table_row = usize::try_from(row).ok().and_then(|i| self.data.get(i));
                if let Some(table_row) = table_row {
                    let selected = self.table.is_selected(row, col);

                    let (bg_color, fg_color) = table_row.row_colors(selected);
                    draw::set_draw_color(bg_color);
                    draw::draw_rectf(x, y, w, h);

                    if w > 2 * CELL_X_PAD {
                        draw::set_draw_color(fg_color);
                        draw::set_font(self.font, self.font_size);

                        table_row.draw_cell(col, x + CELL_X_PAD, y, w - CELL_X_PAD * 2, h);
                    }
                } else {
                    draw::set_draw_color(T::default_bg_color());
                    draw::draw_rectf(x, y, w, h);
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

    fn fltk_changed_selection(&mut self) {
        let (row_top, _, row_bottom, _) = self.table.get_selection();

        // Have to test top and bottom as the user might have pressed shift-down
        // which selects multiple rows and TrTable only supports single row selection.
        if row_top != self.sel_row {
            self.set_sel_row(row_top, true)
        } else {
            self.set_sel_row(row_bottom, true)
        }
    }

    fn set_sel_row(&mut self, row: i32, user_selection: bool) {
        self.set_selection(row, self.sel_col, user_selection);
    }

    fn set_sel_col(&mut self, col: i32) {
        // No need to modify selection.  Row is unchanged.
        self.sel_col = col.clamp(0, T::N_COLUMNS - 1);
    }

    fn set_selection(&mut self, row: i32, col: i32, user_selection: bool) {
        let col = col.clamp(0, T::N_COLUMNS - 1);

        if let Ok(index) = usize::try_from(row) {
            if index < self.data.len() {
                if self.sel_row != row {
                    (self.row_selected_callback)(Some(index), user_selection);
                }

                self.sel_row = row;
                self.sel_col = col;

                if self.table.get_selection() != (row, 0, row, T::N_COLUMNS) {
                    self.table.set_selection(row, 0, row, T::N_COLUMNS);
                }
            } else {
                self.unset_selection(user_selection);
            }
        } else {
            self.unset_selection(user_selection);
        }
    }

    fn unset_selection(&mut self, user_selection: bool) {
        if self.sel_row != -1 {
            (self.row_selected_callback)(None, user_selection);
        }

        self.sel_row = -1;

        self.table.unset_selection();
    }

    fn open_previous_editor(&mut self) {
        if self.editing_col > 0 {
            // ::TODO how do I skip non-editable columns::
            self.do_callback_at(
                TableEvent::EditorRequested,
                self.editing_row,
                self.editing_col - 1,
            );
        } else if self.editing_row > 0 {
            self.do_callback_at(TableEvent::EditorRequested, self.editing_row - 1, 0);
        }
    }

    fn open_next_editor(&mut self) {
        if self.editing_row >= 0 {
            if self.editing_col == T::N_COLUMNS - 1 {
                self.do_callback_at(TableEvent::EditorRequested, self.editing_row + 1, 0);
            } else {
                // ::TODO how do I skip non-editable columns::
                self.do_callback_at(
                    TableEvent::EditorRequested,
                    self.editing_row,
                    self.editing_col + 1,
                );
            }
        }
    }

    fn open_editor(&mut self, row: i32, col: i32) {
        self.set_selection(row, col, true);

        let widget = match &mut self.edit_widget {
            Some(ew) => ew,
            None => return,
        };

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

/// A simple TableRow containing a const generic number of `String`s.
pub struct SimpleRow<const C: usize> {
    pub columns: [String; C],
}

impl<const C: usize> SimpleRow<C> {
    pub fn new(columns: [String; C]) -> Self {
        Self { columns }
    }

    /// Returns true if the column was changed
    ///
    /// Safety: Panics if index is >= N_COLUMNS
    pub fn edit_column(&mut self, index: usize, s: &str) -> bool {
        let c = &mut self.columns[index];

        if c != s {
            s.clone_into(c);
            true
        } else {
            false
        }
    }

    pub fn edit_column_string(&mut self, index: usize, s: String) -> bool {
        let c = &mut self.columns[index];

        if c != &s {
            *c = s;
            true
        } else {
            false
        }
    }
}

impl<const C: usize> TableRow for SimpleRow<C> {
    const N_COLUMNS: i32 = C as i32;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32) {
        if let Ok(i) = usize::try_from(col) {
            if let Some(c) = &self.columns.get(i) {
                draw::draw_text2(c, x, y, w, h, Align::Left);
            }
        }
    }

    fn value(&self, col: i32) -> Option<&str> {
        match usize::try_from(col) {
            Ok(i) => self.columns.get(i).map(String::as_str),
            Err(_) => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RowStatus {
    Ok,
    Error,
    Unchecked,
}

impl RowStatus {
    fn colors(&self, selected: bool) -> (Color, Color) {
        if !selected {
            match self {
                RowStatus::Ok => (Color::Background2, Color::Foreground),
                RowStatus::Error => (Color::BackGround2, Color::Red),
                RowStatus::Unchecked => (Color::BackGround2, Color::Foreground),
            }
        } else {
            match self {
                RowStatus::Ok => (Color::Selection, Color::Background2),
                RowStatus::Error => (Color::Red, Color::Background2),
                RowStatus::Unchecked => (Color::Selection, Color::Gray0),
            }
        }
    }
}

pub struct RowWithStatus<TR>
where
    TR: TableRow,
{
    pub status: RowStatus,
    pub columns: TR,
}

impl<TR> RowWithStatus<TR>
where
    TR: TableRow,
{
    #[allow(dead_code)]
    pub fn new(status: RowStatus, columns: TR) -> Self {
        Self { status, columns }
    }

    pub fn new_unchecked(columns: TR) -> Self {
        Self {
            status: RowStatus::Unchecked,
            columns,
        }
    }

    /// Returns true if `self.status` changed.
    pub fn set_status(&mut self, status: RowStatus) -> bool {
        if self.status != status {
            self.status = status;
            true
        } else {
            false
        }
    }

    /// Returns true if `self.status` changed.
    pub fn set_status_optional_result<O, E>(&mut self, co: &Option<Result<O, E>>) -> bool {
        self.set_status(match co {
            Some(Ok(_)) => RowStatus::Ok,
            Some(Err(_)) => RowStatus::Error,
            None => RowStatus::Unchecked,
        })
    }
}

impl<TR> TableRow for RowWithStatus<TR>
where
    TR: TableRow,
{
    const N_COLUMNS: i32 = TR::N_COLUMNS;

    #[inline]
    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32) {
        self.columns.draw_cell(col, x, y, w, h)
    }

    #[inline]
    fn value(&self, col: i32) -> Option<&str> {
        self.columns.value(col)
    }

    fn row_colors(&self, selected: bool) -> (Color, Color) {
        self.status.colors(selected)
    }
}
