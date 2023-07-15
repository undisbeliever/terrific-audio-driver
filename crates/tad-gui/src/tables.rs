//! custom fltk table

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::cell::RefCell;
use std::cmp::max;
use std::rc::Rc;

extern crate fltk;
use fltk::draw;
use fltk::enums::{Align, Color, Font, FrameType};
use fltk::prelude::{GroupExt, TableExt, WidgetBase, WidgetExt};
use fltk::table::TableContext;

/// Padding to the left/right of each cell.
const CELL_X_PAD: i32 = 3;

pub trait TableRow {
    const N_COLUMNS: i32;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32);
}

pub struct TableState<T>
where
    T: TableRow,
{
    headers: Vec<String>,

    font: Font,
    font_size: i32,

    data: Vec<T>,
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

        table.set_color(Color::Background2);
        table.set_cols(T::N_COLUMNS);

        if !headers.is_empty() {
            table.set_col_header(true);
        }

        let state = Rc::from(RefCell::from(TableState {
            headers,
            font: table.label_font(),
            font_size: table.label_size(),
            data: Vec::new(),
        }));

        table.draw_cell({
            let state = state.clone();
            move |t, ctx, row, col, x, y, w, h| {
                if let Ok(s) = state.try_borrow() {
                    s.draw_cell(t, ctx, row, col, x, y, w, h);
                }
            }
        });

        table.resize_callback({
            |table, _x, _y, w, _h| {
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
    fn draw_cell(
        &self,
        t: &fltk::table::Table,
        ctx: TableContext,
        row: i32,
        col: i32,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
    ) {
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
                draw::push_clip(x, y, w, h);

                let selected = t.is_selected(row, col);

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

            _ => (),
        }
    }
}

pub struct SingleColumnRow(pub String);

impl TableRow for SingleColumnRow {
    const N_COLUMNS: i32 = 1;

    fn draw_cell(&self, _col: i32, x: i32, y: i32, w: i32, h: i32) {
        draw::draw_text2(&self.0, x, y, w, h, Align::Left);
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
}
