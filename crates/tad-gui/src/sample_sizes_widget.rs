//! Sample sizes widget

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::CadOutput;
use crate::helpers::ch_units_to_width;

use compiler::common_audio_data::CommonAudioData;
use compiler::data::Name;
use compiler::driver_constants::addresses;
use compiler::songs::{SongAramSize, BLANK_SONG_ARAM_SIZE};
use fltk::table::TableContext;

use std::cell::RefCell;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

use fltk::draw;
use fltk::draw::LineStyle;
use fltk::enums::{Align, Color, Font, FrameType};
use fltk::group::Flex;
use fltk::prelude::*;
use fltk::widget::Widget;

pub struct SampleSizesWidget {
    graph_widget: Widget,
    table: fltk::table::Table,

    font: Font,
    font_size: i32,

    graph_data: Option<GraphData>,
    largest_song: SongAramSize,

    stat_sizes: [String; N_STAT_ROWS],
    brr_sizes: Vec<String>,
    brr_sample_names: Arc<Vec<Name>>,
}

const AUDIO_DRIVER_COLOR: Color = Color::Magenta;
const CAD_HEADER_COLOR: Color = Color::Green;
const SFX_BYTECODE_COLOR: Color = Color::Blue;
const BRR_SAMPLES_COLOR: Color = Color::Yellow;
const BRR_SAMPLES_LINE_COLOR: Color = Color::DarkYellow;
const LARGEST_SONG_COLOR: Color = Color::Red;
const LARGEST_SONG_ECHO_COLOR: Color = Color::Red;

const AUDIO_DRIVER_SIZE: u16 = addresses::COMMON_DATA;

const N_COLUMNS: i32 = 2;

const STAT_NAMES: [(&str, Color); 5] = [
    ("Audio Driver", AUDIO_DRIVER_COLOR),
    ("Largest Song", LARGEST_SONG_COLOR),
    ("Common Audio Data Header", CAD_HEADER_COLOR),
    ("Sound Effect Bytecode", SFX_BYTECODE_COLOR),
    ("BRR Samples", BRR_SAMPLES_COLOR),
];
const N_STAT_ROWS: usize = STAT_NAMES.len();

const DRIVER_SIZE_IDX: usize = 0;
const LARGEST_SONG_IDX: usize = 1;
const CAD_HEADER_IDX: usize = 2;
const BC_SIZE_IDX: usize = 3;
const BRR_SAMPLES_IDX: usize = 4;

struct GraphData {
    cad_size: u16,
    sfx_bytecode: Range<u16>,
    brr_samples_range: Range<u16>,
    brr_start_addrs: Vec<u16>,
}

fn size_string(s: u16) -> String {
    format!("{s} bytes")
}

fn range_size_string(r: &Range<u16>) -> String {
    let s = r.end - r.start;
    format!("{s} bytes")
}

fn largest_song_string(largest_song: &SongAramSize) -> String {
    let d = largest_song.data_size;
    let e = largest_song.echo_buffer_size;

    format!("{} + {} = {} bytes", d, e, d + e)
}

impl SampleSizesWidget {
    pub fn new(parent: &mut Flex) -> Rc<RefCell<SampleSizesWidget>> {
        let graph_height = ch_units_to_width(parent, 10);

        let graph_widget = Widget::default();
        parent.fixed(&graph_widget, graph_height);

        let mut table = fltk::table::Table::default();
        table.set_tab_cell_nav(true);

        table.set_color(Color::Background2);
        table.set_col_header(false);
        table.set_cols(N_COLUMNS);
        table.set_rows(STAT_NAMES.len() as i32);

        table.resize_callback({
            move |table, _x, _y, w, _h| {
                let w = (w - table.scrollbar().width() - 3) / N_COLUMNS;
                if w != table.col_width(0) {
                    table.set_col_width_all(w);
                }
            }
        });

        let state = Rc::new(RefCell::new(SampleSizesWidget {
            font: table.label_font(),
            font_size: table.label_size(),

            graph_widget,
            table,
            graph_data: None,
            largest_song: BLANK_SONG_ARAM_SIZE,

            stat_sizes: Default::default(),
            brr_sample_names: Arc::default(),
            brr_sizes: Vec::new(),
        }));

        {
            let mut s = state.borrow_mut();

            s.stat_sizes[DRIVER_SIZE_IDX] = size_string(AUDIO_DRIVER_SIZE);

            s.graph_widget.draw({
                let state = state.clone();
                move |_| {
                    state.borrow().draw_graph();
                }
            });

            s.table.draw_cell({
                let state = state.clone();
                move |_t, ctx, row, col, x, y, w, h| {
                    if let Ok(s) = state.try_borrow() {
                        s.draw_table_cell(ctx, row, col, x, y, w, h);
                    }
                }
            })
        }

        state
    }

    pub fn set_largest_song(&mut self, s: SongAramSize) {
        if self.largest_song != s {
            self.stat_sizes[LARGEST_SONG_IDX] = largest_song_string(&s);

            self.largest_song = s;
            self.graph_widget.redraw();
        }
    }

    pub fn cad_changed(&mut self, cad: &CadOutput) {
        match cad {
            CadOutput::None => {
                if self.graph_data.is_some() {
                    self.clear_table();
                }
            }
            CadOutput::NoSfx(cad, names) => {
                self.update_table(&cad.0, names, false);
            }
            CadOutput::WithSfx(cad, names) => {
                self.update_table(&cad.common_audio_data, names, true);
            }
        }
    }

    fn clear_table(&mut self) {
        self.graph_data = None;
        self.brr_sizes.clear();

        self.stat_sizes[CAD_HEADER_IDX].clear();
        self.stat_sizes[BC_SIZE_IDX].clear();
        self.stat_sizes[BRR_SAMPLES_IDX].clear();

        self.graph_widget.redraw();

        self.table.set_rows(N_STAT_ROWS as i32);
    }

    fn update_table(&mut self, cad: &CommonAudioData, names: &Arc<Vec<Name>>, sfx_valid: bool) {
        self.graph_data = Some(GraphData {
            cad_size: cad.aram_size(),
            brr_samples_range: cad.brr_addr_range().clone(),
            sfx_bytecode: cad.sfx_bytecode_addr_range().clone(),
            brr_start_addrs: cad.dir_table_start_iter().collect(),
        });
        let d = self.graph_data.as_ref().unwrap();

        self.brr_sample_names = names.clone();

        self.stat_sizes[CAD_HEADER_IDX] = size_string(cad.header_size());
        if sfx_valid {
            self.stat_sizes[BC_SIZE_IDX] = range_size_string(cad.sfx_bytecode_addr_range());
        } else {
            "ERROR".clone_into(&mut self.stat_sizes[BC_SIZE_IDX]);
        }
        self.stat_sizes[BRR_SAMPLES_IDX] = range_size_string(cad.brr_addr_range());

        {
            self.brr_sizes.clear();

            let mut prev_brr_start = 0;
            for scrn in cad.instruments_scrn() {
                let scrn = usize::from(*scrn);

                if let Some(&brr_start) = d.brr_start_addrs.get(scrn) {
                    let s = if brr_start > prev_brr_start {
                        let brr_end = match d.brr_start_addrs.get(scrn + 1) {
                            Some(&v) => v,
                            None => d.brr_samples_range.end,
                        };
                        size_string(brr_end - brr_start)
                    } else {
                        String::new()
                    };

                    self.brr_sizes.push(s);

                    prev_brr_start = brr_start;
                }
            }
        }

        let n_rows = self.brr_sizes.len() + N_STAT_ROWS;

        self.table.set_rows(n_rows.try_into().unwrap_or(0));
        self.table.redraw();

        self.graph_widget.redraw();
    }

    fn draw_graph(&self) {
        const MAX_W: i32 = (i32::MAX / 2) / 0x10000;
        const _: () = assert!(0xffff * MAX_W / 0x10000 < i32::MAX);

        draw::set_line_style(LineStyle::Solid, 1);

        let x = self.graph_widget.x();
        let y = self.graph_widget.y();
        let w = self.graph_widget.w().clamp(100, MAX_W);
        let h = self.graph_widget.h();

        if let Some(d) = &self.graph_data {
            let addr_x = |addr: u16| x + (i32::from(addr) * w) / 0x10000;
            let y1 = y + 1;
            let y2 = y + h - 1;

            let addr_rect = |addr1: u16, addr2: u16, c: Color| {
                let x1 = addr_x(addr1);
                let x2 = addr_x(addr2);
                draw::draw_rect_fill(x1, y1, x2 - x1, h - 2, c);
            };
            let addr_line = |addr: u16| {
                draw::draw_yxline(addr_x(addr), y1, y2);
            };

            // Largest song
            let song_start = AUDIO_DRIVER_SIZE + d.cad_size;
            let song_end = song_start.checked_add(self.largest_song.data_size);
            let echo_start = 0xffff - self.largest_song.echo_buffer_size + 1;

            if let Some(song_end) = song_end {
                addr_rect(song_start, song_end, LARGEST_SONG_COLOR);
                addr_rect(echo_start, u16::MAX, LARGEST_SONG_ECHO_COLOR);
            }
            addr_rect(0, AUDIO_DRIVER_SIZE, AUDIO_DRIVER_COLOR);
            addr_rect(
                AUDIO_DRIVER_SIZE,
                d.brr_samples_range.start,
                CAD_HEADER_COLOR,
            );
            addr_rect(d.sfx_bytecode.start, d.sfx_bytecode.end, SFX_BYTECODE_COLOR);
            addr_rect(
                d.brr_samples_range.start,
                d.brr_samples_range.end,
                BRR_SAMPLES_COLOR,
            );

            draw::set_draw_color(BRR_SAMPLES_LINE_COLOR);
            for &addr in d.brr_start_addrs.iter().skip(1) {
                addr_line(addr);
            }

            draw::set_draw_color(Color::Foreground);

            addr_line(AUDIO_DRIVER_SIZE);
            addr_line(d.brr_samples_range.start);
            addr_line(d.brr_samples_range.end);
            addr_line(d.sfx_bytecode.start);
            addr_line(d.sfx_bytecode.end);

            if let Some(song_end) = song_end {
                addr_line(song_end);
                addr_line(echo_start);
            }
        }

        draw::draw_box(FrameType::ThinDownFrame, x, y, w, h, Color::FrameDefault);
    }

    #[allow(clippy::too_many_arguments)]
    fn draw_table_cell(
        &self,
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
            TableContext::RowHeader => {
                draw::push_clip(x, y, w, h);
                draw::draw_box(FrameType::ThinUpBox, x, y, w, h, Color::FrameDefault);
                draw::pop_clip();
            }
            TableContext::Cell => self.draw_table_cell_cell(row, col, x, y, w, h),
            _ => (),
        }
    }

    fn draw_table_cell_cell(&self, row: i32, col: i32, x: i32, y: i32, w: i32, h: i32) {
        const BG_COLOR: Color = Color::Background2;
        const FG_COLOR: Color = Color::Foreground;

        const M: i32 = 2;

        draw::push_clip(x, y, w, h);

        draw::set_draw_color(BG_COLOR);
        draw::draw_rectf(x, y, w, h);

        let row = match usize::try_from(row) {
            Ok(r) => r,
            Err(_) => return,
        };

        let x = x + M;
        let w = w - 2 * M;

        let col0_text = |s: &str| {
            draw::set_draw_color(FG_COLOR);
            draw::draw_text2(s, x + h + M, y, w - h - M, h, Align::Left);
        };

        let col1_text = |s: &str| {
            draw::set_draw_color(FG_COLOR);
            draw::draw_text2(s, x, y, w, h, Align::Right);
        };

        if row < N_STAT_ROWS {
            match col {
                0 => {
                    let (label, color) = &STAT_NAMES[row];
                    let x = x + M;
                    let y = y + M;
                    let s = h - M * 2;

                    draw::draw_rect_fill(x, y, s, s, *color);
                    draw::draw_rect_with_color(x, y, s, s, FG_COLOR);

                    col0_text(label);
                }
                1 => {
                    col1_text(&self.stat_sizes[row]);
                }
                _ => {}
            }
        } else {
            let row = row - N_STAT_ROWS;
            match col {
                0 => {
                    if let Some(name) = self.brr_sample_names.get(row) {
                        col0_text(name.as_str());
                    }
                }
                1 => {
                    if let Some(s) = self.brr_sizes.get(row) {
                        col1_text(s);
                    }
                }
                _ => {}
            }
        }

        draw::pop_clip();
    }
}
