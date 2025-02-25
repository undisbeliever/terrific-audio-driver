//! Sample sizes widget

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{CadOutput, InstrumentAndSampleNames, InstrumentOutput, SampleOutput};
use crate::helpers::ch_units_to_width;
use crate::list_editor::{LaVec, ListAction};
use crate::InstrumentsAndSamplesData;

use compiler::common_audio_data::CommonAudioData;
use compiler::data::{Instrument, Name, Sample};
use compiler::driver_constants::{addresses, COMMON_DATA_HEADER_SIZE};
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
    brr_sample_names: Arc<InstrumentAndSampleNames>,

    // Used when the Common Audio Data cannot be compiled
    no_cad_instruments: LaVec<(Name, String)>,
    no_cad_samples: LaVec<(Name, String)>,
}

const AUDIO_DRIVER_COLOR: Color = Color::Magenta;
const CAD_HEADER_COLOR: Color = Color::Green;
const SFX_COLOR: Color = Color::Blue;
const PITCH_TABLE_COLOR: Color = Color::DarkGreen;
const DIR_TABLE_COLOR: Color = Color::Yellow;
const BRR_SAMPLES_COLOR: Color = Color::Yellow;
const BRR_SAMPLES_LINE_COLOR: Color = Color::DarkYellow;
const LARGEST_SONG_COLOR: Color = Color::Red;
const LARGEST_SONG_ECHO_COLOR: Color = Color::Red;

const AUDIO_DRIVER_SIZE: u16 = addresses::COMMON_DATA;
const CAD_HEADER_END: u16 = addresses::COMMON_DATA + COMMON_DATA_HEADER_SIZE as u16;

const N_COLUMNS: i32 = 2;

const STAT_NAMES: [(&str, Color); 6] = [
    ("Audio Driver", AUDIO_DRIVER_COLOR),
    ("Largest Song", LARGEST_SONG_COLOR),
    ("Common Audio Data Header", CAD_HEADER_COLOR),
    ("Sound Effects", SFX_COLOR),
    ("Pitch Table", PITCH_TABLE_COLOR),
    ("Instruments and Samples", BRR_SAMPLES_COLOR),
];
const N_STAT_ROWS: usize = STAT_NAMES.len();

const DRIVER_SIZE_IDX: usize = 0;
const LARGEST_SONG_IDX: usize = 1;
const CAD_HEADER_IDX: usize = 2;
const SFX_IDX: usize = 3;
const PITCH_TABLE_IDX: usize = 4;
const INST_SAMPLES_IDX: usize = 5;

struct GraphData {
    dir_table_range: Range<u16>,
    sfx_range: Range<u16>,
    pitch_table: Range<u16>,
    instruments_samples_range: Range<u16>,
    brr_start_addrs: Vec<u16>,
    song_start: u16,
}

fn size_string(s: u16) -> String {
    format!("{s} bytes")
}

fn usize_string(s: usize) -> String {
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
    pub fn new(
        parent: &mut Flex,
        instruments_and_samples: &InstrumentsAndSamplesData,
    ) -> Rc<RefCell<SampleSizesWidget>> {
        let graph_height = ch_units_to_width(parent, 10);

        let graph_widget = Widget::default();
        parent.fixed(&graph_widget, graph_height);

        let mut table = fltk::table::Table::default();
        table.set_tab_cell_nav(true);

        table.set_color(Color::Background2);
        table.set_col_header(false);
        table.set_cols(N_COLUMNS);

        table.resize_callback({
            move |table, _x, _y, w, _h| {
                let w = (w - table.scrollbar().width() - 3) / N_COLUMNS;
                if w != table.col_width(0) {
                    table.set_col_width_all(w);
                }
            }
        });

        let instruments = instruments_and_samples.list1();
        let samples = instruments_and_samples.list2();

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

            no_cad_instruments: LaVec::from_vec(
                instruments
                    .item_iter()
                    .map(|i| (i.name.clone(), String::new()))
                    .collect(),
            ),
            no_cad_samples: LaVec::from_vec(
                samples
                    .item_iter()
                    .map(|s| (s.name.clone(), String::new()))
                    .collect(),
            ),
        }));

        {
            let mut s = state.borrow_mut();

            s.update_table_size();

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
            CadOutput::None | CadOutput::Err(_) => {
                if self.graph_data.is_some() {
                    self.clear_table();
                }
            }
            CadOutput::NoSfx(cad) => {
                self.update_table(&cad.0, &cad.1, false);
            }
            CadOutput::SfxBuffer(cad) => {
                self.update_table(cad.0.common_data(), &cad.1, false);
            }
            CadOutput::WithSfx(cad) => {
                self.update_table(
                    &cad.common_audio_data,
                    &cad.instrument_and_sample_names,
                    true,
                );
            }
        }
    }

    fn clear_table(&mut self) {
        self.graph_data = None;
        self.brr_sizes.clear();

        self.stat_sizes[CAD_HEADER_IDX].clear();
        self.stat_sizes[SFX_IDX].clear();
        self.stat_sizes[PITCH_TABLE_IDX].clear();
        self.stat_sizes[INST_SAMPLES_IDX].clear();

        self.graph_widget.redraw();
        self.update_table_size();
    }

    fn update_table_size(&mut self) {
        let n_sample_rows = if self.graph_data.is_some() {
            self.brr_sizes.len()
        } else {
            self.no_cad_instruments.len() + self.no_cad_samples.len()
        };

        let n_rows = n_sample_rows + N_STAT_ROWS;
        self.table.set_rows(n_rows.try_into().unwrap_or(0));
    }

    fn update_table(
        &mut self,
        cad: &CommonAudioData,
        names: &Arc<InstrumentAndSampleNames>,
        sfx_valid: bool,
    ) {
        self.graph_data = Some(GraphData {
            dir_table_range: cad.dir_addr_range(),
            sfx_range: cad.sfx_data_and_tables_range(),
            pitch_table: cad.pitch_table_addr_range(),
            instruments_samples_range: Range {
                start: cad.instruments_soa_addr_range().start,
                end: cad.brr_addr_range().end,
            },
            brr_start_addrs: cad.dir_table_start_iter().collect(),
            song_start: cad.min_song_data_addr(),
        });
        let d = self.graph_data.as_ref().unwrap();

        self.brr_sample_names = names.clone();

        self.stat_sizes[CAD_HEADER_IDX] = usize_string(COMMON_DATA_HEADER_SIZE);
        if sfx_valid {
            self.stat_sizes[SFX_IDX] = range_size_string(&d.sfx_range);
        } else {
            "ERROR".clone_into(&mut self.stat_sizes[SFX_IDX]);
        }
        self.stat_sizes[PITCH_TABLE_IDX] = range_size_string(&d.pitch_table);
        self.stat_sizes[INST_SAMPLES_IDX] = size_string(cad.instruments_and_samples_size());

        {
            self.brr_sizes.clear();

            let mut prev_brr_start = 0;
            for scrn in cad.instruments_soa_scrn() {
                let scrn = usize::from(*scrn);

                if let Some(&brr_start) = d.brr_start_addrs.get(scrn) {
                    let s = if brr_start > prev_brr_start {
                        let brr_end = match d.brr_start_addrs.get(scrn + 1) {
                            Some(&v) => v,
                            None => d.instruments_samples_range.end,
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

        self.update_table_size();
        self.table.redraw();

        self.graph_widget.redraw();
    }

    pub fn instrument_edited(&mut self, action: &ListAction<Instrument>) {
        self.no_cad_instruments.process_map(
            action,
            |inst| (inst.name.clone(), String::new()),
            |d, inst| *d = (inst.name.clone(), String::new()),
        );

        self.update_table_size();
    }

    pub fn sample_edited(&mut self, action: &ListAction<Sample>) {
        self.no_cad_samples.process_map(
            action,
            |sample| (sample.name.clone(), String::new()),
            |d, sample| *d = (sample.name.clone(), String::new()),
        );

        self.update_table_size();
    }

    pub fn instrument_compiled(
        &mut self,
        index: usize,
        compiler_output: &Option<InstrumentOutput>,
    ) {
        if let Some(d) = self.no_cad_instruments.get_mut(index) {
            match compiler_output {
                Some(Ok(i)) => d.1 = usize_string(i.0),
                Some(Err(_)) => "ERROR".clone_into(&mut d.1),
                None => d.1.clear(),
            }
            self.table.redraw()
        }
    }

    pub fn sample_compiled(&mut self, index: usize, compiler_output: &Option<SampleOutput>) {
        if let Some(d) = self.no_cad_samples.get_mut(index) {
            match compiler_output {
                Some(Ok(i)) => d.1 = usize_string(i.0),
                Some(Err(_)) => "ERROR".clone_into(&mut d.1),
                None => d.1.clear(),
            }
            self.table.redraw()
        }
    }

    fn draw_graph(&self) {
        const MAX_W: i32 = (i32::MAX / 2) / 0x10000;
        const _: () = assert!(0xffff * MAX_W / 0x10000 < i32::MAX);

        draw::set_line_style(LineStyle::Solid, 1);

        let x = self.graph_widget.x();
        let y = self.graph_widget.y();
        let w = self.graph_widget.w().clamp(100, MAX_W);
        let h = self.graph_widget.h();

        draw::draw_rect_fill(x, y, w, h, Color::Background);

        if let Some(d) = &self.graph_data {
            let addr_x = |addr: u16| x + (i32::from(addr) * w) / 0x10000;
            let y1 = y + 1;
            let y2 = y + h - 1;

            let addr_rect = |addr1: u16, addr2: u16, c: Color| {
                let x1 = addr_x(addr1);
                let x2 = addr_x(addr2);
                draw::draw_rect_fill(x1, y1, x2 - x1, h - 2, c);
            };
            let addr_rect_range = |r: &Range<u16>, c: Color| {
                addr_rect(r.start, r.end, c);
            };
            let addr_line = |addr: u16| {
                draw::draw_yxline(addr_x(addr), y1, y2);
            };

            // Largest song
            let song_start = d.song_start;
            let song_end = song_start.checked_add(self.largest_song.data_size);
            let echo_start = 0xffff - self.largest_song.echo_buffer_size + 1;

            if let Some(song_end) = song_end {
                addr_rect(song_start, song_end, LARGEST_SONG_COLOR);
                addr_rect(echo_start, u16::MAX, LARGEST_SONG_ECHO_COLOR);
            }
            addr_rect(0, AUDIO_DRIVER_SIZE, AUDIO_DRIVER_COLOR);
            addr_rect(AUDIO_DRIVER_SIZE, CAD_HEADER_END, CAD_HEADER_COLOR);
            addr_rect_range(&d.dir_table_range, DIR_TABLE_COLOR);
            addr_rect_range(&d.sfx_range, SFX_COLOR);
            addr_rect_range(&d.pitch_table, PITCH_TABLE_COLOR);
            addr_rect_range(&d.instruments_samples_range, DIR_TABLE_COLOR);

            draw::set_draw_color(BRR_SAMPLES_LINE_COLOR);
            for &addr in d.brr_start_addrs.iter().skip(1) {
                addr_line(addr);
            }

            draw::set_draw_color(Color::Foreground);

            addr_line(AUDIO_DRIVER_SIZE);
            addr_line(CAD_HEADER_END);
            addr_line(d.dir_table_range.end);
            addr_line(d.sfx_range.end);
            addr_line(d.pitch_table.end);
            addr_line(d.instruments_samples_range.end);

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
                    if let Some(n) = self.get_sample_name(row) {
                        col0_text(n.as_str());
                    }
                }
                1 => {
                    if let Some(s) = self.get_sample_size(row) {
                        col1_text(s);
                    }
                }
                _ => {}
            }
        }

        draw::pop_clip();
    }

    fn get_sample_name(&self, i: usize) -> Option<&Name> {
        if self.graph_data.is_some() {
            self.brr_sample_names.get(i)
        } else if i < self.no_cad_instruments.len() {
            self.no_cad_instruments.get(i).map(|d| &d.0)
        } else {
            self.no_cad_samples
                .get(i - self.no_cad_instruments.len())
                .map(|d| &d.0)
        }
    }

    fn get_sample_size(&self, i: usize) -> Option<&String> {
        if self.graph_data.is_some() {
            self.brr_sizes.get(i)
        } else if i < self.no_cad_instruments.len() {
            self.no_cad_instruments.get(i).map(|d| &d.1)
        } else {
            self.no_cad_samples
                .get(i - self.no_cad_instruments.len())
                .map(|d| &d.1)
        }
    }
}
