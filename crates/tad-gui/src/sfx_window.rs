//! Play Sound Effects child window

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::Pan;
use crate::compiler_thread::CadOutput;
use crate::helpers::{ch_units_to_width, label_packed};
use crate::sfx_export_order::GuiSfxExportOrder;
use crate::tabs::FileType;
use crate::GuiMessage;

use compiler::driver_constants::{CENTER_PAN, MAX_PAN};
use compiler::sound_effects::SfxExportOrder;
use fltk::table::TableContext;

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

extern crate fltk;
use fltk::app;
use fltk::button::Button;
use fltk::draw;
use fltk::enums::{Align, Color, Event, Font, FrameType, Key};
use fltk::frame::Frame;
use fltk::group::Flex;
use fltk::prelude::*;
use fltk::valuator::HorNiceSlider;
use fltk::window::Window;

pub struct SfxWindow {
    window: Window,
    group: Flex,

    table: Rc<RefCell<SfxTable>>,
    no_sfx_reason: Frame,

    editing_sfx: bool,
    can_play_sfx: bool,
}

impl SfxWindow {
    pub fn new(sender: app::Sender<GuiMessage>) -> SfxWindow {
        let mut window = fltk::window::Window::default().with_label("Sound Effects");
        window.make_modal(false);
        window.make_resizable(true);

        window.set_size(
            ch_units_to_width(&window, 30),
            ch_units_to_width(&window, 35),
        );
        window.size_range(
            ch_units_to_width(&window, 20),
            ch_units_to_width(&window, 20),
            0,
            0,
        );

        let mut group = Flex::default_fill().column();
        group.set_margin(4);
        group.set_pad(2);
        group.deactivate();

        let pan_slider = {
            let pan_height = ch_units_to_width(&group, 4);

            let mut flex = Flex::default().row();
            group.fixed(&flex, pan_height);

            let label = label_packed("Pan:  ");
            flex.fixed(&label, label.width());

            let mut slider = HorNiceSlider::default();
            slider.set_range(0.0, MAX_PAN as f64);
            slider.set_value(CENTER_PAN as f64);
            slider.set_slider_size(1.0 / MAX_PAN as f32);
            slider.set_tooltip("Pan");

            let mut reset_pan = Button::default().with_label("@center_pan");
            flex.fixed(&reset_pan, pan_height);
            reset_pan.set_label_size(reset_pan.label_size() * 8 / 10);
            reset_pan.set_tooltip("Center pan");
            reset_pan.set_callback({
                let mut p = slider.clone();
                move |_| {
                    p.set_value(CENTER_PAN as f64);
                }
            });

            flex.end();

            slider
        };

        let table = SfxTable::new(pan_slider, sender);

        let mut no_sfx_reason = Frame::default();
        no_sfx_reason.set_align(Align::Inside | Align::Top | Align::Center);
        no_sfx_reason.hide();

        group.end();

        let mut out = Self {
            window,
            group,
            table,
            no_sfx_reason,
            editing_sfx: false,
            can_play_sfx: true,
        };
        out.update_widget_state();

        out
    }

    pub fn show_or_hide(&mut self) {
        if self.window.shown() {
            self.window.hide();
        } else {
            self.window.show();
        }
    }

    pub fn tab_changed(&mut self, tab: Option<FileType>) {
        let editing_sfx = matches!(tab, Some(FileType::SoundEffects));
        if self.editing_sfx != editing_sfx {
            self.editing_sfx = editing_sfx;
            self.update_widget_state();
        }
    }

    pub fn cad_output_changed(&mut self, cad_output: &CadOutput) {
        self.table.borrow_mut().cad_output_changed(cad_output);

        let can_play_sfx = matches!(cad_output, CadOutput::WithSfx(..));
        if self.can_play_sfx != can_play_sfx {
            self.can_play_sfx = can_play_sfx;
            self.update_widget_state();
        }
    }

    fn update_widget_state(&mut self) {
        if self.can_play_sfx && !self.editing_sfx {
            self.group.activate();
            self.table.borrow_mut().table.show();
            self.no_sfx_reason.hide();
        } else {
            let error_message = if self.editing_sfx {
                "Cannot play sound effects\nwhile editing sound effects"
            } else {
                "Sound effect error"
            };

            self.group.deactivate();
            self.table.borrow_mut().table.hide();
            self.no_sfx_reason.set_label(error_message);
            self.no_sfx_reason.show();
        }

        self.group.layout();
    }
}

pub struct SfxTable {
    table: fltk::table::Table,
    export_order: Option<Arc<GuiSfxExportOrder>>,

    pan_slider: HorNiceSlider,
    sender: app::Sender<GuiMessage>,

    font: Font,
    font_size: i32,
}

impl SfxTable {
    const N_COLUMNS: i32 = 1;

    fn new(pan: HorNiceSlider, sender: app::Sender<GuiMessage>) -> Rc<RefCell<SfxTable>> {
        let mut table = fltk::table::Table::default();

        table.set_tab_cell_nav(true);

        table.set_color(Color::Background2);
        table.set_cols(SfxTable::N_COLUMNS);

        table.end();

        table.resize_callback({
            move |table, _x, _y, w, _h| {
                let w = w - table.scrollbar().width() - 3;
                if w != table.col_width(0) {
                    table.set_col_width_all(w);
                }
            }
        });

        let out = Rc::new(RefCell::new(SfxTable {
            font: table.label_font(),
            font_size: table.label_size(),

            pan_slider: pan,
            sender,

            table,
            export_order: None,
        }));

        {
            let mut o = out.borrow_mut();

            o.table.draw_cell({
                let table = out.clone();
                move |_t, ctx, row, col, x, y, w, h| {
                    if let Ok(t) = table.try_borrow() {
                        t.draw_table_cell(ctx, row, col, x, y, w, h);
                    }
                }
            });

            o.table.handle({
                let state = out.clone();
                move |_, ev| Self::handle_events(ev, &state)
            });
        }

        out
    }

    fn handle_events(ev: Event, state: &Rc<RefCell<Self>>) -> bool {
        const SPACEBAR_KEY: Key = Key::from_char(' ');

        match ev {
            Event::Released => {
                if app::event_is_click() && fltk::app::event_clicks() {
                    if let Ok(s) = state.try_borrow() {
                        s.play_selected_sfx();
                    }
                }
                false
            }
            Event::KeyDown => match app::event_key() {
                Key::Enter | SPACEBAR_KEY => {
                    if let Ok(s) = state.try_borrow() {
                        s.play_selected_sfx();
                    }
                    true
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn play_selected_sfx(&self) {
        if let Some(eo) = &self.export_order {
            if let Some((TableContext::Cell, row, _, _)) = self.table.cursor2rowcol() {
                if let Ok(row) = row.try_into() {
                    if let Some(sfx_id) = eo.sfx_id(row) {
                        let pan = Pan::checked_new(self.pan_slider.value() as u8);
                        self.sender
                            .send(GuiMessage::PlaySoundEffectCommand(sfx_id, pan));
                    }
                }
            }
        }
    }

    fn cad_output_changed(&mut self, cad_output: &CadOutput) {
        match cad_output {
            CadOutput::WithSfx(c, _) => {
                let changed = match &self.export_order {
                    Some(e) => !Arc::ptr_eq(e, &c.sfx_export_order),
                    None => true,
                };

                if changed {
                    self.export_order = Some(c.sfx_export_order.clone());

                    self.table
                        .set_rows(c.sfx_export_order.n_sound_effects().try_into().unwrap_or(0));
                    self.table.redraw();
                }
            }
            CadOutput::Err(..) | CadOutput::NoSfx(..) | CadOutput::None => {
                let changed = self.export_order.is_some();

                self.export_order = None;

                if changed {
                    self.table.set_rows(0);
                    self.table.redraw();
                }
            }
        }
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
        // X-axis margin
        const MX: i32 = 3;
        const LOW_PRIORITY_LINE: Color = Color::DarkRed;

        match ctx {
            TableContext::StartPage => draw::set_font(self.font, self.font_size),
            TableContext::RowHeader => {
                draw::push_clip(x, y, w, h);
                draw::draw_box(FrameType::ThinUpBox, x, y, w, h, Color::FrameDefault);
                draw::pop_clip();
            }
            TableContext::Cell => {
                if let Some(eo) = &self.export_order {
                    if let Ok(i) = usize::try_from(row) {
                        if let Some(name) = eo.export_order().get(i) {
                            let (bg_color, fg_color) = match self.table.is_selected(row, col) {
                                false => (Color::Background2, Color::Foreground),
                                true => (Color::Selection, Color::Background2),
                            };
                            draw::set_draw_color(bg_color);
                            draw::draw_rectf(x, y, w, h);

                            if i == eo.low_priority_index() {
                                draw::set_draw_color(LOW_PRIORITY_LINE);
                                draw::draw_xyline(x, y, x + w);
                            } else if i + 1 == eo.low_priority_index() {
                                draw::set_draw_color(LOW_PRIORITY_LINE);
                                draw::draw_xyline(x, y + h, x + w);
                            }

                            draw::set_draw_color(fg_color);
                            draw::set_font(self.font, self.font_size);
                            draw::draw_text2(name.as_str(), x + MX, y, w - 2 * MX, h, Align::Left);
                        }
                    }
                }
            }
            _ => (),
        }
    }
}
