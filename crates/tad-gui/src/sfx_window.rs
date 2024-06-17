//! Play Sound Effects child window

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_thread::Pan;
use crate::helpers::{ch_units_to_width, label_packed};
use crate::list_editor::{process_list_action_map, ListAction, ListData};
use crate::tables::{TableEvent, TableRow, TrTable};
use crate::tabs::FileType;
use crate::GuiMessage;

use compiler::data::Name;
use compiler::driver_constants::{CENTER_PAN, MAX_PAN};

extern crate fltk;
use fltk::app;
use fltk::button::Button;
use fltk::enums::Align;
use fltk::frame::Frame;
use fltk::group::Flex;
use fltk::prelude::*;
use fltk::valuator::HorNiceSlider;
use fltk::window::Window;

pub struct SfxWindow {
    window: Window,
    group: Flex,

    _pan: HorNiceSlider,
    table: TrTable<PlaySfxTableRow>,
    no_sfx_reason: Frame,

    editing_sfx: bool,
    can_play_sfx: bool,
}

struct PlaySfxTableRow {
    name: Name,
}

impl PlaySfxTableRow {
    const HEADER: &'static str = "Play Sound Effect";
}

impl PlaySfxTableRow {
    fn new_row(name: &Name) -> Self {
        Self { name: name.clone() }
    }

    fn edit_row(&mut self, name: &Name) -> bool {
        if &self.name != name {
            self.name = name.clone();
            true
        } else {
            false
        }
    }
}

impl TableRow for PlaySfxTableRow {
    const N_COLUMNS: i32 = 1;

    fn draw_cell(&self, col: i32, x: i32, y: i32, w: i32, h: i32) {
        if col == 0 {
            fltk::draw::draw_text2(self.name.as_str(), x, y, w, h, Align::Left);
        }
    }

    // Return None disables editing
    fn value(&self, _col: i32) -> Option<&str> {
        None
    }
}

impl SfxWindow {
    pub fn new(sfx_export_order: &ListData<Name>, sender: app::Sender<GuiMessage>) -> SfxWindow {
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

        let mut table = TrTable::new(vec![PlaySfxTableRow::HEADER.to_string()]);

        let mut no_sfx_reason = Frame::default();
        no_sfx_reason.set_align(Align::Inside | Align::Top | Align::Center);
        no_sfx_reason.hide();

        group.end();

        table.edit_table(|v| {
            *v = sfx_export_order
                .item_iter()
                .map(PlaySfxTableRow::new_row)
                .collect();
        });

        table.set_callback({
            let pan_slider = pan_slider.clone();
            move |event, row, _col| {
                match event {
                    TableEvent::EditorRequested | TableEvent::DoubleClick => {
                        let pan = Pan::checked_new(pan_slider.value() as u8);
                        sender.send(GuiMessage::PlaySoundEffectCommand(row, pan));
                    }
                    _ => (),
                }
                false
            }
        });

        let mut out = Self {
            window,
            group,
            _pan: pan_slider,
            table,
            no_sfx_reason,
            editing_sfx: false,
            can_play_sfx: false,
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

    pub fn sfx_export_order_edited(&mut self, action: &ListAction<Name>) {
        match action {
            ListAction::None => (),
            a => self.table.edit_table(|table_vec| {
                process_list_action_map(table_vec, a, PlaySfxTableRow::new_row, |row, new_value| {
                    PlaySfxTableRow::edit_row(row, new_value);
                })
            }),
        }
    }

    pub fn tab_changed(&mut self, tab: Option<FileType>) {
        let editng_sfx = matches!(tab, Some(FileType::SoundEffects));
        if self.editing_sfx != editng_sfx {
            self.editing_sfx = editng_sfx;
            self.update_widget_state();
        }
    }

    pub fn set_can_play_sfx(&mut self, can_play: bool) {
        if self.can_play_sfx != can_play {
            self.can_play_sfx = can_play;
            self.update_widget_state();
        }
    }

    fn update_widget_state(&mut self) {
        if self.can_play_sfx && !self.editing_sfx {
            self.group.activate();
            self.table.show();
            self.no_sfx_reason.hide();
        } else {
            let error_message = if self.editing_sfx {
                "Cannot play sound effects\nwhile editing sound effects"
            } else {
                "Sound effect error"
            };

            self.group.deactivate();
            self.table.hide();
            self.no_sfx_reason.set_label(error_message);
            self.no_sfx_reason.show();
        }

        self.group.layout();
    }
}
