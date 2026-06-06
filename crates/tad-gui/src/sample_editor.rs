//! Sample editor (within the Samples tab)

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::compiler_thread::{ItemId, PlaySampleArgs};
use crate::envelope_widget::EnvelopeWidget;
use crate::helpers::*;
use crate::GuiMessage;

use compiler::notes::Note;
use compiler::project::Sample;
use fltk::group::Group;
use fltk::misc::Spinner;

use std::cell::RefCell;
use std::rc::Rc;

use fltk::app;
use fltk::button::Button;
use fltk::prelude::*;

pub struct TestSampleWidget {
    selected_id: Option<ItemId>,

    sender: app::Sender<GuiMessage>,

    group: Group,

    buttons: [Button; 12],

    note_length: Spinner,
    envelope: EnvelopeWidget,
}

#[expect(dead_code)]
impl TestSampleWidget {
    pub fn new(sender: app::Sender<GuiMessage>) -> Rc<RefCell<Self>> {
        let mut group = Group::default();
        group.make_resizable(false);

        let button_size = ch_units_to_width(&group, 5);

        let options_x = 6 * button_size;
        let options_y = button_size;
        let options_width = ch_units_to_width(&group, 30);
        let line_height = ch_units_to_width(&group, 3);

        group.set_size(options_x + options_width, button_size * 4);

        let buttons = std::array::from_fn(|i| {
            let i = i32::try_from(i).unwrap();
            let x = (i % 4 + 1) * button_size;
            let y = (i / 4 + 1) * button_size;

            let mut b = Button::new(x, y, button_size, button_size, None);
            b.set_label(&format!("{}", i));

            b
        });

        let mut note_length = Spinner::new(
            options_x + options_width / 2,
            options_y,
            options_width / 2,
            line_height,
            "Note Length",
        );
        note_length.set_range(2.0, 1000.0);
        note_length.set_value(125.0); // 1 second
        note_length.set_step(1.0);

        let envelope = EnvelopeWidget::new(options_x, options_y + line_height * 2, options_width);

        let out = Rc::from(RefCell::new(Self {
            selected_id: None,
            sender,
            group,
            note_length,
            buttons,
            envelope,
        }));

        for (i, b) in out.borrow_mut().buttons.iter_mut().enumerate() {
            b.set_callback({
                let widget = out.clone();
                let note = Note::from_note_id_usize(i).unwrap();
                move |_| {
                    if let Ok(w) = widget.try_borrow() {
                        w.send_play_sample_message(note);
                    }
                }
            })
        }

        out
    }

    pub fn clear_selected(&mut self) {
        self.selected_id = None;
        self.group.deactivate();
    }

    pub fn set_selected(&mut self, id: ItemId, data: &Sample) {
        self.selected_id = Some(id);
        self.group.activate();
        self.update_buttons(data);
    }

    pub fn set_active(&mut self, active: bool) {
        self.group.set_active(active && self.selected_id.is_some());
    }

    pub fn item_edited(&mut self, id: ItemId, data: &Sample) {
        if self.selected_id == Some(id) {
            self.update_buttons(data);
        }
    }

    fn update_buttons(&mut self, data: &Sample) {
        let n_sample_rates = data.sample_rates.len();
        for (i, b) in self.buttons.iter_mut().enumerate() {
            b.set_active(i < n_sample_rates);
        }
    }

    fn send_play_sample_message(&self, note: Note) {
        if let Some(id) = self.selected_id {
            if let Ok(envelope) = self.envelope.get_envelope() {
                self.sender.send(GuiMessage::PlaySample(
                    id,
                    PlaySampleArgs {
                        note,
                        note_length: self.note_length.value() as u16,
                        envelope,
                    },
                ))
            }
        }
    }
}
