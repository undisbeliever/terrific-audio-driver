//! Drag and drop state management and handlers

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::GuiMessage;

use std::path::PathBuf;

extern crate fltk;
use fltk::app;
use fltk::app::Sender;
use fltk::enums::Event;
use fltk::prelude::WidgetBase;

#[derive(Debug)]
pub struct DroppedFilePath(PathBuf);

impl DroppedFilePath {
    /// CAUTION: path might not be valid
    pub fn take_path(self) -> PathBuf {
        self.0
    }

    fn try_from(event_text: String) -> Option<Self> {
        let path = event_text.trim();

        if !path.contains('\n') && path.len() < 2048 {
            // On X11: path might start with "file://" and contain "%XY" escape sequences
            //
            // https://www.fltk.org/doc-1.4/events.html#events_fl_dnd_files
            let path = match path.strip_prefix("file://") {
                Some(path) => PathBuf::from(fltk::utils::decode_uri(path)),
                None => PathBuf::from(path),
            };
            Some(Self(path))
        } else {
            None
        }
    }
}

pub struct DragDropFileHandler {
    sender: Sender<GuiMessage>,
    message_fn: fn(DroppedFilePath) -> GuiMessage,

    entered: bool,
    released: bool,
}

impl DragDropFileHandler {
    pub fn add_to_widget(
        w: &mut impl WidgetBase,
        sender: Sender<GuiMessage>,
        message_fn: fn(DroppedFilePath) -> GuiMessage,
    ) {
        w.handle({
            let mut cb = Self {
                sender,
                message_fn,
                entered: false,
                released: false,
            };
            move |_, ev| cb.handle(ev)
        })
    }

    fn handle(&mut self, ev: Event) -> bool {
        match ev {
            Event::DndEnter => {
                self.entered = true;
                true
            }
            Event::DndDrag => true,
            Event::DndRelease => {
                self.released = true;
                true
            }
            Event::Paste => {
                let is_dnd = self.entered && self.released;
                self.entered = false;
                self.released = false;

                if is_dnd {
                    if let Some(d) = DroppedFilePath::try_from(app::event_text()) {
                        self.sender.send((self.message_fn)(d))
                    }
                    true
                } else {
                    false
                }
            }
            Event::DndLeave => {
                self.entered = false;
                self.released = false;
                true
            }
            _ => false,
        }
    }
}
