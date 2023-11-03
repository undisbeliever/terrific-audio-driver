//! Audio Driver Timer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::GuiMessage;

use std::cell::RefCell;
use std::rc::Rc;

extern crate fltk;
use fltk::app;

const DEFAULT_TIMEOUT: f64 = 1.0 / 20.0;

pub struct MonitorTimer {
    state: Rc<RefCell<TimerState>>,
}

pub struct TimerState {
    sender: app::Sender<GuiMessage>,
    timeout: f64,
    active: bool,
}

impl MonitorTimer {
    pub fn new(sender: app::Sender<GuiMessage>) -> Self {
        Self {
            state: Rc::new(RefCell::new(TimerState {
                sender,
                timeout: DEFAULT_TIMEOUT,
                active: false,
            })),
        }
    }

    pub fn start(&mut self) {
        let mut state = self.state.borrow_mut();

        if !state.active {
            state.active = true;

            app::add_timeout3(state.timeout, {
                let cloned_state = self.state.clone();
                move |handle| {
                    if let Ok(s) = cloned_state.try_borrow() {
                        match s.active {
                            true => app::repeat_timeout3(s.timeout, handle),
                            false => app::remove_timeout3(handle),
                        }

                        s.sender.send(GuiMessage::SongMonitorTimeout);
                    }
                }
            });
        }
    }

    pub fn stop(&mut self) {
        self.state.borrow_mut().active = false;
    }
}
