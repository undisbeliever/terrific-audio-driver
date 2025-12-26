//! Audio Driver Timer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::GuiMessage;

use std::cell::RefCell;
use std::rc::Rc;

extern crate fltk;
use fltk::app;

// The Audio Driver updates the AudioMonitorData 15.6 times per second (32040 / 2048)
const DEFAULT_TIMEOUT: f64 = 1.0 / 30.0;

pub struct MonitorTimer {
    state: Rc<RefCell<TimerState>>,
}

pub struct TimerState {
    sender: app::Sender<GuiMessage>,
    timeout: f64,
    active: bool,
    last_timout_acknowloged: bool,
}

impl MonitorTimer {
    pub fn new(sender: app::Sender<GuiMessage>) -> Self {
        Self {
            state: Rc::new(RefCell::new(TimerState {
                sender,
                timeout: DEFAULT_TIMEOUT,
                active: false,
                last_timout_acknowloged: false,
            })),
        }
    }

    pub fn start(&mut self) {
        let mut state = self.state.borrow_mut();

        if !state.active {
            state.active = true;
            state.last_timout_acknowloged = false;

            app::add_timeout3(state.timeout, {
                let cloned_state = self.state.clone();
                move |handle| {
                    if let Ok(mut s) = cloned_state.try_borrow_mut() {
                        match s.active {
                            true => app::repeat_timeout3(s.timeout, handle),
                            false => app::remove_timeout3(handle),
                        }

                        // Only send a SongMonitorTimeout message after the previous one has been
                        // acknowledged.
                        // This prevents a large backlog of GUI messages when the main-thread has
                        // been stalled by a message box, open dialog or save dialog.
                        if s.last_timout_acknowloged {
                            s.last_timout_acknowloged = false;
                            s.sender.send(GuiMessage::SongMonitorTimeout);
                        }
                    }
                }
            });
        }
    }

    // Acknowledge `SongMonitorTimeout`
    pub fn ack(&mut self) {
        self.state.borrow_mut().last_timout_acknowloged = true;
    }

    pub fn stop(&mut self) {
        self.state.borrow_mut().active = false;
    }
}
