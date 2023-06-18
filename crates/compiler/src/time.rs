//! Common structs used by bytecode, mml_compiler and samples

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::TickClockError;

pub const MIN_TICK_TIMER: u8 = 64;

#[allow(dead_code)]
pub const MAX_TICK_TIMER: u8 = u8::MAX;

// TickCounter can only be incremented
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TickCounter {
    value: u64,
}

impl TickCounter {
    pub fn new(value: u64) -> TickCounter {
        Self { value }
    }

    pub fn value(&self) -> u64 {
        self.value
    }
}

impl std::ops::Add for TickCounter {
    type Output = Self;

    fn add(self, b: Self) -> Self {
        TickCounter {
            value: self.value + b.value,
        }
    }
}

impl std::ops::AddAssign for TickCounter {
    fn add_assign(&mut self, b: Self) {
        self.value += b.value;
    }
}

pub struct TickClock {
    register_value: u8,
}

impl TickClock {
    fn new(register_value: u8) -> Result<Self, TickClockError> {
        if register_value >= MIN_TICK_TIMER {
            Ok(Self { register_value })
        } else {
            Err(TickClockError::OutOfBounds(register_value.into()))
        }
    }

    pub fn new_from_str(timer: &str) -> Result<Self, TickClockError> {
        let timer: u32 = match timer.parse() {
            Ok(i) => i,
            Err(_) => return Err(TickClockError::CannotParse(timer.to_owned())),
        };
        let timer = match u8::try_from(timer) {
            Ok(i) => i,
            Err(_) => return Err(TickClockError::OutOfBounds(timer)),
        };

        Self::new(timer)
    }

    pub fn register_value(&self) -> u8 {
        self.register_value
    }
}
