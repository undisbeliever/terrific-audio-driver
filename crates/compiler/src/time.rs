//! Common structs used by bytecode, mml_compiler and samples

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use std::time::Duration;

use crate::errors::ValueError;
use crate::value_newtypes::u8_value_newtype;

pub const TIMER_HZ: u32 = 8000;

pub const MIN_TICK_TIMER: u8 = 64;
const STARTING_DEFAULT_NOTE_LENGTH: u8 = 4;

u8_value_newtype!(ZenLen, ZenLenOutOfRange, NoZenLen, 4, u8::MAX);

pub const DEFAULT_ZENLEN: ZenLen = ZenLen(96);
pub const STARTING_MML_LENGTH: MmlDefaultLength = MmlDefaultLength {
    length: STARTING_DEFAULT_NOTE_LENGTH,
    length_in_ticks: false,
    number_of_dots: 0,
};

impl ZenLen {
    pub fn starting_length(&self) -> TickCounter {
        TickCounter::new((self.as_u8() / STARTING_DEFAULT_NOTE_LENGTH).into())
    }
}

const CLOCK_CYCLES_PER_BPM: u32 = 48;
const MIN_BPM: u8 = ((TIMER_HZ * 60) / (CLOCK_CYCLES_PER_BPM * (u8::MAX as u32)) + 1) as u8;
const MAX_BPM: u8 = ((TIMER_HZ * 60) / (CLOCK_CYCLES_PER_BPM * (MIN_TICK_TIMER as u32)) + 1) as u8;

u8_value_newtype!(Bpm, BpmOutOfRange, NoBpm, MIN_BPM, MAX_BPM);

pub const DEFAULT_BPM: Bpm = Bpm(60);

impl Bpm {
    pub fn to_tick_clock(self) -> Result<TickClock, ValueError> {
        let ticks_per_minute = CLOCK_CYCLES_PER_BPM * u32::from(self.as_u8());

        let tc = f64::from(TIMER_HZ * 60) / f64::from(ticks_per_minute);
        let tc = tc.round();

        if tc >= 0.0 && tc < u32::MAX.into() {
            TickClock::try_from(tc as u32)
        } else {
            Err(ValueError::CannotConvertBpmToTickClock)
        }
    }
}

// TickCounter can only be incremented
#[derive(Copy, Clone, Default, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct TickCounter {
    value: u32,
}

impl TickCounter {
    pub fn new(value: u32) -> TickCounter {
        Self { value }
    }

    pub fn value(&self) -> u32 {
        self.value
    }

    pub fn is_zero(&self) -> bool {
        self.value == 0
    }

    pub fn to_duration(&self, clock: TickClock) -> Duration {
        const _: () = assert!(1_000_000 % TIMER_HZ == 0);
        const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

        let ticks = u64::from(self.value());
        let clock = u64::from(clock.as_u8());

        Duration::from_micros(ticks * clock * MICRO_MUL)
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

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TickCounterWithLoopFlag {
    pub ticks: TickCounter,
    pub in_loop: bool,
}

u8_value_newtype!(
    TickClock,
    TickClockOutOfRange,
    NoTickClock,
    MIN_TICK_TIMER,
    u8::MAX
);

#[derive(Debug, Clone, PartialEq)]
pub struct MmlDefaultLength {
    length: u8,
    length_in_ticks: bool,
    number_of_dots: u8,
}

impl MmlDefaultLength {
    pub fn new(length: u8, length_in_ticks: bool, number_of_dots: u8) -> Self {
        Self {
            length,
            length_in_ticks,
            number_of_dots,
        }
    }

    pub fn length(&self) -> u8 {
        self.length
    }

    pub fn length_in_ticks(&self) -> bool {
        self.length_in_ticks
    }

    pub fn number_of_dots(&self) -> u8 {
        self.number_of_dots
    }

    pub fn to_tick_count(&self, zenlen: ZenLen) -> Result<TickCounter, ValueError> {
        if self.length_in_ticks {
            if self.number_of_dots != 0 {
                return Err(ValueError::DotsNotAllowedAfterClockValue);
            }
            Ok(TickCounter::new(self.length.into()))
        } else {
            let l = self.length;
            let zenlen = zenlen.as_u8();

            if l == 0 || l > zenlen {
                return Err(ValueError::InvalidNoteLength);
            }

            let ticks = u32::from(zenlen / l);
            let ticks = add_dots_to_length(ticks, self.number_of_dots);
            Ok(TickCounter::new(ticks))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MmlLength {
    length: Option<u32>,
    length_in_ticks: bool,
    number_of_dots: u8,
}

impl MmlLength {
    pub fn new(length: Option<u32>, length_in_ticks: bool, number_of_dots: u8) -> Self {
        Self {
            length,
            length_in_ticks,
            number_of_dots,
        }
    }

    pub fn length(&self) -> Option<u32> {
        self.length
    }

    pub fn length_in_ticks(&self) -> bool {
        self.length_in_ticks
    }

    pub fn number_of_dots(&self) -> u8 {
        self.number_of_dots
    }

    pub fn to_tick_count(
        &self,
        default_len: TickCounter,
        zenlen: ZenLen,
    ) -> Result<TickCounter, ValueError> {
        let ticks = if self.length_in_ticks {
            let ticks = match self.length {
                Some(l) => l,
                None => return Err(ValueError::MissingNoteLengthTickCount),
            };
            if self.number_of_dots != 0 {
                return Err(ValueError::DotsNotAllowedAfterClockValue);
            }
            ticks
        } else {
            // Whole note length divisor
            let ticks = match self.length {
                Some(l) => {
                    let zenlen = zenlen.0.into();

                    if l == 0 || l > zenlen {
                        return Err(ValueError::InvalidNoteLength);
                    }
                    zenlen / l
                }
                None => default_len.value(),
            };
            add_dots_to_length(ticks, self.number_of_dots)
        };
        Ok(TickCounter::new(ticks))
    }
}

fn add_dots_to_length(ticks: u32, n_dots: u8) -> u32 {
    let mut ticks = ticks;

    if n_dots > 0 {
        let mut half_t = ticks / 2;
        for _ in 0..n_dots {
            ticks += half_t;
            half_t /= 2;
        }
    }

    ticks
}
