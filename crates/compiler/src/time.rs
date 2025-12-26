//! Common structs used by bytecode, mml_compiler and samples

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use std::time::Duration;

use crate::errors::ValueError;
use crate::value_newtypes::{u16_value_newtype, u8_value_newtype};
use crate::{driver_constants, UnsignedValueNewType};

pub const TIMER_HZ: u32 = 8000;

pub const MIN_TICK_TIMER: u8 = 64;
pub const MAX_TICK_TIMER: u32 = 256;

const STARTING_DEFAULT_NOTE_LENGTH: u8 = 4;

u8_value_newtype!(ZenLen, ZenLenOutOfRange, NoZenLen, 4, u8::MAX);

pub const DEFAULT_ZENLEN: ZenLen = ZenLen(96);
pub const STARTING_MML_LENGTH: MmlDefaultLength = MmlDefaultLength {
    length: STARTING_DEFAULT_NOTE_LENGTH,
    length_in_ticks: false,
    number_of_dots: 0,
};

impl ZenLen {
    pub fn starting_length(&self) -> CommandTicks {
        CommandTicks::new((self.as_u8() / STARTING_DEFAULT_NOTE_LENGTH).into())
    }
}

const CLOCK_CYCLES_PER_BPM: u32 = 48;
const MIN_BPM: u8 = ((TIMER_HZ * 60) / (CLOCK_CYCLES_PER_BPM * (MAX_TICK_TIMER)) + 1) as u8;
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

/// A tick counter that can only be incremented
///
/// CAUTION: TickCounter uses saturating_add to ensure it never overflows in release mode.
#[derive(Copy, Clone, Default, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct TickCounter {
    value: u32,
}

impl TickCounter {
    pub const MAX: TickCounter = TickCounter { value: u32::MAX };

    pub const fn new(value: u32) -> TickCounter {
        Self { value }
    }

    pub const fn value(&self) -> u32 {
        self.value
    }

    pub fn is_zero(&self) -> bool {
        self.value == 0
    }

    pub fn to_duration(&self, clock: TickClock) -> Duration {
        const _: () = assert!(1_000_000 % TIMER_HZ == 0);
        const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

        let ticks = u64::from(self.value());
        let clock = u64::from(clock.value());

        Duration::from_micros(ticks * clock * MICRO_MUL)
    }
}

impl std::ops::Add for TickCounter {
    type Output = Self;

    fn add(self, b: Self) -> Self {
        TickCounter {
            value: self.value.saturating_add(b.value),
        }
    }
}

impl std::ops::AddAssign for TickCounter {
    fn add_assign(&mut self, b: Self) {
        self.value = self.value.saturating_add(b.value);
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TickCounterWithLoopFlag {
    pub ticks: TickCounter,
    pub in_loop: bool,
}

u16_value_newtype!(
    TickClock,
    TickClockOutOfRange,
    NoTickClock,
    MIN_TICK_TIMER as u16,
    MAX_TICK_TIMER as u16
);

impl TickClock {
    pub const SFX_TICK_CLOCK: Self = Self(driver_constants::SFX_TICK_CLOCK as u16);

    pub fn into_driver_value(self) -> u8 {
        match self.0 {
            256 => 0,
            v => v.try_into().unwrap(),
        }
    }
}

pub fn timer_register_to_bpm(timer: u8) -> f64 {
    match timer {
        1..=MIN_TICK_TIMER => 0.0,
        0 => {
            f64::from(TIMER_HZ * 60) / (f64::from(MAX_TICK_TIMER) * f64::from(CLOCK_CYCLES_PER_BPM))
        }
        t => f64::from(TIMER_HZ * 60) / (f64::from(t) * f64::from(CLOCK_CYCLES_PER_BPM)),
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct CommandTicks(u16);

impl CommandTicks {
    pub const MIN: Self = Self(u16::MIN);
    pub const MAX: Self = Self(u16::MAX);

    pub const ZERO: Self = Self(0);

    pub const fn new(ticks: u16) -> Self {
        Self(ticks)
    }

    pub const fn value(self) -> u16 {
        self.0
    }

    pub const fn as_u16(self) -> u16 {
        self.0
    }

    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }

    /// Outputs an error if adding `rhs` overflows
    pub fn try_add(self, rhs: Self) -> Result<Self, ValueError> {
        match self.0.checked_add(rhs.0) {
            Some(t) => Ok(Self(t)),
            None => Err(ValueError::CommandTicksOverflow),
        }
    }
}

impl TryFrom<u32> for CommandTicks {
    type Error = ValueError;

    fn try_from(value: u32) -> Result<Self, ValueError> {
        match value.try_into() {
            Ok(v) => Ok(Self(v)),
            Err(_) => Err(ValueError::CommandTicksOutOfRange(value)),
        }
    }
}

impl From<CommandTicks> for TickCounter {
    fn from(value: CommandTicks) -> Self {
        Self {
            value: value.0.into(),
        }
    }
}

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

    pub fn to_command_ticks(&self, zenlen: ZenLen) -> Result<CommandTicks, ValueError> {
        if self.length_in_ticks {
            if self.number_of_dots != 0 {
                return Err(ValueError::DotsNotAllowedAfterClockValue);
            }
            Ok(CommandTicks::new(self.length.into()))
        } else {
            let l = self.length;
            let zenlen = zenlen.as_u8();

            if l == 0 || l > zenlen {
                return Err(ValueError::InvalidLength);
            }

            let ticks = u16::from(zenlen / l);
            let ticks = add_dots_to_length(ticks, self.number_of_dots);
            Ok(CommandTicks::new(ticks))
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

    pub fn to_command_ticks(
        &self,
        default_len: CommandTicks,
        zenlen: ZenLen,
    ) -> Result<CommandTicks, ValueError> {
        let ticks = if self.length_in_ticks {
            let ticks = match self.length {
                Some(l) => match l.try_into() {
                    Ok(l) => l,
                    Err(_) => return Err(ValueError::CommandTicksOutOfRange(l)),
                },
                None => return Err(ValueError::MissingLengthTickCount),
            };
            if self.number_of_dots != 0 {
                return Err(ValueError::DotsNotAllowedAfterClockValue);
            }
            ticks
        } else {
            // Whole note length divisor
            let ticks = match self.length {
                Some(l) => {
                    let zenlen = zenlen.0;

                    if l == 0 || l > zenlen.into() {
                        return Err(ValueError::InvalidLength);
                    }
                    (zenlen / u8::try_from(l).unwrap()).into()
                }
                None => default_len.value(),
            };
            add_dots_to_length(ticks, self.number_of_dots)
        };
        Ok(CommandTicks::new(ticks))
    }
}

fn add_dots_to_length(ticks: u16, n_dots: u8) -> u16 {
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
