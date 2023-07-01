//! Echo buffer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::{ECHO_BUFFER_EDL_MS, ECHO_BUFFER_MAX_EDL, FIR_FILTER_SIZE};
use crate::errors::ValueError;
use crate::newtype_macros::u8_newtype;

u8_newtype!(EchoEdl, EchoEdlOutOfRange, 0, ECHO_BUFFER_MAX_EDL);

pub const DEFAULT_EDL: EchoEdl = EchoEdl(0);

pub struct EchoLength(u8);

impl TryFrom<u32> for EchoLength {
    type Error = ValueError;

    fn try_from(length_ms: u32) -> Result<Self, Self::Error> {
        const MAX: u8 = (ECHO_BUFFER_EDL_MS * ECHO_BUFFER_MAX_EDL as u32) as u8;

        if length_ms % ECHO_BUFFER_EDL_MS != 0 {
            return Err(ValueError::EchoLengthNotMultiple);
        }
        if length_ms > MAX.into() {
            return Err(ValueError::EchoBufferTooLarge);
        }

        Ok(EchoLength(length_ms.try_into().unwrap()))
    }
}

impl EchoLength {
    pub fn to_edl(&self) -> EchoEdl {
        EchoEdl(self.0 / (ECHO_BUFFER_EDL_MS as u8))
    }
}

pub struct EchoBuffer {
    pub edl: EchoEdl,
    pub fir: [i8; FIR_FILTER_SIZE],
    pub feedback: i8,
    pub echo_volume: i8,
}

pub fn parse_fir_filter_string(s: &str) -> Result<[i8; FIR_FILTER_SIZE], ValueError> {
    let input: Vec<&str> = s.split_whitespace().collect();

    if input.len() != FIR_FILTER_SIZE {
        return Err(ValueError::InvalidFirFilterSize);
    }

    let mut out = [0; FIR_FILTER_SIZE];

    for (o, s) in out.iter_mut().zip(input.iter()) {
        *o = match s.as_bytes().first() {
            Some(b'$') => match i8::from_str_radix(&s[1..], 16) {
                Ok(i) => i,
                Err(_) => return Err(ValueError::InvalidFirFilter),
            },
            Some(_) => match s.parse::<i8>() {
                Ok(i) => i,
                Err(_) => return Err(ValueError::InvalidFirFilter),
            },

            None => return Err(ValueError::InvalidFirFilter),
        };
    }

    Ok(out)
}
