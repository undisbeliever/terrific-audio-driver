//! Echo buffer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use crate::driver_constants::{
    ECHO_BUFFER_EDL_MS, ECHO_BUFFER_EDL_SIZE, ECHO_BUFFER_MAX_EDL, ECHO_BUFFER_MIN_SIZE,
    FIR_FILTER_SIZE,
};
use crate::errors::ValueError;
use crate::value_newtypes::{i8_with_hex_byte_value_newtype, u8_value_newtype};

u8_value_newtype!(
    EchoEdl,
    EchoEdlOutOfRange,
    NoEchoEdl,
    0,
    ECHO_BUFFER_MAX_EDL
);

u8_value_newtype!(EchoVolume, EchoVolumeOutOfRange, NoEchoVolume, 0, 127);

impl EchoVolume {
    pub const ZERO: Self = Self(0);
}

i8_with_hex_byte_value_newtype!(
    EchoFeedback,
    EchoFeedbackOutOfRange,
    EchoFeedbackOutOfRangeU32,
    EchoFeedbackHexOutOfRange,
    NoEchoFeedback
);

u8_value_newtype!(FirTap, FirTapOutOfRange, NoFirTap, 0, 7);

i8_with_hex_byte_value_newtype!(
    FirCoefficient,
    FirCoefficientOutOfRange,
    FirCoefficientOutOfRangeU32,
    FirCoefficientHexOutOfRange,
    NoFirCoefficient
);

// Source: SnesLab https://sneslab.net/wiki/FIR_Filter
pub const MAX_FIR_ABS_SUM: i32 = 128;

pub const DEFAULT_EDL: EchoEdl = EchoEdl(0);

impl EchoEdl {
    pub fn buffer_size(&self) -> usize {
        if self.as_u8() == 0 {
            ECHO_BUFFER_MIN_SIZE
        } else {
            usize::from(self.as_u8()) * ECHO_BUFFER_EDL_SIZE
        }
    }

    pub fn buffer_size_u16(&self) -> u16 {
        if self.as_u8() == 0 {
            ECHO_BUFFER_MIN_SIZE as u16
        } else {
            u16::from(self.as_u8()) * (ECHO_BUFFER_EDL_SIZE as u16)
        }
    }

    pub fn echo_buffer_addr(&self) -> u16 {
        u16::try_from(0x10000 - self.buffer_size()).unwrap()
    }

    pub fn esa_register(&self) -> u8 {
        self.echo_buffer_addr().to_le_bytes()[1]
    }

    pub fn to_duration(&self) -> Duration {
        Duration::from_millis(u64::from(self.as_u8()) * u64::from(ECHO_BUFFER_EDL_MS))
    }
}

pub struct EchoLength(u8);

impl EchoLength {
    pub const MAX: Self = Self((ECHO_BUFFER_EDL_MS * ECHO_BUFFER_MAX_EDL as u32) as u8);

    pub fn value(&self) -> u8 {
        self.0
    }
}

impl TryFrom<u32> for EchoLength {
    type Error = ValueError;

    fn try_from(length_ms: u32) -> Result<Self, Self::Error> {
        if length_ms % ECHO_BUFFER_EDL_MS != 0 {
            return Err(ValueError::EchoLengthNotMultiple);
        }
        if length_ms > Self::MAX.0.into() {
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

#[derive(Debug, Clone, PartialEq)]
pub struct EchoBuffer {
    pub edl: EchoEdl,
    pub fir: [i8; FIR_FILTER_SIZE],
    pub feedback: i8,
    pub echo_volume_l: EchoVolume,
    pub echo_volume_r: EchoVolume,
}

impl EchoBuffer {
    pub fn test_fir_gain(&self) -> Result<(), ValueError> {
        test_fir_filter_gain(&self.fir)
    }
}

pub fn parse_fir_filter_string(s: &str) -> Result<[i8; FIR_FILTER_SIZE], ValueError> {
    let input: Vec<&str> = s.split_whitespace().collect();

    if input.len() != FIR_FILTER_SIZE {
        return Err(ValueError::InvalidFirFilterSize);
    }

    let mut out = [0; FIR_FILTER_SIZE];

    for (o, s) in out.iter_mut().zip(input.iter()) {
        *o = match s.as_bytes().first() {
            Some(b'$') => match u8::from_str_radix(&s[1..], 16) {
                Ok(i) => i8::from_le_bytes([i]),
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

pub fn test_fir_filter_gain(fir: &[i8; FIR_FILTER_SIZE]) -> Result<(), ValueError> {
    let abs_sum = fir.iter().map(|&i| i32::from(i).abs()).sum();
    if abs_sum <= MAX_FIR_ABS_SUM {
        Ok(())
    } else {
        Err(ValueError::InvalidFirFilterGain { abs_sum })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fir_filter_decimal() {
        assert_eq!(
            parse_fir_filter_string("8 6 12 100 -63 -128 127 0"),
            Ok([8, 6, 12, 100, -63, -128, 127, 0])
        );
    }

    #[test]
    fn test_fir_filter_hex() {
        assert_eq!(
            parse_fir_filter_string("$50 $32 $64 $7F $00 $Ff $9c $80"),
            Ok([80, 50, 100, 127, 0, -1, -100, -128])
        );
    }
}
