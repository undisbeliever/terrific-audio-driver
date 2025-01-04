//! Echo buffer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::cmp::min;
use std::time::Duration;

use crate::driver_constants::{
    ECHO_BUFFER_EDL_MS, ECHO_BUFFER_EDL_SIZE, ECHO_BUFFER_MAX_EDL, ECHO_BUFFER_MIN_SIZE,
    FIR_FILTER_SIZE,
};
use crate::errors::ValueError;
use crate::invert_flags::InvertFlags;
use crate::value_newtypes::{
    i8_with_hex_byte_value_newtype, parse_i8wh, u8_value_newtype, UnsignedValueNewType,
};

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

pub const IDENTITY_FILTER: [FirCoefficient; FIR_FILTER_SIZE] = [
    FirCoefficient(127),
    FirCoefficient(0),
    FirCoefficient(0),
    FirCoefficient(0),
    FirCoefficient(0),
    FirCoefficient(0),
    FirCoefficient(0),
    FirCoefficient(0),
];

impl EchoEdl {
    pub const ZERO: Self = Self(0);

    pub fn buffer_size(&self) -> u16 {
        if self.0 == 0 {
            ECHO_BUFFER_MIN_SIZE as u16
        } else {
            u16::from(self.0) * (ECHO_BUFFER_EDL_SIZE as u16)
        }
    }

    pub fn to_duration(&self) -> Duration {
        Duration::from_millis(u64::from(self.as_u8()) * u64::from(ECHO_BUFFER_EDL_MS))
    }

    pub fn to_length(&self) -> EchoLength {
        EchoLength(self.0 * (ECHO_BUFFER_EDL_MS as u8))
    }
}

#[derive(Clone, Copy)]
pub struct EchoLength(u8);

impl EchoLength {
    pub const MIN: Self = Self(0);
    pub const MAX: Self = Self((ECHO_BUFFER_EDL_MS * ECHO_BUFFER_MAX_EDL as u32) as u8);

    pub fn to_edl(&self) -> EchoEdl {
        EchoEdl(self.0 / (ECHO_BUFFER_EDL_MS as u8))
    }
}

impl UnsignedValueNewType for EchoLength {
    const MISSING_ERROR: ValueError = ValueError::NoEchoLength;

    type ValueType = u8;

    fn value(&self) -> u8 {
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
            return Err(ValueError::EchoLengthTooLarge(length_ms));
        }

        Ok(EchoLength(length_ms.try_into().unwrap()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EchoBuffer {
    pub max_edl: EchoEdl,
    pub edl: EchoEdl,
    pub fir: [FirCoefficient; FIR_FILTER_SIZE],
    pub feedback: EchoFeedback,
    pub echo_volume_l: EchoVolume,
    pub echo_volume_r: EchoVolume,
    pub invert: InvertFlags,
}

impl EchoBuffer {
    pub fn test_fir_gain(&self) -> Result<(), ValueError> {
        test_fir_filter_gain(&self.fir)
    }

    pub fn buffer_size(&self) -> usize {
        self.buffer_size_u16().into()
    }

    pub fn buffer_size_u16(&self) -> u16 {
        self.max_edl.buffer_size()
    }

    pub fn buffer_addr(&self) -> u16 {
        u16::try_from(0x10000 - self.buffer_size()).unwrap()
    }

    pub fn esa_register(&self) -> u8 {
        self.buffer_addr().to_le_bytes()[1]
    }

    pub fn edl_register(&self) -> u8 {
        min(self.max_edl.as_u8(), self.edl.as_u8())
    }
}

pub fn parse_fir_filter_string(s: &str) -> Result<[FirCoefficient; FIR_FILTER_SIZE], ValueError> {
    let input: Vec<&str> = s.split_whitespace().collect();

    if input.len() != FIR_FILTER_SIZE {
        return Err(ValueError::InvalidFirFilterSize);
    }

    let mut out = [FirCoefficient(0); FIR_FILTER_SIZE];
    assert_eq!(out.len(), input.len());
    for (f, s) in out.iter_mut().zip(input.iter()) {
        *f = parse_i8wh(s)?;
    }

    Ok(out)
}

pub fn test_fir_filter_gain(fir: &[FirCoefficient; FIR_FILTER_SIZE]) -> Result<(), ValueError> {
    let abs_sum = fir.iter().map(|&i| i32::from(i.as_i8()).abs()).sum();
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
            Ok([8, 6, 12, 100, -63, -128, 127, 0].map(FirCoefficient))
        );
    }

    #[test]
    fn test_fir_filter_hex() {
        assert_eq!(
            parse_fir_filter_string("$50 $32 $64 $7F $00 $Ff $9c $80"),
            Ok([80, 50, 100, 127, 0, -1, -100, -128].map(FirCoefficient))
        );
    }
}
