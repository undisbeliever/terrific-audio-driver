//! A BRR data format library

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

mod decoder;
mod encoder;
mod mono_pcm_wav;
mod parse_brr_file;

use std::str::FromStr;

pub use decoder::decode_brr_data;
pub use encoder::{encode_brr, EncodeError};
pub use mono_pcm_wav::{read_16_bit_mono_wave_file, MonoPcm16WaveFile, WavError};
pub use parse_brr_file::{parse_brr_file, ParseError, ValidBrrFile};

pub const SAMPLES_PER_BLOCK: usize = 16;
pub const BYTES_PER_BRR_BLOCK: usize = 9;

pub const BRR_HEADER_END_FLAG: u8 = 0x01;
pub const BRR_HEADER_LOOP_FLAG: u8 = 0x02;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BrrFilter {
    Filter0 = 0,
    Filter1 = 1,
    Filter2 = 2,
    Filter3 = 3,
}

impl FromStr for BrrFilter {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self::Filter0),
            "1" => Ok(Self::Filter1),
            "2" => Ok(Self::Filter2),
            "3" => Ok(Self::Filter3),
            _ => Err("Invalid BRR filter (expected 0-3)"),
        }
    }
}

impl BrrFilter {
    fn as_u8(&self) -> u8 {
        *self as u8
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct BrrSample {
    // Location (within brr_data) for the loop_offset
    loop_offset: Option<u16>,
    brr_data: Vec<u8>,
}

impl BrrSample {
    pub fn loop_offset(&self) -> Option<u16> {
        self.loop_offset
    }
    pub fn brr_data(&self) -> &[u8] {
        &self.brr_data
    }

    pub fn is_looping(&self) -> bool {
        self.loop_offset.is_some()
    }

    pub fn brr_with_loop_header(&self) -> Vec<u8> {
        let lo = self.loop_offset().unwrap_or(0);

        [&lo.to_le_bytes(), self.brr_data.as_slice()].concat()
    }
}
