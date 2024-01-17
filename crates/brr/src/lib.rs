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
pub use mono_pcm_wav::{read_mono_pcm_wave_file, MonoPcm16WaveFile, WavError};
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

    pub fn n_samples(&self) -> usize {
        self.brr_data.len() / BYTES_PER_BRR_BLOCK * SAMPLES_PER_BLOCK
    }

    pub fn loop_point_samples(&self) -> Option<usize> {
        self.loop_offset
            .map(|lo| usize::from(lo) / BYTES_PER_BRR_BLOCK * SAMPLES_PER_BLOCK)
    }

    pub fn is_looping(&self) -> bool {
        self.loop_offset.is_some()
    }

    pub fn brr_with_loop_header(&self) -> Vec<u8> {
        let lo = self.loop_offset().unwrap_or(0);

        [&lo.to_le_bytes(), self.brr_data.as_slice()].concat()
    }

    /// Decodes the sample into a buffer, starting at index.
    ///
    /// Returns: the `brr_data()` index at the end of the buffer.
    ///
    /// Safety:
    ///  * Panics if `buf` is not a multiple of `SAMPLES_PER_BLOCK`
    ///  * Panics if `index` is not a multiple of `BYTES_PER_BRR_BLOCK`
    pub fn decode_into_buffer(
        &self,
        buf: &mut [i16],
        index: usize,
        prev1: i16,
        prev2: i16,
    ) -> usize {
        assert!(buf.len() % SAMPLES_PER_BLOCK == 0);
        assert!(index % BYTES_PER_BRR_BLOCK == 0);

        let mut index = index;
        let mut prev1 = prev1;
        let mut prev2 = prev2;
        for out in buf.chunks_exact_mut(SAMPLES_PER_BLOCK) {
            if index < self.brr_data.len() {
                let block = self.brr_data[index..index + BYTES_PER_BRR_BLOCK]
                    .try_into()
                    .unwrap();
                let decoded = decoder::decode_brr_block(&block, prev1, prev2);
                out.copy_from_slice(&decoded);

                prev1 = decoded[SAMPLES_PER_BLOCK - 1];
                prev2 = decoded[SAMPLES_PER_BLOCK - 2];

                index += BYTES_PER_BRR_BLOCK;
                if index >= self.brr_data.len() {
                    if let Some(lo) = self.loop_offset {
                        index = lo.into()
                    }
                }
            } else {
                out.fill(0)
            }
        }
        index
    }
}
