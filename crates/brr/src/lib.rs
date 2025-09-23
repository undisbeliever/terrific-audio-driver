//! A BRR data format library

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

mod decoder;
mod encoder;
mod gaussian_overflow_test;
mod mono_pcm_wav;
mod parse_brr_file;

use std::str::FromStr;

pub use decoder::decode_brr_data;
pub use encoder::{encode_brr, EncodeError, Evaluator, DEFAULT_EVALUATOR};
use gaussian_overflow_test::test_for_gaussian_overflow_glitch;
pub use mono_pcm_wav::{read_mono_pcm_wave_file, MonoPcm16WaveFile, WavError};
pub use parse_brr_file::{parse_brr_file, ParseError, ValidBrrFile};

pub const SAMPLES_PER_BLOCK: usize = 16;
pub const BYTES_PER_BRR_BLOCK: usize = 9;

pub const BRR_HEADER_END_FLAG: u8 = 0x01;
pub const BRR_HEADER_LOOP_FLAG: u8 = 0x02;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SampleNumber(pub usize);

impl From<usize> for SampleNumber {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockNumber(pub usize);

impl From<usize> for BlockNumber {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

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
    /// Location (within brr_data) for the loop_offset
    loop_offset: Option<u16>,

    /// BRR data
    brr_data: Vec<u8>,
}

impl BrrSample {
    pub fn loop_offset(&self) -> Option<u16> {
        self.loop_offset
    }
    pub fn brr_data(&self) -> &[u8] {
        &self.brr_data
    }

    pub fn n_brr_blocks(&self) -> usize {
        self.brr_data.len() / BYTES_PER_BRR_BLOCK
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

    /// Test if the BRR sample is vulnerable to the S-DSP 3 maximun negative samples overflow glitch.
    ///
    /// Test is done by decoding the BRR sample as it may loop imperfectly.
    ///
    /// # Arguments
    ///  * `n_loops`: Number of times to loop the sample (if it is a looping sample).
    ///
    /// # Caution
    ///  * Expensive if n_loop is too large
    ///  * Panics if `self.loop_offset()` is invalid
    pub fn test_for_gaussian_overflow_glitch_n_loops(&self, n_loops: u32) -> bool {
        let n_loops = match self.is_looping() {
            true => n_loops + 1,
            false => 1,
        };

        let mut loop_prev1 = 0;
        let mut loop_prev2 = 0;

        for i in 0..n_loops {
            let mut prev1 = loop_prev1;
            let mut prev2 = loop_prev2;

            let i: usize = match i {
                0 => 0,
                _ => self.loop_offset.unwrap().into(),
            };

            for block in self.brr_data[i..].chunks_exact(BYTES_PER_BRR_BLOCK) {
                let decoded_samples =
                    decoder::decode_brr_block(&block.try_into().unwrap(), prev1, prev2);

                if test_for_gaussian_overflow_glitch(prev1, prev2, &decoded_samples) {
                    return true;
                }

                prev1 = decoded_samples[SAMPLES_PER_BLOCK - 1];
                prev2 = decoded_samples[SAMPLES_PER_BLOCK - 2];
            }

            // Early exit if a perfectly looping sample
            if (prev1, prev2) == (loop_prev1, loop_prev2) {
                break;
            }

            loop_prev1 = prev1;
            loop_prev2 = prev2;
        }

        false
    }

    /// Calls `test_for_gaussian_overflow_bug_n_loops()`,
    /// Looping between 2 and 20 times, depending on the size of the sample.
    pub fn test_for_gaussian_overflow_glitch_autoloop(&self) -> bool {
        const ONE_SECOND: u32 = 32000 / SAMPLES_PER_BLOCK as u32 * BYTES_PER_BRR_BLOCK as u32;

        let bytes = u32::try_from(self.n_brr_blocks()).unwrap_or(ONE_SECOND);
        let n_loops = (ONE_SECOND / bytes).clamp(2, 20);

        self.test_for_gaussian_overflow_glitch_n_loops(n_loops)
    }
}

#[cfg(test)]
#[allow(clippy::bool_assert_comparison)]
mod test {
    use super::*;

    #[test]
    fn test_for_gaussian_overflow_bug_false() {
        // Sine wave sample created using wav2brr
        let s = BrrSample {
            loop_offset: Some(0),
            brr_data: vec![
                0xc0, 0x00, 0x12, 0x23, 0x44, 0x55, 0x66, 0x77, 0x77, 0x9c, 0x5e, 0x00, 0x00, 0x00,
                0xf0, 0xf0, 0xe0, 0xff, 0x7c, 0xad, 0xbb, 0xcb, 0xbb, 0xcc, 0xbd, 0xcd, 0xdd, 0x79,
                0xbb, 0xac, 0xbc, 0xbd, 0xdd, 0xee, 0xf0, 0xf0,
            ],
        };
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(10), false);

        // Manually created BRR sample
        let s = BrrSample {
            loop_offset: None,
            brr_data: vec![0xc1, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77],
        };
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(10), false);

        // Manually created BRR sample
        // This sample has the gaussian overflow bug if it loops
        let s = BrrSample {
            loop_offset: None,
            brr_data: vec![0xc1, 0x88, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x88],
        };
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(10), false);
    }

    #[test]
    fn test_for_gaussian_overflow_bug_true() {
        // Manually created BRR sample
        let s = BrrSample {
            loop_offset: None,
            brr_data: vec![0xc1, 0x00, 0x00, 0x00, 0x08, 0x88, 0x00, 0x00, 0x00],
        };
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(10), true);
    }

    #[test]
    fn test_for_gaussian_overflow_bug_looping_prev1_prev2() {
        // Manually created BRR sample
        let s = BrrSample {
            loop_offset: Some(9),
            brr_data: vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // 1 max negative at the start of the loop
                // 2 max negative at the end of a looping sample
                0xc3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x88,
            ],
        };

        // If the sample is not looped, there is no bug
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(0), false);
        assert_eq!(s.test_for_gaussian_overflow_glitch_autoloop(), true);
    }

    #[test]
    fn test_for_gaussian_overflow_bug_looping_prev1() {
        // Manually created BRR sample
        // 1 max negative at the end of a looping sample
        let s = BrrSample {
            loop_offset: Some(9),
            brr_data: vec![
                // first sample
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // 2 max negative at the start of the loop
                0xc0, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // 1 max negative at the end of a looping sample
                0xc3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
            ],
        };

        // If the sample is not looped, there is no bug
        assert_eq!(s.test_for_gaussian_overflow_glitch_n_loops(0), false);
        assert_eq!(s.test_for_gaussian_overflow_glitch_autoloop(), true);
    }
}
