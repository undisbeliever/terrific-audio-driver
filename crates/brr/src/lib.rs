//! A BRR data format library

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod encoder;
mod mono_pcm_wav;
mod parse_brr_file;

pub use encoder::{encode_brr, EncodeError};
pub use mono_pcm_wav::{read_16_bit_mono_wave_file, Error, MonoPcm16WaveFile};
pub use parse_brr_file::{parse_brr_file, ParseError};

pub const SAMPLES_PER_BLOCK: usize = 16;
pub const BYTES_PER_BRR_BLOCK: usize = 9;

pub const BRR_HEADER_END_FLAG: u8 = 0x01;
pub const BRR_HEADER_LOOP_FLAG: u8 = 0x02;

#[derive(Hash, Eq, PartialEq)]
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
