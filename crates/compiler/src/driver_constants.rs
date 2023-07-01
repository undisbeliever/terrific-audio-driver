//! Audio-driver constants

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// These values MUST match the audio driver.

#![allow(clippy::assertions_on_constants)]

pub const N_MUSIC_CHANNELS: usize = 6;

pub const MAX_DIR_ITEMS: usize = 256;
pub const MAX_INSTRUMENTS: usize = 256;
pub const MAX_SOUND_EFFECTS: usize = 192;

pub const PITCH_TABLE_SIZE: usize = 256;

pub const COMMON_DATA_HEADER_ADDR: u16 = 0x800 - 4;
pub const COMMON_DATA_HEADER_SIZE: usize = 4 + (2 * PITCH_TABLE_SIZE);

pub const COMMON_DATA_BYTES_PER_DIR: usize = 4;
pub const COMMON_DATA_BYTES_PER_INSTRUMENTS: usize = 4;
pub const COMMON_DATA_BYTES_PER_SOUND_EFFECT: usize = 2;

pub const MAX_COMMON_DATA_SIZE: usize = 0xD000;

const _: () = assert!(
    COMMON_DATA_HEADER_ADDR % 2 == 0,
    "Loader requires an even common data address"
);
const _: () = assert!(
    (COMMON_DATA_HEADER_ADDR as usize + COMMON_DATA_HEADER_SIZE) & 0xff == 0,
    "BRR directory is not page aligned"
);

// S-DSP constants

pub const FIR_FILTER_SIZE: usize = 8;
pub const IDENTITY_FILTER: [i8; FIR_FILTER_SIZE] = [127, 0, 0, 0, 0, 0, 0, 0];

pub const ECHO_BUFFER_EDL_MS: u32 = 16;
pub const ECHO_BUFFER_EDL_SIZE: usize = 2048;
pub const ECHO_BUFFER_MAX_EDL: u8 = 15;

// Song constants
pub const MAX_SUBROUTINES: usize = 256;

pub const SONG_HEADER_SIZE: usize = N_MUSIC_CHANNELS * 4 + 13;

pub const MAX_SONG_DATA_SIZE: usize = 0xD000;
