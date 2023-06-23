//! Audio-driver constants

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// These values MUST match the audio driver.

#![allow(clippy::assertions_on_constants)]

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
