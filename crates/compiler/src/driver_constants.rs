//! Audio-driver constants

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// These values MUST match the audio driver.

#![allow(clippy::assertions_on_constants)]

pub const AUDIO_RAM_SIZE: usize = 0x10000;

pub const N_MUSIC_CHANNELS: usize = 6;

// Song ID 0 is silence
pub const FIRST_SONG_ID: usize = 1;
pub const MAX_N_SONGS: usize = 0x100 - FIRST_SONG_ID;

pub const MAX_DIR_ITEMS: usize = u8::MAX as usize;
pub const MAX_INSTRUMENTS: usize = u8::MAX as usize;
pub const MAX_SOUND_EFFECTS: usize = 192;

pub const PITCH_TABLE_SIZE: usize = 256;

pub const COMMON_DATA_ADDR: u16 = 0x800 - 4;

pub const COMMON_DATA_HEADER_ADDR: u16 = COMMON_DATA_ADDR;
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

// Driver constants

// MUST match `audio-driver/src/common_memmap.wiz`
pub const DRIVER_CODE_ADDR: u16 = 0x200;
pub const DRIVER_LOADER_ADDR: u16 = 0x160;
pub const DRIVER_SONG_PTR_ADDR: u16 = 0x00ec;
pub const DRIVER_LOADER_DATA_TYPE_ADDR: u16 = DRIVER_SONG_PTR_ADDR + 2;

// Loader constants
// MUST match `audio-driver/src/io-commands.wiz`
pub enum LoaderDataType {
    MonoSongData = 0x02,
    StereoSongData = 0x82,
}

// S-DSP constants

pub const FIR_FILTER_SIZE: usize = 8;
pub const IDENTITY_FILTER: [i8; FIR_FILTER_SIZE] = [127, 0, 0, 0, 0, 0, 0, 0];

pub const ECHO_BUFFER_EDL_MS: u32 = 16;
pub const ECHO_BUFFER_EDL_SIZE: usize = 2048;
pub const ECHO_BUFFER_MIN_SIZE: usize = 256;
pub const ECHO_BUFFER_MAX_EDL: u8 = 15;

// Song constants
pub const MAX_SUBROUTINES: usize = 256;

pub const SONG_HEADER_SIZE: usize = N_MUSIC_CHANNELS * 4 + 13;
pub const SONG_HEADER_CHANNELS_SIZE: usize = N_MUSIC_CHANNELS * 4;
pub const SONG_HEADER_TICK_TIMER_OFFSET: usize = SONG_HEADER_SIZE - 2;

pub const MAX_SONG_DATA_SIZE: usize = 0xD000;

// Sound effect constants
pub const SFX_TICK_CLOCK: u8 = 64;
