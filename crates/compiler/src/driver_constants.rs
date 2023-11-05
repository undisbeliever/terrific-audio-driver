//! Audio-driver constants

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// These values MUST match the audio driver.

#![allow(clippy::assertions_on_constants)]

pub const AUDIO_RAM_SIZE: usize = 0x10000;

pub mod addresses {
    mod _symbols {
        include!(concat!(env!("OUT_DIR"), "/symbols.rs"));
    }

    pub const DRIVER_CODE: u16 = _symbols::DRIVER_MAIN;
    pub const LOADER: u16 = _symbols::START_LOADER;
    pub const SONG_PTR: u16 = _symbols::SONG_PTR;
    pub const LOADER_DATA_TYPE: u16 = _symbols::LOADER_DATA_TYPE;

    pub const CHANNEL_INSTRUCTION_PTR_L: u16 = _symbols::CHANNEL_INSTRUCTION_PTR_L;
    pub const CHANNEL_INSTRUCTION_PTR_H: u16 = _symbols::CHANNEL_INSTRUCTION_PTR_H;

    // MUST match `audio-driver/src/common_memmap.wiz`
    pub const COMMON_DATA: u16 = 0x800 - 4;

    const _: () = assert!(
        _symbols::_LAST_LOADER_SYMBOL < DRIVER_CODE,
        "Loader contains a symbol inside the driver code"
    );

    const _: () = assert!(
        COMMON_DATA > _symbols::_LAST_DRIVER_SYMBOL && COMMON_DATA > _symbols::_LAST_LOADER_SYMBOL,
        "Invalid COMMON_DATA address"
    );

    const _: () = assert!(
        DRIVER_CODE % 2 == 0,
        "Loader requires an even DRIVER_CODE address"
    );
    const _: () = assert!(
        COMMON_DATA % 2 == 0,
        "Loader requires an even COMMON_DATA address"
    );
    const _: () = assert!(
        (COMMON_DATA as usize + super::COMMON_DATA_HEADER_SIZE) & 0xff == 0,
        "BRR directory is not page aligned"
    );
}

pub const N_MUSIC_CHANNELS: usize = 6;

// Song ID 0 is silence
pub const FIRST_SONG_ID: usize = 1;
pub const MAX_N_SONGS: usize = 0x100 - FIRST_SONG_ID;

pub const MAX_DIR_ITEMS: usize = u8::MAX as usize;
pub const MAX_INSTRUMENTS: usize = u8::MAX as usize;
pub const MAX_SOUND_EFFECTS: usize = 192;

pub const PITCH_TABLE_SIZE: usize = 256;

pub const COMMON_DATA_HEADER_SIZE: usize = 4 + (2 * PITCH_TABLE_SIZE);

pub const COMMON_DATA_BYTES_PER_DIR: usize = 4;
pub const COMMON_DATA_BYTES_PER_INSTRUMENTS: usize = 4;
pub const COMMON_DATA_BYTES_PER_SOUND_EFFECT: usize = 2;

pub const MAX_COMMON_DATA_SIZE: usize = 0xD000;

// Loader constants
// MUST match `audio-driver/src/io-commands.wiz`
pub struct LoaderDataType {
    pub stereo_flag: bool,

    pub play_song: bool,

    // NOTE: `SkipEchoBufferReset` can corrupt memory if the internal S-DSP echo buffer state
    // does not match the song's echo EDL/ESA register values.
    pub skip_echo_buffer_reset: bool,
}

impl LoaderDataType {
    pub fn driver_value(&self) -> u8 {
        // LoaderDataType.MIN_SONG_VALUE
        let mut o = 2;

        if self.stereo_flag {
            o |= 1 << 7;
        }
        if self.play_song {
            o |= 1 << 6;
        }
        if self.skip_echo_buffer_reset {
            o |= 1 << 3;
        }

        o
    }
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
