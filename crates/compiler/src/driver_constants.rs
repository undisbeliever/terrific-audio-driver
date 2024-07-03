//! Audio-driver constants

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// These values MUST match the audio driver.

#![allow(clippy::assertions_on_constants)]

pub const AUDIO_RAM_SIZE: usize = 0x10000;

mod _symbols {
    include!(concat!(env!("OUT_DIR"), "/symbols.rs"));
}

pub mod addresses {
    use super::_symbols;

    macro_rules! declare_symbols {
        ($($name:ident,)*) => {
            $(
                pub const $name: u16 = _symbols::$name;
            )*
        };
    }

    declare_symbols!(
        DRIVER_CODE,
        LOADER,
        SONG_PTR,
        LOADER_DATA_TYPE,
        EON_SHADOW_MUSIC,
        EON_SHADOW_SFX,
        SONG_TICK_COUNTER,
        IO_MUSIC_CHANNELS_MASK,
        CHANNEL_VC_VOL_L,
        CHANNEL_VC_VOL_R,
        CHANNEL_VC_PITCH_L,
        CHANNEL_VC_PITCH_H,
        CHANNEL_VC_SCRN,
        CHANNEL_VC_ADSR1,
        CHANNEL_VC_ADSR2_OR_GAIN,
        CHANNEL_COUNTDOWN_TIMER,
        CHANNEL_INST_PITCH_OFFSET,
        CHANNEL_INSTRUCTION_PTR_L,
        CHANNEL_INSTRUCTION_PTR_H,
        CHANNEL_LOOP_STATE_COUNTER,
        CHANNEL_LOOP_STATE_LOOP_POINT_L,
        CHANNEL_LOOP_STATE_LOOP_POINT_H,
        CHANNEL_NEXT_EVENT_IS_KEY_OFF,
        CHANNEL_PAN,
        CHANNEL_DIRECTION,
        CHANNEL_RETURN_INST_PTR_L,
        CHANNEL_RETURN_INST_PTR_H,
        CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK,
        CHANNEL_VIBRATO_DIRECTION_COMPARATOR,
        CHANNEL_VIBRATO_TICK_COUNTER,
        CHANNEL_VIBRATO_TICK_COUNTER_START,
        CHANNEL_VIBRATO_WAVELENGTH_IN_TICKS,
        CHANNEL_VOLUME,
    );

    // MUST match `audio-driver/src/common_memmap.wiz`
    pub const COMMON_DATA: u16 = 0x900 - 4;

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

pub const TAD_IO_VERSION: usize = _symbols::TAD_IO_VERSION;

pub const N_MUSIC_CHANNELS: usize = 8;
pub const N_SFX_CHANNELS: usize = 2;
pub const N_CHANNELS: usize = N_MUSIC_CHANNELS + N_SFX_CHANNELS;

pub const FIRST_SFX_CHANNEL: usize = N_MUSIC_CHANNELS;

pub const N_DSP_VOICES: usize = 8;

pub const N_NESTED_LOOPS: usize = 3;

pub const MAX_PAN: u8 = 128;
pub const CENTER_PAN: u8 = MAX_PAN / 2;

// Song ID 0 is silence
pub const FIRST_SONG_ID: usize = 1;
pub const MAX_N_SONGS: usize = 0x100 - FIRST_SONG_ID;

pub const MAX_DIR_ITEMS: usize = u8::MAX as usize;
pub const MAX_INSTRUMENTS_AND_SAMPLES: usize = u8::MAX as usize;
pub const MAX_SOUND_EFFECTS: usize = 254;

pub const PITCH_TABLE_SIZE: usize = 256;

pub const COMMON_DATA_N_DIR_ITEMS_OFFSET: usize = 0;
pub const COMMON_DATA_N_INSTRUMENTS_OFFSET: usize = 1;
pub const COMMON_DATA_N_SOUND_EFFECTS_OFFSET: usize = 2;
pub const COMMON_DATA_HEADER_SIZE: usize = 4 + (2 * PITCH_TABLE_SIZE);

pub const COMMON_DATA_PITCH_TABLE_OFFSET: usize = 4;
pub const COMMON_DATA_DIR_TABLE_OFFSET: usize = COMMON_DATA_HEADER_SIZE;

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

// S-SMP registers
pub const S_SMP_TIMER_0_REGISTER: u8 = 0xfa;

// S-DSP constants

pub const S_DSP_EON_REGISTER: u8 = 0x4d;

pub const FIR_FILTER_SIZE: usize = 8;
pub const IDENTITY_FILTER: [i8; FIR_FILTER_SIZE] = [127, 0, 0, 0, 0, 0, 0, 0];

pub const ECHO_BUFFER_EDL_MS: u32 = 16;
pub const ECHO_BUFFER_EDL_SIZE: usize = 2048;
pub const ECHO_BUFFER_MIN_SIZE: usize = 256;
pub const ECHO_BUFFER_MAX_EDL: u8 = 15;

// Song constants
pub const MAX_SUBROUTINES: usize = 255;

pub const SONG_HEADER_SIZE: usize = N_MUSIC_CHANNELS * 4 + 13;
pub const SONG_HEADER_CHANNELS_SIZE: usize = N_MUSIC_CHANNELS * 4;
pub const SONG_HEADER_TICK_TIMER_OFFSET: usize = SONG_HEADER_SIZE - 2;
pub const SONG_HEADER_N_SUBROUTINES_OFFSET: usize = SONG_HEADER_SIZE - 1;

pub const MAX_SONG_DATA_SIZE: usize = 0xD000;

pub const STARTING_VOLUME: u8 = 96;

// Sound effect constants
pub const SFX_TICK_CLOCK: u8 = 64;

// IO Commands
pub mod io_commands {
    pub const PAUSE: u8 = 0;
    pub const UNPAUSE: u8 = 4;
    pub const PLAY_SOUND_EFFECT: u8 = 6;
    pub const STOP_SOUND_EFFECTS: u8 = 8;
}

pub const IO_COMMAND_MASK: u8 = 0b00001110;
pub const IO_COMMAND_I_MASK: u8 = 0b11100001;
