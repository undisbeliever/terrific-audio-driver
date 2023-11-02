//! Song compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::bytecode::{
    BcTerminator, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, InstrumentId, PlayNoteTicks, Volume,
};
use crate::driver_constants::{
    addresses, AUDIO_RAM_SIZE, MAX_SONG_DATA_SIZE, MAX_SUBROUTINES, N_MUSIC_CHANNELS,
    SFX_TICK_CLOCK, SONG_HEADER_CHANNELS_SIZE, SONG_HEADER_SIZE, SONG_HEADER_TICK_TIMER_OFFSET,
};
use crate::envelope::Envelope;
use crate::errors::{SongError, SongTooLargeError, ValueError};
use crate::mml::{calc_song_duration, MetaData, MmlData};
use crate::notes::Note;
use crate::sound_effects::CompiledSoundEffect;

#[cfg(feature = "mml_tracking")]
use crate::mml;

use std::cmp::min;
use std::fmt::Debug;
use std::time::Duration;

const NULL_OFFSET: u16 = 0xffff_u16;

fn validate_data_size(data: &[u8], expected_size: usize) -> Result<(), SongError> {
    if data.len() == expected_size {
        Ok(())
    } else {
        Err(SongError::DataSizeMismatch(data.len(), expected_size))
    }
}

#[cfg(feature = "mml_tracking")]
#[derive(Clone)]
pub struct ChannelBcTracking {
    pub range: std::ops::Range<u16>,
    pub bytecodes: Vec<mml::BytecodePos>,
}

#[cfg(feature = "mml_tracking")]
impl ChannelBcTracking {
    fn new(start: usize, end: usize, bytecodes: Vec<mml::BytecodePos>) -> Self {
        Self {
            range: std::ops::Range {
                start: start.try_into().unwrap(),
                end: end.try_into().unwrap(),
            },
            bytecodes,
        }
    }
}

#[cfg(feature = "mml_tracking")]
#[derive(Clone)]
pub struct SongBcTracking {
    pub subroutines: Vec<ChannelBcTracking>,
    pub channels: [Option<ChannelBcTracking>; N_MUSIC_CHANNELS],
}

#[derive(Clone)]
pub struct SongData {
    metadata: MetaData,
    data: Vec<u8>,
    duration: Option<Duration>,

    #[cfg(feature = "mml_tracking")]
    tracking: Option<SongBcTracking>,
}

impl Debug for SongData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SongData")
    }
}

impl SongData {
    pub fn metadata(&self) -> &MetaData {
        &self.metadata
    }
    pub fn data(&self) -> &[u8] {
        &self.data
    }
    pub fn duration(&self) -> Option<Duration> {
        self.duration
    }

    pub fn data_and_echo_size(&self) -> usize {
        let song_data_size = self.data().len();
        // Loader can only a multiple of 2 bytes
        let song_data_size = song_data_size + (song_data_size % 2);

        song_data_size + self.metadata.echo_buffer.edl.buffer_size()
    }

    #[cfg(feature = "mml_tracking")]
    pub fn tracking(&self) -> Option<&SongBcTracking> {
        self.tracking.as_ref()
    }

    #[cfg(feature = "mml_tracking")]
    pub fn take_tracking(self) -> Option<SongBcTracking> {
        self.tracking
    }
}

pub fn test_sample_song(
    instrument: u8,
    note: Note,
    note_length: u32,
    envelope: Option<Envelope>,
) -> Result<SongData, ValueError> {
    let mut bc = Bytecode::new(false, true);

    let inst = InstrumentId::try_from(instrument)?;

    bc.set_volume(Volume::new(255));

    match envelope {
        None => bc.set_instrument(inst),
        Some(Envelope::Adsr(adsr)) => bc.set_instrument_and_adsr(inst, adsr),
        Some(Envelope::Gain(gain)) => bc.set_instrument_and_gain(inst, gain),
    };

    let mut remaining_length = min(2000, note_length);
    while remaining_length > BcTicksKeyOff::MAX {
        let nl = min(remaining_length, BcTicksNoKeyOff::MAX);
        remaining_length -= nl;

        let nl = BcTicksNoKeyOff::try_from(nl)?;
        bc.play_note(note, PlayNoteTicks::NoKeyOff(nl));
    }

    let nl = BcTicksKeyOff::try_from(remaining_length)?;
    bc.play_note(note, PlayNoteTicks::KeyOff(nl));

    let bytecode = bc.bytecode(BcTerminator::DisableChannel).unwrap();

    Ok(sfx_bytecode_to_song(&bytecode))
}

pub fn sound_effect_to_song(sfx: &CompiledSoundEffect) -> SongData {
    sfx_bytecode_to_song(sfx.data())
}

fn sfx_bytecode_to_song(bytecode: &[u8]) -> SongData {
    const SONG_DATA_OFFSET: u16 = SONG_HEADER_SIZE as u16;

    assert!(!bytecode.is_empty());

    let header_size = SONG_HEADER_SIZE;
    let total_size = header_size + bytecode.len();

    assert!(total_size < MAX_SONG_DATA_SIZE);

    let mut header = [0; SONG_HEADER_SIZE];
    let channels = &mut header[0..SONG_HEADER_CHANNELS_SIZE];
    channels.fill(0xff); // Disable all channels
    channels[0..2].copy_from_slice(&SONG_DATA_OFFSET.to_le_bytes());

    header[SONG_HEADER_TICK_TIMER_OFFSET] = SFX_TICK_CLOCK;

    SongData {
        data: [header.as_slice(), bytecode].concat(),
        metadata: MetaData::blank_sfx_metadata(),
        duration: None,

        #[cfg(feature = "mml_tracking")]
        tracking: None,
    }
}

pub fn song_data(mml_data: MmlData) -> Result<SongData, SongError> {
    let metadata = mml_data.metadata();
    let channels = mml_data.channels();
    let subroutines = mml_data.subroutines();

    let echo_buffer = &metadata.echo_buffer;

    if channels.is_empty() {
        return Err(SongError::NoMusicChannels);
    }

    assert!(channels.len() <= N_MUSIC_CHANNELS);
    assert!(subroutines.len() <= MAX_SUBROUTINES);

    let channel_data_size: usize = channels.iter().map(|c| c.bytecode().len()).sum();
    let subroutine_data_size: usize = subroutines.iter().map(|s| s.bytecode().len()).sum();

    let header_size = SONG_HEADER_SIZE + subroutines.len() * 2;
    let total_size = header_size + channel_data_size + subroutine_data_size;

    if total_size > MAX_SONG_DATA_SIZE {
        return Err(SongError::SongIsTooLarge(total_size));
    }
    const _: () = assert!(MAX_SONG_DATA_SIZE < u16::MAX as usize);

    let mut out = Vec::with_capacity(header_size);
    {
        let mut data_offset: u16 = header_size.try_into().unwrap();

        // Channel data
        for i in 0..N_MUSIC_CHANNELS {
            let (start_offset, loop_offset) = match channels.get(i) {
                Some(c) => {
                    let c_size: u16 = c.bytecode().len().try_into().unwrap();

                    let loop_offset: u16 = match c.loop_point() {
                        Some(lp) => {
                            if lp.bytecode_offset >= c_size.into() {
                                return Err(SongError::InvalidMmlData);
                            }
                            u16::try_from(lp.bytecode_offset).unwrap()
                        }
                        None => NULL_OFFSET,
                    };

                    let offset = data_offset;
                    data_offset += c_size;

                    (offset, loop_offset)
                }
                None => (NULL_OFFSET, NULL_OFFSET),
            };

            out.extend(start_offset.to_le_bytes());
            out.extend(loop_offset.to_le_bytes());
        }

        // Echo buffer settings
        out.push(echo_buffer.edl.as_u8());
        for f in echo_buffer.fir {
            out.extend(f.to_le_bytes());
        }
        out.extend(echo_buffer.feedback.to_le_bytes());
        out.extend(echo_buffer.echo_volume.to_le_bytes());

        // Tick timer
        out.push(metadata.tick_clock.as_u8());

        // Subroutine table
        out.push(subroutines.len().try_into().unwrap());

        for s in subroutines {
            out.extend(data_offset.to_le_bytes());

            let s_size: u16 = s.bytecode().len().try_into().unwrap();
            data_offset += s_size;
        }

        validate_data_size(&out, header_size)?;
    }

    #[cfg(feature = "mml_tracking")]
    let mut channel_tracking: [Option<ChannelBcTracking>; N_MUSIC_CHANNELS] = Default::default();

    for (_i, c) in channels.iter().enumerate() {
        #[cfg(feature = "mml_tracking")]
        let start = out.len();

        out.extend(c.bytecode());

        #[cfg(feature = "mml_tracking")]
        {
            let end = out.len();
            channel_tracking[_i] = Some(ChannelBcTracking::new(start, end, c.bc_tracking.clone()));
        }
    }

    #[cfg(feature = "mml_tracking")]
    let mut subroutine_tracking = Vec::with_capacity(subroutines.len());

    for s in subroutines {
        #[cfg(feature = "mml_tracking")]
        let start = out.len();

        out.extend(s.bytecode());

        #[cfg(feature = "mml_tracking")]
        {
            let end = out.len();
            subroutine_tracking.push(ChannelBcTracking::new(start, end, s.bc_tracking.clone()));
        }
    }

    validate_data_size(&out, total_size)?;

    Ok(SongData {
        duration: calc_song_duration(&mml_data),
        data: out,
        metadata: mml_data.take_metadata(),

        #[cfg(feature = "mml_tracking")]
        tracking: Some(SongBcTracking {
            subroutines: subroutine_tracking,
            channels: channel_tracking,
        }),
    })
}

pub fn song_duration_string(duration: Option<Duration>) -> String {
    match duration {
        Some(d) => {
            // always round up
            let ms = d.as_millis() + 999;
            let minutes = ms / 60_000;
            let seconds = (ms / 1_000) % 60;

            format!("{}:{}", minutes, seconds)
        }
        None => "unknown".to_owned(),
    }
}

pub fn validate_song_size(
    song: &SongData,
    common_data_size: usize,
) -> Result<(), SongTooLargeError> {
    let song_data_size = song.data().len();
    let echo_buffer_size = song.metadata().echo_buffer.edl.buffer_size();

    // Loader can only transfer data that is a multiple of 2 bytes
    let common_data_size = common_data_size + (common_data_size % 2);
    let song_data_size = song_data_size + (song_data_size % 2);

    let total_size = common_data_size + song_data_size + echo_buffer_size;

    let end_addr = usize::from(addresses::COMMON_DATA) + total_size;

    if end_addr <= AUDIO_RAM_SIZE {
        Ok(())
    } else {
        let too_large_by = end_addr - AUDIO_RAM_SIZE;
        Err(SongTooLargeError {
            too_large_by,
            common_data_size,
            song_data_size,
            echo_buffer_size,
        })
    }
}
