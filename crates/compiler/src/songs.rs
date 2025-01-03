//! Song compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::bytecode::{
    BcTerminator, BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, InstrumentId, PlayNoteTicks,
    StackDepth, SubroutineId, Volume,
};
use crate::channel_bc_generator::MmlInstrument;
use crate::data::{self, single_item_unique_names_list, InstrumentOrSample, Name, UniqueNamesList};
use crate::driver_constants::{
    addresses, AUDIO_RAM_SIZE, ECHO_BUFFER_MIN_SIZE, ECHO_VARIABLES_SIZE, MAX_SONG_DATA_SIZE,
    MAX_SUBROUTINES, N_MUSIC_CHANNELS, SFX_TICK_CLOCK, SONG_HEADER_CHANNELS_SIZE,
    SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, SONG_HEADER_TICK_TIMER_OFFSET,
};
use crate::envelope::{Envelope, Gain};
use crate::errors::{ChannelError, SongError, SongTooLargeError};
use crate::mml::{MetaData, Section};
use crate::notes::{Note, Octave};
use crate::sound_effects::CompiledSoundEffect;
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::{audio_driver, mml};

use std::cmp::min;
use std::fmt::Debug;
use std::ops::Range;
use std::sync::OnceLock;
use std::time::Duration;

const NULL_OFFSET: u16 = 0xffff_u16;

#[derive(Debug, PartialEq)]
pub struct SongAramSize {
    pub data_size: u16,
    pub echo_buffer_size: u16,
}

impl SongAramSize {
    pub fn total_size(&self) -> usize {
        usize::from(self.data_size) + usize::from(self.echo_buffer_size)
    }
}

pub const BLANK_SONG_ARAM_SIZE: SongAramSize = SongAramSize {
    data_size: audio_driver::BLANK_SONG.len() as u16,
    echo_buffer_size: ECHO_BUFFER_MIN_SIZE as u16,
};

#[cfg(feature = "mml_tracking")]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BytecodePos {
    // Position (within song data) at the end of the bytecode instruction.
    pub bc_end_pos: u16,
    // Character index within the input file
    pub char_index: u32,
}

#[cfg(feature = "mml_tracking")]
#[derive(Clone)]
pub struct SongBcTracking {
    pub bytecode: Vec<BytecodePos>,
    pub cursor_tracker: mml::note_tracking::CursorTracker,

    /// Used to determine if a bytecode offset is in a subroutine or not.
    /// `bc_offset` is in a subroutine if `bc_offset < firt_channel_bc_offset`.
    pub first_channel_bc_offset: u16,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine {
    pub identifier: mml::IdentifierBuf,
    pub subroutine_id: SubroutineId,
    pub bytecode_offset: u16,
    pub changes_song_tempo: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopPoint {
    pub bytecode_offset: usize,
    pub tick_counter: TickCounter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Channel {
    pub name: char,

    pub bytecode_offset: u16,
    pub loop_point: Option<LoopPoint>,
    pub tick_counter: TickCounter,

    pub max_stack_depth: StackDepth,

    pub section_tick_counters: Vec<TickCounterWithLoopFlag>,
    pub tempo_changes: Vec<(TickCounter, TickClock)>,
}

#[derive(Clone)]
pub struct SongData {
    metadata: MetaData,
    data: Vec<u8>,
    duration: Option<Duration>,

    sections: Vec<Section>,
    instruments: Vec<MmlInstrument>,
    channels: [Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: Vec<Subroutine>,

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

    pub fn max_tick_count(&self) -> TickCounter {
        self.channels
            .iter()
            .flatten()
            .map(|c| c.tick_counter)
            .max()
            .unwrap_or(TickCounter::new(0))
    }

    pub fn is_looping(&self) -> bool {
        self.channels
            .iter()
            .flatten()
            .any(|c| c.loop_point.is_some())
    }

    pub fn sections(&self) -> &[Section] {
        &self.sections
    }

    pub fn instruments(&self) -> &[MmlInstrument] {
        &self.instruments
    }

    pub fn channels(&self) -> &[Option<Channel>; N_MUSIC_CHANNELS] {
        &self.channels
    }

    pub fn subroutines(&self) -> &[Subroutine] {
        &self.subroutines
    }

    pub fn song_aram_size(&self) -> SongAramSize {
        let data_size = self.data().len();
        // Loader can only load a multiple of 2 bytes
        let data_size = data_size + (data_size % 2);

        SongAramSize {
            data_size: data_size.try_into().unwrap_or(u16::MAX),
            echo_buffer_size: self.metadata.echo_buffer.edl.buffer_size_u16(),
        }
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

pub fn song_header_size(n_subroutines: usize) -> usize {
    SONG_HEADER_SIZE + n_subroutines * 2
}

fn sample_song_fake_instruments() -> &'static UniqueNamesList<InstrumentOrSample> {
    static LOCK: OnceLock<UniqueNamesList<InstrumentOrSample>> = OnceLock::new();

    LOCK.get_or_init(|| {
        let inst = InstrumentOrSample::Instrument(data::Instrument {
            name: Name::try_new("name".to_owned()).unwrap(),
            source: Default::default(),
            freq: 0.0,
            loop_setting: data::LoopSetting::None,
            evaluator: Default::default(),
            ignore_gaussian_overflow: false,
            first_octave: Octave::try_new(Octave::MIN.as_u8().into()).unwrap(),
            last_octave: Octave::try_new(Octave::MAX.as_u8().into()).unwrap(),
            envelope: Envelope::Gain(Gain::new(0)),
            comment: Default::default(),
        });
        single_item_unique_names_list(inst)
    })
}

pub fn test_sample_song(
    instrument: u8,
    note: Note,
    note_length: u32,
    envelope: Option<Envelope>,
) -> Result<SongData, ChannelError> {
    let mut bc = Bytecode::new(
        crate::bytecode::BytecodeContext::SongChannel(0),
        sample_song_fake_instruments(),
        None,
    );

    let inst = InstrumentId::try_from(instrument)?;

    bc.set_volume(Volume::new(255));

    match envelope {
        None => bc.set_instrument(inst),
        Some(Envelope::Adsr(adsr)) => bc.set_instrument_and_adsr(inst, adsr),
        Some(Envelope::Gain(gain)) => bc.set_instrument_and_gain(inst, gain),
    };

    let mut remaining_length = min(2000, note_length);
    while remaining_length > BcTicksKeyOff::MAX_TICKS {
        let nl = min(remaining_length, BcTicksNoKeyOff::MAX_TICKS);
        remaining_length -= nl;

        let nl = BcTicksNoKeyOff::try_from(nl)?;
        bc.play_note(note, PlayNoteTicks::NoKeyOff(nl))?;
    }

    let nl = BcTicksKeyOff::try_from(remaining_length)?;
    bc.play_note(note, PlayNoteTicks::KeyOff(nl))?;

    let bytecode = bc.bytecode(BcTerminator::DisableChannel).unwrap().0;

    Ok(sfx_bytecode_to_song(&bytecode))
}

pub fn blank_song() -> SongData {
    let mut data = vec![0; SONG_HEADER_SIZE];
    let channels = &mut data[0..SONG_HEADER_CHANNELS_SIZE];
    channels.fill(0xff); // Disable all channels

    SongData {
        data,
        metadata: MetaData::blank_sfx_metadata(),
        duration: None,
        sections: Vec::new(),
        instruments: Vec::new(),
        channels: Default::default(),
        subroutines: Vec::new(),

        #[cfg(feature = "mml_tracking")]
        tracking: None,
    }
}

pub fn sound_effect_to_song(sfx: &CompiledSoundEffect) -> SongData {
    sfx_bytecode_to_song(sfx.bytecode())
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
        sections: Vec::new(),
        instruments: Vec::new(),
        channels: Default::default(),
        subroutines: Vec::new(),

        #[cfg(feature = "mml_tracking")]
        tracking: None,
    }
}

fn write_song_header(
    buf: &mut [u8],
    channels: &[Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: &[Subroutine],
    metadata: &MetaData,
) -> Result<(), SongError> {
    if buf.len() > MAX_SONG_DATA_SIZE {
        return Err(SongError::SongIsTooLarge(buf.len()));
    }

    assert!(channels.len() <= N_MUSIC_CHANNELS);
    assert!(subroutines.len() <= MAX_SUBROUTINES);

    let header_size = song_header_size(subroutines.len());

    const _: () = assert!(MAX_SONG_DATA_SIZE < u16::MAX as usize);
    let valid_offsets = Range {
        start: u16::try_from(header_size).unwrap(),
        end: u16::try_from(buf.len()).unwrap(),
    };

    let header = &mut buf[0..header_size];
    debug_assert!(header.iter().all(|&i| i == 0));

    // Channel data
    {
        let channel_header = &mut header[0..SONG_HEADER_CHANNELS_SIZE];

        for (i, c) in channels.iter().enumerate() {
            let start_offset = match c {
                Some(c) => {
                    let offset = c.bytecode_offset;
                    assert!(valid_offsets.contains(&offset));
                    offset
                }
                None => NULL_OFFSET,
            };

            let o = i * 2;
            channel_header[o..o + 2].copy_from_slice(&start_offset.to_le_bytes());
        }
    }

    // Echo buffer settings
    const EBS: usize = SONG_HEADER_CHANNELS_SIZE;
    let echo_buffer = &metadata.echo_buffer;

    header[EBS] = echo_buffer.edl.as_u8();
    for (i, f) in echo_buffer.fir.iter().enumerate() {
        header[EBS + 1 + i] = f.as_i8().to_le_bytes()[0];
    }
    header[EBS + 9] = echo_buffer.feedback.as_i8().to_le_bytes()[0];
    header[EBS + 10] = echo_buffer.echo_volume_l.as_u8();
    header[EBS + 11] = echo_buffer.echo_volume_r.as_u8();
    header[EBS + 12] = echo_buffer.invert.into_driver_value();

    const _: () = assert!(13 == ECHO_VARIABLES_SIZE);
    const _: () = assert!(EBS + ECHO_VARIABLES_SIZE == SONG_HEADER_TICK_TIMER_OFFSET);

    // Tick timer
    header[SONG_HEADER_TICK_TIMER_OFFSET] = metadata.tick_clock.as_u8();

    // Subroutine table
    {
        let n_subroutines = subroutines.len();
        assert!(n_subroutines <= u8::MAX.into());

        header[SONG_HEADER_N_SUBROUTINES_OFFSET] = n_subroutines.try_into().unwrap();
        let subroutine_table = &mut header[SONG_HEADER_SIZE..];

        assert_eq!(subroutine_table.len(), n_subroutines * 2);
        for (i, s) in subroutines.iter().enumerate() {
            let offset = s.bytecode_offset;
            assert!(valid_offsets.contains(&offset));

            let offset = offset.to_le_bytes();
            subroutine_table[i] = offset[0];
            subroutine_table[i + n_subroutines] = offset[1];
        }
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn mml_to_song(
    metadata: MetaData,
    data: Vec<u8>,
    duration: Option<Duration>,
    sections: Vec<Section>,
    instruments: Vec<MmlInstrument>,
    channels: [Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: Vec<Subroutine>,
    #[cfg(feature = "mml_tracking")] tracking: SongBcTracking,
) -> Result<SongData, SongError> {
    let mut data = data;

    match write_song_header(&mut data, &channels, &subroutines, &metadata) {
        Ok(()) => Ok(SongData {
            metadata,
            data,
            duration,
            sections,
            instruments,
            channels,
            subroutines,
            #[cfg(feature = "mml_tracking")]
            tracking: Some(tracking),
        }),
        Err(e) => Err(e),
    }
}

pub fn song_duration_string(duration: Option<Duration>) -> String {
    match duration {
        Some(d) => {
            // always round up
            let ms = d.as_millis() + 999;
            let minutes = ms / 60_000;
            let seconds = (ms / 1_000) % 60;

            format!("{}:{:02}", minutes, seconds)
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
