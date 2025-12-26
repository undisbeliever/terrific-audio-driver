//! Song compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::bytecode::{
    BcTerminator, Bytecode, BytecodeContext, InstrumentId, PlayNoteTicks, StackDepth, Volume,
};
use crate::command_compiler::analysis::TransposeStartRange;
use crate::command_compiler::channel_bc_generator::CommandCompiler;
use crate::command_compiler::commands::MmlInstrument;
use crate::command_compiler::subroutines::subroutine_compile_order;
use crate::data::{self, single_item_unique_names_list, InstrumentOrSample, Name, UniqueNamesList};
use crate::driver_constants::{
    addresses, AUDIO_RAM_SIZE, BLANK_SONG_BIN, ECHO_BUFFER_MIN_SIZE, ECHO_VARIABLES_SIZE,
    MAX_SONG_DATA_SIZE, MAX_SUBROUTINES, N_MUSIC_CHANNELS, SFX_TICK_CLOCK,
    SONG_HEADER_ACTIVE_MUSIC_CHANNELS, SONG_HEADER_ECHO, SONG_HEADER_ECHO_EDL,
    SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, SONG_HEADER_TICK_TIMER_OFFSET,
};
use crate::echo::{EchoBuffer, EchoEdl};
use crate::envelope::{Envelope, Gain};
use crate::errors::{ChannelError, MmlCompileErrors, SongError, SongTooLargeError};
use crate::identifier::{ChannelId, MusicChannelIndex};
use crate::mml::{CommandTickTracker, CursorTracker, CursorTrackerGetter, GlobalSettings, Section};
use crate::notes::Note;
use crate::pitch_table::PitchTable;
use crate::samples::SampleAndInstrumentData;
use crate::subroutines::{BlankSubroutineMap, CompiledSubroutines, SubroutineState};
use crate::time::{TickClock, TickCounter, TIMER_HZ};
use crate::{command_compiler, mml, UnsignedValueNewType};

use std::fmt::Debug;
use std::ops::Range;
use std::sync::OnceLock;
use std::time::Duration;

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
    data_size: BLANK_SONG_BIN.len() as u16,
    echo_buffer_size: ECHO_BUFFER_MIN_SIZE as u16,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BytecodePos {
    // Position (within song data) at the end of the bytecode instruction.
    pub bc_end_pos: u16,
    // Character index within the input file
    pub char_index: u32,
}

#[derive(Clone)]
pub struct SongBcTracking {
    pub bytecode: Vec<BytecodePos>,
    pub cursor_tracker: CursorTracker,

    /// Used to determine if a bytecode offset is in a subroutine or not.
    /// `bc_offset` is in a subroutine if `bc_offset < firt_channel_bc_offset`.
    pub first_channel_bc_offset: u16,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopPoint {
    pub bytecode_offset: usize,
    pub tick_counter: TickCounter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Channel {
    pub channel_index: MusicChannelIndex,

    pub bytecode_offset: u16,
    pub loop_point: Option<LoopPoint>,
    pub tick_counter: TickCounter,

    pub max_stack_depth: StackDepth,

    pub tick_tracker: CommandTickTracker,

    pub tempo_changes: Vec<(TickCounter, TickClock)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MetaData {
    pub title: Option<String>,
    pub game: Option<String>,
    pub date: Option<String>,
    pub composer: Option<String>,
    pub author: Option<String>,
    pub copyright: Option<String>,
    pub license: Option<String>,

    pub echo_buffer: EchoBuffer,

    pub tick_clock: TickClock,

    pub mml_settings: GlobalSettings,

    /// SPC export song length in seconds before fading out
    /// (override calculated song duration)
    pub spc_song_length: Option<u32>,

    /// SPC export fadeout length in milliseconds
    pub spc_fadeout_millis: Option<u32>,
}

#[derive(Clone)]
pub struct SongData {
    name: String,

    metadata: MetaData,
    data: Vec<u8>,
    duration: Option<Duration>,

    sections: Vec<Section>,
    instruments: Vec<MmlInstrument>,
    channels: [Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: CompiledSubroutines,

    subroutine_table_l_addr: u16,

    tracking: Option<SongBcTracking>,
}

impl Debug for SongData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SongData")
    }
}

impl SongData {
    pub fn name(&self) -> &str {
        &self.name
    }
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

    pub fn subroutines(&self) -> &CompiledSubroutines {
        &self.subroutines
    }

    pub fn song_aram_size(&self) -> SongAramSize {
        let data_size = self.data().len();
        // Loader can only load a multiple of 2 bytes
        let data_size = data_size + (data_size % 2);

        SongAramSize {
            data_size: data_size.try_into().unwrap_or(u16::MAX),
            echo_buffer_size: self.metadata.echo_buffer.buffer_size_u16(),
        }
    }

    pub(crate) fn subroutine_table_l_addr(&self) -> u16 {
        self.subroutine_table_l_addr
    }

    pub fn bc_tracking(&self) -> Option<&SongBcTracking> {
        self.tracking.as_ref()
    }
}

impl CursorTrackerGetter for SongData {
    fn cursor_tracker(&self) -> Option<&CursorTracker> {
        self.tracking.as_ref().map(|t| &t.cursor_tracker)
    }

    fn tick_tracker_for_channel(&self, channel: ChannelId) -> Option<&CommandTickTracker> {
        match channel {
            ChannelId::Channel(c) => match self.channels.get(usize::from(c)) {
                Some(Some(c)) => Some(&c.tick_tracker),
                _ => None,
            },
            ChannelId::Subroutine(i) => match self.subroutines.get_compiled(i) {
                Some(s) => Some(&s.tick_tracker),
                _ => None,
            },

            ChannelId::MmlPrefix => None,
            ChannelId::SoundEffect => None,
        }
    }
}

pub fn song_header_size(n_active_channels: usize, n_subroutines: usize) -> usize {
    assert!(n_active_channels <= N_MUSIC_CHANNELS);

    SONG_HEADER_SIZE + n_active_channels * 2 + n_subroutines * 2
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
            note_range: data::InstrumentNoteRange::Note {
                first: Note::MIN,
                last: Note::MAX,
            },
            envelope: Envelope::Gain(Gain::new(0)),
            comment: Default::default(),
        });
        single_item_unique_names_list(inst)
    })
}

pub fn test_sample_song(
    instrument: u8,
    note: Note,
    note_length: u16,
    envelope: Option<Envelope>,
    sample_data: &SampleAndInstrumentData,
) -> Result<SongData, ChannelError> {
    let subroutines = CompiledSubroutines::new_blank();

    let instruments = sample_song_fake_instruments();

    let mut bc = Bytecode::new(
        BytecodeContext::SongChannel {
            index: MusicChannelIndex::CHANNEL_A,
            max_edl: EchoEdl::MIN,
        },
        instruments,
        sample_data.pitch_table(),
        &subroutines,
        &BlankSubroutineMap,
        TransposeStartRange::DISABLED,
    );

    let inst = InstrumentId::try_from(instrument)?;

    bc.set_volume(Volume::new(255));

    match envelope {
        None => bc.set_instrument(inst),
        Some(Envelope::Adsr(adsr)) => bc.set_instrument_and_adsr(inst, adsr),
        Some(Envelope::Gain(gain)) => bc.set_instrument_and_gain(inst, gain),
    };

    bc.play_note(note, PlayNoteTicks::KeyOff(note_length.try_into()?))?;

    let bytecode = bc.bytecode(BcTerminator::DisableChannel).unwrap().0;

    Ok(sfx_bytecode_to_song(&bytecode))
}

pub fn blank_song() -> SongData {
    SongData {
        name: String::new(),
        data: vec![0; 1],
        metadata: MetaData::blank_sfx_metadata(),
        duration: None,
        sections: Vec::new(),
        instruments: Vec::new(),
        channels: Default::default(),
        subroutines: CompiledSubroutines::new_blank(),

        subroutine_table_l_addr: u16::MAX,

        tracking: None,
    }
}

fn sfx_bytecode_to_song(bytecode: &[u8]) -> SongData {
    const HEADER_SIZE: usize = SONG_HEADER_SIZE + 2;
    const SONG_DATA_OFFSET: u16 = HEADER_SIZE as u16;

    assert!(!bytecode.is_empty());

    let total_size = HEADER_SIZE + bytecode.len();

    assert!(total_size < MAX_SONG_DATA_SIZE);

    let mut header = [0; HEADER_SIZE];

    header[SONG_HEADER_ACTIVE_MUSIC_CHANNELS] = 1;
    header[SONG_HEADER_TICK_TIMER_OFFSET] = SFX_TICK_CLOCK;
    header[HEADER_SIZE - 2..].copy_from_slice(&SONG_DATA_OFFSET.to_le_bytes());

    SongData {
        name: String::new(),
        data: [header.as_slice(), bytecode].concat(),
        metadata: MetaData::blank_sfx_metadata(),
        duration: None,
        sections: Vec::new(),
        instruments: Vec::new(),
        channels: Default::default(),
        subroutines: CompiledSubroutines::new_blank(),

        subroutine_table_l_addr: u16::MAX,

        tracking: None,
    }
}

fn write_song_header(
    buf: &mut [u8],
    channels: &[Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: &CompiledSubroutines,
    metadata: &MetaData,
) -> Result<u16, SongError> {
    if buf.len() > MAX_SONG_DATA_SIZE {
        return Err(SongError::SongIsTooLarge(buf.len()));
    }

    assert!(channels.len() <= N_MUSIC_CHANNELS);
    assert!(subroutines.len() <= MAX_SUBROUTINES);

    let n_active_channels = channels.iter().flatten().count();

    let n_subroutines = subroutines.len();
    assert!(n_subroutines <= u8::MAX.into());

    let subroutine_table_addr = SONG_HEADER_SIZE + n_active_channels * 2;

    let header_size = song_header_size(n_active_channels, n_subroutines);

    const _: () = assert!(MAX_SONG_DATA_SIZE < u16::MAX as usize);
    let valid_offsets = Range {
        start: u16::try_from(header_size).unwrap(),
        end: u16::try_from(buf.len()).unwrap(),
    };

    let header = &mut buf[0..header_size];
    debug_assert!(header.iter().all(|&i| i == 0));

    let active_music_channels = channels
        .iter()
        .enumerate()
        .fold(0, |acc, (i, c)| acc | (u8::from(c.is_some()) << i));

    assert_eq!(
        active_music_channels.count_ones(),
        u32::try_from(n_active_channels).unwrap()
    );
    header[SONG_HEADER_ACTIVE_MUSIC_CHANNELS] = active_music_channels;

    // Echo buffer settings
    const EBS: usize = SONG_HEADER_ECHO;
    let echo_buffer = &metadata.echo_buffer;

    const _: () = assert!(EBS == SONG_HEADER_ECHO_EDL);
    header[EBS] = (echo_buffer.max_edl.as_u8() << 4) | echo_buffer.edl.as_u8();
    for (i, f) in echo_buffer.fir.iter().enumerate() {
        header[EBS + 1 + i] = f.as_i8().to_le_bytes()[0];
    }
    header[EBS + 9] = echo_buffer.feedback.as_i8().to_le_bytes()[0];
    header[EBS + 10] = echo_buffer.echo_volume_l.as_u8();
    header[EBS + 11] = echo_buffer.echo_volume_r.as_u8();
    header[EBS + 12] = echo_buffer.invert.into_driver_value();

    const _: () = assert!(13 == ECHO_VARIABLES_SIZE);
    const _: () = assert!(EBS + ECHO_VARIABLES_SIZE == SONG_HEADER_TICK_TIMER_OFFSET);

    header[SONG_HEADER_TICK_TIMER_OFFSET] = metadata.tick_clock.into_driver_value();
    header[SONG_HEADER_N_SUBROUTINES_OFFSET] = n_subroutines.try_into().unwrap();

    // Channel data
    {
        let mut i = SONG_HEADER_SIZE;

        for c in channels.iter().rev().flatten() {
            let offset = c.bytecode_offset;
            assert!(valid_offsets.contains(&offset));

            header[i..i + 2].copy_from_slice(&offset.to_le_bytes());
            i += 2;
        }

        assert_eq!(i, subroutine_table_addr);
    }

    // Subroutine table
    {
        let subroutine_table =
            &mut header[subroutine_table_addr..subroutine_table_addr + subroutines.len() * 2];

        for (i, (_, s)) in subroutines.iter().enumerate() {
            let offset = match s {
                SubroutineState::Compiled(s) => s.bytecode_offset,
                _ => panic!("error in subroutine"),
            };
            assert!(valid_offsets.contains(&offset));

            let offset = offset.to_le_bytes();
            subroutine_table[i] = offset[0];
            subroutine_table[i + n_subroutines] = offset[1];
        }
    }

    let subroutine_table_l_addr = u16::try_from(subroutine_table_addr).unwrap();

    Ok(subroutine_table_l_addr)
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
    let echo_buffer_size = song.metadata().echo_buffer.buffer_size();

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

pub fn override_song_tick_clock(song: &mut SongData, tick_clock: TickClock) {
    song.data[SONG_HEADER_TICK_TIMER_OFFSET] = tick_clock.into_driver_value();
    song.metadata.tick_clock = tick_clock;
}

fn compile_song_commands(
    song: crate::command_compiler::commands::SongCommands,
    pitch_table: &PitchTable,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    errors: MmlCompileErrors,
) -> Result<SongData, SongError> {
    let mut errors = errors;

    let n_active_channels = song.channels.iter().filter(|&c| !c.is_none()).count();
    let header_size = song_header_size(n_active_channels, song.subroutines.len());

    let mut subroutines = subroutine_compile_order(song.subroutines);

    let mut channels = song.channels;
    let a = command_compiler::analysis::analyse(&mut subroutines, Some(&mut channels));

    let mut compiler = CommandCompiler::new(
        header_size,
        pitch_table,
        data_instruments,
        song.metadata.echo_buffer.max_edl,
        true,
    );

    let subroutines = compiler.compile_subroutines(subroutines, &a, &mut errors.subroutine_errors);

    // ::TODO remove::
    if !errors.subroutine_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }

    let first_channel_bc_offset = compiler.bc_size();

    let channels = compiler.compile_song_channels(channels, &subroutines, &mut errors);

    let duration = calc_song_duration(&song.metadata, &channels, &subroutines);

    if errors.channel_errors.is_empty() && errors.subroutine_errors.is_empty() {
        let (mut data, bc_pos) = compiler.take_data();
        let tracking = Some(SongBcTracking {
            bytecode: bc_pos,
            cursor_tracker: song.mml_tracking,
            first_channel_bc_offset: first_channel_bc_offset.try_into().unwrap_or(u16::MAX),
        });

        match write_song_header(&mut data, &channels, &subroutines, &song.metadata) {
            Ok(subroutine_table_l_addr) => Ok(SongData {
                name: song.name,
                metadata: song.metadata,
                data,
                duration,
                sections: song.sections,
                instruments: song.instruments,
                channels,
                subroutines,
                subroutine_table_l_addr,
                tracking,
            }),
            Err(e) => Err(e),
        }
    } else {
        Err(SongError::MmlError(errors))
    }
}

fn calc_song_duration(
    metadata: &MetaData,
    channels: &[Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: &CompiledSubroutines,
) -> Option<Duration> {
    let set_song_tick_in_subroutine = subroutines.iter_compiled().any(|s| s.changes_song_tempo);

    if set_song_tick_in_subroutine {
        return None;
    }

    let total_ticks: u32 = channels
        .iter()
        .filter_map(|c| c.as_ref())
        .map(|c| c.tick_counter.value())
        .max()
        .unwrap_or(0);

    let mut tempo_changes: Vec<(TickCounter, TickClock)> = channels
        .iter()
        .filter_map(|c| c.as_ref())
        .flat_map(|c| &c.tempo_changes)
        .cloned()
        .collect();
    tempo_changes.sort_by_key(|(tc, _tempo)| tc.value());

    let mut out: u64 = 0;
    let mut prev_ticks = 0;
    let mut prev_clock = metadata.tick_clock;

    for (ticks, clock) in tempo_changes {
        let ticks = ticks.value();

        let section_ticks = ticks - prev_ticks;
        out += u64::from(section_ticks) * u64::from(prev_clock.value());

        prev_ticks = ticks;
        prev_clock = clock;
    }

    let remaining_ticks = total_ticks - prev_ticks;
    out += u64::from(remaining_ticks) * u64::from(prev_clock.value());

    const _: () = assert!(1_000_000 % TIMER_HZ == 0);
    const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

    Some(Duration::from_micros(out * MICRO_MUL))
}

pub fn compile_mml_song(
    mml: &str,
    file_name: &str,
    song_name: Option<data::Name>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<SongData, SongError> {
    let (s, errors) = mml::parse_mml_song(mml, file_name, song_name, data_instruments)?;

    compile_song_commands(s, pitch_table, data_instruments, errors)
}
