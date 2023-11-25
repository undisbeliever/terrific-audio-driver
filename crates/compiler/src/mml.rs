//! MML compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

mod bc_generator;
mod instruments;
mod line_splitter;
mod metadata;
mod tokenizer;

pub mod command_parser;
pub mod identifier;
pub mod tick_count_table;

#[cfg(feature = "mml_tracking")]
pub mod note_tracking;

use self::bc_generator::MmlBytecodeGenerator;
use self::instruments::{build_instrument_map, parse_instruments};
use self::line_splitter::split_mml_lines;
use self::metadata::parse_headers;
pub(crate) use identifier::{Identifier, IdentifierStr};

use crate::data::{self, TextFile, UniqueNamesList};
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::MmlCompileErrors;
use crate::pitch_table::PitchTable;
use crate::songs::song_header_size;
use crate::time::{TickClock, TickCounter, TIMER_HZ};

use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::time::Duration;

pub const FIRST_MUSIC_CHANNEL: char = 'A';
pub const LAST_MUSIC_CHANNEL: char = 'F';
const MUSIC_CHANNEL_RANGE: RangeInclusive<char> = 'A'..='F';

const _: () = assert!(
    *MUSIC_CHANNEL_RANGE.end() as usize - *MUSIC_CHANNEL_RANGE.start() as usize + 1
        == N_MUSIC_CHANNELS
);

const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F"];

#[cfg(feature = "mml_tracking")]
pub use self::bc_generator::BytecodePos;

pub use self::bc_generator::ChannelData;
pub use self::bc_generator::MAX_BROKEN_CHORD_NOTES;

pub use self::metadata::MetaData;

#[derive(Debug, PartialEq)]
pub struct Section {
    name: String,
    line_number: u32,
}

#[derive(Debug, PartialEq)]
pub struct MmlData {
    pub(crate) metadata: MetaData,
    pub(crate) subroutines: Vec<ChannelData>,
    pub(crate) channels: Vec<ChannelData>,
    pub(crate) sections: Vec<Section>,

    pub(crate) song_data: Vec<u8>,

    #[cfg(feature = "mml_tracking")]
    pub(crate) cursor_tracker: note_tracking::CursorTracker,

    #[cfg(feature = "mml_tracking")]
    pub(crate) bc_tracker: Vec<BytecodePos>,
}

// ::TODO add list of subroutines and instruments for the GUI (separate from MmlData)::

impl MmlData {
    pub fn metadata(&self) -> &MetaData {
        &self.metadata
    }
    pub fn subroutines(&self) -> &[ChannelData] {
        &self.subroutines
    }
    pub fn channels(&self) -> &[ChannelData] {
        &self.channels
    }
    pub fn sections(&self) -> &[Section] {
        &self.sections
    }
    pub fn song_data(&self) -> &[u8] {
        &self.song_data
    }
}

impl Section {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn line_number(&self) -> u32 {
        self.line_number
    }
}

pub fn compile_mml(
    mml_file: &TextFile,
    song_name: Option<data::Name>,
    inst_map: &UniqueNamesList<data::Instrument>,
    pitch_table: &PitchTable,
) -> Result<MmlData, MmlCompileErrors> {
    let mut errors = MmlCompileErrors {
        song_name,
        file_name: mml_file.file_name.clone(),
        line_errors: Vec::new(),
        subroutine_errors: Vec::new(),
        channel_errors: Vec::new(),
    };

    let lines = match split_mml_lines(&mml_file.contents) {
        Ok(l) => l,
        Err(e) => {
            errors.line_errors.extend(e);
            return Err(errors);
        }
    };

    let metadata = match parse_headers(lines.headers) {
        Ok(m) => Some(m),
        Err(e) => {
            errors.line_errors.extend(e);
            None
        }
    };

    let (instruments, inst_errors) = parse_instruments(lines.instruments, inst_map);

    errors.line_errors.extend(inst_errors);

    let instrument_map = match build_instrument_map(&instruments) {
        Ok(map) => map,
        Err(e) => {
            errors.line_errors.extend(e);
            HashMap::new()
        }
    };

    if !errors.line_errors.is_empty() {
        return Err(errors);
    }
    let metadata = metadata.unwrap();

    assert!(lines.subroutines.len() <= u8::MAX.into());
    let mut bc_gen = MmlBytecodeGenerator::new(
        metadata.zenlen,
        pitch_table,
        &lines.sections,
        &instruments,
        instrument_map,
        song_header_size(lines.subroutines.len()),
    );

    let mut subroutines = Vec::with_capacity(lines.subroutines.len());
    for (s_index, (s_id, s_lines)) in lines.subroutines.iter().enumerate() {
        let s_index = s_index.try_into().unwrap();

        match bc_gen.parse_and_compile_mml_channel(s_lines, s_id.clone(), Some(s_index)) {
            Ok(data) => subroutines.push(data),
            Err(e) => errors.subroutine_errors.push(e),
        }
    }
    let subroutines = subroutines;

    if !errors.subroutine_errors.is_empty() {
        return Err(errors);
    }

    bc_gen.set_subroutines(&subroutines);

    let mut channels = Vec::with_capacity(lines.channels.len());
    for (c_index, c_lines) in lines.channels.iter().enumerate() {
        if !c_lines.is_empty() {
            let c_id = Identifier::try_from_name(CHANNEL_NAMES[c_index].to_owned()).unwrap();

            match bc_gen.parse_and_compile_mml_channel(c_lines, c_id.clone(), None) {
                Ok(data) => channels.push(data),
                Err(e) => errors.channel_errors.push(e),
            }
        }
    }
    let channels = channels;

    if errors.channel_errors.is_empty() {
        #[cfg(feature = "mml_tracking")]
        let (song_data, cursor_tracker, bc_tracker) = bc_gen.take_data();

        #[cfg(not(feature = "mml_tracking"))]
        let song_data = bc_gen.take_data();

        Ok(MmlData {
            #[cfg(feature = "mml_tracking")]
            cursor_tracker,
            #[cfg(feature = "mml_tracking")]
            bc_tracker,

            song_data,
            metadata,
            subroutines,
            channels,
            sections: lines.sections,
        })
    } else {
        Err(errors)
    }
}

pub fn calc_song_duration(mml_data: &MmlData) -> Option<Duration> {
    let set_song_tick_in_subroutine = mml_data
        .subroutines
        .iter()
        .any(|c| !c.tempo_changes.is_empty());

    if set_song_tick_in_subroutine {
        return None;
    }

    let total_ticks: u32 = mml_data
        .channels()
        .iter()
        .map(|c| c.tick_counter().value())
        .max()
        .unwrap_or(0);

    let mut tempo_changes: Vec<(TickCounter, TickClock)> = mml_data
        .channels
        .iter()
        .flat_map(|c| &c.tempo_changes)
        .cloned()
        .collect();
    tempo_changes.sort_by_key(|(tc, _tempo)| tc.value());

    let mut out: u64 = 0;
    let mut prev_ticks = 0;
    let mut prev_clock = mml_data.metadata.tick_clock;

    for (ticks, clock) in tempo_changes {
        let ticks = ticks.value();

        let section_ticks = ticks - prev_ticks;
        out += u64::from(section_ticks) * u64::from(prev_clock.as_u8());

        prev_ticks = ticks;
        prev_clock = clock;
    }

    let remaining_ticks = total_ticks - prev_ticks;
    out += u64::from(remaining_ticks) * u64::from(prev_clock.as_u8());

    const _: () = assert!(1_000_000 % TIMER_HZ == 0);
    const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

    Some(Duration::from_micros(out * MICRO_MUL))
}
