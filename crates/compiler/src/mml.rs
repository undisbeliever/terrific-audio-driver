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

use self::bc_generator::process_mml_commands;
use self::command_parser::parse_mml_lines;
use self::instruments::{build_instrument_map, parse_instruments, MmlInstrument};
use self::line_splitter::split_mml_lines;
use self::metadata::parse_headers;
pub(crate) use identifier::{Identifier, IdentifierStr};

use crate::bytecode::SubroutineId;
use crate::data::{self, TextFile, UniqueNamesList};
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::{MmlChannelError, MmlCompileErrors};
use crate::file_pos::Line;
use crate::pitch_table::PitchTable;
use crate::time::{TickClock, TickCounter, ZenLen, TIMER_HZ};

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
}

impl Section {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn line_number(&self) -> u32 {
        self.line_number
    }
}

struct SharedChannelInput<'a> {
    zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    instruments: &'a Vec<MmlInstrument>,
    instrument_map: HashMap<IdentifierStr<'a>, usize>,
    subroutines: Option<&'a Vec<ChannelData>>,
    subroutine_map: Option<HashMap<IdentifierStr<'a>, SubroutineId>>,
}

fn parse_and_compile_mml_channel(
    lines: &[Line],
    identifier: Identifier,
    subroutine_index: Option<u8>,
    sci: &SharedChannelInput,
) -> Result<ChannelData, MmlChannelError> {
    match parse_mml_lines(
        lines,
        sci.zenlen,
        &sci.instrument_map,
        sci.subroutine_map.as_ref(),
    ) {
        Err(e) => Err(MmlChannelError {
            identifier,
            parse_errors: e,
            command_errors: Vec::new(),
        }),
        Ok((commands, last_pos)) => {
            match process_mml_commands(
                &commands,
                last_pos,
                identifier.clone(),
                subroutine_index,
                sci.pitch_table,
                sci.instruments,
                sci.subroutines,
            ) {
                Ok(data) => Ok(data),
                Err(e) => Err(MmlChannelError {
                    identifier,
                    parse_errors: Vec::new(),
                    command_errors: e,
                }),
            }
        }
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

    let sci = SharedChannelInput {
        zenlen: metadata.zenlen,
        pitch_table,
        instruments: &instruments,
        instrument_map,
        subroutines: None,
        subroutine_map: None,
    };

    assert!(lines.subroutines.len() <= u8::MAX.into());
    let mut subroutines = Vec::with_capacity(lines.subroutines.len());
    for (s_index, (s_id, s_lines)) in lines.subroutines.iter().enumerate() {
        let s_index = s_index.try_into().unwrap();

        match parse_and_compile_mml_channel(s_lines, s_id.clone(), Some(s_index), &sci) {
            Ok(data) => subroutines.push(data),
            Err(e) => errors.subroutine_errors.push(e),
        }
    }
    let subroutines = subroutines;

    if !errors.subroutine_errors.is_empty() {
        return Err(errors);
    }

    let sci = SharedChannelInput {
        subroutines: Some(&subroutines),
        subroutine_map: Some(
            subroutines
                .iter()
                .map(|s| (s.identifier().as_ref(), s.bc_subroutine.unwrap()))
                .collect(),
        ),
        ..sci
    };

    let mut channels = Vec::with_capacity(lines.channels.len());
    for (c_index, c_lines) in lines.channels.iter().enumerate() {
        if !c_lines.is_empty() {
            let c_id = Identifier::try_from_name(CHANNEL_NAMES[c_index].to_owned()).unwrap();

            match parse_and_compile_mml_channel(c_lines, c_id.clone(), None, &sci) {
                Ok(data) => channels.push(data),
                Err(e) => errors.channel_errors.push(e),
            }
        }
    }
    let channels = channels;

    if errors.channel_errors.is_empty() {
        Ok(MmlData {
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
