//! MML compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

mod bc_generator;
mod identifier;
mod instruments;
mod line_splitter;
mod metadata;
mod song_duration;
mod tick_count_table;
mod tokenizer;

pub mod command_parser;

#[cfg(feature = "mml_tracking")]
pub(crate) mod note_tracking;

use self::bc_generator::MmlBytecodeGenerator;
use self::instruments::{build_instrument_map, parse_instruments};
use self::line_splitter::split_mml_lines;
use self::metadata::parse_headers;
pub(crate) use identifier::{Identifier, IdentifierStr};

use crate::data::{self, TextFile, UniqueNamesList};
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::{MmlCompileErrors, SongError};
use crate::pitch_table::PitchTable;
use crate::songs::{song_header_size, write_song_header, SongData};

use std::collections::HashMap;
use std::ops::RangeInclusive;

pub const FIRST_MUSIC_CHANNEL: char = 'A';
pub const LAST_MUSIC_CHANNEL: char = 'F';
const MUSIC_CHANNEL_RANGE: RangeInclusive<char> = 'A'..='F';

const _: () = assert!(
    *MUSIC_CHANNEL_RANGE.end() as usize - *MUSIC_CHANNEL_RANGE.start() as usize + 1
        == N_MUSIC_CHANNELS
);

const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F"];

pub use self::bc_generator::ChannelData;
pub use self::bc_generator::MAX_BROKEN_CHORD_NOTES;
pub use self::song_duration::calc_song_duration;
pub use self::tick_count_table::MmlTickCountTable;

pub use self::metadata::MetaData;

#[derive(Clone, Debug, PartialEq)]
pub struct Section {
    name: String,
    line_number: u32,
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
) -> Result<SongData, SongError> {
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
            return Err(SongError::MmlError(errors));
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
        return Err(SongError::MmlError(errors));
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
        return Err(SongError::MmlError(errors));
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

    if !errors.channel_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }
    drop(errors);

    #[cfg(feature = "mml_tracking")]
    let (mut song_data, tracking) = bc_gen.take_data();
    #[cfg(not(feature = "mml_tracking"))]
    let mut song_data = bc_gen.take_data();

    let duration = calc_song_duration(&metadata, &subroutines, &channels);

    write_song_header(&mut song_data, &channels, &subroutines, &metadata)?;

    Ok(SongData::new(
        metadata,
        song_data,
        duration,
        lines.sections,
        channels,
        #[cfg(feature = "mml_tracking")]
        Some(tracking),
    ))
}
