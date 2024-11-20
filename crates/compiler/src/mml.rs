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
mod subroutines;
mod tick_count_table;
mod tokenizer;

pub mod command_parser;

#[cfg(feature = "mml_tracking")]
pub(crate) mod note_tracking;

use self::bc_generator::{parse_and_compile_sound_effect, MmlSongBytecodeGenerator};
use self::instruments::{build_instrument_map, parse_instruments};
use self::line_splitter::{split_mml_song_lines, split_mml_sound_effect_lines};
use self::metadata::parse_headers;
use bc_generator::parse_and_compile_mml_prefix;
pub(crate) use identifier::{IdentifierBuf, IdentifierStr};
use tokenizer::MmlTokens;

use crate::data::{self, TextFile, UniqueNamesList};
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::{MmlCompileErrors, MmlPrefixError, SongError, SoundEffectErrorList};
use crate::mml::song_duration::calc_song_duration;
use crate::mml::subroutines::compile_subroutines;
use crate::pitch_table::PitchTable;
use crate::songs::{mml_to_song, song_header_size, SongData};
use crate::time::TickCounter;

use std::collections::HashMap;
use std::ops::RangeInclusive;

pub const FIRST_MUSIC_CHANNEL: char = 'A';
pub const LAST_MUSIC_CHANNEL: char = 'H';
const MUSIC_CHANNEL_RANGE: RangeInclusive<char> = 'A'..='H';

const _: () = assert!(
    *MUSIC_CHANNEL_RANGE.end() as usize - *MUSIC_CHANNEL_RANGE.start() as usize + 1
        == N_MUSIC_CHANNELS
);

const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F", "G", "H"];

pub const COMMENT_CHAR: char = ';';
pub const SECTION_PREFIX: &str = ";;";

pub const MAX_MML_PREFIX_STR_LENGTH: usize = 16 * 1024;
pub const MAX_MML_PREFIX_TICKS: TickCounter = TickCounter::new(16);

pub use self::tick_count_table::MmlTickCountTable;

pub use self::metadata::MetaData;

#[cfg(feature = "mml_tracking")]
pub use self::note_tracking::CursorTracker;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelId {
    Channel(char),
    Subroutine(u8),
    SoundEffect,
}

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

#[derive(Debug)]
pub struct MmlSoundEffect {
    bytecode: Vec<u8>,
    tick_counter: TickCounter,

    #[cfg(feature = "mml_tracking")]
    cursor_tracker: note_tracking::CursorTracker,
}

impl MmlSoundEffect {
    pub fn bytecode(&self) -> &[u8] {
        &self.bytecode
    }
    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    #[cfg(feature = "mml_tracking")]
    pub fn cursor_tracker(&self) -> &note_tracking::CursorTracker {
        &self.cursor_tracker
    }
}

#[derive(Debug)]
pub struct MmlPrefixData {
    bytecode: Vec<u8>,
}

impl MmlPrefixData {
    pub fn bytecode(&self) -> &[u8] {
        &self.bytecode
    }
}

pub fn compile_mml(
    mml_file: &TextFile,
    song_name: Option<data::Name>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<SongData, SongError> {
    let mut errors = MmlCompileErrors {
        song_name,
        file_name: mml_file.file_name.clone(),
        line_errors: Vec::new(),
        subroutine_errors: Vec::new(),
        channel_errors: Vec::new(),
    };

    let lines = match split_mml_song_lines(&mml_file.contents) {
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

    let (instruments, inst_errors) = parse_instruments(lines.instruments, data_instruments);

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
    let mut compiler = MmlSongBytecodeGenerator::new(
        metadata.zenlen,
        pitch_table,
        &mml_file.contents,
        data_instruments,
        &lines.sections,
        &instruments,
        instrument_map,
        &lines.subroutine_name_map,
        song_header_size(lines.subroutines.len()),
    );

    errors.subroutine_errors =
        compile_subroutines(&mut compiler, lines.subroutines, &lines.subroutine_name_map);

    if !errors.subroutine_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }

    let mut channels_iter = lines.channels.into_iter();

    let channels = std::array::from_fn(|c_index| {
        let tokens = channels_iter.next().unwrap();
        if !tokens.is_empty() {
            let c_id = IdentifierStr::try_from_name(CHANNEL_NAMES[c_index]).unwrap();

            match compiler.parse_and_compile_song_channel(tokens, c_id) {
                Ok(data) => Some(data),
                Err(e) => {
                    errors.channel_errors.push(e);
                    None
                }
            }
        } else {
            None
        }
    });
    assert!(channels_iter.next().is_none());

    if !errors.channel_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }
    drop(errors);

    #[cfg(feature = "mml_tracking")]
    let (song_data, subroutines, tracking) = compiler.take_data();
    #[cfg(not(feature = "mml_tracking"))]
    let (song_data, subroutines) = compiler.take_data();

    let duration = calc_song_duration(&metadata, &channels, &subroutines);

    mml_to_song(
        metadata,
        song_data,
        duration,
        lines.sections,
        instruments,
        channels,
        subroutines,
        #[cfg(feature = "mml_tracking")]
        tracking,
    )
}

pub fn compile_sound_effect(
    sfx: &str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<MmlSoundEffect, SoundEffectErrorList> {
    let lines = match split_mml_sound_effect_lines(sfx) {
        Ok(l) => l,
        Err(e) => return Err(SoundEffectErrorList::MmlLineErrors(e)),
    };

    let (instruments, inst_errors) = parse_instruments(lines.instruments, data_instruments);
    let mut line_errors = inst_errors;

    let instruments_map = match build_instrument_map(&instruments) {
        Ok(map) => map,
        Err(e) => {
            line_errors.extend(e);
            HashMap::new()
        }
    };

    if !line_errors.is_empty() {
        return Err(SoundEffectErrorList::MmlLineErrors(line_errors));
    }
    drop(line_errors);

    match parse_and_compile_sound_effect(
        sfx,
        lines.tokens,
        pitch_table,
        &instruments,
        data_instruments,
        &instruments_map,
    ) {
        Ok(o) => Ok(o),
        Err(e) => Err(SoundEffectErrorList::MmlErrors(e)),
    }
}

pub fn compile_mml_prefix(
    mml_prefix: &str,
    song: &SongData,
    pitch_table: &PitchTable,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<MmlPrefixData, MmlPrefixError> {
    if mml_prefix.len() > MAX_MML_PREFIX_STR_LENGTH {
        return Err(MmlPrefixError::PrefixStrTooLarge(mml_prefix.len()));
    }

    let mml_instruments = song.instruments();

    let instruments_map = match build_instrument_map(mml_instruments) {
        Ok(map) => map,
        Err(_) => {
            // This should not happen
            return Err(MmlPrefixError::InstrumentError);
        }
    };

    let tokens = MmlTokens::tokenize_one_line(mml_prefix);

    match parse_and_compile_mml_prefix(
        mml_prefix,
        tokens,
        pitch_table,
        mml_instruments,
        data_instruments,
        &instruments_map,
    ) {
        Ok(o) => Ok(o),
        Err(e) => Err(MmlPrefixError::MmlErrors(e)),
    }
}
