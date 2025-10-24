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

pub(crate) mod note_tracking;

use self::bc_generator::{parse_and_compile_sound_effect, MmlSongBytecodeGenerator};
use self::instruments::{build_instrument_map, parse_instruments};
use self::line_splitter::{split_mml_song_lines, split_mml_sound_effect_lines};
use self::metadata::parse_headers;
use bc_generator::parse_and_compile_mml_prefix;
pub(crate) use identifier::{IdentifierBuf, IdentifierStr};
use line_splitter::split_mml_sfx_subroutines_header_lines;
use tokenizer::MmlTokens;

use crate::data::{self, UniqueNamesList};
use crate::driver_constants::{MAX_SFX_SUBROUTINES, N_MUSIC_CHANNELS};
use crate::echo::EchoEdl;
use crate::errors::{
    MmlCompileErrors, MmlPrefixError, SfxSubroutineErrors, SongError, SoundEffectErrorList,
};
use crate::mml::song_duration::calc_song_duration;
use crate::mml::subroutines::compile_subroutines;
use crate::mml::tokenizer::Token;
use crate::pitch_table::PitchTable;
use crate::songs::{mml_to_song, song_header_size, SongData};
use crate::sound_effects::CompiledSfxSubroutines;
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

pub use self::metadata::{GlobalSettings, MetaData};

pub use self::note_tracking::{
    find_cursor_state, line_start_ticks, CommandTickTracker, CursorTracker, CursorTrackerGetter,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelId {
    Channel(char),
    Subroutine(u8),
    SoundEffect,
    MmlPrefix,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Section {
    name: String,
    char_index: u32,
}

#[derive(Debug)]
pub struct MmlSoundEffect {
    bytecode: Vec<u8>,
    tick_counter: TickCounter,

    tick_tracker: CommandTickTracker,
    cursor_tracker: note_tracking::CursorTracker,
}

impl MmlSoundEffect {
    pub fn bytecode(&self) -> &[u8] {
        &self.bytecode
    }
    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn tick_tracker(&self) -> &CommandTickTracker {
        &self.tick_tracker
    }

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
    mml: &str,
    file_name: &str,
    song_name: Option<data::Name>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<SongData, SongError> {
    let mut errors = MmlCompileErrors {
        song_name,
        file_name: file_name.to_owned(),
        line_errors: Vec::new(),
        subroutine_errors: Vec::new(),
        channel_errors: Vec::new(),
    };

    let lines = match split_mml_song_lines(mml) {
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

    let n_active_channels = lines.channels.iter().filter(|t| !t.is_empty()).count();

    let song_uses_driver_transpose =
        scan_for_driver_transpose(&lines.channels, &lines.subroutines, &metadata.mml_settings);

    assert!(lines.subroutines.len() <= u8::MAX.into());
    let mut compiler = MmlSongBytecodeGenerator::new(
        &metadata.mml_settings,
        pitch_table,
        mml,
        data_instruments,
        &instruments,
        instrument_map,
        &lines.subroutine_name_map,
        metadata.echo_buffer.max_edl,
        song_header_size(n_active_channels, lines.subroutines.len()),
        true,
    );

    errors.subroutine_errors = compile_subroutines(
        &mut compiler,
        lines.subroutines,
        &lines.subroutine_name_map,
        song_uses_driver_transpose,
    );

    if !errors.subroutine_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }

    let mut channels_iter = lines.channels.into_iter();

    let channels = std::array::from_fn(|c_index| {
        let tokens = channels_iter.next().unwrap();
        if !tokens.is_empty() {
            match compiler.parse_and_compile_song_channel(
                tokens,
                c_index,
                song_uses_driver_transpose,
            ) {
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

    let name = errors
        .song_name
        .as_ref()
        .map(|n| n.to_string())
        .unwrap_or_else(|| file_name.to_owned());

    drop(errors);

    let (song_data, subroutines, tracking) = compiler.take_data();

    let duration = calc_song_duration(&metadata, &channels, &subroutines);

    mml_to_song(
        name,
        metadata,
        song_data,
        duration,
        lines.sections,
        instruments,
        channels,
        subroutines,
        tracking,
    )
}

pub(crate) fn compile_sfx_subroutines(
    sfx: &str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<CompiledSfxSubroutines, SfxSubroutineErrors> {
    let lines = match split_mml_sfx_subroutines_header_lines(sfx) {
        Ok(l) => l,
        Err(e) => return Err(SfxSubroutineErrors::LineErrors(e)),
    };

    if lines.subroutines.len() > MAX_SFX_SUBROUTINES.into() {
        return Err(SfxSubroutineErrors::TooManySfxSubroutines(
            lines.subroutines.len(),
        ));
    }

    let (instruments, inst_errors) = parse_instruments(lines.instruments, data_instruments);
    let mut line_errors = inst_errors;

    let instrument_map = match build_instrument_map(&instruments) {
        Ok(map) => map,
        Err(e) => {
            line_errors.extend(e);
            HashMap::new()
        }
    };

    if !line_errors.is_empty() {
        return Err(SfxSubroutineErrors::LineErrors(line_errors));
    }
    drop(line_errors);

    let global_settings = GlobalSettings::default();

    assert!(lines.subroutines.len() <= u8::MAX.into());
    let mut compiler = MmlSongBytecodeGenerator::new(
        &global_settings,
        pitch_table,
        sfx,
        data_instruments,
        &instruments,
        instrument_map,
        &lines.subroutine_name_map,
        EchoEdl::MIN,
        0,
        false,
    );

    let errors = compile_subroutines(
        &mut compiler,
        lines.subroutines,
        &lines.subroutine_name_map,
        false,
    );

    if errors.is_empty() {
        Ok(compiler.take_sfx_subroutine_data())
    } else {
        Err(SfxSubroutineErrors::SubroutineErrors(errors))
    }
}

pub fn compile_sound_effect(
    sfx: &str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
    subroutines: &CompiledSfxSubroutines,
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
        subroutines,
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

fn scan_for_driver_transpose(
    channels: &[MmlTokens<'_>; N_MUSIC_CHANNELS],
    subroutines: &Vec<(IdentifierStr<'_>, MmlTokens<'_>)>,
    settings: &GlobalSettings,
) -> bool {
    let old_transpose = settings.old_transpose;

    let test_tokens = |tokens: &MmlTokens<'_>| -> bool {
        for t in tokens.token_iter() {
            match t {
                Token::TransposeAsm(_) => return true,
                Token::Transpose if !old_transpose => return true,
                Token::RelativeTranspose if !old_transpose => return true,
                _ => (),
            }
        }
        false
    };

    channels.iter().any(test_tokens) | subroutines.iter().any(|(_, t)| test_tokens(t))
}
