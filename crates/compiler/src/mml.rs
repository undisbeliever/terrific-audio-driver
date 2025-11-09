//! MML compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

mod command_parser;
mod instruments;
mod line_splitter;
mod metadata;
mod note_tracking;
mod tick_count_table;
mod tokenizer;

use self::instruments::{build_instrument_map, parse_instruments};
use self::line_splitter::{split_mml_song_lines, split_mml_sound_effect_lines};
use self::metadata::parse_headers;
use line_splitter::split_mml_sfx_subroutines_header_lines;
use tokenizer::MmlTokens;

use crate::bytecode::BytecodeContext;
use crate::command_compiler::channel_bc_generator;
use crate::command_compiler::commands::{
    ChannelCommands, SfxSubroutineCommands, SongCommands, SoundEffectCommands, SubroutineCommands,
};
use crate::data::{self, UniqueNamesList};
use crate::driver_constants::{MAX_SFX_SUBROUTINES, MAX_SUBROUTINES};
use crate::errors::{
    MmlChannelError, MmlCompileErrors, MmlPrefixError, SfxSubroutineErrors, SongError,
    SoundEffectErrorList,
};
use crate::identifier::{ChannelId, IdentifierStr, MusicChannelIndex};
use crate::mml::command_parser::parse_mml_tokens;
use crate::pitch_table::PitchTable;
use crate::songs::SongData;
use crate::sound_effects::CompiledSfxSubroutines;
use crate::subroutines::{BlankSubroutineMap, CompiledSubroutines};
use crate::time::TickCounter;

use std::collections::HashMap;

pub const COMMENT_CHAR: char = ';';
pub const SECTION_PREFIX: &str = ";;";

pub const MAX_MML_PREFIX_STR_LENGTH: usize = 16 * 1024;
pub const MAX_MML_PREFIX_TICKS: TickCounter = TickCounter::new(16);

pub const FINE_QUANTIZATION_SCALE: u8 =
    crate::command_compiler::commands::Quantization::FINE_QUANTIZATION_SCALE;

pub(crate) use self::command_parser::{
    MAX_COARSE_TREMOLO_AMPLITUDE, MAX_COARSE_VOLUME, MIN_COARSE_TREMOLO_AMPLITUDE, PX_PAN_RANGE,
};
pub use self::metadata::GlobalSettings;
pub use self::note_tracking::{
    find_cursor_state, line_start_ticks, CommandTickTracker, CursorTracker, CursorTrackerGetter,
};
pub use self::tick_count_table::MmlTickCountTable;

#[derive(Clone, Debug, PartialEq)]
pub struct Section {
    name: String,
    char_index: u32,
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

fn parse_tokens<'a>(
    channel: ChannelId,
    identifier: IdentifierStr,
    tokens: MmlTokens<'a>,
    mml_instrument_map: &HashMap<IdentifierStr, usize>,
    subroutine_name_map: &HashMap<IdentifierStr, usize>,
    global_settings: &GlobalSettings,
    cursor_tracker: &mut CursorTracker,
) -> Result<ChannelCommands<'a>, (ChannelCommands<'a>, MmlChannelError)> {
    let (commands, errors) = parse_mml_tokens(
        channel,
        tokens,
        mml_instrument_map,
        subroutine_name_map,
        global_settings,
        cursor_tracker,
    );

    if errors.is_empty() {
        Ok(commands)
    } else {
        Err((
            commands,
            MmlChannelError {
                identifier: identifier.to_owned(),
                errors,
            },
        ))
    }
}

fn parse_subroutines<'a>(
    subroutines: Vec<(IdentifierStr<'a>, MmlTokens<'a>)>,
    instrument_map: &HashMap<IdentifierStr, usize>,
    subroutine_name_map: &HashMap<IdentifierStr, usize>,
    global_settings: &GlobalSettings,
    cursor_tracking: &mut CursorTracker,
    errors: &mut Vec<MmlChannelError>,
) -> Vec<SubroutineCommands<'a>> {
    assert!(subroutines.len() <= MAX_SUBROUTINES);

    subroutines
        .into_iter()
        .enumerate()
        .map(|(i, (id, tokens))| {
            let i = i.try_into().unwrap();

            let (c, e) = parse_mml_tokens(
                ChannelId::Subroutine(i),
                tokens,
                instrument_map,
                subroutine_name_map,
                global_settings,
                cursor_tracking,
            );

            if !e.is_empty() {
                errors.push(MmlChannelError {
                    identifier: id.to_owned(),
                    errors: e,
                })
            }

            SubroutineCommands {
                index: i,
                identifier: id,
                commands: c.commands,
                end_pos: c.end_pos,
                analysis: Default::default(),
            }
        })
        .collect()
}

pub(crate) fn parse_mml_song<'a>(
    mml: &'a str,
    file_name: &str,
    song_name: Option<data::Name>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<(SongCommands<'a>, MmlCompileErrors), SongError> {
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

    let mut mml_tracking = CursorTracker::new();

    let subroutines = parse_subroutines(
        lines.subroutines,
        &instrument_map,
        &lines.subroutine_name_map,
        &metadata.mml_settings,
        &mut mml_tracking,
        &mut errors.subroutine_errors,
    );

    let mut channels_iter = lines.channels.into_iter();

    let channels = std::array::from_fn(|c_index| {
        let tokens = channels_iter.next().unwrap();
        if !tokens.is_empty() {
            let c_index = MusicChannelIndex::try_new(c_index).unwrap();

            match parse_tokens(
                ChannelId::Channel(c_index),
                c_index.identifier(),
                tokens,
                &instrument_map,
                &lines.subroutine_name_map,
                &metadata.mml_settings,
                &mut mml_tracking,
            ) {
                Ok(c) => Some(c),
                Err((c, e)) => {
                    errors.channel_errors.push(e);
                    Some(c)
                }
            }
        } else {
            None
        }
    });
    assert!(channels_iter.next().is_none());

    if !errors.line_errors.is_empty() {
        return Err(SongError::MmlError(errors));
    }

    let song = SongCommands {
        name: errors
            .song_name
            .as_ref()
            .map(|n| n.to_string())
            .unwrap_or_else(|| file_name.to_owned()),
        metadata,
        sections: lines.sections,
        mml_tracking,
        instruments,
        subroutines,
        channels,
    };

    Ok((song, errors))
}

pub(crate) fn parse_sfx_subroutines<'a>(
    sfx: &'a str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<SfxSubroutineCommands<'a>, SfxSubroutineErrors> {
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

    let mut mml_tracker = CursorTracker::new();
    let mut errors = Vec::new();

    let subroutines = parse_subroutines(
        lines.subroutines,
        &instrument_map,
        &lines.subroutine_name_map,
        &GlobalSettings::default(),
        &mut mml_tracker,
        &mut errors,
    );

    Ok(SfxSubroutineCommands {
        instruments,
        subroutines,
        mml_tracker,
        errors,
    })
}

pub(crate) fn parse_sound_effect<'a>(
    sfx: &'a str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    sfx_subroutines: &CompiledSfxSubroutines,
) -> Result<SoundEffectCommands<'a>, SoundEffectErrorList> {
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

    let mut mml_tracker = CursorTracker::new();

    let (commands, errors) = parse_mml_tokens(
        ChannelId::SoundEffect,
        lines.tokens,
        &instruments_map,
        sfx_subroutines,
        &GlobalSettings::default(),
        &mut mml_tracker,
    );

    Ok(SoundEffectCommands {
        instruments,
        commands,
        errors,
        mml_tracker,
    })
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

    let (commands, errors) = parse_mml_tokens(
        ChannelId::MmlPrefix,
        tokens,
        &instruments_map,
        &BlankSubroutineMap,
        &GlobalSettings::default(),
        &mut CursorTracker::new(),
    );

    channel_bc_generator::compile_mml_prefix(
        &commands,
        BytecodeContext::MmlPrefix,
        mml_instruments,
        data_instruments,
        pitch_table,
        &CompiledSubroutines::new_blank(),
        errors,
    )
    .map(|bytecode| MmlPrefixData { bytecode })
}
