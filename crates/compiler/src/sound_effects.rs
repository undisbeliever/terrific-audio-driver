//! Sound Effects compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{BcTerminator, BytecodeContext};
use crate::bytecode_assembler::BytecodeAssembler;
use crate::data::{
    load_text_file_with_limit, InstrumentOrSample, Name, TextFile, UniqueNamesList,
    UniqueNamesProjectFile,
};
use crate::driver_constants::SFX_TICK_CLOCK;
use crate::errors::{
    BytecodeAssemblerError, CombineSoundEffectsError, ErrorWithPos, FileError, SoundEffectError,
    SoundEffectErrorList, SoundEffectsFileError,
};
use crate::file_pos::{blank_file_range, split_lines};
use crate::mml;
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::pitch_table::PitchTable;
use crate::time::{TickClock, TickCounter};

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::time::Duration;

const COMMENT_CHAR: char = ';';

const NEW_SFX_TOKEN_NO_NEWLINE: &str = "===";
const NEW_SFX_TOKEN: &str = "\n===";
const NEW_SFX_TOKEN_END: &str = "===";
const MML_SFX_IDENTIFIER: &str = "MML\n";
const MML_SFX_IDENTIFIER_NO_NEWLINE: &str = "MML";

#[derive(Debug)]
pub enum SoundEffectData {
    Mml(mml::MmlSoundEffect),
    BytecodeAssembly(Vec<u8>, TickCounter),
}

#[derive(Debug)]
pub struct CompiledSoundEffect {
    name: Name,
    data: SoundEffectData,
}

impl CompiledSoundEffect {
    pub fn bytecode(&self) -> &[u8] {
        match &self.data {
            SoundEffectData::BytecodeAssembly(d, _) => d,
            SoundEffectData::Mml(d) => d.bytecode(),
        }
    }

    pub fn tick_counter(&self) -> TickCounter {
        match &self.data {
            SoundEffectData::BytecodeAssembly(_, tc) => *tc,
            SoundEffectData::Mml(d) => d.tick_counter(),
        }
    }

    pub fn duration(&self) -> Duration {
        self.tick_counter()
            .to_duration(TickClock::try_from(SFX_TICK_CLOCK).unwrap())
    }

    #[cfg(feature = "mml_tracking")]
    pub fn cursor_tracker(&self) -> Option<&mml::CursorTracker> {
        match &self.data {
            SoundEffectData::BytecodeAssembly(_, _) => None,
            SoundEffectData::Mml(d) => Some(d.cursor_tracker()),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_mml_sound_effect(
    name: &Name,
    sfx: &str,
    name_string: &str,
    starting_line_number: u32,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
    name_valid: bool,
    duplicate_name: bool,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    let invalid_name = !name_valid;

    match mml::compile_sound_effect(sfx, inst_map, pitch_table) {
        Ok(o) => {
            if !invalid_name && !duplicate_name {
                Ok(CompiledSoundEffect {
                    name: name.clone(),
                    data: SoundEffectData::Mml(o),
                })
            } else {
                Err(SoundEffectError {
                    sfx_name: name_string.to_owned(),
                    sfx_line_no: starting_line_number,
                    invalid_name,
                    duplicate_name,
                    errors: SoundEffectErrorList::MmlErrors(Vec::new()),
                })
            }
        }
        Err(errors) => Err(SoundEffectError {
            sfx_name: name_string.to_owned(),
            sfx_line_no: starting_line_number,
            invalid_name,
            duplicate_name,
            errors,
        }),
    }
}

fn compile_bytecode_sound_effect(
    name: &Name,
    sfx: &str,
    name_string: &str,
    starting_line_number: u32,
    instruments: &UniqueNamesList<InstrumentOrSample>,
    name_valid: bool,
    duplicate_name: bool,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    let mut errors = Vec::new();

    let mut bc = BytecodeAssembler::new(instruments, None, BytecodeContext::SoundEffect);

    let mut last_line_range = blank_file_range();

    for line in split_lines(sfx) {
        let line = line.trim_start().trim_comments(COMMENT_CHAR);

        if !line.text.is_empty() {
            last_line_range = line.range();

            match bc.parse_line(line.text) {
                Ok(()) => (),
                Err(e) => errors.push(ErrorWithPos(line.range(), e)),
            }
        }
    }

    let tick_counter = bc.get_tick_counter();

    let out = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok(out) => Some(out),
        Err(e) => {
            errors.push(ErrorWithPos(last_line_range.clone(), e));
            None
        }
    };

    if tick_counter.is_zero() {
        errors.push(ErrorWithPos(
            last_line_range,
            BytecodeAssemblerError::NoTicksInSoundEffect,
        ));
    }

    let invalid_name = !name_valid;
    let no_errors = !invalid_name && !duplicate_name && errors.is_empty();

    if let (Some(data), true) = (out, no_errors) {
        Ok(CompiledSoundEffect {
            name: name.clone(),
            data: SoundEffectData::BytecodeAssembly(data, tick_counter),
        })
    } else {
        Err(SoundEffectError {
            sfx_name: name_string.to_owned(),
            sfx_line_no: starting_line_number,
            invalid_name,
            duplicate_name,
            errors: SoundEffectErrorList::BytecodeErrors(errors),
        })
    }
}

pub fn compile_sound_effects_file(
    sfx_file: &SoundEffectsFile,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<Vec<CompiledSoundEffect>, SoundEffectsFileError> {
    let mut sound_effects = Vec::with_capacity(sfx_file.sound_effects.len());
    let mut names = HashSet::with_capacity(sfx_file.sound_effects.len());

    let mut errors = Vec::new();

    for sfx in &sfx_file.sound_effects {
        let duplicate_name = !names.insert(&sfx.name);

        let (name, name_valid) = match sfx.name.parse() {
            Ok(n) => (n, true),
            Err(_) => ("sfx".parse().unwrap(), false),
        };

        let r = match &sfx.sfx {
            SoundEffectText::BytecodeAssembly(text) => compile_bytecode_sound_effect(
                &name,
                text,
                &sfx.name,
                sfx.line_no + 1,
                inst_map,
                name_valid,
                duplicate_name,
            ),
            SoundEffectText::Mml(text) => compile_mml_sound_effect(
                &name,
                text,
                &sfx.name,
                sfx.line_no + 1,
                inst_map,
                pitch_table,
                name_valid,
                duplicate_name,
            ),
        };
        match r {
            Ok(s) => sound_effects.push(s),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(sound_effects)
    } else {
        Err(SoundEffectsFileError {
            path: sfx_file.path.clone(),
            file_name: sfx_file.file_name.clone(),
            errors,
        })
    }
}

pub struct CombinedSoundEffectsData {
    pub(crate) sfx_data: Vec<u8>,
    pub(crate) sfx_offsets: Vec<usize>,
}

fn build_sfx_map(
    sound_effects: &[CompiledSoundEffect],
) -> Result<HashMap<&Name, &[u8]>, CombineSoundEffectsError> {
    let mut out = HashMap::new();
    let mut duplicates = Vec::new();

    for sfx in sound_effects {
        if out.insert(&sfx.name, sfx.bytecode()).is_some() {
            duplicates.push(sfx.name.to_string())
        }
    }

    if duplicates.is_empty() {
        Ok(out)
    } else {
        Err(CombineSoundEffectsError::DuplicateSoundEffects(duplicates))
    }
}

pub fn combine_sound_effects(
    sound_effects: &[CompiledSoundEffect],
    pf: &UniqueNamesProjectFile,
) -> Result<CombinedSoundEffectsData, CombineSoundEffectsError> {
    if sound_effects.is_empty() {
        return Err(CombineSoundEffectsError::NoSoundEffectFiles);
    }

    let sfx_map = build_sfx_map(sound_effects)?;

    let export_order = &pf.sound_effects;

    let mut sfx_data = Vec::new();
    let mut sfx_offsets = Vec::with_capacity(export_order.len());

    let mut missing = Vec::new();

    for name in export_order.list() {
        match sfx_map.get(name) {
            Some(s) => {
                sfx_offsets.push(sfx_data.len());
                sfx_data.extend(*s);
            }
            None => missing.push(name.as_str().to_owned()),
        }
    }

    if missing.is_empty() {
        Ok(CombinedSoundEffectsData {
            sfx_data,
            sfx_offsets,
        })
    } else {
        Err(CombineSoundEffectsError::MissingSoundEffects(missing))
    }
}

pub fn blank_compiled_sound_effects() -> CombinedSoundEffectsData {
    CombinedSoundEffectsData {
        sfx_data: Vec::new(),
        sfx_offsets: Vec::new(),
    }
}

// Sound effect input
// ==================
// (used by the GUI)

#[derive(Clone, Debug, PartialEq)]
pub struct SoundEffectInput {
    pub name: Name,
    pub sfx: SoundEffectText,
}

pub fn convert_sfx_inputs_lossy(sound_effects: Vec<SoundEffectFileSfx>) -> Vec<SoundEffectInput> {
    sound_effects
        .into_iter()
        .map(|s| SoundEffectInput {
            name: Name::new_lossy(s.name),
            sfx: s.sfx,
        })
        .collect()
}

pub fn compile_sound_effect_input(
    input: &SoundEffectInput,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    match &input.sfx {
        SoundEffectText::BytecodeAssembly(text) => compile_bytecode_sound_effect(
            &input.name,
            text,
            input.name.as_str(),
            1,
            inst_map,
            true,
            false,
        ),
        SoundEffectText::Mml(text) => compile_mml_sound_effect(
            &input.name,
            text,
            input.name.as_str(),
            1,
            inst_map,
            pitch_table,
            true,
            false,
        ),
    }
}

// Sound effects file
// ==================

#[derive(Clone, Debug, PartialEq)]
pub enum SoundEffectText {
    BytecodeAssembly(String),
    Mml(String),
}

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectFileSfx {
    pub name: String,
    pub line_no: u32, // Line number of the name line
    pub sfx: SoundEffectText,
}

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectsFile {
    pub path: Option<PathBuf>,
    pub file_name: String,

    pub header: String,

    pub sound_effects: Vec<SoundEffectFileSfx>,
}

fn count_lines_including_end(s: &str) -> usize {
    // `s.lines().count()` skips newlines at the end of the `str`.

    if s.is_empty() {
        return 0;
    }

    s.bytes().filter(|&c| c == b'\n').count() + 1
}

fn sfx_file_from_text_file(tf: TextFile) -> SoundEffectsFile {
    let input = tf.contents;

    // Find first sound effect
    let (header, after_header) = match input.strip_prefix(NEW_SFX_TOKEN_NO_NEWLINE) {
        Some(after_prefix) => {
            // The first line is the sound effect name
            (String::new(), after_prefix)
        }
        None => match input.split_once(NEW_SFX_TOKEN) {
            Some((header, after_header)) => (header.to_owned(), after_header),
            None => (input, ""),
        },
    };

    let mut sound_effects = Vec::new();
    let mut line_no = count_lines_including_end(&header) + 1;

    for sfx_block in after_header.split(NEW_SFX_TOKEN) {
        // The first part of `sfx_block` is the name line
        let (name_line, sfx_lines) = match sfx_block.split_once('\n') {
            Some((a, b)) => (a, b),
            None => (sfx_block, ""),
        };

        let name_line = name_line
            .trim()
            .trim_end_matches(NEW_SFX_TOKEN_END)
            .trim_end();

        let sfx = match sfx_lines.strip_prefix(MML_SFX_IDENTIFIER) {
            Some(s) => SoundEffectText::Mml(s.to_owned()),
            None => {
                if sfx_lines == MML_SFX_IDENTIFIER_NO_NEWLINE {
                    SoundEffectText::Mml(String::new())
                } else {
                    SoundEffectText::BytecodeAssembly(sfx_lines.to_owned())
                }
            }
        };

        sound_effects.push(SoundEffectFileSfx {
            name: name_line.to_owned(),
            line_no: line_no.try_into().unwrap(),
            sfx,
        });

        line_no += count_lines_including_end(sfx_block);
    }

    SoundEffectsFile {
        path: tf.path,
        file_name: tf.file_name,
        header,
        sound_effects,
    }
}

pub fn load_sound_effects_file(
    source: &SourcePathBuf,
    parent_path: &ParentPathBuf,
) -> Result<SoundEffectsFile, FileError> {
    let text_file = load_text_file_with_limit(source, parent_path)?;
    Ok(sfx_file_from_text_file(text_file))
}

pub fn build_sound_effects_file<'a>(
    header: &'a str,
    sound_effects: impl Iterator<Item = &'a SoundEffectInput>,
) -> String {
    let mut sound_effects = sound_effects;

    let mut out = String::with_capacity(32 * 1024);

    if header.is_empty() {
        if let Some(sfx) = sound_effects.next() {
            out.push_str("=== ");
            out.push_str(sfx.name.as_str());
            out.push_str(" ===\n");
            match &sfx.sfx {
                SoundEffectText::BytecodeAssembly(s) => out.push_str(s),
                SoundEffectText::Mml(s) => {
                    out.push_str(MML_SFX_IDENTIFIER);
                    out.push_str(s);
                }
            }
        }
    } else {
        out.push_str(header);
    }

    for sfx in sound_effects {
        out.push_str("\n=== ");
        out.push_str(sfx.name.as_str());
        out.push_str(" ===\n");
        match &sfx.sfx {
            SoundEffectText::BytecodeAssembly(s) => out.push_str(s),
            SoundEffectText::Mml(s) => {
                out.push_str(MML_SFX_IDENTIFIER);
                out.push_str(s);
            }
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sfx_file_from_string_1() {
        const INPUT: &str = r##"=== test_first ===
a
b

=== test_one
c
===empty===
===    test_two   ===
d
e


=== empty_MML ===
MML
=== last_empty ===
"##;

        let file_name = "fn.txt".to_owned();
        let tf = TextFile {
            path: None,
            file_name: file_name.clone(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: None,
                file_name,
                header: "".to_owned(),
                sound_effects: vec![
                    SoundEffectFileSfx {
                        name: "test_first".to_owned(),
                        line_no: 1,
                        sfx: SoundEffectText::BytecodeAssembly("a\nb\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_one".to_owned(),
                        line_no: 5,
                        sfx: SoundEffectText::BytecodeAssembly("c".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty".to_owned(),
                        line_no: 7,
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_two".to_owned(),
                        line_no: 8,
                        sfx: SoundEffectText::BytecodeAssembly("d\ne\n\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty_MML".to_owned(),
                        line_no: 13,
                        sfx: SoundEffectText::Mml("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_empty".to_owned(),
                        line_no: 15,
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    }
                ]
            }
        );
    }

    #[test]
    fn sfx_file_from_string_2() {
        const INPUT: &str = r##"; This is a header
; With multiple lines

=== test_first
a

=== test_MML ===
MML
@1 mml

=== test_only_newline ===

===last_not_empty
b
c
"##;

        let path: PathBuf = "path.txt".to_owned().into();
        let file_name = "test.txt".to_owned();
        let tf = TextFile {
            path: Some(path.clone()),
            file_name: file_name.clone(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Some(path),
                file_name,
                header: "; This is a header\n; With multiple lines\n".to_owned(),
                sound_effects: vec![
                    SoundEffectFileSfx {
                        name: "test_first".to_owned(),
                        line_no: 4,
                        sfx: SoundEffectText::BytecodeAssembly("a\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_MML".to_owned(),
                        line_no: 7,
                        sfx: SoundEffectText::Mml("@1 mml\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_only_newline".to_owned(),
                        line_no: 11,
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_not_empty".to_owned(),
                        line_no: 13,
                        sfx: SoundEffectText::BytecodeAssembly("b\nc\n".to_owned()),
                    }
                ]
            }
        );
    }
}
