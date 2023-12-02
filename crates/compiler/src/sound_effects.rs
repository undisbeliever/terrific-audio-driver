//! Sound Effects compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{BcTerminator, BytecodeContext};
use crate::bytecode_assembler::BytecodeAssembler;
use crate::data::{
    load_text_file_with_limit, Instrument, Name, TextFile, UniqueNamesList, UniqueNamesProjectFile,
};
use crate::errors::{
    BytecodeAssemblerError, CombineSoundEffectsError, ErrorWithLine, FileError, SoundEffectError,
    SoundEffectsFileError,
};
use crate::path::{ParentPathBuf, SourcePathBuf};

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

const COMMENT_CHAR: char = ';';

const NEW_SFX_TOKEN_NO_NEWLINE: &str = "===";
const NEW_SFX_TOKEN: &str = "\n===";
const NEW_SFX_TOKEN_END: &str = "===";

// ::TODO add MML based sound effects::

pub struct CompiledSoundEffect {
    name: Name,
    data: Vec<u8>,
}

impl CompiledSoundEffect {
    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }
}

fn compile_sound_effect(
    name: &Name,
    sfx: &str,
    name_string: &str,
    starting_line_number: u32,
    instruments: &UniqueNamesList<Instrument>,
    name_valid: bool,
    duplicate_name: bool,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    let mut errors = Vec::new();

    let mut bc = BytecodeAssembler::new(instruments, None, BytecodeContext::SoundEffect);

    let mut last_line_no: u32 = starting_line_number;

    for (line_no, line) in sfx.lines().enumerate() {
        let line_no = u32::try_from(line_no).unwrap() + starting_line_number;

        let line = match line.split_once(COMMENT_CHAR) {
            Some((asm, _comment)) => asm,
            None => line,
        };

        let line = line.trim();

        if !line.is_empty() {
            last_line_no = line_no;

            match bc.parse_line(line) {
                Ok(()) => (),
                Err(e) => errors.push(ErrorWithLine(line_no, e)),
            }
        }
    }

    let tick_counter = bc.get_tick_counter();

    let out = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok(out) => Some(out),
        Err(e) => {
            errors.push(ErrorWithLine(last_line_no, e));
            None
        }
    };

    if tick_counter.is_zero() {
        errors.push(ErrorWithLine(
            last_line_no,
            BytecodeAssemblerError::NoTicksInSoundEffect,
        ));
    }

    let invalid_name = !name_valid;

    let no_errors = !invalid_name && !duplicate_name && errors.is_empty();

    if let (Some(data), true) = (out, no_errors) {
        Ok(CompiledSoundEffect {
            name: name.clone(),
            data,
        })
    } else {
        Err(SoundEffectError {
            sfx_name: name_string.to_owned(),
            sfx_line_no: starting_line_number,
            invalid_name,
            duplicate_name,
            errors,
        })
    }
}

pub fn compile_sound_effects_file(
    sfx_file: &SoundEffectsFile,
    instruments: &UniqueNamesList<Instrument>,
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

        match compile_sound_effect(
            &name,
            &sfx.sfx,
            &sfx.name,
            sfx.line_no + 1,
            instruments,
            name_valid,
            duplicate_name,
        ) {
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
        if out.insert(&sfx.name, sfx.data.as_slice()).is_some() {
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
    pub sfx: String,
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
    instruments: &UniqueNamesList<Instrument>,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    compile_sound_effect(
        &input.name,
        &input.sfx,
        input.name.as_str(),
        1,
        instruments,
        true,
        false,
    )
}

// Sound effects file
// ==================

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectFileSfx {
    pub name: String,
    pub line_no: u32, // Line number of the name line
    pub sfx: String,
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

        sound_effects.push(SoundEffectFileSfx {
            name: name_line.to_owned(),
            line_no: line_no.try_into().unwrap(),
            sfx: sfx_lines.to_owned(),
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

    out.push_str(header);

    if header.is_empty() {
        if let Some(sfx) = sound_effects.next() {
            out.push_str("=== ");
            out.push_str(sfx.name.as_str());
            out.push_str(" ===\n");
            out.push_str(&sfx.sfx);
        }
    } else {
        out.push_str(header);
    }

    for sfx in sound_effects {
        out.push_str("\n=== ");
        out.push_str(sfx.name.as_str());
        out.push_str(" ===\n");
        out.push_str(&sfx.sfx);
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
                        sfx: "a\nb\n".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "test_one".to_owned(),
                        line_no: 5,
                        sfx: "c".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "empty".to_owned(),
                        line_no: 7,
                        sfx: "".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "test_two".to_owned(),
                        line_no: 8,
                        sfx: "d\ne\n\n".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "last_empty".to_owned(),
                        line_no: 13,
                        sfx: "".to_owned(),
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
                        sfx: "a\n".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "test_only_newline".to_owned(),
                        line_no: 7,
                        sfx: "".to_owned(),
                    },
                    SoundEffectFileSfx {
                        name: "last_not_empty".to_owned(),
                        line_no: 9,
                        sfx: "b\nc\n".to_owned(),
                    }
                ]
            }
        );
    }
}
