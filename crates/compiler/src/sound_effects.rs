//! Sound Effects compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode_assembler::{build_instruments_map, BytecodeAssembler, InstrumentsMap};
use crate::data::Name;
use crate::errors::{ErrorWithLine, SoundEffectError, SoundEffectsFileError};
use crate::MappingsFile;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

const COMMENT_CHAR: char = ';';
const DISABLE_CHANNEL_STR: &str = "disable_channel";

const NEW_SFX_TOKEN_NO_NEWLINE: &str = "===";
const NEW_SFX_TOKEN: &str = "\n===";
const NEW_SFX_TOKEN_END: &str = "===";

// ::TODO add MML based sound effects::

pub fn compile_sound_effect(
    sfx_name: &str,
    sfx: &str,
    starting_line_number: u32,
    instruments: &InstrumentsMap,
) -> Result<(Name, Vec<u8>), SoundEffectError> {
    let mut errors = Vec::new();

    let name = Name::try_new(sfx_name.to_string());

    let mut bc = BytecodeAssembler::new(instruments, None, false, true);

    let mut last_line: &str = "";
    let mut last_line_no: u32 = starting_line_number;

    for (line_no, line) in sfx.lines().enumerate() {
        let line_no = u32::try_from(line_no).unwrap() + starting_line_number;

        let line = match line.split_once(COMMENT_CHAR) {
            Some((asm, _comment)) => asm,
            None => line,
        };

        let line = line.trim();

        if !line.is_empty() {
            last_line = line;
            last_line_no = line_no;

            match bc.parse_line(line) {
                Ok(()) => (),
                Err(e) => errors.push(ErrorWithLine(line_no, e)),
            }
        }
    }

    let out = match bc.get_bytecode() {
        Ok(out) => Some(out),
        Err(e) => {
            errors.push(ErrorWithLine(last_line_no, e));
            None
        }
    };

    // ::TODO move these checks into the Bytecode.get_bytecode()::
    let no_notes = bc.get_tick_counter().is_zero();
    let no_disable_channel = last_line != DISABLE_CHANNEL_STR;

    let invalid_name = name.is_err();

    let no_errors = !no_notes & !no_disable_channel && !invalid_name && errors.is_empty();

    if let (Ok(name), Some(out), true) = (name, out, no_errors) {
        Ok((name, out.to_vec()))
    } else {
        Err(SoundEffectError {
            sfx_name: sfx_name.to_owned(),
            sfx_line_no: starting_line_number,
            invalid_name,
            no_notes,
            no_disable_channel,
            errors,
        })
    }
}

fn compile_sound_effects(
    sfx_file: &SoundEffectsFile,
    instruments_map: &InstrumentsMap,
) -> Result<HashMap<Name, Vec<u8>>, SoundEffectsFileError> {
    let mut sound_effects = HashMap::with_capacity(sfx_file.sound_effects.len());

    let mut errors = Vec::new();
    let mut duplicates = Vec::new();

    for sfx in &sfx_file.sound_effects {
        match compile_sound_effect(&sfx.name, &sfx.sfx, sfx.line_no + 1, instruments_map) {
            Ok((s_name, s)) => {
                let duplicate_name = sound_effects.insert(s_name.clone(), s).is_some();
                if duplicate_name {
                    duplicates.push((sfx.line_no, s_name));
                }
            }
            Err(e) => errors.push(e),
        }
    }

    if !errors.is_empty() {
        Err(SoundEffectsFileError::SoundEffectErrors(errors))
    } else if !duplicates.is_empty() {
        Err(SoundEffectsFileError::DuplicateSfxNamesInSfxFile(
            duplicates,
        ))
    } else {
        Ok(sound_effects)
    }
}

pub struct CompiledSoundEffects {
    pub(crate) sfx_data: Vec<u8>,
    pub(crate) sfx_offsets: Vec<usize>,
}

fn combine_sound_effects(
    sound_effects: HashMap<Name, Vec<u8>>,
    mapping: &Vec<Name>,
) -> Result<CompiledSoundEffects, SoundEffectsFileError> {
    let mut sfx_data = Vec::new();
    let mut sfx_offsets = Vec::with_capacity(mapping.len());

    let mut missing = Vec::new();

    for name in mapping {
        match sound_effects.get(name) {
            Some(s) => {
                sfx_offsets.push(sfx_data.len());
                sfx_data.extend(s);
            }
            None => missing.push(name.clone()),
        }
    }

    if missing.is_empty() {
        Ok(CompiledSoundEffects {
            sfx_data,
            sfx_offsets,
        })
    } else {
        Err(SoundEffectsFileError::MissingSoundEffects(missing))
    }
}

pub fn compile_sound_effects_file(
    sfx_file: &SoundEffectsFile,
    mappings_file: &MappingsFile,
) -> Result<CompiledSoundEffects, SoundEffectsFileError> {
    let instruments_map = build_instruments_map(&mappings_file.mappings.instruments);
    let sound_effects = compile_sound_effects(sfx_file, &instruments_map)?;

    combine_sound_effects(sound_effects, &mappings_file.mappings.sound_effects)
}

// Sound effects file
// ==================

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectInput {
    pub name: String,
    pub line_no: u32, // Line number of the name line
    pub sfx: String,
}

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectsFile {
    pub path: PathBuf,
    pub file_name: String,

    pub header: String,

    pub sound_effects: Vec<SoundEffectInput>,
}

fn count_lines_including_end(s: &str) -> usize {
    // `s.lines().count()` skips newlines at the end of the `str`.

    if s.is_empty() {
        return 0;
    }

    s.bytes().filter(|&c| c == b'\n').count() + 1
}

pub fn sfx_file_from_string(input: String, path: &Path) -> SoundEffectsFile {
    let file_name = path
        .file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string();

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

        sound_effects.push(SoundEffectInput {
            name: name_line.to_owned(),
            line_no: line_no.try_into().unwrap(),
            sfx: sfx_lines.to_owned(),
        });

        line_no += count_lines_including_end(sfx_block);
    }

    SoundEffectsFile {
        path: path.to_owned(),
        file_name,
        header,
        sound_effects,
    }
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

        let path: PathBuf = "parent/basename.txt".to_owned().into();

        let sfx_file = sfx_file_from_string(INPUT.to_owned(), &path);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path,
                file_name: "basename.txt".to_owned(),
                header: "".to_owned(),
                sound_effects: vec![
                    SoundEffectInput {
                        name: "test_first".to_owned(),
                        line_no: 1,
                        sfx: "a\nb\n".to_owned(),
                    },
                    SoundEffectInput {
                        name: "test_one".to_owned(),
                        line_no: 5,
                        sfx: "c".to_owned(),
                    },
                    SoundEffectInput {
                        name: "empty".to_owned(),
                        line_no: 7,
                        sfx: "".to_owned(),
                    },
                    SoundEffectInput {
                        name: "test_two".to_owned(),
                        line_no: 8,
                        sfx: "d\ne\n\n".to_owned(),
                    },
                    SoundEffectInput {
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

        let path: PathBuf = "only_basename.txt".to_owned().into();

        let sfx_file = sfx_file_from_string(INPUT.to_owned(), &path);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path,
                file_name: "only_basename.txt".to_owned(),
                header: "; This is a header\n; With multiple lines\n".to_owned(),
                sound_effects: vec![
                    SoundEffectInput {
                        name: "test_first".to_owned(),
                        line_no: 4,
                        sfx: "a\n".to_owned(),
                    },
                    SoundEffectInput {
                        name: "test_only_newline".to_owned(),
                        line_no: 7,
                        sfx: "".to_owned(),
                    },
                    SoundEffectInput {
                        name: "last_not_empty".to_owned(),
                        line_no: 9,
                        sfx: "b\nc\n".to_owned(),
                    }
                ]
            }
        );
    }
}
