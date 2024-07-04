//! Sound Effects file

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::{load_text_file_with_limit, Name, TextFile};
use crate::errors::FileError;
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::sound_effects::{SoundEffectInput, SoundEffectText};

use std::path::PathBuf;

const NEW_SFX_TOKEN_NO_NEWLINE: &str = "===";
const NEW_SFX_TOKEN: &str = "\n===";
const NEW_SFX_TOKEN_END: &str = "===";
const MML_SFX_IDENTIFIER: &str = "MML\n";
const MML_SFX_IDENTIFIER_NO_NEWLINE: &str = "MML";

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

pub fn convert_sfx_inputs_lossy(sound_effects: Vec<SoundEffectFileSfx>) -> Vec<SoundEffectInput> {
    sound_effects
        .into_iter()
        .map(|s| SoundEffectInput {
            name: Name::new_lossy(s.name),
            sfx: s.sfx,
        })
        .collect()
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
