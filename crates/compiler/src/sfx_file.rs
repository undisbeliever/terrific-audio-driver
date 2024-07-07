//! Sound Effects file

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::{load_text_file_with_limit, Name, TextFile};
use crate::errors::FileError;
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::sound_effects::{SfxFlags, SoundEffectInput, SoundEffectText};

use std::path::PathBuf;

const NEW_SFX_TOKEN_NO_NEWLINE: &str = "===";
const NEW_SFX_TOKEN: &str = "\n===";
const OLD_MML_SFX_IDENTIFIER: &str = "MML\n";
const OLD_MML_SFX_IDENTIFIER_NO_NEWLINE: &str = "MML";

const MML_ATTR: &str = "mml";
const INTERRUPTIBLE_ATTR: &str = "interruptible";
const UNINTERRUPTIBLE_ATTR: &str = "uninterruptible";
const ONE_CHANNEL_ATTR: &str = "one";
const ONE_CHANNEL_FALSE_ATTR: &str = "both";

// NOTE: fields are not validated
#[derive(Debug, PartialEq)]
pub struct SoundEffectFileSfx {
    pub name: String,
    pub line_no: u32, // Line number of the name line
    pub flags: SfxFlags,
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
            flags: s.flags,
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
        let (name_and_attrs, sfx_lines) = match sfx_block.split_once('\n') {
            Some((a, b)) => (a, b),
            None => (sfx_block, ""),
        };

        let mut flags = SfxFlags::default();

        let mut attrs = name_and_attrs
            .trim_matches('=')
            .split(|c: char| c.is_whitespace() || c == '=')
            .filter(|s| !s.is_empty());
        let mut mml_sfx = false;

        let mut name = match attrs.next() {
            Some(s) => s.to_owned(),
            None => String::new(),
        };

        for a in attrs {
            if a.eq_ignore_ascii_case(MML_ATTR) {
                mml_sfx = true;
            } else if a.eq_ignore_ascii_case(INTERRUPTIBLE_ATTR) {
                flags.interruptible = Some(true)
            } else if a.eq_ignore_ascii_case(UNINTERRUPTIBLE_ATTR) {
                flags.interruptible = Some(false)
            } else if a.eq_ignore_ascii_case(ONE_CHANNEL_ATTR) {
                flags.one_channel = Some(true)
            } else if a.eq_ignore_ascii_case(ONE_CHANNEL_FALSE_ATTR) {
                flags.one_channel = Some(false)
            } else {
                // Save unknown tags in the name so they are not lost and become an error when compiling SFX
                name.push(' ');
                name.push_str(a);
            }
        }

        let (sfx_lines, mml_sfx) = if mml_sfx {
            (sfx_lines.to_owned(), true)
        } else {
            // No MML attribute, check for the old MML tag (the string "MML" on a new line)
            match sfx_lines.strip_prefix(OLD_MML_SFX_IDENTIFIER) {
                Some(s) => (s.to_owned(), true),
                None => match sfx_lines {
                    OLD_MML_SFX_IDENTIFIER_NO_NEWLINE => (String::new(), true),
                    _ => (sfx_lines.to_owned(), false),
                },
            }
        };

        sound_effects.push(SoundEffectFileSfx {
            name,
            line_no: line_no.try_into().unwrap(),
            flags,
            sfx: match mml_sfx {
                true => SoundEffectText::Mml(sfx_lines),
                false => SoundEffectText::BytecodeAssembly(sfx_lines),
            },
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
    let mut out = String::with_capacity(32 * 1024);

    out.push_str(header);

    for (i, sfx) in sound_effects.enumerate() {
        let (sfx_lines, is_mml) = match &sfx.sfx {
            SoundEffectText::BytecodeAssembly(s) => (s, false),
            SoundEffectText::Mml(s) => (s, true),
        };

        if i == 0 && header.is_empty() {
            out.push_str("=== ");
        } else {
            out.push_str("\n=== ");
        }

        out.push_str(sfx.name.as_str());
        out.push_str(" ===");

        let mut add_attr = |attr| {
            out.push(' ');
            out.push_str(attr);
        };

        if is_mml {
            add_attr(MML_ATTR);
        }

        match sfx.flags.one_channel {
            None => (),
            Some(true) => add_attr(ONE_CHANNEL_ATTR),
            Some(false) => add_attr(ONE_CHANNEL_FALSE_ATTR),
        }

        match sfx.flags.interruptible {
            None => (),
            Some(true) => add_attr(INTERRUPTIBLE_ATTR),
            Some(false) => add_attr(UNINTERRUPTIBLE_ATTR),
        }

        out.push('\n');
        out.push_str(sfx_lines);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sfx_old_file_format_from_string_1() {
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
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("a\nb\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_one".to_owned(),
                        line_no: 5,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("c".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty".to_owned(),
                        line_no: 7,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_two".to_owned(),
                        line_no: 8,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("d\ne\n\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty_MML".to_owned(),
                        line_no: 13,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_empty".to_owned(),
                        line_no: 15,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    }
                ]
            }
        );
    }

    #[test]
    fn sfx_old_file_format_from_string_2() {
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
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("a\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_MML".to_owned(),
                        line_no: 7,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("@1 mml\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_only_newline".to_owned(),
                        line_no: 11,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_not_empty".to_owned(),
                        line_no: 13,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("b\nc\n".to_owned()),
                    }
                ]
            }
        );
    }

    #[test]
    fn sfx_new_file_format_from_string_1() {
        const INPUT: &str = r##"=== test_first ===
a
b

=== test_one
c
===empty===
===    test_two   ===
d
e


=== empty_MML mml ===
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
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("a\nb\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_one".to_owned(),
                        line_no: 5,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("c".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty".to_owned(),
                        line_no: 7,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_two".to_owned(),
                        line_no: 8,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("d\ne\n\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "empty_MML".to_owned(),
                        line_no: 13,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_empty".to_owned(),
                        line_no: 14,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    }
                ]
            }
        );
    }

    #[test]
    fn sfx_new_file_format_from_string_2() {
        const INPUT: &str = r##"; This is a header
; With multiple lines

=== test_first
a

=== test_MML mml ===
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
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("a\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_MML".to_owned(),
                        line_no: 7,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("@1 mml\n".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "test_only_newline".to_owned(),
                        line_no: 10,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "last_not_empty".to_owned(),
                        line_no: 12,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("b\nc\n".to_owned()),
                    }
                ]
            }
        );
    }

    #[test]
    fn test_multiple_equals_signs() {
        const INPUT: &str = concat![
            "=== name1 ===\nTest 1\n",
            "==== name2 ====\nTest 2\n",
            "===== name3 ====\nTest 3",
        ];

        let tf = TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Default::default(),
                file_name: Default::default(),
                header: String::new(),
                sound_effects: vec![
                    SoundEffectFileSfx {
                        name: "name1".to_owned(),
                        line_no: 1,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("Test 1".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "name2".to_owned(),
                        line_no: 3,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("Test 2".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "name3".to_owned(),
                        line_no: 5,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::BytecodeAssembly("Test 3".to_owned()),
                    },
                ]
            }
        );
    }

    #[test]
    fn test_attr_order() {
        const INPUT: &str = "=== name ===== mml\nTest";

        let tf = TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Default::default(),
                file_name: Default::default(),
                header: String::new(),
                sound_effects: vec![SoundEffectFileSfx {
                    name: "name".to_owned(),
                    line_no: 1,
                    flags: SfxFlags::default(),
                    sfx: SoundEffectText::Mml("Test".to_owned()),
                }]
            }
        );
    }

    #[test]
    fn test_unknown_attr() {
        const INPUT: &str = "=== name mml unknown ===\nTest";

        let tf = TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Default::default(),
                file_name: Default::default(),
                header: String::new(),
                sound_effects: vec![SoundEffectFileSfx {
                    name: "name unknown".to_owned(),
                    line_no: 1,
                    flags: SfxFlags::default(),
                    sfx: SoundEffectText::Mml("Test".to_owned()),
                }]
            }
        );
    }

    #[test]
    fn test_mml_is_case_insensitive() {
        const INPUT: &str = concat![
            "=== name MML ===\nTest 1\n",
            "=== name MmL ===\nTest 2\n",
            "=== name mMl ===\nTest 3",
        ];

        let tf = TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Default::default(),
                file_name: Default::default(),
                header: String::new(),
                sound_effects: vec![
                    SoundEffectFileSfx {
                        name: "name".to_owned(),
                        line_no: 1,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("Test 1".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "name".to_owned(),
                        line_no: 3,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("Test 2".to_owned()),
                    },
                    SoundEffectFileSfx {
                        name: "name".to_owned(),
                        line_no: 5,
                        flags: SfxFlags::default(),
                        sfx: SoundEffectText::Mml("Test 3".to_owned()),
                    },
                ]
            }
        );
    }

    // Test that a MML attribute takes priority over the old MML tag
    #[test]
    fn test_mml_mml() {
        const INPUT: &str = "=== name mml===\nMML\n\n";

        let tf = TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: INPUT.to_owned(),
        };

        let sfx_file = sfx_file_from_text_file(tf);

        assert_eq!(
            sfx_file,
            SoundEffectsFile {
                path: Default::default(),
                file_name: Default::default(),
                header: String::new(),
                sound_effects: vec![SoundEffectFileSfx {
                    name: "name".to_owned(),
                    line_no: 1,
                    flags: SfxFlags::default(),
                    sfx: SoundEffectText::Mml("MML\n\n".to_owned()),
                }]
            }
        );
    }

    fn read_sfx_from_string(input: &str) -> Vec<SoundEffectInput> {
        let sfx_file = sfx_file_from_text_file(TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: input.to_owned(),
        });
        convert_sfx_inputs_lossy(sfx_file.sound_effects)
    }

    #[test]
    fn test_interruptible_default() {
        assert_eq!(
            read_sfx_from_string("=== name ==="),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    interruptible: None,
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }

    #[test]
    fn test_interruptible_set() {
        assert_eq!(
            read_sfx_from_string("=== name === interruptible"),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    interruptible: Some(true),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }

    #[test]
    fn test_interruptible_clear() {
        assert_eq!(
            read_sfx_from_string("=== name === uninterruptible"),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    interruptible: Some(false),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }

    #[test]
    fn test_one_channel_flag_default() {
        assert_eq!(
            read_sfx_from_string("=== name ==="),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    one_channel: None,
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }

    #[test]
    fn test_one_channel_flag_set() {
        assert_eq!(
            read_sfx_from_string("=== name === one"),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(true),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }

    #[test]
    fn test_one_channel_flag_clear() {
        assert_eq!(
            read_sfx_from_string("=== name === both"),
            [SoundEffectInput {
                name: Name::new_lossy("name".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(false),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new())
            }]
        );
    }
    #[test]
    fn test_save_and_load() {
        let header = "This is the file header\nHello === World ===\n\n";
        let sound_effects = vec![
            SoundEffectInput {
                name: Name::new_lossy("".to_owned()),
                flags: SfxFlags::default(),
                sfx: SoundEffectText::BytecodeAssembly("Bytecode\nAssembly".to_owned()),
            },
            SoundEffectInput {
                name: Name::new_lossy("MML_Sound_Effect".to_owned()),
                flags: SfxFlags::default(),
                sfx: SoundEffectText::Mml("Test".to_owned()),
            },
            SoundEffectInput {
                name: Name::new_lossy("interruptible_1".to_owned()),
                flags: SfxFlags {
                    interruptible: None,
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("interruptible_2".to_owned()),
                flags: SfxFlags {
                    interruptible: Some(false),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("interruptible_3".to_owned()),
                flags: SfxFlags {
                    interruptible: Some(true),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("one_channel_flag_1".to_owned()),
                flags: SfxFlags {
                    one_channel: None,
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("one_channel_flag_2".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(false),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("one_channel_flag_3".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(true),
                    ..SfxFlags::default()
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("both_flags_set".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(true),
                    interruptible: Some(true),
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
            SoundEffectInput {
                name: Name::new_lossy("both_flags_clear".to_owned()),
                flags: SfxFlags {
                    one_channel: Some(true),
                    interruptible: Some(false),
                },
                sfx: SoundEffectText::BytecodeAssembly(String::new()),
            },
        ];

        let sfx_file = build_sound_effects_file(header, sound_effects.iter());

        let read_sfx = sfx_file_from_text_file(TextFile {
            path: Default::default(),
            file_name: Default::default(),
            contents: sfx_file,
        });

        assert_eq!(read_sfx.header, header);
        assert_eq!(
            convert_sfx_inputs_lossy(read_sfx.sound_effects),
            sound_effects
        );
    }
}
