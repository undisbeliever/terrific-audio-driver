//! MML line splitter

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::identifier::IdentifierStr;
use super::tokenizer::MmlTokens;
use super::{Section, FIRST_MUSIC_CHANNEL};
use super::{COMMENT_CHAR, MUSIC_CHANNEL_RANGE, SECTION_PREFIX};

use crate::driver_constants::{MAX_SUBROUTINES, N_MUSIC_CHANNELS};
use crate::errors::{ErrorWithPos, MmlLineError};
use crate::file_pos::{blank_file_range, split_lines, FilePos, Line, MAX_MML_TEXT_LENGTH};

use std::collections::HashMap;

pub(crate) struct MmlLines<'a> {
    pub headers: Vec<Line<'a>>,
    pub instruments: Vec<(IdentifierStr<'a>, Line<'a>)>,
    pub subroutines: Vec<(IdentifierStr<'a>, MmlTokens<'a>)>,
    pub subroutine_name_map: HashMap<IdentifierStr<'a>, usize>,
    pub channels: [MmlTokens<'a>; N_MUSIC_CHANNELS],
    pub sections: Vec<Section>,
}

pub(crate) struct MmlSfxLines<'a> {
    pub instruments: Vec<(IdentifierStr<'a>, Line<'a>)>,
    pub tokens: MmlTokens<'a>,
}

// Assumes `line` starts with a non-whitespace character
// Assumes `mml_text` length is < i32::MAX
fn split_idstr_and_line(line: Line) -> (&str, Line) {
    let s = line.text;
    let (id, after_id, id_char_count) = match s
        .char_indices()
        .enumerate()
        .find(|(_, (_, c))| c.is_whitespace())
    {
        Some((c_count, (index, _))) => {
            let (a, b) = s.split_at(index);
            (a, b, c_count)
        }
        // No whitespace
        None => (s, "", s.chars().count()),
    };

    let (text, ws_char_count, ws_len) = match after_id.chars().next() {
        None => ("", 0, 0),
        Some(_) => {
            match after_id
                .char_indices()
                .enumerate()
                .find(|(_, (_, c))| !c.is_whitespace())
            {
                Some((c_count, (index, _))) => (&after_id[index..], c_count, index),
                // No characters after whitespace
                None => ("", after_id.chars().count(), after_id.len()),
            }
        }
    };

    let line_char: u32 = (id_char_count + ws_char_count + 1).try_into().unwrap();
    let char_index: u32 = (id.bytes().len() + ws_len).try_into().unwrap();

    (
        id,
        Line {
            text,
            position: FilePos {
                line_number: line.position.line_number,
                line_char: line.position.line_char + line_char,
                char_index: line.position.char_index + char_index,
            },
        },
    )
}

// Assumes `mml_test` length is < i32::MAX
fn split_id_and_line(
    line: Line,
    prefix: char,
) -> Result<(IdentifierStr, Line), ErrorWithPos<MmlLineError>> {
    let start_pos = line.position;
    let (id_str, line) = split_idstr_and_line(line);

    let id_str = id_str.trim_start_matches(prefix);
    if !id_str.is_empty() {
        match IdentifierStr::try_from_str(id_str) {
            Ok(id) => Ok((id, line)),
            Err(e) => Err(ErrorWithPos(
                line.position.to_range(1),
                MmlLineError::InvalidIdentifier(e),
            )),
        }
    } else {
        Err(ErrorWithPos(
            start_pos.to_range(1),
            MmlLineError::NoIdentifier(prefix),
        ))
    }
}

fn validate_music_channels<'a>(
    ids: &'a str,
    pos: &FilePos,
) -> Result<&'a str, ErrorWithPos<MmlLineError>> {
    if ids.chars().all(|c| MUSIC_CHANNEL_RANGE.contains(&c)) {
        Ok(ids)
    } else {
        let unknown = ids
            .chars()
            .filter(|c| !MUSIC_CHANNEL_RANGE.contains(c))
            .collect();
        Err(ErrorWithPos(
            pos.to_range_str_len(ids),
            MmlLineError::UnknownChannel(unknown),
        ))
    }
}

pub(super) fn split_mml_song_lines(
    mml_text: &str,
) -> Result<MmlLines, Vec<ErrorWithPos<MmlLineError>>> {
    let mut errors = Vec::new();

    if mml_text.len() > MAX_MML_TEXT_LENGTH {
        errors.push(ErrorWithPos(
            blank_file_range(),
            MmlLineError::MmlTooLarge(mml_text.len()),
        ));
        return Err(errors);
    }

    let mut headers = Vec::new();
    let mut instruments = Vec::new();
    let mut subroutines: Vec<(IdentifierStr, MmlTokens)> = Vec::new();
    let mut channels: [MmlTokens; N_MUSIC_CHANNELS] = Default::default();
    let mut sections = Vec::new();

    let mut subroutine_name_map: HashMap<IdentifierStr, usize> = HashMap::new();

    let mut line_splitter = split_lines(mml_text);

    while let Some(entire_line) = line_splitter.next() {
        let start_pos = entire_line.position;

        if let Some(section_name) = entire_line.text.strip_prefix(SECTION_PREFIX) {
            if section_name.starts_with(char::is_whitespace) {
                let section_name = section_name.trim();
                if !section_name.is_empty() {
                    sections.push(Section {
                        name: section_name.to_owned(),
                        line_number: entire_line.position.line_number,
                    });
                }
            }

            continue;
        }

        let line = match entire_line.text.split_once(COMMENT_CHAR) {
            Some((l, _comment)) => Line {
                text: l,
                ..entire_line
            },
            None => entire_line.clone(),
        };

        match line.text.chars().next() {
            Some('#') => headers.push(line),
            Some('@') => {
                // instruments
                match split_id_and_line(line, '@') {
                    Ok((id, line)) => instruments.push((id, line)),
                    Err(e) => errors.push(e),
                }
            }
            Some('!') => {
                // Subroutines
                match split_id_and_line(line, '!') {
                    Ok((id, line)) => match subroutine_name_map.get(&id) {
                        Some(index) => subroutines[*index].1.parse_line(
                            line,
                            entire_line.index_range(),
                            &mut line_splitter,
                        ),
                        None => {
                            subroutine_name_map.insert(id, subroutines.len());
                            subroutines.push((
                                id,
                                MmlTokens::new_with_line(
                                    line,
                                    entire_line.index_range(),
                                    &mut line_splitter,
                                ),
                            ));
                        }
                    },
                    Err(e) => errors.push(e),
                }
            }

            Some(c) if c.is_ascii_alphanumeric() => {
                // Music channels
                let (id, line) = split_idstr_and_line(line);
                match validate_music_channels(id, &start_pos) {
                    Ok(id) => {
                        if id.len() == 1 {
                            let c = id.bytes().next().unwrap();
                            let index = u32::from(c) - u32::from(FIRST_MUSIC_CHANNEL);
                            let index = usize::try_from(index).unwrap();

                            channels[index].parse_line(
                                line.clone(),
                                entire_line.index_range(),
                                &mut line_splitter,
                            );
                        } else {
                            let tokens = MmlTokens::new_with_line(
                                line,
                                entire_line.index_range(),
                                &mut line_splitter,
                            );

                            // Each channel will only use the line once
                            let mut unused = [true; N_MUSIC_CHANNELS];

                            for c in id.chars() {
                                let index = u32::from(c) - u32::from(FIRST_MUSIC_CHANNEL);
                                let index = usize::try_from(index).unwrap();

                                if unused[index] {
                                    channels[index].extend(&tokens);
                                    unused[index] = false;
                                }
                            }
                        }
                    }
                    Err(e) => errors.push(e),
                }
            }
            Some(_) => errors.push(ErrorWithPos(line.range(), MmlLineError::CannotParseLine)),

            None => (),
        }
    }

    if subroutines.len() > MAX_SUBROUTINES {
        errors.insert(
            0,
            ErrorWithPos(
                blank_file_range(),
                MmlLineError::TooManySubroutines(subroutines.len()),
            ),
        );
    }

    if errors.is_empty() {
        Ok(MmlLines {
            headers,
            instruments,
            subroutines,
            subroutine_name_map,
            channels,
            sections,
        })
    } else {
        Err(errors)
    }
}

pub(super) fn split_mml_sound_effect_lines(
    mml_text: &str,
) -> Result<MmlSfxLines, Vec<ErrorWithPos<MmlLineError>>> {
    let mut errors = Vec::new();

    if mml_text.len() > MAX_MML_TEXT_LENGTH {
        errors.push(ErrorWithPos(
            blank_file_range(),
            MmlLineError::MmlTooLarge(mml_text.len()),
        ));
        return Err(errors);
    }

    let mut instruments = Vec::new();
    let mut mml = MmlTokens::new();

    let mut line_splitter = split_lines(mml_text);

    while let Some(entire_line) = line_splitter.next() {
        let start_pos = entire_line.position;

        let line = match entire_line.text.split_once(COMMENT_CHAR) {
            Some((l, _comment)) => Line {
                text: l,
                ..entire_line
            },
            None => entire_line.clone(),
        };

        match line.text.chars().next() {
            Some('#') => errors.push(ErrorWithPos(
                line.range(),
                MmlLineError::HeaderInSoundEffect,
            )),
            Some('!') => errors.push(ErrorWithPos(
                line.range(),
                MmlLineError::SubroutineInSoundEffect,
            )),
            Some('@') => {
                // instruments
                match split_id_and_line(line, '@') {
                    Ok((id, line)) => instruments.push((id, line)),
                    Err(e) => errors.push(e),
                }
            }
            Some(c) if c.is_ascii_alphanumeric() => {
                // MML channel
                let (id, line) = split_idstr_and_line(line);
                if id == "A" {
                    mml.parse_line(line, entire_line.index_range(), &mut line_splitter);
                } else {
                    errors.push(ErrorWithPos(
                        start_pos.to_range(1),
                        MmlLineError::InvalidSoundEffectChannel,
                    ));
                }
            }
            Some(_) => errors.push(ErrorWithPos(line.range(), MmlLineError::CannotParseLine)),

            None => (),
        }
    }

    if errors.is_empty() {
        Ok(MmlSfxLines {
            instruments,
            tokens: mml,
        })
    } else {
        Err(errors)
    }
}
