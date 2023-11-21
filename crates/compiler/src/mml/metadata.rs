//! MML metadata parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::driver_constants::{IDENTITY_FILTER, SFX_TICK_CLOCK};
use crate::echo::{parse_fir_filter_string, EchoBuffer, EchoLength, DEFAULT_EDL};
use crate::errors::{ErrorWithPos, MmlLineError, ValueError};
use crate::file_pos::Line;
use crate::spc_file_export;
use crate::time::{Bpm, TickClock, ZenLen, DEFAULT_BPM, DEFAULT_ZENLEN};

#[derive(Debug, Clone, PartialEq)]
pub struct MetaData {
    pub title: Option<String>,
    pub date: Option<String>,
    pub composer: Option<String>,
    pub author: Option<String>,
    pub copyright: Option<String>,
    pub license: Option<String>,

    pub echo_buffer: EchoBuffer,

    pub tick_clock: TickClock,

    pub zenlen: ZenLen,

    /// SPC export song length in seconds before fading out
    /// (override calculated song duration)
    pub spc_song_length: Option<u32>,

    /// SPC export fadeout length in milliseconds
    pub spc_fadeout_millis: Option<u32>,
}

//
// Header
// ======

fn split_header_line<'a>(line: &'a Line<'a>) -> Result<(&'a str, Line<'a>), MmlLineError> {
    let (header, value) = match line.split_once() {
        Some(s) => s,
        None => {
            return Err(MmlLineError::NoHeader);
        }
    };

    if header.is_empty() {
        return Err(MmlLineError::NoHeader);
    }
    if value.text.is_empty() {
        return Err(MmlLineError::NoValue);
    }

    Ok((header, value))
}

fn parse_u32(s: &str) -> Result<u32, ValueError> {
    match s.parse() {
        Ok(o) => Ok(o),
        Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
    }
}

impl MetaData {
    fn new() -> Self {
        Self {
            title: None,
            date: None,
            composer: None,
            author: None,
            copyright: None,
            license: None,
            echo_buffer: EchoBuffer {
                edl: DEFAULT_EDL,
                fir: IDENTITY_FILTER,
                feedback: 0,
                echo_volume: 0,
            },
            tick_clock: DEFAULT_BPM.to_tick_clock().unwrap(),
            zenlen: DEFAULT_ZENLEN,
            spc_song_length: None,
            spc_fadeout_millis: None,
        }
    }

    pub fn blank_sfx_metadata() -> Self {
        Self {
            tick_clock: SFX_TICK_CLOCK.try_into().unwrap(),
            ..Self::new()
        }
    }
}

struct HeaderState {
    metadata: MetaData,
    tempo_set: bool,
}

impl HeaderState {
    fn new() -> Self {
        Self {
            tempo_set: false,
            metadata: MetaData::new(),
        }
    }

    fn parse_header(&mut self, header: &str, value: &str) -> Result<(), MmlLineError> {
        match header {
            "#Title" => self.metadata.title = Some(value.to_owned()),
            "#Date" => self.metadata.date = Some(value.to_owned()),
            "#Composer" => self.metadata.composer = Some(value.to_owned()),
            "#Author" => self.metadata.author = Some(value.to_owned()),
            "#Copyright" => self.metadata.copyright = Some(value.to_owned()),
            "#License" => self.metadata.license = Some(value.to_owned()),

            "#ZenLen" => self.metadata.zenlen = parse_u32(value)?.try_into()?,

            "#EchoLength" => {
                let echo_length = EchoLength::try_from(parse_u32(value)?)?;
                self.metadata.echo_buffer.edl = echo_length.to_edl();
            }

            "#FirFilter" => self.metadata.echo_buffer.fir = parse_fir_filter_string(value)?,

            "#EchoFeedback" => match value.parse() {
                Ok(i) => self.metadata.echo_buffer.feedback = i,
                Err(_) => return Err(MmlLineError::InvalidEchoFeedback),
            },

            "#EchoVolume" => match value.parse() {
                Ok(i) => self.metadata.echo_buffer.echo_volume = i,
                Err(_) => return Err(MmlLineError::InvalidEchoVolume),
            },

            "#Tempo" => {
                if self.tempo_set {
                    return Err(MmlLineError::CannotSetTempo);
                }
                self.tempo_set = true;

                let bpm = Bpm::try_from(parse_u32(value)?)?;
                self.metadata.tick_clock = bpm.to_tick_clock()?;
            }
            "#Timer" => {
                if self.tempo_set {
                    return Err(MmlLineError::CannotSetTimer);
                }
                self.tempo_set = true;
                self.metadata.tick_clock = parse_u32(value)?.try_into()?;
            }
            "#SpcSongLength" => match value.parse() {
                Ok(i) => {
                    if i > spc_file_export::MAX_SONG_LENGTH {
                        return Err(MmlLineError::InvalidSpcSongLength);
                    }
                    self.metadata.spc_song_length = Some(i)
                }
                Err(_) => return Err(MmlLineError::InvalidSpcSongLength),
            },
            "#SpcFadeout" => match value.parse() {
                Ok(i) => {
                    if i > spc_file_export::MAX_FADEOUT_MILLIS {
                        return Err(MmlLineError::InvalidSpcFadeout);
                    }
                    self.metadata.spc_fadeout_millis = Some(i)
                }
                Err(_) => return Err(MmlLineError::InvalidSpcSongLength),
            },

            h => return Err(MmlLineError::UnknownHeader(h.to_owned())),
        }

        Ok(())
    }
}

pub fn parse_headers(lines: Vec<Line>) -> Result<MetaData, Vec<ErrorWithPos<MmlLineError>>> {
    let mut errors = Vec::new();

    let mut map: HashMap<&str, &str> = HashMap::with_capacity(lines.len());

    let mut header_state = HeaderState::new();

    for line in &lines {
        match split_header_line(line) {
            Ok((header, value)) => {
                if !map.contains_key(header) {
                    map.insert(header, value.text);

                    match header_state.parse_header(header, value.text) {
                        Ok(()) => (),
                        Err(e) => errors.push(ErrorWithPos(value.range(), e)),
                    }
                } else {
                    errors.push(ErrorWithPos(
                        line.position.to_range_str_len(header),
                        MmlLineError::DuplicateHeader(header.to_string()),
                    ));
                }
            }
            Err(e) => {
                errors.push(ErrorWithPos(line.range(), e));
            }
        }
    }

    if errors.is_empty() {
        Ok(header_state.metadata)
    } else {
        Err(errors)
    }
}