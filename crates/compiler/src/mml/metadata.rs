//! MML metadata parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::driver_constants::SFX_TICK_CLOCK;
use crate::echo::{
    parse_fir_filter_string, EchoBuffer, EchoFeedback, EchoLength, EchoVolume, DEFAULT_EDL,
    IDENTITY_FILTER,
};
use crate::errors::{ErrorWithPos, MmlLineError, ValueError};
use crate::file_pos::{blank_file_range, Line};
use crate::time::{Bpm, TickClock, ZenLen, DEFAULT_BPM, DEFAULT_ZENLEN};
use crate::value_newtypes::{parse_i8wh, I8WithByteHexValueNewType};
use crate::{spc_file_export, FilePosRange};

#[derive(Debug, Clone, PartialEq)]
pub struct MetaData {
    pub title: Option<String>,
    pub game: Option<String>,
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

fn split_header_line<'a>(
    line: &'a Line<'a>,
) -> Result<(&'a str, &'a str, FilePosRange), MmlLineError> {
    match line.split_once() {
        Some((header, value)) => {
            if header.is_empty() {
                Err(MmlLineError::NoHeader)
            } else if value.text.is_empty() {
                Ok((header, "", line.range()))
            } else {
                Ok((header, value.text, value.range()))
            }
        }
        None => Ok((line.text, "", line.range())),
    }
}

fn parse_u32(s: &str) -> Result<u32, ValueError> {
    match s.parse() {
        Ok(o) => Ok(o),
        Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
    }
}

fn parse_echo_volume(s: &str) -> Result<(EchoVolume, EchoVolume), MmlLineError> {
    let arguments: Vec<&str> = s.split_ascii_whitespace().collect();

    match arguments.len() {
        1 => {
            let v = parse_u32(arguments[0])?.try_into()?;
            Ok((v, v))
        }
        2 => {
            let l = parse_u32(arguments[0])?.try_into()?;
            let r = parse_u32(arguments[1])?.try_into()?;
            Ok((l, r))
        }
        _ => Err(MmlLineError::InvalidNumberOfEchoVolumeArguments),
    }
}

impl MetaData {
    fn new() -> Self {
        Self {
            title: None,
            game: None,
            date: None,
            composer: None,
            author: None,
            copyright: None,
            license: None,
            echo_buffer: EchoBuffer {
                edl: DEFAULT_EDL,
                fir: IDENTITY_FILTER,
                feedback: EchoFeedback::ZERO,
                echo_volume_l: EchoVolume::ZERO,
                echo_volume_r: EchoVolume::ZERO,
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
    disable_fir_filter_limit: bool,
    fir_pos: FilePosRange,
}

impl HeaderState {
    fn new() -> Self {
        Self {
            tempo_set: false,
            disable_fir_filter_limit: false,
            fir_pos: blank_file_range(),
            metadata: MetaData::new(),
        }
    }

    fn parse_header(
        &mut self,
        header: &str,
        value: &str,
        pos: &FilePosRange,
    ) -> Result<(), MmlLineError> {
        let to_option_string = || {
            if !value.is_empty() {
                Ok(Some(value.to_owned()))
            } else {
                Err(MmlLineError::NoValue)
            }
        };
        match header {
            "#Title" => self.metadata.title = to_option_string()?,
            "#Game" => self.metadata.game = to_option_string()?,
            "#Date" => self.metadata.date = to_option_string()?,
            "#Composer" => self.metadata.composer = to_option_string()?,
            "#Author" => self.metadata.author = to_option_string()?,
            "#Copyright" => self.metadata.copyright = to_option_string()?,
            "#License" => self.metadata.license = to_option_string()?,

            "#ZenLen" => self.metadata.zenlen = parse_u32(value)?.try_into()?,

            "#EchoLength" => {
                let echo_length = EchoLength::try_from(parse_u32(value)?)?;
                self.metadata.echo_buffer.edl = echo_length.to_edl();
            }

            "#FirFilter" => {
                self.metadata.echo_buffer.fir = parse_fir_filter_string(value)?;
                self.fir_pos = pos.clone();
            }
            "#DisableFirFilterLimit" => match value.is_empty() {
                true => self.disable_fir_filter_limit = true,
                false => return Err(MmlLineError::UnexpectedHeaderValue),
            },

            "#EchoFeedback" => match parse_i8wh(value) {
                Ok(i) => self.metadata.echo_buffer.feedback = i,
                Err(_) => return Err(MmlLineError::InvalidEchoFeedback),
            },

            "#EchoVolume" => {
                let (l, r) = parse_echo_volume(value)?;
                self.metadata.echo_buffer.echo_volume_l = l;
                self.metadata.echo_buffer.echo_volume_r = r;
            }

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
            Ok((header, value, pos)) => {
                if !map.contains_key(header) {
                    map.insert(header, value);

                    match header_state.parse_header(header, value, &pos) {
                        Ok(()) => (),
                        Err(e) => errors.push(ErrorWithPos(pos, e)),
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

    if !header_state.disable_fir_filter_limit {
        if let Err(e) = header_state.metadata.echo_buffer.test_fir_gain() {
            errors.push(ErrorWithPos(header_state.fir_pos, e.into()));
        }
    }

    if errors.is_empty() {
        Ok(header_state.metadata)
    } else {
        Err(errors)
    }
}
