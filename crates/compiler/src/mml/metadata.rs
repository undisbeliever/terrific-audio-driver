//! MML metadata parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::bytecode::Transpose;
use crate::echo::{
    parse_fir_filter_string, EchoBuffer, EchoEdl, EchoFeedback, EchoLength, EchoVolume,
    IDENTITY_FILTER,
};
use crate::errors::{ErrorWithPos, MmlLineError, ValueError};
use crate::file_pos::{blank_file_range, Line};
use crate::invert_flags::{parse_invert_flag_arguments, InvertFlags};
use crate::notes::KeySignature;
use crate::number_parsing::parse_i32_allow_zero;
use crate::songs::MetaData;
use crate::time::{Bpm, TickClock, ZenLen, DEFAULT_BPM, DEFAULT_ZENLEN};
use crate::value_newtypes::{parse_i8wh, I8WithByteHexValueNewType};
use crate::{spc_file_export, FilePosRange, SignedValueNewType};

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalSettings {
    pub zenlen: ZenLen,
    pub channel_transpose: Transpose,
    pub signature: KeySignature,
    pub old_transpose: bool,
}

impl Default for GlobalSettings {
    fn default() -> Self {
        Self {
            zenlen: DEFAULT_ZENLEN,
            channel_transpose: Transpose::new(0),
            signature: KeySignature::default(),
            old_transpose: false,
        }
    }
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
                max_edl: EchoEdl::ZERO,
                edl: EchoEdl::ZERO,
                fir: IDENTITY_FILTER,
                feedback: EchoFeedback::ZERO,
                echo_volume_l: EchoVolume::ZERO,
                echo_volume_r: EchoVolume::ZERO,
                invert: InvertFlags::default(),
            },
            tick_clock: DEFAULT_BPM.to_tick_clock().unwrap(),
            spc_song_length: None,
            spc_fadeout_millis: None,
            mml_settings: GlobalSettings::default(),
        }
    }

    pub fn blank_sfx_metadata() -> Self {
        Self {
            tick_clock: TickClock::SFX_TICK_CLOCK,
            ..Self::new()
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Header {
    Title,
    Game,
    Date,
    Composer,
    Author,
    Copyright,
    License,
    ZenLen,
    Transpose,
    KeySignature,
    OldTranspose,
    MaxEchoLength,
    EchoLength,
    FirFilter,
    DisableFirFilterLimit,
    EchoFeedback,
    EchoVolume,
    EchoInvert,
    Tempo,
    Timer,
    SpcSongLength,
    SpcFadeout,
}

fn match_header(header: &str) -> Option<Header> {
    match header {
        "#Title" | "#title" => Some(Header::Title),
        "#Game" | "#game" => Some(Header::Game),
        "#Date" | "#date" => Some(Header::Date),
        "#Composer" | "#composer" => Some(Header::Composer),
        "#Author" | "#author" => Some(Header::Author),
        "#Copyright" | "#copyright" => Some(Header::Copyright),
        "#License" | "#license" => Some(Header::License),

        "#ZenLen" | "#Zenlen" | "#zenLen" | "#zenlen" => Some(Header::ZenLen),
        "#Transpose" | "#transpose" => Some(Header::Transpose),
        "#KeySignature" | "#keySignature" | "#keysignature" => Some(Header::KeySignature),
        "#OldTranspose" | "#oldTranspose" | "#oldtranspose" => Some(Header::OldTranspose),

        "#MaxEchoLength" | "#maxEchoLength" | "#maxecholength" => Some(Header::MaxEchoLength),
        "#EchoLength" | "#echoLength" | "#echolength" => Some(Header::EchoLength),
        "#FirFilter" | "#firFilter" | "#firfilter" => Some(Header::FirFilter),
        "#DisableFirFilterLimit" | "#disableFirFilterLimit" | "#disablefirfilterlimit" => {
            Some(Header::DisableFirFilterLimit)
        }
        "#EchoFeedback" | "#echoFeedback" | "#echofeedback" => Some(Header::EchoFeedback),
        "#EchoVolume" | "#echoVolume" | "#echovolume" => Some(Header::EchoVolume),
        "#EchoInvert" | "#echoInvert" | "#echoinvert" => Some(Header::EchoInvert),
        "#Tempo" | "#tempo" => Some(Header::Tempo),
        "#Timer" | "#timer" => Some(Header::Timer),
        "#SpcSongLength" | "#spcSongLength" | "#spcsonglength" => Some(Header::SpcSongLength),
        "#SpcFadeout" | "#spcFadeout" | "#spcfadeout" => Some(Header::SpcFadeout),
        _ => None,
    }
}

struct HeaderState {
    metadata: MetaData,
    tempo_set: bool,
    disable_fir_filter_limit: bool,
    fir_pos: FilePosRange,
    max_edl_set: bool,
    edl_pos: FilePosRange,
}

impl HeaderState {
    fn new() -> Self {
        Self {
            tempo_set: false,
            disable_fir_filter_limit: false,
            fir_pos: blank_file_range(),
            max_edl_set: false,
            edl_pos: blank_file_range(),
            metadata: MetaData::new(),
        }
    }

    fn parse_header(
        &mut self,
        header: Header,
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
            Header::Title => self.metadata.title = to_option_string()?,
            Header::Game => self.metadata.game = to_option_string()?,
            Header::Date => self.metadata.date = to_option_string()?,
            Header::Composer => self.metadata.composer = to_option_string()?,
            Header::Author => self.metadata.author = to_option_string()?,
            Header::Copyright => self.metadata.copyright = to_option_string()?,
            Header::License => self.metadata.license = to_option_string()?,

            Header::ZenLen => self.metadata.mml_settings.zenlen = parse_u32(value)?.try_into()?,
            Header::Transpose => {
                self.metadata.mml_settings.channel_transpose =
                    parse_i32_allow_zero(value, &Transpose::MISSING_SIGN_ERROR)?.try_into()?
            }

            Header::OldTranspose => self.metadata.mml_settings.old_transpose = true,

            Header::KeySignature => {
                let mut signature = KeySignature::default();

                for v in value.split(',') {
                    signature = signature.parse_signature_changes(v.trim())?;
                }
                self.metadata.mml_settings.signature = signature;
            }

            Header::MaxEchoLength => {
                let echo_length = EchoLength::try_from(parse_u32(value)?)?;
                self.metadata.echo_buffer.max_edl = echo_length.to_edl();
                self.max_edl_set = true;
            }
            Header::EchoLength => {
                let echo_length = EchoLength::try_from(parse_u32(value)?)?;
                self.metadata.echo_buffer.edl = echo_length.to_edl();
                self.edl_pos = pos.clone();
            }

            Header::FirFilter => {
                self.metadata.echo_buffer.fir = parse_fir_filter_string(value)?;
                self.fir_pos = pos.clone();
            }
            Header::DisableFirFilterLimit => match value.is_empty() {
                true => self.disable_fir_filter_limit = true,
                false => return Err(MmlLineError::UnexpectedHeaderValue),
            },

            Header::EchoFeedback => match parse_i8wh(value) {
                Ok(i) => self.metadata.echo_buffer.feedback = i,
                Err(_) => return Err(MmlLineError::InvalidEchoFeedback),
            },

            Header::EchoVolume => {
                let (l, r) = parse_echo_volume(value)?;
                self.metadata.echo_buffer.echo_volume_l = l;
                self.metadata.echo_buffer.echo_volume_r = r;
            }

            Header::EchoInvert => {
                let args: Vec<_> = value.split_ascii_whitespace().collect();
                self.metadata.echo_buffer.invert = parse_invert_flag_arguments(&args)?;
            }

            Header::Tempo => {
                if self.tempo_set {
                    return Err(MmlLineError::CannotSetTempo);
                }
                self.tempo_set = true;

                let bpm = Bpm::try_from(parse_u32(value)?)?;
                self.metadata.tick_clock = bpm.to_tick_clock()?;
            }
            Header::Timer => {
                if self.tempo_set {
                    return Err(MmlLineError::CannotSetTimer);
                }
                self.tempo_set = true;
                self.metadata.tick_clock = parse_u32(value)?.try_into()?;
            }
            Header::SpcSongLength => match value.parse() {
                Ok(i) => {
                    if i > spc_file_export::MAX_SONG_LENGTH {
                        return Err(MmlLineError::InvalidSpcSongLength);
                    }
                    self.metadata.spc_song_length = Some(i)
                }
                Err(_) => return Err(MmlLineError::InvalidSpcSongLength),
            },
            Header::SpcFadeout => match value.parse() {
                Ok(i) => {
                    if i > spc_file_export::MAX_FADEOUT_MILLIS {
                        return Err(MmlLineError::InvalidSpcFadeout);
                    }
                    self.metadata.spc_fadeout_millis = Some(i)
                }
                Err(_) => return Err(MmlLineError::InvalidSpcSongLength),
            },
        }

        Ok(())
    }
}

pub fn parse_headers(lines: Vec<Line>) -> Result<MetaData, Vec<ErrorWithPos<MmlLineError>>> {
    use std::collections::hash_map::Entry;

    let mut errors = Vec::new();

    let mut map: HashMap<Header, u32> = HashMap::with_capacity(lines.len());

    let mut header_state = HeaderState::new();

    for line in &lines {
        match split_header_line(line) {
            Ok((header_str, value, pos)) => match match_header(header_str) {
                Some(header) => match map.entry(header) {
                    Entry::Vacant(e) => {
                        e.insert(line.position.line_number);

                        match header_state.parse_header(header, value, &pos) {
                            Ok(()) => (),
                            Err(e) => errors.push(ErrorWithPos(pos, e)),
                        }
                    }
                    Entry::Occupied(o) => {
                        errors.push(ErrorWithPos(
                            line.position.to_range_str_len(header_str),
                            MmlLineError::DuplicateHeader {
                                name: header_str.to_string(),
                                line: *o.get(),
                            },
                        ));
                    }
                },
                None => {
                    errors.push(ErrorWithPos(
                        line.position.to_range_str_len(header_str),
                        MmlLineError::UnknownHeader(header_str.to_string()),
                    ));
                }
            },
            Err(e) => {
                errors.push(ErrorWithPos(line.range(), e));
            }
        }
    }

    let eb = &mut header_state.metadata.echo_buffer;

    if header_state.max_edl_set {
        if eb.edl.as_u8() > eb.max_edl.as_u8() {
            errors.push(ErrorWithPos(
                header_state.edl_pos,
                ValueError::EchoEdlLargerThanMaxEdl {
                    edl: eb.edl,
                    max_edl: eb.max_edl,
                }
                .into(),
            ));
        }
    } else {
        eb.max_edl = eb.edl;
    }

    if !header_state.disable_fir_filter_limit {
        if let Err(e) = eb.test_fir_gain() {
            errors.push(ErrorWithPos(header_state.fir_pos, e.into()));
        }
    }

    if errors.is_empty() {
        Ok(header_state.metadata)
    } else {
        Err(errors)
    }
}
