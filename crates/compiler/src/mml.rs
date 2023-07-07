//! MML compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

// ::TODO write integration tests::

use crate::bytecode::{
    BcTerminator, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, InstrumentId, LoopCount,
    PitchOffsetPerTick, PlayNoteTicks, PortamentoVelocity, SubroutineId,
};
use crate::data::{self, UniqueNamesList};
use crate::driver_constants::{FIR_FILTER_SIZE, IDENTITY_FILTER, N_MUSIC_CHANNELS};
use crate::echo::{parse_fir_filter_string, EchoBuffer, EchoEdl, EchoLength, DEFAULT_EDL};
use crate::envelope::{Adsr, Gain};
use crate::errors::{
    ErrorWithLine, ErrorWithPos, IdentifierError, MmlChannelError, MmlCommandError,
    MmlCompileErrors, MmlLineError, ValueError,
};
use crate::mml_command_parser::{
    parse_mml_lines, IdentifierStr, Line, ManualVibrato, MmlCommand, MmlCommandWithPos, MpVibrato,
    PanCommand, PortamentoSpeed, VolumeCommand,
};
use crate::notes::{Note, SEMITONS_PER_OCTAVE};
use crate::pitch_table::PitchTable;
use crate::time::{Bpm, TickClock, TickCounter, ZenLen, DEFAULT_BPM, DEFAULT_ZENLEN};

pub use crate::mml_command_parser::{FilePos, MAX_MML_TEXT_LENGTH};

use std::cmp::max;
use std::collections::HashMap;
use std::ops::Range;

const FIRST_MUSIC_CHANNEL: char = 'A';
const MUSIC_CHANNEL_RANGE: Range<char> = 'A'..'F';

const _: () = assert!(
    MUSIC_CHANNEL_RANGE.end as usize - MUSIC_CHANNEL_RANGE.start as usize + 1 == N_MUSIC_CHANNELS
);

const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F"];

pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

mod identifier {
    use super::data::Name;
    use super::{IdentifierError, IdentifierStr};

    // An identifier is a name or a number
    // Storing the identifier as a string so a it can be hashed and compared without copying
    // the string contents.
    #[derive(Debug, Clone, Hash, Eq, PartialEq)]
    pub struct Identifier(String);

    impl Identifier {
        pub fn try_from_string(s: String) -> Result<Self, IdentifierError> {
            match s.chars().next() {
                Some(c) if c.is_ascii_digit() => Self::try_from_number(s),
                Some(_) => Self::try_from_name(s),
                None => Err(IdentifierError::Empty),
            }
        }

        pub(super) fn try_from_name(s: String) -> Result<Self, IdentifierError> {
            if Name::is_valid_name(&s) {
                Ok(Self(s))
            } else {
                Err(IdentifierError::InvalidName(s))
            }
        }

        pub(super) fn try_from_number(s: String) -> Result<Self, IdentifierError> {
            // Number identifier
            if s.chars().all(|c| c.is_ascii_digit()) {
                Ok(Self(s))
            } else {
                Err(IdentifierError::InvalidNumber(s))
            }
        }

        pub(super) fn as_ref(&self) -> IdentifierStr {
            IdentifierStr::from_str(&self.0)
        }

        pub fn as_str(&self) -> &str {
            &self.0
        }
    }
}

pub use identifier::Identifier;

use self::line_splitter::split_lines;

#[derive(Debug, PartialEq)]
pub struct MetaData {
    pub title: Option<String>,
    pub date: Option<String>,
    pub composer: Option<String>,
    pub author: Option<String>,
    pub copyright: Option<String>,
    pub license: Option<String>,

    pub echo_buffer: EchoBuffer,

    pub tick_clock: TickClock,

    zenlen: ZenLen,
}

#[derive(Debug, PartialEq)]
pub enum EnvelopeOverride {
    None,
    Adsr(Adsr),
    Gain(Gain),
}

#[derive(Debug, PartialEq)]
pub struct MmlInstrument {
    identifier: Identifier,

    line_number: u32,

    instrument_id: InstrumentId,

    first_note: Note,
    last_note: Note,

    envelope_override: EnvelopeOverride,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopPoint {
    pub bytecode_offset: usize,
    pub tick_counter: TickCounter,
}

#[derive(Debug, PartialEq)]
pub struct ChannelData {
    identifier: Identifier,

    bytecode: Vec<u8>,
    loop_point: Option<LoopPoint>,

    tick_counter: TickCounter,
    last_instrument: Option<usize>,

    // Some if this channel is a subroutine
    bc_subroutine: Option<SubroutineId>,
    // ::TODO add tick counter for line::
}

#[derive(Debug, PartialEq)]
pub struct MmlData {
    metadata: MetaData,
    subroutines: Vec<ChannelData>,
    channels: Vec<ChannelData>,
}

// ::TODO add list of subroutines and instruments for the GUI (separate from MmlData)::

impl ChannelData {
    pub fn identifier(&self) -> &Identifier {
        &self.identifier
    }
    pub fn bytecode(&self) -> &[u8] {
        &self.bytecode
    }
    pub fn loop_point(&self) -> Option<LoopPoint> {
        self.loop_point
    }
    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }
}

impl MmlData {
    pub fn metadata(&self) -> &MetaData {
        &self.metadata
    }
    pub fn subroutines(&self) -> &[ChannelData] {
        &self.subroutines
    }
    pub fn channels(&self) -> &[ChannelData] {
        &self.channels
    }
}

// MML Line Splitter
// =================

struct MmlLines<'a> {
    headers: Vec<Line<'a>>,
    instruments: Vec<(Identifier, Line<'a>)>,
    subroutines: Vec<(Identifier, Vec<Line<'a>>)>,
    channels: [Vec<Line<'a>>; N_MUSIC_CHANNELS],
}

/// MML line splitter
mod line_splitter {
    use crate::driver_constants::MAX_SUBROUTINES;

    use super::*;

    const COMMENT_CHAR: char = ';';

    fn trim_start_count_chars(s: &str) -> (&str, usize) {
        let mut char_count = 1;
        for (index, c) in s.char_indices() {
            if !c.is_whitespace() {
                return (&s[index..], char_count);
            }
            char_count += 1;
        }

        // All characters are whitespace
        ("", char_count)
    }

    // Assumes `s` has been trimmed.
    // Assumes `mml_test` length is < i32::MAX
    fn split_idstr_and_line(s: &str, line_number: u32) -> Option<(&str, Line)> {
        match s.split_once(|c: char| c.is_ascii_whitespace()) {
            Some((id, text)) => {
                let id_chars = id.chars().count() + 1;
                let (text, n_ws_chars) = trim_start_count_chars(text);

                Some((
                    id,
                    Line {
                        text,
                        position: FilePos {
                            line_number,
                            line_char: (id_chars + n_ws_chars).try_into().unwrap(),
                        },
                    },
                ))
            }
            None => None,
        }
    }

    // Assumes `mml_test` length is < i32::MAX
    fn split_id_and_line(
        s: &str,
        prefix: char,
        line_number: u32,
    ) -> Result<(Identifier, Line), MmlLineError> {
        match split_idstr_and_line(s, line_number) {
            Some((id_str, line)) => {
                let id_str = id_str.trim_start_matches(prefix);

                match Identifier::try_from_string(id_str.to_owned()) {
                    Ok(id) => Ok((id, line)),
                    Err(e) => Err(MmlLineError::InvalidIdentifier(e)),
                }
            }
            None => Err(MmlLineError::NoIdentifier(prefix)),
        }
    }

    pub(super) fn split_lines(
        mml_text: &str,
    ) -> Result<MmlLines, Vec<ErrorWithLine<MmlLineError>>> {
        let mut errors = Vec::new();

        if mml_text.len() > MAX_MML_TEXT_LENGTH {
            errors.push(ErrorWithLine(0, MmlLineError::MmlTooLarge(mml_text.len())));
            return Err(errors);
        }

        let mut headers = Vec::new();
        let mut instruments = Vec::new();
        let mut subroutines: Vec<(Identifier, Vec<Line>)> = Vec::new();
        let mut channels: [Vec<Line>; N_MUSIC_CHANNELS] = [
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ];

        let mut subroutine_map: HashMap<Identifier, usize> = HashMap::new();

        let mut line_no = 0;
        for line in mml_text.lines() {
            line_no += 1;

            let line = match line.split_once(COMMENT_CHAR) {
                Some((l, _comment)) => l,
                None => line,
            };
            let line = line.trim();

            match line.chars().next() {
                Some('#') => headers.push(Line {
                    text: line,
                    position: FilePos {
                        line_number: line_no,
                        line_char: 0,
                    },
                }),
                Some('@') => {
                    // instruments
                    match split_id_and_line(line, '@', line_no) {
                        Ok((id, line)) => instruments.push((id, line)),
                        Err(e) => errors.push(ErrorWithLine(line_no, e)),
                    }
                }
                Some('!') => {
                    // Subroutines
                    match split_id_and_line(line, '!', line_no) {
                        Ok((id, line)) => match subroutine_map.get(&id) {
                            Some(index) => {
                                subroutines[*index].1.push(line);
                            }
                            None => {
                                subroutine_map.insert(id.clone(), subroutines.len());
                                subroutines.push((id, vec![line]));
                            }
                        },
                        Err(e) => errors.push(ErrorWithLine(line_no, e)),
                    }
                }

                Some(c) if MUSIC_CHANNEL_RANGE.contains(&c) => {
                    // Music channels
                    match split_idstr_and_line(line, line_no) {
                        Some((id, line)) => {
                            if id.chars().all(|c| MUSIC_CHANNEL_RANGE.contains(&c)) {
                                // Each channel will only use the line once
                                let mut used = [true; N_MUSIC_CHANNELS];

                                for c in id.chars() {
                                    let index = u32::from(c) - u32::from(FIRST_MUSIC_CHANNEL);
                                    let index = usize::try_from(index).unwrap();

                                    if used[index] {
                                        channels[index].push(Line {
                                            text: line.text,
                                            position: line.position,
                                        });
                                        used[index] = false;
                                    }
                                }
                            } else {
                                let unknown = id
                                    .chars()
                                    .filter(|c| !MUSIC_CHANNEL_RANGE.contains(c))
                                    .collect();
                                errors.push(ErrorWithLine(
                                    line_no,
                                    MmlLineError::UnknownChannel(unknown),
                                ))
                            }
                        }
                        None => {
                            errors.push(ErrorWithLine(line_no, MmlLineError::MissingChannelText))
                        }
                    }
                }
                Some(_) => errors.push(ErrorWithLine(line_no, MmlLineError::CannotParseLine)),

                None => (),
            }
        }

        if subroutines.len() > MAX_SUBROUTINES {
            errors.insert(
                0,
                ErrorWithLine(0, MmlLineError::TooManySubroutines(subroutines.len())),
            );
        }

        if errors.is_empty() {
            Ok(MmlLines {
                headers,
                instruments,
                subroutines,
                channels,
            })
        } else {
            Err(errors)
        }
    }
}

fn one_argument(iter: std::str::SplitWhitespace) -> Option<&str> {
    let mut iter = iter;

    let a = iter.next();
    let after_a = iter.next();

    match (a, after_a) {
        (Some(a), None) => Some(a),
        _ => None,
    }
}

fn four_arguments(iter: std::str::SplitWhitespace) -> Option<(&str, &str, &str, &str)> {
    let mut iter = iter;

    let a = iter.next();
    let b = iter.next();
    let c = iter.next();
    let d = iter.next();
    let after = iter.next();

    match (a, b, c, d, after) {
        (Some(a), Some(b), Some(c), Some(d), None) => Some((a, b, c, d)),
        _ => None,
    }
}

//
// Header
// ======

fn split_header_line(line: &str) -> Result<(&str, &str), MmlLineError> {
    let (header, value) = match line.split_once(char::is_whitespace) {
        Some(s) => s,
        None => {
            return Err(MmlLineError::NoHeader);
        }
    };

    let value = value.trim();

    if header.is_empty() {
        return Err(MmlLineError::NoHeader);
    }
    if value.is_empty() {
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

struct HeaderState {
    zenlen: Option<ZenLen>,
    tick_clock: Option<TickClock>,
    echo_edl: Option<EchoEdl>,
    echo_fir: Option<[i8; FIR_FILTER_SIZE]>,
    echo_feedback: Option<i8>,
    echo_volume: Option<i8>,
}

impl HeaderState {
    fn parse_header(&mut self, header: &str, value: &str) -> Result<(), MmlLineError> {
        match header {
            "#ZenLen" => self.zenlen = Some(parse_u32(value)?.try_into()?),

            "#EchoLength" => {
                let echo_length = EchoLength::try_from(parse_u32(value)?)?;
                self.echo_edl = Some(echo_length.to_edl());
            }

            "#FirFilter" => self.echo_fir = Some(parse_fir_filter_string(value)?),

            "#EchoFeedback" => match value.parse() {
                Ok(i) => self.echo_feedback = Some(i),
                Err(_) => return Err(MmlLineError::InvalidEchoFeedback),
            },

            "#EchoVolume" => match value.parse() {
                Ok(i) => self.echo_volume = Some(i),
                Err(_) => return Err(MmlLineError::InvalidEchoVolume),
            },

            "#Tempo" => {
                if self.tick_clock.is_some() {
                    return Err(MmlLineError::CannotSetTempo);
                }
                let bpm = Bpm::try_from(parse_u32(value)?)?;
                self.tick_clock = Some(bpm.to_tick_clock()?);
            }
            "#Timer" => {
                if self.tick_clock.is_some() {
                    return Err(MmlLineError::CannotSetTimer);
                }
                self.tick_clock = Some(parse_u32(value)?.try_into()?);
            }

            _ => (),
        }

        Ok(())
    }
}

fn parse_headers(lines: Vec<Line>) -> Result<MetaData, Vec<ErrorWithLine<MmlLineError>>> {
    let mut errors = Vec::new();

    let mut map: HashMap<&str, &str> = HashMap::with_capacity(lines.len());

    let mut header_state = HeaderState {
        zenlen: None,
        tick_clock: None,
        echo_edl: None,
        echo_fir: None,
        echo_feedback: None,
        echo_volume: None,
    };

    for line in lines {
        let line_no = line.position.line_number;

        match split_header_line(line.text) {
            Ok((header, value)) => {
                if !map.contains_key(header) {
                    map.insert(header, value);
                } else {
                    errors.push(ErrorWithLine(
                        line_no,
                        MmlLineError::DuplicateHeader(header.to_string()),
                    ));
                }
                match header_state.parse_header(header, value) {
                    Ok(()) => (),
                    Err(e) => errors.push(ErrorWithLine(line_no, e)),
                }
            }
            Err(e) => {
                errors.push(ErrorWithLine(line_no, e));
            }
        }
    }
    let map = map;

    let get = |name: &str| -> Option<String> { map.get(name).copied().map(str::to_owned) };

    if errors.is_empty() {
        Ok(MetaData {
            title: get("#Title"),
            date: get("#Date"),
            composer: get("#Composer"),
            author: get("#Author"),
            copyright: get("#Copyright"),
            license: get("#License"),
            echo_buffer: EchoBuffer {
                edl: header_state.echo_edl.unwrap_or(DEFAULT_EDL),
                fir: header_state.echo_fir.unwrap_or(IDENTITY_FILTER),
                feedback: header_state.echo_feedback.unwrap_or(0),
                echo_volume: header_state.echo_volume.unwrap_or(0),
            },
            tick_clock: header_state
                .tick_clock
                .unwrap_or(DEFAULT_BPM.to_tick_clock().unwrap()),
            zenlen: header_state.zenlen.unwrap_or(DEFAULT_ZENLEN),
        })
    } else {
        Err(errors)
    }
}

//
// Instruments
// ===========

fn parse_instrument(
    id: Identifier,
    line: &Line,
    inst_map: &UniqueNamesList<data::Instrument>,
) -> Result<MmlInstrument, MmlLineError> {
    let mut args = line.text.split_whitespace();

    let inst_name = match args.next() {
        Some(s) => s,
        None => return Err(MmlLineError::NoInstrument),
    };

    let mut envelope_override = EnvelopeOverride::None;

    match args.next() {
        None => {}
        Some("adsr") => match four_arguments(args) {
            Some((a, b, c, d)) => match Adsr::try_from_strs(a, b, c, d) {
                Ok(a) => {
                    envelope_override = EnvelopeOverride::Adsr(a);
                }
                Err(e) => return Err(MmlLineError::InvalidAdsr(e)),
            },
            _ => return Err(MmlLineError::ExpectedFourAdsrArguments),
        },
        Some("gain") => match one_argument(args) {
            Some(g) => match Gain::try_from(g) {
                Ok(g) => {
                    envelope_override = EnvelopeOverride::Gain(g);
                }
                Err(e) => return Err(MmlLineError::InvalidGain(e)),
            },
            None => return Err(MmlLineError::ExpectedOneGainArgument),
        },
        Some(unknown) => return Err(MmlLineError::UnknownInstrumentArgument(unknown.to_owned())),
    }

    let (instrument_id, inst) = match inst_map.get_with_index(inst_name) {
        Some((inst_id, inst)) => (inst_id, inst),
        None => return Err(MmlLineError::CannotFindInstrument(inst_name.to_owned())),
    };

    match envelope_override {
        EnvelopeOverride::None => {}
        EnvelopeOverride::Adsr(adsr) => {
            if inst.adsr == Some(adsr) {
                envelope_override = EnvelopeOverride::None;
            }
        }
        EnvelopeOverride::Gain(gain) => {
            if inst.gain == Some(gain) {
                envelope_override = EnvelopeOverride::None;
            }
        }
    }

    Ok(MmlInstrument {
        identifier: id,
        line_number: line.position.line_number,
        instrument_id: InstrumentId::new(instrument_id),
        first_note: Note::first_note_for_octave(inst.first_octave),
        last_note: Note::last_note_for_octave(inst.last_octave),
        envelope_override,
    })
}

fn parse_instruments(
    instrument_lines: Vec<(Identifier, Line)>,
    inst_map: &UniqueNamesList<data::Instrument>,
) -> (Vec<MmlInstrument>, Vec<ErrorWithLine<MmlLineError>>) {
    let mut out = Vec::with_capacity(instrument_lines.len());
    let mut errors = Vec::new();

    for (id, line) in instrument_lines {
        match parse_instrument(id, &line, inst_map) {
            Ok(i) => out.push(i),
            Err(e) => errors.push(ErrorWithLine(line.position.line_number, e)),
        }
    }

    (out, errors)
}

fn build_instrument_map(
    instruments: &Vec<MmlInstrument>,
) -> Result<HashMap<IdentifierStr, usize>, Vec<ErrorWithLine<MmlLineError>>> {
    let mut out = HashMap::with_capacity(instruments.len());
    let mut errors = Vec::new();

    for (i, inst) in instruments.iter().enumerate() {
        if out.insert(inst.identifier.as_ref(), i).is_some() {
            errors.push(ErrorWithLine(
                inst.line_number,
                MmlLineError::DuplicateInstrumentName(inst.identifier.as_str().to_owned()),
            ));
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

// MML Bytecode Generator
// ======================

#[derive(PartialEq)]
enum MpState {
    Disabled,
    Manual,
    Mp(MpVibrato),
}

struct MmlBytecodeGenerator<'a> {
    pitch_table: &'a PitchTable,
    instruments: &'a Vec<MmlInstrument>,
    subroutines: Option<&'a Vec<ChannelData>>,

    bc: Bytecode,

    line_tick_counts: Vec<(u32, TickCounter)>,

    instrument: Option<usize>,
    prev_slurred_note: Option<Note>,

    mp: MpState,
    vibrato: Option<ManualVibrato>,

    loop_point: Option<LoopPoint>,

    show_missing_set_instrument_error: bool,
}

impl MmlBytecodeGenerator<'_> {
    fn new<'a>(
        pitch_table: &'a PitchTable,
        instruments: &'a Vec<MmlInstrument>,
        subroutines: Option<&'a Vec<ChannelData>>,
        is_subroutine: bool,
    ) -> MmlBytecodeGenerator<'a> {
        MmlBytecodeGenerator {
            pitch_table,
            instruments,
            subroutines,
            bc: Bytecode::new(is_subroutine, false),
            line_tick_counts: Vec::new(),
            instrument: None,
            prev_slurred_note: None,
            mp: MpState::Disabled,
            vibrato: None,
            loop_point: None,
            show_missing_set_instrument_error: !is_subroutine,
        }
    }

    fn instrument_from_index(&self, i: usize) -> Result<&MmlInstrument, MmlCommandError> {
        match self.instruments.get(i) {
            Some(inst) => Ok(inst),
            None => Err(MmlCommandError::CannotFindInstrument),
        }
    }

    fn test_note(&mut self, note: Note) -> Result<(), MmlCommandError> {
        match self.instrument {
            Some(i) => {
                let inst = self.instrument_from_index(i)?;
                if note >= inst.first_note && note <= inst.last_note {
                    Ok(())
                } else {
                    Err(MmlCommandError::NoteOutOfRange(
                        note,
                        inst.first_note,
                        inst.last_note,
                    ))
                }
            }
            None => {
                if self.show_missing_set_instrument_error {
                    self.show_missing_set_instrument_error = false;
                    Err(MmlCommandError::CannotPlayNoteBeforeSettingInstrument)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn calculate_vibrato_for_note(
        &self,
        mp: &MpVibrato,
        note: Note,
    ) -> Result<ManualVibrato, MmlCommandError> {
        if mp.depth_in_cents == 0 {
            return Err(MmlCommandError::MpDepthZero);
        }
        let inst = match self.instrument {
            Some(index) => self.instrument_from_index(index)?,
            None => return Err(MmlCommandError::CannotUseMpWithoutInstrument),
        };

        let pitch = self.pitch_table.pitch_for_note(inst.instrument_id, note);

        // Calculate the minimum and maximum pitches of the vibrato.
        // This produces more accurate results when cents is very large (ie, 400)
        let pow = f64::from(mp.depth_in_cents) / f64::from(SEMITONS_PER_OCTAVE as u32 * 100);
        let p1 = f64::from(pitch) * 2.0_f64.powf(-pow);
        let p2 = f64::from(pitch) * 2.0_f64.powf(pow);

        let qwt = u32::from(mp.quarter_wavelength_ticks.as_u8());

        let po_per_tick = f64::round((p2 - p1) / f64::from(qwt * 2));
        let po_per_tick = if po_per_tick > 1.0 { po_per_tick } else { 1.0 };

        if po_per_tick > u32::MAX.into() {
            return Err(MmlCommandError::MpPitchOffsetTooLarge(u32::MAX));
        }
        let po_per_tick = po_per_tick as u32;

        match po_per_tick.try_into() {
            Ok(po) => Ok(ManualVibrato {
                quarter_wavelength_ticks: mp.quarter_wavelength_ticks,
                pitch_offset_per_tick: po,
            }),
            Err(_) => Err(MmlCommandError::MpPitchOffsetTooLarge(po_per_tick)),
        }
    }

    fn split_play_note_length(
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(PlayNoteTicks, TickCounter), ValueError> {
        let l = length.value();

        if !is_slur && l <= BcTicksKeyOff::MAX {
            return Ok((
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(l)?),
                TickCounter::new(0),
            ));
        }

        // The play_note instruction requires keyoff.
        let last_min = if is_slur {
            BcTicksNoKeyOff::MIN
        } else {
            BcTicksKeyOff::MIN
        };
        const MAX: u32 = BcTicksNoKeyOff::MAX;

        let pn = {
            if l <= MAX {
                l
            } else if l >= MAX + last_min {
                MAX
            } else {
                MAX - 1
            }
        };

        Ok((
            PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(pn)?),
            TickCounter::new(l - pn),
        ))
    }

    fn play_note_with_mp(
        &mut self,
        note: Note,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), MmlCommandError> {
        let (pn_length, rest) = Self::split_play_note_length(length, is_slur)?;

        self.test_note(note)?;

        self.prev_slurred_note = if is_slur { Some(note) } else { None };

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_length);
            }
            MpState::Disabled => {
                const POPT: PitchOffsetPerTick = PitchOffsetPerTick::new(0);

                let vibrato_disabled = match self.vibrato {
                    None => true,
                    Some(v) => v.pitch_offset_per_tick == POPT,
                };

                if vibrato_disabled {
                    self.bc.play_note(note, pn_length);
                } else {
                    self.bc
                        .set_vibrato_depth_and_play_note(POPT, note, pn_length);

                    if let Some(v) = &mut self.vibrato {
                        v.pitch_offset_per_tick = POPT;
                    }
                }
            }
            MpState::Mp(mp) => {
                let cv = self.calculate_vibrato_for_note(mp, note)?;

                if self.vibrato == Some(cv) {
                    self.bc.play_note(note, pn_length);
                } else {
                    match self.vibrato {
                        Some(sv) if sv.quarter_wavelength_ticks == cv.quarter_wavelength_ticks => {
                            self.bc.set_vibrato_depth_and_play_note(
                                cv.pitch_offset_per_tick,
                                note,
                                pn_length,
                            );
                        }
                        _ => {
                            self.bc
                                .set_vibrato(cv.pitch_offset_per_tick, cv.quarter_wavelength_ticks);
                            self.bc.play_note(note, pn_length);
                        }
                    }

                    self.vibrato = Some(cv);
                }
            }
        }

        self.rest_after_play_note(rest, is_slur)
    }

    fn rest_after_play_note(
        &mut self,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), MmlCommandError> {
        if length.is_zero() {
            return Ok(());
        }
        if is_slur {
            return self.rest(length);
        }

        self.prev_slurred_note = None;

        let mut remaining_ticks = length.value();

        const MAX_REST: u32 = BcTicksNoKeyOff::MAX;
        const MAX_FINAL_REST: u32 = BcTicksKeyOff::MAX;
        const MIN_FINAL_REST: u32 = BcTicksKeyOff::MIN;
        const _: () = assert!(MIN_FINAL_REST > 1);

        while remaining_ticks > MAX_FINAL_REST {
            let l = if remaining_ticks >= MAX_REST + MIN_FINAL_REST {
                MAX_REST
            } else {
                MAX_REST - 1
            };
            self.bc.rest(BcTicksNoKeyOff::try_from(l).unwrap());
            remaining_ticks -= l;
        }

        self.bc
            .rest_keyoff(BcTicksKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    fn rest(&mut self, length: TickCounter) -> Result<(), MmlCommandError> {
        let mut remaining_ticks = length.value();

        let rest_length = BcTicksNoKeyOff::try_from(BcTicksNoKeyOff::MAX).unwrap();
        const _: () = assert!(BcTicksNoKeyOff::MIN == 1);

        while remaining_ticks > rest_length.ticks() {
            self.bc.rest(rest_length);
            remaining_ticks -= rest_length.ticks();
        }

        self.bc.rest(BcTicksNoKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn portamento(
        &mut self,
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        total_length: TickCounter,
        delay_length: TickCounter,
        tie_length: TickCounter,
    ) -> Result<(), MmlCommandError> {
        assert!(delay_length < total_length);

        self.test_note(note1)?;
        self.test_note(note2)?;

        // Play note1 (if required)
        let note1_length = {
            if self.prev_slurred_note != Some(note1) {
                let note_1_length = max(TickCounter::new(1), delay_length);
                let (pn_length, rest) = Self::split_play_note_length(note_1_length, true)?;

                self.bc.play_note(note1, pn_length);
                self.rest_after_play_note(rest, true)?;
                note_1_length
            } else if !delay_length.is_zero() {
                self.rest(delay_length)?;
                delay_length
            } else {
                TickCounter::new(0)
            }
        };

        let portamento_length =
            TickCounter::new(total_length.value().wrapping_sub(note1_length.value()));
        if portamento_length.is_zero() {
            return Err(MmlCommandError::PortamentoDelayTooLong);
        }

        let velocity = match speed_override {
            Some(speed) => {
                if note1 < note2 {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                }
            }
            None => {
                let inst = match self.instrument {
                    Some(index) => self.instrument_from_index(index)?,
                    None => return Err(MmlCommandError::PortamentoRequiresInstrument),
                };
                let p1: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note1)
                    .into();
                let p2: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note2)
                    .into();

                let ticks = i32::try_from(portamento_length.value()).unwrap();

                (p2 - p1) / ticks
            }
        };
        let velocity = PortamentoVelocity::try_from(velocity)?;

        let (p_length, p_rest) =
            Self::split_play_note_length(tie_length + portamento_length, is_slur)?;
        self.bc.portamento(note2, velocity, p_length);

        self.prev_slurred_note = if is_slur { Some(note2) } else { None };

        self.rest_after_play_note(p_rest, is_slur)
    }

    fn broken_chord(
        &mut self,
        notes: &[Note],
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    ) -> Result<(), MmlCommandError> {
        self.prev_slurred_note = None;

        if notes.is_empty() {
            return Err(MmlCommandError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(MmlCommandError::TooManyNotesInBrokenChord(notes.len()));
        }
        let n_notes: u32 = notes.len().try_into().unwrap();

        for n in notes {
            self.test_note(*n)?;
        }

        let expected_tick_counter = self.bc.get_tick_counter() + total_length;

        let total_ticks = total_length.value();

        // Number of ticks in the last note played outside the loop (if any).
        let mut last_note_ticks = total_ticks % note_length.ticks();

        // If tie is true, a keyoff is required after the loop.
        if note_length.is_slur() && last_note_ticks == 0 {
            last_note_ticks += note_length.ticks();
        }

        if last_note_ticks != 0 && last_note_ticks < BcTicksKeyOff::MIN {
            last_note_ticks = BcTicksKeyOff::MIN;
        }
        let last_note_ticks = last_note_ticks;

        let notes_in_loop = (total_ticks - last_note_ticks) / note_length.ticks();

        let break_point = usize::try_from(notes_in_loop % n_notes).unwrap();
        let has_break_point: bool = break_point != 0;

        let n_loops = (notes_in_loop / n_notes) + u32::from(has_break_point);

        if n_loops < 2 {
            return Err(MmlCommandError::BrokenChordTotalLengthTooShort);
        }

        let n_loops = LoopCount::try_from(n_loops)?;

        self.bc.start_loop(Some(n_loops))?;

        for (i, n) in notes.iter().enumerate() {
            if i == break_point && i != 0 {
                self.bc.skip_last_loop()?;
            }
            self.bc.play_note(*n, note_length);
        }

        self.bc.end_loop(None)?;

        if last_note_ticks > 0 {
            // The last note to play is always a keyoff note.
            self.bc.play_note(
                notes[break_point],
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(last_note_ticks)?),
            )
        }

        if self.bc.get_tick_counter() != expected_tick_counter {
            return Err(MmlCommandError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument(&mut self, inst_index: usize) -> Result<(), MmlCommandError> {
        if self.instrument == Some(inst_index) {
            return Ok(());
        }
        let inst = self.instrument_from_index(inst_index)?;
        let old_inst = match self.instrument {
            Some(i) => Some(self.instrument_from_index(i)?),
            None => None,
        };

        let i_id = inst.instrument_id;

        match old_inst {
            Some(old) if old.instrument_id == i_id => {
                // Instrument_id unchanged
                if inst.envelope_override != old.envelope_override {
                    match inst.envelope_override {
                        EnvelopeOverride::None => self.bc.set_instrument(i_id),
                        EnvelopeOverride::Adsr(adsr) => self.bc.set_adsr(adsr),
                        EnvelopeOverride::Gain(gain) => self.bc.set_gain(gain),
                    }
                }
            }
            _ => match inst.envelope_override {
                EnvelopeOverride::None => self.bc.set_instrument(i_id),
                EnvelopeOverride::Adsr(adsr) => self.bc.set_instrument_and_adsr(i_id, adsr),
                EnvelopeOverride::Gain(gain) => self.bc.set_instrument_and_gain(i_id, gain),
            },
        }

        self.instrument = Some(inst_index);

        Ok(())
    }

    fn call_subroutine(&mut self, s_id: SubroutineId) -> Result<(), MmlCommandError> {
        let sub: &ChannelData = match self.subroutines {
            Some(s) => match s.get(s_id.as_usize()) {
                Some(s) => s,
                None => return Err(MmlCommandError::CannotFindSubroutine),
            },
            None => return Err(MmlCommandError::CannotFindSubroutine),
        };

        // Calling a subroutine disables manual vibrato
        self.vibrato = None;
        if self.mp == MpState::Manual {
            self.mp = MpState::Disabled;
        }

        if let Some(inst) = sub.last_instrument {
            self.instrument = Some(inst);
        }

        self.bc.call_subroutine(s_id)?;

        Ok(())
    }

    fn set_manual_vibrato(&mut self, v: Option<ManualVibrato>) {
        self.mp = MpState::Manual;
        match v {
            Some(v) => {
                self.vibrato = Some(v);
                self.bc
                    .set_vibrato(v.pitch_offset_per_tick, v.quarter_wavelength_ticks);
            }
            None => {
                self.vibrato = None;
                self.bc.disable_vibrato();
            }
        }
    }

    fn process_command(
        &mut self,
        command: &MmlCommand,
        pos: &FilePos,
    ) -> Result<(), MmlCommandError> {
        match command {
            MmlCommand::NoCommand => (),

            MmlCommand::NewLine => {
                self.line_tick_counts
                    .push((pos.line_number, self.bc.get_tick_counter()));
            }

            &MmlCommand::SetLoopPoint => {
                if self.loop_point.is_some() {
                    return Err(MmlCommandError::LoopPointAlreadySet);
                }
                self.loop_point = Some(LoopPoint {
                    bytecode_offset: self.bc.get_bytecode_len(),
                    tick_counter: self.bc.get_tick_counter(),
                })
            }

            &MmlCommand::SetInstrument(inst_index) => {
                self.set_instrument(inst_index)?;
            }

            &MmlCommand::CallSubroutine(s_id) => {
                self.call_subroutine(s_id)?;
            }

            &MmlCommand::SetManualVibrato(v) => {
                self.set_manual_vibrato(v);
            }

            &MmlCommand::SetMpVibrato(mp) => match mp {
                Some(mp) => self.mp = MpState::Mp(mp),
                None => self.mp = MpState::Disabled,
            },

            &MmlCommand::Rest(length) => {
                self.rest(length)?;
            }

            &MmlCommand::PlayNote {
                note,
                length,
                is_slur,
            } => {
                self.play_note_with_mp(note, length, is_slur)?;
            }

            &MmlCommand::PlayQuantizedNote {
                note,
                length: _,
                key_on_length,
                rest,
            } => {
                self.play_note_with_mp(note, key_on_length, false)?;
                self.rest(rest)?;
            }

            &MmlCommand::Portamento {
                note1,
                note2,
                is_slur,
                speed_override,
                total_length,
                delay_length,
                tie_length,
            } => {
                self.portamento(
                    note1,
                    note2,
                    is_slur,
                    speed_override,
                    total_length,
                    delay_length,
                    tie_length,
                )?;
            }

            MmlCommand::BrokenChord {
                notes,
                total_length,
                note_length,
            } => {
                self.broken_chord(notes, *total_length, *note_length)?;
            }

            MmlCommand::StartLoop => {
                self.bc.start_loop(None)?;
            }

            MmlCommand::SkipLastLoop => {
                self.bc.skip_last_loop()?;
            }

            &MmlCommand::EndLoop(loop_count) => {
                self.bc.end_loop(Some(loop_count))?;
            }

            &MmlCommand::ChangePanAndOrVolume(pan, volume) => match (pan, volume) {
                (Some(PanCommand::Absolute(p)), Some(VolumeCommand::Absolute(v))) => {
                    self.bc.set_pan_and_volume(p, v);
                }
                (pan, volume) => {
                    match volume {
                        Some(VolumeCommand::Absolute(v)) => self.bc.set_volume(v),
                        Some(VolumeCommand::Relative(v)) => self.bc.adjust_volume(v),
                        None => (),
                    }
                    match pan {
                        Some(PanCommand::Absolute(p)) => self.bc.set_pan(p),
                        Some(PanCommand::Relative(p)) => self.bc.adjust_pan(p),
                        None => (),
                    }
                }
            },

            &MmlCommand::SetEcho(e) => {
                if e {
                    self.bc.enable_echo();
                } else {
                    self.bc.disable_echo();
                }
            }

            &MmlCommand::SetSongTempo(bpm) => {
                self.bc.set_song_tick_clock(bpm.to_tick_clock()?)?;
            }
            &MmlCommand::SetSongTickClock(tick_clock) => {
                self.bc.set_song_tick_clock(tick_clock)?;
            }
        }

        Ok(())
    }
}

fn process_mml_commands(
    commands: &Vec<MmlCommandWithPos>,
    last_pos: FilePos,
    identifier: Identifier,
    subroutine_index: Option<u8>,
    pitch_table: &PitchTable,
    instruments: &Vec<MmlInstrument>,
    subroutines: Option<&Vec<ChannelData>>,
) -> Result<ChannelData, Vec<ErrorWithPos<MmlCommandError>>> {
    let mut errors = Vec::new();

    // Cannot have subroutine_index and subroutines list at the same time.
    if subroutine_index.is_some() {
        assert!(
            subroutines.is_none(),
            "Cannot set `subroutine_index` and `subroutines` vec at the same time"
        );
    }

    let mut gen = MmlBytecodeGenerator::new(
        pitch_table,
        instruments,
        subroutines,
        subroutine_index.is_some(),
    );

    for c in commands {
        match gen.process_command(c.command(), c.pos()) {
            Ok(()) => (),
            Err(e) => errors.push(ErrorWithPos(*c.pos(), e)),
        }
    }

    let tick_counter = gen.bc.get_tick_counter();
    let max_nested_loops = gen.bc.get_max_nested_loops();

    let terminator = match (subroutine_index, gen.loop_point) {
        (Some(_), Some(_)) => {
            panic!("Loop point not allowed in subroutine")
        }
        (None, None) => BcTerminator::DisableChannel,
        (Some(_), None) => BcTerminator::ReturnFromSubroutine,
        (None, Some(lp)) => {
            if lp.tick_counter == tick_counter {
                errors.push(ErrorWithPos(
                    last_pos,
                    MmlCommandError::NoTicksAfterLoopPoint,
                ));
            }
            BcTerminator::LoopChannel
        }
    };

    let bytecode = match gen.bc.bytecode(terminator) {
        Ok(b) => b,
        Err(e) => {
            errors.push(ErrorWithPos(last_pos, MmlCommandError::BytecodeError(e)));
            Vec::new()
        }
    };

    let bc_subroutine =
        subroutine_index.map(|si| SubroutineId::new(si, tick_counter, max_nested_loops));

    if errors.is_empty() {
        Ok(ChannelData {
            identifier,
            bytecode,
            loop_point: gen.loop_point,
            tick_counter,
            last_instrument: gen.instrument,
            bc_subroutine,
        })
    } else {
        Err(errors)
    }
}

struct SharedChannelInput<'a> {
    zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    instruments: &'a Vec<MmlInstrument>,
    instrument_map: HashMap<IdentifierStr<'a>, usize>,
    subroutines: Option<&'a Vec<ChannelData>>,
    subroutine_map: Option<HashMap<IdentifierStr<'a>, SubroutineId>>,
}

fn parse_and_compile_mml_channel(
    lines: &[Line],
    identifier: Identifier,
    subroutine_index: Option<u8>,
    sci: &SharedChannelInput,
) -> Result<ChannelData, MmlChannelError> {
    match parse_mml_lines(
        lines,
        sci.zenlen,
        &sci.instrument_map,
        sci.subroutine_map.as_ref(),
    ) {
        Err(e) => Err(MmlChannelError {
            identifier,
            parse_errors: e,
            command_errors: Vec::new(),
        }),
        Ok((commands, last_pos)) => {
            match process_mml_commands(
                &commands,
                last_pos,
                identifier.clone(),
                subroutine_index,
                sci.pitch_table,
                sci.instruments,
                sci.subroutines,
            ) {
                Ok(data) => Ok(data),
                Err(e) => Err(MmlChannelError {
                    identifier,
                    parse_errors: Vec::new(),
                    command_errors: e,
                }),
            }
        }
    }
}

pub fn parse_mml(
    mml_text: &str,
    inst_map: &UniqueNamesList<data::Instrument>,
    pitch_table: &PitchTable,
) -> Result<MmlData, MmlCompileErrors> {
    let mut errors = MmlCompileErrors {
        line_errors: Vec::new(),
        subroutine_errors: Vec::new(),
        channel_errors: Vec::new(),
    };

    let lines = match split_lines(mml_text) {
        Ok(l) => l,
        Err(e) => {
            errors.line_errors.extend(e);
            return Err(errors);
        }
    };

    let metadata = match parse_headers(lines.headers) {
        Ok(m) => Some(m),
        Err(e) => {
            errors.line_errors.extend(e);
            None
        }
    };

    let (instruments, inst_errors) = parse_instruments(lines.instruments, inst_map);

    errors.line_errors.extend(inst_errors);

    let instrument_map = match build_instrument_map(&instruments) {
        Ok(map) => map,
        Err(e) => {
            errors.line_errors.extend(e);
            HashMap::new()
        }
    };

    if !errors.line_errors.is_empty() {
        return Err(errors);
    }
    let metadata = metadata.unwrap();

    let sci = SharedChannelInput {
        zenlen: metadata.zenlen,
        pitch_table,
        instruments: &instruments,
        instrument_map,
        subroutines: None,
        subroutine_map: None,
    };

    assert!(lines.subroutines.len() <= u8::MAX.into());
    let mut subroutines = Vec::with_capacity(lines.subroutines.len());
    for (s_index, (s_id, s_lines)) in lines.subroutines.iter().enumerate() {
        let s_index = s_index.try_into().unwrap();

        match parse_and_compile_mml_channel(s_lines, s_id.clone(), Some(s_index), &sci) {
            Ok(data) => subroutines.push(data),
            Err(e) => errors.subroutine_errors.push(e),
        }
    }
    let subroutines = subroutines;

    if !errors.subroutine_errors.is_empty() {
        return Err(errors);
    }

    let sci = SharedChannelInput {
        subroutines: Some(&subroutines),
        subroutine_map: Some(
            subroutines
                .iter()
                .map(|s| (s.identifier().as_ref(), s.bc_subroutine.unwrap()))
                .collect(),
        ),
        ..sci
    };

    let mut channels = Vec::with_capacity(lines.channels.len());
    for (c_index, c_lines) in lines.channels.iter().enumerate() {
        if !c_lines.is_empty() {
            let c_id = Identifier::try_from_name(CHANNEL_NAMES[c_index].to_owned()).unwrap();

            match parse_and_compile_mml_channel(c_lines, c_id.clone(), None, &sci) {
                Ok(data) => channels.push(data),
                Err(e) => errors.channel_errors.push(e),
            }
        }
    }
    let channels = channels;

    if errors.channel_errors.is_empty() {
        Ok(MmlData {
            metadata,
            subroutines,
            channels,
        })
    } else {
        Err(errors)
    }
}
