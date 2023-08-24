//! MML tokenizer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    LoopCount, Pan, PitchOffsetPerTick, PlayNoteTicks, QuarterWavelengthInTicks, RelativePan,
    RelativeVolume, SubroutineId, Volume, KEY_OFF_TICK_DELAY,
};
use crate::errors::{ErrorWithPos, MmlParserError, ValueError};
use crate::notes::{parse_pitch_char, MidiNote, MmlPitch, Note, Octave, STARTING_OCTAVE};
use crate::time::{Bpm, MmlLength, TickClock, TickCounter, ZenLen};
use crate::value_newtypes::{i8_value_newtype, u8_value_newtype, ValueNewType};

use std::cmp::min;
use std::collections::HashMap;

// Having a cap on MML text length ensures all positions can fit inside a u32.
pub const MAX_MML_TEXT_LENGTH: usize = 512 * 1024;

pub const MAX_COARSE_VOLUME: u32 = 16;
pub const COARSE_VOLUME_MULTIPLIER: u8 = 16;
pub const MIN_RELATIVE_COARSE_VOLUME: i8 = i8::MIN / COARSE_VOLUME_MULTIPLIER as i8;
pub const MAX_RELATIVE_COARSE_VOLUME: i8 = i8::MAX / COARSE_VOLUME_MULTIPLIER as i8;

u8_value_newtype!(
    PortamentoSpeed,
    PortamentoSpeedOutOfRange,
    NoPortamentoSpeed
);
u8_value_newtype!(Quantization, QuantizeOutOfRange, NoQuantize, 0, 8);
i8_value_newtype!(Transpose, TransposeOutOfRange, NoTranspose);

#[derive(Debug, Copy, Clone)]
pub struct FilePos {
    pub(crate) line_number: u32,
    pub(crate) line_char: u32,
    pub(crate) char_index: u32,
}
const _: () = assert!(
    MAX_MML_TEXT_LENGTH < i32::MAX as usize,
    "Cannot use u32 in FilePos"
);

impl FilePos {
    pub fn char_index(&self) -> u32 {
        self.char_index
    }
}

pub(crate) struct Line<'a> {
    pub text: &'a str,
    pub position: FilePos,
}

// ::TODO Add tick counter and instrument to MmlParser output (for the GUI)::
// ::TODO Add `Option<MmlPitch>` to PlayNote (for a GUI tracker)::

// An identifier str.
// Using `&str` to avoid a string copy.
// The contents of this variable might not be a valid Identifier
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub(crate) struct IdentifierStr<'a>(&'a str);

impl IdentifierStr<'_> {
    pub fn from_str(s: &str) -> IdentifierStr {
        IdentifierStr(s)
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

#[derive(Debug, Copy, Clone)]
pub enum VolumeCommand {
    Absolute(Volume),
    Relative(RelativeVolume),
}

#[derive(Debug, Copy, Clone)]
pub enum PanCommand {
    Absolute(Pan),
    Relative(RelativePan),
}

fn merge_volumes(v1: Option<VolumeCommand>, v2: VolumeCommand) -> VolumeCommand {
    match (v1, v2) {
        (Some(VolumeCommand::Absolute(v1)), VolumeCommand::Relative(v2)) => {
            let v = v1.as_u8().saturating_add_signed(v2.as_i8());
            VolumeCommand::Absolute(Volume::new(v))
        }
        (Some(VolumeCommand::Relative(v1)), VolumeCommand::Relative(v2)) => {
            let v = v1.as_i8().saturating_add(v2.as_i8());
            VolumeCommand::Relative(RelativeVolume::new(v))
        }
        (Some(_), VolumeCommand::Absolute(_)) => v2,
        (None, v2) => v2,
    }
}

fn merge_pans(p1: Option<PanCommand>, p2: PanCommand) -> PanCommand {
    match (p1, p2) {
        (Some(PanCommand::Absolute(p1)), PanCommand::Relative(p2)) => {
            let p = min(p1.as_u8().saturating_add_signed(p2.as_i8()), Pan::MAX);
            PanCommand::Absolute(p.try_into().unwrap())
        }
        (Some(PanCommand::Relative(p1)), PanCommand::Relative(p2)) => {
            let p: i32 = p1.as_i8().saturating_add(p2.as_i8()).into();
            PanCommand::Relative(p.try_into().unwrap())
        }
        (Some(_), PanCommand::Absolute(_)) => p2,
        (None, p2) => p2,
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct MpVibrato {
    pub depth_in_cents: u32,
    pub quarter_wavelength_ticks: QuarterWavelengthInTicks,
}

#[derive(Copy, Clone, PartialEq)]
pub struct ManualVibrato {
    pub pitch_offset_per_tick: PitchOffsetPerTick,
    pub quarter_wavelength_ticks: QuarterWavelengthInTicks,
}

pub enum MmlCommand {
    NoCommand,

    // Used for tick counter tracking.
    NewLine,

    SetLoopPoint,

    SetManualVibrato(Option<ManualVibrato>),
    SetMpVibrato(Option<MpVibrato>),

    Rest(TickCounter),

    PlayNote {
        note: Note,
        length: TickCounter,
        is_slur: bool,
    },
    PlayQuantizedNote {
        note: Note,
        length: TickCounter,
        key_on_length: TickCounter,
        // May be longer the `length - key_on_length`.
        rest: TickCounter,
    },
    Portamento {
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        total_length: TickCounter,
        delay_length: TickCounter,
        tie_length: TickCounter,
    },
    BrokenChord {
        notes: Vec<Note>,
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    },

    CallSubroutine(SubroutineId),
    StartLoop,
    SkipLastLoop,
    EndLoop(LoopCount),

    // index into Vec<ChannelData>.
    SetInstrument(usize),

    ChangePanAndOrVolume(Option<PanCommand>, Option<VolumeCommand>),
    SetEcho(bool),

    SetSongTempo(Bpm),
    SetSongTickClock(TickClock),
}

pub struct MmlCommandWithPos {
    command: MmlCommand,
    pos: FilePos,
}

impl MmlCommandWithPos {
    pub fn command(&self) -> &MmlCommand {
        &self.command
    }
    pub fn pos(&self) -> &FilePos {
        &self.pos
    }
}

pub enum Match<'a> {
    Some(FilePos, &'a str),
    None(FilePos),
}

// ::TODO is this the right name?
mod scanner {
    use super::{FilePos, Match};

    pub(crate) struct Scanner<'a> {
        // The position of the last consumed value
        before_pos: FilePos,

        pos: FilePos,
        to_parse: &'a str,
    }

    impl Scanner<'_> {
        pub fn new(pos: FilePos, to_parse: &str) -> Scanner {
            assert!(
                to_parse.len() < i32::MAX as usize,
                "str too large, pos will overflow"
            );
            let mut s = Scanner {
                before_pos: pos,
                pos,
                to_parse,
            };
            s.skip_whitespace();
            s
        }

        pub fn is_line_empty(&self) -> bool {
            self.to_parse.is_empty()
        }

        pub fn pos(&self) -> FilePos {
            self.pos
        }

        pub fn before_pos(&self) -> FilePos {
            self.before_pos
        }

        fn skip_whitespace(&mut self) {
            let (index, char_count) = self.index_and_char_count_for_pattern(|&c| c.is_whitespace());
            if index > 0 {
                self.to_parse = &self.to_parse[index..];
                self.pos.line_char += char_count;
                self.pos.char_index += u32::try_from(index).unwrap();
            }
        }

        fn extract_str_skip_whitespace(&mut self, index: usize, char_count: u32) -> Match {
            if index != 0 {
                let (s, remaining) = self.to_parse.split_at(index);

                self.before_pos = self.pos;

                self.to_parse = remaining;
                self.pos.line_char += char_count;
                self.pos.char_index += u32::try_from(index).unwrap();

                self.skip_whitespace();

                Match::Some(self.before_pos, s)
            } else {
                Match::None(self.before_pos)
            }
        }

        fn advance(&mut self, index: usize, char_count: u32) -> u32 {
            if index > 0 {
                self.before_pos = self.pos;

                self.to_parse = &self.to_parse[index..];
                self.pos.line_char += char_count;
                self.pos.char_index += u32::try_from(index).unwrap();

                self.skip_whitespace();
            }
            char_count
        }

        // MUST only be used to advance ASCII characters.
        pub(super) fn advance_ascii_char_count(&mut self, n_ascii_chars: u8) -> u32 {
            self.advance(n_ascii_chars.into(), n_ascii_chars.into())
        }

        // All methods below this line must not modify fields

        pub fn peek_u8(&self) -> Option<u8> {
            self.to_parse.as_bytes().first().copied()
        }

        pub fn peek_second_u8(&self) -> Option<u8> {
            self.to_parse.as_bytes().get(1).copied()
        }

        fn index_and_char_count_pattern_str(
            s: &str,
            pattern: impl Fn(&char) -> bool,
        ) -> (usize, u32) {
            let mut char_count = 0;
            for (index, c) in s.char_indices() {
                if !pattern(&c) {
                    return (index, char_count);
                }
                char_count += 1;
            }

            (s.len(), char_count)
        }

        fn index_and_char_count_for_pattern(
            &self,
            pattern: impl Fn(&char) -> bool,
        ) -> (usize, u32) {
            Self::index_and_char_count_pattern_str(self.to_parse, pattern)
        }

        fn index_for_char_then_pattern(
            &self,
            first_char_pattern: impl Fn(&char) -> bool,
            pattern: impl Fn(&char) -> bool,
        ) -> (usize, u32) {
            let mut iter = self.to_parse.char_indices();
            let first = iter.next();

            match first {
                Some((_, c)) if first_char_pattern(&c) => (),
                Some(_) => return (0, 0),
                None => return (0, 0),
            };
            let mut char_count = 1;
            for (index, c) in iter {
                if !pattern(&c) {
                    return (index, char_count);
                }
                char_count += 1;
            }
            (self.to_parse.len(), char_count)
        }

        pub fn skip_pattern(&mut self, pattern: impl Fn(&char) -> bool) -> u32 {
            let (i, char_count) = self.index_and_char_count_for_pattern(pattern);
            self.advance(i, char_count);
            char_count
        }

        pub fn match_pattern(&mut self, pattern: impl Fn(&char) -> bool) -> Match {
            let (i, char_count) = self.index_and_char_count_for_pattern(pattern);
            self.extract_str_skip_whitespace(i, char_count)
        }

        pub fn match_char_then_pattern(
            &mut self,
            first_char_pattern: impl Fn(&char) -> bool,
            pattern: impl Fn(&char) -> bool,
        ) -> Match {
            let (i, char_count) = self.index_for_char_then_pattern(first_char_pattern, pattern);
            self.extract_str_skip_whitespace(i, char_count)
        }

        // No whitespace between patterns
        pub fn match_two_patterns(
            &mut self,
            pattern1: impl Fn(&char) -> bool,
            pattern2: impl Fn(&char) -> bool,
        ) -> (FilePos, Option<&str>, Option<&str>) {
            let start_pos = self.pos;

            let (index_1, char_count_1) =
                Self::index_and_char_count_pattern_str(self.to_parse, pattern1);

            let (str_1, remaining) = self.to_parse.split_at(index_1);

            let (index_2, char_count_2) =
                Self::index_and_char_count_pattern_str(remaining, pattern2);

            let str_1 = if !str_1.is_empty() { Some(str_1) } else { None };

            let str_2 = match self
                .extract_str_skip_whitespace(index_1 + index_2, char_count_1 + char_count_2)
            {
                Match::Some(_, str_1_and_2) => {
                    if index_2 == 0 {
                        None
                    } else {
                        Some(&str_1_and_2[..index_2])
                    }
                }
                Match::None(_pos) => None,
            };

            (start_pos, str_1, str_2)
        }

        pub fn match_ascii_digits(&mut self) -> Match {
            self.match_pattern(char::is_ascii_digit)
        }

        pub fn match_always_signed_ascii_digits(&mut self) -> Match {
            self.match_char_then_pattern(|&c| c == '-' || c == '+', char::is_ascii_digit)
        }

        pub fn match_maybe_signed_ascii_digits(&mut self) -> Match {
            self.match_char_then_pattern(
                |&c| c == '-' || c == '+' || c.is_ascii_digit(),
                char::is_ascii_digit,
            )
        }

        pub fn match_for<T: MatchValueDigits>(&mut self) -> Match {
            <T as MatchValueDigits>::match_for(self)
        }
    }

    pub(crate) trait MatchValueDigits {
        fn match_for<'a>(s: &'a mut Scanner) -> Match<'a>;
    }

    impl MatchValueDigits for u32 {
        fn match_for<'a>(s: &'a mut Scanner) -> Match<'a> {
            s.match_ascii_digits()
        }
    }

    impl MatchValueDigits for i32 {
        fn match_for<'a>(s: &'a mut Scanner) -> Match<'a> {
            s.match_always_signed_ascii_digits()
        }
    }
}
use scanner::Scanner;

// List of peekable tokens
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(super) enum Symbol {
    CallSubroutine,
    SetInstrument,
    StartLoop,
    SkipLastLoop,
    EndLoop,
    Tie,
    Slur,
    ChangeWholeNoteLength,
    PlayMidiNoteNumber,
    SetDefaultLength,
    Rest,
    SetOctave,
    IncrementOctave,
    DecrementOctave,
    CoarseVolume,
    FineVolume,
    Pan,
    Quantize,
    Transpose,
    RelativeTranspose,
    StartBrokenChord,
    EndBrokenChord,
    StartPortamento,
    EndPortamento,
    ManualVibrato,
    MpVibrato,
    Echo,
    SetSongTempo,
    SetSongTickClock,
    SetLoopPoint,
    Comma,
    Divider,
}

pub(super) enum NewCommandToken {
    EndOfStream,
    EndOfLine,
    Pitch(MmlPitch),
    // Might not be a valid command token.
    Symbol(Symbol),
    PitchError(ErrorWithPos<MmlParserError>),
    // u32 = Number of skipped symbols::
    UnknownCharacters(u32),
}

enum ParseResult<T> {
    Ok(T),
    None(FilePos),
    Error(ErrorWithPos<MmlParserError>),
}

enum AbsoluteOrRelative {
    Absolute(FilePos, u32),
    Relative(FilePos, i32),
    None(FilePos),
    Error(ErrorWithPos<MmlParserError>),
}

pub(crate) struct MmlStreamParser<'a> {
    scanner: scanner::Scanner<'a>,
    remaining_lines: &'a [Line<'a>],

    at_end: bool,
}

macro_rules! parsing_error {
    ($pos:expr, $reason:ident) => {{
        ErrorWithPos($pos, MmlParserError::$reason)
    }};
}

fn value_error(pos: FilePos, e: ValueError) -> ErrorWithPos<MmlParserError> {
    ErrorWithPos(pos, MmlParserError::ValueError(e))
}

macro_rules! parse_absolute_or_relative {
    ($fn_name:ident, $type:ident, $missing_error:ident) => {
        pub fn $fn_name(&mut self) -> Result<$type, ErrorWithPos<MmlParserError>> {
            match self.parse_absolute_or_relative() {
                AbsoluteOrRelative::Absolute(pos, p) => match p.try_into() {
                    Ok(p) => Ok($type::Absolute(p)),
                    Err(e) => Err(value_error(pos, e)),
                },
                AbsoluteOrRelative::Relative(pos, p) => match p.try_into() {
                    Ok(p) => Ok($type::Relative(p)),
                    Err(e) => Err(value_error(pos, e)),
                },
                AbsoluteOrRelative::None(pos) => Err(value_error(pos, ValueError::$missing_error)),
                AbsoluteOrRelative::Error(e) => Err(e),
            }
        }
    };
}

impl MmlStreamParser<'_> {
    pub fn new<'a>(lines: &'a [Line<'a>]) -> MmlStreamParser<'a> {
        let blank_pos = FilePos {
            line_number: 0,
            line_char: 0,
            char_index: 0,
        };

        MmlStreamParser {
            remaining_lines: lines,
            scanner: Scanner::new(blank_pos, ""),
            at_end: false,
        }
    }

    pub fn pos(&self) -> FilePos {
        self.scanner.pos()
    }

    pub fn before_pos(&self) -> FilePos {
        self.scanner.before_pos()
    }

    /// Advances to the next line (if required)
    pub fn parse_newline(&mut self) {
        if self.scanner.is_line_empty() {
            match self.remaining_lines.split_first() {
                Some((first, remaining)) => {
                    self.scanner = Scanner::new(first.position, first.text);
                    self.remaining_lines = remaining;
                }
                None => {
                    self.at_end = true;
                }
            }
        }
    }

    fn is_unknown_starting_char(c: &char) -> bool {
        if c.is_ascii() {
            match u8::try_from(*c).unwrap_or(0) {
                b'a'..=b'g' => false,
                // This list must match `peek_symbol`.
                b'!' | b'@' | b'[' | b':' | b']' | b'^' | b'&' | b'C' | b'n' | b'l' | b'r'
                | b'o' | b'>' | b'<' | b'v' | b'V' | b'p' | b'Q' | b'~' | b'E' | b't' | b'T'
                | b'L' | b',' | b'|' | b'_' | b'{' | b'}' | b'M' => false,
                _ => true,
            }
        } else {
            true
        }
    }

    pub fn peek_symbol(&self) -> Option<Symbol> {
        let c1 = match self.scanner.peek_u8() {
            Some(c) => c,
            None => return None,
        };

        match c1 {
            // Symbols MUST be ASCII characters.
            b'!' => Some(Symbol::CallSubroutine),
            b'@' => Some(Symbol::SetInstrument),
            b'[' => Some(Symbol::StartLoop),
            b':' => Some(Symbol::SkipLastLoop),
            b']' => Some(Symbol::EndLoop),
            b'^' => Some(Symbol::Tie),
            b'&' => Some(Symbol::Slur),
            b'C' => Some(Symbol::ChangeWholeNoteLength),
            b'n' => Some(Symbol::PlayMidiNoteNumber),
            b'l' => Some(Symbol::SetDefaultLength),
            b'r' => Some(Symbol::Rest),
            b'o' => Some(Symbol::SetOctave),
            b'>' => Some(Symbol::IncrementOctave),
            b'<' => Some(Symbol::DecrementOctave),
            b'v' => Some(Symbol::CoarseVolume),
            b'V' => Some(Symbol::FineVolume),
            b'p' => Some(Symbol::Pan),
            b'Q' => Some(Symbol::Quantize),
            b'~' => Some(Symbol::ManualVibrato),
            b'E' => Some(Symbol::Echo),
            b't' => Some(Symbol::SetSongTempo),
            b'T' => Some(Symbol::SetSongTickClock),
            b'L' => Some(Symbol::SetLoopPoint),
            b',' => Some(Symbol::Comma),
            b'|' => Some(Symbol::Divider),

            // Possibly multiple character tokens
            b'_' | b'{' | b'}' | b'M' => {
                let c2 = self.scanner.peek_second_u8();
                match (c1, c2) {
                    (b'_', Some(b'_')) => Some(Symbol::RelativeTranspose),
                    (b'{', Some(b'{')) => Some(Symbol::StartBrokenChord),
                    (b'}', Some(b'}')) => Some(Symbol::EndBrokenChord),
                    (b'M', Some(b'P')) => Some(Symbol::MpVibrato),

                    (b'_', _) => Some(Symbol::Transpose),
                    (b'{', _) => Some(Symbol::StartPortamento),
                    (b'}', _) => Some(Symbol::EndPortamento),

                    (_, _) => None,
                }
            }

            _ => None,
        }
    }

    fn n_ascii_chars_in_symbol(s: Symbol) -> u8 {
        // ASSUMES symbol is ASCII characters only
        match s {
            Symbol::RelativeTranspose => 2,
            Symbol::StartBrokenChord => 2,
            Symbol::EndBrokenChord => 2,
            Symbol::MpVibrato => 2,
            _ => 1,
        }
    }

    pub fn next_symbol(&mut self) -> Option<Symbol> {
        let symbol = self.peek_symbol();

        if let Some(s) = &symbol {
            self.scanner
                .advance_ascii_char_count(Self::n_ascii_chars_in_symbol(*s));
        }

        symbol
    }

    // Does not advance to the next line
    pub fn next_new_command_token(&mut self) -> NewCommandToken {
        if self.scanner.is_line_empty() {
            if self.at_end {
                return NewCommandToken::EndOfStream;
            } else {
                return NewCommandToken::EndOfLine;
            }
        }

        match self.parse_optional_pitch() {
            Err(e) => NewCommandToken::PitchError(e),
            Ok(Some(p)) => NewCommandToken::Pitch(p),
            Ok(None) => match self.next_symbol() {
                Some(s) => NewCommandToken::Symbol(s),
                None => {
                    let n_chars = self.scanner.skip_pattern(Self::is_unknown_starting_char);
                    NewCommandToken::UnknownCharacters(n_chars)
                }
            },
        }
    }

    pub fn next_symbol_matches(&mut self, s: Symbol) -> bool {
        if self.peek_symbol() == Some(s) {
            self.scanner
                .advance_ascii_char_count(Self::n_ascii_chars_in_symbol(s));
            true
        } else {
            false
        }
    }

    pub fn next_symbol_one_of(&mut self, to_match: &[Symbol]) -> Option<Symbol> {
        match self.peek_symbol() {
            Some(s) if to_match.contains(&s) => {
                self.scanner
                    .advance_ascii_char_count(Self::n_ascii_chars_in_symbol(s));
                Some(s)
            }
            _ => None,
        }
    }

    pub fn parse_optional_bool(&mut self) -> Result<Option<bool>, ErrorWithPos<MmlParserError>> {
        match self.scanner.match_ascii_digits() {
            Match::None(_pos) => Ok(None),
            Match::Some(_, "1") => Ok(Some(true)),
            Match::Some(_, "0") => Ok(Some(false)),
            Match::Some(pos, _) => Err(value_error(pos, ValueError::InvalidMmlBool)),
        }
    }

    pub fn parse_bool(&mut self) -> Result<bool, ErrorWithPos<MmlParserError>> {
        match self.scanner.match_ascii_digits() {
            Match::Some(_pos, "1") => Ok(true),
            Match::Some(_pos, "0") => Ok(false),
            Match::Some(pos, _) => Err(value_error(pos, ValueError::InvalidMmlBool)),
            Match::None(pos) => Err(value_error(pos, ValueError::NoBool)),
        }
    }

    fn parse_u32(&mut self) -> ParseResult<u32> {
        match self.scanner.match_ascii_digits() {
            Match::None(pos) => ParseResult::None(pos),
            Match::Some(pos, ascii_digits) => match ascii_digits.parse() {
                Ok(v) => ParseResult::Ok(v),
                Err(_) => ParseResult::Error(value_error(
                    pos,
                    ValueError::CannotParseUnsigned(ascii_digits.to_owned()),
                )),
            },
        }
    }

    pub fn parse_newtype<T>(&mut self) -> Result<T, ErrorWithPos<MmlParserError>>
    where
        T: ValueNewType,
        <T as ValueNewType>::ConvertFrom: scanner::MatchValueDigits,
    {
        match self.scanner.match_for::<T::ConvertFrom>() {
            Match::None(pos) => Err(value_error(pos, T::MISSING_ERROR)),
            Match::Some(pos, ascii_digits) => match T::try_from_str(ascii_digits) {
                Ok(v) => Ok(v),
                Err(e) => Err(value_error(pos, e)),
            },
        }
    }

    fn parse_absolute_or_relative(&mut self) -> AbsoluteOrRelative {
        match self.scanner.match_maybe_signed_ascii_digits() {
            Match::None(pos) => AbsoluteOrRelative::None(pos),
            Match::Some(pos, ascii_digits) => {
                let fc = ascii_digits.as_bytes()[0];
                if fc == b'-' || fc == b'+' {
                    match ascii_digits.parse::<i32>() {
                        Ok(v) => AbsoluteOrRelative::Relative(pos, v),
                        Err(_) => AbsoluteOrRelative::Error(value_error(
                            pos,
                            ValueError::CannotParseSigned(ascii_digits.to_owned()),
                        )),
                    }
                } else {
                    match ascii_digits.parse::<u32>() {
                        Ok(v) => AbsoluteOrRelative::Absolute(pos, v),
                        Err(_) => AbsoluteOrRelative::Error(value_error(
                            pos,
                            ValueError::CannotParseUnsigned(ascii_digits.to_owned()),
                        )),
                    }
                }
            }
        }
    }

    parse_absolute_or_relative!(parse_pan, PanCommand, NoPan);
    parse_absolute_or_relative!(parse_fine_volume, VolumeCommand, NoVolume);

    pub fn parse_coarse_volume(&mut self) -> Result<VolumeCommand, ErrorWithPos<MmlParserError>> {
        match self.parse_absolute_or_relative() {
            AbsoluteOrRelative::Absolute(pos, v) => {
                if v <= MAX_COARSE_VOLUME {
                    let v = u8::try_from(v).unwrap();
                    let v = v.saturating_mul(COARSE_VOLUME_MULTIPLIER);
                    Ok(VolumeCommand::Absolute(Volume::new(v)))
                } else {
                    Err(value_error(pos, ValueError::CoarseVolumeOutOfRange))
                }
            }
            AbsoluteOrRelative::Relative(pos, v) => {
                match v.saturating_mul(COARSE_VOLUME_MULTIPLIER.into()).try_into() {
                    Ok(v) => Ok(VolumeCommand::Relative(v)),
                    Err(_) => Err(value_error(pos, ValueError::RelativeCoarseVolumeOutOfRange)),
                }
            }
            AbsoluteOrRelative::None(pos) => Err(value_error(pos, ValueError::NoVolume)),
            AbsoluteOrRelative::Error(e) => Err(e),
        }
    }

    pub fn parse_optional_pitch(
        &mut self,
    ) -> Result<Option<MmlPitch>, ErrorWithPos<MmlParserError>> {
        let (pos, pitch_str) = match self
            .scanner
            .match_char_then_pattern(|&c| matches!(c, 'a'..='g'), |&c| c == '-' || c == '+')
        {
            Match::Some(pos, s) => (pos, s),
            Match::None(_pos) => return Ok(None),
        };

        let pitch_char = pitch_str.chars().next().unwrap();
        let pitch = parse_pitch_char(pitch_char).unwrap();

        let semitone_offset = if !pitch_str.len() > 1 {
            if pitch_str.len() > 32 {
                return Err(parsing_error!(pos, TooManyAccidentals));
            }
            let n_minuses = i8::try_from(pitch_str.bytes().filter(|&c| c == b'-').count()).unwrap();
            let n_plusses = i8::try_from(pitch_str.len() - 1).unwrap() - n_minuses;

            n_plusses - n_minuses
        } else {
            0
        };

        Ok(Some(MmlPitch::new(pitch, semitone_offset)))
    }

    pub fn parse_optional_length(
        &mut self,
    ) -> Result<Option<MmlLength>, ErrorWithPos<MmlParserError>> {
        let is_fc_percent_sign = self.scanner.peek_u8() == Some(b'%');
        if is_fc_percent_sign {
            self.scanner.advance_ascii_char_count(1);
        }

        let (pos, ascii_digits, dots) = self
            .scanner
            .match_two_patterns(char::is_ascii_digit, |&c| c == '.');

        if ascii_digits.is_none() && dots.is_none() {
            return Ok(None);
        }

        let length = match ascii_digits {
            Some(ascii_digits) => match ascii_digits.parse() {
                Ok(v) => Some(v),
                Err(_) => {
                    return Err(value_error(
                        pos,
                        ValueError::CannotParseUnsigned(ascii_digits.to_owned()),
                    ))
                }
            },
            None => None,
        };

        let number_of_dots = match dots {
            Some(dots) => match dots.len().try_into() {
                Ok(n) if n < 16 => n,
                _ => return Err(parsing_error!(pos, TooManyDotsInNoteLength)),
            },
            None => 0,
        };

        Ok(Some(MmlLength::new(
            length,
            is_fc_percent_sign,
            number_of_dots,
        )))
    }

    // Returns Option so the appropriate error message can be generated by `MmlParser`
    pub fn parse_identifier(&mut self) -> Option<IdentifierStr> {
        let s = match self.scanner.peek_u8() {
            Some(c) if c.is_ascii_digit() => self.scanner.match_ascii_digits(),
            _ => self.scanner.match_pattern(|&c| !c.is_whitespace()),
        };
        match s {
            Match::Some(_pos, s) => Some(IdentifierStr(s)),
            Match::None(_pos) => None,
        }
    }

    fn parse_comma_quarter_wavelength_ticks(
        &mut self,
    ) -> Result<QuarterWavelengthInTicks, ErrorWithPos<MmlParserError>> {
        if !self.next_symbol_matches(Symbol::Comma) {
            return Err(value_error(
                self.before_pos(),
                ValueError::NoCommaQuarterWavelength,
            ));
        }

        self.parse_newtype()
    }

    // When None is returned, vibrato is disabled.
    pub fn parse_manual_vibrato(
        &mut self,
    ) -> Result<Option<ManualVibrato>, ErrorWithPos<MmlParserError>> {
        let value = match self.parse_u32() {
            ParseResult::Ok(v) => v,
            ParseResult::None(pos) => return Err(value_error(pos, ValueError::NoVibratoDepth)),
            ParseResult::Error(e) => return Err(e),
        };

        if value == 0 {
            Ok(None)
        } else {
            let pitch_offset_per_tick = match value.try_into() {
                Ok(d) => d,
                Err(e) => return Err(value_error(self.before_pos(), e)),
            };
            let quarter_wavelength_ticks = self.parse_comma_quarter_wavelength_ticks()?;
            Ok(Some(ManualVibrato {
                pitch_offset_per_tick,
                quarter_wavelength_ticks,
            }))
        }
    }

    // When None is returned, MP vibrato is disabled.
    pub fn parse_mp_vibtato(&mut self) -> Result<Option<MpVibrato>, ErrorWithPos<MmlParserError>> {
        let depth_in_cents = match self.parse_u32() {
            ParseResult::Ok(v) => v,
            ParseResult::None(pos) => return Err(value_error(pos, ValueError::NoVibratoDepth)),
            ParseResult::Error(e) => return Err(e),
        };

        if depth_in_cents == 0 {
            Ok(None)
        } else {
            let quarter_wavelength_ticks = self.parse_comma_quarter_wavelength_ticks()?;
            Ok(Some(MpVibrato {
                depth_in_cents,
                quarter_wavelength_ticks,
            }))
        }
    }
}

// The MML parser keeps track of note lengths and octaves.
//
// This allows me to merge slurs, ties and rests while parsing.
// It also allows me to include octave/transpose commands inside pitch lists.
struct MmlParserState {
    zenlen: ZenLen,
    default_length: TickCounter,
    octave: Octave,
    semitone_offset: i8,
    quantize: Quantization,
}

impl MmlParserState {
    fn set_zenlen(&mut self, z: ZenLen) {
        self.zenlen = z;
    }
    fn set_defualt_length(&mut self, tc: Option<TickCounter>) {
        if let Some(tc) = tc {
            self.default_length = tc;
        }
    }
    fn set_octave(&mut self, o: Octave) {
        self.octave = o;
    }
    fn increment_octave(&mut self) {
        self.octave.saturating_increment();
    }
    fn decrement_octave(&mut self) {
        self.octave.saturating_decrement();
    }
    fn transpose(&mut self, t: Transpose) {
        self.semitone_offset = t.0;
    }
    fn relative_transpose(&mut self, t: Transpose) {
        self.semitone_offset = self.semitone_offset.saturating_add(t.0);
    }
    fn set_quantize(&mut self, q: Quantization) {
        self.quantize = q;
    }
}

// This parser also calculates specific notes and total lengths
//
// This allows me to merge slurs, ties and rests while parsing.
struct MmlParser<'a> {
    instruments_map: &'a HashMap<IdentifierStr<'a>, usize>,
    subroutine_map: Option<&'a HashMap<IdentifierStr<'a>, SubroutineId>>,

    parser: MmlStreamParser<'a>,
    state: MmlParserState,
    error_list: Vec<ErrorWithPos<MmlParserError>>,
}

impl MmlParser<'_> {
    fn new<'a>(
        lines: &'a [Line<'a>],
        zenlen: ZenLen,
        instruments_map: &'a HashMap<IdentifierStr<'a>, usize>,
        subroutine_map: Option<&'a HashMap<IdentifierStr<'a>, SubroutineId>>,
    ) -> MmlParser<'a> {
        MmlParser {
            instruments_map,
            subroutine_map,

            parser: MmlStreamParser::new(lines),
            state: MmlParserState {
                zenlen,
                default_length: zenlen.starting_length(),
                octave: STARTING_OCTAVE,
                semitone_offset: 0,
                quantize: Quantization(8),
            },
            error_list: Vec::new(),
        }
    }

    fn add_error(&mut self, e: ErrorWithPos<MmlParserError>) {
        self.error_list.push(e)
    }

    fn add_error_before(&mut self, e: MmlParserError) {
        self.error_list
            .push(ErrorWithPos(self.parser.before_pos(), e))
    }

    fn pitch_to_note(&self, p: MmlPitch) -> Result<Note, MmlParserError> {
        match Note::from_mml_pitch(p, self.state.octave, self.state.semitone_offset) {
            Ok(n) => Ok(n),
            Err(_) => Err(MmlParserError::InvalidNote),
        }
    }

    fn calculate_note_length(&self, l: MmlLength) -> Result<TickCounter, MmlParserError> {
        Ok(l.to_tick_count(self.state.default_length, self.state.zenlen)?)
    }

    fn parse_optional_note_length(
        &mut self,
    ) -> Result<Option<TickCounter>, ErrorWithPos<MmlParserError>> {
        match self.parser.parse_optional_length() {
            Ok(Some(l)) => match self.calculate_note_length(l) {
                Ok(tc) => Ok(Some(tc)),
                Err(e) => Err(ErrorWithPos(self.parser.before_pos(), e)),
            },
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn parse_note_length(&mut self) -> Result<TickCounter, ErrorWithPos<MmlParserError>> {
        match self.parser.parse_optional_length() {
            Ok(Some(l)) => match self.calculate_note_length(l) {
                Ok(tc) => Ok(tc),
                Err(e) => Err(ErrorWithPos(self.parser.before_pos(), e)),
            },
            Ok(None) => Ok(self.state.default_length),
            Err(e) => Err(e),
        }
    }

    // Does not create a new MML Command
    fn try_change_octave_st_offset_symbol(&mut self, s: Symbol) -> bool {
        match s {
            Symbol::SetOctave => match self.parser.parse_newtype() {
                Ok(o) => self.state.octave = o,
                Err(e) => self.add_error(e),
            },
            Symbol::IncrementOctave => {
                self.state.octave.saturating_increment();
            }
            Symbol::DecrementOctave => {
                self.state.octave.saturating_decrement();
            }
            Symbol::Transpose => match self.parser.parse_newtype() {
                Ok(t) => self.state.transpose(t),
                Err(e) => self.add_error(e),
            },
            Symbol::RelativeTranspose => match self.parser.parse_newtype() {
                Ok(t) => self.state.relative_transpose(t),
                Err(e) => self.add_error(e),
            },
            _ => return false,
        }

        true
    }

    // Does not create a new MML Command
    fn try_change_state_symbol(&mut self, s: Symbol) {
        match s {
            Symbol::ChangeWholeNoteLength => match self.parser.parse_newtype() {
                Ok(z) => self.state.zenlen = z,
                Err(e) => self.add_error(e),
            },
            Symbol::SetDefaultLength => match self.parse_optional_note_length() {
                Ok(Some(tc)) => self.state.default_length = tc,
                Ok(None) => (),
                Err(e) => self.add_error(e),
            },
            _ => {
                self.try_change_octave_st_offset_symbol(s);
            }
        }
    }

    // Merge multiple rest commands
    fn merge_multiple_rest_symbols(&mut self, rest_length: TickCounter) -> TickCounter {
        // No const symbol concatenate
        const SYMBOLS: [Symbol; 10] = [
            Symbol::Divider,
            Symbol::SetDefaultLength,
            Symbol::SetOctave,
            Symbol::IncrementOctave,
            Symbol::DecrementOctave,
            Symbol::Transpose,
            Symbol::RelativeTranspose,
            Symbol::ChangeWholeNoteLength,
            Symbol::SetDefaultLength,
            Symbol::Rest,
        ];

        let mut rest_length = rest_length;

        while let Some(s) = self.parser.next_symbol_one_of(&SYMBOLS) {
            match s {
                Symbol::Rest => {
                    // Extend tick_length on ties
                    match self.parse_note_length() {
                        Ok(l) => rest_length += l,
                        Err(e) => self.add_error(e),
                    }
                }

                // Allow note/pitch changes
                _ => {
                    self.try_change_state_symbol(s);
                }
            }
        }

        rest_length
    }

    // Merges tie and slur commands
    // Assumes the previous token was a note token.
    fn parse_tie_and_slur(&mut self) -> (TickCounter, bool) {
        // No const symbol concatenate
        const SYMBOLS: [Symbol; 11] = [
            Symbol::Divider,
            Symbol::SetDefaultLength,
            Symbol::SetOctave,
            Symbol::IncrementOctave,
            Symbol::DecrementOctave,
            Symbol::Transpose,
            Symbol::RelativeTranspose,
            Symbol::ChangeWholeNoteLength,
            Symbol::SetDefaultLength,
            Symbol::Tie,
            Symbol::Slur,
        ];

        let mut tie_length = TickCounter::new(0);
        let mut slur_note = false;

        while let Some(s) = self.parser.next_symbol_one_of(&SYMBOLS) {
            match s {
                // Allow note length and octave to change before tie/slur
                Symbol::Tie => {
                    // Extend tick_length on ties
                    match self.parse_note_length() {
                        Ok(l) => tie_length += l,
                        Err(e) => self.add_error(e),
                    }
                }
                Symbol::Slur => {
                    // Slur or tie
                    match self.parse_optional_note_length() {
                        Ok(Some(l)) => tie_length += l,
                        Ok(None) => slur_note = true,
                        Err(e) => self.add_error(e),
                    }
                }

                // Allow note/pitch changes
                _ => {
                    self.try_change_state_symbol(s);
                }
            }
        }

        (tie_length, slur_note)
    }

    // Merges multiple pan and volume commands
    fn parse_pan_or_volume(
        &mut self,
        pan: Option<PanCommand>,
        volume: Option<VolumeCommand>,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        // No const symbol concatenate
        const SYMBOLS: [Symbol; 12] = [
            Symbol::Divider,
            Symbol::SetDefaultLength,
            Symbol::SetOctave,
            Symbol::IncrementOctave,
            Symbol::DecrementOctave,
            Symbol::Transpose,
            Symbol::RelativeTranspose,
            Symbol::ChangeWholeNoteLength,
            Symbol::SetDefaultLength,
            Symbol::FineVolume,
            Symbol::CoarseVolume,
            Symbol::Pan,
        ];

        let mut pan = pan;
        let mut volume = volume;

        // ::TODO throughly test merging::
        while let Some(s) = self.parser.next_symbol_one_of(&SYMBOLS) {
            match s {
                Symbol::CoarseVolume => match self.parser.parse_coarse_volume() {
                    Ok(v) => volume = Some(merge_volumes(volume, v)),
                    Err(e) => self.add_error(e),
                },
                Symbol::FineVolume => match self.parser.parse_fine_volume() {
                    Ok(v) => volume = Some(merge_volumes(volume, v)),
                    Err(e) => self.add_error(e),
                },
                Symbol::Pan => match self.parser.parse_pan() {
                    Ok(p) => pan = Some(merge_pans(pan, p)),
                    Err(e) => self.add_error(e),
                },
                // Allow note/pitch changes
                _ => {
                    self.try_change_state_symbol(s);
                }
            }
        }

        Ok(MmlCommand::ChangePanAndOrVolume(pan, volume))
    }

    fn parse_portamento(
        &mut self,
        pos: FilePos,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        let notes = match self.parse_pitch_list(Symbol::EndPortamento) {
            Some(n) => n,
            None => {
                return Err(ErrorWithPos(pos, MmlParserError::MissingEndPortamento));
            }
        };

        if notes.len() != 2 {
            return Err(ErrorWithPos(
                pos,
                MmlParserError::PortamentoRequiresTwoPitches,
            ));
        }

        let total_length = self.parse_note_length()?;

        let mut delay_length = TickCounter::new(0);
        let mut speed_override = None;

        if self.parser.next_symbol_matches(Symbol::Comma) {
            if let Some(dt) = self.parse_optional_note_length()? {
                if dt < total_length {
                    delay_length = dt;
                } else {
                    self.add_error_before(MmlParserError::InvalidPortamentoDelay);
                }
            };
            if self.parser.next_symbol_matches(Symbol::Comma) {
                speed_override = Some(self.parser.parse_newtype()?);
            }
        }

        let (tie_length, is_slur) = self.parse_tie_and_slur();

        Ok(MmlCommand::Portamento {
            note1: notes[0],
            note2: notes[1],
            is_slur,
            speed_override,
            total_length,
            delay_length,
            tie_length,
        })
    }

    fn parse_broken_chord(
        &mut self,
        pos: FilePos,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        let notes = match self.parse_pitch_list(Symbol::EndBrokenChord) {
            Some(n) => n,
            None => {
                return Err(ErrorWithPos(pos, MmlParserError::MissingEndBrokenChord));
            }
        };

        let total_length = self.parse_note_length()?;
        let mut tie = true;
        let mut note_length = None;
        let mut note_length_pos = self.parser.before_pos();

        if self.parser.next_symbol_matches(Symbol::Comma) {
            note_length = self.parser.parse_optional_length()?;
            note_length_pos = self.parser.before_pos();

            if self.parser.next_symbol_matches(Symbol::Comma) {
                tie = self.parser.parse_bool()?;
            }
        }

        let note_length = match note_length {
            Some(nl) => match self.calculate_note_length(nl) {
                Ok(ticks) => match PlayNoteTicks::try_from_is_slur(ticks.value(), tie) {
                    Ok(t) => t,
                    Err(e) => {
                        return Err(ErrorWithPos(note_length_pos, MmlParserError::ValueError(e)))
                    }
                },
                Err(e) => return Err(ErrorWithPos(note_length_pos, e)),
            },
            None => PlayNoteTicks::min_for_is_slur(tie),
        };

        Ok(MmlCommand::BrokenChord {
            notes,
            total_length,
            note_length,
        })
    }

    fn parse_note(
        &mut self,
        note: Note,
        note_length: TickCounter,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        let (tie_length, is_slur) = self.parse_tie_and_slur();
        let length = note_length + tie_length;

        let q = self.state.quantize.0;
        const MAX_Q: u8 = Quantization::MAX;

        if q >= MAX_Q || is_slur {
            Ok(MmlCommand::PlayNote {
                note,
                length,
                is_slur,
            })
        } else {
            // Note is quantized

            // ::TODO write a test to confirm the tick count is unchanged by quantization::
            let q = u32::from(q);
            let l = length.value();
            let key_on_length = (l * q) / u32::from(MAX_Q) + KEY_OFF_TICK_DELAY;
            let key_off_length = l - key_on_length;

            if key_on_length > KEY_OFF_TICK_DELAY && key_off_length > KEY_OFF_TICK_DELAY {
                let key_on_length = TickCounter::new(key_on_length);
                let rest = TickCounter::new(key_off_length);

                let rest = self.merge_multiple_rest_symbols(rest);

                Ok(MmlCommand::PlayQuantizedNote {
                    note,
                    length,
                    key_on_length,
                    rest,
                })
            } else {
                // Note is too short to be quantized
                Ok(MmlCommand::PlayNote {
                    note,
                    length,
                    is_slur,
                })
            }
        }
    }

    fn parse_play_pitch(
        &mut self,
        pitch: MmlPitch,
        pos: FilePos,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        let note = match self.pitch_to_note(pitch) {
            Ok(n) => n,
            Err(e) => return Err(ErrorWithPos(pos, e)),
        };
        let length = self.parse_note_length()?;

        self.parse_note(note, length)
    }

    fn parse_symbol(
        &mut self,
        symbol: Symbol,
        pos: FilePos,
    ) -> Result<MmlCommand, ErrorWithPos<MmlParserError>> {
        type Error = MmlParserError;

        let err = |e: Error| Err(ErrorWithPos(pos, e));

        match symbol {
            Symbol::CallSubroutine => {
                let id = self.parser.parse_identifier();

                match self.subroutine_map {
                    Some(subroutine_map) => match id {
                        Some(id) => match subroutine_map.get(&id) {
                            Some(s) => Ok(MmlCommand::CallSubroutine(*s)),
                            None => err(Error::CannotFindSubroutine(id.as_str().to_owned())),
                        },
                        None => err(Error::NoSubroutine),
                    },
                    None => {
                        // Subroutine calls are disabled.
                        // ignore identifier
                        err(Error::CannotCallSubroutineInASubroutine)
                    }
                }
            }

            Symbol::SetInstrument => match self.parser.parse_identifier() {
                Some(id) => match self.instruments_map.get(&id) {
                    Some(index) => Ok(MmlCommand::SetInstrument(*index)),
                    None => err(Error::CannotFindSubroutine(id.as_str().to_owned())),
                },
                None => err(Error::NoInstrument),
            },

            Symbol::StartLoop => Ok(MmlCommand::StartLoop),
            Symbol::SkipLastLoop => Ok(MmlCommand::SkipLastLoop),
            Symbol::EndLoop => Ok(MmlCommand::EndLoop(self.parser.parse_newtype()?)),

            Symbol::PlayMidiNoteNumber => match self.parser.parse_newtype::<MidiNote>()?.try_into()
            {
                Ok(n) => self.parse_note(n, self.state.default_length),
                Err(e) => err(e.into()),
            },

            Symbol::Rest => {
                let r = self.parse_note_length()?;
                let r = self.merge_multiple_rest_symbols(r);
                Ok(MmlCommand::Rest(r))
            }

            Symbol::CoarseVolume => {
                let v = self.parser.parse_coarse_volume()?;
                self.parse_pan_or_volume(None, Some(v))
            }
            Symbol::FineVolume => {
                let v = self.parser.parse_fine_volume()?;
                self.parse_pan_or_volume(None, Some(v))
            }
            Symbol::Pan => {
                let p = self.parser.parse_pan()?;
                self.parse_pan_or_volume(Some(p), None)
            }

            Symbol::StartBrokenChord => self.parse_broken_chord(pos),
            Symbol::EndBrokenChord => err(Error::NoStartBrokenChord),

            Symbol::StartPortamento => self.parse_portamento(pos),
            Symbol::EndPortamento => err(Error::NoStartPortamento),

            Symbol::ManualVibrato => Ok(MmlCommand::SetManualVibrato(
                self.parser.parse_manual_vibrato()?,
            )),

            Symbol::MpVibrato => Ok(MmlCommand::SetMpVibrato(self.parser.parse_mp_vibtato()?)),

            Symbol::Echo => Ok(MmlCommand::SetEcho(
                self.parser.parse_optional_bool()?.unwrap_or(true),
            )),

            Symbol::SetSongTempo => Ok(MmlCommand::SetSongTempo(self.parser.parse_newtype()?)),

            Symbol::SetSongTickClock => {
                Ok(MmlCommand::SetSongTickClock(self.parser.parse_newtype()?))
            }

            Symbol::SetLoopPoint => Ok(MmlCommand::SetLoopPoint),

            Symbol::Tie => err(Error::MissingNoteBeforeTie),
            Symbol::Slur => err(Error::MissingNoteBeforeSlur),
            Symbol::Comma => err(Error::CannotParseComma),

            // state change commands
            Symbol::ChangeWholeNoteLength => {
                self.state.set_zenlen(self.parser.parse_newtype()?);
                Ok(MmlCommand::NoCommand)
            }
            Symbol::SetDefaultLength => {
                let tc = self.parse_optional_note_length()?;
                self.state.set_defualt_length(tc);
                Ok(MmlCommand::NoCommand)
            }
            Symbol::SetOctave => {
                self.state.set_octave(self.parser.parse_newtype()?);
                Ok(MmlCommand::NoCommand)
            }
            Symbol::IncrementOctave => {
                self.state.increment_octave();
                Ok(MmlCommand::NoCommand)
            }
            Symbol::DecrementOctave => {
                self.state.decrement_octave();
                Ok(MmlCommand::NoCommand)
            }
            Symbol::Transpose => {
                self.state.transpose(self.parser.parse_newtype()?);
                Ok(MmlCommand::NoCommand)
            }

            Symbol::RelativeTranspose => {
                self.state.relative_transpose(self.parser.parse_newtype()?);
                Ok(MmlCommand::NoCommand)
            }

            Symbol::Quantize => {
                self.state.set_quantize(self.parser.parse_newtype()?);
                Ok(MmlCommand::NoCommand)
            }

            // Formatting symbols
            Symbol::Divider => Ok(MmlCommand::NoCommand),
        }
    }

    fn parse_pitch_list(&mut self, end_symbol: Symbol) -> Option<Vec<Note>> {
        let mut out = Vec::new();

        loop {
            // Do not advance to the next line.
            match self.parser.next_new_command_token() {
                NewCommandToken::Symbol(s) if s == end_symbol => {
                    return Some(out);
                }
                NewCommandToken::EndOfLine | NewCommandToken::EndOfStream => {
                    // Missing EndBrokenChord token
                    return None;
                }
                NewCommandToken::UnknownCharacters(count) => {
                    self.add_error_before(MmlParserError::UnknownCharacters(count));
                }
                NewCommandToken::PitchError(e) => {
                    self.add_error(e);
                }
                NewCommandToken::Pitch(p) => match self.pitch_to_note(p) {
                    Ok(n) => out.push(n),
                    Err(e) => self.add_error_before(e),
                },
                NewCommandToken::Symbol(s) => {
                    if s == Symbol::PlayMidiNoteNumber {
                        match self.parser.parse_newtype::<MidiNote>() {
                            Err(e) => self.add_error(e),
                            Ok(n) => match Note::try_from(n) {
                                Ok(n) => out.push(n),
                                Err(e) => self.add_error_before(e.into()),
                            },
                        }
                    } else if self.try_change_octave_st_offset_symbol(s) {
                        // Change octave or semitone offset symbol
                    } else {
                        self.add_error_before(MmlParserError::InvalidPitchListSymbol)
                    }
                }
            }
        }
    }

    fn parse_all(
        mut self,
    ) -> Result<(Vec<MmlCommandWithPos>, FilePos), Vec<ErrorWithPos<MmlParserError>>> {
        let mut out = Vec::new();

        // ::TODO detect and panic on infinite loop::

        loop {
            // Do not advance to the next line.
            match self.parser.next_new_command_token() {
                NewCommandToken::EndOfStream => {
                    break;
                }
                NewCommandToken::EndOfLine => {
                    self.parser.parse_newline();
                    out.push(MmlCommandWithPos {
                        command: MmlCommand::NewLine,
                        pos: self.parser.pos(),
                    });
                }
                NewCommandToken::UnknownCharacters(count) => {
                    self.add_error_before(MmlParserError::UnknownCharacters(count));
                }
                NewCommandToken::PitchError(e) => {
                    self.add_error(e);
                }
                NewCommandToken::Pitch(pitch) => {
                    let pos = self.parser.before_pos();
                    match self.parse_play_pitch(pitch, pos) {
                        Ok(command) => out.push(MmlCommandWithPos { command, pos }),
                        Err(e) => self.add_error(e),
                    }
                }
                NewCommandToken::Symbol(symbol) => {
                    let pos = self.parser.before_pos();
                    match self.parse_symbol(symbol, pos) {
                        Ok(command) => match command {
                            MmlCommand::NoCommand => (),
                            _ => out.push(MmlCommandWithPos { command, pos }),
                        },
                        Err(e) => self.add_error(e),
                    }
                }
            }
        }

        if self.error_list.is_empty() {
            Ok((out, self.parser.pos()))
        } else {
            Err(self.error_list)
        }
    }
}

pub(crate) fn parse_mml_lines(
    lines: &[Line],
    zenlen: ZenLen,
    instruments_map: &HashMap<IdentifierStr, usize>,
    subroutine_map: Option<&HashMap<IdentifierStr, SubroutineId>>,
) -> Result<(Vec<MmlCommandWithPos>, FilePos), Vec<ErrorWithPos<MmlParserError>>> {
    MmlParser::new(lines, zenlen, instruments_map, subroutine_map).parse_all()
}
