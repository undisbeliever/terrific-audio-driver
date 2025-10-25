//! MML tokenizer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::Range;

use super::{IdentifierStr, COMMENT_CHAR};

use crate::bytecode_assembler;
use crate::channel_bc_generator::SubroutineCallType;
use crate::envelope::GainMode;
use crate::errors::{ChannelError, ValueError};
use crate::file_pos::{blank_line_splitter, FilePos, Line, LineIndexRange, LineSplitter};
use crate::invert_flags::{parse_mml_invert_flags, InvertFlags};
use crate::notes::{parse_pitch_char, MmlPitch};

#[derive(Debug, Clone)]
pub enum Token<'a> {
    End,

    NewLine(LineIndexRange),

    Error(ChannelError),

    Pitch(MmlPitch),

    CallSubroutine(IdentifierStr<'a>, SubroutineCallType),
    SetInstrument(IdentifierStr<'a>),
    SetSubroutineInstrumentHint(IdentifierStr<'a>),

    Number(u32),
    HexNumber(u32),
    RelativeNumber(i32),

    StartLoop,
    SkipLastLoop,
    EndLoop,
    Tie,
    Slur,
    ChangeWholeNoteLength,
    PlaySample,
    PlayMidiNoteNumber,
    SetDefaultLength,
    Rest,
    Wait,
    PlayPitch,
    PlayPitchSampleRate,
    PlayPitchFrequency,
    PlayNoise,
    DisableNoise,
    SetOctave,
    IncrementOctave,
    DecrementOctave,
    CoarseVolume,
    FineVolume,
    DecrementVolumeParentheses,
    IncrementVolumeParentheses,
    Pan,
    PxPan,
    SetChannelInvert(InvertFlags),
    CoarseVolumeSlide,
    FineVolumeSlide,
    CoarseTremolo,
    FineTremolo,
    PanSlide,
    Panbrello,
    Quantize,
    EarlyRelease,
    Transpose,
    RelativeTranspose,
    ChannelTranspose,
    RelativeChannelTranspose,
    StartBrokenChord,
    EndBrokenChord,
    StartPortamento,
    EndPortamento,
    ManualVibrato,
    MpVibrato,
    DetuneCents,
    SetAdsr,
    SetGain(GainMode),
    TempGain(GainMode),
    PitchMod,
    Echo,
    Keyoff,
    SetSongTempo,
    SetSongTickClock,
    SetLoopPoint,
    PercentSign,
    Dot,
    Comma,
    Divider,

    Evol,
    Efb,
    EfbPlus,
    EfbMinus,
    Fir,
    Ftap,
    FtapPlus,
    FtapMinus,
    SetEchoInvert(InvertFlags),
    SetEchoDelay,

    // Temp GAIN after quantization tokens
    // (GainModeE is the Echo token)
    GainModeB,
    DetuneOrGainModeD,
    GainModeF,
    GainModeI,

    StartBytecodeAsm,
    EndBytecodeAsm,

    KeySignature(&'a str),

    // Must not contain a call subroutine instruction.
    // Using Range to remove lifetime from MmlCommand.
    BytecodeAsm(Range<usize>),

    // Used to detect `set_transpose` and `adjust_transpose` commands when processing subroutines
    TransposeAsm(Range<usize>),
}

#[derive(Clone)]
pub struct TokenWithPosition<'a> {
    pub pos: FilePos,
    pub token: Token<'a>,
    // ::TODO optimise::
    pub end: FilePos,
}

struct Scanner<'a> {
    to_process: &'a str,
    pos: FilePos,
}

impl<'a> Scanner<'a> {
    fn new(to_process: &'a str, pos: FilePos) -> Self {
        Self { to_process, pos }
    }

    fn pos(&self) -> FilePos {
        self.pos
    }

    fn first_byte(&self) -> Option<u8> {
        self.to_process.bytes().next()
    }

    fn second_byte(&self) -> Option<u8> {
        self.to_process.as_bytes().get(1).copied()
    }

    fn third_byte(&self) -> Option<u8> {
        self.to_process.as_bytes().get(2).copied()
    }

    fn match_str(&self, s: &str) -> bool {
        self.to_process.starts_with(s)
    }

    fn starts_with(&self, p: impl Fn(char) -> bool) -> bool {
        self.to_process.starts_with(p)
    }

    fn read_while(&mut self, pattern: impl Fn(u8) -> bool) -> &'a str {
        let mut char_count = 0;

        for (index, c) in self.to_process.bytes().enumerate() {
            if !pattern(c) {
                let (s1, s2) = self.to_process.split_at(index);
                self.to_process = s2;
                self.pos.line_char += char_count;
                self.pos.char_index += index.try_into().unwrap_or(0);
                return s1;
            }
            char_count += 1;
        }

        // All characters match pattern
        let s = self.to_process;
        self.to_process = "";

        self.pos.line_char += char_count;
        self.pos.char_index += s.len().try_into().unwrap_or(0);

        s
    }

    fn skip_while_char(&mut self, pattern: impl Fn(char) -> bool) -> u32 {
        let mut char_count = 0;

        for (index, c) in self.to_process.char_indices() {
            if !pattern(c) {
                self.pos.line_char += char_count;
                self.pos.char_index += index.try_into().unwrap_or(0);
                self.to_process = &self.to_process[index..];
                return char_count;
            }
            char_count += 1;
        }

        // All characters match pattern
        self.pos.line_char += char_count;
        self.pos.char_index += self.to_process.len().try_into().unwrap_or(0);
        self.to_process = "";

        char_count
    }

    // Assumes `@` and `!` and `?@` token have been advanced
    fn identifier_token(&mut self) -> Option<IdentifierStr<'a>> {
        let c = self.to_process.bytes().next();
        match c {
            Some(c) if c.is_ascii_digit() => {
                let num = self.read_while(|c| c.is_ascii_digit());
                Some(IdentifierStr::from_str(num))
            }

            Some(_) => {
                let name = self.read_while(|c| !c.is_ascii_whitespace());
                if !name.is_empty() {
                    Some(IdentifierStr::from_str(name))
                } else {
                    None
                }
            }

            None => None,
        }
    }

    fn advance_one_ascii(&mut self) {
        self.pos.line_char += 1;
        self.pos.char_index += 1;
        self.to_process = &self.to_process[1..];
    }

    fn advance_two_ascii(&mut self) {
        self.pos.line_char += 2;
        self.pos.char_index += 2;
        self.to_process = &self.to_process[2..];
    }

    fn advance_three_ascii(&mut self) {
        self.pos.line_char += 3;
        self.pos.char_index += 3;
        self.to_process = &self.to_process[3..];
    }

    fn advance_one_char(&mut self) {
        let mut c = self.to_process.char_indices();

        if c.next().is_some() {
            let s = c.as_str();
            let i = self.to_process.len() - s.len();

            self.pos.line_char += 1;
            self.pos.char_index += u32::try_from(i).unwrap();
            self.to_process = s;
        }
    }

    fn skip_whitespace(&mut self) {
        if self.starts_with(|c: char| c.is_ascii_whitespace()) {
            self.skip_while_char(|c| c.is_ascii_whitespace());
        }
    }
}

fn is_unknown_u8(c: u8) -> bool {
    match c {
        // This list must match `next()`.
        b'0'..=b'9' => false,
        b'a'..=b'g' => false,
        b'$' => false,
        b'!' | b'@' | b'+' | b'-' | b'[' | b':' | b']' | b'^' | b'&' | b'C' | b's' | b'n'
        | b'l' | b'r' | b'w' | b'o' | b'>' | b'<' | b'v' | b'V' | b'p' | b'Q' | b'q' | b'~'
        | b'A' | b'G' | b'E' | b't' | b'T' | b'L' | b'%' | b'.' | b',' | b'|' | b'_' | b'{'
        | b'}' | b'B' | b'D' | b'F' | b'I' | b'M' | b'P' | b'N' | b'i' | b'K' | b'(' | b')'
        | b'?' => false,
        b'\\' => false,
        c if c.is_ascii_whitespace() => false,
        _ => true,
    }
}

fn parse_unknown_chars<'a>(scanner: &mut Scanner<'a>) -> Token<'a> {
    // Required to skip an unknown "M" character
    scanner.advance_one_char();

    let n_chars = scanner.skip_while_char(|c| match c.try_into() {
        Ok(b) => is_unknown_u8(b),
        Err(_) => true,
    });

    Token::Error(ChannelError::UnknownCharacters(n_chars + 1))
}

fn next_token<'a>(scanner: &mut Scanner<'a>) -> Option<TokenWithPosition<'a>> {
    macro_rules! one_ascii_token {
        ($t:expr) => {{
            scanner.advance_one_ascii();
            $t
        }};
    }
    macro_rules! two_ascii_token {
        ($t:expr) => {{
            scanner.advance_two_ascii();
            $t
        }};
    }
    macro_rules! three_ascii_token {
        ($t:expr) => {{
            scanner.advance_three_ascii();
            $t
        }};
    }

    scanner.skip_whitespace();

    let pos = scanner.pos();

    let c1 = match scanner.first_byte() {
        Some(c) => c,
        None => {
            return None;
        }
    };

    let token = match c1 {
        b'0'..=b'9' => {
            let num = scanner.read_while(|c| c.is_ascii_digit());
            match num.parse() {
                Ok(i) => Token::Number(i),
                Err(_) => Token::Error(ValueError::CannotParseUnsigned(num.to_owned()).into()),
            }
        }
        b'$' => {
            // Skip '$'
            scanner.advance_one_ascii();

            let num = scanner.read_while(|c| c.is_ascii_hexdigit());
            if !num.is_empty() {
                match u32::from_str_radix(num, 16) {
                    Ok(i) => Token::HexNumber(i),
                    Err(_) => Token::Error(ValueError::CannotParseHex(num.to_owned()).into()),
                }
            } else {
                Token::Error(ValueError::NoHexDigits.into())
            }
        }
        b'a'..=b'g' => {
            let pitch = c1.into();
            let pitch = parse_pitch_char(pitch).unwrap();

            // skip pitch character
            scanner.advance_one_ascii();

            let natural = match scanner.first_byte() {
                Some(b'=') => {
                    scanner.advance_one_ascii();
                    true
                }
                _ => false,
            };

            let mut semitone_offset: i8 = 0;
            let offset_str = scanner.read_while(|c| c == b'-' || c == b'+');
            for o in offset_str.bytes() {
                if o == b'+' {
                    semitone_offset = semitone_offset.saturating_add(1);
                } else {
                    semitone_offset = semitone_offset.saturating_sub(1);
                }
            }

            Token::Pitch(MmlPitch::new(pitch, natural, semitone_offset))
        }

        b'!' => {
            scanner.advance_one_ascii();
            match scanner.identifier_token() {
                Some(id) => Token::CallSubroutine(id, SubroutineCallType::Mml),
                None => Token::Error(ChannelError::NoSubroutine),
            }
        }

        b'@' => {
            scanner.advance_one_ascii();

            match scanner.identifier_token() {
                Some(id) => Token::SetInstrument(id),
                None => Token::Error(ChannelError::NoInstrument),
            }
        }

        b'?' => match scanner.second_byte() {
            Some(b'@') => {
                scanner.advance_two_ascii();
                match scanner.identifier_token() {
                    Some(id) => Token::SetSubroutineInstrumentHint(id),
                    None => Token::Error(ChannelError::NoInstrumentHint),
                }
            }
            _ => parse_unknown_chars(scanner),
        },

        b'+' => {
            // skip '+'
            scanner.advance_one_ascii();
            let num = scanner.read_while(|c| c.is_ascii_digit());
            match num.parse() {
                Ok(i) => Token::RelativeNumber(i),
                Err(_) => Token::Error(ChannelError::ValueError(ValueError::CannotParseSigned(
                    num.to_owned(),
                ))),
            }
        }

        b'-' => {
            let num = scanner.read_while(|c| c == b'-' || c.is_ascii_digit());
            match num.parse() {
                Ok(i) => Token::RelativeNumber(i),
                Err(_) => Token::Error(ChannelError::ValueError(ValueError::CannotParseSigned(
                    num.to_owned(),
                ))),
            }
        }

        b'[' => one_ascii_token!(Token::StartLoop),
        b':' => one_ascii_token!(Token::SkipLastLoop),
        b']' => one_ascii_token!(Token::EndLoop),
        b'^' => one_ascii_token!(Token::Tie),
        b'&' => one_ascii_token!(Token::Slur),
        b'C' => one_ascii_token!(Token::ChangeWholeNoteLength),
        b's' => one_ascii_token!(Token::PlaySample),
        b'n' => one_ascii_token!(Token::PlayMidiNoteNumber),
        b'l' => one_ascii_token!(Token::SetDefaultLength),
        b'r' => one_ascii_token!(Token::Rest),
        b'w' => one_ascii_token!(Token::Wait),
        b'o' => one_ascii_token!(Token::SetOctave),
        b'>' => one_ascii_token!(Token::IncrementOctave),
        b'<' => one_ascii_token!(Token::DecrementOctave),
        b'Q' => one_ascii_token!(Token::Quantize),
        b'q' => one_ascii_token!(Token::EarlyRelease),
        b'~' => one_ascii_token!(Token::ManualVibrato),
        b'A' => one_ascii_token!(Token::SetAdsr),
        b'E' => one_ascii_token!(Token::Echo),
        b'K' => one_ascii_token!(Token::Keyoff),
        b't' => one_ascii_token!(Token::SetSongTempo),
        b'T' => one_ascii_token!(Token::SetSongTickClock),
        b'L' => one_ascii_token!(Token::SetLoopPoint),
        b'%' => one_ascii_token!(Token::PercentSign),
        b'.' => one_ascii_token!(Token::Dot),
        b',' => one_ascii_token!(Token::Comma),
        b'|' => one_ascii_token!(Token::Divider),

        b'B' => one_ascii_token!(Token::GainModeB),
        b'F' => one_ascii_token!(Token::GainModeF),
        b'I' => one_ascii_token!(Token::GainModeI),

        b'D' => one_ascii_token!(Token::DetuneOrGainModeD),

        b'P' => match scanner.second_byte() {
            Some(b'M') => two_ascii_token!(Token::PitchMod),
            Some(b'F') => two_ascii_token!(Token::PlayPitchFrequency),
            Some(b'R') => two_ascii_token!(Token::PlayPitchSampleRate),
            _ => one_ascii_token!(Token::PlayPitch),
        },

        b'N' => {
            let c2 = scanner.second_byte();

            match c2 {
                Some(b'-') => two_ascii_token!(Token::DisableNoise),
                _ => one_ascii_token!(Token::PlayNoise),
            }
        }

        // Pan might be 1 or 2 characters
        b'p' => {
            let c2 = scanner.second_byte();

            match c2 {
                Some(b'x') => two_ascii_token!(Token::PxPan),
                Some(b's') => two_ascii_token!(Token::PanSlide),
                Some(b'~') => two_ascii_token!(Token::Panbrello),
                _ => one_ascii_token!(Token::Pan),
            }
        }

        b'v' => match scanner.second_byte() {
            Some(b's') => two_ascii_token!(Token::CoarseVolumeSlide),
            Some(b'~') => two_ascii_token!(Token::CoarseTremolo),
            _ => one_ascii_token!(Token::CoarseVolume),
        },

        b'V' => match scanner.second_byte() {
            Some(b's') => two_ascii_token!(Token::FineVolumeSlide),
            Some(b'~') => two_ascii_token!(Token::FineTremolo),
            _ => one_ascii_token!(Token::FineVolume),
        },

        b'(' => one_ascii_token!(Token::DecrementVolumeParentheses),
        b')' => one_ascii_token!(Token::IncrementVolumeParentheses),

        // Gain might use 2 or 3 chacters
        b'G' => {
            let c2 = scanner.second_byte();

            if c2 == Some(b'T') {
                two_ascii_token!(Token::TempGain(GainMode::Raw))
            } else if let Some(mode) = c2.and_then(GainMode::from_u8_char) {
                match scanner.third_byte() {
                    Some(b'T') => three_ascii_token!(Token::TempGain(mode)),
                    _ => two_ascii_token!(Token::SetGain(mode)),
                }
            } else {
                one_ascii_token!(Token::SetGain(GainMode::Raw))
            }
        }

        b'_' => {
            if scanner.match_str("__M") {
                three_ascii_token!(Token::RelativeChannelTranspose)
            } else if scanner.match_str("_M") {
                two_ascii_token!(Token::ChannelTranspose)
            } else if scanner.match_str("__") {
                two_ascii_token!(Token::RelativeTranspose)
            } else if scanner.match_str("_{") {
                scanner.advance_two_ascii();
                let signature = scanner.read_while(|b: u8| b != b'}');

                match scanner.first_byte() {
                    Some(b'}') => {
                        scanner.advance_one_ascii();
                        Token::KeySignature(signature.trim())
                    }
                    _ => Token::Error(ChannelError::MissingEndKeySignature),
                }
            } else {
                one_ascii_token!(Token::Transpose)
            }
        }
        b'{' => match scanner.second_byte() {
            Some(b'{') => two_ascii_token!(Token::StartBrokenChord),
            _ => one_ascii_token!(Token::StartPortamento),
        },
        b'}' => match scanner.second_byte() {
            Some(b'}') => two_ascii_token!(Token::EndBrokenChord),
            _ => one_ascii_token!(Token::EndPortamento),
        },
        b'M' => match scanner.second_byte() {
            Some(b'P') => two_ascii_token!(Token::MpVibrato),
            Some(b'D') => two_ascii_token!(Token::DetuneCents),
            _ => parse_unknown_chars(scanner),
        },

        b'i' => {
            scanner.advance_one_ascii();

            let flags = scanner.read_while(|b: u8| !b.is_ascii_whitespace());
            if flags.is_empty() {
                Token::SetChannelInvert(InvertFlags::BOTH)
            } else {
                match parse_mml_invert_flags(flags) {
                    Ok(f) => Token::SetChannelInvert(f),
                    Err(e) => Token::Error(e.into()),
                }
            }
        }

        b'\\' => {
            scanner.advance_one_ascii();

            match scanner.read_while(|b: u8| !b.is_ascii_whitespace()) {
                "asm" => Token::StartBytecodeAsm,
                "evol" => Token::Evol,
                "efb" => Token::Efb,
                "efb+" => Token::EfbPlus,
                "efb-" => Token::EfbMinus,
                "fir" => Token::Fir,
                "ftap" => Token::Ftap,
                "ftap+" => Token::FtapPlus,
                "ftap-" => Token::FtapMinus,
                "edl" => Token::SetEchoDelay,
                "ei" => {
                    scanner.skip_whitespace();

                    let flags = scanner.read_while(|b: u8| !b.is_ascii_whitespace());
                    match parse_mml_invert_flags(flags) {
                        Ok(f) => Token::SetEchoInvert(f),
                        Err(e) => Token::Error(e.into()),
                    }
                }
                "" => Token::Error(ChannelError::NoSlashCommand),
                s => Token::Error(ChannelError::InvalidSlashCommand(s.to_owned())),
            }
        }

        _ => parse_unknown_chars(scanner),
    };

    Some(TokenWithPosition {
        pos,
        token,
        end: scanner.pos(),
    })
}

fn parse_bytecode_asm<'a>(
    tokens: &mut Vec<TokenWithPosition<'a>>,
    scanner: &mut Scanner<'a>,
    remaining_lines: &mut LineSplitter<'a>,
) {
    scanner.skip_whitespace();

    if scanner.first_byte() != Some(b'{') {
        tokens.push(TokenWithPosition {
            pos: scanner.pos(),
            token: Token::Error(ChannelError::NoBraceAfterAsm),
            end: scanner.pos(),
        });

        return;
    }
    scanner.advance_one_ascii();

    tokens.push(TokenWithPosition {
        pos: scanner.pos(),
        token: Token::StartBytecodeAsm,
        end: scanner.pos(),
    });

    scanner.skip_whitespace();

    loop {
        scanner.skip_whitespace();

        match scanner.first_byte() {
            None => match remaining_lines.next() {
                Some(l) => {
                    let line_range = l.index_range();
                    let l = l.trim_comments(COMMENT_CHAR);

                    tokens.push(TokenWithPosition {
                        pos: scanner.pos(),
                        token: Token::NewLine(line_range),
                        end: scanner.pos(),
                    });
                    *scanner = Scanner::new(l.text, l.position);
                }
                None => {
                    tokens.push(TokenWithPosition {
                        pos: scanner.pos(),
                        token: Token::Error(ChannelError::MissingEndAsm),
                        end: scanner.pos(),
                    });
                    return;
                }
            },
            Some(b'}') => {
                let pos = scanner.pos();
                scanner.advance_one_ascii();

                tokens.push(TokenWithPosition {
                    pos,
                    token: Token::EndBytecodeAsm,
                    end: scanner.pos(),
                });
                return;
            }
            Some(b'|') => {
                scanner.advance_one_ascii();
            }
            _ => {
                let pos = scanner.pos();
                let asm = scanner.read_while(|c: u8| c != b'|' && c != b'}');

                // This assert ensures no infinite loops
                assert!(!asm.is_empty());

                if let Some(token) = parse_call_subroutine_asm(asm) {
                    tokens.push(TokenWithPosition {
                        pos,
                        token,
                        end: scanner.pos(),
                    });
                } else {
                    let start = usize::try_from(pos.char_index()).unwrap();

                    if asm.starts_with(bytecode_assembler::SET_TRANSPOSE)
                        | asm.starts_with(bytecode_assembler::ADJUST_TRANSPOSE)
                    {
                        tokens.push(TokenWithPosition {
                            pos,
                            token: Token::TransposeAsm(start..(start + asm.len())),
                            end: scanner.pos(),
                        });
                    } else {
                        tokens.push(TokenWithPosition {
                            pos,
                            token: Token::BytecodeAsm(start..(start + asm.len())),
                            end: scanner.pos(),
                        });
                    }

                    if scanner.first_byte() == Some(b'|') {
                        scanner.advance_one_ascii();
                    }
                }
            }
        }
    }
}

fn parse_call_subroutine_asm(asm: &str) -> Option<Token<'_>> {
    if !asm.starts_with(bytecode_assembler::CALL_SUBROUTINE) {
        return None;
    }

    let (instruction, argument) = asm.split_once(|c: char| c.is_ascii_whitespace())?;
    let argument = argument.trim();

    if argument.bytes().any(|c| c.is_ascii_whitespace()) {
        return None;
    }

    match instruction {
        bytecode_assembler::CALL_SUBROUTINE => Some(Token::CallSubroutine(
            IdentifierStr::from_str(argument),
            SubroutineCallType::Asm,
        )),
        bytecode_assembler::CALL_SUBROUTINE_AND_DISABLE_VIBRATO => Some(Token::CallSubroutine(
            IdentifierStr::from_str(argument),
            SubroutineCallType::AsmDisableVibrato,
        )),
        _ => None,
    }
}

pub struct MmlTokens<'a> {
    tokens: Vec<TokenWithPosition<'a>>,
    end_pos: FilePos,
}

impl Default for MmlTokens<'_> {
    fn default() -> Self {
        Self {
            tokens: Vec::new(),
            end_pos: blank_pos(),
        }
    }
}

impl<'a> MmlTokens<'a> {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            end_pos: blank_pos(),
        }
    }

    pub fn new_with_line(
        line: Line<'a>,
        entire_line_range: LineIndexRange,
        remaining_lines: &mut LineSplitter<'a>,
    ) -> Self {
        let mut s = Self::new();
        s.parse_line(line, entire_line_range, remaining_lines);
        s
    }

    /// Safety: Panics if text len() is > u32::MAX
    pub fn tokenize_one_line(text: &'a str) -> Self {
        let file_pos = FilePos {
            line_number: 1,
            line_char: 0,
            char_index: 0,
        };

        Self::new_with_line(
            Line {
                text,
                position: file_pos,
            },
            LineIndexRange {
                start: 0,
                end: text.len().try_into().unwrap(),
            },
            &mut blank_line_splitter(),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn extend(&mut self, tokens: &MmlTokens<'a>) {
        self.tokens.extend(tokens.tokens.iter().cloned());
        self.end_pos = tokens.end_pos;
    }

    pub fn parse_line(
        &mut self,
        line: Line<'a>,
        entire_line_range: LineIndexRange,
        remaining_lines: &mut LineSplitter<'a>,
    ) {
        debug_assert!(entire_line_range.start <= line.position.char_index);
        debug_assert!(entire_line_range.end >= line.position.char_index);
        debug_assert!(
            entire_line_range.end
                >= line.position.char_index + u32::try_from(line.text.len()).unwrap()
        );

        self.tokens.push(TokenWithPosition {
            pos: self.end_pos,
            token: Token::NewLine(entire_line_range),
            end: self.end_pos,
        });

        let mut scanner = Scanner::new(line.text, line.position);

        while let Some(t) = next_token(&mut scanner) {
            match t.token {
                Token::StartBytecodeAsm => {
                    parse_bytecode_asm(&mut self.tokens, &mut scanner, remaining_lines);
                }
                _ => self.tokens.push(t),
            }
        }

        self.end_pos = scanner.pos();
    }

    pub fn token_iter(&self) -> impl Iterator<Item = &Token<'a>> {
        self.tokens.iter().map(|t| &t.token)
    }

    pub fn first_token(&self) -> Option<&Token<'a>> {
        self.tokens.first().map(|t| &t.token)
    }
}

// ::TODO find better name - not a rust iterator::
pub(crate) struct PeekableTokenIterator<'a> {
    next: TokenWithPosition<'a>,
    remaining: std::vec::IntoIter<TokenWithPosition<'a>>,
    prev_end_pos: FilePos,
    end_pos: FilePos,
}

impl<'a> PeekableTokenIterator<'a> {
    pub fn new(tokens: MmlTokens<'a>) -> Self {
        let mut iter = tokens.tokens.into_iter();

        Self {
            next: iter
                .next()
                .unwrap_or_else(|| Self::end_token(tokens.end_pos)),
            remaining: iter,
            prev_end_pos: blank_pos(),
            end_pos: tokens.end_pos,
        }
    }

    fn end_token(pos: FilePos) -> TokenWithPosition<'a> {
        TokenWithPosition {
            pos,
            token: Token::End,
            end: pos,
        }
    }

    pub fn peek_pos(&self) -> FilePos {
        self.next.pos
    }

    pub fn peek(&self) -> &Token<'a> {
        &self.next.token
    }

    pub fn prev_end_pos(&self) -> &FilePos {
        &self.prev_end_pos
    }

    pub fn next(&mut self) {
        self.prev_end_pos = self.next.end;

        self.next = self
            .remaining
            .next()
            .unwrap_or_else(|| Self::end_token(self.end_pos));
    }

    pub fn peek_and_next(&mut self) -> (FilePos, Token<'a>) {
        let mut n = self
            .remaining
            .next()
            .unwrap_or_else(|| Self::end_token(self.end_pos));

        std::mem::swap(&mut n, &mut self.next);

        self.prev_end_pos = n.end;
        (n.pos, n.token)
    }
}

fn blank_pos() -> FilePos {
    FilePos {
        line_number: 0,
        line_char: 0,
        char_index: 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Confirm `Tokenizer::is_unknown_u8()` matches `Tokenizer::next()`
    #[test]
    fn test_is_unknown_u8() {
        // List of starting chars in a two-character token where first character is NOT a token.
        // ie, "MP" is a token while "M" is not a token.
        const SPECIAL_CHARS: [u8; 2] = [b'M', b'?'];

        for c in 0..127_u8 {
            let s = [c];
            let text = std::str::from_utf8(&s).unwrap();

            let mut scanner = Scanner::new(text, blank_pos());

            let token = next_token(&mut scanner).map(|t| t.token);
            let is_unknown_token = matches!(
                token,
                Some(Token::Error(ChannelError::UnknownCharacters(_)))
            );

            if SPECIAL_CHARS.contains(&c) {
                // Confirm a single SPECIAL_CHARS character is not a token according to `next()`
                assert!(is_unknown_token);
                // Confirm a single SPECIAL_CHARS character is a token according to `is_unknown_u8()`
                assert!(!is_unknown_u8(c), "is_unknown_u8() mismatch for char: {c}");
            } else {
                assert_eq!(
                    is_unknown_token,
                    is_unknown_u8(c),
                    "is_unknown_u8() mismatch for char: {c}"
                );
            }
        }
    }
}
