//! MML tokenizer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::bytecode::SubroutineId;
use crate::errors::{MmlError, ValueError};
use crate::file_pos::{FilePos, Line};
use crate::notes::{parse_pitch_char, MmlPitch};

use super::IdentifierStr;

#[derive(Debug)]
pub enum Token {
    End,
    EndOfLine,

    Error(MmlError),

    Pitch(MmlPitch),

    // I am unable to use `&str` here and still have a peekable tokenizer.
    // I keep runnng into lifetime issues (lifetime may not live long enough, or use of a mutable borrowed value).
    //
    // My options are to:
    //  a) Clone &str into a String, but I don't want lots of temporary Strings that are going to be immediatly dropped.
    //  b) Use a range in the Token and extract the &str in the calling code.
    //  c) Convert the &str into a SubroutineId/instrument-index in the Tokenizer.  This adds some complexity to the Tokenizer code.
    //
    // I have chosen option C as it simplifies the MML Command Parser code.
    CallSubroutine(SubroutineId),
    SetInstrument(usize),

    Number(u32),
    RelativeNumber(i32),

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
    PercentSign,
    Dot,
    Comma,
    Divider,
}

pub(crate) struct Scanner<'a> {
    to_process: &'a str,
    pos: FilePos,
}

impl<'a> Scanner<'a> {
    fn new(line: &Line<'a>) -> Self {
        Self {
            to_process: line.text,
            pos: line.position,
        }
    }

    fn blank() -> Self {
        Self {
            to_process: "",
            pos: blank_pos(),
        }
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

    fn starts_with(&self, p: impl Fn(char) -> bool) -> bool {
        self.to_process.starts_with(p)
    }

    fn read_while(&mut self, pattern: impl Fn(u8) -> bool) -> &str {
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

    fn skip_while_u8(&mut self, pattern: impl Fn(u8) -> bool) -> u32 {
        let mut char_count = 0;

        for (index, c) in self.to_process.bytes().enumerate() {
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

    // Assumes `self.current_line[0]` is '@' or '!'
    fn identifier_token(&mut self) -> Option<IdentifierStr> {
        // Advance the '@' or '!' token
        self.advance_one();

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

    fn advance_one(&mut self) {
        self.pos.line_char += 1;
        self.pos.char_index += 1;
        self.to_process = &self.to_process[1..];
    }

    fn advance_two(&mut self) {
        self.pos.line_char += 2;
        self.pos.char_index += 2;
        self.to_process = &self.to_process[2..];
    }
}

pub(crate) struct Tokenizer<'a> {
    remaining_lines: &'a [Line<'a>],
    scanner: Scanner<'a>,

    instruments_map: &'a HashMap<IdentifierStr<'a>, usize>,
    subroutine_map: Option<&'a HashMap<IdentifierStr<'a>, SubroutineId>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        lines: &'a [Line<'a>],
        instruments_map: &'a HashMap<IdentifierStr<'a>, usize>,
        subroutine_map: Option<&'a HashMap<IdentifierStr<'a>, SubroutineId>>,
    ) -> Self {
        let (scanner, remaining_lines) = match lines.split_first() {
            Some((first, remaining)) => (Scanner::new(first), remaining),
            None => (Scanner::blank(), lines),
        };

        Self {
            remaining_lines,
            scanner,
            instruments_map,
            subroutine_map,
        }
    }

    fn is_unknown_u8(c: u8) -> bool {
        match c {
            // This list must match `next()`.
            b'0'..=b'9' => false,
            b'a'..=b'g' => false,
            b'!' | b'@' | b'+' | b'-' | b'[' | b':' | b']' | b'^' | b'&' | b'C' | b'n' | b'l'
            | b'r' | b'o' | b'>' | b'<' | b'v' | b'V' | b'p' | b'Q' | b'~' | b'E' | b't' | b'T'
            | b'L' | b'%' | b'.' | b',' | b'|' | b'_' | b'{' | b'}' | b'M' => false,
            c if c.is_ascii_whitespace() => false,
            _ => true,
        }
    }

    fn parse_unknown_chars(&mut self) -> Token {
        // Required to skip an unknown "M" character
        self.scanner.advance_one();

        let mut n_chars = 1;

        n_chars += self.scanner.skip_while_u8(Self::is_unknown_u8);
        while self.scanner.starts_with(|c: char| c.is_ascii_whitespace()) {
            let whitespace_chars = self.scanner.skip_while_char(|c| c.is_ascii_whitespace());
            let unknown_chars = self.scanner.skip_while_u8(Self::is_unknown_u8);

            if unknown_chars > 0 {
                n_chars += whitespace_chars;
                n_chars += unknown_chars;
            }
        }

        Token::Error(MmlError::UnknownCharacters(n_chars))
    }

    pub fn pos(&self) -> FilePos {
        self.scanner.pos()
    }

    pub fn next(&mut self) -> (FilePos, Token) {
        macro_rules! one_ascii_token {
            ($t:expr) => {{
                self.scanner.advance_one();
                $t
            }};
        }
        macro_rules! two_ascii_token {
            ($t:expr) => {{
                self.scanner.advance_two();
                $t
            }};
        }

        // Skip whitespace
        if self.scanner.starts_with(|c: char| c.is_ascii_whitespace()) {
            self.scanner.skip_while_char(|c| c.is_ascii_whitespace());
        }

        let pos = self.scanner.pos();

        let c1 = match self.scanner.first_byte() {
            Some(c) => c,
            None => {
                // End of line
                // Go to next line (if possible)
                return match self.remaining_lines.split_first() {
                    Some((first, remaining)) => {
                        self.scanner = Scanner::new(first);
                        self.remaining_lines = remaining;

                        (pos, Token::EndOfLine)
                    }
                    None => (pos, Token::End),
                };
            }
        };

        let t = match c1 {
            b'0'..=b'9' => {
                let num = self.scanner.read_while(|c| c.is_ascii_digit());
                match num.parse() {
                    Ok(i) => Token::Number(i),
                    Err(_) => Token::Error(MmlError::ValueError(ValueError::CannotParseUnsigned(
                        num.to_owned(),
                    ))),
                }
            }
            b'a'..=b'g' => {
                let pitch = c1.try_into().unwrap();
                let pitch = parse_pitch_char(pitch).unwrap();

                // skip pitch character
                self.scanner.advance_one();

                let mut semitone_offset: i8 = 0;
                let offset_str = self.scanner.read_while(|c| c == b'-' || c == b'+');
                for o in offset_str.bytes() {
                    if o == b'+' {
                        semitone_offset = semitone_offset.saturating_add(1);
                    } else {
                        semitone_offset = semitone_offset.saturating_sub(1);
                    }
                }

                Token::Pitch(MmlPitch::new(pitch, semitone_offset))
            }

            b'!' => match self.scanner.identifier_token() {
                Some(id) => match self.subroutine_map {
                    Some(sm) => match sm.get(&id) {
                        Some(i) => Token::CallSubroutine(*i),
                        None => {
                            Token::Error(MmlError::CannotFindSubroutine(id.as_str().to_owned()))
                        }
                    },
                    None => Token::Error(MmlError::CannotCallSubroutineInASubroutine),
                },
                None => Token::Error(MmlError::NoSubroutine),
            },

            b'@' => match self.scanner.identifier_token() {
                Some(id) => match self.instruments_map.get(&id) {
                    Some(i) => Token::SetInstrument(*i),
                    None => Token::Error(MmlError::CannotFindInstrument(id.as_str().to_owned())),
                },
                None => Token::Error(MmlError::NoInstrument),
            },

            b'+' => {
                // skip '+'
                self.scanner.advance_one();
                let num = self.scanner.read_while(|c| c.is_ascii_digit());
                match num.parse() {
                    Ok(i) => Token::RelativeNumber(i),
                    Err(_) => Token::Error(MmlError::ValueError(ValueError::CannotParseSigned(
                        num.to_owned(),
                    ))),
                }
            }

            b'-' => {
                let num = self.scanner.read_while(|c| c == b'-' || c.is_ascii_digit());
                match num.parse() {
                    Ok(i) => Token::RelativeNumber(i),
                    Err(_) => Token::Error(MmlError::ValueError(ValueError::CannotParseSigned(
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
            b'n' => one_ascii_token!(Token::PlayMidiNoteNumber),
            b'l' => one_ascii_token!(Token::SetDefaultLength),
            b'r' => one_ascii_token!(Token::Rest),
            b'o' => one_ascii_token!(Token::SetOctave),
            b'>' => one_ascii_token!(Token::IncrementOctave),
            b'<' => one_ascii_token!(Token::DecrementOctave),
            b'v' => one_ascii_token!(Token::CoarseVolume),
            b'V' => one_ascii_token!(Token::FineVolume),
            b'p' => one_ascii_token!(Token::Pan),
            b'Q' => one_ascii_token!(Token::Quantize),
            b'~' => one_ascii_token!(Token::ManualVibrato),
            b'E' => one_ascii_token!(Token::Echo),
            b't' => one_ascii_token!(Token::SetSongTempo),
            b'T' => one_ascii_token!(Token::SetSongTickClock),
            b'L' => one_ascii_token!(Token::SetLoopPoint),
            b'%' => one_ascii_token!(Token::PercentSign),
            b'.' => one_ascii_token!(Token::Dot),
            b',' => one_ascii_token!(Token::Comma),
            b'|' => one_ascii_token!(Token::Divider),

            // Possibly multiple character tokens
            b'_' | b'{' | b'}' | b'M' => {
                let c2 = self.scanner.second_byte();
                match (c1, c2) {
                    (b'_', Some(b'_')) => two_ascii_token!(Token::RelativeTranspose),
                    (b'{', Some(b'{')) => two_ascii_token!(Token::StartBrokenChord),
                    (b'}', Some(b'}')) => two_ascii_token!(Token::EndBrokenChord),
                    (b'M', Some(b'P')) => two_ascii_token!(Token::MpVibrato),

                    (b'_', _) => one_ascii_token!(Token::Transpose),
                    (b'{', _) => one_ascii_token!(Token::StartPortamento),
                    (b'}', _) => one_ascii_token!(Token::EndPortamento),

                    // This should not happen
                    (_, _) => self.parse_unknown_chars(),
                }
            }

            _ => self.parse_unknown_chars(),
        };

        (pos, t)
    }
}
pub(crate) struct PeekingTokenizer<'a> {
    tokenizer: Tokenizer<'a>,
    prev_end_pos: FilePos,
    next: (FilePos, Token),
}

impl PeekingTokenizer<'_> {
    pub fn new<'a>(
        lines: &'a [Line],
        instruments_map: &'a HashMap<IdentifierStr<'a>, usize>,
        subroutine_map: Option<&'a HashMap<IdentifierStr<'a>, SubroutineId>>,
    ) -> PeekingTokenizer<'a> {
        let mut tokenizer = Tokenizer::new(lines, instruments_map, subroutine_map);
        let next = tokenizer.next();

        PeekingTokenizer {
            tokenizer,
            next,
            prev_end_pos: blank_pos(),
        }
    }

    pub fn peek(&self) -> &Token {
        &self.next.1
    }

    pub fn peek_pos(&self) -> &FilePos {
        &self.next.0
    }

    pub fn prev_end_pos(&self) -> &FilePos {
        &self.prev_end_pos
    }

    pub fn next(&mut self) {
        self.prev_end_pos = self.tokenizer.pos();
        self.next = self.tokenizer.next();
    }

    pub fn peek_and_next(&mut self) -> (FilePos, Token) {
        self.prev_end_pos = self.tokenizer.pos();
        let mut n = self.tokenizer.next();

        std::mem::swap(&mut n, &mut self.next);
        n
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
        let blank_instruments = HashMap::new();

        // List of starting chars in a two-character token where first character is NOT a token.
        // ie, "MP" is a token while "M" is not a token.
        const SPECIAL_CHARS: [u8; 1] = [b'M'];

        for c in 0..127_u8 {
            let s = [c];
            let lines = [Line {
                text: std::str::from_utf8(&s).unwrap(),
                position: blank_pos(),
            }];

            let mut tokeninzer = Tokenizer::new(&lines, &blank_instruments, None);
            let token = tokeninzer.next().1;
            let is_unknown_token = matches!(token, Token::Error(MmlError::UnknownCharacters(_)));

            if SPECIAL_CHARS.contains(&c) {
                // Confirm a single SPECIAL_CHARS character is not a token according to `next()`
                assert!(is_unknown_token == true);
                // Confirm a single SPECIAL_CHARS character is a token according to `is_unknown_u8()`
                assert!(
                    Tokenizer::is_unknown_u8(c) == false,
                    "is_unknown_u8() mismatch for char: {c}"
                );
            } else {
                assert_eq!(
                    is_unknown_token,
                    Tokenizer::is_unknown_u8(c),
                    "is_unknown_u8() mismatch for char: {c}"
                );
            }
        }
    }
}
