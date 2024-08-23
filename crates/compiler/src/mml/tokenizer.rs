//! MML tokenizer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::IdentifierStr;

use crate::envelope::GainMode;
use crate::errors::{MmlError, ValueError};
use crate::file_pos::{FilePos, Line, LineIndexRange};
use crate::notes::{parse_pitch_char, MmlPitch};

#[derive(Debug)]
pub enum Token<'a> {
    End,

    NewLine(LineIndexRange),

    Error(MmlError),

    Pitch(MmlPitch),

    CallSubroutine(IdentifierStr<'a>),
    SetInstrument(IdentifierStr<'a>),

    Number(u32),
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
    SetAdsr,
    SetGain(GainMode),
    Echo,
    SetSongTempo,
    SetSongTickClock,
    SetLoopPoint,
    PercentSign,
    Dot,
    Comma,
    Divider,
}

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
    fn identifier_token(&mut self) -> Option<IdentifierStr<'a>> {
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

fn is_unknown_u8(c: u8) -> bool {
    match c {
        // This list must match `next()`.
        b'0'..=b'9' => false,
        b'a'..=b'g' => false,
        b'$' => false,
        b'!' | b'@' | b'+' | b'-' | b'[' | b':' | b']' | b'^' | b'&' | b'C' | b's' | b'n'
        | b'l' | b'r' | b'w' | b'o' | b'>' | b'<' | b'v' | b'V' | b'p' | b'Q' | b'~' | b'A'
        | b'G' | b'E' | b't' | b'T' | b'L' | b'%' | b'.' | b',' | b'|' | b'_' | b'{' | b'}'
        | b'M' => false,
        c if c.is_ascii_whitespace() => false,
        _ => true,
    }
}

fn parse_unknown_chars<'a>(scanner: &mut Scanner<'a>) -> Token<'a> {
    // Required to skip an unknown "M" character
    scanner.advance_one();

    let mut n_chars = 1;

    n_chars += scanner.skip_while_u8(is_unknown_u8);
    while scanner.starts_with(|c: char| c.is_ascii_whitespace()) {
        let whitespace_chars = scanner.skip_while_char(|c| c.is_ascii_whitespace());
        let unknown_chars = scanner.skip_while_u8(is_unknown_u8);

        if unknown_chars > 0 {
            n_chars += whitespace_chars;
            n_chars += unknown_chars;
        }
    }

    Token::Error(MmlError::UnknownCharacters(n_chars))
}

fn next_token<'a>(scanner: &mut Scanner<'a>) -> Option<TokenWithPosition<'a>> {
    macro_rules! one_ascii_token {
        ($t:expr) => {{
            scanner.advance_one();
            $t
        }};
    }
    macro_rules! two_ascii_token {
        ($t:expr) => {{
            scanner.advance_two();
            $t
        }};
    }

    // Skip whitespace
    if scanner.starts_with(|c: char| c.is_ascii_whitespace()) {
        scanner.skip_while_char(|c| c.is_ascii_whitespace());
    }

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
            scanner.advance_one();

            let num = scanner.read_while(|c| c.is_ascii_hexdigit());
            if !num.is_empty() {
                match u32::from_str_radix(num, 16) {
                    Ok(i) => Token::Number(i),
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
            scanner.advance_one();

            let mut semitone_offset: i8 = 0;
            let offset_str = scanner.read_while(|c| c == b'-' || c == b'+');
            for o in offset_str.bytes() {
                if o == b'+' {
                    semitone_offset = semitone_offset.saturating_add(1);
                } else {
                    semitone_offset = semitone_offset.saturating_sub(1);
                }
            }

            Token::Pitch(MmlPitch::new(pitch, semitone_offset))
        }

        b'!' => match scanner.identifier_token() {
            Some(id) => Token::CallSubroutine(id),
            None => Token::Error(MmlError::NoSubroutine),
        },

        b'@' => match scanner.identifier_token() {
            Some(id) => Token::SetInstrument(id),
            None => Token::Error(MmlError::NoInstrument),
        },

        b'+' => {
            // skip '+'
            scanner.advance_one();
            let num = scanner.read_while(|c| c.is_ascii_digit());
            match num.parse() {
                Ok(i) => Token::RelativeNumber(i),
                Err(_) => Token::Error(MmlError::ValueError(ValueError::CannotParseSigned(
                    num.to_owned(),
                ))),
            }
        }

        b'-' => {
            let num = scanner.read_while(|c| c == b'-' || c.is_ascii_digit());
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
        b's' => one_ascii_token!(Token::PlaySample),
        b'n' => one_ascii_token!(Token::PlayMidiNoteNumber),
        b'l' => one_ascii_token!(Token::SetDefaultLength),
        b'r' => one_ascii_token!(Token::Rest),
        b'w' => one_ascii_token!(Token::Wait),
        b'o' => one_ascii_token!(Token::SetOctave),
        b'>' => one_ascii_token!(Token::IncrementOctave),
        b'<' => one_ascii_token!(Token::DecrementOctave),
        b'v' => one_ascii_token!(Token::CoarseVolume),
        b'V' => one_ascii_token!(Token::FineVolume),
        b'p' => one_ascii_token!(Token::Pan),
        b'Q' => one_ascii_token!(Token::Quantize),
        b'~' => one_ascii_token!(Token::ManualVibrato),
        b'A' => one_ascii_token!(Token::SetAdsr),
        b'E' => one_ascii_token!(Token::Echo),
        b't' => one_ascii_token!(Token::SetSongTempo),
        b'T' => one_ascii_token!(Token::SetSongTickClock),
        b'L' => one_ascii_token!(Token::SetLoopPoint),
        b'%' => one_ascii_token!(Token::PercentSign),
        b'.' => one_ascii_token!(Token::Dot),
        b',' => one_ascii_token!(Token::Comma),
        b'|' => one_ascii_token!(Token::Divider),

        // Gain might use 2 chacters
        b'G' => {
            let c2 = scanner.second_byte();

            match c2.and_then(GainMode::from_u8_char) {
                Some(m) => two_ascii_token!(Token::SetGain(m)),
                None => one_ascii_token!(Token::SetGain(GainMode::Raw)),
            }
        }

        // Possibly multiple character tokens
        b'_' | b'{' | b'}' | b'M' => {
            let c2 = scanner.second_byte();
            match (c1, c2) {
                (b'_', Some(b'_')) => two_ascii_token!(Token::RelativeTranspose),
                (b'{', Some(b'{')) => two_ascii_token!(Token::StartBrokenChord),
                (b'}', Some(b'}')) => two_ascii_token!(Token::EndBrokenChord),
                (b'M', Some(b'P')) => two_ascii_token!(Token::MpVibrato),

                (b'_', _) => one_ascii_token!(Token::Transpose),
                (b'{', _) => one_ascii_token!(Token::StartPortamento),
                (b'}', _) => one_ascii_token!(Token::EndPortamento),

                // This should not happen
                (_, _) => parse_unknown_chars(scanner),
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

    pub fn new_with_line(line: Line<'a>, entire_line_range: LineIndexRange) -> Self {
        let mut s = Self::new();
        s.parse_line(line, entire_line_range);
        s
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn parse_line(&mut self, line: Line<'a>, entire_line_range: LineIndexRange) {
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
            self.tokens.push(t)
        }

        self.end_pos = scanner.pos();
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
                .unwrap_or_else(|| (Self::end_token(tokens.end_pos))),
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
        const SPECIAL_CHARS: [u8; 1] = [b'M'];

        for c in 0..127_u8 {
            let s = [c];
            let text = std::str::from_utf8(&s).unwrap();

            let mut scanner = Scanner::new(text, blank_pos());

            let token = next_token(&mut scanner).map(|t| t.token);
            let is_unknown_token =
                matches!(token, Some(Token::Error(MmlError::UnknownCharacters(_))));

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
