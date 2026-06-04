//! Identifiers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};

use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::{IdentifierError, ValueError};

use std::fmt::Display;
use std::ops::RangeInclusive;
use std::str::FromStr;

pub const FIRST_MUSIC_CHANNEL: char = 'A';
pub const LAST_MUSIC_CHANNEL: char = 'H';
pub(crate) const MUSIC_CHANNEL_RANGE: RangeInclusive<char> = 'A'..='H';

const _: () = assert!(
    *MUSIC_CHANNEL_RANGE.end() as usize - *MUSIC_CHANNEL_RANGE.start() as usize + 1
        == N_MUSIC_CHANNELS
);

const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F", "G", "H"];

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MusicChannelIndex(u8);

impl MusicChannelIndex {
    pub const CHANNEL_A: Self = Self(0);
    pub const CHANNEL_B: Self = Self(1);

    pub(crate) fn try_new(value: usize) -> Result<Self, ()> {
        match value {
            0..N_MUSIC_CHANNELS => Ok(MusicChannelIndex(value.try_into().unwrap())),
            _ => Err(()),
        }
    }

    pub fn identifier(&self) -> IdentifierStr<'static> {
        IdentifierStr::from_str(CHANNEL_NAMES[usize::from(self.0)])
    }

    pub fn identifier_str(&self) -> &'static str {
        CHANNEL_NAMES[usize::from(self.0)]
    }
}

impl From<MusicChannelIndex> for u8 {
    fn from(val: MusicChannelIndex) -> Self {
        val.0
    }
}

impl From<MusicChannelIndex> for usize {
    fn from(val: MusicChannelIndex) -> Self {
        val.0.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelId {
    Channel(MusicChannelIndex),
    Subroutine(u8),
    SoundEffect,
    MmlPrefix,
}

// An identifier is a name or a number
// Using `&str` to avoid a string copy.
// CAUTION: might not be valid.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct IdentifierStr<'a>(&'a str);

impl<'a> IdentifierStr<'a> {
    // Must only be called by the tokenizer.
    // Should not be used to get the actual instrument or subroutine name.
    pub(crate) fn from_str(s: &'a str) -> Self {
        Self(s)
    }

    pub fn try_from_str(s: &'a str) -> Result<Self, IdentifierError> {
        match s.chars().next() {
            Some(c) if c.is_ascii_digit() => Self::try_from_number(s),
            Some(_) => Self::try_from_name(s),
            None => Err(IdentifierError::Empty),
        }
    }

    pub(crate) fn try_from_name(s: &'a str) -> Result<Self, IdentifierError> {
        if Name::is_valid_name(s) {
            Ok(Self(s))
        } else {
            Err(IdentifierError::InvalidName(s.to_owned()))
        }
    }

    pub(super) fn try_from_number(s: &'a str) -> Result<Self, IdentifierError> {
        // Number identifier
        if s.chars().all(|c| c.is_ascii_digit()) {
            Ok(Self(s))
        } else {
            Err(IdentifierError::InvalidNumber(s.to_owned()))
        }
    }

    pub fn to_owned(self) -> IdentifierBuf {
        IdentifierBuf(self.0.to_owned())
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

// An owned identifier stored in a String
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct IdentifierBuf(String);

impl IdentifierBuf {
    pub(crate) fn as_ref(&self) -> IdentifierStr<'_> {
        IdentifierStr(&self.0)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Deserialize, Serialize, Clone, Hash, Eq, PartialEq, Debug)]
#[serde(try_from = "String")]
pub struct Name(String);

pub fn is_name_or_id(s: &str) -> bool {
    s.bytes()
        .all(|b| matches!(b, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_'))
}

impl Name {
    fn is_name_char(c: char) -> bool {
        matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
    }

    pub fn is_valid_name(s: &str) -> bool {
        let mut iter = s.bytes();

        // first character
        if let Some(b) = iter.next() {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {}
                _ => return false,
            }
        }

        for b in iter {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => {}
                _ => return false,
            }
        }

        true
    }

    pub fn try_new(s: String) -> Result<Self, ValueError> {
        if Self::is_valid_name(&s) {
            Ok(Self(s))
        } else {
            Err(ValueError::InvalidName(s))
        }
    }

    pub fn try_new_lossy(s: String) -> Option<Self> {
        if !s.is_empty() {
            Some(Self::new_lossy(s))
        } else {
            None
        }
    }

    pub fn new_lossy(s: String) -> Self {
        if s.is_empty() {
            Self("_".to_owned())
        } else {
            let mut s = s.replace(|c| !Self::is_name_char(c), "_");
            if let Some(c) = s.chars().next() {
                if c.is_ascii_digit() {
                    s.replace_range(0..0, "_");
                }
            }

            Self(s)
        }
    }

    pub fn take_string(self) -> String {
        self.0
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Name {
    type Error = ValueError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::try_new(s)
    }
}

impl FromStr for Name {
    type Err = ValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_new(s.to_owned())
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
