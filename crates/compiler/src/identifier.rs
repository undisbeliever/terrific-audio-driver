//! Identifiers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::Name;
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::errors::IdentifierError;

use std::ops::RangeInclusive;

pub const FIRST_MUSIC_CHANNEL: char = 'A';
pub const LAST_MUSIC_CHANNEL: char = 'H';
pub(crate) const MUSIC_CHANNEL_RANGE: RangeInclusive<char> = 'A'..='H';

const _: () = assert!(
    *MUSIC_CHANNEL_RANGE.end() as usize - *MUSIC_CHANNEL_RANGE.start() as usize + 1
        == N_MUSIC_CHANNELS
);

pub const CHANNEL_NAMES: [&str; N_MUSIC_CHANNELS] = ["A", "B", "C", "D", "E", "F", "G", "H"];

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelId {
    Channel(char),
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
