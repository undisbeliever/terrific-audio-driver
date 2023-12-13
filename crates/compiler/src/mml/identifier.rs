//! MML identifier

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::Name;
use crate::errors::IdentifierError;

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

// An identifier str.
// Using `&str` to avoid a string copy.
// The contents of this variable might not be a valid Identifier
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct IdentifierStr<'a>(&'a str);

impl IdentifierStr<'_> {
    pub fn from_str(s: &str) -> IdentifierStr {
        IdentifierStr(s)
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}
