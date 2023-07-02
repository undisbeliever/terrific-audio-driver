//! JSON Data

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::envelope::{Adsr, Gain};
use crate::errors::{DeserializeError, ParseError};
use crate::notes::Octave;

use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;
use std::str::FromStr;

use serde::Deserialize;

#[derive(Deserialize, Clone, Hash, Eq, PartialEq, Debug)]
#[serde(try_from = "String")]
pub struct Name(String);

// ::TODO confirm serde validates names::

impl Name {
    pub fn is_valid_name(s: &str) -> bool {
        let mut iter = s.bytes();

        // first character
        match iter.next() {
            // Empty name
            None => return false,
            Some(b) => match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {}
                _ => return false,
            },
        };

        for b in iter {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => {}
                _ => return false,
            }
        }

        true
    }

    pub fn try_new(s: String) -> Result<Self, ParseError> {
        if Self::is_valid_name(&s) {
            Ok(Self(s))
        } else {
            Err(ParseError::InvalidName(s))
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Name {
    type Error = ParseError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::try_new(s)
    }
}

impl FromStr for Name {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_new(s.to_owned())
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Deserialize, Debug)]
pub struct Instrument {
    pub name: Name,

    pub source: PathBuf,
    pub freq: f64,
    pub looping: bool,

    pub loop_point: Option<usize>,

    // Duplicates `N` blocks to the end of the sample in an attempt to improve the sample quality of the first-looping BRR block.
    // Increases the sample size by `N * 9` bytes.
    // Most samples created by this hack will not loop perfectly which adds low-frequency oscillation to the sample.
    // (Hence the name `duple_block_hack`.)
    pub dupe_block_hack: Option<usize>,

    pub first_octave: Octave,
    pub last_octave: Octave,

    pub adsr: Option<Adsr>,
    pub gain: Option<Gain>,

    pub comment: Option<String>,
}

// ::TODO test for duplicate names::
#[derive(Deserialize, Debug)]
pub struct Mappings {
    pub instruments: Vec<Instrument>,

    pub sound_effects: Vec<Name>,
}

pub struct MappingsFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: PathBuf,

    pub mappings: Mappings,
}

pub fn load_mappings_file(path: PathBuf) -> Result<MappingsFile, DeserializeError> {
    let file_name = path
        .file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string();

    let parent_path = match path.parent() {
        Some(p) => p.to_owned(),
        None => return Err(DeserializeError::NoParentPath(file_name)),
    };

    let file = match File::open(&path) {
        Ok(file) => file,
        Err(e) => return Err(DeserializeError::OpenError(file_name, e)),
    };
    let reader = BufReader::new(file);

    let mappings = match serde_json::from_reader(reader) {
        Ok(m) => m,
        Err(e) => return Err(DeserializeError::SerdeError(file_name, e)),
    };

    Ok(MappingsFile {
        path,
        file_name,
        parent_path,
        mappings,
    })
}
