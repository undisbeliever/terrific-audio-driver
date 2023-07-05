//! JSON Data

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::{MAX_INSTRUMENTS, MAX_SOUND_EFFECTS};
use crate::envelope::{Adsr, Gain};
use crate::errors::{
    DeserializeError, MappingError, MappingListError, MappingsFileErrors, ValueError,
};
use crate::notes::Octave;

use std::collections::HashMap;
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

    pub fn try_new(s: String) -> Result<Self, ValueError> {
        if Self::is_valid_name(&s) {
            Ok(Self(s))
        } else {
            Err(ValueError::InvalidName(s))
        }
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

trait NameGetter {
    fn name(&self) -> &Name;
}

impl NameGetter for Name {
    fn name(&self) -> &Name {
        self
    }
}

impl NameGetter for Instrument {
    fn name(&self) -> &Name {
        &self.name
    }
}

pub struct UniqueNamesList<T> {
    list: Vec<T>,
    map: HashMap<String, u8>,
}

impl<T> UniqueNamesList<T> {
    pub fn list(&self) -> &Vec<T> {
        &self.list
    }
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }
    pub fn len(&self) -> usize {
        self.list.len()
    }
    pub fn map(&self) -> &HashMap<String, u8> {
        &self.map
    }
    pub fn get(&self, name: &str) -> Option<(u8, &T)> {
        self.map.get(name).map(|&i| (i, &self.list[usize::from(i)]))
    }
}

pub struct UniqueNamesMappingsFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: PathBuf,

    pub instruments: UniqueNamesList<Instrument>,
    pub sound_effects: UniqueNamesList<Name>,
}

fn validate_list_names<T>(
    list: Vec<T>,
    check_zero: bool,
    max: usize,
    mut add_error: impl FnMut(MappingListError),
) -> UniqueNamesList<T>
where
    T: NameGetter,
{
    assert!(max <= 256);

    if check_zero && list.is_empty() {
        add_error(MappingListError::Empty);
    }
    if list.len() > max {
        add_error(MappingListError::TooManyItems(list.len(), max));
    }

    let mut map = HashMap::with_capacity(list.len());

    for (i, item) in list.iter().enumerate() {
        let name = item.name().as_str();
        let i_u8 = (i & 0xff).try_into().unwrap();

        if map.insert(name.to_owned(), i_u8).is_some() {
            add_error(MappingListError::DuplicateName(i, name.to_owned()));
        }
    }

    UniqueNamesList { list, map }
}

pub fn validate_instrument_names(
    instruments: Vec<Instrument>,
) -> Result<UniqueNamesList<Instrument>, MappingsFileErrors> {
    let mut errors = Vec::new();

    let out = validate_list_names(instruments, true, MAX_INSTRUMENTS, |e| {
        errors.push(MappingError::Instrument(e))
    });

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(MappingsFileErrors(errors))
    }
}

pub fn validate_mappings_file_names(
    mappings: MappingsFile,
) -> Result<UniqueNamesMappingsFile, MappingsFileErrors> {
    let mut errors = Vec::new();

    let instruments = match validate_instrument_names(mappings.mappings.instruments) {
        Ok(instruments) => Some(instruments),
        Err(e) => {
            errors = e.0;
            None
        }
    };

    let sound_effects = validate_list_names(
        mappings.mappings.sound_effects,
        false,
        MAX_SOUND_EFFECTS,
        |e| errors.push(MappingError::SoundEffect(e)),
    );

    if errors.is_empty() {
        Ok(UniqueNamesMappingsFile {
            path: mappings.path,
            file_name: mappings.file_name,
            parent_path: mappings.parent_path,
            instruments: instruments.unwrap(),
            sound_effects,
        })
    } else {
        Err(MappingsFileErrors(errors))
    }
}
