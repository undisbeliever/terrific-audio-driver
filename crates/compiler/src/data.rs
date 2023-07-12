//! JSON Data

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants;
use crate::driver_constants::{MAX_INSTRUMENTS, MAX_N_SONGS, MAX_SOUND_EFFECTS};
use crate::envelope::{Adsr, Gain};
use crate::errors::{
    DeserializeError, FileError, ProjectFileError, ProjectFileErrors, UniqueNameListError,
    ValueError,
};
use crate::notes::Octave;

use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::Deserialize;

pub const MAX_FILE_SIZE: u32 = 5 * 1024 * 1024;

#[derive(Deserialize, Clone, Hash, Eq, PartialEq, Debug)]
#[serde(try_from = "String")]
pub struct Name(String);

pub fn is_name_or_id(s: &str) -> bool {
    s.bytes()
        .all(|b| matches!(b, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_'))
}

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

    /// Duplicates `N` blocks to the end of the sample in an attempt to improve the sample quality of the first-looping BRR block.
    ///  * Increases the sample size by `N * 9` bytes.
    ///  * Most samples created by this hack will not loop perfectly which adds low-frequency oscillation to the sample.
    ///  * dupe_block_hack may create create a glitched sample, hence the name `dupe_block_hack`.
    pub dupe_block_hack: Option<usize>,

    /// Reset the BRR filter at the loop point.
    ///
    /// If true, the BRR block after the loop point will always use BRR filter 0, which ensures
    /// perfect looping at the cost of reduced quality for the BRR block after the loop point.
    ///
    /// This setting is incompatible with `dupe_block_hack`.
    #[serde(default)]
    pub loop_resets_filter: bool,

    pub first_octave: Octave,
    pub last_octave: Octave,

    pub adsr: Option<Adsr>,
    pub gain: Option<Gain>,

    pub comment: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct Song {
    pub name: Name,
    pub source: PathBuf,
}

#[derive(Deserialize, Debug)]
pub struct Project {
    pub instruments: Vec<Instrument>,

    pub sound_effects: Vec<Name>,
    pub sound_effect_file: Option<PathBuf>,

    #[serde(default)]
    pub songs: Vec<Song>,
}

pub struct ProjectFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: PathBuf,

    pub contents: Project,
}

pub fn load_project_file(path: &Path) -> Result<ProjectFile, DeserializeError> {
    let text_file = match load_text_file_with_limit(path) {
        Ok(tf) => tf,
        Err(e) => return Err(DeserializeError::FileError(e)),
    };
    let file_name = text_file.file_name;

    let parent_path = match path.parent() {
        Some(p) => p.to_owned(),
        None => return Err(DeserializeError::NoParentPath(file_name)),
    };

    let contents = match serde_json::from_str(&text_file.contents) {
        Ok(m) => m,
        Err(e) => return Err(DeserializeError::SerdeError(file_name, e)),
    };

    Ok(ProjectFile {
        path: path.to_path_buf(),
        file_name,
        parent_path,
        contents,
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

impl NameGetter for Song {
    fn name(&self) -> &Name {
        &self.name
    }
}

pub struct UniqueNamesList<T> {
    list: Vec<T>,
    map: HashMap<String, u8>,
}

impl<T> UniqueNamesList<T> {
    pub fn list(&self) -> &[T] {
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
    pub fn get(&self, name: &str) -> Option<&T> {
        self.map.get(name).map(|&i| (&self.list[usize::from(i)]))
    }
    pub fn get_with_index(&self, name: &str) -> Option<(u8, &T)> {
        self.map.get(name).map(|&i| (i, &self.list[usize::from(i)]))
    }
}

pub struct UniqueNamesProjectFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: PathBuf,

    pub instruments: UniqueNamesList<Instrument>,

    pub sound_effects: UniqueNamesList<Name>,
    pub sound_effect_file: Option<PathBuf>,

    pub songs: UniqueNamesList<Song>,
}

impl UniqueNamesProjectFile {
    pub const FIRST_SONG_ID: usize = driver_constants::FIRST_SONG_ID;

    pub fn last_song_id(&self) -> usize {
        self.songs.len() + Self::FIRST_SONG_ID - 1
    }

    pub fn get_song_from_id(&self, id: usize) -> Option<&Song> {
        match id.checked_sub(Self::FIRST_SONG_ID) {
            Some(i) => self.songs.list().get(i),
            None => None,
        }
    }
}

fn validate_list_names<T>(
    list: Vec<T>,
    check_zero: bool,
    max: usize,
    mut add_error: impl FnMut(UniqueNameListError),
) -> UniqueNamesList<T>
where
    T: NameGetter,
{
    assert!(max <= 256);

    if check_zero && list.is_empty() {
        add_error(UniqueNameListError::Empty);
    }
    if list.len() > max {
        add_error(UniqueNameListError::TooManyItems(list.len(), max));
    }

    let mut map = HashMap::with_capacity(list.len());

    for (i, item) in list.iter().enumerate() {
        let name = item.name().as_str();
        let i_u8 = (i & 0xff).try_into().unwrap();

        if map.insert(name.to_owned(), i_u8).is_some() {
            add_error(UniqueNameListError::DuplicateName(i, name.to_owned()));
        }
    }

    UniqueNamesList { list, map }
}

pub fn validate_instrument_names(
    instruments: Vec<Instrument>,
) -> Result<UniqueNamesList<Instrument>, ProjectFileErrors> {
    let mut errors = Vec::new();

    let out = validate_list_names(instruments, true, MAX_INSTRUMENTS, |e| {
        errors.push(ProjectFileError::Instrument(e))
    });

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(ProjectFileErrors(errors))
    }
}

pub fn validate_project_file_names(
    pf: ProjectFile,
) -> Result<UniqueNamesProjectFile, ProjectFileErrors> {
    let mut errors = Vec::new();

    let instruments = match validate_instrument_names(pf.contents.instruments) {
        Ok(instruments) => Some(instruments),
        Err(e) => {
            errors = e.0;
            None
        }
    };

    let sound_effects =
        validate_list_names(pf.contents.sound_effects, false, MAX_SOUND_EFFECTS, |e| {
            errors.push(ProjectFileError::SoundEffect(e))
        });

    let songs = validate_list_names(pf.contents.songs, false, MAX_N_SONGS, |e| {
        errors.push(ProjectFileError::Song(e))
    });

    if errors.is_empty() {
        Ok(UniqueNamesProjectFile {
            path: pf.path,
            file_name: pf.file_name,
            parent_path: pf.parent_path,
            instruments: instruments.unwrap(),
            sound_effects,
            sound_effect_file: pf.contents.sound_effect_file,
            songs,
        })
    } else {
        Err(ProjectFileErrors(errors))
    }
}

// TextFile
// ========

pub struct TextFile {
    pub path: Option<PathBuf>,
    pub file_name: String,
    pub contents: String,
}

pub fn load_text_file_with_limit(path: &Path) -> Result<TextFile, FileError> {
    let file_name = path
        .file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string();

    let file = match File::open(path) {
        Ok(file) => file,
        Err(e) => return Err(FileError::OpenError(file_name, e)),
    };

    // Reading into a buffer as reading exactly `MAX_FILE_SIZE` bytes might output a valid UTF-8 string.
    let mut buffer = Vec::new();

    match file.take(MAX_FILE_SIZE.into()).read_to_end(&mut buffer) {
        Ok(n_bytes_read) => {
            if n_bytes_read >= MAX_FILE_SIZE as usize {
                return Err(FileError::FileTooLarge(file_name));
            }
        }
        Err(e) => return Err(FileError::ReadError(file_name, e)),
    };

    match String::from_utf8(buffer) {
        Ok(contents) => Ok(TextFile {
            path: Some(path.to_owned()),
            file_name,
            contents,
        }),
        Err(_) => Err(FileError::Utf8Error(file_name)),
    }
}
