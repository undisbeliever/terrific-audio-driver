//! Project Data

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants;
use crate::driver_constants::{MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_SONGS, MAX_SOUND_EFFECTS};
use crate::envelope::Envelope;
use crate::errors::{
    DeserializeError, FileError, ProjectFileError, ProjectFileErrors, UniqueNameListError,
    ValueError,
};
use crate::notes::Octave;
use crate::path::{ParentPathBuf, SourcePathBuf};

use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize, Serializer};

pub const MAX_FILE_SIZE: u32 = 5 * 1024 * 1024;

pub const PROJECT_FILE_EXTENSION: &str = "terrificaudio";

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

// I am including the filter as part of the enum item name for 3 reasons:
//   1. DupeBlockHack cannot be used with loop_point_filter=BrrFilter::Filter0.
//   2. Simpler JSON format (only 1 fields in `loop_setting`)
//   3. Backwards compatible with the v0.0.3 LoopSetting serde JSON
#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
#[serde(tag = "loop", content = "loop_setting")]
pub enum LoopSetting {
    /// This setting depends on the source file:
    ///     * wav files - The sample does not loop
    ///     * brr files - The sample loops if the brr file has a 2 byte loop header and the loop flag is set.
    #[serde(rename = "none")]
    None,

    /// The sample is a looping BRR file
    #[serde(rename = "override_brr_loop_point")]
    OverrideBrrLoopPoint(usize),

    /// Loop point in samples.
    ///
    /// This mode will not reset the BRR filter at the loop point.  It can create better sounding
    /// sample, however most samples will not loop perfectly, which can add low-frequency
    /// oscillation or glitches to the sample.
    #[serde(rename = "loop_with_filter")]
    LoopWithFilter(usize),

    /// Resets the BRR filter at the loop point.
    ///
    /// The BRR block after the loop point will always use BRR filter 0, which ensures
    /// perfect looping at the cost of reduced quality for the BRR block after the loop point.
    #[serde(rename = "loop_reset_filter")]
    LoopResetFilter(usize),

    /// Loop the sample and use BRR filter 1 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_1")]
    LoopFilter1(usize),

    /// Loop the sample and use BRR filter 2 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_2")]
    LoopFilter2(usize),

    /// Loop the sample and use BRR filter 3 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_3")]
    LoopFilter3(usize),

    /// Duplicates `N` blocks to the end of the sample in an attempt to improve the sample quality of the first-looping BRR block.
    ///  * Increases the sample size by `N * 9` bytes.
    ///  * This mode will not reset the filter at the loop point.
    ///  * Most samples created by this hack will not loop perfectly, which adds low-frequency oscillation to the sample.
    ///  * dupe_block_hack may create create a glitched sample, hence the name `dupe_block_hack`.
    #[serde(rename = "dupe_block_hack")]
    DupeBlockHack(usize),

    // DupeBlockHack that uses loop-point BRR filter 1.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_1")]
    DupeBlockHackFilter1(usize),

    // DupeBlockHack that uses loop-point BRR filter 2.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_2")]
    DupeBlockHackFilter2(usize),

    // DupeBlockHack that uses loop-point BRR filter 3.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_3")]
    DupeBlockHackFilter3(usize),
}

impl LoopSetting {
    pub fn serialier_value(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::OverrideBrrLoopPoint(_) => "override_brr_loop_point",
            Self::LoopWithFilter(_) => "loop_with_filter",
            Self::LoopResetFilter(_) => "loop_reset_filter",
            Self::LoopFilter1(_) => "loop_filter_1",
            Self::LoopFilter2(_) => "loop_filter_2",
            Self::LoopFilter3(_) => "loop_filter_3",
            Self::DupeBlockHack(_) => "dupe_block_hack",
            Self::DupeBlockHackFilter1(_) => "dupe_block_hack_filter_1)",
            Self::DupeBlockHackFilter2(_) => "dupe_block_hack_filter_2)",
            Self::DupeBlockHackFilter3(_) => "dupe_block_hack_filter_3)",
        }
    }
}

impl LoopSetting {
    /// Returns true if the argument is loop point in samples
    pub fn samples_argument(&self) -> bool {
        match self {
            Self::OverrideBrrLoopPoint(_)
            | Self::LoopWithFilter(_)
            | Self::LoopResetFilter(_)
            | Self::LoopFilter1(_)
            | Self::LoopFilter2(_)
            | Self::LoopFilter3(_) => true,

            Self::None
            | Self::DupeBlockHack(_)
            | Self::DupeBlockHackFilter1(_)
            | Self::DupeBlockHackFilter2(_)
            | Self::DupeBlockHackFilter3(_) => false,
        }
    }

    /// Returns true if LoopSetting is dupe block hack and the argument is number of blocks.
    pub fn is_dupe_block_hack(&self) -> bool {
        match self {
            Self::DupeBlockHack(_)
            | Self::DupeBlockHackFilter1(_)
            | Self::DupeBlockHackFilter2(_)
            | Self::DupeBlockHackFilter3(_) => true,

            Self::None
            | Self::OverrideBrrLoopPoint(_)
            | Self::LoopWithFilter(_)
            | Self::LoopResetFilter(_)
            | Self::LoopFilter1(_)
            | Self::LoopFilter2(_)
            | Self::LoopFilter3(_) => false,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct Instrument {
    pub name: Name,

    pub source: SourcePathBuf,
    pub freq: f64,

    #[serde(flatten)]
    pub loop_setting: LoopSetting,

    pub first_octave: Octave,
    pub last_octave: Octave,

    pub envelope: Envelope,

    pub comment: Option<String>,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct Sample {
    pub name: Name,

    pub source: SourcePathBuf,

    #[serde(flatten)]
    pub loop_setting: LoopSetting,

    pub sample_rates: Vec<u32>,

    pub envelope: Envelope,

    pub comment: Option<String>,
}

#[derive(Deserialize, Serialize, Clone, Copy, PartialEq, Debug)]
pub struct DefaultSfxFlags {
    pub one_channel: bool,
    pub interruptible: bool,
}

impl Default for DefaultSfxFlags {
    fn default() -> Self {
        Self {
            one_channel: true,
            interruptible: true,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct Song {
    pub name: Name,
    pub source: SourcePathBuf,
}

/// A small struct that documents (in the project file) what the project file is and the tad-gui
/// version used to create the project file.
#[derive(Debug, Default, Deserialize)]
pub struct About {
    pub version: String,
}

impl Serialize for About {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("About", 2)?;
        s.serialize_field("file_type", "Terrific Audio Driver project file")?;
        s.serialize_field("version", &self.version)?;
        s.end()
    }
}

#[derive(Deserialize, Serialize, Default, Debug)]
pub struct Project {
    #[serde(default, rename = "_about")]
    pub about: About,

    pub instruments: Vec<Instrument>,

    #[serde(default)]
    pub samples: Vec<Sample>,

    #[serde(default)]
    pub default_sfx_flags: DefaultSfxFlags,

    #[serde(default)]
    pub high_priority_sound_effects: Vec<Name>,
    #[serde(default)]
    pub sound_effects: Vec<Name>,
    #[serde(default)]
    pub low_priority_sound_effects: Vec<Name>,

    pub sound_effect_file: Option<SourcePathBuf>,

    #[serde(default)]
    pub songs: Vec<Song>,
}

pub struct ProjectFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: ParentPathBuf,

    pub contents: Project,
}

pub fn load_project_file(path: &Path) -> Result<ProjectFile, DeserializeError> {
    let text_file = match load_text_file_with_limit_path(path) {
        Ok(tf) => tf,
        Err(e) => return Err(DeserializeError::FileError(e)),
    };
    let file_name = text_file.file_name;

    let parent_path = match path.parent() {
        Some(p) => ParentPathBuf::new(p.to_owned()),
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

pub fn serialize_project(project: &Project) -> Result<Vec<u8>, serde_json::error::Error> {
    serde_json::to_vec_pretty(project)
}

pub(crate) trait NameGetter {
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

impl NameGetter for Sample {
    fn name(&self) -> &Name {
        &self.name
    }
}

impl NameGetter for InstrumentOrSample {
    fn name(&self) -> &Name {
        match self {
            Self::Instrument(i) => &i.name,
            Self::Sample(s) => &s.name,
        }
    }
}

impl NameGetter for Song {
    fn name(&self) -> &Name {
        &self.name
    }
}

pub struct UniqueNamesList<T> {
    list: Vec<T>,
    map: HashMap<String, u32>,
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
    pub fn map(&self) -> &HashMap<String, u32> {
        &self.map
    }
    pub fn get(&self, name: &str) -> Option<&T> {
        self.map
            .get(name)
            .and_then(|&i| usize::try_from(i).ok())
            .map(|i| &self.list[i])
    }
    pub fn get_with_index(&self, name: &str) -> Option<(u32, &T)> {
        match self.map.get(name) {
            None => None,
            Some(&i) => match usize::try_from(i) {
                Ok(index) => Some((i, &self.list[index])),
                Err(_) => None,
            },
        }
    }
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.list.get(index)
    }
}

pub enum InstrumentOrSample {
    Instrument(Instrument),
    Sample(Sample),
}

impl InstrumentOrSample {
    pub fn envelope(&self) -> Envelope {
        match self {
            Self::Instrument(i) => i.envelope,
            Self::Sample(s) => s.envelope,
        }
    }
}

pub struct UniqueSoundEffectExportOrder {
    pub export_order: UniqueNamesList<Name>,
    pub n_high_priority_sfx: usize,
    pub low_priority_index: usize,
}

pub struct UniqueNamesProjectFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: ParentPathBuf,

    pub instruments: UniqueNamesList<Instrument>,
    pub samples: UniqueNamesList<Sample>,

    pub instruments_and_samples: UniqueNamesList<InstrumentOrSample>,

    pub default_sfx_flags: DefaultSfxFlags,
    pub sfx_export_order: UniqueSoundEffectExportOrder,
    pub sound_effect_file: Option<SourcePathBuf>,

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

pub(crate) fn single_item_unique_names_list<T>(item: T) -> UniqueNamesList<T>
where
    T: NameGetter,
{
    let mut map = HashMap::with_capacity(1);
    map.insert(item.name().as_str().to_owned(), 0);

    let list = vec![item];

    UniqueNamesList { list, map }
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

pub fn validate_instrument_and_sample_names<'a>(
    instruments: impl Iterator<Item = &'a Instrument>,
    samples: impl Iterator<Item = &'a Sample>,
) -> Result<UniqueNamesList<InstrumentOrSample>, ProjectFileErrors> {
    let mut errors = Vec::new();

    let instruments = instruments.cloned().map(InstrumentOrSample::Instrument);
    let samples = samples.cloned().map(InstrumentOrSample::Sample);

    let list = instruments.chain(samples).collect();

    let out = validate_list_names(list, true, MAX_INSTRUMENTS_AND_SAMPLES, |e| {
        errors.push(ProjectFileError::InstrumentOrSample(e))
    });

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(ProjectFileErrors(errors))
    }
}

pub fn validate_sfx_export_order(
    high_priority_sound_effects: Vec<Name>,
    sound_effects: Vec<Name>,
    low_priority_sound_effects: Vec<Name>,
) -> Result<UniqueSoundEffectExportOrder, ProjectFileErrors> {
    let mut errors = Vec::new();

    let n_high_priority_sfx = high_priority_sound_effects.len();
    let low_priority_index = high_priority_sound_effects.len() + sound_effects.len();
    let export_order = [
        high_priority_sound_effects,
        sound_effects,
        low_priority_sound_effects,
    ]
    .concat();

    let export_order = validate_list_names(export_order, false, MAX_SOUND_EFFECTS, |e| {
        errors.push(ProjectFileError::SoundEffect(e))
    });

    if errors.is_empty() {
        Ok(UniqueSoundEffectExportOrder {
            export_order,
            n_high_priority_sfx,
            low_priority_index,
        })
    } else {
        Err(ProjectFileErrors(errors))
    }
}

pub fn validate_project_file_names(
    pf: ProjectFile,
) -> Result<UniqueNamesProjectFile, ProjectFileErrors> {
    let mut errors = Vec::new();

    let instruments = validate_list_names(
        pf.contents.instruments,
        false,
        MAX_INSTRUMENTS_AND_SAMPLES,
        |e| errors.push(ProjectFileError::Instrument(e)),
    );

    let samples = validate_list_names(
        pf.contents.samples,
        false,
        MAX_INSTRUMENTS_AND_SAMPLES,
        |e| errors.push(ProjectFileError::Sample(e)),
    );

    let sfx_export_order = match validate_sfx_export_order(
        pf.contents.high_priority_sound_effects,
        pf.contents.sound_effects,
        pf.contents.low_priority_sound_effects,
    ) {
        Ok(sfx) => Some(sfx),
        Err(e) => {
            errors.extend(e.0);
            None
        }
    };

    let songs = validate_list_names(pf.contents.songs, false, MAX_N_SONGS, |e| {
        errors.push(ProjectFileError::Song(e))
    });

    let instruments_and_samples = if errors.is_empty() {
        match validate_instrument_and_sample_names(instruments.list().iter(), samples.list().iter())
        {
            Ok(instruments) => Some(instruments),
            Err(e) => {
                errors.extend(e.0);
                None
            }
        }
    } else {
        None
    };

    if errors.is_empty() {
        Ok(UniqueNamesProjectFile {
            path: pf.path,
            file_name: pf.file_name,
            parent_path: pf.parent_path,
            instruments,
            samples,
            instruments_and_samples: instruments_and_samples.unwrap(),
            sfx_export_order: sfx_export_order.unwrap(),
            default_sfx_flags: pf.contents.default_sfx_flags,
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

pub fn load_text_file_with_limit(
    source: &SourcePathBuf,
    parent_path: &ParentPathBuf,
) -> Result<TextFile, FileError> {
    _load_text_file_with_limit(&source.to_path(parent_path), source.file_name().to_owned())
}

pub fn load_text_file_with_limit_path(path: &Path) -> Result<TextFile, FileError> {
    let file_name = path
        .file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string();

    _load_text_file_with_limit(path, file_name)
}

pub fn _load_text_file_with_limit(path: &Path, file_name: String) -> Result<TextFile, FileError> {
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

    // Forbid ASCII control characters that are not whitespace
    if buffer
        .iter()
        .any(|&c| c.is_ascii_control() && !c.is_ascii_whitespace())
    {
        return Err(FileError::InvalidAsciiControlCharacter(file_name));
    }

    match String::from_utf8(buffer) {
        Ok(contents) => {
            // Normalize line endings
            // Fixes compiler errors
            // Prevents FLTK TextEditor glitches in Linux
            let contents = contents.replace("\r\n", "\n");

            Ok(TextFile {
                path: Some(path.to_owned()),
                file_name,
                contents,
            })
        }
        Err(_) => Err(FileError::Utf8Error(file_name)),
    }
}
