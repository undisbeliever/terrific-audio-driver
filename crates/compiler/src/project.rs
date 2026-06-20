//! Project Data

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants;
use crate::driver_constants::{MAX_BRR_SAMPLES, MAX_N_SONGS, MAX_SOUND_EFFECTS};
use crate::envelope::Envelope;
use crate::errors::{DeserializeError, ProjectFileError, ProjectFileErrors, UniqueNameListError};
use crate::identifier::Name;
use crate::notes::{Note, Octave};
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::pitch_table::SPC_SAMPLE_RATE;
use crate::samples::{BRR_EXTENSION, WAV_EXTENSION};
use crate::textfile::load_text_file_with_limit_path;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize, Serializer};

pub const PROJECT_FILE_EXTENSION: &str = "terrificaudio";

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
#[serde(transparent)]
pub struct SampleNumber(pub usize);

impl From<SampleNumber> for brr::SampleNumber {
    fn from(value: SampleNumber) -> Self {
        brr::SampleNumber(value.0)
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
#[serde(transparent)]
pub struct BlockNumber(pub usize);

impl From<BlockNumber> for brr::BlockNumber {
    fn from(value: BlockNumber) -> Self {
        brr::BlockNumber(value.0)
    }
}

// I am including the filter as part of the enum item name for 3 reasons:
//   1. DupeBlockHack cannot be used with loop_point_filter=BrrFilter::Filter0.
//   2. Simpler JSON format (only 1 fields in `loop_setting`)
//   3. Backwards compatible with the v0.0.3 LoopSetting serde JSON
#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
#[serde(tag = "loop", content = "loop_setting")]
// ::TODO move and/or rename::
// ::TODO remove `pub`::
pub enum LoopSetting {
    /// This setting depends on the source file:
    ///     * wav files - The sample does not loop
    ///     * brr files - The sample loops if the brr file has a 2 byte loop header and the loop flag is set.
    #[serde(rename = "none")]
    None,

    /// The sample is a looping BRR file
    #[serde(rename = "override_brr_loop_point")]
    OverrideBrrLoopPoint(SampleNumber),

    /// Loop point in samples.
    ///
    /// This mode will not reset the BRR filter at the loop point.  It can create better sounding
    /// sample, however most samples will not loop perfectly, which can add low-frequency
    /// oscillation or glitches to the sample.
    #[serde(rename = "loop_with_filter")]
    LoopWithFilter(SampleNumber),

    /// Resets the BRR filter at the loop point.
    ///
    /// The BRR block after the loop point will always use BRR filter 0, which ensures
    /// perfect looping at the cost of reduced quality for the BRR block after the loop point.
    #[serde(rename = "loop_reset_filter")]
    LoopResetFilter(SampleNumber),

    /// Loop the sample and use BRR filter 1 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_1")]
    LoopFilter1(SampleNumber),

    /// Loop the sample and use BRR filter 2 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_2")]
    LoopFilter2(SampleNumber),

    /// Loop the sample and use BRR filter 3 at the loop-point.
    /// Argument is loop point in samples.
    #[serde(rename = "loop_filter_3")]
    LoopFilter3(SampleNumber),

    /// Duplicates `N` blocks to the end of the sample in an attempt to improve the sample quality of the first-looping BRR block.
    ///  * Increases the sample size by `N * 9` bytes.
    ///  * This mode will not reset the filter at the loop point.
    ///  * Most samples created by this hack will not loop perfectly, which adds low-frequency oscillation to the sample.
    ///  * dupe_block_hack may create create a glitched sample, hence the name `dupe_block_hack`.
    #[serde(rename = "dupe_block_hack")]
    DupeBlockHack(BlockNumber),

    // DupeBlockHack that uses loop-point BRR filter 1.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_1")]
    DupeBlockHackFilter1(BlockNumber),

    // DupeBlockHack that uses loop-point BRR filter 2.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_2")]
    DupeBlockHackFilter2(BlockNumber),

    // DupeBlockHack that uses loop-point BRR filter 3.
    // (See `DupeBlockHack`)
    #[serde(rename = "dupe_block_hack_filter_3")]
    DupeBlockHackFilter3(BlockNumber),
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

// ::TODO move and/or rename::
#[derive(Serialize, Clone, PartialEq, Debug)]
#[serde(untagged)]
pub enum InstrumentNoteRange {
    Octave {
        #[serde(rename = "first_octave")]
        first: Octave,
        #[serde(rename = "last_octave")]
        last: Octave,
    },
    Note {
        #[serde(rename = "first_note")]
        first: Note,
        #[serde(rename = "last_note")]
        last: Note,
    },
}

// Custom deserializer for better error handling
impl<'de> Deserialize<'de> for InstrumentNoteRange {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct T {
            first_octave: Option<Octave>,
            last_octave: Option<Octave>,
            first_note: Option<Note>,
            last_note: Option<Note>,
        }

        let tmp = T::deserialize(deserializer)?;

        match (
            tmp.first_octave,
            tmp.last_octave,
            tmp.first_note,
            tmp.last_note,
        ) {
            (Some(first), Some(last), None, None) => Ok(Self::Octave { first, last }),
            (None, None, Some(first), Some(last)) => Ok(Self::Note { first, last }),
            (None, None, None, None) => Err(serde::de::Error::custom(
                "missing `first_octave` & `last_octave` or `first_note` & `last_note` fields",
            )),
            _ => Err(serde::de::Error::custom(
                "invalid instrument note range, cannot combine notes and octaves",
            )),
        }
    }
}

// ::TODO move and/or rename::
// ::TODO remove `pub`::
#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct Instrument {
    pub name: Name,

    pub source: SourcePathBuf,
    pub freq: f64,

    #[serde(flatten)]
    pub loop_setting: LoopSetting,

    #[serde(default)]
    pub evaluator: BrrEvaluator,

    #[serde(default)]
    pub ignore_gaussian_overflow: bool,

    #[serde(flatten)]
    pub note_range: InstrumentNoteRange,

    pub envelope: Envelope,

    pub comment: Option<String>,
}

impl Instrument {
    pub fn wavelength(&self) -> f64 {
        crate::pitch_table::SPC_SAMPLE_RATE as f64 / self.freq
    }

    pub fn set_wavelength(&mut self, w: f64) {
        self.freq = crate::pitch_table::SPC_SAMPLE_RATE as f64 / w;
    }
}

// ::TODO move and/or rename::
// ::TODO remove `pub`::
#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct Sample {
    pub name: Name,

    pub source: SourcePathBuf,

    #[serde(flatten)]
    pub loop_setting: LoopSetting,

    #[serde(default)]
    pub evaluator: BrrEvaluator,

    #[serde(default)]
    pub ignore_gaussian_overflow: bool,

    pub sample_rates: Vec<u32>,

    pub envelope: Envelope,

    pub comment: Option<String>,
}

#[derive(Deserialize, Serialize, Debug, Default, Clone, Copy, PartialEq)]
pub enum BrrEvaluator {
    #[serde(rename = "default")]
    #[default]
    Default,
    #[serde(rename = "square_error")]
    SquaredError,
    #[serde(rename = "se_avoid_gaussian_overflow")]
    SquaredErrorAvoidGaussianOverflow,
}

impl BrrEvaluator {
    pub fn to_evaluator(&self) -> brr::Evaluator {
        match self {
            Self::Default => brr::DEFAULT_EVALUATOR,
            Self::SquaredError => brr::Evaluator::SquaredError,
            Self::SquaredErrorAvoidGaussianOverflow => {
                brr::Evaluator::SquaredErrorAvoidGaussianOverflow
            }
        }
    }

    pub fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }

    // Used to verify I have implemented all `brr::Evaluator` items
    #[allow(dead_code)]
    fn from_evaluator(e: brr::Evaluator) -> Self {
        match e {
            brr::Evaluator::SquaredError => Self::SquaredError,
            brr::Evaluator::SquaredErrorAvoidGaussianOverflow => {
                Self::SquaredErrorAvoidGaussianOverflow
            }
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Copy, PartialEq, Default, Debug)]
pub enum BrrLoopFilter {
    /// Resets the BRR filter at the loop point.
    ///
    /// The BRR block after the loop point will always use BRR filter 0, which ensures
    /// perfect looping at the cost of reduced quality for the BRR block after the loop point.
    #[serde(rename = "reset_filter")]
    #[default]
    Reset,

    /// This mode will not reset the BRR filter at the loop point.  It can create better sounding
    /// sample, however most samples will not loop perfectly, which can add low-frequency
    /// oscillation or glitches to the sample.
    #[serde(rename = "with_filter")]
    Auto,

    /// Use BRR filter 1 at the loop-point.
    #[serde(rename = "filter_1")]
    Filter1,

    /// Use BRR filter 2 at the loop-point.
    #[serde(rename = "filter_2")]
    Filter2,

    /// Use BRR filter 3 at the loop-point.
    #[serde(rename = "filter_3")]
    Filter3,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Default, Debug)]
pub struct BrrEncoderSettings {
    #[serde(default)]
    pub evaluator: BrrEvaluator,

    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub loop_point: Option<SampleNumber>,

    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub loop_filter: Option<BrrLoopFilter>,

    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dupe_block_hack: Option<BlockNumber>,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Default, Debug)]
pub struct WaveSource {
    pub source: SourcePathBuf,

    #[serde(flatten)]
    pub settings: BrrEncoderSettings,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Default, Debug)]
pub struct BrrSource {
    pub source: SourcePathBuf,

    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub loop_point: Option<SampleNumber>,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
#[serde(tag = "type")]
pub enum BrrSampleSource {
    #[serde(rename = "wav")]
    WaveFile(WaveSource),
    #[serde(rename = "brr")]
    BrrFile(BrrSource),
}

impl BrrSampleSource {
    pub fn to_wave_source(&self) -> WaveSource {
        match self {
            BrrSampleSource::WaveFile(s) => s.clone(),
            BrrSampleSource::BrrFile(s) => WaveSource {
                source: s.source.clone(),
                settings: BrrEncoderSettings {
                    evaluator: Default::default(),
                    loop_point: s.loop_point,
                    loop_filter: Default::default(),
                    dupe_block_hack: Default::default(),
                },
            },
        }
    }

    pub fn to_brr_source(&self) -> BrrSource {
        match self {
            BrrSampleSource::BrrFile(s) => s.clone(),
            BrrSampleSource::WaveFile(s) => BrrSource {
                source: s.source.clone(),
                loop_point: s.settings.loop_point,
            },
        }
    }

    pub fn new_from_source(source: SourcePathBuf) -> Self {
        match source.extension() {
            Some(BRR_EXTENSION) => Self::BrrFile(BrrSource {
                source,
                loop_point: None,
            }),
            _ => Self::WaveFile(WaveSource {
                source,
                settings: Default::default(),
            }),
        }
    }
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub enum SampleTuning {
    #[serde(rename = "frequency")]
    Frequency(f64),
    #[serde(rename = "wavelength")]
    Wavelength(f64),
}

impl SampleTuning {
    pub fn frequency(&self) -> f64 {
        match *self {
            SampleTuning::Frequency(f) => f,
            SampleTuning::Wavelength(w) => SPC_SAMPLE_RATE as f64 / w,
        }
    }

    pub fn wavelength(&self) -> f64 {
        match *self {
            SampleTuning::Frequency(f) => SPC_SAMPLE_RATE as f64 / f,
            SampleTuning::Wavelength(w) => w,
        }
    }
}

/// Pitch table inputs
#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
#[serde(tag = "type")]
pub enum BrrSamplePitches {
    #[serde(rename = "octave")]
    Octaves {
        #[serde(flatten)]
        tuning: SampleTuning,

        #[serde(rename = "first_octave")]
        first: Octave,
        #[serde(rename = "last_octave")]
        last: Octave,
    },
    #[serde(rename = "notes")]
    Notes {
        #[serde(flatten)]
        tuning: SampleTuning,

        #[serde(rename = "first_note")]
        first: Note,
        #[serde(rename = "last_note")]
        last: Note,
    },
    #[serde(rename = "samples")]
    SampleRates { sample_rates: Vec<u32> },
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
pub struct BrrSample {
    pub name: Name,

    pub source: BrrSampleSource,

    #[serde(default)]
    pub ignore_gaussian_overflow: bool,

    #[serde(default)]
    pub pitches: Option<BrrSamplePitches>,

    pub envelope: Envelope,

    #[serde(default)]
    #[serde(skip_serializing_if = "String::is_empty")]
    pub comment: String,
}

impl BrrSample {
    pub fn source_path(&self) -> Option<&SourcePathBuf> {
        match &self.source {
            BrrSampleSource::WaveFile(s) => Some(&s.source),
            BrrSampleSource::BrrFile(s) => Some(&s.source),
        }
    }

    pub fn set_source_path(&mut self, path: SourcePathBuf) {
        match path.extension() {
            Some(WAV_EXTENSION) => match &mut self.source {
                BrrSampleSource::WaveFile(s) => s.source = path,
                _ => {
                    self.source = BrrSampleSource::WaveFile(WaveSource {
                        source: path,
                        ..self.source.to_wave_source()
                    });
                }
            },
            Some(BRR_EXTENSION) => match &mut self.source {
                BrrSampleSource::BrrFile(s) => s.source = path,
                _ => {
                    self.source = BrrSampleSource::BrrFile(BrrSource {
                        source: path,
                        ..self.source.to_brr_source()
                    });
                }
            },
            _ => match &mut self.source {
                BrrSampleSource::WaveFile(s) => s.source = path,
                BrrSampleSource::BrrFile(s) => s.source = path,
            },
        }
    }
}

fn convert_old_source(
    source: SourcePathBuf,
    loop_setting: LoopSetting,
    evaluator: BrrEvaluator,
) -> BrrSampleSource {
    match (source.extension(), loop_setting) {
        (Some(BRR_EXTENSION), LoopSetting::None) => BrrSampleSource::BrrFile(BrrSource {
            source,
            loop_point: None,
        }),
        (Some(BRR_EXTENSION), LoopSetting::OverrideBrrLoopPoint(lp)) => {
            BrrSampleSource::BrrFile(BrrSource {
                source,
                loop_point: Some(lp),
            })
        }
        (_, loop_setting) => {
            let (loop_point, loop_filter, dupe_block_hack) = match loop_setting {
                LoopSetting::None => (None, None, None),
                LoopSetting::OverrideBrrLoopPoint(lp) => {
                    (Some(lp), Some(BrrLoopFilter::Reset), None)
                }
                LoopSetting::LoopWithFilter(lp) => (Some(lp), Some(BrrLoopFilter::Auto), None),
                LoopSetting::LoopResetFilter(lp) => (Some(lp), Some(BrrLoopFilter::Reset), None),
                LoopSetting::LoopFilter1(lp) => (Some(lp), Some(BrrLoopFilter::Filter1), None),
                LoopSetting::LoopFilter2(lp) => (Some(lp), Some(BrrLoopFilter::Filter2), None),
                LoopSetting::LoopFilter3(lp) => (Some(lp), Some(BrrLoopFilter::Filter3), None),
                LoopSetting::DupeBlockHack(b) => {
                    (Some(SampleNumber(0)), Some(BrrLoopFilter::Auto), Some(b))
                }
                LoopSetting::DupeBlockHackFilter1(b) => {
                    (Some(SampleNumber(0)), Some(BrrLoopFilter::Filter1), Some(b))
                }
                LoopSetting::DupeBlockHackFilter2(b) => {
                    (Some(SampleNumber(0)), Some(BrrLoopFilter::Filter2), Some(b))
                }
                LoopSetting::DupeBlockHackFilter3(b) => {
                    (Some(SampleNumber(0)), Some(BrrLoopFilter::Filter3), Some(b))
                }
            };

            BrrSampleSource::WaveFile(WaveSource {
                source,
                settings: BrrEncoderSettings {
                    evaluator,
                    loop_point,
                    loop_filter,
                    dupe_block_hack,
                },
            })
        }
    }
}

impl From<Instrument> for BrrSample {
    fn from(value: Instrument) -> Self {
        Self {
            name: value.name,
            source: convert_old_source(value.source, value.loop_setting, value.evaluator),
            ignore_gaussian_overflow: value.ignore_gaussian_overflow,
            pitches: Some(match value.note_range {
                InstrumentNoteRange::Octave { first, last } => BrrSamplePitches::Octaves {
                    tuning: SampleTuning::Frequency(value.freq),
                    first,
                    last,
                },
                InstrumentNoteRange::Note { first, last } => BrrSamplePitches::Notes {
                    tuning: SampleTuning::Frequency(value.freq),
                    first,
                    last,
                },
            }),
            envelope: value.envelope,
            comment: value.comment.unwrap_or_default(),
        }
    }
}

impl From<Sample> for BrrSample {
    fn from(value: Sample) -> Self {
        Self {
            name: value.name,
            source: convert_old_source(value.source, value.loop_setting, value.evaluator),
            ignore_gaussian_overflow: value.ignore_gaussian_overflow,
            pitches: Some(BrrSamplePitches::SampleRates {
                sample_rates: value.sample_rates,
            }),
            envelope: value.envelope,
            comment: value.comment.unwrap_or_default(),
        }
    }
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
#[derive(Debug, Default, Deserialize, PartialEq)]
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

#[derive(Serialize, Default, Debug, PartialEq)]
pub struct Project {
    #[serde(rename = "_about")]
    pub about: About,

    pub brr_samples: Vec<BrrSample>,

    pub default_sfx_flags: DefaultSfxFlags,

    pub high_priority_sound_effects: Vec<Name>,
    pub sound_effects: Vec<Name>,
    pub low_priority_sound_effects: Vec<Name>,

    pub sound_effect_file: Option<SourcePathBuf>,

    pub songs: Vec<Song>,
}

impl<'de> Deserialize<'de> for Project {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // P is used to convert the old instruments and samples formats to BrrSamples

        #[derive(Deserialize)]
        pub struct P {
            #[serde(default, rename = "_about")]
            about: About,
            #[serde(default)]
            instruments: Vec<Instrument>,
            #[serde(default)]
            brr_samples: Vec<BrrSample>,
            #[serde(default)]
            samples: Vec<Sample>,
            #[serde(default)]
            default_sfx_flags: DefaultSfxFlags,
            #[serde(default)]
            high_priority_sound_effects: Vec<Name>,
            #[serde(default)]
            sound_effects: Vec<Name>,
            #[serde(default)]
            low_priority_sound_effects: Vec<Name>,
            #[serde(default)]
            sound_effect_file: Option<SourcePathBuf>,
            #[serde(default)]
            songs: Vec<Song>,
        }

        let p = <P>::deserialize(deserializer)?;

        let mut brr_samples = p.brr_samples;
        brr_samples.extend(p.instruments.into_iter().map(|i| i.into()));
        brr_samples.extend(p.samples.into_iter().map(|s| s.into()));

        Ok(Project {
            about: p.about,
            brr_samples,
            default_sfx_flags: p.default_sfx_flags,
            high_priority_sound_effects: p.high_priority_sound_effects,
            sound_effects: p.sound_effects,
            low_priority_sound_effects: p.low_priority_sound_effects,
            sound_effect_file: p.sound_effect_file,
            songs: p.songs,
        })
    }
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

impl NameGetter for BrrSample {
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
    pub fn get_name_index(&self, name: &str) -> Option<u32> {
        self.map.get(name).copied()
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

pub struct UniqueSoundEffectExportOrder {
    pub export_order: UniqueNamesList<Name>,
    pub n_high_priority_sfx: usize,
    pub low_priority_index: usize,
}

pub struct UniqueNamesProjectFile {
    pub path: PathBuf,
    pub file_name: String,
    pub parent_path: ParentPathBuf,

    pub brr_samples: UniqueNamesList<BrrSample>,

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

pub fn validate_brr_sample_names(
    brr_samples: Vec<BrrSample>,
) -> Result<UniqueNamesList<BrrSample>, ProjectFileErrors> {
    let mut errors = Vec::new();

    let out = validate_list_names(brr_samples, true, MAX_BRR_SAMPLES, |e| {
        errors.push(ProjectFileError::BrrSample(e))
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

    let brr_samples = validate_list_names(pf.contents.brr_samples, false, MAX_BRR_SAMPLES, |e| {
        errors.push(ProjectFileError::BrrSample(e))
    });

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

    if errors.is_empty() {
        Ok(UniqueNamesProjectFile {
            path: pf.path,
            file_name: pf.file_name,
            parent_path: pf.parent_path,
            brr_samples,
            sfx_export_order: sfx_export_order.unwrap(),
            default_sfx_flags: pf.contents.default_sfx_flags,
            sound_effect_file: pf.contents.sound_effect_file,
            songs,
        })
    } else {
        Err(ProjectFileErrors(errors))
    }
}
