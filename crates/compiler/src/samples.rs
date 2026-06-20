//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::{BrrError, SampleAndInstrumentDataError, SampleError};
use crate::identifier::Name;
use crate::notes::{Note, LAST_NOTE_ID};
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::pitch_table::{
    brr_sample_pitches, maximize_pitch_note_range, merge_pitch_vec, sort_pitches_iterator,
    PitchTable, SamplePitches,
};
use crate::project::{
    self, BrrEncoderSettings, BrrSamplePitches, Instrument, InstrumentNoteRange,
    UniqueNamesProjectFile,
};

use brr::{
    self, parse_brr_file, read_mono_pcm_wave_file, BrrFilter, BrrSample, MonoPcm16WaveFile,
    ValidBrrFile, BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK,
};

use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::ops::{Deref, RangeInclusive};
use std::sync::Arc;

// A blank project has 61211 bytes of free space (2025)
const MAX_BRR_SAMPLE_LOAD: u64 = 60 * 1024;
const MAX_WAV_SAMPLES: usize =
    (MAX_BRR_SAMPLE_LOAD as usize) / BYTES_PER_BRR_BLOCK * SAMPLES_PER_BLOCK;

pub const WAV_EXTENSION: &str = "wav";
pub const BRR_EXTENSION: &str = "brr";

fn read_file_limited(
    source: &SourcePathBuf,
    parent: &ParentPathBuf,
    max_size: u64,
) -> Result<Vec<u8>, BrrError> {
    let file = match fs::File::open(source.to_path(parent)) {
        Ok(file) => file,
        Err(e) => return Err(BrrError::IoError(Arc::from((source.to_path_string(), e)))),
    };

    let mut buffer = Vec::new();

    match file.take(max_size + 1).read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => return Err(BrrError::IoError(Arc::from((source.to_path_string(), e)))),
    }

    if buffer.len() > max_size as usize {
        return Err(BrrError::FileTooLarge(source.to_path_string()));
    }

    Ok(buffer)
}

pub struct SampleFileCache {
    parent_path: ParentPathBuf,
    brr_files: HashMap<SourcePathBuf, Result<ValidBrrFile, BrrError>>,
    wav_files: HashMap<SourcePathBuf, Result<MonoPcm16WaveFile, BrrError>>,
}

impl SampleFileCache {
    pub fn new(parent_path: ParentPathBuf) -> Self {
        Self {
            parent_path,
            brr_files: HashMap::new(),
            wav_files: HashMap::new(),
        }
    }

    pub fn clear_cache(&mut self) {
        self.brr_files.clear();
        self.wav_files.clear();
    }

    pub fn remove_path(&mut self, source: &SourcePathBuf) {
        self.brr_files.remove(source);
        self.wav_files.remove(source);
    }

    pub fn load_brr_file(&mut self, source: &SourcePathBuf) -> &Result<ValidBrrFile, BrrError> {
        self.brr_files.entry(source.to_owned()).or_insert_with(|| {
            if !source.is_empty() {
                match read_file_limited(source, &self.parent_path, MAX_BRR_SAMPLE_LOAD) {
                    Ok(data) => match parse_brr_file(&data) {
                        Ok(b) => Ok(b),
                        Err(e) => Err(BrrError::BrrParseError(source.to_path_string(), e)),
                    },
                    Err(e) => Err(e),
                }
            } else {
                Err(BrrError::NoFilename)
            }
        })
    }

    pub fn load_wav_file(
        &mut self,
        source: &SourcePathBuf,
    ) -> &Result<MonoPcm16WaveFile, BrrError> {
        self.wav_files.entry(source.to_owned()).or_insert_with(|| {
            if !source.is_empty() {
                let p = &source.to_path(&self.parent_path);
                match fs::File::open(p) {
                    Ok(mut file) => match read_mono_pcm_wave_file(&mut file, MAX_WAV_SAMPLES) {
                        Ok(w) => Ok(w),
                        Err(e) => Err(BrrError::WaveFileError(Arc::from((
                            source.to_path_string(),
                            e,
                        )))),
                    },
                    Err(e) => Err(BrrError::IoError(Arc::from((source.to_path_string(), e)))),
                }
            } else {
                Err(BrrError::NoFilename)
            }
        })
    }
}

fn encode_brr_samples(
    samples: &[i16],
    s: &BrrEncoderSettings,
) -> Result<BrrSample, brr::EncodeError> {
    let loop_filter = match s.loop_filter {
        None => None,
        Some(project::BrrLoopFilter::Reset) => Some(BrrFilter::Filter0),
        Some(project::BrrLoopFilter::Auto) => None,
        Some(project::BrrLoopFilter::Filter1) => Some(BrrFilter::Filter1),
        Some(project::BrrLoopFilter::Filter2) => Some(BrrFilter::Filter2),
        Some(project::BrrLoopFilter::Filter3) => Some(BrrFilter::Filter3),
    };

    // ::TODO allow loop offset and dupe_block_hack at the same time::
    let loop_point = match (s.loop_point, s.dupe_block_hack) {
        (Some(project::SampleNumber(0)), Some(_)) => None,
        (Some(sn), _) => Some(sn.into()),
        (None, _) => None,
    };

    brr::encode_brr(
        samples,
        s.evaluator.to_evaluator(),
        loop_point,
        s.dupe_block_hack.map(|o| o.into()),
        loop_filter,
    )
}

fn load_and_encode_wave_file(
    s: &project::WaveSource,
    cache: &mut SampleFileCache,
) -> Result<Arc<BrrSample>, BrrError> {
    let wav = match cache.load_wav_file(&s.source) {
        Ok(w) => w,
        Err(e) => return Err(e.clone()),
    };

    encode_brr_samples(&wav.samples, &s.settings)
        .map(Arc::new)
        .map_err(|e| BrrError::BrrEncodeError(s.source.to_path_string(), e))
}

fn load_brr_file(
    s: &project::BrrSource,
    cache: &mut SampleFileCache,
) -> Result<Arc<BrrSample>, BrrError> {
    let brr = match cache.load_brr_file(&s.source) {
        Ok(b) => b,
        Err(e) => return Err(e.clone()),
    };

    match brr.clone().into_brr_sample(s.loop_point.map(|l| l.into())) {
        Ok(b) => Ok(b.into()),
        Err(e) => Err(BrrError::BrrParseError(s.source.to_path_string(), e)),
    }
}

#[derive(Clone)]
pub struct SampleData {
    brr_sample: Arc<BrrSample>,
    pitch: SamplePitches,
    adsr1: u8,
    adsr2_or_gain: u8,
}

impl SampleData {
    pub fn sample_size(&self) -> usize {
        self.brr_sample.brr_data().len()
    }

    pub fn sample_data(&self) -> &BrrSample {
        &self.brr_sample
    }

    pub fn share_sample_data(&self) -> Arc<BrrSample> {
        Arc::clone(&self.brr_sample)
    }
}

// ::TODO When recompiling a sample, skip compiling brr::BrrSample if sample.source is unchanged::
pub fn compile_brr_sample(
    input: &project::BrrSample,
    cache: &mut SampleFileCache,
) -> Result<SampleData, (Option<Arc<brr::BrrSample>>, SampleError)> {
    let brr_sample = {
        let mut s = match &input.source {
            project::BrrSampleSource::WaveFile(s) => load_and_encode_wave_file(s, cache),
            project::BrrSampleSource::BrrFile(s) => load_brr_file(s, cache),
        };

        if !input.ignore_gaussian_overflow
            && s.as_ref()
                .is_ok_and(|b| b.test_for_gaussian_overflow_glitch_autoloop())
        {
            s = Err(BrrError::GaussianOverflowDetected);
        }
        s
    };

    let pitch = brr_sample_pitches(input);

    let envelope = input.envelope.engine_value();

    match (brr_sample, pitch) {
        (Ok(brr_sample), Ok(pitch)) => Ok(SampleData {
            brr_sample,
            pitch,
            adsr1: envelope.0,
            adsr2_or_gain: envelope.1,
        }),
        (Ok(b), p) => Err((
            Some(b),
            SampleError {
                brr_error: None,
                pitch_error: p.err(),
            },
        )),
        (Err(be), p) => Err((
            None,
            SampleError {
                brr_error: Some(be),
                pitch_error: p.err(),
            },
        )),
    }
}

pub trait CompiledDataList {
    type Item;
    fn expected_len(&self) -> usize;
    fn data_iter(&self) -> impl Iterator<Item = &Self::Item>;
}

impl<T> CompiledDataList for [T] {
    type Item = T;

    fn expected_len(&self) -> usize {
        self.len()
    }

    fn data_iter(&self) -> impl Iterator<Item = &Self::Item> {
        self.iter()
    }
}

fn compile_samples(
    project: &UniqueNamesProjectFile,
) -> Result<Vec<SampleData>, Vec<(usize, Name, SampleError)>> {
    let mut errors = Vec::new();

    let mut cache = SampleFileCache::new(project.parent_path.clone());

    let mut out = Vec::new();

    for (i, s) in project.brr_samples.list().iter().enumerate() {
        match compile_brr_sample(s, &mut cache) {
            Ok(b) => out.push(b),
            Err((_, e)) => errors.push((i, s.name.clone(), e)),
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

#[derive(Clone)]
pub struct BrrDirectoryOffset {
    pub start: u16,
    pub loop_point: u16,
}

struct BrrDirectory {
    brr_data: Vec<u8>,
    brr_directory_offsets: Vec<BrrDirectoryOffset>,
}

// NOTE: Does not check the size of the directory.
fn build_brr_directroy(
    samples: &(impl CompiledDataList<Item = SampleData> + ?Sized),
) -> BrrDirectory {
    let mut brr_data = Vec::new();
    let mut brr_directory_offsets = Vec::new();

    let mut sample_map: HashMap<&BrrSample, BrrDirectoryOffset> = HashMap::new();

    for s in samples.data_iter() {
        let brr = &s.brr_sample;

        match sample_map.get(brr.deref()) {
            None => {
                let start = brr_data.len();
                let loop_point = start + usize::from(brr.loop_offset().unwrap_or(0));

                // Size checking preformed in `build_sample_and_instrument_data()`.
                let dir_item = BrrDirectoryOffset {
                    start: u16::try_from(start & 0xffff).unwrap(),
                    loop_point: u16::try_from(loop_point & 0xffff).unwrap(),
                };

                sample_map.insert(brr, dir_item.clone());

                brr_directory_offsets.push(dir_item);
                brr_data.extend(brr.brr_data());
            }
            Some(o) => {
                brr_directory_offsets.push(o.clone());
            }
        };
    }

    BrrDirectory {
        brr_data,
        brr_directory_offsets,
    }
}

// An instrument is a BRR sample that is loaded into Audio-RAM
// with a pitch-table offset and an envelope.
pub struct SampleAndInstrumentData {
    pub(crate) pitch_table: PitchTable,

    pub(crate) n_instruments: usize,

    // Instruments SoA
    // (pitchOffset is in pitch_table)
    pub(crate) instruments_adsr1: Vec<u8>,
    pub(crate) instruments_adsr2_or_gain: Vec<u8>,

    pub(crate) brr_data: Vec<u8>,
    pub(crate) brr_directory_offsets: Vec<BrrDirectoryOffset>,
}

impl SampleAndInstrumentData {
    pub fn pitch_table(&self) -> &PitchTable {
        &self.pitch_table
    }
}

/// Creates SampleAndInstrumentData without the first/last octave limits.
///
/// Returns: sample data and the largest note that can be played by the sample.
pub fn create_test_instrument_data(sample: &SampleData) -> Option<(SampleAndInstrumentData, Note)> {
    let (pitch, max_note) = maximize_pitch_note_range(&sample.pitch);

    let samples = [SampleData {
        pitch,
        ..sample.clone()
    }];
    let data = combine_samples(samples.as_slice()).ok()?;

    Some((data, max_note))
}

/// Panics if instrument/samples `data_iter().count()` != `expected_len()`.
pub fn combine_samples(
    samples: &(impl CompiledDataList<Item = SampleData> + ?Sized),
) -> Result<SampleAndInstrumentData, SampleAndInstrumentDataError> {
    let total_len = samples.expected_len();

    let mut instruments_adsr1 = Vec::with_capacity(total_len);
    instruments_adsr1.extend(samples.data_iter().map(|s| s.adsr1));

    let mut instruments_adsr2_or_gain = Vec::with_capacity(total_len);
    instruments_adsr2_or_gain.extend(samples.data_iter().map(|s| s.adsr2_or_gain));

    let brr = build_brr_directroy(samples);

    let sorted_pitches = sort_pitches_iterator(samples.data_iter().map(|s| s.pitch.clone()));

    let pitch_table = match merge_pitch_vec(sorted_pitches, total_len) {
        Ok(pt) => pt,
        Err(e) => {
            return Err(SampleAndInstrumentDataError {
                sample_errors: Vec::new(),
                pitch_table_error: Some(e),
            })
        }
    };

    assert_eq!(instruments_adsr1.len(), total_len);
    assert_eq!(instruments_adsr2_or_gain.len(), total_len);
    assert_eq!(pitch_table.instruments_pitch_offset_len(), total_len);

    Ok(SampleAndInstrumentData {
        n_instruments: total_len,

        pitch_table,

        instruments_adsr1,
        instruments_adsr2_or_gain,

        brr_data: brr.brr_data,
        brr_directory_offsets: brr.brr_directory_offsets,
    })
}

pub fn build_sample_and_instrument_data(
    project: &UniqueNamesProjectFile,
) -> Result<SampleAndInstrumentData, SampleAndInstrumentDataError> {
    let mut error = SampleAndInstrumentDataError {
        sample_errors: Vec::new(),
        pitch_table_error: None,
    };

    match compile_samples(project) {
        Ok(s) => combine_samples(s.as_slice()),
        Err(e) => {
            error.sample_errors.extend(e);
            Err(error)
        }
    }
}

pub fn instrument_note_range(inst: &Instrument) -> RangeInclusive<Note> {
    match inst.note_range {
        InstrumentNoteRange::Octave { first, last } => {
            Note::first_note_for_octave(first)..=Note::last_note_for_octave(last)
        }
        InstrumentNoteRange::Note { first, last } => first..=last,
    }
}

pub fn note_range(s: &project::BrrSample) -> RangeInclusive<Note> {
    match &s.pitches {
        Some(BrrSamplePitches::Octaves {
            tuning: _,
            first,
            last,
        }) => Note::first_note_for_octave(*first)..=Note::last_note_for_octave(*last),
        Some(BrrSamplePitches::Notes {
            tuning: _,
            first,
            last,
        }) => *first..=*last,
        Some(BrrSamplePitches::SampleRates { sample_rates }) => {
            let last = sample_rates
                .len()
                .saturating_sub(1)
                .clamp(0, LAST_NOTE_ID.into());
            Note::from_note_id_usize(0).unwrap()..=Note::from_note_id_usize(last).unwrap()
        }
        None => Note::MAX..=Note::MIN,
    }
}
