//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::{
    BrrEvaluator, Instrument, InstrumentOrSample, LoopSetting, Sample, UniqueNamesProjectFile,
};
use crate::errors::{BrrError, SampleAndInstrumentDataError, SampleError, TaggedSampleError};
use crate::notes::{Note, Octave, LAST_NOTE_ID};
use crate::path::{ParentPathBuf, SourcePathBuf};
use crate::pitch_table::{
    instrument_pitch, maximize_pitch_range, merge_pitch_vec, sample_pitch, sort_pitches_iterator,
    InstrumentPitch, PitchTable, SamplePitches,
};

use brr::{
    encode_brr, parse_brr_file, read_mono_pcm_wave_file, BrrFilter, BrrSample, MonoPcm16WaveFile,
    ValidBrrFile, BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK,
};

use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::ops::RangeInclusive;
use std::sync::Arc;

const MAX_BRR_SAMPLE_LOAD: u64 = 16 * 1024;
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

    fn load_brr_file(&mut self, source: &SourcePathBuf) -> &Result<ValidBrrFile, BrrError> {
        self.brr_files.entry(source.to_owned()).or_insert_with(|| {
            match read_file_limited(source, &self.parent_path, MAX_BRR_SAMPLE_LOAD) {
                Ok(data) => match parse_brr_file(&data) {
                    Ok(b) => Ok(b),
                    Err(e) => Err(BrrError::BrrParseError(source.to_path_string(), e)),
                },
                Err(e) => Err(e),
            }
        })
    }

    pub fn load_wav_file(
        &mut self,
        source: &SourcePathBuf,
    ) -> &Result<MonoPcm16WaveFile, BrrError> {
        self.wav_files.entry(source.to_owned()).or_insert_with(|| {
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
        })
    }
}

fn encode_wave_file(
    source: &SourcePathBuf,
    cache: &mut SampleFileCache,
    loop_setting: &LoopSetting,
    evaluator: BrrEvaluator,
) -> Result<BrrSample, BrrError> {
    let wav = match cache.load_wav_file(source) {
        Ok(w) => w,
        Err(e) => return Err(e.clone()),
    };

    let (loop_point, dupe_block_hack, loop_filter) = match loop_setting {
        LoopSetting::None => (None, None, None),
        LoopSetting::OverrideBrrLoopPoint(_) => {
            return Err(BrrError::InvalidLoopSettingWav(loop_setting.clone()))
        }
        LoopSetting::LoopWithFilter(lp) => (Some(*lp), None, None),
        LoopSetting::LoopResetFilter(lp) => (Some(*lp), None, Some(BrrFilter::Filter0)),
        LoopSetting::LoopFilter1(lp) => (Some(*lp), None, Some(BrrFilter::Filter1)),
        LoopSetting::LoopFilter2(lp) => (Some(*lp), None, Some(BrrFilter::Filter2)),
        LoopSetting::LoopFilter3(lp) => (Some(*lp), None, Some(BrrFilter::Filter3)),
        LoopSetting::DupeBlockHack(dbh) => (None, Some(*dbh), None),
        LoopSetting::DupeBlockHackFilter1(dbh) => (None, Some(*dbh), Some(BrrFilter::Filter1)),
        LoopSetting::DupeBlockHackFilter2(dbh) => (None, Some(*dbh), Some(BrrFilter::Filter2)),
        LoopSetting::DupeBlockHackFilter3(dbh) => (None, Some(*dbh), Some(BrrFilter::Filter3)),
    };

    match encode_brr(
        &wav.samples,
        evaluator.to_evaluator(),
        loop_point,
        dupe_block_hack,
        loop_filter,
    ) {
        Ok(b) => Ok(b),
        Err(e) => Err(BrrError::BrrEncodeError(source.to_path_string(), e)),
    }
}

fn load_brr_file(
    source: &SourcePathBuf,
    cache: &mut SampleFileCache,
    loop_setting: &LoopSetting,
) -> Result<BrrSample, BrrError> {
    let loop_point = match loop_setting {
        LoopSetting::None => None,
        LoopSetting::OverrideBrrLoopPoint(lp) => Some(*lp),

        ls => return Err(BrrError::InvalidLoopSettingBrr(ls.clone())),
    };

    let brr = match cache.load_brr_file(source) {
        Ok(b) => b,
        Err(e) => return Err(e.clone()),
    };

    match brr.clone().into_brr_sample(loop_point) {
        Ok(b) => Ok(b),
        Err(e) => Err(BrrError::BrrParseError(source.to_path_string(), e)),
    }
}

// MUST NOT detect gaussian overflow here - used by tad-gui sample analyser.
pub fn encode_or_load_brr_file(
    source: &SourcePathBuf,
    cache: &mut SampleFileCache,
    loop_setting: &LoopSetting,
    evaluator: BrrEvaluator,
) -> Result<BrrSample, BrrError> {
    match source.extension() {
        Some(WAV_EXTENSION) => encode_wave_file(source, cache, loop_setting, evaluator),
        Some(BRR_EXTENSION) => load_brr_file(source, cache, loop_setting),
        _ => Err(BrrError::UnknownFileType(source.to_path_string())),
    }
}

#[derive(Clone)]
pub struct SampleData<PitchT> {
    brr_sample: BrrSample,
    pitch: PitchT,
    adsr1: u8,
    adsr2_or_gain: u8,
}

impl<PitchT> SampleData<PitchT> {
    pub fn sample_size(&self) -> usize {
        self.brr_sample.brr_data().len()
    }
}

pub type InstrumentSampleData = SampleData<InstrumentPitch>;
pub type SampleSampleData = SampleData<SamplePitches>;

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

pub fn load_sample_for_instrument(
    inst: &Instrument,
    cache: &mut SampleFileCache,
) -> Result<InstrumentSampleData, SampleError> {
    let mut brr_sample =
        encode_or_load_brr_file(&inst.source, cache, &inst.loop_setting, inst.evaluator);

    if !inst.ignore_gaussian_overflow
        && brr_sample
            .as_ref()
            .is_ok_and(BrrSample::test_for_gaussian_overflow_glitch_autoloop)
    {
        brr_sample = Err(BrrError::GaussianOverflowDetected);
    }

    let pitch = instrument_pitch(inst);

    let envelope = inst.envelope.engine_value();

    match (brr_sample, pitch) {
        (Ok(brr_sample), Ok(pitch)) => Ok(SampleData {
            brr_sample,
            pitch,
            adsr1: envelope.0,
            adsr2_or_gain: envelope.1,
        }),
        (b, p) => Err(SampleError {
            brr_error: b.err(),
            pitch_error: p.err(),
        }),
    }
}

pub fn load_sample_for_sample(
    sample: &Sample,
    cache: &mut SampleFileCache,
) -> Result<SampleSampleData, SampleError> {
    let mut brr_sample = encode_or_load_brr_file(
        &sample.source,
        cache,
        &sample.loop_setting,
        sample.evaluator,
    );

    if !sample.ignore_gaussian_overflow
        && brr_sample
            .as_ref()
            .is_ok_and(BrrSample::test_for_gaussian_overflow_glitch_autoloop)
    {
        brr_sample = Err(BrrError::GaussianOverflowDetected);
    }

    let pitch = sample_pitch(sample);

    let envelope = sample.envelope.engine_value();

    match (brr_sample, pitch) {
        (Ok(brr_sample), Ok(pitch)) => Ok(SampleData {
            brr_sample,
            pitch,
            adsr1: envelope.0,
            adsr2_or_gain: envelope.1,
        }),
        (b, p) => Err(SampleError {
            brr_error: b.err(),
            pitch_error: p.err(),
        }),
    }
}

fn compile_samples(
    project: &UniqueNamesProjectFile,
) -> Result<(Vec<InstrumentSampleData>, Vec<SampleSampleData>), Vec<TaggedSampleError>> {
    let mut errors = Vec::new();

    let mut cache = SampleFileCache::new(project.parent_path.clone());

    let mut instruments = Vec::new();
    for (i, inst) in project.instruments.list().iter().enumerate() {
        match load_sample_for_instrument(inst, &mut cache) {
            Ok(b) => instruments.push(b),
            Err(e) => errors.push(TaggedSampleError::Instrument(i, inst.name.clone(), e)),
        }
    }

    let mut samples = Vec::new();
    for (i, sample) in project.samples.list().iter().enumerate() {
        match load_sample_for_sample(sample, &mut cache) {
            Ok(b) => samples.push(b),
            Err(e) => errors.push(TaggedSampleError::Sample(i, sample.name.clone(), e)),
        }
    }

    if errors.is_empty() {
        Ok((instruments, samples))
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

    instruments_scrn: Vec<u8>,
}

// NOTE: Does not check the size of the directory.
fn build_brr_directroy(
    instruments: &(impl CompiledDataList<Item = InstrumentSampleData> + ?Sized),
    samples: &(impl CompiledDataList<Item = SampleSampleData> + ?Sized),
) -> BrrDirectory {
    let mut brr_data = Vec::new();
    let mut brr_directory_offsets = Vec::new();
    let mut instruments_scrn =
        Vec::with_capacity(instruments.expected_len() + samples.expected_len());

    let mut sample_map: HashMap<&BrrSample, u8> = HashMap::new();

    let instruments = instruments.data_iter().map(|s| &s.brr_sample);
    let samples = samples.data_iter().map(|s| &s.brr_sample);

    for brr in instruments.chain(samples) {
        let scrn = match sample_map.get(&brr) {
            Some(o) => *o,

            None => {
                let start = brr_data.len();
                let loop_point = start + usize::from(brr.loop_offset().unwrap_or(0));

                // Size checking preformed in `build_sample_and_instrument_data()`.
                let dir_item = BrrDirectoryOffset {
                    start: u16::try_from(start & 0xffff).unwrap(),
                    loop_point: u16::try_from(loop_point & 0xffff).unwrap(),
                };

                let scrn = u8::try_from(brr_directory_offsets.len() & 0xff).unwrap();

                brr_directory_offsets.push(dir_item);
                brr_data.extend(brr.brr_data());

                sample_map.insert(brr, scrn);

                scrn
            }
        };

        instruments_scrn.push(scrn);
    }

    BrrDirectory {
        brr_data,
        brr_directory_offsets,
        instruments_scrn,
    }
}

pub struct SampleAndInstrumentData {
    pub(crate) pitch_table: PitchTable,

    pub(crate) n_instruments: usize,

    // Instruments SoA
    // (pitchOffset is in pitch_table)
    pub(crate) instruments_scrn: Vec<u8>,
    pub(crate) instruments_adsr1: Vec<u8>,
    pub(crate) instruments_adsr2_or_gain: Vec<u8>,

    pub(crate) brr_data: Vec<u8>,
    pub(crate) brr_directory_offsets: Vec<BrrDirectoryOffset>,
}

impl SampleAndInstrumentData {
    pub fn pitch_table(&self) -> &PitchTable {
        &self.pitch_table
    }

    pub fn take_pitch_table(self) -> PitchTable {
        self.pitch_table
    }
}

/// Creates SampleAndInstrumentData without the first/last octave limits.
///
/// Returns: sample data and the maximum octave that can be played by the sample
pub fn create_test_instrument_data(
    sample: &InstrumentSampleData,
) -> Option<(SampleAndInstrumentData, Octave)> {
    let (pitch, max_octave) = maximize_pitch_range(&sample.pitch);

    let samples = [SampleData {
        pitch,
        ..sample.clone()
    }];
    let data = combine_samples(samples.as_slice(), [].as_slice()).ok()?;

    Some((data, max_octave))
}

/// Panics if instrument/samples `data_iter().count()` != `expected_len()`.
pub fn combine_samples(
    instruments: &(impl CompiledDataList<Item = InstrumentSampleData> + ?Sized),
    samples: &(impl CompiledDataList<Item = SampleSampleData> + ?Sized),
) -> Result<SampleAndInstrumentData, SampleAndInstrumentDataError> {
    let total_len = instruments.expected_len() + samples.expected_len();

    let mut instruments_adsr1 = Vec::with_capacity(total_len);
    instruments_adsr1.extend(instruments.data_iter().map(|s| s.adsr1));
    instruments_adsr1.extend(samples.data_iter().map(|s| s.adsr1));

    let mut instruments_adsr2_or_gain = Vec::with_capacity(total_len);
    instruments_adsr2_or_gain.extend(instruments.data_iter().map(|s| s.adsr2_or_gain));
    instruments_adsr2_or_gain.extend(samples.data_iter().map(|s| s.adsr2_or_gain));

    let brr = build_brr_directroy(instruments, samples);

    let sorted_pitches = sort_pitches_iterator(
        instruments.data_iter().map(|s| s.pitch.clone()),
        samples.data_iter().map(|s| s.pitch.clone()),
    );

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
    assert_eq!(pitch_table.instruments_pitch_offset.len(), total_len);

    Ok(SampleAndInstrumentData {
        n_instruments: total_len,

        pitch_table,

        instruments_scrn: brr.instruments_scrn,
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

    let sample_data = match compile_samples(project) {
        Ok(o) => Some(o),
        Err(e) => {
            error.sample_errors.extend(e);
            None
        }
    };

    if !error.sample_errors.is_empty() {
        return Err(error);
    }
    let (instruments, samples) = sample_data.unwrap();
    combine_samples(instruments.as_slice(), samples.as_slice())
}

pub fn instrument_note_range(inst: &Instrument) -> RangeInclusive<Note> {
    Note::first_note_for_octave(inst.first_octave)..=Note::last_note_for_octave(inst.last_octave)
}

pub fn sample_note_range(sample: &Sample) -> RangeInclusive<Note> {
    let last = sample
        .sample_rates
        .len()
        .saturating_sub(1)
        .clamp(0, LAST_NOTE_ID.into());

    Note::from_note_id_usize(0).unwrap()..=Note::from_note_id_usize(last).unwrap()
}

pub fn note_range(s: &InstrumentOrSample) -> RangeInclusive<Note> {
    match s {
        InstrumentOrSample::Instrument(inst) => instrument_note_range(inst),
        InstrumentOrSample::Sample(sample) => sample_note_range(sample),
    }
}
