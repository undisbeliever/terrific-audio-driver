//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// ::TODO add pub(crate) to more things::

use crate::data::{Instrument, UniqueNamesProjectFile};
use crate::errors::{
    BrrError, EnvelopeError, SampleAndInstrumentDataError, SampleError, TaggedSampleError,
};
use crate::pitch_table::{
    instrument_pitch, merge_pitch_vec, sort_pitches, InstrumentPitch, PitchTable,
};

use brr::{encode_brr, parse_brr_file, read_16_bit_mono_wave_file, BrrSample};

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

const MAX_BRR_SAMPLE_LOAD: u64 = 16 * 1024;

// ::TODO add BRR file cache (for the GUI)::
fn read_file_limited(filename: &Path, max_size: u64) -> Result<Vec<u8>, BrrError> {
    let file = match fs::File::open(filename) {
        Ok(file) => file,
        Err(e) => return Err(BrrError::IoError(filename.to_path_buf(), e)),
    };

    let mut buffer = Vec::new();

    match file.take(max_size + 1).read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => return Err(BrrError::IoError(filename.to_path_buf(), e)),
    }

    if buffer.len() > max_size as usize {
        return Err(BrrError::FileTooLarge(filename.to_path_buf()));
    }

    Ok(buffer)
}

fn encode_wave_file(
    filename: PathBuf,
    is_looping: bool,
    loop_point: Option<usize>,
    dupe_block_hack: Option<usize>,
    reset_filter_at_loop_point: bool,
) -> Result<BrrSample, BrrError> {
    let wav = {
        // ::TODO add wave file cache (for the GUI)::
        let mut wave_file = match fs::File::open(&filename) {
            Ok(file) => file,
            Err(e) => return Err(BrrError::IoError(filename, e)),
        };

        match read_16_bit_mono_wave_file(&mut wave_file, u16::MAX.into()) {
            Ok(wav) => wav,
            Err(e) => return Err(BrrError::WaveFileError(filename, e)),
        }
    };

    let loop_point = if is_looping && dupe_block_hack.is_none() && loop_point.is_none() {
        // No loop point set.
        // Set loop_point to the start of the file.
        Some(0)
    } else {
        loop_point
    };

    match encode_brr(
        &wav.samples,
        loop_point,
        dupe_block_hack,
        reset_filter_at_loop_point,
    ) {
        Ok(b) => Ok(b),
        Err(e) => Err(BrrError::BrrEncodeError(filename, e)),
    }
}

fn load_brr_file(filename: PathBuf, loop_point: Option<usize>) -> Result<BrrSample, BrrError> {
    let brr_data = read_file_limited(&filename, MAX_BRR_SAMPLE_LOAD)?;

    parse_brr_file(&brr_data)
        .and_then(|b| b.into_brr_sample(loop_point))
        .map_err(|e| BrrError::BrrParseError(filename, e))
}

#[derive(Clone)]
pub struct Sample {
    brr_sample: BrrSample,
    pitch: InstrumentPitch,
    adsr1: u8,
    adsr2_or_gain: u8,
}

fn instrument_envelope(inst: &Instrument) -> Result<(u8, u8), EnvelopeError> {
    match (&inst.adsr, &inst.gain) {
        (Some(adsr), None) => Ok((adsr.adsr1(), adsr.adsr2())),
        (None, Some(gain)) => {
            // adsr1 is 0 (no adsr)
            Ok((0, gain.value()))
        }
        (Some(_), Some(_)) => Err(EnvelopeError::GainAndAdsr),
        (None, None) => Err(EnvelopeError::NoGainOrAdsr),
    }
}

pub fn load_sample_for_instrument(
    parent_path: &Path,
    index: usize,
    inst: &Instrument,
) -> Result<Sample, SampleError> {
    let filename = parent_path.join(&inst.source);

    let mut brr_sample = match filename.extension().and_then(OsStr::to_str) {
        Some("wav") => encode_wave_file(
            filename,
            inst.looping,
            inst.loop_point,
            inst.dupe_block_hack,
            inst.loop_resets_filter,
        ),
        Some("brr") => {
            if inst.dupe_block_hack.is_none() {
                load_brr_file(filename, inst.loop_point)
            } else {
                Err(BrrError::CannotUseDupeBlockHackOnBrrFiles)
            }
        }
        _ => Err(BrrError::UnknownFileType(inst.source.clone())),
    };

    if let Ok(b) = &brr_sample {
        if inst.looping != b.is_looping() {
            brr_sample = Err(BrrError::LoopingFlagMismatch {
                brr_looping: b.is_looping(),
            });
        }
    }

    let pitch = instrument_pitch(index, inst);

    let envelope = instrument_envelope(inst);

    match (brr_sample, pitch, envelope) {
        (Ok(brr_sample), Ok(pitch), Ok(envelope)) => Ok(Sample {
            brr_sample,
            pitch,
            adsr1: envelope.0,
            adsr2_or_gain: envelope.1,
        }),
        (b, p, e) => Err(SampleError {
            brr_error: b.err(),
            pitch_error: p.err(),
            envelope_error: e.err(),
        }),
    }
}

fn compile_samples(
    project: &UniqueNamesProjectFile,
) -> Result<Vec<Sample>, Vec<TaggedSampleError>> {
    let mut out = Vec::new();
    let mut errors = Vec::new();

    for (i, inst) in project.instruments.list().iter().enumerate() {
        match load_sample_for_instrument(&project.parent_path, i, inst) {
            Ok(b) => out.push(b),
            Err(e) => errors.push(TaggedSampleError::Instrument(i, inst.name.clone(), e)),
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

    instruments_scrn: Vec<u8>,
}

// NOTE: Does not check the size of the directory.
fn build_brr_directroy(samples: &[Sample]) -> BrrDirectory {
    let mut brr_data = Vec::new();
    let mut brr_directory_offsets = Vec::new();
    let mut instruments_scrn = Vec::with_capacity(samples.len());

    let mut sample_map: HashMap<&BrrSample, u8> = HashMap::new();

    for s in samples.iter() {
        let scrn = match sample_map.get(&s.brr_sample) {
            Some(o) => *o,

            None => {
                let brr = &s.brr_sample;
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
}

pub fn combine_samples(
    samples: &[Sample],
) -> Result<SampleAndInstrumentData, SampleAndInstrumentDataError> {
    let n_samples = samples.len();

    let instruments_adsr1 = samples.iter().map(|s| s.adsr1).collect();
    let instruments_adsr2_or_gain = samples.iter().map(|s| s.adsr2_or_gain).collect();

    let brr = build_brr_directroy(samples);

    let pitches = sort_pitches(samples.iter().map(|s| s.pitch.clone()).collect());
    let pitch_table = match merge_pitch_vec(pitches, n_samples) {
        Ok(pt) => pt,
        Err(e) => {
            return Err(SampleAndInstrumentDataError {
                sample_errors: Vec::new(),
                pitch_table_error: Some(e),
            })
        }
    };

    Ok(SampleAndInstrumentData {
        n_instruments: n_samples,

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

    let samples = match compile_samples(project) {
        Ok(o) => Some(o),
        Err(e) => {
            error.sample_errors.extend(e);
            None
        }
    };

    if !error.sample_errors.is_empty() {
        return Err(error);
    }
    combine_samples(&samples.unwrap())
}
