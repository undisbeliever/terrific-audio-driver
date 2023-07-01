//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// ::TODO add pub(crate) to more things::

use crate::data::{Instrument, MappingsFile};
use crate::driver_constants::{MAX_DIR_ITEMS, MAX_INSTRUMENTS};
use crate::errors::{OtherSamplesError, SampleError, SamplesErrors};
use crate::pitch_table::{build_pitch_table, PitchTable};

use brr::{encode_brr, parse_brr_file, read_16_bit_mono_wave_file, BrrSample};

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

const MAX_BRR_SAMPLE_LOAD: u64 = 16 * 1024;
const MAX_BRR_SAMPLES: usize = 48 * 1024;

// ::TODO add BRR file cache (for the GUI)::
fn read_file_limited(filename: &Path, max_size: u64) -> Result<Vec<u8>, SampleError> {
    let file = match fs::File::open(filename) {
        Ok(file) => file,
        Err(e) => return Err(SampleError::IoError(filename.to_path_buf(), e)),
    };

    let mut buffer = Vec::new();

    match file.take(max_size + 1).read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => return Err(SampleError::IoError(filename.to_path_buf(), e)),
    }

    if buffer.len() > max_size as usize {
        return Err(SampleError::FileTooLarge(filename.to_path_buf()));
    }

    Ok(buffer)
}

fn encode_wave_file(
    filename: PathBuf,
    is_looping: bool,
    loop_point: Option<usize>,
    dupe_block_hack: Option<usize>,
) -> Result<BrrSample, SampleError> {
    let wav = {
        // ::TODO add wave file cache (for the GUI)::
        let mut wave_file = match fs::File::open(&filename) {
            Ok(file) => file,
            Err(e) => return Err(SampleError::IoError(filename, e)),
        };

        match read_16_bit_mono_wave_file(&mut wave_file, u16::MAX.into()) {
            Ok(wav) => wav,
            Err(e) => return Err(SampleError::WaveFileError(filename, e)),
        }
    };

    let loop_point = if is_looping && dupe_block_hack.is_none() && loop_point.is_none() {
        // No loop point set.
        // Set loop_point to the start of the file.
        Some(0)
    } else {
        loop_point
    };

    match encode_brr(&wav.samples, loop_point, dupe_block_hack) {
        Ok(b) => Ok(b),
        Err(e) => Err(SampleError::BrrEncodeError(filename, e)),
    }
}

fn load_brr_file(filename: PathBuf, loop_point: Option<usize>) -> Result<BrrSample, SampleError> {
    let brr_data = read_file_limited(&filename, MAX_BRR_SAMPLE_LOAD)?;

    let brr = match parse_brr_file(&brr_data, loop_point) {
        Ok(b) => b,
        Err(e) => return Err(SampleError::BrrParseError(filename, e)),
    };

    Ok(brr)
}

fn load_sample_for_instrument(
    parent_path: &Path,
    inst: &Instrument,
) -> Result<BrrSample, SampleError> {
    let filename = parent_path.join(&inst.source);

    let brr_sample = match filename.extension().and_then(OsStr::to_str) {
        Some("wav") => encode_wave_file(
            filename,
            inst.looping,
            inst.loop_point,
            inst.dupe_block_hack,
        )?,
        Some("brr") => {
            if inst.dupe_block_hack.is_some() {
                return Err(SampleError::CannotUseDupeBlockHackOnBrrFiles);
            }
            load_brr_file(filename, inst.loop_point)?
        }
        _ => return Err(SampleError::UnknownFileType(inst.source.clone())),
    };

    if inst.looping != brr_sample.is_looping() {
        return Err(SampleError::LoopingFlagMismatch {
            brr_looping: brr_sample.is_looping(),
        });
    }

    Ok(brr_sample)
}

fn compile_samples(
    mappings_file: &MappingsFile,
) -> Result<Vec<BrrSample>, Vec<(usize, SampleError)>> {
    let mut out = Vec::new();
    let mut errors = Vec::new();

    for (i, inst) in mappings_file.mappings.instruments.iter().enumerate() {
        match load_sample_for_instrument(&mappings_file.parent_path, inst) {
            Ok(b) => out.push(b),
            Err(e) => errors.push((i, e)),
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

struct EnvelopeSoA {
    adsr1: Vec<u8>,
    adsr2_or_gain: Vec<u8>,
}

fn envelope_soa(instruments: &Vec<Instrument>) -> Result<EnvelopeSoA, Vec<(usize, SampleError)>> {
    let mut adsr1 = vec![0; instruments.len()];
    let mut adsr2_or_gain = vec![0; instruments.len()];

    let mut errors = Vec::new();

    for (i, inst) in instruments.iter().enumerate() {
        match (&inst.adsr, &inst.gain) {
            (Some(adsr), None) => {
                adsr1[i] = adsr.adsr1();
                adsr2_or_gain[i] = adsr.adsr2();
            }
            (None, Some(gain)) => {
                // adsr1 is 0 (no adsr)
                adsr2_or_gain[i] = gain.value();
            }
            (Some(_), Some(_)) => errors.push((i, SampleError::GainAndAdsr)),
            (None, None) => errors.push((i, SampleError::NoGainOrAdsr)),
        }
    }

    if errors.is_empty() {
        Ok(EnvelopeSoA {
            adsr1,
            adsr2_or_gain,
        })
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
fn build_brr_directroy(samples: Vec<BrrSample>) -> BrrDirectory {
    let mut brr_data = Vec::new();
    let mut brr_directory_offsets = Vec::new();
    let mut instruments_scrn = Vec::with_capacity(samples.len());

    let mut sample_map: HashMap<BrrSample, u8> = HashMap::new();

    for s in samples.into_iter() {
        let scrn = match sample_map.get(&s) {
            Some(o) => *o,

            None => {
                let start = brr_data.len();
                let loop_point = start + usize::from(s.loop_offset().unwrap_or(0));

                // Size checking preformed in `build_sample_and_instrument_data()`.
                let dir_item = BrrDirectoryOffset {
                    start: u16::try_from(start & 0xffff).unwrap(),
                    loop_point: u16::try_from(loop_point & 0xffff).unwrap(),
                };

                let scrn = u8::try_from(brr_directory_offsets.len() & 0xff).unwrap();

                brr_directory_offsets.push(dir_item);
                brr_data.extend(s.brr_data());

                sample_map.insert(s, scrn);

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
    pub(crate) n_instruments: usize,

    pub(crate) pitch_table: PitchTable,

    // Instruments SoA
    // (pitchOffset is in pitch_table)
    pub(crate) instruments_scrn: Vec<u8>,
    pub(crate) instruments_adsr1: Vec<u8>,
    pub(crate) instruments_adsr2_or_gain: Vec<u8>,

    pub(crate) brr_data: Vec<u8>,
    pub(crate) brr_directory_offsets: Vec<BrrDirectoryOffset>,
}

pub fn build_sample_and_instrument_data(
    mappings: &MappingsFile,
) -> Result<SampleAndInstrumentData, SamplesErrors> {
    let mut other_errors = Vec::new();
    let mut instrument_errors = Vec::new();

    let instruments = &mappings.mappings.instruments;

    if instruments.len() > MAX_INSTRUMENTS {
        other_errors.push(OtherSamplesError::TooManyInstruments(instruments.len()));
    }

    let envelopes = match envelope_soa(instruments) {
        Ok(o) => Some(o),
        Err(e) => {
            instrument_errors.extend(e);
            None
        }
    };
    let pitch_table = match build_pitch_table(instruments) {
        Ok(o) => Some(o),
        Err(e) => {
            other_errors.push(OtherSamplesError::PitchTableError(e));
            None
        }
    };
    let samples = match compile_samples(mappings) {
        Ok(o) => Some(o),
        Err(e) => {
            instrument_errors.extend(e);
            None
        }
    };

    if !instrument_errors.is_empty() {
        return Err(SamplesErrors {
            instrument_errors,
            other_errors,
        });
    }
    let samples = samples.unwrap();

    let brr = build_brr_directroy(samples);

    if brr.brr_directory_offsets.len() > MAX_DIR_ITEMS {
        other_errors.push(OtherSamplesError::TooManyBrrSamples(brr.brr_data.len()));
    }
    if brr.brr_data.len() > MAX_BRR_SAMPLES {
        other_errors.push(OtherSamplesError::BrrDataOverflow(brr.brr_data.len()));
    }

    if instrument_errors.is_empty() && other_errors.is_empty() {
        assert!(pitch_table.is_some());
        let envelopes = envelopes.unwrap();

        Ok(SampleAndInstrumentData {
            n_instruments: instruments.len(),
            pitch_table: pitch_table.unwrap(),

            instruments_scrn: brr.instruments_scrn,
            instruments_adsr1: envelopes.adsr1,
            instruments_adsr2_or_gain: envelopes.adsr2_or_gain,

            brr_data: brr.brr_data,
            brr_directory_offsets: brr.brr_directory_offsets,
        })
    } else {
        Err(SamplesErrors {
            instrument_errors,
            other_errors,
        })
    }
}
