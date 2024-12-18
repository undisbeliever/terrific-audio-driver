//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::opcodes;
use crate::bytecode::InstrumentId;
use crate::data::{Instrument, InstrumentOrSample, Sample, UniqueNamesList};
use crate::driver_constants::{MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_PITCHES};
use crate::errors::{PitchError, PitchTableError};
use crate::notes::{self, Note, Octave};

const SEMITONES_PER_OCTAVE: i32 = notes::SEMITONES_PER_OCTAVE as i32;
const PITCH_TABLE_OFFSET: u8 = opcodes::FIRST_PLAY_NOTE_INSTRUCTION / 2;

// Using micro-semitones to remove floating point equality comparisons.
const SEMITONE_SCALE: i32 = 1_000_000;
const MICROSEMITONES_PER_OCTAVE: i32 = SEMITONE_SCALE * SEMITONES_PER_OCTAVE;

const A4_C0_MICROSEMITONE_OFFSET: i32 = 57 * SEMITONE_SCALE;
const A4_FREQ: u32 = 440;

const F64_A4_FREQ: f64 = A4_FREQ as f64;
const F64_MST_PER_OCTAVE: f64 = MICROSEMITONES_PER_OCTAVE as f64;
const F64_A4_C0_MST_OFFSET: f64 = A4_C0_MICROSEMITONE_OFFSET as f64;

const SPC_SAMPLE_RATE: u32 = 32000;

const MIN_SAMPLE_FREQ: f64 = 27.5; // a0
const MAX_SAMPLE_FREQ: f64 = (SPC_SAMPLE_RATE / 2) as f64;

const MIN_MIN_OCTAVE_OFFSET: i32 = -6;

const PITCH_REGISTER_FP_SCALE: u32 = 0x1000;
pub const PITCH_REGISTER_MAX: u16 = 0x3fff;
const PITCH_REGISTER_FLOAT_LIMIT: f64 = 4.0;

#[derive(Clone)]
pub struct InstrumentPitch {
    microsemitones_above_c: i32,
    octaves_above_c0: i32,
    min_octave_offset: i32,
    max_octave_offset: i32,
}

#[derive(Clone)]
pub struct SamplePitches {
    pitches: Vec<u16>,
}

// Calculates the maximum number of octaves a sample can be incremented by.
//
// Samples whose frequency is > b and < c can be increased by two octaves.
// All other sample frequencies can only be increased a single octave.
fn maximum_octave_increment(microsemitones_above_c: i32) -> i32 {
    assert!((0..MICROSEMITONES_PER_OCTAVE).contains(&microsemitones_above_c));

    if microsemitones_above_c <= (SEMITONES_PER_OCTAVE - 1) * SEMITONE_SCALE {
        1
    } else {
        2
    }
}

pub fn instrument_pitch(inst: &Instrument) -> Result<InstrumentPitch, PitchError> {
    if inst.freq < MIN_SAMPLE_FREQ {
        return Err(PitchError::SampleRateTooLow);
    }

    if inst.freq > MAX_SAMPLE_FREQ {
        return Err(PitchError::SampleRateTooHigh);
    }

    if inst.first_octave > inst.last_octave {
        return Err(PitchError::FirstOctaveGreaterThanLastOctave);
    }

    let mst_above_c0 =
        f64::log2(inst.freq / F64_A4_FREQ) * F64_MST_PER_OCTAVE + F64_A4_C0_MST_OFFSET;

    assert!(
        f64::log2(MAX_SAMPLE_FREQ / F64_A4_FREQ) * F64_MST_PER_OCTAVE + F64_A4_C0_MST_OFFSET
            < (i32::MAX / 4) as f64,
        "Cast overflows"
    );
    assert!(mst_above_c0 > 0.0);
    let mst_above_c0 = mst_above_c0.round() as i32;

    let octaves_above_c0: i32 = mst_above_c0 / MICROSEMITONES_PER_OCTAVE;
    let microsemitones_above_c: i32 = mst_above_c0 % MICROSEMITONES_PER_OCTAVE;

    let maximum_octave_increment = maximum_octave_increment(microsemitones_above_c);

    let min_octave_offset = inst.first_octave.as_i32() - octaves_above_c0;
    let max_octave_offset = inst.last_octave.as_i32() - octaves_above_c0;

    let first_octave_valid = min_octave_offset >= MIN_MIN_OCTAVE_OFFSET;
    let last_octave_valid = max_octave_offset <= maximum_octave_increment;

    if first_octave_valid && last_octave_valid {
        Ok(InstrumentPitch {
            microsemitones_above_c,
            octaves_above_c0,
            min_octave_offset,
            max_octave_offset,
        })
    } else {
        let min_off_by = MIN_MIN_OCTAVE_OFFSET - min_octave_offset;
        let max_off_by = max_octave_offset - maximum_octave_increment;

        match (first_octave_valid, last_octave_valid) {
            (false, true) => Err(PitchError::FirstOctaveTooLow(min_off_by)),
            (true, false) => Err(PitchError::LastOctaveTooHigh(max_off_by)),
            (_, _) => Err(PitchError::FirstOctaveTooLowLastOctaveTooHigh(
                min_off_by, max_off_by,
            )),
        }
    }
}

/// Build a new `InstrumentPitch` that contains all octaves that can be played by the sample.
///
/// Used by the play-sample feature of the GUI.
///
/// Returns: a new `InstrumentPitch` and the maximum octave used by the pitch.
pub(crate) fn maximize_pitch_range(pitch: &InstrumentPitch) -> (InstrumentPitch, Octave) {
    let maximum_octave_increment = maximum_octave_increment(pitch.microsemitones_above_c);

    let max_octave = (pitch.octaves_above_c0 + maximum_octave_increment)
        .clamp(Octave::MIN.as_i32(), Octave::MAX.as_i32());
    let max_octave = Octave::try_new(max_octave.try_into().unwrap()).unwrap();

    let min_octave_offset = Octave::MIN.as_i32() - pitch.octaves_above_c0;
    let max_octave_offset = max_octave.as_i32() - pitch.octaves_above_c0;

    let new_pitch = InstrumentPitch {
        microsemitones_above_c: pitch.microsemitones_above_c,
        octaves_above_c0: pitch.octaves_above_c0,
        min_octave_offset,
        max_octave_offset,
    };

    (new_pitch, max_octave)
}

// Using sorted vector instead of Map as I need a reproducible pitch table.
pub(crate) struct SortedPitches {
    instruments: Vec<(usize, InstrumentPitch)>,
    samples: Vec<(usize, SamplePitches)>,
}

/// Assumes `pv` iterator outputs all instruments in the correct order.
pub(crate) fn sort_pitches_iterator(
    instruments: impl Iterator<Item = InstrumentPitch>,
    samples: impl Iterator<Item = SamplePitches>,
) -> SortedPitches {
    let instruments: Vec<(usize, InstrumentPitch)> = instruments.enumerate().collect();

    let samples = samples
        .enumerate()
        .map(|(i, p)| (i + instruments.len(), p))
        .collect();

    sort_pitches_vec(instruments, samples)
}

fn sort_pitches_vec(
    mut instruments: Vec<(usize, InstrumentPitch)>,
    samples: Vec<(usize, SamplePitches)>,
) -> SortedPitches {
    // Using stable sort instead of a hashmap to ensure pitch_table is deterministic
    instruments.sort_by_key(|(_, p)| p.microsemitones_above_c);
    SortedPitches {
        instruments,
        samples,
    }
}

pub fn sample_pitch(sample: &Sample) -> Result<SamplePitches, PitchError> {
    if sample.sample_rates.is_empty() {
        return Err(PitchError::NoSampleRatesInSample);
    }

    let mut invalid_sample_rates = Vec::new();
    let mut pitches = Vec::with_capacity(sample.sample_rates.len());

    for sample_rate in &sample.sample_rates {
        let sample_rate = *sample_rate;

        let pitch = u64::from(sample_rate) * u64::from(PITCH_REGISTER_FP_SCALE)
            / u64::from(SPC_SAMPLE_RATE);

        if pitch <= u64::from(PITCH_REGISTER_MAX) {
            pitches.push(pitch.try_into().unwrap());
        } else {
            invalid_sample_rates.push(sample_rate);
        }
    }

    if invalid_sample_rates.is_empty() {
        Ok(SamplePitches { pitches })
    } else {
        Err(PitchError::InvalidSampleRates(invalid_sample_rates))
    }
}

fn inst_pitch_vec(
    instruments_and_samples: &UniqueNamesList<InstrumentOrSample>,
) -> Result<SortedPitches, PitchTableError> {
    let mut instruments = Vec::with_capacity(instruments_and_samples.len());
    let mut samples = Vec::with_capacity(instruments_and_samples.len());

    let mut errors = Vec::new();

    for (i, inst) in instruments_and_samples.list().iter().enumerate() {
        match inst {
            InstrumentOrSample::Instrument(inst) => match instrument_pitch(inst) {
                Ok(ip) => instruments.push((i, ip)),
                Err(e) => errors.push((i, inst.name.clone(), e)),
            },
            InstrumentOrSample::Sample(sample) => match sample_pitch(sample) {
                Ok(sp) => samples.push((i, sp)),
                Err(e) => errors.push((i, sample.name.clone(), e)),
            },
        }
    }

    if errors.is_empty() {
        Ok(sort_pitches_vec(instruments, samples))
    } else {
        Err(PitchTableError::InstrumentErrors(errors))
    }
}

struct MstIterator<'a> {
    // assumes `remaining` is sorted by `microsemitones_above_c.
    remaining: &'a [(usize, InstrumentPitch)],
}

fn group_by_mst(pitches: &SortedPitches) -> MstIterator {
    MstIterator {
        remaining: pitches.instruments.as_slice(),
    }
}

impl<'a> Iterator for MstIterator<'a> {
    type Item = (i32, &'a [(usize, InstrumentPitch)]);

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.remaining;

        let first = match r.first() {
            Some(ip) => ip,
            None => return None,
        };
        let mst = first.1.microsemitones_above_c;

        let p = r.partition_point(|(_, ip)| ip.microsemitones_above_c == mst);

        let (out, r) = r.split_at(p);
        self.remaining = r;

        Some((mst, out))
    }
}

struct Pt {
    pitches: Vec<u16>,
    instruments_pitch_offset: Vec<u8>,
}

fn process_pitch_vecs(sorted_pitches: SortedPitches, n_instruments_and_samples: usize) -> Pt {
    let mut pitches = Vec::with_capacity(256);
    let mut instruments_pitch_offset = vec![0; n_instruments_and_samples];

    // Instruments
    for (mst, slice) in group_by_mst(&sorted_pitches) {
        // mst = microsemitones_above_c
        assert!(mst >= 0);
        assert!(!slice.is_empty());

        let min_octave_offset = slice
            .iter()
            .map(|(_, ip)| ip.min_octave_offset)
            .min()
            .unwrap();
        let max_octave_offset = slice
            .iter()
            .map(|(_, ip)| ip.max_octave_offset)
            .max()
            .unwrap();

        // The mask to prevent an overflow panic.
        // The length of `pitches` is tested after this loop ends
        let pt_offset = i32::try_from(pitches.len() & 0xff).unwrap();

        for (instrument_id, pitch) in slice {
            let o = pt_offset
                - (pitch.octaves_above_c0 + min_octave_offset) * SEMITONES_PER_OCTAVE
                - PITCH_TABLE_OFFSET as i32;
            instruments_pitch_offset[*instrument_id] = o.to_le_bytes()[0];
        }

        for octave_shift in min_octave_offset..=max_octave_offset {
            for note in 0..SEMITONES_PER_OCTAVE {
                let mst_to_shift =
                    octave_shift * MICROSEMITONES_PER_OCTAVE + note * SEMITONE_SCALE - mst;

                let pitch = 2.0_f64.powf(f64::from(mst_to_shift) / F64_MST_PER_OCTAVE);

                // ::TODO write a test to confirm all valid instrument pitches are in bounds::
                assert!(pitch >= 0.0);
                assert!(pitch < PITCH_REGISTER_FLOAT_LIMIT);
                let pitch_fp = (pitch * f64::from(0x1000)).round() as u16;

                pitches.push(pitch_fp);
            }
        }
    }

    // Samples
    for (inst_id, sp) in &sorted_pitches.samples {
        let o: u8 = match pitches
            .windows(sp.pitches.len())
            .position(|s| s == sp.pitches)
        {
            Some(i) => i.try_into().unwrap_or(0),
            None => {
                let o = pitches.len().try_into().unwrap_or(0);
                pitches.extend(&sp.pitches);
                o
            }
        };

        instruments_pitch_offset[*inst_id] = o.wrapping_sub(PITCH_TABLE_OFFSET);
    }

    Pt {
        pitches,
        instruments_pitch_offset,
    }
}

pub struct PitchTable {
    pub(crate) table_data_l: [u8; MAX_N_PITCHES],
    pub(crate) table_data_h: [u8; MAX_N_PITCHES],

    pub(crate) n_pitches: usize,

    pub(crate) instruments_pitch_offset: Vec<u8>,
}

pub(crate) fn merge_pitch_vec(
    sorted_pitches: SortedPitches,
    n_instruments_and_samples: usize,
) -> Result<PitchTable, PitchTableError> {
    let pt = process_pitch_vecs(sorted_pitches, n_instruments_and_samples);

    if pt.pitches.len() > MAX_N_PITCHES {
        return Err(PitchTableError::TooManyPitches(pt.pitches.len()));
    }
    if pt.instruments_pitch_offset.len() > MAX_INSTRUMENTS_AND_SAMPLES {
        return Err(PitchTableError::TooManyInstruments);
    }

    // By default play at 1.0 pitch
    const DEFAULT_L: u8 = PITCH_REGISTER_FP_SCALE.to_le_bytes()[0];
    const DEFAULT_H: u8 = PITCH_REGISTER_FP_SCALE.to_le_bytes()[1];

    let mut out = PitchTable {
        table_data_l: [DEFAULT_L; MAX_N_PITCHES],
        table_data_h: [DEFAULT_H; MAX_N_PITCHES],
        instruments_pitch_offset: pt.instruments_pitch_offset,
        n_pitches: pt.pitches.len(),
    };

    for (i, p) in pt.pitches.iter().enumerate() {
        let p = p.to_le_bytes();

        out.table_data_l[i] = p[0];
        out.table_data_h[i] = p[1];
    }

    Ok(out)
}

pub fn build_pitch_table(
    instruments_and_samples: &UniqueNamesList<InstrumentOrSample>,
) -> Result<PitchTable, PitchTableError> {
    let sorted_pitches = inst_pitch_vec(instruments_and_samples)?;

    merge_pitch_vec(sorted_pitches, instruments_and_samples.len())
}

impl PitchTable {
    pub fn pitch_for_note(&self, inst_id: InstrumentId, note: Note) -> u16 {
        let offset: u8 = self.instruments_pitch_offset[usize::from(inst_id.as_u8())];
        let index: u8 = offset
            .wrapping_add(PITCH_TABLE_OFFSET)
            .wrapping_add(note.note_id());

        let i = usize::from(index);

        u16::from_le_bytes([self.table_data_l[i], self.table_data_h[i]])
    }

    pub(crate) fn pitch_table_l(&self) -> &[u8] {
        &self.table_data_l[..self.n_pitches]
    }

    pub(crate) fn pitch_table_h(&self) -> &[u8] {
        &self.table_data_h[..self.n_pitches]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstrumentHintFreq(u32);

impl InstrumentHintFreq {
    // Determines accuracy when comparing instrument hints
    const FRACTIONAL_UNITS: u32 = 100;

    pub fn from_freq(frequency: f64) -> Self {
        let f = (frequency * Self::FRACTIONAL_UNITS as f64).round();
        Self(f as u32)
    }

    pub fn from_instrument(inst: &Instrument) -> Self {
        Self::from_freq(inst.freq)
    }
}

impl std::fmt::Display for InstrumentHintFreq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}.{} Hz",
            self.0 / Self::FRACTIONAL_UNITS,
            self.0 % Self::FRACTIONAL_UNITS
        )
    }
}
