//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::RangeInclusive;

use crate::bytecode::opcodes;
use crate::bytecode::InstrumentId;
use crate::bytecode::PlayPitchPitch;
use crate::data::InstrumentNoteRange;
use crate::data::{Instrument, InstrumentOrSample, Sample, UniqueNamesList};
use crate::driver_constants::{MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_PITCHES};
use crate::errors::ValueError;
use crate::errors::{PitchError, PitchTableError};
use crate::notes::{self, Note};
use crate::value_newtypes::u16_value_newtype;
use crate::value_newtypes::u32_value_newtype;

const PITCH_TABLE_OFFSET: u8 = opcodes::FIRST_PLAY_NOTE_INSTRUCTION / 2;

const SEMITONES_PER_OCTAVE: i32 = notes::SEMITONES_PER_OCTAVE as i32;

// Using micro-semitones to remove floating point equality comparisons.
const MICROSEMITONE_SCALE: i32 = 1_000_000;
const MICROSEMITONES_PER_OCTAVE: i32 = MICROSEMITONE_SCALE * SEMITONES_PER_OCTAVE;

const FIRST_SEMITONE: i32 = Note::MIN.note_id() as i32;
const LAST_SEMITONE: i32 = Note::MAX.note_id() as i32;

const A4_C0_MICROSEMITONE_OFFSET: i32 = 57 * MICROSEMITONE_SCALE;
const A4_FREQ: u32 = 440;

const F64_A4_FREQ: f64 = A4_FREQ as f64;
const F64_MST_PER_OCTAVE: f64 = MICROSEMITONES_PER_OCTAVE as f64;
const F64_A4_C0_MST_OFFSET: f64 = A4_C0_MICROSEMITONE_OFFSET as f64;

const SPC_SAMPLE_RATE: u32 = 32000;

const MIN_SAMPLE_FREQ: f64 = 27.5; // a0
const MAX_SAMPLE_FREQ: f64 = (SPC_SAMPLE_RATE / 2) as f64;

const MIN_MIN_SEMITONE_OFFSET: i32 = -(8 * SEMITONES_PER_OCTAVE - 1);

const PITCH_REGISTER_FP_SCALE: u32 = 0x1000;
pub const PITCH_REGISTER_MAX: u16 = 0x3fff;
const PITCH_REGISTER_FLOAT_LIMIT: f64 = 4.0;

#[derive(Clone)]
pub struct InstrumentPitch {
    /// Fractional micro-semitones for the tuning frequency.
    /// Fractional component only.
    microsemitones: i32,
    /// Number of semitones between tuning frequency and c0
    semitones_above_c0: i32,

    /// Minimum offset between note-range and tuning frequency, in semitones.
    min_semitone_offset: i32,

    /// Maximum offset between note-range and tuning frequency, in semitones.
    max_semitone_offset: i32,

    note_range: RangeInclusive<Note>,
}

#[derive(Clone)]
pub struct SamplePitches {
    pitches: Vec<u16>,
    note_range: RangeInclusive<Note>,
}

// Calculates the maximum number of semitones a sample can be incremented by.
//
// Samples whose frequency is > b and < c can be increased by two octaves.
// All other sample frequencies can only be increased a single octave.
fn maximum_semitone_increment(microsemitones: i32) -> i32 {
    assert!((0..MICROSEMITONE_SCALE).contains(&microsemitones));

    let max_microsemitones_playback =
        (f64::log2((PITCH_REGISTER_MAX as f64) / (PITCH_REGISTER_FP_SCALE as f64))
            * MICROSEMITONES_PER_OCTAVE as f64) as i32;

    (max_microsemitones_playback + microsemitones) / MICROSEMITONE_SCALE
}

pub fn instrument_pitch(inst: &Instrument) -> Result<InstrumentPitch, PitchError> {
    if inst.freq < MIN_SAMPLE_FREQ {
        return Err(PitchError::SampleRateTooLow);
    }

    if inst.freq > MAX_SAMPLE_FREQ {
        return Err(PitchError::SampleRateTooHigh);
    }

    let note_range = match inst.note_range {
        InstrumentNoteRange::Octave { first, last } => {
            if first > last {
                return Err(PitchError::FirstOctaveGreaterThanLastOctave);
            }
            Note::first_note_for_octave(first)..=Note::last_note_for_octave(last)
        }
        InstrumentNoteRange::Note { first, last } => {
            if first > last {
                return Err(PitchError::FirstNoteGreaterThanLastNote);
            }
            first..=last
        }
    };
    let first_semitone = note_range.start().i32_note_id();
    let last_semitone = note_range.end().i32_note_id();

    let mst_above_c0 =
        f64::log2(inst.freq / F64_A4_FREQ) * F64_MST_PER_OCTAVE + F64_A4_C0_MST_OFFSET;

    assert!(
        f64::log2(MAX_SAMPLE_FREQ / F64_A4_FREQ) * F64_MST_PER_OCTAVE + F64_A4_C0_MST_OFFSET
            < (i32::MAX / 4) as f64,
        "Cast overflows"
    );
    assert!(mst_above_c0 > 0.0);
    let mst_above_c0 = mst_above_c0.round() as i32;

    let semitones_above_c0: i32 = mst_above_c0 / MICROSEMITONE_SCALE;
    let microsemitones: i32 = mst_above_c0 % MICROSEMITONE_SCALE;

    let maximum_semitone_increment = maximum_semitone_increment(microsemitones);

    let min_semitone_offset = first_semitone - semitones_above_c0;
    let max_semitone_offset = last_semitone - semitones_above_c0;

    let first_semitone_valid = min_semitone_offset >= MIN_MIN_SEMITONE_OFFSET;
    let last_semitone_valid = max_semitone_offset <= maximum_semitone_increment;

    if first_semitone_valid && last_semitone_valid {
        Ok(InstrumentPitch {
            microsemitones,
            semitones_above_c0,
            min_semitone_offset,
            max_semitone_offset,
            note_range,
        })
    } else {
        const SEMITONES_PER_OCTAVE: u8 = notes::SEMITONES_PER_OCTAVE;

        let valid_notes = Note::from_i32_clamp(semitones_above_c0 + MIN_MIN_SEMITONE_OFFSET)
            ..=Note::from_i32_clamp(semitones_above_c0 + maximum_semitone_increment);

        match inst.note_range {
            InstrumentNoteRange::Octave { .. } => {
                let valid_octaves = valid_notes.start().note_id().div_ceil(SEMITONES_PER_OCTAVE)
                    ..=((valid_notes.end().note_id() - notes::SEMITONES_PER_OCTAVE + 1)
                        / SEMITONES_PER_OCTAVE);

                match (first_semitone_valid, last_semitone_valid) {
                    (false, true) => Err(PitchError::FirstOctaveTooLow(valid_octaves)),
                    (true, false) => Err(PitchError::LastOctaveTooHigh(valid_octaves)),
                    (_, _) => Err(PitchError::FirstOctaveTooLowLastOctaveTooHigh(
                        valid_octaves,
                    )),
                }
            }
            InstrumentNoteRange::Note { .. } => Err(PitchError::InvalidNoteRange(valid_notes)),
        }
    }
}

/// Build a new `InstrumentPitch` that contains all octaves that can be played by the sample.
///
/// Used by the play-sample feature of the GUI.
///
/// Returns: a new `InstrumentPitch` and the largest note that can be played with `pitch`.
pub(crate) fn maximize_pitch_range(pitch: &InstrumentPitch) -> (InstrumentPitch, Note) {
    let maximum_semitone_increment = maximum_semitone_increment(pitch.microsemitones);

    let max_semitone = (pitch.semitones_above_c0 + maximum_semitone_increment)
        .clamp(FIRST_SEMITONE, LAST_SEMITONE);

    let max_note = Note::from_note_id_u32(max_semitone.try_into().unwrap()).unwrap();

    let min_semitone_offset = FIRST_SEMITONE - pitch.semitones_above_c0;
    let max_semitone_offset = max_semitone - pitch.semitones_above_c0;

    let new_pitch = InstrumentPitch {
        microsemitones: pitch.microsemitones,
        semitones_above_c0: pitch.semitones_above_c0,
        note_range: pitch.note_range.clone(),

        min_semitone_offset,
        max_semitone_offset,
    };

    (new_pitch, max_note)
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
    instruments.sort_by_key(|(_, p)| p.microsemitones);
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
        Ok(SamplePitches {
            note_range: Note::MIN..=Note::from_note_id_usize(pitches.len() - 1).unwrap(),
            pitches,
        })
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
    // assumes `remaining` is sorted by `microsemitones`.
    remaining: &'a [(usize, InstrumentPitch)],
}

fn group_by_mst(pitches: &SortedPitches) -> MstIterator<'_> {
    MstIterator {
        remaining: pitches.instruments.as_slice(),
    }
}

impl<'a> Iterator for MstIterator<'a> {
    type Item = (i32, &'a [(usize, InstrumentPitch)]);

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.remaining;

        let mst = r.first()?.1.microsemitones;

        let p = r.partition_point(|(_, ip)| ip.microsemitones == mst);

        let (out, r) = r.split_at(p);
        self.remaining = r;

        Some((mst, out))
    }
}

struct Pt {
    pitches: Vec<u16>,
    instrument_pto_and_note_range: Vec<(PitchTableOffset, RangeInclusive<Note>)>,
}

fn process_pitch_vecs(sorted_pitches: SortedPitches, n_instruments_and_samples: usize) -> Pt {
    let mut pitches = Vec::with_capacity(256);
    let mut instrument_pto_and_note_range =
        vec![(PitchTableOffset(0), Note::MIN..=Note::MIN); n_instruments_and_samples];

    // Instruments
    for (mst, slice) in group_by_mst(&sorted_pitches) {
        // mst = microsemitones
        assert!(mst >= 0);
        assert!(!slice.is_empty());

        let min_semitone_offset = slice
            .iter()
            .map(|(_, ip)| ip.min_semitone_offset)
            .min()
            .unwrap();
        let max_semitone_offset = slice
            .iter()
            .map(|(_, ip)| ip.max_semitone_offset)
            .max()
            .unwrap();

        // The mask to prevent an overflow panic.
        // The length of `pitches` is tested after this loop ends
        let pt_offset = i32::try_from(pitches.len() & 0xff).unwrap();

        for (instrument_id, pitch) in slice {
            let o = pt_offset
                - (pitch.semitones_above_c0 + min_semitone_offset)
                - PITCH_TABLE_OFFSET as i32;
            instrument_pto_and_note_range[*instrument_id] = (
                PitchTableOffset(o.to_le_bytes()[0]),
                pitch.note_range.clone(),
            );
        }

        for semitone_shift in min_semitone_offset..=max_semitone_offset {
            let mst_to_shift = semitone_shift * MICROSEMITONE_SCALE - mst;

            let pitch = 2.0_f64.powf(f64::from(mst_to_shift) / F64_MST_PER_OCTAVE);

            // ::TODO write a test to confirm all valid instrument pitches are in bounds::
            assert!(pitch >= 0.0);
            assert!(pitch < PITCH_REGISTER_FLOAT_LIMIT);
            let pitch_fp = (pitch * f64::from(0x1000)).round() as u16;

            pitches.push(pitch_fp);
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

        instrument_pto_and_note_range[*inst_id] = (
            PitchTableOffset(o.wrapping_sub(PITCH_TABLE_OFFSET)),
            sp.note_range.clone(),
        );
    }

    Pt {
        pitches,
        instrument_pto_and_note_range,
    }
}

pub struct PitchTable {
    table_data: [u16; MAX_N_PITCHES],

    pub(crate) n_pitches: usize,

    instrument_pto_and_note_range: Vec<(PitchTableOffset, RangeInclusive<Note>)>,
}

pub(crate) fn merge_pitch_vec(
    sorted_pitches: SortedPitches,
    n_instruments_and_samples: usize,
) -> Result<PitchTable, PitchTableError> {
    let pt = process_pitch_vecs(sorted_pitches, n_instruments_and_samples);

    if pt.pitches.len() > MAX_N_PITCHES {
        return Err(PitchTableError::TooManyPitches(pt.pitches.len()));
    }
    if pt.instrument_pto_and_note_range.len() > MAX_INSTRUMENTS_AND_SAMPLES {
        return Err(PitchTableError::TooManyInstruments);
    }

    let mut out = PitchTable {
        table_data: [PITCH_REGISTER_FP_SCALE as u16; MAX_N_PITCHES],
        instrument_pto_and_note_range: pt.instrument_pto_and_note_range,
        n_pitches: pt.pitches.len(),
    };

    out.table_data[..pt.pitches.len()].copy_from_slice(&pt.pitches);

    Ok(out)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PitchTableOffset(u8);

pub fn build_pitch_table(
    instruments_and_samples: &UniqueNamesList<InstrumentOrSample>,
) -> Result<PitchTable, PitchTableError> {
    let sorted_pitches = inst_pitch_vec(instruments_and_samples)?;

    merge_pitch_vec(sorted_pitches, instruments_and_samples.len())
}

impl PitchTable {
    pub(crate) fn pitch_table_offset(&self, inst_id: InstrumentId) -> PitchTableOffset {
        self.instrument_pto_and_note_range[usize::from(inst_id.as_u8())].0
    }

    pub(crate) fn instrument_pto_and_note_range(
        &self,
        inst_id: InstrumentId,
    ) -> (PitchTableOffset, RangeInclusive<Note>) {
        self.instrument_pto_and_note_range[usize::from(inst_id.as_u8())].clone()
    }

    pub(crate) fn get_pitch(&self, offset: PitchTableOffset, note: Note) -> u16 {
        let index: u8 = offset
            .0
            .wrapping_add(PITCH_TABLE_OFFSET)
            .wrapping_add(note.note_id());

        self.table_data[usize::from(index)]
    }

    pub(crate) fn instruments_pitch_offset_len(&self) -> usize {
        self.instrument_pto_and_note_range.len()
    }

    pub(crate) fn instruments_pitch_offset(&self) -> impl ExactSizeIterator<Item = u8> + use<'_> {
        self.instrument_pto_and_note_range.iter().map(|(o, _)| o.0)
    }

    pub(crate) fn pitch_table_l(&self) -> impl ExactSizeIterator<Item = u8> + use<'_> {
        self.table_data[..self.n_pitches]
            .iter()
            .map(|p| p.to_le_bytes()[0])
    }

    pub(crate) fn pitch_table_h(&self) -> impl ExactSizeIterator<Item = u8> + use<'_> {
        self.table_data[..self.n_pitches]
            .iter()
            .map(|p| p.to_le_bytes()[1])
    }
}

u32_value_newtype!(
    PlayPitchSampleRate,
    PlayPitchSampleRateOutOfRange,
    NoPlayPitchSampleRate,
    0,
    127999
);

impl PlayPitchSampleRate {
    pub fn to_vxpitch(self) -> PlayPitchPitch {
        const _: () = assert!(
            (PlayPitchSampleRate::MAX.as_u32() as u64) * (PITCH_REGISTER_FP_SCALE as u64)
                / (SPC_SAMPLE_RATE as u64)
                <= PlayPitchPitch::MAX.as_u16() as u64
        );

        let pitch =
            u64::from(self.0) * u64::from(PITCH_REGISTER_FP_SCALE) / u64::from(SPC_SAMPLE_RATE);

        u32::try_from(pitch).unwrap().try_into().unwrap()
    }
}

u16_value_newtype!(
    PlayPitchFrequency,
    PlayPitchFrequencyOutOfRange,
    NoPlayPitchFrequency,
    0,
    16000
);

impl PlayPitchFrequency {
    pub fn to_vxpitch(
        self,
        instrument: Option<&InstrumentOrSample>,
    ) -> Result<PlayPitchPitch, ValueError> {
        match instrument {
            Some(InstrumentOrSample::Instrument(i)) => {
                let pitch = f64::from(self.as_u16()) / i.freq * f64::from(PITCH_REGISTER_FP_SCALE);
                let pitch = pitch.round() as u32;

                match PlayPitchPitch::try_from(pitch) {
                    Ok(p) => Ok(p),
                    Err(_) => Err(ValueError::CannotConvertPitchFrequency(self, pitch)),
                }
            }
            Some(InstrumentOrSample::Sample(_)) => {
                Err(ValueError::CannotConvertPitchFrequencySample)
            }
            None => Err(ValueError::CannotConvertPitchFrequencyUnknownInstrument),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstrumentHintFreq(u32);

impl InstrumentHintFreq {
    // Store frequency in fractional (fixed-point) units so frequency can be == compared
    // ::TODO confirm 2 decimal units is enough accuracy::
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
        const _: () = assert!(InstrumentHintFreq::FRACTIONAL_UNITS == 100);
        write!(
            f,
            "{}.{:02} Hz",
            self.0 / Self::FRACTIONAL_UNITS,
            self.0 % Self::FRACTIONAL_UNITS
        )
    }
}
