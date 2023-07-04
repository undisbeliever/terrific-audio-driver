//! Sample compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::InstrumentId;
use crate::data::{Instrument, UniqueNamesList};
use crate::driver_constants::{MAX_INSTRUMENTS, PITCH_TABLE_SIZE};
use crate::errors::{PitchError, PitchTableError};
use crate::notes::{self, Note};

const SEMITONES_PER_OCTAVE: i32 = notes::SEMITONS_PER_OCTAVE as i32;

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
const MAX_MAX_OCTAVE_OFFSET: i32 = 2;

const PITCH_REGISTER_FP_SCALE: u32 = 0x1000;
const PITCH_REGISTER_FLOAT_LIMIT: f64 = 4.0;

struct InstrumentPitch {
    instrument_id: usize,
    microsemitones_above_c: i32,
    octaves_above_c0: i32,
    min_octave_offset: i32,
    max_octave_offset: i32,
}

fn instrument_pitch(index: usize, inst: &Instrument) -> Result<InstrumentPitch, PitchError> {
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

    let min_octave_offset = inst.first_octave.as_i32() - octaves_above_c0;
    let max_octave_offset = inst.last_octave.as_i32() - octaves_above_c0;

    let first_octave_valid = min_octave_offset >= -MIN_MIN_OCTAVE_OFFSET;
    let last_octave_valid = max_octave_offset <= MAX_MAX_OCTAVE_OFFSET;

    if min_octave_offset >= MIN_MIN_OCTAVE_OFFSET && max_octave_offset <= MAX_MAX_OCTAVE_OFFSET {
        Ok(InstrumentPitch {
            instrument_id: index,
            microsemitones_above_c,
            octaves_above_c0,
            min_octave_offset,
            max_octave_offset,
        })
    } else {
        let min_off_by = MIN_MIN_OCTAVE_OFFSET - min_octave_offset;
        let max_off_by = max_octave_offset - MAX_MAX_OCTAVE_OFFSET;

        match (first_octave_valid, last_octave_valid) {
            (false, true) => Err(PitchError::FirstOctaveTooLow(min_off_by)),
            (true, false) => Err(PitchError::LastOctaveTooHigh(max_off_by)),
            (_, _) => Err(PitchError::FirstOctaveTooLowLastOctaveTooHigh(
                min_off_by, max_off_by,
            )),
        }
    }
}

// Using sorted vector instead of Map as I need a reproducible pitch table.
struct SortedInstrumentPitches(Vec<InstrumentPitch>);

fn pitch_vec(
    instruments: &UniqueNamesList<Instrument>,
) -> Result<SortedInstrumentPitches, PitchTableError> {
    let mut out = Vec::with_capacity(instruments.len());

    let mut errors = Vec::new();

    for (i, inst) in instruments.list().iter().enumerate() {
        match instrument_pitch(i, inst) {
            Ok(ip) => out.push(ip),
            Err(e) => errors.push((i, e)),
        }
    }

    if errors.is_empty() {
        // Using stable sort instead of a hashmap to ensure pitch_table is deterministic
        out.sort_by_key(|p| p.microsemitones_above_c);

        Ok(SortedInstrumentPitches(out))
    } else {
        Err(PitchTableError::InstrumentErrors(errors))
    }
}

struct MstIterator<'a> {
    // assumes `remaining` is sorted by `microsemitones_above_c.
    remaining: &'a [InstrumentPitch],
}

fn group_by_mst(pitches: &SortedInstrumentPitches) -> MstIterator {
    MstIterator {
        remaining: pitches.0.as_slice(),
    }
}

impl<'a> Iterator for MstIterator<'a> {
    type Item = (i32, &'a [InstrumentPitch]);

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.remaining;

        let first = match r.first() {
            Some(ip) => ip,
            None => return None,
        };
        let mst = first.microsemitones_above_c;

        let p = r.partition_point(|ip| ip.microsemitones_above_c == mst);

        let (out, r) = r.split_at(p);
        self.remaining = r;

        Some((mst, out))
    }
}

struct Pt {
    pitches: Vec<u16>,
    instruments_pitch_offset: Vec<u8>,
}

fn process_pitch_vec(inst_pitches: SortedInstrumentPitches, n_instruments: usize) -> Pt {
    let mut pitches = Vec::with_capacity(256);
    let mut instruments_pitch_offset = vec![0; n_instruments];

    for (mst, slice) in group_by_mst(&inst_pitches) {
        // mst = microsemitones_above_c
        assert!(mst > 0);
        assert!(!slice.is_empty());

        let min_octave_offset = slice.iter().map(|ip| ip.min_octave_offset).min().unwrap();
        let max_octave_offset = slice.iter().map(|ip| ip.max_octave_offset).max().unwrap();

        // The mask to prevent an overflow panic.
        // The length of `pitches` is tested after this loop ends
        let pt_offset = i32::try_from(pitches.len() & 0xff).unwrap();

        for inst in slice {
            let o = pt_offset - (inst.octaves_above_c0 + min_octave_offset) * SEMITONES_PER_OCTAVE;
            instruments_pitch_offset[inst.instrument_id] = o.to_le_bytes()[0];
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

    Pt {
        pitches,
        instruments_pitch_offset,
    }
}

pub struct PitchTable {
    pub(crate) table_data_l: [u8; PITCH_TABLE_SIZE],
    pub(crate) table_data_h: [u8; PITCH_TABLE_SIZE],

    pub(crate) instruments_pitch_offset: Vec<u8>,
}

pub fn build_pitch_table(
    instruments: &UniqueNamesList<Instrument>,
) -> Result<PitchTable, PitchTableError> {
    let pv = pitch_vec(instruments)?;
    let pt = process_pitch_vec(pv, instruments.len());

    if pt.pitches.len() > PITCH_TABLE_SIZE {
        return Err(PitchTableError::TooManyPitches(pt.pitches.len()));
    }
    if pt.instruments_pitch_offset.len() > MAX_INSTRUMENTS {
        return Err(PitchTableError::TooManyInstruments);
    }

    // By default play at 1.0 pitch
    const DEFAULT_L: u8 = PITCH_REGISTER_FP_SCALE.to_le_bytes()[0];
    const DEFAULT_H: u8 = PITCH_REGISTER_FP_SCALE.to_le_bytes()[1];

    let mut out = PitchTable {
        table_data_l: [DEFAULT_L; PITCH_TABLE_SIZE],
        table_data_h: [DEFAULT_H; PITCH_TABLE_SIZE],
        instruments_pitch_offset: pt.instruments_pitch_offset,
    };

    for (i, p) in pt.pitches.iter().enumerate() {
        let p = p.to_le_bytes();

        out.table_data_l[i] = p[0];
        out.table_data_h[i] = p[1];
    }

    Ok(out)
}

impl PitchTable {
    pub fn pitch_for_note(&self, inst_id: InstrumentId, note: Note) -> u16 {
        let offset: u8 = self.instruments_pitch_offset[inst_id.as_usize()];
        let index: u8 = offset.wrapping_add(note.note_id());

        let i = usize::from(index);

        u16::from_le_bytes([self.table_data_l[i], self.table_data_h[i]])
    }
}
