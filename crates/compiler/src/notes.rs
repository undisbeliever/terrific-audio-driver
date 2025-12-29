//! Note and pitch functions and structs

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use std::ops::RangeInclusive;

use crate::errors::{BytecodeError, ValueError};
use crate::value_newtypes::{u8_value_newtype, UnsignedValueNewType};

use serde::de::Error;
use serde::{Deserialize, Serialize, Serializer};

u8_value_newtype!(MidiNote, MidiNoteNumberOutOfRange, NoMidiNote, 0, 127);

pub const LAST_OCTAVE: u8 = 7;
pub const SEMITONES_PER_OCTAVE: u8 = 12;
pub const LAST_NOTE_ID: u8 = (LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE - 1;

pub const N_NOTES: u8 = (LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE;

pub const N_PITCHES: usize = 7;

const PITCH_LETTERS: [char; N_PITCHES] = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];

const NOTE_STRS: [&str; SEMITONES_PER_OCTAVE as usize] = [
    "c", "c+", "d", "d+", "e", "f", "f+", "g", "g+", "a", "a+", "b",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeySignature([i8; N_PITCHES]);

impl KeySignature {
    const DEFAULT: [i8; N_PITCHES] = [0, 2, 4, 5, 7, 9, 11];

    fn parse_named_signature(s: &str) -> Option<Self> {
        match s {
            "A" | "f+" => Some(Self([1, 2, 4, 6, 8, 9, 11])),
            "A-" | "f" => Some(Self([0, 1, 3, 5, 7, 8, 10])),
            "B" | "g+" => Some(Self([1, 3, 4, 6, 8, 10, 11])),
            "B-" | "g" => Some(Self([0, 2, 3, 5, 7, 9, 10])),
            "C" | "a" => Some(Self([0, 2, 4, 5, 7, 9, 11])),
            "C+" | "a+" => Some(Self([1, 3, 5, 6, 8, 10, 12])),
            "C-" | "a-" => Some(Self([-1, 1, 3, 4, 6, 8, 10])),
            "D" | "b" => Some(Self([1, 2, 4, 6, 7, 9, 11])),
            "D-" | "b-" => Some(Self([0, 1, 3, 5, 6, 8, 10])),
            "E" | "c+" => Some(Self([1, 3, 4, 6, 8, 9, 11])),
            "E-" | "c" => Some(Self([0, 2, 3, 5, 7, 8, 10])),
            "F" | "d" => Some(Self([0, 2, 4, 5, 7, 9, 10])),
            "F+" | "d+" => Some(Self([1, 3, 5, 6, 8, 10, 11])),
            "G" | "e" => Some(Self([0, 2, 4, 6, 7, 9, 11])),
            "G-" | "e-" => Some(Self([-1, 1, 3, 5, 6, 8, 10])),
            _ => None,
        }
    }

    pub fn parse_scale_or_signature_change(&self, s: &str) -> Result<Self, ValueError> {
        match Self::parse_named_signature(s) {
            Some(k) => Ok(k),
            None => match self.parse_signature_changes(s) {
                Err(ValueError::NoKeySignatureSign) => Err(ValueError::UnknownKeySignature),
                r => r,
            },
        }
    }

    pub fn parse_signature_changes(&self, s: &str) -> Result<Self, ValueError> {
        let mut it = s.chars();

        let offset = match it.next() {
            Some('=') => 0,
            Some('+') => 1,
            Some('-') => -1,
            _ => return Err(ValueError::NoKeySignatureSign),
        };

        let s = it.as_str().trim();

        if s.is_empty() {
            return Err(ValueError::NoTonesInKeySignature);
        }

        let mut out = self.clone();

        for c in s.chars() {
            if !c.is_whitespace() {
                match parse_pitch_char(c) {
                    Ok(p) => {
                        let i = usize::from(p.0);
                        out.0[i] = Self::DEFAULT[i] + offset;
                    }
                    Err(_) => return Err(ValueError::InvalidKeySignatureTone(c)),
                }
            }
        }

        Ok(out)
    }

    pub fn space_mml_string_if_not_default(&self) -> String {
        let iter = self.0.iter().zip(&Self::DEFAULT);

        let mut out = String::new();

        if iter.clone().any(|(s, d)| s > d) {
            out.push_str(" _{+");
            for (i, (s, d)) in iter.clone().enumerate() {
                if s > d {
                    out.push(PITCH_LETTERS[i]);
                }
            }
            out.push('}');
        }

        if iter.clone().any(|(s, d)| s < d) {
            out.push_str(" _{-");
            for (i, (s, d)) in iter.enumerate() {
                if s < d {
                    out.push(PITCH_LETTERS[i]);
                }
            }
            out.push('}');
        }

        out
    }
}

impl Default for KeySignature {
    fn default() -> Self {
        Self(Self::DEFAULT)
    }
}

/// Format is pitch character number (0..=6)
#[derive(Debug, Clone, Copy)]
pub struct MmlPitchChar(u8);

pub fn parse_pitch_char(c: char) -> Result<MmlPitchChar, ValueError> {
    match c {
        'c' => Ok(MmlPitchChar(0)),
        'd' => Ok(MmlPitchChar(1)),
        'e' => Ok(MmlPitchChar(2)),
        'f' => Ok(MmlPitchChar(3)),
        'g' => Ok(MmlPitchChar(4)),
        'a' => Ok(MmlPitchChar(5)),
        'b' => Ok(MmlPitchChar(6)),

        n => Err(ValueError::UnknownNotePitch(n)),
    }
}

/// Format is semitone offset (0..=11)
#[derive(Debug, Clone, Copy)]
pub struct PitchSemitoneIndex(u8);

pub fn parse_bytecode_pitch_char(c: char) -> Result<PitchSemitoneIndex, ValueError> {
    match c {
        'c' => Ok(PitchSemitoneIndex(0)),
        'd' => Ok(PitchSemitoneIndex(2)),
        'e' => Ok(PitchSemitoneIndex(4)),
        'f' => Ok(PitchSemitoneIndex(5)),
        'g' => Ok(PitchSemitoneIndex(7)),
        'a' => Ok(PitchSemitoneIndex(9)),
        'b' => Ok(PitchSemitoneIndex(11)),

        n => Err(ValueError::UnknownNotePitch(n)),
    }
}

impl TryFrom<u8> for PitchSemitoneIndex {
    type Error = ValueError;

    fn try_from(i: u8) -> Result<Self, Self::Error> {
        if i < SEMITONES_PER_OCTAVE {
            Ok(PitchSemitoneIndex(i))
        } else {
            Err(ValueError::InvalidPitch)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Note {
    note_id: u8,
}

impl TryFrom<MidiNote> for Note {
    type Error = ValueError;

    fn try_from(n: MidiNote) -> Result<Self, Self::Error> {
        const MIDI_C0: u8 = 12;

        match n.as_u8().checked_sub(MIDI_C0) {
            None => Err(ValueError::CannotConvertMidiNote),
            Some(note_id) => match Note::from_note_id(note_id) {
                Err(_) => Err(ValueError::CannotConvertMidiNote),
                Ok(n) => Ok(n),
            },
        }
    }
}

impl Note {
    pub const MIN: Self = Self { note_id: 0 };
    pub const MAX: Self = Self {
        note_id: LAST_NOTE_ID,
    };

    /// audio-driver note_id.
    /// NOT MIDI-note-number and NOT Piano-key-number
    pub const fn note_id(&self) -> u8 {
        self.note_id
    }

    pub fn i32_note_id(&self) -> i32 {
        self.note_id.into()
    }

    fn from_note_id(note_id: u8) -> Result<Self, ValueError> {
        if note_id > LAST_NOTE_ID {
            return Err(ValueError::InvalidNote);
        }
        assert!(LAST_NOTE_ID < u8::MAX);

        Ok(Note { note_id })
    }

    pub fn from_i32_clamp(note_id: i32) -> Self {
        const MIN: i32 = Note::MIN.note_id as i32;
        const MAX: i32 = Note::MAX.note_id as i32;

        match note_id {
            MIN..=MAX => Self {
                note_id: note_id.try_into().unwrap(),
            },
            ..MIN => Self::MIN,
            _ => Self::MAX,
        }
    }

    pub fn from_note_id_u32(note_id: u32) -> Result<Self, ValueError> {
        match note_id.try_into() {
            Ok(i) => Self::from_note_id(i),
            Err(_) => Err(ValueError::InvalidNote),
        }
    }

    pub fn from_note_id_usize(note_id: usize) -> Result<Self, ValueError> {
        match note_id.try_into() {
            Ok(i) => Self::from_note_id(i),
            Err(_) => Err(ValueError::InvalidNote),
        }
    }

    pub fn from_pitch_and_octave(p: PitchSemitoneIndex, o: Octave) -> Result<Self, ValueError> {
        Self::from_note_id(p.0 + o.as_u8() * SEMITONES_PER_OCTAVE)
    }

    pub fn first_note_for_octave(o: Octave) -> Self {
        const _: () = assert!(LAST_OCTAVE * SEMITONES_PER_OCTAVE <= LAST_NOTE_ID);

        Self::from_note_id(o.as_u8() * SEMITONES_PER_OCTAVE).unwrap()
    }

    pub fn last_note_for_octave(o: Octave) -> Self {
        const _: () = assert!((LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE - 1 == LAST_NOTE_ID);

        let note_id = (o.as_u8() + 1) * SEMITONES_PER_OCTAVE - 1;
        Self::from_note_id(note_id).unwrap()
    }

    fn from_bc_pitch_stoffset_octave(
        pitch: PitchSemitoneIndex,
        semitone_offset: i32,
        octave: u32,
    ) -> Result<Self, ValueError> {
        if octave > LAST_OCTAVE.into() {
            return Err(ValueError::OctaveOutOfRange(octave));
        }
        assert!((LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE < u8::MAX);
        let semitones_per_octave = u32::from(SEMITONES_PER_OCTAVE);

        let note_id = (u32::from(pitch.0) + octave * semitones_per_octave)
            .checked_add_signed(semitone_offset);
        let note_id = match note_id {
            Some(n) => n,
            None => return Err(ValueError::InvalidNote),
        };

        let note_id = match u8::try_from(note_id) {
            Ok(i) => i,
            Err(_) => return Err(ValueError::InvalidNote),
        };

        Self::from_note_id(note_id)
    }

    pub fn from_mml_pitch(
        p: MmlPitch,
        o: Octave,
        signature: &KeySignature,
        semitone_offset: i8,
    ) -> Result<Self, ValueError> {
        let pitch = match p.natural {
            false => signature.0[p.pitch.0 as usize],
            true => KeySignature::default().0[p.pitch.0 as usize],
        };

        let note_id: i32 = i32::from(pitch)
            + i32::from(o.0 * SEMITONES_PER_OCTAVE)
            + i32::from(p.semitone_offset)
            + i32::from(semitone_offset);

        match note_id.try_into() {
            Ok(n) => Self::from_note_id(n),
            Err(_) => Err(ValueError::InvalidNote),
        }
    }

    pub fn parse_bytecode_argument(arg: &str) -> Result<Note, ValueError> {
        if let Ok(i) = arg.parse() {
            // Integer argument
            Note::from_note_id(i)
        } else {
            // Non-integer argument
            let mut note_chars = arg.chars();

            let pitch = match note_chars.next() {
                Some(c) => parse_bytecode_pitch_char(c)?,
                None => return Err(ValueError::NoNote),
            };

            let mut semitone_offset = 0;
            let mut octave = None;
            for c in note_chars.by_ref() {
                match c {
                    c if c.is_ascii_digit() => {
                        octave = c.to_digit(10);
                        break;
                    }
                    '+' => {
                        semitone_offset += 1;
                    }
                    '-' => {
                        semitone_offset -= 1;
                    }
                    c => return Err(ValueError::UnknownNoteCharacter(c)),
                }
            }

            if note_chars.next().is_some() {
                // Unparsed characters remain in `note_chars`
                return Err(ValueError::InvalidNote);
            }

            let octave = match octave {
                Some(o) => o,
                None => return Err(ValueError::NoNoteOctave),
            };

            Note::from_bc_pitch_stoffset_octave(pitch, semitone_offset, octave)
        }
    }

    pub fn to_bytecode_argument(self) -> String {
        NoteBcArgDisplay(self).to_string()
    }

    pub fn bytecode_argument_display(self) -> NoteBcArgDisplay {
        NoteBcArgDisplay(self)
    }

    pub fn add_transpose(self, t: i8) -> Result<Self, BytecodeError> {
        const MIN: u8 = Note::MIN.note_id;
        const MAX: u8 = Note::MAX.note_id;

        match self.note_id.checked_add_signed(t) {
            Some(note_id @ MIN..=MAX) => Ok(Note { note_id }),
            _ => Err(BytecodeError::TransposedNoteOverflow(self, t..=t)),
        }
    }

    pub fn add_transpose_range(self, tmin: i8, tmax: i8) -> Result<(Self, Self), BytecodeError> {
        const MIN: u8 = Note::MIN.note_id;
        const MAX: u8 = Note::MAX.note_id;

        debug_assert!(tmin <= tmax);

        match (
            self.note_id.checked_add_signed(tmin),
            self.note_id.checked_add_signed(tmax),
        ) {
            (Some(min @ MIN..=MAX), Some(max @ MIN..=MAX)) => {
                Ok((Note { note_id: min }, Note { note_id: max }))
            }
            _ => Err(BytecodeError::TransposedNoteOverflow(self, tmin..=tmax)),
        }
    }

    fn to_note_str_and_octave(self) -> (&'static str, u8) {
        (
            NOTE_STRS[usize::from(self.note_id % SEMITONES_PER_OCTAVE)],
            self.note_id / SEMITONES_PER_OCTAVE,
        )
    }

    pub fn octave(self) -> Octave {
        Octave(self.note_id / SEMITONES_PER_OCTAVE)
    }
}

impl<'de> Deserialize<'de> for Note {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let note = <&str>::deserialize(deserializer)?;

        Note::parse_bytecode_argument(note).map_err(D::Error::custom)
    }
}

impl Serialize for Note {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_bytecode_argument())
    }
}

pub struct NoteBcArgDisplay(Note);

impl std::fmt::Display for NoteBcArgDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (note, octave) = self.0.to_note_str_and_octave();

        write!(f, "{note}{octave}")
    }
}

pub(crate) trait NoteRange {
    fn extend_note(&mut self, n: Note);

    fn merge(&mut self, r: &RangeInclusive<Note>);
}

impl NoteRange for RangeInclusive<Note> {
    fn extend_note(&mut self, note: Note) {
        if self.is_empty() {
            *self = note..=note;
        } else if note < *self.start() {
            *self = note..=*(self.end());
        } else if note > *self.end() {
            *self = *(self.start())..=note;
        }
    }

    fn merge(&mut self, r: &RangeInclusive<Note>) {
        if self.is_empty() {
            *self = r.clone()
        } else {
            *self = std::cmp::min(*self.start(), *r.start())..=std::cmp::max(*self.end(), *r.end())
        }
    }
}

pub(crate) fn add_transpose_range_to_note_range(
    note_range: &RangeInclusive<Note>,
    tmin: i8,
    tmax: i8,
) -> Result<RangeInclusive<Note>, BytecodeError> {
    if !note_range.is_empty() {
        let start = note_range.start().add_transpose(tmin)?;
        let end = note_range.end().add_transpose(tmax)?;
        Ok(start..=end)
    } else {
        Ok(note_range.clone())
    }
}

#[derive(Deserialize, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
#[serde(try_from = "u32")]
pub struct Octave(u8);

impl Serialize for Octave {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u8(self.as_u8())
    }
}

pub const STARTING_OCTAVE: Octave = Octave(4);

impl Octave {
    pub const MIN: Self = Self(0);
    pub const MAX: Self = Self(LAST_OCTAVE);

    pub fn try_new(o: u32) -> Result<Self, ValueError> {
        if o <= LAST_OCTAVE.into() {
            Ok(Octave(o.try_into().unwrap()))
        } else {
            Err(ValueError::OctaveOutOfRange(o))
        }
    }

    pub fn as_i32(&self) -> i32 {
        self.0.into()
    }

    pub fn as_u8(&self) -> u8 {
        self.0
    }

    pub fn saturating_increment(&mut self) {
        if self.0 < LAST_OCTAVE {
            self.0 += 1;
        }
    }

    pub fn saturating_decrement(&mut self) {
        if self.0 != 0 {
            self.0 -= 1;
        }
    }
}

impl UnsignedValueNewType for Octave {
    type ValueType = u8;

    const MISSING_ERROR: ValueError = ValueError::NoOctave;

    fn value(&self) -> Self::ValueType {
        self.0
    }
}

impl TryFrom<u32> for Octave {
    type Error = ValueError;

    fn try_from(o: u32) -> Result<Self, Self::Error> {
        Self::try_new(o)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MmlPitch {
    pitch: MmlPitchChar,
    natural: bool,
    semitone_offset: i8,
}

impl MmlPitch {
    pub fn new(pitch: MmlPitchChar, natural: bool, semitone_offset: i8) -> Self {
        Self {
            pitch,
            natural,
            semitone_offset,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn note_to_bytecode_argument() {
        for n in Note::MIN.note_id()..=Note::MAX.note_id() {
            let n = Note::from_note_id(n).unwrap();
            assert_eq!(
                Note::parse_bytecode_argument(&n.to_bytecode_argument()),
                Ok(n)
            );
        }
    }
}
