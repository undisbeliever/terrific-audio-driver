//! Note and pitch functions and structs

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::errors::ValueError;
use crate::value_newtypes::{u8_value_newtype, ValueNewType};

use serde::{Deserialize, Serialize, Serializer};

u8_value_newtype!(MidiNote, MidiNoteNumberOutOfRange, NoMidiNote, 0, 127);

pub const LAST_OCTAVE: u8 = 7;
pub const SEMITONES_PER_OCTAVE: u8 = 12;
pub const LAST_NOTE_ID: u8 = (LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE - 1;

pub const N_NOTES: u8 = (LAST_OCTAVE + 1) * SEMITONES_PER_OCTAVE;

#[derive(Debug, Clone, Copy)]
pub struct PitchChar(u8);

pub fn parse_pitch_char(c: char) -> Result<PitchChar, ValueError> {
    match c {
        'c' => Ok(PitchChar(0)),
        'd' => Ok(PitchChar(2)),
        'e' => Ok(PitchChar(4)),
        'f' => Ok(PitchChar(5)),
        'g' => Ok(PitchChar(7)),
        'a' => Ok(PitchChar(9)),
        'b' => Ok(PitchChar(11)),

        n => Err(ValueError::UnknownNotePitch(n)),
    }
}

impl TryFrom<u8> for PitchChar {
    type Error = ValueError;

    fn try_from(i: u8) -> Result<Self, Self::Error> {
        if i < SEMITONES_PER_OCTAVE {
            Ok(PitchChar(i))
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
    /// audio-driver note_id.
    /// NOT MIDI-note-number and NOT Piano-key-number
    pub fn note_id(&self) -> u8 {
        self.note_id
    }

    fn from_note_id(note_id: u8) -> Result<Self, ValueError> {
        if note_id > LAST_NOTE_ID {
            return Err(ValueError::InvalidNote);
        }
        assert!(LAST_NOTE_ID < u8::MAX);

        Ok(Note { note_id })
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

    pub fn from_pitch_and_octave(p: PitchChar, o: Octave) -> Result<Self, ValueError> {
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

    fn from_pitch_stoffset_octave(
        pitch: PitchChar,
        semitone_offset: i32,
        octave: u32,
    ) -> Result<Self, ValueError> {
        if octave > LAST_OCTAVE.into() {
            return Err(ValueError::OctaveOutOfRange);
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

    pub fn from_mml_pitch(p: MmlPitch, o: Octave, semitone_offset: i8) -> Result<Self, ValueError> {
        let note_id = (p.pitch.0 + o.0 * SEMITONES_PER_OCTAVE)
            .checked_add_signed(p.semitone_offset)
            .and_then(|n| n.checked_add_signed(semitone_offset));

        let note_id = match note_id {
            Some(n) => n,
            None => return Err(ValueError::InvalidNote),
        };

        Self::from_note_id(note_id)
    }

    pub fn parse_bytecode_argument(arg: &str) -> Result<Note, ValueError> {
        if let Ok(i) = arg.parse() {
            // Integer argument
            Note::from_note_id(i)
        } else {
            // Non-integer argument
            let mut note_chars = arg.chars();

            let pitch = match note_chars.next() {
                Some(c) => parse_pitch_char(c)?,
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

            Note::from_pitch_stoffset_octave(pitch, semitone_offset, octave)
        }
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
            Err(ValueError::OctaveOutOfRange)
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

impl ValueNewType for Octave {
    type ValueType = u8;
    type ConvertFrom = u32;
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
    pitch: PitchChar,
    semitone_offset: i8,
}

impl MmlPitch {
    pub fn new(pitch: PitchChar, semitone_offset: i8) -> Self {
        Self {
            pitch,
            semitone_offset,
        }
    }
}
