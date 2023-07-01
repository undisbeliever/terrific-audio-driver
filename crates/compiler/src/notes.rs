//! Note and pitch functions and structs

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::errors::{NoteError, ValueError};

use serde::Deserialize;

pub const LAST_OCTAVE: u8 = 7;
pub const SEMITONS_PER_OCTAVE: u8 = 12;
pub const LAST_NOTE_ID: u8 = (LAST_OCTAVE + 1) * SEMITONS_PER_OCTAVE - 1;

pub struct PitchChar(u8);

pub fn parse_pitch_char(c: char) -> Result<PitchChar, NoteError> {
    match c {
        'c' => Ok(PitchChar(0)),
        'd' => Ok(PitchChar(2)),
        'e' => Ok(PitchChar(4)),
        'f' => Ok(PitchChar(5)),
        'g' => Ok(PitchChar(7)),
        'a' => Ok(PitchChar(9)),
        'b' => Ok(PitchChar(11)),

        n => Err(NoteError::UnknownNote(n)),
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Note {
    note_id: u8,
}

impl TryFrom<u32> for Note {
    type Error = ValueError;

    fn try_from(note_id: u32) -> Result<Self, Self::Error> {
        if note_id > LAST_NOTE_ID.into() {
            return Err(ValueError::NoteOutOfRange);
        }

        Ok(Note {
            note_id: note_id.try_into().unwrap(),
        })
    }
}

impl Note {
    pub fn note_id(&self) -> u8 {
        self.note_id
    }

    pub fn from_note_id(note_id: u8) -> Result<Self, NoteError> {
        if note_id > LAST_NOTE_ID {
            return Err(NoteError::InvalidNote);
        }
        assert!(LAST_NOTE_ID < u8::MAX);

        Ok(Note { note_id })
    }

    pub fn first_note_for_octave(o: Octave) -> Self {
        const _: () = assert!(LAST_OCTAVE * SEMITONS_PER_OCTAVE <= LAST_NOTE_ID);

        Self::from_note_id(o.as_u8() * SEMITONS_PER_OCTAVE).unwrap()
    }

    pub fn last_note_for_octave(o: Octave) -> Self {
        const _: () = assert!((LAST_OCTAVE + 1) * SEMITONS_PER_OCTAVE - 1 == LAST_NOTE_ID);

        let note_id = (o.as_u8() + 1) * SEMITONS_PER_OCTAVE - 1;
        Self::from_note_id(note_id).unwrap()
    }

    fn from_pitch_stoffset_octave(
        pitch: PitchChar,
        semitone_offset: i32,
        octave: u32,
    ) -> Result<Self, NoteError> {
        if octave > LAST_OCTAVE.into() {
            return Err(NoteError::InvalidNoteOctave(octave));
        }
        assert!((LAST_OCTAVE + 1) * SEMITONS_PER_OCTAVE < u8::MAX);
        let semitones_per_octave = u32::from(SEMITONS_PER_OCTAVE);

        let note_id = (u32::from(pitch.0) + octave * semitones_per_octave)
            .checked_add_signed(semitone_offset);
        let note_id = match note_id {
            Some(n) => n,
            None => return Err(NoteError::InvalidNote),
        };

        let note_id = match u8::try_from(note_id) {
            Ok(i) => i,
            Err(_) => return Err(NoteError::InvalidNote),
        };

        Self::from_note_id(note_id)
    }

    pub fn from_mml_pitch(p: MmlPitch, o: Octave, semitone_offset: i8) -> Result<Self, NoteError> {
        let note_id = (p.pitch.0 + o.0 * SEMITONS_PER_OCTAVE)
            .checked_add_signed(p.semitone_offset)
            .and_then(|n| n.checked_add_signed(semitone_offset));

        let note_id = match note_id {
            Some(n) => n,
            None => return Err(NoteError::InvalidNote),
        };

        Self::from_note_id(note_id)
    }

    pub fn parse_bytecode_argument(arg: &str) -> Result<Note, NoteError> {
        if let Ok(i) = arg.parse() {
            // Integer argument
            Note::from_note_id(i)
        } else {
            // Non-integer argument
            let n_chars = arg.len();

            if n_chars == 0 {
                return Err(NoteError::CannotParseNote(arg.to_owned()));
            }

            let mut note_chars = arg.chars();

            let pitch = match note_chars.next() {
                Some(c) => parse_pitch_char(c)?,
                None => return Err(NoteError::CannotParseNote(arg.to_owned())),
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
                    _ => return Err(NoteError::CannotParseNote(arg.to_owned())),
                }
            }

            if note_chars.next().is_some() {
                // Unparsed characters remain in `note_chars`
                return Err(NoteError::CannotParseNote(arg.to_owned()));
            }

            let octave = match octave {
                Some(o) => o,
                None => return Err(NoteError::CannotParseNote(arg.to_owned())),
            };

            Note::from_pitch_stoffset_octave(pitch, semitone_offset, octave)
        }
    }
}

#[derive(Deserialize, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
#[serde(try_from = "u32")]
pub struct Octave(u8);

pub const STARTING_OCTAVE: Octave = Octave(4);

impl Octave {
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

impl TryFrom<u32> for Octave {
    type Error = ValueError;

    fn try_from(o: u32) -> Result<Self, Self::Error> {
        Self::try_new(o)
    }
}

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
