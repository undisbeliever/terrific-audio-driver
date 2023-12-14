//! MML tokenizer

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::tokenizer::{PeekingTokenizer, Token};
use super::{IdentifierStr, Section};

#[cfg(feature = "mml_tracking")]
use super::note_tracking::CursorTracker;

use crate::bytecode::{
    LoopCount, Pan, PitchOffsetPerTick, PlayNoteTicks, QuarterWavelengthInTicks, RelativePan,
    RelativeVolume, SubroutineId, Volume, KEY_OFF_TICK_DELAY,
};
use crate::errors::{ErrorWithPos, MmlError, ValueError};
use crate::file_pos::{FilePos, FilePosRange};
use crate::notes::{MidiNote, MmlPitch, Note, Octave, STARTING_OCTAVE};
use crate::time::{
    Bpm, MmlDefaultLength, MmlLength, TickClock, TickCounter, TickCounterWithLoopFlag, ZenLen,
    STARTING_MML_LENGTH,
};
use crate::value_newtypes::{i8_value_newtype, u8_value_newtype};
use crate::ValueNewType;

use std::cmp::min;
use std::collections::HashMap;

pub const MAX_COARSE_VOLUME: u32 = 16;
pub const COARSE_VOLUME_MULTIPLIER: u8 = 16;
pub const MIN_RELATIVE_COARSE_VOLUME: i8 = i8::MIN / COARSE_VOLUME_MULTIPLIER as i8;
pub const MAX_RELATIVE_COARSE_VOLUME: i8 = i8::MAX / COARSE_VOLUME_MULTIPLIER as i8;

u8_value_newtype!(
    PortamentoSpeed,
    PortamentoSpeedOutOfRange,
    NoPortamentoSpeed
);
u8_value_newtype!(Quantization, QuantizeOutOfRange, NoQuantize, 0, 8);
i8_value_newtype!(Transpose, TransposeOutOfRange, NoTranspose);

#[derive(Debug, Copy, Clone)]
pub enum VolumeCommand {
    Absolute(Volume),
    Relative(RelativeVolume),
}

#[derive(Debug, Copy, Clone)]
pub enum PanCommand {
    Absolute(Pan),
    Relative(RelativePan),
}

fn merge_volumes_commands(v1: Option<VolumeCommand>, v2: VolumeCommand) -> VolumeCommand {
    match (v1, v2) {
        (Some(VolumeCommand::Absolute(v1)), VolumeCommand::Relative(v2)) => {
            let v = v1.as_u8().saturating_add_signed(v2.as_i8());
            VolumeCommand::Absolute(Volume::new(v))
        }
        (Some(VolumeCommand::Relative(v1)), VolumeCommand::Relative(v2)) => {
            let v = v1.as_i8().saturating_add(v2.as_i8());
            VolumeCommand::Relative(RelativeVolume::new(v))
        }
        (Some(_), VolumeCommand::Absolute(_)) => v2,
        (None, v2) => v2,
    }
}

fn merge_pan_commands(p1: Option<PanCommand>, p2: PanCommand) -> PanCommand {
    match (p1, p2) {
        (Some(PanCommand::Absolute(p1)), PanCommand::Relative(p2)) => {
            let p = min(p1.as_u8().saturating_add_signed(p2.as_i8()), Pan::MAX);
            PanCommand::Absolute(p.try_into().unwrap())
        }
        (Some(PanCommand::Relative(p1)), PanCommand::Relative(p2)) => {
            let p: i32 = p1.as_i8().saturating_add(p2.as_i8()).into();
            PanCommand::Relative(p.try_into().unwrap())
        }
        (Some(_), PanCommand::Absolute(_)) => p2,
        (None, p2) => p2,
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct MpVibrato {
    pub depth_in_cents: u32,
    pub quarter_wavelength_ticks: QuarterWavelengthInTicks,
}

#[derive(Copy, Clone, PartialEq)]
pub struct ManualVibrato {
    pub pitch_offset_per_tick: PitchOffsetPerTick,
    pub quarter_wavelength_ticks: QuarterWavelengthInTicks,
}

pub enum MmlCommand {
    NoCommand,

    SetLoopPoint,

    SetManualVibrato(Option<ManualVibrato>),
    SetMpVibrato(Option<MpVibrato>),

    Rest(TickCounter),

    // wait with no keyoff
    Wait(TickCounter),

    PlayNote {
        note: Note,
        length: TickCounter,
        is_slur: bool,
    },
    PlayQuantizedNote {
        note: Note,
        length: TickCounter,
        key_on_length: TickCounter,
        // May be longer the `length - key_on_length`.
        rest: TickCounter,
    },
    Portamento {
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        total_length: TickCounter,
        delay_length: TickCounter,
        tie_length: TickCounter,
    },
    BrokenChord {
        notes: Vec<Note>,
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    },

    CallSubroutine(SubroutineId),
    StartLoop,
    SkipLastLoop,
    EndLoop(LoopCount),

    // index into Vec<ChannelData>.
    SetInstrument(usize),

    ChangePanAndOrVolume(Option<PanCommand>, Option<VolumeCommand>),
    SetEcho(bool),

    SetSongTempo(Bpm),
    SetSongTickClock(TickClock),
}

pub struct MmlCommandWithPos {
    command: MmlCommand,
    pos: FilePosRange,
}

impl MmlCommandWithPos {
    pub fn command(&self) -> &MmlCommand {
        &self.command
    }
    pub fn pos(&self) -> &FilePosRange {
        &self.pos
    }
}

// Parser state that is saved in CursorTracker accessed by the GUI
#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub zenlen: ZenLen,
    pub default_length: MmlDefaultLength,
    pub octave: Octave,
    pub semitone_offset: i8,
    pub quantize: Quantization,
}

mod parser {
    use crate::{
        file_pos::LineIndexRange,
        mml::{line_splitter::MmlLine, ChannelId},
    };

    use super::*;

    pub(crate) struct Parser<'a> {
        #[allow(dead_code)]
        channel: ChannelId,

        tokenizer: PeekingTokenizer<'a>,
        errors: Vec<ErrorWithPos<MmlError>>,
        state: State,

        default_length: TickCounter,

        tick_counter: TickCounterWithLoopFlag,
        sections_tick_counters: Vec<TickCounterWithLoopFlag>,

        pending_sections: Option<&'a [Section]>,

        #[cfg(feature = "mml_tracking")]
        cursor_tracker: &'a mut CursorTracker,
    }

    impl Parser<'_> {
        pub fn new<'a>(
            channel: ChannelId,
            lines: &'a [MmlLine],
            instruments_map: &'a HashMap<IdentifierStr, usize>,
            subroutine_map: Option<&'a HashMap<IdentifierStr, SubroutineId>>,
            zenlen: ZenLen,
            sections: Option<&'a [Section]>,

            #[cfg(feature = "mml_tracking")] cursor_tracking: &'a mut CursorTracker,
        ) -> Parser<'a> {
            // Remove the first section to prevent an off-by-one error
            let (n_sections, pending_sections) = match sections {
                Some(sections) => match sections.split_first() {
                    Some((_, r)) => (r.len(), Some(r)),
                    None => (0, None),
                },
                None => (0, None),
            };

            let mut p = Parser {
                channel,
                tokenizer: PeekingTokenizer::new(lines, instruments_map, subroutine_map),
                errors: Vec::new(),
                state: State {
                    zenlen,
                    default_length: STARTING_MML_LENGTH,
                    octave: STARTING_OCTAVE,
                    semitone_offset: 0,
                    quantize: Quantization(8),
                },

                default_length: zenlen.starting_length(),

                tick_counter: TickCounterWithLoopFlag::default(),
                sections_tick_counters: Vec::with_capacity(n_sections),
                pending_sections,

                #[cfg(feature = "mml_tracking")]
                cursor_tracker: cursor_tracking,
            };
            if let Some(line) = lines.first() {
                p.process_new_line(line.entire_line_range);
            }
            p
        }

        pub(super) fn state(&self) -> &State {
            &self.state
        }

        pub(super) fn set_state(&mut self, new_state: State) {
            self.state = new_state;

            #[cfg(feature = "mml_tracking")]
            self.add_to_cursor_tracker();
        }

        // Must ONLY be called by the parsing macros in this module.
        pub(super) fn __peek(&self) -> &Token {
            self.tokenizer.peek()
        }

        // Must ONLY be called by the parsing macros in this module.
        pub(super) fn __next_token(&mut self) {
            self.tokenizer.next();
        }

        pub fn peek_pos(&self) -> &FilePos {
            self.tokenizer.peek_pos()
        }

        pub(super) fn file_pos_range_from(&self, pos: FilePos) -> FilePosRange {
            pos.to_range_pos(self.tokenizer.prev_end_pos())
        }

        pub(super) fn peek_and_next(&mut self) -> (FilePos, Token) {
            self.tokenizer.peek_and_next()
        }

        pub fn add_error(&mut self, pos: FilePos, e: MmlError) {
            self.errors
                .push(ErrorWithPos(self.file_pos_range_from(pos), e))
        }

        pub fn add_error_range(&mut self, pos: FilePosRange, e: MmlError) {
            self.errors.push(ErrorWithPos(pos, e))
        }

        pub fn set_tick_counter(&mut self, tc: TickCounterWithLoopFlag) {
            self.tick_counter = tc;

            #[cfg(feature = "mml_tracking")]
            self.add_to_cursor_tracker();
        }

        pub(super) fn increment_tick_counter(&mut self, t: TickCounter) {
            self.tick_counter.ticks += t;

            #[cfg(feature = "mml_tracking")]
            self.add_to_cursor_tracker();
        }

        pub(super) fn set_loop_flag(&mut self) {
            self.tick_counter.in_loop = true;

            #[cfg(feature = "mml_tracking")]
            self.add_to_cursor_tracker();
        }

        pub(super) fn set_default_length(&mut self, ticks: TickCounter) {
            self.default_length = ticks;
        }

        pub(super) fn default_length(&self) -> TickCounter {
            self.default_length
        }

        pub(super) fn process_new_line(&mut self, r: LineIndexRange) {
            let _ = r;

            if let Some(pending_sections) = self.pending_sections.as_mut() {
                while pending_sections
                    .first()
                    .is_some_and(|s| self.tokenizer.peek_pos().line_number >= s.line_number)
                {
                    *pending_sections = &pending_sections[1..];
                    self.sections_tick_counters.push(self.tick_counter);
                }
            }

            #[cfg(feature = "mml_tracking")]
            self.cursor_tracker
                .new_line(self.channel, r, self.tick_counter, self.state.clone());
        }

        #[cfg(feature = "mml_tracking")]
        fn add_to_cursor_tracker(&mut self) {
            self.cursor_tracker.add(
                self.tokenizer.prev_end_pos(),
                self.tick_counter,
                self.state.clone(),
            );
        }

        pub fn finalize(mut self) -> (Vec<TickCounterWithLoopFlag>, Vec<ErrorWithPos<MmlError>>) {
            if let Some(pending_sections) = self.pending_sections {
                for _ in pending_sections {
                    self.sections_tick_counters.push(self.tick_counter);
                }
            }

            #[cfg(feature = "mml_tracking")]
            self.cursor_tracker.end_channel();

            (self.sections_tick_counters, self.errors)
        }
    }

    impl Iterator for Parser<'_> {
        type Item = MmlCommandWithPos;

        fn next(&mut self) -> Option<Self::Item> {
            let (pos, token) = self.peek_and_next();

            match token {
                Token::End => None,
                t => Some(MmlCommandWithPos {
                    command: parse_token(pos, t, self),
                    pos: self.file_pos_range_from(pos),
                }),
            }
        }
    }
}
pub(crate) use parser::Parser;

// PARSING MACROS
// ==============
//
// These are the only things allowed to call `Parser::__peek()` and `Parser::__next_token()`.

// Builds a match statement for the tokenizer.
//
// Will consume the token and advance the tokenizer if there is a match
macro_rules! match_next_token {
    // Using `#` to separate patterns from the fallthrough no-matches case.
    ($parser:ident, $($pattern:pat => $expression:expr,)+ #_ => $no_matches:expr) => {
        match $parser.__peek() {
        $(
            $pattern => {
                $parser.__next_token();
                $expression
            }
        )+

            _ => { $no_matches }
        }
    };
}

// Will consume the token and advance the tokenizer if there is a match
macro_rules! next_token_matches {
    ($parser:ident, $pattern:pat) => {
        match $parser.__peek() {
            $pattern => {
                $parser.__next_token();
                true
            }
            _ => false,
        }
    };
}

fn parse_unsigned_newtype<T>(pos: FilePos, p: &mut Parser) -> Option<T>
where
    T: ValueNewType + TryFrom<u32, Error = ValueError>,
{
    match_next_token!(
        p,

        &Token::Number(n) => {
            match n.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    )
}

fn parse_signed_newtype<T>(pos: FilePos, p: &mut Parser) -> Option<T>
where
    T: ValueNewType + TryFrom<i32, Error = ValueError>,
{
    match_next_token!(
        p,

        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    )
}

fn parse_set_default_length(pos: FilePos, p: &mut Parser) {
    let length_in_ticks = next_token_matches!(p, Token::PercentSign);
    let length = match_next_token!(
        p,
        &Token::Number(n) => match n.try_into() {
            Ok(n) => n,
            Err(_) => {
                p.add_error(pos, ValueError::InvalidDefaultLength.into());
                return;
            }
        },
        #_ => {
            p.add_error(pos, ValueError::MissingDefaultLength.into());
            return;
        }
    );
    let mut number_of_dots = 0;
    while next_token_matches!(p, Token::Dot) {
        number_of_dots += 1;
    }

    let default_length = MmlDefaultLength::new(length, length_in_ticks, number_of_dots);

    match default_length.to_tick_count(p.state().zenlen) {
        Ok(tc) => {
            p.set_default_length(tc);
            p.set_state(State {
                default_length,
                ..p.state().clone()
            });
        }
        Err(e) => {
            p.add_error(pos, e.into());
        }
    }
}

fn parse_change_whole_note_length(pos: FilePos, p: &mut Parser) {
    if let Some(new_zenlen) = parse_unsigned_newtype(pos, p) {
        let state = p.state().clone();

        let tc = match state.default_length.to_tick_count(new_zenlen) {
            Ok(tc) => tc,
            Err(e) => {
                p.add_error(pos, e.into());
                p.default_length()
            }
        };
        p.set_default_length(tc);
        p.set_state(State {
            zenlen: new_zenlen,
            ..p.state().clone()
        });
    }
}

fn parse_set_octave(pos: FilePos, p: &mut Parser) {
    if let Some(o) = parse_unsigned_newtype(pos, p) {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        });
    }
}

fn parse_increment_octave(p: &mut Parser) {
    let mut o = p.state().octave;
    o.saturating_increment();

    if o != p.state().octave {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        })
    }
}

fn parse_decrement_octave(p: &mut Parser) {
    let mut o = p.state().octave;
    o.saturating_decrement();

    if o != p.state().octave {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        })
    }
}

fn parse_transpose(pos: FilePos, p: &mut Parser) {
    if let Some(t) = parse_signed_newtype(pos, p) {
        let t: Transpose = t;
        p.set_state(State {
            semitone_offset: t.as_i8(),
            ..p.state().clone()
        });
    }
}

fn parse_relative_transpose(pos: FilePos, p: &mut Parser) {
    if let Some(rt) = parse_signed_newtype(pos, p) {
        let rt: Transpose = rt;

        let state = p.state().clone();
        p.set_state(State {
            semitone_offset: state.semitone_offset.saturating_add(rt.as_i8()),
            ..state
        });
    }
}

fn parse_quantize(pos: FilePos, p: &mut Parser) {
    if let Some(q) = parse_unsigned_newtype(pos, p) {
        p.set_state(State {
            quantize: q,
            ..p.state().clone()
        });
    }
}

// Returns true if the token was recognised and processed
fn merge_state_change(p: &mut Parser) -> bool {
    let pos = *p.peek_pos();

    match_next_token!(
        p,

        &Token::NewLine(r) => {
            p.process_new_line(r);
            true
        },

        Token::SetOctave => {
            parse_set_octave(pos, p);
            true
        },
        Token::IncrementOctave => {
            parse_increment_octave(p);
            true
        },
        Token::DecrementOctave => {
            parse_decrement_octave(p);
            true
        },
        Token::Transpose => {
            parse_transpose(pos, p);
            true
        },
        Token::RelativeTranspose => {
            parse_relative_transpose(pos, p);
            true
        },

        Token::SetDefaultLength => {
            parse_set_default_length(pos, p);
            true
        },
        Token::ChangeWholeNoteLength => {
            parse_change_whole_note_length(pos, p);
            true
        },
        Token::Divider => {
            true
        },
        #_ => false
    )
}

fn parse_tracked_length(p: &mut Parser) -> TickCounter {
    let pos = *p.peek_pos();

    let length_in_ticks = next_token_matches!(p, Token::PercentSign);

    let length = match_next_token!(
        p,
        &Token::Number(n) => Some(n),
        #_ => None
    );

    let mut number_of_dots = 0;
    while next_token_matches!(p, Token::Dot) {
        number_of_dots += 1;
    }

    let note_length = MmlLength::new(length, length_in_ticks, number_of_dots);

    let length = match note_length.to_tick_count(p.default_length(), p.state().zenlen) {
        Ok(tc) => tc,
        Err(e) => {
            p.add_error(pos, e.into());
            p.default_length()
        }
    };

    p.increment_tick_counter(length);

    length
}

fn parse_tracked_optional_length(p: &mut Parser) -> Option<TickCounter> {
    let length = parse_untracked_optional_length(p);
    if let Some(tc) = length {
        p.increment_tick_counter(tc)
    }
    length
}

fn parse_untracked_optional_mml_length(p: &mut Parser) -> Option<MmlLength> {
    let length_in_ticks = next_token_matches!(p, Token::PercentSign);

    let length = match_next_token!(
        p,
        &Token::Number(n) => Some(n),
        #_ => None
    );

    let mut number_of_dots = 0;
    while next_token_matches!(p, Token::Dot) {
        number_of_dots += 1;
    }

    if length_in_ticks || length.is_some() || number_of_dots > 0 {
        Some(MmlLength::new(length, length_in_ticks, number_of_dots))
    } else {
        None
    }
}

fn parse_untracked_optional_length(p: &mut Parser) -> Option<TickCounter> {
    let pos = *p.peek_pos();

    let note_length = parse_untracked_optional_mml_length(p)?;
    let tc = match note_length.to_tick_count(p.default_length(), p.state().zenlen) {
        Ok(tc) => tc,
        Err(e) => {
            p.add_error(pos, e.into());
            p.default_length()
        }
    };
    Some(tc)
}

fn parse_pitch_list(p: &mut Parser) -> (Vec<Note>, FilePos, Token) {
    let mut out = Vec::new();

    loop {
        let (pos, token) = p.peek_and_next();

        match token {
            Token::EndPortamento | Token::EndBrokenChord => {
                return (out, pos, token);
            }
            Token::NewLine(r) => {
                p.process_new_line(r);
                return (out, pos, token);
            }
            Token::End => {
                return (out, pos, token);
            }

            Token::Pitch(pitch) => {
                match Note::from_mml_pitch(pitch, p.state().octave, p.state().semitone_offset) {
                    Ok(note) => out.push(note),
                    Err(e) => p.add_error(pos, e.into()),
                }
            }

            Token::SetOctave => parse_set_octave(pos, p),
            Token::IncrementOctave => parse_increment_octave(p),
            Token::DecrementOctave => parse_decrement_octave(p),
            Token::Transpose => parse_transpose(pos, p),
            Token::RelativeTranspose => parse_relative_transpose(pos, p),

            _ => p.add_error(pos, MmlError::InvalidPitchListSymbol),
        }
    }
}

fn parse_pan_value(pos: FilePos, p: &mut Parser) -> Option<PanCommand> {
    match_next_token!(
        p,

        &Token::Number(n) => {
            match n.try_into() {
                Ok(v) => Some(PanCommand::Absolute(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(v) => Some(PanCommand::Relative(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        #_ => None
    )
}

fn parse_fine_volume_value(pos: FilePos, p: &mut Parser) -> Option<VolumeCommand> {
    match_next_token!(
        p,

        &Token::Number(n) => {
            match n.try_into() {
                Ok(v) => Some(VolumeCommand::Absolute(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(v) => Some(VolumeCommand::Relative(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        #_ => None
    )
}

fn parse_coarse_volume_value(pos: FilePos, p: &mut Parser) -> Option<VolumeCommand> {
    match_next_token!(
        p,

        &Token::Number(v) => {
            if v <= MAX_COARSE_VOLUME {
                let v = u8::try_from(v).unwrap();
                let v = v.saturating_mul(COARSE_VOLUME_MULTIPLIER);
                Some(VolumeCommand::Absolute(Volume::new(v)))
            }
            else {
                p.add_error(pos, ValueError::CoarseVolumeOutOfRange.into());
                None
            }
        },
        &Token::RelativeNumber(n) => {
            match n.saturating_mul(COARSE_VOLUME_MULTIPLIER.into()).try_into() {
                Ok(v) => Some(VolumeCommand::Relative(v)),
                Err(_) => {
                    p.add_error(pos, ValueError::RelativeCoarseVolumeOutOfRange.into());
                    None
                }
            }
        },
        #_ => None
    )
}

fn merge_pan_or_volume(
    pan: Option<PanCommand>,
    volume: Option<VolumeCommand>,
    p: &mut Parser,
) -> MmlCommand {
    let mut pan = pan;
    let mut volume = volume;

    loop {
        let pos = *p.peek_pos();

        match_next_token!(
            p,

            Token::CoarseVolume => {
                if let Some(v) = parse_coarse_volume_value(pos, p) {
                    volume = Some(merge_volumes_commands(volume, v));
                }
            },
            Token::FineVolume => {
                if let Some(v) = parse_fine_volume_value(pos, p) {
                    volume = Some(merge_volumes_commands(volume, v));
                }
            },
            Token::Pan => {
                if let Some(new_pan) = parse_pan_value(pos, p) {
                    pan = Some(merge_pan_commands(pan, new_pan));
                }
            },

            #_ => {
                if !merge_state_change(p) {
                    return MmlCommand::ChangePanAndOrVolume(pan, volume);
                }
            }
        )
    }
}

// Assumes the current command has a rest.
fn merge_rest_tokens(p: &mut Parser, rest_tc: TickCounter) -> TickCounter {
    let mut rest_tc = rest_tc;

    loop {
        match_next_token!(
            p,

            Token::Rest => {
                rest_tc += parse_tracked_length(p);
            },
            Token::Tie => {
                rest_tc += parse_tracked_length(p);
            },
            #_ => {
                if !merge_state_change(p) {
                    return rest_tc;
                }
            }
        )
    }
}

fn parse_ties_and_slur(p: &mut Parser) -> (TickCounter, bool) {
    let mut tie_length = TickCounter::new(0);

    loop {
        match_next_token!(
            p,

            Token::Tie => {
                tie_length += parse_tracked_length(p);
            },
            Token::Slur => {
                // Slur or tie
                if let Some(tc) = parse_tracked_optional_length(p) {
                    tie_length += tc;
                } else {
                    // slur
                    return (tie_length, true);
                }
            },

            #_ => {
                if !merge_state_change(p) {
                    return (tie_length, false);
                }
            }
        )
    }
}

fn parse_wait(p: &mut Parser) -> MmlCommand {
    let mut ticks = parse_tracked_length(p);

    // Merge `w` wait and `^` tie commands
    loop {
        match_next_token!(
            p,

            Token::Wait => {
                ticks += parse_tracked_length(p);
            },
            Token::Tie => {
                ticks += parse_tracked_length(p);
            },
            #_ => {
                if !merge_state_change(p) {
                    return MmlCommand::Wait(ticks);
                }
            }
        )
    }
}

fn parse_rest(p: &mut Parser) -> MmlCommand {
    let tc = parse_tracked_length(p);
    let tc = merge_rest_tokens(p, tc);

    MmlCommand::Rest(tc)
}

fn play_note(note: Note, length: TickCounter, p: &mut Parser) -> MmlCommand {
    let (tie_length, is_slur) = parse_ties_and_slur(p);
    let length = length + tie_length;

    let q = p.state().quantize.as_u8();
    const MAX_Q: u8 = Quantization::MAX;

    if q >= MAX_Q || is_slur {
        MmlCommand::PlayNote {
            note,
            length,
            is_slur,
        }
    } else {
        // Note is quantized
        let q = u32::from(q);
        let l = length.value();
        let key_on_length = (l * q) / u32::from(MAX_Q) + KEY_OFF_TICK_DELAY;
        let key_off_length = l - key_on_length;

        if key_on_length > KEY_OFF_TICK_DELAY && key_off_length > KEY_OFF_TICK_DELAY {
            let key_on_length = TickCounter::new(key_on_length);
            let rest = TickCounter::new(key_off_length);

            let rest = merge_rest_tokens(p, rest);

            MmlCommand::PlayQuantizedNote {
                note,
                length,
                key_on_length,
                rest,
            }
        } else {
            // Note is too short to be quantized
            MmlCommand::PlayNote {
                note,
                length,
                is_slur,
            }
        }
    }
}

fn parse_pitch(pos: FilePos, pitch: MmlPitch, p: &mut Parser) -> MmlCommand {
    match Note::from_mml_pitch(pitch, p.state().octave, p.state().semitone_offset) {
        Ok(note) => {
            let length = parse_tracked_length(p);
            play_note(note, length, p)
        }
        Err(e) => {
            p.add_error(pos, e.into());

            // Output a rest (so tick-counter is correct)
            let length = parse_tracked_length(p);
            let (tie_length, _) = parse_ties_and_slur(p);
            let length = length + tie_length;
            MmlCommand::Rest(length)
        }
    }
}

fn parse_play_midi_note_number(pos: FilePos, p: &mut Parser) -> MmlCommand {
    let note = match parse_unsigned_newtype::<MidiNote>(pos, p) {
        Some(n) => match n.try_into() {
            Ok(n) => Some(n),
            Err(e) => {
                let e: ValueError = e;
                p.add_error(pos, e.into());
                None
            }
        },
        None => None,
    };

    match note {
        Some(note) => {
            let length = parse_tracked_length(p);
            play_note(note, length, p)
        }
        None => {
            // Output a rest (so tick-counter is correct)
            let length = parse_tracked_length(p);
            let (tie_length, _) = parse_ties_and_slur(p);
            let length = length + tie_length;
            MmlCommand::Rest(length)
        }
    }
}

fn parse_portamento(pos: FilePos, p: &mut Parser) -> MmlCommand {
    let (notes, end_pos, end_token) = parse_pitch_list(p);

    if !matches!(end_token, Token::EndPortamento) {
        return invalid_token_error(p, end_pos, MmlError::MissingEndPortamento);
    }

    if notes.len() != 2 {
        p.add_error(pos, MmlError::PortamentoRequiresTwoPitches);
    }

    let total_length = parse_tracked_length(p);

    let mut delay_length = TickCounter::new(0);
    let mut speed_override = None;

    if next_token_matches!(p, Token::Comma) {
        let dt_pos = *p.peek_pos();
        if let Some(dt) = parse_untracked_optional_length(p) {
            if dt < total_length {
                delay_length = dt;
            } else {
                p.add_error(dt_pos, MmlError::InvalidPortamentoDelay);
            }
        }
        if next_token_matches!(p, Token::Comma) {
            speed_override = parse_unsigned_newtype(*p.peek_pos(), p);
        }
    }

    let (tie_length, is_slur) = parse_ties_and_slur(p);

    if notes.len() == 2 {
        MmlCommand::Portamento {
            note1: notes[0],
            note2: notes[1],
            is_slur,
            speed_override,
            total_length,
            delay_length,
            tie_length,
        }
    } else {
        MmlCommand::NoCommand
    }
}

fn parse_broken_chord(p: &mut Parser) -> MmlCommand {
    let (notes, end_pos, end_token) = parse_pitch_list(p);

    if !matches!(end_token, Token::EndBrokenChord) {
        return invalid_token_error(p, end_pos, MmlError::MissingEndBrokenChord);
    }

    let mut note_length_pos = end_pos;

    let total_length = parse_tracked_length(p);
    let mut tie = true;
    let mut note_length = None;

    if next_token_matches!(p, Token::Comma) {
        note_length_pos = *p.peek_pos();
        note_length = parse_untracked_optional_length(p);

        if next_token_matches!(p, Token::Comma) {
            let tie_pos = *p.peek_pos();
            match_next_token!(
                p,
                Token::Number(0) => tie = false,
                Token::Number(1) => tie = true,
                #_ => p.add_error(tie_pos, ValueError::NoBool.into())
            )
        }
    }

    let note_length = match note_length {
        Some(nl) => match PlayNoteTicks::try_from_is_slur(nl.value(), tie) {
            Ok(t) => t,
            Err(e) => {
                p.add_error(note_length_pos, e.into());
                PlayNoteTicks::min_for_is_slur(tie)
            }
        },
        None => PlayNoteTicks::min_for_is_slur(tie),
    };

    MmlCommand::BrokenChord {
        notes,
        total_length,
        note_length,
    }
}

fn parse_mp_vibrato(pos: FilePos, p: &mut Parser) -> Option<MpVibrato> {
    match_next_token!(
        p,

        &Token::Number(0) => {
            // Disable MP Vibrato
            None
        },
        &Token::Number(depth_in_cents) => {
            if next_token_matches!(p, Token::Comma) {
                parse_unsigned_newtype(pos, p).map(|qwt| MpVibrato {
                    depth_in_cents,
                    quarter_wavelength_ticks: qwt,
                })
            } else {
                p.add_error(pos, ValueError::NoCommaQuarterWavelength.into());
                None
            }
        },
        #_ => {
            p.add_error(pos, ValueError::NoMpDepth.into());
            None
        }
    )
}

fn parse_manual_vibrato(pos: FilePos, p: &mut Parser) -> Option<ManualVibrato> {
    let pitch_offset_per_tick: PitchOffsetPerTick = match parse_unsigned_newtype(pos, p) {
        Some(v) => v,
        None => return None,
    };

    if pitch_offset_per_tick.as_u8() == 0 {
        // Disable MP Vibrato
        None
    } else if next_token_matches!(p, Token::Comma) {
        parse_unsigned_newtype(pos, p).map(|qwt| ManualVibrato {
            pitch_offset_per_tick,
            quarter_wavelength_ticks: qwt,
        })
    } else {
        p.add_error(pos, ValueError::NoCommaQuarterWavelength.into());
        None
    }
}

fn parse_echo(pos: FilePos, p: &mut Parser) -> MmlCommand {
    match_next_token!(p,
        Token::Number(0) => MmlCommand::SetEcho(false),
        Token::Number(1) => MmlCommand::SetEcho(true),
        Token::Number(_) => invalid_token_error(p, pos, ValueError::NoBool.into()),
        #_ => MmlCommand::SetEcho(true)
    )
}

fn invalid_token_error(p: &mut Parser, pos: FilePos, e: MmlError) -> MmlCommand {
    p.add_error(pos, e);
    MmlCommand::NoCommand
}

fn parse_token(pos: FilePos, token: Token, p: &mut Parser) -> MmlCommand {
    match token {
        Token::End => MmlCommand::NoCommand,

        Token::NewLine(r) => {
            p.process_new_line(r);
            MmlCommand::NoCommand
        }

        Token::Pitch(pitch) => parse_pitch(pos, pitch, p),
        Token::PlayMidiNoteNumber => parse_play_midi_note_number(pos, p),
        Token::Rest => parse_rest(p),
        Token::Wait => parse_wait(p),
        Token::StartPortamento => parse_portamento(pos, p),
        Token::StartBrokenChord => parse_broken_chord(p),

        Token::MpVibrato => MmlCommand::SetMpVibrato(parse_mp_vibrato(pos, p)),
        Token::ManualVibrato => MmlCommand::SetManualVibrato(parse_manual_vibrato(pos, p)),

        Token::SetInstrument(inst) => MmlCommand::SetInstrument(inst),

        Token::CallSubroutine(id) => {
            p.increment_tick_counter(id.tick_counter());
            MmlCommand::CallSubroutine(id)
        }

        Token::StartLoop => {
            p.set_loop_flag();
            MmlCommand::StartLoop
        }
        Token::SkipLastLoop => MmlCommand::SkipLastLoop,
        Token::EndLoop => {
            let lc = parse_unsigned_newtype(pos, p).unwrap_or(LoopCount::MIN_LOOPCOUNT);
            MmlCommand::EndLoop(lc)
        }

        Token::SetLoopPoint => MmlCommand::SetLoopPoint,

        Token::Echo => parse_echo(pos, p),

        Token::SetSongTempo => match parse_unsigned_newtype(pos, p) {
            Some(t) => MmlCommand::SetSongTempo(t),
            None => MmlCommand::NoCommand,
        },
        Token::SetSongTickClock => match parse_unsigned_newtype(pos, p) {
            Some(t) => MmlCommand::SetSongTickClock(t),
            None => MmlCommand::NoCommand,
        },

        Token::CoarseVolume => {
            let v = parse_coarse_volume_value(pos, p);
            merge_pan_or_volume(None, v, p)
        }
        Token::FineVolume => {
            let v = parse_fine_volume_value(pos, p);
            merge_pan_or_volume(None, v, p)
        }
        Token::Pan => {
            let pan = parse_pan_value(pos, p);
            merge_pan_or_volume(pan, None, p)
        }

        Token::Quantize => {
            parse_quantize(pos, p);
            MmlCommand::NoCommand
        }
        Token::SetDefaultLength => {
            parse_set_default_length(pos, p);
            MmlCommand::NoCommand
        }
        Token::SetOctave => {
            parse_set_octave(pos, p);
            MmlCommand::NoCommand
        }
        Token::IncrementOctave => {
            parse_increment_octave(p);
            MmlCommand::NoCommand
        }
        Token::DecrementOctave => {
            parse_decrement_octave(p);
            MmlCommand::NoCommand
        }
        Token::Transpose => {
            parse_transpose(pos, p);
            MmlCommand::NoCommand
        }
        Token::RelativeTranspose => {
            parse_relative_transpose(pos, p);
            MmlCommand::NoCommand
        }
        Token::ChangeWholeNoteLength => {
            parse_change_whole_note_length(pos, p);
            MmlCommand::NoCommand
        }
        Token::Divider => MmlCommand::NoCommand,

        Token::EndPortamento => invalid_token_error(p, pos, MmlError::NoStartPortamento),
        Token::EndBrokenChord => invalid_token_error(p, pos, MmlError::NoStartBrokenChord),

        Token::Tie => invalid_token_error(p, pos, MmlError::MissingNoteBeforeTie),
        Token::Slur => invalid_token_error(p, pos, MmlError::MissingNoteBeforeTie),
        Token::Comma => invalid_token_error(p, pos, MmlError::CannotParseComma),
        Token::Dot => invalid_token_error(p, pos, MmlError::CannotParseDot),
        Token::PercentSign => invalid_token_error(p, pos, MmlError::CannotParsePercentSign),
        Token::Number(_) => invalid_token_error(p, pos, MmlError::UnexpectedNumber),
        Token::RelativeNumber(_) => invalid_token_error(p, pos, MmlError::UnexpectedNumber),

        Token::Error(e) => invalid_token_error(p, pos, e),
    }
}
