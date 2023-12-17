//! MML bytecode generator

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::command_parser::{
    ManualVibrato, MmlCommand, MpVibrato, PanCommand, Parser, PortamentoSpeed, VolumeCommand,
};
use super::identifier::Identifier;
use super::instruments::MmlInstrument;
use super::line_splitter::MmlLine;
use super::{ChannelId, IdentifierStr, MmlSoundEffect, Section};

#[cfg(feature = "mml_tracking")]
use super::note_tracking::CursorTracker;
use crate::envelope::Envelope;
#[cfg(feature = "mml_tracking")]
use crate::songs::{BytecodePos, SongBcTracking};

use crate::bytecode::{
    BcTerminator, BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, BytecodeContext, LoopCount,
    PitchOffsetPerTick, PlayNoteTicks, PortamentoVelocity, SubroutineId, MAX_NESTED_LOOPS,
};
use crate::errors::{ErrorWithPos, MmlChannelError, MmlError, ValueError};
use crate::notes::{Note, SEMITONES_PER_OCTAVE};
use crate::pitch_table::PitchTable;
use crate::songs::{Channel, LoopPoint, Subroutine};
use crate::time::{TickClock, TickCounter, ZenLen, DEFAULT_ZENLEN};

use std::cmp::max;
use std::collections::HashMap;

pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

// Number of rest instructions before a loop uses less space
pub const REST_LOOP_INSTRUCTION_THREASHOLD: u32 = 3;

struct RestLoop {
    ticks_in_loop: u32,
    n_loops: u32,

    // May be 0
    remainder: u32,

    n_rest_instructions: u32,
}

const fn div_ceiling(a: u32, b: u32) -> u32 {
    (a + b - 1) / b
}

#[inline]
fn build_rest_loop<Loop, Rem, const ALLOW_ZERO_REM: bool>(ticks: TickCounter) -> RestLoop
where
    Loop: BcTicks,
    Rem: BcTicks,
{
    let ticks = ticks.value();

    assert!(ticks > Loop::MAX * 3);

    (LoopCount::MIN..=LoopCount::MAX)
        .filter_map(|l| {
            let div = ticks / l;
            let rem = ticks % l;

            if div >= Loop::MIN && ((ALLOW_ZERO_REM && rem == 0) || rem >= Rem::MIN) {
                Some(RestLoop {
                    ticks_in_loop: div,
                    n_loops: l,
                    remainder: rem,

                    n_rest_instructions: div_ceiling(div, Loop::MAX) + div_ceiling(rem, Rem::MAX),
                })
            } else {
                None
            }
        })
        .min_by_key(|rl| rl.n_rest_instructions)
        .unwrap()
}

#[derive(Clone, PartialEq)]
enum MpState {
    Disabled,
    Manual,
    Mp(MpVibrato),
}

struct SkipLastLoopState {
    instrument: Option<usize>,
    envelope: Option<Envelope>,
    prev_slurred_note: Option<Note>,
    vibrato: Option<ManualVibrato>,
}

struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    instruments: &'a Vec<MmlInstrument>,
    subroutines: Option<&'a Vec<Subroutine>>,

    bc: Bytecode,

    tempo_changes: Vec<(TickCounter, TickClock)>,

    instrument: Option<usize>,
    envelope: Option<Envelope>,

    prev_slurred_note: Option<Note>,

    mp: MpState,
    vibrato: Option<ManualVibrato>,

    skip_last_loop_state: Vec<Option<SkipLastLoopState>>,

    loop_point: Option<LoopPoint>,

    show_missing_set_instrument_error: bool,
}

impl ChannelBcGenerator<'_> {
    fn new<'a>(
        bc_data: Vec<u8>,
        pitch_table: &'a PitchTable,
        instruments: &'a Vec<MmlInstrument>,
        subroutines: Option<&'a Vec<Subroutine>>,
        context: BytecodeContext,
    ) -> ChannelBcGenerator<'a> {
        let is_subroutine = matches!(context, BytecodeContext::SongSubroutine);

        ChannelBcGenerator {
            pitch_table,
            instruments,
            subroutines,
            bc: Bytecode::new_append_to_vec(bc_data, context),
            tempo_changes: Vec::new(),
            instrument: None,
            envelope: None,
            prev_slurred_note: None,
            mp: MpState::Disabled,
            vibrato: None,
            skip_last_loop_state: Vec::new(),
            loop_point: None,
            show_missing_set_instrument_error: !is_subroutine,
        }
    }

    fn can_loop(&self) -> bool {
        self.bc.get_loop_stack_len() < MAX_NESTED_LOOPS.into()
    }

    fn instrument_from_index(&self, i: usize) -> &MmlInstrument {
        // `i` should always be valid
        match self.instruments.get(i) {
            Some(inst) => inst,
            None => panic!("invalid instrument index"),
        }
    }

    fn test_note(&mut self, note: Note) -> Result<(), MmlError> {
        match self.instrument {
            Some(i) => {
                let inst = self.instrument_from_index(i);
                if note >= inst.first_note && note <= inst.last_note {
                    Ok(())
                } else {
                    Err(MmlError::NoteOutOfRange(
                        note,
                        inst.first_note,
                        inst.last_note,
                    ))
                }
            }
            None => {
                if self.show_missing_set_instrument_error {
                    self.show_missing_set_instrument_error = false;
                    Err(MmlError::CannotPlayNoteBeforeSettingInstrument)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn calculate_vibrato_for_note(
        &self,
        mp: &MpVibrato,
        note: Note,
    ) -> Result<ManualVibrato, MmlError> {
        if mp.depth_in_cents == 0 {
            return Err(MmlError::MpDepthZero);
        }
        let inst = match self.instrument {
            Some(index) => self.instrument_from_index(index),
            None => return Err(MmlError::CannotUseMpWithoutInstrument),
        };

        let pitch = self.pitch_table.pitch_for_note(inst.instrument_id, note);

        // Calculate the minimum and maximum pitches of the vibrato.
        // This produces more accurate results when cents is very large (ie, 400)
        let pow = f64::from(mp.depth_in_cents) / f64::from(SEMITONES_PER_OCTAVE as u32 * 100);
        let p1 = f64::from(pitch) * 2.0_f64.powf(-pow);
        let p2 = f64::from(pitch) * 2.0_f64.powf(pow);

        let qwt = u32::from(mp.quarter_wavelength_ticks.as_u8());

        let po_per_tick = f64::round((p2 - p1) / f64::from(qwt * 2));
        let po_per_tick = if po_per_tick > 1.0 { po_per_tick } else { 1.0 };

        if po_per_tick > u32::MAX.into() {
            return Err(MmlError::MpPitchOffsetTooLarge(u32::MAX));
        }
        let po_per_tick = po_per_tick as u32;

        match po_per_tick.try_into() {
            Ok(po) => Ok(ManualVibrato {
                quarter_wavelength_ticks: mp.quarter_wavelength_ticks,
                pitch_offset_per_tick: po,
            }),
            Err(_) => Err(MmlError::MpPitchOffsetTooLarge(po_per_tick)),
        }
    }

    fn split_play_note_length(
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(PlayNoteTicks, TickCounter), ValueError> {
        let l = length.value();

        if !is_slur && l <= BcTicksKeyOff::MAX {
            return Ok((
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(l)?),
                TickCounter::new(0),
            ));
        }

        // The play_note instruction requires keyoff.
        let last_min = if is_slur {
            BcTicksNoKeyOff::MIN
        } else {
            BcTicksKeyOff::MIN
        };
        const MAX: u32 = BcTicksNoKeyOff::MAX;

        let pn = {
            if l <= MAX {
                l
            } else if l >= MAX + last_min {
                MAX
            } else {
                MAX - 1
            }
        };

        Ok((
            PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(pn)?),
            TickCounter::new(l - pn),
        ))
    }

    fn play_note_with_mp(
        &mut self,
        note: Note,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), MmlError> {
        let (pn_length, rest) = Self::split_play_note_length(length, is_slur)?;

        self.test_note(note)?;

        self.prev_slurred_note = if is_slur { Some(note) } else { None };

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_length);
            }
            MpState::Disabled => {
                const POPT: PitchOffsetPerTick = PitchOffsetPerTick::new(0);

                let vibrato_disabled = match self.vibrato {
                    None => true,
                    Some(v) => v.pitch_offset_per_tick == POPT,
                };

                if vibrato_disabled {
                    self.bc.play_note(note, pn_length);
                } else {
                    self.bc
                        .set_vibrato_depth_and_play_note(POPT, note, pn_length);

                    if let Some(v) = &mut self.vibrato {
                        v.pitch_offset_per_tick = POPT;
                    }
                }
            }
            MpState::Mp(mp) => {
                let cv = self.calculate_vibrato_for_note(mp, note)?;

                if self.vibrato == Some(cv) {
                    self.bc.play_note(note, pn_length);
                } else {
                    match self.vibrato {
                        Some(sv) if sv.quarter_wavelength_ticks == cv.quarter_wavelength_ticks => {
                            self.bc.set_vibrato_depth_and_play_note(
                                cv.pitch_offset_per_tick,
                                note,
                                pn_length,
                            );
                        }
                        _ => {
                            self.bc
                                .set_vibrato(cv.pitch_offset_per_tick, cv.quarter_wavelength_ticks);
                            self.bc.play_note(note, pn_length);
                        }
                    }

                    self.vibrato = Some(cv);
                }
            }
        }

        self.rest_after_play_note(rest, is_slur)
    }

    fn rest_after_play_note(&mut self, length: TickCounter, is_slur: bool) -> Result<(), MmlError> {
        if length.is_zero() {
            Ok(())
        } else if is_slur {
            // no keyoff event
            self.wait(length)
        } else {
            self.rest_one_keyoff(length)
        }
    }

    // A rest that sends a single keyoff event
    fn rest_one_keyoff_no_loop(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MAX_REST: u32 = BcTicksNoKeyOff::MAX;
        const MAX_FINAL_REST: u32 = BcTicksKeyOff::MAX;
        const MIN_FINAL_REST: u32 = BcTicksKeyOff::MIN;
        const _: () = assert!(MIN_FINAL_REST > 1);

        let mut remaining_ticks = length.value();

        while remaining_ticks > MAX_FINAL_REST {
            let l = if remaining_ticks >= MAX_REST + MIN_FINAL_REST {
                MAX_REST
            } else {
                MAX_REST - 1
            };
            self.bc.rest(BcTicksNoKeyOff::try_from(l).unwrap());
            remaining_ticks -= l;
        }

        self.bc
            .rest_keyoff(BcTicksKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    // Rest that can send multiple `rest_keyoff` instructions
    // The `rest_keyoff` instruction can wait for more ticks than the `rest` instruction.
    fn rest_many_keyoffs_no_loop(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const _: () = assert!(BcTicksKeyOff::MAX > BcTicksNoKeyOff::MAX);
        const MAX_REST: u32 = BcTicksKeyOff::MAX;
        const MIN_REST: u32 = BcTicksKeyOff::MIN;
        const _: () = assert!(MIN_REST > 1);

        let mut remaining_ticks = length.value();

        while remaining_ticks > MAX_REST {
            let l = if remaining_ticks >= MAX_REST + MIN_REST {
                MAX_REST
            } else {
                MAX_REST - 1
            };
            self.bc.rest_keyoff(BcTicksKeyOff::try_from(l).unwrap());
            remaining_ticks -= l;
        }

        self.bc
            .rest_keyoff(BcTicksKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    // rest with no keyoff
    fn wait_no_loop(&mut self, length: TickCounter) -> Result<(), MmlError> {
        let mut remaining_ticks = length.value();

        let rest_length = BcTicksNoKeyOff::try_from(BcTicksNoKeyOff::MAX).unwrap();
        const _: () = assert!(BcTicksNoKeyOff::MIN == 1);

        while remaining_ticks > rest_length.ticks() {
            self.bc.rest(rest_length);
            remaining_ticks -= rest_length.ticks();
        }

        self.bc.rest(BcTicksNoKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    // A rest that sends a single keyoff event
    fn rest_one_keyoff(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MIN_LOOP_REST: u32 =
            BcTicksNoKeyOff::MAX * (REST_LOOP_INSTRUCTION_THREASHOLD - 1) + BcTicksKeyOff::MAX + 1;

        if length.is_zero() {
            return Ok(());
        }
        self.prev_slurred_note = None;

        if length.value() < MIN_LOOP_REST || !self.can_loop() {
            self.rest_one_keyoff_no_loop(length)
        } else {
            // Convert a long rest to a rest loop.

            let rl = build_rest_loop::<BcTicksNoKeyOff, BcTicksKeyOff, false>(length);

            self.bc.start_loop(Some(LoopCount::try_from(rl.n_loops)?))?;
            self.wait_no_loop(TickCounter::new(rl.ticks_in_loop))?;
            self.bc.end_loop(None)?;

            self.bc.rest_keyoff(rl.remainder.try_into()?);

            Ok(())
        }
    }

    // Rest that can send multiple `rest_keyoff` instructions
    // The `rest_keyoff` instruction can wait for more ticks than the `rest` instruction.
    fn rest_many_keyoffs(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MIN_LOOP_REST: u32 = BcTicksKeyOff::MAX * REST_LOOP_INSTRUCTION_THREASHOLD + 1;

        if length.is_zero() {
            return Ok(());
        }
        self.prev_slurred_note = None;

        if length.value() < MIN_LOOP_REST || !self.can_loop() {
            self.rest_many_keyoffs_no_loop(length)
        } else {
            // Convert a long rest to a rest loop.

            let rl = build_rest_loop::<BcTicksKeyOff, BcTicksKeyOff, true>(length);

            self.bc.start_loop(Some(LoopCount::try_from(rl.n_loops)?))?;
            self.rest_many_keyoffs_no_loop(TickCounter::new(rl.ticks_in_loop))?;
            self.bc.end_loop(None)?;

            if rl.remainder > 0 {
                self.bc.rest_keyoff(rl.remainder.try_into()?);
            }

            Ok(())
        }
    }

    // rest with no keyoff
    fn wait(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MIN_LOOP_REST: u32 = BcTicksNoKeyOff::MAX * REST_LOOP_INSTRUCTION_THREASHOLD + 1;

        if length.value() < MIN_LOOP_REST || !self.can_loop() {
            self.wait_no_loop(length)
        } else {
            // Convert a long rest to a rest loop.

            let rl = build_rest_loop::<BcTicksNoKeyOff, BcTicksNoKeyOff, true>(length);

            self.bc.start_loop(Some(LoopCount::try_from(rl.n_loops)?))?;
            self.wait_no_loop(TickCounter::new(rl.ticks_in_loop))?;
            self.bc.end_loop(None)?;

            if rl.remainder > 0 {
                self.bc.rest(rl.remainder.try_into()?);
            }

            Ok(())
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn portamento(
        &mut self,
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        total_length: TickCounter,
        delay_length: TickCounter,
        tie_length: TickCounter,
    ) -> Result<(), MmlError> {
        assert!(delay_length < total_length);

        self.test_note(note1)?;
        self.test_note(note2)?;

        // Play note1 (if required)
        let note1_length = {
            if self.prev_slurred_note != Some(note1) {
                let note_1_length = max(TickCounter::new(1), delay_length);
                let (pn_length, rest) = Self::split_play_note_length(note_1_length, true)?;

                self.bc.play_note(note1, pn_length);
                self.rest_after_play_note(rest, true)?;
                note_1_length
            } else if !delay_length.is_zero() {
                self.wait(delay_length)?;
                delay_length
            } else {
                TickCounter::new(0)
            }
        };

        let portamento_length =
            TickCounter::new(total_length.value().wrapping_sub(note1_length.value()));
        if portamento_length.is_zero() {
            return Err(MmlError::PortamentoDelayTooLong);
        }

        let velocity = match speed_override {
            Some(speed) => {
                if note1 < note2 {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                }
            }
            None => {
                let inst = match self.instrument {
                    Some(index) => self.instrument_from_index(index),
                    None => return Err(MmlError::PortamentoRequiresInstrument),
                };
                let p1: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note1)
                    .into();
                let p2: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note2)
                    .into();

                let ticks = i32::try_from(portamento_length.value()).unwrap();

                (p2 - p1) / ticks
            }
        };
        let velocity = PortamentoVelocity::try_from(velocity)?;

        let (p_length, p_rest) =
            Self::split_play_note_length(tie_length + portamento_length, is_slur)?;
        self.bc.portamento(note2, velocity, p_length);

        self.prev_slurred_note = if is_slur { Some(note2) } else { None };

        self.rest_after_play_note(p_rest, is_slur)
    }

    fn broken_chord(
        &mut self,
        notes: &[Note],
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    ) -> Result<(), MmlError> {
        self.prev_slurred_note = None;

        if notes.is_empty() {
            return Err(MmlError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(MmlError::TooManyNotesInBrokenChord(notes.len()));
        }
        let n_notes: u32 = notes.len().try_into().unwrap();

        for n in notes {
            self.test_note(*n)?;
        }

        let expected_tick_counter = self.bc.get_tick_counter() + total_length;

        let total_ticks = total_length.value();

        // Number of ticks in the last note played outside the loop (if any).
        let mut last_note_ticks = total_ticks % note_length.ticks();

        // If tie is true, a keyoff is required after the loop.
        if note_length.is_slur() && last_note_ticks == 0 {
            last_note_ticks += note_length.ticks();
        }

        if last_note_ticks != 0 && last_note_ticks < BcTicksKeyOff::MIN {
            last_note_ticks = BcTicksKeyOff::MIN;
        }
        let last_note_ticks = last_note_ticks;

        if total_ticks < last_note_ticks {
            return Err(MmlError::BrokenChordTotalLengthTooShort);
        }
        let notes_in_loop = (total_ticks - last_note_ticks) / note_length.ticks();

        let break_point = usize::try_from(notes_in_loop % n_notes).unwrap();
        let has_break_point: bool = break_point != 0;

        let n_loops = (notes_in_loop / n_notes) + u32::from(has_break_point);

        if n_loops < 2 {
            return Err(MmlError::BrokenChordTotalLengthTooShort);
        }

        let n_loops = LoopCount::try_from(n_loops)?;

        self.bc.start_loop(Some(n_loops))?;

        for (i, n) in notes.iter().enumerate() {
            if i == break_point && i != 0 {
                self.bc.skip_last_loop()?;
            }
            self.bc.play_note(*n, note_length);
        }

        self.bc.end_loop(None)?;

        if last_note_ticks > 0 {
            // The last note to play is always a keyoff note.
            self.bc.play_note(
                notes[break_point],
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(last_note_ticks)?),
            )
        }

        if self.bc.get_tick_counter() != expected_tick_counter {
            return Err(MmlError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument(&mut self, inst_index: usize) -> Result<(), MmlError> {
        if self.instrument == Some(inst_index) {
            return Ok(());
        }
        let inst = self.instrument_from_index(inst_index);
        let old_inst = self.instrument.map(|i| self.instrument_from_index(i));

        let i_id = inst.instrument_id;
        let envelope = inst.envelope.clone();

        match old_inst {
            Some(old) if old.instrument_id == i_id => {
                // InstrumentId unchanged, check envelope
                if Some(&inst.envelope) != self.envelope.as_ref() {
                    if inst.envelope_unchanged {
                        // Outputs fewer bytes then a `set_adsr` instruction.
                        self.bc.set_instrument(i_id);
                    } else {
                        match inst.envelope {
                            Envelope::Adsr(adsr) => self.bc.set_adsr(adsr),
                            Envelope::Gain(gain) => self.bc.set_gain(gain),
                        }
                    }
                }
            }
            _ => {
                if inst.envelope_unchanged {
                    self.bc.set_instrument(i_id);
                } else {
                    match inst.envelope {
                        Envelope::Adsr(adsr) => self.bc.set_instrument_and_adsr(i_id, adsr),
                        Envelope::Gain(gain) => self.bc.set_instrument_and_gain(i_id, gain),
                    }
                }
            }
        }

        self.instrument = Some(inst_index);
        self.envelope = Some(envelope);

        Ok(())
    }

    fn call_subroutine(&mut self, s_id: SubroutineId) -> Result<(), MmlError> {
        // CallSubroutine commands should only be created if the channel can call a subroutine.
        // `s_id` should always be valid
        let sub = match self.subroutines {
            Some(s) => match s.get(s_id.as_usize()) {
                Some(s) => s,
                None => panic!("invalid SubroutineId"),
            },
            None => panic!("subroutines is None"),
        };

        // Calling a subroutine disables manual vibrato
        self.vibrato = None;
        if self.mp == MpState::Manual {
            self.mp = MpState::Disabled;
        }

        if let Some(inst) = sub.last_instrument {
            self.instrument = Some(inst);
        }
        if let Some(e) = &sub.last_envelope {
            self.envelope = Some(e.clone());
        }

        self.bc.call_subroutine(s_id)?;

        Ok(())
    }

    fn set_manual_vibrato(&mut self, v: Option<ManualVibrato>) {
        self.mp = MpState::Manual;
        match v {
            Some(v) => {
                self.vibrato = Some(v);
                self.bc
                    .set_vibrato(v.pitch_offset_per_tick, v.quarter_wavelength_ticks);
            }
            None => {
                self.vibrato = None;
                self.bc.disable_vibrato();
            }
        }
    }

    fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), MmlError> {
        let tc = (self.bc.get_tick_counter(), tick_clock);
        self.tempo_changes.push(tc);

        self.bc.set_song_tick_clock(tick_clock)?;
        Ok(())
    }

    fn process_command(&mut self, command: &MmlCommand) -> Result<(), MmlError> {
        match command {
            MmlCommand::NoCommand => (),

            &MmlCommand::SetLoopPoint => match self.bc.get_context() {
                BytecodeContext::SongChannel => {
                    if self.loop_point.is_some() {
                        return Err(MmlError::LoopPointAlreadySet);
                    }
                    self.loop_point = Some(LoopPoint {
                        bytecode_offset: self.bc.get_bytecode_len(),
                        tick_counter: self.bc.get_tick_counter(),
                    })
                }
                BytecodeContext::SongSubroutine => return Err(MmlError::CannotSetLoopPoint),
                &BytecodeContext::SoundEffect => return Err(MmlError::CannotSetLoopPoint),
            },

            &MmlCommand::SetInstrument(inst_index) => {
                self.set_instrument(inst_index)?;
            }

            &MmlCommand::CallSubroutine(s_id) => {
                self.call_subroutine(s_id)?;
            }

            &MmlCommand::SetManualVibrato(v) => {
                self.set_manual_vibrato(v);
            }

            &MmlCommand::SetMpVibrato(mp) => match mp {
                Some(mp) => self.mp = MpState::Mp(mp),
                None => self.mp = MpState::Disabled,
            },

            &MmlCommand::Rest {
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                self.rest_one_keyoff(ticks_until_keyoff)?;
                self.rest_many_keyoffs(ticks_after_keyoff)?;
            }

            &MmlCommand::Wait(length) => {
                self.wait(length)?;
            }

            &MmlCommand::PlayNote {
                note,
                length,
                is_slur,
            } => {
                self.play_note_with_mp(note, length, is_slur)?;
            }

            &MmlCommand::PlayQuantizedNote {
                note,
                length: _,
                key_on_length,
                rest,
            } => {
                self.play_note_with_mp(note, key_on_length, false)?;
                self.rest_many_keyoffs(rest)?;
            }

            &MmlCommand::Portamento {
                note1,
                note2,
                is_slur,
                speed_override,
                total_length,
                delay_length,
                tie_length,
            } => {
                self.portamento(
                    note1,
                    note2,
                    is_slur,
                    speed_override,
                    total_length,
                    delay_length,
                    tie_length,
                )?;
            }

            MmlCommand::BrokenChord {
                notes,
                total_length,
                note_length,
            } => {
                self.broken_chord(notes, *total_length, *note_length)?;
            }

            MmlCommand::StartLoop => {
                self.bc.start_loop(None)?;
                self.skip_last_loop_state.push(None);
            }

            MmlCommand::SkipLastLoop => {
                self.bc.skip_last_loop()?;

                if let Some(s) = self.skip_last_loop_state.last_mut() {
                    *s = Some(SkipLastLoopState {
                        instrument: self.instrument,
                        envelope: self.envelope.clone(),
                        prev_slurred_note: self.prev_slurred_note,
                        vibrato: self.vibrato,
                    });
                }
            }

            &MmlCommand::EndLoop(loop_count) => {
                self.bc.end_loop(Some(loop_count))?;

                if let Some(Some(s)) = &self.skip_last_loop_state.pop() {
                    self.instrument = s.instrument;
                    self.envelope = s.envelope.clone();
                    self.prev_slurred_note = s.prev_slurred_note;
                    self.vibrato = s.vibrato;
                }
            }

            &MmlCommand::ChangePanAndOrVolume(pan, volume) => match (pan, volume) {
                (Some(PanCommand::Absolute(p)), Some(VolumeCommand::Absolute(v))) => {
                    self.bc.set_pan_and_volume(p, v);
                }
                (pan, volume) => {
                    match volume {
                        Some(VolumeCommand::Absolute(v)) => self.bc.set_volume(v),
                        Some(VolumeCommand::Relative(v)) => self.bc.adjust_volume(v),
                        None => (),
                    }
                    match pan {
                        Some(PanCommand::Absolute(p)) => self.bc.set_pan(p),
                        Some(PanCommand::Relative(p)) => self.bc.adjust_pan(p),
                        None => (),
                    }
                }
            },

            &MmlCommand::SetEcho(e) => {
                if e {
                    self.bc.enable_echo();
                } else {
                    self.bc.disable_echo();
                }
            }

            &MmlCommand::SetSongTempo(bpm) => {
                self.set_song_tick_clock(bpm.to_tick_clock()?)?;
            }
            &MmlCommand::SetSongTickClock(tick_clock) => {
                self.set_song_tick_clock(tick_clock)?;
            }
        }

        Ok(())
    }
}

pub struct MmlSongBytecodeGenerator<'a, 'b> {
    song_data: Vec<u8>,

    default_zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    sections: &'a [Section],
    instruments: &'a Vec<MmlInstrument>,
    instrument_map: HashMap<IdentifierStr<'a>, usize>,

    subroutines: Option<&'b Vec<Subroutine>>,
    subroutine_map: Option<HashMap<IdentifierStr<'b>, SubroutineId>>,

    #[cfg(feature = "mml_tracking")]
    first_channel_bc_offset: Option<u16>,
    #[cfg(feature = "mml_tracking")]
    cursor_tracker: CursorTracker,
    #[cfg(feature = "mml_tracking")]
    bytecode_tracker: Vec<BytecodePos>,
}

impl<'a, 'b> MmlSongBytecodeGenerator<'a, 'b> {
    pub fn new(
        default_zenlen: ZenLen,
        pitch_table: &'a PitchTable,
        sections: &'a [Section],
        instruments: &'a Vec<MmlInstrument>,
        instrument_map: HashMap<IdentifierStr<'a>, usize>,
        header_size: usize,
    ) -> Self {
        Self {
            song_data: vec![0; header_size],
            default_zenlen,
            pitch_table,
            sections,
            instruments,
            instrument_map,
            subroutines: None,
            subroutine_map: None,

            #[cfg(feature = "mml_tracking")]
            first_channel_bc_offset: None,
            #[cfg(feature = "mml_tracking")]
            cursor_tracker: CursorTracker::new(),
            #[cfg(feature = "mml_tracking")]
            bytecode_tracker: Vec::new(),
        }
    }

    #[cfg(feature = "mml_tracking")]
    pub(crate) fn take_data(self) -> (Vec<u8>, SongBcTracking) {
        (
            self.song_data,
            SongBcTracking {
                bytecode: self.bytecode_tracker,
                cursor_tracker: self.cursor_tracker,
                first_channel_bc_offset: self.first_channel_bc_offset.unwrap_or(u16::MAX),
            },
        )
    }

    #[cfg(not(feature = "mml_tracking"))]
    pub(crate) fn take_data(self) -> Vec<u8> {
        self.song_data
    }

    // Should only be called when all subroutines have been compiled.
    pub fn set_subroutines(&mut self, subroutines: &'b Vec<Subroutine>) {
        let subroutine_map = subroutines
            .iter()
            .map(|s| (s.identifier.as_ref(), s.subroutine_id))
            .collect();

        self.subroutines = Some(subroutines);
        self.subroutine_map = Some(subroutine_map);
    }

    fn parse_and_compile(
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) {
        while let Some(c) = parser.next() {
            match gen.process_command(c.command()) {
                Ok(()) => (),
                Err(e) => parser.add_error_range(c.pos().clone(), e),
            }

            if matches!(c.command(), MmlCommand::EndLoop(_)) {
                parser.set_tick_counter(gen.bc.get_tick_counter_with_loop_flag());
            }

            #[cfg(feature = "mml_tracking")]
            bytecode_tracker.push(BytecodePos {
                bc_end_pos: gen.bc.get_bytecode_len().try_into().unwrap_or(0xffff),
                char_index: c.pos().index_start,
            });
        }
    }

    pub fn parse_and_compile_song_subroutione(
        &mut self,
        lines: &[MmlLine],
        identifier: Identifier,
        subroutine_index: u8,
    ) -> Result<Subroutine, MmlChannelError> {
        assert!(self.subroutine_map.is_none());

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        let mut parser = Parser::new(
            ChannelId::Subroutine(subroutine_index),
            lines,
            &self.instrument_map,
            None,
            self.default_zenlen,
            None, // No sections in subroutines
            #[cfg(feature = "mml_tracking")]
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.instruments,
            None,
            BytecodeContext::SongSubroutine,
        );

        Self::parse_and_compile(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        let last_pos = *parser.peek_pos();
        let tick_counter = gen.bc.get_tick_counter();
        let max_nested_loops = gen.bc.get_max_nested_loops();

        assert!(gen.loop_point.is_none());

        self.song_data = match gen.bc.bytecode(BcTerminator::ReturnFromSubroutine) {
            Ok(b) => b,
            Err((e, b)) => {
                parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
                b
            }
        };

        let (_, errors) = parser.finalize();

        if errors.is_empty() {
            Ok(Subroutine {
                identifier,
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                subroutine_id: SubroutineId::new(subroutine_index, tick_counter, max_nested_loops),
                last_instrument: gen.instrument,
                last_envelope: gen.envelope,
                changes_song_tempo: !gen.tempo_changes.is_empty(),
            })
        } else {
            Err(MmlChannelError { identifier, errors })
        }
    }

    pub fn parse_and_compile_song_channel(
        &mut self,
        lines: &[MmlLine],
        identifier: Identifier,
    ) -> Result<Channel, MmlChannelError> {
        assert!(self.subroutine_map.is_some());

        assert!(identifier.as_str().len() == 1);
        let channel_char = identifier.as_str().chars().next().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        #[cfg(feature = "mml_tracking")]
        if self.first_channel_bc_offset.is_none() {
            self.first_channel_bc_offset = sd_start_index.try_into().ok();
        }

        let mut parser = Parser::new(
            ChannelId::Channel(channel_char),
            lines,
            &self.instrument_map,
            self.subroutine_map.as_ref(),
            self.default_zenlen,
            Some(self.sections),
            #[cfg(feature = "mml_tracking")]
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.instruments,
            self.subroutines,
            BytecodeContext::SongChannel,
        );

        Self::parse_and_compile(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        let last_pos = *parser.peek_pos();
        let tick_counter = gen.bc.get_tick_counter();

        let terminator = match gen.loop_point {
            None => BcTerminator::DisableChannel,
            Some(lp) => {
                if lp.tick_counter == tick_counter {
                    parser.add_error_range(last_pos.to_range(1), MmlError::NoTicksAfterLoopPoint);
                }
                BcTerminator::LoopChannel
            }
        };

        self.song_data = match gen.bc.bytecode(terminator) {
            Ok(b) => b,
            Err((e, b)) => {
                parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
                b
            }
        };

        let (section_tick_counters, errors) = parser.finalize();

        if errors.is_empty() {
            Ok(Channel {
                name: identifier.as_str().chars().next().unwrap(),
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                loop_point: gen.loop_point,
                tick_counter,
                section_tick_counters,
                tempo_changes: gen.tempo_changes,
            })
        } else {
            Err(MmlChannelError { identifier, errors })
        }
    }
}

pub fn parse_and_compile_sound_effect(
    lines: &[MmlLine],
    pitch_table: &PitchTable,
    instruments: &Vec<MmlInstrument>,
    instruments_map: &HashMap<IdentifierStr, usize>,
) -> Result<MmlSoundEffect, Vec<ErrorWithPos<MmlError>>> {
    #[cfg(feature = "mml_tracking")]
    let mut cursor_tracker = CursorTracker::new();

    let mut parser = Parser::new(
        ChannelId::SoundEffect,
        lines,
        instruments_map,
        None,
        DEFAULT_ZENLEN,
        None, // No sections in sound effect
        #[cfg(feature = "mml_tracking")]
        &mut cursor_tracker,
    );

    let mut gen = ChannelBcGenerator::new(
        Vec::new(),
        pitch_table,
        instruments,
        None,
        BytecodeContext::SoundEffect,
    );

    while let Some(c) = parser.next() {
        match gen.process_command(c.command()) {
            Ok(()) => (),
            Err(e) => parser.add_error_range(c.pos().clone(), e),
        }
        if matches!(c.command(), MmlCommand::EndLoop(_)) {
            parser.set_tick_counter(gen.bc.get_tick_counter_with_loop_flag());
        }
    }

    let last_pos = *parser.peek_pos();
    let tick_counter = gen.bc.get_tick_counter();

    assert!(gen.loop_point.is_none());

    let bytecode = match gen.bc.bytecode(BcTerminator::DisableChannel) {
        Ok(b) => b,
        Err((e, b)) => {
            parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
            b
        }
    };

    let (_, errors) = parser.finalize();

    if errors.is_empty() {
        Ok(MmlSoundEffect {
            bytecode,
            tick_counter,

            #[cfg(feature = "mml_tracking")]
            cursor_tracker,
        })
    } else {
        Err(errors)
    }
}
