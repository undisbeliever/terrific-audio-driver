//! MML bytecode generator

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::command_parser::{
    ManualVibrato, MmlCommand, MmlCommandWithPos, MpVibrato, PanCommand, Parser, PortamentoSpeed,
    VolumeCommand,
};
use super::identifier::IdentifierStr;
use super::instruments::MmlInstrument;
use super::tokenizer::{MmlTokens, SubroutineCallType};
use super::{ChannelId, MmlSoundEffect, Section};

#[cfg(feature = "mml_tracking")]
use super::note_tracking::CursorTracker;
use crate::bytecode_assembler::parse_asm_line;
use crate::data::{self, UniqueNamesList};
use crate::envelope::{Adsr, Envelope, Gain, TempGain};
#[cfg(feature = "mml_tracking")]
use crate::songs::{BytecodePos, SongBcTracking};

use crate::bytecode::{
    BcTerminator, BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, BytecodeContext, IeState,
    LoopCount, PitchOffsetPerTick, PlayNoteTicks, PortamentoVelocity, RelativeVolume,
    SlurredNoteState, SubroutineId, VibratoState, KEY_OFF_TICK_DELAY,
};
use crate::errors::{ErrorWithPos, MmlChannelError, MmlError, ValueError};
use crate::notes::{Note, SEMITONES_PER_OCTAVE};
use crate::pitch_table::{PitchTable, PITCH_REGISTER_MAX};
use crate::songs::{Channel, LoopPoint, Subroutine};
use crate::sound_effects::MAX_SFX_TICKS;
use crate::time::{TickClock, TickCounter, ZenLen, DEFAULT_ZENLEN};

use std::collections::HashMap;

pub const MAX_PORTAMENTO_SLIDE_TICKS: u32 = 16 * 1024;
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

    assert!(ticks > Loop::MAX_TICKS * 3);

    (LoopCount::MIN_LOOPS..=LoopCount::MAX_LOOPS)
        .filter_map(|l| {
            let div = ticks / l;
            let rem = ticks % l;

            if div >= Loop::MIN_TICKS && ((ALLOW_ZERO_REM && rem == 0) || rem >= Rem::MIN_TICKS) {
                Some(RestLoop {
                    ticks_in_loop: div,
                    n_loops: l,
                    remainder: rem,

                    n_rest_instructions: div_ceiling(div, Loop::MAX_TICKS)
                        + div_ceiling(rem, Rem::MAX_TICKS),
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

struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    instruments: &'a Vec<MmlInstrument>,
    subroutines: Option<&'a Vec<Subroutine>>,

    bc: Bytecode<'a>,

    mp: MpState,

    loop_point: Option<LoopPoint>,
}

impl ChannelBcGenerator<'_> {
    fn new<'a>(
        bc_data: Vec<u8>,
        pitch_table: &'a PitchTable,
        mml_file: &'a str,
        data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        mml_instruments: &'a Vec<MmlInstrument>,
        subroutines: Option<&'a Vec<Subroutine>>,
        context: BytecodeContext,
    ) -> ChannelBcGenerator<'a> {
        ChannelBcGenerator {
            pitch_table,
            mml_file,
            instruments: mml_instruments,
            subroutines,
            // Using None for subroutines to forbid subroutine calls in bytecode assembly
            bc: Bytecode::new_append_to_vec(bc_data, context, data_instruments, None),
            mp: MpState::Manual,
            loop_point: None,
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
        let instrument_id = match self.bc.get_state().instrument {
            IeState::Known(i) | IeState::Maybe(i) => i,
            IeState::Unknown => return Err(MmlError::CannotUseMpWithoutInstrument),
        };

        let pitch = self.pitch_table.pitch_for_note(instrument_id, note);

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

    fn split_wait_length(length: TickCounter) -> Result<(BcTicksNoKeyOff, TickCounter), MmlError> {
        let l = length.value();

        let bc = u32::min(l, BcTicksNoKeyOff::MAX_TICKS);
        let bc = BcTicksNoKeyOff::try_from(bc)?;

        Ok((bc, TickCounter::new(l - bc.ticks())))
    }

    fn split_play_note_length(
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(PlayNoteTicks, TickCounter), ValueError> {
        let l = length.value();

        if !is_slur {
            // Add rest
            const MIN_TWO_INSTRUCTIONS: u32 = BcTicksKeyOff::MAX_TICKS + 1;
            const MIN_THREE_INSTRUCTIONS: u32 =
                BcTicksNoKeyOff::MAX_TICKS + BcTicksKeyOff::MAX_TICKS + 1;
            match l {
                0..=BcTicksKeyOff::MAX_TICKS => Ok((
                    PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(l)?),
                    TickCounter::new(0),
                )),
                MIN_TWO_INSTRUCTIONS..MIN_THREE_INSTRUCTIONS => {
                    let w = BcTicksNoKeyOff::try_from(l - BcTicksKeyOff::MAX_TICKS)?;
                    let r = l - w.ticks();
                    debug_assert_eq!(w.ticks() + r, l);

                    Ok((PlayNoteTicks::NoKeyOff(w), TickCounter::new(r)))
                }
                MIN_THREE_INSTRUCTIONS.. => {
                    let w = BcTicksNoKeyOff::try_from(BcTicksNoKeyOff::MAX_TICKS).unwrap();
                    let r = l - w.ticks();
                    debug_assert_eq!(w.ticks() + r, l);

                    Ok((PlayNoteTicks::NoKeyOff(w), TickCounter::new(r)))
                }
            }
        } else {
            match l {
                0..=BcTicksNoKeyOff::MAX_TICKS => Ok((
                    PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(l)?),
                    TickCounter::new(0),
                )),
                _ => {
                    let w1 = BcTicksNoKeyOff::MAX;
                    let w2 = l - w1.ticks();

                    Ok((PlayNoteTicks::NoKeyOff(w1), TickCounter::new(w2)))
                }
            }
        }
    }

    fn play_note_with_mp(
        &mut self,
        note: Note,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), MmlError> {
        let (pn_length, rest) = Self::split_play_note_length(length, is_slur)?;

        let vibrato = self.bc.get_state().vibrato;

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_length)?;
            }
            MpState::Disabled => {
                const POPT: PitchOffsetPerTick = PitchOffsetPerTick::new(0);

                let vibrato_disabled = match vibrato {
                    VibratoState::Unchanged => true,
                    VibratoState::Unknown => false,
                    VibratoState::Disabled => true,
                    VibratoState::Set(v_popt, _) => v_popt == POPT,
                };

                if vibrato_disabled {
                    self.bc.play_note(note, pn_length)?;
                } else {
                    self.bc
                        .set_vibrato_depth_and_play_note(POPT, note, pn_length);
                }

                // Switching MpState to Manual to skip the `vibrato_disabled` checks on subsequent notes.
                // This must be done outside of the loop in case the `MP0` is after a `:` skip-last-loop command.
                // (see `test_mp0_after_skip_last_loop()` test)
                if !self.bc.is_in_loop() {
                    self.mp = MpState::Manual;
                }
            }
            MpState::Mp(mp) => {
                let cv = self.calculate_vibrato_for_note(mp, note)?;

                match vibrato {
                    v if v
                        == VibratoState::Set(
                            cv.pitch_offset_per_tick,
                            cv.quarter_wavelength_ticks,
                        ) =>
                    {
                        self.bc.play_note(note, pn_length)?;
                    }
                    VibratoState::Set(_, qwt) if qwt == cv.quarter_wavelength_ticks => {
                        self.bc.set_vibrato_depth_and_play_note(
                            cv.pitch_offset_per_tick,
                            note,
                            pn_length,
                        );
                    }
                    _ => {
                        self.bc
                            .set_vibrato(cv.pitch_offset_per_tick, cv.quarter_wavelength_ticks);
                        self.bc.play_note(note, pn_length)?;
                    }
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
            debug_assert!(length.value() >= BcTicksKeyOff::MAX_TICKS);
            self.rest_one_keyoff(length)
        }
    }

    // Rest that can send multiple `rest_keyoff` instructions
    // The `rest_keyoff` instruction can wait for more ticks than the `rest` instruction.
    fn rest_many_keyoffs_no_loop(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const _: () = assert!(BcTicksKeyOff::MAX_TICKS > BcTicksNoKeyOff::MAX_TICKS);
        const MAX_REST: u32 = BcTicksKeyOff::MAX_TICKS;
        const MIN_REST: u32 = BcTicksKeyOff::MIN_TICKS;
        const _: () = assert!(MIN_REST > 1);

        let mut remaining_ticks = length.value();

        while remaining_ticks > MAX_REST {
            let l = if remaining_ticks >= MAX_REST + MIN_REST {
                MAX_REST
            } else {
                MAX_REST - 1
            };
            self.bc.rest(BcTicksKeyOff::try_from(l).unwrap());
            remaining_ticks -= l;
        }

        // The channel is in the release state.  The last rest **can** be less than max.
        self.bc.rest(BcTicksKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    // rest with no keyoff
    fn wait_no_loop(&mut self, length: TickCounter) -> Result<(), MmlError> {
        let mut remaining_ticks = length.value();

        let rest_length = BcTicksNoKeyOff::MAX;
        const _: () = assert!(BcTicksNoKeyOff::MIN_TICKS == 1);

        while remaining_ticks > rest_length.ticks() {
            self.bc.wait(rest_length);
            remaining_ticks -= rest_length.ticks();
        }

        self.bc.wait(BcTicksNoKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    // A rest that sends a single keyoff event
    fn rest_one_keyoff(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const REST_TICKS: u32 = BcTicksKeyOff::MAX_TICKS;
        const MIN_TWO_INSTRUCTIONS: u32 = REST_TICKS + 1;

        match length.value() {
            0 => (),
            l @ 1..=BcTicksKeyOff::MAX_TICKS => self.bc.rest(BcTicksKeyOff::try_from(l)?),
            l @ MIN_TWO_INSTRUCTIONS.. => {
                self.wait(TickCounter::new(l - REST_TICKS))?;
                self.bc.rest(BcTicksKeyOff::try_from(REST_TICKS).unwrap());
            }
        }
        Ok(())
    }

    // Rest that can send multiple `rest_keyoff` instructions
    // The `rest_keyoff` instruction can wait for more ticks than the `rest` instruction.
    fn rest_many_keyoffs(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MIN_LOOP_REST: u32 = BcTicksKeyOff::MAX_TICKS * REST_LOOP_INSTRUCTION_THREASHOLD + 1;

        if length.is_zero() {
            return Ok(());
        }

        if length.value() < MIN_LOOP_REST || !self.bc.can_loop() {
            self.rest_many_keyoffs_no_loop(length)
        } else {
            // Convert a long rest to a rest loop.

            let rl = build_rest_loop::<BcTicksKeyOff, BcTicksKeyOff, true>(length);

            self.bc.start_loop(Some(LoopCount::try_from(rl.n_loops)?))?;
            self.rest_many_keyoffs_no_loop(TickCounter::new(rl.ticks_in_loop))?;
            self.bc.end_loop(None)?;

            // The channel is in the release state.  The last rest **can** be less than max.
            if rl.remainder > 0 {
                self.bc.rest(rl.remainder.try_into()?);
            }

            Ok(())
        }
    }

    // rest with no keyoff
    fn wait(&mut self, length: TickCounter) -> Result<(), MmlError> {
        const MIN_LOOP_REST: u32 =
            BcTicksNoKeyOff::MAX_TICKS * REST_LOOP_INSTRUCTION_THREASHOLD + 1;

        if length.value() < MIN_LOOP_REST || !self.bc.can_loop() {
            self.wait_no_loop(length)
        } else {
            // Convert a long rest to a rest loop.

            let rl = build_rest_loop::<BcTicksNoKeyOff, BcTicksNoKeyOff, true>(length);

            self.bc.start_loop(Some(LoopCount::try_from(rl.n_loops)?))?;
            self.wait_no_loop(TickCounter::new(rl.ticks_in_loop))?;
            self.bc.end_loop(None)?;

            if rl.remainder > 0 {
                self.bc.wait(rl.remainder.try_into()?);
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
        delay_length: TickCounter,
        slide_length: TickCounter,
        tie_length: TickCounter,
    ) -> Result<(), MmlError> {
        #[cfg(debug_assertions)]
        let expected_tick_counter =
            self.bc.get_tick_counter() + delay_length + slide_length + tie_length;

        let play_note1 = self.bc.get_state().prev_slurred_note != SlurredNoteState::Slurred(note1);

        // Play note1 (if required)
        let slide_length = match (play_note1, delay_length.value()) {
            (true, 0) => {
                // Play note1 for a single tick
                let t = PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(1).unwrap());
                self.bc.play_note(note1, t)?;

                // subtract 1 tick from slide_length
                TickCounter::new(slide_length.value().saturating_sub(1))
            }
            (true, _) => {
                let (pn_length, rest) = Self::split_play_note_length(delay_length, true)?;

                self.bc.play_note(note1, pn_length)?;
                self.rest_after_play_note(rest, true)?;

                slide_length
            }
            (false, 0) => {
                // No delay and pitch is correct
                slide_length
            }
            (false, _) => {
                // pitch is correct
                self.wait(delay_length)?;
                slide_length
            }
        };

        // slide_length: number of ticks in the pitch-slide.  Does NOT include the key-off tick.
        // tie_length: number of ticks after pitch-slide reaches note2, includes the key-off tick (as required).
        let (slide_length, tie_length) = if !is_slur && tie_length.is_zero() {
            (
                TickCounter::new(slide_length.value().saturating_sub(1)),
                TickCounter::new(1),
            )
        } else {
            (slide_length, tie_length)
        };

        if slide_length.is_zero() {
            return Err(MmlError::PortamentoTooShort);
        }
        if slide_length.value() > MAX_PORTAMENTO_SLIDE_TICKS {
            const _: () = assert!(
                (MAX_PORTAMENTO_SLIDE_TICKS as u64) * (PITCH_REGISTER_MAX as u64)
                    < (i32::MAX as u64) / 2,
                "Portamento velocity can overflow"
            );

            return Err(MmlError::PortamentoTooLong);
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
                let instrument_id = match self.bc.get_state().instrument {
                    IeState::Known(i) | IeState::Maybe(i) => i,
                    IeState::Unknown => return Err(MmlError::PortamentoRequiresInstrument),
                };
                let p1: i32 = self.pitch_table.pitch_for_note(instrument_id, note1).into();
                let p2: i32 = self.pitch_table.pitch_for_note(instrument_id, note2).into();
                let delta = p2 - p1;

                let ticks = i32::try_from(slide_length.value()).unwrap();
                assert!(ticks > 0);

                // division with rounding
                if delta > 0 {
                    (delta + (ticks / 2)) / ticks
                } else {
                    (delta - (ticks / 2)) / ticks
                }
            }
        };
        let velocity = PortamentoVelocity::try_from(velocity)?;

        let (p_length, p_rest) = Self::split_play_note_length(slide_length + tie_length, is_slur)?;
        self.bc.portamento(note2, velocity, p_length)?;

        self.rest_after_play_note(p_rest, is_slur)?;

        #[cfg(debug_assertions)]
        debug_assert_eq!(self.bc.get_tick_counter(), expected_tick_counter);

        Ok(())
    }

    fn broken_chord(
        &mut self,
        notes: &[Note],
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    ) -> Result<(), MmlError> {
        if notes.is_empty() {
            return Err(MmlError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(MmlError::TooManyNotesInBrokenChord(notes.len()));
        }
        let n_notes: u32 = notes.len().try_into().unwrap();

        let expected_tick_counter = self.bc.get_tick_counter() + total_length;

        let total_ticks = total_length.value();

        // Number of ticks in the last note played outside the loop (if any).
        let mut last_note_ticks = total_ticks % note_length.ticks();

        // If tie is true, a keyoff is required after the loop.
        if note_length.is_slur() && last_note_ticks == 0 {
            last_note_ticks += note_length.ticks();
        }

        if last_note_ticks != 0 && last_note_ticks < BcTicksKeyOff::MIN_TICKS {
            last_note_ticks = BcTicksKeyOff::MIN_TICKS;
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
            match self.bc.play_note(*n, note_length) {
                Ok(()) => (),
                Err(e) => {
                    // Hides an unneeded "loop stack not empty" (or end_loop) error
                    let _ = self.bc.end_loop(None);
                    return Err(e.into());
                }
            }
        }

        self.bc.end_loop(None)?;

        if last_note_ticks > 0 {
            // The last note to play is always a keyoff note.
            self.bc.play_note(
                notes[break_point],
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(last_note_ticks)?),
            )?;
        }

        if self.bc.get_tick_counter() != expected_tick_counter {
            return Err(MmlError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument(&mut self, inst: usize) -> Result<(), MmlError> {
        let inst = match self.instruments.get(inst) {
            Some(inst) => inst,
            None => panic!("invalid instrument index"),
        };

        let old_inst = self.bc.get_state().instrument;
        let old_envelope = self.bc.get_state().envelope;

        let i_id = inst.instrument_id;
        let envelope = inst.envelope;

        if old_inst.is_known_and_eq(&i_id) {
            // InstrumentId unchanged, check envelope
            if !old_envelope.is_known_and_eq(&envelope) {
                if inst.envelope_unchanged {
                    // Outputs fewer bytes then a `set_adsr` instruction.
                    self.bc.set_instrument(i_id);
                } else {
                    match envelope {
                        Envelope::Adsr(adsr) => self.bc.set_adsr(adsr),
                        Envelope::Gain(gain) => self.bc.set_gain(gain),
                    }
                }
            }
        } else if inst.envelope_unchanged {
            self.bc.set_instrument(i_id);
        } else {
            match envelope {
                Envelope::Adsr(adsr) => self.bc.set_instrument_and_adsr(i_id, adsr),
                Envelope::Gain(gain) => self.bc.set_instrument_and_gain(i_id, gain),
            }
        }

        Ok(())
    }

    fn set_adsr(&mut self, adsr: Adsr) {
        if !self
            .bc
            .get_state()
            .envelope
            .is_known_and_eq(&Envelope::Adsr(adsr))
        {
            self.bc.set_adsr(adsr);
        }
    }

    fn set_gain(&mut self, gain: Gain) {
        if !self
            .bc
            .get_state()
            .envelope
            .is_known_and_eq(&Envelope::Gain(gain))
        {
            self.bc.set_gain(gain);
        }
    }

    fn temp_gain(&mut self, temp_gain: Option<TempGain>) {
        if temp_gain.is_some_and(|t| !self.bc.get_state().prev_temp_gain.is_known_and_eq(&t)) {
            self.bc.set_temp_gain(temp_gain.unwrap())
        } else {
            self.bc.reuse_temp_gain()
        }
    }

    fn temp_gain_and_rest(
        &mut self,
        temp_gain: Option<TempGain>,
        ticks_until_keyoff: TickCounter,
        ticks_after_keyoff: TickCounter,
    ) -> Result<(), MmlError> {
        if temp_gain.is_some_and(|t| !self.bc.get_state().prev_temp_gain.is_known_and_eq(&t)) {
            let temp_gain = temp_gain.unwrap();

            match ticks_until_keyoff.value() {
                l @ ..=BcTicksKeyOff::MAX_TICKS => {
                    self.bc
                        .set_temp_gain_and_rest(temp_gain, BcTicksKeyOff::try_from(l)?);
                }
                l => {
                    let (wait1, wait2) =
                        Self::split_wait_length(TickCounter::new(l - BcTicksKeyOff::MAX_TICKS))?;
                    let rest = BcTicksKeyOff::MAX;
                    debug_assert_eq!(wait1.ticks() + wait2.value() + rest.ticks(), l);

                    self.bc.set_temp_gain_and_wait(temp_gain, wait1);
                    if !wait2.is_zero() {
                        self.wait(wait2)?;
                    }
                    self.bc.rest(rest);
                }
            }
        } else {
            // Reuse temp GAIN
            match ticks_until_keyoff.value() {
                l @ ..=BcTicksKeyOff::MAX_TICKS => {
                    self.bc
                        .reuse_temp_gain_and_rest(BcTicksKeyOff::try_from(l)?);
                }
                l => {
                    let (wait1, wait2) =
                        Self::split_wait_length(TickCounter::new(l - BcTicksKeyOff::MAX_TICKS))?;
                    let rest = BcTicksKeyOff::MAX;
                    debug_assert_eq!(wait1.ticks() + wait2.value() + rest.ticks(), l);

                    self.bc.reuse_temp_gain_and_wait(wait1);
                    if !wait2.is_zero() {
                        self.wait(wait2)?;
                    }
                    self.bc.rest(rest);
                }
            }
        }

        self.rest_many_keyoffs(ticks_after_keyoff)?;

        Ok(())
    }

    fn temp_gain_and_wait(
        &mut self,
        temp_gain: Option<TempGain>,
        ticks: TickCounter,
    ) -> Result<(), MmlError> {
        let (wait1, wait2) = Self::split_wait_length(ticks)?;

        if temp_gain.is_some_and(|t| !self.bc.get_state().prev_temp_gain.is_known_and_eq(&t)) {
            let temp_gain = temp_gain.unwrap();

            self.bc.set_temp_gain_and_wait(temp_gain, wait1);
        } else {
            self.bc.reuse_temp_gain_and_wait(wait1);
        }

        if !wait2.is_zero() {
            self.wait(wait2)?;
        }

        Ok(())
    }

    fn call_subroutine(
        &mut self,
        index: usize,
        disable_vibrato: SubroutineCallType,
    ) -> Result<(), MmlError> {
        // CallSubroutine commands should only be created if the channel can call a subroutine.
        // `s_id` should always be valid
        let sub = match self.subroutines {
            Some(s) => match s.get(index) {
                Some(s) => s,
                None => panic!("invalid SubroutineId"),
            },
            None => panic!("subroutines is None"),
        };

        match (disable_vibrato, &self.mp) {
            (SubroutineCallType::Asm, _) => {
                self.bc
                    .call_subroutine(sub.identifier.as_str(), &sub.subroutine_id)?;
            }
            (SubroutineCallType::AsmDisableVibrato, _) => {
                self.bc.call_subroutine_and_disable_vibrato(
                    sub.identifier.as_str(),
                    &sub.subroutine_id,
                )?;
            }
            (SubroutineCallType::Mml, MpState::Mp(_)) => {
                self.bc.call_subroutine_and_disable_vibrato(
                    sub.identifier.as_str(),
                    &sub.subroutine_id,
                )?;
            }
            (SubroutineCallType::Mml, MpState::Disabled | MpState::Manual) => {
                self.bc
                    .call_subroutine(sub.identifier.as_str(), &sub.subroutine_id)?;
            }
        }

        if let VibratoState::Set(..) = &self.bc.get_state().vibrato {
            match self.mp {
                MpState::Disabled | MpState::Manual => self.mp = MpState::Manual,
                MpState::Mp(_) => (),
            }
        }

        Ok(())
    }

    fn set_manual_vibrato(&mut self, v: Option<ManualVibrato>) {
        self.mp = MpState::Manual;
        match v {
            Some(v) => {
                self.bc
                    .set_vibrato(v.pitch_offset_per_tick, v.quarter_wavelength_ticks);
            }
            None => {
                self.bc.disable_vibrato();
            }
        }
    }

    fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), MmlError> {
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
                    if self.bc.is_in_loop() {
                        return Err(MmlError::CannotSetLoopPointInALoop);
                    }
                    self.loop_point = Some(LoopPoint {
                        bytecode_offset: self.bc.get_bytecode_len(),
                        tick_counter: self.bc.get_tick_counter(),
                    });

                    self.bc._song_loop_point();
                }
                BytecodeContext::SongSubroutine => return Err(MmlError::CannotSetLoopPoint),
                &BytecodeContext::SoundEffect => return Err(MmlError::CannotSetLoopPoint),
            },

            &MmlCommand::SetInstrument(inst_index) => {
                self.set_instrument(inst_index)?;
            }
            &MmlCommand::SetAdsr(adsr) => self.set_adsr(adsr),
            &MmlCommand::SetGain(gain) => self.set_gain(gain),

            &MmlCommand::CallSubroutine(s_id, d) => {
                self.call_subroutine(s_id, d)?;
            }

            &MmlCommand::SetManualVibrato(v) => {
                self.set_manual_vibrato(v);
            }

            &MmlCommand::SetMpVibrato(mp) => {
                self.mp = match mp {
                    Some(mp) => MpState::Mp(mp),
                    None => match self.mp {
                        MpState::Mp(_) => MpState::Disabled,
                        MpState::Disabled => MpState::Disabled,
                        MpState::Manual => match self.bc.get_state().vibrato.is_active() {
                            true => MpState::Disabled,
                            false => MpState::Manual,
                        },
                    },
                };
            }

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

            &MmlCommand::TempGain(temp_gain) => {
                self.temp_gain(temp_gain);
            }
            &MmlCommand::TempGainAndRest {
                temp_gain,
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                self.temp_gain_and_rest(temp_gain, ticks_until_keyoff, ticks_after_keyoff)?;
            }
            &MmlCommand::TempGainAndWait(temp_gain, ticks) => {
                self.temp_gain_and_wait(temp_gain, ticks)?;
            }

            &MmlCommand::DisableEarlyRelease => {
                if !self.bc.get_state().early_release.is_known_and_eq(&None) {
                    self.bc.disable_early_release();
                }
            }
            &MmlCommand::SetEarlyRelease(ticks, min, gain) => {
                let state = self.bc.get_state();
                if !state
                    .early_release
                    .is_known_and_eq(&Some((ticks, min, gain)))
                {
                    self.bc.set_early_release(ticks, min, gain);
                }
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
                length,
                key_on_length,
                temp_gain,
                rest_ticks_after_note,
            } => {
                assert!(length.value() > key_on_length.value() + KEY_OFF_TICK_DELAY);

                if temp_gain.is_disabled() {
                    let note_length = TickCounter::new(key_on_length.value() + KEY_OFF_TICK_DELAY);
                    let rest = TickCounter::new(
                        length.value() - note_length.value() + rest_ticks_after_note.value(),
                    );

                    self.play_note_with_mp(note, note_length, false)?;
                    // can `wait` here, `play_note_with_mp()` sends a key-off event
                    match rest.value() {
                        1 => self.wait(rest)?,
                        _ => self.rest_many_keyoffs(rest)?,
                    }
                } else {
                    let temp_gain_ticks = TickCounter::new(length.value() - key_on_length.value());

                    self.play_note_with_mp(note, key_on_length, true)?;
                    self.temp_gain_and_rest(
                        Some(temp_gain),
                        temp_gain_ticks,
                        rest_ticks_after_note,
                    )?;
                }
            }

            &MmlCommand::Portamento {
                note1,
                note2,
                is_slur,
                speed_override,
                delay_length,
                slide_length,
                tie_length,
            } => {
                self.portamento(
                    note1,
                    note2,
                    is_slur,
                    speed_override,
                    delay_length,
                    slide_length,
                    tie_length,
                )?;
            }

            &MmlCommand::QuantizedPortamento {
                note1,
                note2,
                speed_override,
                delay_length,
                slide_length,
                tie_length,
                temp_gain,
                rest,
                rest_ticks_after_note,
            } => {
                if temp_gain.is_disabled() {
                    assert!(rest.value() > KEY_OFF_TICK_DELAY);
                    let slide_length = TickCounter::new(slide_length.value() + KEY_OFF_TICK_DELAY);
                    let rest = TickCounter::new(
                        rest.value() - KEY_OFF_TICK_DELAY + rest_ticks_after_note.value(),
                    );

                    self.portamento(
                        note1,
                        note2,
                        false,
                        speed_override,
                        delay_length,
                        slide_length,
                        tie_length,
                    )?;
                    // can `wait` here, `portamento` will emit a key-off event
                    match rest.value() {
                        1 => self.wait(rest)?,
                        _ => self.rest_many_keyoffs(rest)?,
                    }
                } else {
                    self.portamento(
                        note1,
                        note2,
                        true,
                        speed_override,
                        delay_length,
                        slide_length,
                        tie_length,
                    )?;

                    self.temp_gain_and_rest(Some(temp_gain), rest, rest_ticks_after_note)?;
                }
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
            }

            MmlCommand::SkipLastLoop => {
                self.bc.skip_last_loop()?;
            }

            &MmlCommand::EndLoop(loop_count) => {
                self.bc.end_loop(Some(loop_count))?;
            }

            &MmlCommand::ChangePanAndOrVolume(pan, volume) => match (pan, volume) {
                (Some(PanCommand::Absolute(p)), Some(VolumeCommand::Absolute(v))) => {
                    self.bc.set_pan_and_volume(p, v);
                }
                (pan, volume) => {
                    match volume {
                        Some(VolumeCommand::Absolute(v)) => self.bc.set_volume(v),
                        Some(VolumeCommand::Relative(v)) => {
                            match RelativeVolume::try_from(v) {
                                Ok(v) => self.bc.adjust_volume(v),
                                Err(_) => {
                                    // Two relative volume commands are required
                                    assert!(
                                        v >= RelativeVolume::MIN.as_i8() as i32 * 2
                                            && v <= RelativeVolume::MAX.as_i8() as i32 * 2
                                    );

                                    let v1: i32 = if v < 0 {
                                        RelativeVolume::MIN.as_i8().into()
                                    } else {
                                        RelativeVolume::MAX.as_i8().into()
                                    };
                                    let v2 = v - v1;
                                    self.bc.adjust_volume(RelativeVolume::try_from(v1).unwrap());
                                    self.bc.adjust_volume(RelativeVolume::try_from(v2).unwrap());
                                }
                            }
                        }
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

            MmlCommand::StartBytecodeAsm => {
                self.bc._start_asm_block();
            }

            MmlCommand::EndBytecodeAsm => {
                self.bc._end_asm_block()?;
            }

            MmlCommand::BytecodeAsm(range) => {
                let asm = &self.mml_file[range.clone()];

                parse_asm_line(&mut self.bc, asm)?
            }
        }

        Ok(())
    }
}

pub struct MmlSongBytecodeGenerator<'a> {
    song_data: Vec<u8>,

    default_zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    sections: &'a [Section],
    mml_instruments: &'a Vec<MmlInstrument>,
    mml_instrument_map: HashMap<IdentifierStr<'a>, usize>,

    subroutines: Vec<Subroutine>,
    subroutine_map: HashMap<IdentifierStr<'a>, Option<SubroutineId>>,
    subroutine_name_map: &'a HashMap<IdentifierStr<'a>, usize>,

    #[cfg(feature = "mml_tracking")]
    first_channel_bc_offset: Option<u16>,
    #[cfg(feature = "mml_tracking")]
    cursor_tracker: CursorTracker,
    #[cfg(feature = "mml_tracking")]
    bytecode_tracker: Vec<BytecodePos>,
}

impl<'a> MmlSongBytecodeGenerator<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        default_zenlen: ZenLen,
        pitch_table: &'a PitchTable,
        mml_file: &'a str,
        data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        sections: &'a [Section],
        instruments: &'a Vec<MmlInstrument>,
        instrument_map: HashMap<IdentifierStr<'a>, usize>,
        subroutine_name_map: &'a HashMap<IdentifierStr<'a>, usize>,
        header_size: usize,
    ) -> Self {
        Self {
            song_data: vec![0; header_size],
            default_zenlen,
            pitch_table,
            mml_file,
            data_instruments,
            sections,
            mml_instruments: instruments,
            mml_instrument_map: instrument_map,
            subroutine_name_map,

            subroutines: Vec::new(),
            subroutine_map: HashMap::new(),

            #[cfg(feature = "mml_tracking")]
            first_channel_bc_offset: None,
            #[cfg(feature = "mml_tracking")]
            cursor_tracker: CursorTracker::new(),
            #[cfg(feature = "mml_tracking")]
            bytecode_tracker: Vec::new(),
        }
    }

    #[cfg(feature = "mml_tracking")]
    pub(crate) fn take_data(self) -> (Vec<u8>, Vec<Subroutine>, SongBcTracking) {
        (
            self.song_data,
            self.subroutines,
            SongBcTracking {
                bytecode: self.bytecode_tracker,
                cursor_tracker: self.cursor_tracker,
                first_channel_bc_offset: self.first_channel_bc_offset.unwrap_or(u16::MAX),
            },
        )
    }

    #[cfg(not(feature = "mml_tracking"))]
    pub(crate) fn take_data(self) -> (Vec<u8>, Vec<Subroutine>) {
        (self.song_data, self.subroutines)
    }

    fn parse_and_compile_tail_call(
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) -> Option<MmlCommandWithPos> {
        // ::TODO refactor to remove this hack::
        // ::HACK to peek into parser tokens without a mutable borrow::
        let mut next = parser.next();

        while let Some(c) = next {
            next = parser.next();

            if next.is_none() && matches!(c.command(), MmlCommand::CallSubroutine(_, _)) {
                return Some(c);
            }
            Self::_compile_command(
                c,
                parser,
                gen,
                #[cfg(feature = "mml_tracking")]
                bytecode_tracker,
            );
        }
        None
    }

    fn parse_and_compile(
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) {
        while let Some(c) = parser.next() {
            Self::_compile_command(
                c,
                parser,
                gen,
                #[cfg(feature = "mml_tracking")]
                bytecode_tracker,
            );
        }
    }

    fn _compile_command(
        c: MmlCommandWithPos,
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) {
        match gen.process_command(c.command()) {
            Ok(()) => (),
            Err(e) => parser.add_error_range(c.pos().clone(), e),
        }

        match c.command() {
            MmlCommand::EndLoop(_) | MmlCommand::BytecodeAsm(_) => {
                parser.set_tick_counter(gen.bc.get_tick_counter_with_loop_flag());
            }
            _ => (),
        }

        #[cfg(feature = "mml_tracking")]
        bytecode_tracker.push(BytecodePos {
            bc_end_pos: gen.bc.get_bytecode_len().try_into().unwrap_or(0xffff),
            char_index: c.pos().index_start,
        });
    }

    pub fn parse_and_compile_song_subroutione(
        &mut self,
        identifier: IdentifierStr<'a>,
        tokens: MmlTokens,
    ) -> Result<(), MmlChannelError> {
        // Index in SongData, not mml file
        let song_subroutine_index = self.subroutines.len().try_into().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        let mut parser = Parser::new(
            ChannelId::Subroutine(song_subroutine_index),
            tokens,
            &self.mml_instrument_map,
            Some((&self.subroutine_map, self.subroutine_name_map)),
            self.default_zenlen,
            None, // No sections in subroutines
            #[cfg(feature = "mml_tracking")]
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.mml_file,
            self.data_instruments,
            self.mml_instruments,
            Some(&self.subroutines),
            BytecodeContext::SongSubroutine,
        );

        let tail_call = Self::parse_and_compile_tail_call(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        let terminator = match (&gen.mp, gen.bc.get_state().vibrato.is_active()) {
            (MpState::Mp(_), true) | (MpState::Disabled, true) => {
                // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                // less Audio-RAM then `disable_vibraro` + `goto_relative``
                if let Some(tc) = tail_call {
                    Self::_compile_command(
                        tc,
                        &mut parser,
                        &mut gen,
                        #[cfg(feature = "mml_tracking")]
                        &mut self.bytecode_tracker,
                    );
                }
                BcTerminator::ReturnFromSubroutineAndDisableVibrato
            }
            (MpState::Mp(_), false) | (MpState::Disabled, false) | (MpState::Manual, _) => {
                match tail_call {
                    Some(tc) => match tc.command() {
                        MmlCommand::CallSubroutine(
                            s,
                            SubroutineCallType::Mml | SubroutineCallType::Asm,
                        ) => {
                            let sub = self.subroutines.get(*s).unwrap();
                            BcTerminator::TailSubroutineCall(
                                sub.bytecode_offset.into(),
                                &sub.subroutine_id,
                            )
                        }
                        MmlCommand::CallSubroutine(_, SubroutineCallType::AsmDisableVibrato) => {
                            // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                            // less Audio-RAM then `disable_vibraro` + `goto_relative``
                            Self::_compile_command(
                                tc,
                                &mut parser,
                                &mut gen,
                                #[cfg(feature = "mml_tracking")]
                                &mut self.bytecode_tracker,
                            );
                            BcTerminator::ReturnFromSubroutine
                        }
                        _ => panic!("tail_call is not a CallSubroutine command"),
                    },
                    None => BcTerminator::ReturnFromSubroutine,
                }
            }
        };

        let last_pos = parser.peek_pos();

        assert!(gen.loop_point.is_none());

        let (bc_data, bc_state) = match gen.bc.bytecode(terminator) {
            Ok((d, s)) => (d, Some(s)),
            Err((e, d)) => {
                parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
                (d, None)
            }
        };
        self.song_data = bc_data;

        let (_, errors) = parser.finalize();

        if errors.is_empty() && bc_state.is_some() {
            let bc_state = bc_state.unwrap();

            let changes_song_tempo = !bc_state.tempo_changes.is_empty();
            let subroutine_id = SubroutineId::new(song_subroutine_index, bc_state);

            self.subroutine_map
                .insert(identifier, Some(subroutine_id.clone()));

            self.subroutines.push(Subroutine {
                identifier: identifier.to_owned(),
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                subroutine_id,
                changes_song_tempo,
            });

            Ok(())
        } else {
            self.subroutine_map.insert(identifier, None);

            Err(MmlChannelError {
                identifier: identifier.to_owned(),
                errors,
            })
        }
    }

    pub fn parse_and_compile_song_channel(
        &mut self,
        tokens: MmlTokens,
        identifier: IdentifierStr<'a>,
    ) -> Result<Channel, MmlChannelError> {
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
            tokens,
            &self.mml_instrument_map,
            Some((&self.subroutine_map, self.subroutine_name_map)),
            self.default_zenlen,
            Some(self.sections),
            #[cfg(feature = "mml_tracking")]
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.mml_file,
            self.data_instruments,
            self.mml_instruments,
            Some(&self.subroutines),
            BytecodeContext::SongChannel,
        );

        Self::parse_and_compile(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        let last_pos = parser.peek_pos();
        let tick_counter = gen.bc.get_tick_counter();

        let terminator = match gen.loop_point {
            None => BcTerminator::DisableChannel,
            Some(lp) => {
                if lp.tick_counter == tick_counter {
                    parser.add_error_range(last_pos.to_range(1), MmlError::NoTicksAfterLoopPoint);
                }
                BcTerminator::Goto(lp.bytecode_offset)
            }
        };

        let (bc_data, bc_state) = match gen.bc.bytecode(terminator) {
            Ok((b, s)) => (b, Some(s)),
            Err((e, b)) => {
                parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
                (b, None)
            }
        };
        self.song_data = bc_data;

        let (section_tick_counters, errors) = parser.finalize();

        if errors.is_empty() && bc_state.is_some() {
            let bc_state = bc_state.unwrap();

            Ok(Channel {
                name: identifier.as_str().chars().next().unwrap(),
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                loop_point: gen.loop_point,
                tick_counter: bc_state.tick_counter,
                max_stack_depth: bc_state.max_stack_depth,
                section_tick_counters,
                tempo_changes: bc_state.tempo_changes,
            })
        } else {
            Err(MmlChannelError {
                identifier: identifier.to_owned(),
                errors,
            })
        }
    }
}

pub fn parse_and_compile_sound_effect(
    mml_file: &str,
    tokens: MmlTokens,
    pitch_table: &PitchTable,
    mml_instruments: &Vec<MmlInstrument>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    instruments_map: &HashMap<IdentifierStr, usize>,
) -> Result<MmlSoundEffect, Vec<ErrorWithPos<MmlError>>> {
    #[cfg(feature = "mml_tracking")]
    let mut cursor_tracker = CursorTracker::new();

    let mut parser = Parser::new(
        ChannelId::SoundEffect,
        tokens,
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
        mml_file,
        data_instruments,
        mml_instruments,
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

    let last_pos = parser.peek_pos();
    let tick_counter = gen.bc.get_tick_counter();

    assert!(gen.loop_point.is_none());

    let bytecode = match gen.bc.bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
            b
        }
    };

    let (_, mut errors) = parser.finalize();

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            last_pos.to_range(1),
            MmlError::TooManySfxTicks(tick_counter),
        ));
    }

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
