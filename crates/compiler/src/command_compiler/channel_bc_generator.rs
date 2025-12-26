//! Audio driver channel commands bytecode generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::*;

use crate::bytecode::{
    self, BcTerminator, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, BytecodeContext, DetuneValue,
    IeState, InstrumentId, LoopCount, PlayNoteTicks, PlayPitchPitch, RelativeVolume,
    SlurredNoteState, VibratoPitchOffsetPerTick, VibratoState, KEY_OFF_TICK_DELAY,
};
use crate::bytecode_assembler::parse_asm_line;
use crate::command_compiler::analysis::{
    AnalysedSoundEffectCommands, SubroutineAnalysis, TransposeStartRange,
};
use crate::command_compiler::subroutines::SubroutineCommandsWithCompileOrder;
use crate::data::{self, UniqueNamesList};
use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::echo::EchoEdl;
use crate::envelope::{Adsr, Envelope, Gain, TempGain};
use crate::errors::{
    ChannelError, ErrorWithPos, MmlChannelError, MmlCompileErrors, MmlPrefixError,
    SoundEffectErrorList, ValueError,
};
use crate::identifier::MusicChannelIndex;
use crate::mml::{CommandTickTracker, CursorTracker, MAX_MML_PREFIX_TICKS};
use crate::notes::Note;
use crate::notes::SEMITONES_PER_OCTAVE;
use crate::pitch_table::{PitchTable, PitchTableOffset, PITCH_REGISTER_MAX};
use crate::songs::{BytecodePos, Channel, LoopPoint};
use crate::sound_effects::MAX_SFX_TICKS;
use crate::subroutines::{
    BlankSubroutineMap, CompiledSubroutines, GetSubroutineResult, Subroutine, SubroutineState,
};
use crate::time::{CommandTicks, TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::UnsignedValueNewType;

pub const MAX_NO_LOOP_TICKS: CommandTicks = CommandTicks::new(256 * 32);

#[derive(Debug, Clone, Copy)]
enum DetuneCentsOutput {
    Manual,
    Automatic(DetuneValue),
}

impl DetuneCentsOutput {
    fn detune_value(&self, bc_state: &bytecode::State) -> DetuneValue {
        match self {
            Self::Automatic(d) => *d,
            Self::Manual => match bc_state.detune {
                IeState::Unset => DetuneValue::ZERO,
                IeState::Known(d) => d,
                IeState::MaybeInLoop(d, _) => d,
                // ::TODO remove and make looping with changed detune invalid::
                IeState::Unknown => DetuneValue::ZERO,
            },
        }
    }
}

#[derive(Debug)]
enum NoteOrPitchOut {
    Note(DetuneCentsOutput, Note, DetuneValue),
    Pitch(PlayPitchPitch),
}

#[derive(Clone, PartialEq)]
enum MpState {
    Disabled,
    Manual,
    Mp(MpVibrato),
}

/// Waits and rests after a play-note/portamento/etc instructions
enum AfterPlayNote {
    Rest {
        ticks: CommandTicks,
    },
    WaitAfterKeyoff {
        ticks_after_keyoff: CommandTicks,
    },
    TempGain {
        temp_gain: TempGain,
        ticks_until_keyoff: CommandTicks,
        ticks_after_keyoff: TicksAfterKeyoff,
    },
    EnableKeyOn,
}

struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    subroutines: &'a CompiledSubroutines,

    bc: Bytecode<'a>,

    detune_cents: DetuneCents,
    mp: MpState,
    quantize: Quantize,
    keyoff_enabled: bool,

    loop_point: Option<LoopPoint>,

    prev_ticks: TickCounterWithLoopFlag,
    tick_tracker: CommandTickTracker,
}

impl<'a> ChannelBcGenerator<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn new(
        bc_data: Vec<u8>,
        pitch_table: &'a PitchTable,
        instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        subroutines: &'a CompiledSubroutines,
        context: BytecodeContext,
        driver_transpose: TransposeStartRange,
    ) -> ChannelBcGenerator<'a> {
        ChannelBcGenerator {
            pitch_table,
            instruments,
            subroutines,
            bc: Bytecode::new_append_to_vec(
                bc_data,
                context,
                instruments,
                pitch_table,
                subroutines,
                // Using BlankSubroutineMap here to forbid direct subroutine calls in bytecode assembly
                // (\asm subroutine calls must go through `Self::call_subroutine()`)
                &BlankSubroutineMap,
                driver_transpose,
            ),
            mp: MpState::Manual,
            detune_cents: DetuneCents::ZERO,
            quantize: Quantize::None,
            keyoff_enabled: true,
            loop_point: None,

            prev_ticks: TickCounterWithLoopFlag::default(),
            tick_tracker: CommandTickTracker::new(),
        }
    }

    fn calculate_vibrato_for_note(
        &self,
        mp: &MpVibrato,
        note: Note,
        detune: DetuneCentsOutput,
    ) -> Result<ManualVibrato, ChannelError> {
        if self.bc.is_driver_transpose_active() {
            return Err(ChannelError::MpVibratoWithDriverTransposeActive);
        }

        if mp.depth_in_cents == 0 {
            return Err(ChannelError::MpDepthZero);
        }
        let instrument_tuning = match self.bc.get_state().instrument_tuning {
            Some(t) => t,
            None => return Err(ChannelError::CannotUseMpWithUnknownInstrumentTuning),
        };

        let detune = detune.detune_value(self.bc.get_state());

        let pitch = self
            .pitch_table
            .get_pitch(instrument_tuning, note)
            .wrapping_add_signed(detune.as_i16());

        // Calculate the minimum and maximum pitches of the vibrato.
        // This produces more accurate results when cents is very large (ie, 400)
        let pow = f64::from(mp.depth_in_cents) / f64::from(SEMITONES_PER_OCTAVE as u32 * 100);
        let p1 = f64::from(pitch) * 2.0_f64.powf(-pow);
        let p2 = f64::from(pitch) * 2.0_f64.powf(pow);

        let qwt = u32::from(mp.quarter_wavelength_ticks.as_u8());

        let po_per_tick = f64::round((p2 - p1) / f64::from(qwt * 2));
        let po_per_tick = if po_per_tick > 1.0 { po_per_tick } else { 1.0 };

        if po_per_tick > u32::MAX.into() {
            return Err(ChannelError::MpPitchOffsetTooLarge(u32::MAX));
        }
        let po_per_tick = po_per_tick as u32;

        match po_per_tick.try_into() {
            Ok(po) => Ok(ManualVibrato {
                pitch_offset_per_tick: po,
                quarter_wavelength_ticks: mp.quarter_wavelength_ticks,
                delay: mp.delay,
            }),
            Err(_) => Err(ChannelError::MpPitchOffsetTooLarge(po_per_tick)),
        }
    }

    fn calculate_detune_for_note(&self, note: Note) -> Result<DetuneCentsOutput, ChannelError> {
        match self.detune_cents.as_i16() {
            0 => Ok(DetuneCentsOutput::Manual),
            cents => {
                if self.bc.is_driver_transpose_active() {
                    return Err(ChannelError::DetuneCentsWithDriverTransposeActive);
                }

                let instrument_tuning = match self.bc.get_state().instrument_tuning {
                    Some(i) => i,
                    None => {
                        return Err(ChannelError::CannotUseDetuneCentsWithUnknownInstrumentTuning)
                    }
                };
                let pitch = self.pitch_table.get_pitch(instrument_tuning, note);
                let pow = f64::from(cents) / f64::from(SEMITONES_PER_OCTAVE as u32 * 100);
                let d = f64::round(f64::from(pitch) * 2.0_f64.powf(pow) - f64::from(pitch));

                if d < i32::MIN.into() || d > i32::MAX.into() {
                    return Err(ChannelError::DetuneCentsTooLargeForNote(i32::MAX));
                }
                let d = d as i32;

                match d.try_into() {
                    Ok(d) => Ok(DetuneCentsOutput::Automatic(d)),
                    Err(_) => Err(ChannelError::DetuneCentsTooLargeForNote(d)),
                }
            }
        }
    }

    fn emit_detune_output(&mut self, detune: DetuneCentsOutput) {
        match detune {
            DetuneCentsOutput::Manual => (),
            DetuneCentsOutput::Automatic(d) => {
                self.set_detune_if_changed(d);
            }
        }
    }

    fn set_detune_if_changed(&mut self, detune: DetuneValue) {
        let state = self.bc.get_state();
        if !state.detune.is_known_and_eq(&detune) {
            self.bc.set_detune(detune);
        }
    }

    fn broken_chord_play_note_or_pitch_with_detune(
        &mut self,
        note: NoteOrPitch,
        length: PlayNoteTicks,
    ) -> Result<(), ChannelError> {
        if self.keyoff_enabled || length.is_slur() {
            match note {
                NoteOrPitch::Note(n) => {
                    self.emit_detune_output(self.calculate_detune_for_note(n)?);
                    self.bc.play_note(n, length)?;
                }
                NoteOrPitch::Pitch(p) => {
                    // Pitch is not detuned
                    self.bc.play_pitch(p, length)?;
                }
                NoteOrPitch::PitchFrequency(f) => {
                    // Pitch is not detuned
                    let p = f.to_vxpitch(self.bc.get_instrument())?;
                    self.bc.play_pitch(p, length)?;
                }
            }
        } else {
            // key-off is disabled
            let ticks = match length {
                PlayNoteTicks::KeyOff(l) => l.ticks(),
                PlayNoteTicks::NoKeyOff(_) => panic!(),
            };
            let length = PlayNoteTicks::NoKeyOff(ticks.try_into()?);

            match note {
                NoteOrPitch::Note(n) => {
                    self.emit_detune_output(self.calculate_detune_for_note(n)?);
                    self.bc.play_note(n, length)?;
                }
                NoteOrPitch::Pitch(p) => {
                    // Pitch is not detuned
                    self.bc.play_pitch(p, length)?;
                }
                NoteOrPitch::PitchFrequency(f) => {
                    // Pitch is not detuned
                    let p = f.to_vxpitch(self.bc.get_instrument())?;
                    self.bc.play_pitch(p, length)?;
                }
            }

            self.bc.keyon_next_note();
        }

        Ok(())
    }

    fn split_play_note_length(
        &self,
        length: CommandTicks,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    ) -> Result<(PlayNoteTicks, AfterPlayNote), ValueError> {
        let rest_after_note = rest_after_note.0;

        let no_quantize_or_slur_keyoff = || {
            let pn_ticks = PlayNoteTicks::try_from_is_slur(length, false)?;
            Ok((
                pn_ticks,
                AfterPlayNote::WaitAfterKeyoff {
                    ticks_after_keyoff: rest_after_note,
                },
            ))
        };

        match (is_slur, &self.quantize) {
            (true, _) => {
                let pn_ticks = PlayNoteTicks::try_from_is_slur(length, true)?;
                Ok((
                    pn_ticks,
                    AfterPlayNote::Rest {
                        ticks: rest_after_note,
                    },
                ))
            }
            (false, Quantize::None) => match self.keyoff_enabled {
                true => no_quantize_or_slur_keyoff(),
                false => {
                    if rest_after_note.is_zero() {
                        // No rest, slur note and emit a `keyon_next_note` instruction.
                        let pn_ticks = PlayNoteTicks::try_from_is_slur(length, true)?;
                        Ok((pn_ticks, AfterPlayNote::EnableKeyOn))
                    } else {
                        // Join note + rest and do not emit the `keyon_next_note` instruction.
                        // ::TODO remove return Err on checked_add overflow::
                        let pn_ticks = PlayNoteTicks::try_from_is_slur(
                            length.try_add(rest_after_note)?,
                            false,
                        )?;

                        Ok((
                            pn_ticks,
                            AfterPlayNote::WaitAfterKeyoff {
                                ticks_after_keyoff: CommandTicks::new(0),
                            },
                        ))
                    }
                }
            },
            (false, &Quantize::Rest(q)) => {
                let l = length.value();
                let note_length = q.quantize(l) + KEY_OFF_TICK_DELAY;

                if note_length < l {
                    let note_length = CommandTicks::new(note_length);
                    // ::TODO remove return Err on checked_add overflow::
                    let ticks_after_keyoff =
                        CommandTicks::new(l - note_length.value()).try_add(rest_after_note)?;
                    let pn_ticks = PlayNoteTicks::try_from_is_slur(note_length, false)?;
                    Ok((
                        pn_ticks,
                        AfterPlayNote::WaitAfterKeyoff { ticks_after_keyoff },
                    ))
                } else {
                    // Note is too short to be quantized
                    no_quantize_or_slur_keyoff()
                }
            }
            (false, &Quantize::WithTempGain(q, temp_gain)) => {
                let l = length.value();
                let pn_length = q.quantize(l);

                if pn_length + KEY_OFF_TICK_DELAY < l {
                    let note_length = CommandTicks::new(pn_length);
                    let ticks_until_keyoff = CommandTicks::new(l - note_length.value());
                    let pn_ticks = PlayNoteTicks::try_from_is_slur(note_length, true)?;
                    Ok((
                        pn_ticks,
                        AfterPlayNote::TempGain {
                            temp_gain,
                            ticks_until_keyoff,
                            ticks_after_keyoff: TicksAfterKeyoff(rest_after_note),
                        },
                    ))
                } else {
                    // Note is too short to be quantized
                    no_quantize_or_slur_keyoff()
                }
            }
        }
    }

    fn after_note(&mut self, after: AfterPlayNote) -> Result<(), ChannelError> {
        match after {
            AfterPlayNote::Rest { ticks } => self.maybe_rest(ticks),
            AfterPlayNote::WaitAfterKeyoff { ticks_after_keyoff } => {
                debug_assert!(self.bc.get_state().prev_slurred_note.is_not_slurred());
                if !ticks_after_keyoff.is_zero() {
                    self.wait(ticks_after_keyoff)
                } else {
                    Ok(())
                }
            }
            AfterPlayNote::TempGain {
                temp_gain,
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => self.temp_gain_and_rest(Some(temp_gain), ticks_until_keyoff, ticks_after_keyoff),
            AfterPlayNote::EnableKeyOn => {
                self.bc.keyon_next_note();
                Ok(())
            }
        }
    }

    fn play_note_with_mp_and_detune_cents(
        &mut self,
        note: Note,
        pn_ticks: PlayNoteTicks,
    ) -> Result<(), ChannelError> {
        let detune = self.calculate_detune_for_note(note)?;
        self.emit_detune_output(detune);

        let vibrato = self.bc.get_state().vibrato;

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_ticks)?;
            }
            MpState::Disabled => {
                const POPT: VibratoPitchOffsetPerTick = VibratoPitchOffsetPerTick::new(0);

                let vibrato_disabled = match vibrato {
                    VibratoState::Unchanged => true,
                    VibratoState::Unknown => false,
                    VibratoState::Disabled => true,
                    VibratoState::Set(v_popt, _, _) => v_popt == POPT,
                };

                if vibrato_disabled {
                    self.bc.play_note(note, pn_ticks)?;
                } else {
                    self.bc
                        .set_vibrato_depth_and_play_note(POPT, note, pn_ticks)?;
                }

                // Switching MpState to Manual to skip the `vibrato_disabled` checks on subsequent notes.
                // This must be done outside of the loop in case the `MP0` is after a `:` skip-last-loop command.
                // (see `test_mp0_after_skip_last_loop()` test)
                if !self.bc.is_in_loop() {
                    self.mp = MpState::Manual;
                }
            }
            MpState::Mp(mp) => {
                let cv = self.calculate_vibrato_for_note(mp, note, detune)?;

                match vibrato {
                    v if v
                        == VibratoState::Set(
                            cv.pitch_offset_per_tick,
                            cv.quarter_wavelength_ticks,
                            cv.delay,
                        ) =>
                    {
                        self.bc.play_note(note, pn_ticks)?;
                    }
                    VibratoState::Set(_, qwt, delay)
                        if qwt == cv.quarter_wavelength_ticks && delay == cv.delay =>
                    {
                        self.bc.set_vibrato_depth_and_play_note(
                            cv.pitch_offset_per_tick,
                            note,
                            pn_ticks,
                        )?;
                    }
                    _ => {
                        self.bc.set_vibrato_with_delay(
                            cv.pitch_offset_per_tick,
                            cv.quarter_wavelength_ticks,
                            cv.delay,
                        );
                        self.bc.play_note(note, pn_ticks)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn maybe_rest(&mut self, length: CommandTicks) -> Result<(), ChannelError> {
        if !length.is_zero() {
            self.bc.rest(length.try_into()?);
        }
        Ok(())
    }

    fn wait_after_keyoff(&mut self, length: TicksAfterKeyoff) -> Result<(), ChannelError> {
        debug_assert!(self.bc.get_state().prev_slurred_note.is_not_slurred());
        if !length.0.is_zero() {
            self.bc.wait(length.0.try_into()?);
        }
        Ok(())
    }

    fn rest(&mut self, length: CommandTicks) -> Result<(), ChannelError> {
        self.bc.rest(length.try_into()?);

        Ok(())
    }

    // sleep with no keyoff
    fn wait(&mut self, length: CommandTicks) -> Result<(), ChannelError> {
        self.bc.wait(length.try_into()?);

        Ok(())
    }

    fn portamento_note_pitch(
        &self,
        note: NoteOrPitch,
        instrument_tuning: Option<PitchTableOffset>,
    ) -> Result<(Option<u16>, NoteOrPitchOut), ChannelError> {
        match note {
            NoteOrPitch::Note(note) => {
                let detune_output = self.calculate_detune_for_note(note)?;

                let detune = detune_output.detune_value(self.bc.get_state());

                match (self.bc.is_driver_transpose_active(), instrument_tuning) {
                    (false, Some(instrument_tuning)) => {
                        let pitch = self
                            .pitch_table
                            .get_pitch(instrument_tuning, note)
                            .wrapping_add_signed(detune.as_i16());

                        Ok((
                            Some(pitch),
                            NoteOrPitchOut::Note(detune_output, note, detune),
                        ))
                    }
                    (false, None) | (true, _) => {
                        Ok((None, NoteOrPitchOut::Note(detune_output, note, detune)))
                    }
                }
            }
            NoteOrPitch::Pitch(p) => Ok((Some(p.as_u16()), NoteOrPitchOut::Pitch(p))),
            NoteOrPitch::PitchFrequency(f) => {
                let p = f.to_vxpitch(self.bc.get_instrument())?;
                Ok((Some(p.as_u16()), NoteOrPitchOut::Pitch(p)))
            }
        }
    }

    fn portamento_play_pn_note_out(
        &mut self,
        note: NoteOrPitchOut,
        t: PlayNoteTicks,
    ) -> Result<(), ChannelError> {
        match note {
            NoteOrPitchOut::Note(dco, n, _) => {
                self.emit_detune_output(dco);
                self.bc.play_note(n, t)?;
            }
            NoteOrPitchOut::Pitch(p) => {
                self.bc.play_pitch(p, t)?;
            }
        }
        Ok(())
    }

    fn portamento_get_prev_note_and_pitch(
        &self,
    ) -> Result<(NoteOrPitch, Option<u16>), ChannelError> {
        match self.bc.get_state().prev_slurred_note {
            SlurredNoteState::Slurred(prev_note, prev_detune, prev_tuning) => {
                match (self.bc.is_driver_transpose_active(), prev_tuning) {
                    (false, Some(prev_tuning)) => {
                        let prev_pitch = self
                            .pitch_table
                            .get_pitch(prev_tuning, prev_note)
                            .wrapping_add_signed(prev_detune.as_i16());

                        Ok((NoteOrPitch::Note(prev_note), Some(prev_pitch)))
                    }
                    (false, None) | (true, _) => Ok((NoteOrPitch::Note(prev_note), None)),
                }
            }
            SlurredNoteState::SlurredPitch(prev) => {
                Ok((NoteOrPitch::Pitch(prev), Some(prev.as_u16())))
            }

            SlurredNoteState::Unchanged => Err(ChannelError::OneNotePortamentoNoPreviousNote),
            SlurredNoteState::Unknown => Err(ChannelError::OneNotePortamentoPreviousNoteIsUnknown),
            SlurredNoteState::None => Err(ChannelError::OneNotePortamentoPreviousNoteIsNotSlurred),
            SlurredNoteState::SlurredNoise => {
                Err(ChannelError::OneNotePortamentoPreviousNoteIsNoise)
            }
        }
    }

    fn portamento_prev_note_matches(
        &self,
        note1_pitch: Option<u16>,
        note1: &NoteOrPitchOut,
    ) -> bool {
        match self.bc.get_state().prev_slurred_note {
            SlurredNoteState::Slurred(prev_note, prev_detune, prev_tuning) => {
                match (prev_tuning, note1_pitch) {
                    // Previous and current VxPITCH is known
                    (Some(prev_tuning), Some(note1_pitch)) => {
                        let prev_pitch = self
                            .pitch_table
                            .get_pitch(prev_tuning, prev_note)
                            .wrapping_add_signed(prev_detune.as_i16());

                        note1_pitch == prev_pitch
                    }
                    // No instrument set on both notes.  Only accepted if poramento speed set.
                    (None, None) => match note1 {
                        &NoteOrPitchOut::Note(_, n1_note, n1_detune) => {
                            (n1_note, n1_detune) == (prev_note, prev_detune)
                        }
                        NoteOrPitchOut::Pitch(_) => false,
                    },
                    // Invalid instrument after previous note
                    (Some(_), None) => false,
                    // Instrument set after previous note
                    (None, Some(_)) => false,
                }
            }
            SlurredNoteState::SlurredPitch(prev) => Some(prev.as_u16()) == note1_pitch,

            SlurredNoteState::Unchanged => false,
            SlurredNoteState::Unknown => false,
            SlurredNoteState::None => false,
            SlurredNoteState::SlurredNoise => false,
        }
    }

    // Returns new slide_length
    // A tick from the slide_length may be removed if a key-on is required and delay = 0
    fn portamento_play_note1_delay_if_required(
        &mut self,
        note1: NoteOrPitch,
        instrument_tuning: Option<PitchTableOffset>,
        delay_length: CommandTicks,
        slide_length: CommandTicks,
    ) -> Result<(Option<u16>, CommandTicks), ChannelError> {
        let (note1_pitch, pn1) = self.portamento_note_pitch(note1, instrument_tuning)?;

        let play_note1 = !self.portamento_prev_note_matches(note1_pitch, &pn1);

        // Play note1 (if required)
        let slide_length = match (play_note1, delay_length.value()) {
            (true, 0) => {
                // Play note1 for a single tick
                debug_assert_eq!(BcTicksNoKeyOff::MIN.ticks(), 1);
                let t = PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::MIN);

                self.portamento_play_pn_note_out(pn1, t)?;

                // subtract 1 tick from slide_length
                CommandTicks::new(slide_length.value().saturating_sub(1))
            }
            (true, _) => {
                let pn_length = PlayNoteTicks::try_from_is_slur(delay_length, true)?;

                self.portamento_play_pn_note_out(pn1, pn_length)?;

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

        Ok((note1_pitch, slide_length))
    }

    #[allow(clippy::too_many_arguments)]
    fn portamento(
        &mut self,
        note1: Option<NoteOrPitch>,
        note2: NoteOrPitch,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        delay_length: CommandTicks,
        slide_length: CommandTicks,
        tie_length: CommandTicks,
        rest_after_note: RestTicksAfterNote,
    ) -> Result<(), ChannelError> {
        #[cfg(debug_assertions)]
        let expected_tick_counter = self.bc.get_tick_counter()
            + delay_length.into()
            + slide_length.into()
            + tie_length.into()
            + rest_after_note.0.into();

        let instrument_tuning = self.bc.get_state().instrument_tuning;

        let (note1, note1_pitch, slide_length) = match note1 {
            None => {
                let (n, p) = self.portamento_get_prev_note_and_pitch()?;
                if !delay_length.is_zero() {
                    self.wait(delay_length)?;
                }
                (n, p, slide_length)
            }
            Some(note1) => {
                let (p, s) = self.portamento_play_note1_delay_if_required(
                    note1,
                    instrument_tuning,
                    delay_length,
                    slide_length,
                )?;
                (note1, p, s)
            }
        };

        let (note2_pitch, pn2) = self.portamento_note_pitch(note2, instrument_tuning)?;

        // slide_length: number of ticks in the pitch-slide.  Does NOT include the key-off tick.
        // tie_length: number of ticks after pitch-slide reaches note2, includes the key-off tick (as required).
        let (slide_length, tie_length) = if !is_slur && tie_length.is_zero() {
            (
                CommandTicks::new(slide_length.value().saturating_sub(1)),
                CommandTicks::new(1),
            )
        } else {
            (slide_length, tie_length)
        };

        if slide_length.is_zero() {
            return Err(ChannelError::PortamentoTooShort);
        }
        if slide_length.value() > MAX_PORTAMENTO_SLIDE_TICKS {
            const _: () = assert!(
                (MAX_PORTAMENTO_SLIDE_TICKS as u64) * (PITCH_REGISTER_MAX as u64)
                    < (i32::MAX as u64) / 2,
                "Portamento velocity can overflow"
            );

            return Err(ChannelError::PortamentoTooLong);
        }

        let velocity = match speed_override {
            Some(speed) => {
                let direction = if let (Some(p1), Some(p2)) = (note1_pitch, note2_pitch) {
                    p1 < p2
                } else if let (NoteOrPitch::Note(n1), NoteOrPitch::Note(n2)) = (note1, note2) {
                    n1 < n2
                } else {
                    return Err(ChannelError::PortamentoNoteAndPitchWithoutInstrument);
                };

                const _: () = assert!(PortamentoSpeed::MIN.as_u8() > 0);
                let v = if direction {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                };
                Some(v.try_into()?)
            }
            None => {
                match (note1_pitch, note2_pitch) {
                    (Some(note1_pitch), Some(note2_pitch)) => {
                        let delta = i32::from(note2_pitch) - i32::from(note1_pitch);

                        let ticks = i32::from(slide_length.value());
                        assert!(ticks > 0);

                        // division with rounding
                        let v = if delta > 0 {
                            (delta + (ticks / 2)) / ticks
                        } else {
                            (delta - (ticks / 2)) / ticks
                        };

                        Some(v.try_into()?)
                    }
                    // Instrument is unknown, audio driver will calculate velocity
                    _ => None,
                }
            }
        };

        // In PMDMML only the portamento slide is quantized, delay_length is not.
        let (p_length, after) = self.split_play_note_length(
            // ::TODO remove checked_add in portamento::
            slide_length.try_add(tie_length)?,
            is_slur,
            rest_after_note,
        )?;

        match velocity {
            Some(velocity) => match pn2 {
                NoteOrPitchOut::Note(dco, n, _) => {
                    self.emit_detune_output(dco);
                    self.bc.portamento(n, velocity, p_length)?;
                }
                NoteOrPitchOut::Pitch(p) => {
                    self.bc.portamento_pitch(p, velocity, p_length)?;
                }
            },
            None => {
                let slide_length = match u32::from(slide_length.value()).try_into() {
                    Ok(s) => s,
                    Err(ValueError::PortamentoSlideTicksOutOfRange(v)) => {
                        return Err(ChannelError::PortamentoCalcSlideTicksOutOfRange {
                            ticks: v,
                            tuning_unknown: instrument_tuning.is_none(),
                            transpose_active: self.bc.is_driver_transpose_active(),
                        });
                    }
                    Err(e) => return Err(e.into()),
                };

                match pn2 {
                    NoteOrPitchOut::Note(dco, n, _) => {
                        self.emit_detune_output(dco);
                        self.bc.portamento_calc(n, slide_length, p_length)?;
                    }
                    NoteOrPitchOut::Pitch(p) => {
                        self.bc.portamento_pitch_calc(p, slide_length, p_length)?;
                    }
                }
            }
        }

        self.after_note(after)?;

        #[cfg(debug_assertions)]
        debug_assert_eq!(self.bc.get_tick_counter(), expected_tick_counter);

        Ok(())
    }

    fn broken_chord(
        &mut self,
        notes: &[NoteOrPitch],
        total_length: CommandTicks,
        note_length: PlayNoteTicks,
        slur_last_note: bool,
    ) -> Result<(), ChannelError> {
        if notes.is_empty() {
            return Err(ChannelError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(ChannelError::TooManyNotesInBrokenChord(notes.len()));
        }
        let n_notes: u16 = notes.len().try_into().unwrap();

        let expected_tick_counter = self.bc.get_tick_counter() + total_length.into();

        let total_ticks = total_length.value();

        // Number of ticks in the last note played outside the loop (if any).
        let last_note_ticks = {
            let mut t = total_ticks % note_length.ticks();

            if slur_last_note != note_length.is_slur() {
                // When slur_last_note is false: A keyoff is required after the loop.
                // When slur_last_note is true: A slurred note is required after the loop.
                if t == 0 {
                    t = note_length.ticks();
                }
            }

            if !slur_last_note && t > 0 && t < BcTicksKeyOff::MIN.ticks() {
                t = BcTicksKeyOff::MIN.ticks();
            }

            t
        };

        if total_ticks < last_note_ticks {
            return Err(ChannelError::BrokenChordTotalLengthTooShort);
        }
        let notes_in_loop = (total_ticks - last_note_ticks) / note_length.ticks();

        let break_point = usize::from(notes_in_loop % n_notes);
        let has_break_point: bool = break_point != 0;

        let n_loops = u32::from(notes_in_loop / n_notes) + u32::from(has_break_point);

        if n_loops < 2 {
            return Err(ChannelError::BrokenChordTotalLengthTooShort);
        }

        let n_loops = LoopCount::try_from(n_loops)?;

        self.bc.start_loop(Some(n_loops), LoopAnalysis::BLANK)?;

        for (i, &n) in notes.iter().enumerate() {
            if i == break_point && i != 0 {
                self.bc.skip_last_loop(SkipLastLoopAnalysis::BLANK)?;
            }

            match self.broken_chord_play_note_or_pitch_with_detune(n, note_length) {
                Ok(()) => (),
                Err(e) => {
                    // Hides an unneeded "loop stack not empty" (or end_loop) error
                    let _ = self.bc.end_loop(None, LoopAnalysis::BLANK);
                    return Err(e);
                }
            }
        }

        self.bc.end_loop(None, LoopAnalysis::BLANK)?;

        if last_note_ticks > 0 {
            let l = match slur_last_note {
                false => PlayNoteTicks::KeyOff(last_note_ticks.try_into()?),
                true => PlayNoteTicks::NoKeyOff(last_note_ticks.try_into()?),
            };
            self.broken_chord_play_note_or_pitch_with_detune(notes[break_point], l)?;
        }

        if self.bc.get_tick_counter() != expected_tick_counter {
            return Err(ChannelError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument_hint(
        &mut self,
        inst_id: InstrumentId,
        envelope: Option<Envelope>,
    ) -> Result<(), ChannelError> {
        self.bc.set_subroutine_instrument_hint(inst_id, envelope)
    }

    // Will not be optimised away
    fn set_instrument_asm(&mut self, inst_id: InstrumentId, envelope: Option<Envelope>) {
        match envelope {
            None => self.bc.set_instrument(inst_id),
            Some(Envelope::Adsr(adsr)) => self.bc.set_instrument_and_adsr(inst_id, adsr),
            Some(Envelope::Gain(gain)) => self.bc.set_instrument_and_gain(inst_id, gain),
        }
    }

    fn set_instrument(
        &mut self,
        inst_id: InstrumentId,
        envelope: Option<Envelope>,
    ) -> Result<(), ChannelError> {
        // ::TODO optimise when adding sample swapping::
        let inst_envelope = match self.instruments.get_index(inst_id.value().into()) {
            Some(data::InstrumentOrSample::Instrument(i)) => i.envelope,
            Some(data::InstrumentOrSample::Sample(s)) => s.envelope,
            None => return Err(ChannelError::NoInstrument),
        };

        let old_inst = &self.bc.get_state().instrument;
        let old_envelope = &self.bc.get_state().envelope;

        if old_inst.is_known_and_eq(&inst_id) {
            let envelope = envelope.unwrap_or(inst_envelope);

            // InstrumentId unchanged, check envelope
            if !old_envelope.is_known_and_eq(&envelope) {
                if envelope == inst_envelope {
                    // Outputs fewer bytes then a `set_adsr` instruction.
                    self.bc.set_instrument(inst_id);
                } else {
                    match envelope {
                        Envelope::Adsr(adsr) => self.bc.set_adsr(adsr),
                        Envelope::Gain(gain) => self.bc.set_gain(gain),
                    }
                }
            }
        } else {
            match envelope {
                None => self.bc.set_instrument(inst_id),
                Some(e) if e == inst_envelope => self.bc.set_instrument(inst_id),
                Some(Envelope::Adsr(adsr)) => self.bc.set_instrument_and_adsr(inst_id, adsr),
                Some(Envelope::Gain(gain)) => self.bc.set_instrument_and_gain(inst_id, gain),
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
        ticks_until_keyoff: CommandTicks,
        ticks_after_keyoff: TicksAfterKeyoff,
    ) -> Result<(), ChannelError> {
        let l = ticks_until_keyoff.try_into()?;

        if temp_gain.is_some_and(|t| !self.bc.get_state().prev_temp_gain.is_known_and_eq(&t)) {
            let temp_gain = temp_gain.unwrap();

            self.bc.set_temp_gain_and_rest(temp_gain, l);
        } else {
            // Reuse temp GAIN
            self.bc.reuse_temp_gain_and_rest(l);
        }

        self.wait_after_keyoff(ticks_after_keyoff)?;

        Ok(())
    }

    fn temp_gain_and_wait(
        &mut self,
        temp_gain: Option<TempGain>,
        ticks: CommandTicks,
    ) -> Result<(), ChannelError> {
        let wait = ticks.value().try_into()?;

        if temp_gain.is_some_and(|t| !self.bc.get_state().prev_temp_gain.is_known_and_eq(&t)) {
            let temp_gain = temp_gain.unwrap();

            self.bc.set_temp_gain_and_wait(temp_gain, wait);
        } else {
            self.bc.reuse_temp_gain_and_wait(wait);
        }

        Ok(())
    }

    fn call_subroutine(
        &mut self,
        index: u8,
        disable_vibrato: SubroutineCallType,
    ) -> Result<(), ChannelError> {
        let (name, sub) = match self.subroutines.get(index.into()) {
            GetSubroutineResult::NotFound => panic!("subroutine not found"),
            GetSubroutineResult::Compiled(name, sub) => (name, sub),
            GetSubroutineResult::NotCompiled(name) => {
                return Err(ChannelError::CannotCallSubroutineRecursion(
                    name.as_str().to_owned(),
                ))
            }
            GetSubroutineResult::CompileError(_) => return Ok(()),
        };

        match (disable_vibrato, &self.mp) {
            (SubroutineCallType::Asm, _) => {
                self.bc.call_subroutine(name.as_str(), sub)?;
            }
            (SubroutineCallType::AsmDisableVibrato, _) => {
                self.bc
                    .call_subroutine_and_disable_vibrato(name.as_str(), sub)?;
            }
            (SubroutineCallType::Mml, MpState::Mp(_)) => {
                self.bc
                    .call_subroutine_and_disable_vibrato(name.as_str(), sub)?;
            }
            (SubroutineCallType::Mml, MpState::Disabled | MpState::Manual) => {
                self.bc.call_subroutine(name.as_str(), sub)?;
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
                self.bc.set_vibrato_with_delay(
                    v.pitch_offset_per_tick,
                    v.quarter_wavelength_ticks,
                    v.delay,
                );
            }
            None => {
                self.bc.disable_vibrato();
            }
        }
    }

    fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), ChannelError> {
        self.bc.set_song_tick_clock(tick_clock)?;
        Ok(())
    }

    fn _command(&mut self, command: &Command) -> Result<(), ChannelError> {
        match command {
            Command::None => (),

            Command::SetLoopPoint(analysis) => match self.bc.get_context() {
                BytecodeContext::SongChannel { .. } => {
                    if self.loop_point.is_some() {
                        return Err(ChannelError::LoopPointAlreadySet);
                    }
                    if self.bc.is_in_loop() {
                        return Err(ChannelError::CannotSetLoopPointInALoop);
                    }
                    self.loop_point = Some(LoopPoint {
                        bytecode_offset: self.bc.get_bytecode_len(),
                        tick_counter: self.bc.get_tick_counter(),
                    });

                    self.bc._song_loop_point(analysis);
                }
                BytecodeContext::SongSubroutine { .. }
                | BytecodeContext::SfxSubroutine
                | BytecodeContext::SoundEffect
                | BytecodeContext::MmlPrefix => return Err(ChannelError::CannotSetLoopPoint),

                #[cfg(feature = "test")]
                BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => {
                    return Err(ChannelError::CannotSetLoopPoint)
                }
            },

            &Command::SetSubroutineInstrumentHint(inst_id, envelope) => {
                self.set_instrument_hint(inst_id, envelope)?;
            }
            &Command::SetInstrumentAsm(inst_id, envelope) => {
                self.set_instrument_asm(inst_id, envelope);
            }
            &Command::SetInstrument(inst_id, envelope) => {
                self.set_instrument(inst_id, envelope)?;
            }

            &Command::SetAdsr(adsr) => self.set_adsr(adsr),
            &Command::SetGain(gain) => self.set_gain(gain),

            &Command::CallSubroutine(s_id, d) => {
                self.call_subroutine(s_id, d)?;
            }

            &Command::SetManualVibrato(v) => {
                self.set_manual_vibrato(v);
            }

            &Command::SetMpVibrato(mp) => {
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

            &Command::SetQuantize(q) => {
                self.quantize = q;
            }

            &Command::Rest {
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                self.rest(ticks_until_keyoff)?;
                self.wait_after_keyoff(ticks_after_keyoff)?;
            }

            &Command::Wait(length) => {
                self.wait(length)?;
            }

            &Command::TempGain(temp_gain) => {
                self.temp_gain(temp_gain);
            }
            &Command::TempGainAndRest {
                temp_gain,
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                self.temp_gain_and_rest(temp_gain, ticks_until_keyoff, ticks_after_keyoff)?;
            }
            &Command::TempGainAndWait(temp_gain, ticks) => {
                self.temp_gain_and_wait(temp_gain, ticks)?;
            }

            &Command::DisableEarlyRelease => {
                if !self.bc.get_state().early_release.is_known_and_eq(&None) {
                    self.bc.disable_early_release();
                }
            }
            &Command::SetEarlyRelease(ticks, min, gain) => {
                let state = self.bc.get_state();
                if !state
                    .early_release
                    .is_known_and_eq(&Some((ticks, min, gain)))
                {
                    self.bc.set_early_release(ticks, min, gain);
                }
            }

            &Command::SetTranspose(t) => {
                self.bc.set_transpose(t);
            }
            &Command::AdjustTranspose(t) => {
                self.bc.adjust_transpose(t)?;
            }

            &Command::SetDetune(detune) => {
                self.set_detune_if_changed(detune);
                self.detune_cents = DetuneCents::ZERO;
            }

            &Command::SetDetuneCents(detune_cents) => {
                self.detune_cents = detune_cents;

                if detune_cents == DetuneCents::ZERO {
                    self.set_detune_if_changed(DetuneValue::ZERO);
                }
            }

            &Command::PlayNote {
                note,
                length,
                is_slur,
                rest_after_note,
            } => {
                let (pn_ticks, after) =
                    self.split_play_note_length(length, is_slur, rest_after_note)?;

                self.play_note_with_mp_and_detune_cents(note, pn_ticks)?;
                self.after_note(after)?;
            }

            &Command::PlayPitch {
                pitch,
                length,
                is_slur,
                rest_after_note,
            } => {
                let (pp_length, after) =
                    self.split_play_note_length(length, is_slur, rest_after_note)?;

                self.bc.play_pitch(pitch, pp_length)?;
                self.after_note(after)?;
            }

            &Command::PlayPitchFrequency {
                frequency,
                length,
                is_slur,
                rest_after_note,
            } => {
                let pitch = frequency.to_vxpitch(self.bc.get_instrument())?;

                let (pp_length, after) =
                    self.split_play_note_length(length, is_slur, rest_after_note)?;

                self.bc.play_pitch(pitch, pp_length)?;
                self.after_note(after)?;
            }

            &Command::PlayNoise {
                frequency,
                length,
                is_slur,
                rest_after_note,
            } => {
                let (pp_length, after) =
                    self.split_play_note_length(length, is_slur, rest_after_note)?;

                self.bc.play_noise(frequency, pp_length)?;
                self.after_note(after)?;
            }

            &Command::Portamento {
                note1,
                note2,
                is_slur,
                speed_override,
                delay_length,
                slide_length,
                tie_length,
                rest_after_note,
            } => {
                self.portamento(
                    note1,
                    note2,
                    is_slur,
                    speed_override,
                    delay_length,
                    slide_length,
                    tie_length,
                    rest_after_note,
                )?;
            }

            Command::BrokenChord {
                notes,
                total_length,
                note_length,
                slur_last_note,
            } => {
                self.broken_chord(notes, *total_length, *note_length, *slur_last_note)?;
            }

            Command::DisableNoise => {
                self.bc.disable_noise();
            }

            Command::StartLoop(loop_count, analysis) => {
                self.bc.start_loop(*loop_count, analysis)?;
            }

            Command::SkipLastLoop(analysis) => {
                self.bc.skip_last_loop(analysis)?;
            }

            Command::EndLoop(loop_count, analysis) => {
                self.bc.end_loop(*loop_count, analysis)?;
            }

            &Command::ChangePanAndOrVolume(pan, volume) => match (pan, volume) {
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

            &Command::SetChannelInvert(flags) => {
                self.bc.set_channel_invert(flags);
            }

            &Command::VolumeSlide(amount, ticks) => {
                self.bc.volume_slide(amount, ticks);
            }

            &Command::Tremolo(amplitude, quarter_wavelength_ticks) => {
                self.bc.tremolo(amplitude, quarter_wavelength_ticks);
            }

            &Command::PanSlide(amount, ticks) => {
                self.bc.pan_slide(amount, ticks);
            }

            &Command::Panbrello(amplitude, quarter_wavelength_ticks) => {
                self.bc.panbrello(amplitude, quarter_wavelength_ticks);
            }

            Command::EnablePitchMod => {
                self.bc.enable_pmod()?;
            }
            Command::DisablePitchMod => {
                self.bc.disable_pmod()?;
            }

            &Command::SetEcho(e) => {
                if e {
                    self.bc.enable_echo();
                } else {
                    self.bc.disable_echo();
                }
            }

            &Command::SetKeyOff(k) => {
                self.keyoff_enabled = k;
            }

            &Command::SetSongTempo(bpm) => {
                self.set_song_tick_clock(bpm.to_tick_clock()?)?;
            }
            &Command::SetSongTickClock(tick_clock) => {
                self.set_song_tick_clock(tick_clock)?;
            }

            &Command::SetEchoVolume(evol) => self.bc.set_echo_volume(evol),
            &Command::SetStereoEchoVolume(left, right) => {
                self.bc.set_stereo_echo_volume(left, right)
            }
            &Command::RelativeEchoVolume(relative) => self.bc.adjust_echo_volume(relative),
            &Command::RelativeStereoEchoVolume(left, right) => {
                self.bc.adjust_stereo_echo_volume(left, right)
            }
            &Command::SetEchoFeedback(efb) => self.bc.set_echo_feedback(efb),
            &Command::RelativeEchoFeedback(adjust) => self.bc.adjust_echo_feedback(adjust),
            &Command::RelativeEchoFeedbackWithLimit(adjust, limit) => {
                self.bc.adjust_echo_feedback_limit(adjust, limit)
            }
            &Command::SetFirFilter(filter) => self.bc.set_fir_filter(filter),
            &Command::SetFirTap(tap, value) => self.bc.set_fir_tap(tap, value),
            &Command::AdjustFirTap(tap, adjust) => self.bc.adjust_fir_tap(tap, adjust),
            &Command::AdjustFirTapWithLimit(tap, adjust, limit) => {
                self.bc.adjust_fir_tap_limit(tap, adjust, limit)
            }
            &Command::SetEchoInvert(flags) => self.bc.set_echo_invert(flags),
            &Command::SetEchoDelay(length) => self.bc.set_echo_delay(length)?,

            Command::StartBytecodeAsm => {
                self.bc._start_asm_block();
            }

            Command::EndBytecodeAsm => {
                self.bc._end_asm_block()?;
            }

            Command::BytecodeAsm(asm) => parse_asm_line(&mut self.bc, asm)?,
        }

        Ok(())
    }

    fn process_command(&mut self, command: &CommandWithPos) -> Result<(), ChannelError> {
        let r = self._command(command.command());

        let ticks = self.bytecode().get_tick_counter_with_loop_flag();
        if self.prev_ticks != ticks {
            self.prev_ticks = ticks;

            self.tick_tracker.push(command.end_pos(), ticks);
        }

        r
    }

    pub(super) fn process_command_with_tracker(
        &mut self,
        command: &CommandWithPos,
        bytecode_tracker: &mut Vec<BytecodePos>,
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) {
        match self.process_command(command) {
            Ok(()) => (),
            Err(e) => errors.push(ErrorWithPos(command.pos().clone(), e)),
        }

        bytecode_tracker.push(BytecodePos {
            bc_end_pos: self
                .bytecode()
                .get_bytecode_len()
                .try_into()
                .unwrap_or(0xffff),
            char_index: command.pos().index_start,
        });
    }

    pub(super) fn process_commands_with_tracker(
        &mut self,
        commands: &[CommandWithPos],
        bytecode_tracker: &mut Vec<BytecodePos>,
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) {
        for c in commands {
            self.process_command_with_tracker(c, bytecode_tracker, errors);
        }
    }

    fn process_commands(
        &mut self,
        commands: &[CommandWithPos],
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) {
        for c in commands {
            match self.process_command(c) {
                Ok(()) => (),
                Err(e) => errors.push(ErrorWithPos(c.pos().clone(), e)),
            }
        }
    }

    pub fn loop_point(&self) -> Option<LoopPoint> {
        self.loop_point
    }

    pub fn mp_state(&self) -> &MpState {
        &self.mp
    }

    pub fn bytecode(&self) -> &Bytecode<'a> {
        &self.bc
    }

    pub fn take_bytecode(self) -> Bytecode<'a> {
        self.bc
    }

    pub fn take_bytecode_and_tick_tracker(self) -> (Bytecode<'a>, CommandTickTracker) {
        (self.bc, self.tick_tracker)
    }
}

pub(crate) struct CommandCompiler<'a> {
    pitch_table: &'a PitchTable,
    data_instruments: &'a data::UniqueNamesList<data::InstrumentOrSample>,
    max_edl: EchoEdl,

    is_song: bool,

    bc_data: Vec<u8>,
    bytecode_tracker: Vec<BytecodePos>,
}

impl<'a> CommandCompiler<'a> {
    pub fn new(
        header_size: usize,
        pitch_table: &'a PitchTable,
        data_instruments: &'a data::UniqueNamesList<data::InstrumentOrSample>,
        max_edl: EchoEdl,

        is_song: bool,
    ) -> Self {
        Self {
            bc_data: vec![0; header_size],
            bytecode_tracker: Vec::new(),

            pitch_table,
            data_instruments,
            max_edl,
            is_song,
        }
    }

    pub fn bc_size(&self) -> usize {
        self.bc_data.len()
    }

    pub fn take_data(self) -> (Vec<u8>, Vec<BytecodePos>) {
        (self.bc_data, self.bytecode_tracker)
    }

    // Takes `SubroutineCommands::errors` out of `input`
    fn compile_subroutine(
        &mut self,
        input: &mut SubroutineCommands,
        subroutines: &CompiledSubroutines,
        analysis: &SubroutineAnalysis,
    ) -> Result<Subroutine, MmlChannelError> {
        let mut errors = std::mem::take(&mut input.errors);

        let sd_start_index = self.bc_data.len();

        let mut gen = ChannelBcGenerator::new(
            std::mem::take(&mut self.bc_data),
            self.pitch_table,
            self.data_instruments,
            subroutines,
            match self.is_song {
                true => BytecodeContext::SongSubroutine {
                    max_edl: self.max_edl,
                },
                false => BytecodeContext::SfxSubroutine,
            },
            analysis.transpose_at_subroutine_start(input.index),
        );

        let tail_call = match input.commands.split_last() {
            Some((last, remaining)) => {
                gen.process_commands_with_tracker(
                    remaining,
                    &mut self.bytecode_tracker,
                    &mut errors,
                );

                match last.command() {
                    Command::CallSubroutine(_, _) => Some(last),
                    _ => {
                        gen.process_command_with_tracker(
                            last,
                            &mut self.bytecode_tracker,
                            &mut errors,
                        );
                        None
                    }
                }
            }
            None => None,
        };

        let (terminator, tail_call_end_pos) = match (
            &gen.mp_state(),
            gen.bytecode().get_state().vibrato.is_active(),
        ) {
            (MpState::Mp(_), true) | (MpState::Disabled, true) => {
                // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                // less Audio-RAM then `disable_vibraro` + `goto_relative``
                if let Some(tc) = tail_call {
                    gen.process_command_with_tracker(tc, &mut self.bytecode_tracker, &mut errors);
                }
                (BcTerminator::ReturnFromSubroutineAndDisableVibrato, None)
            }
            (MpState::Mp(_), false) | (MpState::Disabled, false) | (MpState::Manual, _) => {
                match tail_call {
                    Some(tc) => match tc.command() {
                        Command::CallSubroutine(
                            s,
                            SubroutineCallType::Mml | SubroutineCallType::Asm,
                        ) => match subroutines.get_compiled(*s) {
                            Some(sub) => {
                                (BcTerminator::TailSubroutineCall(sub), Some(tc.end_pos()))
                            }
                            None => {
                                // Process CallSubroutine command to generate an error
                                gen.process_command_with_tracker(
                                    tc,
                                    &mut self.bytecode_tracker,
                                    &mut errors,
                                );
                                (BcTerminator::ReturnFromSubroutine, None)
                            }
                        },
                        Command::CallSubroutine(_, SubroutineCallType::AsmDisableVibrato) => {
                            // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                            // less Audio-RAM then `disable_vibraro` + `goto_relative``
                            gen.process_command_with_tracker(
                                tc,
                                &mut self.bytecode_tracker,
                                &mut errors,
                            );
                            (BcTerminator::ReturnFromSubroutine, None)
                        }
                        _ => panic!("tail_call is not a CallSubroutine command"),
                    },
                    None => (BcTerminator::ReturnFromSubroutine, None),
                }
            }
        };

        assert!(gen.loop_point().is_none());

        let (bc, mut tick_tracker) = gen.take_bytecode_and_tick_tracker();

        let (bc_data, bc_state) = match bc.bytecode(terminator) {
            Ok((d, s)) => (d, Some(s)),
            Err((e, d)) => {
                errors.push(ErrorWithPos(input.end_pos_range(), e.into()));
                (d, None)
            }
        };
        self.bc_data = bc_data;

        if let (Some(end_pos), Some(bc_state)) = (tail_call_end_pos, &bc_state) {
            tick_tracker.push(
                end_pos,
                TickCounterWithLoopFlag {
                    ticks: bc_state.tick_counter,
                    in_loop: false,
                },
            );
        }

        match (errors.is_empty(), bc_state) {
            (true, Some(bc_state)) => {
                if let (Some(bc_instrument), Some(inst_tuning)) = (
                    bc_state.instrument.instrument_id(),
                    bc_state.instrument_tuning,
                ) {
                    assert_eq!(
                        inst_tuning,
                        self.pitch_table.pitch_table_offset(bc_instrument),
                        "instrument_tuning does not match bytecode instrument tracking (sub {})",
                        input.identifier.as_str()
                    );
                }

                let changes_song_tempo = !bc_state.tempo_changes.is_empty();

                let sd_end_index = self.bc_data.len();

                Ok(Subroutine {
                    index: input.index,
                    bc_state,
                    bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                    bytecode_end_offset: sd_end_index.try_into().unwrap_or(u16::MAX),
                    changes_song_tempo,
                    tick_tracker,
                    analysis: input.analysis,
                })
            }
            _ => Err(MmlChannelError {
                identifier: input.identifier.to_owned(),
                errors,
            }),
        }
    }

    pub(crate) fn compile_subroutines(
        &mut self,
        subroutines: SubroutineCommandsWithCompileOrder,
        analysis: &SubroutineAnalysis,
        errors: &mut Vec<MmlChannelError>,
    ) -> CompiledSubroutines {
        let mut out = CompiledSubroutines::new(subroutines.original_order());

        let mut subroutines = subroutines;
        for si in 0..subroutines.len() {
            let s = subroutines.get_compile_order(si);
            match self.compile_subroutine(s, &out, analysis) {
                Ok(s) => out.store(s.index, SubroutineState::Compiled(s)),
                Err(e) => {
                    errors.push(e);
                    out.store(s.index, SubroutineState::CompileError);
                }
            }
        }

        if errors.is_empty() {
            // This test is done after all subroutines have been compiled to avoid a panic when a
            // subroutine calls a subroutine with an error.
            for s in out.iter_compiled() {
                assert_eq!(
                    s.analysis.instrument.map(|i| i.instrument_id()),
                    s.bc_state.instrument.instrument_id(),
                    "instrument analysis does not match bytecode instrument tracking (sub #{})",
                    s.index,
                );
            }
        }

        out
    }

    // Takes `ChannelCommands::errors` out of `input`
    fn compile_song_channel(
        &mut self,
        channel_index: MusicChannelIndex,
        input: &mut ChannelCommands,
        subroutines: &CompiledSubroutines,
    ) -> Result<Channel, MmlChannelError> {
        let identifier = channel_index.identifier();
        assert!(identifier.as_str().len() == 1);

        let mut errors = std::mem::take(&mut input.errors);

        let sd_start_index = self.bc_data.len();

        let mut gen = ChannelBcGenerator::new(
            std::mem::take(&mut self.bc_data),
            self.pitch_table,
            self.data_instruments,
            subroutines,
            BytecodeContext::SongChannel {
                index: channel_index,
                max_edl: self.max_edl,
            },
            TransposeStartRange::DISABLED,
        );

        gen.process_commands_with_tracker(&input.commands, &mut self.bytecode_tracker, &mut errors);

        let loop_point = gen.loop_point();
        let tick_counter = gen.bytecode().get_tick_counter();

        let terminator = match gen.loop_point() {
            None => BcTerminator::DisableChannel,
            Some(lp) => {
                if lp.tick_counter == tick_counter {
                    errors.push(ErrorWithPos(
                        input.end_pos.to_range(1),
                        ChannelError::NoTicksAfterLoopPoint,
                    ));
                }
                BcTerminator::Goto(lp.bytecode_offset)
            }
        };

        let (bc, tick_tracker) = gen.take_bytecode_and_tick_tracker();

        let (bc_data, bc_state) = match bc.bytecode(terminator) {
            Ok((b, s)) => (b, Some(s)),
            Err((e, b)) => {
                errors.push(ErrorWithPos(
                    input.end_pos.to_range(1),
                    ChannelError::BytecodeError(e),
                ));
                (b, None)
            }
        };
        self.bc_data = bc_data;

        match (errors.is_empty(), bc_state) {
            (true, Some(bc_state)) => {
                if let (Some(bc_instrument), Some(inst_tuning)) = (
                    bc_state.instrument.instrument_id(),
                    bc_state.instrument_tuning,
                ) {
                    assert_eq!(
                        inst_tuning,
                        self.pitch_table.pitch_table_offset(bc_instrument),
                        "instrument_tuning does not match bytecode instrument tracking (channel {})", identifier.as_str()
                    );
                }

                Ok(Channel {
                    channel_index,
                    bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                    loop_point,
                    tick_counter: bc_state.tick_counter,
                    max_stack_depth: bc_state.max_stack_depth,
                    tick_tracker,
                    tempo_changes: bc_state.tempo_changes,
                })
            }
            _ => Err(MmlChannelError {
                identifier: identifier.to_owned(),
                errors,
            }),
        }
    }

    pub fn compile_song_channels(
        &mut self,
        channels: [Option<ChannelCommands>; N_MUSIC_CHANNELS],
        subroutines: &CompiledSubroutines,
        errors: &mut MmlCompileErrors,
    ) -> [Option<Channel>; N_MUSIC_CHANNELS] {
        let mut channels = channels;

        std::array::from_fn(|c_index| {
            channels[c_index].as_mut().and_then(|c| {
                let c_index = MusicChannelIndex::try_new(c_index).unwrap();

                match self.compile_song_channel(c_index, c, subroutines) {
                    Ok(c) => Some(c),
                    Err(e) => {
                        errors.channel_errors.push(e);
                        None
                    }
                }
            })
        })
    }
}

pub(crate) fn compile_sound_effect(
    input: AnalysedSoundEffectCommands,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
    subroutines: &CompiledSubroutines,
    use_line_errors: bool,
) -> Result<
    (
        Vec<u8>,
        TickCounter,
        Option<CursorTracker>,
        CommandTickTracker,
    ),
    SoundEffectErrorList,
> {
    let input = input.0;

    let bc_data = Vec::new();
    let mut errors = input.errors;

    let mut gen = ChannelBcGenerator::new(
        bc_data,
        pitch_table,
        data_instruments,
        subroutines,
        BytecodeContext::SoundEffect,
        TransposeStartRange::DISABLED,
    );
    gen.process_commands(&input.commands, &mut errors);

    let tick_counter = gen.bytecode().get_tick_counter();
    assert!(gen.loop_point().is_none());

    let (bc, tick_tracker) = gen.take_bytecode_and_tick_tracker();

    let bytecode = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            errors.push(ErrorWithPos(
                input.end_pos.to_range(1),
                ChannelError::BytecodeError(e),
            ));
            b
        }
    };

    if tick_counter.is_zero() {
        errors.push(ErrorWithPos(
            input.end_pos.to_range(1),
            ChannelError::NoTicksInSoundEffect,
        ));
    }

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            input.end_pos.to_range(1),
            ChannelError::TooManySfxTicks(tick_counter),
        ));
    }

    if errors.is_empty() {
        Ok((bytecode, tick_counter, input.mml_tracker, tick_tracker))
    } else if use_line_errors {
        Err(SoundEffectErrorList::BytecodeErrors(errors))
    } else {
        Err(SoundEffectErrorList::MmlErrors(errors))
    }
}

pub(crate) fn compile_mml_prefix(
    input: ChannelCommands,
    context: BytecodeContext,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &PitchTable,
    subroutines: &CompiledSubroutines,
) -> Result<Vec<u8>, MmlPrefixError> {
    let bc_data = Vec::new();
    let mut errors = input.errors;

    let mut gen = ChannelBcGenerator::new(
        bc_data,
        pitch_table,
        data_instruments,
        subroutines,
        context,
        TransposeStartRange::DISABLED,
    );
    gen.process_commands(&input.commands, &mut errors);

    let tick_counter = gen.bytecode().get_tick_counter();
    assert!(gen.loop_point().is_none());

    let bc = gen.take_bytecode();

    let bytecode = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            errors.push(ErrorWithPos(
                input.end_pos.to_range(1),
                ChannelError::BytecodeError(e),
            ));
            b
        }
    };

    if tick_counter > MAX_MML_PREFIX_TICKS {
        errors.push(ErrorWithPos(
            input.end_pos.to_range(1),
            ChannelError::TooManyTicksInMmlPrefix(tick_counter),
        ))
    }

    if errors.is_empty() {
        Ok(bytecode)
    } else {
        Err(MmlPrefixError::MmlErrors(errors))
    }
}
