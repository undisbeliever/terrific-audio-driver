//! Audio driver channel commands bytecode generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, BytecodeContext, IeState, InstrumentId,
    PortamentoVelocity, RelativeVolume, SlurredNoteState, VibratoState, KEY_OFF_TICK_DELAY,
};
use crate::bytecode::{
    EarlyReleaseMinTicks, EarlyReleaseTicks, LoopCount, Pan, PlayNoteTicks, RelativePan,
    VibratoPitchOffsetPerTick, VibratoQuarterWavelengthInTicks, Volume,
};
use crate::bytecode_assembler::parse_asm_line;
use crate::data::{self, UniqueNamesList};
use crate::envelope::{Adsr, Envelope, Gain, OptionalGain, TempGain};
use crate::errors::{ChannelError, ValueError};
use crate::mml::IdentifierBuf;
use crate::notes::Note;
use crate::notes::SEMITONES_PER_OCTAVE;
use crate::pitch_table::{PitchTable, PITCH_REGISTER_MAX};
use crate::songs::{LoopPoint, Subroutine};
use crate::time::{Bpm, TickClock, TickCounter};
use crate::value_newtypes::u8_value_newtype;
use crate::FilePosRange;

use std::cmp::min;
use std::ops::Range;

pub const MAX_PORTAMENTO_SLIDE_TICKS: u32 = 16 * 1024;
pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

// Number of rest instructions before a loop uses less space
pub const REST_LOOP_INSTRUCTION_THREASHOLD: u32 = 3;

u8_value_newtype!(
    PortamentoSpeed,
    PortamentoSpeedOutOfRange,
    NoPortamentoSpeed
);

u8_value_newtype!(Quantization, QuantizeOutOfRange, NoQuantize, 0, 8);
u8_value_newtype!(FineQuantization, FineQuantizeOutOfRange, NoFineQuantizate);

impl Quantization {
    pub const FINE_QUANTIZATION_SCALE: u8 = 32;

    pub fn to_fine(self) -> Option<FineQuantization> {
        if self.0 < 8 {
            Some(FineQuantization(self.0 * Self::FINE_QUANTIZATION_SCALE))
        } else {
            None
        }
    }
}

impl FineQuantization {
    pub const UNITS: u32 = 256;

    pub fn quantize(&self, l: u32) -> u32 {
        let q = u32::from(self.0);
        std::cmp::max((l * q) / Self::UNITS, 1)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum VolumeCommand {
    Absolute(Volume),
    Relative(i32),
}

#[derive(Debug, Copy, Clone)]
pub enum PanCommand {
    Absolute(Pan),
    Relative(RelativePan),
}

pub fn relative_volume(v: i32) -> VolumeCommand {
    const _: () = assert!(Volume::MIN.as_u8() == 0);

    if v <= -(Volume::MAX.as_u8() as i32) {
        VolumeCommand::Absolute(Volume::MIN)
    } else if v >= Volume::MAX.as_u8().into() {
        VolumeCommand::Absolute(Volume::MAX)
    } else {
        VolumeCommand::Relative(v)
    }
}

pub fn merge_volumes_commands(v1: Option<VolumeCommand>, v2: VolumeCommand) -> VolumeCommand {
    match (v1, v2) {
        (Some(VolumeCommand::Absolute(v1)), VolumeCommand::Relative(v2)) => {
            let v = u32::from(v1.as_u8()).saturating_add_signed(v2);
            let v = u8::try_from(v).unwrap_or(u8::MAX);
            VolumeCommand::Absolute(Volume::new(v))
        }
        (Some(VolumeCommand::Relative(v1)), VolumeCommand::Relative(v2)) => {
            let v = v1.saturating_add(v2);
            relative_volume(v)
        }
        (Some(_), VolumeCommand::Absolute(_)) => v2,
        (None, v2) => v2,
    }
}

pub fn relative_pan(p: i32) -> PanCommand {
    const _: () = assert!(Pan::MAX.as_u8() as i32 == i8::MAX as i32 + 1);
    const _: () = assert!(Pan::MAX.as_u8() as i32 == -(i8::MIN as i32));
    const _: () = assert!(Pan::MIN.as_u8() == 0);

    if p <= -(Pan::MAX.as_u8() as i32) {
        PanCommand::Absolute(Pan::MIN)
    } else if p >= Pan::MAX.as_u8().into() {
        PanCommand::Absolute(Pan::MAX)
    } else {
        PanCommand::Relative(p.try_into().unwrap())
    }
}

pub fn merge_pan_commands(p1: Option<PanCommand>, p2: PanCommand) -> PanCommand {
    match (p1, p2) {
        (Some(PanCommand::Absolute(p1)), PanCommand::Relative(p2)) => {
            let p = min(
                p1.as_u8().saturating_add_signed(p2.as_i8()),
                Pan::MAX.as_u8(),
            );
            PanCommand::Absolute(p.try_into().unwrap())
        }
        (Some(PanCommand::Relative(p1)), PanCommand::Relative(p2)) => {
            let p1: i32 = p1.as_i8().into();
            let p2: i32 = p2.as_i8().into();
            relative_pan(p1 + p2)
        }
        (Some(_), PanCommand::Absolute(_)) => p2,
        (None, p2) => p2,
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct MpVibrato {
    pub depth_in_cents: u32,
    pub quarter_wavelength_ticks: VibratoQuarterWavelengthInTicks,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ManualVibrato {
    pub pitch_offset_per_tick: VibratoPitchOffsetPerTick,
    pub quarter_wavelength_ticks: VibratoQuarterWavelengthInTicks,
}

#[derive(Debug, Clone, Copy)]
pub enum SubroutineCallType {
    Mml,
    Asm,
    AsmDisableVibrato,
}

pub(crate) enum Command {
    None,

    SetLoopPoint,

    SetManualVibrato(Option<ManualVibrato>),
    SetMpVibrato(Option<MpVibrato>),

    Rest {
        /// Length of the first rest
        /// (The user expects a keyoff after the first rest command)
        ticks_until_keyoff: TickCounter,
        /// Combined length of all rests after the first rest
        /// (keyoff already sent, it does not matter if these rests keyoff or not)
        ticks_after_keyoff: TickCounter,
    },

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
        temp_gain: TempGain,
        /// Combined length of all rests after the note
        /// (keyoff already sent, it does not matter if these rests keyoff or not)
        rest_ticks_after_note: TickCounter,
    },
    Portamento {
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        /// Number of ticks to hold the pitch at note1 before the pitch slide
        delay_length: TickCounter,
        /// Length of the pitch slide (portamento_length - delay_length)
        slide_length: TickCounter,
        /// Number of ticks to hold the pitch at note2
        tie_length: TickCounter,
    },
    QuantizedPortamento {
        note1: Note,
        note2: Note,
        speed_override: Option<PortamentoSpeed>,
        delay_length: TickCounter,
        slide_length: TickCounter,
        tie_length: TickCounter,
        temp_gain: TempGain,
        rest: TickCounter,
        /// Combined length of all rests after the portamento
        /// (keyoff already sent, it does not matter if these rests keyoff or not)
        rest_ticks_after_note: TickCounter,
    },
    BrokenChord {
        notes: Vec<Note>,
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    },

    CallSubroutine(usize, SubroutineCallType),
    StartLoop,
    SkipLastLoop,
    EndLoop(LoopCount),

    // index into Vec<ChannelData>.
    SetInstrument(usize),
    SetAdsr(Adsr),
    SetGain(Gain),

    // None reuses previous temp gain
    TempGain(Option<TempGain>),
    TempGainAndRest {
        temp_gain: Option<TempGain>,
        ticks_until_keyoff: TickCounter,
        ticks_after_keyoff: TickCounter,
    },
    TempGainAndWait(Option<TempGain>, TickCounter),

    DisableEarlyRelease,
    SetEarlyRelease(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain),

    ChangePanAndOrVolume(Option<PanCommand>, Option<VolumeCommand>),
    SetEcho(bool),

    SetSongTempo(Bpm),
    SetSongTickClock(TickClock),

    StartBytecodeAsm,
    EndBytecodeAsm,

    // Using range so there is no lifetime in MmlCommand
    BytecodeAsm(Range<usize>),
}

#[derive(Debug, Clone)]
pub struct MmlInstrument {
    pub(crate) identifier: IdentifierBuf,

    pub(crate) file_range: FilePosRange,

    pub(crate) instrument_id: InstrumentId,

    // No envelope override or envelope override matches instrument
    pub(crate) envelope_unchanged: bool,
    pub(crate) envelope: Envelope,
}

#[derive(Clone, PartialEq)]
pub enum MpState {
    Disabled,
    Manual,
    Mp(MpVibrato),
}

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

pub(crate) struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    instruments: &'a Vec<MmlInstrument>,
    subroutines: Option<&'a Vec<Subroutine>>,

    bc: Bytecode<'a>,

    mp: MpState,

    loop_point: Option<LoopPoint>,
}

impl<'a> ChannelBcGenerator<'a> {
    pub fn new(
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
    ) -> Result<ManualVibrato, ChannelError> {
        if mp.depth_in_cents == 0 {
            return Err(ChannelError::MpDepthZero);
        }
        let instrument_id = match self.bc.get_state().instrument {
            IeState::Known(i) | IeState::Maybe(i) => i,
            IeState::Unknown => return Err(ChannelError::CannotUseMpWithoutInstrument),
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
            return Err(ChannelError::MpPitchOffsetTooLarge(u32::MAX));
        }
        let po_per_tick = po_per_tick as u32;

        match po_per_tick.try_into() {
            Ok(po) => Ok(ManualVibrato {
                quarter_wavelength_ticks: mp.quarter_wavelength_ticks,
                pitch_offset_per_tick: po,
            }),
            Err(_) => Err(ChannelError::MpPitchOffsetTooLarge(po_per_tick)),
        }
    }

    fn split_wait_length(
        length: TickCounter,
    ) -> Result<(BcTicksNoKeyOff, TickCounter), ChannelError> {
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
            const MIN_THREE_INSTRUCTIONS: u32 =
                BcTicksNoKeyOff::MAX_TICKS + BcTicksKeyOff::MAX_TICKS + 1;
            const MIN_TWO_INSTRUCTIONS: u32 = BcTicksKeyOff::MAX_TICKS + 1;
            const MAX_TWO_INSTRUCTIONS: u32 = MIN_THREE_INSTRUCTIONS - 1;

            match l {
                0..=BcTicksKeyOff::MAX_TICKS => Ok((
                    PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(l)?),
                    TickCounter::new(0),
                )),
                MIN_TWO_INSTRUCTIONS..=MAX_TWO_INSTRUCTIONS => {
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
    ) -> Result<(), ChannelError> {
        let (pn_length, rest) = Self::split_play_note_length(length, is_slur)?;

        let vibrato = self.bc.get_state().vibrato;

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_length)?;
            }
            MpState::Disabled => {
                const POPT: VibratoPitchOffsetPerTick = VibratoPitchOffsetPerTick::new(0);

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

    fn rest_after_play_note(
        &mut self,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), ChannelError> {
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
    fn rest_many_keyoffs_no_loop(&mut self, length: TickCounter) -> Result<(), ChannelError> {
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
    fn wait_no_loop(&mut self, length: TickCounter) -> Result<(), ChannelError> {
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
    fn rest_one_keyoff(&mut self, length: TickCounter) -> Result<(), ChannelError> {
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
    fn rest_many_keyoffs(&mut self, length: TickCounter) -> Result<(), ChannelError> {
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
    fn wait(&mut self, length: TickCounter) -> Result<(), ChannelError> {
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
    ) -> Result<(), ChannelError> {
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
                if note1 < note2 {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                }
            }
            None => {
                let instrument_id = match self.bc.get_state().instrument {
                    IeState::Known(i) | IeState::Maybe(i) => i,
                    IeState::Unknown => return Err(ChannelError::PortamentoRequiresInstrument),
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
    ) -> Result<(), ChannelError> {
        if notes.is_empty() {
            return Err(ChannelError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(ChannelError::TooManyNotesInBrokenChord(notes.len()));
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
            return Err(ChannelError::BrokenChordTotalLengthTooShort);
        }
        let notes_in_loop = (total_ticks - last_note_ticks) / note_length.ticks();

        let break_point = usize::try_from(notes_in_loop % n_notes).unwrap();
        let has_break_point: bool = break_point != 0;

        let n_loops = (notes_in_loop / n_notes) + u32::from(has_break_point);

        if n_loops < 2 {
            return Err(ChannelError::BrokenChordTotalLengthTooShort);
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
            return Err(ChannelError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument(&mut self, inst: usize) -> Result<(), ChannelError> {
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
    ) -> Result<(), ChannelError> {
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
    ) -> Result<(), ChannelError> {
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
    ) -> Result<(), ChannelError> {
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

    fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), ChannelError> {
        self.bc.set_song_tick_clock(tick_clock)?;
        Ok(())
    }

    pub fn process_command(&mut self, command: &Command) -> Result<(), ChannelError> {
        match command {
            Command::None => (),

            &Command::SetLoopPoint => match self.bc.get_context() {
                BytecodeContext::SongChannel => {
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

                    self.bc._song_loop_point();
                }
                BytecodeContext::SongSubroutine => return Err(ChannelError::CannotSetLoopPoint),
                &BytecodeContext::SoundEffect => return Err(ChannelError::CannotSetLoopPoint),
            },

            &Command::SetInstrument(inst_index) => {
                self.set_instrument(inst_index)?;
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

            &Command::Rest {
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                self.rest_one_keyoff(ticks_until_keyoff)?;
                self.rest_many_keyoffs(ticks_after_keyoff)?;
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

            &Command::PlayNote {
                note,
                length,
                is_slur,
            } => {
                self.play_note_with_mp(note, length, is_slur)?;
            }

            &Command::PlayQuantizedNote {
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

            &Command::Portamento {
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

            &Command::QuantizedPortamento {
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

            Command::BrokenChord {
                notes,
                total_length,
                note_length,
            } => {
                self.broken_chord(notes, *total_length, *note_length)?;
            }

            Command::StartLoop => {
                self.bc.start_loop(None)?;
            }

            Command::SkipLastLoop => {
                self.bc.skip_last_loop()?;
            }

            &Command::EndLoop(loop_count) => {
                self.bc.end_loop(Some(loop_count))?;
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

            &Command::SetEcho(e) => {
                if e {
                    self.bc.enable_echo();
                } else {
                    self.bc.disable_echo();
                }
            }

            &Command::SetSongTempo(bpm) => {
                self.set_song_tick_clock(bpm.to_tick_clock()?)?;
            }
            &Command::SetSongTickClock(tick_clock) => {
                self.set_song_tick_clock(tick_clock)?;
            }

            Command::StartBytecodeAsm => {
                self.bc._start_asm_block();
            }

            Command::EndBytecodeAsm => {
                self.bc._end_asm_block()?;
            }

            Command::BytecodeAsm(range) => {
                let asm = &self.mml_file[range.clone()];

                parse_asm_line(&mut self.bc, asm)?
            }
        }

        Ok(())
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
}
