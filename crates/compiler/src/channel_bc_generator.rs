//! Audio driver channel commands bytecode generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    self, BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, BytecodeContext, DetuneValue,
    EarlyReleaseMinTicks, EarlyReleaseTicks, IeState, InstrumentId, LoopCount, NoiseFrequency, Pan,
    PanSlideAmount, PanSlideTicks, PanbrelloAmplitude, PanbrelloQuarterWavelengthInTicks,
    PlayNoteTicks, PlayPitchPitch, PortamentoVelocity, RelativeEchoFeedback, RelativeEchoVolume,
    RelativeFirCoefficient, RelativePan, RelativeVolume, SlurredNoteState, TremoloAmplitude,
    TremoloQuarterWavelengthInTicks, VibratoDelayTicks, VibratoPitchOffsetPerTick,
    VibratoQuarterWavelengthInTicks, VibratoState, Volume, VolumeSlideAmount, VolumeSlideTicks,
    KEY_OFF_TICK_DELAY,
};
use crate::bytecode_assembler::parse_asm_line;
use crate::data::{self, UniqueNamesList};
use crate::driver_constants::FIR_FILTER_SIZE;
use crate::echo::{EchoFeedback, EchoLength, EchoVolume, FirCoefficient, FirTap};
use crate::envelope::{Adsr, Envelope, Gain, OptionalGain, TempGain};
use crate::errors::{ChannelError, ValueError};
use crate::invert_flags::InvertFlags;
use crate::mml::IdentifierBuf;
use crate::notes::Note;
use crate::notes::SEMITONES_PER_OCTAVE;
use crate::pitch_table::{PitchTable, PlayPitchFrequency, PITCH_REGISTER_MAX};
use crate::songs::LoopPoint;
use crate::subroutines::{NoSubroutines, SubroutineStore};
use crate::time::{Bpm, TickClock, TickCounter};
use crate::value_newtypes::{i16_value_newtype, u8_value_newtype, SignedValueNewType};
use crate::FilePosRange;

use std::cmp::min;
use std::ops::{Range, RangeInclusive};

pub const MAX_PORTAMENTO_SLIDE_TICKS: u32 = 16 * 1024;
pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

// Number of rest instructions before a loop uses less space
pub const REST_LOOP_INSTRUCTION_THREASHOLD: u32 = 3;

u8_value_newtype!(
    PortamentoSpeed,
    PortamentoSpeedOutOfRange,
    NoPortamentoSpeed
);

#[derive(Debug, Copy, Clone)]
pub enum NoteOrPitch {
    Note(Note),
    Pitch(PlayPitchPitch),
    PitchFrequency(PlayPitchFrequency),
}

#[derive(Debug)]
enum NoteOrPitchOut {
    Note(DetuneCentsOutput, Note, DetuneValue),
    Pitch(PlayPitchPitch),
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
    pub delay: VibratoDelayTicks,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ManualVibrato {
    pub pitch_offset_per_tick: VibratoPitchOffsetPerTick,
    pub quarter_wavelength_ticks: VibratoQuarterWavelengthInTicks,
    pub delay: VibratoDelayTicks,
}

u8_value_newtype!(Quantization, QuantizeOutOfRange, NoQuantize, 0, 8);
u8_value_newtype!(FineQuantization, FineQuantizeOutOfRange, NoFineQuantize);

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
        const UNITS: u64 = FineQuantization::UNITS as u64;
        const _: () = assert!((u32::MAX as u64 * FineQuantization::MAX.0 as u64) < u64::MAX);
        const _: () =
            assert!((u32::MAX as u64 * FineQuantization::MAX.0 as u64) / UNITS < u32::MAX as u64);

        let l = u64::from(l);
        let q = u64::from(self.0);
        std::cmp::max((l * q) / UNITS, 1).try_into().unwrap()
    }
}

#[derive(Clone, Copy)]
pub enum Quantize {
    None,
    Rest(FineQuantization),
    WithTempGain(FineQuantization, TempGain),
}

#[derive(Clone, Copy)]
pub struct RestTicksAfterNote(pub TickCounter);

i16_value_newtype!(
    DetuneCents,
    DetuneCentsOutOfRange,
    NoDetuneCents,
    NoDetuneCentsSign,
    -1200,
    1200
);

impl DetuneCents {
    pub const ZERO: Self = Self(0);
}

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
    SetQuantize(Quantize),

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
        rest_after_note: RestTicksAfterNote,
    },
    PlayPitch {
        pitch: PlayPitchPitch,
        length: TickCounter,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    PlayPitchFrequency {
        frequency: PlayPitchFrequency,
        length: TickCounter,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    PlayNoise {
        frequency: NoiseFrequency,
        length: TickCounter,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    Portamento {
        note1: Option<NoteOrPitch>,
        note2: NoteOrPitch,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        /// Number of ticks to hold the pitch at note1 before the pitch slide
        delay_length: TickCounter,
        /// Length of the pitch slide (portamento_length - delay_length)
        slide_length: TickCounter,
        /// Number of ticks to hold the pitch at note2
        tie_length: TickCounter,
        rest_after_note: RestTicksAfterNote,
    },
    BrokenChord {
        notes: Vec<NoteOrPitch>,
        total_length: TickCounter,
        note_length: PlayNoteTicks,
        slur_last_note: bool,
    },

    DisableNoise,

    CallSubroutine(usize, SubroutineCallType),
    StartLoop,
    SkipLastLoop,
    EndLoop(LoopCount),

    // index into Vec<MmlInstrument>.
    SetSubroutineInstrumentHint(usize),

    // index into Vec<MmlInstrument>.
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

    SetDetune(DetuneValue),
    SetDetuneCents(DetuneCents),

    ChangePanAndOrVolume(Option<PanCommand>, Option<VolumeCommand>),
    SetChannelInvert(InvertFlags),

    VolumeSlide(VolumeSlideAmount, VolumeSlideTicks),
    Tremolo(TremoloAmplitude, TremoloQuarterWavelengthInTicks),
    PanSlide(PanSlideAmount, PanSlideTicks),
    Panbrello(PanbrelloAmplitude, PanbrelloQuarterWavelengthInTicks),

    EnablePitchMod,
    DisablePitchMod,
    SetEcho(bool),

    SetKeyOff(bool),

    SetSongTempo(Bpm),
    SetSongTickClock(TickClock),

    SetEchoVolume(EchoVolume),
    SetStereoEchoVolume(EchoVolume, EchoVolume),
    RelativeEchoVolume(RelativeEchoVolume),
    RelativeStereoEchoVolume(RelativeEchoVolume, RelativeEchoVolume),

    SetEchoFeedback(EchoFeedback),
    RelativeEchoFeedback(RelativeEchoFeedback),
    RelativeEchoFeedbackWithLimit(RelativeEchoFeedback, EchoFeedback),
    SetFirFilter([FirCoefficient; FIR_FILTER_SIZE]),
    SetFirTap(FirTap, FirCoefficient),
    AdjustFirTap(FirTap, RelativeFirCoefficient),
    AdjustFirTapWithLimit(FirTap, RelativeFirCoefficient, FirCoefficient),
    SetEchoInvert(InvertFlags),
    SetEchoDelay(EchoLength),

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

    pub(crate) note_range: RangeInclusive<Note>,
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

                    n_rest_instructions: div.div_ceil(Loop::MAX_TICKS)
                        + rem.div_ceil(Rem::MAX_TICKS),
                })
            } else {
                None
            }
        })
        .min_by_key(|rl| rl.n_rest_instructions)
        .unwrap()
}

/// Waits and rests after a play-note/portamento/etc instructions
enum AfterPlayNote {
    Wait {
        wait: TickCounter,
        rest: TickCounter,
    },
    WaitEnableKeyOn {
        wait: TickCounter,
    },
    KeyOff {
        wait: TickCounter,
        ticks_after_keyoff: TickCounter,
    },
    TempGain {
        wait: TickCounter,
        temp_gain: TempGain,
        ticks_until_keyoff: TickCounter,
        ticks_after_keyoff: TickCounter,
    },
}

pub(crate) struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    instruments: &'a [MmlInstrument],
    subroutines: &'a dyn SubroutineStore,

    bc: Bytecode<'a>,

    detune_cents: DetuneCents,
    mp: MpState,
    quantize: Quantize,
    keyoff_enabled: bool,

    loop_point: Option<LoopPoint>,
}

impl<'a> ChannelBcGenerator<'a> {
    pub fn new(
        bc_data: Vec<u8>,
        pitch_table: &'a PitchTable,
        mml_file: &'a str,
        data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        mml_instruments: &'a [MmlInstrument],
        subroutines: &'a dyn SubroutineStore,
        context: BytecodeContext,
    ) -> ChannelBcGenerator<'a> {
        ChannelBcGenerator {
            pitch_table,
            mml_file,
            instruments: mml_instruments,
            subroutines,
            // Using NoSubroutines here to forbid direct subroutine calls in bytecode assembly
            // (\asm subroutine calls must go through `Self::call_subroutine()`)
            bc: Bytecode::new_append_to_vec(bc_data, context, data_instruments, &NoSubroutines()),
            mp: MpState::Manual,
            detune_cents: DetuneCents::ZERO,
            quantize: Quantize::None,
            keyoff_enabled: true,
            loop_point: None,
        }
    }

    fn calculate_vibrato_for_note(
        &self,
        mp: &MpVibrato,
        note: Note,
        detune: DetuneCentsOutput,
    ) -> Result<ManualVibrato, ChannelError> {
        if mp.depth_in_cents == 0 {
            return Err(ChannelError::MpDepthZero);
        }
        let instrument_id = match self.bc.get_state().instrument.instrument_id() {
            Some(i) => i,
            None => return Err(ChannelError::CannotUseMpWithoutInstrument),
        };

        let detune = detune.detune_value(self.bc.get_state());

        let pitch = self
            .pitch_table
            .pitch_for_note(instrument_id, note)
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
                let instrument_id = match self.bc.get_state().instrument.instrument_id() {
                    Some(i) => i,
                    None => return Err(ChannelError::CannotUseDetuneCentsWithoutInstrument),
                };
                let pitch = self.pitch_table.pitch_for_note(instrument_id, note);
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
                    self.bc.play_pitch(p, length);
                }
                NoteOrPitch::PitchFrequency(f) => {
                    // Pitch is not detuned
                    let p = f.to_vxpitch(self.bc.get_instrument())?;
                    self.bc.play_pitch(p, length);
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
                    self.bc.play_pitch(p, length);
                }
                NoteOrPitch::PitchFrequency(f) => {
                    // Pitch is not detuned
                    let p = f.to_vxpitch(self.bc.get_instrument())?;
                    self.bc.play_pitch(p, length);
                }
            }

            self.bc.keyon_next_note();
        }

        Ok(())
    }

    fn split_wait_length(
        length: TickCounter,
    ) -> Result<(BcTicksNoKeyOff, TickCounter), ChannelError> {
        let l = length.value();

        let bc = u32::min(l, BcTicksNoKeyOff::MAX_TICKS);
        let bc = BcTicksNoKeyOff::try_from(bc)?;

        Ok((bc, TickCounter::new(l - bc.ticks())))
    }

    fn split_note_length(
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

    fn split_play_note_length(
        &self,
        length: TickCounter,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    ) -> Result<(PlayNoteTicks, AfterPlayNote), ValueError> {
        let rest_after_note = rest_after_note.0;

        let no_quantize_or_slur_keyoff = || {
            let (pn_ticks, wait) = Self::split_note_length(length, false)?;
            Ok((
                pn_ticks,
                AfterPlayNote::KeyOff {
                    wait,
                    ticks_after_keyoff: rest_after_note,
                },
            ))
        };

        match (is_slur, &self.quantize) {
            (true, _) => {
                let (pn_ticks, wait) = Self::split_note_length(length, true)?;
                Ok((
                    pn_ticks,
                    AfterPlayNote::Wait {
                        wait,
                        rest: rest_after_note,
                    },
                ))
            }
            (false, Quantize::None) => match self.keyoff_enabled {
                true => no_quantize_or_slur_keyoff(),
                false => {
                    if rest_after_note.is_zero() {
                        // No rest, slur note and emit a `keyon_next_note` instruction.
                        let (pn_ticks, wait) = Self::split_note_length(length, true)?;
                        Ok((pn_ticks, AfterPlayNote::WaitEnableKeyOn { wait }))
                    } else {
                        // Join note + rest and do not emit the `keyon_next_note` instruction.
                        let (pn_ticks, wait) =
                            Self::split_note_length(length + rest_after_note, false)?;

                        Ok((
                            pn_ticks,
                            AfterPlayNote::KeyOff {
                                wait,
                                ticks_after_keyoff: TickCounter::new(0),
                            },
                        ))
                    }
                }
            },
            (false, &Quantize::Rest(q)) => {
                let l = length.value();
                let note_length = q.quantize(l) + KEY_OFF_TICK_DELAY;

                if note_length < l {
                    let note_length = TickCounter::new(note_length);
                    let ticks_after_keyoff =
                        TickCounter::new(l - note_length.value()) + rest_after_note;
                    let (pn_ticks, wait) = Self::split_note_length(note_length, false)?;
                    Ok((
                        pn_ticks,
                        AfterPlayNote::KeyOff {
                            wait,
                            ticks_after_keyoff,
                        },
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
                    let note_length = TickCounter::new(pn_length);
                    let ticks_until_keyoff = TickCounter::new(l - note_length.value());
                    let (pn_ticks, wait) = Self::split_note_length(note_length, true)?;
                    Ok((
                        pn_ticks,
                        AfterPlayNote::TempGain {
                            wait,
                            temp_gain,
                            ticks_until_keyoff,
                            ticks_after_keyoff: rest_after_note,
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
            AfterPlayNote::Wait { wait, rest } => {
                if !wait.is_zero() {
                    self.wait(wait)?;
                }
                self.rest_one_keyoff(rest)
            }
            AfterPlayNote::WaitEnableKeyOn { wait } => {
                if !wait.is_zero() {
                    self.wait(wait)?;
                }

                self.bc.keyon_next_note();
                Ok(())
            }
            AfterPlayNote::KeyOff {
                wait,
                ticks_after_keyoff,
            } => {
                if !wait.is_zero() {
                    self.rest_one_keyoff(wait)?;
                }
                match ticks_after_keyoff.value() {
                    1 => self.wait(ticks_after_keyoff),
                    _ => self.rest_many_keyoffs(ticks_after_keyoff),
                }
            }
            AfterPlayNote::TempGain {
                wait,
                temp_gain,
                ticks_until_keyoff,
                ticks_after_keyoff,
            } => {
                if !wait.is_zero() {
                    self.wait(wait)?;
                }
                self.temp_gain_and_rest(Some(temp_gain), ticks_until_keyoff, ticks_after_keyoff)
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

    fn portamento_note_pitch(
        &self,
        note: NoteOrPitch,
        instrument: Option<InstrumentId>,
    ) -> Result<(Option<u16>, NoteOrPitchOut), ChannelError> {
        match note {
            NoteOrPitch::Note(note) => {
                let detune_output = self.calculate_detune_for_note(note)?;

                let detune = detune_output.detune_value(self.bc.get_state());

                match instrument {
                    Some(instrument) => {
                        let pitch = self
                            .pitch_table
                            .pitch_for_note(instrument, note)
                            .wrapping_add_signed(detune.as_i16());

                        Ok((
                            Some(pitch),
                            NoteOrPitchOut::Note(detune_output, note, detune),
                        ))
                    }
                    None => Ok((None, NoteOrPitchOut::Note(detune_output, note, detune))),
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
                self.bc.play_pitch(p, t);
            }
        }
        Ok(())
    }

    fn portamento_get_prev_note_and_pitch(
        &self,
    ) -> Result<(NoteOrPitch, Option<u16>), ChannelError> {
        match self.bc.get_state().prev_slurred_note {
            SlurredNoteState::Slurred(prev_note, prev_detune, prev_inst) => match prev_inst {
                Some(prev_inst) => {
                    let prev_pitch = self
                        .pitch_table
                        .pitch_for_note(prev_inst, prev_note)
                        .wrapping_add_signed(prev_detune.as_i16());

                    Ok((NoteOrPitch::Note(prev_note), Some(prev_pitch)))
                }
                None => Ok((NoteOrPitch::Note(prev_note), None)),
            },
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
            SlurredNoteState::Slurred(prev_note, prev_detune, prev_inst) => {
                match (prev_inst, note1_pitch) {
                    // Previous and current VxPITCH is known
                    (Some(prev_inst), Some(note1_pitch)) => {
                        let prev_pitch = self
                            .pitch_table
                            .pitch_for_note(prev_inst, prev_note)
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
        instrument: Option<InstrumentId>,
        delay_length: TickCounter,
        slide_length: TickCounter,
    ) -> Result<(Option<u16>, TickCounter), ChannelError> {
        let (note1_pitch, pn1) = self.portamento_note_pitch(note1, instrument)?;

        let play_note1 = !self.portamento_prev_note_matches(note1_pitch, &pn1);

        // Play note1 (if required)
        let slide_length = match (play_note1, delay_length.value()) {
            (true, 0) => {
                // Play note1 for a single tick
                let t = PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(1).unwrap());

                self.portamento_play_pn_note_out(pn1, t)?;

                // subtract 1 tick from slide_length
                TickCounter::new(slide_length.value().saturating_sub(1))
            }
            (true, _) => {
                let (pn_length, wait) = Self::split_note_length(delay_length, true)?;

                self.portamento_play_pn_note_out(pn1, pn_length)?;

                if !wait.is_zero() {
                    // no keyoff event
                    self.wait(wait)?;
                }

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
        delay_length: TickCounter,
        slide_length: TickCounter,
        tie_length: TickCounter,
        rest_after_note: RestTicksAfterNote,
    ) -> Result<(), ChannelError> {
        #[cfg(debug_assertions)]
        let expected_tick_counter = self.bc.get_tick_counter()
            + delay_length
            + slide_length
            + tie_length
            + rest_after_note.0;

        let instrument = self.bc.get_state().instrument.instrument_id();

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
                    instrument,
                    delay_length,
                    slide_length,
                )?;
                (note1, p, s)
            }
        };

        let (note2_pitch, pn2) = self.portamento_note_pitch(note2, instrument)?;

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
                let direction = if let (Some(p1), Some(p2)) = (note1_pitch, note2_pitch) {
                    p1 < p2
                } else if let (NoteOrPitch::Note(n1), NoteOrPitch::Note(n2)) = (note1, note2) {
                    n1 < n2
                } else {
                    return Err(ChannelError::PortamentoNoteAndPitchWithoutInstrument);
                };

                if direction {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                }
            }
            None => {
                match (note1_pitch, note2_pitch) {
                    (Some(note1_pitch), Some(note2_pitch)) => {
                        let delta = i32::from(note2_pitch) - i32::from(note1_pitch);

                        let ticks = i32::try_from(slide_length.value()).unwrap();
                        assert!(ticks > 0);

                        // division with rounding
                        if delta > 0 {
                            (delta + (ticks / 2)) / ticks
                        } else {
                            (delta - (ticks / 2)) / ticks
                        }
                    }
                    _ => return Err(ChannelError::PortamentoRequiresInstrument),
                }
            }
        };
        let velocity = PortamentoVelocity::try_from(velocity)?;

        // In PMDMML only the portamento slide is quantized, delay_length is not.
        let (p_length, after) =
            self.split_play_note_length(slide_length + tie_length, is_slur, rest_after_note)?;

        match pn2 {
            NoteOrPitchOut::Note(dco, n, _) => {
                self.emit_detune_output(dco);
                self.bc.portamento(n, velocity, p_length)?;
            }
            NoteOrPitchOut::Pitch(p) => {
                self.bc.portamento_pitch(p, velocity, p_length);
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
        total_length: TickCounter,
        note_length: PlayNoteTicks,
        slur_last_note: bool,
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
        let last_note_ticks = {
            let mut t = total_ticks % note_length.ticks();

            if slur_last_note != note_length.is_slur() {
                // When slur_last_note is false: A keyoff is required after the loop.
                // When slur_last_note is true: A slurred note is required after the loop.
                if t == 0 {
                    t = note_length.ticks();
                }
            }

            if !slur_last_note && t > 0 && t < BcTicksKeyOff::MIN_TICKS {
                t = BcTicksKeyOff::MIN_TICKS;
            }

            t
        };

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

        for (i, &n) in notes.iter().enumerate() {
            if i == break_point && i != 0 {
                self.bc.skip_last_loop()?;
            }

            match self.broken_chord_play_note_or_pitch_with_detune(n, note_length) {
                Ok(()) => (),
                Err(e) => {
                    // Hides an unneeded "loop stack not empty" (or end_loop) error
                    let _ = self.bc.end_loop(None);
                    return Err(e);
                }
            }
        }

        self.bc.end_loop(None)?;

        if last_note_ticks > 0 {
            let l = match slur_last_note {
                false => PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(last_note_ticks)?),
                true => PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(last_note_ticks)?),
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

    fn set_instrument(&mut self, inst: usize) -> Result<(), ChannelError> {
        let inst = match self.instruments.get(inst) {
            Some(inst) => inst,
            None => panic!("invalid instrument index"),
        };

        let old_inst = &self.bc.get_state().instrument;
        let old_envelope = &self.bc.get_state().envelope;

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
        let sub = match self.subroutines.get(index) {
            Some(s) => s,
            None => panic!("invalid SubroutineId"),
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

    pub fn process_command(&mut self, command: &Command) -> Result<(), ChannelError> {
        match command {
            Command::None => (),

            &Command::SetLoopPoint => match self.bc.get_context() {
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

                    self.bc._song_loop_point();
                }
                BytecodeContext::SongSubroutine { .. }
                | BytecodeContext::SfxSubroutine
                | BytecodeContext::SoundEffect
                | BytecodeContext::MmlPrefix => return Err(ChannelError::CannotSetLoopPoint),
            },

            &Command::SetSubroutineInstrumentHint(inst_index) => {
                let inst = self
                    .instruments
                    .get(inst_index)
                    .expect("invalid instrument index");
                self.bc.set_subroutine_instrument_hint(inst)?;
            }

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

            &Command::SetQuantize(q) => {
                self.quantize = q;
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

                self.bc.play_pitch(pitch, pp_length);
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

                self.bc.play_pitch(pitch, pp_length);
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

                self.bc.play_noise(frequency, pp_length);
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
