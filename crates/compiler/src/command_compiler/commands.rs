//! Compiler commands

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    DetuneValue, EarlyReleaseMinTicks, EarlyReleaseTicks, InstrumentId, LoopCount, NoiseFrequency,
    Pan, PanSlideAmount, PanSlideTicks, PanbrelloAmplitude, PanbrelloQuarterWavelengthInTicks,
    PlayNoteTicks, PlayPitchPitch, RelativeEchoFeedback, RelativeEchoVolume,
    RelativeFirCoefficient, RelativePan, RelativeTranspose, Transpose, TremoloAmplitude,
    TremoloQuarterWavelengthInTicks, VibratoDelayTicks, VibratoPitchOffsetPerTick,
    VibratoQuarterWavelengthInTicks, Volume, VolumeSlideAmount, VolumeSlideTicks,
};
use crate::driver_constants::{FIR_FILTER_SIZE, N_MUSIC_CHANNELS};
use crate::echo::{EchoFeedback, EchoLength, EchoVolume, FirCoefficient, FirTap};
use crate::envelope::{Adsr, Envelope, Gain, OptionalGain, TempGain};
use crate::errors::{ChannelError, ErrorWithPos, MmlChannelError, ValueError};
use crate::identifier::{IdentifierBuf, IdentifierStr};
use crate::invert_flags::InvertFlags;
use crate::mml::{CursorTracker, Section};
use crate::notes::Note;
use crate::pitch_table::PlayPitchFrequency;
use crate::songs::MetaData;
use crate::time::{Bpm, TickClock, TickCounter};
use crate::value_newtypes::{i16_value_newtype, u8_value_newtype, SignedValueNewType};
use crate::{FilePos, FilePosRange};

use std::cmp::min;
use std::ops::RangeInclusive;

pub const MAX_PORTAMENTO_SLIDE_TICKS: u32 = 16 * 1024;
pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

// Number of rest instructions before a loop uses less space
pub const REST_LOOP_INSTRUCTION_THREASHOLD: u32 = 3;

u8_value_newtype!(
    PortamentoSpeed,
    PortamentoSpeedOutOfRange,
    NoPortamentoSpeed,
    1,
    u8::MAX
);

#[derive(Debug, Copy, Clone)]
pub enum NoteOrPitch {
    Note(Note),
    Pitch(PlayPitchPitch),
    PitchFrequency(PlayPitchFrequency),
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

#[derive(Debug, Copy, Clone, PartialEq)]
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

#[derive(Debug, Clone, Copy)]
pub enum Quantize {
    None,
    Rest(FineQuantization),
    WithTempGain(FineQuantization, TempGain),
}

#[derive(Debug, Clone, Copy)]
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
pub enum SubroutineCallType {
    Mml,
    Asm,
    AsmDisableVibrato,
}

#[derive(Debug)]
pub(crate) enum Command<'a> {
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

    CallSubroutine(u8, SubroutineCallType),
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

    SetTranspose(Transpose),
    AdjustTranspose(RelativeTranspose),

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

    BytecodeAsm(&'a str),
}

pub(crate) struct CommandWithPos<'a> {
    command: Command<'a>,
    pos: FilePosRange,
    end_pos: u32,
}

impl<'a> CommandWithPos<'a> {
    pub fn new(command: Command<'a>, pos: FilePosRange, end_pos: u32) -> Self {
        Self {
            command,
            pos,
            end_pos,
        }
    }

    pub fn command(&self) -> &Command<'a> {
        &self.command
    }
    pub fn pos(&self) -> &FilePosRange {
        &self.pos
    }
    pub fn end_pos(&self) -> u32 {
        self.end_pos
    }
}

pub(crate) struct ChannelCommands<'a> {
    pub(crate) commands: Vec<CommandWithPos<'a>>,
    pub(crate) end_pos: FilePos,
}

impl ChannelCommands<'_> {
    pub fn end_pos_range(&self) -> FilePosRange {
        self.end_pos.to_range(1)
    }
}

pub(crate) struct SubroutineCommands<'a> {
    pub(crate) index: u8,
    pub(crate) identifier: IdentifierStr<'a>,
    pub(crate) commands: Vec<CommandWithPos<'a>>,
    pub(crate) end_pos: FilePos,
}

impl SubroutineCommands<'_> {
    pub fn end_pos_range(&self) -> FilePosRange {
        self.end_pos.to_range(1)
    }
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

pub(crate) struct SongCommands<'a> {
    pub name: String,
    pub metadata: MetaData,
    pub sections: Vec<Section>,
    pub mml_tracking: CursorTracker,

    pub instruments: Vec<MmlInstrument>,
    pub subroutines: Vec<SubroutineCommands<'a>>,
    pub channels: [Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS],

    // ::TODO move this check to command_compiler::
    pub song_uses_driver_transpose: bool,
}

pub(crate) struct SfxSubroutineCommands<'a> {
    pub instruments: Vec<MmlInstrument>,
    pub subroutines: Vec<SubroutineCommands<'a>>,
    pub mml_tracker: CursorTracker,
    pub errors: Vec<MmlChannelError>,
}

pub(crate) struct SoundEffectCommands<'a> {
    pub instruments: Vec<MmlInstrument>,
    pub commands: ChannelCommands<'a>,
    pub errors: Vec<ErrorWithPos<ChannelError>>,
    pub mml_tracker: CursorTracker,
}
