//! Compiler commands

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::analysis::{SkipLastLoopTransposeAnalysis, TransposeAnalysis};

use crate::bytecode::{
    DetuneValue, EarlyReleaseMinTicks, EarlyReleaseTicks, InstrumentId, LoopCount, NoiseFrequency,
    Pan, PanSlideAmount, PanSlideTicks, PanbrelloAmplitude, PanbrelloQuarterWavelengthInTicks,
    PlayNoteTicks, PlayPitchPitch, RelativeEchoFeedback, RelativeEchoVolume,
    RelativeFirCoefficient, RelativePan, RelativeTranspose, Transpose, TremoloAmplitude,
    TremoloQuarterWavelengthInTicks, VibratoDelayTicks, VibratoPitchOffsetPerTick,
    VibratoQuarterWavelengthInTicks, Volume, VolumeSlideAmount, VolumeSlideTicks,
};
use crate::command_compiler::analysis::SubroutineAnalysis;
use crate::command_compiler::subroutines::SubroutineCommandsWithCompileOrder;
use crate::driver_constants::{FIR_FILTER_SIZE, N_MUSIC_CHANNELS};
use crate::echo::{EchoFeedback, EchoLength, EchoVolume, FirCoefficient, FirTap};
use crate::envelope::{Envelope, OptionalGain, TempGain};
use crate::errors::{ChannelError, ErrorWithPos, ValueError};
use crate::identifier::{IdentifierBuf, IdentifierStr};
use crate::invert_flags::InvertFlags;
use crate::mml::{CursorTracker, Section};
use crate::notes::Note;
use crate::pitch_table::PlayPitchFrequency;
use crate::songs::MetaData;
use crate::time::{Bpm, CommandTicks, TickClock};
use crate::value_newtypes::{i16_value_newtype, u8_value_newtype, SignedValueNewType};
use crate::{FilePos, FilePosRange};

use std::cmp::min;
use std::io::Write;

pub const MAX_PORTAMENTO_SLIDE_TICKS: u16 = 16 * 1024;
pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

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
pub(super) enum VolumeCommand {
    Absolute(Volume),
    Relative(i16),
}

#[derive(Debug, Copy, Clone)]
pub(super) enum PanCommand {
    Absolute(Pan),
    Relative(RelativePan),
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

    pub fn quantize(&self, l: u16) -> u16 {
        const UNITS: u64 = FineQuantization::UNITS as u64;
        const _: () = assert!((u16::MAX as u64 * FineQuantization::MAX.0 as u64) < u64::MAX);
        const _: () =
            assert!((u16::MAX as u64 * FineQuantization::MAX.0 as u64) / UNITS < u16::MAX as u64);

        // ::TODO can this be changed to u32?::
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
pub struct RestTicksAfterNote(pub CommandTicks);

#[derive(Debug, Clone, Copy)]
pub struct TicksAfterKeyoff(pub CommandTicks);

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

#[derive(Default, Debug)]
pub(crate) struct MergeableCommands {
    pub(super) instrument: Option<InstrumentId>,
    pub(super) envelope: Option<Envelope>,

    pub(super) volume: Option<VolumeCommand>,
    pub(super) pan: Option<PanCommand>,
}

impl MergeableCommands {
    // SAFETY: ChannelBcGenerator will PANIC if instrument_id is invalid
    pub fn set_instrument(&mut self, instrument_id: InstrumentId, envelope: Option<Envelope>) {
        self.instrument = Some(instrument_id);
        self.envelope = envelope;
    }

    pub fn set_envelope(&mut self, envelope: Envelope) {
        self.envelope = Some(envelope);
    }

    pub fn set_volume(&mut self, v: Volume) {
        self.volume = Some(VolumeCommand::Absolute(v));
    }

    pub fn adjust_volume(&mut self, adjust: i32) {
        let rel = |rv: i32| {
            if rv <= -(Volume::MAX.as_u8() as i32) {
                VolumeCommand::Absolute(Volume::MIN)
            } else if rv >= Volume::MAX.as_u8().into() {
                VolumeCommand::Absolute(Volume::MAX)
            } else {
                VolumeCommand::Relative(rv.try_into().unwrap())
            }
        };

        self.volume = Some(match self.volume {
            Some(VolumeCommand::Absolute(v)) => {
                let v = u32::from(v.as_u8()).saturating_add_signed(adjust);
                let v = u8::try_from(v).unwrap_or(u8::MAX);
                VolumeCommand::Absolute(Volume::new(v))
            }
            Some(VolumeCommand::Relative(rv)) => rel(i32::from(rv).saturating_add(adjust)),
            None => rel(adjust),
        });
    }

    pub fn set_pan(&mut self, v: Pan) {
        self.pan = Some(PanCommand::Absolute(v));
    }

    pub fn adjust_pan(&mut self, adjust: i32) {
        let rel = |rp: i32| {
            if rp <= -(Pan::MAX.as_u8() as i32) {
                PanCommand::Absolute(Pan::MIN)
            } else if rp >= Pan::MAX.as_u8().into() {
                PanCommand::Absolute(Pan::MAX)
            } else {
                PanCommand::Relative(rp.try_into().unwrap())
            }
        };

        self.pan = Some(match self.pan {
            Some(PanCommand::Absolute(p)) => {
                let p = min(
                    u32::from(p.as_u8()).saturating_add_signed(adjust),
                    Pan::MAX.as_u8() as u32,
                );
                PanCommand::Absolute(p.try_into().unwrap())
            }
            Some(PanCommand::Relative(rp)) => rel(i32::from(rp.value()).saturating_add(adjust)),
            None => rel(adjust),
        });
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum InstrumentAnalysis {
    Set(InstrumentId),
    Hint(InstrumentId),
}

impl InstrumentAnalysis {
    pub fn instrument_id(self) -> InstrumentId {
        match self {
            Self::Set(i) | Self::Hint(i) => i,
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct LoopAnalysis {
    pub(crate) instrument: Option<InstrumentAnalysis>,
    pub(crate) transpose: TransposeAnalysis,
}

impl LoopAnalysis {
    pub const BLANK: &'static Self = &Self {
        instrument: None,
        transpose: TransposeAnalysis::BLANK,
    };
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SkipLastLoopAnalysis {
    pub(crate) transpose: SkipLastLoopTransposeAnalysis,
}

impl SkipLastLoopAnalysis {
    pub const BLANK: &'static Self = &Self {
        transpose: SkipLastLoopTransposeAnalysis::BLANK,
    };
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

    SetLoopPoint(LoopAnalysis),

    MergeableCommands(MergeableCommands),

    SetManualVibrato(Option<ManualVibrato>),
    SetMpVibrato(Option<MpVibrato>),
    SetQuantize(Quantize),

    Rest {
        /// Length of the first rest
        /// (The user expects a keyoff after the first rest command)
        ticks_until_keyoff: CommandTicks,
        /// Combined length of all rests after the first rest
        /// (keyoff already sent, it does not matter if these rests keyoff or not)
        ticks_after_keyoff: TicksAfterKeyoff,
    },

    // wait with no keyoff
    Wait(CommandTicks),

    PlayNote {
        note: Note,
        length: CommandTicks,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    PlayPitch {
        pitch: PlayPitchPitch,
        length: CommandTicks,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    PlayPitchFrequency {
        frequency: PlayPitchFrequency,
        length: CommandTicks,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    PlayNoise {
        frequency: NoiseFrequency,
        length: CommandTicks,
        is_slur: bool,
        rest_after_note: RestTicksAfterNote,
    },
    Portamento {
        note1: Option<NoteOrPitch>,
        note2: NoteOrPitch,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        /// Number of ticks to hold the pitch at note1 before the pitch slide
        delay_length: CommandTicks,
        /// Length of the pitch slide (portamento_length - delay_length)
        slide_length: CommandTicks,
        /// Number of ticks to hold the pitch at note2
        tie_length: CommandTicks,
        rest_after_note: RestTicksAfterNote,
    },
    BrokenChord {
        notes: Vec<NoteOrPitch>,
        total_length: CommandTicks,
        note_length: PlayNoteTicks,
        slur_last_note: bool,
    },

    DisableNoise,

    CallSubroutine(u8, SubroutineCallType),
    StartLoop(Option<LoopCount>, LoopAnalysis),
    SkipLastLoop(SkipLastLoopAnalysis),
    EndLoop(Option<LoopCount>, LoopAnalysis),

    SetSubroutineInstrumentHint(InstrumentId, Option<Envelope>),

    // InstrumentId must be valid
    // Will not be optimised away
    SetInstrumentAsm(InstrumentId, Option<Envelope>),

    // None reuses previous temp gain
    TempGain(Option<TempGain>),
    TempGainAndRest {
        temp_gain: Option<TempGain>,
        ticks_until_keyoff: CommandTicks,
        ticks_after_keyoff: TicksAfterKeyoff,
    },
    TempGainAndWait(Option<TempGain>, CommandTicks),

    DisableEarlyRelease,
    SetEarlyRelease(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain),

    SetTranspose(Transpose),
    AdjustTranspose(RelativeTranspose),

    SetDetune(DetuneValue),
    SetDetuneCents(DetuneCents),

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

    // MUST NOT contain a transpose, loop or subroutine call instruction.
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

    pub(super) fn command_mut(&mut self) -> &mut Command<'a> {
        &mut self.command
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
    pub(crate) errors: Vec<ErrorWithPos<ChannelError>>,
    pub(crate) end_pos: FilePos,
}

impl ChannelCommands<'_> {
    pub fn blank_commands() -> Self {
        Self {
            commands: Vec::new(),
            errors: Vec::new(),
            end_pos: FilePos {
                line_number: 0,
                line_char: 0,
                char_index: 0,
            },
        }
    }
}

pub(crate) struct SubroutineCommands<'a> {
    pub(crate) index: u8,
    pub(crate) identifier: IdentifierStr<'a>,
    pub(crate) commands: Vec<CommandWithPos<'a>>,
    pub(crate) errors: Vec<ErrorWithPos<ChannelError>>,
    pub(crate) end_pos: FilePos,
    pub(crate) analysis: LoopAnalysis,
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
    pub(crate) envelope: Option<Envelope>,
}

pub(crate) struct SongCommands<'a> {
    pub name: String,
    pub metadata: MetaData,
    pub sections: Vec<Section>,
    pub mml_tracking: CursorTracker,

    pub instruments: Vec<MmlInstrument>,
    pub subroutines: Vec<SubroutineCommands<'a>>,
    pub channels: [Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS],
}

pub(crate) struct SfxSubroutineCommands<'a> {
    pub subroutines: Vec<SubroutineCommands<'a>>,
    pub mml_tracker: CursorTracker,
}

pub(crate) struct SoundEffectCommands<'a> {
    pub commands: Vec<CommandWithPos<'a>>,
    pub end_pos: FilePos,
    pub errors: Vec<ErrorWithPos<ChannelError>>,
    pub mml_tracker: Option<CursorTracker>,
}

// Prints the song commands and analysis to stderr.
//
// Panics if writing to io::stderr() fails.
#[allow(dead_code)]
pub(crate) fn debug_print_song_commands(
    subroutines: &SubroutineCommandsWithCompileOrder,
    channels: &[Option<ChannelCommands>; N_MUSIC_CHANNELS],
    analysis: &SubroutineAnalysis,
) {
    let mut f = std::io::stderr().lock();

    for s in subroutines.original_order() {
        writeln!(f, "SUBROUTINE {} !{}", s.index, s.identifier.as_str()).unwrap();
        writeln!(
            f,
            "  transpose_at_subroutine_start: {:?}",
            analysis.transpose_at_subroutine_start(s.index)
        )
        .unwrap();
        writeln!(
            f,
            "  subroutine_analysis: {:?}",
            &analysis.subroutine_analysis[usize::from(s.index)]
        )
        .unwrap();

        writeln!(f, "  {:?}", s.analysis).unwrap();
        for e in &s.errors {
            writeln!(f, "  {e:?}").unwrap();
        }
        for c in &s.commands {
            writeln!(f, "  {:?}", c.command).unwrap();
        }
        writeln!(f).unwrap();
    }

    for (i, c) in channels.iter().enumerate() {
        if let Some(c) = c {
            writeln!(f, "CHANNEL {}", i).unwrap();
            for e in &c.errors {
                writeln!(f, "  {e:?}").unwrap();
            }
            for c in &c.commands {
                writeln!(f, "  {:?}", c.command).unwrap();
            }
            writeln!(f).unwrap();
        }
    }
}
