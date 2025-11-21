//! Audio driver bytecode

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::command_compiler::analysis::{TransposeRange, TransposeStartRange};
use crate::command_compiler::commands::{InstrumentAnalysis, LoopAnalysis, SkipLastLoopAnalysis};
use crate::data::{self, InstrumentOrSample, UniqueNamesList};
use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, BC_STACK_BYTES_PER_SUBROUTINE_CALL,
    FIR_FILTER_SIZE, MAX_INSTRUMENTS_AND_SAMPLES,
};
use crate::echo::{EchoEdl, EchoFeedback, EchoLength, EchoVolume, FirCoefficient, FirTap};
use crate::envelope::{Adsr, Envelope, Gain, OptionalGain, TempGain};
use crate::errors::{BytecodeError, ChannelError, ValueError};
use crate::identifier::MusicChannelIndex;
use crate::invert_flags::InvertFlags;
use crate::notes::{Note, LAST_NOTE_ID, N_NOTES};
use crate::pitch_table::{InstrumentHintFreq, PitchTable, PitchTableOffset};
use crate::samples::{instrument_note_range, note_range};
use crate::subroutines::{CompiledSubroutines, GetSubroutineResult, Subroutine, SubroutineNameMap};
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::value_newtypes::{
    i16_non_zero_value_newtype, i16_value_newtype, i8_value_newtype, u16_value_newtype,
    u8_0_is_256_value_newtype, u8_value_newtype, SignedValueNewType, UnsignedValueNewType,
};

use core::unreachable;
use std::cmp::{max, min};
use std::ops::RangeInclusive;

pub const KEY_OFF_TICK_DELAY: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct StackDepth(u32);

const MAX_STACK_DEPTH: StackDepth = StackDepth(BC_CHANNEL_STACK_SIZE as u32);

u8_value_newtype!(Volume, VolumeOutOfRange, NoVolume);
u8_value_newtype!(Pan, PanOutOfRange, NoPan, 0, 128);
i8_value_newtype!(
    RelativeVolume,
    RelativeVolumeOutOfRange,
    NoRelativeVolume,
    NoRelativeVolumeSign
);
i8_value_newtype!(
    RelativePan,
    RelativePanOutOfRange,
    NoRelativePan,
    NoRelativePanSign
);

u8_value_newtype!(
    VibratoPitchOffsetPerTick,
    VibratoPitchOffsetPerTickOutOfRange,
    NoVibratoPitchOffsetPerTick
);
u8_value_newtype!(
    VibratoQuarterWavelengthInTicks,
    VibratoQuarterWavelengthOutOfRange,
    NoVibratoQuarterWavelength,
    1,
    128
);

u8_value_newtype!(
    VibratoDelayTicks,
    VibratoDelayTicksOutOfRange,
    NoVibratoDelayTicks
);

i16_non_zero_value_newtype!(
    VolumeSlideAmount,
    VolumeSlideAmountOutOfRange,
    NoVolumeSlideDirection,
    NoVolumeSlideDirection,
    VolumeSlideAmountZero,
    -(Volume::MAX.as_u8() as i16),
    Volume::MAX.as_u8() as i16
);
u8_0_is_256_value_newtype!(
    VolumeSlideTicks,
    VolumeSlideTicksOutOfRange,
    NoVolumeSlideTicks
);

u8_value_newtype!(
    TremoloAmplitude,
    TremoloAmplitudeOutOfRange,
    NoTremoloAmplitude,
    1,
    Volume::MAX.as_u8() / 2
);
u8_value_newtype!(
    TremoloQuarterWavelengthInTicks,
    TremoloQuarterWavelengthTicksOutOfRange,
    NoTremoloQuarterWavelengthTicks,
    1,
    127
);

i16_non_zero_value_newtype!(
    PanSlideAmount,
    PanSlideAmountOutOfRange,
    NoPanSlideDirection,
    NoPanSlideDirection,
    PanSlideAmountZero,
    -(Pan::MAX.as_u8() as i16),
    Pan::MAX.as_u8() as i16
);
u8_0_is_256_value_newtype!(PanSlideTicks, PanSlideTicksOutOfRange, NoPanSlideTicks);

u8_value_newtype!(
    PanbrelloAmplitude,
    PanbrelloAmplitudeOutOfRange,
    NoPanbrelloAmplitude,
    1,
    Pan::MAX.as_u8() / 2
);
u8_value_newtype!(
    PanbrelloQuarterWavelengthInTicks,
    PanbrelloQuarterWavelengthTicksOutOfRange,
    NoPanbrelloQuarterWavelengthTicks,
    1,
    127
);

u16_value_newtype!(
    PlayPitchPitch,
    PlayPitchPitchOutOfRange,
    NoPlayPitchPitch,
    0,
    (1 << 14) - 1
);

impl PlayPitchPitch {
    pub const NATIVE: Self = Self(0x1000);
}

i8_value_newtype!(Transpose, TransposeOutOfRange, NoTranspose, NoTransposeSign);

impl Transpose {
    pub const ZERO: Self = Self(0);
}

i8_value_newtype!(
    RelativeTranspose,
    RelativeTransposeOutOfRange,
    NoRelativeTranspose,
    NoRelativeTransposeSign
);

i16_value_newtype!(
    DetuneValue,
    DetuneValueOutOfRange,
    NoDetuneValue,
    NoDetuneValueSign,
    -((1 << 14) - 1),
    (1 << 14) - 1
);

impl DetuneValue {
    pub const ZERO: Self = Self(0);
}

u8_value_newtype!(
    NoiseFrequency,
    NoiseFrequencyOutOfRange,
    NoNoiseFrequency,
    0,
    (1 << 5) - 1
);

impl NoiseFrequency {
    pub const MASK: u8 = (1 << 5) - 1;
}

impl Pan {
    pub const CENTER: Pan = Self(Self::MAX.0 / 2);
}

i8_value_newtype!(
    RelativeEchoVolume,
    RelativeEchoVolumeOutOfRange,
    NoRelativeEchoVolume,
    NoRelativeEchoVolumeSign,
    -(EchoVolume::MAX.as_u8() as i8),
    EchoVolume::MAX.as_u8() as i8
);

i8_value_newtype!(
    RelativeEchoFeedback,
    RelativeEchoFeedbackOutOfRange,
    NoRelativeEchoFeedback,
    NoRelativeEchoFeedbackSign
);

impl RelativeEchoFeedback {
    pub const ZERO: Self = Self(0);
}

i8_value_newtype!(
    RelativeFirCoefficient,
    RelativeFirCoefficientOutOfRange,
    NoRelativeFirCoefficient,
    NoRelativeFirCoefficientSign
);

impl RelativeFirCoefficient {
    pub const ZERO: Self = Self(0);
}

pub enum BytecodeContext {
    SongSubroutine {
        max_edl: EchoEdl,
    },
    SongChannel {
        index: MusicChannelIndex,
        max_edl: EchoEdl,
    },
    SfxSubroutine,
    SoundEffect,
    MmlPrefix,

    // UnitTestAssembly* disables note range checking and max_edl tests
    // (note range checks requires loop analysis and I do not want any pre-processing in unit test assembly)
    #[cfg(feature = "test")]
    UnitTestAssembly,
    #[cfg(feature = "test")]
    UnitTestAssemblySubroutine,
}

impl BytecodeContext {
    fn is_subroutine(&self) -> bool {
        match self {
            Self::SongSubroutine { .. } | Self::SfxSubroutine => true,

            Self::SongChannel { .. } | Self::SoundEffect | Self::MmlPrefix => false,

            #[cfg(feature = "test")]
            Self::UnitTestAssemblySubroutine => true,
            #[cfg(feature = "test")]
            Self::UnitTestAssembly => false,
        }
    }

    #[cfg(feature = "test")]
    pub(crate) fn is_unit_test_assembly(&self) -> bool {
        match self {
            Self::SongSubroutine { .. }
            | Self::SfxSubroutine
            | Self::SongChannel { .. }
            | Self::SoundEffect
            | Self::MmlPrefix => false,

            #[cfg(feature = "test")]
            Self::UnitTestAssembly | Self::UnitTestAssemblySubroutine => true,
        }
    }
}

pub mod opcodes {
    // opcodes 0x00..FIRST_PLAY_NOTE_INSTRUCTION

    macro_rules! declare_opcode_recursive {
        ($opcode:expr, $name:ident) => {
            pub const $name : u8 = $opcode;
            const _ : () = assert!($name < DISABLE_CHANNEL, "Too many opcodes");
        };
        ($opcode:expr, $name:ident, $($tail:ident),+) => {
            pub const $name : u8 = $opcode;
            declare_opcode_recursive!($opcode + 1u8, $($tail),+);
        };
    }

    macro_rules! declare_opcodes {
        ($first:ident, $($tail:ident),+ $(,)?) => {
            declare_opcode_recursive!(0u8, $first, $($tail),+);
        };
    }

    // Order MUST MATCH `audio-driver/src/bytecode.wiz`
    declare_opcodes!(
        RESERVED_FOR_CUSTOM_USE,
        MISCELLANEOUS,
        PORTAMENTO_DOWN,
        PORTAMENTO_UP,
        PORTAMENTO_CALC,
        PORTAMENTO_PITCH_DOWN,
        PORTAMENTO_PITCH_UP,
        PORTAMENTO_PITCH_CALC,
        SET_VIBRATO,
        SET_VIBRATO_WITH_DELAY,
        SET_VIBRATO_DEPTH_AND_PLAY_NOTE,
        PLAY_PITCH,
        PLAY_NOISE,
        WAIT,
        REST,
        SET_INSTRUMENT,
        SET_INSTRUMENT_AND_ADSR_OR_GAIN,
        SET_ADSR,
        SET_GAIN,
        SET_TEMP_GAIN,
        SET_TEMP_GAIN_AND_WAIT,
        SET_TEMP_GAIN_AND_REST,
        REUSE_TEMP_GAIN_AND_WAIT,
        REUSE_TEMP_GAIN_AND_REST,
        SET_EARLY_RELEASE,
        SET_EARLY_RELEASE_NO_MINIMUM,
        SET_TRANSPOSE,
        ADJUST_TRANSPOSE,
        SET_DETUNE_I16,
        SET_DETUNE_P8,
        SET_DETUNE_N8,
        ADJUST_PAN,
        SET_PAN,
        SET_PAN_AND_VOLUME,
        ADJUST_VOLUME,
        SET_VOLUME,
        SET_CHANNEL_OR_ECHO_INVERT,
        VOLUME_SLIDE_UP,
        VOLUME_SLIDE_DOWN,
        TREMOLO,
        PAN_SLIDE_UP,
        PAN_SLIDE_DOWN,
        PANBRELLO,
        START_LOOP,
        SKIP_LAST_LOOP_U8,
        SKIP_LAST_LOOP_U16BE,
        CALL_SUBROUTINE_AND_DISABLE_VIBRATO,
        CALL_SUBROUTINE,
        GOTO_RELATIVE,
        SET_ECHO_VOLUME,
        SET_STEREO_ECHO_VOLUME,
        ADJUST_ECHO_VOLUME,
        ADJUST_STEREO_ECHO_VOLUME,
        SET_FIR_FILTER,
        SET_OR_ADJUST_ECHO_I8,
        ADJUST_ECHO_I8_LIMIT,
        END_LOOP,
        RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO,
        RETURN_FROM_SUBROUTINE,
        ENABLE_ECHO,
        DISABLE_ECHO,
        REUSE_TEMP_GAIN,
        KEYON_NEXT_NOTE,
    );

    // Last non play-note opcode
    pub const DISABLE_CHANNEL: u8 = FIRST_PLAY_NOTE_INSTRUCTION - 1;

    // Opcodes FIRST_PLAY_NOTE_INSTRUCTION.. are play note opcodes
    pub const FIRST_PLAY_NOTE_INSTRUCTION: u8 = 64;
}

const _: () = assert!(
    opcodes::FIRST_PLAY_NOTE_INSTRUCTION as u32 + (N_NOTES * 2) as u32 == 0x100,
    "There are unaccounted bytecode opcodes"
);

const _: () = assert!(opcodes::RESERVED_FOR_CUSTOM_USE == 0);

#[derive(Clone, Copy)]
#[allow(clippy::unusual_byte_groupings)]
pub enum MiscInstruction {
    DisableNoise = 0,
    DisablePmod = 0b000001_00,
    EnablePmod = 0b000000_10,
    SetSongTickClock = 0b000000_01,
    SetEchoDelay = 0b000000_11,
}

impl MiscInstruction {
    fn as_u8(&self) -> u8 {
        *self as u8
    }

    pub fn from_opcode(o: u8) -> Self {
        if o == 0 {
            Self::DisableNoise
        } else {
            match o & 0b11 {
                0b00 => Self::DisablePmod,
                0b10 => Self::EnablePmod,
                0b01 => Self::SetSongTickClock,
                0b11 => Self::SetEchoDelay,
                _ => unreachable!("Invalid prefix"),
            }
        }
    }
}

const ECHO_I8_EFB_INDEX: u8 = 8;

pub const MAX_SET_OR_ADJUST_ECHO_I8_PARAM: u8 = (ECHO_I8_EFB_INDEX << 1) | 1;

const _: () = assert!(FirTap::MAX.as_u8() < ECHO_I8_EFB_INDEX);

pub(crate) const GOTO_RELATIVE_INSTRUCTION_SIZE: usize = 3;

u8_value_newtype!(
    InstrumentId,
    InstrumentIdOutOfRange,
    NoInstrumentId,
    0,
    (MAX_INSTRUMENTS_AND_SAMPLES - 1) as u8
);

impl StackDepth {
    pub fn to_u32(self) -> u32 {
        self.0
    }
}

pub trait BcTicks
where
    Self: TryFrom<u32>,
{
    const MIN: Self;
    const MAX: Self;

    const MIN_TICKS: u32;
    const MAX_TICKS: u32;

    fn ticks(self) -> u32;
    fn to_tick_count(self) -> TickCounter;
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BcTicksKeyOff {
    ticks: u16,
    bc_argument: u8,
}

impl BcTicks for BcTicksKeyOff {
    const MIN_TICKS: u32 = 1 + KEY_OFF_TICK_DELAY;
    const MAX_TICKS: u32 = 0x100 + KEY_OFF_TICK_DELAY;

    const MIN: Self = Self {
        ticks: Self::MIN_TICKS as u16,
        bc_argument: 1,
    };
    const MAX: Self = Self {
        ticks: Self::MAX_TICKS as u16,
        bc_argument: 0,
    };

    fn ticks(self) -> u32 {
        self.ticks.into()
    }

    fn to_tick_count(self) -> TickCounter {
        TickCounter::new(self.ticks.into())
    }
}

impl TryFrom<u32> for BcTicksKeyOff {
    type Error = ValueError;

    fn try_from(ticks: u32) -> Result<Self, ValueError> {
        if matches!(ticks, Self::MIN_TICKS..=Self::MAX_TICKS) {
            Ok(Self {
                ticks: ticks.try_into().unwrap(),
                bc_argument: ((ticks - KEY_OFF_TICK_DELAY) & 0xff).try_into().unwrap(),
            })
        } else {
            Err(ValueError::BcTicksKeyOffOutOfRange(ticks))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BcTicksNoKeyOff {
    ticks: u16,
    bc_argument: u8,
}

impl BcTicks for BcTicksNoKeyOff {
    const MIN_TICKS: u32 = 1;
    const MAX_TICKS: u32 = 0x100;

    const MIN: Self = Self {
        ticks: Self::MIN_TICKS as u16,
        bc_argument: 1,
    };
    const MAX: Self = Self {
        ticks: Self::MAX_TICKS as u16,
        bc_argument: 0,
    };

    #[allow(dead_code)]
    fn ticks(self) -> u32 {
        self.ticks.into()
    }

    fn to_tick_count(self) -> TickCounter {
        TickCounter::new(self.ticks.into())
    }
}

impl TryFrom<u32> for BcTicksNoKeyOff {
    type Error = ValueError;

    fn try_from(ticks: u32) -> Result<Self, ValueError> {
        if matches!(ticks, Self::MIN_TICKS..=Self::MAX_TICKS) {
            // A note length of 0 will wait for 256 ticks.
            let bc_argument = (ticks & 0xff).try_into().unwrap();
            Ok(Self {
                ticks: ticks.try_into().unwrap(),
                bc_argument,
            })
        } else {
            Err(ValueError::BcTicksNoKeyOffOutOfRange(ticks))
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PlayNoteTicks {
    KeyOff(BcTicksKeyOff),
    NoKeyOff(BcTicksNoKeyOff),
}

impl PlayNoteTicks {
    pub fn try_from_is_slur(ticks: u32, is_slur: bool) -> Result<Self, ValueError> {
        if is_slur {
            Ok(PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(ticks)?))
        } else {
            Ok(PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(ticks)?))
        }
    }

    pub fn min_for_is_slur(is_slur: bool) -> Self {
        if is_slur {
            PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::MIN)
        } else {
            PlayNoteTicks::KeyOff(BcTicksKeyOff::MIN)
        }
    }

    pub fn ticks(&self) -> u32 {
        match self {
            Self::KeyOff(l) => l.ticks.into(),
            Self::NoKeyOff(l) => l.ticks.into(),
        }
    }

    pub fn is_slur(&self) -> bool {
        match self {
            Self::KeyOff(_) => false,
            Self::NoKeyOff(_) => true,
        }
    }

    fn bc_argument(&self) -> u8 {
        match self {
            Self::KeyOff(l) => l.bc_argument,
            Self::NoKeyOff(l) => l.bc_argument,
        }
    }

    fn to_tick_count(self) -> TickCounter {
        match self {
            Self::KeyOff(l) => TickCounter::new(l.ticks.into()),
            Self::NoKeyOff(l) => TickCounter::new(l.ticks.into()),
        }
    }
}

i16_non_zero_value_newtype!(
    PortamentoVelocity,
    PortamentoVelocityOutOfRange,
    NoPortamentoVelocity,
    NoDirectionInPortamentoVelocity,
    PortamentoVelocityZero,
    -(u8::MAX as i16),
    u8::MAX as i16
);

impl PortamentoVelocity {
    pub fn pitch_offset_per_tick(&self) -> u8 {
        u8::try_from(self.0.unsigned_abs()).unwrap()
    }
}

u8_value_newtype!(
    PortamentoSlideTicks,
    PortamentoSlideTicksOutOfRange,
    NoPortamentoSlideTicks,
    1,
    u8::MAX
);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopCount(u8);

impl LoopCount {
    pub const MIN_LOOPS: u32 = 2;
    pub const MAX_LOOPS: u32 = 0x100;

    pub const MIN: Self = Self(Self::MIN_LOOPS as u8);
    pub const MAX: Self = Self(0);

    pub fn to_i16(self) -> i16 {
        if self.0 == 0 {
            0x100
        } else {
            self.0.into()
        }
    }

    pub fn to_u32(self) -> u32 {
        if self.0 == 0 {
            0x100
        } else {
            self.0.into()
        }
    }
}

impl UnsignedValueNewType for LoopCount {
    type ValueType = u32;

    const MISSING_ERROR: ValueError = ValueError::NoLoopCount;

    fn value(&self) -> Self::ValueType {
        self.to_u32()
    }
}

impl TryFrom<u32> for LoopCount {
    type Error = ValueError;

    fn try_from(loop_count: u32) -> Result<Self, Self::Error> {
        if loop_count == 0x100 {
            Ok(LoopCount(0))
        } else if loop_count < Self::MIN_LOOPS {
            Err(ValueError::NotEnoughLoops)
        } else {
            match u8::try_from(loop_count) {
                Ok(i) => Ok(LoopCount(i)),
                Err(_) => Err(ValueError::TooManyLoops),
            }
        }
    }
}

struct NoteOpcode {
    opcode: u8,
}

impl NoteOpcode {
    fn new(note: Note, length: &PlayNoteTicks) -> Self {
        const _: () = assert!(
            opcodes::FIRST_PLAY_NOTE_INSTRUCTION as u32 + (((LAST_NOTE_ID as u32) << 1) | 1)
                <= u8::MAX as u32,
            "Overflow test failed"
        );

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };
        let opcode = opcodes::FIRST_PLAY_NOTE_INSTRUCTION + ((note.note_id() << 1) | key_off_bit);
        Self { opcode }
    }
}

u8_value_newtype!(
    EarlyReleaseTicks,
    EarlyReleaseTicksOutOfRange,
    NoEarlyReleaseTicks,
    1,
    u8::MAX - 1
);

impl EarlyReleaseTicks {
    fn bc_argument(self) -> u8 {
        self.0 + 1
    }
}

u8_value_newtype!(
    EarlyReleaseMinTicks,
    EarlyReleaseMinTicksOutOfRange,
    NoEarlyReleaseMinTicks,
    1,
    u8::MAX
);

impl EarlyReleaseMinTicks {
    fn bc_argument(self) -> u8 {
        self.0
    }
}

#[derive(PartialEq)]
pub enum BcTerminator<'a> {
    DisableChannel,
    Goto(usize),
    ReturnFromSubroutine,
    ReturnFromSubroutineAndDisableVibrato,
    TailSubroutineCall(&'a Subroutine),
}

impl BcTerminator<'_> {
    pub fn is_return(&self) -> bool {
        match self {
            Self::DisableChannel => false,
            Self::Goto(_) => false,
            Self::ReturnFromSubroutine => true,
            Self::ReturnFromSubroutineAndDisableVibrato => true,
            Self::TailSubroutineCall(..) => true,
        }
    }
}

// Macro to automatically cast Opcode/NoteOpcode/i8 values and append it to `Bytecode::bytecode`.
macro_rules! emit_bytecode {
    ($self:expr, $opcode:expr) => {
        $self.bytecode.push($opcode);
    };

    ($self:expr, $opcode:expr $(, $param:expr)+) => {
        $self.bytecode.extend([$opcode, $(emit_bytecode::Parameter::cast($param)),*])
    }
}

macro_rules! emit_bytecode_array {
    ($self:expr, $opcode:expr, $array:expr) => {
        $self.bytecode.push($opcode);
        $self.bytecode.extend(
            $array
                .into_iter()
                .map(|i| emit_bytecode::Parameter::cast(i)),
        );
    };
}

mod emit_bytecode {
    use super::{opcodes, FirCoefficient, NoteOpcode};

    pub trait Parameter {
        fn cast(self) -> u8;
    }

    impl Parameter for u8 {
        fn cast(self) -> u8 {
            self
        }
    }

    impl Parameter for i8 {
        fn cast(self) -> u8 {
            self.to_le_bytes()[0]
        }
    }

    impl Parameter for NoteOpcode {
        fn cast(self) -> u8 {
            debug_assert!(self.opcode >= opcodes::FIRST_PLAY_NOTE_INSTRUCTION);
            self.opcode
        }
    }

    impl Parameter for FirCoefficient {
        fn cast(self) -> u8 {
            self.as_i8().to_le_bytes()[0]
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InstrumentState {
    Unset,
    Known(InstrumentId, RangeInclusive<Note>),

    // Multiple instruments are playing the note
    // Set at the start of a loop if the loop changes the instrument
    Multiple(RangeInclusive<Note>),

    Hint(InstrumentId, RangeInclusive<Note>),

    Unknown,
}

impl InstrumentState {
    /// CAUTION: Includes Maybe, Hint instruments
    pub fn instrument_id(&self) -> Option<InstrumentId> {
        match self {
            Self::Known(i, _) => Some(*i),
            Self::Hint(i, _) => Some(*i),
            Self::Multiple(_) => None,
            Self::Unset => None,
            Self::Unknown => None,
        }
    }

    pub fn is_known_and_eq(&self, o: &InstrumentId) -> bool {
        match self {
            Self::Unset => false,
            Self::Known(i, _) => o == i,
            Self::Multiple(..) => false,
            Self::Hint(..) => false,
            Self::Unknown => false,
        }
    }

    fn start_loop_analysis(
        &mut self,
        analysis: InstrumentAnalysis,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        match analysis {
            InstrumentAnalysis::Set(loop_inst_id) => {
                match instruments.get_index(loop_inst_id.as_u8().into()) {
                    Some(loop_inst) => {
                        let loop_range = note_range(loop_inst);
                        match &self {
                            Self::Known(si, s_range) | Self::Hint(si, s_range) => {
                                if *si != loop_inst_id {
                                    *self = InstrumentState::Multiple(
                                        max(*s_range.start(), *loop_range.start())
                                            ..=min(*s_range.end(), *loop_range.end()),
                                    );
                                }
                            }

                            Self::Multiple(s_range) => {
                                *self = InstrumentState::Multiple(
                                    max(*s_range.start(), *loop_range.start())
                                        ..=min(*s_range.end(), *loop_range.end()),
                                );
                            }

                            Self::Unset | Self::Unknown => {
                                *self = InstrumentState::Multiple(loop_range);
                            }
                        }
                    }
                    None => {
                        *self = InstrumentState::Unset;
                    }
                }
            }
            InstrumentAnalysis::Hint(_) => (),
        }
    }

    // The instrument is known at the end of a loop or subroutine
    fn end_loop_analysis(
        &mut self,
        analysis: InstrumentAnalysis,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        let id = analysis.instrument_id();
        match instruments.get_index(id.as_u8().into()) {
            Some(inst) => {
                let range = note_range(inst);
                match analysis {
                    InstrumentAnalysis::Set(i) => *self = InstrumentState::Known(i, range),
                    InstrumentAnalysis::Hint(i) => {
                        if *self == InstrumentState::Unset {
                            *self = InstrumentState::Hint(i, range)
                        }
                    }
                }
            }
            None => *self = InstrumentState::Unknown,
        };
    }
}

// Audio driver state
#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum IeState<T>
where
    T: Copy + PartialEq,
{
    Unset,
    Known(T),
    // u8 = stack depth
    MaybeInLoop(T, u8),
    Unknown,
}

impl<T> IeState<T>
where
    T: Copy + PartialEq,
{
    pub fn is_known_and_eq(&self, o: &T) -> bool {
        match self {
            Self::Known(v) => o == v,
            Self::MaybeInLoop(..) => false,
            Self::Unknown => false,
            Self::Unset => false,
        }
    }

    fn start_loop(&mut self, stack_depth: u8) {
        match self {
            Self::Unset => (),
            Self::Known(v) => *self = Self::MaybeInLoop(*v, stack_depth),
            Self::MaybeInLoop(..) => (),
            Self::Unknown => (),
        }
    }

    fn merge_skip_last_loop(&mut self, skip_last_loop: Self) {
        match &skip_last_loop {
            Self::Unset | Self::MaybeInLoop(..) => (),

            Self::Known(_) | Self::Unknown => *self = skip_last_loop,
        }
    }

    fn end_loop(&mut self, stack_depth: u8) {
        match self {
            Self::Unset => (),
            Self::Known(..) => (),
            Self::MaybeInLoop(v, d) => {
                if *d == stack_depth {
                    *self = Self::Known(*v)
                }
            }
            Self::Unknown => (),
        }
    }

    fn merge_subroutine(&mut self, subroutine: &Self) {
        match &subroutine {
            Self::Unset => (),
            Self::Known(..) | Self::Unknown => *self = *subroutine,
            Self::MaybeInLoop(..) => panic!("unexpected maybe state"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VibratoState {
    Unchanged,
    Unknown,
    // Disabled with an unknown value
    Disabled,
    Set(
        VibratoPitchOffsetPerTick,
        VibratoQuarterWavelengthInTicks,
        VibratoDelayTicks,
    ),
}

impl VibratoState {
    pub fn is_active(&self) -> bool {
        match self {
            Self::Unchanged => false,
            Self::Unknown => false,
            Self::Disabled => false,
            Self::Set(pitch_offset_per_tick, _, _) => pitch_offset_per_tick.as_u8() > 0,
        }
    }

    fn set_depth(&mut self, depth: VibratoPitchOffsetPerTick) {
        *self = match self {
            VibratoState::Unchanged | VibratoState::Unknown | VibratoState::Disabled => {
                if depth.as_u8() > 0 {
                    VibratoState::Unknown
                } else {
                    VibratoState::Disabled
                }
            }
            VibratoState::Set(_, qwt, delay) => VibratoState::Set(depth, *qwt, *delay),
        }
    }

    fn disable(&mut self) {
        *self = match self {
            VibratoState::Unchanged => VibratoState::Disabled,
            VibratoState::Unknown => VibratoState::Disabled,
            VibratoState::Disabled => VibratoState::Disabled,
            VibratoState::Set(_, qwt, delay) => {
                VibratoState::Set(VibratoPitchOffsetPerTick::new(0), *qwt, *delay)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SlurredNoteState {
    Unchanged,
    Unknown,
    None,
    Slurred(Note, DetuneValue, Option<PitchTableOffset>),
    SlurredPitch(PlayPitchPitch),
    SlurredNoise,
}

impl SlurredNoteState {
    fn merge_skip_last_loop(&mut self, o: &Self) {
        match o {
            SlurredNoteState::Unchanged => (),
            SlurredNoteState::Unknown => (),
            SlurredNoteState::None => *self = SlurredNoteState::None,
            SlurredNoteState::Slurred(n, d, i) => *self = SlurredNoteState::Slurred(*n, *d, *i),
            SlurredNoteState::SlurredPitch(p) => *self = SlurredNoteState::SlurredPitch(*p),
            SlurredNoteState::SlurredNoise => *self = SlurredNoteState::SlurredNoise,
        }
    }

    fn merge_subroutine(
        &mut self,
        o: &Self,
        sub_inst_tuning: Option<PitchTableOffset>,
        caller_inst_tuning: Option<PitchTableOffset>,
    ) {
        match o {
            SlurredNoteState::Unchanged => (),
            SlurredNoteState::Unknown => (),
            SlurredNoteState::None => *self = SlurredNoteState::None,
            SlurredNoteState::Slurred(n, d, i) => match i {
                Some(i) => *self = SlurredNoteState::Slurred(*n, *d, Some(*i)),
                None => {
                    // Test why it is None (subroutine might not set an instrument or the instrument could be unknown)
                    match sub_inst_tuning {
                        None => {
                            *self = SlurredNoteState::Slurred(*n, *d, caller_inst_tuning);
                        }
                        _ => *self = SlurredNoteState::None,
                    }
                }
            },
            SlurredNoteState::SlurredPitch(p) => *self = SlurredNoteState::SlurredPitch(*p),
            SlurredNoteState::SlurredNoise => *self = SlurredNoteState::SlurredNoise,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub tick_counter: TickCounter,
    pub max_stack_depth: StackDepth,
    pub tempo_changes: Vec<(TickCounter, TickClock)>,

    // State tracked by the command analyser
    pub(crate) instrument_tuning: Option<PitchTableOffset>,
    pub(crate) transpose_range: TransposeRange,
    pub(crate) instrument: InstrumentState,

    // State set to unknown on loop start
    pub(crate) envelope: IeState<Envelope>,
    pub(crate) prev_temp_gain: IeState<TempGain>,
    pub(crate) early_release:
        IeState<Option<(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain)>>,
    pub(crate) detune: IeState<DetuneValue>,
    pub(crate) vibrato: VibratoState,
    pub(crate) prev_slurred_note: SlurredNoteState,

    pub(crate) instrument_hint: Option<(InstrumentId, Option<Envelope>, InstrumentHintFreq)>,
    pub(crate) no_instrument_notes: RangeInclusive<Note>,
}

impl State {
    fn set_prev_slurred_note(&mut self, note: Note, length: PlayNoteTicks) {
        self.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,

            PlayNoteTicks::NoKeyOff(_) => {
                // ::TODO emit a warning if this instrument was used in a loop and was changed with a different source frequency::
                let o = self.instrument_tuning;

                match self.detune {
                    IeState::Unset => SlurredNoteState::Slurred(note, DetuneValue::ZERO, o),
                    IeState::Known(d) => SlurredNoteState::Slurred(note, d, o),
                    // ::TODO emit a warning if detune was changed in a loop and this value is used::
                    IeState::MaybeInLoop(d, _) => SlurredNoteState::Slurred(note, d, o),
                    IeState::Unknown => SlurredNoteState::None,
                }
            }
        };
    }

    // The analysis is processed at the start of a bytecode loop or song-loop
    fn start_loop_analysis(
        &mut self,
        analysis: &LoopAnalysis,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        if let Some(i) = analysis.instrument {
            // There can be 2 or more different instrument values at the start of the loop

            self.instrument.start_loop_analysis(i, instruments);

            // Clear instrument tuning if the loop is played with a different pitch table offset
            let o = pitch_table.pitch_table_offset(i.instrument_id());
            if self.instrument_tuning != Some(o) {
                self.instrument_tuning = None;
            }
        }

        self.transpose_range.start_loop(analysis.transpose);
    }

    // The analysis contains the known value at the end of the loop or subroutine
    fn end_loop_analysis(
        &mut self,
        analysis: &LoopAnalysis,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        if let Some(i) = analysis.instrument {
            self.instrument.end_loop_analysis(i, instruments);

            // The instrument is known at the end of the loop
            self.instrument_tuning = Some(pitch_table.pitch_table_offset(i.instrument_id()));
        }
    }

    fn song_loop(
        &mut self,
        analysis: &LoopAnalysis,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        self.start_loop_analysis(analysis, pitch_table, instruments);

        self.envelope = IeState::Unknown;
        self.prev_temp_gain = IeState::Unknown;
        self.early_release = IeState::Unknown;

        // Fixes erroneous OneNotePortamentoPreviousNoteIsNotSlurred in single pitch portamento after song-loop
        self.detune.start_loop(u8::MAX);

        self.vibrato = VibratoState::Unknown;

        // Loop might end on a note that is not slurred and matching `prev_slurred_note`.
        self.prev_slurred_note = SlurredNoteState::Unknown;
    }

    // The loop might change the instrument and evelope.
    // When the loop loops, the instrument/envelope might have changed.
    fn start_loop(
        &mut self,
        stack_depth: usize,
        analysis: &LoopAnalysis,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        self.start_loop_analysis(analysis, pitch_table, instruments);

        let stack_depth = u8::try_from(stack_depth).unwrap_or(u8::MAX);

        self.envelope.start_loop(stack_depth);
        self.prev_temp_gain.start_loop(stack_depth);
        self.early_release.start_loop(stack_depth);
        self.detune.start_loop(stack_depth);

        self.vibrato = VibratoState::Unknown;

        // Loop might end on a note that is not slurred and matching `prev_slurred_note`.
        self.prev_slurred_note = SlurredNoteState::Unknown;
    }

    fn skip_last_loop(
        &mut self,
        analysis: &SkipLastLoopAnalysis,
        start_loop_transpose_range: TransposeRange,
    ) {
        self.transpose_range
            .skip_last_loop(analysis.transpose, start_loop_transpose_range);
    }

    fn merge_skip_last_loop(&mut self, s: SkipLastLoop) {
        self.envelope.merge_skip_last_loop(s.envelope);
        self.prev_temp_gain.merge_skip_last_loop(s.prev_temp_gain);
        self.early_release.merge_skip_last_loop(s.early_release);
        self.detune.merge_skip_last_loop(s.detune);

        // Ok - vibrato state is unknown at the start of the loop
        self.vibrato = s.vibrato;

        self.prev_slurred_note
            .merge_skip_last_loop(&s.prev_slurred_note);
    }

    fn end_loop(
        &mut self,
        stack_depth: usize,
        analysis: &LoopAnalysis,
        start_loop_transpose_range: TransposeRange,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        self.end_loop_analysis(analysis, pitch_table, instruments);
        self.transpose_range
            .end_loop(analysis.transpose, start_loop_transpose_range);

        let stack_depth = u8::try_from(stack_depth).unwrap_or(u8::MAX);

        self.envelope.end_loop(stack_depth);
        self.prev_temp_gain.end_loop(stack_depth);
        self.early_release.end_loop(stack_depth);
        self.detune.end_loop(stack_depth);

        // No maybe vibrato state
        // No maybe prev_slurred_note state
    }

    fn merge_subroutine(
        &mut self,
        s: &State,
        analysis: &LoopAnalysis,
        pitch_table: &PitchTable,
        instruments: &UniqueNamesList<data::InstrumentOrSample>,
    ) {
        self.end_loop_analysis(analysis, pitch_table, instruments);
        self.transpose_range.subroutine_call(analysis.transpose);

        self.envelope.merge_subroutine(&s.envelope);
        self.prev_temp_gain.merge_subroutine(&s.prev_temp_gain);
        self.early_release.merge_subroutine(&s.early_release);
        self.detune.merge_subroutine(&s.detune);

        match &s.vibrato {
            VibratoState::Unchanged => (),
            VibratoState::Unknown => self.vibrato = VibratoState::Unknown,
            VibratoState::Disabled => self.vibrato = VibratoState::Disabled,
            v @ VibratoState::Set(..) => self.vibrato = *v,
        }

        self.prev_slurred_note.merge_subroutine(
            &s.prev_slurred_note,
            s.instrument_tuning,
            self.instrument_tuning,
        );
    }

    pub fn instrument_hint_freq(&self) -> Option<InstrumentHintFreq> {
        self.instrument_hint.map(|(_, _, f)| f)
    }
}

struct SkipLastLoop {
    tick_counter: TickCounter,
    // Location of the parameter of the `skip_last_loop` instruction inside `Bytecode::bytecode`.
    bc_parameter_position: usize,

    envelope: IeState<Envelope>,
    prev_temp_gain: IeState<TempGain>,
    early_release: IeState<Option<(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain)>>,
    detune: IeState<DetuneValue>,
    vibrato: VibratoState,
    prev_slurred_note: SlurredNoteState,
}

struct LoopState {
    start_loop_count: Option<LoopCount>,
    // Location of the `start_loop` instruction inside `Bytecode::bytecode`.
    start_loop_pos: usize,

    start_loop_transpose_range: TransposeRange,

    tick_counter_at_start_of_loop: TickCounter,
    skip_last_loop: Option<SkipLastLoop>,
}

pub struct Bytecode<'a> {
    context: BytecodeContext,
    bytecode: Vec<u8>,

    instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: &'a PitchTable,
    subroutines: &'a CompiledSubroutines,
    subroutin_name_map: &'a dyn SubroutineNameMap,

    state: State,

    loop_stack: Vec<LoopState>,

    /// The loop_stack len() (NOT depth) at the start of a `\asm` MML block
    asm_block_stack_len: usize,

    show_missing_set_instrument_error: bool,
}

impl<'a> Bytecode<'a> {
    pub fn new(
        context: BytecodeContext,
        instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        pitch_table: &'a PitchTable,
        subroutines: &'a CompiledSubroutines,
        subroutine_name_map: &'a dyn SubroutineNameMap,
        driver_transpose: TransposeStartRange,
    ) -> Bytecode<'a> {
        Self::new_append_to_vec(
            Vec::new(),
            context,
            instruments,
            pitch_table,
            subroutines,
            subroutine_name_map,
            driver_transpose,
        )
    }

    // Instead of creating a new `Vec<u8>` to hold the bytecode, write the data to the end of `vec`.
    // Takes ownership of the `Vec<u8>`.
    // The `Vec<u8>` can be taken out of `Bytecode` with `Self::bytecode()`.
    pub fn new_append_to_vec(
        vec: Vec<u8>,
        context: BytecodeContext,
        instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        pitch_table: &'a PitchTable,
        subroutines: &'a CompiledSubroutines,
        subroutine_name_map: &'a dyn SubroutineNameMap,
        driver_transpose: TransposeStartRange,
    ) -> Bytecode<'a> {
        Bytecode {
            bytecode: vec,
            instruments,
            pitch_table,
            subroutines,
            subroutin_name_map: subroutine_name_map,
            state: State {
                tick_counter: TickCounter::new(0),
                max_stack_depth: StackDepth(0),
                tempo_changes: Vec::new(),

                instrument_tuning: None,
                instrument: InstrumentState::Unset,
                transpose_range: TransposeRange::new(driver_transpose),

                envelope: IeState::Unset,
                prev_temp_gain: IeState::Unset,
                early_release: IeState::Unset,
                detune: match &context {
                    BytecodeContext::SoundEffect => IeState::Known(DetuneValue::ZERO),
                    BytecodeContext::SongChannel { .. } => IeState::Known(DetuneValue::ZERO),
                    BytecodeContext::SongSubroutine { .. } => IeState::Unset,
                    BytecodeContext::SfxSubroutine => IeState::Unset,
                    BytecodeContext::MmlPrefix => IeState::Unset,

                    #[cfg(feature = "test")]
                    BytecodeContext::UnitTestAssembly
                    | BytecodeContext::UnitTestAssemblySubroutine => IeState::Unset,
                },
                vibrato: match &context {
                    BytecodeContext::SoundEffect => VibratoState::Disabled,
                    BytecodeContext::SongChannel { .. } => VibratoState::Disabled,
                    BytecodeContext::SongSubroutine { .. } => VibratoState::Unchanged,
                    BytecodeContext::SfxSubroutine => VibratoState::Unchanged,
                    BytecodeContext::MmlPrefix => VibratoState::Disabled,

                    #[cfg(feature = "test")]
                    BytecodeContext::UnitTestAssembly
                    | BytecodeContext::UnitTestAssemblySubroutine => VibratoState::Disabled,
                },
                prev_slurred_note: SlurredNoteState::Unchanged,
                instrument_hint: None,
                no_instrument_notes: Note::MAX..=Note::MIN,
            },
            loop_stack: Vec::new(),
            asm_block_stack_len: 0,
            show_missing_set_instrument_error: match &context {
                BytecodeContext::SoundEffect => true,
                BytecodeContext::SongChannel { .. } => true,
                BytecodeContext::SongSubroutine { .. } => false,
                BytecodeContext::SfxSubroutine => false,
                BytecodeContext::MmlPrefix => false,

                #[cfg(feature = "test")]
                BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => {
                    false
                }
            },
            context,
        }
    }

    pub fn get_context(&self) -> &BytecodeContext {
        &self.context
    }

    pub fn get_state(&self) -> &State {
        &self.state
    }

    pub fn get_instrument(&self) -> Option<&InstrumentOrSample> {
        self.state
            .instrument
            .instrument_id()
            .and_then(|i| self.instruments.get_index(i.as_u8().into()))
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.state.tick_counter
    }

    pub fn get_tick_counter_with_loop_flag(&self) -> TickCounterWithLoopFlag {
        TickCounterWithLoopFlag {
            ticks: self.state.tick_counter,
            in_loop: self.is_in_loop(),
        }
    }

    pub fn is_driver_transpose_active(&self) -> bool {
        self.state.transpose_range.is_active()
    }

    pub fn is_in_loop(&self) -> bool {
        !self.loop_stack.is_empty()
    }

    pub fn can_loop(&self) -> bool {
        self.loop_stack.len() < BC_CHANNEL_STACK_SIZE / BC_STACK_BYTES_PER_LOOP
    }

    pub fn get_stack_depth(&self) -> StackDepth {
        StackDepth(
            (self.loop_stack.len() * BC_STACK_BYTES_PER_LOOP)
                .try_into()
                .unwrap_or(u32::MAX),
        )
    }

    pub fn get_bytecode_len(&self) -> usize {
        self.bytecode.len()
    }

    /// CAUTION: Does not emit bytecode
    pub fn _song_loop_point(&mut self, analysis: &LoopAnalysis) {
        assert!(matches!(self.context, BytecodeContext::SongChannel { .. }));
        assert_eq!(self.get_stack_depth(), StackDepth(0));

        self.state
            .song_loop(analysis, self.pitch_table, self.instruments);
    }

    pub fn _start_asm_block(&mut self) {
        debug_assert_eq!(self.asm_block_stack_len, 0);

        self.asm_block_stack_len = self.loop_stack.len();
    }

    pub fn _end_asm_block(&mut self) -> Result<(), BytecodeError> {
        let expected_stack_len = self.asm_block_stack_len;

        self.asm_block_stack_len = 0;

        if self.loop_stack.len() <= expected_stack_len {
            Ok(())
        } else {
            Err(BytecodeError::MissingEndLoopInAsmBlock)
        }
    }

    pub fn bytecode(
        mut self,
        terminator: BcTerminator,
    ) -> Result<(Vec<u8>, State), (BytecodeError, Vec<u8>)> {
        if !self.loop_stack.is_empty() {
            return Err((
                BytecodeError::OpenLoopStack(self.loop_stack.len()),
                self.bytecode,
            ));
        }

        if terminator.is_return() && !self.context.is_subroutine() {
            return Err((BytecodeError::ReturnInNonSubroutine, self.bytecode));
        }

        if let BcTerminator::TailSubroutineCall(s) = &terminator {
            if let Err(e) = self._update_subtroutine_state_excluding_stack_depth(s) {
                return Err((e, self.bytecode));
            }
        }

        // ::TODO test song loop notes at the end of the channel::

        match terminator {
            BcTerminator::DisableChannel => {
                emit_bytecode!(self, opcodes::DISABLE_CHANNEL);
            }
            BcTerminator::ReturnFromSubroutine => {
                emit_bytecode!(self, opcodes::RETURN_FROM_SUBROUTINE);
            }
            BcTerminator::ReturnFromSubroutineAndDisableVibrato => {
                self.state.vibrato.disable();

                emit_bytecode!(self, opcodes::RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO);
            }
            BcTerminator::Goto(pos) => {
                match (u16::try_from(self.bytecode.len()), u16::try_from(pos)) {
                    (Ok(inst_ptr), Ok(pos)) => {
                        let o = pos.wrapping_sub(inst_ptr).wrapping_sub(2).to_le_bytes();
                        emit_bytecode!(self, opcodes::GOTO_RELATIVE, o[0], o[1]);
                    }
                    _ => {
                        emit_bytecode!(self, opcodes::DISABLE_CHANNEL);
                        return Err((BytecodeError::GotoRelativeOutOfBounds, self.bytecode));
                    }
                }
            }
            BcTerminator::TailSubroutineCall(s) => match u16::try_from(self.bytecode.len()) {
                Ok(inst_ptr) => {
                    let pos = s.bytecode_offset;
                    let o = pos.wrapping_sub(inst_ptr).wrapping_sub(2).to_le_bytes();
                    emit_bytecode!(self, opcodes::GOTO_RELATIVE, o[0], o[1]);
                }
                Err(_) => {
                    emit_bytecode!(self, opcodes::DISABLE_CHANNEL);
                    return Err((BytecodeError::GotoRelativeOutOfBounds, self.bytecode));
                }
            },
        }

        Ok((self.bytecode, self.state))
    }

    fn _test_note_in_range(&mut self, note: Note) -> Result<(), BytecodeError> {
        // Disable note range checks for bytecode assembly in the MML tests
        #[cfg(feature = "test")]
        if self.context.is_unit_test_assembly() {
            return Ok(());
        }

        // ::TODO process transpose range::

        match self.state.instrument {
            InstrumentState::Hint(..) | InstrumentState::Unknown | InstrumentState::Unset => {
                let s = &mut self.state;
                if s.no_instrument_notes.is_empty() {
                    s.no_instrument_notes = note..=note;
                } else if note < *s.no_instrument_notes.start() {
                    s.no_instrument_notes = note..=*(s.no_instrument_notes.end());
                } else if note > *s.no_instrument_notes.end() {
                    s.no_instrument_notes = *(s.no_instrument_notes.start())..=note;
                }
            }
            InstrumentState::Known(..) | InstrumentState::Multiple(..) => (),
        }

        match &self.state.instrument {
            InstrumentState::Known(_, note_range)
            | InstrumentState::Multiple(note_range)
            | InstrumentState::Hint(_, note_range) => {
                if note_range.contains(&note) {
                    Ok(())
                } else if !note_range.is_empty() {
                    Err(BytecodeError::NoteOutOfRange(note, note_range.clone()))
                } else {
                    Err(BytecodeError::NoteOutOfRangeEmptyNoteRange(note))
                }
            }
            InstrumentState::Unknown | InstrumentState::Unset => {
                if self.show_missing_set_instrument_error {
                    self.show_missing_set_instrument_error = false;
                    Err(BytecodeError::CannotPlayNoteBeforeSettingInstrument)
                } else {
                    Ok(())
                }
            }
        }
    }

    pub fn wait(&mut self, length: BcTicksNoKeyOff) {
        self.state.tick_counter += length.to_tick_count();

        emit_bytecode!(self, opcodes::WAIT, length.bc_argument);
    }

    pub fn rest(&mut self, length: BcTicksKeyOff) {
        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = SlurredNoteState::None;

        emit_bytecode!(self, opcodes::REST, length.bc_argument);
    }

    pub fn play_pitch(&mut self, pitch: PlayPitchPitch, length: PlayNoteTicks) {
        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::SlurredPitch(pitch),
        };

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };

        let pitch = pitch.as_u16().to_le_bytes();
        let arg1 = pitch[0];
        let arg2 = (pitch[1] << 1) | key_off_bit;

        emit_bytecode!(self, opcodes::PLAY_PITCH, arg1, arg2, length.bc_argument());
    }

    pub fn play_noise(&mut self, frequency: NoiseFrequency, length: PlayNoteTicks) {
        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::SlurredNoise,
        };

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };

        let arg = (frequency.as_u8() << 1) | key_off_bit;

        emit_bytecode!(self, opcodes::PLAY_NOISE, arg, length.bc_argument());
    }

    pub fn disable_noise(&mut self) {
        emit_bytecode!(
            self,
            opcodes::MISCELLANEOUS,
            MiscInstruction::DisableNoise.as_u8()
        )
    }

    pub fn play_note(&mut self, note: Note, length: PlayNoteTicks) -> Result<(), BytecodeError> {
        let r = self._test_note_in_range(note);

        self.state.tick_counter += length.to_tick_count();
        self.state.set_prev_slurred_note(note, length);

        let opcode = NoteOpcode::new(note, &length);

        emit_bytecode!(self, opcode.opcode, length.bc_argument());

        r
    }

    pub fn portamento(
        &mut self,
        note: Note,
        velocity: PortamentoVelocity,
        length: PlayNoteTicks,
    ) -> Result<(), BytecodeError> {
        let r = self._test_note_in_range(note);

        self.state.tick_counter += length.to_tick_count();
        self.state.set_prev_slurred_note(note, length);

        let note_param = NoteOpcode::new(note, &length);
        let speed = velocity.pitch_offset_per_tick();
        let length = length.bc_argument();

        if velocity.is_negative() {
            emit_bytecode!(self, opcodes::PORTAMENTO_DOWN, note_param, speed, length);
        } else {
            emit_bytecode!(self, opcodes::PORTAMENTO_UP, note_param, speed, length);
        }

        r
    }

    pub fn portamento_calc(
        &mut self,
        note: Note,
        slide_ticks: PortamentoSlideTicks,
        length: PlayNoteTicks,
    ) -> Result<(), BytecodeError> {
        let r = self._test_note_in_range(note);

        self.state.tick_counter += length.to_tick_count();
        self.state.set_prev_slurred_note(note, length);

        let note_param = NoteOpcode::new(note, &length);
        let length = length.bc_argument();

        emit_bytecode!(
            self,
            opcodes::PORTAMENTO_CALC,
            note_param,
            length,
            slide_ticks.as_u8()
        );

        r
    }

    pub fn portamento_pitch(
        &mut self,
        target: PlayPitchPitch,
        velocity: PortamentoVelocity,
        length: PlayNoteTicks,
    ) {
        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::SlurredPitch(target),
        };

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };

        let opcode = if velocity.is_negative() {
            opcodes::PORTAMENTO_PITCH_DOWN
        } else {
            opcodes::PORTAMENTO_PITCH_UP
        };

        let target = target.as_u16().to_le_bytes();
        let arg1 = target[0];
        let arg2 = (target[1] << 1) | key_off_bit;

        let speed = velocity.pitch_offset_per_tick();
        let length = length.bc_argument();

        emit_bytecode!(self, opcode, arg1, arg2, speed, length);
    }

    pub fn portamento_pitch_calc(
        &mut self,
        target: PlayPitchPitch,
        slide_ticks: PortamentoSlideTicks,
        length: PlayNoteTicks,
    ) {
        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::SlurredPitch(target),
        };

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };

        let target = target.as_u16().to_le_bytes();
        let arg1 = target[0];
        let arg2 = (target[1] << 1) | key_off_bit;

        let length = length.bc_argument();

        emit_bytecode!(
            self,
            opcodes::PORTAMENTO_PITCH_CALC,
            arg1,
            arg2,
            length,
            slide_ticks.as_u8()
        );
    }

    pub fn set_vibrato_depth_and_play_note(
        &mut self,
        pitch_offset_per_tick: VibratoPitchOffsetPerTick,
        note: Note,
        length: PlayNoteTicks,
    ) -> Result<(), BytecodeError> {
        let r = self._test_note_in_range(note);

        self.state.tick_counter += length.to_tick_count();
        self.state.vibrato.set_depth(pitch_offset_per_tick);

        let play_note_opcode = NoteOpcode::new(note, &length);

        emit_bytecode!(
            self,
            opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE,
            pitch_offset_per_tick.as_u8(),
            play_note_opcode,
            length.bc_argument()
        );

        r
    }

    pub fn set_vibrato(
        &mut self,
        pitch_offset_per_tick: VibratoPitchOffsetPerTick,
        quarter_wavelength_ticks: VibratoQuarterWavelengthInTicks,
    ) {
        self.state.vibrato = VibratoState::Set(
            pitch_offset_per_tick,
            quarter_wavelength_ticks,
            VibratoDelayTicks::new(0),
        );

        emit_bytecode!(
            self,
            opcodes::SET_VIBRATO,
            pitch_offset_per_tick.as_u8(),
            quarter_wavelength_ticks.as_u8()
        );
    }

    pub fn set_vibrato_with_delay(
        &mut self,
        pitch_offset_per_tick: VibratoPitchOffsetPerTick,
        quarter_wavelength_ticks: VibratoQuarterWavelengthInTicks,
        delay: VibratoDelayTicks,
    ) {
        self.state.vibrato =
            VibratoState::Set(pitch_offset_per_tick, quarter_wavelength_ticks, delay);

        match delay.as_u8() {
            0 => emit_bytecode!(
                self,
                opcodes::SET_VIBRATO,
                pitch_offset_per_tick.as_u8(),
                quarter_wavelength_ticks.as_u8()
            ),
            delay => emit_bytecode!(
                self,
                opcodes::SET_VIBRATO_WITH_DELAY,
                pitch_offset_per_tick.as_u8(),
                delay,
                quarter_wavelength_ticks.as_u8()
            ),
        }
    }

    pub fn disable_vibrato(&mut self) {
        self.state.vibrato.disable();

        emit_bytecode!(self, opcodes::SET_VIBRATO, 0u8, 0u8);
    }

    // Not a bytecode instruction
    pub(crate) fn set_subroutine_instrument_hint(
        &mut self,
        id: InstrumentId,
        envelope: Option<Envelope>,
    ) -> Result<(), ChannelError> {
        if self.state.instrument_hint.is_some() {
            return Err(ChannelError::InstrumentHintAlreadySet);
        }

        if !self.context.is_subroutine() {
            return Err(ChannelError::InstrumentHintOnlyAllowedInSubroutines);
        }

        match &self.state.instrument {
            InstrumentState::Unset => (),

            InstrumentState::Unknown | InstrumentState::Hint(..) => (),

            InstrumentState::Known(..) | InstrumentState::Multiple(..) => {
                return Err(ChannelError::InstrumentHintInstrumentAlreadySet)
            }
        }

        match self.instruments.get_index(id.as_u8().into()) {
            Some(InstrumentOrSample::Instrument(i)) => {
                self.state.instrument = InstrumentState::Hint(id, instrument_note_range(i));
                self.state.instrument_tuning = Some(self.pitch_table.pitch_table_offset(id));
                self.state.instrument_hint =
                    Some((id, envelope, InstrumentHintFreq::from_instrument(i)));
                Ok(())
            }
            Some(InstrumentOrSample::Sample(_)) => {
                Err(ChannelError::CannotSetInstrumentHintForSample)
            }
            None => Err(ChannelError::CannotSetInstrumentHintForUnknown),
        }
    }

    fn _set_state_instrument_and_note_range(&mut self, instrument: InstrumentId) {
        self.state.instrument = match self.instruments.get_index(instrument.as_u8().into()) {
            Some(i) => InstrumentState::Known(instrument, note_range(i)),
            None => InstrumentState::Unknown,
        };

        self.state.instrument_tuning = Some(self.pitch_table.pitch_table_offset(instrument));
    }

    pub fn set_instrument(&mut self, instrument: InstrumentId) {
        self._set_state_instrument_and_note_range(instrument);

        self.state.envelope = match self.instruments.get_index(instrument.as_u8().into()) {
            Some(i) => IeState::Known(i.envelope()),
            None => IeState::Unknown,
        };

        emit_bytecode!(self, opcodes::SET_INSTRUMENT, instrument.as_u8());
    }

    pub fn set_instrument_and_adsr(&mut self, instrument: InstrumentId, adsr: Adsr) {
        self._set_state_instrument_and_note_range(instrument);

        self.state.envelope = IeState::Known(Envelope::Adsr(adsr));

        emit_bytecode!(
            self,
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN,
            instrument.as_u8(),
            adsr.adsr1(),
            adsr.adsr2()
        );
    }

    pub fn set_instrument_and_gain(&mut self, instrument: InstrumentId, gain: Gain) {
        self._set_state_instrument_and_note_range(instrument);

        self.state.envelope = IeState::Known(Envelope::Gain(gain));

        emit_bytecode!(
            self,
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN,
            instrument.as_u8(),
            0u8,
            gain.value()
        );
    }

    pub fn set_adsr(&mut self, adsr: Adsr) {
        self.state.envelope = IeState::Known(Envelope::Adsr(adsr));

        emit_bytecode!(self, opcodes::SET_ADSR, adsr.adsr1(), adsr.adsr2());
    }

    pub fn set_gain(&mut self, gain: Gain) {
        self.state.envelope = IeState::Known(Envelope::Gain(gain));

        emit_bytecode!(self, opcodes::SET_GAIN, gain.value());
    }

    pub fn set_temp_gain(&mut self, gain: TempGain) {
        self.state.prev_temp_gain = IeState::Known(gain);

        emit_bytecode!(self, opcodes::SET_TEMP_GAIN, gain.as_u8());
    }

    pub fn set_temp_gain_and_wait(&mut self, gain: TempGain, length: BcTicksNoKeyOff) {
        self.state.prev_temp_gain = IeState::Known(gain);
        self.state.tick_counter += length.to_tick_count();

        emit_bytecode!(
            self,
            opcodes::SET_TEMP_GAIN_AND_WAIT,
            gain.as_u8(),
            length.bc_argument
        );
    }

    pub fn set_temp_gain_and_rest(&mut self, gain: TempGain, length: BcTicksKeyOff) {
        self.state.prev_temp_gain = IeState::Known(gain);
        self.state.tick_counter += length.to_tick_count();

        emit_bytecode!(
            self,
            opcodes::SET_TEMP_GAIN_AND_REST,
            gain.as_u8(),
            length.bc_argument
        );
    }

    pub fn reuse_temp_gain(&mut self) {
        emit_bytecode!(self, opcodes::REUSE_TEMP_GAIN);
    }

    pub fn reuse_temp_gain_and_wait(&mut self, length: BcTicksNoKeyOff) {
        self.state.tick_counter += length.to_tick_count();

        emit_bytecode!(self, opcodes::REUSE_TEMP_GAIN_AND_WAIT, length.bc_argument);
    }

    pub fn reuse_temp_gain_and_rest(&mut self, length: BcTicksKeyOff) {
        self.state.tick_counter += length.to_tick_count();

        emit_bytecode!(self, opcodes::REUSE_TEMP_GAIN_AND_REST, length.bc_argument);
    }

    pub fn disable_early_release(&mut self) {
        self.state.early_release = IeState::Known(None);

        emit_bytecode!(self, opcodes::SET_EARLY_RELEASE_NO_MINIMUM, 0u8, 0u8);
    }

    pub fn set_early_release_no_minimum(&mut self, ticks: EarlyReleaseTicks, gain: OptionalGain) {
        let min = EarlyReleaseMinTicks::MIN;
        self.state.early_release = IeState::Known(Some((ticks, min, gain)));

        emit_bytecode!(
            self,
            opcodes::SET_EARLY_RELEASE_NO_MINIMUM,
            ticks.bc_argument(),
            gain.as_u8()
        );
    }

    pub fn set_early_release(
        &mut self,
        ticks: EarlyReleaseTicks,
        min: EarlyReleaseMinTicks,
        gain: OptionalGain,
    ) {
        if min == EarlyReleaseMinTicks::MIN {
            self.set_early_release_no_minimum(ticks, gain);
        } else {
            self.state.early_release = IeState::Known(Some((ticks, min, gain)));

            emit_bytecode!(
                self,
                opcodes::SET_EARLY_RELEASE,
                ticks.bc_argument(),
                min.bc_argument(),
                gain.as_u8()
            );
        }
    }

    pub fn set_transpose(&mut self, transpose: Transpose) {
        self.state.transpose_range.set(transpose);

        emit_bytecode!(self, opcodes::SET_TRANSPOSE, transpose.as_i8());
    }

    pub fn adjust_transpose(&mut self, adjust: RelativeTranspose) {
        self.state.transpose_range.adjust(adjust);

        if adjust.as_i8() != 0 {
            emit_bytecode!(self, opcodes::ADJUST_TRANSPOSE, adjust.as_i8());
        }
    }

    pub fn disable_transpose(&mut self) {
        emit_bytecode!(self, opcodes::SET_TRANSPOSE, 0i8);
    }

    pub fn set_detune(&mut self, v: DetuneValue) {
        self.state.detune = IeState::Known(v);

        let (arg1, arg2) = v.as_i16().to_le_bytes().into();

        match arg2 {
            0x00 => emit_bytecode!(self, opcodes::SET_DETUNE_P8, arg1),
            0xff => emit_bytecode!(self, opcodes::SET_DETUNE_N8, arg1),
            _ => emit_bytecode!(self, opcodes::SET_DETUNE_I16, arg1, arg2),
        }
    }

    pub fn disable_detune(&mut self) {
        self.set_detune(DetuneValue::ZERO);
    }

    pub fn adjust_volume(&mut self, v: RelativeVolume) {
        emit_bytecode!(self, opcodes::ADJUST_VOLUME, v.as_i8());
    }

    pub fn set_volume(&mut self, volume: Volume) {
        emit_bytecode!(self, opcodes::SET_VOLUME, volume.as_u8());
    }

    pub fn adjust_pan(&mut self, p: RelativePan) {
        emit_bytecode!(self, opcodes::ADJUST_PAN, p.as_i8());
    }

    pub fn set_pan(&mut self, pan: Pan) {
        emit_bytecode!(self, opcodes::SET_PAN, pan.as_u8());
    }

    pub fn set_pan_and_volume(&mut self, pan: Pan, volume: Volume) {
        emit_bytecode!(
            self,
            opcodes::SET_PAN_AND_VOLUME,
            pan.as_u8(),
            volume.as_u8()
        );
    }

    pub fn set_channel_invert(&mut self, flags: InvertFlags) {
        emit_bytecode!(
            self,
            opcodes::SET_CHANNEL_OR_ECHO_INVERT,
            flags.into_bytecode_value(true)
        )
    }

    fn _pan_vol_slide_offset_per_tick<A, T>(&mut self, amount: A, ticks: T) -> (u8, u8)
    where
        A: SignedValueNewType<ValueType = i16>,
        T: UnsignedValueNewType<ValueType = u32>,
    {
        let u16_ticks = u16::try_from(ticks.value()).unwrap();

        let offset_per_tick = ((amount.value().unsigned_abs() << 8) | 0xff) / u16_ticks;

        // slide up
        debug_assert_eq!(
            offset_per_tick.wrapping_mul(u16_ticks).to_le_bytes()[1],
            u8::try_from(amount.value().unsigned_abs()).unwrap()
        );

        // slide down
        debug_assert_eq!(
            u16::MAX
                .wrapping_sub(offset_per_tick.wrapping_mul(u16_ticks))
                .to_le_bytes()[1],
            u8::MAX - u8::try_from(amount.value().unsigned_abs()).unwrap()
        );

        offset_per_tick.to_le_bytes().into()
    }

    pub fn volume_slide(&mut self, amount: VolumeSlideAmount, ticks: VolumeSlideTicks) {
        let opt = self._pan_vol_slide_offset_per_tick(amount, ticks);

        let opcode = match amount.is_negative() {
            false => opcodes::VOLUME_SLIDE_UP,
            true => opcodes::VOLUME_SLIDE_DOWN,
        };

        emit_bytecode!(self, opcode, ticks.driver_value(), opt.0, opt.1);
    }

    pub fn pan_slide(&mut self, amount: PanSlideAmount, ticks: PanSlideTicks) {
        let opt = self._pan_vol_slide_offset_per_tick(amount, ticks);

        let opcode = match amount.is_negative() {
            false => opcodes::PAN_SLIDE_UP,
            true => opcodes::PAN_SLIDE_DOWN,
        };

        emit_bytecode!(self, opcode, ticks.driver_value(), opt.0, opt.1);
    }

    fn _tremolo_panbrello<A, T>(&mut self, opcode: u8, amplitude: A, quarter_wavelength_ticks: T)
    where
        A: UnsignedValueNewType<ValueType = u8>,
        T: UnsignedValueNewType<ValueType = u8>,
    {
        const SUB_VALUE: u16 = 0x7f;

        let u16_ticks: u16 = quarter_wavelength_ticks.value().into();

        let offset_per_tick = ((u16::from(amplitude.value()) << 8) | SUB_VALUE) / u16_ticks;
        let arg_2 = offset_per_tick.to_le_bytes()[0];
        let arg_3 = offset_per_tick.to_le_bytes()[1];

        debug_assert_eq!(
            u16::from_le_bytes([SUB_VALUE as u8, 127])
                .wrapping_add(offset_per_tick.wrapping_mul(u16_ticks))
                .to_le_bytes()[1],
            127 + amplitude.value()
        );
        debug_assert_eq!(
            u16::from_le_bytes([SUB_VALUE as u8, 127])
                .wrapping_add(offset_per_tick.wrapping_mul(u16_ticks))
                .wrapping_sub(offset_per_tick.wrapping_mul(u16_ticks * 2))
                .to_le_bytes()[1],
            127 - amplitude.value()
        );

        emit_bytecode!(self, opcode, quarter_wavelength_ticks.value(), arg_2, arg_3);
    }

    pub fn tremolo(
        &mut self,
        amplitude: TremoloAmplitude,
        quarter_wavelength_ticks: TremoloQuarterWavelengthInTicks,
    ) {
        self._tremolo_panbrello(opcodes::TREMOLO, amplitude, quarter_wavelength_ticks);
    }

    pub fn panbrello(
        &mut self,
        amplitude: PanbrelloAmplitude,
        quarter_wavelength_ticks: PanbrelloQuarterWavelengthInTicks,
    ) {
        self._tremolo_panbrello(opcodes::PANBRELLO, amplitude, quarter_wavelength_ticks);
    }

    fn _pmod_instruction(
        &mut self,
        prefixed_instruction: MiscInstruction,
    ) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongChannel { index, .. } => match u8::from(index) {
                0 => Err(BytecodeError::PmodNotAllowedInChannelA),
                1..=5 => {
                    emit_bytecode!(self, opcodes::MISCELLANEOUS, prefixed_instruction.as_u8());
                    Ok(())
                }
                6 | 7 => Err(BytecodeError::PmodNotAllowedInChannelsGH),
                8.. => {
                    panic!("Invalid song channel");
                }
            },
            BytecodeContext::SongSubroutine { .. } | BytecodeContext::MmlPrefix => {
                emit_bytecode!(self, opcodes::MISCELLANEOUS, prefixed_instruction.as_u8());
                Ok(())
            }
            BytecodeContext::SfxSubroutine | BytecodeContext::SoundEffect => {
                Err(BytecodeError::PmodNotAllowedInSoundEffect)
            }

            #[cfg(feature = "test")]
            BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => {
                // Always emit the instruction in a unit test bytecode
                emit_bytecode!(self, opcodes::MISCELLANEOUS, prefixed_instruction.as_u8());
                Ok(())
            }
        }
    }

    pub fn enable_pmod(&mut self) -> Result<(), BytecodeError> {
        self._pmod_instruction(MiscInstruction::EnablePmod)
    }

    pub fn disable_pmod(&mut self) -> Result<(), BytecodeError> {
        self._pmod_instruction(MiscInstruction::DisablePmod)
    }

    pub fn enable_echo(&mut self) {
        emit_bytecode!(self, opcodes::ENABLE_ECHO);
    }

    pub fn disable_echo(&mut self) {
        emit_bytecode!(self, opcodes::DISABLE_ECHO);
    }

    pub fn start_loop(
        &mut self,
        loop_count: Option<LoopCount>,
        analysis: &LoopAnalysis,
    ) -> Result<(), BytecodeError> {
        self.loop_stack.push(LoopState {
            start_loop_count: loop_count,
            start_loop_pos: self.bytecode.len(),
            start_loop_transpose_range: self.state.transpose_range,
            tick_counter_at_start_of_loop: self.state.tick_counter,
            skip_last_loop: None,
        });

        self.state.start_loop(
            self.loop_stack.len() - 1,
            analysis,
            self.pitch_table,
            self.instruments,
        );

        let loop_count = match loop_count {
            Some(lc) => lc.0,
            None => 0,
        };

        emit_bytecode!(self, opcodes::START_LOOP, loop_count);

        let stack_depth = self.get_stack_depth();
        self.state.max_stack_depth = max(self.state.max_stack_depth, stack_depth);

        if stack_depth <= MAX_STACK_DEPTH {
            Ok(())
        } else {
            Err(BytecodeError::StackOverflowInStartLoop(stack_depth.0))
        }
    }

    const START_LOOP_INST_SIZE: usize = 2;
    const SKIP_LAST_LOOP_INST_SIZE: usize = 2;

    pub fn skip_last_loop(&mut self, analysis: &SkipLastLoopAnalysis) -> Result<(), BytecodeError> {
        let loop_state = match self.loop_stack.last_mut() {
            Some(l) => l,
            None => return Err(BytecodeError::NotInALoop),
        };

        if loop_state.skip_last_loop.is_some() {
            return Err(BytecodeError::MultipleSkipLastLoopInstructions);
        }

        if loop_state.start_loop_pos + Self::START_LOOP_INST_SIZE == self.bytecode.len() {
            return Err(BytecodeError::NoInstructionsBeforeSkipLastLoop);
        }

        loop_state.skip_last_loop = Some(SkipLastLoop {
            tick_counter: self.state.tick_counter,
            bc_parameter_position: self.bytecode.len() + 1,
            envelope: self.state.envelope,
            prev_temp_gain: self.state.prev_temp_gain,
            early_release: self.state.early_release,
            detune: self.state.detune,
            vibrato: self.state.vibrato,
            prev_slurred_note: self.state.prev_slurred_note.clone(),
        });

        self.state
            .skip_last_loop(analysis, loop_state.start_loop_transpose_range);

        emit_bytecode!(self, opcodes::SKIP_LAST_LOOP_U8, 0u8);

        if self.loop_stack.len() > self.asm_block_stack_len {
            Ok(())
        } else {
            Err(BytecodeError::CannotModifyLoopOutsideAsmBlock)
        }
    }

    pub fn end_loop(
        &mut self,
        loop_count: Option<LoopCount>,
        analysis: &LoopAnalysis,
    ) -> Result<(), BytecodeError> {
        let start_loop_transpose_range = self
            .loop_stack
            .last()
            .map(|ls| ls.start_loop_transpose_range)
            .unwrap_or(TransposeRange::DISABLED);

        let r = self._end_loop(loop_count);

        // Must always call `state.end_loop()` (especially if _end_loop exits early with an error)
        // to fix a "song-loop in loop" panic
        self.state.end_loop(
            self.loop_stack.len(),
            analysis,
            start_loop_transpose_range,
            self.pitch_table,
            self.instruments,
        );

        r
    }

    fn _end_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
        let loop_state = match self.loop_stack.pop() {
            Some(l) => l,
            None => return Err(BytecodeError::NotInALoop),
        };

        let loop_count_u32 = match (loop_state.start_loop_count, &loop_count) {
            (Some(start_lc), None) => start_lc.to_u32(),
            (None, Some(end_lc)) => end_lc.to_u32(),

            (None, None) => return Err(BytecodeError::MissingLoopCount),
            (Some(_), Some(_)) => return Err(BytecodeError::CannotHaveLoopCountAtStartAndEndLoop),
        };

        // Increment tick counter
        {
            let ticks_in_loop =
                self.state.tick_counter.value() - loop_state.tick_counter_at_start_of_loop.value();
            if ticks_in_loop == 0 {
                return Err(BytecodeError::NoTicksInLoop);
            }

            let ticks_skipped_in_last_loop = match &loop_state.skip_last_loop {
                Some(s) => {
                    if s.bc_parameter_position - 1 + Self::SKIP_LAST_LOOP_INST_SIZE
                        == self.bytecode.len()
                    {
                        return Err(BytecodeError::NoInstructionsAfterSkipLastLoop);
                    }
                    self.state.tick_counter.value() - s.tick_counter.value()
                }
                None => 0,
            };

            assert!(ticks_skipped_in_last_loop <= ticks_in_loop);

            let ticks_to_add = ticks_in_loop
                .checked_mul(loop_count_u32 - 1)
                .and_then(|t| t.checked_sub(ticks_skipped_in_last_loop))
                .unwrap_or(u32::MAX);

            self.state.tick_counter += TickCounter::new(ticks_to_add);
        }

        // Write the loop_count parameter for the start_loop instruction (if required)
        if let Some(loop_count) = loop_count {
            assert_eq!(
                self.bytecode[loop_state.start_loop_pos],
                opcodes::START_LOOP
            );

            self.bytecode[loop_state.start_loop_pos + 1] = loop_count.0;
        }

        if let Some(skip_last_loop) = loop_state.skip_last_loop {
            // Write the parameter for the skip_last_loop instruction (if required)
            let sll_param_pos = skip_last_loop.bc_parameter_position;

            self.state.merge_skip_last_loop(skip_last_loop);

            assert_eq!(self.bytecode[sll_param_pos - 1], opcodes::SKIP_LAST_LOOP_U8);

            match self.bytecode.len() - sll_param_pos {
                0 => {
                    return Err(BytecodeError::InvalidSkipLastLoopParameter(0));
                }
                to_skip @ 1..=0xff => {
                    self.bytecode[sll_param_pos] = u8::try_from(to_skip).unwrap();
                }
                to_skip @ 0x100..=0xffff => {
                    let to_skip = u16::try_from(to_skip).unwrap();
                    let (l, h) = to_skip.to_le_bytes().into();

                    self.bytecode[sll_param_pos - 1] = opcodes::SKIP_LAST_LOOP_U16BE;
                    self.bytecode.insert(sll_param_pos, h);
                    self.bytecode[sll_param_pos + 1] = l;

                    debug_assert!(self.loop_stack.iter().all(|c| match &c.skip_last_loop {
                        Some(s) => s.bc_parameter_position < sll_param_pos,
                        None => true,
                    }));
                }
                err @ 0x1000.. => return Err(BytecodeError::InvalidSkipLastLoopParameter(err)),
            }
        }

        emit_bytecode!(self, opcodes::END_LOOP);

        if self.loop_stack.len() >= self.asm_block_stack_len {
            Ok(())
        } else {
            Err(BytecodeError::MissingStartLoopInAsmBlock)
        }
    }

    fn _update_subtroutine_state_excluding_stack_depth(
        &mut self,
        subroutine: &Subroutine,
    ) -> Result<(), BytecodeError> {
        let s = &subroutine.bc_state;

        self.state.tick_counter += s.tick_counter;

        let old_instrument = self.state.instrument.clone();

        self.state
            .merge_subroutine(s, &subroutine.analysis, self.pitch_table, self.instruments);

        // Disable note range checks for bytecode assembly in the MML tests
        #[cfg(feature = "test")]
        if self.context.is_unit_test_assembly() {
            return Ok(());
        }

        if let Some(sub_hint_freq) = s.instrument_hint_freq() {
            match old_instrument {
                InstrumentState::Known(id, _) | InstrumentState::Hint(id, _) => {
                    match self.instruments.get_index(id.as_u8().into()) {
                        Some(InstrumentOrSample::Instrument(i)) => {
                            let inst_freq = InstrumentHintFreq::from_instrument(i);
                            if sub_hint_freq != inst_freq {
                                return Err(
                                    BytecodeError::SubroutineInstrumentHintFrequencyMismatch {
                                        subroutine: sub_hint_freq,
                                        instrument: inst_freq,
                                    },
                                );
                            }
                        }
                        Some(InstrumentOrSample::Sample(_)) => {
                            return Err(BytecodeError::SubroutineInstrumentHintSampleMismatch);
                        }
                        None => return Err(BytecodeError::SubroutineInstrumentHintNoInstrumentSet),
                    }
                }

                InstrumentState::Multiple(..)
                | InstrumentState::Unknown
                | InstrumentState::Unset => match self.context {
                    BytecodeContext::SongSubroutine { .. } | BytecodeContext::SfxSubroutine => {
                        self.state.instrument_hint = s.instrument_hint;
                    }

                    BytecodeContext::SongChannel { .. }
                    | BytecodeContext::SoundEffect
                    | BytecodeContext::MmlPrefix => {
                        return Err(BytecodeError::SubroutineInstrumentHintNoInstrumentSet)
                    }

                    #[cfg(feature = "test")]
                    BytecodeContext::UnitTestAssembly
                    | BytecodeContext::UnitTestAssemblySubroutine => (),
                },
            }
        }

        if !s.no_instrument_notes.is_empty() {
            // Subroutine plays a note without an instrument

            let self_notes = &self.state.no_instrument_notes;
            let sub_notes = &s.no_instrument_notes;

            match old_instrument {
                InstrumentState::Unknown | InstrumentState::Unset => match self.context {
                    BytecodeContext::SongChannel { .. } | BytecodeContext::SoundEffect => {
                        return Err(BytecodeError::SubroutinePlaysNotesWithNoInstrument)
                    }
                    BytecodeContext::SongSubroutine { .. } | BytecodeContext::SfxSubroutine => {
                        self.state.no_instrument_notes = if self_notes.is_empty() {
                            sub_notes.clone()
                        } else {
                            min(*sub_notes.start(), *self_notes.start())
                                ..=max(*sub_notes.end(), *self_notes.end())
                        };
                    }
                    BytecodeContext::MmlPrefix => {
                        return Err(BytecodeError::SubroutineCallInMmlPrefix)
                    }

                    // Do not show error
                    #[cfg(feature = "test")]
                    BytecodeContext::UnitTestAssembly
                    | BytecodeContext::UnitTestAssemblySubroutine => (),
                },

                InstrumentState::Multiple(old_note_range)
                | InstrumentState::Known(_, old_note_range)
                | InstrumentState::Hint(_, old_note_range) => {
                    if !old_note_range.contains(sub_notes.start())
                        || !old_note_range.contains(sub_notes.end())
                    {
                        return Err(BytecodeError::SubroutineNotesOutOfRange {
                            subroutine_range: sub_notes.clone(),
                            inst_range: old_note_range,
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn _call_subroutine(
        &mut self,
        name: &str,
        subroutine: &Subroutine,
        disable_vibraro: bool,
    ) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongSubroutine { .. }
            | BytecodeContext::SongChannel { .. }
            | BytecodeContext::SoundEffect
            | BytecodeContext::SfxSubroutine => (),

            BytecodeContext::MmlPrefix => return Err(BytecodeError::SubroutineCallInMmlPrefix),

            #[cfg(feature = "test")]
            BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => (),
        }

        self._update_subtroutine_state_excluding_stack_depth(subroutine)?;

        let stack_depth = StackDepth(
            self.get_stack_depth().0
                + BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
                + subroutine.bc_state.max_stack_depth.0,
        );

        self.state.max_stack_depth = max(self.state.max_stack_depth, stack_depth);

        if stack_depth > MAX_STACK_DEPTH {
            return Err(BytecodeError::StackOverflowInSubroutineCall(
                name.to_owned(),
                stack_depth.0,
            ));
        }

        let opcode = match disable_vibraro {
            true => opcodes::CALL_SUBROUTINE_AND_DISABLE_VIBRATO,
            false => opcodes::CALL_SUBROUTINE,
        };

        emit_bytecode!(self, opcode, subroutine.index);
        Ok(())
    }

    pub fn call_subroutine_and_disable_vibrato(
        &mut self,
        name: &str,
        subroutine: &Subroutine,
    ) -> Result<(), BytecodeError> {
        self.state.vibrato.disable();

        self._call_subroutine(name, subroutine, true)
    }

    pub fn call_subroutine(
        &mut self,
        name: &str,
        subroutine: &Subroutine,
    ) -> Result<(), BytecodeError> {
        self._call_subroutine(name, subroutine, false)
    }

    pub fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SoundEffect | BytecodeContext::SfxSubroutine => {
                return Err(BytecodeError::CannotChangeTickClockInASoundEffect)
            }
            BytecodeContext::SongChannel { .. }
            | BytecodeContext::SongSubroutine { .. }
            | BytecodeContext::MmlPrefix => (),

            #[cfg(feature = "test")]
            BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => (),
        }

        self.state
            .tempo_changes
            .push((self.state.tick_counter, tick_clock));

        emit_bytecode!(
            self,
            opcodes::MISCELLANEOUS,
            MiscInstruction::SetSongTickClock.as_u8(),
            tick_clock.into_driver_value()
        );
        Ok(())
    }

    fn _find_subroutine(&self, name: &'a str) -> Result<&'a Subroutine, BytecodeError> {
        match self.subroutin_name_map.find_subroutine_index(name) {
            Some(index) => match self.subroutines.get(index.into()) {
                GetSubroutineResult::Compiled(_, s) => Ok(s),
                GetSubroutineResult::NotCompiled(_) => {
                    Err(BytecodeError::SubroutineRecursion(name.to_owned()))
                }
                GetSubroutineResult::CompileError(_) => {
                    // Subroutine has been compiled, but it contains an error
                    Err(BytecodeError::SubroutineHasError(name.to_owned()))
                }
                GetSubroutineResult::NotFound => {
                    Err(BytecodeError::UnknownSubroutine(name.to_owned()))
                }
            },
            None => match &self.context {
                BytecodeContext::SongSubroutine { .. }
                | BytecodeContext::SongChannel { .. }
                | BytecodeContext::SfxSubroutine
                | BytecodeContext::SoundEffect => {
                    Err(BytecodeError::UnknownSubroutine(name.to_owned()))
                }

                BytecodeContext::MmlPrefix => Err(BytecodeError::NotAllowedToCallSubroutine),

                #[cfg(feature = "test")]
                BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => {
                    Err(BytecodeError::UnknownSubroutine(name.to_owned()))
                }
            },
        }
    }

    pub fn call_subroutine_str(&mut self, name: &str) -> Result<(), BytecodeError> {
        self.call_subroutine(name, self._find_subroutine(name)?)
    }

    pub fn call_subroutine_and_disable_vibrato_str(
        &mut self,
        name: &str,
    ) -> Result<(), BytecodeError> {
        self.call_subroutine_and_disable_vibrato(name, self._find_subroutine(name)?)
    }

    fn _find_instrument(&self, name: &str) -> Result<InstrumentId, BytecodeError> {
        match self.instruments.get_with_index(name) {
            Some((i, _inst)) => match InstrumentId::try_from(i) {
                Ok(inst) => Ok(inst),
                Err(_) => Err(BytecodeError::InvalidInstrumentId),
            },
            None => Err(BytecodeError::UnknownInstrument(name.to_owned())),
        }
    }

    pub fn set_instrument_str(&mut self, name: &str) -> Result<(), BytecodeError> {
        self.set_instrument(self._find_instrument(name)?);
        Ok(())
    }

    pub fn set_instrument_and_adsr_str(
        &mut self,
        name: &str,
        adsr: Adsr,
    ) -> Result<(), BytecodeError> {
        self.set_instrument_and_adsr(self._find_instrument(name)?, adsr);
        Ok(())
    }

    pub fn set_instrument_and_gain_str(
        &mut self,
        name: &str,
        gain: Gain,
    ) -> Result<(), BytecodeError> {
        self.set_instrument_and_gain(self._find_instrument(name)?, gain);
        Ok(())
    }

    pub fn set_echo_volume(&mut self, evol: EchoVolume) {
        emit_bytecode!(self, opcodes::SET_ECHO_VOLUME, evol.as_u8());
    }

    pub fn set_stereo_echo_volume(&mut self, left: EchoVolume, right: EchoVolume) {
        if left == right {
            self.set_echo_volume(left);
        } else {
            emit_bytecode!(
                self,
                opcodes::SET_STEREO_ECHO_VOLUME,
                left.as_u8(),
                right.as_u8()
            );
        }
    }

    pub fn adjust_echo_volume(&mut self, relative: RelativeEchoVolume) {
        if relative.as_i8() != 0 {
            emit_bytecode!(self, opcodes::ADJUST_ECHO_VOLUME, relative.as_i8());
        }
    }

    pub fn adjust_stereo_echo_volume(
        &mut self,
        left: RelativeEchoVolume,
        right: RelativeEchoVolume,
    ) {
        if left == right {
            self.adjust_echo_volume(left);
        } else {
            emit_bytecode!(
                self,
                opcodes::ADJUST_STEREO_ECHO_VOLUME,
                left.as_i8(),
                right.as_i8()
            );
        }
    }

    pub fn set_echo_feedback(&mut self, efb: EchoFeedback) {
        emit_bytecode!(
            self,
            opcodes::SET_OR_ADJUST_ECHO_I8,
            (ECHO_I8_EFB_INDEX << 1) | 1,
            efb.as_i8()
        );
    }

    pub fn adjust_echo_feedback(&mut self, adjust: RelativeEchoFeedback) {
        if adjust.value() != 0 {
            emit_bytecode!(
                self,
                opcodes::SET_OR_ADJUST_ECHO_I8,
                ECHO_I8_EFB_INDEX << 1,
                adjust.as_i8()
            );
        }
    }

    pub fn adjust_echo_feedback_limit(
        &mut self,
        adjust: RelativeEchoFeedback,
        limit: EchoFeedback,
    ) {
        if adjust.as_i8() != 0 {
            emit_bytecode!(
                self,
                opcodes::ADJUST_ECHO_I8_LIMIT,
                ECHO_I8_EFB_INDEX,
                adjust.as_i8(),
                limit.as_i8()
            );
        }
    }

    pub fn set_fir_filter(&mut self, filter: [FirCoefficient; FIR_FILTER_SIZE]) {
        emit_bytecode_array!(self, opcodes::SET_FIR_FILTER, filter);
    }

    pub fn set_fir_tap(&mut self, tap: FirTap, value: FirCoefficient) {
        emit_bytecode!(
            self,
            opcodes::SET_OR_ADJUST_ECHO_I8,
            (tap.as_u8() << 1) | 1,
            value.as_i8()
        );
    }

    pub fn adjust_fir_tap(&mut self, tap: FirTap, adjust: RelativeFirCoefficient) {
        if adjust.as_i8() != 0 {
            emit_bytecode!(
                self,
                opcodes::SET_OR_ADJUST_ECHO_I8,
                tap.as_u8() << 1,
                adjust.as_i8()
            );
        }
    }

    pub fn adjust_fir_tap_limit(
        &mut self,
        tap: FirTap,
        adjust: RelativeFirCoefficient,
        limit: FirCoefficient,
    ) {
        if adjust.as_i8() != 0 {
            emit_bytecode!(
                self,
                opcodes::ADJUST_ECHO_I8_LIMIT,
                tap.as_u8(),
                adjust.as_i8(),
                limit.as_i8()
            );
        }
    }

    pub fn set_echo_invert(&mut self, flags: InvertFlags) {
        emit_bytecode!(
            self,
            opcodes::SET_CHANNEL_OR_ECHO_INVERT,
            flags.into_bytecode_value(false)
        )
    }

    pub fn set_echo_delay(&mut self, length: EchoLength) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongSubroutine { max_edl }
            | BytecodeContext::SongChannel { max_edl, .. } => {
                let edl = length.to_edl();
                if edl.as_u8() <= max_edl.as_u8() {
                    emit_bytecode!(
                        self,
                        opcodes::MISCELLANEOUS,
                        MiscInstruction::SetEchoDelay.as_u8(),
                        edl.as_u8()
                    );
                    Ok(())
                } else {
                    Err(BytecodeError::EchoLengthLargerThanMaxEdl { edl, max_edl })
                }
            }
            BytecodeContext::SoundEffect | BytecodeContext::SfxSubroutine => {
                Err(BytecodeError::CannotSetEchoDelayInSoundEffect)
            }
            BytecodeContext::MmlPrefix => Err(BytecodeError::CannotSetEchoDelayInMmlPrefix),

            // Always emit bytecode in unit test assembly
            #[cfg(feature = "test")]
            BytecodeContext::UnitTestAssembly | BytecodeContext::UnitTestAssemblySubroutine => {
                emit_bytecode!(
                    self,
                    opcodes::MISCELLANEOUS,
                    MiscInstruction::SetEchoDelay.as_u8(),
                    length.to_edl().as_u8()
                );
                Ok(())
            }
        }
    }

    pub fn keyon_next_note(&mut self) {
        // Force key-on for note1 of an MML portamento
        self.state.prev_slurred_note = SlurredNoteState::None;

        emit_bytecode!(self, opcodes::KEYON_NEXT_NOTE);
    }
}
