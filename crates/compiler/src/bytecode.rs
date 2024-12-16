//! Audio driver bytecode

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::data::{self, UniqueNamesList};
use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, BC_STACK_BYTES_PER_SUBROUTINE_CALL,
    MAX_INSTRUMENTS_AND_SAMPLES,
};
use crate::envelope::{Adsr, Envelope, Gain, OptionalGain, TempGain};
use crate::errors::{BytecodeError, ValueError};
use crate::notes::{Note, LAST_NOTE_ID, N_NOTES};
use crate::samples::note_range;
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::value_newtypes::{
    i16_non_zero_value_newtype, i16_value_newtype, i8_value_newtype, u16_value_newtype,
    u8_0_is_256_value_newtype, u8_value_newtype, SignedValueNewType, UnsignedValueNewType,
};

use std::cmp::{max, min};
use std::collections::HashMap;
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

pub enum BytecodeContext {
    SongSubroutine,
    SongChannel(u8),
    SoundEffect,
    MmlPrefix,
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
        PORTAMENTO_DOWN,
        PORTAMENTO_UP,
        SET_VIBRATO,
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
        SET_DETUNE_I16,
        SET_DETUNE_P8,
        SET_DETUNE_N8,
        ADJUST_PAN,
        SET_PAN,
        SET_PAN_AND_VOLUME,
        ADJUST_VOLUME,
        SET_VOLUME,
        VOLUME_SLIDE_UP,
        VOLUME_SLIDE_DOWN,
        TREMOLO,
        PAN_SLIDE_UP,
        PAN_SLIDE_DOWN,
        PANBRELLO,
        SET_SONG_TICK_CLOCK,
        START_LOOP,
        SKIP_LAST_LOOP,
        CALL_SUBROUTINE_AND_DISABLE_VIBRATO,
        CALL_SUBROUTINE,
        GOTO_RELATIVE,
        END_LOOP,
        RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO,
        RETURN_FROM_SUBROUTINE,
        DISABLE_NOISE,
        ENABLE_PMOD,
        DISABLE_PMOD,
        ENABLE_ECHO,
        DISABLE_ECHO,
        REUSE_TEMP_GAIN,
    );

    // Last not play-note opcode
    pub const DISABLE_CHANNEL: u8 = FIRST_PLAY_NOTE_INSTRUCTION - 1;

    // Opcodes FIRST_PLAY_NOTE_INSTRUCTION.. are play note opcodes
    pub const FIRST_PLAY_NOTE_INSTRUCTION: u8 = 64;
}

const _: () = assert!(
    opcodes::FIRST_PLAY_NOTE_INSTRUCTION as u32 + (N_NOTES * 2) as u32 == 0x100,
    "There are unaccounted bytecode opcodes"
);

u8_value_newtype!(
    InstrumentId,
    InstrumentIdOutOfRange,
    NoInstrumentId,
    0,
    (MAX_INSTRUMENTS_AND_SAMPLES - 1) as u8
);

#[derive(Debug, Clone, PartialEq)]
pub struct SubroutineId {
    id: u8,
    state: State,
}

impl SubroutineId {
    pub fn new(id: u8, state: State) -> Self {
        Self { id, state }
    }

    pub fn as_usize(&self) -> usize {
        self.id.into()
    }

    pub fn tick_counter(&self) -> TickCounter {
        self.state.tick_counter
    }

    pub fn max_stack_depth(&self) -> StackDepth {
        self.state.max_stack_depth
    }

    pub fn no_instrument_notes(&self) -> &RangeInclusive<Note> {
        &self.state.no_instrument_notes
    }
}

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

#[derive(Copy, Clone)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopCount(u8);

impl LoopCount {
    pub const MIN_LOOPS: u32 = 2;
    pub const MAX_LOOPS: u32 = 0x100;

    pub const MIN: Self = Self(Self::MIN_LOOPS as u8);
    pub const MAX: Self = Self(0);

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
    TailSubroutineCall(usize, &'a SubroutineId),
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

mod emit_bytecode {
    use super::NoteOpcode;
    use crate::opcodes;

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
}

// Instrument or envelope state
#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum IeState<T>
where
    T: Copy + PartialEq,
{
    Known(T),
    Maybe(T),
    Unknown,
}

impl<T> IeState<T>
where
    T: Copy + PartialEq,
{
    pub fn is_known_and_eq(&self, o: &T) -> bool {
        match self {
            Self::Known(v) => o == v,
            Self::Maybe(_) => false,
            Self::Unknown => false,
        }
    }

    fn promote_to_known(&self) -> Self {
        match self {
            Self::Known(v) => Self::Known(*v),
            Self::Maybe(v) => Self::Known(*v),
            Self::Unknown => Self::Unknown,
        }
    }

    fn demote_to_maybe(&self) -> Self {
        match self {
            Self::Known(v) => Self::Maybe(*v),
            Self::Maybe(v) => Self::Maybe(*v),
            Self::Unknown => Self::Unknown,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VibratoState {
    Unchanged,
    Unknown,
    // Disabled with an unknown value
    Disabled,
    Set(VibratoPitchOffsetPerTick, VibratoQuarterWavelengthInTicks),
}

impl VibratoState {
    pub fn is_active(&self) -> bool {
        match self {
            Self::Unchanged => false,
            Self::Unknown => false,
            Self::Disabled => false,
            Self::Set(pitch_offset_per_tick, _) => pitch_offset_per_tick.as_u8() > 0,
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
            VibratoState::Set(_, qwt) => VibratoState::Set(depth, *qwt),
        }
    }

    fn disable(&mut self) {
        *self = match self {
            VibratoState::Unchanged => VibratoState::Disabled,
            VibratoState::Unknown => VibratoState::Disabled,
            VibratoState::Disabled => VibratoState::Disabled,
            VibratoState::Set(_, qwt) => VibratoState::Set(VibratoPitchOffsetPerTick::new(0), *qwt),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SlurredNoteState {
    Unchanged,
    None,
    Slurred(Note),
    SlurredPitch,
    SlurredNoise,
}

impl SlurredNoteState {
    fn merge(&mut self, o: &Self) {
        match o {
            SlurredNoteState::Unchanged => (),
            SlurredNoteState::None => *self = SlurredNoteState::None,
            SlurredNoteState::Slurred(n) => *self = SlurredNoteState::Slurred(*n),
            SlurredNoteState::SlurredPitch => *self = SlurredNoteState::SlurredPitch,
            SlurredNoteState::SlurredNoise => *self = SlurredNoteState::SlurredNoise,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub tick_counter: TickCounter,
    pub max_stack_depth: StackDepth,
    pub tempo_changes: Vec<(TickCounter, TickClock)>,

    pub(crate) instrument: IeState<InstrumentId>,
    pub(crate) envelope: IeState<Envelope>,
    pub(crate) prev_temp_gain: IeState<TempGain>,
    pub(crate) early_release:
        IeState<Option<(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain)>>,
    pub(crate) detune: IeState<DetuneValue>,
    pub(crate) vibrato: VibratoState,
    pub(crate) prev_slurred_note: SlurredNoteState,

    note_range: Option<RangeInclusive<Note>>,
    no_instrument_notes: RangeInclusive<Note>,
}

struct SkipLastLoop {
    tick_counter: TickCounter,
    // Location of the parameter of the `skip_last_loop` instruction inside `Bytecode::bytecode`.
    bc_parameter_position: usize,

    note_range: Option<RangeInclusive<Note>>,

    instrument: IeState<InstrumentId>,
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

    tick_counter_at_start_of_loop: TickCounter,
    skip_last_loop: Option<SkipLastLoop>,
}

pub struct Bytecode<'a> {
    context: BytecodeContext,
    bytecode: Vec<u8>,

    instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    subroutines: Option<&'a HashMap<&'a str, SubroutineId>>,

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
        subroutines: Option<&'a HashMap<&'a str, SubroutineId>>,
    ) -> Bytecode<'a> {
        Self::new_append_to_vec(Vec::new(), context, instruments, subroutines)
    }

    // Instead of creating a new `Vec<u8>` to hold the bytecode, write the data to the end of `vec`.
    // Takes ownership of the `Vec<u8>`.
    // The `Vec<u8>` can be taken out of `Bytecode` with `Self::bytecode()`.
    pub fn new_append_to_vec(
        vec: Vec<u8>,
        context: BytecodeContext,
        instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        subroutines: Option<&'a HashMap<&'a str, SubroutineId>>,
    ) -> Bytecode<'a> {
        Bytecode {
            bytecode: vec,
            instruments,
            subroutines,
            state: State {
                tick_counter: TickCounter::new(0),
                max_stack_depth: StackDepth(0),
                tempo_changes: Vec::new(),
                instrument: IeState::Unknown,
                envelope: IeState::Unknown,
                prev_temp_gain: IeState::Unknown,
                early_release: IeState::Unknown,
                detune: match &context {
                    BytecodeContext::SoundEffect => IeState::Known(DetuneValue::ZERO),
                    BytecodeContext::SongChannel(_) => IeState::Known(DetuneValue::ZERO),
                    BytecodeContext::SongSubroutine => IeState::Unknown,
                    BytecodeContext::MmlPrefix => IeState::Unknown,
                },
                vibrato: match &context {
                    BytecodeContext::SoundEffect => VibratoState::Disabled,
                    BytecodeContext::SongChannel(_) => VibratoState::Disabled,
                    BytecodeContext::SongSubroutine => VibratoState::Unchanged,
                    BytecodeContext::MmlPrefix => VibratoState::Disabled,
                },
                prev_slurred_note: SlurredNoteState::Unchanged,
                note_range: None,
                no_instrument_notes: Note::MAX..=Note::MIN,
            },
            loop_stack: Vec::new(),
            asm_block_stack_len: 0,
            show_missing_set_instrument_error: match &context {
                BytecodeContext::SoundEffect => true,
                BytecodeContext::SongChannel(_) => true,
                BytecodeContext::SongSubroutine => false,
                BytecodeContext::MmlPrefix => false,
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

    pub fn get_tick_counter(&self) -> TickCounter {
        self.state.tick_counter
    }

    pub fn get_tick_counter_with_loop_flag(&self) -> TickCounterWithLoopFlag {
        TickCounterWithLoopFlag {
            ticks: self.state.tick_counter,
            in_loop: self.is_in_loop(),
        }
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
    pub fn _song_loop_point(&mut self) {
        assert!(matches!(self.context, BytecodeContext::SongChannel(_)));
        assert_eq!(self.get_stack_depth(), StackDepth(0));

        // The instrument or envelope may have changed when the song loops.
        self.state.instrument = self.state.instrument.demote_to_maybe();
        self.state.envelope = self.state.envelope.demote_to_maybe();
        self.state.prev_temp_gain = self.state.prev_temp_gain.demote_to_maybe();
        self.state.early_release = self.state.early_release.demote_to_maybe();
        self.state.detune = self.state.detune.demote_to_maybe();

        self.state.vibrato = VibratoState::Unknown;
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

        if terminator.is_return() && !matches!(self.context, BytecodeContext::SongSubroutine) {
            return Err((BytecodeError::ReturnInNonSubroutine, self.bytecode));
        }

        if let BcTerminator::TailSubroutineCall(_, s) = &terminator {
            if let Err(e) = self._update_subtroutine_state_excluding_stack_depth(s) {
                return Err((e, self.bytecode));
            }
        }

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
            BcTerminator::Goto(pos) | BcTerminator::TailSubroutineCall(pos, _) => {
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
        }

        Ok((self.bytecode, self.state))
    }

    fn _test_note_in_range(&mut self, note: Note) -> Result<(), BytecodeError> {
        match &self.state.note_range {
            Some(note_range) => {
                if note_range.contains(&note) {
                    Ok(())
                } else {
                    Err(BytecodeError::NoteOutOfRange(note, note_range.clone()))
                }
            }
            None => {
                let s = &mut self.state;
                if s.no_instrument_notes.is_empty() {
                    s.no_instrument_notes = note..=note;
                } else if note < *s.no_instrument_notes.start() {
                    s.no_instrument_notes = note..=*(s.no_instrument_notes.end());
                } else if note > *s.no_instrument_notes.end() {
                    s.no_instrument_notes = *(s.no_instrument_notes.start())..=note;
                }

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
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::SlurredPitch,
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
        emit_bytecode!(self, opcodes::DISABLE_NOISE);
    }

    pub fn play_note(&mut self, note: Note, length: PlayNoteTicks) -> Result<(), BytecodeError> {
        let r = self._test_note_in_range(note);

        self.state.tick_counter += length.to_tick_count();
        self.state.prev_slurred_note = match length {
            PlayNoteTicks::KeyOff(_) => SlurredNoteState::None,
            PlayNoteTicks::NoKeyOff(_) => SlurredNoteState::Slurred(note),
        };

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

        let speed = velocity.pitch_offset_per_tick();
        let note_param = NoteOpcode::new(note, &length);
        let length = length.bc_argument();

        if velocity.is_negative() {
            emit_bytecode!(self, opcodes::PORTAMENTO_DOWN, speed, length, note_param);
        } else {
            emit_bytecode!(self, opcodes::PORTAMENTO_UP, speed, length, note_param);
        }

        r
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
        self.state.vibrato = VibratoState::Set(pitch_offset_per_tick, quarter_wavelength_ticks);

        emit_bytecode!(
            self,
            opcodes::SET_VIBRATO,
            pitch_offset_per_tick.as_u8(),
            quarter_wavelength_ticks.as_u8()
        );
    }

    pub fn disable_vibrato(&mut self) {
        self.state.vibrato.disable();

        emit_bytecode!(self, opcodes::SET_VIBRATO, 0u8, 0u8);
    }

    fn _set_state_instrument_and_note_range(&mut self, instrument: InstrumentId) {
        self.state.note_range = self
            .instruments
            .get_index(instrument.as_u8().into())
            .map(note_range);
        self.state.instrument = IeState::Known(instrument);
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

    fn _pmod_instruction(&mut self, opcode: u8) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongChannel(0) => Err(BytecodeError::PmodNotAllowedInChannelA),
            BytecodeContext::SongChannel(6) | BytecodeContext::SongChannel(7) => {
                Err(BytecodeError::PmodNotAllowedInChannelsGH)
            }
            BytecodeContext::SoundEffect => Err(BytecodeError::PmodNotAllowedInSoundEffect),

            BytecodeContext::SongChannel(_)
            | BytecodeContext::SongSubroutine
            | BytecodeContext::MmlPrefix => {
                emit_bytecode!(self, opcode);
                Ok(())
            }
        }
    }

    pub fn enable_pmod(&mut self) -> Result<(), BytecodeError> {
        self._pmod_instruction(opcodes::ENABLE_PMOD)
    }

    pub fn disable_pmod(&mut self) -> Result<(), BytecodeError> {
        self._pmod_instruction(opcodes::DISABLE_PMOD)
    }

    pub fn enable_echo(&mut self) {
        emit_bytecode!(self, opcodes::ENABLE_ECHO);
    }

    pub fn disable_echo(&mut self) {
        emit_bytecode!(self, opcodes::DISABLE_ECHO);
    }

    pub fn start_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
        // The loop might change the instrument and evelope.
        // When the loop loops, the instrument/envelope might have changed.
        self.state.instrument = self.state.instrument.demote_to_maybe();
        self.state.envelope = self.state.envelope.demote_to_maybe();
        self.state.prev_temp_gain = self.state.prev_temp_gain.demote_to_maybe();
        self.state.early_release = self.state.early_release.demote_to_maybe();
        self.state.detune = self.state.detune.demote_to_maybe();
        self.state.vibrato = VibratoState::Unknown;

        // Loop might end on a note that is not slurred and matching `prev_slurred_note`.
        self.state.prev_slurred_note = SlurredNoteState::Unchanged;

        self.loop_stack.push(LoopState {
            start_loop_count: loop_count,
            start_loop_pos: self.bytecode.len(),
            tick_counter_at_start_of_loop: self.state.tick_counter,
            skip_last_loop: None,
        });

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

    pub fn skip_last_loop(&mut self) -> Result<(), BytecodeError> {
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
            note_range: self.state.note_range.clone(),
            instrument: self.state.instrument,
            envelope: self.state.envelope,
            prev_temp_gain: self.state.prev_temp_gain,
            early_release: self.state.early_release,
            detune: self.state.detune,
            vibrato: self.state.vibrato,
            prev_slurred_note: self.state.prev_slurred_note.clone(),
        });

        emit_bytecode!(self, opcodes::SKIP_LAST_LOOP, 0u8);

        if self.loop_stack.len() > self.asm_block_stack_len {
            Ok(())
        } else {
            Err(BytecodeError::CannotModifyLoopOutsideAsmBlock)
        }
    }

    pub fn end_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
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
            self.state.instrument = skip_last_loop.instrument;
            self.state.note_range = skip_last_loop.note_range;
            self.state.envelope = skip_last_loop.envelope;
            self.state.prev_temp_gain = skip_last_loop.prev_temp_gain;
            self.state.early_release = skip_last_loop.early_release;
            self.state.detune = skip_last_loop.detune;
            self.state.vibrato = skip_last_loop.vibrato;
            self.state
                .prev_slurred_note
                .merge(&skip_last_loop.prev_slurred_note);

            // Write the parameter for the skip_last_loop instruction (if required)
            let sll_param_pos = skip_last_loop.bc_parameter_position;

            assert_eq!(self.bytecode[sll_param_pos - 1], opcodes::SKIP_LAST_LOOP);

            let to_skip = self.bytecode.len() - sll_param_pos;
            if to_skip < 1 {
                return Err(BytecodeError::SkipLastLoopOutOfBounds(to_skip));
            }
            let to_skip = match u8::try_from(to_skip) {
                Ok(i) => i,
                Err(_) => return Err(BytecodeError::SkipLastLoopOutOfBounds(to_skip)),
            };

            self.bytecode[sll_param_pos] = to_skip;
        }

        if self.loop_stack.is_empty() {
            self.state.instrument = self.state.instrument.promote_to_known();
            self.state.envelope = self.state.envelope.promote_to_known();
            self.state.prev_temp_gain = self.state.prev_temp_gain.promote_to_known();
            self.state.early_release = self.state.early_release.promote_to_known();
            self.state.detune = self.state.detune.promote_to_known();
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
        subroutine: &SubroutineId,
    ) -> Result<(), BytecodeError> {
        self.state.tick_counter += subroutine.state.tick_counter;

        let old_note_range = self.state.note_range.clone();

        match subroutine.state.instrument {
            IeState::Known(i) => {
                assert!(subroutine.state.note_range.is_some());

                self.state.instrument = IeState::Known(i);
                self.state.note_range = subroutine.state.note_range.clone();
            }
            IeState::Maybe(_) => panic!("unexpected maybe instrument"),
            IeState::Unknown => (),
        }

        match subroutine.state.envelope {
            IeState::Known(e) => self.state.envelope = IeState::Known(e),
            IeState::Maybe(_) => panic!("unexpected maybe envelope"),
            IeState::Unknown => (),
        }
        match subroutine.state.prev_temp_gain {
            IeState::Known(e) => self.state.prev_temp_gain = IeState::Known(e),
            IeState::Maybe(_) => panic!("unexpected maybe prevTempGain"),
            IeState::Unknown => (),
        }
        match subroutine.state.early_release {
            IeState::Known(e) => self.state.early_release = IeState::Known(e),
            IeState::Maybe(_) => panic!("unexpected maybe early_release"),
            IeState::Unknown => (),
        }
        match subroutine.state.detune {
            IeState::Known(e) => self.state.detune = IeState::Known(e),
            IeState::Maybe(_) => panic!("unexpected maybe detune"),
            IeState::Unknown => (),
        }
        match &subroutine.state.vibrato {
            VibratoState::Unchanged => (),
            VibratoState::Unknown => self.state.vibrato = VibratoState::Unknown,
            VibratoState::Disabled => self.state.vibrato = VibratoState::Disabled,
            v @ VibratoState::Set(..) => self.state.vibrato = *v,
        }
        self.state
            .prev_slurred_note
            .merge(&subroutine.state.prev_slurred_note);

        if !subroutine.state.no_instrument_notes.is_empty() {
            // Subroutine plays a note without an instrument

            let self_notes = &self.state.no_instrument_notes;
            let sub_notes = &subroutine.state.no_instrument_notes;

            match old_note_range {
                None => match self.context {
                    BytecodeContext::SongChannel(_) | BytecodeContext::SoundEffect => {
                        return Err(BytecodeError::SubroutinePlaysNotesWithNoInstrument)
                    }
                    BytecodeContext::SongSubroutine => {
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
                },
                Some(old_note_range) => {
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
        subroutine: &SubroutineId,
        disable_vibraro: bool,
    ) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongSubroutine => (),
            BytecodeContext::SongChannel(_) => (),
            BytecodeContext::SoundEffect => return Err(BytecodeError::SubroutineCallInSoundEffect),
            BytecodeContext::MmlPrefix => return Err(BytecodeError::SubroutineCallInMmlPrefix),
        }

        self._update_subtroutine_state_excluding_stack_depth(subroutine)?;

        let stack_depth = StackDepth(
            self.get_stack_depth().0
                + BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
                + subroutine.state.max_stack_depth.0,
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

        emit_bytecode!(self, opcode, subroutine.id);
        Ok(())
    }

    pub fn call_subroutine_and_disable_vibrato(
        &mut self,
        name: &str,
        subroutine: &SubroutineId,
    ) -> Result<(), BytecodeError> {
        self.state.vibrato.disable();

        self._call_subroutine(name, subroutine, true)
    }

    pub fn call_subroutine(
        &mut self,
        name: &str,
        subroutine: &SubroutineId,
    ) -> Result<(), BytecodeError> {
        self._call_subroutine(name, subroutine, false)
    }

    pub fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SoundEffect => {
                return Err(BytecodeError::CannotChangeTickClockInASoundEffect)
            }
            BytecodeContext::SongChannel(_)
            | BytecodeContext::SongSubroutine
            | BytecodeContext::MmlPrefix => (),
        }

        self.state
            .tempo_changes
            .push((self.state.tick_counter, tick_clock));

        emit_bytecode!(self, opcodes::SET_SONG_TICK_CLOCK, tick_clock.as_u8());
        Ok(())
    }

    fn _find_subroutine(&self, name: &str) -> Result<&'a SubroutineId, BytecodeError> {
        match self.subroutines {
            Some(s) => match s.get(name) {
                Some(s) => Ok(s),
                None => Err(BytecodeError::UnknownSubroutine(name.to_owned())),
            },
            None => match &self.context {
                BytecodeContext::SoundEffect => Err(BytecodeError::SubroutineCallInSoundEffect),
                BytecodeContext::MmlPrefix => Err(BytecodeError::SubroutineCallInMmlPrefix),
                BytecodeContext::SongChannel(_) | BytecodeContext::SongSubroutine => {
                    Err(BytecodeError::NotAllowedToCallSubroutine)
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
}
