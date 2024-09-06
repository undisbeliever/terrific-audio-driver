//! Audio driver bytecode

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, BC_STACK_BYTES_PER_SUBROUTINE_CALL,
    MAX_INSTRUMENTS_AND_SAMPLES,
};
use crate::envelope::{Adsr, Gain};
use crate::errors::{BytecodeError, ValueError};
use crate::notes::{Note, LAST_NOTE_ID};
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::value_newtypes::{i8_value_newtype, u8_value_newtype, ValueNewType};

use std::cmp::max;

pub const KEY_OFF_TICK_DELAY: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct StackDepth(u32);

const MAX_STACK_DEPTH: StackDepth = StackDepth(BC_CHANNEL_STACK_SIZE as u32);

pub const LAST_PLAY_NOTE_OPCODE: u8 = 0xbf;

u8_value_newtype!(Volume, VolumeOutOfRange, NoVolume);
u8_value_newtype!(Pan, PanOutOfRange, NoPan, 0, 128);
i8_value_newtype!(RelativeVolume, RelativeVolumeOutOfRange, NoVolume);
i8_value_newtype!(RelativePan, RelativePanOutOfRange, NoPan);

u8_value_newtype!(
    PitchOffsetPerTick,
    PitchOffsetPerTickOutOfRange,
    NoPitchOffsetPerTick
);
u8_value_newtype!(
    QuarterWavelengthInTicks,
    QuarterWavelengthOutOfRange,
    NoQuarterWavelength,
    1,
    u8::MAX
);

pub enum BytecodeContext {
    SongSubroutine,
    SongChannel,
    SoundEffect,
}

// Using lower case to match bytecode names in the audio-driver source code.
pub mod opcodes {
    // opcodes 0x00 - 0xbf are play_note opcodes

    pub const PORTAMENTO_DOWN: u8 = 0xc0;
    pub const PORTAMENTO_UP: u8 = 0xc2;

    pub const SET_VIBRATO: u8 = 0xc4;
    pub const SET_VIBRATO_DEPTH_AND_PLAY_NOTE: u8 = 0xc6;

    pub const WAIT: u8 = 0xc8;
    pub const REST: u8 = 0xca;

    pub const SET_INSTRUMENT: u8 = 0xcc;
    pub const SET_INSTRUMENT_AND_ADSR_OR_GAIN: u8 = 0xce;
    pub const SET_ADSR: u8 = 0xd0;
    pub const SET_GAIN: u8 = 0xd2;

    pub const ADJUST_PAN: u8 = 0xd4;
    pub const SET_PAN: u8 = 0xd6;
    pub const SET_PAN_AND_VOLUME: u8 = 0xd8;
    pub const ADJUST_VOLUME: u8 = 0xda;
    pub const SET_VOLUME: u8 = 0xdc;

    pub const SET_SONG_TICK_CLOCK: u8 = 0xde;

    pub const START_LOOP: u8 = 0xe0;
    pub const SKIP_LAST_LOOP: u8 = 0xe2;

    pub const CALL_SUBROUTINE_AND_DISABLE_VIBRATO: u8 = 0xe4;
    pub const CALL_SUBROUTINE: u8 = 0xe6;

    pub const GOTO_RELATIVE: u8 = 0xe8;

    pub const END_LOOP: u8 = 0xea;
    pub const RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO: u8 = 0xec;
    pub const RETURN_FROM_SUBROUTINE: u8 = 0xee;
    pub const ENABLE_ECHO: u8 = 0xf0;
    pub const DISABLE_ECHO: u8 = 0xf2;

    // Last opcode
    pub const DISABLE_CHANNEL: u8 = 0xfe;
}

u8_value_newtype!(
    InstrumentId,
    InstrumentIdOutOfRange,
    NoInstrumentId,
    0,
    (MAX_INSTRUMENTS_AND_SAMPLES - 1) as u8
);

// Added Copy trait to Subroutine to satisfy the borrow checker in `BytecodeAssembler`.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SubroutineId {
    id: u8,
    tick_counter: TickCounter,
    // NOTE: does not include the stack usage when calling the subroutine (ie, can be 0).
    max_stack_depth: StackDepth,
}

impl SubroutineId {
    pub fn new(id: u8, tick_counter: TickCounter, max_stack_depth: StackDepth) -> Self {
        Self {
            id,
            tick_counter,
            max_stack_depth,
        }
    }

    pub fn as_usize(&self) -> usize {
        self.id.into()
    }

    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn max_stack_depth(&self) -> StackDepth {
        self.max_stack_depth
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
    const MIN: u32;
    const MAX: u32;

    fn ticks(self) -> u32;
    fn to_tick_count(self) -> TickCounter;
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BcTicksKeyOff {
    ticks: u16,
    bc_argument: u8,
}

impl BcTicks for BcTicksKeyOff {
    const MIN: u32 = 1 + KEY_OFF_TICK_DELAY;
    const MAX: u32 = 0x100 + KEY_OFF_TICK_DELAY;

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
        if matches!(ticks, Self::MIN..=Self::MAX) {
            Ok(Self {
                ticks: ticks.try_into().unwrap(),
                bc_argument: ((ticks - KEY_OFF_TICK_DELAY) & 0xff).try_into().unwrap(),
            })
        } else {
            Err(ValueError::BcTicksKeyOffOutOfRange)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BcTicksNoKeyOff {
    ticks: u16,
    bc_argument: u8,
}

impl BcTicks for BcTicksNoKeyOff {
    const MIN: u32 = 1;
    const MAX: u32 = 0x100;

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
        if matches!(ticks, Self::MIN..=Self::MAX) {
            // A note length of 0 will wait for 256 ticks.
            let bc_argument = (ticks & 0xff).try_into().unwrap();
            Ok(Self {
                ticks: ticks.try_into().unwrap(),
                bc_argument,
            })
        } else {
            Err(ValueError::BcTicksNoKeyOffOutOfRange)
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
            PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(BcTicksNoKeyOff::MIN).unwrap())
        } else {
            PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(BcTicksKeyOff::MIN).unwrap())
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct PortamentoVelocity(i16);

impl PortamentoVelocity {
    pub const MIN: i16 = -Self::MAX;
    pub const MAX: i16 = u8::MAX as i16;

    pub fn is_negative(&self) -> bool {
        self.0 < 0
    }
    pub fn pitch_offset_per_tick(&self) -> u8 {
        u8::try_from(self.0.abs()).unwrap()
    }
}

impl ValueNewType for PortamentoVelocity {
    type ConvertFrom = i32;
    const MISSING_ERROR: ValueError = ValueError::NoPortamentoVelocity;
}

impl TryFrom<i32> for PortamentoVelocity {
    type Error = ValueError;

    fn try_from(velocity: i32) -> Result<Self, Self::Error> {
        if velocity == 0 {
            Err(ValueError::PortamentoVelocityZero)
        } else if velocity >= Self::MIN.into() && velocity <= Self::MAX.into() {
            Ok(PortamentoVelocity(velocity.try_into().unwrap()))
        } else {
            Err(ValueError::PortamentoVelocityOutOfRange)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopCount(u8);

impl LoopCount {
    pub const MIN: u32 = 2;
    pub const MAX: u32 = 0x100;

    pub const MIN_LOOPCOUNT: LoopCount = LoopCount(Self::MIN as u8);

    pub fn to_u32(self) -> u32 {
        if self.0 == 0 {
            0x100
        } else {
            self.0.into()
        }
    }
}

impl ValueNewType for LoopCount {
    type ConvertFrom = u32;
    const MISSING_ERROR: ValueError = ValueError::NoLoopCount;
}

impl TryFrom<u32> for LoopCount {
    type Error = ValueError;

    fn try_from(loop_count: u32) -> Result<Self, Self::Error> {
        if loop_count == 0x100 {
            Ok(LoopCount(0))
        } else if loop_count < Self::MIN {
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
        assert!(LAST_NOTE_ID * 2 < LAST_PLAY_NOTE_OPCODE);

        let key_off_bit = match length {
            PlayNoteTicks::NoKeyOff(_) => 0,
            PlayNoteTicks::KeyOff(_) => 1,
        };
        let opcode = (note.note_id() << 1) | key_off_bit;
        Self { opcode }
    }
}

#[derive(PartialEq)]
pub enum BcTerminator {
    DisableChannel,
    Goto(usize),
    ReturnFromSubroutine,
    ReturnFromSubroutineAndDisableVibrato,
}

impl BcTerminator {
    pub fn is_return(&self) -> bool {
        match self {
            Self::DisableChannel => false,
            Self::Goto(_) => false,
            Self::ReturnFromSubroutine => true,
            Self::ReturnFromSubroutineAndDisableVibrato => true,
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
    use super::{NoteOpcode, LAST_PLAY_NOTE_OPCODE};

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
            assert!(self.opcode <= LAST_PLAY_NOTE_OPCODE);
            self.opcode
        }
    }
}

struct SkipLastLoop {
    tick_counter: TickCounter,
    // Location of the parameter of the `skip_last_loop` instruction inside `Bytecode::bytecode`.
    bc_parameter_position: usize,
}

struct LoopState {
    start_loop_count: Option<LoopCount>,
    // Location of the `start_loop` instruction inside `Bytecode::bytecode`.
    start_loop_pos: usize,

    tick_counter_at_start_of_loop: TickCounter,
    skip_last_loop: Option<SkipLastLoop>,
}

pub struct Bytecode {
    context: BytecodeContext,
    bytecode: Vec<u8>,

    tick_counter: TickCounter,

    loop_stack: Vec<LoopState>,
    max_stack_depth: StackDepth,
}

impl Bytecode {
    pub fn new(context: BytecodeContext) -> Bytecode {
        Self::new_append_to_vec(Vec::new(), context)
    }

    // Instead of creating a new `Vec<u8>` to hold the bytecode, write the data to the end of `vec`.
    // Takes ownership of the `Vec<u8>`.
    // The `Vec<u8>` can be taken out of `Bytecode` with `Self::bytecode()`.
    pub fn new_append_to_vec(vec: Vec<u8>, context: BytecodeContext) -> Bytecode {
        Bytecode {
            context,
            bytecode: vec,
            tick_counter: TickCounter::new(0),
            loop_stack: Vec::new(),
            max_stack_depth: StackDepth(0),
        }
    }

    pub fn get_context(&self) -> &BytecodeContext {
        &self.context
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn get_tick_counter_with_loop_flag(&self) -> TickCounterWithLoopFlag {
        TickCounterWithLoopFlag {
            ticks: self.tick_counter,
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

    pub fn get_max_stack_depth(&self) -> StackDepth {
        self.max_stack_depth
    }

    pub fn get_bytecode_len(&self) -> usize {
        self.bytecode.len()
    }

    pub fn bytecode(
        mut self,
        terminator: BcTerminator,
    ) -> Result<Vec<u8>, (BytecodeError, Vec<u8>)> {
        if !self.loop_stack.is_empty() {
            return Err((
                BytecodeError::OpenLoopStack(self.loop_stack.len()),
                self.bytecode,
            ));
        }

        if terminator.is_return() && !matches!(self.context, BytecodeContext::SongSubroutine) {
            return Err((BytecodeError::ReturnInNonSubroutine, self.bytecode));
        }

        match terminator {
            BcTerminator::DisableChannel => {
                emit_bytecode!(self, opcodes::DISABLE_CHANNEL);
            }
            BcTerminator::ReturnFromSubroutine => {
                emit_bytecode!(self, opcodes::RETURN_FROM_SUBROUTINE);
            }
            BcTerminator::ReturnFromSubroutineAndDisableVibrato => {
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
                };
            }
        }

        Ok(self.bytecode)
    }

    pub fn wait(&mut self, length: BcTicksNoKeyOff) {
        self.tick_counter += length.to_tick_count();

        emit_bytecode!(self, opcodes::WAIT, length.bc_argument);
    }

    pub fn rest(&mut self, length: BcTicksKeyOff) {
        self.tick_counter += length.to_tick_count();

        emit_bytecode!(self, opcodes::REST, length.bc_argument);
    }

    pub fn play_note(&mut self, note: Note, length: PlayNoteTicks) {
        self.tick_counter += length.to_tick_count();

        let opcode = NoteOpcode::new(note, &length);

        emit_bytecode!(self, opcode.opcode, length.bc_argument());
    }

    pub fn portamento(&mut self, note: Note, velocity: PortamentoVelocity, length: PlayNoteTicks) {
        self.tick_counter += length.to_tick_count();

        let speed = velocity.pitch_offset_per_tick();
        let note_param = NoteOpcode::new(note, &length);
        let length = length.bc_argument();

        if velocity.is_negative() {
            emit_bytecode!(self, opcodes::PORTAMENTO_DOWN, speed, length, note_param);
        } else {
            emit_bytecode!(self, opcodes::PORTAMENTO_UP, speed, length, note_param);
        }
    }

    pub fn set_vibrato_depth_and_play_note(
        &mut self,
        pitch_offset_per_tick: PitchOffsetPerTick,
        note: Note,
        length: PlayNoteTicks,
    ) {
        self.tick_counter += length.to_tick_count();

        let play_note_opcode = NoteOpcode::new(note, &length);

        emit_bytecode!(
            self,
            opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE,
            pitch_offset_per_tick.as_u8(),
            play_note_opcode,
            length.bc_argument()
        );
    }

    pub fn set_vibrato(
        &mut self,
        pitch_offset_per_tick: PitchOffsetPerTick,
        quarter_wavelength_ticks: QuarterWavelengthInTicks,
    ) {
        emit_bytecode!(
            self,
            opcodes::SET_VIBRATO,
            pitch_offset_per_tick.as_u8(),
            quarter_wavelength_ticks.as_u8()
        );
    }

    pub fn disable_vibrato(&mut self) {
        emit_bytecode!(self, opcodes::SET_VIBRATO, 0u8, 0u8);
    }

    pub fn set_instrument(&mut self, instrument: InstrumentId) {
        emit_bytecode!(self, opcodes::SET_INSTRUMENT, instrument.as_u8());
    }

    pub fn set_instrument_and_adsr(&mut self, instrument: InstrumentId, adsr: Adsr) {
        emit_bytecode!(
            self,
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN,
            instrument.as_u8(),
            adsr.adsr1(),
            adsr.adsr2()
        );
    }

    pub fn set_instrument_and_gain(&mut self, instrument: InstrumentId, gain: Gain) {
        emit_bytecode!(
            self,
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN,
            instrument.as_u8(),
            0u8,
            gain.value()
        );
    }

    pub fn set_adsr(&mut self, adsr: Adsr) {
        emit_bytecode!(self, opcodes::SET_ADSR, adsr.adsr1(), adsr.adsr2());
    }

    pub fn set_gain(&mut self, gain: Gain) {
        emit_bytecode!(self, opcodes::SET_GAIN, gain.value());
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

    pub fn enable_echo(&mut self) {
        emit_bytecode!(self, opcodes::ENABLE_ECHO);
    }

    pub fn disable_echo(&mut self) {
        emit_bytecode!(self, opcodes::DISABLE_ECHO);
    }

    pub fn start_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
        self.loop_stack.push(LoopState {
            start_loop_count: loop_count,
            start_loop_pos: self.bytecode.len(),
            tick_counter_at_start_of_loop: self.tick_counter,
            skip_last_loop: None,
        });

        let loop_count = match loop_count {
            Some(lc) => lc.0,
            None => 0,
        };

        emit_bytecode!(self, opcodes::START_LOOP, loop_count);

        let stack_depth = self.get_stack_depth();
        self.max_stack_depth = max(self.max_stack_depth, stack_depth);

        if stack_depth <= MAX_STACK_DEPTH {
            Ok(())
        } else {
            Err(BytecodeError::StackOverflowInStartLoop(stack_depth.0))
        }
    }

    pub fn skip_last_loop(&mut self) -> Result<(), BytecodeError> {
        let loop_state = match self.loop_stack.last_mut() {
            Some(l) => l,
            None => return Err(BytecodeError::NotInALoop),
        };

        if loop_state.skip_last_loop.is_some() {
            return Err(BytecodeError::MultipleSkipLastLoopInstructions);
        }

        if loop_state.tick_counter_at_start_of_loop == self.tick_counter {
            return Err(BytecodeError::NoTicksBeforeSkipLastLoop);
        }

        loop_state.skip_last_loop = Some(SkipLastLoop {
            tick_counter: self.tick_counter,
            bc_parameter_position: self.bytecode.len() + 1,
        });

        emit_bytecode!(self, opcodes::SKIP_LAST_LOOP, 0u8);

        Ok(())
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
                self.tick_counter.value() - loop_state.tick_counter_at_start_of_loop.value();
            if ticks_in_loop == 0 {
                return Err(BytecodeError::NoTicksInLoop);
            }

            let ticks_skipped_in_last_loop = match &loop_state.skip_last_loop {
                Some(s) => {
                    if self.tick_counter == s.tick_counter {
                        return Err(BytecodeError::NoTicksAfterSkipLastLoop);
                    }
                    self.tick_counter.value() - s.tick_counter.value()
                }
                None => 0,
            };

            assert!(ticks_skipped_in_last_loop < ticks_in_loop);

            let ticks_to_add = ticks_in_loop
                .checked_mul(loop_count_u32 - 1)
                .and_then(|t| t.checked_sub(ticks_skipped_in_last_loop))
                .unwrap_or(u32::MAX);

            self.tick_counter += TickCounter::new(ticks_to_add);
        }

        // Write the loop_count parameter for the start_loop instruction (if required)
        if let Some(loop_count) = loop_count {
            assert_eq!(
                self.bytecode[loop_state.start_loop_pos],
                opcodes::START_LOOP
            );

            self.bytecode[loop_state.start_loop_pos + 1] = loop_count.0;
        }

        // Write the parameter for the skip_last_loop instruction (if required)
        if let Some(skip_last_loop) = loop_state.skip_last_loop {
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

        emit_bytecode!(self, opcodes::END_LOOP);
        Ok(())
    }

    fn _call_subroutine(
        &mut self,
        name: &str,
        subroutine: SubroutineId,
        disable_vibraro: bool,
    ) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SongSubroutine => (),
            BytecodeContext::SongChannel => (),
            BytecodeContext::SoundEffect => return Err(BytecodeError::SubroutineCallInSoundEffect),
        }

        self.tick_counter += subroutine.tick_counter;

        let stack_depth = StackDepth(
            self.get_stack_depth().0
                + BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
                + subroutine.max_stack_depth.0,
        );

        self.max_stack_depth = max(self.max_stack_depth, stack_depth);

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
        subroutine: SubroutineId,
    ) -> Result<(), BytecodeError> {
        self._call_subroutine(name, subroutine, true)
    }

    pub fn call_subroutine(
        &mut self,
        name: &str,
        subroutine: SubroutineId,
    ) -> Result<(), BytecodeError> {
        self._call_subroutine(name, subroutine, false)
    }

    pub fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), BytecodeError> {
        match self.context {
            BytecodeContext::SoundEffect => {
                return Err(BytecodeError::CannotChangeTickClockInASoundEffect)
            }
            BytecodeContext::SongChannel | BytecodeContext::SongSubroutine => (),
        }

        emit_bytecode!(self, opcodes::SET_SONG_TICK_CLOCK, tick_clock.as_u8());
        Ok(())
    }
}
