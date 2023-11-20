//! Audio driver bytecode

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::driver_constants::MAX_INSTRUMENTS;
use crate::envelope::{Adsr, Gain};
use crate::errors::{BytecodeError, ValueError};
use crate::notes::{Note, LAST_NOTE_ID};
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag};
use crate::value_newtypes::{i8_value_newtype, u8_value_newtype, ValueNewType};

use std::cmp::max;

pub const KEY_OFF_TICK_DELAY: u32 = 1;
pub const MAX_NESTED_LOOPS: u8 = 3;

const LAST_PLAY_NOTE_OPCODE: u8 = 0xbf;

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

// Using lower case to match bytecode names in the audio-driver source code.
#[allow(non_camel_case_types)]
pub enum Opcode {
    // opcodes 0x00 - 0xbf are play_note opcodes
    portamento_down = 0xc0,
    portamento_up = 0xc2,

    set_vibrato = 0xc4,
    set_vibrato_depth_and_play_note = 0xc6,

    rest = 0xc8,
    rest_keyoff = 0xca,
    call_subroutine = 0xcc,

    start_loop_0 = 0xce,
    start_loop_1 = 0xd0,
    start_loop_2 = 0xd2,
    skip_last_loop_0 = 0xd4,
    skip_last_loop_1 = 0xd6,
    skip_last_loop_2 = 0xd8,

    set_instrument = 0xda,
    set_instrument_and_adsr_or_gain = 0xdc,
    set_adsr = 0xde,
    set_gain = 0xe0,

    adjust_pan = 0xe2,
    set_pan = 0xe4,
    set_pan_and_volume = 0xe6,
    adjust_volume = 0xe8,
    set_volume = 0xea,

    set_song_tick_clock = 0xec,

    end = 0xee,
    return_from_subroutine = 0xf0,
    end_loop_0 = 0xf2,
    end_loop_1 = 0xf4,
    end_loop_2 = 0xf6,

    enable_echo = 0xf8,
    disable_echo = 0xfa,

    disable_channel = 0xfe,
}

u8_value_newtype!(
    InstrumentId,
    InstrumentIdOutOfRange,
    NoInstrumentId,
    0,
    (MAX_INSTRUMENTS - 1) as u8
);

// Added Copy trait to Subroutine to satisfy the borrow checker in `BytecodeAssembler`.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SubroutineId {
    id: u8,
    tick_counter: TickCounter,
    max_nested_loops: usize,
}

impl SubroutineId {
    pub fn new(id: u8, tick_counter: TickCounter, max_nested_loops: usize) -> Self {
        Self {
            id,
            tick_counter,
            max_nested_loops,
        }
    }

    pub fn as_usize(&self) -> usize {
        self.id.into()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BcTicksKeyOff {
    ticks: u16,
    bc_argument: u8,
}

impl BcTicksKeyOff {
    pub const MIN: u32 = 1 + KEY_OFF_TICK_DELAY;
    pub const MAX: u32 = 0x100 + KEY_OFF_TICK_DELAY;

    #[allow(dead_code)]
    pub fn ticks(&self) -> u32 {
        self.ticks.into()
    }

    pub fn to_tick_count(self) -> TickCounter {
        TickCounter::new(self.ticks.into())
    }

    pub fn try_from(ticks: u32) -> Result<Self, ValueError> {
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

impl BcTicksNoKeyOff {
    pub const MIN: u32 = 1;
    pub const MAX: u32 = 0x100;

    #[allow(dead_code)]
    pub fn ticks(self) -> u32 {
        self.ticks.into()
    }

    pub fn to_tick_count(self) -> TickCounter {
        TickCounter::new(self.ticks.into())
    }

    pub fn try_from(ticks: u32) -> Result<Self, ValueError> {
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
    LoopChannel,
    ReturnFromSubroutine,
}

// Macro to automatically cast Opcode/NoteOpcode/i8 values and append it to `Bytecode::bytecode`.
macro_rules! emit_bytecode {
    ($self:expr, $opcode:expr) => {
        $self.bytecode.push(emit_bytecode::OpcodeByte::cast($opcode));
    };

    ($self:expr, $opcode:expr $(, $param:expr)+) => {
        $self.bytecode.extend([emit_bytecode::OpcodeByte::cast($opcode), $(emit_bytecode::Parameter::cast($param)),*])
    }
}

mod emit_bytecode {
    use super::{NoteOpcode, Opcode, LAST_PLAY_NOTE_OPCODE};

    pub trait OpcodeByte {
        fn cast(self) -> u8;
    }

    impl OpcodeByte for Opcode {
        fn cast(self) -> u8 {
            self as u8
        }
    }

    impl OpcodeByte for NoteOpcode {
        fn cast(self) -> u8 {
            self.opcode
        }
    }

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
    is_subroutine: bool,
    is_sound_effect: bool,
    bytecode: Vec<u8>,

    tick_counter: TickCounter,

    loop_stack: Vec<LoopState>,
    max_nested_loops: usize,
}

impl Bytecode {
    pub fn new(is_subroutine: bool, is_sound_effect: bool) -> Bytecode {
        Bytecode {
            is_subroutine,
            is_sound_effect,
            bytecode: Vec::new(),
            tick_counter: TickCounter::new(0),
            loop_stack: Vec::new(),
            max_nested_loops: 0,
        }
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

    pub fn get_max_nested_loops(&self) -> usize {
        self.max_nested_loops
    }

    pub fn get_bytecode_len(&self) -> usize {
        self.bytecode.len()
    }

    pub fn bytecode(mut self, terminator: BcTerminator) -> Result<Vec<u8>, BytecodeError> {
        if !self.loop_stack.is_empty() {
            return Err(BytecodeError::OpenLoopStack(self.loop_stack.len()));
        }

        if !self.is_subroutine && terminator == BcTerminator::ReturnFromSubroutine {
            return Err(BytecodeError::ReturnInNonSubroutine);
        }

        let opcode = match terminator {
            BcTerminator::DisableChannel => Opcode::disable_channel,
            BcTerminator::LoopChannel => Opcode::end,
            BcTerminator::ReturnFromSubroutine => Opcode::return_from_subroutine,
        };
        emit_bytecode!(self, opcode);

        Ok(self.bytecode)
    }

    pub fn rest(&mut self, length: BcTicksNoKeyOff) {
        self.tick_counter += length.to_tick_count();

        emit_bytecode!(self, Opcode::rest, length.bc_argument);
    }

    pub fn rest_keyoff(&mut self, length: BcTicksKeyOff) {
        self.tick_counter += length.to_tick_count();

        emit_bytecode!(self, Opcode::rest_keyoff, length.bc_argument);
    }

    pub fn play_note(&mut self, note: Note, length: PlayNoteTicks) {
        self.tick_counter += length.to_tick_count();

        let opcode = NoteOpcode::new(note, &length);

        emit_bytecode!(self, opcode, length.bc_argument());
    }

    pub fn portamento(&mut self, note: Note, velocity: PortamentoVelocity, length: PlayNoteTicks) {
        self.tick_counter += length.to_tick_count();

        let speed = velocity.pitch_offset_per_tick();
        let note_param = NoteOpcode::new(note, &length);
        let length = length.bc_argument();

        if velocity.is_negative() {
            emit_bytecode!(self, Opcode::portamento_down, speed, length, note_param);
        } else {
            emit_bytecode!(self, Opcode::portamento_up, speed, length, note_param);
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
            Opcode::set_vibrato_depth_and_play_note,
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
            Opcode::set_vibrato,
            pitch_offset_per_tick.as_u8(),
            quarter_wavelength_ticks.as_u8()
        );
    }

    pub fn disable_vibrato(&mut self) {
        emit_bytecode!(self, Opcode::set_vibrato, 0u8, 0u8);
    }

    pub fn set_instrument(&mut self, instrument: InstrumentId) {
        emit_bytecode!(self, Opcode::set_instrument, instrument.as_u8());
    }

    pub fn set_instrument_and_adsr(&mut self, instrument: InstrumentId, adsr: Adsr) {
        emit_bytecode!(
            self,
            Opcode::set_instrument_and_adsr_or_gain,
            instrument.as_u8(),
            adsr.adsr1(),
            adsr.adsr2()
        );
    }

    pub fn set_instrument_and_gain(&mut self, instrument: InstrumentId, gain: Gain) {
        emit_bytecode!(
            self,
            Opcode::set_instrument_and_adsr_or_gain,
            instrument.as_u8(),
            0u8,
            gain.value()
        );
    }

    pub fn set_adsr(&mut self, adsr: Adsr) {
        emit_bytecode!(self, Opcode::set_adsr, adsr.adsr1(), adsr.adsr2());
    }

    pub fn set_gain(&mut self, gain: Gain) {
        emit_bytecode!(self, Opcode::set_gain, gain.value());
    }

    pub fn adjust_volume(&mut self, v: RelativeVolume) {
        emit_bytecode!(self, Opcode::adjust_volume, v.as_i8());
    }

    pub fn set_volume(&mut self, volume: Volume) {
        emit_bytecode!(self, Opcode::set_volume, volume.as_u8());
    }

    pub fn adjust_pan(&mut self, p: RelativePan) {
        emit_bytecode!(self, Opcode::adjust_pan, p.as_i8());
    }

    pub fn set_pan(&mut self, pan: Pan) {
        emit_bytecode!(self, Opcode::set_pan, pan.as_u8());
    }

    pub fn set_pan_and_volume(&mut self, pan: Pan, volume: Volume) {
        emit_bytecode!(
            self,
            Opcode::set_pan_and_volume,
            pan.as_u8(),
            volume.as_u8()
        );
    }

    pub fn enable_echo(&mut self) {
        emit_bytecode!(self, Opcode::enable_echo);
    }

    pub fn disable_echo(&mut self) {
        emit_bytecode!(self, Opcode::disable_echo);
    }

    // Returns the current loops id.
    //   * For subroutines:     loop_id starts at `MAX_NESTED_LOOPS-1` and decreases to 0
    //   * For non-subroutines: loop_id starts at 0 and increases to `MAX_NESTED_LOOPS-1`
    //
    // When starting a new loop, this method MUST be called after `skip_last_loop_pos` is appended.
    fn _loop_id(&self) -> Result<u8, BytecodeError> {
        let n_loops = self.loop_stack.len();

        if n_loops == 0 {
            return Err(BytecodeError::NotInALoop);
        }

        let n_loops = match u8::try_from(n_loops) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::TooManyLoops),
        };
        if n_loops > MAX_NESTED_LOOPS {
            return Err(BytecodeError::TooManyLoops);
        }

        if self.is_subroutine {
            Ok(MAX_NESTED_LOOPS - n_loops)
        } else {
            Ok(n_loops - 1)
        }
    }

    pub fn start_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
        self.loop_stack.push(LoopState {
            start_loop_count: loop_count,
            start_loop_pos: self.bytecode.len(),
            tick_counter_at_start_of_loop: self.tick_counter,
            skip_last_loop: None,
        });

        self.max_nested_loops = max(self.max_nested_loops, self.loop_stack.len());

        let loop_id = self._loop_id()?;
        let opcode = match loop_id {
            0 => Opcode::start_loop_0,
            1 => Opcode::start_loop_1,
            2 => Opcode::start_loop_2,
            MAX_NESTED_LOOPS.. => panic!("Invalid loop_id"),
        };

        let loop_count = match loop_count {
            Some(lc) => lc.0,
            None => 0,
        };

        emit_bytecode!(self, opcode, loop_count);
        Ok(())
    }

    pub fn skip_last_loop(&mut self) -> Result<(), BytecodeError> {
        let loop_id = self._loop_id()?;
        let opcode = match loop_id {
            0 => Opcode::skip_last_loop_0,
            1 => Opcode::skip_last_loop_1,
            2 => Opcode::skip_last_loop_2,
            MAX_NESTED_LOOPS.. => panic!("Invalid loop_id"),
        };

        // loop_stack contains at least 1 item
        let loop_stack = self.loop_stack.last_mut().unwrap();

        if loop_stack.skip_last_loop.is_some() {
            return Err(BytecodeError::MultipleSkipLastLoopInstructions);
        }

        if loop_stack.tick_counter_at_start_of_loop == self.tick_counter {
            return Err(BytecodeError::NoTicksBeforeSkipLastLoop);
        }

        loop_stack.skip_last_loop = Some(SkipLastLoop {
            tick_counter: self.tick_counter,
            bc_parameter_position: self.bytecode.len() + 1,
        });

        emit_bytecode!(self, opcode, 0u8);

        Ok(())
    }

    pub fn end_loop(&mut self, loop_count: Option<LoopCount>) -> Result<(), BytecodeError> {
        let loop_id = match self._loop_id() {
            Ok(i) => i,
            Err(e) => {
                // Always pop loop_stack
                self.loop_stack.pop();
                return Err(e);
            }
        };
        let opcode = match loop_id {
            0 => Opcode::end_loop_0,
            1 => Opcode::end_loop_1,
            2 => Opcode::end_loop_2,
            MAX_NESTED_LOOPS.. => panic!("Invalid loop_id"),
        };

        // loop_stack contains at least 1 item
        let loop_state = self.loop_stack.pop().unwrap();

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

            self.tick_counter += TickCounter::new(
                (ticks_in_loop * (loop_count_u32 - 1)) - ticks_skipped_in_last_loop,
            );
        }

        // Write the loop_count parameter for the start_loop instruction (if required)
        if let Some(loop_count) = loop_count {
            let start_loop_opcode = Opcode::start_loop_0 as u8 + loop_id * 2;
            assert!(self.bytecode[loop_state.start_loop_pos] == start_loop_opcode);

            self.bytecode[loop_state.start_loop_pos + 1] = loop_count.0;
        }

        // Write the parameter for the skip_last_loop instruction (if required)
        if let Some(skip_last_loop) = loop_state.skip_last_loop {
            let sll_param_pos = skip_last_loop.bc_parameter_position;

            let skip_last_loop_opcode = Opcode::skip_last_loop_0 as u8 + loop_id * 2;
            assert!(self.bytecode[sll_param_pos - 1] == skip_last_loop_opcode);

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

        emit_bytecode!(self, opcode);
        Ok(())
    }

    pub fn call_subroutine(&mut self, subroutine: SubroutineId) -> Result<(), BytecodeError> {
        if self.is_subroutine {
            return Err(BytecodeError::SubroutineCallInSubroutine);
        }

        self.tick_counter += subroutine.tick_counter;

        let n_nested_loops = self.loop_stack.len() + subroutine.max_nested_loops;
        self.max_nested_loops = max(self.max_nested_loops, n_nested_loops);

        if n_nested_loops > MAX_NESTED_LOOPS.into() {
            return Err(BytecodeError::TooManyLoopsInSubroutineCall);
        }

        emit_bytecode!(self, Opcode::call_subroutine, subroutine.id);
        Ok(())
    }

    pub fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), BytecodeError> {
        if self.is_sound_effect {
            return Err(BytecodeError::CannotChangeTickClockInASoundEffect);
        }

        emit_bytecode!(self, Opcode::set_song_tick_clock, tick_clock.as_u8());
        Ok(())
    }
}
