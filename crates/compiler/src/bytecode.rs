//! Audio driver bytecode

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::envelope::{Adsr, Gain};
use crate::errors::BytecodeError;
use crate::notes::{Note, LAST_NOTE_ID};
use crate::time::{TickClock, TickCounter};

const KEY_OFF_TICK_DELAY: u64 = 1;
const MAX_NESTED_LOOPS: u8 = 3;

const MAX_PAN: u8 = 128;

const LAST_PLAY_NOTE_OPCODE: u8 = 0xbf;

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

// Added Copy trait to Subroutine to satisfy the borrow checker in `BytecodeAssembler`.
#[derive(Copy, Clone)]
pub struct InstrumentId {
    id: u8,
}

impl InstrumentId {
    pub fn new(id: u8) -> Self {
        Self { id }
    }
}

// Added Copy trait to Subroutine to satisfy the borrow checker in `BytecodeAssembler`.
#[derive(Copy, Clone)]
pub struct SubroutineId {
    id: u8,
    tick_counter: TickCounter,
}

impl SubroutineId {
    // ::TODO remove::
    #[allow(dead_code)]
    pub fn new(id: u8, tick_counter: TickCounter) -> Self {
        Self { id, tick_counter }
    }
}

fn u8_try_from_limit(value: u32, max: u8) -> Result<u8, ()> {
    match u8::try_from(value) {
        Ok(v) => {
            if v <= max {
                Ok(v)
            } else {
                Err(())
            }
        }
        Err(_) => Err(()),
    }
}

fn u8_try_from_range(value: u32, min: u8, max: u8) -> Result<u8, ()> {
    assert!(min <= max);

    match u8::try_from(value) {
        Ok(v) => {
            if v >= min && v <= max {
                Ok(v)
            } else {
                Err(())
            }
        }
        Err(_) => Err(()),
    }
}

fn loop_count_u8(loop_count: u32) -> Result<u8, BytecodeError> {
    if loop_count == 0x100 {
        return Ok(0);
    }

    match u8_try_from_range(loop_count, 2, u8::MAX) {
        Ok(i) => Ok(i),
        Err(_) => Err(BytecodeError::LoopCountOutOfRange(loop_count)),
    }
}

struct NoteOpcode {
    opcode: u8,
}

impl NoteOpcode {
    fn new(note: Note, key_off: bool) -> Self {
        assert!(LAST_NOTE_ID * 2 < LAST_PLAY_NOTE_OPCODE);

        let opcode = (note.note_id() << 1) | (key_off as u8);
        Self { opcode }
    }
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
    loop_count: u32,
    tick_counter_at_start_of_loop: TickCounter,
    skip_last_loop: Option<SkipLastLoop>,
}

pub struct Bytecode {
    is_subroutine: bool,
    is_sound_effect: bool,
    bytecode: Vec<u8>,

    tick_counter: TickCounter,

    loop_stack: Vec<LoopState>,
}

impl Bytecode {
    pub fn new(is_subroutine: bool, is_sound_effect: bool) -> Bytecode {
        Bytecode {
            is_subroutine,
            is_sound_effect,
            bytecode: Vec::new(),
            tick_counter: TickCounter::new(0),
            loop_stack: Vec::new(),
        }
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    // error enum
    pub fn get_bytecode(&self) -> Result<&[u8], BytecodeError> {
        if !self.loop_stack.is_empty() {
            return Err(BytecodeError::OpenLoopStack(self.loop_stack.len()));
        }

        Ok(self.bytecode.as_slice())
    }

    fn _length_argument(
        &mut self,
        length: TickCounter,
        key_off: bool,
    ) -> Result<u8, BytecodeError> {
        self.tick_counter += length;

        let mut l = length.value();

        if key_off {
            if l <= KEY_OFF_TICK_DELAY {
                return Err(BytecodeError::NoteLengthTooShortKeyOffDelay);
            }
            l -= KEY_OFF_TICK_DELAY;
        }

        if l == 0 {
            return Err(BytecodeError::NoteLengthZero);
        }
        if l == 0x100 {
            // A note length of 0 will wait for 256 ticks.
            return Ok(0);
        }

        match u8::try_from(l) {
            Ok(i) => Ok(i),
            Err(_) => Err(BytecodeError::NoteLengthTooLarge),
        }
    }

    pub fn rest(&mut self, length: TickCounter) -> Result<(), BytecodeError> {
        let length = self._length_argument(length, false)?;

        emit_bytecode!(self, Opcode::rest, length);
        Ok(())
    }

    pub fn rest_keyoff(&mut self, length: TickCounter) -> Result<(), BytecodeError> {
        let length = self._length_argument(length, false)?;

        emit_bytecode!(self, Opcode::rest_keyoff, length);
        Ok(())
    }

    pub fn play_note(
        &mut self,
        note: Note,
        key_off: bool,
        length: TickCounter,
    ) -> Result<(), BytecodeError> {
        let opcode = NoteOpcode::new(note, key_off);
        let length = self._length_argument(length, key_off)?;

        emit_bytecode!(self, opcode, length);
        Ok(())
    }

    pub fn portamento(
        &mut self,
        note: Note,
        key_off: bool,
        velocity: i32,
        length: TickCounter,
    ) -> Result<(), BytecodeError> {
        let speed = velocity.abs();
        if speed == 0 {
            return Err(BytecodeError::PortamentoVelocityZero);
        }
        let speed = match u8::try_from(speed) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PortamentoVelocityTooLarge),
        };

        let note_param = NoteOpcode::new(note, key_off);
        let length = self._length_argument(length, key_off)?;

        if velocity < 0 {
            emit_bytecode!(self, Opcode::portamento_down, speed, length, note_param);
        } else {
            emit_bytecode!(self, Opcode::portamento_up, speed, length, note_param);
        }
        Ok(())
    }

    pub fn set_vibrato_depth_and_play_note(
        &mut self,
        pitch_offset_per_tick: u32,
        note: Note,
        key_off: bool,
        length: TickCounter,
    ) -> Result<(), BytecodeError> {
        let pitch_offset_per_tick = match u8::try_from(pitch_offset_per_tick) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PitchOffsetOutOfRange),
        };
        let play_note_opcode = NoteOpcode::new(note, key_off);
        let length = self._length_argument(length, key_off)?;

        emit_bytecode!(
            self,
            Opcode::set_vibrato_depth_and_play_note,
            pitch_offset_per_tick,
            play_note_opcode,
            length
        );
        Ok(())
    }

    pub fn set_vibrato(
        &mut self,
        pitch_offset_per_tick: u32,
        quarter_wavelength_ticks: u32,
    ) -> Result<(), BytecodeError> {
        if pitch_offset_per_tick == 0 {
            return Err(BytecodeError::PitchOffsetPerTickZero);
        }
        if quarter_wavelength_ticks == 0 {
            return Err(BytecodeError::QuarterWaveLengthZero);
        }

        let pitch_offset_per_tick = match u8::try_from(pitch_offset_per_tick) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PitchOffsetOutOfRange),
        };

        let quarter_wavelength_ticks = match u8::try_from(quarter_wavelength_ticks) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::QuarterWaveLengthOutOfRange),
        };

        emit_bytecode!(
            self,
            Opcode::set_vibrato,
            pitch_offset_per_tick,
            quarter_wavelength_ticks
        );
        Ok(())
    }

    pub fn disable_vibrato(&mut self) {
        emit_bytecode!(self, Opcode::set_vibrato, 0u8, 0u8);
    }

    pub fn set_instrument(&mut self, instrument: InstrumentId) {
        emit_bytecode!(self, Opcode::set_instrument, instrument.id);
    }

    pub fn set_instrument_and_adsr(&mut self, instrument: InstrumentId, adsr: Adsr) {
        emit_bytecode!(
            self,
            Opcode::set_instrument_and_adsr_or_gain,
            instrument.id,
            adsr.adsr1(),
            adsr.adsr2()
        );
    }

    pub fn set_instrument_and_gain(&mut self, instrument: InstrumentId, gain: Gain) {
        emit_bytecode!(
            self,
            Opcode::set_instrument_and_adsr_or_gain,
            instrument.id,
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

    pub fn adjust_volume(&mut self, volume_adjust: i32) -> Result<(), BytecodeError> {
        let volume_adjust = match i8::try_from(volume_adjust) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::VolumeAdjustOutOfRange(volume_adjust)),
        };

        emit_bytecode!(self, Opcode::adjust_volume, volume_adjust);
        Ok(())
    }

    pub fn set_volume(&mut self, volume: u32) -> Result<(), BytecodeError> {
        let volume = match u8::try_from(volume) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::VolumeOutOfRange(volume)),
        };

        emit_bytecode!(self, Opcode::set_volume, volume);
        Ok(())
    }

    pub fn adjust_pan(&mut self, pan_adjust: i32) -> Result<(), BytecodeError> {
        let pan_adjust = match i8::try_from(pan_adjust) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PanAdjustOutOfRange(pan_adjust)),
        };

        emit_bytecode!(self, Opcode::adjust_pan, pan_adjust);
        Ok(())
    }

    pub fn set_pan(&mut self, pan: u32) -> Result<(), BytecodeError> {
        let pan = match u8_try_from_limit(pan, MAX_PAN) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PanOutOfRange(pan)),
        };

        emit_bytecode!(self, Opcode::set_pan, pan);
        Ok(())
    }

    pub fn set_pan_and_volume(&mut self, volume: u32, pan: u32) -> Result<(), BytecodeError> {
        let pan = match u8_try_from_limit(pan, MAX_PAN) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::PanOutOfRange(pan)),
        };

        let volume = match u8::try_from(volume) {
            Ok(i) => i,
            Err(_) => return Err(BytecodeError::VolumeOutOfRange(volume)),
        };

        emit_bytecode!(self, Opcode::set_pan_and_volume, pan, volume);
        Ok(())
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

    pub fn start_loop(&mut self, loop_count: u32) -> Result<(), BytecodeError> {
        self.loop_stack.push(LoopState {
            loop_count,
            tick_counter_at_start_of_loop: self.tick_counter,
            skip_last_loop: None,
        });

        let loop_id = self._loop_id()?;
        let opcode = match loop_id {
            0 => Opcode::start_loop_0,
            1 => Opcode::start_loop_1,
            2 => Opcode::start_loop_2,
            MAX_NESTED_LOOPS.. => panic!("Invalid loop_id"),
        };

        let loop_count = loop_count_u8(loop_count)?;

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

    pub fn end_loop(&mut self) -> Result<(), BytecodeError> {
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

            let loop_count = u64::from(loop_state.loop_count);

            self.tick_counter +=
                TickCounter::new(ticks_in_loop * loop_count - ticks_skipped_in_last_loop);
        }

        if let Some(skip_last_loop) = loop_state.skip_last_loop {
            // Write the parameter for the skip_last_loop instruction
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

        emit_bytecode!(self, Opcode::call_subroutine, subroutine.id);
        Ok(())
    }

    pub fn return_from_subroutine(&mut self) -> Result<(), BytecodeError> {
        if self.is_subroutine {
            return Err(BytecodeError::ReturnInNonSubroutine);
        }

        emit_bytecode!(self, Opcode::return_from_subroutine);
        Ok(())
    }

    pub fn end(&mut self) {
        emit_bytecode!(self, Opcode::end);
    }

    pub fn disable_channel(&mut self) {
        emit_bytecode!(self, Opcode::disable_channel);
    }

    pub fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), BytecodeError> {
        if self.is_sound_effect {
            return Err(BytecodeError::CannotChangeTickClockInASoundEffect);
        }

        emit_bytecode!(
            self,
            Opcode::set_song_tick_clock,
            tick_clock.register_value()
        );
        Ok(())
    }
}
