//! bytecode interpreter

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::opcodes;
use crate::bytecode::Pan;
use crate::channel_bc_generator::MmlInstrument;
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{
    addresses, LoaderDataType, BC_CHANNEL_STACK_OFFSET, BC_CHANNEL_STACK_SIZE,
    BC_STACK_BYTES_PER_LOOP, COMMON_DATA_BYTES_PER_INSTRUMENT, N_MUSIC_CHANNELS,
    SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, STARTING_VOLUME, S_DSP_EON_REGISTER,
    S_SMP_TIMER_0_REGISTER,
};
use crate::mml::MmlPrefixData;
use crate::songs::Channel as SongChannel;
use crate::songs::SongData;
use crate::songs::Subroutine;
use crate::time::TickClock;
use crate::time::TickCounter;

use std::cmp::min;
use std::ops::Deref;

pub const UNINITIALISED: u8 = 0xaa;

const MAX_PAN: u8 = Pan::MAX.as_u8();
const PITCH_MOD_MASK: u8 = 0b00111110;

/// Error advancing subroutine to the end of the pointer
#[derive(Debug)]
pub struct SongSubroutineError;

#[derive(Clone)]
struct VirtualChannel {
    vol_l: u8,
    vol_r: u8,
    // pitch is not fully emulated
    pitch_l: u8,
    pitch_h: u8,
    scrn: u8,
    adsr1: u8,
    adsr2_or_gain: u8,
    temp_gain: u8,

    /// Only set if the channel is a PMON source.
    key_on: bool,

    echo: bool,
    pitch_mod: bool,
}

#[derive(Clone)]
struct ChannelSoAPanVol {
    value: u8,
    sub_value: u8,
    direction: u8,
    offset_l: u8,
    offset_h: u8,
    counter: u8,
    half_wavelength: u8,
}

#[derive(Clone)]
struct ChannelSoA {
    countdown_timer: u8,
    next_event_is_key_off: u8,

    instruction_ptr: u16,

    stack_pointer: u8,
    loop_stack_pointer: u8,

    inst_pitch_offset: u8,

    volume: ChannelSoAPanVol,
    pan: ChannelSoAPanVol,

    // Not emulating portamento

    // Not accurate but since no notes are playing when the GUI starts playing this
    // InterpreterOutput it will not be audible at all.
    vibrato_tick_counter: u8,

    vibrato_pitch_offset_per_tick: u8,
    vibrato_tick_counter_start: u8,
    vibrato_half_wavelength: u8,

    prev_temp_gain: u8,

    early_release_cmp: u8,
    early_release_min_ticks: u8,
    early_release_gain: u8,

    detune_l: u8,
    detune_h: u8,
}

#[derive(Clone)]
struct Channel {
    soa: ChannelSoA,
    bc_stack: [u8; BC_CHANNEL_STACK_SIZE],
    dsp: VirtualChannel,
}

struct InterpreterOutput {
    channels: [Channel; N_MUSIC_CHANNELS],
    song_data_addr: u16,
    stereo_flag: bool,
    song_tick_counter: u16,
    tick_clock: u8,
}

struct GlobalState {
    timer_register: u8,
}

impl GlobalState {
    fn new(tick_clock: TickClock) -> Self {
        Self {
            timer_register: tick_clock.as_u8(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PanVolEffectDirection {
    None = 0,
    SlideUp = 0x80,
    SlideDown = 0x81,
    TriangleUp = 0x40,
    TriangleDown = 0x41,
}

#[derive(Debug, Clone)]
struct PanVolValue<const MAX: u8> {
    tc: TickCounter,

    value: u8,
    sub_value: u8,
    counter: u8,
    direction: PanVolEffectDirection,
    half_wavelength: u8,

    offset: u32,

    triangle_starting_value: u8,
}

impl<const M: u8> PanVolValue<M> {
    const MAX: u8 = M;
    const MAX_U32: u32 = ((Self::MAX as u32) << 8) | 0xff;

    fn new(value: u8) -> Self {
        Self {
            tc: TickCounter::new(0),
            value,
            sub_value: UNINITIALISED,
            counter: UNINITIALISED,
            direction: PanVolEffectDirection::None,
            offset: u32::from_le_bytes([UNINITIALISED, UNINITIALISED, 0, 0]),
            half_wavelength: UNINITIALISED,
            triangle_starting_value: UNINITIALISED,
        }
    }

    fn as_soa(&self) -> ChannelSoAPanVol {
        ChannelSoAPanVol {
            value: self.value,
            sub_value: self.sub_value,
            direction: self.direction as u8,
            offset_l: self.offset.to_le_bytes()[0],
            offset_h: self.offset.to_le_bytes()[1],
            counter: self.counter,
            half_wavelength: self.half_wavelength,
        }
    }

    pub(self) fn u8_0is256_to_tick_counter(t: u8) -> TickCounter {
        match t {
            0 => TickCounter::new(0x100),
            t => TickCounter::new(u32::from(t)),
        }
    }

    pub(self) fn slide_offset(&self, channel_ticks: TickCounter) -> (u8, u32) {
        debug_assert!(self.tc < channel_ticks);

        let slide_ticks = Self::u8_0is256_to_tick_counter(self.counter).value();

        let elapsed = (channel_ticks.value() - self.tc.value()).min(slide_ticks);
        let offset = self.offset * elapsed;

        ((slide_ticks - elapsed).try_into().unwrap(), offset)
    }

    fn update(&mut self, channel_ticks: TickCounter) {
        if self.tc == channel_ticks {
            return;
        }

        match self.direction {
            PanVolEffectDirection::None => (),

            PanVolEffectDirection::SlideUp => {
                let value = u32::from_le_bytes([self.sub_value, self.value, 0, 0]);

                let (counter, offset) = self.slide_offset(channel_ticks);

                let value = value.wrapping_add(offset);

                if value <= Self::MAX_U32 {
                    self.value = value.to_le_bytes()[1];
                    self.sub_value = value.to_le_bytes()[0];
                } else {
                    self.value = Self::MAX;
                    self.direction = PanVolEffectDirection::None;
                }

                self.counter = counter;
                if counter == 0 {
                    self.direction = PanVolEffectDirection::None;
                }

                self.tc = channel_ticks;
            }

            PanVolEffectDirection::SlideDown => {
                let value = u32::from_le_bytes([self.sub_value, self.value, 0, 0]);

                let (counter, offset) = self.slide_offset(channel_ticks);

                let value = value.wrapping_sub(offset);

                self.sub_value = value.to_le_bytes()[0];

                if value <= Self::MAX_U32 {
                    self.value = value.to_le_bytes()[1];
                    self.sub_value = value.to_le_bytes()[0];
                } else {
                    self.value = 0;
                    self.direction = PanVolEffectDirection::None;
                }

                self.counter = counter;
                if counter == 0 {
                    self.direction = PanVolEffectDirection::None;
                }

                self.tc = channel_ticks;
            }

            PanVolEffectDirection::TriangleUp | PanVolEffectDirection::TriangleDown => {
                self.process_triangle(channel_ticks)
            }
        }
    }

    const TRIANGLE_SUB_START: u8 = u8::MAX / 2;

    pub(self) fn process_triangle(&mut self, channel_ticks: TickCounter) {
        let starting_value =
            u32::from_le_bytes([Self::TRIANGLE_SUB_START, self.triangle_starting_value, 0, 0]);

        let wavelength = u32::from(self.half_wavelength) * 2;
        let quarter_wavelength = wavelength / 4;

        let elapsed = channel_ticks.value() - self.tc.value();

        let position = elapsed % wavelength;
        let quadrant = position / quarter_wavelength;

        if elapsed >= wavelength || quadrant > 0 {
            // Test for overflow
            if starting_value + (quarter_wavelength * self.offset) > Self::MAX_U32 {
                self.value = Self::MAX;
                self.direction = PanVolEffectDirection::None;
                return;
            }
        }

        if elapsed >= wavelength || quadrant > 2 {
            // Test for underflow
            if quarter_wavelength * self.offset > starting_value {
                self.value = 0;
                self.direction = PanVolEffectDirection::None;
                return;
            }
        }

        let (value, direction) = match quadrant {
            0 => (
                starting_value.wrapping_add(position * self.offset),
                PanVolEffectDirection::TriangleUp,
            ),
            1 => {
                let p = u32::from(self.half_wavelength) - position;
                (
                    starting_value.wrapping_add(p * self.offset),
                    PanVolEffectDirection::TriangleDown,
                )
            }
            2 => {
                let p = position - u32::from(self.half_wavelength);
                (
                    starting_value.wrapping_sub(p * self.offset),
                    PanVolEffectDirection::TriangleDown,
                )
            }
            3 => {
                let p = wavelength - position;
                (
                    starting_value.wrapping_sub(p * self.offset),
                    PanVolEffectDirection::TriangleUp,
                )
            }
            _ => panic!("Wrong quadrant"),
        };

        if value <= Self::MAX_U32 {
            let half_wavelength = u32::from(self.half_wavelength);

            self.value = value.to_le_bytes()[1];
            self.sub_value = value.to_le_bytes()[0];
            self.counter = (half_wavelength - (position + quarter_wavelength) % half_wavelength)
                .to_le_bytes()[0];
            self.direction = direction;
        } else {
            self.value = if quadrant < 2 { Self::MAX } else { 0 };
            self.direction = PanVolEffectDirection::None;
        }
    }

    fn set_value(&mut self, value: u8) {
        self.direction = PanVolEffectDirection::None;
        self.value = value;
    }

    fn adjust_value(&mut self, amount: i8, tc: TickCounter) {
        self.update(tc);

        self.direction = PanVolEffectDirection::None;
        self.value = self.value.saturating_add_signed(amount).clamp(0, Self::MAX);
    }

    fn slide_up_instruction(&mut self, ticks: u8, o1: u8, o2: u8, tc: TickCounter) {
        self.update(tc);

        self.tc = tc;
        self.counter = ticks;
        self.half_wavelength = 0;
        self.direction = PanVolEffectDirection::SlideUp;
        self.offset = u32::from_le_bytes([o1, o2, 0, 0]);
        self.sub_value = 0;
    }

    fn slide_down_instruction(&mut self, ticks: u8, o1: u8, o2: u8, tc: TickCounter) {
        self.update(tc);

        self.tc = tc;
        self.counter = ticks;
        self.half_wavelength = 0;
        self.direction = PanVolEffectDirection::SlideDown;
        self.offset = u32::from_le_bytes([o1, o2, 0, 0]);
        self.sub_value = u8::MAX;
    }

    fn tremolo_panbrello_instruction(&mut self, qwt: u8, o1: u8, o2: u8, tc: TickCounter) {
        self.update(tc);

        self.tc = tc;
        self.counter = qwt;
        self.half_wavelength = qwt.wrapping_mul(2);
        self.direction = PanVolEffectDirection::TriangleUp;
        self.offset = u32::from_le_bytes([o1, o2, 0, 0]);
        self.sub_value = Self::TRIANGLE_SUB_START;

        self.triangle_starting_value = self.value;
    }
}

#[derive(Debug)]
enum ChannelNote {
    None,
    PlayNote {
        note_opcode: u8,
        instrument: Option<u8>,
        detune: i16,
    },
    PlayPitch(u16),

    // Not emulating pitch-effects
    // It's too much work (effort and CPU time) for very little gain.
    Portamento {
        target_opcode: u8,
        instrument: Option<u8>,
        detune: i16,
    },
}

#[derive(Debug)]
pub struct ChannelState {
    ticks: TickCounter,
    disabled: bool,

    song_ptr: u16,

    pub instruction_ptr: u16,

    /// The return position (with SongData) of the topmost subroutine call.
    pub topmost_return_pos: Option<u16>,

    call_stack_depth: u8,

    // Stack pointer
    // Grows downwards (from CHANNEL_STACK_SIZE to 0)
    // MUST always be <= `CHANNEL_STACK_SIZE`
    stack_pointer: usize,

    // Stack pointer to use in the SKIP_LAST_LOOP and END_LOOP instructions.
    // Used to remove bounds checking when reading/modifying loop counter.
    // MUST always be <= `CHANNEL_STACK_SIZE - STACK_BYTES_PER_LOOP`
    loop_stack_pointer: usize,

    bc_stack: [u8; BC_CHANNEL_STACK_SIZE],

    instrument: Option<u8>,
    adsr_or_gain_override: Option<(u8, u8)>,
    temp_gain: u8,
    prev_temp_gain: u8,
    early_release_cmp: u8,
    early_release_min_ticks: u8,
    early_release_gain: u8,

    detune: i16,

    next_event_is_key_off: bool,
    note: ChannelNote,
    note_time: TickCounter,

    volume: PanVolValue<0xff>,
    pan: PanVolValue<MAX_PAN>,

    echo: bool,
    pitch_mod: bool,

    // Not emulating pitch
    // Not emulating portamento

    // Partially emulating vibrato
    vibrato_pitch_offset_per_tick: u8,
    vibrato_quarter_wavelength_in_ticks: Option<u8>,
}

impl ChannelState {
    fn new(channel: Option<&SongChannel>, song_ptr: u16) -> Self {
        Self {
            ticks: TickCounter::new(0),
            disabled: false,
            song_ptr,
            instruction_ptr: channel.map(|c| c.bytecode_offset).unwrap_or(u16::MAX),
            topmost_return_pos: None,
            call_stack_depth: 0,
            stack_pointer: BC_CHANNEL_STACK_SIZE,
            loop_stack_pointer: BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP,
            bc_stack: [UNINITIALISED; BC_CHANNEL_STACK_SIZE],
            instrument: None,
            next_event_is_key_off: false,
            note: ChannelNote::None,
            note_time: TickCounter::new(0),
            adsr_or_gain_override: Some((0, 0)),
            temp_gain: 0,
            prev_temp_gain: UNINITIALISED,
            early_release_cmp: 0,
            early_release_min_ticks: UNINITIALISED,
            early_release_gain: UNINITIALISED,
            detune: 0,
            volume: PanVolValue::new(STARTING_VOLUME),
            pan: PanVolValue::new(Pan::CENTER.as_u8()),
            echo: false,
            pitch_mod: false,
            vibrato_pitch_offset_per_tick: 0,
            vibrato_quarter_wavelength_in_ticks: None,
        }
    }

    fn read_subroutine_instruction_ptr(s_id: u8, song_data: &[u8]) -> u16 {
        let n_subroutines = song_data[SONG_HEADER_N_SUBROUTINES_OFFSET];

        let li = usize::from(s_id) + SONG_HEADER_SIZE;
        let hi = li + usize::from(n_subroutines);

        let l = song_data.get(li).copied().unwrap_or(0xff);
        let h = song_data.get(hi).copied().unwrap_or(0xff);

        u16::from_le_bytes([l, h])
    }

    fn increment_tick_count(&mut self, length: u8, key_off: bool) {
        self.next_event_is_key_off = key_off;

        if length > 0 {
            self.ticks += TickCounter::new(u32::from(length) + u32::from(key_off));
        } else {
            self.ticks += TickCounter::new(0x100 + u32::from(key_off));
        }
    }

    fn disable_channel(&mut self) {
        self.disabled = true;
        self.instruction_ptr = u16::MAX;
        self.ticks = TickCounter::MAX;

        self.vibrato_pitch_offset_per_tick = 0;
    }

    fn play_note(&mut self, note_and_key_off_bit: u8, length: u8) {
        let key_off = note_and_key_off_bit & 1 == 1;

        self.note_time = self.ticks;
        self.increment_tick_count(length, key_off);
    }

    fn call_subroutine(&mut self, s_id: u8, song_data: &[u8]) {
        self.call_stack_depth += 1;
        if self.call_stack_depth == 1 {
            self.topmost_return_pos = Some(self.instruction_ptr);
        }

        match self.stack_pointer.checked_sub(2) {
            Some(sp) => {
                self.stack_pointer = sp;

                let inst_ptr = self.instruction_ptr + self.song_ptr;
                let inst_ptr = inst_ptr.to_le_bytes();

                self.bc_stack[sp] = inst_ptr[0];
                self.bc_stack[sp + 1] = inst_ptr[1];

                self.instruction_ptr = Self::read_subroutine_instruction_ptr(s_id, song_data);
            }
            None => self.disable_channel(),
        }
    }

    fn return_from_subroutine(&mut self) {
        self.call_stack_depth = self.call_stack_depth.saturating_sub(1);
        if self.call_stack_depth == 0 {
            self.topmost_return_pos = None;
        }

        let sp = self.stack_pointer;

        if sp <= BC_CHANNEL_STACK_SIZE - 2 {
            self.stack_pointer = sp + 2;

            if self.stack_pointer <= BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP {
                self.loop_stack_pointer = self.stack_pointer;
            }

            let inst_ptr = u16::from_le_bytes([self.bc_stack[sp], self.bc_stack[sp + 1]]);

            match inst_ptr.checked_sub(self.song_ptr) {
                Some(i) => self.instruction_ptr = i,
                None => self.disable_channel(),
            }
        } else {
            self.disable_channel();
        }
    }

    fn process_next_bytecode(&mut self, global: &mut GlobalState, song_data: &[u8]) {
        if self.next_event_is_key_off {
            self.note = ChannelNote::None;

            // Temp gain is reset on key-off
            self.temp_gain = 0;

            self.next_event_is_key_off = false;
        }

        let mut read_pc = || match song_data.get(usize::from(self.instruction_ptr)) {
            Some(b) => {
                self.instruction_ptr += 1;
                *b
            }
            None => {
                self.disable_channel();
                opcodes::DISABLE_CHANNEL
            }
        };

        let opcode: u8 = read_pc();

        match opcode {
            note_opcode @ opcodes::FIRST_PLAY_NOTE_INSTRUCTION.. => {
                let length = read_pc();
                self.note = ChannelNote::PlayNote {
                    note_opcode,
                    instrument: self.instrument,
                    detune: self.detune,
                };
                self.play_note(opcode, length);
            }

            opcodes::PORTAMENTO_DOWN | opcodes::PORTAMENTO_UP => {
                let note_and_key_off_bit = read_pc();
                let _portamento_speed = read_pc();
                let wait_length = read_pc();

                // Ignoring portamento speed and direction
                self.note = ChannelNote::Portamento {
                    target_opcode: note_and_key_off_bit,
                    instrument: self.instrument,
                    detune: self.detune,
                };

                self.play_note(note_and_key_off_bit, wait_length);
            }

            opcodes::SET_VIBRATO => {
                let depth = read_pc();
                let wavelength = read_pc();

                self.vibrato_pitch_offset_per_tick = depth;
                self.vibrato_quarter_wavelength_in_ticks = Some(wavelength);
            }
            opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE => {
                let depth = read_pc();
                let note_opcode = read_pc();
                let length = read_pc();

                self.note = ChannelNote::PlayNote {
                    note_opcode,
                    instrument: self.instrument,
                    detune: self.detune,
                };

                self.vibrato_pitch_offset_per_tick = depth;
                self.play_note(note_opcode, length);
            }

            opcodes::WAIT => {
                let to_rest = read_pc();
                self.increment_tick_count(to_rest, false);
            }
            opcodes::REST => {
                let to_rest = read_pc();
                self.increment_tick_count(to_rest, true);
            }

            opcodes::PLAY_PITCH => {
                let pitch_l = read_pc();
                let pitch_h_and_keyoff = read_pc();
                let length = read_pc();

                let key_off = (pitch_h_and_keyoff & 1) == 1;

                self.note =
                    ChannelNote::PlayPitch(u16::from_le_bytes([pitch_l, pitch_h_and_keyoff >> 1]));

                self.increment_tick_count(length, key_off);
            }

            opcodes::PLAY_NOISE => {
                let freq_and_keyoff = read_pc();
                let length = read_pc();

                let key_off = (freq_and_keyoff & 1) == 1;

                self.increment_tick_count(length, key_off);
            }

            opcodes::DISABLE_NOISE => {}

            opcodes::SET_INSTRUMENT => {
                self.instrument = Some(read_pc());
                self.adsr_or_gain_override = None;
                self.temp_gain = 0;
            }
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN => {
                let instrument = read_pc();
                let adsr1 = read_pc();
                let adsr2_or_gain = read_pc();

                self.instrument = Some(instrument);
                self.adsr_or_gain_override = Some((adsr1, adsr2_or_gain));
                self.temp_gain = 0;
            }
            opcodes::SET_ADSR => {
                let adsr1 = read_pc();
                let adsr2 = read_pc();

                self.adsr_or_gain_override = Some((adsr1, adsr2));
                self.temp_gain = 0;
            }
            opcodes::SET_GAIN => {
                let gain = read_pc();

                self.adsr_or_gain_override = Some((0, gain));
                self.temp_gain = 0;
            }

            opcodes::SET_TEMP_GAIN => {
                let temp_gain = read_pc();

                self.temp_gain = temp_gain;
                self.prev_temp_gain = temp_gain;
            }

            opcodes::SET_TEMP_GAIN_AND_REST => {
                let temp_gain = read_pc();
                let to_rest = read_pc();

                self.temp_gain = temp_gain;
                self.prev_temp_gain = temp_gain;
                self.increment_tick_count(to_rest, true);
            }

            opcodes::SET_TEMP_GAIN_AND_WAIT => {
                let temp_gain = read_pc();
                let to_rest = read_pc();

                self.temp_gain = temp_gain;
                self.prev_temp_gain = temp_gain;
                self.increment_tick_count(to_rest, false);
            }

            opcodes::REUSE_TEMP_GAIN => {
                self.temp_gain = self.prev_temp_gain;
            }
            opcodes::REUSE_TEMP_GAIN_AND_REST => {
                let to_rest = read_pc();

                self.temp_gain = self.prev_temp_gain;
                self.increment_tick_count(to_rest, true);
            }
            opcodes::REUSE_TEMP_GAIN_AND_WAIT => {
                let to_rest = read_pc();

                self.temp_gain = self.prev_temp_gain;
                self.increment_tick_count(to_rest, false);
            }

            opcodes::SET_EARLY_RELEASE => {
                let cmp = read_pc();
                let min = read_pc();
                let gain = read_pc();

                self.early_release_cmp = cmp;
                self.early_release_min_ticks = min;
                self.early_release_gain = gain;
            }

            opcodes::SET_EARLY_RELEASE_NO_MINIMUM => {
                let cmp = read_pc();
                let gain = read_pc();

                self.early_release_cmp = cmp;
                self.early_release_min_ticks = 0;
                self.early_release_gain = gain;
            }

            opcodes::SET_DETUNE_I16 => {
                let l = read_pc();
                let h = read_pc();

                self.detune = i16::from_le_bytes([l, h]);
            }
            opcodes::SET_DETUNE_P8 => {
                let l = read_pc();

                self.detune = i16::from_le_bytes([l, 0x00]);
            }
            opcodes::SET_DETUNE_N8 => {
                let l = read_pc();

                self.detune = i16::from_le_bytes([l, 0xff]);
            }

            opcodes::ADJUST_PAN => {
                let p = i8::from_le_bytes([read_pc()]);

                self.pan.adjust_value(p, self.ticks);
            }
            opcodes::SET_PAN => {
                let pan = read_pc();

                self.pan.set_value(pan);
            }
            opcodes::SET_PAN_AND_VOLUME => {
                let pan = read_pc();
                let volume = read_pc();

                self.pan.set_value(pan);
                self.volume.set_value(volume);
            }
            opcodes::ADJUST_VOLUME => {
                let v = i8::from_le_bytes([read_pc()]);

                self.volume.adjust_value(v, self.ticks);
            }
            opcodes::SET_VOLUME => {
                let volume = read_pc();

                self.volume.set_value(volume);
            }

            opcodes::VOLUME_SLIDE_UP => {
                let ticks = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.volume.slide_up_instruction(ticks, o1, o2, self.ticks);
            }

            opcodes::VOLUME_SLIDE_DOWN => {
                let ticks = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.volume
                    .slide_down_instruction(ticks, o1, o2, self.ticks);
            }

            opcodes::TREMOLO => {
                let qwt = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.volume
                    .tremolo_panbrello_instruction(qwt, o1, o2, self.ticks);
            }

            opcodes::PAN_SLIDE_UP => {
                let ticks = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.pan.slide_up_instruction(ticks, o1, o2, self.ticks);
            }

            opcodes::PAN_SLIDE_DOWN => {
                let ticks = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.pan.slide_down_instruction(ticks, o1, o2, self.ticks);
            }

            opcodes::PANBRELLO => {
                let qwt = read_pc();
                let o1 = read_pc();
                let o2 = read_pc();

                self.pan
                    .tremolo_panbrello_instruction(qwt, o1, o2, self.ticks);
            }

            opcodes::SET_SONG_TICK_CLOCK => {
                let timer = read_pc();

                global.timer_register = timer;
            }

            opcodes::GOTO_RELATIVE => {
                let l = read_pc();
                let h = read_pc();

                if !self.disabled {
                    // undo `h = read_pc()`.
                    self.instruction_ptr -= 1;

                    let offset = i16::from_le_bytes([l, h]);

                    match self.instruction_ptr.checked_add_signed(offset) {
                        Some(i) => self.instruction_ptr = i,
                        None => self.disable_channel(),
                    }
                }
            }

            opcodes::START_LOOP => {
                let counter = read_pc();

                match self.stack_pointer.checked_sub(3) {
                    Some(sp) => {
                        self.stack_pointer = sp;
                        self.loop_stack_pointer = sp;

                        let inst_ptr = self.song_ptr + self.instruction_ptr;
                        let inst_ptr = inst_ptr.to_le_bytes();

                        self.bc_stack[sp] = counter;
                        self.bc_stack[sp + 1] = inst_ptr[0];
                        self.bc_stack[sp + 2] = inst_ptr[1];
                    }
                    None => self.disable_channel(),
                }
            }
            opcodes::SKIP_LAST_LOOP => {
                let bytes_to_skip = read_pc();

                // No bounds testing required when reading counter
                let sp = self.loop_stack_pointer;

                let counter = self.bc_stack[sp];
                if counter == 1 {
                    self.instruction_ptr += u16::from(bytes_to_skip);

                    let sp = sp + 3;
                    self.stack_pointer = sp;
                    if sp <= BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP {
                        self.loop_stack_pointer = sp;
                    }
                }
            }
            opcodes::END_LOOP => {
                // No bounds testing required when modifying counter
                let sp = self.loop_stack_pointer;

                let counter = self.bc_stack[sp].wrapping_sub(1);

                if counter != 0 {
                    self.bc_stack[sp] = counter;
                    let inst_ptr =
                        u16::from_le_bytes([self.bc_stack[sp + 1], self.bc_stack[sp + 2]]);
                    match inst_ptr.checked_sub(self.song_ptr) {
                        Some(i) => self.instruction_ptr = i,
                        None => self.disable_channel(),
                    }
                } else {
                    let sp = sp + 3;
                    self.stack_pointer = sp;
                    if sp <= BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP {
                        self.loop_stack_pointer = sp;
                    }
                }
            }

            opcodes::CALL_SUBROUTINE_AND_DISABLE_VIBRATO => {
                let s_id = read_pc();

                self.vibrato_pitch_offset_per_tick = 0;
                self.call_subroutine(s_id, song_data);
            }
            opcodes::CALL_SUBROUTINE => {
                let s_id = read_pc();

                self.call_subroutine(s_id, song_data);
            }

            opcodes::RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO => {
                self.vibrato_pitch_offset_per_tick = 0;
                self.return_from_subroutine();
            }
            opcodes::RETURN_FROM_SUBROUTINE => {
                self.return_from_subroutine();
            }

            opcodes::ENABLE_ECHO => self.echo = true,
            opcodes::DISABLE_ECHO => self.echo = false,

            opcodes::ENABLE_PMOD => self.pitch_mod = true,
            opcodes::DISABLE_PMOD => self.pitch_mod = false,

            opcodes::DISABLE_CHANNEL => self.disable_channel(),

            _ => self.disable_channel(),
        }
    }

    /// MUST be called after processing the channel's bytecode
    fn finalise(&mut self, target_ticks: TickCounter) {
        self.volume.update(target_ticks);
        self.pan.update(target_ticks);
    }

    // Create a new song subroutine interpreter channel and process a mml-prefix.
    fn subroutine_prefix(
        song_ptr: u16,
        inst: &MmlInstrument,
        prefix: Option<MmlPrefixData>,
        sub: &Subroutine,
        global: &mut GlobalState,
    ) -> Option<Self> {
        let mut c = Self::new(None, song_ptr);

        c.instrument = Some(inst.instrument_id.as_u8());
        c.adsr_or_gain_override = Some(inst.envelope.engine_value());

        if let Some(prefix) = prefix {
            // Prevent infinite loops by limiting the number of processed instructions
            let mut watchdog_counter: u32 = 8_000;

            c.instruction_ptr = 0;

            while !c.disabled {
                c.process_next_bytecode(global, prefix.bytecode());

                watchdog_counter -= 1;
                if watchdog_counter == 0 {
                    return None;
                }
            }
        }

        c.finalise(c.ticks);

        c.instruction_ptr = sub.bytecode_offset;
        c.disabled = false;
        c.ticks = TickCounter::new(0);

        Some(c)
    }

    /// Returns instruction_size if instruction does not sleep, branch or disable the channel.
    fn instruction_size_if_not_sleep_nor_branch(opcode: u8) -> Option<usize> {
        match opcode {
            opcodes::FIRST_PLAY_NOTE_INSTRUCTION.. => None,
            opcodes::PORTAMENTO_DOWN | opcodes::PORTAMENTO_UP => None,
            opcodes::SET_VIBRATO => Some(3),
            opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE => None,
            opcodes::WAIT => None,
            opcodes::REST => None,
            opcodes::PLAY_PITCH => None,
            opcodes::PLAY_NOISE => None,
            opcodes::DISABLE_NOISE => Some(1),
            opcodes::SET_INSTRUMENT => Some(2),
            opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN => Some(4),
            opcodes::SET_ADSR => Some(3),
            opcodes::SET_GAIN => Some(2),
            opcodes::SET_TEMP_GAIN => Some(2),
            opcodes::SET_TEMP_GAIN_AND_REST => None,
            opcodes::SET_TEMP_GAIN_AND_WAIT => None,
            opcodes::REUSE_TEMP_GAIN => Some(1),
            opcodes::REUSE_TEMP_GAIN_AND_REST => None,
            opcodes::REUSE_TEMP_GAIN_AND_WAIT => None,
            opcodes::SET_EARLY_RELEASE => Some(4),
            opcodes::SET_EARLY_RELEASE_NO_MINIMUM => Some(3),
            opcodes::SET_DETUNE_I16 => Some(3),
            opcodes::SET_DETUNE_P8 => Some(2),
            opcodes::SET_DETUNE_N8 => Some(2),
            opcodes::ADJUST_PAN => Some(2),
            opcodes::SET_PAN => Some(2),
            opcodes::SET_PAN_AND_VOLUME => Some(3),
            opcodes::ADJUST_VOLUME => Some(2),
            opcodes::SET_VOLUME => Some(2),
            opcodes::VOLUME_SLIDE_UP => Some(4),
            opcodes::VOLUME_SLIDE_DOWN => Some(4),
            opcodes::TREMOLO => Some(4),
            opcodes::PAN_SLIDE_UP => Some(4),
            opcodes::PAN_SLIDE_DOWN => Some(4),
            opcodes::PANBRELLO => Some(4),
            opcodes::SET_SONG_TICK_CLOCK => Some(2),
            opcodes::GOTO_RELATIVE => None,
            opcodes::START_LOOP => Some(2),
            opcodes::SKIP_LAST_LOOP => Some(2),
            opcodes::END_LOOP => None,
            opcodes::CALL_SUBROUTINE_AND_DISABLE_VIBRATO => None,
            opcodes::CALL_SUBROUTINE => None,
            opcodes::RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO => None,
            opcodes::ENABLE_ECHO => Some(1),
            opcodes::DISABLE_ECHO => Some(1),
            opcodes::ENABLE_PMOD => Some(1),
            opcodes::DISABLE_PMOD => Some(1),
            opcodes::DISABLE_CHANNEL => None,

            _ => None,
        }
    }

    fn is_pitch_mod(&self, song_data: &[u8]) -> bool {
        let mut instruction_ptr: usize = self.instruction_ptr.into();

        loop {
            let next_instruction = song_data.get(instruction_ptr).copied();

            match next_instruction {
                Some(opcodes::ENABLE_PMOD) => return true,
                Some(opcodes::DISABLE_PMOD) => return false,
                Some(opcode) => match Self::instruction_size_if_not_sleep_nor_branch(opcode) {
                    Some(length) => instruction_ptr += length,
                    None => return self.pitch_mod,
                },
                None => return self.pitch_mod,
            }
        }
    }
}

pub struct SongInterpreter<CAD, SD>
where
    CAD: Deref<Target = CommonAudioData>,
    SD: Deref<Target = SongData>,
{
    common_audio_data: CAD,
    song_data: SD,

    global: GlobalState,
    channels: [Option<ChannelState>; N_MUSIC_CHANNELS],
    tick_counter: TickCounter,
    stereo_flag: bool,
}

impl<CAD, SD> SongInterpreter<CAD, SD>
where
    CAD: Deref<Target = CommonAudioData>,
    SD: Deref<Target = SongData>,
{
    pub fn new(common_audio_data: CAD, song_data: SD, stereo_flag: bool) -> Self {
        Self {
            channels: std::array::from_fn(|i| {
                song_data.channels()[i]
                    .as_ref()
                    .map(|c| ChannelState::new(Some(c), common_audio_data.song_data_addr()))
            }),
            tick_counter: TickCounter::default(),
            global: GlobalState::new(song_data.metadata().tick_clock),
            stereo_flag,
            song_data,
            common_audio_data,
        }
    }

    pub fn new_song_subroutine(
        common_audio_data: CAD,
        song_data: SD,
        prefix: Option<MmlPrefixData>,
        subroutine_index: u8,
        stereo_flag: bool,
    ) -> Result<Self, SongSubroutineError> {
        let mut out = Self {
            channels: Default::default(),
            tick_counter: TickCounter::default(),
            global: GlobalState::new(song_data.metadata().tick_clock),
            stereo_flag,
            song_data,
            common_audio_data,
        };

        let sub = match out
            .song_data
            .subroutines()
            .get(usize::from(subroutine_index))
        {
            Some(s) => s,
            None => return Err(SongSubroutineError),
        };

        // Subroutine might not set an instrument before the play_note instructions.
        //
        // Find the first instrument that can play all notes in the subroutine.
        // If no instrument can be found, use the first instrument in the MML file.
        let inst = {
            let instruments = out.song_data.instruments();
            let notes = sub.subroutine_id.no_instrument_notes();

            let i = match notes.is_empty() {
                true => instruments.first(),
                false => instruments.iter().find(|i| {
                    i.note_range.contains(notes.start()) && i.note_range.contains(notes.end())
                }),
            };

            match i {
                Some(i) => i,
                None => return Err(SongSubroutineError),
            }
        };

        out.channels[0] = ChannelState::subroutine_prefix(
            out.common_audio_data.song_data_addr(),
            inst,
            prefix,
            sub,
            &mut out.global,
        );

        Ok(out)
    }

    pub fn channels(&self) -> &[Option<ChannelState>; N_MUSIC_CHANNELS] {
        &self.channels
    }

    /// Return the channel with the smallest tick-counter and the tick-counter to execute to
    fn next_channel_to_process(
        channels: &mut [Option<ChannelState>; N_MUSIC_CHANNELS],
        target_ticks: TickCounter,
    ) -> Option<(&mut ChannelState, TickCounter)> {
        // ::TODO optimise (profile before and after)::

        let mut iter = channels.iter_mut().flatten();

        let mut smallest = iter.next()?;
        let mut second_smallest_ticks = TickCounter::new(u32::MAX - 1);

        if let Some(c) = iter.next() {
            second_smallest_ticks = c.ticks;
            if c.ticks < smallest.ticks {
                second_smallest_ticks = smallest.ticks;
                smallest = c;
            }
        }

        for c in iter {
            if c.ticks < smallest.ticks {
                second_smallest_ticks = smallest.ticks;
                smallest = c;
            }
        }

        if smallest.ticks < target_ticks {
            Some((
                smallest,
                min(second_smallest_ticks + TickCounter::new(1), target_ticks),
            ))
        } else {
            None
        }
    }

    /// Returns false if there was a timeout
    pub fn process_ticks(&mut self, ticks: TickCounter) -> bool {
        // Prevent infinite loops by limiting the number of processed instructions
        let mut watchdog_counter: u32 = 2_000_000;

        let song_data = self.song_data.data();

        let target_ticks = self.tick_counter + ticks;

        while let Some((c, next_channel_ticks)) =
            Self::next_channel_to_process(&mut self.channels, target_ticks)
        {
            debug_assert!(next_channel_ticks <= target_ticks);

            while c.ticks < next_channel_ticks {
                c.process_next_bytecode(&mut self.global, song_data);

                watchdog_counter -= 1;
                if watchdog_counter == 0 {
                    return false;
                }
            }
        }

        for c in self.channels.iter_mut().flatten() {
            c.finalise(target_ticks);
        }

        debug_assert!(self
            .channels
            .iter()
            .flatten()
            .all(|c| c.ticks == TickCounter::MAX || c.ticks >= target_ticks));

        self.tick_counter = target_ticks;

        true
    }

    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn write_to_emulator(&self, emu: &mut impl Emulator) {
        let common = CommonAudioDataSoA::new(&self.common_audio_data, self.stereo_flag);

        let o = InterpreterOutput {
            channels: std::array::from_fn(|i| match &self.channels[i] {
                Some(c) => {
                    let pmon_source = match self.channels.get(i + 1) {
                        Some(Some(c)) => c.is_pitch_mod(self.song_data.data()),
                        _ => false,
                    };

                    build_channel(i, c, self.tick_counter, &common, pmon_source)
                }
                None => unused_channel(i),
            }),
            tick_clock: self.global.timer_register,
            song_tick_counter: (self.tick_counter.value() & 0xffff).try_into().unwrap(),
            song_data_addr: self.common_audio_data.song_data_addr(),
            stereo_flag: self.stereo_flag,
        };

        o.write_to_emulator(emu);
    }
}

struct CommonAudioDataSoA<'a> {
    stereo_flag: bool,

    song_data_addr: u16,
    n_instruments: u8,

    instruments_scrn: &'a [u8],
    instruments_pitch_offset: &'a [u8],
    instruments_adsr1: &'a [u8],
    instruments_adsr2_or_gain: &'a [u8],

    pitch_table_l: &'a [u8],
    pitch_table_h: &'a [u8],
}

impl CommonAudioDataSoA<'_> {
    fn new(c: &CommonAudioData, stereo_flag: bool) -> CommonAudioDataSoA {
        let inst_soa_data = |i| {
            assert!(i < COMMON_DATA_BYTES_PER_INSTRUMENT);

            let n_instruments = c.n_instruments_and_samples();
            let start = n_instruments * i;
            let end = start + n_instruments;

            &c.instruments_soa_data()[start..end]
        };

        let pitch_table = c.pitch_table_data();

        let n_instruments = c.n_instruments_and_samples().try_into().unwrap();

        CommonAudioDataSoA {
            stereo_flag,
            song_data_addr: c.song_data_addr(),
            n_instruments,
            instruments_scrn: inst_soa_data(0),
            instruments_pitch_offset: inst_soa_data(1),
            instruments_adsr1: inst_soa_data(2),
            instruments_adsr2_or_gain: inst_soa_data(3),
            pitch_table_l: pitch_table.0,
            pitch_table_h: pitch_table.1,
        }
    }

    fn pitch_table_entry(&self, note_opcode: u8, instrument: Option<u8>, detune: i16) -> (u8, u8) {
        match instrument {
            Some(i) => {
                let i: usize = self
                    .instruments_pitch_offset
                    .get(usize::from(i))
                    .copied()
                    .unwrap_or(0)
                    .wrapping_add(note_opcode >> 1)
                    .into();

                let pitch = u16::from_le_bytes([
                    self.pitch_table_l.get(i).copied().unwrap_or(0),
                    self.pitch_table_h.get(i).copied().unwrap_or(0),
                ]);

                pitch.wrapping_add_signed(detune).to_le_bytes().into()
            }
            None => (0, 0),
        }
    }
}

fn build_channel(
    channel_index: usize,
    c: &ChannelState,
    target_ticks: TickCounter,
    common: &CommonAudioDataSoA,
    pmon_source: bool,
) -> Channel {
    assert_eq!(c.song_ptr, common.song_data_addr);

    assert!(c.ticks >= target_ticks);
    let delay = match c.disabled {
        true => 0,
        false => c.ticks.value() - target_ticks.value(),
    };

    let (countdown_timer, next_event_is_key_off) = match (c.next_event_is_key_off, delay) {
        (_, 0) => (1, 0),
        (false, 1..=0xfe) => (u8::try_from(delay + 1).unwrap(), 0),
        (false, 0xff) => (0, 0),
        (true, 1..=0xff) => (u8::try_from(delay).unwrap(), 0xff),
        (true, 0x100) => (0, 0xff),
        _ => panic!("Invalid ChannelState.ticks value (delay: {})", delay),
    };

    let (inst_pitch_offset, scrn, inst_adsr_or_gain) = match c.instrument {
        Some(i) => {
            let i: usize = i.clamp(0, common.n_instruments).into();
            (
                common.instruments_pitch_offset[i],
                common.instruments_scrn[i],
                (
                    common.instruments_adsr1[i],
                    common.instruments_adsr2_or_gain[i],
                ),
            )
        }
        None => (UNINITIALISED, 0, (0, 0)),
    };

    let (pitch_l, pitch_h) = match c.note {
        ChannelNote::None => (0, 0),
        ChannelNote::PlayNote {
            instrument: None, ..
        } => (0, 0),
        ChannelNote::PlayNote {
            note_opcode,
            instrument,
            detune,
        } => common.pitch_table_entry(note_opcode, instrument, detune),
        ChannelNote::Portamento {
            target_opcode,
            instrument,
            detune,
        } => common.pitch_table_entry(target_opcode, instrument, detune),
        ChannelNote::PlayPitch(p) => p.to_le_bytes().into(),
    };

    let key_on = if pmon_source {
        match c.note {
            ChannelNote::None => false,
            ChannelNote::PlayNote { .. }
            | ChannelNote::PlayPitch(..)
            | ChannelNote::Portamento { .. } => {
                if c.next_event_is_key_off {
                    delay > 1
                } else {
                    true
                }
            }
        }
    } else {
        false
    };

    let (adsr1, adsr2_or_gain) = match c.adsr_or_gain_override {
        Some((a1, a2)) => (a1, a2),
        None => inst_adsr_or_gain,
    };

    let temp_gain = if delay == 0 && c.next_event_is_key_off {
        0
    } else if c.early_release_gain != 0
        && c.next_event_is_key_off
        && (target_ticks.value() - c.note_time.value()) > u32::from(c.early_release_min_ticks)
        && delay < u32::from(c.early_release_cmp)
    {
        c.early_release_gain
    } else {
        c.temp_gain
    };

    let volume_soa = c.volume.as_soa();
    let pan_soa = c.pan.as_soa();

    assert!(c.stack_pointer <= BC_CHANNEL_STACK_SIZE);
    assert!(c.loop_stack_pointer + BC_STACK_BYTES_PER_LOOP <= BC_CHANNEL_STACK_SIZE);

    let stack_start = BC_CHANNEL_STACK_OFFSET + channel_index * BC_CHANNEL_STACK_SIZE;
    let stack_pointer = u8::try_from(stack_start + c.stack_pointer).unwrap();
    let loop_stack_index = u8::try_from(stack_start + c.loop_stack_pointer).unwrap();

    let volume = volume_soa.value;
    let pan = pan_soa.value;

    let (vibrato_tick_counter_start, vibrato_half_wavelength) =
        match c.vibrato_quarter_wavelength_in_ticks {
            Some(v) => (v, v.wrapping_shl(1)),
            None => (UNINITIALISED, UNINITIALISED),
        };

    assert!(Pan::try_from(pan).is_ok());

    Channel {
        soa: ChannelSoA {
            countdown_timer,
            next_event_is_key_off,

            instruction_ptr: match c.disabled {
                true => 0,
                false => c
                    .instruction_ptr
                    .checked_add(common.song_data_addr)
                    .unwrap_or(0),
            },
            stack_pointer,
            loop_stack_pointer: loop_stack_index,
            inst_pitch_offset,
            volume: volume_soa,
            pan: pan_soa,
            vibrato_pitch_offset_per_tick: c.vibrato_pitch_offset_per_tick,
            vibrato_tick_counter: vibrato_tick_counter_start,
            vibrato_tick_counter_start,
            vibrato_half_wavelength,
            prev_temp_gain: c.prev_temp_gain,
            early_release_cmp: c.early_release_cmp,
            early_release_min_ticks: c.early_release_min_ticks,
            early_release_gain: c.early_release_gain,
            detune_l: c.detune.to_le_bytes()[0],
            detune_h: c.detune.to_le_bytes()[1],
        },
        bc_stack: c.bc_stack,
        dsp: VirtualChannel {
            vol_l: match common.stereo_flag {
                true => (u16::from(volume) * u16::from(Pan::MAX.as_u8() - pan)).to_le_bytes()[1],
                false => volume >> 2,
            },
            vol_r: match common.stereo_flag {
                true => (u16::from(volume) * u16::from(pan)).to_le_bytes()[1],
                false => volume >> 2,
            },
            pitch_l,
            pitch_h,
            scrn,
            adsr1,
            adsr2_or_gain,
            temp_gain,
            key_on,
            echo: c.echo,
            pitch_mod: c.pitch_mod,
        },
    }
}

fn unused_channel(channel_index: usize) -> Channel {
    let stack_pointer =
        u8::try_from((channel_index + 1) * BC_CHANNEL_STACK_SIZE + BC_CHANNEL_STACK_OFFSET)
            .unwrap();

    Channel {
        soa: ChannelSoA {
            countdown_timer: 0,
            next_event_is_key_off: 0,

            instruction_ptr: 0,
            stack_pointer,
            loop_stack_pointer: stack_pointer - BC_STACK_BYTES_PER_LOOP as u8,

            inst_pitch_offset: UNINITIALISED,

            volume: ChannelSoAPanVol {
                value: STARTING_VOLUME,
                sub_value: UNINITIALISED,
                direction: 0,
                offset_l: UNINITIALISED,
                offset_h: UNINITIALISED,
                counter: UNINITIALISED,
                half_wavelength: UNINITIALISED,
            },
            pan: ChannelSoAPanVol {
                value: Pan::CENTER.as_u8(),
                sub_value: UNINITIALISED,
                direction: 0,
                offset_l: UNINITIALISED,
                offset_h: UNINITIALISED,
                counter: UNINITIALISED,
                half_wavelength: UNINITIALISED,
            },
            vibrato_pitch_offset_per_tick: 0,
            vibrato_tick_counter: UNINITIALISED,
            vibrato_tick_counter_start: UNINITIALISED,
            vibrato_half_wavelength: UNINITIALISED,

            prev_temp_gain: UNINITIALISED,

            early_release_cmp: 0,
            // No need to reset `earlyRelease_minTicks`
            // (early release is not active if `earlyRelease_cmp == 0`).
            early_release_min_ticks: UNINITIALISED,
            early_release_gain: UNINITIALISED,

            detune_l: 0,
            detune_h: 0,
        },
        bc_stack: [UNINITIALISED; BC_CHANNEL_STACK_SIZE],

        // Not initialised by `__reset_channel()`, these variables are zeroed by zero-page clear.
        // Does not cause an issue with sound effects as they are only written
        // to the S-DSP when voice channels are marked dirty.
        dsp: VirtualChannel {
            vol_l: STARTING_VOLUME >> 2,
            vol_r: STARTING_VOLUME >> 2,
            pitch_l: 0,
            pitch_h: 0,
            scrn: 0,
            adsr1: 0,
            adsr2_or_gain: 0,
            temp_gain: 0,
            key_on: false,
            echo: false,
            pitch_mod: false,
        },
    }
}

pub trait Emulator {
    fn apuram_mut(&mut self) -> &mut [u8; 0x10000];
    fn write_dsp_register(&mut self, addr: u8, value: u8);
    fn write_smp_register(&mut self, addr: u8, value: u8);
}

/// Writes `InterpreterOutput` to the emulator.
///
/// REQUIRES:
///     * Audio driver loaded into memory
///     * Audio driver initialization has just finished executing
///     * Audio driver is in the paused state
///
/// SAFETY: panics if the audio driver is not the paused state
impl InterpreterOutput {
    fn write_to_emulator(&self, emu: &mut impl Emulator) {
        let key_on_shadow: u8 = self
            .channels
            .iter()
            .enumerate()
            .fold(0, |acc, (i, c)| acc | (u8::from(c.dsp.key_on) << i));

        let pmon_shadow: u8 = self
            .channels
            .iter()
            .enumerate()
            .fold(0, |acc, (i, c)| acc | (u8::from(c.dsp.pitch_mod) << i))
            & PITCH_MOD_MASK;

        let eon_shadow: u8 = self
            .channels
            .iter()
            .enumerate()
            .fold(0, |acc, (i, c)| acc | (u8::from(c.dsp.echo) << i));

        // write to apuram
        {
            let apuram: &mut [u8; 0x10000] = emu.apuram_mut();

            // Verify songPtr is correct
            let get_u16 = |addr| {
                let addr = usize::from(addr);
                u16::from_le_bytes(apuram[addr..addr + 2].try_into().unwrap())
            };
            assert_eq!(
                get_u16(addresses::SONG_PTR),
                self.song_data_addr,
                "songPtr does not match"
            );

            // ::TODO find a way to determine if the audio driver is paused::

            let mut apu_write = |addr: u16, value: u8| {
                apuram[usize::from(addr)] = value;
            };

            let mut apu_write_16 = |addr: u16, value: u16| {
                let value = value.to_le_bytes();
                apu_write(addr, value[0]);
                apu_write(addr + 1, value[1]);
            };

            apu_write_16(addresses::SONG_TICK_COUNTER, self.song_tick_counter);

            apu_write(
                addresses::LOADER_DATA_TYPE,
                LoaderDataType {
                    stereo_flag: self.stereo_flag,
                    play_song: false,
                    skip_echo_buffer_reset: false,
                }
                .driver_value(),
            );

            apu_write(addresses::VOICE_CHANNELS_DIRTY_MUSIC, 0xff);

            apu_write(addresses::KEYON_SHADOW_MUSIC, key_on_shadow);
            apu_write(addresses::PMON_SHADOW, pmon_shadow);
            apu_write(addresses::EON_SHADOW_MUSIC, eon_shadow);

            for (channel_index, c) in self.channels.iter().enumerate() {
                let i = u16::try_from(channel_index).unwrap();
                let vc = &c.dsp;
                let c = &c.soa;

                let mut soa_write_u8 = |addr, value| {
                    apu_write(addr + i, value);
                };

                let mut soa_write_u16 = |addr_l, addr_h, value: u16| {
                    let b = value.to_le_bytes();
                    soa_write_u8(addr_l, b[0]);
                    soa_write_u8(addr_h, b[1]);
                };

                soa_write_u16(
                    addresses::CHANNEL_INSTRUCTION_PTR_L,
                    addresses::CHANNEL_INSTRUCTION_PTR_H,
                    c.instruction_ptr,
                );

                soa_write_u8(addresses::CHANNEL_STACK_POINTER, c.stack_pointer);

                soa_write_u8(addresses::CHANNEL_LOOP_STACK_POINTER, c.loop_stack_pointer);

                soa_write_u8(addresses::CHANNEL_COUNTDOWN_TIMER, c.countdown_timer);
                soa_write_u8(
                    addresses::CHANNEL_NEXT_EVENT_IS_KEY_OFF,
                    c.next_event_is_key_off,
                );

                soa_write_u8(addresses::CHANNEL_INST_PITCH_OFFSET, c.inst_pitch_offset);

                soa_write_u8(addresses::CHANNEL_VOLUME, c.volume.value);
                soa_write_u8(addresses::CHANNEL_SUB_VOLUME, c.volume.sub_value);
                soa_write_u8(addresses::CHANNEL_VOL_EFFECT_DIRECTION, c.volume.direction);
                soa_write_u8(addresses::CHANNEL_VOL_EFFECT_OFFSET_L, c.volume.offset_l);
                soa_write_u8(addresses::CHANNEL_VOL_EFFECT_OFFSET_H, c.volume.offset_h);
                soa_write_u8(addresses::CHANNEL_VOL_EFFECT_COUNTER, c.volume.counter);
                soa_write_u8(
                    addresses::CHANNEL_VOL_EFFECT_HALF_WAVELENGTH,
                    c.volume.half_wavelength,
                );

                soa_write_u8(addresses::CHANNEL_PAN, c.pan.value);
                soa_write_u8(addresses::CHANNEL_SUB_PAN, c.pan.sub_value);
                soa_write_u8(addresses::CHANNEL_PAN_EFFECT_DIRECTION, c.pan.direction);
                soa_write_u8(addresses::CHANNEL_PAN_EFFECT_OFFSET_L, c.pan.offset_l);
                soa_write_u8(addresses::CHANNEL_PAN_EFFECT_OFFSET_H, c.pan.offset_h);
                soa_write_u8(addresses::CHANNEL_PAN_EFFECT_COUNTER, c.pan.counter);
                soa_write_u8(
                    addresses::CHANNEL_PAN_EFFECT_HALF_WAVELENGTH,
                    c.pan.half_wavelength,
                );

                // Not interpreting portamento

                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK,
                    c.vibrato_pitch_offset_per_tick,
                );
                // Fixed vibrato direction
                soa_write_u8(addresses::CHANNEL_VIBRATO_DIRECTION, 0);
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_TICK_COUNTER,
                    c.vibrato_tick_counter,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_TICK_COUNTER_START,
                    c.vibrato_tick_counter_start,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_HALF_WAVELENGTH,
                    c.vibrato_half_wavelength,
                );
                soa_write_u8(addresses::CHANNEL_PREV_TEMP_GAIN, c.prev_temp_gain);
                soa_write_u8(addresses::CHANNEL_EARLY_RELEASE_CMP, c.early_release_cmp);
                soa_write_u8(
                    addresses::CHANNEL_EARLY_RELEASE_MIN_TICKS,
                    c.early_release_min_ticks,
                );
                soa_write_u8(addresses::CHANNEL_EARLY_RELEASE_GAIN, c.early_release_gain);

                soa_write_u8(addresses::CHANNEL_DETUNE_L, c.detune_l);
                soa_write_u8(addresses::CHANNEL_DETUNE_H, c.detune_h);

                // Virtual channels
                soa_write_u8(addresses::CHANNEL_VC_VOL_L, vc.vol_l);
                soa_write_u8(addresses::CHANNEL_VC_VOL_R, vc.vol_r);
                soa_write_u8(addresses::CHANNEL_VC_PITCH_L, vc.pitch_l);
                soa_write_u8(addresses::CHANNEL_VC_PITCH_H, vc.pitch_h);
                soa_write_u8(addresses::CHANNEL_VC_SCRN, vc.scrn);
                soa_write_u8(addresses::CHANNEL_VC_ADSR1, vc.adsr1);
                soa_write_u8(addresses::CHANNEL_VC_ADSR2_OR_GAIN, vc.adsr2_or_gain);
                soa_write_u8(addresses::CHANNEL_VC_TEMP_GAIN, vc.temp_gain);
            }

            for (channel_index, c) in self.channels.iter().enumerate() {
                let addr =
                    usize::from(addresses::BYTECODE_STACK) + BC_CHANNEL_STACK_SIZE * channel_index;
                apuram[addr..addr + BC_CHANNEL_STACK_SIZE].copy_from_slice(&c.bc_stack);
            }
        }

        // write dsp registers
        {
            // Not writing voice S-DSP registers
            // The audio driver's virtual channels will write to the DSP for me.

            emu.write_dsp_register(S_DSP_EON_REGISTER, eon_shadow);
        }

        emu.write_smp_register(S_SMP_TIMER_0_REGISTER, self.tick_clock);
    }
}

#[cfg(test)]
mod test {
    use super::{ChannelState, GlobalState};
    use crate::{
        driver_constants::SONG_HEADER_SIZE,
        time::{TickClock, TickCounter},
    };

    #[test]
    fn test_instruction_size_if_not_sleep_nor_branch() {
        let mut bytecode = [128; 32];
        assert!(bytecode.len() > SONG_HEADER_SIZE);

        for opcode in u8::MIN..u8::MAX {
            bytecode[0] = opcode;

            let mut global = GlobalState::new(TickClock::MAX);
            let mut cs = ChannelState::new(None, 0);
            cs.ticks = TickCounter::new(0);
            cs.instruction_ptr = 0;
            // Set stack counter to force a branch outside `bytecode` for the `end_loop` instruction
            cs.bc_stack.iter_mut().for_each(|s| *s = 10);

            cs.process_next_bytecode(&mut global, &bytecode);

            let expected = if cs.ticks.is_zero() && !cs.disabled {
                let s = usize::from(cs.instruction_ptr);

                if s < bytecode.len() {
                    Some(s)
                } else {
                    None
                }
            } else {
                None
            };

            let o = ChannelState::instruction_size_if_not_sleep_nor_branch(opcode);
            assert_eq!(o, expected, "opocde: {opcode}");
        }
    }
}
