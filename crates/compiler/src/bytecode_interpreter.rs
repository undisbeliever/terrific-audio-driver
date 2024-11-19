//! bytecode interpreter

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::opcodes;
use crate::bytecode::Pan;
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{
    addresses, LoaderDataType, BC_CHANNEL_STACK_OFFSET, BC_CHANNEL_STACK_SIZE,
    BC_STACK_BYTES_PER_LOOP, COMMON_DATA_BYTES_PER_INSTRUMENT, N_MUSIC_CHANNELS,
    SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, STARTING_VOLUME, S_DSP_EON_REGISTER,
    S_SMP_TIMER_0_REGISTER,
};
use crate::songs::Channel as SongChannel;
use crate::songs::SongData;
use crate::time::TickClock;
use crate::time::TickCounter;

use std::cmp::min;
use std::ops::Deref;

#[derive(Clone)]
struct VirtualChannel {
    vol_l: u8,
    vol_r: u8,
    // Not emulating pitch (all key-on bytecode instructions set the pitch)
    scrn: u8,
    adsr1: u8,
    adsr2_or_gain: u8,
    temp_gain: u8,

    echo: bool,
}

#[derive(Clone)]
struct ChannelSoA {
    countdown_timer: u8,
    next_event_is_key_off: u8,

    instruction_ptr: u16,

    stack_pointer: u8,
    loop_stack_pointer: u8,

    inst_pitch_offset: u8,
    volume: u8,
    pan: u8,

    // Not emulating portamento

    // Not accurate but since no notes are playing when the GUI starts playing this
    // InterpreterOutput it will not be audible at all.
    vibrato_tick_counter: u8,

    vibrato_pitch_offset_per_tick: u8,
    vibrato_direction_comparator: u8,
    vibrato_tick_counter_start: u8,
    vibrato_wavelength_in_ticks: u8,

    prev_temp_gain: u8,

    early_release_cmp: u8,
    early_release_min_ticks: u8,
    early_release_gain: u8,
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

    volume: u8,
    pan: u8,

    echo: bool,

    // Not emulating pitch
    // Not emulating portamento

    // Partially emulating vibrato
    vibrato_pitch_offset_per_tick: u8,
    vibrato_quarter_wavelength_in_ticks: u8,
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
            bc_stack: Default::default(),
            instrument: None,
            adsr_or_gain_override: Some((0, 0)),
            temp_gain: 0,
            prev_temp_gain: 0,
            early_release_cmp: 0,
            early_release_min_ticks: 0,
            early_release_gain: 0,
            volume: STARTING_VOLUME,
            pan: Pan::MAX.as_u8() / 2,
            echo: false,
            vibrato_pitch_offset_per_tick: 0,
            vibrato_quarter_wavelength_in_ticks: 0,
        }
    }

    fn new_subroutine(song: &SongData, song_ptr: u16, subroutine_index: u8) -> Self {
        let mut c = ChannelState::new(None, song_ptr);

        match song.subroutines().get(usize::from(subroutine_index)) {
            Some(sub) => {
                let notes = sub.subroutine_id.no_instrument_notes();
                let song_instruments = song.instruments();

                c.instruction_ptr = sub.bytecode_offset;

                // Subroutine might not set an instrument before the play_note instructions.
                //
                // Find the first instrument that can play all notes in the subroutine.
                // If no instrument can be found, use the first instrument in the MML file.
                let inst = match notes.is_empty() {
                    true => song_instruments.first(),
                    false => song_instruments
                        .iter()
                        .find(|i| {
                            i.note_range.contains(notes.start())
                                && i.note_range.contains(notes.end())
                        })
                        .or_else(|| song_instruments.first()),
                };

                if let Some(i) = inst {
                    c.instrument = Some(i.instrument_id.as_u8());
                    c.adsr_or_gain_override = Some(i.envelope.engine_value());
                }
            }
            None => {
                c.disable_channel();
            }
        }

        c
    }

    fn read_subroutine_instruction_ptr(s_id: u8, song_data: &[u8]) -> u16 {
        let n_subroutines = song_data[SONG_HEADER_N_SUBROUTINES_OFFSET];

        let li = usize::from(s_id) + SONG_HEADER_SIZE;
        let hi = li + usize::from(n_subroutines);

        let l = song_data.get(li).copied().unwrap_or(0xff);
        let h = song_data.get(hi).copied().unwrap_or(0xff);

        u16::from_le_bytes([l, h])
    }

    fn to_tick_count(length: u8, key_off: bool) -> TickCounter {
        if length > 0 {
            TickCounter::new(u32::from(length) + u32::from(key_off))
        } else {
            TickCounter::new(0x100 + u32::from(key_off))
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

        self.ticks += Self::to_tick_count(length, key_off);

        if key_off {
            self.temp_gain = 0;
        }
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
            opcodes::FIRST_PLAY_NOTE_INSTRUCTION.. => {
                let length = read_pc();
                self.play_note(opcode, length);
            }

            opcodes::PORTAMENTO_DOWN | opcodes::PORTAMENTO_UP => {
                // Ignore portamento state
                let _portamento_speed = read_pc();
                let wait_length = read_pc();
                let note_and_key_off_bit = read_pc();

                self.play_note(note_and_key_off_bit, wait_length);
            }

            opcodes::SET_VIBRATO => {
                let depth = read_pc();
                let wavelength = read_pc();

                self.vibrato_pitch_offset_per_tick = depth;
                self.vibrato_quarter_wavelength_in_ticks = wavelength;
            }
            opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE => {
                let depth = read_pc();
                let note = read_pc();
                let length = read_pc();

                self.vibrato_pitch_offset_per_tick = depth;
                self.play_note(note, length);
            }

            opcodes::WAIT => {
                let to_rest = read_pc();
                self.ticks += Self::to_tick_count(to_rest, false);
            }
            opcodes::REST => {
                let to_rest = read_pc();
                self.ticks += Self::to_tick_count(to_rest, true);
                self.temp_gain = 0;
            }

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

                self.prev_temp_gain = temp_gain;
                self.ticks += Self::to_tick_count(to_rest, true);
                // Temp gain is reset on key-off
                self.temp_gain = 0;
            }

            opcodes::SET_TEMP_GAIN_AND_WAIT => {
                let temp_gain = read_pc();
                let to_rest = read_pc();

                self.temp_gain = temp_gain;
                self.prev_temp_gain = temp_gain;
                self.ticks += Self::to_tick_count(to_rest, false);
            }

            opcodes::REUSE_TEMP_GAIN => {
                self.temp_gain = self.prev_temp_gain;
            }
            opcodes::REUSE_TEMP_GAIN_AND_REST => {
                let to_rest = read_pc();

                self.ticks += Self::to_tick_count(to_rest, true);
                // Temp gain is reset on key-off
                self.temp_gain = 0;
            }
            opcodes::REUSE_TEMP_GAIN_AND_WAIT => {
                let to_rest = read_pc();

                self.temp_gain = self.prev_temp_gain;
                self.ticks += Self::to_tick_count(to_rest, false);
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

            opcodes::ADJUST_PAN => {
                let adjust = read_pc();

                let p = i8::from_le_bytes([adjust]);
                self.pan = self.pan.saturating_add_signed(p).clamp(0, Pan::MAX.as_u8());
            }
            opcodes::SET_PAN => {
                self.pan = read_pc();
            }
            opcodes::SET_PAN_AND_VOLUME => {
                let pan = read_pc();
                let volume = read_pc();

                self.pan = pan;
                self.volume = volume;
            }
            opcodes::ADJUST_VOLUME => {
                let adjust = read_pc();

                let v = i8::from_le_bytes([adjust]);
                self.volume = self.volume.saturating_add_signed(v);
            }
            opcodes::SET_VOLUME => {
                self.volume = read_pc();
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

            opcodes::DISABLE_CHANNEL => self.disable_channel(),

            _ => self.disable_channel(),
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
        subroutine_index: u8,
        stereo_flag: bool,
    ) -> Self {
        let mut channels: [Option<ChannelState>; N_MUSIC_CHANNELS] = Default::default();

        if usize::from(subroutine_index) < song_data.subroutines().len() {
            channels[0] = Some(ChannelState::new_subroutine(
                &song_data,
                common_audio_data.song_data_addr(),
                subroutine_index,
            ));
        }

        Self {
            channels,
            tick_counter: TickCounter::default(),
            global: GlobalState::new(song_data.metadata().tick_clock),
            stereo_flag,
            song_data,
            common_audio_data,
        }
    }

    pub fn channels(&self) -> &[Option<ChannelState>; N_MUSIC_CHANNELS] {
        &self.channels
    }

    /// Return the channel with the smallest tick-counter and the tick-counter of the next smallest channel
    fn next_channel_to_process(
        channels: &mut [Option<ChannelState>; N_MUSIC_CHANNELS],
        target_ticks: TickCounter,
    ) -> Option<(&mut ChannelState, TickCounter)> {
        // ::SHOULDDO optimise (profile before and after)::

        let mut out = None;

        for c in channels.iter_mut().flatten() {
            match out {
                None => out = Some((c, TickCounter::MAX)),
                Some(ref o) => {
                    if c.ticks < o.0.ticks {
                        out = Some((c, o.0.ticks))
                    }
                }
            }
        }

        out.filter(|c| c.0.ticks < target_ticks)
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
            let next_channel_ticks = min(next_channel_ticks, target_ticks);

            while c.ticks < next_channel_ticks {
                c.process_next_bytecode(&mut self.global, song_data);

                watchdog_counter -= 1;
                if watchdog_counter == 0 {
                    return false;
                }
            }
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
                Some(c) => build_channel(i, c, self.tick_counter, &common),
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

        let n_instruments = c.n_instruments_and_samples().try_into().unwrap();

        CommonAudioDataSoA {
            stereo_flag,
            song_data_addr: c.song_data_addr(),
            n_instruments,
            instruments_scrn: inst_soa_data(0),
            instruments_pitch_offset: inst_soa_data(1),
            instruments_adsr1: inst_soa_data(2),
            instruments_adsr2_or_gain: inst_soa_data(3),
        }
    }
}

fn build_channel(
    channel_index: usize,
    c: &ChannelState,
    target_ticks: TickCounter,
    common: &CommonAudioDataSoA,
) -> Channel {
    assert!(Pan::try_from(c.pan).is_ok());

    assert_eq!(c.song_ptr, common.song_data_addr);

    assert!(c.ticks >= target_ticks);
    let delay = match c.disabled {
        true => 0,
        false => c.ticks.value() - target_ticks.value(),
    };

    let (countdown_timer, next_event_is_key_off) = match delay {
        0..=0xfe => (u8::try_from(delay + 1).unwrap(), 0),
        0xff => (0, 0),
        0x100 => (0, 0xff),
        _ => panic!("Invalid ChannelInterpreter.ticks value (delay: {})", delay),
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
        None => (0, 0, (0, 0)),
    };

    let (adsr1, adsr2_or_gain) = match c.adsr_or_gain_override {
        Some((a1, a2)) => (a1, a2),
        None => inst_adsr_or_gain,
    };

    let volume = c.volume;
    let pan = c.pan;

    assert!(c.stack_pointer <= BC_CHANNEL_STACK_SIZE);
    assert!(c.loop_stack_pointer + BC_STACK_BYTES_PER_LOOP <= BC_CHANNEL_STACK_SIZE);

    let stack_start = BC_CHANNEL_STACK_OFFSET + channel_index * BC_CHANNEL_STACK_SIZE;
    let stack_pointer = u8::try_from(stack_start + c.stack_pointer).unwrap();
    let loop_stack_index = u8::try_from(stack_start + c.loop_stack_pointer).unwrap();

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
            volume,
            pan,
            vibrato_pitch_offset_per_tick: c.vibrato_pitch_offset_per_tick,
            vibrato_tick_counter_start: c.vibrato_quarter_wavelength_in_ticks,
            vibrato_tick_counter: c.vibrato_quarter_wavelength_in_ticks,
            vibrato_direction_comparator: c.vibrato_quarter_wavelength_in_ticks << 1,
            vibrato_wavelength_in_ticks: c.vibrato_quarter_wavelength_in_ticks << 2,
            prev_temp_gain: c.prev_temp_gain,
            early_release_cmp: c.early_release_cmp,
            early_release_min_ticks: c.early_release_min_ticks,
            early_release_gain: c.early_release_gain,
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
            scrn,
            adsr1,
            adsr2_or_gain,
            temp_gain: c.temp_gain,
            echo: c.echo,
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
            inst_pitch_offset: 0,
            volume: STARTING_VOLUME,
            pan: Pan::CENTER.as_u8(),
            vibrato_pitch_offset_per_tick: 0,
            vibrato_tick_counter_start: 0,
            vibrato_tick_counter: 0,
            vibrato_direction_comparator: 0,
            vibrato_wavelength_in_ticks: 0,
            prev_temp_gain: 0,
            early_release_cmp: 0,
            early_release_min_ticks: 0,
            early_release_gain: 0,
        },
        bc_stack: [0; BC_CHANNEL_STACK_SIZE],
        dsp: VirtualChannel {
            vol_l: 0,
            vol_r: 0,
            scrn: 0,
            adsr1: 0,
            adsr2_or_gain: 0,
            temp_gain: 0,
            echo: false,
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
        let eon_shadow: u8 = self
            .channels
            .iter()
            .enumerate()
            .map(|(i, c)| u8::from(c.dsp.echo) << i)
            .sum();

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
                soa_write_u8(addresses::CHANNEL_VOLUME, c.volume);
                soa_write_u8(addresses::CHANNEL_PAN, c.pan);

                // Not interpreting portamento

                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK,
                    c.vibrato_pitch_offset_per_tick,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_DIRECTION_COMPARATOR,
                    c.vibrato_direction_comparator,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_TICK_COUNTER,
                    c.vibrato_tick_counter,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_TICK_COUNTER_START,
                    c.vibrato_tick_counter_start,
                );
                soa_write_u8(
                    addresses::CHANNEL_VIBRATO_WAVELENGTH_IN_TICKS,
                    c.vibrato_wavelength_in_ticks,
                );
                soa_write_u8(addresses::CHANNEL_PREV_TEMP_GAIN, c.prev_temp_gain);
                soa_write_u8(addresses::CHANNEL_EARLY_RELEASE_CMP, c.early_release_cmp);
                soa_write_u8(
                    addresses::CHANNEL_EARLY_RELEASE_MIN_TICKS,
                    c.early_release_min_ticks,
                );
                soa_write_u8(addresses::CHANNEL_EARLY_RELEASE_GAIN, c.early_release_gain);

                // Virtual channels
                soa_write_u8(addresses::CHANNEL_VC_VOL_L, vc.vol_l);
                soa_write_u8(addresses::CHANNEL_VC_VOL_R, vc.vol_r);
                // Not interpreting pitch
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
