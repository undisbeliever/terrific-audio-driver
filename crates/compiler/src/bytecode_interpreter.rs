//! bytecode interpreter

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::opcodes;
use crate::bytecode::{Pan, LAST_PLAY_NOTE_OPCODE};
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{
    addresses, LoaderDataType, BC_CHANNEL_STACK_OFFSET, BC_CHANNEL_STACK_SIZE,
    BC_STACK_BYTES_PER_LOOP, CENTER_PAN, COMMON_DATA_BYTES_PER_INSTRUMENT, N_MUSIC_CHANNELS,
    SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, STARTING_VOLUME, S_DSP_EON_REGISTER,
    S_SMP_TIMER_0_REGISTER,
};
use crate::songs::SongData;
use crate::time::TickCounter;

#[derive(Clone)]
pub struct VirtualChannel {
    vol_l: u8,
    vol_r: u8,
    // Not emulating pitch (all key-on bytecode instructions set the pitch)
    scrn: u8,
    adsr1: u8,
    adsr2_or_gain: u8,

    echo: bool,
}

#[derive(Clone)]
pub struct ChannelSoA {
    countdown_timer: u8,
    next_event_is_key_off: u8,

    instruction_ptr: u16,
    return_ptr: u16,

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
}

#[derive(Clone)]
pub struct Channel {
    soa: ChannelSoA,
    bc_stack: [u8; BC_CHANNEL_STACK_SIZE],
    dsp: VirtualChannel,
}

pub struct InterpreterOutput {
    channels: [Channel; N_MUSIC_CHANNELS],
    song_data_addr: u16,
    stereo_flag: bool,
    tick_clock: u8,
}

#[derive(Clone)]
struct TickClockOverride {
    timer_register: u8,
    when: TickCounter,
}

struct ChannelState {
    ticks: TickCounter,
    disabled: bool,

    song_ptr: u16,

    instruction_ptr: u16,
    instruction_ptr_after_end: u16,
    return_ptr: Option<u16>,

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

    volume: u8,
    pan: u8,

    echo: bool,

    // Not emulating pitch
    // Not emulating portamento

    // Partially emulating vibrato
    vibrato_pitch_offset_per_tick: u8,
    vibrato_quarter_wavelength_in_ticks: u8,

    tick_clock_override: Option<TickClockOverride>,
}

impl ChannelState {
    fn new(song: &SongData, song_ptr: u16, channel_id: usize) -> Self {
        let channel = song.channels()[channel_id].as_ref();

        Self {
            ticks: TickCounter::new(0),
            disabled: false,
            song_ptr,
            instruction_ptr: channel.map(|c| c.bytecode_offset).unwrap_or(u16::MAX),
            instruction_ptr_after_end: channel
                .and_then(|c| c.loop_point)
                .and_then(|lp| u16::try_from(lp.bytecode_offset).ok())
                .unwrap_or(u16::MAX),
            return_ptr: None,
            stack_pointer: BC_CHANNEL_STACK_SIZE,
            loop_stack_pointer: BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP,
            bc_stack: Default::default(),
            instrument: None,
            adsr_or_gain_override: Some((0, 0)),
            volume: STARTING_VOLUME,
            pan: Pan::MAX / 2,
            echo: false,
            vibrato_pitch_offset_per_tick: 0,
            vibrato_quarter_wavelength_in_ticks: 0,
            tick_clock_override: None,
        }
    }
}

struct ChannelInterpreter<'a> {
    song_data: &'a [u8],
    target_ticks: TickCounter,
    s: ChannelState,
}

impl ChannelInterpreter<'_> {
    fn new(
        song: &SongData,
        song_ptr: u16,
        channel_id: usize,
        target_ticks: TickCounter,
    ) -> ChannelInterpreter {
        ChannelInterpreter {
            song_data: song.data(),
            target_ticks,
            s: ChannelState::new(song, song_ptr, channel_id),
        }
    }

    fn new_subroutine_intrepreter(
        song: &SongData,
        song_ptr: u16,
        channel_id: usize,
        subroutine_index: u8,
        target_ticks: TickCounter,
    ) -> ChannelInterpreter {
        let mut ci = ChannelInterpreter::new(song, song_ptr, channel_id, target_ticks);

        ci.s.instruction_ptr = ci.read_subroutine_instruction_ptr(subroutine_index);
        ci.s.return_ptr = None;

        // Subroutine might not set an instruemnt before the play_note instructions.
        //
        // Use the first instrument defined in the MML file.
        if let Some(i) = song.instruments().first() {
            ci.s.instrument = Some(i.instrument_id.as_u8());
            ci.s.adsr_or_gain_override = Some(i.envelope.engine_value());
        }

        ci
    }

    fn to_tick_count(length: u8, key_off: bool) -> TickCounter {
        if length > 0 {
            TickCounter::new(u32::from(length) + u32::from(key_off))
        } else {
            TickCounter::new(0x100 + u32::from(key_off))
        }
    }

    fn read_pc(&mut self) -> u8 {
        match self.song_data.get(usize::from(self.s.instruction_ptr)) {
            Some(b) => {
                self.s.instruction_ptr += 1;
                *b
            }
            None => {
                self.disable_channel();
                opcodes::DISABLE_CHANNEL
            }
        }
    }

    fn read_pc_i8(&mut self) -> i8 {
        i8::from_le_bytes([self.read_pc()])
    }

    fn read_subroutine_instruction_ptr(&self, s_id: u8) -> u16 {
        let n_subroutines = self.song_data[SONG_HEADER_N_SUBROUTINES_OFFSET];

        let li = usize::from(s_id) + SONG_HEADER_SIZE;
        let hi = li + usize::from(n_subroutines);

        let l = self.song_data.get(li).copied().unwrap_or(0xff);
        let h = self.song_data.get(hi).copied().unwrap_or(0xff);

        u16::from_le_bytes([l, h])
    }

    fn disable_channel(&mut self) {
        self.s.disabled = true;
        self.s.ticks = self.target_ticks;

        self.s.vibrato_pitch_offset_per_tick = 0;
    }

    fn play_note(&mut self, note_and_key_off_bit: u8, length: u8) {
        let key_off = note_and_key_off_bit & 1 == 1;

        self.s.ticks += Self::to_tick_count(length, key_off);
    }

    fn process_next_bytecode(&mut self) {
        let opcode: u8 = self.read_pc();

        if opcode <= LAST_PLAY_NOTE_OPCODE {
            let length = self.read_pc();
            self.play_note(opcode, length)
        } else {
            let opcode = opcode & 0b11111110;

            match opcode {
                opcodes::PORTAMENTO_DOWN | opcodes::PORTAMENTO_UP => {
                    // Ignore portamento state
                    let _portamento_speed = self.read_pc();
                    let wait_length = self.read_pc();
                    let note_and_key_off_bit = self.read_pc();

                    self.play_note(note_and_key_off_bit, wait_length);
                }

                opcodes::SET_VIBRATO => {
                    self.s.vibrato_pitch_offset_per_tick = self.read_pc();
                    self.s.vibrato_quarter_wavelength_in_ticks = self.read_pc();
                }
                opcodes::SET_VIBRATO_DEPTH_AND_PLAY_NOTE => {
                    self.s.vibrato_pitch_offset_per_tick = self.read_pc();
                    let note = self.read_pc();
                    let length = self.read_pc();
                    self.play_note(note, length);
                }

                opcodes::REST => {
                    let to_rest = self.read_pc();
                    self.s.ticks += Self::to_tick_count(to_rest, false);
                }
                opcodes::REST_KEYOFF => {
                    let to_rest = self.read_pc();
                    self.s.ticks += Self::to_tick_count(to_rest, true);
                }

                opcodes::CALL_SUBROUTINE => {
                    let s_id = self.read_pc();
                    self.s.vibrato_pitch_offset_per_tick = 0;

                    self.s.return_ptr = Some(self.s.instruction_ptr);
                    self.s.instruction_ptr = self.read_subroutine_instruction_ptr(s_id);
                }
                opcodes::RETURN_FROM_SUBROUTINE => match self.s.return_ptr {
                    Some(pc) => {
                        self.s.return_ptr = None;
                        self.s.vibrato_pitch_offset_per_tick = 0;
                        self.s.instruction_ptr = pc;
                    }
                    None => {
                        self.disable_channel();
                    }
                },

                opcodes::SET_INSTRUMENT => {
                    self.s.instrument = Some(self.read_pc());
                    self.s.adsr_or_gain_override = None;
                }
                opcodes::SET_INSTRUMENT_AND_ADSR_OR_GAIN => {
                    self.s.instrument = Some(self.read_pc());
                    let adsr1 = self.read_pc();
                    let adsr2_or_gain = self.read_pc();

                    self.s.adsr_or_gain_override = Some((adsr1, adsr2_or_gain));
                }
                opcodes::SET_ADSR => {
                    let adsr1 = self.read_pc();
                    let adsr2 = self.read_pc();

                    self.s.adsr_or_gain_override = Some((adsr1, adsr2));
                }
                opcodes::SET_GAIN => {
                    let gain = self.read_pc();

                    self.s.adsr_or_gain_override = Some((0, gain));
                }

                opcodes::ADJUST_PAN => {
                    let p = self.read_pc_i8();
                    self.s.pan = self.s.pan.saturating_add_signed(p).clamp(0, Pan::MAX);
                }
                opcodes::SET_PAN => {
                    self.s.pan = self.read_pc();
                }
                opcodes::SET_PAN_AND_VOLUME => {
                    self.s.pan = self.read_pc();
                    self.s.volume = self.read_pc();
                }
                opcodes::ADJUST_VOLUME => {
                    let v = self.read_pc_i8();
                    self.s.volume = self.s.volume.saturating_add_signed(v);
                }
                opcodes::SET_VOLUME => {
                    self.s.volume = self.read_pc();
                }

                opcodes::SET_SONG_TICK_CLOCK => {
                    let timer = self.read_pc();
                    self.s.tick_clock_override = Some(TickClockOverride {
                        timer_register: timer,
                        when: self.s.ticks,
                    });
                }

                opcodes::END => {
                    if self.s.instruction_ptr_after_end != 0 {
                        self.s.instruction_ptr = self.s.instruction_ptr_after_end;
                    } else {
                        self.disable_channel()
                    }
                }

                opcodes::START_LOOP => {
                    let counter = self.read_pc();

                    match self.s.stack_pointer.checked_sub(3) {
                        Some(sp) => {
                            self.s.stack_pointer = sp;
                            self.s.loop_stack_pointer = sp;

                            let inst_ptr = self.s.song_ptr + self.s.instruction_ptr;
                            let inst_ptr = inst_ptr.to_le_bytes();

                            self.s.bc_stack[sp] = counter;
                            self.s.bc_stack[sp + 1] = inst_ptr[0];
                            self.s.bc_stack[sp + 2] = inst_ptr[1];
                        }
                        None => self.disable_channel(),
                    }
                }
                opcodes::SKIP_LAST_LOOP => {
                    let bytes_to_skip = self.read_pc();

                    // No bounds testing required when reading counter
                    let sp = self.s.loop_stack_pointer;

                    let counter = self.s.bc_stack[sp];
                    if counter == 1 {
                        self.s.instruction_ptr += u16::from(bytes_to_skip);

                        let sp = sp + 3;
                        self.s.stack_pointer = sp;
                        if sp <= BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP {
                            self.s.loop_stack_pointer = sp;
                        }
                    }
                }
                opcodes::END_LOOP => {
                    // No bounds testing required when modifying counter
                    let sp = self.s.loop_stack_pointer;

                    let counter = self.s.bc_stack[sp].wrapping_sub(1);

                    if counter != 0 {
                        self.s.bc_stack[sp] = counter;
                        let inst_ptr =
                            u16::from_le_bytes([self.s.bc_stack[sp + 1], self.s.bc_stack[sp + 2]]);
                        match inst_ptr.checked_sub(self.s.song_ptr) {
                            Some(i) => self.s.instruction_ptr = i,
                            None => self.disable_channel(),
                        }
                    } else {
                        let sp = sp + 3;
                        self.s.stack_pointer = sp;
                        if sp <= BC_CHANNEL_STACK_SIZE - BC_STACK_BYTES_PER_LOOP {
                            self.s.loop_stack_pointer = sp;
                        }
                    }
                }

                opcodes::ENABLE_ECHO => self.s.echo = true,
                opcodes::DISABLE_ECHO => self.s.echo = false,

                opcodes::DISABLE_CHANNEL => self.disable_channel(),

                _ => self.disable_channel(),
            }
        }
    }

    // Returns false if target could not be reached in the time limit
    fn process_until_target(&mut self) -> bool {
        // Prevent infinite loops by limiting the number of processed instructions
        let mut watchdog_counter: u32 = 250_000;

        while self.s.ticks < self.target_ticks && watchdog_counter > 0 {
            self.process_next_bytecode();
            watchdog_counter -= 1;
        }

        watchdog_counter > 0
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

    assert!(c.ticks >= target_ticks);
    let delay = c.ticks.value() - target_ticks.value();

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
            return_ptr: match c.return_ptr {
                Some(o) => o.checked_add(common.song_data_addr).unwrap_or(0),
                None => 0,
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
        },
        bc_stack: c.bc_stack,
        dsp: VirtualChannel {
            vol_l: match common.stereo_flag {
                true => (u16::from(volume) * u16::from(Pan::MAX - pan)).to_le_bytes()[1],
                false => volume >> 2,
            },
            vol_r: match common.stereo_flag {
                true => (u16::from(volume) * u16::from(pan)).to_le_bytes()[1],
                false => volume >> 2,
            },
            scrn,
            adsr1,
            adsr2_or_gain,
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
            return_ptr: 0,
            stack_pointer,
            loop_stack_pointer: stack_pointer - BC_STACK_BYTES_PER_LOOP as u8,
            inst_pitch_offset: 0,
            volume: STARTING_VOLUME,
            pan: CENTER_PAN,
            vibrato_pitch_offset_per_tick: 0,
            vibrato_tick_counter_start: 0,
            vibrato_tick_counter: 0,
            vibrato_direction_comparator: 0,
            vibrato_wavelength_in_ticks: 0,
        },
        bc_stack: [0; BC_CHANNEL_STACK_SIZE],
        dsp: VirtualChannel {
            vol_l: 0,
            vol_r: 0,
            scrn: 0,
            adsr1: 0,
            adsr2_or_gain: 0,
            echo: false,
        },
    }
}

/// returns None if there is a timeout
pub fn interpret_song(
    song: &SongData,
    common_audio_data: &CommonAudioData,
    stereo_flag: bool,
    target_ticks: TickCounter,
) -> Option<InterpreterOutput> {
    let mut tick_clock_override = TickClockOverride {
        timer_register: song.metadata().tick_clock.as_u8(),
        when: TickCounter::new(0),
    };
    let mut valid = true;

    let common = CommonAudioDataSoA::new(common_audio_data, stereo_flag);
    let song_data_addr = common_audio_data.song_data_addr();

    let channels: [Channel; N_MUSIC_CHANNELS] =
        std::array::from_fn(|i| match song.channels().get(i) {
            Some(Some(_)) => {
                let mut ci = ChannelInterpreter::new(song, song_data_addr, i, target_ticks);

                valid &= ci.process_until_target();

                if let Some(tco) = &ci.s.tick_clock_override {
                    if tco.when >= tick_clock_override.when {
                        tick_clock_override = tco.clone();
                    }
                }

                build_channel(i, &ci.s, target_ticks, &common)
            }
            Some(None) | None => unused_channel(i),
        });

    if valid {
        Some(InterpreterOutput {
            channels,
            tick_clock: tick_clock_override.timer_register,
            song_data_addr,
            stereo_flag,
        })
    } else {
        None
    }
}

/// Intrepret a single subroutine within the song.
///
/// Returns None if `subroutine_index` is invalid or there is a timeout.
pub fn interpret_song_subroutine(
    song: &SongData,
    common_audio_data: &CommonAudioData,
    stereo_flag: bool,
    subroutine_index: u8,
    ticks: TickCounter,
) -> Option<InterpreterOutput> {
    if usize::from(subroutine_index) >= song.subroutines().len() {
        return None;
    }

    let common = CommonAudioDataSoA::new(common_audio_data, stereo_flag);
    let song_data_addr = common_audio_data.song_data_addr();

    let mut ci = ChannelInterpreter::new_subroutine_intrepreter(
        song,
        song_data_addr,
        0,
        subroutine_index,
        ticks,
    );
    let valid = ci.process_until_target();

    // return_ptr must be None so audio-driver disables the channel on a return_from_subroutine instruction
    assert_eq!(ci.s.return_ptr, None, "Invalid return_ptr");

    if valid {
        Some(InterpreterOutput {
            channels: std::array::from_fn(|i| {
                if i == 0 {
                    build_channel(i, &ci.s, ticks, &common)
                } else {
                    unused_channel(i)
                }
            }),
            tick_clock: match &ci.s.tick_clock_override {
                Some(tco) => tco.timer_register,
                None => song.metadata().tick_clock.as_u8(),
            },
            song_data_addr,
            stereo_flag,
        })
    } else {
        None
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
    pub fn write_to_emulator(&self, emu: &mut impl Emulator) {
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
                soa_write_u16(
                    addresses::CHANNEL_RETURN_INST_PTR_L,
                    addresses::CHANNEL_RETURN_INST_PTR_H,
                    c.return_ptr,
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

                // Virtual channels
                soa_write_u8(addresses::CHANNEL_VC_VOL_L, vc.vol_l);
                soa_write_u8(addresses::CHANNEL_VC_VOL_R, vc.vol_r);
                // Not intrepreting pitch
                soa_write_u8(addresses::CHANNEL_VC_SCRN, vc.scrn);
                soa_write_u8(addresses::CHANNEL_VC_ADSR1, vc.adsr1);
                soa_write_u8(addresses::CHANNEL_VC_ADSR2_OR_GAIN, vc.adsr2_or_gain);
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
