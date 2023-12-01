//! bytecode interpreter

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::opcodes;
use crate::bytecode::{Pan, LAST_PLAY_NOTE_OPCODE};
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::addresses::CHANNEL_DISABLED_BYTECODE;
use crate::driver_constants::{
    addresses, LoaderDataType, COMMON_DATA_BYTES_PER_DIR, COMMON_DATA_BYTES_PER_INSTRUMENTS,
    COMMON_DATA_HEADER_SIZE, COMMON_DATA_N_DIR_ITEMS_OFFSET, COMMON_DATA_N_INSTRUMENTS_OFFSET,
    N_MUSIC_CHANNELS, SONG_HEADER_N_SUBROUTINES_OFFSET, SONG_HEADER_SIZE, STARTING_VOLUME,
    S_DSP_EON_REGISTER, S_SMP_TIMER_0_REGISTER,
};
use crate::songs::SongData;
use crate::time::TickCounter;

use std::ops::Range;

pub struct ChannelDsp {
    vol_l: u8,
    vol_r: u8,
    // Not emulating pitch (all key-on bytecode instructions set the pitch)
    scrn: u8,
    adsr1: u8,
    adsr2: u8,
    gain: u8,

    echo: bool,
}

struct LoopStateSoA {
    counter: u8,
    loop_point: u16,
}

pub struct ChannelSoA {
    countdown_timer: u8,
    next_event_is_key_off: u8,

    instruction_ptr: u16,
    return_ptr: u16,

    loop_state: [LoopStateSoA; 3],

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

pub struct Channel {
    soa: ChannelSoA,
    dsp: ChannelDsp,
}

pub struct InterpreterOutput {
    channels: [Channel; N_MUSIC_CHANNELS],
    stereo_flag: bool,
    tick_clock: u8,
}

#[derive(Clone)]
struct TickClockOverride {
    timer_register: u8,
    when: TickCounter,
}

struct LoopState {
    counter: u8,
    loop_point: u16,
}

impl Default for LoopState {
    fn default() -> Self {
        Self {
            counter: 0,
            loop_point: u16::MAX,
        }
    }
}

struct ChannelState {
    ticks: TickCounter,
    disabled: bool,

    instruction_ptr: u16,
    instruction_ptr_after_end: u16,
    return_ptr: u16,

    loop_state: [LoopState; 3],

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
    fn new(song: &SongData, channel_id: usize) -> Self {
        Self {
            ticks: TickCounter::new(0),
            disabled: false,
            instruction_ptr: song
                .channels()
                .get(channel_id)
                .map(|c| c.bytecode_offset)
                .unwrap_or(u16::MAX),
            instruction_ptr_after_end: song
                .channels()
                .get(channel_id)
                .and_then(|c| c.loop_point)
                .and_then(|lp| u16::try_from(lp.bytecode_offset).ok())
                .unwrap_or(u16::MAX),
            return_ptr: u16::MAX,
            loop_state: Default::default(),
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
    fn new(song: &SongData, channel_id: usize, target_ticks: TickCounter) -> ChannelInterpreter {
        ChannelInterpreter {
            song_data: song.data(),
            target_ticks,
            s: ChannelState::new(song, channel_id),
        }
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

    fn start_loop(&mut self, i: usize) {
        let counter = self.read_pc();

        self.s.loop_state[i] = LoopState {
            counter,
            loop_point: self.s.instruction_ptr,
        };
    }

    fn skip_last_loop(&mut self, i: usize) {
        let bytes_to_skip = self.read_pc();

        let ls = &self.s.loop_state[i];
        if ls.counter == 1 {
            self.s.instruction_ptr += u16::from(bytes_to_skip);
        }
    }

    fn end_loop(&mut self, i: usize) {
        let ls = &mut self.s.loop_state[i];

        ls.counter = ls.counter.wrapping_sub(1);
        if ls.counter != 0 {
            self.s.instruction_ptr = ls.loop_point;
        }
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

                    self.s.return_ptr = self.s.instruction_ptr;
                    self.s.instruction_ptr = self.read_subroutine_instruction_ptr(s_id);
                }
                opcodes::RETURN_FROM_SUBROUTINE => {
                    self.s.vibrato_pitch_offset_per_tick = 0;
                    self.s.instruction_ptr = self.s.return_ptr;
                }

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

                opcodes::START_LOOP_0 => self.start_loop(0),
                opcodes::START_LOOP_1 => self.start_loop(1),
                opcodes::START_LOOP_2 => self.start_loop(2),
                opcodes::SKIP_LAST_LOOP_0 => self.skip_last_loop(0),
                opcodes::SKIP_LAST_LOOP_1 => self.skip_last_loop(1),
                opcodes::SKIP_LAST_LOOP_2 => self.skip_last_loop(2),
                opcodes::END_LOOP_0 => self.end_loop(0),
                opcodes::END_LOOP_1 => self.end_loop(1),
                opcodes::END_LOOP_2 => self.end_loop(2),

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
    fn new(c: &CommonAudioData, stereo_flag: bool, song_data_addr: u16) -> CommonAudioDataSoA {
        let data = c.data();

        let n_dir_items = data[COMMON_DATA_N_DIR_ITEMS_OFFSET];
        let n_instruments = data[COMMON_DATA_N_INSTRUMENTS_OFFSET];

        let inst_soa_range = |i| {
            assert!(i < COMMON_DATA_BYTES_PER_INSTRUMENTS);

            let n_instruments = usize::from(n_instruments);

            let start = COMMON_DATA_HEADER_SIZE
                + usize::from(n_dir_items) * COMMON_DATA_BYTES_PER_DIR
                + i * n_instruments;
            Range {
                start,
                end: start + n_instruments,
            }
        };

        CommonAudioDataSoA {
            stereo_flag,
            song_data_addr,
            n_instruments,
            instruments_scrn: &data[inst_soa_range(0)],
            instruments_pitch_offset: &data[inst_soa_range(1)],
            instruments_adsr1: &data[inst_soa_range(2)],
            instruments_adsr2_or_gain: &data[inst_soa_range(3)],
        }
    }
}

fn loop_soa(ls: &LoopState, common: &CommonAudioDataSoA) -> LoopStateSoA {
    LoopStateSoA {
        counter: ls.counter,
        loop_point: ls
            .loop_point
            .checked_add(common.song_data_addr)
            .unwrap_or(0),
    }
}

fn build_channel(
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

    Channel {
        soa: ChannelSoA {
            countdown_timer,
            next_event_is_key_off,

            instruction_ptr: match c.disabled {
                true => addresses::CHANNEL_DISABLED_BYTECODE,
                false => c
                    .instruction_ptr
                    .checked_add(common.song_data_addr)
                    .unwrap_or(CHANNEL_DISABLED_BYTECODE),
            },
            return_ptr: c.return_ptr.checked_add(common.song_data_addr).unwrap_or(0),
            loop_state: [
                loop_soa(&c.loop_state[0], common),
                loop_soa(&c.loop_state[1], common),
                loop_soa(&c.loop_state[2], common),
            ],
            inst_pitch_offset,
            volume,
            pan,
            vibrato_pitch_offset_per_tick: c.vibrato_pitch_offset_per_tick,
            vibrato_tick_counter_start: c.vibrato_quarter_wavelength_in_ticks,
            vibrato_tick_counter: c.vibrato_quarter_wavelength_in_ticks,
            vibrato_direction_comparator: c.vibrato_quarter_wavelength_in_ticks << 1,
            vibrato_wavelength_in_ticks: c.vibrato_quarter_wavelength_in_ticks << 2,
        },
        dsp: ChannelDsp {
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
            adsr2: adsr2_or_gain,
            gain: adsr2_or_gain,
            echo: c.echo,
        },
    }
}

/// returns None if there is a timeout
pub fn interpret_song(
    song: &SongData,
    common_audio_data: &CommonAudioData,
    stereo_flag: bool,
    song_data_addr: u16,
    target_ticks: TickCounter,
) -> Option<InterpreterOutput> {
    let mut tick_clock_override = TickClockOverride {
        timer_register: song.metadata().tick_clock.as_u8(),
        when: TickCounter::new(0),
    };
    let mut valid = true;

    let common = CommonAudioDataSoA::new(common_audio_data, stereo_flag, song_data_addr);

    let channel_states: [ChannelState; N_MUSIC_CHANNELS] = std::array::from_fn(|i| {
        let mut ci = ChannelInterpreter::new(song, i, target_ticks);

        valid &= ci.process_until_target();

        if let Some(tco) = &ci.s.tick_clock_override {
            if tco.when >= tick_clock_override.when {
                tick_clock_override = tco.clone();
            }
        }
        ci.s
    });

    if valid {
        let channels =
            std::array::from_fn(|i| build_channel(&channel_states[i], target_ticks, &common));
        Some(InterpreterOutput {
            channels,
            tick_clock: tick_clock_override.timer_register,
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
        // write to apuram
        {
            let apuram: &mut [u8; 0x10000] = emu.apuram_mut();

            assert_eq!(
                apuram[usize::from(addresses::PAUSED_IF_ZERO)],
                0,
                "Audio driver is not paused"
            );

            const STC: usize = addresses::SONG_TICK_COUNTER as usize;
            assert_eq!(
                apuram[STC..STC + 2],
                [0, 0],
                "Audio driver is not at the start of the song"
            );

            // Test all music channels are active.
            assert_eq!(
                apuram[usize::from(addresses::ACTIVE_CHANNELS)],
                (1 << N_MUSIC_CHANNELS) - 1,
                "Audio driver is not initialized"
            );

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

            for (channel_index, c) in self.channels.iter().enumerate() {
                let i = u16::try_from(channel_index).unwrap();
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

                for (li, loop_state) in c.loop_state.iter().enumerate() {
                    let li = u16::try_from(li).unwrap();
                    let mut write_ls = |array_id: u16, value: u8| {
                        const SOA_ARRAY_SIZE: u16 = 8;
                        soa_write_u8(
                            addresses::CHANNEL_LOOP_STATE + (li * 3 + array_id) * SOA_ARRAY_SIZE,
                            value,
                        );
                    };
                    write_ls(0, loop_state.counter);
                    write_ls(1, loop_state.loop_point.to_le_bytes()[0]);
                    write_ls(2, loop_state.loop_point.to_le_bytes()[1]);
                }
            }
        }

        // write dsp registers
        {
            for (channel_index, c) in self.channels.iter().enumerate() {
                let c = &c.dsp;

                let voice_registers: [u8; 8] =
                    [c.vol_l, c.vol_r, 0, 0, c.scrn, c.adsr1, c.adsr2, c.gain];

                let dsp_addr = u8::try_from(0x10 * channel_index).unwrap();
                for (i, r) in voice_registers.into_iter().enumerate() {
                    let i = u8::try_from(i).unwrap();
                    emu.write_dsp_register(dsp_addr + i, r);
                }
            }

            let echo = self
                .channels
                .iter()
                .enumerate()
                .map(|(i, c)| u8::from(c.dsp.echo) << i)
                .sum();
            emu.write_dsp_register(S_DSP_EON_REGISTER, echo);
        }

        emu.write_smp_register(S_SMP_TIMER_0_REGISTER, self.tick_clock);
    }
}
