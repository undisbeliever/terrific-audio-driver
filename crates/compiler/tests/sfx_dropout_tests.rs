//! Sound effect dropout tests

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;
use std::ops::Range;
use std::str::FromStr;
use std::sync::OnceLock;

use compiler::audio_driver;
use compiler::common_audio_data::{build_common_audio_data, CommonAudioData};
use compiler::data;
use compiler::data::{validate_sfx_export_order, DefaultSfxFlags, Instrument, Name};
use compiler::driver_constants::{
    addresses, io_commands, LoaderDataType, CENTER_PAN, FIRST_SFX_CHANNEL, IO_COMMAND_I_MASK,
    IO_COMMAND_MASK, N_SFX_CHANNELS,
};
use compiler::envelope::{Envelope, Gain};
use compiler::notes::Octave;
use compiler::samples::combine_samples;
use compiler::sound_effects::{
    combine_sound_effects, compile_sound_effect_input, SfxFlags, SoundEffectInput, SoundEffectText,
};

use shvc_sound_emu::ShvcSoundEmu;

#[derive(Clone)]
struct TestSoundEffect {
    name: &'static str,
    ticks: u8,
    interruptible: bool,
    one_channel: bool,
}

// Order MUST match HIGH_PRIORITY_SFX, NORMAL_PRIORITY_SFX & LOW_PRIORITY_SFX
#[derive(Debug, Clone, Copy, PartialEq)]
enum Sfx {
    HighPriority,

    Interruptible,
    ShortInterruptible,
    LongInterruptible,
    Uninterruptible,
    ShortUninterruptible,
    OneInterruptible,
    OneUninterruptible,

    LowPriority,
    LowPriorityOneInterruptible,
    LowPriorityOneUninterruptible,
}

const NO_SFX: u8 = 0xff;

#[rustfmt::skip]
const HIGH_PRIORITY_SFX: &[TestSoundEffect] = &[
    TestSoundEffect { name: "HighPriority",                     ticks: 100, interruptible: true,  one_channel: false },
];

#[rustfmt::skip]
const NORMAL_PRIORITY_SFX: &[TestSoundEffect] = &[
    TestSoundEffect { name: "Interruptible",                    ticks: 100, interruptible: true,  one_channel: false },
    TestSoundEffect { name: "ShortInterruptible",               ticks:   5, interruptible: true,  one_channel: false },
    TestSoundEffect { name: "LongInterruptible",                ticks: 200, interruptible: true,  one_channel: false },

    TestSoundEffect { name: "Uninterruptible",                  ticks: 100, interruptible: false, one_channel: false },
    TestSoundEffect { name: "ShortUninterruptible",             ticks:   5, interruptible: false, one_channel: false },

    TestSoundEffect { name: "OneInterruptible",                 ticks: 100, interruptible: true,  one_channel: true },
    TestSoundEffect { name: "OneUninterruptible",               ticks: 100, interruptible: false, one_channel: true },
];

#[rustfmt::skip]
const LOW_PRIORITY_SFX: &[TestSoundEffect] = &[
    TestSoundEffect { name: "LowPriority",                      ticks: 100, interruptible: true,  one_channel: false },

    TestSoundEffect { name: "LowPriorityOneInterruptible",      ticks: 100, interruptible: true,  one_channel: true },
    TestSoundEffect { name: "LowPriorityOneUninterruptible",    ticks: 100, interruptible: false, one_channel: true },
];

macro_rules! sfx_id {
    (None) => {
        None
    };
    ($s:ident) => {
        Some(Sfx::$s)
    };
}

macro_rules! assert_sfx_channels {
    ($emu:ident, $sfx1:ident, $sfx2:ident) => {
        assert_sfx_channels(&$emu, sfx_id!($sfx1), sfx_id!($sfx2));
    };
}

fn assert_sfx_channels(emu: &Emu, sfx1: Option<Sfx>, sfx2: Option<Sfx>) {
    let f = |s| match s {
        Some(s) => s as u8,
        None => NO_SFX,
    };

    assert_eq!(
        emu.sfx_channels_sfx_id(),
        [f(sfx1), f(sfx2)],
        "sfx_id mismatch"
    );
}

fn assert_channel_position(emu: &Emu, channel: usize, sfx: Sfx, at_start: bool) {
    let inst_ptr = emu.sfx_channel_instruction_ptr(channel);
    let sfx_bc_range = emu.sfx_bc_addr_range(sfx);
    let sfx_addr = sfx_bc_range.start;

    assert!(sfx_bc_range.contains(&inst_ptr), "Channel not playing sfx");

    if at_start {
        assert_eq!(inst_ptr, sfx_addr, "instructionPtr == start of sfx");
    } else {
        assert_ne!(inst_ptr, sfx_addr, "instructionPtr != start of sfx");
    }
}

#[test]
fn test_emu_and_common_audio_data() {
    let mut emu = test_emu();
    assert_sfx_channels!(emu, None, None);

    // Test that `sfx_channels_sfx_id()` works with sfx_id 0
    assert_eq!(Sfx::HighPriority as u8, 0);

    emu.play_sound_effect_command(Sfx::ShortInterruptible);
    assert_sfx_channels!(emu, None, ShortInterruptible);

    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, HighPriority, ShortInterruptible);

    emu.emulate(5);
    assert_sfx_channels!(emu, HighPriority, None);

    emu.pause_command();
    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, HighPriority, HighPriority);

    assert_channel_position(&emu, 0, Sfx::HighPriority, false);
    assert_channel_position(&emu, 1, Sfx::HighPriority, true);
}

#[test]
fn one_interruptible_sfx_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::LongInterruptible);
    emu.play_sound_effect_command(Sfx::Uninterruptible);
    assert_sfx_channels!(emu, Uninterruptible, LongInterruptible);

    // The interruptible channel is dropped (even tho it finishes later the the other channel)
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, Uninterruptible, Interruptible);
}

#[test]
fn one_interruptible_sfx_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Uninterruptible);
    emu.play_sound_effect_command(Sfx::LongInterruptible);
    assert_sfx_channels!(emu, LongInterruptible, Uninterruptible);

    // The interruptible channel is dropped (even tho it finishes later the the other channel)
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, Interruptible, Uninterruptible);
}

#[test]
fn two_interruptible_sfx_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::ShortInterruptible);
    emu.play_sound_effect_command(Sfx::LongInterruptible);
    assert_sfx_channels!(emu, LongInterruptible, ShortInterruptible);

    // the sfx that finishes first is dropped
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, LongInterruptible, Interruptible);
}

#[test]
fn two_interruptible_sfx_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::LongInterruptible);
    emu.play_sound_effect_command(Sfx::ShortInterruptible);
    assert_sfx_channels!(emu, ShortInterruptible, LongInterruptible);

    // the sfx that finishes first is dropped
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, Interruptible, LongInterruptible);
}

#[test]
fn two_uninterruptible_sfx_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Uninterruptible);
    emu.play_sound_effect_command(Sfx::Uninterruptible);
    assert_sfx_channels!(emu, Uninterruptible, Uninterruptible);

    // The play_sound_effect command is dropped
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, Uninterruptible, Uninterruptible);
}

#[test]
fn two_uninterruptible_sfx_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Uninterruptible);
    emu.play_sound_effect_command(Sfx::ShortUninterruptible);
    assert_sfx_channels!(emu, ShortUninterruptible, Uninterruptible);

    // The play_sound_effect command is dropped
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, ShortUninterruptible, Uninterruptible);
}

#[test]
fn high_priority_one_interruptible_sfx_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::LongInterruptible);
    emu.play_sound_effect_command(Sfx::Uninterruptible);
    assert_sfx_channels!(emu, Uninterruptible, LongInterruptible);

    // The interruptible channel is dropped (even tho it finishes later the the other channel)
    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, Uninterruptible, HighPriority);
}

#[test]
fn high_priority_one_interruptible_sfx_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Uninterruptible);
    emu.play_sound_effect_command(Sfx::LongInterruptible);
    assert_sfx_channels!(emu, LongInterruptible, Uninterruptible);

    // The interruptible channel is dropped (even tho it finishes later the the other channel)
    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, HighPriority, Uninterruptible);
}

#[test]
fn high_priority_overrides_two_uninterruptible_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Uninterruptible);
    emu.play_sound_effect_command(Sfx::ShortUninterruptible);
    assert_sfx_channels!(emu, ShortUninterruptible, Uninterruptible);

    // The interruptible flag is ignored
    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, HighPriority, Uninterruptible);
}

#[test]
fn high_priority_overrides_two_uninterruptible_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::ShortUninterruptible);
    emu.play_sound_effect_command(Sfx::Uninterruptible);
    assert_sfx_channels!(emu, Uninterruptible, ShortUninterruptible);

    // The interruptible flag is ignored
    emu.play_sound_effect_command(Sfx::HighPriority);
    assert_sfx_channels!(emu, Uninterruptible, HighPriority);
}

#[test]
fn low_priority_1() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Interruptible);
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels!(emu, Interruptible, Interruptible);

    // Both sfx channels are occupied, the play_sound_effect command is dropped
    emu.play_sound_effect_command(Sfx::LowPriority);
    assert_sfx_channels!(emu, Interruptible, Interruptible);
}

#[test]
fn low_priority_2() {
    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Interruptible);
    emu.play_sound_effect_command(Sfx::LowPriority);
    assert_sfx_channels!(emu, LowPriority, Interruptible);

    emu.emulate(5);
    emu.pause_command();

    // Both sfx channels are occupied, the play_sound_effect command is dropped
    emu.play_sound_effect_command(Sfx::LowPriority);
    assert_sfx_channels!(emu, LowPriority, Interruptible);
    assert_channel_position(&emu, 0, Sfx::LowPriority, false);
}

fn _one_channel_1(sfx: Sfx, is_reset: bool) {
    assert_ne!(sfx, Sfx::Interruptible);

    let mut emu = test_emu();

    emu.play_sound_effect_command(sfx);

    emu.emulate(5);
    assert_sfx_channels(&emu, None, Some(sfx));
    assert_channel_position(&emu, 1, sfx, false);

    emu.pause_command();

    // Test if the sound effect has been reset
    emu.play_sound_effect_command(sfx);
    assert_sfx_channels(&emu, None, Some(sfx));
    assert_channel_position(&emu, 1, sfx, is_reset);
}

fn _one_channel_2(sfx: Sfx, is_reset: bool) {
    assert_ne!(sfx, Sfx::Interruptible);

    let mut emu = test_emu();

    emu.play_sound_effect_command(Sfx::Interruptible);
    emu.play_sound_effect_command(sfx);
    assert_sfx_channels(&emu, Some(sfx), Some(Sfx::Interruptible));

    emu.emulate(5);
    assert_channel_position(&emu, 0, sfx, false);

    emu.pause_command();

    // Test if the sound effect has been reset
    emu.play_sound_effect_command(sfx);
    assert_sfx_channels(&emu, Some(sfx), Some(Sfx::Interruptible));
    assert_channel_position(&emu, 0, sfx, is_reset);
}

fn _one_channel_3(sfx: Sfx, is_reset: bool) {
    assert_ne!(sfx, Sfx::Interruptible);

    let mut emu = test_emu();

    emu.play_sound_effect_command(sfx);
    emu.play_sound_effect_command(Sfx::Interruptible);
    assert_sfx_channels(&emu, Some(Sfx::Interruptible), Some(sfx));

    emu.emulate(5);
    assert_channel_position(&emu, 1, sfx, false);

    emu.pause_command();

    // Test if the sound effect has been reset
    emu.play_sound_effect_command(sfx);
    assert_sfx_channels(&emu, Some(Sfx::Interruptible), Some(sfx));
    assert_channel_position(&emu, 1, sfx, is_reset);
}

fn _one_channel_tests(sfx: Sfx, is_reset: bool) {
    _one_channel_1(sfx, is_reset);
    _one_channel_2(sfx, is_reset);
    _one_channel_3(sfx, is_reset);
}

#[test]
fn one_channel_interruptible() {
    _one_channel_tests(Sfx::OneInterruptible, true);
}

#[test]
fn one_channel_uninterruptible() {
    _one_channel_tests(Sfx::OneUninterruptible, false);
}

// Tests that one-channel low-priority sfx have priority over many-channel sfx
#[test]
fn low_priority_one_channel_interruptible() {
    _one_channel_tests(Sfx::LowPriorityOneInterruptible, true);
}

// Tests that one-channel low-priority sfx have priority over many-channel sfx
#[test]
fn low_priority_one_channel_uninterruptible() {
    _one_channel_tests(Sfx::LowPriorityOneUninterruptible, false);
}

/// Audio driver emulator
struct Emu {
    emu: ShvcSoundEmu,
    sfx_addrs: Vec<u16>,
    previous_command: u8,
}

impl Emu {
    const STEREO_FLAG: bool = true;

    pub fn new(common_audio_data: &CommonAudioData) -> Emu {
        const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

        let mut emu = ShvcSoundEmu::new(&[0; 64]);

        let common_data = common_audio_data.data();
        let song_data = audio_driver::BLANK_SONG;
        let song_data_addr = common_audio_data.song_data_addr();

        let apuram = emu.apuram_mut();

        let mut write_spc_ram = |addr: u16, data: &[u8]| {
            let addr = usize::from(addr);
            apuram[addr..addr + data.len()].copy_from_slice(data);
        };

        // Load driver
        write_spc_ram(addresses::LOADER, audio_driver::LOADER);
        write_spc_ram(addresses::DRIVER_CODE, audio_driver::AUDIO_DRIVER);

        write_spc_ram(addresses::COMMON_DATA, common_data);
        write_spc_ram(addresses::SONG_PTR, &song_data_addr.to_le_bytes());
        write_spc_ram(song_data_addr, song_data);

        // Set loader flags
        apuram[LOADER_DATA_TYPE_ADDR] = LoaderDataType {
            stereo_flag: Self::STEREO_FLAG,
            play_song: true,
            skip_echo_buffer_reset: true,
        }
        .driver_value();

        emu.set_spc_registers(addresses::DRIVER_CODE, 0, 0, 0, 0, 0xff);

        let mut sfx_addrs = common_audio_data.sound_effect_addresses();
        sfx_addrs.push(common_audio_data.sfx_bytecode_addr_range().end);

        // Assert `sfx_addrs` sorted
        assert!(sfx_addrs.windows(2).all(|w| w[0] <= w[1]));

        Emu {
            emu,
            sfx_addrs,
            previous_command: 0,
        }
    }

    pub fn sfx_channel_instruction_ptr(&self, i: usize) -> u16 {
        assert!(i < N_SFX_CHANNELS);

        let apuram = self.emu.apuram();
        let read_soa = |addr| apuram[usize::from(addr) + FIRST_SFX_CHANNEL + i];

        u16::from_le_bytes([
            read_soa(addresses::CHANNEL_INSTRUCTION_PTR_L),
            read_soa(addresses::CHANNEL_INSTRUCTION_PTR_H),
        ])
    }

    pub fn sfx_bc_addr_range(&self, s: Sfx) -> Range<u16> {
        let s = s as usize;

        Range {
            start: self.sfx_addrs[s],
            end: self.sfx_addrs[s + 1],
        }
    }

    pub fn sfx_channels_sfx_id(&self) -> [u8; N_SFX_CHANNELS] {
        let apuram = self.emu.apuram();

        std::array::from_fn(|i| {
            let read_soa = |addr| apuram[usize::from(addr) + FIRST_SFX_CHANNEL + i];

            let inst_ptr = u16::from_le_bytes([
                read_soa(addresses::CHANNEL_INSTRUCTION_PTR_L),
                read_soa(addresses::CHANNEL_INSTRUCTION_PTR_H),
            ]);

            if inst_ptr < addresses::COMMON_DATA {
                0xff
            } else {
                match self.sfx_addrs.binary_search(&inst_ptr) {
                    Ok(i) => i.try_into().unwrap(),
                    Err(i) => i.saturating_sub(1).try_into().unwrap(),
                }
            }
        })
    }

    pub fn emulate(&mut self, count: usize) {
        for _ in 0..count {
            self.emu.emulate();
        }
    }

    fn is_io_command_acknowledged(&self) -> bool {
        self.emu.read_io_ports()[0] == self.previous_command
    }

    fn emu_and_send_command(&mut self, command: u8, param1: u8, param2: u8) {
        while !self.is_io_command_acknowledged() {
            self.emu.emulate();
        }

        let command =
            ((self.previous_command ^ u8::MAX) & IO_COMMAND_I_MASK) | (command & IO_COMMAND_MASK);

        self.emu.write_io_ports([command, param1, param2, 0]);
        self.previous_command = command;

        while !self.is_io_command_acknowledged() {
            self.emu.emulate();
        }
    }

    pub fn play_sound_effect_command(&mut self, sfx_id: Sfx) {
        self.emu_and_send_command(io_commands::PLAY_SOUND_EFFECT, sfx_id as u8, CENTER_PAN);
    }

    pub fn pause_command(&mut self) {
        self.emu_and_send_command(io_commands::PAUSE, 0, 0);
    }
}

fn test_emu() -> Emu {
    static LOCK: OnceLock<CommonAudioData> = OnceLock::new();
    let cad = LOCK.get_or_init(_build_test_common_audio_data);

    Emu::new(cad)
}

// Should only be called once
fn _build_test_common_audio_data() -> CommonAudioData {
    let samples = combine_samples([].as_slice(), [].as_slice()).unwrap();
    let pitch_table = samples.pitch_table();

    // Required to prevent a `ProjectFileErrors([InstrumentOrSample(Empty)])` error
    let dummy_instrument = Instrument {
        name: Name::from_str("__dummy").unwrap(),
        source: Default::default(),
        freq: 500.0,
        loop_setting: data::LoopSetting::None,
        first_octave: Octave::try_new(2).unwrap(),
        last_octave: Octave::try_new(5).unwrap(),
        envelope: Envelope::Gain(Gain::new(127)),
        comment: None,
    };

    let instruments_and_samples =
        data::validate_instrument_and_sample_names([dummy_instrument].iter(), std::iter::empty())
            .unwrap();

    let sfx_map: HashMap<_, _> = [HIGH_PRIORITY_SFX, NORMAL_PRIORITY_SFX, LOW_PRIORITY_SFX]
        .concat()
        .iter()
        .map(|s| {
            let sfx = SoundEffectInput {
                name: Name::from_str(s.name).unwrap(),
                sfx: SoundEffectText::BytecodeAssembly(format!("rest {}", s.ticks)),
                flags: SfxFlags {
                    interruptible: Some(s.interruptible),
                    one_channel: Some(s.one_channel),
                },
            };
            let compiled_sfx =
                compile_sound_effect_input(&sfx, &instruments_and_samples, pitch_table).unwrap();

            (sfx.name, compiled_sfx)
        })
        .collect();

    let to_name_vec =
        |s: &[TestSoundEffect]| s.iter().map(|s| Name::from_str(s.name).unwrap()).collect();

    let export_order = validate_sfx_export_order(
        to_name_vec(HIGH_PRIORITY_SFX),
        to_name_vec(NORMAL_PRIORITY_SFX),
        to_name_vec(LOW_PRIORITY_SFX),
    )
    .unwrap();

    let default_flags = DefaultSfxFlags {
        interruptible: true,
        one_channel: true,
    };

    let sfx = combine_sound_effects(&sfx_map, &export_order, default_flags).unwrap();

    build_common_audio_data(&samples, &sfx).unwrap()
}
