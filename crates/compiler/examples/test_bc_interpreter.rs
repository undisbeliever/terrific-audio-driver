//! Bytecode interpreter test
//!
//! Tests the bytecode interpreter matches the audio-driver by emulating the audio-driver and
//! repeatedly testing the bytecode_interpreter output matches emulator's memory.
//!
//! This is an example and not a test as:
//!    * test_bc_interpreter requires a command line input parameter (currently there are no MML examples in the repo)
//!    * test_bc_interpreter is slow and thorough, emulating the first few minutes of every song in the project file.

use compiler::{
    bytecode_interpreter::{self, SongInterpreter},
    common_audio_data::{build_common_audio_data, CommonAudioData},
    data::{load_project_file, load_text_file_with_limit, validate_project_file_names},
    driver_constants::{
        addresses, io_commands, AudioMode, BC_TOTAL_STACK_SIZE, ECHO_VARIABLES_SIZE,
        N_MUSIC_CHANNELS, S_SMP_TIMER_0_REGISTER,
    },
    samples::build_sample_and_instrument_data,
    songs::{compile_mml_song, override_song_tick_clock, SongData},
    sound_effects::{blank_compiled_sound_effects, CompiledSfxSubroutines},
    tad_apu::ApuEmulator,
    time::{TickClock, TickCounter},
};
use tad_emu::TadEmulator;

use std::path::PathBuf;

#[derive(Clone)]
struct DummyEmu {
    apuram: [u8; 0x10000],
}

impl ApuEmulator for DummyEmu {
    fn apuram(&self) -> &[u8; compiler::driver_constants::AUDIO_RAM_SIZE] {
        &self.apuram
    }

    fn apuram_mut(&mut self) -> &mut [u8; 0x10000] {
        &mut self.apuram
    }

    fn write_smp_register(&mut self, addr: u8, value: u8) {
        self.apuram[usize::from(addr)] = value;
    }

    // HACK: Always return a mainloop() address
    fn program_counter(&self) -> u16 {
        addresses::MAINLOOP_CODE
    }

    fn reset(&mut self, _registers: compiler::tad_apu::ResetRegisters) {
        unimplemented!()
    }
}

fn mask_channel_soa(
    apuram: &[u8; 0x10000],
    addr: u16,
    mask: u16,
) -> [Option<u8>; N_MUSIC_CHANNELS] {
    std::array::from_fn(|i| match apuram[usize::from(mask) + i] {
        0 => None,
        _ => Some(apuram[usize::from(addr) + i]),
    })
}

fn assert_bc_intrepreter_matches_emu(
    to_test: &SongInterpreter<&CommonAudioData, &SongData>,
    dummy: &DummyEmu,
    emu: &TadEmulator,
    tick_count: TickCounter,
) {
    let tick_count = tick_count.value();

    let intrepreter_memory = {
        let mut dummy = dummy.clone();
        to_test.write_to_emulator(&mut dummy);
        dummy
    };

    // Not testing voice S-DSP registers.
    // (The S-DSP registers lag begind virtual registers by 1 tick)

    let int_apuram = &intrepreter_memory.apuram;
    let emu_apuram = emu.apuram();

    assert_eq!(
        int_apuram[usize::from(S_SMP_TIMER_0_REGISTER)],
        emu_apuram[usize::from(S_SMP_TIMER_0_REGISTER)],
        "S-SMP TIMER0 register mismatch (tick_count: {tick_count})"
    );

    assert_eq!(
        int_apuram[usize::from(addresses::PMON_SHADOW)],
        emu_apuram[usize::from(addresses::PMON_SHADOW)],
        "pmonShadow (tick_count: {tick_count})"
    );

    assert_eq!(
        int_apuram[usize::from(addresses::EON_SHADOW_MUSIC)],
        emu_apuram[usize::from(addresses::EON_SHADOW_MUSIC)],
        "eonShadow_music (tick_count: {tick_count})"
    );

    let test_byte = |addr: u16, name: &'static str| {
        let addr = usize::from(addr);
        assert_eq!(
            int_apuram[addr], emu_apuram[addr],
            "{name} mismatch (tick_count: {tick_count})"
        );
    };
    let test_range = |addr: u16, size: usize, name: &'static str| {
        let addr = usize::from(addr);
        let range = addr..addr + size;
        assert_eq!(
            int_apuram[range.clone()],
            emu_apuram[range],
            "{name} mismatch (tick_count: {tick_count})"
        );
    };
    let test_channel_soa = |addr: u16, name: &'static str| {
        let addr = usize::from(addr);
        let range = addr..addr + N_MUSIC_CHANNELS;
        assert_eq!(
            int_apuram[range.clone()],
            emu_apuram[range],
            "channelsSoA.{name} mismatch (tick_count: {tick_count})"
        );
    };
    let test_masked_channel_soa = |addr: u16, name: &'static str, mask: u16| {
        assert_eq!(
            mask_channel_soa(int_apuram, addr, mask),
            mask_channel_soa(emu_apuram, addr, mask),
            "channelsSoA.{name} mismatch (tick_count: {tick_count})"
        );
    };
    let test_channel_soa_ptrs = |addr_l: u16, addr_h, name: &'static str| {
        assert_eq!(
            read_ptrs(int_apuram, addr_l, addr_h),
            read_ptrs(emu_apuram, addr_l, addr_h),
            "channelsSoA.{name} mismatch (tick_count: {tick_count})"
        );
    };

    test_channel_soa(addresses::CHANNEL_VC_VOL_L, "virtualChannels.vol_l");
    test_channel_soa(addresses::CHANNEL_VC_VOL_R, "virtualChannels.vol_r");
    // Not testing PITCH, bytecode_interpreter does not implement pitches.
    test_channel_soa(addresses::CHANNEL_VC_SCRN, "virtualChannels.scrn");
    test_channel_soa(addresses::CHANNEL_VC_ADSR1, "virtualChannels.adsr1");
    test_channel_soa(
        addresses::CHANNEL_VC_ADSR2_OR_GAIN,
        "virtualChannels.adsr2OrGain",
    );
    test_channel_soa(addresses::CHANNEL_VC_TEMP_GAIN, "virtualChannels.tempGain");

    test_channel_soa_ptrs(
        addresses::CHANNEL_INSTRUCTION_PTR_L,
        addresses::CHANNEL_INSTRUCTION_PTR_H,
        "instructionPtr",
    );

    test_channel_soa(addresses::CHANNEL_STACK_POINTER, "channel_stackPointer");
    test_channel_soa(
        addresses::CHANNEL_LOOP_STACK_POINTER,
        "channel_loopStackPointer",
    );

    test_channel_soa(addresses::CHANNEL_INST_PITCH_OFFSET, "instPitchOffset");

    test_channel_soa(addresses::CHANNEL_INVERT_FLAGS, "invertFlags");

    test_channel_soa(addresses::CHANNEL_VOLUME, "volume");
    test_channel_soa(
        addresses::CHANNEL_VOL_EFFECT_DIRECTION,
        "volEffect_direction",
    );
    test_channel_soa(addresses::CHANNEL_VOL_EFFECT_OFFSET_L, "volEffect_offset_l");
    test_channel_soa(addresses::CHANNEL_VOL_EFFECT_OFFSET_H, "volEffect_offset_h");
    test_channel_soa(
        addresses::CHANNEL_VOL_EFFECT_HALF_WAVELENGTH,
        "volEffect_halfWavelength",
    );
    test_masked_channel_soa(
        addresses::CHANNEL_VOL_EFFECT_COUNTER,
        "volEffect_counter",
        addresses::CHANNEL_VOL_EFFECT_DIRECTION,
    );
    test_masked_channel_soa(
        addresses::CHANNEL_SUB_VOLUME,
        "subVolume",
        addresses::CHANNEL_VOL_EFFECT_DIRECTION,
    );

    test_channel_soa(addresses::CHANNEL_PAN, "pan");
    test_channel_soa(
        addresses::CHANNEL_PAN_EFFECT_DIRECTION,
        "panEffect_direction",
    );
    test_channel_soa(addresses::CHANNEL_PAN_EFFECT_OFFSET_L, "panEffect_offset_l");
    test_channel_soa(addresses::CHANNEL_PAN_EFFECT_OFFSET_H, "panEffect_offset_h");
    test_channel_soa(
        addresses::CHANNEL_PAN_EFFECT_HALF_WAVELENGTH,
        "panEffect_halfWavelength",
    );
    test_masked_channel_soa(
        addresses::CHANNEL_PAN_EFFECT_COUNTER,
        "panEffect_counter",
        addresses::CHANNEL_PAN_EFFECT_DIRECTION,
    );
    test_masked_channel_soa(
        addresses::CHANNEL_SUB_PAN,
        "subPan",
        addresses::CHANNEL_PAN_EFFECT_DIRECTION,
    );

    // Not testing portamento

    test_channel_soa(
        addresses::CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK,
        "vibrato_pitchOffsetPerTick",
    );
    // Not testing `vibrato_tickCounter` - bc_interpreter does not emulate vibrato
    // Not testing `vibrato_direction` - bc_interpreter does not emulate vibrato
    test_channel_soa(
        addresses::CHANNEL_VIBRATO_TICK_COUNTER_START,
        "vibrato_tickCounterStart",
    );
    test_channel_soa(
        addresses::CHANNEL_VIBRATO_HALF_WAVELENGTH,
        "vibrato_halfWaveLengthInTicks",
    );
    test_channel_soa(addresses::CHANNEL_PREV_TEMP_GAIN, "prevTempGain");
    test_channel_soa(addresses::CHANNEL_EARLY_RELEASE_CMP, "earlyRelease_cmp");
    test_channel_soa(
        addresses::CHANNEL_EARLY_RELEASE_MIN_TICKS,
        "earlyRelease_minTicks",
    );
    test_channel_soa(addresses::CHANNEL_EARLY_RELEASE_GAIN, "earlyRelease_gain");

    test_channel_soa(addresses::CHANNEL_DETUNE_L, "detune_l");
    test_channel_soa(addresses::CHANNEL_DETUNE_H, "detune_h");

    for v in 0..N_MUSIC_CHANNELS {
        assert_eq!(
            read_ticks_until_next_bytecode(int_apuram, v),
            read_ticks_until_next_bytecode(emu_apuram, v),
            "ticks until next bytecode mismatch (tick_count: {tick_count}, voice: {v})"
        );
    }

    test_range(
        addresses::BYTECODE_STACK,
        BC_TOTAL_STACK_SIZE,
        "bytecode stack",
    );
    test_range(addresses::ECHO_VARIABLES, ECHO_VARIABLES_SIZE, "echo");

    test_byte(addresses::MAX_EDL, "maxEdl");
}

fn read_ptrs(apuram: &[u8; 0x10000], addr_l: u16, addr_h: u16) -> [Option<u16>; N_MUSIC_CHANNELS] {
    std::array::from_fn(|i| {
        let h = apuram[usize::from(addr_h) + i];
        if h != 0 {
            let l = apuram[usize::from(addr_l) + i];
            Some(u16::from_le_bytes([l, h]))
        } else {
            None
        }
    })
}

// returns u32::MAX if the channel is disabled
fn read_ticks_until_next_bytecode(apuram: &[u8; 0x10000], v: usize) -> u32 {
    assert!(v < N_MUSIC_CHANNELS);
    let read = |addr: u16| apuram[usize::from(addr) + v];

    if read(addresses::CHANNEL_INSTRUCTION_PTR_H) > addresses::COMMON_DATA.to_le_bytes()[1] {
        // Channel is not disabled.

        let countdown_timer = u16::from_le_bytes([
            read(addresses::CHANNEL_COUNTDOWN_TIMER_L),
            read(addresses::CHANNEL_COUNTDOWN_TIMER_H),
        ]);

        u32::from(countdown_timer)
    } else {
        u32::MAX
    }
}

fn song_ticks(song: &SongData) -> TickCounter {
    song.channels()
        .iter()
        .filter_map(|c| c.as_ref())
        .map(|c| c.tick_counter)
        .max()
        .unwrap_or_default()
}

fn test_bc_intrepreter(song: SongData, common_audio_data: &CommonAudioData) {
    // Run the song at the fastest tick clock.
    let mut song = song;
    override_song_tick_clock(&mut song, TickClock::MIN);
    let song = song;

    let audio_mode = AudioMode::Surround;

    let song_addr = common_audio_data.min_song_data_addr();

    // Clamp to ensure 16 bit tick_counter does not overflow
    // +30 ticks to test for song looping
    let ticks_to_test = song_ticks(&song).clamp(TickCounter::new(192), TickCounter::new(0x8000))
        + TickCounter::new(30);

    let mut emu = TadEmulator::new();
    emu.fill_apuram(bytecode_interpreter::UNINITIALISED);
    emu.load_song(common_audio_data, &song, None, audio_mode)
        .unwrap();

    // Wait for the audio-driver to finish initialization
    emu.emulate();
    emu.emulate();

    let dummy_emu_init = Box::from(DummyEmu {
        apuram: *emu.apuram(),
    });

    // Add a second limit, so it doesn't emulate a very very long (119 minute) song
    for _ in 0..1000 {
        emu.try_send_io_command(io_commands::UNPAUSE, 0, 0);
        for _ in 0..10 {
            emu.emulate();
        }

        assert!(emu.is_io_command_acknowledged(), "audio driver not playing");

        // Pausing the emulator ensures all bytecode instructions have completed
        emu.try_send_io_command(io_commands::PAUSE, 0, 0);
        emu.emulate();
        emu.emulate();

        assert!(emu.is_io_command_acknowledged(), "audio driver not paused");

        const STC: usize = addresses::SONG_TICK_COUNTER as usize;
        let tick_count = u16::from_le_bytes(emu.apuram()[STC..STC + 2].try_into().unwrap());

        let tick_count = TickCounter::new(tick_count.into());

        let mut interpreter =
            SongInterpreter::new_uninitialised(common_audio_data, &song, song_addr, audio_mode);
        let valid = interpreter.process_ticks(tick_count);
        assert!(valid, "SongIntreperter time out");
        assert_bc_intrepreter_matches_emu(&interpreter, &dummy_emu_init, &emu, tick_count);

        if tick_count > ticks_to_test {
            break;
        }
    }
}

fn main() {
    let mut args = std::env::args_os();

    if args.len() != 2 {
        panic!("Expected a single argument: project file");
    }
    let pf_path = PathBuf::from(args.nth(1).unwrap());

    let project = load_project_file(&pf_path).unwrap();
    let project = validate_project_file_names(project).unwrap();

    let samples = build_sample_and_instrument_data(&project).unwrap();

    let sfx_subs = CompiledSfxSubroutines::blank();
    let sfx = blank_compiled_sound_effects();

    let common_audio_data = build_common_audio_data(&samples, &sfx_subs, &sfx).unwrap();

    for song in project.songs.list() {
        println!("Testing song: {}", song.name);

        let mml_file = load_text_file_with_limit(&song.source, &project.parent_path).unwrap();

        let song_data = compile_mml_song(
            &mml_file.contents,
            &mml_file.file_name,
            Some(song.name.clone()),
            &project.instruments_and_samples,
            samples.pitch_table(),
        )
        .unwrap();

        test_bc_intrepreter(song_data, &common_audio_data);
    }
}
