//! Bytecode interpreter test
//!
//! Tests the bytecode interpreter matches the audio-driver by emulating the audio-driver and
//! repeatedly testing the bytecode_interpreter output matches emulator's memory.
//!
//! This is an example and not a test as:
//!    * test_bc_interpreter requires a command line input parameter (currently there are no MML examples in the repo)
//!    * test_bc_interpreter is slow and thorough, emulating the first few minutes of every song in the project file.

use compiler::{
    audio_driver,
    bytecode_interpreter::{self, SongInterpreter},
    common_audio_data::{build_common_audio_data, CommonAudioData},
    data::{load_project_file, load_text_file_with_limit, validate_project_file_names},
    driver_constants::{
        addresses, io_commands, LoaderDataType, BC_TOTAL_STACK_SIZE, N_CHANNELS,
        S_DSP_EON_REGISTER, S_SMP_TIMER_0_REGISTER,
    },
    mml::compile_mml,
    samples::build_sample_and_instrument_data,
    songs::SongData,
    sound_effects::blank_compiled_sound_effects,
    time::TickCounter,
};
use shvc_sound_emu::ShvcSoundEmu;

use std::path::PathBuf;

fn load_song(
    common_audio_data: &CommonAudioData,
    song: &SongData,
    stereo_flag: bool,
) -> ShvcSoundEmu {
    const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

    let mut emu = ShvcSoundEmu::new(&[0; 64]);

    let common_data = common_audio_data.data();
    let song_data = song.data();
    let song_data_addr = common_audio_data.song_data_addr();
    let edl = &song.metadata().echo_buffer.edl;

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

    // Reset echo buffer
    let eb_start = usize::from(song.metadata().echo_buffer.edl.echo_buffer_addr());
    let eb_end = eb_start + edl.buffer_size();
    apuram[eb_start..eb_end].fill(0);

    // Set loader flags
    apuram[LOADER_DATA_TYPE_ADDR] = LoaderDataType {
        stereo_flag,
        play_song: false,
        skip_echo_buffer_reset: true,
    }
    .driver_value();

    emu.set_echo_buffer_size(edl.esa_register(), edl.as_u8());

    emu.set_spc_registers(addresses::DRIVER_CODE, 0, 0, 0, 0, 0xff);

    emu
}

#[derive(Clone)]
struct DummyEmu {
    apuram: [u8; 0x10000],
    dsp_registers: [u8; 0x80],
}

impl bytecode_interpreter::Emulator for DummyEmu {
    fn apuram_mut(&mut self) -> &mut [u8; 0x10000] {
        &mut self.apuram
    }

    fn write_dsp_register(&mut self, addr: u8, value: u8) {
        self.dsp_registers[usize::from(addr)] = value;
    }

    fn write_smp_register(&mut self, addr: u8, value: u8) {
        self.apuram[usize::from(addr)] = value;
    }
}

fn assert_bc_intrepreter_matches_emu(
    to_test: &SongInterpreter<&CommonAudioData, &SongData>,
    dummy: &DummyEmu,
    emu: &ShvcSoundEmu,
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

    assert_eq!(
        intrepreter_memory.dsp_registers[usize::from(S_DSP_EON_REGISTER)],
        emu.dsp_registers()[usize::from(S_DSP_EON_REGISTER)],
        "EON S-DSP register mismatch (tick_count: {tick_count})"
    );

    let int_apuram = &intrepreter_memory.apuram;
    let emu_apuram = emu.apuram();

    assert_eq!(
        int_apuram[usize::from(S_SMP_TIMER_0_REGISTER)],
        emu_apuram[usize::from(S_SMP_TIMER_0_REGISTER)],
        "S-SMP TIMER0 register mismatch (tick_count: {tick_count})"
    );

    assert_eq!(
        int_apuram[usize::from(addresses::EON_SHADOW_MUSIC)],
        emu_apuram[usize::from(addresses::EON_SHADOW_MUSIC)],
        "eonShadow_music (tick_count: {tick_count})"
    );

    let test_channel_soa = |addr: u16, name: &'static str| {
        let addr = usize::from(addr);
        let range = addr..addr + N_CHANNELS;
        assert_eq!(
            int_apuram[range.clone()],
            emu_apuram[range],
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

    // Bytecode intrepreter clears temp gain **at the start** of the rest instructions.
    // Only testing temp-gain if the interpreter data is non-zero.
    {
        let addr = usize::from(addresses::CHANNEL_VC_TEMP_GAIN);
        let range = addr..addr + N_CHANNELS;

        let mut int_data: [u8; N_CHANNELS] = int_apuram[range.clone()].try_into().unwrap();
        let emu_data: [u8; N_CHANNELS] = emu_apuram[range].try_into().unwrap();

        for i in 0..N_CHANNELS {
            if int_data[i] == 0 {
                int_data[i] = emu_data[i]
            }
        }
        assert_eq!(
            int_data, emu_data,
            "channelsSoA.virtualChannels.tempGain mismatch (tick_count: {tick_count})"
        );
    }

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

    test_channel_soa(addresses::CHANNEL_VOLUME, "volume");
    test_channel_soa(
        addresses::CHANNEL_VOL_EFFECT_DIRECTION,
        "volEffect_direction",
    );
    test_channel_soa(addresses::CHANNEL_VOL_EFFECT_OFFSET_L, "volEffect_offset_l");
    test_channel_soa(addresses::CHANNEL_VOL_EFFECT_OFFSET_H, "volEffect_offset_h");

    // Not testing subVolume, bc_iterpreter value is invalid on overflow
    // Not testing counter, bc_interpreter counter is invalid on overflow

    test_channel_soa(addresses::CHANNEL_PAN, "pan");

    // Not testing portamento

    test_channel_soa(
        addresses::CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK,
        "vibrato_pitchOffsetPerTick",
    );
    test_channel_soa(
        addresses::CHANNEL_VIBRATO_DIRECTION_COMPARATOR,
        "vibrato_directionComparator",
    );
    // Not testing `vibrato_tickCounter`
    test_channel_soa(
        addresses::CHANNEL_VIBRATO_TICK_COUNTER_START,
        "vibrato_tickCounterStart",
    );
    test_channel_soa(
        addresses::CHANNEL_VIBRATO_WAVELENGTH_IN_TICKS,
        "vibrato_waveLengthInTicks",
    );
    test_channel_soa(addresses::CHANNEL_PREV_TEMP_GAIN, "prevTempGain");
    test_channel_soa(addresses::CHANNEL_EARLY_RELEASE_CMP, "earlyRelease_cmp");
    test_channel_soa(
        addresses::CHANNEL_EARLY_RELEASE_MIN_TICKS,
        "earlyRelease_minTicks",
    );
    test_channel_soa(addresses::CHANNEL_EARLY_RELEASE_GAIN, "earlyRelease_gain");

    for v in 0..N_CHANNELS {
        assert_eq!(
            read_ticks_until_next_bytecode(int_apuram, v),
            read_ticks_until_next_bytecode(emu_apuram, v),
            "ticks until next bytecode mismatch (tick_count: {tick_count}, voice: {v})"
        );
    }

    {
        let addr = usize::from(addresses::BYTECODE_STACK);
        let range = addr..addr + BC_TOTAL_STACK_SIZE;
        assert_eq!(
            int_apuram[range.clone()],
            emu_apuram[range],
            "bytecode stack mismatch (tick_count: {tick_count})"
        );
    }
}

fn read_ptrs(apuram: &[u8; 0x10000], addr_l: u16, addr_h: u16) -> [Option<u16>; N_CHANNELS] {
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
    assert!(v < N_CHANNELS);
    let read = |addr: u16| apuram[usize::from(addr) + v];

    if read(addresses::CHANNEL_INSTRUCTION_PTR_H) > addresses::COMMON_DATA.to_le_bytes()[1] {
        // Channel is not disabled.

        let countdown_timer: u32 = match read(addresses::CHANNEL_COUNTDOWN_TIMER) {
            0 => 0x100,
            c => u32::from(c),
        };
        let next_event_is_key_off: bool = read(addresses::CHANNEL_NEXT_EVENT_IS_KEY_OFF) != 0;

        countdown_timer + u32::from(next_event_is_key_off)
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

fn test_bc_intrepreter(song: &SongData, common_audio_data: &CommonAudioData) {
    const STEREO_FLAG: bool = true;

    // Clamp to ensure 16 bit tick_counter does not overflow
    // +30 ticks to test for song looping
    let ticks_to_test = song_ticks(song).clamp(TickCounter::new(192), TickCounter::new(0x8000))
        + TickCounter::new(30);

    let mut emu = load_song(common_audio_data, song, STEREO_FLAG);

    // Wait for the audio-driver to finish initialization
    emu.emulate();

    let dummy_emu_init = Box::from(DummyEmu {
        apuram: *emu.apuram(),
        dsp_registers: *emu.dsp_registers(),
    });

    // Add a second limit, so it doesn't emulate a very very long (119 minute) song
    for _ in 0..1000 {
        emu.write_io_ports([io_commands::UNPAUSE, 0, 0, 0]);
        for _ in 0..17 {
            emu.emulate();
        }

        // Pausing the emulator ensures all bytecode instructions have completed
        emu.write_io_ports([io_commands::PAUSE, 0, 0, 0]);
        emu.emulate();
        emu.emulate();

        // Confirm previous command was acknowledged by the audio driver.
        assert_eq!(
            emu.read_io_ports()[0],
            io_commands::PAUSE,
            "audio driver not paused"
        );

        const STC: usize = addresses::SONG_TICK_COUNTER as usize;
        let tick_count = u16::from_le_bytes(emu.apuram()[STC..STC + 2].try_into().unwrap());

        let tick_count = TickCounter::new(tick_count.into());

        let mut interpreter = SongInterpreter::new(common_audio_data, song, STEREO_FLAG);
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

    let sfx = blank_compiled_sound_effects();

    let common_audio_data = build_common_audio_data(&samples, &sfx).unwrap();

    for song in project.songs.list() {
        println!("Testing song: {}", song.name);

        let mml_file = load_text_file_with_limit(&song.source, &project.parent_path).unwrap();

        let song_data = compile_mml(
            &mml_file,
            Some(song.name.clone()),
            &project.instruments_and_samples,
            samples.pitch_table(),
        )
        .unwrap();

        test_bc_intrepreter(&song_data, &common_audio_data);
    }
}
