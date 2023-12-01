//! Bytecode interpreter test
//!
//! Tests the bytecode interpreter matches the audio-driver by emulating the audio-driver and
//! repeatedly testing the bytecode_interpreter output matches emulator's memory.
//!
//! This is an example and not a test as:
//!    * test_bc_interpreter requires a command line input parameter (currently there are no MML examples in the repo)
//!    * test_bc_interpreter is slow and thorough, emulating the first few minutes of every song in the project file.

use compiler::{
    audio_driver, bytecode_interpreter,
    bytecode_interpreter::{interpret_song, InterpreterOutput},
    common_audio_data::{build_common_audio_data, CommonAudioData},
    data::{load_project_file, load_text_file_with_limit, validate_project_file_names},
    driver_constants::{
        addresses, io_commands, LoaderDataType, N_NESTED_LOOPS, N_VOICES, S_DSP_EON_REGISTER,
        S_SMP_TIMER_0_REGISTER,
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
    song_data_addr: u16,
) -> ShvcSoundEmu {
    const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

    let mut emu = ShvcSoundEmu::new(&[0; 64]);

    let song_data = song.data();
    let common_data = common_audio_data.data();
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
    to_test: InterpreterOutput,
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

    for i in 0..N_VOICES {
        let voice_offset = i * 0x10;

        let int_dsp: [u8; 8] = intrepreter_memory.dsp_registers[voice_offset..voice_offset + 8]
            .try_into()
            .unwrap();
        let emu_dsp: [u8; 8] = emu.dsp_registers()[voice_offset..voice_offset + 8]
            .try_into()
            .unwrap();

        // Not testing PITCH, bytecode_interpreter does not implement pitches.

        let vol_match = int_dsp[0] == emu_dsp[0] && int_dsp[1] == emu_dsp[1];
        let scrn_match = int_dsp[4] == emu_dsp[4];

        let envelope_match = envelope_from_dsp(&emu_dsp) == envelope_from_dsp(&int_dsp);

        assert!(vol_match && scrn_match && envelope_match,
                "voice DSP mismatch: tick_count: {tick_count}, voice: {i}\n  bc_int:{int_dsp:?}\n  ad_emu:{emu_dsp:?}");
    }

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

    let test_channel_soa = |addr: u16, name: &'static str| {
        let addr = usize::from(addr);
        let range = addr..addr + N_VOICES;
        assert_eq!(
            int_apuram[range.clone()],
            emu_apuram[range],
            "channelsSoA.{name} mismatch (tick_count: {tick_count})"
        );
    };

    test_channel_soa(addresses::CHANNEL_INSTRUCTION_PTR_L, "instructionPtr_l");
    test_channel_soa(addresses::CHANNEL_INSTRUCTION_PTR_H, "instructionPtr_h");

    test_channel_soa(addresses::CHANNEL_RETURN_INST_PTR_L, "returnInstPtr_l");
    test_channel_soa(addresses::CHANNEL_RETURN_INST_PTR_H, "returnInstPtr_h");

    test_channel_soa(addresses::CHANNEL_INST_PITCH_OFFSET, "instPitchOffset");

    test_channel_soa(addresses::CHANNEL_VOLUME, "volume");
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

    for loop_id in 0..N_NESTED_LOOPS {
        let loop_state_size = N_VOICES * 3;
        let addr = usize::from(addresses::CHANNEL_LOOP_STATE) + loop_id * loop_state_size;
        let end = addr + loop_state_size;

        assert_eq!(
            int_apuram[addr..end],
            emu_apuram[addr..end],
            "channelsSoA.LoopState[{loop_id}] mismatch (tick_count: {tick_count})"
        );
    }

    for v in 0..N_VOICES {
        assert_eq!(
            read_ticks_until_next_bytecode(int_apuram, v),
            read_ticks_until_next_bytecode(emu_apuram, v),
            "ticks until next bytecode mismatch (tick_count: {tick_count}, voice: {v})"
        );
    }
}

#[derive(PartialEq, Eq)]
enum Envelope {
    Adsr(u8, u8),
    Gain(u8),
}

// bytecode_interpreter does not store and manipulate the ADSR1 register
// Therefore I have to test `ADSR1.b7` to see if it is an ADSR or GAIN envelope
fn envelope_from_dsp(voice_dsp: &[u8; 8]) -> Envelope {
    let adsr1 = voice_dsp[5];
    let adsr2 = voice_dsp[6];
    let gain = voice_dsp[7];

    match adsr1 & 0x80 != 0 {
        true => Envelope::Adsr(adsr1, adsr2),
        false => Envelope::Gain(gain),
    }
}

// returns u32::MAX if the channel is disabled
fn read_ticks_until_next_bytecode(apuram: &[u8; 0x10000], v: usize) -> u32 {
    assert!(v < N_VOICES);
    let read = |addr: u16| apuram[usize::from(addr) + v];

    if read(addresses::CHANNEL_INSTRUCTION_PTR_H) < addresses::COMMON_DATA.to_le_bytes()[1] {
        // Channel is not disabled.

        let countdown_timer: u8 = read(addresses::CHANNEL_COUNTDOWN_TIMER);
        let next_event_is_key_off: bool =
            read(addresses::CHANNEL_NEXT_EVENT_IS_KEY_OFF) & 0x80 != 0;

        u32::from(countdown_timer) + u32::from(next_event_is_key_off)
    } else {
        u32::MAX
    }
}

fn song_ticks(song: &SongData) -> TickCounter {
    song.channels()
        .iter()
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

    let song_data_addr = usize::from(addresses::COMMON_DATA) + common_audio_data.data().len();
    let song_data_addr = u16::try_from(song_data_addr).unwrap();

    let mut emu = load_song(common_audio_data, song, STEREO_FLAG, song_data_addr);

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

        assert_eq!(
            emu.apuram()[usize::from(addresses::PAUSED_IF_ZERO)],
            0,
            "Audio driver is not paused"
        );

        const STC: usize = addresses::SONG_TICK_COUNTER as usize;
        let tick_count = u16::from_le_bytes(emu.apuram()[STC..STC + 2].try_into().unwrap());

        let tick_count = TickCounter::new(tick_count.into());

        let intrepreter = interpret_song(
            song,
            common_audio_data,
            STEREO_FLAG,
            song_data_addr,
            tick_count,
        )
        .unwrap();

        assert_bc_intrepreter_matches_emu(intrepreter, &dummy_emu_init, &emu, tick_count);

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
            &project.instruments,
            samples.pitch_table(),
        )
        .unwrap();

        test_bc_intrepreter(&song_data, &common_audio_data);
    }
}
