//! Audio Driver compiler build script

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use spc700asm::{assemble_loaded_file, load_asm_file_and_includes, CompiledAsm};
use std::{
    env,
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

const AUDIO_DRIVER_DIR: &str = "../../audio-driver/src";

const COMMON_SYMBOLS: &[&str] = &[
    "LOADER_ADDR",
    "CODE_ADDR",
    "TAD_IO_VERSION",
    "loaderDataType",
    "songPtr",
    "globalVolume_music",
    "globalVolume_sfx",
];

const LOADER_SYMBOLS: &[(&str, &str)] = &[("start_loader", "LOADER")];

#[rustfmt::skip]
const AUDIO_DRIVER_SYMBOLS: &[(&str, &str)] = &[
    ("TAD_IO_VERSION", "TAD_IO_VERSION"),

    ("main", "DRIVER_CODE"),
    ("mainloop", "MAINLOOP_CODE"),
    ("process_music_channels", "PROCESS_MUSIC_CHANNELS_CODE"),
    ("songPtr", "SONG_PTR"),
    ("loaderDataType", "LOADER_DATA_TYPE"),
    ("lagDetector", "LAG_DETECTOR"),
    ("songTickCounter", "SONG_TICK_COUNTER"),
    ("keyOnShadow_music", "KEYON_SHADOW_MUSIC"),
    ("keyOffShadow_music", "KEYOFF_SHADOW_MUSIC"),
    ("keyOnMask_music", "KEYON_MASK_MUSIC"),
    ("pmonShadow", "PMON_SHADOW"),
    ("eonShadow_music", "EON_SHADOW_MUSIC"),
    ("eonShadow_sfx", "EON_SHADOW_SFX"),
    ("io_musicChannelsMask", "IO_MUSIC_CHANNELS_MASK"),
    ("__bcStack", "BYTECODE_STACK"),

    ("voiceChannelsDirty_music", "VOICE_CHANNELS_DIRTY_MUSIC"),
    ("channelSoA_virtualChannels_vol_l", "CHANNEL_VC_VOL_L"),
    ("channelSoA_virtualChannels_vol_r", "CHANNEL_VC_VOL_R"),
    ("channelSoA_virtualChannels_pitch_l", "CHANNEL_VC_PITCH_L"),
    ("channelSoA_virtualChannels_pitch_h", "CHANNEL_VC_PITCH_H"),
    ("channelSoA_virtualChannels_scrn", "CHANNEL_VC_SCRN"),
    ("channelSoA_virtualChannels_adsr1", "CHANNEL_VC_ADSR1"),
    ("channelSoA_virtualChannels_adsr2OrGain", "CHANNEL_VC_ADSR2_OR_GAIN"),
    ("channelSoA_virtualChannels_tempGain", "CHANNEL_VC_TEMP_GAIN"),

    ("channelSoA_countdownTimer_l", "CHANNEL_COUNTDOWN_TIMER_L"),
    ("channelSoA_countdownTimer_h", "CHANNEL_COUNTDOWN_TIMER_H"),
    ("channelSoA_instPitchOffset", "CHANNEL_INST_PITCH_OFFSET"),
    ("channelSoA_instructionPtr_l", "CHANNEL_INSTRUCTION_PTR_L"),
    ("channelSoA_instructionPtr_h", "CHANNEL_INSTRUCTION_PTR_H"),
    ("channelSoA_stackPointer", "CHANNEL_STACK_POINTER"),
    ("channelSoA_loopStackPointer", "CHANNEL_LOOP_STACK_POINTER"),
    ("channelSoA_keyoffMsbFlag", "CHANNEL_KEYOFF_MSB_FLAG"),
    ("channelSoA_portamento_direction", "CHANNEL_DIRECTION"),
    ("channelSoA_vibrato_pitchOffsetPerTick", "CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK"),
    ("channelSoA_vibrato_delay", "CHANNEL_VIBRATO_DELAY"),
    ("channelSoA_vibrato_direction", "CHANNEL_VIBRATO_DIRECTION"),
    ("channelSoA_vibrato_tickCounter", "CHANNEL_VIBRATO_TICK_COUNTER"),
    ("channelSoA_vibrato_tickCounterStart", "CHANNEL_VIBRATO_TICK_COUNTER_START"),
    ("channelSoA_vibrato_halfWavelength", "CHANNEL_VIBRATO_HALF_WAVELENGTH"),
    ("channelSoA_prevTempGain", "CHANNEL_PREV_TEMP_GAIN"),
    ("channelSoA_earlyRelease_cmp", "CHANNEL_EARLY_RELEASE_CMP"),
    ("channelSoA_earlyRelease_minTicks", "CHANNEL_EARLY_RELEASE_MIN_TICKS"),
    ("channelSoA_earlyRelease_gain", "CHANNEL_EARLY_RELEASE_GAIN"),
    ("channelSoA_transpose", "CHANNEL_TRANSPOSE"),
    ("channelSoA_detune_l", "CHANNEL_DETUNE_L"),
    ("channelSoA_detune_h", "CHANNEL_DETUNE_H"),
    ("channelSoA_volume", "CHANNEL_VOLUME"),
    ("channelSoA_subVolume", "CHANNEL_SUB_VOLUME"),
    ("channelSoA_volEffect_direction", "CHANNEL_VOL_EFFECT_DIRECTION"),
    ("channelSoA_volEffect_offset_l", "CHANNEL_VOL_EFFECT_OFFSET_L"),
    ("channelSoA_volEffect_offset_h", "CHANNEL_VOL_EFFECT_OFFSET_H"),
    ("channelSoA_volEffect_counter", "CHANNEL_VOL_EFFECT_COUNTER"),
    ("channelSoA_volEffect_halfWavelength", "CHANNEL_VOL_EFFECT_HALF_WAVELENGTH"),
    ("channelSoA_pan", "CHANNEL_PAN"),
    ("channelSoA_subPan", "CHANNEL_SUB_PAN"),
    ("channelSoA_panEffect_direction", "CHANNEL_PAN_EFFECT_DIRECTION"),
    ("channelSoA_panEffect_offset_l", "CHANNEL_PAN_EFFECT_OFFSET_L"),
    ("channelSoA_panEffect_offset_h", "CHANNEL_PAN_EFFECT_OFFSET_H"),
    ("channelSoA_panEffect_counter", "CHANNEL_PAN_EFFECT_COUNTER"),
    ("channelSoA_panEffect_halfWavelength", "CHANNEL_PAN_EFFECT_HALF_WAVELENGTH"),
    ("channelSoA_invertFlags", "CHANNEL_INVERT_FLAGS"),

    ("echo", "ECHO_VARIABLES"),
    ("commonData", "COMMON_DATA_POINTERS"),
    ("echoDirty", "ECHO_DIRTY"),
    ("maxEdl", "MAX_EDL"),

    ("globalVolume_music", "GLOBAL_VOLUME_MUSIC"),
    ("globalVolume_sfx", "GLOBAL_VOLUME_SFX"),

    ("main.ClearEchoBufferStart", "MAIN_CLEAR_ECHO_BUFFER_START"),
    ("main.ClearEchoBufferEnd", "MAIN_CLEAR_ECHO_BUFFER_END"),
];

fn assemble_file(filename: &Path) -> CompiledAsm {
    let full_path = Path::new(AUDIO_DRIVER_DIR).join(filename);

    let asm_file = match load_asm_file_and_includes(&full_path) {
        Ok(f) => f,
        Err(e) => {
            panic!("{}", e.display());
        }
    };

    match assemble_loaded_file(&asm_file) {
        Ok(c) => c,
        Err(e) => {
            panic!("{}", e.display(&asm_file));
        }
    }
}

fn assert_valid_rust_const_name(name: &str) {
    if !name
        .chars()
        .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
    {
        panic!("Invalid const name: {name}");
    }
}

fn build_rust_consts(c: &CompiledAsm, const_list: &[(&str, &str)], end_name: &str) -> String {
    let mut out = String::new();

    for (s, const_name) in const_list {
        let addr = u16::try_from(c.sym(s)).unwrap_or_else(|_| panic!("symbol {s:?} is not 16 bit"));

        assert_valid_rust_const_name(const_name);
        writeln!(out, "pub(crate) const {} : u16 = 0x{:x};", const_name, addr).unwrap();
    }

    assert_valid_rust_const_name(end_name);
    writeln!(
        out,
        "pub(crate) const {} : u16 = 0x{:x};",
        end_name, c.end_addr
    )
    .unwrap();

    out
}

fn assert_common_symbols_match(loader: &CompiledAsm, audio_driver: &CompiledAsm) {
    for label in COMMON_SYMBOLS {
        assert_eq!(
            loader.sym(label),
            audio_driver.sym(label),
            "symbol mismatch between loader and driver: {label}"
        );
    }
}

fn main() {
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let audio_driver_dir = Path::new(AUDIO_DRIVER_DIR);

    let loader = assemble_file(Path::new("loader.asm"));
    let audio_driver = assemble_file(Path::new("audio-driver.asm"));

    assert_common_symbols_match(&loader, &audio_driver);

    let rust_symbol_consts = [
        "// autogenerated by build.rs\n\n",
        &build_rust_consts(&loader, LOADER_SYMBOLS, "_LOADER_END_ADDR"),
        &build_rust_consts(&audio_driver, AUDIO_DRIVER_SYMBOLS, "_DRIVER_END_ADDR"),
    ]
    .concat();

    fs::write(out_dir.join("loader.bin"), loader.output).unwrap();
    fs::write(out_dir.join("audio-driver.bin"), audio_driver.output).unwrap();
    fs::write(out_dir.join("symbols.rs"), rust_symbol_consts.as_bytes()).unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", audio_driver_dir.display());
}
