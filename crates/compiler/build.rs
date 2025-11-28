//! Audio Driver build script

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs};

use regex::Regex;

const LOADER: &str = "loader";
const AUDIO_DRIVER: &str = "audio-driver";

const COMMON_SYMBOLS: &[&str] = &[
    "__loader_dataType",
    "__loader_songPtr",
    "globalVolume_music",
    "globalVolume_sfx",
];

const LOADER_SYMBOLS: &[(&str, &str)] = &[("start_loader", "LOADER")];

const IO_COMMANDS_FILE: &str = "io-commands.wiz";
const IO_COMMANDS_VERSION_REGEX: &str = r"\nlet TAD_IO_VERSION = ([0-9]+);";
const TAD_IO_VERSION: &str = "TAD_IO_VERSION";

#[rustfmt::skip]
const AUDIO_DRIVER_SYMBOLS: &[(&str, &str)] = &[
    ("main", "DRIVER_CODE"),
    ("mainloop", "MAINLOOP_CODE"),
    ("process_music_channels", "PROCESS_MUSIC_CHANNELS_CODE"),
    ("__loader_songPtr", "SONG_PTR"),
    ("__loader_dataType", "LOADER_DATA_TYPE"),
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
    ("channelSoA.virtualChannels.vol_l", "CHANNEL_VC_VOL_L"),
    ("channelSoA.virtualChannels.vol_r", "CHANNEL_VC_VOL_R"),
    ("channelSoA.virtualChannels.pitch_l", "CHANNEL_VC_PITCH_L"),
    ("channelSoA.virtualChannels.pitch_h", "CHANNEL_VC_PITCH_H"),
    ("channelSoA.virtualChannels.scrn", "CHANNEL_VC_SCRN"),
    ("channelSoA.virtualChannels.adsr1", "CHANNEL_VC_ADSR1"),
    ("channelSoA.virtualChannels.adsr2OrGain", "CHANNEL_VC_ADSR2_OR_GAIN"),
    ("channelSoA.virtualChannels.tempGain", "CHANNEL_VC_TEMP_GAIN"),

    ("channelSoA.countdownTimer_l", "CHANNEL_COUNTDOWN_TIMER_L"),
    ("channelSoA.countdownTimer_h", "CHANNEL_COUNTDOWN_TIMER_H"),
    ("channelSoA.instPitchOffset", "CHANNEL_INST_PITCH_OFFSET"),
    ("channelSoA.instructionPtr_l", "CHANNEL_INSTRUCTION_PTR_L"),
    ("channelSoA.instructionPtr_h", "CHANNEL_INSTRUCTION_PTR_H"),
    ("channelSoA.stackPointer", "CHANNEL_STACK_POINTER"),
    ("channelSoA.loopStackPointer", "CHANNEL_LOOP_STACK_POINTER"),
    ("channelSoA.keyoffFlag", "CHANNEL_KEY_OFF_FLAG"),
    ("channelSoA.portamento_direction", "CHANNEL_DIRECTION"),
    ("channelSoA.vibrato_pitchOffsetPerTick", "CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK"),
    ("channelSoA.vibrato_delay", "CHANNEL_VIBRATO_DELAY"),
    ("channelSoA.vibrato_direction", "CHANNEL_VIBRATO_DIRECTION"),
    ("channelSoA.vibrato_tickCounter", "CHANNEL_VIBRATO_TICK_COUNTER"),
    ("channelSoA.vibrato_tickCounterStart", "CHANNEL_VIBRATO_TICK_COUNTER_START"),
    ("channelSoA.vibrato_halfWavelength", "CHANNEL_VIBRATO_HALF_WAVELENGTH"),
    ("channelSoA.prevTempGain", "CHANNEL_PREV_TEMP_GAIN"),
    ("channelSoA.earlyRelease_cmp", "CHANNEL_EARLY_RELEASE_CMP"),
    ("channelSoA.earlyRelease_minTicks", "CHANNEL_EARLY_RELEASE_MIN_TICKS"),
    ("channelSoA.earlyRelease_gain", "CHANNEL_EARLY_RELEASE_GAIN"),
    ("channelSoA.transpose", "CHANNEL_TRANSPOSE"),
    ("channelSoA.detune_l", "CHANNEL_DETUNE_L"),
    ("channelSoA.detune_h", "CHANNEL_DETUNE_H"),
    ("channelSoA.volume", "CHANNEL_VOLUME"),
    ("channelSoA.subVolume", "CHANNEL_SUB_VOLUME"),
    ("channelSoA.volEffect_direction", "CHANNEL_VOL_EFFECT_DIRECTION"),
    ("channelSoA.volEffect_offset_l", "CHANNEL_VOL_EFFECT_OFFSET_L"),
    ("channelSoA.volEffect_offset_h", "CHANNEL_VOL_EFFECT_OFFSET_H"),
    ("channelSoA.volEffect_counter", "CHANNEL_VOL_EFFECT_COUNTER"),
    ("channelSoA.volEffect_halfWavelength", "CHANNEL_VOL_EFFECT_HALF_WAVELENGTH"),
    ("channelSoA.pan", "CHANNEL_PAN"),
    ("channelSoA.subPan", "CHANNEL_SUB_PAN"),
    ("channelSoA.panEffect_direction", "CHANNEL_PAN_EFFECT_DIRECTION"),
    ("channelSoA.panEffect_offset_l", "CHANNEL_PAN_EFFECT_OFFSET_L"),
    ("channelSoA.panEffect_offset_h", "CHANNEL_PAN_EFFECT_OFFSET_H"),
    ("channelSoA.panEffect_counter", "CHANNEL_PAN_EFFECT_COUNTER"),
    ("channelSoA.panEffect_halfWavelength", "CHANNEL_PAN_EFFECT_HALF_WAVELENGTH"),
    ("channelSoA.invertFlags", "CHANNEL_INVERT_FLAGS"),

    ("echo", "ECHO_VARIABLES"),
    ("commonData", "COMMON_DATA_POINTERS"),
    ("echoDirty", "ECHO_DIRTY"),
    ("maxEdl", "MAX_EDL"),

    ("globalVolume_music", "GLOBAL_VOLUME_MUSIC"),
    ("globalVolume_sfx", "GLOBAL_VOLUME_SFX"),
];

#[rustfmt::skip]
const AUDIO_DRIVER_SYMBOLS_REGEX: &[(&str, &str)] = &[
    (r"__blk\d+.ClearEchoBufferStart", "MAIN_CLEAR_ECHO_BUFFER_START"),
    (r"__blk\d+.ClearEchoBufferEnd", "MAIN_CLEAR_ECHO_BUFFER_END"),
];

fn find_wiz_binary() -> PathBuf {
    // Using join to ensure directory separators are correct
    let wiz_exe = Path::new("wiz").join("bin").join("wiz");

    #[cfg(windows)]
    let wiz_exe = wiz_exe.with_extension("exe");

    let workspace_dir = Path::new("..").join("..");
    let wiz_bin = workspace_dir.join(&wiz_exe);

    if !wiz_bin.is_file() {
        panic!(
            r##"Cannot find wiz binary.  Please import the wiz git submodule and compile wiz.
This build script is looking for an executable at "{}"."##,
            wiz_exe.display()
        );
    }

    wiz_bin
}

fn read_sym_line(s: &str) -> Option<(&str, u16)> {
    if s.starts_with('[') {
        return None;
    }

    let (addr, name) = s.split_once(' ')?;
    let (bank, addr) = addr.split_once(':')?;

    match usize::from_str_radix(bank, 16) {
        Ok(0) => (),
        _ => panic!("Error in symbol file: {} bank is not 0", name),
    }

    match u16::from_str_radix(addr, 16) {
        Ok(a) => Some((name, a)),
        Err(_) => panic!("Error in symbol file: {} is not 16 bit", name),
    }
}

fn read_symbol_file<'a>(name: &'a str, sym_file: &'a str) -> Symbols<'a> {
    Symbols {
        name,
        symbols: sym_file.lines().filter_map(read_sym_line).collect(),
    }
}

struct Symbols<'a> {
    name: &'a str,
    symbols: HashMap<&'a str, u16>,
}

impl Symbols<'_> {
    fn get(&self, symbol: &str) -> u16 {
        match self.symbols.get(symbol) {
            Some(addr) => *addr,
            None => panic!("Cannot find {} symbol: {}", self.name, symbol),
        }
    }

    fn find_regex(&self, regex: &str) -> u16 {
        let re = Regex::new(regex).unwrap();

        let mut iter = self.symbols.iter().filter(|(n, _)| re.is_match(n));

        match iter.next() {
            None => panic!("Cannot find {} symbol regex: {}", self.name, regex),
            Some((_, v)) => {
                if iter.next().is_some() {
                    panic!(
                        "More than one {} symbol matches regex: {}",
                        self.name, regex
                    );
                }
                *v
            }
        }
    }

    fn last_addr(&self) -> u16 {
        self.symbols.values().max().cloned().unwrap_or(0)
    }
}

fn assert_valid_const_name(name: &str) {
    if !name
        .chars()
        .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
    {
        panic!("Invalid const name: {name}");
    }
}

fn read_tad_io_version(audio_driver_dir: &Path) -> usize {
    let path = audio_driver_dir.join(IO_COMMANDS_FILE);

    let io_commands = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => panic!("Error reading file {}: {}", path.display(), e),
    };

    let re = Regex::new(IO_COMMANDS_VERSION_REGEX).unwrap();

    match re.captures(&io_commands) {
        Some(c) => c[1].parse().unwrap(),
        None => panic!(
            "Could not find {TAD_IO_VERSION} const in {}.",
            path.display()
        ),
    }
}

fn build_rust_consts(
    symbols: Symbols,
    const_list: &[(&str, &str)],
    regex_list: &[(&str, &str)],
    last_symbol_name: &str,
) -> String {
    let mut out = String::new();

    for (s, const_name) in const_list {
        let addr: u16 = symbols.get(s);

        assert_valid_const_name(const_name);
        writeln!(out, "pub(crate) const {} : u16 = 0x{:x};", const_name, addr).unwrap();
    }

    for (s, const_name) in regex_list {
        let addr: u16 = symbols.find_regex(s);

        assert_valid_const_name(const_name);
        writeln!(out, "pub(crate) const {} : u16 = 0x{:x};", const_name, addr).unwrap();
    }

    let last_addr = symbols.last_addr();
    assert_valid_const_name(last_symbol_name);
    writeln!(
        out,
        "pub(crate) const {} : u16 = 0x{:x};",
        last_symbol_name, last_addr
    )
    .unwrap();

    out
}

struct AudioDriverCompiler {
    audio_driver_dir: PathBuf,
    out_dir: PathBuf,
    wiz_bin: PathBuf,
}

impl AudioDriverCompiler {
    fn compile(&self, name: &OsStr) {
        let in_path = self.audio_driver_dir.join(name).with_extension("wiz");
        let out_path = self.out_dir.join(name).with_extension("bin");

        let status = Command::new(&self.wiz_bin)
            .args(["--system=spc700", "-s", "wla", "-o"])
            .arg(&out_path)
            .arg(&in_path)
            .status()
            .expect("failed to execute wiz");

        if !status.success() {
            panic!("Error compiling {}: {}", in_path.display(), status);
        }
    }

    fn load_sym_file(&self, name: &OsStr) -> String {
        let sym_path = self.out_dir.join(name).with_extension("sym");

        match fs::read_to_string(&sym_path) {
            Ok(s) => s,
            Err(e) => panic!("Error reading symbol file {}: {}", sym_path.display(), e),
        }
    }
}

fn main() {
    let wiz = AudioDriverCompiler {
        audio_driver_dir: "../../audio-driver/src".into(),
        out_dir: env::var_os("OUT_DIR").unwrap().into(),
        wiz_bin: find_wiz_binary(),
    };

    wiz.compile(LOADER.as_ref());
    wiz.compile(AUDIO_DRIVER.as_ref());

    let loader_sym_file = wiz.load_sym_file(LOADER.as_ref());
    let driver_sym_file = wiz.load_sym_file(AUDIO_DRIVER.as_ref());

    let loader_symbols = read_symbol_file("loader", &loader_sym_file);
    let driver_symbols = read_symbol_file("audio-driver", &driver_sym_file);

    let tad_io_version = read_tad_io_version(&wiz.audio_driver_dir);

    // confirm common variables are the same in the loader and driver
    for label in COMMON_SYMBOLS {
        assert_eq!(
            loader_symbols.get(label),
            driver_symbols.get(label),
            "symbol mismatch between loader and driver: {label}"
        );
    }

    // Generate symbols.rs
    let symbols_rs_path = wiz.out_dir.join("symbols.rs");

    let rust_symbol_consts = [
        "// autogenerated by build.rs\n\n",
        &format!("pub(crate) const {TAD_IO_VERSION} : usize = {tad_io_version};\n\n"),
        &build_rust_consts(loader_symbols, LOADER_SYMBOLS, &[], "_LAST_LOADER_SYMBOL"),
        &build_rust_consts(
            driver_symbols,
            AUDIO_DRIVER_SYMBOLS,
            AUDIO_DRIVER_SYMBOLS_REGEX,
            "_LAST_DRIVER_SYMBOL",
        ),
    ]
    .concat();

    fs::write(symbols_rs_path, rust_symbol_consts.as_bytes()).unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", wiz.audio_driver_dir.display());
}
