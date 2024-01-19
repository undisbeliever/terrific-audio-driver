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

const LOADER: &str = "loader";
const AUDIO_DRIVER: &str = "audio-driver";
const BLANK_SONG: &str = "blank-song";

const COMMON_SYMBOLS: &[&str] = &["__loader_dataType", "__loader_songPtr"];

const LOADER_SYMBOLS: &[(&str, &str)] = &[("start_loader", "LOADER")];

#[rustfmt::skip]
const AUDIO_DRIVER_SYMBOLS: &[(&str, &str)] = &[
    ("main", "DRIVER_CODE"),
    ("__loader_songPtr", "SONG_PTR"),
    ("__loader_dataType", "LOADER_DATA_TYPE"),
    ("ChannelDisabledBytecode", "CHANNEL_DISABLED_BYTECODE"),
    ("pausedIfZero", "PAUSED_IF_ZERO"),
    ("songTickCounter", "SONG_TICK_COUNTER"),
    ("eonShadow_music", "EON_SHADOW_MUSIC"),
    ("eonShadow_sfx", "EON_SHADOW_SFX"),
    ("enabledChannelsMask", "ENABLED_CHANNELS_MASK"),

    ("channelSoA.virtualChannels.updateState", "CHANNEL_VC_UPDATE_STATE"),
    ("channelSoA.virtualChannels.vol_l", "CHANNEL_VC_VOL_L"),
    ("channelSoA.virtualChannels.vol_r", "CHANNEL_VC_VOL_R"),
    ("channelSoA.virtualChannels.pitch_l", "CHANNEL_VC_PITCH_L"),
    ("channelSoA.virtualChannels.pitch_h", "CHANNEL_VC_PITCH_H"),
    ("channelSoA.virtualChannels.scrn", "CHANNEL_VC_SCRN"),
    ("channelSoA.virtualChannels.adsr1", "CHANNEL_VC_ADSR1"),
    ("channelSoA.virtualChannels.adsr2OrGain", "CHANNEL_VC_ADSR2_OR_GAIN"),

    ("channelSoA.countdownTimer", "CHANNEL_COUNTDOWN_TIMER"),
    ("channelSoA.instPitchOffset", "CHANNEL_INST_PITCH_OFFSET"),
    ("channelSoA.instructionPtr_l", "CHANNEL_INSTRUCTION_PTR_L"),
    ("channelSoA.instructionPtr_h", "CHANNEL_INSTRUCTION_PTR_H"),
    ("channelSoA.loopState_counter", "CHANNEL_LOOP_STATE_COUNTER"),
    ("channelSoA.loopState_loopPoint_l", "CHANNEL_LOOP_STATE_LOOP_POINT_L"),
    ("channelSoA.loopState_loopPoint_h", "CHANNEL_LOOP_STATE_LOOP_POINT_H"),
    ("channelSoA.nextEventIsKeyOff", "CHANNEL_NEXT_EVENT_IS_KEY_OFF"),
    ("channelSoA.pan", "CHANNEL_PAN"),
    ("channelSoA.portamento_direction", "CHANNEL_DIRECTION"),
    ("channelSoA.returnInstPtr_l", "CHANNEL_RETURN_INST_PTR_L"),
    ("channelSoA.returnInstPtr_h", "CHANNEL_RETURN_INST_PTR_H"),
    ("channelSoA.vibrato_pitchOffsetPerTick", "CHANNEL_VIBRATO_PITCH_OFFSET_PER_TICK"),
    ("channelSoA.vibrato_directionComparator", "CHANNEL_VIBRATO_DIRECTION_COMPARATOR"),
    ("channelSoA.vibrato_tickCounter", "CHANNEL_VIBRATO_TICK_COUNTER"),
    ("channelSoA.vibrato_tickCounterStart", "CHANNEL_VIBRATO_TICK_COUNTER_START"),
    ("channelSoA.vibrato_waveLengthInTicks", "CHANNEL_VIBRATO_WAVELENGTH_IN_TICKS"),
    ("channelSoA.volume", "CHANNEL_VOLUME"),
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

    fn last_addr(&self) -> u16 {
        return self.symbols.values().max().cloned().unwrap_or(0);
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

fn build_rust_consts(
    symbols: Symbols,
    const_list: &[(&str, &str)],
    last_symbol_name: &str,
) -> String {
    let mut out = String::new();

    for (s, const_name) in const_list {
        let addr: u16 = symbols.get(s);

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
    wiz.compile(BLANK_SONG.as_ref());

    let loader_sym_file = wiz.load_sym_file(LOADER.as_ref());
    let driver_sym_file = wiz.load_sym_file(AUDIO_DRIVER.as_ref());

    let loader_symbols = read_symbol_file("loader", &loader_sym_file);
    let driver_symbols = read_symbol_file("audio-driver", &driver_sym_file);

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
        &build_rust_consts(loader_symbols, LOADER_SYMBOLS, "_LAST_LOADER_SYMBOL"),
        &build_rust_consts(driver_symbols, AUDIO_DRIVER_SYMBOLS, "_LAST_DRIVER_SYMBOL"),
    ]
    .concat();

    fs::write(symbols_rs_path, rust_symbol_consts.as_bytes()).unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", wiz.audio_driver_dir.display());
}
