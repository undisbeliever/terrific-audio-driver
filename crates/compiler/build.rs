//! Audio Driver build script

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

fn find_wiz_binary() -> PathBuf {
    // Using join to ensure directory separators are correct
    let wiz_dir = Path::new("..").join("..").join("wiz");
    let wiz_bin = wiz_dir.join("bin").join("wiz");

    if !wiz_bin.is_file() {
        panic!("Cannot find wiz binary.  Please import the wiz git submodule and compile wiz.");
    }

    wiz_bin
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
}

fn main() {
    let wiz = AudioDriverCompiler {
        audio_driver_dir: "../../audio-driver/src".into(),
        out_dir: env::var_os("OUT_DIR").unwrap().into(),
        wiz_bin: find_wiz_binary(),
    };

    wiz.compile("loader".as_ref());
    wiz.compile("audio-driver".as_ref());
    wiz.compile("blank-song".as_ref());

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", wiz.audio_driver_dir.display());
}
