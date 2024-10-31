//! Audio Driver compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

mod bytecode;
mod file_pos;
mod value_newtypes;

pub mod bytecode_assembler;
pub mod bytecode_interpreter;
pub mod common_audio_data;
pub mod data;
pub mod driver_constants;
pub mod echo;
pub mod envelope;
pub mod errors;
pub mod export;
pub mod mml;
pub mod notes;
pub mod path;
pub mod pitch_table;
pub mod samples;
pub mod sfx_file;
pub mod songs;
pub mod sound_effects;
pub mod spc_file_export;
pub mod time;

pub use bytecode::opcodes;
pub use file_pos::{FilePos, FilePosRange};

pub mod audio_driver {
    // SPDX-SnippetBegin

    // SDPX—SnippetName: SPC700 Audio Driver
    // SPDX-SnippetCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
    // SPDX-License-Identifier: Zlib
    pub const LOADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/loader.bin"));
    pub const AUDIO_DRIVER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/audio-driver.bin"));
    pub const BLANK_SONG: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/blank-song.bin"));

    // SPDX-SnippetEnd
}
