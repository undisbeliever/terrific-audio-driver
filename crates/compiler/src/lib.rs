//! Audio Driver compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod bytecode;
mod common_audio_data;
mod echo;
mod envelope;
mod file_pos;
mod mml_command_parser;
mod notes;
mod pitch_table;
mod songs;
mod spc_file_export;
mod time;
mod value_newtypes;

pub mod bytecode_assembler;
pub mod data;
pub mod driver_constants;
pub mod errors;
pub mod mml;
pub mod samples;
pub mod sound_effects;

pub use file_pos::{FilePos, FilePosRange};
pub use value_newtypes::ValueNewType;

pub use data::{
    load_project_file, validate_project_file_names, Name, ProjectFile, UniqueNamesProjectFile,
};
pub use envelope::{Adsr, Gain};
pub use notes::{Note, Octave, STARTING_OCTAVE};

pub use pitch_table::{build_pitch_table, PitchTable};
pub use samples::{build_sample_and_instrument_data, SampleAndInstrumentData};

pub use common_audio_data::{build_common_audio_data, CommonAudioData};

pub use mml::{compile_mml, MmlData};
pub use songs::{song_data, SongData};
pub use spc_file_export::export_spc_file;

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
