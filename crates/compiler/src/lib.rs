//! Audio Driver compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod bytecode;
mod bytecode_assembler;
mod common_audio_data;
mod data;
mod driver_constants;
mod envelope;
mod errors;
mod notes;
mod pitch_table;
mod samples;
mod sound_effects;
mod time;

pub use data::{load_mappings_file, MappingsFile};

pub use pitch_table::build_pitch_table;

pub use common_audio_data::{build_common_audio_data, compile_common_audio_data};
pub use sound_effects::{sfx_file_from_string, SoundEffectsFile};
