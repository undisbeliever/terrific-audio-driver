//! Audio Driver compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod bytecode;
mod bytecode_assembler;
mod data;
mod driver_constants;
mod envelope;
mod errors;
mod notes;
mod pitch_table;
mod samples;
mod time;

pub use bytecode_assembler::BytecodeAssembler;
pub use data::{load_mappings_file, MappingsFile};

pub use pitch_table::build_pitch_table;
