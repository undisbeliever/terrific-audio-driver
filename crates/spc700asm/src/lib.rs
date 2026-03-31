//! An spc700 assembler, used to compile the audio driver

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

mod assembler;
mod evaluator;
mod file_loader;
mod file_parser;
mod instructions;
mod state;
mod string;
mod symbol_file;

pub mod errors;
pub use assembler::{assemble, assemble_loaded_file, CompiledAsm};
pub use file_loader::load_asm_file_and_includes;
