//! An spc700 assembler, used to compile the audio driver

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

mod assembler;
mod evaluator;
mod file_parser;
mod instructions;
mod state;
mod string;

pub mod errors;
pub use assembler::{assemble, CompiledAsm};
