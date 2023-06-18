//! Audio Driver compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod bytecode;
mod bytecode_assembler;
mod envelope;
mod errors;
mod notes;
mod time;

pub use bytecode_assembler::BytecodeAssembler;
