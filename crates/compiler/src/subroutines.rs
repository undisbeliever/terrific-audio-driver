//! Subroutines

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::mml::note_tracking::CommandTickTracker;
use crate::mml::{IdentifierBuf, IdentifierStr};

pub use crate::bytecode::SubroutineId;

#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine {
    pub identifier: IdentifierBuf,
    pub subroutine_id: SubroutineId,
    pub bytecode_offset: u16,
    pub bytecode_end_offset: u16,
    pub changes_song_tempo: bool,

    pub tick_tracker: CommandTickTracker,
}

#[derive(Debug, Clone)]
pub enum SubroutineState {
    Compiled(Subroutine),
    NotCompiled,
    CompileError,
}

pub enum GetSubroutineResult<'a> {
    NotFound,
    Compiled(&'a Subroutine),
    NotCompiled(IdentifierStr<'a>),
    CompileError(IdentifierStr<'a>),
}

pub trait SubroutineStore {
    fn get(&self, index: usize) -> GetSubroutineResult<'_>;

    fn find_subroutine(&self, name: &str) -> Option<u8>;
}

pub struct NoSubroutines();

impl SubroutineStore for NoSubroutines {
    fn get(&self, _: usize) -> GetSubroutineResult<'_> {
        GetSubroutineResult::NotFound
    }

    fn find_subroutine(&self, _: &str) -> Option<u8> {
        None
    }
}
