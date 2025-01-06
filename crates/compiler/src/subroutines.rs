//! Subroutines

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::mml::IdentifierBuf;

pub use crate::bytecode::SubroutineId;

#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine {
    pub identifier: IdentifierBuf,
    pub subroutine_id: SubroutineId,
    pub bytecode_offset: u16,
    pub changes_song_tempo: bool,
}

pub enum FindSubroutineResult<'a> {
    Found(&'a SubroutineId),
    NotCompiled,
    Recussion,
    NotFound,
    NotAllowed,
}

pub trait SubroutineStore {
    fn get(&self, index: usize) -> Option<&Subroutine>;

    fn find_subroutine<'a, 'b>(&'a self, name: &'b str) -> FindSubroutineResult<'b>
    where
        'a: 'b;
}

pub struct NoSubroutines();

impl SubroutineStore for NoSubroutines {
    fn get(&self, _: usize) -> Option<&Subroutine> {
        None
    }

    fn find_subroutine<'a, 'b>(&'a self, _: &'b str) -> FindSubroutineResult<'b>
    where
        'a: 'b,
    {
        FindSubroutineResult::NotAllowed
    }
}
