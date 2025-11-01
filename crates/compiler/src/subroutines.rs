//! Subroutines

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::bytecode;
use crate::command_compiler::commands::SubroutineCommands;
use crate::mml::note_tracking::CommandTickTracker;
use crate::mml::{IdentifierBuf, IdentifierStr};

#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine {
    pub index: u8,
    pub bc_state: bytecode::State,
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
    Compiled(IdentifierStr<'a>, &'a Subroutine),
    NotCompiled(IdentifierStr<'a>),
    CompileError(IdentifierStr<'a>),
}

#[derive(Debug, Clone)]
pub struct CompiledSubroutines(Vec<(IdentifierBuf, SubroutineState)>);

impl CompiledSubroutines {
    pub fn new_blank() -> Self {
        Self(Vec::new())
    }

    #[allow(clippy::new_without_default)]
    pub(crate) fn new(subroutines: &[SubroutineCommands]) -> Self {
        Self(
            subroutines
                .iter()
                .map(|s| (s.identifier.to_owned(), SubroutineState::NotCompiled))
                .collect(),
        )
    }

    pub(crate) fn store(&mut self, index: u8, state: SubroutineState) {
        let s = &mut self.0[usize::from(index)].1;
        assert!(matches!(s, SubroutineState::NotCompiled));

        *s = state;
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> GetSubroutineResult<'_> {
        match self.0.get(index) {
            Some((name, SubroutineState::Compiled(s))) => {
                GetSubroutineResult::Compiled(name.as_ref(), s)
            }
            Some((name, SubroutineState::NotCompiled)) => {
                GetSubroutineResult::NotCompiled(name.as_ref())
            }
            Some((name, SubroutineState::CompileError)) => {
                GetSubroutineResult::CompileError(name.as_ref())
            }
            None => GetSubroutineResult::NotFound,
        }
    }

    pub fn get_name(&self, index: u8) -> Option<&str> {
        match self.0.get(usize::from(index)) {
            Some((name, _)) => Some(name.as_str()),
            None => None,
        }
    }

    pub fn get_compiled(&self, index: u8) -> Option<&Subroutine> {
        match self.0.get(usize::from(index)) {
            Some((_, SubroutineState::Compiled(s))) => Some(s),
            Some((_, SubroutineState::NotCompiled)) => None,
            Some((_, SubroutineState::CompileError)) => None,
            None => None,
        }
    }

    pub fn as_slice(&self) -> &[(IdentifierBuf, SubroutineState)] {
        &self.0
    }

    pub fn iter(&self) -> impl Iterator<Item = &(IdentifierBuf, SubroutineState)> {
        self.0.iter()
    }

    pub fn iter_compiled(&self) -> impl Iterator<Item = &Subroutine> {
        self.0.iter().filter_map(|(_name, s)| match s {
            SubroutineState::Compiled(s) => Some(s),
            SubroutineState::CompileError => None,
            SubroutineState::NotCompiled => None,
        })
    }
}

pub trait SubroutineNameMap {
    fn find_subroutine_index(&self, name: &str) -> Option<u8>;
}

impl<'a> SubroutineNameMap for HashMap<IdentifierStr<'a>, usize> {
    fn find_subroutine_index(&self, name: &str) -> Option<u8> {
        let name = IdentifierStr::from_str(name);

        self.get(&name).map(|i| (*i).try_into().unwrap())
    }
}

pub struct BlankSubroutineMap;

impl SubroutineNameMap for BlankSubroutineMap {
    fn find_subroutine_index(&self, _: &str) -> Option<u8> {
        None
    }
}
