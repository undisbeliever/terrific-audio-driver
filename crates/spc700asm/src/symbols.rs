//! Assembler symbols

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug, PartialEq)]
pub enum SymbolError<'s> {
    InvalidName(&'s str),
    DuplicateSymbol(String),
}

impl std::fmt::Display for SymbolError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolError::InvalidName(name) => write!(f, "invalid name: {name}"),
            SymbolError::DuplicateSymbol(name) => write!(f, "duplicate symbol: {name}"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum DirectPageFlag {
    #[default]
    Zero,
    One,
}

pub fn is_symbol_start_character(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

pub fn is_symbol_character(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

pub fn is_identifier_character(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'.'
}

const INVALID_SYMBOL_NAMES: [&str; 16] = [
    "A", "X", "Y", "YA", "SP", "PSW", "C", "PC", "a", "x", "y", "ya", "sp", "psw", "c", "pc",
];

#[must_use]
pub fn is_symbol_name_valid(s: &str) -> bool {
    if !INVALID_SYMBOL_NAMES.contains(&s) {
        let mut it = s.bytes();
        let first = it.next().unwrap_or(0);
        is_symbol_start_character(first) && it.all(is_symbol_character)
    } else {
        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OldScope<'s>(Option<&'s str>);

impl OldScope<'_> {
    pub const NONE: Self = Self(None);
}

pub(crate) struct Symbols<'s> {
    direct_page: DirectPageFlag,

    scope: Option<&'s str>,

    symbols: HashMap<String, Option<i64>>,
    struct_offsets: HashMap<(&'s str, &'s str), u16>,

    // only used in repeat statements
    repeat_value: Option<(&'s str, i64)>,

    // only used in assert statements in `process_aserts()`
    assert_pc: Option<i64>,
}

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Self {
            direct_page: DirectPageFlag::Zero,
            scope: None,
            symbols: HashMap::new(),
            struct_offsets: HashMap::new(),
            repeat_value: None,
            assert_pc: None,
        }
    }

    pub fn take_symbols(self) -> HashMap<String, Option<i64>> {
        self.symbols
    }

    pub fn direct_page(&self) -> DirectPageFlag {
        self.direct_page
    }

    pub fn set_direct_page(&mut self, flag: DirectPageFlag) {
        self.direct_page = flag
    }

    pub fn set_repeat_value(&mut self, name: &'s str, value: i64) {
        self.repeat_value = Some((name, value));
    }

    pub fn clear_repeat_value(&mut self) {
        self.repeat_value = None;
    }

    pub fn assert_pc(&self) -> Option<i64> {
        self.assert_pc
    }

    pub fn set_assert_pc(&mut self, pc: i64) {
        self.assert_pc = Some(pc);
    }

    pub fn clear_assert_pc(&mut self) {
        self.assert_pc = None;
    }

    pub fn scope(&self) -> OldScope<'s> {
        OldScope(self.scope)
    }

    pub fn take_scope(&mut self) -> OldScope<'s> {
        OldScope(std::mem::take(&mut self.scope))
    }

    pub fn restore_scope(&mut self, old: OldScope<'s>) {
        self.scope = old.0
    }

    // CAUTION: Does not add proc to the symbol file.
    // CAUTION: Does not check if the scope has already been opened.
    pub fn open_scope(&mut self, name: &'s str, child_symbols: Vec<&'s str>) {
        for label in child_symbols {
            let full_name = [name, ".", label].concat();
            self.symbols.entry(full_name).or_insert(None);
        }

        self.scope = Some(name);
    }

    pub fn close_scope(&mut self) {
        self.scope = None;
    }

    fn add_unchecked_symbol(
        &mut self,
        full_name: String,
        value: i64,
    ) -> Result<(), SymbolError<'s>> {
        match self.symbols.entry(full_name) {
            Entry::Vacant(v) => {
                v.insert(Some(value));
                Ok(())
            }
            Entry::Occupied(mut v) => {
                if v.get().is_none() {
                    v.insert(Some(value));
                    Ok(())
                } else {
                    Err(SymbolError::DuplicateSymbol(v.key().clone()))
                }
            }
        }
    }

    /// Adds a scoped name to the symbols.
    ///
    /// **CAUTION**: Does not check if `parent` or `child_name` is valid
    pub fn add_unchecked_scoped_symbol(
        &mut self,
        parent: &str,
        child_name: &str,
        value: i64,
    ) -> Result<(), SymbolError<'s>> {
        let full_name = [parent, ".", child_name].concat();
        self.add_unchecked_symbol(full_name, value)
    }

    pub fn add_symbol(
        &mut self,
        name: &'s str,
        value: i64,
    ) -> Result<(String, i64), SymbolError<'s>> {
        match is_symbol_name_valid(name) {
            true => match &self.scope {
                Some(scope) => {
                    let full_name = [scope, ".", name].concat();
                    self.add_unchecked_symbol(full_name.clone(), value)
                        .map(|()| (full_name, value))
                }
                None => self
                    .add_unchecked_symbol(name.into(), value)
                    .map(|()| (name.into(), value)),
            },
            false => Err(SymbolError::InvalidName(name)),
        }
    }

    pub fn get_symbol(&self, name: &str) -> Option<i64> {
        if name == "PC" {
            return self.assert_pc;
        }

        if let Some((r_name, r_value)) = self.repeat_value {
            if name == r_name {
                return Some(r_value);
            }
        }

        match (self.scope, name.contains(".")) {
            (Some(scope), false) => {
                let full_name = [scope, ".", name].concat();

                match self.symbols.get(&full_name) {
                    Some(Some(v)) => Some(*v),
                    Some(None) => None,
                    None => self.symbols.get(name).copied().flatten(),
                }
            }
            (Some(_), true) | (None, _) => self.symbols.get(name).copied().flatten(),
        }
    }

    pub fn add_struct_offset(&mut self, struct_name: &'s str, field_name: &'s str, value: u16) {
        self.struct_offsets.insert((struct_name, field_name), value);
    }

    pub fn get_struct_offset(&self, struct_name: &'s str, field_name: &'s str) -> Option<i64> {
        self.struct_offsets
            .get(&(struct_name, field_name))
            .map(|v| i64::from(*v))
    }

    pub fn dp_addr_to_addr(&self, dp: u8) -> u16 {
        match self.direct_page {
            DirectPageFlag::Zero => u16::from_le_bytes([dp, 0]),
            DirectPageFlag::One => u16::from_le_bytes([dp, 1]),
        }
    }
}
