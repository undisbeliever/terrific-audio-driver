//! Assembler state

use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
#[allow(dead_code)] // ::TODO remove::
pub enum SymbolError {
    InvalidSymbol(String),
    DuplicateSymbol,
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum DirectPageFlag {
    #[default]
    Zero,
    One,
}

pub fn is_symbol_character(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

pub fn is_symbol_name_valid(s: &str) -> bool {
    let mut it = s.bytes();

    it.next().unwrap_or(0).is_ascii_alphabetic() && it.all(is_symbol_character)
}

#[derive(Default)]
pub struct State {
    pub direct_page: DirectPageFlag,
    symbols: HashMap<String, i64>,
}

impl State {
    #[allow(dead_code)] // ::TODO remove::
    pub fn add_symbol(&mut self, name: impl Into<String>, value: i64) -> Result<(), SymbolError> {
        let name = name.into();

        match is_symbol_name_valid(&name) {
            true => match self.symbols.entry(name) {
                Entry::Vacant(v) => {
                    v.insert(value);
                    Ok(())
                }
                Entry::Occupied(_) => Err(SymbolError::DuplicateSymbol),
            },
            false => Err(SymbolError::InvalidSymbol(name)),
        }
    }

    pub fn get_symbol(&self, name: &str) -> Option<i64> {
        self.symbols.get(name).copied()
    }
}
