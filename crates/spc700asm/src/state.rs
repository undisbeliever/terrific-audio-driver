//! Assembler state

use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
#[allow(dead_code)] // ::TODO remove::
pub enum SymbolError {
    InvalidSymbol(String),
    DuplicateSymbol,
}

pub const MAX_ABS_BIT_ADDR: u16 = u16::MAX >> 3;

#[derive(Debug, PartialEq)]
#[allow(dead_code)] // ::TODO remove::
pub struct AbsBitOutOfRangeError(pub u16);

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

#[derive(Debug, PartialEq)]
pub enum U8Value {
    Known(u8),
    Unknown(String),
}

#[derive(Debug, PartialEq)]
pub enum U16Value {
    Known(u16),
    Unknown(String),
}

#[derive(Debug, PartialEq)]
pub struct BitArgument(u8);

impl TryFrom<u16> for BitArgument {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        if (0..=7).contains(&value) {
            Ok(Self(u8::try_from(value).unwrap()))
        } else {
            Err(())
        }
    }
}

impl TryFrom<i64> for BitArgument {
    type Error = ();

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if (0..=7).contains(&value) {
            Ok(Self(u8::try_from(value).unwrap()))
        } else {
            Err(())
        }
    }
}

impl BitArgument {
    pub fn to_opcode(&self, base: u8) -> u8 {
        base | (self.0 << 5)
    }
}

#[derive(Default)]
pub struct State {
    pub direct_page: DirectPageFlag,

    symbols: HashMap<String, i64>,
    output: Vec<u8>,
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

    #[allow(dead_code)] // ::TODO remove::
    pub fn output(&self) -> &[u8] {
        &self.output
    }

    pub fn dp_addr_to_addr(&self, dp: u8) -> u16 {
        match self.direct_page {
            DirectPageFlag::Zero => u16::from_le_bytes([dp, 0]),
            DirectPageFlag::One => u16::from_le_bytes([dp, 1]),
        }
    }

    pub fn write_u8(&mut self, value: u8) {
        self.output.push(value);
    }

    pub fn write_u16(&mut self, value: u16) {
        self.output.extend(value.to_le_bytes());
    }

    pub fn write_u8v(&mut self, value: U8Value) {
        match value {
            U8Value::Known(v) => {
                self.output.push(v);
            }
            U8Value::Unknown(_expression) => {
                // ::TODO store `expression`::
                self.output.push(0);
            }
        }
    }

    pub fn write_u16v(&mut self, value: U16Value) {
        match value {
            U16Value::Known(v) => {
                let [l, h] = v.to_le_bytes();

                self.output.extend(&[l, h]);
            }
            U16Value::Unknown(_expression) => {
                // ::TODO store `expression`::
                self.output.extend(&[0, 0]);
            }
        }
    }

    pub fn write_op_u8(&mut self, opcode: u8, value: u8) {
        self.output.push(opcode);
        self.output.push(value);
    }

    pub fn write_op_u8v(&mut self, opcode: u8, value: U8Value) {
        self.output.push(opcode);
        self.write_u8v(value);
    }

    pub fn write_op_u16v(&mut self, opcode: u8, value: U16Value) {
        self.output.push(opcode);
        self.write_u16v(value);
    }

    pub fn write_op_abs_bit(
        &mut self,
        opcode: u8,
        value: U16Value,
        bit: BitArgument,
    ) -> Result<(), AbsBitOutOfRangeError> {
        self.output.push(opcode);

        match value {
            U16Value::Known(addr) => {
                if addr <= MAX_ABS_BIT_ADDR {
                    let [l, h] = addr.to_le_bytes();

                    self.output.extend(&[l, h | (bit.0 << 5)]);
                    Ok(())
                } else {
                    // Populate dummy value so instruction length is correct
                    self.output.extend(&[0, 0]);

                    Err(AbsBitOutOfRangeError(addr))
                }
            }
            U16Value::Unknown(_expression) => {
                // ::TODO store `expression` and `bit`::

                self.output.extend(&[0, 0]);
                Ok(())
            }
        }
    }

    pub fn write_relative_goto(&mut self, expression: &str) {
        // ::TODO store `expression`::
        let _ = expression;

        self.output.push(0);
    }

    pub fn write_pcall_address(&mut self, expression: &str) {
        // ::TODO store `expression`::
        let _ = expression;

        self.output.push(0xff);
    }
}

// Only allow state cloning in unit tests
#[cfg(test)]
impl Clone for State {
    fn clone(&self) -> Self {
        Self {
            direct_page: self.direct_page.clone(),
            symbols: self.symbols.clone(),
            output: self.output.clone(),
        }
    }
}
