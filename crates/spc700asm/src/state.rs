//! Assembler state

use crate::evaluator::{evaluate, ExpressionError, ExpressionResult};

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
pub enum OutputError {
    AbsBitOutOfRange(u16),
    ExpressionError(String, ExpressionError),
    NotANumber(String),
    ExpressionHasUnknownValue(String),

    OutOfRange { value: i64, min: i32, max: i32 },
    BranchOutOfRange(i64),
    InvalidPcall(i64),
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Clone)]
enum PendingOutput {
    U8,
    U16,
    AbsBit(BitArgument),
    RelativeBranch,
    Pcall,
}

pub struct State {
    pub direct_page: DirectPageFlag,

    symbols: HashMap<String, i64>,
    output: Vec<u8>,

    pc_base: u16,

    pending_output: Vec<(usize, String, PendingOutput)>,
}

impl State {
    #[allow(dead_code)] // ::TODO remove::
    pub fn new(pc_base: u16) -> Self {
        Self {
            direct_page: DirectPageFlag::Zero,
            symbols: HashMap::new(),
            output: Vec::new(),
            pc_base,
            pending_output: Vec::new(),
        }
    }

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

    fn push_pending_output(&mut self, expression: String, p: PendingOutput) {
        self.pending_output.push((self.output.len(), expression, p))
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
            U8Value::Unknown(expression) => {
                self.push_pending_output(expression, PendingOutput::U8);
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
            U16Value::Unknown(expression) => {
                self.push_pending_output(expression, PendingOutput::U16);
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
    ) -> Result<(), OutputError> {
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

                    Err(OutputError::AbsBitOutOfRange(addr))
                }
            }
            U16Value::Unknown(expression) => {
                self.push_pending_output(expression, PendingOutput::AbsBit(bit));
                self.output.extend(&[0, 0]);
                Ok(())
            }
        }
    }

    pub fn write_relative_goto(&mut self, expression: &str) {
        self.push_pending_output(expression.to_owned(), PendingOutput::RelativeBranch);

        self.output.push(0);
    }

    pub fn write_pcall_address(&mut self, expression: &str) {
        self.push_pending_output(expression.to_owned(), PendingOutput::Pcall);

        self.output.push(0xff);
    }
}

#[allow(dead_code)] // ::TODO remove::
pub fn process_pending_output_expressions(s: &mut State) -> Result<(), Vec<OutputError>> {
    let mut errors = Vec::new();

    for (pos, expr, p) in std::mem::take(&mut s.pending_output) {
        match evaluate(&expr, s) {
            ExpressionResult::Value(value) => match p {
                PendingOutput::U8 => match u8::try_from(value) {
                    Ok(v) => s.output[pos] = v,
                    Err(_) => errors.push(OutputError::OutOfRange {
                        value,
                        min: u8::MIN.into(),
                        max: u8::MAX.into(),
                    }),
                },
                PendingOutput::U16 => match u16::try_from(value) {
                    Ok(v) => {
                        s.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes());
                    }
                    Err(_) => errors.push(OutputError::OutOfRange {
                        value,
                        min: u16::MIN.into(),
                        max: u16::MAX.into(),
                    }),
                },
                PendingOutput::AbsBit(bit) => match u16::try_from(value) {
                    Ok(v) if v <= MAX_ABS_BIT_ADDR => {
                        let v = v | (u16::from(bit.0) << 13);
                        s.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes());
                    }
                    _ => errors.push(OutputError::OutOfRange {
                        value,
                        min: 0,
                        max: MAX_ABS_BIT_ADDR.into(),
                    }),
                },
                PendingOutput::RelativeBranch => {
                    let pc =
                        i64::try_from(pos + usize::from(s.pc_base) + 1).expect("output too big");

                    let value = value - pc;
                    match i8::try_from(value) {
                        Ok(v) => s.output[pos] = v.to_le_bytes()[0],
                        Err(_) => errors.push(OutputError::BranchOutOfRange(value)),
                    }
                }
                PendingOutput::Pcall => match u16::try_from(value) {
                    Ok(v) if v & 0xff00 == 0xff00 => {
                        s.output[pos] = v.to_le_bytes()[0];
                    }
                    _ => errors.push(OutputError::InvalidPcall(value)),
                },
            },
            crate::evaluator::ExpressionResult::Boolean(_) => {
                errors.push(OutputError::NotANumber(expr))
            }
            crate::evaluator::ExpressionResult::Unknown => {
                errors.push(OutputError::ExpressionHasUnknownValue(expr))
            }
            ExpressionResult::Error(e) => errors.push(OutputError::ExpressionError(expr, e)),
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// Only allow state cloning in unit tests
#[cfg(test)]
impl Clone for State {
    fn clone(&self) -> Self {
        Self {
            direct_page: self.direct_page.clone(),
            symbols: self.symbols.clone(),
            pc_base: self.pc_base,
            output: self.output.clone(),
            pending_output: self.pending_output.clone(),
        }
    }
}
