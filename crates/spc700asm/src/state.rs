//! Assembler state

use crate::{
    errors::{FileErrors, LineNo},
    evaluator::{evaluate, ExpressionError, ExpressionResult},
};

use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug, PartialEq)]
pub enum SymbolError {
    InvalidSymbol(String),
    InvalidLabel(String),
    DuplicateSymbol,
}

pub const MAX_ABS_BIT_ADDR: u16 = u16::MAX >> 3;

#[derive(Debug, PartialEq)]
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

pub fn is_symbol_start_character(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

pub fn is_symbol_character(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

pub fn is_symbol_name_valid(s: &str) -> bool {
    let mut it = s.bytes();
    let first = it.next().unwrap_or(0);
    is_symbol_start_character(first) && it.all(is_symbol_character)
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

#[derive(Clone)]
struct Pending<'s> {
    line_no: LineNo,
    offset: usize,
    scope: Option<&'s str>,
    expression: String,
    output_type: PendingOutput,
}

pub struct OldScope<'s>(Option<&'s str>);

pub struct State<'s> {
    pub direct_page: DirectPageFlag,

    scope: Option<&'s str>,

    symbols: HashMap<String, Option<i64>>,
    output: Vec<u8>,

    pc_base: u16,

    line_no: LineNo,
    pending_output: Vec<Pending<'s>>,
}

impl<'s> State<'s> {
    #[allow(dead_code)] // ::TODO remove::
    pub fn new(pc_base: u16) -> Self {
        Self {
            direct_page: DirectPageFlag::Zero,
            scope: None,
            symbols: HashMap::new(),
            output: Vec::new(),
            pc_base,
            line_no: LineNo(0),
            pending_output: Vec::new(),
        }
    }

    pub(crate) fn take_output_and_symbols(self) -> (Vec<u8>, HashMap<String, Option<i64>>) {
        (self.output, self.symbols)
    }

    pub fn program_counter(&self) -> i64 {
        i64::try_from(self.output.len())
            .unwrap_or(i64::MAX)
            .saturating_add(self.pc_base.into())
    }

    /// Panics if pc_base has already been set
    pub fn set_pc_base(&mut self, pc_base: u16) {
        assert_ne!(pc_base, 0, "pc_base cannot be 0");
        assert_eq!(self.output.len(), 0, "Cannot set pc_base after assembly");

        if self.pc_base != 0 {
            panic!("Can only set pc_base once");
        }
        self.pc_base = pc_base;
    }

    fn override_scope(&mut self, scope: Option<&'s str>) {
        self.scope = scope;
    }

    pub fn take_scope(&mut self) -> OldScope<'s> {
        OldScope(std::mem::take(&mut self.scope))
    }

    pub fn restore_scope(&mut self, old: OldScope<'s>) {
        self.scope = old.0
    }

    // CAUTION: Does not add proc to the symbol file
    pub fn open_scope(&mut self, name: &'s str, labels: Vec<&str>) {
        for label in labels {
            let full_name = [name, ".", label].concat();
            self.symbols.entry(full_name).or_insert(None);
        }

        self.scope = Some(name);
    }

    pub fn close_scope(&mut self) {
        self.scope = None;
    }

    fn add_unchecked_symbol(&mut self, full_name: String, value: i64) -> Result<(), SymbolError> {
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
                    Err(SymbolError::DuplicateSymbol)
                }
            }
        }
    }

    pub fn add_scoped_symbol(
        &mut self,
        parent: &str,
        child_name: &str,
        value: i64,
    ) -> Result<(), SymbolError> {
        let full_name = [parent, ".", child_name].concat();

        match is_symbol_name_valid(child_name) {
            true => self.add_unchecked_symbol(full_name, value),
            false => Err(SymbolError::InvalidSymbol(full_name)),
        }
    }

    pub fn add_symbol(&mut self, name: &str, value: i64) -> Result<(), SymbolError> {
        match is_symbol_name_valid(name) {
            true => match &self.scope {
                Some(scope) => {
                    let full_name = [scope, ".", name].concat();
                    self.add_unchecked_symbol(full_name, value)
                }
                None => self.add_unchecked_symbol(name.to_owned(), value),
            },
            false => Err(SymbolError::InvalidLabel(name.into())),
        }
    }

    pub fn get_symbol(&self, name: &str) -> Option<i64> {
        match &self.scope {
            Some(scope) => {
                let full_name = [scope, ".", name].concat();
                match self.symbols.get(&full_name) {
                    Some(Some(v)) => Some(*v),
                    Some(None) => None,
                    None => self.symbols.get(name).copied().flatten(),
                }
            }
            None => self.symbols.get(name).copied().flatten(),
        }
    }

    #[cfg(test)]
    pub fn output(&self) -> &[u8] {
        &self.output
    }

    pub fn set_line_no(&mut self, line_no: LineNo) {
        self.line_no = line_no;
    }

    pub fn dp_addr_to_addr(&self, dp: u8) -> u16 {
        match self.direct_page {
            DirectPageFlag::Zero => u16::from_le_bytes([dp, 0]),
            DirectPageFlag::One => u16::from_le_bytes([dp, 1]),
        }
    }

    fn push_pending_output(&mut self, expression: String, output_type: PendingOutput) {
        self.pending_output.push(Pending {
            line_no: self.line_no,
            offset: self.output.len(),
            scope: self.scope,
            expression,
            output_type,
        });
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

pub fn process_pending_output_expressions(s: &mut State, errors: &mut FileErrors) {
    for p in std::mem::take(&mut s.pending_output) {
        let pos = p.offset;

        s.override_scope(p.scope);
        match evaluate(&p.expression, s) {
            ExpressionResult::Value(value) => match p.output_type {
                PendingOutput::U8 => match u8::try_from(value) {
                    Ok(v) => s.output[pos] = v,
                    Err(_) => errors.push(
                        p.line_no,
                        OutputError::OutOfRange {
                            value,
                            min: u8::MIN.into(),
                            max: u8::MAX.into(),
                        },
                    ),
                },
                PendingOutput::U16 => match u16::try_from(value) {
                    Ok(v) => {
                        s.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes());
                    }
                    Err(_) => errors.push(
                        p.line_no,
                        OutputError::OutOfRange {
                            value,
                            min: u16::MIN.into(),
                            max: u16::MAX.into(),
                        },
                    ),
                },
                PendingOutput::AbsBit(bit) => match u16::try_from(value) {
                    Ok(v) if v <= MAX_ABS_BIT_ADDR => {
                        let v = v | (u16::from(bit.0) << 13);
                        s.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes());
                    }
                    _ => errors.push(
                        p.line_no,
                        OutputError::OutOfRange {
                            value,
                            min: 0,
                            max: MAX_ABS_BIT_ADDR.into(),
                        },
                    ),
                },
                PendingOutput::RelativeBranch => {
                    let pc =
                        i64::try_from(pos + usize::from(s.pc_base) + 1).expect("output too big");

                    let value = value - pc;
                    match i8::try_from(value) {
                        Ok(v) => s.output[pos] = v.to_le_bytes()[0],
                        Err(_) => errors.push(p.line_no, OutputError::BranchOutOfRange(value)),
                    }
                }
                PendingOutput::Pcall => match u16::try_from(value) {
                    Ok(v) if v & 0xff00 == 0xff00 => {
                        s.output[pos] = v.to_le_bytes()[0];
                    }
                    _ => errors.push(p.line_no, OutputError::InvalidPcall(value)),
                },
            },
            crate::evaluator::ExpressionResult::Boolean(_) => {
                errors.push(p.line_no, OutputError::NotANumber(p.expression))
            }
            crate::evaluator::ExpressionResult::Unknown => errors.push(
                p.line_no,
                OutputError::ExpressionHasUnknownValue(p.expression),
            ),
            ExpressionResult::Error(e) => {
                errors.push(p.line_no, OutputError::ExpressionError(p.expression, e))
            }
        }
    }
}

// Only allow state cloning in unit tests
#[cfg(test)]
impl Clone for State<'_> {
    fn clone(&self) -> Self {
        Self {
            direct_page: self.direct_page.clone(),
            scope: self.scope.clone(),
            symbols: self.symbols.clone(),
            pc_base: self.pc_base,
            output: self.output.clone(),
            line_no: self.line_no,
            pending_output: self.pending_output.clone(),
        }
    }
}
