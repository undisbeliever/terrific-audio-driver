//! Assembler output

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{ExpressionError, FileErrors, LineNo},
    evaluator::{evaluate, ExpressionResult},
    symbols::{OldScope, Symbols},
};

pub const MAX_ABS_BIT_ADDR: u16 = u16::MAX >> 3;

#[derive(Debug, PartialEq)]
pub enum OutputError<'s> {
    AbsBitOutOfRange(u16),
    ExpressionError(&'s str, ExpressionError),
    NotANumber(&'s str),
    ExpressionHasUnknownValue(&'s str),

    OutOfRange { value: i64, min: i32, max: i32 },
    BranchOutOfRange(i64),
    InvalidPcall(i64),
}

impl std::fmt::Display for OutputError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputError::AbsBitOutOfRange(b) => write!(f, "abs bit address out of range: ${b:04x}"),
            OutputError::ExpressionError(expr, e) => write!(f, "expression error: {expr} {e}"),
            OutputError::NotANumber(expr) => write!(f, "not a number: {expr}"),
            OutputError::ExpressionHasUnknownValue(expr) => {
                write!(f, "expression has unknown value: {expr}")
            }
            OutputError::OutOfRange { value, min, max } => {
                write!(f, "value out of range: {value} (expected {min}..={max})")
            }
            OutputError::BranchOutOfRange(v) => write!(f, "branch of out range: {v:+}"),
            OutputError::InvalidPcall(v) => write!(f, "invalid pcall: ${v:04x}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum U8Value<'s> {
    Known(u8),
    Unknown(&'s str, OldScope<'s>),
}

#[derive(Debug, PartialEq)]
pub enum U16Value<'s> {
    Known(u16),
    Unknown(&'s str, OldScope<'s>),
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
    U16LoByte,
    U16HiByte,
    AbsBit(BitArgument),
    RelativeBranch,
    Pcall,
}

#[derive(Clone)]
struct Pending<'s> {
    line_no: LineNo,
    offset: usize,
    scope: OldScope<'s>,
    expression: &'s str,
    output_type: PendingOutput,
}

#[derive(Clone)]
struct Assert<'s> {
    line_no: LineNo,
    pc: i64,
    scope: OldScope<'s>,
    expression: &'s str,
}

pub(crate) struct Output<'s> {
    output: Vec<u8>,
    pc_base: u16,
    line_no: LineNo,
    pending_output: Vec<Pending<'s>>,
    asserts: Vec<Assert<'s>>,
}

impl<'s> Output<'s> {
    pub fn new(pc_base: u16) -> Self {
        Self {
            output: Vec::new(),
            pc_base,
            line_no: LineNo(0, 0),
            pending_output: Vec::new(),
            asserts: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn output(&self) -> &[u8] {
        &self.output
    }

    pub fn take_output(self) -> Vec<u8> {
        self.output
    }

    pub fn output_len(&self) -> usize {
        self.output.len()
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

    pub fn program_counter(&self) -> i64 {
        i64::try_from(self.output.len())
            .unwrap_or(i64::MAX)
            .saturating_add(self.pc_base.into())
    }

    pub fn set_line_no(&mut self, line_no: LineNo) {
        self.line_no = line_no;
    }

    fn push_pending_output(
        &mut self,
        expression: &'s str,
        scope: OldScope<'s>,
        output_type: PendingOutput,
    ) {
        self.pending_output.push(Pending {
            line_no: self.line_no,
            offset: self.output.len(),
            scope,
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

    pub fn write_u8v(&mut self, value: U8Value<'s>) {
        match value {
            U8Value::Known(v) => {
                self.output.push(v);
            }
            U8Value::Unknown(expression, scope) => {
                self.push_pending_output(expression, scope, PendingOutput::U8);
                self.output.push(0);
            }
        }
    }

    pub fn write_u16v(&mut self, value: U16Value<'s>) {
        match value {
            U16Value::Known(v) => {
                let [l, h] = v.to_le_bytes();

                self.output.extend(&[l, h]);
            }
            U16Value::Unknown(expression, scope) => {
                self.push_pending_output(expression, scope, PendingOutput::U16);
                self.output.extend(&[0, 0]);
            }
        }
    }

    pub fn write_lo_u16v(&mut self, value: U16Value<'s>) {
        match value {
            U16Value::Known(v) => {
                self.output.push(v.to_le_bytes()[0]);
            }
            U16Value::Unknown(expression, scope) => {
                self.push_pending_output(expression, scope, PendingOutput::U16LoByte);
                self.output.push(0);
            }
        }
    }

    pub fn write_hi_u16v(&mut self, value: U16Value<'s>) {
        match value {
            U16Value::Known(v) => {
                self.output.push(v.to_le_bytes()[1]);
            }
            U16Value::Unknown(expression, scope) => {
                self.push_pending_output(expression, scope, PendingOutput::U16HiByte);
                self.output.push(0);
            }
        }
    }

    pub fn write_op_u8(&mut self, opcode: u8, value: u8) {
        self.output.push(opcode);
        self.output.push(value);
    }

    pub fn write_op_u8v(&mut self, opcode: u8, value: U8Value<'s>) {
        self.output.push(opcode);
        self.write_u8v(value);
    }

    pub fn write_op_u16v(&mut self, opcode: u8, value: U16Value<'s>) {
        self.output.push(opcode);
        self.write_u16v(value);
    }

    pub fn write_op_abs_bit(
        &mut self,
        opcode: u8,
        value: U16Value<'s>,
        bit: BitArgument,
    ) -> Result<(), OutputError<'s>> {
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
            U16Value::Unknown(expression, scope) => {
                self.push_pending_output(expression, scope, PendingOutput::AbsBit(bit));
                self.output.extend(&[0, 0]);
                Ok(())
            }
        }
    }

    pub fn write_relative_goto(&mut self, expression: &'s str, state: &Symbols<'s>) {
        self.push_pending_output(expression, state.scope(), PendingOutput::RelativeBranch);

        self.output.push(0);
    }

    pub fn write_pcall_address(&mut self, expression: &'s str, state: &Symbols<'s>) {
        self.push_pending_output(expression, state.scope(), PendingOutput::Pcall);

        self.output.push(0xff);
    }

    pub fn add_assert(&mut self, line_no: LineNo, expression: &'s str, state: &Symbols<'s>) {
        self.asserts.push(Assert {
            line_no,
            pc: self.program_counter(),
            scope: state.scope(),
            expression,
        });
    }
}

pub fn process_pending_output_expressions<'s>(
    o: &mut Output<'s>,
    s: &mut Symbols<'s>,
    errors: &mut FileErrors<'s>,
) {
    assert_eq!(s.assert_pc(), None);
    assert_eq!(s.scope(), OldScope::NONE);

    for p in std::mem::take(&mut o.pending_output) {
        let pos = p.offset;

        s.restore_scope(p.scope);
        match evaluate(p.expression, s) {
            ExpressionResult::Value(value) => match p.output_type {
                PendingOutput::U8 => match u8::try_from(value) {
                    Ok(v) => o.output[pos] = v,
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
                    Ok(v) => o.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes()),
                    Err(_) => errors.push(
                        p.line_no,
                        OutputError::OutOfRange {
                            value,
                            min: u16::MIN.into(),
                            max: u16::MAX.into(),
                        },
                    ),
                },
                PendingOutput::U16LoByte => match u16::try_from(value) {
                    Ok(v) => o.output[pos] = v.to_le_bytes()[0],
                    Err(_) => errors.push(
                        p.line_no,
                        OutputError::OutOfRange {
                            value,
                            min: u16::MIN.into(),
                            max: u16::MAX.into(),
                        },
                    ),
                },
                PendingOutput::U16HiByte => match u16::try_from(value) {
                    Ok(v) => o.output[pos] = v.to_le_bytes()[1],
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
                        o.output[pos..pos + 2].copy_from_slice(&v.to_le_bytes());
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
                        i64::try_from(pos + usize::from(o.pc_base) + 1).expect("output too big");

                    let value = value - pc;
                    match i8::try_from(value) {
                        Ok(v) => o.output[pos] = v.to_le_bytes()[0],
                        Err(_) => errors.push(p.line_no, OutputError::BranchOutOfRange(value)),
                    }
                }
                PendingOutput::Pcall => match u16::try_from(value) {
                    Ok(v) if v & 0xff00 == 0xff00 => {
                        o.output[pos] = v.to_le_bytes()[0];
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

    s.take_scope();
}

#[derive(Debug, PartialEq)]
pub enum AssertError<'s> {
    AssertFailure(&'s str),
    NoConditional(&'s str),
    UnknownSymbol(&'s str),
    ExpressionError(&'s str, ExpressionError),
}

impl std::fmt::Display for AssertError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssertError::AssertFailure(expr) => write!(f, "assert failure: {expr}"),
            AssertError::NoConditional(expr) => {
                write!(f, "assert failure: no conditional: {expr}")
            }
            AssertError::UnknownSymbol(expr) => write!(f, "assert failure: unknown symbol: {expr}"),
            AssertError::ExpressionError(expr, e) => write!(f, "expression error: {expr}: {e}"),
        }
    }
}

pub fn process_asserts<'s>(o: &mut Output<'s>, s: &mut Symbols<'s>, errors: &mut FileErrors<'s>) {
    assert_eq!(s.assert_pc(), None);
    assert_eq!(s.scope(), OldScope::NONE);

    for assert in std::mem::take(&mut o.asserts) {
        s.set_assert_pc(assert.pc);
        s.restore_scope(assert.scope);

        match evaluate(assert.expression, s) {
            ExpressionResult::Boolean(true) => (),
            ExpressionResult::Boolean(false) => errors.push(
                assert.line_no,
                AssertError::AssertFailure(assert.expression),
            ),
            ExpressionResult::Value(_) => errors.push(
                assert.line_no,
                AssertError::NoConditional(assert.expression),
            ),
            ExpressionResult::Unknown => errors.push(
                assert.line_no,
                AssertError::UnknownSymbol(assert.expression),
            ),
            ExpressionResult::Error(e) => errors.push(
                assert.line_no,
                AssertError::ExpressionError(assert.expression, e),
            ),
        }
    }

    s.take_scope();
    s.clear_assert_pc();
}
