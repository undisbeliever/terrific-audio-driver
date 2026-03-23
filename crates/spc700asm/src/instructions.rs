//! Spc700 instructions and addressing modes

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    evaluator::{evaluate, ExpressionError, ExpressionResult},
    state::{BitArgument, DirectPageFlag, OutputError, State, U16Value, U8Value},
    string::comma_iter,
};

#[derive(Debug, PartialEq)]
pub enum AddressingModeError<'a> {
    UnknownAddressingMode(&'a str),
    ExpressionError(&'a str, ExpressionError),
    NotANumber(&'a str),
    DpAddressOutOfBounds(&'a str, i64),
    AbsoluteAddressOutOfBounds(&'a str, i64),
    DpOutOfBounds(&'a str, DirectPageFlag, i64),
    UnknownDpValue(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum InstructionError<'a> {
    AddressingModeError(AddressingModeError<'a>),
    ExpressionError(&'a str, ExpressionError),
    OutputError(OutputError<'a>),

    UnknownInstruction(&'a str),
    UnknownInstructionArguments(&'a str, String),
    InvalidNumberOfArguments { expected: u8, got: usize },
    // bit must be known and `0..=7`
    InvalidBitInstructionBit,
    InvalidTcallArgument,
}

impl<'a> From<AddressingModeError<'a>> for InstructionError<'a> {
    fn from(v: AddressingModeError<'a>) -> Self {
        Self::AddressingModeError(v)
    }
}

impl<'a> From<OutputError<'a>> for InstructionError<'a> {
    fn from(v: OutputError<'a>) -> Self {
        Self::OutputError(v)
    }
}

fn unknown_instruction_args_err<'a>(
    instruction: &'a str,
    modes: impl AddressingModeString,
) -> Result<(), InstructionError<'a>> {
    Err(InstructionError::UnknownInstructionArguments(
        instruction,
        AddressingModeString::stringify(modes),
    ))
}

trait AddressingModeString {
    fn stringify(modes: Self) -> String;
}

impl AddressingModeString for AddressingMode<'_> {
    fn stringify(modes: AddressingMode) -> String {
        modes.shorthand().to_owned()
    }
}

impl AddressingModeString for (AddressingMode<'_>, &str) {
    fn stringify(modes: (AddressingMode, &str)) -> String {
        format!("{}, rel", modes.0.shorthand())
    }
}

impl AddressingModeString for (AddressingMode<'_>, BitArgument, &str) {
    fn stringify(modes: (AddressingMode, BitArgument, &str)) -> String {
        format!("{}, bit, rel", modes.0.shorthand())
    }
}

impl AddressingModeString for (AddressingMode<'_>, BitArgument) {
    fn stringify(modes: (AddressingMode, BitArgument)) -> String {
        format!("{}, bit", modes.0.shorthand())
    }
}

impl AddressingModeString for [AddressingMode<'_>; 2] {
    fn stringify(modes: [AddressingMode; 2]) -> String {
        format!("{}, {}", modes[0].shorthand(), modes[1].shorthand())
    }
}

impl AddressingModeString for [AddressingMode<'_>; 3] {
    fn stringify(modes: [AddressingMode; 3]) -> String {
        format!(
            "{}, {}, {}",
            modes[0].shorthand(),
            modes[1].shorthand(),
            modes[2].shorthand()
        )
    }
}

fn invalid_n_argments_err(expected: u8, s: &str) -> InstructionError<'static> {
    InstructionError::InvalidNumberOfArguments {
        expected,
        got: comma_iter(s).count(),
    }
}

#[derive(Debug, PartialEq)]
pub enum AddressingMode<'a> {
    A,
    X,
    Y,
    Ya,
    Sp,
    Psw,
    C,

    XIndirect,
    XIndirectIncrement,
    YIndirect,

    Dp(u8),
    DpX(u8),
    DpY(u8),

    DpIndirectX(u8),
    DpIndirectY(u8),

    Abs(U16Value<'a>),
    AbsX(U16Value<'a>),
    AbsY(U16Value<'a>),

    NotAbs(U16Value<'a>),

    // jmp
    AbsIndirectX(U16Value<'a>),
    // For completeness
    AbsIndirectY(U16Value<'a>),

    Immediate(U8Value<'a>),
}

impl AddressingMode<'_> {
    #[allow(dead_code)] // ::TODO remove::
    pub fn shorthand(&self) -> &'static str {
        match self {
            Self::A => "A",
            Self::X => "X",
            Self::Y => "Y",
            Self::Ya => "YA",
            Self::Sp => "SP",
            Self::Psw => "PSW",
            Self::C => "C",
            Self::XIndirect => "(X)",
            Self::XIndirectIncrement => "(X)+",
            Self::YIndirect => "(Y)",
            Self::Dp(_) => "dp",
            Self::DpX(_) => "dp+X",
            Self::DpY(_) => "dp+Y",
            Self::DpIndirectX(_) => "[dp+X]",
            Self::DpIndirectY(_) => "[dp]+Y",
            Self::Abs(_) => "abs",
            Self::AbsX(_) => "abs+X",
            Self::AbsY(_) => "abs+Y",
            Self::NotAbs(_) => "/abs",
            Self::AbsIndirectX(_) => "[abs+X]",
            Self::AbsIndirectY(_) => "[abs]+Y",
            Self::Immediate(_) => "#imm",
        }
    }

    fn try_into_bit_argument(&self) -> Result<BitArgument, InstructionError<'static>> {
        match self {
            &Self::Abs(U16Value::Known(v)) => v
                .try_into()
                .map_err(|_| InstructionError::InvalidBitInstructionBit),
            _ => Err(InstructionError::InvalidBitInstructionBit),
        }
    }
}

fn parse_immediate_value<'a>(
    s: &'a str,
    symbols: &State,
) -> Result<U8Value<'a>, AddressingModeError<'a>> {
    match evaluate(s, symbols) {
        ExpressionResult::Value(value) => match value.try_into() {
            Ok(v) => Ok(U8Value::Known(v)),
            Err(_) => Err(AddressingModeError::DpAddressOutOfBounds(s, value)),
        },
        ExpressionResult::Unknown => Ok(U8Value::Unknown(s)),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

fn parse_abs_address<'a>(
    s: &'a str,
    symbols: &State,
) -> Result<U16Value<'a>, AddressingModeError<'a>> {
    match evaluate(s, symbols) {
        ExpressionResult::Value(value) => match value.try_into() {
            Ok(v) => Ok(U16Value::Known(v)),
            Err(_) => Err(AddressingModeError::AbsoluteAddressOutOfBounds(s, value)),
        },
        ExpressionResult::Unknown => Ok(U16Value::Unknown(s)),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

#[derive(Debug, PartialEq)]
enum DpOrAbs<'a> {
    Dp(u8),
    Abs(U16Value<'a>),
}

fn parse_dp_or_abs_address<'a>(
    s: &'a str,
    symbols: &State,
) -> Result<DpOrAbs<'a>, AddressingModeError<'a>> {
    match evaluate(s, symbols) {
        ExpressionResult::Value(value) => match u16::try_from(value) {
            Ok(addr) => {
                if symbols.direct_page == DirectPageFlag::Zero && addr < 0x100 {
                    Ok(DpOrAbs::Dp(u8::try_from(addr).unwrap()))
                } else if symbols.direct_page == DirectPageFlag::One
                    && (0x100..0x200).contains(&addr)
                {
                    Ok(DpOrAbs::Dp(u8::try_from(addr - 0x100).unwrap()))
                } else {
                    Ok(DpOrAbs::Abs(U16Value::Known(addr)))
                }
            }
            Err(_) => Err(AddressingModeError::AbsoluteAddressOutOfBounds(s, value)),
        },
        ExpressionResult::Unknown => Ok(DpOrAbs::Abs(U16Value::Unknown(s))),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

fn parse_dp_address<'a>(s: &'a str, symbols: &State) -> Result<u8, AddressingModeError<'a>> {
    match evaluate(s, symbols) {
        ExpressionResult::Value(value) => match u16::try_from(value) {
            Ok(addr) => {
                if symbols.direct_page == DirectPageFlag::Zero && addr < 0x100 {
                    Ok(u8::try_from(addr).unwrap())
                } else if symbols.direct_page == DirectPageFlag::One
                    && (0x100..0x200).contains(&addr)
                {
                    Ok(u8::try_from(addr - 0x100).unwrap())
                } else {
                    Err(AddressingModeError::DpOutOfBounds(
                        s,
                        symbols.direct_page,
                        value,
                    ))
                }
            }
            Err(_) => Err(AddressingModeError::DpOutOfBounds(
                s,
                symbols.direct_page,
                value,
            )),
        },
        ExpressionResult::Unknown => Err(AddressingModeError::UnknownDpValue(s)),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

fn strip_final_three_chars(s: &str) -> ([u8; 3], &str) {
    let mut it = s
        .bytes()
        .enumerate()
        .rev()
        .filter(|(_, c)| !c.is_ascii_whitespace());

    let (a, b, c) = (it.next(), it.next(), it.next());

    match (a, b, c) {
        (Some((_, a)), Some((_, b)), Some((i, c))) => ([c, b, a], s[..i].trim()),
        _ => ([0, 0, 0], ""),
    }
}

fn strip_plus_index_suffix<'a>(s: &'a str, register: &'static str) -> Option<&'a str> {
    if s.ends_with(register) {
        let mut it = s
            .bytes()
            .enumerate()
            .rev()
            .filter(|(_, c)| !c.is_ascii_whitespace());

        let (_, b) = (it.next(), it.next());

        match b {
            Some((i, b'+')) => Some(s[..i].trim_end()),
            _ => None,
        }
    } else {
        None
    }
}

fn parse_addressing_mode<'a>(
    input: &'a str,
    symbols: &State,
) -> Result<AddressingMode<'a>, AddressingModeError<'a>> {
    match input {
        "A" => Ok(AddressingMode::A),
        "X" => Ok(AddressingMode::X),
        "Y" => Ok(AddressingMode::Y),
        "YA" => Ok(AddressingMode::Ya),
        "SP" => Ok(AddressingMode::Sp),
        "PSW" => Ok(AddressingMode::Psw),
        "C" => Ok(AddressingMode::C),

        "(X)" => Ok(AddressingMode::XIndirect),
        "(X)+" => Ok(AddressingMode::XIndirectIncrement),
        "(Y)" => Ok(AddressingMode::YIndirect),

        s => {
            if let Some(s) = s.strip_prefix("#") {
                Ok(AddressingMode::Immediate(parse_immediate_value(
                    s, symbols,
                )?))
            } else if let Some(s) = s.strip_prefix("[") {
                let (suffix, expr) = strip_final_three_chars(s);
                match suffix {
                    [b'+', b'X', b']'] => Ok(AddressingMode::DpIndirectX(parse_dp_address(
                        expr, symbols,
                    )?)),
                    [b']', b'+', b'Y'] => Ok(AddressingMode::DpIndirectY(parse_dp_address(
                        expr, symbols,
                    )?)),
                    _ => Err(AddressingModeError::UnknownAddressingMode(input)),
                }
            } else if let Some(s) = s.strip_prefix("/") {
                Ok(AddressingMode::NotAbs(parse_abs_address(s, symbols)?))
            } else if let Some(s) = strip_plus_index_suffix(s, "X") {
                match parse_dp_or_abs_address(s, symbols)? {
                    DpOrAbs::Dp(a) => Ok(AddressingMode::DpX(a)),
                    DpOrAbs::Abs(a) => Ok(AddressingMode::AbsX(a)),
                }
            } else if let Some(s) = strip_plus_index_suffix(s, "Y") {
                match parse_dp_or_abs_address(s, symbols)? {
                    DpOrAbs::Dp(a) => Ok(AddressingMode::DpY(a)),
                    DpOrAbs::Abs(a) => Ok(AddressingMode::AbsY(a)),
                }
            } else {
                match parse_dp_or_abs_address(s, symbols)? {
                    DpOrAbs::Dp(a) => Ok(AddressingMode::Dp(a)),
                    DpOrAbs::Abs(a) => Ok(AddressingMode::Abs(a)),
                }
            }
        }
    }
}

#[allow(dead_code)] // ::TODO remove::
fn parse_no_dp_addressing_mode<'a>(
    input: &'a str,
    symbols: &State,
) -> Result<AddressingMode<'a>, AddressingModeError<'a>> {
    match input {
        "A" => Ok(AddressingMode::A),
        "X" => Ok(AddressingMode::X),
        "Y" => Ok(AddressingMode::Y),
        "YA" => Ok(AddressingMode::Ya),
        "SP" => Ok(AddressingMode::Sp),
        "PSW" => Ok(AddressingMode::Psw),
        "C" => Ok(AddressingMode::C),

        "(X)" => Ok(AddressingMode::XIndirect),
        "(X)+" => Ok(AddressingMode::XIndirectIncrement),
        "(Y)" => Ok(AddressingMode::YIndirect),

        s => {
            if let Some(s) = s.strip_prefix("#") {
                Ok(AddressingMode::Immediate(parse_immediate_value(
                    s, symbols,
                )?))
            } else if let Some(s) = s.strip_prefix("[") {
                let (suffix, expr) = strip_final_three_chars(s);
                match suffix {
                    [b'+', b'X', b']'] => Ok(AddressingMode::AbsIndirectX(parse_abs_address(
                        expr, symbols,
                    )?)),
                    [b']', b'+', b'Y'] => Ok(AddressingMode::AbsIndirectY(parse_abs_address(
                        expr, symbols,
                    )?)),
                    _ => Err(AddressingModeError::UnknownAddressingMode(input)),
                }
            } else if let Some(s) = s.strip_prefix("/") {
                Ok(AddressingMode::NotAbs(parse_abs_address(s, symbols)?))
            } else if let Some(s) = strip_plus_index_suffix(s, "X") {
                Ok(AddressingMode::AbsX(parse_abs_address(s, symbols)?))
            } else if let Some(s) = strip_plus_index_suffix(s, "Y") {
                Ok(AddressingMode::AbsY(parse_abs_address(s, symbols)?))
            } else {
                Ok(AddressingMode::Abs(parse_abs_address(s, symbols)?))
            }
        }
    }
}

fn parse_bit_argument<'a>(s: &'a str, state: &State) -> Result<BitArgument, InstructionError<'a>> {
    match evaluate(s, state) {
        ExpressionResult::Value(bit) => {
            BitArgument::try_from(bit).map_err(|_| InstructionError::InvalidBitInstructionBit)
        }
        ExpressionResult::Error(e) => Err(InstructionError::ExpressionError(s, e)),
        _ => Err(InstructionError::InvalidBitInstructionBit),
    }
}

fn split_one_argument<'a>(s: &'a str) -> Result<&'a str, InstructionError<'a>> {
    let mut it = comma_iter(s);

    match (it.next(), it.next()) {
        (Some(a), None) => Ok(a),
        _ => Err(invalid_n_argments_err(1, s)),
    }
}

fn split_two_arguments<'a>(s: &'a str) -> Result<[&'a str; 2], InstructionError<'a>> {
    let mut it = comma_iter(s);

    match (it.next(), it.next(), it.next()) {
        (Some(a), Some(b), None) => Ok([a, b]),
        _ => Err(invalid_n_argments_err(2, s)),
    }
}

fn split_three_arguments<'a>(s: &'a str) -> Result<[&'a str; 3], InstructionError<'a>> {
    let mut it = comma_iter(s);

    match (it.next(), it.next(), it.next(), it.next()) {
        (Some(a), Some(b), Some(c), None) => Ok([a, b, c]),
        _ => Err(invalid_n_argments_err(3, s)),
    }
}

fn parse_one_argument<'a>(
    s: &'a str,
    state: &State,
) -> Result<AddressingMode<'a>, InstructionError<'a>> {
    let arg = split_one_argument(s)?;

    Ok(parse_addressing_mode(arg, state)?)
}

fn parse_one_no_dp_argument<'a>(
    s: &'a str,
    state: &State,
) -> Result<AddressingMode<'a>, InstructionError<'a>> {
    let arg = split_one_argument(s)?;

    Ok(parse_no_dp_addressing_mode(arg, state)?)
}

fn parse_two_arguments<'a>(
    s: &'a str,
    state: &State,
) -> Result<[AddressingMode<'a>; 2], InstructionError<'a>> {
    let [a1, a2] = split_two_arguments(s)?;
    Ok([
        parse_addressing_mode(a1, state)?,
        parse_addressing_mode(a2, state)?,
    ])
}

fn parse_two_no_dp_arguments<'a>(
    s: &'a str,
    state: &State,
) -> Result<[AddressingMode<'a>; 2], InstructionError<'a>> {
    let [a1, a2] = split_two_arguments(s)?;
    Ok([
        parse_no_dp_addressing_mode(a1, state)?,
        parse_no_dp_addressing_mode(a2, state)?,
    ])
}

fn parse_three_no_dp_arguments<'a>(
    s: &'a str,
    state: &State,
) -> Result<[AddressingMode<'a>; 3], InstructionError<'a>> {
    let [a1, a2, a3] = split_three_arguments(s)?;
    Ok([
        parse_no_dp_addressing_mode(a1, state)?,
        parse_no_dp_addressing_mode(a2, state)?,
        parse_no_dp_addressing_mode(a3, state)?,
    ])
}

fn parse_argument_and_rel<'a>(
    s: &'a str,
    state: &State,
) -> Result<(AddressingMode<'a>, &'a str), InstructionError<'a>> {
    let [a1, a2] = split_two_arguments(s)?;

    Ok((parse_addressing_mode(a1, state)?, a2))
}

fn arithmatic_instruction_impl<'a>(
    instruction: &'a str,
    arguments: [AddressingMode<'a>; 2],
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match arguments {
        [AddressingMode::A, AddressingMode::Immediate(i)] => {
            state.write_op_u8v(opcode_base | 0x08, i)
        }
        [AddressingMode::A, AddressingMode::XIndirect] => state.write_u8(opcode_base | 0x06),
        [AddressingMode::A, AddressingMode::Dp(a)] => state.write_op_u8(opcode_base | 0x04, a),
        [AddressingMode::A, AddressingMode::DpX(a)] => state.write_op_u8(opcode_base | 0x14, a),
        [AddressingMode::A, AddressingMode::Abs(a)] => state.write_op_u16v(opcode_base | 0x05, a),
        [AddressingMode::A, AddressingMode::AbsX(a)] => state.write_op_u16v(opcode_base | 0x15, a),
        [AddressingMode::A, AddressingMode::AbsY(a)] => state.write_op_u16v(opcode_base | 0x16, a),
        [AddressingMode::A, AddressingMode::DpIndirectX(a)] => {
            state.write_op_u8(opcode_base | 0x07, a)
        }
        [AddressingMode::A, AddressingMode::DpIndirectY(a)] => {
            state.write_op_u8(opcode_base | 0x17, a)
        }

        [AddressingMode::A, AddressingMode::DpY(a)] => {
            // promote to `addr+Y`
            state.write_u8(opcode_base | 0x15);
            state.write_u16(state.dp_addr_to_addr(a));
        }

        [AddressingMode::Dp(d), AddressingMode::Dp(s)] => {
            state.write_u8(opcode_base | 0x09);
            state.write_u8(d);
            state.write_u8(s);
        }
        [AddressingMode::Dp(d), AddressingMode::Immediate(i)] => {
            state.write_u8(opcode_base | 0x18);
            state.write_u8(d);
            state.write_u8v(i);
        }
        [AddressingMode::XIndirect, AddressingMode::YIndirect] => {
            state.write_u8(opcode_base | 0x19)
        }

        am => return unknown_instruction_args_err(instruction, am),
    }

    Ok(())
}

fn no_argument_instruction<'a>(
    arguments: &'a str,
    opcode: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    if arguments.is_empty() {
        state.write_u8(opcode);
        Ok(())
    } else {
        Err(invalid_n_argments_err(0, arguments))
    }
}

fn only_a_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::A => {
            state.write_u8(opcode);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn only_ya_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::Ya => {
            state.write_u8(opcode);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn only_dp_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::Dp(dp) => {
            state.write_op_u8(opcode, dp);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn only_abs_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_one_no_dp_argument(arguments, state)? {
        AddressingMode::Abs(abs) => {
            state.write_op_u16v(opcode, abs);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn ya_dp_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    match parse_two_arguments(arguments, state)? {
        [AddressingMode::Ya, AddressingMode::Dp(a)] => {
            state.write_op_u8(opcode, a);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn arithmatic_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    arithmatic_instruction_impl(
        instruction,
        parse_two_arguments(arguments, state)?,
        opcode_base,
        state,
    )
}

fn inc_dec_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::A => state.write_u8(opcode_base + 0x9c),
        AddressingMode::Dp(a) => state.write_op_u8(opcode_base + 0x8b, a),
        AddressingMode::DpX(a) => state.write_op_u8(opcode_base + 0x9b, a),
        AddressingMode::Abs(a) => state.write_op_u16v(opcode_base + 0x8c, a),
        AddressingMode::X => state.write_u8(opcode_base + 0x1d),
        AddressingMode::Y => state.write_u8(opcode_base + 0xdc),

        am => return unknown_instruction_args_err(instruction, am),
    }

    Ok(())
}

fn rmw_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::A => state.write_u8(opcode_base | 0x1c),
        AddressingMode::Dp(a) => state.write_op_u8(opcode_base | 0x0b, a),
        AddressingMode::DpX(a) => state.write_op_u8(opcode_base | 0x1b, a),
        AddressingMode::Abs(a) => state.write_op_u16v(opcode_base | 0x0c, a),

        am => return unknown_instruction_args_err(instruction, am),
    }

    Ok(())
}

fn branch_instruction<'a>(
    arguments: &'a str,
    opcode: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    let rel = split_one_argument(arguments)?;
    state.write_u8(opcode);
    state.write_relative_goto(rel);
    Ok(())
}

fn branch_bit_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    let [addr, bit, rel] = split_three_arguments(arguments)?;

    let am = parse_addressing_mode(addr, state)?;
    let bit = parse_bit_argument(bit, state)?;

    let opcode = bit.to_opcode(opcode_base);

    match am {
        AddressingMode::Dp(dp) => {
            state.write_op_u8(opcode, dp);
            state.write_relative_goto(rel);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, (am, bit, rel)),
    }
}

fn pcall_instruction<'a>(
    arguments: &'a str,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    let addr = split_one_argument(arguments)?;

    state.write_u8(0x4f);
    state.write_pcall_address(addr);
    Ok(())
}

fn tcall_instruction<'a>(
    arguments: &'a str,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    let addr = split_one_argument(arguments)?;

    match evaluate(addr, state) {
        ExpressionResult::Value(i) if (0..=16).contains(&i) => {
            state.write_u8(0x01 | u8::try_from(i << 4).unwrap());
            Ok(())
        }
        ExpressionResult::Error(e) => Err(InstructionError::ExpressionError(addr, e)),
        _ => Err(InstructionError::InvalidTcallArgument),
    }
}

fn stack_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    match parse_one_argument(arguments, state)? {
        AddressingMode::A => {
            state.write_u8(opcode_base + 0x20);
            Ok(())
        }
        AddressingMode::X => {
            state.write_u8(opcode_base + 0x40);
            Ok(())
        }
        AddressingMode::Y => {
            state.write_u8(opcode_base + 0x60);
            Ok(())
        }
        AddressingMode::Psw => {
            state.write_u8(opcode_base);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn dp_bit_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State,
) -> Result<(), InstructionError<'a>> {
    let [addr, bit] = split_two_arguments(arguments)?;

    let am = parse_addressing_mode(addr, state)?;
    let bit = parse_bit_argument(bit, state)?;

    let opcode = bit.to_opcode(opcode_base);

    match am {
        AddressingMode::Dp(dp) => {
            state.write_op_u8(opcode, dp);
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, (am, bit)),
    }
}

fn abs_bit_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_two_no_dp_arguments(arguments, state)? {
        [AddressingMode::Abs(abs), bit] => {
            let bit = bit.try_into_bit_argument()?;

            state.write_op_abs_bit(opcode, abs, bit)?;
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn abs_or_not_abs_bit_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode_base: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_three_no_dp_arguments(arguments, state)? {
        [AddressingMode::C, AddressingMode::Abs(abs), bit] => {
            let bit = bit.try_into_bit_argument()?;

            state.write_op_abs_bit(opcode_base, abs, bit)?;
            Ok(())
        }
        [AddressingMode::C, AddressingMode::NotAbs(abs), bit] => {
            let bit = bit.try_into_bit_argument()?;

            state.write_op_abs_bit(opcode_base | 0x20, abs, bit)?;
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

fn carry_abs_bit_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    opcode: u8,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match parse_three_no_dp_arguments(arguments, state)? {
        [AddressingMode::C, AddressingMode::Abs(abs), bit] => {
            let bit = bit.try_into_bit_argument()?;

            state.write_op_abs_bit(opcode, abs, bit)?;
            Ok(())
        }
        am => unknown_instruction_args_err(instruction, am),
    }
}

pub fn process_instruction<'a>(
    instruction: &'a str,
    arguments: &'a str,
    state: &mut State<'a>,
) -> Result<(), InstructionError<'a>> {
    match instruction {
        "or" => arithmatic_instruction(instruction, arguments, 0x00, state),
        "and" => arithmatic_instruction(instruction, arguments, 0x20, state),
        "eor" => arithmatic_instruction(instruction, arguments, 0x40, state),
        "adc" => arithmatic_instruction(instruction, arguments, 0x80, state),
        "sbc" => arithmatic_instruction(instruction, arguments, 0xa0, state),

        "dec" => inc_dec_instruction(instruction, arguments, 0x00, state),
        "inc" => inc_dec_instruction(instruction, arguments, 0x20, state),

        "asl" => rmw_instruction(instruction, arguments, 0x00, state),
        "rol" => rmw_instruction(instruction, arguments, 0x20, state),
        "lsr" => rmw_instruction(instruction, arguments, 0x40, state),
        "ror" => rmw_instruction(instruction, arguments, 0x60, state),

        "xcn" => only_a_instruction(instruction, arguments, 0x9f, state),

        "incw" => only_dp_instruction(instruction, arguments, 0x3a, state),
        "decw" => only_dp_instruction(instruction, arguments, 0x1a, state),
        "addw" => ya_dp_instruction(instruction, arguments, 0x7a, state),
        "subw" => ya_dp_instruction(instruction, arguments, 0x9a, state),
        "cmpw" => ya_dp_instruction(instruction, arguments, 0x5a, state),

        "mul" => only_ya_instruction(instruction, arguments, 0xcf, state),

        "daa" => only_a_instruction(instruction, arguments, 0xdf, state),
        "das" => only_a_instruction(instruction, arguments, 0xbe, state),

        "bra" => branch_instruction(arguments, 0x2f, state),
        "beq" => branch_instruction(arguments, 0xf0, state),
        "bne" => branch_instruction(arguments, 0xd0, state),
        "bcs" => branch_instruction(arguments, 0xb0, state),
        "bcc" => branch_instruction(arguments, 0x90, state),
        "bvs" => branch_instruction(arguments, 0x70, state),
        "bvc" => branch_instruction(arguments, 0x50, state),
        "bmi" => branch_instruction(arguments, 0x30, state),
        "bpl" => branch_instruction(arguments, 0x10, state),

        "bbs" => branch_bit_instruction(instruction, arguments, 0x03, state),
        "bbc" => branch_bit_instruction(instruction, arguments, 0x13, state),

        "call" => only_abs_instruction(instruction, arguments, 0x3f, state),
        "pcall" => pcall_instruction(arguments, state),
        "tcall" => tcall_instruction(arguments, state),

        "brk" => no_argument_instruction(arguments, 0x0f, state),
        "ret" => no_argument_instruction(arguments, 0x6f, state),
        "reti" => no_argument_instruction(arguments, 0x7f, state),

        "push" => stack_instruction(instruction, arguments, 0x0d, state),
        "pop" => stack_instruction(instruction, arguments, 0x8e, state),

        "set1" => dp_bit_instruction(instruction, arguments, 0x02, state),
        "clr1" => dp_bit_instruction(instruction, arguments, 0x12, state),

        "tset1" => only_abs_instruction(instruction, arguments, 0x0e, state),
        "tclr1" => only_abs_instruction(instruction, arguments, 0x4e, state),

        "and1" => abs_or_not_abs_bit_instruction(instruction, arguments, 0x4a, state),
        "or1" => abs_or_not_abs_bit_instruction(instruction, arguments, 0x0a, state),
        "eor1" => carry_abs_bit_instruction(instruction, arguments, 0x8a, state),
        "not1" => abs_bit_instruction(instruction, arguments, 0xea, state),

        "clrc" => no_argument_instruction(arguments, 0x60, state),
        "setc" => no_argument_instruction(arguments, 0x80, state),
        "notc" => no_argument_instruction(arguments, 0xED, state),
        "clrv" => no_argument_instruction(arguments, 0xE0, state),
        "clrp" => no_argument_instruction(arguments, 0x20, state),
        "setp" => no_argument_instruction(arguments, 0x40, state),
        "ei" => no_argument_instruction(arguments, 0xA0, state),
        "di" => no_argument_instruction(arguments, 0xC0, state),
        "nop" => no_argument_instruction(arguments, 0x00, state),
        "sleep" => no_argument_instruction(arguments, 0xEF, state),
        "stop" => no_argument_instruction(arguments, 0xFF, state),

        "mov" => {
            match parse_two_arguments(arguments, state)? {
                [AddressingMode::A, AddressingMode::Immediate(i)] => state.write_op_u8v(0xe8, i),
                [AddressingMode::A, AddressingMode::XIndirect] => state.write_u8(0xe6),
                [AddressingMode::A, AddressingMode::XIndirectIncrement] => state.write_u8(0xbf),
                [AddressingMode::A, AddressingMode::Dp(a)] => state.write_op_u8(0xe4, a),
                [AddressingMode::A, AddressingMode::DpX(a)] => state.write_op_u8(0xf4, a),
                [AddressingMode::A, AddressingMode::Abs(a)] => state.write_op_u16v(0xe5, a),
                [AddressingMode::A, AddressingMode::AbsX(a)] => state.write_op_u16v(0xf5, a),
                [AddressingMode::A, AddressingMode::AbsY(a)] => state.write_op_u16v(0xf6, a),
                [AddressingMode::A, AddressingMode::DpIndirectX(a)] => state.write_op_u8(0xe7, a),
                [AddressingMode::A, AddressingMode::DpIndirectY(a)] => state.write_op_u8(0xf7, a),

                [AddressingMode::A, AddressingMode::DpY(a)] => {
                    // promote to `addr+Y`
                    state.write_u8(0xf6);
                    state.write_u16(state.dp_addr_to_addr(a));
                }

                [AddressingMode::X, AddressingMode::Immediate(v)] => state.write_op_u8v(0xcd, v),
                [AddressingMode::X, AddressingMode::Dp(a)] => state.write_op_u8(0xf8, a),
                [AddressingMode::X, AddressingMode::DpY(a)] => state.write_op_u8(0xf9, a),
                [AddressingMode::X, AddressingMode::Abs(a)] => state.write_op_u16v(0xe9, a),

                [AddressingMode::Y, AddressingMode::Immediate(v)] => state.write_op_u8v(0x8d, v),
                [AddressingMode::Y, AddressingMode::Dp(a)] => state.write_op_u8(0xeb, a),
                [AddressingMode::Y, AddressingMode::DpX(a)] => state.write_op_u8(0xfb, a),
                [AddressingMode::Y, AddressingMode::Abs(a)] => state.write_op_u16v(0xec, a),

                [AddressingMode::XIndirect, AddressingMode::A] => state.write_u8(0xc6),
                [AddressingMode::XIndirectIncrement, AddressingMode::A] => state.write_u8(0xaf),
                [AddressingMode::Dp(a), AddressingMode::A] => state.write_op_u8(0xc4, a),
                [AddressingMode::DpX(a), AddressingMode::A] => state.write_op_u8(0xd4, a),
                [AddressingMode::Abs(a), AddressingMode::A] => state.write_op_u16v(0xc5, a),
                [AddressingMode::AbsX(a), AddressingMode::A] => state.write_op_u16v(0xd5, a),
                [AddressingMode::AbsY(a), AddressingMode::A] => state.write_op_u16v(0xd6, a),
                [AddressingMode::DpIndirectX(a), AddressingMode::A] => state.write_op_u8(0xc7, a),
                [AddressingMode::DpIndirectY(a), AddressingMode::A] => state.write_op_u8(0xd7, a),

                [AddressingMode::DpY(a), AddressingMode::A] => {
                    // promote to `addr+Y`
                    state.write_u8(0xd6);
                    state.write_u16(state.dp_addr_to_addr(a));
                }

                [AddressingMode::Dp(a), AddressingMode::X] => state.write_op_u8(0xd8, a),
                [AddressingMode::DpY(a), AddressingMode::X] => state.write_op_u8(0xd9, a),
                [AddressingMode::Abs(a), AddressingMode::X] => state.write_op_u16v(0xc9, a),
                [AddressingMode::Dp(a), AddressingMode::Y] => state.write_op_u8(0xcb, a),
                [AddressingMode::DpX(a), AddressingMode::Y] => state.write_op_u8(0xdb, a),
                [AddressingMode::Abs(a), AddressingMode::Y] => state.write_op_u16v(0xcc, a),

                [AddressingMode::A, AddressingMode::X] => state.write_u8(0x7d),
                [AddressingMode::A, AddressingMode::Y] => state.write_u8(0xdd),
                [AddressingMode::X, AddressingMode::A] => state.write_u8(0x5d),
                [AddressingMode::Y, AddressingMode::A] => state.write_u8(0xfd),
                [AddressingMode::X, AddressingMode::Sp] => state.write_u8(0x9d),
                [AddressingMode::Sp, AddressingMode::X] => state.write_u8(0xbd),

                [AddressingMode::Dp(ad), AddressingMode::Dp(ds)] => {
                    state.write_u8(0xfa);
                    state.write_u8(ad);
                    state.write_u8(ds);
                }
                [AddressingMode::Dp(a), AddressingMode::Immediate(i)] => {
                    state.write_u8(0x8f);
                    state.write_u8(a);
                    state.write_u8v(i);
                }

                am => return unknown_instruction_args_err(instruction, am),
            }
            Ok(())
        }

        "movw" => {
            match parse_two_arguments(arguments, state)? {
                [AddressingMode::Ya, AddressingMode::Dp(a)] => state.write_op_u8(0xba, a),
                [AddressingMode::Dp(a), AddressingMode::Ya] => state.write_op_u8(0xda, a),
                am => arithmatic_instruction_impl(instruction, am, 0x60, state)?,
            }
            Ok(())
        }
        "mov1" => match parse_three_no_dp_arguments(arguments, state)? {
            [AddressingMode::C, AddressingMode::Abs(abs), bit] => {
                let bit = bit.try_into_bit_argument()?;

                state.write_op_abs_bit(0xaa, abs, bit)?;
                Ok(())
            }
            [AddressingMode::Abs(abs), bit, AddressingMode::C] => {
                let bit = bit.try_into_bit_argument()?;

                state.write_op_abs_bit(0xca, abs, bit)?;
                Ok(())
            }
            am => unknown_instruction_args_err(instruction, am),
        },
        "cmp" => {
            match parse_two_arguments(arguments, state)? {
                [AddressingMode::X, AddressingMode::Immediate(v)] => state.write_op_u8v(0xc8, v),
                [AddressingMode::X, AddressingMode::Dp(a)] => state.write_op_u8(0x3e, a),
                [AddressingMode::X, AddressingMode::Abs(a)] => state.write_op_u16v(0x1e, a),

                [AddressingMode::Y, AddressingMode::Immediate(v)] => state.write_op_u8v(0xad, v),
                [AddressingMode::Y, AddressingMode::Dp(a)] => state.write_op_u8(0x7e, a),
                [AddressingMode::Y, AddressingMode::Abs(a)] => state.write_op_u16v(0x5e, a),

                am => arithmatic_instruction_impl(instruction, am, 0x60, state)?,
            }
            Ok(())
        }
        "div" => match parse_two_arguments(arguments, state)? {
            [AddressingMode::Ya, AddressingMode::X] => {
                state.write_u8(0x9e);
                Ok(())
            }
            am => unknown_instruction_args_err(instruction, am),
        },
        "cbne" => match parse_argument_and_rel(arguments, state)? {
            (AddressingMode::Dp(dp), rel) => {
                state.write_op_u8(0x2e, dp);
                state.write_relative_goto(rel);
                Ok(())
            }
            (AddressingMode::DpX(dp), rel) => {
                state.write_op_u8(0xde, dp);
                state.write_relative_goto(rel);
                Ok(())
            }
            am => unknown_instruction_args_err(instruction, am),
        },

        "dbnz" => match parse_argument_and_rel(arguments, state)? {
            (AddressingMode::Dp(dp), rel) => {
                state.write_op_u8(0x6e, dp);
                state.write_relative_goto(rel);
                Ok(())
            }
            (AddressingMode::Y, rel) => {
                state.write_u8(0xfe);
                state.write_relative_goto(rel);
                Ok(())
            }
            am => unknown_instruction_args_err(instruction, am),
        },

        "jmp" => match parse_one_no_dp_argument(arguments, state)? {
            AddressingMode::Abs(abs) => {
                state.write_op_u16v(0x5f, abs);
                Ok(())
            }
            AddressingMode::AbsIndirectX(abs) => {
                state.write_op_u16v(0x1f, abs);
                Ok(())
            }
            am => unknown_instruction_args_err(instruction, am),
        },

        _ => Err(InstructionError::UnknownInstruction(instruction)),
    }
}

#[cfg(test)]
mod addressing_mode_tests {
    use super::{
        parse_addressing_mode, parse_no_dp_addressing_mode, AddressingMode, AddressingModeError,
        DirectPageFlag, State, U16Value, U8Value,
    };

    macro_rules! test_ok {
        ($s:literal, $v:expr) => {
            assert_eq!(parse_addressing_mode($s, &State::new(0x200)), Ok($v))
        };
    }
    macro_rules! test_err {
        ($s:literal, Err($v:expr)) => {
            assert_eq!(parse_addressing_mode($s, &State::new(0x200)), Err($v))
        };
    }

    #[test]
    fn registers() {
        test_ok!("A", AddressingMode::A);
        test_ok!("X", AddressingMode::X);
        test_ok!("Y", AddressingMode::Y);
        test_ok!("YA", AddressingMode::Ya);
        test_ok!("SP", AddressingMode::Sp);
        test_ok!("PSW", AddressingMode::Psw);
        test_ok!("C", AddressingMode::C);
    }

    #[test]
    fn register_indirect() {
        test_ok!("(X)", AddressingMode::XIndirect);
        test_ok!("(X)+", AddressingMode::XIndirectIncrement);
        test_ok!("(Y)", AddressingMode::YIndirect);
    }

    #[test]
    fn immediate() {
        test_ok!("#123", AddressingMode::Immediate(U8Value::Known(123)));
        test_ok!(
            "#unknown + 123 / 2",
            AddressingMode::Immediate(U8Value::Unknown("unknown + 123 / 2"))
        );
    }

    #[test]
    fn dp_indirect() {
        test_ok!("[$42+X]", AddressingMode::DpIndirectX(0x42));
        test_ok!("[123 + X]", AddressingMode::DpIndirectX(123));
        test_ok!("[0+ X]", AddressingMode::DpIndirectX(0));
        test_ok!("[1 +X]", AddressingMode::DpIndirectX(1));
        test_ok!("[2 +X ]", AddressingMode::DpIndirectX(2));

        test_ok!("[100+20+X]", AddressingMode::DpIndirectX(120));
        test_ok!("[100 + 20 + X]", AddressingMode::DpIndirectX(120));

        test_ok!("[$42]+Y", AddressingMode::DpIndirectY(0x42));
        test_ok!("[0] +Y", AddressingMode::DpIndirectY(0));
        test_ok!("[1]+ Y", AddressingMode::DpIndirectY(1));
        test_ok!("[2]+ Y ", AddressingMode::DpIndirectY(2));
        test_ok!("[  3  ] + Y ", AddressingMode::DpIndirectY(3));

        test_ok!("[80+5]+Y", AddressingMode::DpIndirectY(85));
        test_ok!("[  80 + 5 ] + Y ", AddressingMode::DpIndirectY(85));
    }

    #[test]
    fn abs_indirect_is_err() {
        test_err!(
            "[$500+X]",
            Err(AddressingModeError::DpOutOfBounds(
                "$500",
                DirectPageFlag::Zero,
                0x500
            ))
        );
        test_err!(
            "[ $500 ] + Y",
            Err(AddressingModeError::DpOutOfBounds(
                "$500",
                DirectPageFlag::Zero,
                0x500
            ))
        );

        test_err!(
            "[$500+2+X]",
            Err(AddressingModeError::DpOutOfBounds(
                "$500+2",
                DirectPageFlag::Zero,
                0x502
            ))
        );
        test_err!(
            "[ $500 + 1 ] + Y",
            Err(AddressingModeError::DpOutOfBounds(
                "$500 + 1",
                DirectPageFlag::Zero,
                0x501
            ))
        );
    }

    #[test]
    fn unknown_dp_indirect_dp_is_err() {
        test_err!(
            "[unknown+X]",
            Err(AddressingModeError::UnknownDpValue("unknown"))
        );
        test_err!(
            "[ unknown ] + Y",
            Err(AddressingModeError::UnknownDpValue("unknown"))
        );

        test_err!(
            "[unknown+2+X]",
            Err(AddressingModeError::UnknownDpValue("unknown+2"))
        );
        test_err!(
            "[ unknown + 1 ] + Y",
            Err(AddressingModeError::UnknownDpValue("unknown + 1"))
        );
    }

    #[test]
    fn absolute_or_direct_page() {
        test_ok!("128", AddressingMode::Dp(128));
        test_ok!("$ff", AddressingMode::Dp(0xff));
        test_ok!("$100", AddressingMode::Abs(U16Value::Known(0x100)));
        test_ok!("$ffff", AddressingMode::Abs(U16Value::Known(0xffff)));

        test_ok!("25 + 35 + 12", AddressingMode::Dp(25 + 35 + 12));
        test_ok!("100 + 100 + 100", AddressingMode::Abs(U16Value::Known(300)));

        test_ok!("unknown", AddressingMode::Abs(U16Value::Unknown("unknown")));
        test_ok!(
            "unknown + 2",
            AddressingMode::Abs(U16Value::Unknown("unknown + 2"))
        );
    }

    #[test]
    fn not_abs() {
        test_ok!("/128", AddressingMode::NotAbs(U16Value::Known(128)));
        test_ok!("/$4000", AddressingMode::NotAbs(U16Value::Known(0x4000)));
    }

    #[test]
    fn x_indexed() {
        test_ok!("128+X", AddressingMode::DpX(128));
        test_ok!("$ff+X", AddressingMode::DpX(0xff));
        test_ok!("$100 + X", AddressingMode::AbsX(U16Value::Known(0x100)));
        test_ok!("$ffff +X", AddressingMode::AbsX(U16Value::Known(0xffff)));

        test_ok!("25 + 35 + 12 + X", AddressingMode::DpX(25 + 35 + 12));
        test_ok!(
            "100 + 100 + 100 + X",
            AddressingMode::AbsX(U16Value::Known(300))
        );

        test_ok!(
            "unknown+X",
            AddressingMode::AbsX(U16Value::Unknown("unknown"))
        );
        test_ok!(
            "unknown + 2+X",
            AddressingMode::AbsX(U16Value::Unknown("unknown + 2"))
        );
        test_ok!(
            "unknown + 3 + X",
            AddressingMode::AbsX(U16Value::Unknown("unknown + 3"))
        );
    }

    #[test]
    fn y_indexed() {
        test_ok!("128+Y", AddressingMode::DpY(128));
        test_ok!("$ff+Y", AddressingMode::DpY(0xff));

        test_ok!("$100 + Y", AddressingMode::AbsY(U16Value::Known(0x100)));
        test_ok!("$ffff +Y", AddressingMode::AbsY(U16Value::Known(0xffff)));

        test_ok!("25 + 35 + 12 + Y", AddressingMode::DpY(25 + 35 + 12));
        test_ok!(
            "100 + 100 + 100 + Y",
            AddressingMode::AbsY(U16Value::Known(300))
        );

        test_ok!(
            "unknown+Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown"))
        );
        test_ok!(
            "unknown + 2+Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown + 2"))
        );
        test_ok!(
            "unknown + 3 + Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown + 3"))
        );
    }

    #[test]
    fn direct_page_set() {
        let state = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::One;
            s
        };

        macro_rules! test {
            ($s:literal, $v:expr) => {
                assert_eq!(parse_addressing_mode($s, &state), $v)
            };
        }

        test!("128", Ok(AddressingMode::Abs(U16Value::Known(128))));
        test!("$ff", Ok(AddressingMode::Abs(U16Value::Known(0xff))));
        test!("$100", Ok(AddressingMode::Dp(0)));
        test!("$1ff", Ok(AddressingMode::Dp(0xff)));
        test!("$200", Ok(AddressingMode::Abs(U16Value::Known(0x200))));
        test!("$100 + 50", Ok(AddressingMode::Dp(50)));

        test!("128+X", Ok(AddressingMode::AbsX(U16Value::Known(128))));
        test!("$ff+X", Ok(AddressingMode::AbsX(U16Value::Known(0xff))));
        test!("$100 + X", Ok(AddressingMode::DpX(0)));
        test!("$1ff + X", Ok(AddressingMode::DpX(0xff)));
        test!("$200+X", Ok(AddressingMode::AbsX(U16Value::Known(0x200))));
        test!("$100 + 50 + X", Ok(AddressingMode::DpX(50)));

        test!("[$100 + X]", Ok(AddressingMode::DpIndirectX(0)));
        test!("[$1ff + X]", Ok(AddressingMode::DpIndirectX(0xff)));
        test!(
            "[$ff+X]",
            Err(AddressingModeError::DpOutOfBounds(
                "$ff",
                DirectPageFlag::One,
                0xff
            ))
        );
        test!(
            "[$200+X]",
            Err(AddressingModeError::DpOutOfBounds(
                "$200",
                DirectPageFlag::One,
                0x200
            ))
        );
        test!(
            "[250+250+250+X]",
            Err(AddressingModeError::DpOutOfBounds(
                "250+250+250",
                DirectPageFlag::One,
                750
            ))
        );

        test!("[$100] + Y", Ok(AddressingMode::DpIndirectY(0)));
        test!("[$1ff] + Y", Ok(AddressingMode::DpIndirectY(0xff)));
        test!(
            "[$ff]+Y",
            Err(AddressingModeError::DpOutOfBounds(
                "$ff",
                DirectPageFlag::One,
                0xff
            ))
        );
        test!(
            "[$200]+Y",
            Err(AddressingModeError::DpOutOfBounds(
                "$200",
                DirectPageFlag::One,
                0x200
            ))
        );
        test!(
            "[250+250+250]+Y",
            Err(AddressingModeError::DpOutOfBounds(
                "250+250+250",
                DirectPageFlag::One,
                750
            ))
        );
    }

    #[test]
    fn test_no_dp_addressing_modes() {
        let state0 = State::new(0x200);

        let state1 = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::One;
            s
        };

        macro_rules! test {
            ($s:literal, $v:expr) => {
                assert_eq!(parse_no_dp_addressing_mode($s, &state0), $v);
                assert_eq!(parse_no_dp_addressing_mode($s, &state1), $v);
            };
        }

        test!("A", Ok(AddressingMode::A));
        test!("X", Ok(AddressingMode::X));
        test!("Y", Ok(AddressingMode::Y));
        test!("YA", Ok(AddressingMode::Ya));
        test!("SP", Ok(AddressingMode::Sp));
        test!("PSW", Ok(AddressingMode::Psw));
        test!("C", Ok(AddressingMode::C));

        test!("(X)", Ok(AddressingMode::XIndirect));
        test!("(X)+", Ok(AddressingMode::XIndirectIncrement));
        test!("(Y)", Ok(AddressingMode::YIndirect));

        test!("#123", Ok(AddressingMode::Immediate(U8Value::Known(123))));

        test!("$00", Ok(AddressingMode::Abs(U16Value::Known(0))));
        test!("$ff", Ok(AddressingMode::Abs(U16Value::Known(0xff))));
        test!("$100", Ok(AddressingMode::Abs(U16Value::Known(0x100))));
        test!("$200", Ok(AddressingMode::Abs(U16Value::Known(0x200))));

        test!("128+X", Ok(AddressingMode::AbsX(U16Value::Known(128))));
        test!("$ff+X", Ok(AddressingMode::AbsX(U16Value::Known(0xff))));
        test!("$100+X", Ok(AddressingMode::AbsX(U16Value::Known(0x100))));
        test!("$200+X", Ok(AddressingMode::AbsX(U16Value::Known(0x200))));
        test!("$100+50+X", Ok(AddressingMode::AbsX(U16Value::Known(306))));

        test!("128+Y", Ok(AddressingMode::AbsY(U16Value::Known(128))));
        test!("$ff+Y", Ok(AddressingMode::AbsY(U16Value::Known(0xff))));
        test!("$100+Y", Ok(AddressingMode::AbsY(U16Value::Known(0x100))));
        test!("$200+Y", Ok(AddressingMode::AbsY(U16Value::Known(0x200))));
        test!("$100+50+Y", Ok(AddressingMode::AbsY(U16Value::Known(306))));

        test!(
            "[$00 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0)))
        );
        test!(
            "[$ff + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0xff)))
        );
        test!(
            "[$1ff + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0x1ff)))
        );
        test!(
            "[$200 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0x200)))
        );

        test!(
            "[$00]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0)))
        );
        test!(
            "[$ff]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0xff)))
        );
        test!(
            "[$1ff]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0x1ff)))
        );
        test!(
            "[$200]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0x200)))
        );
    }

    #[test]
    fn test_no_dp_addressing_modes_known_symbols() {
        let state0 = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::Zero;
            s.add_symbol("s1", 0x80).unwrap();
            s.add_symbol("s2", 0x180).unwrap();
            s
        };

        let state1 = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::One;
            s.add_symbol("s1", 0x80).unwrap();
            s.add_symbol("s2", 0x180).unwrap();
            s
        };

        macro_rules! test {
            ($s:literal, $v:expr) => {
                assert_eq!(parse_no_dp_addressing_mode($s, &state0), $v);
                assert_eq!(parse_no_dp_addressing_mode($s, &state1), $v);
            };
        }

        test!("#s1", Ok(AddressingMode::Immediate(U8Value::Known(0x80))));

        test!("s1", Ok(AddressingMode::Abs(U16Value::Known(0x0080))));
        test!("s2", Ok(AddressingMode::Abs(U16Value::Known(0x0180))));

        test!("s1+X", Ok(AddressingMode::AbsX(U16Value::Known(0x0080))));
        test!("s2+X", Ok(AddressingMode::AbsX(U16Value::Known(0x0180))));
        test!("s2+1+X", Ok(AddressingMode::AbsX(U16Value::Known(0x0181))));

        test!("s1+Y", Ok(AddressingMode::AbsY(U16Value::Known(0x0080))));
        test!("s2+Y", Ok(AddressingMode::AbsY(U16Value::Known(0x0180))));
        test!("s2+1+Y", Ok(AddressingMode::AbsY(U16Value::Known(0x0181))));

        test!(
            "[s1 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0x0080)))
        );
        test!(
            "[s2 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0x0180)))
        );
        test!(
            "[s1 + 1 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Known(0x0081)))
        );

        test!(
            "[s1]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0x0080)))
        );
        test!(
            "[s2]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0x0180)))
        );
        test!(
            "[s1+1]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Known(0x0081)))
        );
    }

    #[test]
    fn test_no_dp_addressing_modes_unknown_symbols() {
        let state0 = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::Zero;
            s
        };

        let state1 = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::One;
            s
        };

        macro_rules! test {
            ($s:literal, $v:expr) => {
                assert_eq!(parse_no_dp_addressing_mode($s, &state0), $v);
                assert_eq!(parse_no_dp_addressing_mode($s, &state1), $v);
            };
        }

        test!("#u", Ok(AddressingMode::Immediate(U8Value::Unknown("u"))));

        test!("u", Ok(AddressingMode::Abs(U16Value::Unknown("u"))));

        test!("u+X", Ok(AddressingMode::AbsX(U16Value::Unknown("u"))));
        test!("u+1+X", Ok(AddressingMode::AbsX(U16Value::Unknown("u+1"))));

        test!("u +Y", Ok(AddressingMode::AbsY(U16Value::Unknown("u"))));
        test!(
            "u + 1 + Y",
            Ok(AddressingMode::AbsY(U16Value::Unknown("u + 1")))
        );

        test!(
            "[u + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Unknown("u")))
        );
        test!(
            "[u + 1 + X]",
            Ok(AddressingMode::AbsIndirectX(U16Value::Unknown("u + 1")))
        );

        test!(
            "[u]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Unknown("u")))
        );
        test!(
            "[u+1]+Y",
            Ok(AddressingMode::AbsIndirectY(U16Value::Unknown("u+1")))
        );
    }
}

// Instruction opcodes from the SNESdev wiki.
//
// https://snes.nesdev.org/wiki/SPC-700_instruction_set
#[cfg(test)]
mod instruction_tests {
    use super::{DirectPageFlag, InstructionError, OutputError, State};

    use crate::{
        errors::{FileErrors, LineNo},
        state::process_pending_output_expressions,
        string::split_first_word,
    };
    use std::panic::Location;

    pub fn process_line<'a>(
        line: &'a str,
        state: &mut State<'a>,
    ) -> Result<(), InstructionError<'a>> {
        let (instruction, arguments) = split_first_word(line);
        super::process_instruction(instruction, arguments, state)
    }

    #[track_caller]
    fn test_sym(line: &str, state: &State, expected: &[u8]) {
        let mut state = state.clone();

        process_line(line, &mut state).unwrap();

        assert_eq!(
            state.output(),
            expected,
            "{line:?} @ {}",
            Location::caller()
        );
    }

    #[track_caller]
    fn test(line: &str, expected: &[u8]) {
        let mut state = State::new(0x200);

        process_line(line, &mut state).unwrap();

        assert_eq!(
            state.output(),
            expected,
            "{line:?} @ {}",
            Location::caller()
        );
    }

    #[track_caller]
    fn test_result(line: &str, expected: Result<(), InstructionError>) {
        let mut state = State::new(0x200);

        assert_eq!(process_line(line, &mut state), expected);
    }

    #[test]
    fn instructions_with_literals() {
        test("mov A, #$ff", &[0xE8, 0xff]);
        test("mov A, (X)", &[0xE6]);
        test("mov A, (X)+", &[0xBF]);
        test("mov A, $10", &[0xE4, 0x10]);
        test("mov A, $10+X", &[0xF4, 0x10]);
        test("mov A, $3ff", &[0xE5, 0xff, 0x3]);
        test("mov A, $3ff+X", &[0xF5, 0xff, 0x3]);
        test("mov A, $3ff+Y", &[0xF6, 0xff, 0x3]);
        test("mov A, [$10+X]", &[0xE7, 0x10]);
        test("mov A, [$10]+Y", &[0xF7, 0x10]);
        test("mov X, #$ff", &[0xCD, 0xff]);
        test("mov X, $10", &[0xF8, 0x10]);
        test("mov X, $10+Y", &[0xF9, 0x10]);
        test("mov X, $3ff", &[0xE9, 0xff, 0x3]);
        test("mov Y, #$ff", &[0x8D, 0xff]);
        test("mov Y, $10", &[0xEB, 0x10]);
        test("mov Y, $10+X", &[0xFB, 0x10]);
        test("mov Y, $3ff", &[0xEC, 0xff, 0x3]);
        test("mov (X), A", &[0xC6]);
        test("mov (X)+, A", &[0xAF]);
        test("mov $10, A", &[0xC4, 0x10]);
        test("mov $10+X, A", &[0xD4, 0x10]);
        test("mov $3ff, A", &[0xC5, 0xff, 0x3]);
        test("mov $3ff+X, A", &[0xD5, 0xff, 0x3]);
        test("mov $3ff+Y, A", &[0xD6, 0xff, 0x3]);
        test("mov [$10+X], A", &[0xC7, 0x10]);
        test("mov [$10]+Y, A", &[0xD7, 0x10]);
        test("mov $10, X", &[0xD8, 0x10]);
        test("mov $10+Y, X", &[0xD9, 0x10]);
        test("mov $3ff, X", &[0xC9, 0xff, 0x3]);
        test("mov $10, Y", &[0xCB, 0x10]);
        test("mov $10+X, Y", &[0xDB, 0x10]);
        test("mov $3ff, Y", &[0xCC, 0xff, 0x3]);
        test("mov A, X", &[0x7D]);
        test("mov A, Y", &[0xDD]);
        test("mov X, A", &[0x5D]);
        test("mov Y, A", &[0xFD]);
        test("mov X, SP", &[0x9D]);
        test("mov SP, X", &[0xBD]);
        test("mov $20, $30", &[0xFA, 0x20, 0x30]);
        test("mov $40, #$50", &[0x8F, 0x40, 0x50]);
        test("adc A, #$ff", &[0x88, 0xff]);
        test("adc A, (X)", &[0x86]);
        test("adc A, $10", &[0x84, 0x10]);
        test("adc A, $10+X", &[0x94, 0x10]);
        test("adc A, $3ff", &[0x85, 0xff, 0x3]);
        test("adc A, $3ff+X", &[0x95, 0xff, 0x3]);
        test("adc A, $3ff+Y", &[0x96, 0xff, 0x3]);
        test("adc A, [$10+X]", &[0x87, 0x10]);
        test("adc A, [$10]+Y", &[0x97, 0x10]);
        test("adc (X), (Y)", &[0x99]);
        test("adc $20, $30", &[0x89, 0x20, 0x30]);
        test("adc $40, #$50", &[0x98, 0x40, 0x50]);
        test("sbc A, #$ff", &[0xA8, 0xff]);
        test("sbc A, (X)", &[0xA6]);
        test("sbc A, $10", &[0xA4, 0x10]);
        test("sbc A, $10+X", &[0xB4, 0x10]);
        test("sbc A, $3ff", &[0xA5, 0xff, 0x3]);
        test("sbc A, $3ff+X", &[0xB5, 0xff, 0x3]);
        test("sbc A, $3ff+Y", &[0xB6, 0xff, 0x3]);
        test("sbc A, [$10+X]", &[0xA7, 0x10]);
        test("sbc A, [$10]+Y", &[0xB7, 0x10]);
        test("sbc (X), (Y)", &[0xB9]);
        test("sbc $20, $30", &[0xA9, 0x20, 0x30]);
        test("sbc $40, #$50", &[0xB8, 0x40, 0x50]);
        test("cmp A, #$ff", &[0x68, 0xff]);
        test("cmp A, (X)", &[0x66]);
        test("cmp A, $10", &[0x64, 0x10]);
        test("cmp A, $10+X", &[0x74, 0x10]);
        test("cmp A, $3ff", &[0x65, 0xff, 0x3]);
        test("cmp A, $3ff+X", &[0x75, 0xff, 0x3]);
        test("cmp A, $3ff+Y", &[0x76, 0xff, 0x3]);
        test("cmp A, [$10+X]", &[0x67, 0x10]);
        test("cmp A, [$10]+Y", &[0x77, 0x10]);
        test("cmp (X), (Y)", &[0x79]);
        test("cmp $20, $30", &[0x69, 0x20, 0x30]);
        test("cmp $40, #$50", &[0x78, 0x40, 0x50]);
        test("cmp X, #$ff", &[0xC8, 0xff]);
        test("cmp X, $10", &[0x3E, 0x10]);
        test("cmp X, $3ff", &[0x1E, 0xff, 0x3]);
        test("cmp Y, #$ff", &[0xAD, 0xff]);
        test("cmp Y, $10", &[0x7E, 0x10]);
        test("cmp Y, $3ff", &[0x5E, 0xff, 0x3]);
        test("and A, #$ff", &[0x28, 0xff]);
        test("and A, (X)", &[0x26]);
        test("and A, $10", &[0x24, 0x10]);
        test("and A, $10+X", &[0x34, 0x10]);
        test("and A, $3ff", &[0x25, 0xff, 0x3]);
        test("and A, $3ff+X", &[0x35, 0xff, 0x3]);
        test("and A, $3ff+Y", &[0x36, 0xff, 0x3]);
        test("and A, [$10+X]", &[0x27, 0x10]);
        test("and A, [$10]+Y", &[0x37, 0x10]);
        test("and (X), (Y)", &[0x39]);
        test("and $20, $30", &[0x29, 0x20, 0x30]);
        test("and $40, #$50", &[0x38, 0x40, 0x50]);
        test("or A, #$ff", &[0x08, 0xff]);
        test("or A, (X)", &[0x06]);
        test("or A, $10", &[0x04, 0x10]);
        test("or A, $10+X", &[0x14, 0x10]);
        test("or A, $3ff", &[0x05, 0xff, 0x3]);
        test("or A, $3ff+X", &[0x15, 0xff, 0x3]);
        test("or A, $3ff+Y", &[0x16, 0xff, 0x3]);
        test("or A, [$10+X]", &[0x07, 0x10]);
        test("or A, [$10]+Y", &[0x17, 0x10]);
        test("or (X), (Y)", &[0x19]);
        test("or $20, $30", &[0x09, 0x20, 0x30]);
        test("or $40, #$50", &[0x18, 0x40, 0x50]);
        test("eor A, #$ff", &[0x48, 0xff]);
        test("eor A, (X)", &[0x46]);
        test("eor A, $10", &[0x44, 0x10]);
        test("eor A, $10+X", &[0x54, 0x10]);
        test("eor A, $3ff", &[0x45, 0xff, 0x3]);
        test("eor A, $3ff+X", &[0x55, 0xff, 0x3]);
        test("eor A, $3ff+Y", &[0x56, 0xff, 0x3]);
        test("eor A, [$10+X]", &[0x47, 0x10]);
        test("eor A, [$10]+Y", &[0x57, 0x10]);
        test("eor (X), (Y)", &[0x59]);
        test("eor $20, $30", &[0x49, 0x20, 0x30]);
        test("eor $40, #$50", &[0x58, 0x40, 0x50]);
        test("inc A", &[0xBC]);
        test("inc $10", &[0xAB, 0x10]);
        test("inc $10+X", &[0xBB, 0x10]);
        test("inc $3ff", &[0xAC, 0xff, 0x3]);
        test("inc X", &[0x3D]);
        test("inc Y", &[0xFC]);
        test("dec A", &[0x9C]);
        test("dec $10", &[0x8B, 0x10]);
        test("dec $10+X", &[0x9B, 0x10]);
        test("dec $3ff", &[0x8C, 0xff, 0x3]);
        test("dec X", &[0x1D]);
        test("dec Y", &[0xDC]);
        test("asl A", &[0x1C]);
        test("asl $10", &[0x0B, 0x10]);
        test("asl $10+X", &[0x1B, 0x10]);
        test("asl $3ff", &[0x0C, 0xff, 0x3]);
        test("lsr A", &[0x5C]);
        test("lsr $10", &[0x4B, 0x10]);
        test("lsr $10+X", &[0x5B, 0x10]);
        test("lsr $3ff", &[0x4C, 0xff, 0x3]);
        test("rol A", &[0x3C]);
        test("rol $10", &[0x2B, 0x10]);
        test("rol $10+X", &[0x3B, 0x10]);
        test("rol $3ff", &[0x2C, 0xff, 0x3]);
        test("ror A", &[0x7C]);
        test("ror $10", &[0x6B, 0x10]);
        test("ror $10+X", &[0x7B, 0x10]);
        test("ror $3ff", &[0x6C, 0xff, 0x3]);
        test("xcn A", &[0x9F]);
        test("movw YA, $10", &[0xBA, 0x10]);
        test("movw $10, YA", &[0xDA, 0x10]);
        test("incw $10", &[0x3A, 0x10]);
        test("decw $10", &[0x1A, 0x10]);
        test("addw YA, $10", &[0x7A, 0x10]);
        test("subw YA, $10", &[0x9A, 0x10]);
        test("cmpw YA, $10", &[0x5A, 0x10]);
        test("mul YA", &[0xCF]);
        test("div YA, X", &[0x9E]);
        test("daa A", &[0xDF]);
        test("das A", &[0xBE]);
        test("bra rel", &[0x2F, 0]);
        test("beq rel", &[0xF0, 0]);
        test("bne rel", &[0xD0, 0]);
        test("bcs rel", &[0xB0, 0]);
        test("bcc rel", &[0x90, 0]);
        test("bvs rel", &[0x70, 0]);
        test("bvc rel", &[0x50, 0]);
        test("bmi rel", &[0x30, 0]);
        test("bpl rel", &[0x10, 0]);
        test("bbs $10, 0, rel", &[0x03, 0x10, 0]);
        test("bbc $10, 0, rel", &[0x13, 0x10, 0]);
        test("cbne $10, rel", &[0x2E, 0x10, 0x00]);
        test("cbne $10+X, rel", &[0xDE, 0x10, 0x00]);
        test("dbnz $10, rel", &[0x6E, 0x10, 0x00]);
        test("dbnz Y, rel", &[0xFE, 0]);
        test("jmp $3ff", &[0x5F, 0xff, 0x3]);
        test("jmp [$3ff+X]", &[0x1F, 0xff, 0x3]);
        test("call $3ff", &[0x3F, 0xff, 0x3]);
        test("pcall $ffff", &[0x4f, 0xff]);
        test("tcall 0", &[0x01]);
        test("brk", &[0x0F]);
        test("ret", &[0x6F]);
        test("reti", &[0x7F]);
        test("push A", &[0x2D]);
        test("push X", &[0x4D]);
        test("push Y", &[0x6D]);
        test("push PSW", &[0x0D]);
        test("pop A", &[0xAE]);
        test("pop X", &[0xCE]);
        test("pop Y", &[0xEE]);
        test("pop PSW", &[0x8E]);
        test("set1 $10, 0", &[0x02, 0x10]);
        test("clr1 $10, 0", &[0x12, 0x10]);
        test("tset1 $3ff", &[0x0E, 0xff, 0x3]);
        test("tclr1 $3ff", &[0x4E, 0xff, 0x3]);
        test("and1 C, $3ff, 0", &[0x4A, 0xff, 0x3]);
        test("and1 C, /$3ff, 0", &[0x6A, 0xff, 0x3]);
        test("or1 C, $3ff, 0", &[0x0A, 0xff, 0x3]);
        test("or1 C, /$3ff, 0", &[0x2A, 0xff, 0x3]);
        test("eor1 C, $3ff, 0", &[0x8A, 0xff, 0x3]);
        test("not1 $3ff, 0", &[0xEA, 0xff, 0x3]);
        test("mov1 C, $3ff, 0", &[0xAA, 0xff, 0x3]);
        test("mov1 $3ff, 0, C", &[0xCA, 0xff, 0x3]);
        test("clrc", &[0x60]);
        test("setc", &[0x80]);
        test("notc", &[0xED]);
        test("clrv", &[0xE0]);
        test("clrp", &[0x20]);
        test("setp", &[0x40]);
        test("ei", &[0xA0]);
        test("di", &[0xC0]);
        test("nop", &[0x00]);
        test("sleep", &[0xEF]);
        test("stop", &[0xFF]);
    }

    #[test]
    fn instructions_with_known_symbols() {
        let s = {
            let mut s = State::new(0x200);
            s.add_symbol("imm", 0x80).unwrap();
            s.add_symbol("dp", 0x10).unwrap();
            s.add_symbol("abs", 0x3ff).unwrap();
            s.add_symbol("bit", 0).unwrap();
            s.add_symbol("up", 0xffff).unwrap();
            s.add_symbol("n", 0xc).unwrap();
            s
        };

        test_sym("mov A, #imm", &s, &[0xE8, 0x80]);
        test_sym("mov A, (X)", &s, &[0xE6]);
        test_sym("mov A, (X)+", &s, &[0xBF]);
        test_sym("mov A, dp", &s, &[0xE4, 0x10]);
        test_sym("mov A, dp+X", &s, &[0xF4, 0x10]);
        test_sym("mov A, abs", &s, &[0xE5, 0xff, 0x3]);
        test_sym("mov A, abs+X", &s, &[0xF5, 0xff, 0x3]);
        test_sym("mov A, abs+Y", &s, &[0xF6, 0xff, 0x3]);
        test_sym("mov A, [dp+X]", &s, &[0xE7, 0x10]);
        test_sym("mov A, [dp]+Y", &s, &[0xF7, 0x10]);
        test_sym("mov X, #imm", &s, &[0xCD, 0x80]);
        test_sym("mov X, dp", &s, &[0xF8, 0x10]);
        test_sym("mov X, dp+Y", &s, &[0xF9, 0x10]);
        test_sym("mov X, abs", &s, &[0xE9, 0xff, 0x3]);
        test_sym("mov Y, #imm", &s, &[0x8D, 0x80]);
        test_sym("mov Y, dp", &s, &[0xEB, 0x10]);
        test_sym("mov Y, dp+X", &s, &[0xFB, 0x10]);
        test_sym("mov Y, abs", &s, &[0xEC, 0xff, 0x3]);
        test_sym("mov (X), A", &s, &[0xC6]);
        test_sym("mov (X)+, A", &s, &[0xAF]);
        test_sym("mov dp, A", &s, &[0xC4, 0x10]);
        test_sym("mov dp+X, A", &s, &[0xD4, 0x10]);
        test_sym("mov abs, A", &s, &[0xC5, 0xff, 0x3]);
        test_sym("mov abs+X, A", &s, &[0xD5, 0xff, 0x3]);
        test_sym("mov abs+Y, A", &s, &[0xD6, 0xff, 0x3]);
        test_sym("mov [dp+X], A", &s, &[0xC7, 0x10]);
        test_sym("mov [dp]+Y, A", &s, &[0xD7, 0x10]);
        test_sym("mov dp, X", &s, &[0xD8, 0x10]);
        test_sym("mov dp+Y, X", &s, &[0xD9, 0x10]);
        test_sym("mov abs, X", &s, &[0xC9, 0xff, 0x3]);
        test_sym("mov dp, Y", &s, &[0xCB, 0x10]);
        test_sym("mov dp+X, Y", &s, &[0xDB, 0x10]);
        test_sym("mov abs, Y", &s, &[0xCC, 0xff, 0x3]);
        test_sym("mov A, X", &s, &[0x7D]);
        test_sym("mov A, Y", &s, &[0xDD]);
        test_sym("mov X, A", &s, &[0x5D]);
        test_sym("mov Y, A", &s, &[0xFD]);
        test_sym("mov X, SP", &s, &[0x9D]);
        test_sym("mov SP, X", &s, &[0xBD]);
        test_sym("mov dp, dp", &s, &[0xFA, 0x10, 0x10]);
        test_sym("mov dp, #imm", &s, &[0x8F, 0x10, 0x80]);
        test_sym("adc A, #imm", &s, &[0x88, 0x80]);
        test_sym("adc A, (X)", &s, &[0x86]);
        test_sym("adc A, dp", &s, &[0x84, 0x10]);
        test_sym("adc A, dp+X", &s, &[0x94, 0x10]);
        test_sym("adc A, abs", &s, &[0x85, 0xff, 0x3]);
        test_sym("adc A, abs+X", &s, &[0x95, 0xff, 0x3]);
        test_sym("adc A, abs+Y", &s, &[0x96, 0xff, 0x3]);
        test_sym("adc A, [dp+X]", &s, &[0x87, 0x10]);
        test_sym("adc A, [dp]+Y", &s, &[0x97, 0x10]);
        test_sym("adc (X), (Y)", &s, &[0x99]);
        test_sym("adc dp, dp", &s, &[0x89, 0x10, 0x10]);
        test_sym("adc dp, #imm", &s, &[0x98, 0x10, 0x80]);
        test_sym("sbc A, #imm", &s, &[0xA8, 0x80]);
        test_sym("sbc A, (X)", &s, &[0xA6]);
        test_sym("sbc A, dp", &s, &[0xA4, 0x10]);
        test_sym("sbc A, dp+X", &s, &[0xB4, 0x10]);
        test_sym("sbc A, abs", &s, &[0xA5, 0xff, 0x3]);
        test_sym("sbc A, abs+X", &s, &[0xB5, 0xff, 0x3]);
        test_sym("sbc A, abs+Y", &s, &[0xB6, 0xff, 0x3]);
        test_sym("sbc A, [dp+X]", &s, &[0xA7, 0x10]);
        test_sym("sbc A, [dp]+Y", &s, &[0xB7, 0x10]);
        test_sym("sbc (X), (Y)", &s, &[0xB9]);
        test_sym("sbc dp, dp", &s, &[0xA9, 0x10, 0x10]);
        test_sym("sbc dp, #imm", &s, &[0xB8, 0x10, 0x80]);
        test_sym("cmp A, #imm", &s, &[0x68, 0x80]);
        test_sym("cmp A, (X)", &s, &[0x66]);
        test_sym("cmp A, dp", &s, &[0x64, 0x10]);
        test_sym("cmp A, dp+X", &s, &[0x74, 0x10]);
        test_sym("cmp A, abs", &s, &[0x65, 0xff, 0x3]);
        test_sym("cmp A, abs+X", &s, &[0x75, 0xff, 0x3]);
        test_sym("cmp A, abs+Y", &s, &[0x76, 0xff, 0x3]);
        test_sym("cmp A, [dp+X]", &s, &[0x67, 0x10]);
        test_sym("cmp A, [dp]+Y", &s, &[0x77, 0x10]);
        test_sym("cmp (X), (Y)", &s, &[0x79]);
        test_sym("cmp dp, dp", &s, &[0x69, 0x10, 0x10]);
        test_sym("cmp dp, #imm", &s, &[0x78, 0x10, 0x80]);
        test_sym("cmp X, #imm", &s, &[0xC8, 0x80]);
        test_sym("cmp X, dp", &s, &[0x3E, 0x10]);
        test_sym("cmp X, abs", &s, &[0x1E, 0xff, 0x3]);
        test_sym("cmp Y, #imm", &s, &[0xAD, 0x80]);
        test_sym("cmp Y, dp", &s, &[0x7E, 0x10]);
        test_sym("cmp Y, abs", &s, &[0x5E, 0xff, 0x3]);
        test_sym("and A, #imm", &s, &[0x28, 0x80]);
        test_sym("and A, (X)", &s, &[0x26]);
        test_sym("and A, dp", &s, &[0x24, 0x10]);
        test_sym("and A, dp+X", &s, &[0x34, 0x10]);
        test_sym("and A, abs", &s, &[0x25, 0xff, 0x3]);
        test_sym("and A, abs+X", &s, &[0x35, 0xff, 0x3]);
        test_sym("and A, abs+Y", &s, &[0x36, 0xff, 0x3]);
        test_sym("and A, [dp+X]", &s, &[0x27, 0x10]);
        test_sym("and A, [dp]+Y", &s, &[0x37, 0x10]);
        test_sym("and (X), (Y)", &s, &[0x39]);
        test_sym("and dp, dp", &s, &[0x29, 0x10, 0x10]);
        test_sym("and dp, #imm", &s, &[0x38, 0x10, 0x80]);
        test_sym("or A, #imm", &s, &[0x08, 0x80]);
        test_sym("or A, (X)", &s, &[0x06]);
        test_sym("or A, dp", &s, &[0x04, 0x10]);
        test_sym("or A, dp+X", &s, &[0x14, 0x10]);
        test_sym("or A, abs", &s, &[0x05, 0xff, 0x3]);
        test_sym("or A, abs+X", &s, &[0x15, 0xff, 0x3]);
        test_sym("or A, abs+Y", &s, &[0x16, 0xff, 0x3]);
        test_sym("or A, [dp+X]", &s, &[0x07, 0x10]);
        test_sym("or A, [dp]+Y", &s, &[0x17, 0x10]);
        test_sym("or (X), (Y)", &s, &[0x19]);
        test_sym("or dp, dp", &s, &[0x09, 0x10, 0x10]);
        test_sym("or dp, #imm", &s, &[0x18, 0x10, 0x80]);
        test_sym("eor A, #imm", &s, &[0x48, 0x80]);
        test_sym("eor A, (X)", &s, &[0x46]);
        test_sym("eor A, dp", &s, &[0x44, 0x10]);
        test_sym("eor A, dp+X", &s, &[0x54, 0x10]);
        test_sym("eor A, abs", &s, &[0x45, 0xff, 0x3]);
        test_sym("eor A, abs+X", &s, &[0x55, 0xff, 0x3]);
        test_sym("eor A, abs+Y", &s, &[0x56, 0xff, 0x3]);
        test_sym("eor A, [dp+X]", &s, &[0x47, 0x10]);
        test_sym("eor A, [dp]+Y", &s, &[0x57, 0x10]);
        test_sym("eor (X), (Y)", &s, &[0x59]);
        test_sym("eor dp, dp", &s, &[0x49, 0x10, 0x10]);
        test_sym("eor dp, #imm", &s, &[0x58, 0x10, 0x80]);
        test_sym("inc A", &s, &[0xBC]);
        test_sym("inc dp", &s, &[0xAB, 0x10]);
        test_sym("inc dp+X", &s, &[0xBB, 0x10]);
        test_sym("inc abs", &s, &[0xAC, 0xff, 0x3]);
        test_sym("inc X", &s, &[0x3D]);
        test_sym("inc Y", &s, &[0xFC]);
        test_sym("dec A", &s, &[0x9C]);
        test_sym("dec dp", &s, &[0x8B, 0x10]);
        test_sym("dec dp+X", &s, &[0x9B, 0x10]);
        test_sym("dec abs", &s, &[0x8C, 0xff, 0x3]);
        test_sym("dec X", &s, &[0x1D]);
        test_sym("dec Y", &s, &[0xDC]);
        test_sym("asl A", &s, &[0x1C]);
        test_sym("asl dp", &s, &[0x0B, 0x10]);
        test_sym("asl dp+X", &s, &[0x1B, 0x10]);
        test_sym("asl abs", &s, &[0x0C, 0xff, 0x3]);
        test_sym("lsr A", &s, &[0x5C]);
        test_sym("lsr dp", &s, &[0x4B, 0x10]);
        test_sym("lsr dp+X", &s, &[0x5B, 0x10]);
        test_sym("lsr abs", &s, &[0x4C, 0xff, 0x3]);
        test_sym("rol A", &s, &[0x3C]);
        test_sym("rol dp", &s, &[0x2B, 0x10]);
        test_sym("rol dp+X", &s, &[0x3B, 0x10]);
        test_sym("rol abs", &s, &[0x2C, 0xff, 0x3]);
        test_sym("ror A", &s, &[0x7C]);
        test_sym("ror dp", &s, &[0x6B, 0x10]);
        test_sym("ror dp+X", &s, &[0x7B, 0x10]);
        test_sym("ror abs", &s, &[0x6C, 0xff, 0x3]);
        test_sym("xcn A", &s, &[0x9F]);
        test_sym("movw YA, dp", &s, &[0xBA, 0x10]);
        test_sym("movw dp, YA", &s, &[0xDA, 0x10]);
        test_sym("incw dp", &s, &[0x3A, 0x10]);
        test_sym("decw dp", &s, &[0x1A, 0x10]);
        test_sym("addw YA, dp", &s, &[0x7A, 0x10]);
        test_sym("subw YA, dp", &s, &[0x9A, 0x10]);
        test_sym("cmpw YA, dp", &s, &[0x5A, 0x10]);
        test_sym("mul YA", &s, &[0xCF]);
        test_sym("div YA, X", &s, &[0x9E]);
        test_sym("daa A", &s, &[0xDF]);
        test_sym("das A", &s, &[0xBE]);
        test_sym("bra rel", &s, &[0x2F, 0]);
        test_sym("beq rel", &s, &[0xF0, 0]);
        test_sym("bne rel", &s, &[0xD0, 0]);
        test_sym("bcs rel", &s, &[0xB0, 0]);
        test_sym("bcc rel", &s, &[0x90, 0]);
        test_sym("bvs rel", &s, &[0x70, 0]);
        test_sym("bvc rel", &s, &[0x50, 0]);
        test_sym("bmi rel", &s, &[0x30, 0]);
        test_sym("bpl rel", &s, &[0x10, 0]);
        test_sym("bbs dp, 0, rel", &s, &[0x03, 0x10, 0]);
        test_sym("bbc dp, 0, rel", &s, &[0x13, 0x10, 0]);
        test_sym("cbne dp, rel", &s, &[0x2E, 0x10, 0]);
        test_sym("cbne dp+X, rel", &s, &[0xDE, 0x10, 0]);
        test_sym("dbnz dp, rel", &s, &[0x6E, 0x10, 0]);
        test_sym("dbnz Y, rel", &s, &[0xFE, 0]);
        test_sym("jmp abs", &s, &[0x5F, 0xff, 0x3]);
        test_sym("jmp [abs+X]", &s, &[0x1F, 0xff, 0x3]);
        test_sym("call abs", &s, &[0x3F, 0xff, 0x3]);
        test_sym("pcall up", &s, &[0x4f, 0xff]);
        test_sym("tcall n", &s, &[0xc1]);
        test_sym("brk", &s, &[0x0F]);
        test_sym("ret", &s, &[0x6F]);
        test_sym("reti", &s, &[0x7F]);
        test_sym("push A", &s, &[0x2D]);
        test_sym("push X", &s, &[0x4D]);
        test_sym("push Y", &s, &[0x6D]);
        test_sym("push PSW", &s, &[0x0D]);
        test_sym("pop A", &s, &[0xAE]);
        test_sym("pop X", &s, &[0xCE]);
        test_sym("pop Y", &s, &[0xEE]);
        test_sym("pop PSW", &s, &[0x8E]);
        test_sym("set1 dp, bit", &s, &[0x02, 0x10]);
        test_sym("clr1 dp, bit", &s, &[0x12, 0x10]);
        test_sym("tset1 abs", &s, &[0x0E, 0xff, 0x3]);
        test_sym("tclr1 abs", &s, &[0x4E, 0xff, 0x3]);
        test_sym("and1 C, abs, bit", &s, &[0x4A, 0xff, 0x3]);
        test_sym("and1 C, /abs, bit", &s, &[0x6A, 0xff, 0x3]);
        test_sym("or1 C, abs, bit", &s, &[0x0A, 0xff, 0x3]);
        test_sym("or1 C, /abs, bit", &s, &[0x2A, 0xff, 0x3]);
        test_sym("eor1 C, abs, bit", &s, &[0x8A, 0xff, 0x3]);
        test_sym("not1 abs, bit", &s, &[0xEA, 0xff, 0x3]);
        test_sym("mov1 C, abs, bit", &s, &[0xAA, 0xff, 0x3]);
        test_sym("mov1 abs, bit, C", &s, &[0xCA, 0xff, 0x3]);
        test_sym("clrc", &s, &[0x60]);
        test_sym("setc", &s, &[0x80]);
        test_sym("notc", &s, &[0xED]);
        test_sym("clrv", &s, &[0xE0]);
        test_sym("clrp", &s, &[0x20]);
        test_sym("setp", &s, &[0x40]);
        test_sym("ei", &s, &[0xA0]);
        test_sym("di", &s, &[0xC0]);
        test_sym("nop", &s, &[0x00]);
        test_sym("sleep", &s, &[0xEF]);
        test_sym("stop", &s, &[0xFF]);
    }

    #[test]
    fn bit_instructions_literals() {
        for b in 0..=7 {
            test(&format!("bbs 10, {b}, rel"), &[(b << 5) | 0x03, 10, 0]);
            test(&format!("bbc 10, {b}, rel"), &[(b << 5) | 0x13, 10, 0]);

            test(&format!("set1 10, {b}"), &[(b << 5) | 0x02, 10]);
            test(&format!("clr1 10, {b}"), &[(b << 5) | 0x12, 10]);
        }
    }

    #[test]
    fn bit_instructions_known_symbols() {
        for b in 0..=7 {
            let s = {
                let mut s = State::new(0x200);
                s.add_symbol("dp", 0xff).unwrap();
                s.add_symbol("bit", b.into()).unwrap();
                s
            };

            test_sym("bbs dp, bit, rel", &s, &[(b << 5) | 0x03, 0xff, 0]);
            test_sym("bbc dp, bit, rel", &s, &[(b << 5) | 0x13, 0xff, 0]);
            test_sym("set1 dp, bit", &s, &[(b << 5) | 0x02, 0xff]);
            test_sym("clr1 dp, bit", &s, &[(b << 5) | 0x12, 0xff]);
        }
    }

    #[test]
    fn memory_bit_instruction_literals() {
        for b in 0..=7 {
            test(
                &format!("and1 C, $3ff, {b}"),
                &[0x4A, 0xff, 0x03 | (b << 5)],
            );
            test(
                &format!("and1 C, /$3ff, {b}"),
                &[0x6A, 0xff, 0x03 | (b << 5)],
            );
            test(&format!("or1 C, $3ff, {b}"), &[0x0A, 0xff, 0x03 | (b << 5)]);
            test(
                &format!("or1 C, /$3ff, {b}"),
                &[0x2A, 0xff, 0x03 | (b << 5)],
            );
            test(
                &format!("eor1 C, $3ff, {b}"),
                &[0x8A, 0xff, 0x03 | (b << 5)],
            );
            test(&format!("not1 $3ff, {b}"), &[0xEA, 0xff, 0x03 | (b << 5)]);
            test(
                &format!("mov1 C, $3ff, {b}"),
                &[0xAA, 0xff, 0x03 | (b << 5)],
            );
            test(
                &format!("mov1 $3ff, {b}, C"),
                &[0xCA, 0xff, 0x03 | (b << 5)],
            );
        }
    }

    #[test]
    fn memory_bit_instruction_known_symbols() {
        for b in 0..=7 {
            let s = {
                let mut s = State::new(0x200);
                s.add_symbol("abs", 0x3ff).unwrap();
                s.add_symbol("bit", b.into()).unwrap();
                s
            };

            test_sym("and1 C, abs, bit", &s, &[0x4A, 0xff, 0x03 | (b << 5)]);
            test_sym("and1 C, /abs, bit", &s, &[0x6A, 0xff, 0x03 | (b << 5)]);
            test_sym("or1 C, abs, bit", &s, &[0x0A, 0xff, 0x03 | (b << 5)]);
            test_sym("or1 C, /abs, bit", &s, &[0x2A, 0xff, 0x03 | (b << 5)]);
            test_sym("eor1 C, abs, bit", &s, &[0x8A, 0xff, 0x03 | (b << 5)]);
            test_sym("not1 abs, bit", &s, &[0xEA, 0xff, 0x03 | (b << 5)]);
            test_sym("mov1 C, abs, bit", &s, &[0xAA, 0xff, 0x03 | (b << 5)]);
            test_sym("mov1 abs, bit, C", &s, &[0xCA, 0xff, 0x03 | (b << 5)]);
        }
    }

    #[test]
    fn tcall() {
        test("tcall 0", &[0x01]);
        test("tcall 5", &[0x51]);
        test("tcall 15", &[0xf1]);
    }

    #[test]
    fn abs_rel_and_imm_instructions_with_unknown_values() {
        test("mov A, #imm", &[0xE8, 0]);
        test("mov A, abs", &[0xE5, 0, 0]);
        test("mov A, abs+X", &[0xF5, 0, 0]);
        test("mov A, abs+Y", &[0xF6, 0, 0]);
        test("mov X, #imm", &[0xCD, 0]);
        test("mov X, abs", &[0xE9, 0, 0]);
        test("mov Y, #imm", &[0x8D, 0]);
        test("mov Y, abs", &[0xEC, 0, 0]);
        test("mov abs, A", &[0xC5, 0, 0]);
        test("mov abs+X, A", &[0xD5, 0, 0]);
        test("mov abs+Y, A", &[0xD6, 0, 0]);
        test("mov abs, X", &[0xC9, 0, 0]);
        test("mov abs, Y", &[0xCC, 0, 0]);
        test("adc A, #imm", &[0x88, 0]);
        test("adc A, abs", &[0x85, 0, 0]);
        test("adc A, abs+X", &[0x95, 0, 0]);
        test("adc A, abs+Y", &[0x96, 0, 0]);
        test("sbc A, #imm", &[0xA8, 0]);
        test("sbc A, abs", &[0xA5, 0, 0]);
        test("sbc A, abs+X", &[0xB5, 0, 0]);
        test("sbc A, abs+Y", &[0xB6, 0, 0]);
        test("cmp A, #imm", &[0x68, 0]);
        test("cmp A, abs", &[0x65, 0, 0]);
        test("cmp A, abs+X", &[0x75, 0, 0]);
        test("cmp A, abs+Y", &[0x76, 0, 0]);
        test("cmp X, #imm", &[0xC8, 0]);
        test("cmp X, abs", &[0x1E, 0, 0]);
        test("cmp Y, #imm", &[0xAD, 0]);
        test("cmp Y, abs", &[0x5E, 0, 0]);
        test("and A, #imm", &[0x28, 0]);
        test("and A, abs", &[0x25, 0, 0]);
        test("and A, abs+X", &[0x35, 0, 0]);
        test("and A, abs+Y", &[0x36, 0, 0]);
        test("or A, #imm", &[0x08, 0]);
        test("or A, abs", &[0x05, 0, 0]);
        test("or A, abs+X", &[0x15, 0, 0]);
        test("or A, abs+Y", &[0x16, 0, 0]);
        test("eor A, #imm", &[0x48, 0]);
        test("eor A, abs", &[0x45, 0, 0]);
        test("eor A, abs+X", &[0x55, 0, 0]);
        test("eor A, abs+Y", &[0x56, 0, 0]);
        test("inc abs", &[0xAC, 0, 0]);
        test("dec abs", &[0x8C, 0, 0]);
        test("asl abs", &[0x0C, 0, 0]);
        test("lsr abs", &[0x4C, 0, 0]);
        test("rol abs", &[0x2C, 0, 0]);
        test("ror abs", &[0x6C, 0, 0]);
        test("bra rel", &[0x2F, 0]);
        test("beq rel", &[0xF0, 0]);
        test("bne rel", &[0xD0, 0]);
        test("bcs rel", &[0xB0, 0]);
        test("bcc rel", &[0x90, 0]);
        test("bvs rel", &[0x70, 0]);
        test("bvc rel", &[0x50, 0]);
        test("bmi rel", &[0x30, 0]);
        test("bpl rel", &[0x10, 0]);
        test("dbnz Y, rel", &[0xFE, 0]);
        test("jmp abs", &[0x5F, 0, 0]);
        test("jmp [abs+X]", &[0x1F, 0, 0]);
        test("call abs", &[0x3F, 0, 0]);
        test("tset1 abs", &[0x0E, 0, 0]);
        test("tclr1 abs", &[0x4E, 0, 0]);
    }

    #[test]
    fn zp_address_in_abs_only_instructions() {
        test("call 10", &[0x3f, 10, 0x00]);

        test("jmp 10", &[0x5f, 10, 0x00]);
        test("jmp [10+X]", &[0x1f, 10, 0x00]);

        test("tset1 $80", &[0x0E, 0x80, 0x00]);
        test("tclr1 $80", &[0x4E, 0x80, 0x00]);

        test("tset1 $ff", &[0x0E, 0xff, 0x00]);
        test("tclr1 $ff", &[0x4E, 0xff, 0x00]);
        test("and1 C, $ff, 0", &[0x4A, 0xff, 0x00]);
        test("and1 C, /$ff, 0", &[0x6A, 0xff, 0x00]);
        test("or1 C, $ff, 0", &[0x0A, 0xff, 0x00]);
        test("or1 C, /$ff, 0", &[0x2A, 0xff, 0x00]);
        test("eor1 C, $ff, 0", &[0x8A, 0xff, 0x00]);
        test("not1 $ff, 0", &[0xEA, 0xff, 0x00]);
        test("mov1 C, $ff, 0", &[0xAA, 0xff, 0x00]);
        test("mov1 $ff, 0, C", &[0xCA, 0xff, 0x00]);
    }

    #[test]
    fn mov_dp_y_direct_page_clear() {
        let s = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::Zero;
            s.add_symbol("dp", 0x080).unwrap();
            s
        };

        test_sym("mov A, dp+X", &s, &[0xF4, 0x80]);

        test_sym("mov A, dp+Y", &s, &[0xF6, 0x80, 0x00]);
        test_sym("mov dp+Y, A", &s, &[0xD6, 0x80, 0x00]);
    }

    #[test]
    fn mov_dp_y_direct_page_set() {
        let s = {
            let mut s = State::new(0x200);
            s.direct_page = DirectPageFlag::One;
            s.add_symbol("dp", 0x180).unwrap();
            s
        };

        test_sym("mov A, dp+X", &s, &[0xF4, 0x80]);

        test_sym("mov A, dp+Y", &s, &[0xF6, 0x80, 0x01]);
        test_sym("mov dp+Y, A", &s, &[0xD6, 0x80, 0x01]);
    }

    #[test]
    fn unknown_addressing_err() {
        test_result(
            "mov #1, A",
            Err(InstructionError::UnknownInstructionArguments(
                "mov",
                "#imm, A".to_owned(),
            )),
        );

        test_result(
            "addw YA, $200",
            Err(InstructionError::UnknownInstructionArguments(
                "addw",
                "YA, abs".to_owned(),
            )),
        );

        test_result(
            "bbs $200, 3, rel",
            Err(InstructionError::UnknownInstructionArguments(
                "bbs",
                "abs, bit, rel".to_owned(),
            )),
        );

        test_result(
            "set1 $200, 0",
            Err(InstructionError::UnknownInstructionArguments(
                "set1",
                "abs, bit".to_owned(),
            )),
        );

        test_result(
            "cbne $200+X, rel",
            Err(InstructionError::UnknownInstructionArguments(
                "cbne",
                "abs+X, rel".to_owned(),
            )),
        );

        test_result(
            "mov1 $200, C, $10",
            Err(InstructionError::UnknownInstructionArguments(
                "mov1",
                "abs, C, abs".to_owned(),
            )),
        );

        test_result(
            "mov1 $200, 5, X",
            Err(InstructionError::UnknownInstructionArguments(
                "mov1",
                "abs, abs, X".to_owned(),
            )),
        );
    }

    #[test]
    fn bit_out_of_range_err() {
        test_result("set1 $20, 7", Ok(()));
        test_result(
            "set1 $20, 8",
            Err(InstructionError::InvalidBitInstructionBit),
        );

        test_result("mov1 $200, 7, C", Ok(()));
        test_result(
            "mov1 $200, 8, C",
            Err(InstructionError::InvalidBitInstructionBit),
        );
    }

    #[test]
    fn abs_bit_out_of_range_err() {
        test_result("mov1 $1fff, 3, C", Ok(()));
        test_result(
            "mov1 $2000, 3, C",
            Err(OutputError::AbsBitOutOfRange(0x2000).into()),
        );

        test_result("mov1 C, $1fff, 3", Ok(()));
        test_result(
            "mov1 C, $2000, 3",
            Err(OutputError::AbsBitOutOfRange(0x2000).into()),
        );
    }

    #[test]
    fn forward_references() {
        fn t(line: &str, expected: &[u8]) {
            let mut s = State::new(0x200);
            assert_eq!(process_line(line, &mut s), Ok(()));

            assert_ne!(s.output(), expected, "asm: {line:?}");

            s.add_symbol("imm", 10).unwrap();
            s.add_symbol("abs", 0xfff).unwrap();

            let mut errors = FileErrors::new();
            process_pending_output_expressions(&mut s, &mut errors);

            assert!(errors.is_empty());
            assert_eq!(s.output(), expected, "asm: {line:?}");
        }

        t("mov A, #imm", &[0xE8, 10]);
        t("mov A, abs", &[0xE5, 0xff, 0x0f]);
        t("mov A, abs+X", &[0xF5, 0xff, 0x0f]);
        t("mov A, abs+Y", &[0xF6, 0xff, 0x0f]);
        t("mov X, #imm", &[0xCD, 10]);
        t("mov X, abs", &[0xE9, 0xff, 0x0f]);
        t("mov Y, #imm", &[0x8D, 10]);
        t("mov Y, abs", &[0xEC, 0xff, 0x0f]);
        t("mov abs, A", &[0xC5, 0xff, 0x0f]);
        t("mov abs+X, A", &[0xD5, 0xff, 0x0f]);
        t("mov abs+Y, A", &[0xD6, 0xff, 0x0f]);
        t("mov abs, X", &[0xC9, 0xff, 0x0f]);
        t("mov abs, Y", &[0xCC, 0xff, 0x0f]);
        t("adc A, #imm", &[0x88, 10]);
        t("adc A, abs", &[0x85, 0xff, 0x0f]);
        t("adc A, abs+X", &[0x95, 0xff, 0x0f]);
        t("adc A, abs+Y", &[0x96, 0xff, 0x0f]);
        t("sbc A, #imm", &[0xA8, 10]);
        t("sbc A, abs", &[0xA5, 0xff, 0x0f]);
        t("sbc A, abs+X", &[0xB5, 0xff, 0x0f]);
        t("sbc A, abs+Y", &[0xB6, 0xff, 0x0f]);
        t("cmp A, #imm", &[0x68, 10]);
        t("cmp A, abs", &[0x65, 0xff, 0x0f]);
        t("cmp A, abs+X", &[0x75, 0xff, 0x0f]);
        t("cmp A, abs+Y", &[0x76, 0xff, 0x0f]);
        t("cmp X, #imm", &[0xC8, 10]);
        t("cmp X, abs", &[0x1E, 0xff, 0x0f]);
        t("cmp Y, #imm", &[0xAD, 10]);
        t("cmp Y, abs", &[0x5E, 0xff, 0x0f]);
        t("and A, #imm", &[0x28, 10]);
        t("and A, abs", &[0x25, 0xff, 0x0f]);
        t("and A, abs+X", &[0x35, 0xff, 0x0f]);
        t("and A, abs+Y", &[0x36, 0xff, 0x0f]);
        t("or A, #imm", &[0x08, 10]);
        t("or A, abs", &[0x05, 0xff, 0x0f]);
        t("or A, abs+X", &[0x15, 0xff, 0x0f]);
        t("or A, abs+Y", &[0x16, 0xff, 0x0f]);
        t("eor A, #imm", &[0x48, 10]);
        t("eor A, abs", &[0x45, 0xff, 0x0f]);
        t("eor A, abs+X", &[0x55, 0xff, 0x0f]);
        t("eor A, abs+Y", &[0x56, 0xff, 0x0f]);
        t("inc abs", &[0xAC, 0xff, 0x0f]);
        t("dec abs", &[0x8C, 0xff, 0x0f]);
        t("asl abs", &[0x0C, 0xff, 0x0f]);
        t("lsr abs", &[0x4C, 0xff, 0x0f]);
        t("rol abs", &[0x2C, 0xff, 0x0f]);
        t("ror abs", &[0x6C, 0xff, 0x0f]);
        t("jmp abs", &[0x5F, 0xff, 0x0f]);
        t("jmp [abs+X]", &[0x1F, 0xff, 0x0f]);
        t("call abs", &[0x3F, 0xff, 0x0f]);
        t("tset1 abs", &[0x0E, 0xff, 0x0f]);
        t("tclr1 abs", &[0x4E, 0xff, 0x0f]);

        t("tset1 abs", &[0x0E, 0xff, 0x0f]);
        t("tclr1 abs", &[0x4E, 0xff, 0x0f]);

        t("and1 C, abs, 0", &[0x4A, 0xff, 0x0f]);
        t("and1 C, /abs, 0", &[0x6A, 0xff, 0x0f]);
        t("or1 C, abs, 0", &[0x0A, 0xff, 0x0f]);
        t("or1 C, /abs, 0", &[0x2A, 0xff, 0x0f]);
        t("eor1 C, abs, 0", &[0x8A, 0xff, 0x0f]);
        t("not1 abs, 0", &[0xEA, 0xff, 0x0f]);
        t("mov1 C, abs, 0", &[0xAA, 0xff, 0x0f]);
        t("mov1 abs, 0, C", &[0xCA, 0xff, 0x0f]);

        t("and1 C, abs, 3", &[0x4A, 0xff, 0x6f]);
        t("and1 C, /abs, 3", &[0x6A, 0xff, 0x6f]);
        t("or1 C, abs, 3", &[0x0A, 0xff, 0x6f]);
        t("or1 C, /abs, 3", &[0x2A, 0xff, 0x6f]);
        t("eor1 C, abs, 3", &[0x8A, 0xff, 0x6f]);
        t("not1 abs, 3", &[0xEA, 0xff, 0x6f]);
        t("mov1 C, abs, 3", &[0xAA, 0xff, 0x6f]);
        t("mov1 abs, 3, C", &[0xCA, 0xff, 0x6f]);
    }

    #[test]
    fn forward_reference_branches() {
        fn t(line: &str, expected: &[u8]) {
            let mut s = State::new(0x200);
            assert_eq!(process_line(line, &mut s), Ok(()));

            assert_ne!(s.output(), expected, "asm: {line:?}");

            s.add_symbol("rel", 0x200).unwrap();

            let mut errors = FileErrors::new();
            process_pending_output_expressions(&mut s, &mut errors);

            assert!(errors.is_empty());
            assert_eq!(s.output(), expected, "asm: {line:?}");
        }

        assert_eq!(0u8.wrapping_sub(2), 0xfe);
        t("bra rel", &[0x2F, 0xfe]);
        t("beq rel", &[0xF0, 0xfe]);
        t("bne rel", &[0xD0, 0xfe]);
        t("bcs rel", &[0xB0, 0xfe]);
        t("bcc rel", &[0x90, 0xfe]);
        t("bvs rel", &[0x70, 0xfe]);
        t("bvc rel", &[0x50, 0xfe]);
        t("bmi rel", &[0x30, 0xfe]);
        t("bpl rel", &[0x10, 0xfe]);
        t("dbnz Y, rel", &[0xFE, 0xfe]);

        assert_eq!(0u8.wrapping_sub(3), 0xfd);
        t("bbs $10, 0, rel", &[0x03, 0x10, 0xfd]);
        t("bbc $10, 0, rel", &[0x13, 0x10, 0xfd]);

        t("bbs $10, 3, rel", &[0x63, 0x10, 0xfd]);
        t("bbc $10, 3, rel", &[0x73, 0x10, 0xfd]);
    }

    #[test]
    fn forward_reference_pcall() {
        fn t(line: &str, value: u16, expected: &[u8]) {
            let mut s = State::new(0x200);
            assert_eq!(process_line(line, &mut s), Ok(()));

            assert_ne!(s.output(), expected, "asm: {line:?}");

            s.add_symbol("up", value.into()).unwrap();

            let mut errors = FileErrors::new();
            process_pending_output_expressions(&mut s, &mut errors);

            assert!(errors.is_empty());
            assert_eq!(s.output(), expected, "asm: {line:?}");
        }

        t("pcall $ff00", 0xff00, &[0x4f, 0x00]);
        t("pcall up", 0xff00, &[0x4f, 0x00]);
        t("pcall up", 0xff70, &[0x4f, 0x70]);
    }

    #[test]
    fn invalid_abs_bit_forward_reference() {
        let mut s = State::new(0x200);
        assert_eq!(process_line("mov1 C, abs, 0", &mut s), Ok(()));

        s.add_symbol("abs", 0x2000.into()).unwrap();

        let mut errors = FileErrors::new();
        process_pending_output_expressions(&mut s, &mut errors);

        assert_eq!(
            errors.errors(),
            &[(
                LineNo(0),
                OutputError::OutOfRange {
                    value: 0x2000,
                    min: 0,
                    max: 0x1fff
                }
                .into()
            )]
        );
    }

    #[test]
    fn invalid_pcall_forward_reference() {
        let mut s = State::new(0x200);
        assert_eq!(process_line("pcall up", &mut s), Ok(()));

        s.add_symbol("up", 0xfe00.into()).unwrap();

        let mut errors = FileErrors::new();
        process_pending_output_expressions(&mut s, &mut errors);

        assert_eq!(
            errors.errors(),
            &[(LineNo(0), OutputError::InvalidPcall(0xfe00).into())]
        );
    }
}
