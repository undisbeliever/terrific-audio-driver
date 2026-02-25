//! Spc700 instructions and addressing modes

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::evaluator::{evaluate, ExpressionError, ExpressionResult};

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
enum U8Value {
    Known(u8),
    Unknown(String),
}

#[derive(Debug, PartialEq)]
enum U16Value {
    Known(u16),
    Unknown(String),
}

#[derive(Debug, PartialEq)]
enum AddressingMode {
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

    DpIndirectX(u8),
    DpIndirectY(u8),

    Abs(U16Value),
    AbsX(U16Value),
    AbsY(U16Value),

    NotAbs(U16Value),

    Immediate(U8Value),
}

impl AddressingMode {
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
            Self::DpIndirectX(_) => "[dp+X]",
            Self::DpIndirectY(_) => "[dp]+Y",
            Self::Abs(_) => "abs",
            Self::AbsX(_) => "abs+X",
            Self::AbsY(_) => "abs+Y",
            Self::NotAbs(_) => "/abs",
            Self::Immediate(_) => "#imm",
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum DirectPageFlag {
    #[default]
    Zero,
    One,
}

// ::TODO proper symbol management::
#[derive(Default)]
struct Dummy {
    direct_page: DirectPageFlag,
}

#[allow(unused_variables)] // ::TODO remove::
fn parse_immediate_value<'a>(
    s: &'a str,
    symbols: &Dummy,
) -> Result<U8Value, AddressingModeError<'a>> {
    match evaluate(s) {
        ExpressionResult::Value(value) => match value.try_into() {
            Ok(v) => Ok(U8Value::Known(v)),
            Err(_) => Err(AddressingModeError::DpAddressOutOfBounds(s, value)),
        },
        ExpressionResult::Unknown => Ok(U8Value::Unknown(s.to_owned())),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

#[allow(unused_variables)] // ::TODO remove::
fn parse_abs_address<'a>(s: &'a str, symbols: &Dummy) -> Result<U16Value, AddressingModeError<'a>> {
    match evaluate(s) {
        ExpressionResult::Value(value) => match value.try_into() {
            Ok(v) => Ok(U16Value::Known(v)),
            Err(_) => Err(AddressingModeError::AbsoluteAddressOutOfBounds(s, value)),
        },
        ExpressionResult::Unknown => Ok(U16Value::Unknown(s.to_owned())),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

#[derive(Debug, PartialEq)]
enum DpOrAbs {
    Dp(u8),
    Abs(U16Value),
}

#[allow(unused_variables)] // ::TODO remove::
fn parse_dp_or_abs_address<'a>(
    s: &'a str,
    symbols: &Dummy,
) -> Result<DpOrAbs, AddressingModeError<'a>> {
    match evaluate(s) {
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
        ExpressionResult::Unknown => Ok(DpOrAbs::Abs(U16Value::Unknown(s.to_string()))),
        ExpressionResult::Boolean(_) => Err(AddressingModeError::NotANumber(s)),
        ExpressionResult::Error(e) => Err(AddressingModeError::ExpressionError(s, e)),
    }
}

#[allow(unused_variables)] // ::TODO remove::
fn parse_dp_address<'a>(s: &'a str, symbols: &Dummy) -> Result<u8, AddressingModeError<'a>> {
    match evaluate(s) {
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

#[allow(dead_code)] // ::TODO remove::
fn parse_addressing_mode<'a>(
    input: &'a str,
    symbols: &Dummy,
) -> Result<AddressingMode, AddressingModeError<'a>> {
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
            }
            else if let Some(s) = s.strip_prefix("/") {
                Ok(AddressingMode::NotAbs(parse_abs_address(s, symbols)?))
            } else if let Some(s) = strip_plus_index_suffix(s, "X") {
                match parse_dp_or_abs_address(s, symbols)? {
                    DpOrAbs::Dp(a) => Ok(AddressingMode::DpX(a)),
                    DpOrAbs::Abs(a) => Ok(AddressingMode::AbsX(a)),
                }
            } else if let Some(s) = strip_plus_index_suffix(s, "Y") {
                Ok(AddressingMode::AbsY(parse_abs_address(s, symbols)?))
            } else {
                match parse_dp_or_abs_address(s, symbols)? {
                    DpOrAbs::Dp(a) => Ok(AddressingMode::Dp(a)),
                    DpOrAbs::Abs(a) => Ok(AddressingMode::Abs(a)),
                }
            }
        }
    }
}

#[cfg(test)]
mod addressing_mode_tests {
    use super::{
        parse_addressing_mode, AddressingMode, AddressingModeError, DirectPageFlag, Dummy,
        U16Value, U8Value,
    };

    macro_rules! test_ok {
        ($s:literal, $v:expr) => {
            assert_eq!(parse_addressing_mode($s, &Dummy::default()), Ok($v))
        };
    }
    macro_rules! test_err {
        ($s:literal, Err($v:expr)) => {
            assert_eq!(parse_addressing_mode($s, &Dummy::default()), Err($v))
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
            AddressingMode::Immediate(U8Value::Unknown("unknown + 123 / 2".to_string()))
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

        test_ok!(
            "unknown",
            AddressingMode::Abs(U16Value::Unknown("unknown".to_string()))
        );
        test_ok!(
            "unknown + 2",
            AddressingMode::Abs(U16Value::Unknown("unknown + 2".to_string()))
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
            AddressingMode::AbsX(U16Value::Unknown("unknown".to_string()))
        );
        test_ok!(
            "unknown + 2+X",
            AddressingMode::AbsX(U16Value::Unknown("unknown + 2".to_string()))
        );
        test_ok!(
            "unknown + 3 + X",
            AddressingMode::AbsX(U16Value::Unknown("unknown + 3".to_string()))
        );
    }

    #[test]
    fn y_indexed() {
        test_ok!("128+Y", AddressingMode::AbsY(U16Value::Known(128)));
        test_ok!("$ff+Y", AddressingMode::AbsY(U16Value::Known(0xff)));
        test_ok!("$100 + Y", AddressingMode::AbsY(U16Value::Known(0x100)));
        test_ok!("$ffff +Y", AddressingMode::AbsY(U16Value::Known(0xffff)));

        test_ok!(
            "25 + 35 + 12 + Y",
            AddressingMode::AbsY(U16Value::Known(25 + 35 + 12))
        );
        test_ok!(
            "100 + 100 + 100 + Y",
            AddressingMode::AbsY(U16Value::Known(300))
        );

        test_ok!(
            "unknown+Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown".to_string()))
        );
        test_ok!(
            "unknown + 2+Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown + 2".to_string()))
        );
        test_ok!(
            "unknown + 3 + Y",
            AddressingMode::AbsY(U16Value::Unknown("unknown + 3".to_string()))
        );
    }

    #[test]
    fn direct_page_set() {
        macro_rules! test {
            ($s:literal, $v:expr) => {
                assert_eq!(
                    parse_addressing_mode(
                        $s,
                        &Dummy {
                            direct_page: DirectPageFlag::One
                        }
                    ),
                    $v
                )
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
}
