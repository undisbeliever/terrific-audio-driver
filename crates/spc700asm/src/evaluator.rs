//! Expression evaluator

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// BNF syntax:
// ```
//   <expression>                ::= <logical-or-expression>
//
//   <logical-or-expression>     ::= <logical-and-expression> [ "||" <logical-and-expression> ]*
//   <logical-and-expression>    ::= <comparison> [ "&&" <comparison> ]*
//   <comparison>                ::= <bit-or-expression> [ ( "==" | "!=" | "<" | "<=" | ">=" | ">) <bit-or-expression> ]*
//
//   <bit-or-expression>         ::= <xor-expression> [ "|" <xor-expression> ]*
//   <xor-expression>            ::= <bit-and-expression> [ "^" <bit-and-expression> ]*
//   <bit-and-expression>        ::= <bit-shift-expression> [ "&" <bit-shift-expression> ]*
//   <bit-shift-expression>      ::= <additive-expression> [ ("<<" | ">>") <additive-expression> ]*
//
//   <additive-expression>       ::= <divmul-expression> [ ("+" | "-") <divmul-expression> ]*
//   <divmul-expression>         ::= <unary> [ ("*" | "/" | "%") <unary> ]*
//   <unary>                     ::= ["+" | "-"] <value>
//
//   <value>                     ::= <decimal> | "$"<hexadecimal> | "%"<binary> | "true" | "false" | <symbol> | "(" <expression> ")"
// ```
//
// Resources:
//   * LET'S BUILD A COMPILER! by Jack W. Crenshaw, Ph.D.
//   * Crafting Interpreters by Robert Nystrom
//   * [Rust Expression precedence](https://doc.rust-lang.org/reference/expressions.html#r-expr.precedence)

use bitflags::bitflags;

use crate::state::{is_identifier_character, State, U16Value, U8Value};

bitflags! {
    #[derive(Debug, PartialEq)]
    pub struct ExpressionError: u32 {
        const SyntaxError = 1 << 0;
        const UnmatchedParenthesis = 1 << 1;
        const InvalidDecimalLiteral = 1 << 2;
        const InvalidHexadecimalLiteral = 1 << 3;
        const InvalidBinaryLiteral= 1 << 4;

        const Overflow = 1 << 10;
        const DivisionByZero = 1 << 11;

        const BooleanInIntegerOperation = 1 << 20;
        const IntegerInBooleanOperation = 1 << 21;
        const BooleanInRelativeComparison = 1 << 22;
        const BooleanAndIntegerComparison = 1 << 23;
        const InvalidShift = 1 << 24;
        const BooleanNegation = 1 << 25;
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionResult {
    Value(i64),
    Boolean(bool),
    Unknown,
    Error(ExpressionError),
}

struct Matcher<'a, 'b> {
    s: &'a str,
    state: &'b State<'b>,
}

impl<'a> Matcher<'a, '_> {
    fn new<'b>(s: &'a str, state: &'b State) -> Matcher<'a, 'b> {
        Matcher {
            s: s.trim_start(),
            state,
        }
    }

    #[must_use]
    fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    #[must_use]
    fn next_byte(&self) -> u8 {
        self.s.bytes().next().unwrap_or(0)
    }

    fn consume(&mut self, p: &'static str) {
        self.s = self
            .s
            .strip_prefix(p)
            .expect("matcher does not start with {p}")
            .trim_start();
    }

    #[must_use]
    fn starts_with(&self, p: &'static str) -> bool {
        self.s.starts_with(p)
    }

    #[must_use]
    fn matches(&mut self, p: &'static str) -> bool {
        match self.s.strip_prefix(p) {
            Some(s) => {
                self.s = s.trim_start();
                true
            }
            None => false,
        }
    }

    #[must_use]
    fn matches_multiple(&mut self, p: &'static [&'static str]) -> Option<&'static str> {
        p.iter().find_map(|&p| match self.matches(p) {
            true => Some(p),
            false => None,
        })
    }

    #[must_use]
    fn take_symbol_name(&mut self) -> &'a str {
        match self.s.bytes().position(|c| !is_identifier_character(c)) {
            Some(i) => {
                let (p, s) = self.s.split_at(i);
                self.s = s.trim_start();
                p
            }
            None => {
                let r = self.s;
                self.s = "";
                r
            }
        }
    }
}

fn integer_operation(
    a: ExpressionResult,
    b: ExpressionResult,
    f: impl Fn(i64, i64) -> Result<i64, ExpressionError>,
) -> ExpressionResult {
    match (a, b) {
        (ExpressionResult::Value(a), ExpressionResult::Value(b)) => match f(a, b) {
            Ok(v) => ExpressionResult::Value(v),
            Err(e) => ExpressionResult::Error(e),
        },
        (ExpressionResult::Error(e1), ExpressionResult::Error(e2)) => {
            ExpressionResult::Error(e1 | e2)
        }
        (ExpressionResult::Error(e), _) | (_, ExpressionResult::Error(e)) => {
            ExpressionResult::Error(e)
        }
        (ExpressionResult::Unknown, _) | (_, ExpressionResult::Unknown) => {
            ExpressionResult::Unknown
        }
        (ExpressionResult::Boolean(_), _) | (_, ExpressionResult::Boolean(_)) => {
            ExpressionResult::Error(ExpressionError::BooleanInIntegerOperation)
        }
    }
}

fn boolean_operation(
    a: ExpressionResult,
    b: ExpressionResult,
    f: impl Fn(bool, bool) -> bool,
) -> ExpressionResult {
    match (a, b) {
        (ExpressionResult::Boolean(a), ExpressionResult::Boolean(b)) => {
            ExpressionResult::Boolean(f(a, b))
        }
        (ExpressionResult::Error(e1), ExpressionResult::Error(e2)) => {
            ExpressionResult::Error(e1 | e2)
        }
        (ExpressionResult::Error(e), _) | (_, ExpressionResult::Error(e)) => {
            ExpressionResult::Error(e)
        }
        (ExpressionResult::Unknown, _) | (_, ExpressionResult::Unknown) => {
            ExpressionResult::Unknown
        }
        (ExpressionResult::Value(_), _) | (_, ExpressionResult::Value(_)) => {
            ExpressionResult::Error(ExpressionError::IntegerInBooleanOperation)
        }
    }
}

fn integer_comparison_operation(
    a: ExpressionResult,
    b: ExpressionResult,
    fv: impl Fn(i64, i64) -> bool,
) -> ExpressionResult {
    match (a, b) {
        (ExpressionResult::Value(a), ExpressionResult::Value(b)) => {
            ExpressionResult::Boolean(fv(a, b))
        }
        (ExpressionResult::Error(e1), ExpressionResult::Error(e2)) => {
            ExpressionResult::Error(e1 | e2)
        }
        (ExpressionResult::Error(e), _) | (_, ExpressionResult::Error(e)) => {
            ExpressionResult::Error(e)
        }
        (ExpressionResult::Unknown, _) | (_, ExpressionResult::Unknown) => {
            ExpressionResult::Unknown
        }
        (ExpressionResult::Boolean(_), _) | (_, ExpressionResult::Boolean(_)) => {
            ExpressionResult::Error(ExpressionError::BooleanInRelativeComparison)
        }
    }
}

fn comparison_operation(
    a: ExpressionResult,
    b: ExpressionResult,
    fv: impl Fn(i64, i64) -> bool,
    fb: impl Fn(bool, bool) -> bool,
) -> ExpressionResult {
    match (a, b) {
        (ExpressionResult::Value(a), ExpressionResult::Value(b)) => {
            ExpressionResult::Boolean(fv(a, b))
        }
        (ExpressionResult::Boolean(a), ExpressionResult::Boolean(b)) => {
            ExpressionResult::Boolean(fb(a, b))
        }
        (ExpressionResult::Error(e1), ExpressionResult::Error(e2)) => {
            ExpressionResult::Error(e1 | e2)
        }
        (ExpressionResult::Error(e), _) | (_, ExpressionResult::Error(e)) => {
            ExpressionResult::Error(e)
        }
        (ExpressionResult::Unknown, _) | (_, ExpressionResult::Unknown) => {
            ExpressionResult::Unknown
        }
        (ExpressionResult::Value(_), ExpressionResult::Boolean(_))
        | (ExpressionResult::Boolean(_), ExpressionResult::Value(_)) => {
            ExpressionResult::Error(ExpressionError::BooleanAndIntegerComparison)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ConstexprError<'a> {
    UnknownValue(&'a str),
    InvalidAddress(&'a str, ExpressionError),
    AddressOutOfRange(&'a str),
    AddressNotANumber(&'a str),
    InvalidU16(&'a str, ExpressionError),
    U16OutOfRange(&'a str),
    U16NotANumber(&'a str),
}

pub fn evaluate_constexpr_address<'a>(
    expr: &'a str,
    state: &State,
) -> Result<u16, ConstexprError<'a>> {
    match evaluate(expr, state) {
        ExpressionResult::Value(v) => v
            .try_into()
            .map_err(|_| ConstexprError::AddressOutOfRange(expr)),
        ExpressionResult::Boolean(_) => Err(ConstexprError::AddressNotANumber(expr)),
        ExpressionResult::Unknown => Err(ConstexprError::UnknownValue(expr)),
        ExpressionResult::Error(e) => Err(ConstexprError::InvalidAddress(expr, e)),
    }
}

pub fn evaluate_constexpr_u16<'a>(expr: &'a str, state: &State) -> Result<u16, ConstexprError<'a>> {
    match evaluate(expr, state) {
        ExpressionResult::Value(v) => v
            .try_into()
            .map_err(|_| ConstexprError::U16OutOfRange(expr)),
        ExpressionResult::Boolean(_) => Err(ConstexprError::U16NotANumber(expr)),
        ExpressionResult::Unknown => Err(ConstexprError::UnknownValue(expr)),
        ExpressionResult::Error(e) => Err(ConstexprError::InvalidU16(expr, e)),
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueError<'a> {
    U8OutOfRange(&'a str, i64),
    U8NotANumber(&'a str),
    U16OutOfRange(&'a str, i64),
    U16NotANumber(&'a str),
    Error(&'a str, ExpressionError),
}

pub fn evaluate_u8v<'s>(expr: &'s str, state: &State<'s>) -> Result<U8Value<'s>, ValueError<'s>> {
    match evaluate(expr, state) {
        ExpressionResult::Value(v) => match v.try_into() {
            Ok(v) => Ok(U8Value::Known(v)),
            Err(_) => Err(ValueError::U8OutOfRange(expr, v)),
        },
        ExpressionResult::Unknown => Ok(U8Value::Unknown(expr)),
        ExpressionResult::Boolean(_) => Err(ValueError::U8NotANumber(expr)),
        ExpressionResult::Error(e) => Err(ValueError::Error(expr, e)),
    }
}

pub fn evaluate_u16v<'s>(expr: &'s str, state: &State) -> Result<U16Value<'s>, ValueError<'s>> {
    match evaluate(expr, state) {
        ExpressionResult::Value(v) => match v.try_into() {
            Ok(v) => Ok(U16Value::Known(v)),
            Err(_) => Err(ValueError::U16OutOfRange(expr, v)),
        },
        ExpressionResult::Unknown => Ok(U16Value::Unknown(expr)),
        ExpressionResult::Boolean(_) => Err(ValueError::U16NotANumber(expr)),
        ExpressionResult::Error(e) => Err(ValueError::Error(expr, e)),
    }
}

pub fn evaluate(s: &str, state: &State) -> ExpressionResult {
    let mut m = Matcher::new(s, state);

    match (expression(&mut m), m.is_empty()) {
        (r, true) => r,

        (ExpressionResult::Value(_), false)
        | (ExpressionResult::Boolean(_), false)
        | (ExpressionResult::Unknown, false) => {
            ExpressionResult::Error(ExpressionError::SyntaxError)
        }

        (ExpressionResult::Error(e), false) => {
            ExpressionResult::Error(e | ExpressionError::SyntaxError)
        }
    }
}

fn expression(m: &mut Matcher) -> ExpressionResult {
    logical_or_expression(m)
}

fn logical_or_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = logical_and_expression(m);

    while m.matches("||") {
        let r = logical_and_expression(m);
        l = boolean_operation(l, r, |a, b| a || b);
    }

    l
}

fn logical_and_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = comparison(m);

    while m.matches("&&") {
        let r = comparison(m);
        l = boolean_operation(l, r, |a, b| a && b);
    }

    l
}

fn comparison(m: &mut Matcher) -> ExpressionResult {
    let mut l = bit_or_expression(m);

    while let Some(c) = m.matches_multiple(&["==", "!=", "<=", ">=", "<", ">"]) {
        let r = bit_or_expression(m);

        l = match c {
            "==" => comparison_operation(l, r, |a, b| a == b, |a, b| a == b),
            "!=" => comparison_operation(l, r, |a, b| a != b, |a, b| a != b),
            "<" => integer_comparison_operation(l, r, |a, b| a < b),
            "<=" => integer_comparison_operation(l, r, |a, b| a <= b),
            ">=" => integer_comparison_operation(l, r, |a, b| a >= b),
            ">" => integer_comparison_operation(l, r, |a, b| a > b),
            _ => unreachable!(),
        };
    }

    l
}

fn bit_or_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = xor_expression(m);

    while !m.starts_with("||") && m.matches("|") {
        let r = xor_expression(m);
        l = integer_operation(l, r, |a, b| Ok(a | b));
    }

    l
}

fn xor_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = bit_and_expression(m);

    while m.matches("^") {
        let r = bit_and_expression(m);
        l = integer_operation(l, r, |a, b| Ok(a ^ b));
    }

    l
}

fn bit_and_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = bit_shift_expression(m);

    while !m.starts_with("&&") && m.matches("&") {
        let r = bit_shift_expression(m);
        l = integer_operation(l, r, |a, b| Ok(a & b));
    }

    l
}

fn bit_shift_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = additive_expression(m);

    loop {
        if m.matches("<<") {
            let r = additive_expression(m);
            l = integer_operation(l, r, |a, b| match u32::try_from(b) {
                Ok(b) => a.checked_shl(b).ok_or(ExpressionError::Overflow),
                Err(_) => Err(ExpressionError::InvalidShift),
            });
        } else if m.matches(">>") {
            let r = additive_expression(m);
            l = integer_operation(l, r, |a, b| match u32::try_from(b) {
                Ok(b) => a.checked_shr(b).ok_or(ExpressionError::Overflow),
                Err(_) => Err(ExpressionError::InvalidShift),
            });
        } else {
            break;
        }
    }

    l
}

fn additive_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = divmul_expression(m);

    loop {
        if m.matches("+") {
            let r = divmul_expression(m);
            l = integer_operation(l, r, |a, b| {
                a.checked_add(b).ok_or(ExpressionError::Overflow)
            });
        } else if m.matches("-") {
            let r = divmul_expression(m);
            l = integer_operation(l, r, |a, b| {
                a.checked_sub(b).ok_or(ExpressionError::Overflow)
            });
        } else {
            break;
        }
    }

    l
}

fn divmul_expression(m: &mut Matcher) -> ExpressionResult {
    let mut l = unary(m);

    loop {
        if m.matches("*") {
            let r = unary(m);
            l = integer_operation(l, r, |a, b| {
                a.checked_mul(b).ok_or(ExpressionError::Overflow)
            });
        } else if m.matches("/") {
            let r = unary(m);
            l = integer_operation(l, r, |a, b| {
                a.checked_div(b).ok_or(ExpressionError::DivisionByZero)
            });
        } else if m.matches("%") {
            let r = unary(m);
            l = integer_operation(l, r, |a, b| {
                a.checked_rem(b).ok_or(ExpressionError::DivisionByZero)
            });
        } else {
            break;
        }
    }

    l
}

fn unary(m: &mut Matcher) -> ExpressionResult {
    if m.matches("-") {
        match value(m) {
            ExpressionResult::Value(v) => match v.checked_neg() {
                Some(v) => ExpressionResult::Value(v),
                None => ExpressionResult::Error(ExpressionError::Overflow),
            },
            ExpressionResult::Boolean(_) => {
                ExpressionResult::Error(ExpressionError::BooleanNegation)
            }
            r @ ExpressionResult::Unknown | r @ ExpressionResult::Error(_) => r,
        }
    } else {
        // Ignore `+`
        let _ = m.matches("+");

        value(m)
    }
}

fn value(m: &mut Matcher) -> ExpressionResult {
    match m.next_byte() {
        b'0'..=b'9' => parse_decimal(m.take_symbol_name()),
        b'$' => {
            m.consume("$");
            parse_hexadecimal(m.take_symbol_name())
        }
        b'%' => {
            m.consume("%");
            parse_binary(m.take_symbol_name())
        }

        b'a'..=b'z' | b'A'..=b'Z' | b'_' => match m.take_symbol_name() {
            "true" => ExpressionResult::Boolean(true),
            "false" => ExpressionResult::Boolean(false),

            sym => match m.state.get_symbol(sym) {
                Some(v) => ExpressionResult::Value(v),
                None => ExpressionResult::Unknown,
            },
        },

        b'(' => {
            m.consume("(");
            let v = expression(m);
            match m.matches(")") {
                true => v,
                false => ExpressionResult::Error(ExpressionError::UnmatchedParenthesis),
            }
        }

        b')' => ExpressionResult::Error(ExpressionError::UnmatchedParenthesis),

        _ => ExpressionResult::Error(ExpressionError::SyntaxError),
    }
}

fn parse_decimal(s: &str) -> ExpressionResult {
    let mut value: i64 = 0;

    for c in s.bytes() {
        match c {
            b'0'..=b'9' => match value
                .checked_mul(10)
                .and_then(|v| v.checked_add(i64::from(c - b'0')))
            {
                Some(v) => value = v,
                None => return ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral),
            },
            b'_' => (),
            _ => return ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral),
        }
    }

    ExpressionResult::Value(value)
}

fn parse_hexadecimal(s: &str) -> ExpressionResult {
    let mut value: i64 = 0;

    for c in s.bytes() {
        match c {
            b'_' => (),
            _ => {
                let d = match c {
                    b'0'..=b'9' => i64::from(c - b'0'),
                    b'a'..=b'f' => i64::from(c - b'a' + 10),
                    b'A'..=b'F' => i64::from(c - b'A' + 10),
                    _ => {
                        return ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
                    }
                };
                match value.checked_mul(16) {
                    Some(v) => value = v | d,
                    None => {
                        return ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
                    }
                }
            }
        }
    }

    ExpressionResult::Value(value)
}

fn parse_binary(s: &str) -> ExpressionResult {
    let mut value: i64 = 0;

    for c in s.bytes() {
        match c {
            b'_' => (),
            b'0' | b'1' => match value.checked_mul(2) {
                Some(v) => value = v | i64::from(c == b'1'),
                None => return ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral),
            },
            _ => return ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral),
        }
    }

    ExpressionResult::Value(value)
}

#[cfg(test)]
mod tests {
    use crate::state::State;

    use super::{evaluate, ExpressionError, ExpressionResult};

    macro_rules! test_integer_expression {
        ($expr:expr) => {
            let s = stringify![$expr];
            assert_eq!(
                evaluate(s, &State::new(0x200)),
                ExpressionResult::Value($expr),
                "Expression mismatch {s}"
            );
        };
        ($expr:literal, $value:expr) => {
            let s: &'static str = $expr;
            assert_eq!(
                evaluate(s, &State::new(0x200)),
                ExpressionResult::Value($value),
                "Expression mismatch \"{s}\""
            );
        };
    }

    macro_rules! test_boolean_expression {
        ($expr:expr) => {
            let s = stringify![$expr];
            assert_eq!(
                evaluate(s, &State::new(0x200)),
                ExpressionResult::Boolean($expr),
                "Expression mismatch {s}"
            );
        };
        ($expr:literal, $value:expr) => {
            let s: &'static str = $expr;
            assert_eq!(
                evaluate(s, &State::new(0x200)),
                ExpressionResult::Boolean($value),
                "Expression mismatch {s}"
            );
        };
    }

    #[test]
    fn decimal_numbers() {
        test_integer_expression!("0", 0);
        test_integer_expression!("1", 1);
        test_integer_expression!("1234567890123456789", 1234567890123456789);
        test_integer_expression!("100_000", 100_000);
    }

    #[test]
    fn binary_numbers() {
        test_integer_expression!("%0", 0b0);
        test_integer_expression!("%1", 0b1);
        test_integer_expression!("%01101111", 0b01101111);
        test_integer_expression!("%11_10_01_00", 0b11_10_01_00);
    }

    #[test]
    fn hexadecimal_numbers() {
        test_integer_expression!("$00", 0x00);
        test_integer_expression!("$ff", 0xff);
        test_integer_expression!("$FF", 0xFF);

        test_integer_expression!("$1234_5678_9abc_def0", 0x1234_5678_9abc_def0);
    }

    #[test]
    fn boolean_literals() {
        test_boolean_expression!("false", false);
        test_boolean_expression!("true", true);
    }

    #[test]
    fn basic_arithmatic() {
        test_integer_expression!(1 + 1);
        test_integer_expression!(10 - 20);
        test_integer_expression!(10 * 20);
        test_integer_expression!(1234 / 100);
        test_integer_expression!(1234 % 100);
    }

    #[test]
    fn bodmas() {
        test_integer_expression!(123 + 456 * 7890 - 987 + 654 / 32);
        test_integer_expression!((123 + 456) * 7890 - (987 + 654) / 32);
        test_integer_expression!((123 + 456) * (7890 - 987 + 654 / 32));
        test_integer_expression!(123 + (456 * 7890 - 987) + 654 / 32);
    }

    #[test]
    fn negate() {
        test_integer_expression!(-123);
        test_integer_expression!("+123", 123);

        test_integer_expression!(-123 + 456 * -7890 - 987 + 654 / 32);
        test_integer_expression!(123 + 456 * -7890 - 987 + 654 / 32);
        test_integer_expression!((123 + 456) * 7890 - (-987 + 654) / 32);
        test_integer_expression!((123 + 456) * (7890 - 987 + -654 / 32));
        test_integer_expression!(123 + -(456 * 7890 - 987) + 654 / -32);
    }

    #[test]
    fn bit_shift() {
        test_integer_expression!(123456789 << 10);
        test_integer_expression!(123456789 >> 10);

        test_integer_expression!(123456789 << 10 << 3);
        test_integer_expression!(123456789 >> 10 >> 4);

        test_integer_expression!(123456789 << 10 >> 3);
        test_integer_expression!(123456789 >> 10 << 4);

        test_integer_expression!(123456789 << (10 + 1));
        test_integer_expression!(123456789 << (10 - 1));

        test_integer_expression!(-1000 << 4);
        test_integer_expression!(-1000 >> 4);
        test_integer_expression!(-(1000 << 4));
        test_integer_expression!(-(1000 >> 4));

        test_integer_expression!((123456789 & 3) << (10 - 1));
        test_integer_expression!((512 + 928) << (10 - 1));

        test_integer_expression!(123456789 & 3 << 10 - 1);
        test_integer_expression!(512 + 928 << 10 - 1);
    }

    #[test]
    fn bit_expressions() {
        test_integer_expression!(1234 | 5678);
        test_integer_expression!(1234 ^ 5678);
        test_integer_expression!(1234 & 5678);

        test_integer_expression!(1 | 2 | 64 | 128);
        test_integer_expression!(65535 & 65527 & 64511);
        test_integer_expression!(65535 ^ 65527 ^ 512);

        test_integer_expression!(1234 | 5678 ^ 3567 & 1213546);
        test_integer_expression!(1234 ^ 546546 & 654654 | 879879);
        test_integer_expression!(1234 & 5678 | 1546 ^ 245);
    }

    #[test]
    fn comparisons() {
        test_boolean_expression!(123456789 == 987654321);
        test_boolean_expression!(123456789 != 987654321);
        test_boolean_expression!(123456789 < 987654321);
        test_boolean_expression!(123456789 <= 987654321);
        test_boolean_expression!(123456789 >= 987654321);
        test_boolean_expression!(123456789 > 987654321);
    }

    #[test]
    fn logical_operations() {
        test_boolean_expression!(false && false);
        test_boolean_expression!(false && true);
        test_boolean_expression!(true && false);
        test_boolean_expression!(true && true);

        test_boolean_expression!(false || false);
        test_boolean_expression!(false || true);
        test_boolean_expression!(true || false);
        test_boolean_expression!(true || true);

        test_boolean_expression!(true && false || true || false);
        test_boolean_expression!(true || false && true || false);
    }

    #[test]
    fn complex_comparisons() {
        test_boolean_expression!(1234 != 8 && 265 == 256);

        test_boolean_expression!(1234 != 1234 || 266 >= 256);
        test_boolean_expression!(1234 != 1234 || 267 >= 256);
        test_boolean_expression!(1230 != 1234 || 267 >= 256);

        test_boolean_expression!(1234 == 1234 && 266 >= 256);
        test_boolean_expression!(1230 == 1234 && 265 >= 256);
    }

    #[test]
    fn shifts_and_comparisons() {
        test_boolean_expression!(123456 << 10 < 7890 << 10);

        test_boolean_expression!(
            "123456789 << 10 < 1234567890 << 10",
            (123456789u64 << 10) < (1234567890u64 << 10)
        );
        test_boolean_expression!(123456789 << 10 << 2 < 256 << 30 << 4);
        test_boolean_expression!(123456789 << 10 < 1234567890 >> 10);

        test_boolean_expression!(100000000 >> 8 > 2000000 >> 3);
        test_boolean_expression!(100000000 >> 8 >> 2 > 2000000 >> 3 >> 4);
        test_boolean_expression!(100000000 >> 8 << 2 > 2000000 >> 3 << 2);
    }

    #[test]
    fn test_literal_overflow() {
        let state = State::new(0x200);

        assert_eq!(
            evaluate("00009223372036854775807", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9223372036854775807", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9223372036854775808", &state),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("9_223_372_036_854_775_807", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9_223_372_036_854_775_808", &state),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("-9223372036854775807", &state),
            ExpressionResult::Value(-i64::MAX)
        );
        assert_eq!(
            evaluate("-9223372036854775808", &state),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("10000000000000000000", &state),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );
        assert_eq!(
            evaluate("-10000000000000000000", &state),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("$7fffffffffffffff", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$8000000000000000", &state),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );
        assert_eq!(
            evaluate("$ffffffffffffffff", &state),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );

        assert_eq!(
            evaluate("$0000_7fffffff_ffffffff", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$7fffffff_ffffffff", &state),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$80000000_00000000", &state),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );
        assert_eq!(
            evaluate("$ffffffff_ffffffff_", &state),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );

        assert_eq!(
            evaluate(
                "%00000000111111111111111111111111111111111111111111111111111111111111111",
                &state
            ),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate(
                "%111111111111111111111111111111111111111111111111111111111111111",
                &state
            ),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate(
                "%1000000000000000000000000000000000000000000000000000000000000000",
                &state
            ),
            ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral)
        );
        assert_eq!(
            evaluate(
                "%11111111111111111111111111111111111111111111111111111111111111111",
                &state
            ),
            ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral)
        );
    }

    #[test]
    fn unknown_value() {
        let state = State::new(0x200);

        assert_eq!(
            evaluate("(unknown + 1) * 2 / 3", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("(0 + 1) * 2 / u", &state),
            ExpressionResult::Unknown
        );

        assert_eq!(
            evaluate("unknown && false", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("true || unknown", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("true ^ unknown", &state),
            ExpressionResult::Unknown
        );

        assert_eq!(
            evaluate("unknown == unknown", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(evaluate("1 == unknown", &state), ExpressionResult::Unknown);
        assert_eq!(evaluate("unknown != 2", &state), ExpressionResult::Unknown);
        assert_eq!(
            evaluate("true == unknown", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("unknown == false", &state),
            ExpressionResult::Unknown
        );
    }

    #[test]
    fn known_symbols() {
        let state = {
            let mut s = State::new(0x200);
            s.add_symbol("one", 1).unwrap();
            s.add_symbol("two", 2).unwrap();
            s.add_symbol("three", 3).unwrap();
            s.add_symbol("minus_seven", -7).unwrap();
            s
        };

        assert_eq!(evaluate("one", &state), ExpressionResult::Value(1));
        assert_eq!(evaluate("two", &state), ExpressionResult::Value(2));
        assert_eq!(evaluate("minus_seven", &state), ExpressionResult::Value(-7));

        assert_eq!(
            evaluate("one + two * minus_seven / three", &state),
            ExpressionResult::Value(1 + 2 * -7 / 3)
        );

        assert_eq!(
            evaluate("(unknown + one) * two / three", &state),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("(5 + one) * two / three", &state),
            ExpressionResult::Value((5 + 1) * 2 / 3)
        );

        assert_eq!(
            evaluate("1 == one", &state),
            ExpressionResult::Boolean(true)
        );
        assert_eq!(
            evaluate("two == 2", &state),
            ExpressionResult::Boolean(true)
        );
        assert_eq!(
            evaluate("1 == three", &state),
            ExpressionResult::Boolean(false)
        );
        assert_eq!(
            evaluate("three < 2", &state),
            ExpressionResult::Boolean(false)
        );
    }

    #[test]
    fn scoped_lookup() {
        let state = {
            let mut s = State::new(0x200);
            s.add_unchecked_scoped_symbol("scope", "const", 2).unwrap();
            s.add_unchecked_scoped_symbol("scope.const", "const", 9999)
                .unwrap();
            s.add_symbol("const", 9999).unwrap();
            s.open_scope("scope", vec![]);
            s
        };

        assert_eq!(
            evaluate("(scope.const + const) * 10", &state),
            ExpressionResult::Value((2 + 2) * 10)
        );

        assert_eq!(state.get_symbol("scope.const.const"), Some(9999));
    }

    #[test]
    fn characters_after_expression_is_syntax_error() {
        let state = {
            let mut s = State::new(0x200);
            s.add_symbol("const", 10).unwrap();
            s
        };

        assert_eq!(
            evaluate("1 2", &state),
            ExpressionResult::Error(ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("unknown + 2 unknown", &state),
            ExpressionResult::Error(ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("( 10000000000 * 200000000000 garbage)", &state),
            ExpressionResult::Error(
                ExpressionError::UnmatchedParenthesis | ExpressionError::SyntaxError
            )
        );

        assert_eq!(
            evaluate("10000000000 * 200000000000 garbage", &state),
            ExpressionResult::Error(ExpressionError::Overflow | ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("$uuu unknown", &state),
            ExpressionResult::Error(
                ExpressionError::InvalidHexadecimalLiteral | ExpressionError::SyntaxError
            )
        );
    }
}
