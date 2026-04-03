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
//   <value>                     ::= <decimal> | "$"<hexadecimal> | "%"<binary>
//                                 | "true" | "false"
//                                 | <symbol>
//                                 | <unary_value_function> | <offsetof>
//                                 | "(" <expression> ")"
//
//  <unary_value_function>       ::= <function_name> "(" <expression> ")"
//  <offsetof>                   ::= "offsetof" "(" <struct_name> "," <field_name> ")"
// ```
//
// Resources:
//   * LET'S BUILD A COMPILER! by Jack W. Crenshaw, Ph.D.
//   * Crafting Interpreters by Robert Nystrom
//   * [Rust Expression precedence](https://doc.rust-lang.org/reference/expressions.html#r-expr.precedence)

use bitflags::bitflags;

use crate::{
    output::{U16Value, U8Value},
    symbols::{is_identifier_character, Symbols},
};

bitflags! {
    #[derive(Debug, PartialEq)]
    pub struct ExpressionError: u32 {
        const SyntaxError = 1 << 0;
        const UnmatchedParenthesis = 1 << 1;
        const InvalidDecimalLiteral = 1 << 2;
        const InvalidHexadecimalLiteral = 1 << 3;
        const InvalidBinaryLiteral= 1 << 4;
        const NoParenthesisAfterFunction = 1 << 5;
        const CannotFindOffsetofField = 1 << 6;

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

impl std::fmt::Display for ExpressionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn bit_to_str(b: ExpressionError) -> &'static str {
            match b {
                ExpressionError::SyntaxError => "syntax error",
                ExpressionError::UnmatchedParenthesis => "unmatched parenthesis",
                ExpressionError::InvalidDecimalLiteral => "invalid decimal literal",
                ExpressionError::InvalidHexadecimalLiteral => "invalid hexadecimal literal",
                ExpressionError::InvalidBinaryLiteral => "invalid binary literal",
                ExpressionError::NoParenthesisAfterFunction => "no ( after function name",
                ExpressionError::CannotFindOffsetofField => "cannot find offsetof() field",
                ExpressionError::Overflow => "overflow",
                ExpressionError::DivisionByZero => "division by zero",
                ExpressionError::BooleanInIntegerOperation => "boolean in integer operation",
                ExpressionError::IntegerInBooleanOperation => "integer in boolean operation",
                ExpressionError::BooleanInRelativeComparison => "boolean in relative comparison",
                ExpressionError::BooleanAndIntegerComparison => "boolean/integer comparison",
                ExpressionError::InvalidShift => "invalid shift",
                ExpressionError::BooleanNegation => "cannot negate a boolean",
                _ => "UNKNOWN",
            }
        }

        let mut it = self.iter();
        if let Some(b) = it.next() {
            write!(f, "{}", bit_to_str(b))?;
            for b in it {
                write!(f, ", {}", bit_to_str(b))?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionResult {
    Value(i64),
    Boolean(bool),
    Unknown,
    Error(ExpressionError),
}

struct Matcher<'a, 's> {
    s: &'s str,
    symbols: &'a Symbols<'s>,
}

impl<'a, 's> Matcher<'a, 's> {
    fn new(s: &'s str, symbols: &'a Symbols<'s>) -> Self {
        Self {
            s: s.trim_start(),
            symbols,
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
    fn take_symbol_name(&mut self) -> &'s str {
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
pub enum ConstexprError<'s> {
    UnknownValue(&'s str),
    InvalidAddress(&'s str, ExpressionError),
    AddressOutOfRange(&'s str),
    AddressNotANumber(&'s str),
    InvalidU16(&'s str, ExpressionError),
    U16OutOfRange(&'s str),
    U16NotANumber(&'s str),
    InvalidU8(&'s str, ExpressionError),
    U8OutOfRange(&'s str),
    U8NotANumber(&'s str),
}

impl std::fmt::Display for ConstexprError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstexprError::UnknownValue(e) => write!(f, "unknown value: {e}"),
            ConstexprError::InvalidAddress(expr, e) => write!(f, "invalid address: {expr}: {e}"),
            ConstexprError::AddressOutOfRange(expr) => write!(f, "address out of range: {expr}"),
            ConstexprError::AddressNotANumber(expr) => write!(f, "address is not a number: {expr}"),
            ConstexprError::InvalidU16(expr, e) => write!(f, "invalid u16: {expr}: {e}"),
            ConstexprError::U16OutOfRange(expr) => write!(f, "u16 out of range: {expr}"),
            ConstexprError::U16NotANumber(expr) => write!(f, "u16 not a number: {expr}"),
            ConstexprError::InvalidU8(expr, e) => write!(f, "invalid u8: {expr}: {e}"),
            ConstexprError::U8OutOfRange(expr) => write!(f, "u8 out of range: {expr}"),
            ConstexprError::U8NotANumber(expr) => write!(f, "u8 not a number: {expr}"),
        }
    }
}

pub fn evaluate_constexpr_address<'s>(
    expr: &'s str,
    symbols: &Symbols<'s>,
) -> Result<u16, ConstexprError<'s>> {
    match evaluate(expr, symbols) {
        ExpressionResult::Value(v) => v
            .try_into()
            .map_err(|_| ConstexprError::AddressOutOfRange(expr)),
        ExpressionResult::Boolean(_) => Err(ConstexprError::AddressNotANumber(expr)),
        ExpressionResult::Unknown => Err(ConstexprError::UnknownValue(expr)),
        ExpressionResult::Error(e) => Err(ConstexprError::InvalidAddress(expr, e)),
    }
}

pub fn evaluate_constexpr_u16<'s>(
    expr: &'s str,
    symbols: &Symbols<'s>,
) -> Result<u16, ConstexprError<'s>> {
    match evaluate(expr, symbols) {
        ExpressionResult::Value(v) => v
            .try_into()
            .map_err(|_| ConstexprError::U16OutOfRange(expr)),
        ExpressionResult::Boolean(_) => Err(ConstexprError::U16NotANumber(expr)),
        ExpressionResult::Unknown => Err(ConstexprError::UnknownValue(expr)),
        ExpressionResult::Error(e) => Err(ConstexprError::InvalidU16(expr, e)),
    }
}

pub fn evaluate_constexpr_u8<'s>(
    expr: &'s str,
    symbols: &Symbols<'s>,
) -> Result<u8, ConstexprError<'s>> {
    match evaluate(expr, symbols) {
        ExpressionResult::Value(v) => v.try_into().map_err(|_| ConstexprError::U8OutOfRange(expr)),
        ExpressionResult::Boolean(_) => Err(ConstexprError::U8NotANumber(expr)),
        ExpressionResult::Unknown => Err(ConstexprError::UnknownValue(expr)),
        ExpressionResult::Error(e) => Err(ConstexprError::InvalidU8(expr, e)),
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueError<'s> {
    U8OutOfRange(&'s str, i64),
    U8NotANumber(&'s str),
    U16OutOfRange(&'s str, i64),
    U16NotANumber(&'s str),
    Error(&'s str, ExpressionError),
}

impl std::fmt::Display for ValueError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::U8OutOfRange(expr, _) => write!(f, "u8 out of range: {expr}"),
            ValueError::U8NotANumber(expr) => write!(f, "u8 not a number: {expr}"),
            ValueError::U16OutOfRange(expr, _) => write!(f, "u16 out of range: {expr}"),
            ValueError::U16NotANumber(expr) => write!(f, "u16 not a number: {expr}"),
            ValueError::Error(expr, e) => write!(f, "expression error: {expr}: {e}"),
        }
    }
}

pub fn evaluate_u8v<'s>(
    expr: &'s str,
    symbols: &Symbols<'s>,
) -> Result<U8Value<'s>, ValueError<'s>> {
    match evaluate(expr, symbols) {
        ExpressionResult::Value(v) => match v.try_into() {
            Ok(v) => Ok(U8Value::Known(v)),
            Err(_) => Err(ValueError::U8OutOfRange(expr, v)),
        },
        ExpressionResult::Unknown => Ok(U8Value::Unknown(expr, symbols.scope())),
        ExpressionResult::Boolean(_) => Err(ValueError::U8NotANumber(expr)),
        ExpressionResult::Error(e) => Err(ValueError::Error(expr, e)),
    }
}

pub fn evaluate_u16v<'s>(
    expr: &'s str,
    symbols: &Symbols<'s>,
) -> Result<U16Value<'s>, ValueError<'s>> {
    match evaluate(expr, symbols) {
        ExpressionResult::Value(v) => match v.try_into() {
            Ok(v) => Ok(U16Value::Known(v)),
            Err(_) => Err(ValueError::U16OutOfRange(expr, v)),
        },
        ExpressionResult::Unknown => Ok(U16Value::Unknown(expr, symbols.scope())),
        ExpressionResult::Boolean(_) => Err(ValueError::U16NotANumber(expr)),
        ExpressionResult::Error(e) => Err(ValueError::Error(expr, e)),
    }
}

pub fn evaluate(s: &str, symbols: &Symbols) -> ExpressionResult {
    let mut m = Matcher::new(s, symbols);

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

#[cfg(test)]
pub const EVALUATOR_FUNCTIONS: [&'static str; 5] =
    ["lobyte", "hibyte", "page", "inZeropage", "inFirstpage"];

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

            "offsetof" => offsetof(m),

            "lobyte" => {
                unary_value_function(m, |v| ExpressionResult::Value(v.to_le_bytes()[0].into()))
            }
            "hibyte" => {
                unary_value_function(m, |v| ExpressionResult::Value(v.to_le_bytes()[1].into()))
            }
            "page" => unary_value_function(m, |v| ExpressionResult::Value(v >> 8)),
            "inZeropage" => unary_value_function(m, |v| {
                ExpressionResult::Boolean((0x000..0x100).contains(&v))
            }),
            "inFirstpage" => unary_value_function(m, |v| {
                ExpressionResult::Boolean((0x100..0x200).contains(&v))
            }),
            // `EXPRESSION_FUNCTIONS` list MUST BE updated when adding a new function
            //
            sym => match m.symbols.get_symbol(sym) {
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

fn offsetof(m: &mut Matcher) -> ExpressionResult {
    if !m.matches("(") {
        return ExpressionResult::Error(ExpressionError::NoParenthesisAfterFunction);
    }
    let struct_name = m.take_symbol_name();
    if !m.matches(",") {
        return ExpressionResult::Error(ExpressionError::SyntaxError);
    }
    let field_name = m.take_symbol_name();
    if !m.matches(")") {
        return ExpressionResult::Error(ExpressionError::UnmatchedParenthesis);
    }

    match m.symbols.get_struct_offset(struct_name, field_name) {
        Some(v) => ExpressionResult::Value(v),
        None => ExpressionResult::Error(ExpressionError::CannotFindOffsetofField),
    }
}

fn unary_value_function(m: &mut Matcher, f: impl Fn(i64) -> ExpressionResult) -> ExpressionResult {
    match m.matches("(") {
        true => (),
        false => return ExpressionResult::Error(ExpressionError::NoParenthesisAfterFunction),
    }

    let r = match expression(m) {
        ExpressionResult::Value(v) => f(v),
        ExpressionResult::Boolean(_) => {
            ExpressionResult::Error(ExpressionError::BooleanInIntegerOperation)
        }
        r @ ExpressionResult::Unknown | r @ ExpressionResult::Error(_) => r,
    };

    match m.matches(")") {
        true => r,
        false => ExpressionResult::Error(ExpressionError::UnmatchedParenthesis),
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
    use crate::{evaluator::EVALUATOR_FUNCTIONS, symbols::Symbols};

    use super::{evaluate, ExpressionError, ExpressionResult};

    macro_rules! test_integer_expression {
        ($expr:expr) => {
            let s = stringify![$expr];
            assert_eq!(
                evaluate(s, &Symbols::new()),
                ExpressionResult::Value($expr),
                "Expression mismatch {s}"
            );
        };
        ($expr:literal, $value:expr) => {
            let s: &'static str = $expr;
            assert_eq!(
                evaluate(s, &Symbols::new()),
                ExpressionResult::Value($value),
                "Expression mismatch \"{s}\""
            );
        };
    }

    macro_rules! test_boolean_expression {
        ($expr:expr) => {
            let s = stringify![$expr];
            assert_eq!(
                evaluate(s, &Symbols::new()),
                ExpressionResult::Boolean($expr),
                "Expression mismatch {s}"
            );
        };
        ($expr:literal, $value:expr) => {
            let s: &'static str = $expr;
            assert_eq!(
                evaluate(s, &Symbols::new()),
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
        let symbols = Symbols::new();

        assert_eq!(
            evaluate("00009223372036854775807", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9223372036854775807", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9223372036854775808", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("9_223_372_036_854_775_807", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("9_223_372_036_854_775_808", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("-9223372036854775807", &symbols),
            ExpressionResult::Value(-i64::MAX)
        );
        assert_eq!(
            evaluate("-9223372036854775808", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("10000000000000000000", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );
        assert_eq!(
            evaluate("-10000000000000000000", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidDecimalLiteral)
        );

        assert_eq!(
            evaluate("$7fffffffffffffff", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$8000000000000000", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );
        assert_eq!(
            evaluate("$ffffffffffffffff", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );

        assert_eq!(
            evaluate("$0000_7fffffff_ffffffff", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$7fffffff_ffffffff", &symbols),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate("$80000000_00000000", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );
        assert_eq!(
            evaluate("$ffffffff_ffffffff_", &symbols),
            ExpressionResult::Error(ExpressionError::InvalidHexadecimalLiteral)
        );

        assert_eq!(
            evaluate(
                "%00000000111111111111111111111111111111111111111111111111111111111111111",
                &symbols
            ),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate(
                "%111111111111111111111111111111111111111111111111111111111111111",
                &symbols
            ),
            ExpressionResult::Value(i64::MAX)
        );
        assert_eq!(
            evaluate(
                "%1000000000000000000000000000000000000000000000000000000000000000",
                &symbols
            ),
            ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral)
        );
        assert_eq!(
            evaluate(
                "%11111111111111111111111111111111111111111111111111111111111111111",
                &symbols
            ),
            ExpressionResult::Error(ExpressionError::InvalidBinaryLiteral)
        );
    }

    #[test]
    fn unknown_value() {
        let symbols = Symbols::new();

        assert_eq!(
            evaluate("(unknown + 1) * 2 / 3", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("(0 + 1) * 2 / u", &symbols),
            ExpressionResult::Unknown
        );

        assert_eq!(
            evaluate("unknown && false", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("true || unknown", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("true ^ unknown", &symbols),
            ExpressionResult::Unknown
        );

        assert_eq!(
            evaluate("unknown == unknown", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("1 == unknown", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("unknown != 2", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("true == unknown", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("unknown == false", &symbols),
            ExpressionResult::Unknown
        );
    }

    #[test]
    fn known_symbols() {
        let symbols = {
            let mut s = Symbols::new();
            s.add_symbol("one", 1).unwrap();
            s.add_symbol("two", 2).unwrap();
            s.add_symbol("three", 3).unwrap();
            s.add_symbol("minus_seven", -7).unwrap();
            s
        };

        assert_eq!(evaluate("one", &symbols), ExpressionResult::Value(1));
        assert_eq!(evaluate("two", &symbols), ExpressionResult::Value(2));
        assert_eq!(
            evaluate("minus_seven", &symbols),
            ExpressionResult::Value(-7)
        );

        assert_eq!(
            evaluate("one + two * minus_seven / three", &symbols),
            ExpressionResult::Value(1 + 2 * -7 / 3)
        );

        assert_eq!(
            evaluate("(unknown + one) * two / three", &symbols),
            ExpressionResult::Unknown
        );
        assert_eq!(
            evaluate("(5 + one) * two / three", &symbols),
            ExpressionResult::Value((5 + 1) * 2 / 3)
        );

        assert_eq!(
            evaluate("1 == one", &symbols),
            ExpressionResult::Boolean(true)
        );
        assert_eq!(
            evaluate("two == 2", &symbols),
            ExpressionResult::Boolean(true)
        );
        assert_eq!(
            evaluate("1 == three", &symbols),
            ExpressionResult::Boolean(false)
        );
        assert_eq!(
            evaluate("three < 2", &symbols),
            ExpressionResult::Boolean(false)
        );
    }

    #[test]
    fn scoped_lookup() {
        let symbols = {
            let mut s = Symbols::new();
            s.add_unchecked_scoped_symbol("scope", "const", 2).unwrap();
            s.add_unchecked_scoped_symbol("scope.const", "const", 9999)
                .unwrap();
            s.add_symbol("const", 9999).unwrap();
            s.open_scope("scope", vec![]);
            s
        };

        assert_eq!(
            evaluate("(scope.const + const) * 10", &symbols),
            ExpressionResult::Value((2 + 2) * 10)
        );

        assert_eq!(symbols.get_symbol("scope.const.const"), Some(9999));
    }

    #[test]
    fn characters_after_expression_is_syntax_error() {
        let symbols = {
            let mut s = Symbols::new();
            s.add_symbol("const", 10).unwrap();
            s
        };

        assert_eq!(
            evaluate("1 2", &symbols),
            ExpressionResult::Error(ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("unknown + 2 unknown", &symbols),
            ExpressionResult::Error(ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("( 10000000000 * 200000000000 garbage)", &symbols),
            ExpressionResult::Error(
                ExpressionError::UnmatchedParenthesis | ExpressionError::SyntaxError
            )
        );

        assert_eq!(
            evaluate("10000000000 * 200000000000 garbage", &symbols),
            ExpressionResult::Error(ExpressionError::Overflow | ExpressionError::SyntaxError)
        );

        assert_eq!(
            evaluate("$uuu unknown", &symbols),
            ExpressionResult::Error(
                ExpressionError::InvalidHexadecimalLiteral | ExpressionError::SyntaxError
            )
        );
    }

    #[test]
    fn lobyte() {
        test_integer_expression!("lobyte($1234)", 0x34);
        test_integer_expression!("lobyte($1200)", 0x00);

        test_integer_expression!("lobyte(-1)", 0xff);
        test_integer_expression!("lobyte(-128)", 0x80);

        test_integer_expression!("lobyte(70 + 80 * 90)", (70 + 80 * 90) & 0xff);
        test_integer_expression!("lobyte((70 + 80) * 90)", ((70 + 80) * 90) & 0xff);
    }

    #[test]
    fn hibyte() {
        test_integer_expression!("hibyte($1234)", 0x12);
        test_integer_expression!("hibyte($1200)", 0x12);

        test_integer_expression!("hibyte(-1)", 0xff);
        test_integer_expression!("hibyte(-128)", 0xff);
        test_integer_expression!("hibyte(-32768)", 0x80);

        test_integer_expression!("hibyte($aabbcc)", 0xbb);
        test_integer_expression!("hibyte($aabbccdd)", 0xcc);

        test_integer_expression!("hibyte(99 + 8 * 700)", ((99 + 8 * 700) >> 8) & 0xff);
        test_integer_expression!("hibyte((99 + 8) * 700)", (((99 + 8) * 700) >> 8) & 0xff);
    }

    #[test]
    fn page() {
        test_integer_expression!("page($1234)", 0x12);
        test_integer_expression!("page($1200)", 0x12);

        test_integer_expression!("page($aabbcc)", 0xaabb);
        test_integer_expression!("page($aabbccdd)", 0xaabbcc);

        test_integer_expression!("page(70 + 80 * 90)", (70 + 80 * 90) >> 8);
        test_integer_expression!("page((70 + 80) * 90)", ((70 + 80) * 90) >> 8);
    }

    #[test]
    fn in_zeropage() {
        test_boolean_expression!("inZeropage(-1)", false);
        test_boolean_expression!("inZeropage(0)", true);
        test_boolean_expression!("inZeropage(255)", true);
        test_boolean_expression!("inZeropage(256)", false);

        test_boolean_expression!("inZeropage(1 + 2 + 3 + 4)", true);

        test_boolean_expression!("inZeropage($1234)", false);
        test_boolean_expression!("inZeropage($1200)", false);
        test_boolean_expression!("inZeropage(4 * 200)", false);

        test_boolean_expression!("inZeropage($aabbcc)", false);
        test_boolean_expression!("inZeropage($aabbccdd)", false);

        test_boolean_expression!("inZeropage($1200 + 100)", false);

        test_boolean_expression!("inZeropage(11 + 12 * 13)", true);
        test_boolean_expression!("inZeropage((11 + 12) * 13)", false);
    }

    #[test]
    fn in_firstpage() {
        test_boolean_expression!("inFirstpage(-1)", false);
        test_boolean_expression!("inFirstpage(0)", false);
        test_boolean_expression!("inFirstpage($ff)", false);
        test_boolean_expression!("inFirstpage($100)", true);
        test_boolean_expression!("inFirstpage($1ff)", true);
        test_boolean_expression!("inFirstpage($200)", false);

        test_boolean_expression!("inFirstpage(300 + 20 + 1)", true);
        test_boolean_expression!("inFirstpage(1 + 2 + 3 + 4)", false);

        test_boolean_expression!("inFirstpage($1234)", false);
        test_boolean_expression!("inFirstpage($1200)", false);
        test_boolean_expression!("inFirstpage(4 * 200)", false);

        test_boolean_expression!("inFirstpage($aabbcc)", false);
        test_boolean_expression!("inFirstpage($aabbccdd)", false);

        test_boolean_expression!("inFirstpage(11 + 12 * 13)", false);
        test_boolean_expression!("inFirstpage((11 + 12) * 13)", true);
    }

    #[test]
    fn function_err() {
        let symbols = Symbols::new();

        for f in EVALUATOR_FUNCTIONS {
            assert_eq!(
                evaluate(&format!("{f} 2"), &symbols),
                ExpressionResult::Error(
                    ExpressionError::SyntaxError | ExpressionError::NoParenthesisAfterFunction
                )
            );

            assert_eq!(
                evaluate(&format!("{f}(2"), &symbols),
                ExpressionResult::Error(ExpressionError::UnmatchedParenthesis)
            );

            assert_eq!(
                evaluate(&format!("{f}(true)"), &symbols),
                ExpressionResult::Error(ExpressionError::BooleanInIntegerOperation)
            );
        }
    }
}
