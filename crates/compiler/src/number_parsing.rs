//! Number parsing functions

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::ValueError;
use crate::value_newtypes::{SignedValueNewType, UnsignedValueNewType};

pub fn parse_u32(s: &str) -> Result<u32, ValueError> {
    match s.bytes().next() {
        Some(b'$') => match u32::from_str_radix(&s[1..], 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(s.to_owned())),
        },
        _ => match s.parse() {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
        },
    }
}

fn parse_i32(src: &str, missing_sign_err: &ValueError) -> Result<i32, ValueError> {
    if let Some(s) = src.strip_prefix("+$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else if let Some(s) = src.strip_prefix("-$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(-i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else {
        match src.bytes().next() {
            Some(b'-') | Some(b'+') => match src.parse() {
                Ok(i) => Ok(i),
                Err(_) => Err(ValueError::CannotParseSigned(src.to_owned())),
            },
            _ => Err(missing_sign_err.clone()),
        }
    }
}

fn parse_i32_allow_zero(src: &str, missing_sign_err: &ValueError) -> Result<i32, ValueError> {
    if let Some(s) = src.strip_prefix("+$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else if let Some(s) = src.strip_prefix("-$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(-i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else {
        match src.bytes().next() {
            Some(b'-') | Some(b'+') => match src.parse() {
                Ok(i) => Ok(i),
                Err(_) => Err(ValueError::CannotParseSigned(src.to_owned())),
            },
            _ => match src.parse::<i32>() {
                Ok(0) => Ok(0),
                _ => Err(missing_sign_err.clone()),
            },
        }
    }
}

/// Parse UnsignedValueNewType
pub fn parse_uvnt<T>(s: &str) -> Result<T, ValueError>
where
    T: UnsignedValueNewType,
{
    parse_u32(s)?.try_into()
}

/// Parse SignedValueNewType
pub fn parse_svnt<T>(s: &str) -> Result<T, ValueError>
where
    T: SignedValueNewType,
{
    parse_i32(s, &T::MISSING_SIGN_ERROR)?.try_into()
}

/// Parse SignedValueNewType
pub fn parse_svnt_allow_zero<T>(s: &str) -> Result<T, ValueError>
where
    T: SignedValueNewType,
{
    parse_i32_allow_zero(s, &T::MISSING_SIGN_ERROR)?.try_into()
}
