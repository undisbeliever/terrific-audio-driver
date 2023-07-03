//! Newtype generation macros

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::ValueError;

pub trait ValueNewType
where
    Self: TryFrom<<Self as ValueNewType>::ConvertFrom, Error = ValueError>,
    <Self as ValueNewType>::ConvertFrom: std::str::FromStr,
    <Self as ValueNewType>::ConvertFrom: VntStrParser,
{
    type ConvertFrom;
    const MISSING_ERROR: ValueError;

    // I have no idea how to turn this into a `FromStr` trait implementation.
    fn try_from_str(s: &str) -> Result<Self, ValueError> {
        let v = <Self::ConvertFrom as VntStrParser>::parse_str(s)?;
        <Self as TryFrom<Self::ConvertFrom>>::try_from(v)
    }
}

/// Used to convert a `&str` to `ValueNewType::ConvertFrom`
pub trait VntStrParser: Sized {
    fn parse_str(s: &str) -> Result<Self, ValueError>;
}

impl VntStrParser for u32 {
    fn parse_str(s: &str) -> Result<Self, ValueError> {
        match s.parse::<u32>() {
            Ok(v) => Ok(v),
            Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
        }
    }
}

impl VntStrParser for i32 {
    fn parse_str(s: &str) -> Result<Self, ValueError> {
        match s.parse::<i32>() {
            Ok(v) => Ok(v),
            Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
        }
    }
}

macro_rules! u8_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u8);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: u8 = u8::MIN;
            pub const MAX: u8 = u8::MAX;

            pub const fn new(value: u8) -> Self {
                Self(value)
            }
            pub fn as_u8(&self) -> u8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ConvertFrom = u32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                match value.try_into() {
                    Ok(v) => Ok($name(v)),
                    Err(_) => Err(ValueError::$error),
                }
            }
        }
    };
    ($name:ident, $error:ident, $missing_error:ident, $min: expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u8);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: u8 = $min;
            pub const MAX: u8 = $max;

            pub fn as_u8(&self) -> u8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ConvertFrom = u32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;
        }

        impl TryFrom<u8> for $name {
            type Error = ValueError;

            #[allow(clippy::manual_range_contains)]
            fn try_from(value: u8) -> Result<Self, Self::Error> {
                if value >= Self::MIN && value <= Self::MAX {
                    Ok(Self(value))
                } else {
                    Err(ValueError::$error)
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.into() && value <= Self::MAX.into() {
                    Ok(Self(u8::try_from(value).unwrap()))
                } else {
                    Err(ValueError::$error)
                }
            }
        }
    };
}

macro_rules! i8_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(i8);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: i8 = i8::MIN;
            pub const MAX: i8 = i8::MAX;

            pub const fn new(p: i8) -> Self {
                Self(p)
            }
            pub fn as_i8(&self) -> i8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ConvertFrom = i32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;
        }

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(p: i32) -> Result<Self, Self::Error> {
                match p.try_into() {
                    Ok(p) => Ok($name(p)),
                    Err(_) => Err(ValueError::$error),
                }
            }
        }
    };
}

pub(crate) use {i8_value_newtype, u8_value_newtype};
