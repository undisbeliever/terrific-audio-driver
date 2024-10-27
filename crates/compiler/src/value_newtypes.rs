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
    /// May not be the internal value of the ValueNewType
    type ValueType;
    type ConvertFrom;

    const MISSING_ERROR: ValueError;

    // I have no idea how to turn this into a `FromStr` trait implementation.
    fn try_from_str(s: &str) -> Result<Self, ValueError> {
        let v = <Self::ConvertFrom as VntStrParser>::parse_str(s)?;
        <Self as TryFrom<Self::ConvertFrom>>::try_from(v)
    }

    fn value(&self) -> Self::ValueType;
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
            pub const MIN: Self = Self(u8::MIN);
            pub const MAX: Self = Self(u8::MAX);

            pub const fn new(value: u8) -> Self {
                Self(value)
            }
            pub const fn as_u8(&self) -> u8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ValueType = u8;
            type ConvertFrom = u32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
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
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub const fn as_u8(&self) -> u8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ValueType = u8;
            type ConvertFrom = u32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<u8> for $name {
            type Error = ValueError;

            #[allow(clippy::manual_range_contains)]
            fn try_from(value: u8) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0 && value <= Self::MAX.0 {
                    Ok(Self(value))
                } else {
                    Err(ValueError::$error)
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
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
            pub const MIN: Self = Self(i8::MIN);
            pub const MAX: Self = Self(i8::MAX);

            pub const fn new(p: i8) -> Self {
                Self(p)
            }
            pub const fn as_i8(&self) -> i8 {
                self.0
            }
        }

        impl crate::value_newtypes::ValueNewType for $name {
            type ValueType = i8;
            type ConvertFrom = i32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
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
