//! Newtype generation macros

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::ValueError;

pub trait UnsignedValueNewType
where
    Self: TryFrom<u32, Error = ValueError>,
{
    /// May not be the internal value of the ValueNewType
    type ValueType;

    const MISSING_ERROR: ValueError;

    fn value(&self) -> Self::ValueType;
}

pub trait SignedValueNewType
where
    Self: TryFrom<i32, Error = ValueError>,
{
    /// May not be the internal value of the ValueNewType
    type ValueType;

    const MISSING_ERROR: ValueError;
    const MISSING_SIGN_ERROR: ValueError;

    fn value(&self) -> Self::ValueType;
}

pub trait I8WithByteHexValueNewType
where
    Self: TryFrom<i32, Error = ValueError> + TryFrom<u32, Error = ValueError>,
{
    const ZERO: Self;

    const MISSING_ERROR: ValueError;

    fn value(&self) -> i8;

    fn try_from_hex_byte(hex: u32) -> Result<Self, ValueError>;
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

        impl crate::value_newtypes::UnsignedValueNewType for $name {
            type ValueType = u8;

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
                    Err(_) => Err(ValueError::$error(value)),
                }
            }
        }
    };
    ($name:ident, $error:ident, $missing_error:ident, $min:expr, $max:expr) => {
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

        impl crate::value_newtypes::UnsignedValueNewType for $name {
            type ValueType = u8;

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
                    Err(ValueError::$error(u32::from(value)))
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
                    Ok(Self(u8::try_from(value).unwrap()))
                } else {
                    Err(ValueError::$error(value))
                }
            }
        }
    };
}

macro_rules! u16_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident, $min:expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u16);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub const fn as_u16(&self) -> u16 {
                self.0
            }
        }

        impl crate::value_newtypes::UnsignedValueNewType for $name {
            type ValueType = u16;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<u16> for $name {
            type Error = ValueError;

            #[allow(clippy::manual_range_contains)]
            fn try_from(value: u16) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0 && value <= Self::MAX.0 {
                    Ok(Self(value))
                } else {
                    Err(ValueError::$error(u32::from(value)))
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
                    Ok(Self(u16::try_from(value).unwrap()))
                } else {
                    Err(ValueError::$error(value))
                }
            }
        }
    };
}

macro_rules! u32_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident, $min:expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u32);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub const fn as_u32(&self) -> u32 {
                self.0
            }
        }

        impl crate::value_newtypes::UnsignedValueNewType for $name {
            type ValueType = u32;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                if (Self::MIN.0..=Self::MAX.0).contains(&value) {
                    Ok(Self(value))
                } else {
                    Err(ValueError::$error(value))
                }
            }
        }
    };
}

macro_rules! u8_0_is_256_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u8);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self(1);
            pub const MAX: Self = Self(0);

            pub const fn driver_value(&self) -> u8 {
                self.0
            }
        }

        impl crate::value_newtypes::UnsignedValueNewType for $name {
            type ValueType = u32;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                match self.0 {
                    0 => 256,
                    v => v.into(),
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                match value {
                    1..=255 => Ok(Self(value.try_into().unwrap())),
                    256 => Ok(Self(0)),
                    _ => Err(ValueError::$error(value)),
                }
            }
        }
    };
}

macro_rules! i8_value_newtype {
    ($name:ident, $error:ident, $missing_error:ident, $missing_sign_error:ident) => {
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

        impl crate::value_newtypes::SignedValueNewType for $name {
            type ValueType = i8;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;
            const MISSING_SIGN_ERROR: ValueError = ValueError::$missing_sign_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                match value.try_into() {
                    Ok(v) => Ok($name(v)),
                    Err(_) => Err(ValueError::$error(value)),
                }
            }
        }
    };

    ($name:ident, $error:ident, $missing_error:ident, $missing_sign_error:ident, $min:expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(i8);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub const fn as_i8(&self) -> i8 {
                self.0
            }
        }

        impl crate::value_newtypes::SignedValueNewType for $name {
            type ValueType = i8;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;
            const MISSING_SIGN_ERROR: ValueError = ValueError::$missing_sign_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<i8> for $name {
            type Error = ValueError;

            #[allow(clippy::manual_range_contains)]
            fn try_from(value: i8) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0 && value <= Self::MAX.0 {
                    Ok(Self(value))
                } else {
                    Err(ValueError::$error(i32::from(value)))
                }
            }
        }

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
                    Ok(Self(i8::try_from(value).unwrap()))
                } else {
                    Err(ValueError::$error(value))
                }
            }
        }
    };
}

macro_rules! i8_with_hex_byte_value_newtype {
    ($name:ident, $error:ident, $error_u32:ident, $error_hex:ident, $missing_error:ident) => {
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

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                match value.try_into() {
                    Ok(v) => Ok($name(v)),
                    Err(_) => Err(ValueError::$error(value)),
                }
            }
        }

        impl TryFrom<u32> for $name {
            type Error = ValueError;

            fn try_from(value: u32) -> Result<Self, Self::Error> {
                match value.try_into() {
                    Ok(v) => Ok($name(v)),
                    Err(_) => Err(ValueError::$error_u32(value)),
                }
            }
        }

        impl crate::value_newtypes::I8WithByteHexValueNewType for $name {
            const ZERO: Self = Self(0);

            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> i8 {
                self.0
            }

            fn try_from_hex_byte(hex: u32) -> Result<Self, ValueError> {
                match u8::try_from(hex) {
                    Ok(i) => Ok($name(i8::from_le_bytes([i]))),
                    Err(_) => Err(ValueError::$error_hex(hex)),
                }
            }
        }
    };
}

macro_rules! i16_value_newtype {
    ($name:ident, $range_error:ident, $missing_error:ident, $missing_sign_error:ident, $min:expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(i16);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub fn as_i16(&self) -> i16 {
                self.0
            }

            pub fn is_negative(&self) -> bool {
                self.0 < 0
            }
        }

        impl SignedValueNewType for $name {
            type ValueType = i16;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;
            const MISSING_SIGN_ERROR: ValueError = ValueError::$missing_sign_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
                    Ok($name(value.try_into().unwrap()))
                } else {
                    Err(ValueError::$range_error(value))
                }
            }
        }
    };
}

macro_rules! i16_non_zero_value_newtype {
    ($name:ident, $range_error:ident, $missing_error:ident, $missing_sign_error:ident, $zero_error:ident, $min:expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(i16);

        #[allow(dead_code)]
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            pub fn as_i16(&self) -> i16 {
                self.0
            }

            pub fn is_negative(&self) -> bool {
                self.0 < 0
            }
        }

        impl SignedValueNewType for $name {
            type ValueType = i16;

            const MISSING_ERROR: ValueError = ValueError::$missing_error;
            const MISSING_SIGN_ERROR: ValueError = ValueError::$missing_sign_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl TryFrom<i32> for $name {
            type Error = ValueError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                if value == 0 {
                    Err(ValueError::$zero_error)
                } else if value >= Self::MIN.0.into() && value <= Self::MAX.0.into() {
                    Ok($name(value.try_into().unwrap()))
                } else {
                    Err(ValueError::$range_error(value))
                }
            }
        }
    };
}

pub(crate) use {
    i16_non_zero_value_newtype, i16_value_newtype, i8_value_newtype,
    i8_with_hex_byte_value_newtype, u16_value_newtype, u32_value_newtype,
    u8_0_is_256_value_newtype, u8_value_newtype,
};

pub fn parse_i8wh<T: I8WithByteHexValueNewType>(s: &str) -> Result<T, ValueError> {
    match s.bytes().next() {
        None => Err(T::MISSING_ERROR),
        Some(c) => match c {
            b'0'..=b'9' | b'-' | b'+' => match s.parse::<i32>() {
                Ok(i) => i.try_into(),
                Err(_) => Err(ValueError::CannotParseSigned(s.to_owned())),
            },
            b'$' => match u32::from_str_radix(&s[1..], 16) {
                Ok(h) => T::try_from_hex_byte(h),
                Err(_) => Err(ValueError::CannotParseHex(s.to_owned())),
            },
            _ => Err(ValueError::CannotParseSigned(s.to_owned())),
        },
    }
}
