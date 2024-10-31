//! Newtype generation macros

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::ValueError;

pub trait ValueNewType
where
    Self: TryFrom<<Self as ValueNewType>::ConvertFrom, Error = ValueError>,
{
    /// May not be the internal value of the ValueNewType
    type ValueType;
    type ConvertFrom;

    const MISSING_ERROR: ValueError;

    fn value(&self) -> Self::ValueType;
}

pub trait SignedValueNewType
where
    Self: ValueNewType<ConvertFrom = i32>,
{
    const MISSING_SIGN_ERROR: ValueError;
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

        impl crate::value_newtypes::ValueNewType for $name {
            type ValueType = i8;
            type ConvertFrom = i32;
            const MISSING_ERROR: ValueError = ValueError::$missing_error;

            fn value(&self) -> Self::ValueType {
                self.0
            }
        }

        impl crate::value_newtypes::SignedValueNewType for $name {
            const MISSING_SIGN_ERROR: ValueError = ValueError::$missing_sign_error;
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
