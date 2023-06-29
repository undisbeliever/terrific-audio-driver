//! Newtype generation macros

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

macro_rules! u8_newtype {
    ($name:ident, $error:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u8);

        #[allow(dead_code)]
        impl $name {
            const MIN: u8 = u8::MIN;
            const MAX: u8 = u8::MAX;

            pub fn new(value: u8) -> Self {
                Self(value)
            }
            pub fn as_u8(&self) -> u8 {
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
    ($name:ident, $error:ident, $min: expr, $max:expr) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(u8);

        #[allow(dead_code)]
        impl $name {
            const MIN: u8 = $min;
            const MAX: u8 = $max;

            pub fn as_u8(&self) -> u8 {
                self.0
            }
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

macro_rules! i8_newtype {
    ($name:ident, $error:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq)]
        pub struct $name(i8);

        #[allow(dead_code)]
        impl $name {
            const MIN: i8 = i8::MIN;
            const MAX: i8 = i8::MAX;

            pub fn new(p: i8) -> Self {
                Self(p)
            }
            pub fn as_i8(&self) -> i8 {
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

pub(crate) use {i8_newtype, u8_newtype};
