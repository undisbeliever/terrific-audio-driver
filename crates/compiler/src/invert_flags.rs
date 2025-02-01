//! Invert flags

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::ValueError;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct InvertFlags {
    pub right: bool,
    pub left: bool,
    pub mono: bool,
}

impl InvertFlags {
    pub const BOTH: Self = Self {
        right: true,
        left: true,
        mono: true,
    };

    pub fn into_driver_value(self) -> u8 {
        (u8::from(self.right) << 7) | (u8::from(self.left) << 6) | (u8::from(self.mono))
    }

    pub(crate) fn from_driver_value(e: u8) -> Self {
        Self {
            right: e & 0x80 != 0,
            left: e & 0x40 != 0,
            mono: e & 0x01 != 0,
        }
    }
}

pub fn parse_mml_invert_flags(s: &str) -> Result<InvertFlags, ValueError> {
    match s {
        "" => Err(ValueError::NoInvertFlags),
        "0" => Ok(InvertFlags::default()),
        "B" => Ok(InvertFlags::BOTH),
        s => {
            let mut out = InvertFlags::default();

            for c in s.bytes() {
                match c {
                    b'L' => {
                        if out.left {
                            return Err(ValueError::DuplicateMmlInvertFlag);
                        }
                        out.left = true;
                    }
                    b'R' => {
                        if out.right {
                            return Err(ValueError::DuplicateMmlInvertFlag);
                        }
                        out.right = true;
                    }
                    b'M' => {
                        if out.mono {
                            return Err(ValueError::DuplicateMmlInvertFlag);
                        }
                        out.mono = true;
                    }
                    _ => return Err(ValueError::InvalidMmlInvertFlags),
                }
            }
            Ok(out)
        }
    }
}

pub fn parse_invert_flag_arguments(args: &[&str]) -> Result<InvertFlags, ValueError> {
    let mut out = InvertFlags::default();

    if args.is_empty() {
        return Err(ValueError::NoInvertFlags);
    }

    if args.len() == 1 {
        let s = args[0];
        match s.bytes().next() {
            Some(b'L' | b'R' | b'M' | b'B' | b'0') => return parse_mml_invert_flags(s),
            _ => match s {
                "none" => (),
                "both" => out = InvertFlags::BOTH,
                "left" => out.left = true,
                "right" => out.right = true,
                "mono" => out.mono = true,
                s => return Err(ValueError::UnknownInvertFlagStr(s.to_owned())),
            },
        }
    } else {
        for &s in args {
            match s {
                "left" => {
                    if out.left {
                        return Err(ValueError::DuplicateInvertFlag);
                    }
                    out.left = true;
                }
                "right" => {
                    if out.right {
                        return Err(ValueError::DuplicateInvertFlag);
                    }
                    out.right = true;
                }
                "mono" => {
                    if out.mono {
                        return Err(ValueError::DuplicateInvertFlag);
                    }
                    out.mono = true;
                }
                "none" => return Err(ValueError::InvalidMultiArgInvertFlag("none")),
                "both" => return Err(ValueError::InvalidMultiArgInvertFlag("both")),
                s => return Err(ValueError::UnknownInvertFlagStr(s.to_owned())),
            }
        }
    }

    Ok(out)
}
