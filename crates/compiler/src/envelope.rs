//! Envelope data structures

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::errors::{InvalidAdsrError, InvalidGainError, ParseError};

use serde::Deserialize;

fn value_fits_in_bits(value: u8, bits: u8) -> bool {
    assert!(bits < 8);

    let mask = (1 << bits) - 1;

    value & mask == value
}

#[derive(Deserialize, Clone, Debug)]
#[serde(try_from = "String")]
pub struct Adsr {
    adsr1: u8,
    adsr2: u8,
}

impl Adsr {
    fn new(
        attack: u8,
        decay: u8,
        sustain_level: u8,
        sustain_rate: u8,
    ) -> Result<Adsr, InvalidAdsrError> {
        let valid_a = value_fits_in_bits(attack, 4);
        let valid_d = value_fits_in_bits(decay, 3);
        let valid_sl = value_fits_in_bits(sustain_level, 3);
        let valid_sr = value_fits_in_bits(sustain_rate, 5);

        if valid_a && valid_d && valid_sl && valid_sr {
            let adsr1 = (1 << 7) | (decay << 4) | (attack);
            let adsr2 = (sustain_level << 5) | (sustain_rate);

            Ok(Adsr { adsr1, adsr2 })
        } else {
            Err(InvalidAdsrError {
                valid_a,
                valid_d,
                valid_sl,
                valid_sr,
            })
        }
    }

    pub fn from_strs(
        attack: &str,
        decay: &str,
        sustain_level: &str,
        sustain_rate: &str,
    ) -> Result<Adsr, InvalidAdsrError> {
        // All fields in Adsr require fewer then 8 bits of space.
        // Converting u8 parsing errors to u8::MAX will always output an error for that value.
        Adsr::new(
            attack.parse().unwrap_or(u8::MAX),
            decay.parse().unwrap_or(u8::MAX),
            sustain_level.parse().unwrap_or(u8::MAX),
            sustain_rate.parse().unwrap_or(u8::MAX),
        )
    }

    pub fn from_str(s: &str) -> Result<Adsr, ParseError> {
        let mut iter = s.split_ascii_whitespace();

        let a = iter.next().ok_or(ParseError::AdsrNotFourValues)?;
        let d = iter.next().ok_or(ParseError::AdsrNotFourValues)?;
        let sl = iter.next().ok_or(ParseError::AdsrNotFourValues)?;
        let sr = iter.next().ok_or(ParseError::AdsrNotFourValues)?;

        if iter.next().is_some() {
            // Too many values
            return Err(ParseError::AdsrNotFourValues);
        }

        Ok(Adsr::from_strs(a, d, sl, sr)?)
    }

    pub fn adsr1(&self) -> u8 {
        self.adsr1
    }

    pub fn adsr2(&self) -> u8 {
        self.adsr2
    }
}

impl TryFrom<String> for Adsr {
    type Error = ParseError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Adsr::from_str(&s)
    }
}

#[derive(Deserialize, Clone, Debug)]
#[serde(from = "u8")]
pub struct Gain {
    value: u8,
}

impl Gain {
    pub fn new(value: u8) -> Self {
        Self { value }
    }

    pub fn from_str(s: &str) -> Result<Gain, InvalidGainError> {
        // ::TODO figure out what the gain bits do and properly parse them::

        let value = match s.parse() {
            Ok(i) => i,
            Err(_) => return Err(InvalidGainError::InvalidGain(s.to_owned())),
        };

        Ok(Gain { value })
    }

    pub fn value(&self) -> u8 {
        self.value
    }
}

impl From<u8> for Gain {
    fn from(i: u8) -> Self {
        Gain::new(i)
    }
}
