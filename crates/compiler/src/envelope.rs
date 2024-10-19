//! Envelope data structures

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::str::FromStr;

use crate::{
    errors::{InvalidAdsrError, ValueError},
    value_newtypes::u8_value_newtype,
};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub const ADSR_STR: &str = "adsr";
pub const GAIN_STR: &str = "gain";

const fn value_fits_in_bits(value: u8, bits: u8) -> bool {
    assert!(bits < 8);

    let mask = (1 << bits) - 1;

    value & mask == value
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Adsr {
    adsr1: u8,
    adsr2: u8,
}

impl Adsr {
    pub const ATTACK_MAX: u8 = (1 << 4) - 1;
    pub const DECAY_MAX: u8 = (1 << 3) - 1;
    pub const SUSTAIN_LEVEL_MAX: u8 = (1 << 3) - 1;
    pub const SUSTAIN_RATE_MAX: u8 = (1 << 5) - 1;

    pub const fn try_new(
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

    pub fn to_gui_string(self) -> String {
        let attack = self.adsr1 & 0b1111;
        let decay = (self.adsr1 >> 4) & 0b111;
        let sustain_level = self.adsr2 >> 5;
        let sustain_rate = self.adsr2 & 0b11111;

        format!("{} {} {} {}", attack, decay, sustain_level, sustain_rate)
    }

    pub fn to_envelope_string(self) -> String {
        let attack = self.adsr1 & 0b1111;
        let decay = (self.adsr1 >> 4) & 0b111;
        let sustain_level = self.adsr2 >> 5;
        let sustain_rate = self.adsr2 & 0b11111;

        format!(
            "{} {} {} {} {}",
            ADSR_STR, attack, decay, sustain_level, sustain_rate
        )
    }

    pub fn try_from_strs(
        attack: &str,
        decay: &str,
        sustain_level: &str,
        sustain_rate: &str,
    ) -> Result<Adsr, InvalidAdsrError> {
        // All fields in Adsr require fewer then 8 bits of space.
        // Converting parsing errors to u8::MAX will always output an error for that value.
        let parse = |s: &str| -> u8 {
            match s.as_bytes().first() {
                Some(b'$') => u8::from_str_radix(&s[1..], 16).unwrap_or(u8::MAX),
                Some(_) => s.parse().unwrap_or(u8::MAX),
                None => u8::MAX,
            }
        };

        Adsr::try_new(
            parse(attack),
            parse(decay),
            parse(sustain_level),
            parse(sustain_rate),
        )
    }

    pub fn adsr1(&self) -> u8 {
        self.adsr1
    }

    pub fn adsr2(&self) -> u8 {
        self.adsr2
    }
}

impl FromStr for Adsr {
    type Err = ValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split_ascii_whitespace();

        let a = iter.next().ok_or(ValueError::AdsrNotFourValues)?;
        let d = iter.next().ok_or(ValueError::AdsrNotFourValues)?;
        let sl = iter.next().ok_or(ValueError::AdsrNotFourValues)?;
        let sr = iter.next().ok_or(ValueError::AdsrNotFourValues)?;

        if iter.next().is_some() {
            // Too many values
            return Err(ValueError::AdsrNotFourValues);
        }

        match Adsr::try_from_strs(a, d, sl, sr) {
            Ok(adsr) => Ok(adsr),
            Err(e) => Err(ValueError::InvalidAdsr(e)),
        }
    }
}

impl TryFrom<[u32; 4]> for Adsr {
    type Error = ValueError;

    fn try_from(value: [u32; 4]) -> Result<Self, Self::Error> {
        // All fields in Adsr require fewer then 8 bits of space.
        // Converting u8 try_into errors to u8::MAX will always output an error for that value.
        Ok(Adsr::try_new(
            value[0].try_into().unwrap_or(u8::MAX),
            value[1].try_into().unwrap_or(u8::MAX),
            value[2].try_into().unwrap_or(u8::MAX),
            value[3].try_into().unwrap_or(u8::MAX),
        )?)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GainMode {
    Raw,
    Fixed,
    LinearDecrease,
    ExponentialDecrease,
    LinearIncrease,
    BentIncrease,
}

impl GainMode {
    pub fn from_u8_char(c: u8) -> Option<GainMode> {
        match c {
            b'F' => Some(GainMode::Fixed),
            b'D' => Some(GainMode::LinearDecrease),
            b'E' => Some(GainMode::ExponentialDecrease),
            b'I' => Some(GainMode::LinearIncrease),
            b'B' => Some(GainMode::BentIncrease),
            _ => None,
        }
    }

    pub fn to_prefix_str(self) -> &'static str {
        match self {
            Self::Raw => "",
            Self::Fixed => "F",
            Self::LinearDecrease => "D",
            Self::ExponentialDecrease => "E",
            Self::LinearIncrease => "I",
            Self::BentIncrease => "B",
        }
    }

    pub fn max_value(self) -> u8 {
        match self {
            Self::Raw => u8::MAX,
            Self::Fixed => Gain::FIXED_GAIN_MASK,
            Self::LinearDecrease
            | Self::ExponentialDecrease
            | Self::LinearIncrease
            | Self::BentIncrease => Gain::RATE_MASK,
        }
    }
}

u8_value_newtype!(Gain, GainOutOfRange, NoGain);

impl Gain {
    pub const FIXED_GAIN_MASK: u8 = 127;
    pub const RATE_MASK: u8 = 0b000_11111;

    const LINEAR_DECREASE_PREFIX: u8 = 0b100_00000;
    const EXPONENTIAL_DECREASE_PREFIX: u8 = 0b101_00000;
    const LINEAR_INCREASE_PREFIX: u8 = 0b110_00000;
    const BENT_INCREASE_PREFIX: u8 = 0b111_00000;

    pub fn from_mode_and_value(mode: GainMode, value: u32) -> Result<Gain, ValueError> {
        let to_fixed_gain = || match u8::try_from(value) {
            Ok(v) => {
                if v <= Self::FIXED_GAIN_MASK {
                    Ok(Gain::new(v))
                } else {
                    Err(ValueError::FixedGainOutOfRange)
                }
            }
            Err(_) => Err(ValueError::FixedGainOutOfRange),
        };
        let to_rate_gain = |prefix: u8| match u8::try_from(value) {
            Ok(v) => {
                if v <= Self::RATE_MASK {
                    Ok(Gain::new(prefix | v))
                } else {
                    Err(ValueError::GainRateOutOfRange)
                }
            }
            Err(_) => Err(ValueError::GainRateOutOfRange),
        };

        match mode {
            GainMode::Raw => Gain::try_from(value),
            GainMode::Fixed => to_fixed_gain(),
            GainMode::LinearDecrease => to_rate_gain(Self::LINEAR_DECREASE_PREFIX),
            GainMode::ExponentialDecrease => to_rate_gain(Self::EXPONENTIAL_DECREASE_PREFIX),
            GainMode::LinearIncrease => to_rate_gain(Self::LINEAR_INCREASE_PREFIX),
            GainMode::BentIncrease => to_rate_gain(Self::BENT_INCREASE_PREFIX),
        }
    }

    pub fn to_mode_and_value(&self) -> (GainMode, u8) {
        const RATE_BIT: u8 = Gain::FIXED_GAIN_MASK ^ u8::MAX;

        let raw = self.0;

        if (raw & RATE_BIT) == 0 {
            (GainMode::Fixed, raw & Self::FIXED_GAIN_MASK)
        } else {
            let rate = raw & Self::RATE_MASK;
            let mode = raw & (Self::RATE_MASK ^ u8::MAX);

            let mode = match mode {
                Self::LINEAR_DECREASE_PREFIX => GainMode::LinearDecrease,
                Self::EXPONENTIAL_DECREASE_PREFIX => GainMode::ExponentialDecrease,
                Self::LINEAR_INCREASE_PREFIX => GainMode::LinearIncrease,
                Self::BENT_INCREASE_PREFIX => GainMode::BentIncrease,
                _ => panic!(), // This should not happen
            };
            (mode, rate)
        }
    }

    pub fn value(&self) -> u8 {
        self.as_u8()
    }

    pub fn to_envelope_string(self) -> String {
        let (mode, value) = self.to_mode_and_value();
        format!("{} {}{}", GAIN_STR, mode.to_prefix_str(), value)
    }

    pub fn to_gui_string(self) -> String {
        let (mode, value) = self.to_mode_and_value();
        format!("{}{}", mode.to_prefix_str(), value)
    }
}

impl From<u8> for Gain {
    fn from(i: u8) -> Self {
        Gain::new(i)
    }
}

impl FromStr for Gain {
    type Err = ValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mode, value) = extract_gain_mode_and_value(s)?;
        Gain::from_mode_and_value(mode, value)
    }
}

fn extract_gain_mode_and_value(s: &str) -> Result<(GainMode, u32), ValueError> {
    let first = match s.as_bytes().first() {
        Some(f) => *f,
        None => return Err(ValueError::NoGain),
    };

    let (mode, value_str) = match GainMode::from_u8_char(first) {
        Some(mode) => (mode, s[1..].trim_start()),
        None => (GainMode::Raw, s),
    };

    let value = match value_str.as_bytes().first() {
        Some(b'$') => match u32::from_str_radix(&value_str[1..], 16) {
            Ok(i) => i,
            Err(_) => return Err(ValueError::InvalidGainString(value_str.to_owned())),
        },
        Some(_) => match value_str.parse() {
            Ok(i) => i,
            Err(_) => return Err(ValueError::InvalidGainString(value_str.to_owned())),
        },
        None => return Err(ValueError::NoGain),
    };
    Ok((mode, value))
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Envelope {
    Adsr(Adsr),
    Gain(Gain),
}

impl Envelope {
    pub fn try_from_envelope_str(s: &str) -> Result<Self, ValueError> {
        let s = s.trim();

        if s.is_empty() {
            return Err(ValueError::NoEnvelope);
        }

        match s.split_once(char::is_whitespace) {
            Some((a, b)) => Self::try_from_strs(a, b),
            None => Self::try_from_strs(s, ""),
        }
    }

    pub fn try_from_strs(envelope_type: &str, value: &str) -> Result<Self, ValueError> {
        match envelope_type {
            t if t == ADSR_STR => {
                let adsr = value.parse()?;
                Ok(Envelope::Adsr(adsr))
            }
            t if t == GAIN_STR => {
                let gain = value.parse()?;
                Ok(Envelope::Gain(gain))
            }
            u => Err(ValueError::UnknownEnvelopeType(u.to_owned())),
        }
    }

    pub fn to_envelope_string(&self) -> String {
        match self {
            Self::Adsr(a) => a.to_envelope_string(),
            Self::Gain(g) => g.to_envelope_string(),
        }
    }

    pub fn engine_value(&self) -> (u8, u8) {
        // for Adsr: adsr1 bit 7 is set.
        // for Gain: adsr1 is 0 (no adsr)

        match self {
            Envelope::Adsr(adsr) => (adsr.adsr1(), adsr.adsr2()),
            Envelope::Gain(gain) => (0, gain.value()),
        }
    }
}

impl<'de> Deserialize<'de> for Envelope {
    fn deserialize<D>(deserializer: D) -> Result<Envelope, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;

        Envelope::try_from_envelope_str(s).map_err(serde::de::Error::custom)
    }
}

impl Serialize for Envelope {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_envelope_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TempGain(u8);

impl TempGain {
    pub const DISABLED: TempGain = TempGain(0);

    pub fn try_from_mode_and_value(mode: GainMode, value: u32) -> Result<Self, ValueError> {
        match (mode, value) {
            (GainMode::Fixed, 0) => Err(ValueError::F0TempGain),
            (mode, value) => {
                let g = Gain::from_mode_and_value(mode, value)?;
                Ok(Self(g.as_u8()))
            }
        }
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }

    pub fn is_disabled(self) -> bool {
        self.0 == 0
    }
}

impl FromStr for TempGain {
    type Err = ValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mode, value) = extract_gain_mode_and_value(s)?;
        Self::try_from_mode_and_value(mode, value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OptionalGain(u8);

impl OptionalGain {
    pub const NONE: OptionalGain = OptionalGain(0);

    pub fn try_from_mode_and_value_forbid_none(
        mode: GainMode,
        value: u32,
    ) -> Result<Self, ValueError> {
        match Gain::from_mode_and_value(mode, value)?.as_u8() {
            0 => Err(ValueError::OptionalGainCannotBeZero),
            g => Ok(Self(g)),
        }
    }

    pub fn try_from_str_forbid_none(s: &str) -> Result<Self, ValueError> {
        match Gain::from_str(s)?.as_u8() {
            0 => Err(ValueError::OptionalGainCannotBeZero),
            g => Ok(Self(g)),
        }
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }

    pub fn is_none(self) -> bool {
        self.0 == 0
    }
}

impl From<OptionalGain> for TempGain {
    fn from(value: OptionalGain) -> Self {
        Self(value.0)
    }
}
