//! Envelope data structures

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

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

impl TryFrom<&str> for Adsr {
    type Error = ValueError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
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

u8_value_newtype!(Gain, GainOutOfRange, NoGain);

impl Gain {
    pub fn value(&self) -> u8 {
        self.as_u8()
    }

    pub fn to_envelope_string(self) -> String {
        format!("{} {}", GAIN_STR, self.as_u8())
    }

    pub fn to_gui_string(self) -> String {
        self.as_u8().to_string()
    }
}

impl From<u8> for Gain {
    fn from(i: u8) -> Self {
        Gain::new(i)
    }
}

impl TryFrom<&str> for Gain {
    type Error = ValueError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        // ::TODO figure out what the gain bits do and properly parse them::

        match s.as_bytes().first() {
            Some(b'$') => match u8::from_str_radix(&s[1..], 16) {
                Ok(i) => Ok(Gain::new(i)),
                Err(_) => Err(ValueError::InvalidGainString(s.to_owned())),
            },
            Some(_) => match s.parse() {
                Ok(i) => Ok(Gain::new(i)),
                Err(_) => Err(ValueError::InvalidGainString(s.to_owned())),
            },
            None => Err(ValueError::NoGain),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
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
                let adsr = Adsr::try_from(value)?;
                Ok(Envelope::Adsr(adsr))
            }
            t if t == GAIN_STR => {
                let gain = Gain::try_from(value)?;
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
