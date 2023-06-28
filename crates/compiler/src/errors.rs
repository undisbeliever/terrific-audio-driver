//! A single location for all of the errors in the compilers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::fmt::Display;
use std::io;
use std::path::PathBuf;

use crate::data::Name;

#[derive(Debug)]
pub enum DeserializeError {
    NoParentPath(String),
    OpenError(String, io::Error),
    SerdeError(String, serde_json::error::Error),
}

#[derive(Debug)]
pub struct InvalidAdsrError {
    pub valid_a: bool,
    pub valid_d: bool,
    pub valid_sl: bool,
    pub valid_sr: bool,
}

#[derive(Debug)]
pub enum InvalidGainError {
    InvalidGain(String),
}

#[derive(Debug)]
pub enum ParseError {
    EmptyName,
    InvalidName(String),
    AdsrNotFourValues,
    InvalidAdsr(InvalidAdsrError),
}

#[derive(Debug)]
pub enum NoteError {
    CannotParseNote(String),
    UnknownNote(char),
    InvalidNoteOctave(u32),
    InvalidNote,
}

#[derive(Debug)]
pub enum TickClockError {
    CannotParse(String),
    OutOfBounds(u32),
}

#[derive(Debug)]
pub enum LoopCountError {
    NotEnoughLoops,
    TooManyLoops,
}

#[derive(Debug)]
pub enum BytecodeError {
    OpenLoopStack(usize),
    NotInALoop,
    MissingLoopCount,
    CannotHaveLoopCountAtStartAndEndLoop,
    TooManyLoops,
    MultipleSkipLastLoopInstructions,
    NoTicksBeforeSkipLastLoop,
    NoTicksAfterSkipLastLoop,
    NoTicksInLoop,
    SkipLastLoopOutOfBounds(usize),

    NoteLengthTooShortKeyOffDelay,
    NoteLengthZero,
    NoteLengthTooLarge,

    PortamentoVelocityZero,
    PortamentoVelocityTooLarge,

    PitchOffsetPerTickZero,
    PitchOffsetOutOfRange,
    QuarterWaveLengthZero,
    QuarterWaveLengthOutOfRange,

    VolumeAdjustOutOfRange(i32),
    VolumeOutOfRange(u32),
    PanAdjustOutOfRange(i32),
    PanOutOfRange(u32),

    SubroutineCallInSubroutine,
    ReturnInNonSubroutine,

    CannotChangeTickClockInASoundEffect,
}

#[derive(Debug)]
pub enum BytecodeAssemblerError {
    BytecodeError(BytecodeError),

    UnknownInstruction(String),

    InvalidNumberOfArguments(u8),
    InvalidNumberOfArgumentsRange(u8, u8),

    CannotParseTickCounter(String),
    CannotParseUnsigned(String),
    CannotParseSigned(String),

    UnknownInstrument(String),
    UnknownSubroutine(String),

    InvalidKeyoffArgument(String),
    InvalidPortamentoVelocity(String),

    InvalidLoopCount(LoopCountError),
    InvalidNote(NoteError),
    InvalidAdsr(InvalidAdsrError),
    InvalidGain(InvalidGainError),
    InvalidTickClock(TickClockError),
}

#[derive(Debug)]
pub struct SoundEffectError {
    pub sfx_name: String,
    pub sfx_line_no: usize,
    pub invalid_name: bool,
    pub no_notes: bool,
    // Set if the last instruction is not disable_channel
    pub no_disable_channel: bool,
    pub errors: Vec<(usize, BytecodeAssemblerError)>,
}

#[derive(Debug)]
pub enum SoundEffectsFileError {
    SoundEffectErrors(Vec<SoundEffectError>),
    // Line number, Name
    DuplicateSfxNamesInSfxFile(Vec<(usize, Name)>),
    MissingSoundEffects(Vec<Name>),
}

#[derive(Debug)]
pub enum SampleError {
    IoError(PathBuf, io::Error),
    UnknownFileType(PathBuf),
    WaveFileError(PathBuf, brr::Error),
    BrrEncodeError(PathBuf, brr::EncodeError),
    BrrParseError(PathBuf, brr::ParseError),
    FileTooLarge(PathBuf),
    CannotUseDupeBlockHackOnBrrFiles,
    LoopingFlagMismatch { brr_looping: bool },

    GainAndAdsr,
    NoGainOrAdsr,
}

#[derive(Debug)]
pub enum OtherSamplesError {
    TooManyInstruments(usize),
    TooManyBrrSamples(usize),
    BrrDataOverflow(usize),
    PitchTableError(PitchTableError),
}

#[derive(Debug)]
pub struct SamplesErrors {
    pub other_errors: Vec<OtherSamplesError>,
    // Instrument index, SampleError
    pub instrument_errors: Vec<(usize, SampleError)>,
}

// ::TODO Do not use Display for sample errors::

#[derive(Debug)]
pub enum CommonAudioDataError {
    TooManyInstruments(usize),
    TooManySamples(usize),
    TooManySoundEffects(usize),
    CommonAudioDataTooLarge(usize),
    SampleError(SamplesErrors),
    SoundEffectError(SoundEffectsFileError),
}

pub type CommonAudioDataErrors = Vec<CommonAudioDataError>;

#[derive(Debug)]
pub enum PitchError {
    SampleRateTooHigh,
    SampleRateTooLow,
    FirstOctaveGreaterThanLastOctave,
    FirstOctaveTooLow(i32),
    LastOctaveTooHigh(i32),
    FirstOctaveTooLowLastOctaveTooHigh(i32, i32),
}

#[derive(Debug)]
pub enum PitchTableError {
    TooManyInstruments,
    TooManyPitches(usize),
    InstrumentErrors(Vec<(usize, PitchError)>),
}

impl From<InvalidAdsrError> for ParseError {
    fn from(e: InvalidAdsrError) -> Self {
        Self::InvalidAdsr(e)
    }
}

impl From<NoteError> for BytecodeAssemblerError {
    fn from(e: NoteError) -> Self {
        Self::InvalidNote(e)
    }
}

impl From<LoopCountError> for BytecodeAssemblerError {
    fn from(e: LoopCountError) -> Self {
        Self::InvalidLoopCount(e)
    }
}

impl From<InvalidAdsrError> for BytecodeAssemblerError {
    fn from(e: InvalidAdsrError) -> Self {
        Self::InvalidAdsr(e)
    }
}

impl From<InvalidGainError> for BytecodeAssemblerError {
    fn from(e: InvalidGainError) -> Self {
        Self::InvalidGain(e)
    }
}

impl From<TickClockError> for BytecodeAssemblerError {
    fn from(e: TickClockError) -> Self {
        Self::InvalidTickClock(e)
    }
}

// Display
// =======

impl Display for DeserializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoParentPath(filename) => {
                write!(f, "Cannot load {}: No parent path", filename)
            }
            Self::OpenError(filename, e) => write!(f, "Unable to open {}: {}", filename, e),
            Self::SerdeError(filename, e) => write!(f, "Unable to read {}: {}", filename, e),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}

impl Display for NoteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}

impl Display for BytecodeAssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}

impl Display for CommonAudioDataError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}
