//! A single location for all of the errors in the compilers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

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
pub enum BytecodeError {
    OpenLoopStack(usize),
    NotInALoop,
    TooManyLoops,
    LoopCountOutOfRange(u32),
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

    InvalidNote(NoteError),
    InvalidAdsr(InvalidAdsrError),
    InvalidGain(InvalidGainError),
    InvalidTickClock(TickClockError),
}

impl From<NoteError> for BytecodeAssemblerError {
    fn from(e: NoteError) -> Self {
        Self::InvalidNote(e)
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

impl std::fmt::Display for BytecodeAssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}
