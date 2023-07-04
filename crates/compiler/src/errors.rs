//! A single location for all of the errors in the compilers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::data::Name;
use crate::mml;
use crate::notes::Note;
use crate::time::TickCounter;

use std::fmt::Display;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ErrorWithLine<T>(pub u32, pub T);

#[derive(Debug)]
pub struct ErrorWithPos<T>(pub mml::FilePos, pub T);

#[derive(Debug)]
pub enum DeserializeError {
    NoParentPath(String),
    OpenError(String, io::Error),
    SerdeError(String, serde_json::error::Error),
}

#[derive(Debug)]
pub enum MappingListError {
    Empty,
    TooManyItems(usize),
    DuplicateName(usize, String),
}

#[derive(Debug)]
pub enum MappingError {
    Instrument(MappingListError),
    SoundEffect(MappingListError),
}

#[derive(Debug)]
pub struct MappingsFileErrors(pub Vec<MappingError>);

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
pub enum ValueError {
    CannotParseUnsigned(String),
    CannotParseSigned(String),

    InvalidName(String),

    AdsrNotFourValues,
    InvalidAdsr(InvalidAdsrError),

    InvalidGain(InvalidGainError),

    InvalidNote,
    NoNoteOctave,
    UnknownNotePitch(char),
    UnknownNoteCharacter(char),

    NoteOutOfRange,
    OctaveOutOfRange,

    // bytecode tick-count arguments out of range
    BcTicksKeyOffOutOfRange,
    BcTicksNoKeyOffOutOfRange,

    PanOutOfRange,
    VolumeOutOfRange,
    CoarseVolumeOutOfRange,

    RelativePanOutOfRange,
    RelativeVolumeOutOfRange,
    RelativeCoarseVolumeOutOfRange,

    PitchOffsetPerTickOutOfRange,
    QuarterWavelengthOutOfRange,

    PortamentoVelocityZero,
    PortamentoVelocityOutOfRange,

    QuantizeOutOfRange,
    TransposeOutOfRange,
    PortamentoSpeedOutOfRange,

    ZenLenOutOfRange,
    TickClockOutOfRange,
    BpmOutOfRange,

    MidiNoteNumberOutOfRange,
    CannotConvertMidiNote,

    InvalidMmlBool,

    NotEnoughLoops,
    TooManyLoops,

    MissingTickCount,
    InvalidNoteLength,
    DotsNotAllowedAfterClockValue,

    CannotConvertBpmToTickClock,

    EchoEdlOutOfRange,
    EchoLengthNotMultiple,
    EchoBufferTooLarge,

    InvalidFirFilterSize,
    InvalidFirFilter,

    NoName,
    NoBool,
    NoNumber,
    NoNote,
    NoIncrementOrDecrement,
    NoVolume,
    NoPan,
    NoOctave,
    NoZenLen,
    NoTranspose,
    NoLoopCount,
    NoQuantize,
    NoTickClock,
    NoBpm,
    NoMidiNote,
    NoPortamentoSpeed,
    NoPortamentoVelocity,
    NoMpDepth,
    NoPitchOffsetPerTick,
    NoVibratoDepth,
    NoCommaQuarterWavelength,
    NoQuarterWavelength,
    NoEchoEdl,
}

#[derive(Debug)]
pub enum BytecodeError {
    OpenLoopStack(usize),
    NotInALoop,
    MissingLoopCount,
    CannotHaveLoopCountAtStartAndEndLoop,
    TooManyLoops,
    TooManyLoopsInSubroutineCall,
    MultipleSkipLastLoopInstructions,
    NoTicksBeforeSkipLastLoop,
    NoTicksAfterSkipLastLoop,
    NoTicksInLoop,
    SkipLastLoopOutOfBounds(usize),

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

    ArgumentError(ValueError),

    UnknownInstrument(String),
    UnknownSubroutine(String),

    InvalidKeyoffArgument(String),
    InvalidPortamentoVelocity(String),
}

#[derive(Debug)]
pub struct SoundEffectError {
    pub sfx_name: String,
    pub sfx_line_no: u32,
    pub invalid_name: bool,
    pub no_notes: bool,
    // Set if the last instruction is not disable_channel
    pub no_disable_channel: bool,
    pub errors: Vec<ErrorWithLine<BytecodeAssemblerError>>,
}

#[derive(Debug)]
pub enum SoundEffectsFileError {
    SoundEffectErrors(Vec<SoundEffectError>),
    // Line number, Name
    DuplicateSfxNamesInSfxFile(Vec<(u32, Name)>),
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

#[derive(Debug)]
pub enum IdentifierError {
    Empty,
    InvalidName(String),
    InvalidNumber(String),
}

// u32 is line number
// char is the first character in the line
// String is the erroneous characters/string
#[derive(Debug)]
pub enum SplitMmlLinesError {
    MmlTooLarge(usize),
    TooManySubroutines(usize),

    NoIdentifier(u32, char),
    InvalidIdentifier(u32, char, IdentifierError),

    UnknownChannel(u32, String),
    MissingInstrumentText(u32),
    MissingSubroutineText(u32),
    MissingChannelText(u32),
    CannotParseLine(u32, char),
}

#[derive(Debug)]
pub enum MmlHeaderError {
    NoHeader,
    NoValue,
    ValueError(ValueError),
    DuplicateHeader(String),

    InvalidEchoFeedback,
    InvalidEchoVolume,
    CannotSetTempo,
    CannotSetTimer,
}

// u32 is line number
#[derive(Debug)]
pub enum MmlInstrumentError {
    NoInstrument,
    CannotFindInstrument(String),
    ExpectedFourAdsrArguments,
    InvalidAdsr(InvalidAdsrError),
    ExpectedOneGainArgument,
    InvalidGain(InvalidGainError),
    UnknownArgument(String),
}

#[derive(Debug)]
pub enum MmlParserError {
    // MmlStreamParser errors
    ValueError(ValueError),

    // + or - after a pitch
    TooManyAccidentals,
    TooManyDotsInNoteLength,

    UnknownCharacters(u32),

    // MmlParser errors
    InvalidNote,

    NoSubroutine,
    NoInstrument,
    CannotCallSubroutineInASubroutine,
    CannotFindSubroutine(String),

    NoStartBrokenChord,
    NoStartPortamento,

    MissingEndBrokenChord,
    MissingEndPortamento,

    PortamentoRequiresTwoPitches,
    InvalidPortamentoDelay,

    InvalidBrokenChordNoteLength { tie: bool },

    MissingNoteBeforeTie,
    MissingNoteBeforeSlur,
    CannotParseComma,

    InvalidPitchListSymbol,

    NoteOutOfRange(Note, Note, Note),
    NoInstrumentSet,
}

#[derive(Debug)]
pub enum MmlCommandError {
    BytecodeError(BytecodeError),
    ValueError(ValueError),

    NoteOutOfRange(Note, Note, Note),
    CannotPlayNoteBeforeSettingInstrument,

    LoopPointAlreadySet,
    CannotFindSubroutine,
    CannotFindInstrument,
    CannotCallSubroutineTooManyNestedLoops(mml::Identifier, usize),
    CannotUseMpWithoutInstrument,
    MpPitchOffsetTooLarge(u32),
    MpDepthZero,

    PortamentoDelayTooLong,
    CannotCalculatePortamentoVelocity,

    NoNotesInBrokenChord,
    TooManyNotesInBrokenChord(usize),
    BrokenChordTotalLengthTooShort,

    BrokenChordTickCountMismatch(TickCounter, TickCounter),

    NoTicksAfterLoopPoint,
}

#[derive(Debug)]
pub struct MmlChannelError {
    pub identifier: mml::Identifier,
    pub parse_errors: Vec<ErrorWithPos<MmlParserError>>,
    pub command_errors: Vec<ErrorWithPos<MmlCommandError>>,
}

#[derive(Debug)]
pub enum MmlError {
    Lines(Vec<SplitMmlLinesError>),
    Header(Vec<ErrorWithLine<MmlHeaderError>>),
    Instruments(Vec<ErrorWithLine<MmlInstrumentError>>),
    DuplicateInstructuments(Vec<(u32, mml::Identifier)>),
    SubroutineError(MmlChannelError),
    ChannelError(MmlChannelError),
}

#[derive(Debug)]
pub enum SongError {
    PitchTableError(String, PitchTableError),
    MmlError(String, Vec<MmlError>),

    NoMusicChannels,
    InvalidMmlData,
    SongIsTooLarge(usize),

    InvalidHeaderSize,

    // This should not happen
    DataSizeMismatch(usize, usize),
}

// From Traits
// ===========

impl From<ValueError> for BytecodeAssemblerError {
    fn from(e: ValueError) -> Self {
        Self::ArgumentError(e)
    }
}

impl From<InvalidAdsrError> for BytecodeAssemblerError {
    fn from(e: InvalidAdsrError) -> Self {
        Self::ArgumentError(ValueError::InvalidAdsr(e))
    }
}

impl From<InvalidGainError> for BytecodeAssemblerError {
    fn from(e: InvalidGainError) -> Self {
        Self::ArgumentError(ValueError::InvalidGain(e))
    }
}

impl From<ValueError> for MmlHeaderError {
    fn from(e: ValueError) -> Self {
        Self::ValueError(e)
    }
}

impl From<ValueError> for MmlParserError {
    fn from(e: ValueError) -> Self {
        Self::ValueError(e)
    }
}

impl From<BytecodeError> for MmlCommandError {
    fn from(e: BytecodeError) -> Self {
        Self::BytecodeError(e)
    }
}

impl From<ValueError> for MmlCommandError {
    fn from(e: ValueError) -> Self {
        Self::ValueError(e)
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

impl Display for MappingsFileErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}

impl Display for ValueError {
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

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::TODO human readable error messages::
        write!(f, "{:?}", self)
    }
}
