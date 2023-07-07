//! A single location for all of the errors in the compilers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    BcTicksKeyOff, BcTicksNoKeyOff, LoopCount, Pan, PitchOffsetPerTick, PortamentoVelocity,
    QuarterWavelengthInTicks, RelativePan, RelativeVolume, Volume, MAX_NESTED_LOOPS,
};
use crate::data::Name;
use crate::driver_constants::{
    COMMON_DATA_ADDR, ECHO_BUFFER_EDL_MS, FIR_FILTER_SIZE, MAX_COMMON_DATA_SIZE, MAX_DIR_ITEMS,
    MAX_INSTRUMENTS, MAX_SONG_DATA_SIZE, MAX_SOUND_EFFECTS, MAX_SUBROUTINES, PITCH_TABLE_SIZE,
};
use crate::echo::{EchoEdl, EchoLength};
use crate::mml::{MAX_BROKEN_CHORD_NOTES, MAX_MML_TEXT_LENGTH};
use crate::mml_command_parser::{
    PortamentoSpeed, Quantization, Transpose, MAX_COARSE_VOLUME, MAX_RELATIVE_COARSE_VOLUME,
    MIN_RELATIVE_COARSE_VOLUME,
};
use crate::notes::{MidiNote, Note};
use crate::time::{Bpm, TickClock, TickCounter, ZenLen};
use crate::{mml, Octave};

use std::fmt::Display;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ErrorWithLine<T>(pub u32, pub T);

#[derive(Debug)]
pub struct ErrorWithPos<T>(pub mml::FilePos, pub T);

#[derive(Debug)]
pub enum FileError {
    OpenError(String, io::Error),
    ReadError(String, io::Error),
    FileTooLarge(String),
    Utf8Error(String),
}

#[derive(Debug)]
pub enum DeserializeError {
    FileError(FileError),
    NoParentPath(String),
    SerdeError(String, serde_json::error::Error),
}

#[derive(Debug)]
pub enum UniqueNameListError {
    Empty,
    TooManyItems(usize, usize),
    DuplicateName(usize, String),
}

#[derive(Debug)]
pub enum ProjectFileError {
    Instrument(UniqueNameListError),
    SoundEffect(UniqueNameListError),
    Song(UniqueNameListError),
}

#[derive(Debug)]
pub struct ProjectFileErrors(pub Vec<ProjectFileError>);

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

    MissingNoteLengthTickCount,
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
    NoDirectionInPortamentoVelocity,
}

#[derive(Debug)]
pub struct SoundEffectError {
    pub sfx_name: String,
    pub sfx_line_no: u32,
    pub invalid_name: bool,
    pub no_notes: bool,
    pub errors: Vec<ErrorWithLine<BytecodeAssemblerError>>,
}

#[derive(Debug)]
pub enum SoundEffectsFileError {
    SoundEffectErrors(Vec<SoundEffectError>),
    // Line number, Name
    DuplicateSfxNamesInSfxFile(Vec<(u32, Name)>),

    // Using String so they can be joined with slice::join
    MissingSoundEffects(Vec<String>),
}

#[derive(Debug)]
pub enum SampleError {
    IoError(PathBuf, io::Error),
    UnknownFileType(PathBuf),
    WaveFileError(PathBuf, brr::WavError),
    BrrEncodeError(PathBuf, brr::EncodeError),
    BrrParseError(PathBuf, brr::ParseError),
    FileTooLarge(PathBuf),
    CannotUseDupeBlockHackOnBrrFiles,
    LoopingFlagMismatch { brr_looping: bool },

    GainAndAdsr,
    NoGainOrAdsr,
}

#[derive(Debug)]
pub enum TaggedSampleError {
    Instrument(usize, Name, SampleError),
}

#[derive(Debug)]
pub struct SampleAndInstrumentDataError {
    pub sample_errors: Vec<TaggedSampleError>,
    pub pitch_table_error: Option<PitchTableError>,
}

#[derive(Debug)]
pub enum CommonAudioDataError {
    TooManyInstruments(usize),
    TooManyBrrSamples(usize),
    TooManySoundEffects(usize),
    CommonAudioDataTooLarge(usize),
}

#[derive(Debug)]
pub struct CommonAudioDataErrors {
    pub errors: Vec<CommonAudioDataError>,
}

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
    InstrumentErrors(Vec<(usize, Name, PitchError)>),
}

#[derive(Debug)]
pub enum IdentifierError {
    Empty,
    InvalidName(String),
    InvalidNumber(String),
}

#[derive(Debug)]
pub enum MmlLineError {
    ValueError(ValueError),

    // Split Mml Line errors
    MmlTooLarge(usize),
    TooManySubroutines(usize),

    // char is the first character in the line (! or @)
    NoIdentifier(char),
    InvalidIdentifier(IdentifierError),

    // String is a list of invalid channels in the line
    UnknownChannel(String),
    MissingInstrumentText,
    MissingSubroutineText,
    MissingChannelText,
    CannotParseLine,

    // MML Header errors
    NoHeader,
    NoValue,
    DuplicateHeader(String),

    InvalidEchoFeedback,
    InvalidEchoVolume,
    CannotSetTempo,
    CannotSetTimer,

    // Instrument errors
    NoInstrument,
    CannotFindInstrument(String),
    ExpectedFourAdsrArguments,
    InvalidAdsr(InvalidAdsrError),
    ExpectedOneGainArgument,
    InvalidGain(InvalidGainError),
    UnknownInstrumentArgument(String),
    DuplicateInstrumentName(String),
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

    MissingNoteBeforeTie,
    MissingNoteBeforeSlur,
    CannotParseComma,

    InvalidPitchListSymbol,
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
    PortamentoRequiresInstrument,

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

impl MmlChannelError {
    fn n_errors(&self) -> usize {
        self.parse_errors.len() + self.command_errors.len()
    }
}

#[derive(Debug)]
pub struct MmlCompileErrors {
    pub line_errors: Vec<ErrorWithLine<MmlLineError>>,
    pub subroutine_errors: Vec<MmlChannelError>,
    pub channel_errors: Vec<MmlChannelError>,
}

impl MmlCompileErrors {
    fn n_errors(&self) -> usize {
        self.line_errors.len()
            + self
                .subroutine_errors
                .iter()
                .map(MmlChannelError::n_errors)
                .sum::<usize>()
            + self
                .channel_errors
                .iter()
                .map(MmlChannelError::n_errors)
                .sum::<usize>()
    }
}

#[derive(Debug)]
pub enum SongError {
    NoMusicChannels,
    InvalidMmlData,
    SongIsTooLarge(usize),

    InvalidHeaderSize,

    // This should not happen
    DataSizeMismatch(usize, usize),
}

#[derive(Debug)]
pub enum ExportSpcFileError {
    SongError(SongError),
    TooMuchData {
        common: usize,
        song: usize,
        echo: usize,
    },
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

impl From<ValueError> for MmlLineError {
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

impl Display for FileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::OpenError(fname, e) => write!(f, "unable to open {}: {}", fname, e),
            Self::ReadError(fname, e) => write!(f, "unable to read {}: {}", fname, e),
            Self::FileTooLarge(fname) => write!(f, "unable to read {}: file too large", fname),
            Self::Utf8Error(fname) => write!(
                f,
                "unable to read {}: file did not contain valid UTF-8",
                fname
            ),
        }
    }
}

impl Display for DeserializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::FileError(e) => e.fmt(f),
            Self::NoParentPath(filename) => {
                write!(f, "cannot load {}: no parent path", filename)
            }
            Self::SerdeError(filename, e) => write!(f, "unable to read {}: {}", filename, e),
        }
    }
}

fn fmt_unique_name_list_error(
    f: &mut std::fmt::Formatter,
    e: &UniqueNameListError,
    list_type: &str,
) -> std::fmt::Result {
    match e {
        UniqueNameListError::Empty => write!(f, "no {}", list_type),
        UniqueNameListError::TooManyItems(len, max) => {
            write!(f, "too many {} ({}, max {})", list_type, len, max)
        }
        UniqueNameListError::DuplicateName(i, name) => {
            write!(f, "duplicate {}: {} {}", list_type, i, name)
        }
    }
}

impl Display for ProjectFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Instrument(e) => fmt_unique_name_list_error(f, e, "instrument"),
            Self::SoundEffect(e) => fmt_unique_name_list_error(f, e, "sound effect"),
            Self::Song(e) => fmt_unique_name_list_error(f, e, "song"),
        }
    }
}

impl Display for ProjectFileErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} errors in project file", self.0.len())
    }
}

impl Display for InvalidAdsrError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "invalid adsr")?;

        if self.valid_a {
            write!(f, " attack")?;
        }
        if self.valid_d {
            write!(f, " decay")?;
        }
        if self.valid_sl {
            write!(f, " sustain-level")?;
        }
        if self.valid_sr {
            write!(f, " sustain-rate")?;
        }

        write!(f, " values")
    }
}

impl Display for InvalidGainError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InvalidGain(s) => write!(f, "invalid gain value: {}", s),
        }
    }
}

impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        #[rustfmt::skip]
        macro_rules! out_of_range {
            ($name:literal, $newtype:ident) => {
                write!(f, concat!($name, " out of bounds ({} - {})"), $newtype::MIN, $newtype::MAX)
            };
        }

        match self {
            Self::CannotParseUnsigned(s) => write!(f, "cannot parse unsigned number: {}", s),
            Self::CannotParseSigned(s) => write!(f, "cannot parse signed number: {}", s),

            Self::InvalidName(s) => write!(f, "invalid name: {}", s),

            Self::AdsrNotFourValues => write!(f, "expected 4 adsr values"),
            Self::InvalidAdsr(e) => e.fmt(f),
            Self::InvalidGain(e) => e.fmt(f),

            Self::InvalidNote => write!(f, "invalid note"),
            Self::NoNoteOctave => write!(f, "no octave in note"),
            Self::UnknownNotePitch(c) => write!(f, "invalid note pitch: {}", c),
            Self::UnknownNoteCharacter(c) => write!(f, "invalid note character: {}", c),

            Self::NoteOutOfRange => write!(f, "note out of range"),
            Self::OctaveOutOfRange => out_of_range!("octave", Octave),

            Self::BcTicksKeyOffOutOfRange => write!(
                f,
                "invalid tick count (expected {} - {})",
                BcTicksKeyOff::MIN,
                BcTicksKeyOff::MAX
            ),
            Self::BcTicksNoKeyOffOutOfRange => write!(
                f,
                "invalid tick count (expected {} - {})",
                BcTicksNoKeyOff::MIN,
                BcTicksNoKeyOff::MAX
            ),

            Self::PanOutOfRange => out_of_range!("pan", Pan),
            Self::VolumeOutOfRange => out_of_range!("volume", Volume),
            Self::CoarseVolumeOutOfRange => {
                write!(f, "volume out of range ({} - {})", 0, MAX_COARSE_VOLUME)
            }

            Self::RelativePanOutOfRange => out_of_range!("relative pan", RelativePan),
            Self::RelativeVolumeOutOfRange => out_of_range!("relative volume", RelativeVolume),
            Self::RelativeCoarseVolumeOutOfRange => write!(
                f,
                "relative volume out of range ({} - {})",
                MIN_RELATIVE_COARSE_VOLUME, MAX_RELATIVE_COARSE_VOLUME
            ),

            Self::PitchOffsetPerTickOutOfRange => {
                out_of_range!("pitch offset per tick", PitchOffsetPerTick)
            }
            Self::QuarterWavelengthOutOfRange => {
                out_of_range!("quarter wavelength", QuarterWavelengthInTicks)
            }

            Self::PortamentoVelocityZero => write!(f, "portamento velocity cannot be 0"),
            Self::PortamentoVelocityOutOfRange => {
                out_of_range!("portamento velocity", PortamentoVelocity)
            }

            Self::QuantizeOutOfRange => out_of_range!("quantization", Quantization),
            Self::TransposeOutOfRange => out_of_range!("transpose", Transpose),

            Self::PortamentoSpeedOutOfRange => out_of_range!("portamento speed", PortamentoSpeed),

            Self::ZenLenOutOfRange => out_of_range!("zenlen", ZenLen),
            Self::TickClockOutOfRange => out_of_range!("tick clock", TickClock),
            Self::BpmOutOfRange => out_of_range!("bpm", Bpm),

            Self::MidiNoteNumberOutOfRange => out_of_range!("MIDI note number", MidiNote),
            Self::CannotConvertMidiNote => {
                write!(f, "cannot convert MIDI note to audio-driver note")
            }

            Self::InvalidMmlBool => write!(f, "cannot parse bool, expected a 0 or a 1"),

            Self::NotEnoughLoops => write!(f, "not enough loops (min: {})", LoopCount::MIN),
            Self::TooManyLoops => write!(f, "too many loops (max: {})", LoopCount::MAX),

            Self::MissingNoteLengthTickCount => write!(f, "missing tick count in note length"),
            Self::InvalidNoteLength => write!(f, "invalid note length"),
            Self::DotsNotAllowedAfterClockValue => {
                write!(f, "dots not allowed after a tick count length")
            }
            Self::CannotConvertBpmToTickClock => write!(f, "cannot convert bpm to tick clock"),

            Self::EchoEdlOutOfRange => out_of_range!("echo EDL", EchoEdl),
            Self::EchoLengthNotMultiple => write!(
                f,
                "echo length is not a multiple of {}ms",
                ECHO_BUFFER_EDL_MS
            ),
            Self::EchoBufferTooLarge => {
                write!(f, "echo buffer too large (max {}ms)", EchoLength::MAX)
            }

            Self::InvalidFirFilterSize => {
                write!(f, "expected {} FIR filter values", FIR_FILTER_SIZE)
            }
            Self::InvalidFirFilter => write!(
                f,
                "invalid FIR filter (all {} values must be between {} - {})",
                FIR_FILTER_SIZE,
                i8::MIN,
                i8::MAX
            ),

            Self::NoName => write!(f, "no name"),
            Self::NoBool => write!(f, "no bool"),
            Self::NoNote => write!(f, "no note"),
            Self::NoIncrementOrDecrement => {
                write!(f, "cannot parse relative value: expected a + or -")
            }
            Self::NoVolume => write!(f, "no volume"),
            Self::NoPan => write!(f, "no pan"),
            Self::NoOctave => write!(f, "no octave"),
            Self::NoZenLen => write!(f, "no zenlen value"),
            Self::NoTranspose => write!(f, "no transpose value"),
            Self::NoLoopCount => write!(f, "no loop count"),
            Self::NoQuantize => write!(f, "no quantization value"),
            Self::NoTickClock => write!(f, "no tick clock"),
            Self::NoBpm => write!(f, "no tempo bpm"),
            Self::NoMidiNote => write!(f, "no MIDI note number"),
            Self::NoPortamentoSpeed => write!(f, "no portamento speed"),
            Self::NoPortamentoVelocity => write!(f, "no portamento velocity"),
            Self::NoMpDepth => write!(f, "no MP depth"),
            Self::NoPitchOffsetPerTick => write!(f, "no pitch-offset-per-tick"),
            Self::NoVibratoDepth => write!(f, "no vibrato depth"),
            Self::NoCommaQuarterWavelength => {
                write!(f, "cannot parse quarter-wavelength, expected a comma ','")
            }
            Self::NoQuarterWavelength => write!(f, "no quarter-wavelength"),
            Self::NoEchoEdl => write!(f, "no echo EDL"),
        }
    }
}

impl Display for BytecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::OpenLoopStack(len) => write!(f, "loop stack not empty ({} open loops)", len),
            Self::NotInALoop => write!(f, "not in a loop"),
            Self::MissingLoopCount => write!(f, "missing loop count"),
            Self::CannotHaveLoopCountAtStartAndEndLoop => {
                write!(f, "cannot have a loop count at the start and end of a loop")
            }
            Self::TooManyLoops => write!(f, "too many nested loops (max: {})", MAX_NESTED_LOOPS),
            Self::TooManyLoopsInSubroutineCall => write!(f, "too many loops in subroutine call"),
            Self::MultipleSkipLastLoopInstructions => {
                write!(f, "multiple skip_last_loop instructions in a single loop")
            }
            Self::NoTicksBeforeSkipLastLoop => {
                write!(f, "no ticks before skip_last_loop instruction")
            }
            Self::NoTicksAfterSkipLastLoop => {
                write!(f, "no ticks after skip_last_loop instruction")
            }
            Self::NoTicksInLoop => write!(f, "no ticks in loop"),
            Self::SkipLastLoopOutOfBounds(to_skip) => {
                write!(
                    f,
                    "skip_last_loop out of bounds ({}, max: {}",
                    to_skip,
                    u8::MAX
                )
            }

            Self::SubroutineCallInSubroutine => write!(f, "cannot call subroutine in a subroutine"),
            Self::ReturnInNonSubroutine => {
                write!(f, "return_from_subroutine instruction in a non-subroutine")
            }

            Self::CannotChangeTickClockInASoundEffect => {
                write!(f, "cannot change tick clock in a sound effect")
            }
        }
    }
}

impl Display for BytecodeAssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BytecodeError(e) => e.fmt(f),
            Self::ArgumentError(e) => e.fmt(f),

            Self::UnknownInstruction(s) => write!(f, "unknown instruction {}", s),

            Self::InvalidNumberOfArguments(n) => write!(f, "expected {} arguments", n),
            Self::InvalidNumberOfArgumentsRange(min, max) => {
                write!(f, "expected {} - {} arguments", min, max)
            }

            Self::UnknownInstrument(s) => write!(f, "cannot find instrument: {}", s),
            Self::UnknownSubroutine(s) => write!(f, "cannot find subroutine: {}", s),

            Self::InvalidKeyoffArgument(s) => write!(f, "invalid keyoff argument: {}", s),
            Self::NoDirectionInPortamentoVelocity => {
                write!(f, "missing + or - in portamento velocity")
            }
        }
    }
}

impl Display for SoundEffectError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "error compiling {}", self.sfx_name)
    }
}

impl Display for SoundEffectsFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "error compiling sound effects file")
    }
}

impl Display for SampleError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IoError(p, e) => write!(f, "cannot read {}: {}", p.display(), e),
            Self::UnknownFileType(p) => write!(f, "unknown file type: {}", p.display()),
            Self::WaveFileError(p, e) => write!(f, "error loading {}: {}", p.display(), e),
            Self::BrrEncodeError(p, e) => write!(f, "error encoding {}: {}", p.display(), e),
            Self::BrrParseError(p, e) => write!(f, "error loading {}: {}", p.display(), e),
            Self::FileTooLarge(p) => write!(f, "file too large: {}", p.display()),
            Self::CannotUseDupeBlockHackOnBrrFiles => {
                write!(f, "cannot use dupe_block_hack on .brr files")
            }
            Self::LoopingFlagMismatch { brr_looping } => write!(
                f,
                "looping flag in BRR ({}) does not match JSON ({})",
                brr_looping, !brr_looping
            ),

            Self::GainAndAdsr => write!(f, "cannot use adsr and gain at the same time"),
            Self::NoGainOrAdsr => write!(f, "no adsr or gain"),
        }
    }
}

impl Display for CommonAudioDataError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::TooManyInstruments(len) => {
                write!(f, "too many instruments ({}, max: {}", len, MAX_INSTRUMENTS)
            }
            Self::TooManyBrrSamples(len) => {
                write!(f, "too many BRR samples ({}, max: {})", len, MAX_DIR_ITEMS)
            }
            Self::TooManySoundEffects(len) => write!(
                f,
                "too many sound effects ({}, max: {})",
                len, MAX_SOUND_EFFECTS
            ),
            Self::CommonAudioDataTooLarge(len) => write!(
                f,
                "common audio data is too large ({} bytes, max: {})",
                len, MAX_COMMON_DATA_SIZE
            ),
        }
    }
}

impl Display for PitchError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SampleRateTooHigh => write!(f, "sample rate too high"),
            Self::SampleRateTooLow => write!(f, "sample rate too low"),
            Self::FirstOctaveGreaterThanLastOctave => {
                write!(f, "first octave must be <= last octave")
            }
            Self::FirstOctaveTooLow(by) => write!(f, "first octave too low (by {} octaves)", by),
            Self::LastOctaveTooHigh(by) => write!(f, "last octave too high (by {} octaves)", by),
            Self::FirstOctaveTooLowLastOctaveTooHigh(fo_by, lo_by) => write!(
                f,
                "first octave too low (by {}) and last octave too high (by {} octaves)",
                fo_by, lo_by
            ),
        }
    }
}

impl Display for PitchTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "error building pitch table")
    }
}

impl Display for IdentifierError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "no identifier"),
            Self::InvalidName(s) => write!(f, "invalid name: {s}"),
            Self::InvalidNumber(s) => write!(f, "identifier is not a number: {s}"),
        }
    }
}

impl Display for MmlLineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ValueError(e) => e.fmt(f),

            Self::MmlTooLarge(len) => writeln!(
                f,
                ": MML file too large ({} bytes, max {})",
                len, MAX_MML_TEXT_LENGTH
            ),
            Self::TooManySubroutines(len) => writeln!(
                f,
                ": too many subroutines ({}, max {})",
                len, MAX_SUBROUTINES
            ),

            Self::NoIdentifier(c) => writeln!(f, "missing identifier after {}", c),
            Self::InvalidIdentifier(e) => e.fmt(f),
            Self::UnknownChannel(name) => writeln!(f, "unknown channels {}", name),
            Self::MissingInstrumentText => writeln!(f, "missing instrument"),
            Self::MissingSubroutineText => writeln!(f, "missing subroutine"),
            Self::MissingChannelText => writeln!(f, "no MML"),
            Self::CannotParseLine => writeln!(f, "cannot parse line"),

            Self::NoHeader => write!(f, "no header name"),
            Self::NoValue => write!(f, "no header value"),
            Self::DuplicateHeader(name) => write!(f, "duplicate header: {}", name),

            Self::InvalidEchoFeedback => write!(f, "invalid echo feedback"),
            Self::InvalidEchoVolume => write!(f, "invalid echo volume"),
            Self::CannotSetTempo => write!(f, "tick clock already set by #Timer"),
            Self::CannotSetTimer => write!(f, "tick clock already set by #Tempo"),

            Self::NoInstrument => write!(f, "no instrument"),
            Self::CannotFindInstrument(name) => write!(f, "cannot find instrument: {}", name),
            Self::ExpectedFourAdsrArguments => write!(f, "adsr requires 4 arguments"),
            Self::InvalidAdsr(e) => e.fmt(f),
            Self::ExpectedOneGainArgument => write!(f, "gain requires 1 argument"),
            Self::InvalidGain(e) => e.fmt(f),
            Self::UnknownInstrumentArgument(s) => write!(f, "unknown instrument argument: {}", s),
            Self::DuplicateInstrumentName(s) => write!(f, "duplicate instrument name: {}", s),
        }
    }
}

impl Display for MmlParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ValueError(e) => e.fmt(f),

            Self::TooManyAccidentals => write!(f, "too many accidentals"),
            Self::TooManyDotsInNoteLength => write!(f, "too many dots in note length"),

            Self::UnknownCharacters(n) => write!(f, "{} unknown characters", n),

            Self::InvalidNote => write!(f, "invalid note"),

            Self::NoSubroutine => write!(f, "no subroutine"),
            Self::NoInstrument => write!(f, "no instrument"),
            Self::CannotCallSubroutineInASubroutine => {
                write!(f, "cannot call subroutine in a subroutine")
            }

            Self::CannotFindSubroutine(name) => write!(f, "cannot find subroutine: {}", name),

            Self::NoStartBrokenChord => write!(f, "not in a broken chord (no `{{{{`)"),
            Self::NoStartPortamento => write!(f, "not in a portamento (no `{{)`"),

            Self::MissingEndBrokenChord => write!(f, "cannot find broken chord end (no `}}}}`)"),
            Self::MissingEndPortamento => write!(f, "cannot find portamento end (no `}}`)"),

            Self::PortamentoRequiresTwoPitches => write!(f, "portamento requires 2 pitches"),
            Self::InvalidPortamentoDelay => {
                write!(f, "portamento delay must be < portamento length")
            }

            Self::MissingNoteBeforeTie => write!(f, "missing note before tie"),
            Self::MissingNoteBeforeSlur => write!(f, "missing note before slur"),
            Self::CannotParseComma => write!(f, "cannot parse comma"),

            Self::InvalidPitchListSymbol => write!(f, "invalid pitch list symbol"),
        }
    }
}

impl Display for MmlCommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BytecodeError(e) => e.fmt(f),
            Self::ValueError(e) => e.fmt(f),

            Self::NoteOutOfRange(n, min, max) => {
                write!(
                    f,
                    "note out of range ({}, {} - {})",
                    n.note_id(),
                    min.note_id(),
                    max.note_id()
                )
            }
            Self::CannotPlayNoteBeforeSettingInstrument => {
                write!(f, "cannot play note before setting an instrument")
            }

            Self::LoopPointAlreadySet => write!(f, "loop point already set"),
            Self::CannotFindSubroutine => write!(f, "cannot find subroutine"),
            Self::CannotFindInstrument => write!(f, "cannot find instrument"),
            Self::CannotCallSubroutineTooManyNestedLoops(name, loops) => {
                write!(
                    f,
                    "cannot call subroutine {}, too many nested loops ({} required, max {})",
                    name.as_str(),
                    loops,
                    MAX_NESTED_LOOPS
                )
            }
            Self::CannotUseMpWithoutInstrument => {
                write!(f, "cannot use MP vibrato without setting an instrument")
            }
            Self::MpPitchOffsetTooLarge(po) => {
                write!(
                    f,
                    "cannot MP vibrato note.  Pitch offset per tick too large ({}, max: {})",
                    po,
                    PitchOffsetPerTick::MAX
                )
            }
            Self::MpDepthZero => write!(f, "MP vibrato depth cannot be 0"),

            Self::PortamentoDelayTooLong => write!(f, "portamento delay too long"),
            Self::PortamentoRequiresInstrument => {
                write!(
                    f,
                    "cannot calculate portamento velocity without setting instrument"
                )
            }

            Self::NoNotesInBrokenChord => write!(f, "no notes in broken chord"),
            Self::TooManyNotesInBrokenChord(len) => {
                write!(
                    f,
                    "too many notes in broken chord ({}, max {})",
                    len, MAX_BROKEN_CHORD_NOTES
                )
            }
            Self::BrokenChordTotalLengthTooShort => {
                write!(
                    f,
                    "broken chord total length too short (a minimum of {} loops are required)",
                    LoopCount::MIN
                )
            }

            Self::BrokenChordTickCountMismatch(tc1, tc2) => {
                write!(
                    f,
                    "broken chord tick count mismatch ({} != {})",
                    tc1.value(),
                    tc2.value()
                )
            }

            Self::NoTicksAfterLoopPoint => write!(f, "no notes or rests after loop point"),
        }
    }
}

impl Display for MmlCompileErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} errors compiling mml", self.n_errors())
    }
}

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NoMusicChannels => write!(f, "no music channels"),
            Self::InvalidMmlData => write!(f, "invalid MmlData"),
            Self::SongIsTooLarge(len) => {
                write!(
                    f,
                    "song is too large ({} bytes, max {})",
                    len, MAX_SONG_DATA_SIZE
                )
            }
            Self::InvalidHeaderSize => write!(f, "invalid header size"),

            Self::DataSizeMismatch(len1, len2) => {
                write!(f, "data size mismatch ({} != {})", len1, len2)
            }
        }
    }
}

impl Display for ExportSpcFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SongError(e) => e.fmt(f),
            Self::TooMuchData { common, song, echo } => {
                write!(f, "cannot fit data in audio-ram (driver: {} bytes, common_audio_data: {} bytes, song data: {} bytes, echo buffer: {} bytes", COMMON_DATA_ADDR, common, song, echo)
            }
        }
    }
}

// Indented Multiline Display
// ==========================

pub struct ProjectFileErrorsIndentedDisplay<'a>(&'a ProjectFileErrors);

impl Display for ProjectFileErrorsIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let errors = &self.0 .0;

        if errors.len() == 1 {
            writeln!(f, "1 error in project file")?;
        } else {
            writeln!(f, "{} errors in project file", errors.len())?;
        }

        for e in errors {
            writeln!(f, "  {}", e)?;
        }

        Ok(())
    }
}

impl ProjectFileErrors {
    pub fn multiline_display(&self) -> ProjectFileErrorsIndentedDisplay {
        ProjectFileErrorsIndentedDisplay(self)
    }
}

pub struct SoundEffectsFileErrorIndentedDisplay<'a>(&'a SoundEffectsFileError, &'a str);

impl SoundEffectsFileError {
    pub fn multiline_display<'a>(
        &'a self,
        file_name: &'a str,
    ) -> SoundEffectsFileErrorIndentedDisplay {
        SoundEffectsFileErrorIndentedDisplay(self, file_name)
    }
}

#[rustfmt::skip::macros(writeln)]
impl Display for SoundEffectsFileErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;
        let file_name = self.1;

        match error {
            SoundEffectsFileError::SoundEffectErrors(errors) => {
                for (i, e) in errors.iter().enumerate() {
                    if i != 0 {
                        writeln!(f)?;
                    }
                    fmt_indented_sound_effect_error(f, e, file_name)?;
                }
            }
            SoundEffectsFileError::DuplicateSfxNamesInSfxFile(errors) => {
                writeln!(f, "Error compiling sound effects")?;

                for (line_no, name) in errors {
                    writeln!(f, "  {}:{} duplicate name: {}", file_name, line_no, name)?;
                }
            }
            SoundEffectsFileError::MissingSoundEffects(names) => {
                writeln!(f, "Error compiling sound effects")?;

                if names.len() == 1 {
                    writeln!(f, "  missing 1 sound effect: {}", names[0])?;
                } else {
                    writeln!(f, "  missing {} sound effects: {}", names.len(), names.join(", "))?;
                }
            }
        }

        Ok(())
    }
}

#[rustfmt::skip::macros(writeln)]
fn fmt_indented_sound_effect_error(
    f: &mut std::fmt::Formatter,
    error: &SoundEffectError,
    file_name: &str,
) -> std::fmt::Result {
    let line_no = error.sfx_line_no;

    if !error.invalid_name {
        writeln!(f, "Error compiling {} sound effect", error.sfx_name)?;
    } else {
        writeln!(f, "Error compiling sound effect")?;
        writeln!(f, "  {}:{}: invalid name", file_name, line_no)?;
    }

    if error.no_notes {
        writeln!(f, "  {}:{}: no notes in sound effect", file_name, line_no)?;
    }

    for e in &error.errors {
        writeln!(f, "  {}:{}: {}", file_name, line_no + e.0, e.1)?;
    }

    Ok(())
}

pub struct SampleAndInstrumentDataErrorIndentedDisplay<'a>(&'a SampleAndInstrumentDataError);

impl Display for SampleAndInstrumentDataErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;

        writeln!(f, "Error compiling samples")?;

        for e in &error.sample_errors {
            match e {
                TaggedSampleError::Instrument(i, n, e) => {
                    writeln!(f, "  Instrument {} {}: {}", i, n, e)?
                }
            }
        }

        if let Some(e) = &error.pitch_table_error {
            e.multiline_display().fmt(f)?;
        }

        Ok(())
    }
}

impl SampleAndInstrumentDataError {
    pub fn multiline_display(&self) -> SampleAndInstrumentDataErrorIndentedDisplay {
        SampleAndInstrumentDataErrorIndentedDisplay(self)
    }
}

pub struct PitchTableErrorIndentedDisplay<'a>(&'a PitchTableError);

impl Display for PitchTableErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Cannot build pitch table")?;

        match self.0 {
            PitchTableError::TooManyInstruments => writeln!(f, "  too many instruments"),
            PitchTableError::TooManyPitches(len) => {
                writeln!(f, "  too many pitches ({}, max {})", len, PITCH_TABLE_SIZE)
            }
            PitchTableError::InstrumentErrors(errors) => {
                for (i, name, e) in errors {
                    writeln!(f, "  Instrument {} {}: {}", i, name, e)?;
                }
                Ok(())
            }
        }
    }
}

impl PitchTableError {
    pub fn multiline_display(&self) -> PitchTableErrorIndentedDisplay {
        PitchTableErrorIndentedDisplay(self)
    }
}

pub struct CommonAudioDataErrorsMultilineDisplay<'a>(&'a CommonAudioDataErrors);

impl Display for CommonAudioDataErrorsMultilineDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let errors = &self.0.errors;

        if errors.len() == 1 {
            writeln!(f, "Cannot compile common audio data: {}", errors[0])?;
        } else {
            writeln!(f, "Cannot compile common audio data")?;
            for e in errors {
                writeln!(f, "  {}", e)?;
            }
        }

        Ok(())
    }
}

impl CommonAudioDataErrors {
    pub fn multiline_display(&self) -> CommonAudioDataErrorsMultilineDisplay {
        CommonAudioDataErrorsMultilineDisplay(self)
    }
}

pub struct MmlCompileErrorsIndentedDisplay<'a>(&'a MmlCompileErrors, &'a str);

impl MmlCompileErrors {
    pub fn multiline_display<'a>(&'a self, file_name: &'a str) -> MmlCompileErrorsIndentedDisplay {
        MmlCompileErrorsIndentedDisplay(self, file_name)
    }
}

impl Display for MmlCompileErrorsIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;
        let file_name = self.1;

        let n_errors = error.n_errors();
        if n_errors == 1 {
            writeln!(f, "1 error compiling {}", file_name)?;
        } else {
            writeln!(f, "{} errors compiling {}", n_errors, file_name)?;
        }

        for e in &error.line_errors {
            writeln!(f, "  {}:{} {}", file_name, e.0, e.1)?;
        }
        for e in &error.subroutine_errors {
            fmt_indented_channel_errors(f, e, file_name, true)?;
        }
        for e in &error.channel_errors {
            fmt_indented_channel_errors(f, e, file_name, false)?;
        }
        Ok(())
    }
}

#[rustfmt::skip::macros(writeln)]
fn fmt_indented_channel_errors(
    f: &mut std::fmt::Formatter,
    error: &MmlChannelError,
    file_name: &str,
    is_subroutine: bool,
) -> std::fmt::Result {
    let n_errors = error.n_errors();
    if n_errors == 1 {
        write!(f, "  1 error in ")?;
    } else {
        write!(f, "  {} errors in ", n_errors)?;
    }

    if is_subroutine {
        writeln!(f, "subroutine !{}", error.identifier.as_str())?;
    } else {
        writeln!(f, "channel {}", error.identifier.as_str())?;
    }

    for e in &error.parse_errors {
        writeln!(f, "    {}:{}:{}: {}", file_name, e.0.line_number, e.0.line_char, e.1)?;
    }
    for e in &error.command_errors {
        writeln!(f, "    {}:{}:{}: {}", file_name, e.0.line_number, e.0.line_char, e.1)?;
    }

    Ok(())
}
