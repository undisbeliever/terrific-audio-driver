//! A single location for all of the errors in the compilers

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use relative_path::RelativeToError;

use crate::bytecode::{
    BcTicks, BcTicksKeyOff, BcTicksNoKeyOff, EarlyReleaseMinTicks, EarlyReleaseTicks, InstrumentId,
    LoopCount, Pan, PortamentoVelocity, RelativePan, RelativeVolume, VibratoPitchOffsetPerTick,
    VibratoQuarterWavelengthInTicks, Volume,
};
use crate::channel_bc_generator::{
    FineQuantization, PortamentoSpeed, Quantization, MAX_BROKEN_CHORD_NOTES,
};
use crate::data::{LoopSetting, Name};
use crate::driver_constants::{
    addresses, BC_CHANNEL_STACK_SIZE, ECHO_BUFFER_EDL_MS, FIR_FILTER_SIZE, MAX_COMMON_DATA_SIZE,
    MAX_DIR_ITEMS, MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_PITCHES, MAX_N_SONGS, MAX_SONG_DATA_SIZE,
    MAX_SOUND_EFFECTS, MAX_SUBROUTINES,
};
use crate::echo::{EchoEdl, EchoLength, MAX_FIR_ABS_SUM};
use crate::envelope::Gain;
use crate::file_pos::{FilePosRange, MAX_MML_TEXT_LENGTH};
use crate::mml::command_parser::{Transpose, MAX_COARSE_VOLUME, PX_PAN_RANGE};
use crate::notes::{MidiNote, Note, Octave};
use crate::path::PathString;
use crate::sound_effects::MAX_SFX_TICKS;
use crate::time::{Bpm, TickClock, TickCounter, ZenLen};
use crate::value_newtypes::{SignedValueNewType, UnsignedValueNewType};
use crate::{export, mml, spc_file_export};

use std::fmt::Display;
use std::io;
use std::ops::RangeInclusive;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ErrorWithPos<T>(pub FilePosRange, pub T);

#[derive(Debug)]
pub enum FileError {
    OpenError(String, io::Error),
    ReadError(String, io::Error),
    FileTooLarge(String),
    Utf8Error(String),
    InvalidAsciiControlCharacter(String),
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
    Sample(UniqueNameListError),
    SoundEffect(UniqueNameListError),
    Song(UniqueNameListError),
    InstrumentOrSample(UniqueNameListError),
}

#[derive(Debug)]
pub struct ProjectFileErrors(pub Vec<ProjectFileError>);

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidAdsrError {
    pub valid_a: bool,
    pub valid_d: bool,
    pub valid_sl: bool,
    pub valid_sr: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueError {
    CannotParseUnsigned(String),
    CannotParseSigned(String),
    CannotParseHex(String),

    InvalidName(String),

    NoEnvelope,
    UnknownEnvelopeType(String),
    AdsrNotFourValues,
    InvalidAdsr(InvalidAdsrError),

    InvalidGainString(String),
    GainOutOfRange,
    FixedGainOutOfRange,
    GainRateOutOfRange,
    F0TempGain,
    OptionalGainCannotBeZero,

    InvalidNote,
    NoNoteOctave,
    UnknownNotePitch(char),
    InvalidPitch,
    UnknownNoteCharacter(char),

    InstrumentIdOutOfRange(u32),

    OctaveOutOfRange(u32),

    // bytecode tick-count arguments out of range
    BcTicksKeyOffOutOfRange(u32),
    BcTicksNoKeyOffOutOfRange(u32),

    PxPanOutOfRange(i32),
    PanOutOfRange(u32),
    VolumeOutOfRange(u32),
    CoarseVolumeOutOfRange(u32),

    RelativePanOutOfRange(i32),
    RelativeVolumeOutOfRange(i32),

    EarlyReleaseTicksOutOfRange(u32),
    EarlyReleaseMinTicksOutOfRange(u32),
    VibratoPitchOffsetPerTickOutOfRange(u32),
    VibratoQuarterWavelengthOutOfRange(u32),

    NoPxPanSign,
    NoRelativeVolumeSign,
    NoRelativePanSign,
    NoDirectionInPortamentoVelocity,
    NoTransposeSign,

    PortamentoVelocityZero,
    PortamentoVelocityOutOfRange(i32),

    QuantizeOutOfRange(u32),
    FineQuantizeOutOfRange(u32),
    TransposeOutOfRange(i32),
    PortamentoSpeedOutOfRange(u32),

    ZenLenOutOfRange(u32),
    TickClockOutOfRange(u32),
    BpmOutOfRange(u32),

    MidiNoteNumberOutOfRange(u32),
    CannotConvertMidiNote,

    InvalidMmlBool,

    NotEnoughLoops,
    TooManyLoops,

    MissingNoteLengthTickCount,
    InvalidNoteLength,
    DotsNotAllowedAfterClockValue,
    InvalidDefaultLength,
    MissingDefaultLength,

    CannotConvertBpmToTickClock,

    EchoEdlOutOfRange(u32),
    EchoLengthNotMultiple,
    EchoBufferTooLarge,

    InvalidFirFilterSize,
    InvalidFirFilter,
    InvalidFirFilterGain { abs_sum: i32 },

    NoHexDigits,
    NoBool,
    NoNote,
    NoVolume,
    NoPan,
    NoRelativeVolume,
    NoRelativePan,
    NoOctave,
    NoZenLen,
    NoTranspose,
    NoLoopCount,
    NoQuantize,
    NoFineQuantizate,
    NoTickClock,
    NoBpm,
    NoMidiNote,
    NoPortamentoSpeed,
    NoPortamentoVelocity,
    NoMpDepth,
    NoVibratoPitchOffsetPerTick,
    NoCommaQuarterWavelength,
    NoVibratoQuarterWavelength,
    NoEchoEdl,
    NoInstrumentId,
    NoGain,
    NoOptionalGainMode,
    NoOptionalGainValue,
    NoEarlyReleaseTicks,
    NoEarlyReleaseMinTicks,
    NoEarlyReleaseMinTicksOrGain,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeError {
    OpenLoopStack(usize),
    NotInALoop,
    MissingLoopCount,
    CannotHaveLoopCountAtStartAndEndLoop,
    MultipleSkipLastLoopInstructions,
    NoTicksBeforeSkipLastLoop,
    NoTicksAfterSkipLastLoop,
    NoTicksInLoop,
    SkipLastLoopOutOfBounds(usize),

    StackOverflowInStartLoop(u32),
    StackOverflowInSubroutineCall(String, u32),

    UnknownInstrument(String),
    InvalidInstrumentId,
    UnknownSubroutine(String),
    NotAllowedToCallSubroutine,

    SubroutineCallInSoundEffect,
    ReturnInNonSubroutine,

    CannotChangeTickClockInASoundEffect,

    GotoRelativeOutOfBounds,

    NoteOutOfRange(Note, RangeInclusive<Note>),
    CannotPlayNoteBeforeSettingInstrument,

    MissingEndLoopInAsmBlock,
    CannotModifyLoopOutsideAsmBlock,
    MissingStartLoopInAsmBlock,
}

#[derive(Debug)]
pub enum SoundEffectErrorList {
    BytecodeErrors(Vec<ErrorWithPos<ChannelError>>),
    MmlLineErrors(Vec<ErrorWithPos<MmlLineError>>),
    MmlErrors(Vec<ErrorWithPos<ChannelError>>),
}

#[derive(Debug)]
pub enum OtherSfxError {
    InvalidName(String),
    DuplicateName(Name),
}

#[derive(Debug)]
pub struct SoundEffectError {
    pub sfx_name: String,
    pub sfx_line_no: u32,

    pub other_errors: Vec<OtherSfxError>,
    pub errors: SoundEffectErrorList,
}

#[derive(Debug)]
pub struct SoundEffectsFileError {
    pub path: Option<PathBuf>,
    pub file_name: String,

    pub errors: Vec<SoundEffectError>,
}

#[derive(Debug)]
pub enum CombineSoundEffectsError {
    // Using String so they can be joined with slice::join
    NoSoundEffectFiles,
    MissingSoundEffects(Vec<String>),
    InvalidNumberOfHighPrioritySfx,
    InvalidLowPriorityIndex,
}

// BrrError is cloneable as the sample file cache caches errors.
// `io::Error` is not cloneable, requiring to enclose `IoError` and `WaveFileError` inside an Arc.
#[derive(Debug, Clone)]
pub enum BrrError {
    IoError(std::sync::Arc<(PathString, io::Error)>),
    WaveFileError(std::sync::Arc<(PathString, brr::WavError)>),

    UnknownFileType(PathString),
    BrrEncodeError(PathString, brr::EncodeError),
    BrrParseError(PathString, brr::ParseError),
    FileTooLarge(PathString),

    InvalidLoopSettingWav(LoopSetting),
    InvalidLoopSettingBrr(LoopSetting),
}

#[derive(Debug)]
pub struct SampleError {
    pub brr_error: Option<BrrError>,
    pub pitch_error: Option<PitchError>,
}

#[derive(Debug)]
pub enum TaggedSampleError {
    Instrument(usize, Name, SampleError),
    Sample(usize, Name, SampleError),
}

#[derive(Debug)]
pub struct SampleAndInstrumentDataError {
    pub sample_errors: Vec<TaggedSampleError>,
    pub pitch_table_error: Option<PitchTableError>,
}

#[derive(Debug)]
pub enum CommonAudioDataError {
    TooManyInstrumentsAndSamples(usize),
    TooManyBrrSamples(usize),
    TooManySoundEffects(usize),
    CommonAudioDataTooLarge(usize),
    SoundEffectDataTooLarge { by: usize },
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

    NoSampleRatesInSample,
    InvalidSampleRates(Vec<u32>),
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
    CannotParseLine,

    // MML Header errors
    NoHeader,
    NoValue,
    UnexpectedHeaderValue,
    UnknownHeader(String),
    DuplicateHeader(String),

    InvalidEchoFeedback,
    InvalidEchoVolume,
    CannotSetTempo,
    CannotSetTimer,
    InvalidSpcSongLength,
    InvalidSpcFadeout,

    // Instrument errors
    NoInstrument,
    CannotFindInstrument(String),
    DuplicateInstrumentName(String),

    // Sound effect errors
    HeaderInSoundEffect,
    SubroutineInSoundEffect,
    InvalidSoundEffectChannel,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelError {
    ValueError(ValueError),

    BytecodeError(BytecodeError),

    // Number of unknown characters
    UnknownCharacters(u32),
    NoSlashCommand,
    InvalidSlashCommand(String),

    NoBraceAfterAsm,
    MissingEndAsm,

    NoteIsTooShort,

    NoSubroutine,
    NoInstrument,
    CannotFindSubroutine(String),
    CannotFindInstrument(String),

    NoStartBrokenChord,
    NoStartPortamento,

    MissingEndBrokenChord,
    MissingEndPortamento,

    PortamentoRequiresTwoPitches,
    InvalidPortamentoDelay,

    MissingNoteBeforeTie,
    MissingNoteBeforeSlur,
    CannotParseComma,
    CannotParseDot,
    CannotParsePercentSign,
    CannotParseGainMode,
    UnexpectedNumber,

    InvalidPitchListSymbol,

    LoopPointAlreadySet,
    CannotSetLoopPoint,
    CannotSetLoopPointInALoop,
    CannotUseMpWithoutInstrument,
    MpPitchOffsetTooLarge(u32),
    MpDepthZero,

    PortamentoTooShort,
    PortamentoTooLong,
    PortamentoRequiresInstrument,

    NoNotesInBrokenChord,
    TooManyNotesInBrokenChord(usize),
    BrokenChordTotalLengthTooShort,

    BrokenChordTickCountMismatch(TickCounter, TickCounter),

    NoTicksAfterLoopPoint,

    CannotCallSubroutineInASoundEffect,
    CannotCallSubroutineRecursion(String),

    TooManySfxTicks(TickCounter),

    // Bytecode assembler errors
    UnknownInstruction(String),
    InvalidNumberOfArguments(u8),
    InvalidNumberOfArgumentsRange(u8, u8),
    InvalidKeyoffArgument(String),
    NoTicksInSoundEffect,
}

#[derive(Debug)]
pub struct MmlChannelError {
    pub identifier: mml::IdentifierBuf,
    pub errors: Vec<ErrorWithPos<ChannelError>>,
}

#[derive(Debug)]
pub struct MmlCompileErrors {
    pub song_name: Option<Name>,
    pub file_name: String,

    pub line_errors: Vec<ErrorWithPos<MmlLineError>>,
    pub subroutine_errors: Vec<MmlChannelError>,
    pub channel_errors: Vec<MmlChannelError>,
}

#[derive(Debug)]
pub enum SongError {
    MmlError(MmlCompileErrors),

    NoMusicChannels,
    InvalidMmlData,
    SongIsTooLarge(usize),

    InvalidHeaderSize,

    // This should not happen
    DataSizeMismatch(usize, usize),
}

#[derive(Debug)]
pub struct SongTooLargeError {
    pub too_large_by: usize,
    pub common_data_size: usize,
    pub song_data_size: usize,
    pub echo_buffer_size: usize,
}

#[derive(Debug)]
pub enum ExportSpcFileError {
    TooMuchData {
        common: usize,
        song: usize,
        echo: usize,
    },
}

#[derive(Debug)]
pub enum ExportError {
    InvalidSegmentName(String),
    NoSegmentNumberSuffix(String),
    PvSnesLibInvalidFirstBank,
    BinPathNotUtf8,
    BinPathContainsQuotes,
    BinPathRelativeToError(RelativeToError),
    NoBinPathParent,
    TooManySongs(usize),
    BinFileTooLarge(usize),
}

// From Traits
// ===========

impl From<InvalidAdsrError> for ValueError {
    fn from(e: InvalidAdsrError) -> Self {
        Self::InvalidAdsr(e)
    }
}

impl From<ValueError> for MmlLineError {
    fn from(e: ValueError) -> Self {
        Self::ValueError(e)
    }
}

impl From<InvalidAdsrError> for ChannelError {
    fn from(e: InvalidAdsrError) -> Self {
        Self::ValueError(ValueError::InvalidAdsr(e))
    }
}

impl From<ValueError> for ChannelError {
    fn from(e: ValueError) -> Self {
        Self::ValueError(e)
    }
}

impl From<BytecodeError> for ChannelError {
    fn from(e: BytecodeError) -> Self {
        Self::BytecodeError(e)
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
            Self::InvalidAsciiControlCharacter(fname) => write!(
                f,
                "unable to read {}: file contains invalid ASCII control characters",
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
            Self::Sample(e) => fmt_unique_name_list_error(f, e, "sample"),
            Self::InstrumentOrSample(e) => fmt_unique_name_list_error(f, e, "instrument or sample"),
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

        if !self.valid_a {
            write!(f, " attack")?;
        }
        if !self.valid_d {
            write!(f, " decay")?;
        }
        if !self.valid_sl {
            write!(f, " sustain-level")?;
        }
        if !self.valid_sr {
            write!(f, " sustain-rate")?;
        }

        write!(f, " values")
    }
}

impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        #[rustfmt::skip]
        macro_rules! out_of_range {
            ($name:literal, $v:ident, $newtype:ident) => {
                write!(f, concat!($name, " out of bounds ({}, expected {} - {})"), $v, $newtype::MIN.value(), $newtype::MAX.value())
            };
        }

        match self {
            Self::CannotParseUnsigned(s) => write!(f, "cannot parse unsigned number: {}", s),
            Self::CannotParseSigned(s) => write!(f, "cannot parse signed number: {}", s),
            Self::CannotParseHex(s) => write!(f, "cannot parse hexidecimal number: {}", s),

            Self::InvalidName(s) => write!(f, "invalid name: {}", s),

            Self::NoEnvelope => write!(f, "no envelope"),
            Self::UnknownEnvelopeType(s) => write!(f, "unknown envelope type: {}", s),
            Self::AdsrNotFourValues => write!(f, "expected 4 adsr values"),
            Self::InvalidAdsr(e) => e.fmt(f),

            Self::InvalidGainString(e) => e.fmt(f),
            Self::GainOutOfRange => {
                write!(
                    f,
                    "invalid gain (expected {} - {})",
                    Gain::MIN.value(),
                    Gain::MAX.value()
                )
            }
            Self::FixedGainOutOfRange => {
                write!(
                    f,
                    "invalid fixed gain value (expected {} - {})",
                    0,
                    Gain::FIXED_GAIN_MASK
                )
            }
            Self::GainRateOutOfRange => {
                write!(
                    f,
                    "invalid gain rate (expected {} - {})",
                    0,
                    Gain::RATE_MASK
                )
            }
            Self::F0TempGain => {
                write!(f, "temp-GAIN cannot be F0")
            }
            Self::OptionalGainCannotBeZero => {
                write!(f, "optional GAIN cannot be 0 or F0")
            }

            Self::InvalidNote => write!(f, "invalid note"),
            Self::NoNoteOctave => write!(f, "no octave in note"),
            Self::UnknownNotePitch(c) => write!(f, "invalid note pitch: {}", c),
            Self::InvalidPitch => write!(f, "invalid note pitch"),
            Self::UnknownNoteCharacter(c) => write!(f, "invalid note character: {}", c),

            Self::InstrumentIdOutOfRange(v) => out_of_range!("instrument id", v, InstrumentId),

            Self::OctaveOutOfRange(v) => out_of_range!("octave", v, Octave),

            Self::BcTicksKeyOffOutOfRange(v) => write!(
                f,
                "invalid tick count ({}, expected {} - {})",
                v,
                BcTicksKeyOff::MIN_TICKS,
                BcTicksKeyOff::MAX_TICKS
            ),
            Self::BcTicksNoKeyOffOutOfRange(v) => write!(
                f,
                "invalid tick count ({}, expected {} - {})",
                v,
                BcTicksNoKeyOff::MIN_TICKS,
                BcTicksNoKeyOff::MAX_TICKS
            ),

            Self::PxPanOutOfRange(v) => write!(
                f,
                "px pan out of bounds ({}, expected {} - {})",
                v,
                PX_PAN_RANGE.start(),
                PX_PAN_RANGE.end()
            ),
            Self::PanOutOfRange(v) => out_of_range!("pan", v, Pan),
            Self::VolumeOutOfRange(v) => out_of_range!("volume", v, Volume),
            Self::CoarseVolumeOutOfRange(v) => {
                write!(
                    f,
                    "volume out of range ({}, expected {} - {})",
                    v, 0, MAX_COARSE_VOLUME
                )
            }

            Self::RelativePanOutOfRange(v) => out_of_range!("relative pan", v, RelativePan),
            Self::RelativeVolumeOutOfRange(v) => {
                out_of_range!("relative volume", v, RelativeVolume)
            }

            Self::EarlyReleaseTicksOutOfRange(v) => {
                out_of_range!("early-release ticks", v, EarlyReleaseTicks)
            }
            Self::EarlyReleaseMinTicksOutOfRange(v) => {
                out_of_range!("early-release minimum ticks", v, EarlyReleaseMinTicks)
            }
            Self::VibratoPitchOffsetPerTickOutOfRange(v) => {
                out_of_range!(
                    "vibrato pitch offset per tick",
                    v,
                    VibratoPitchOffsetPerTick
                )
            }
            Self::VibratoQuarterWavelengthOutOfRange(v) => {
                out_of_range!(
                    "vibrato quarter wavelength",
                    v,
                    VibratoQuarterWavelengthInTicks
                )
            }

            Self::NoPxPanSign => {
                write!(f, "missing + or - in px pan")
            }
            Self::NoRelativeVolumeSign => {
                write!(f, "missing + or - in relative volume")
            }
            Self::NoRelativePanSign => {
                write!(f, "missing + or - in relative pan")
            }
            Self::NoDirectionInPortamentoVelocity => {
                write!(f, "missing + or - in portamento velocity")
            }
            Self::NoTransposeSign => {
                write!(f, "missing + or - in transpose value")
            }

            Self::PortamentoVelocityZero => write!(f, "portamento velocity cannot be 0"),
            Self::PortamentoVelocityOutOfRange(v) => {
                out_of_range!("portamento velocity", v, PortamentoVelocity)
            }

            Self::QuantizeOutOfRange(v) => out_of_range!("quantization", v, Quantization),
            Self::FineQuantizeOutOfRange(v) => {
                out_of_range!("fine quantization", v, FineQuantization)
            }
            Self::TransposeOutOfRange(v) => out_of_range!("transpose", v, Transpose),

            Self::PortamentoSpeedOutOfRange(v) => {
                out_of_range!("portamento speed", v, PortamentoSpeed)
            }

            Self::ZenLenOutOfRange(v) => out_of_range!("zenlen", v, ZenLen),
            Self::TickClockOutOfRange(v) => out_of_range!("tick clock", v, TickClock),
            Self::BpmOutOfRange(v) => out_of_range!("bpm", v, Bpm),

            Self::MidiNoteNumberOutOfRange(v) => out_of_range!("MIDI note number", v, MidiNote),
            Self::CannotConvertMidiNote => {
                write!(f, "cannot convert MIDI note to audio-driver note")
            }

            Self::InvalidMmlBool => write!(f, "cannot parse bool, expected a 0 or a 1"),

            Self::NotEnoughLoops => write!(f, "not enough loops (min: {})", LoopCount::MIN_LOOPS),
            Self::TooManyLoops => write!(f, "too many loops (max: {})", LoopCount::MAX_LOOPS),

            Self::MissingNoteLengthTickCount => write!(f, "missing tick count in note length"),
            Self::InvalidNoteLength => write!(f, "invalid note length"),
            Self::DotsNotAllowedAfterClockValue => {
                write!(f, "dots not allowed after a tick count length")
            }
            Self::InvalidDefaultLength => write!(f, "invalid length"),
            Self::MissingDefaultLength => write!(f, "missing length"),

            Self::CannotConvertBpmToTickClock => write!(f, "cannot convert bpm to tick clock"),

            Self::EchoEdlOutOfRange(v) => out_of_range!("echo EDL", v, EchoEdl),
            Self::EchoLengthNotMultiple => write!(
                f,
                "echo length is not a multiple of {}ms",
                ECHO_BUFFER_EDL_MS
            ),
            Self::EchoBufferTooLarge => {
                write!(
                    f,
                    "echo buffer too large (max {}ms)",
                    EchoLength::MAX.value()
                )
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
            Self::InvalidFirFilterGain { abs_sum } => write!(
                f,
                "invalid FIR filter gain (absolute sum is {}, max; {})",
                abs_sum, MAX_FIR_ABS_SUM,
            ),

            Self::NoHexDigits => write!(f, "no hexidecimal digits"),
            Self::NoBool => write!(f, "no bool"),
            Self::NoNote => write!(f, "no note"),
            Self::NoVolume => write!(f, "no volume"),
            Self::NoPan => write!(f, "no pan"),
            Self::NoRelativeVolume => write!(f, "no relative volume"),
            Self::NoRelativePan => write!(f, "no relative pan"),
            Self::NoOctave => write!(f, "no octave"),
            Self::NoZenLen => write!(f, "no zenlen value"),
            Self::NoTranspose => write!(f, "no transpose value"),
            Self::NoLoopCount => write!(f, "no loop count"),
            Self::NoQuantize => write!(f, "no quantization value"),
            Self::NoFineQuantizate => write!(f, "no fine quntization value"),
            Self::NoTickClock => write!(f, "no tick clock"),
            Self::NoBpm => write!(f, "no tempo bpm"),
            Self::NoMidiNote => write!(f, "no MIDI note number"),
            Self::NoPortamentoSpeed => write!(f, "no portamento speed"),
            Self::NoPortamentoVelocity => write!(f, "no portamento velocity"),
            Self::NoMpDepth => write!(f, "no MP depth"),
            Self::NoVibratoPitchOffsetPerTick => write!(f, "no vibrato pitch-offset-per-tick"),
            Self::NoCommaQuarterWavelength => {
                write!(f, "cannot parse quarter-wavelength, expected a comma ','")
            }
            Self::NoVibratoQuarterWavelength => write!(f, "no vibrato quarter-wavelength"),
            Self::NoEchoEdl => write!(f, "no echo EDL"),
            Self::NoInstrumentId => write!(f, "no instrument id"),
            Self::NoGain => write!(f, "no gain"),
            Self::NoOptionalGainMode => write!(f, "no optional GAIN mode (F, D, E, I, B)"),
            Self::NoOptionalGainValue => write!(f, "no optional GAIN value"),
            Self::NoEarlyReleaseTicks => write!(f, "no early-release ticks"),
            Self::NoEarlyReleaseMinTicks => write!(f, "no early-release minimum ticks"),
            Self::NoEarlyReleaseMinTicksOrGain => {
                write!(f, "no early-release minimum ticks or GAIN (F, D, E, I, B)")
            }
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

            Self::StackOverflowInStartLoop(stack_depth) => {
                write!(
                    f,
                    "too many loops (requires {} bytes of stack, stack is {} bytes)",
                    stack_depth, BC_CHANNEL_STACK_SIZE
                )
            }
            Self::StackOverflowInSubroutineCall(name, stack_depth) => {
                write!(
                    f,
                    "cannot call subroutine {}, stack overflow ({} bytes required, max {})",
                    name.as_str(),
                    stack_depth,
                    BC_CHANNEL_STACK_SIZE
                )
            }

            Self::UnknownInstrument(s) => write!(f, "cannot find instrument: {}", s),
            Self::InvalidInstrumentId => write!(f, "invalid instrument id"),
            Self::UnknownSubroutine(s) => write!(f, "cannot find subroutine: {}", s),
            Self::NotAllowedToCallSubroutine => write!(f, "not allowed to call subroutine here"),

            Self::SubroutineCallInSoundEffect => {
                write!(f, "cannot call subroutine in a sound effect")
            }
            Self::ReturnInNonSubroutine => {
                write!(f, "return_from_subroutine instruction in a non-subroutine")
            }

            Self::CannotChangeTickClockInASoundEffect => {
                write!(f, "cannot change tick clock in a sound effect")
            }

            Self::GotoRelativeOutOfBounds => {
                write!(
                    f,
                    "goto_relative offset out of bounds (bytecode is too large)"
                )
            }

            Self::NoteOutOfRange(n, range) => {
                write!(
                    f,
                    "note out of range ({}, {} - {})",
                    n.note_id(),
                    range.start().note_id(),
                    range.end().note_id(),
                )
            }
            Self::CannotPlayNoteBeforeSettingInstrument => {
                write!(f, "cannot play note before setting an instrument")
            }

            Self::MissingEndLoopInAsmBlock => {
                write!(
                    f,
                    r"missing end_loop in \asm block (loops inside \asm blocks must be self-contained)"
                )
            }
            Self::CannotModifyLoopOutsideAsmBlock => {
                write!(f, r"cannot modify loop outside of an \asm block")
            }
            Self::MissingStartLoopInAsmBlock => {
                write!(
                    f,
                    r"missing start_loop in \asm block (loops inside \asm blocks must be self-contained)"
                )
            }
        }
    }
}

impl Display for OtherSfxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InvalidName(n) => write!(f, "invalid name: {}", n),
            Self::DuplicateName(n) => write!(f, "duplicate name: {}", n),
        }
    }
}

impl Display for SoundEffectError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "error compiling {}", self.sfx_name)
    }
}

impl Display for CombineSoundEffectsError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NoSoundEffectFiles => write!(f, "no sound effect files"),
            Self::MissingSoundEffects(names) => write!(
                f,
                "missing {} sound effects: {}",
                names.len(),
                names.join(", ")
            ),
            Self::InvalidNumberOfHighPrioritySfx => {
                write!(f, "Invalid number of high-priority sound effects")
            }
            Self::InvalidLowPriorityIndex => write!(f, "Invalid low-priority index"),
        }
    }
}

impl Display for BrrError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IoError(arc) => write!(f, "cannot read {}: {}", arc.0, arc.1),
            Self::WaveFileError(arc) => write!(f, "cannot read {}: {}", arc.0, arc.1),

            Self::UnknownFileType(p) => write!(f, "unknown file type: {}", p),
            Self::BrrEncodeError(p, e) => write!(f, "error encoding {}: {}", p, e),
            Self::BrrParseError(p, e) => write!(f, "error loading {}: {}", p, e),
            Self::FileTooLarge(p) => write!(f, "file too large: {}", p),

            Self::InvalidLoopSettingWav(ls) => {
                write!(f, "cannot use {} on wav files", ls.serialier_value())
            }
            Self::InvalidLoopSettingBrr(ls) => {
                write!(f, "cannot use {} on brr files", ls.serialier_value())
            }
        }
    }
}

impl Display for CommonAudioDataError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::TooManyInstrumentsAndSamples(len) => {
                write!(
                    f,
                    "too many instruments and samples ({}, max: {}",
                    len, MAX_INSTRUMENTS_AND_SAMPLES
                )
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
            Self::SoundEffectDataTooLarge { by } => {
                write!(f, "sound effect data is too large (by {} bytes)", by)
            }
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
            Self::NoSampleRatesInSample => write!(f, "no sample rates in sample"),
            Self::InvalidSampleRates(e) => write!(f, "invalid sample rates: {:?}", &e),
        }
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

            Self::MmlTooLarge(len) => write!(
                f,
                ": MML file too large ({} bytes, max {})",
                len, MAX_MML_TEXT_LENGTH
            ),
            Self::TooManySubroutines(len) => write!(
                f,
                ": too many subroutines ({}, max {})",
                len, MAX_SUBROUTINES
            ),

            Self::NoIdentifier(c) => write!(f, "missing identifier after {}", c),
            Self::InvalidIdentifier(e) => e.fmt(f),
            Self::UnknownChannel(name) => write!(f, "unknown channels {}", name),
            Self::CannotParseLine => write!(f, "cannot parse line"),

            Self::NoHeader => write!(f, "no header name"),
            Self::NoValue => write!(f, "no header value"),
            Self::UnexpectedHeaderValue => write!(f, "unexpected header value"),
            Self::UnknownHeader(name) => write!(f, "unknown header: {}", name),
            Self::DuplicateHeader(name) => write!(f, "duplicate header: {}", name),

            Self::InvalidEchoFeedback => write!(f, "invalid echo feedback"),
            Self::InvalidEchoVolume => write!(f, "invalid echo volume"),
            Self::CannotSetTempo => write!(f, "tick clock already set by #Timer"),
            Self::CannotSetTimer => write!(f, "tick clock already set by #Tempo"),
            Self::InvalidSpcSongLength => write!(
                f,
                "invalid spc export song length (expected 0 - {})",
                spc_file_export::MAX_SONG_LENGTH
            ),
            Self::InvalidSpcFadeout => write!(
                f,
                "invalid spc export fadeout (expected 0 - {})",
                spc_file_export::MAX_FADEOUT_MILLIS
            ),

            Self::NoInstrument => write!(f, "no instrument"),
            Self::CannotFindInstrument(name) => write!(f, "cannot find instrument: {}", name),
            Self::DuplicateInstrumentName(s) => write!(f, "duplicate instrument name: {}", s),

            Self::HeaderInSoundEffect => write!(f, "# headers not allowed in sound effects"),
            Self::SubroutineInSoundEffect => write!(f, "subroutines not allowed in sound effects"),
            Self::InvalidSoundEffectChannel => write!(
                f,
                "invalid channel (sound effects have only 1 single channel)"
            ),
        }
    }
}

impl Display for ChannelError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ValueError(e) => e.fmt(f),
            Self::BytecodeError(e) => e.fmt(f),

            Self::UnknownCharacters(n) => write!(f, "{} unknown characters", n),
            Self::NoSlashCommand => write!(f, "invalid command: \\"),
            Self::InvalidSlashCommand(t) => write!(f, "invalid command: \\{t}"),

            Self::NoBraceAfterAsm => write!(f, r"missing `{{` brace after \asm"),
            Self::MissingEndAsm => write!(f, r"cannot find \asm end (no `}}`)"),

            Self::NoteIsTooShort => write!(
                f,
                "note is too short (2 ticks are required for a key-off note)"
            ),

            Self::NoSubroutine => write!(f, "no subroutine"),
            Self::NoInstrument => write!(f, "no instrument"),

            Self::CannotFindSubroutine(name) => write!(f, "cannot find subroutine: !{}", name),
            Self::CannotFindInstrument(name) => write!(f, "cannot find instrument: {}", name),

            Self::CannotCallSubroutineInASoundEffect => {
                write!(f, "cannot call subroutine in a sound effect")
            }
            Self::CannotCallSubroutineRecursion(id) => {
                write!(f, "cannot call subroutine !{id}: recursion detected")
            }

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
            Self::CannotParseDot => write!(f, "cannot parse dot"),
            Self::CannotParsePercentSign => write!(f, "cannot parse %"),
            Self::CannotParseGainMode => {
                write!(f, "cannot parse GAIN mode (used by Q and q commands)")
            }
            Self::UnexpectedNumber => write!(f, "unexpected number"),

            Self::InvalidPitchListSymbol => write!(f, "invalid pitch list symbol"),

            Self::LoopPointAlreadySet => write!(f, "loop point already set"),
            Self::CannotSetLoopPoint => write!(f, "cannot set loop point"),
            Self::CannotSetLoopPointInALoop => write!(f, "cannot set loop point in a loop"),
            Self::CannotUseMpWithoutInstrument => {
                write!(f, "cannot use MP vibrato without setting an instrument")
            }
            Self::MpPitchOffsetTooLarge(po) => {
                write!(
                    f,
                    "cannot MP vibrato note.  Pitch offset per tick too large ({}, max: {})",
                    po,
                    VibratoPitchOffsetPerTick::MAX.value()
                )
            }
            Self::MpDepthZero => write!(f, "MP vibrato depth cannot be 0"),

            Self::PortamentoTooShort => write!(f, "portamento length is too short"),
            Self::PortamentoTooLong => write!(f, "portamento length is too long"),
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
                    LoopCount::MIN_LOOPS
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

            Self::TooManySfxTicks(t) => write!(
                f,
                "sound effect too long ({} ticks, max {})",
                t.value(),
                MAX_SFX_TICKS.value()
            ),

            // Bytecode assembler errors
            Self::UnknownInstruction(s) => write!(f, "unknown instruction {}", s),
            Self::InvalidNumberOfArguments(n) => write!(f, "expected {} arguments", n),
            Self::InvalidNumberOfArgumentsRange(min, max) => {
                write!(f, "expected {} - {} arguments", min, max)
            }
            Self::InvalidKeyoffArgument(s) => write!(f, "invalid keyoff argument: {}", s),
            Self::NoTicksInSoundEffect => write!(f, "No notes in sound effect"),
        }
    }
}

impl Display for SongError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::MmlError(_) => write!(f, "mml error"),
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

impl Display for SongTooLargeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "song too large by {} bytes", self.too_large_by)
    }
}

impl Display for ExportSpcFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::TooMuchData { common, song, echo } => {
                write!(f, "cannot fit data in audio-ram (driver: {} bytes, common_audio_data: {} bytes, song data: {} bytes, echo buffer: {} bytes", addresses::COMMON_DATA, common, song, echo)
            }
        }
    }
}

impl Display for ExportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidSegmentName(name) => write!(f, "invalid segment name: {}", name),
            Self::NoSegmentNumberSuffix(name) => {
                write!(f, "segment name must end with a number: {}", name)
            }
            Self::PvSnesLibInvalidFirstBank => write!(
                f,
                "the first BANK to store the audio data in must be >= {}",
                export::pvsneslib::FIRST_VALID_BANK
            ),
            Self::BinPathNotUtf8 => write!(f, "invalid binary path: not a valid utf-8 string"),
            Self::BinPathContainsQuotes => {
                write!(f, "invalid binary path, it cannot contain \" or \'.")
            }
            Self::BinPathRelativeToError(e) => {
                write!(f, "cannot determine binary include path: {}", e)
            }
            Self::NoBinPathParent => write!(f, "cannot determine the parent of the binary path"),
            Self::TooManySongs(c) => write!(f, "too many songs ({}, max: {})", c, MAX_N_SONGS),
            Self::BinFileTooLarge(c) => write!(
                f,
                "binary file is too large ({} bytes, max: {}",
                c,
                export::MAX_BIN_FILE
            ),
        }
    }
}

// Indented Multiline Display
// ==========================

const SFX_MML_ERROR_LIMIT: usize = 30;

fn plus_more_errors_line(f: &mut std::fmt::Formatter, prefix: &str, n: usize) -> std::fmt::Result {
    if n > SFX_MML_ERROR_LIMIT {
        let n = n - SFX_MML_ERROR_LIMIT;
        if n > 1 {
            writeln!(f, "{}+ {} more errors", prefix, n)
        } else {
            writeln!(f, "{}+ {} more error", prefix, n)
        }
    } else {
        Ok(())
    }
}

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

pub struct SoundEffectsFileErrorIndentedDisplay<'a>(&'a SoundEffectsFileError);

impl SoundEffectsFileError {
    pub fn multiline_display(&self) -> SoundEffectsFileErrorIndentedDisplay {
        SoundEffectsFileErrorIndentedDisplay(self)
    }
}

#[rustfmt::skip::macros(writeln)]
impl Display for SoundEffectsFileErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;

        match &error.path {
            Some(p) => writeln!(f, "Error compiling sound effects file: {}", p.display())?,
            None => writeln!(f, "Error compiling sound effects file")?,
        }

        for (i, e) in error.errors.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            let line_prefix = [&error.file_name, ":"].concat();
            fmt_indented_sound_effect_error(f, e, "  ", &line_prefix)?;
        }

        Ok(())
    }
}

pub struct SoundEffectErrorIndentedDisplay<'a>(&'a SoundEffectError, &'a str);

impl SoundEffectError {
    pub fn multiline_display<'a>(
        &'a self,
        file_name: &'a str,
    ) -> SoundEffectErrorIndentedDisplay<'a> {
        SoundEffectErrorIndentedDisplay(self, file_name)
    }
}

impl Display for SoundEffectErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_indented_sound_effect_error(f, self.0, "Error compiling ", self.1)
    }
}

#[rustfmt::skip::macros(writeln)]
fn fmt_indented_sound_effect_error(
    f: &mut std::fmt::Formatter,
    error: &SoundEffectError,
    name_prefix: &str,
    line_prefix: &str,
) -> std::fmt::Result {
    let line_no = error.sfx_line_no;

    writeln!(f, "{}{}:", name_prefix, error.sfx_name)?;

    for e in &error.other_errors {
        writeln!(f, "    {}{}: {}", line_prefix, line_no, e)?;
    }

    match &error.errors {
        SoundEffectErrorList::BytecodeErrors(errors) => {
            for e in errors.iter().take(SFX_MML_ERROR_LIMIT) {
                writeln!(f, "    {}{}: {}", line_prefix, e.0.line_number + line_no, e.1)?;
            }
            plus_more_errors_line(f, "    ", errors.len())?;
        }
        SoundEffectErrorList::MmlLineErrors(errors) => {
            for e in errors.iter().take(SFX_MML_ERROR_LIMIT) {
                writeln!(f, "    {}{}:{} {}", line_prefix, e.0.line_number + line_no, e.0.line_char, e.1)?;
            }
            plus_more_errors_line(f, "    ", errors.len())?;
        }
        SoundEffectErrorList::MmlErrors(errors) => {
            for e in errors.iter().take(SFX_MML_ERROR_LIMIT) {
                writeln!(f, "    {}{}:{} {}", line_prefix, e.0.line_number + line_no, e.0.line_char, e.1)?;
            }
            plus_more_errors_line(f, "    ", errors.len())?;
        }
    }

    Ok(())
}

pub struct SfxErrorLines {
    pub offset: usize,
    pub lines: Vec<u32>,
}

impl SoundEffectError {
    pub fn error_lines(&self) -> SfxErrorLines {
        SfxErrorLines {
            offset: 1 + self.other_errors.len(),
            lines: match &self.errors {
                SoundEffectErrorList::BytecodeErrors(errors) => {
                    errors.iter().map(|e| e.0.line_number).collect()
                }
                SoundEffectErrorList::MmlLineErrors(errors) => {
                    errors.iter().map(|e| e.0.line_number).collect()
                }
                SoundEffectErrorList::MmlErrors(errors) => {
                    errors.iter().map(|e| e.0.line_number).collect()
                }
            },
        }
    }
}

pub struct SampleAndInstrumentDataErrorIndentedDisplay<'a>(&'a SampleAndInstrumentDataError);

impl Display for SampleAndInstrumentDataErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;

        writeln!(f, "Error compiling samples")?;

        for e in &error.sample_errors {
            match e {
                TaggedSampleError::Instrument(i, n, e) => {
                    if let Some(e) = &e.brr_error {
                        writeln!(f, "  Instrument {} {}: {}", i, n, e)?
                    }
                    if let Some(e) = &e.pitch_error {
                        writeln!(f, "  Instrument {} {}: {}", i, n, e)?
                    }
                }
                TaggedSampleError::Sample(i, n, e) => {
                    if let Some(e) = &e.brr_error {
                        writeln!(f, "  Sample {} {}: {}", i, n, e)?
                    }
                    if let Some(e) = &e.pitch_error {
                        writeln!(f, "  Sample {} {}: {}", i, n, e)?
                    }
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
                writeln!(f, "  too many pitches ({}, max {})", len, MAX_N_PITCHES)
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

pub struct MmlCompileErrorsIndentedDisplay<'a>(&'a MmlCompileErrors);

impl MmlCompileErrors {
    pub fn multiline_display(&self) -> MmlCompileErrorsIndentedDisplay {
        MmlCompileErrorsIndentedDisplay(self)
    }
}

impl Display for MmlCompileErrorsIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let error = self.0;

        match &error.song_name {
            Some(s) => writeln!(f, "Error compiling {}:", s)?,
            None => writeln!(f, "Error compiling {}:", error.file_name)?,
        }

        for e in error.line_errors.iter().take(SFX_MML_ERROR_LIMIT) {
            writeln!(f, "  {}:{} {}", error.file_name, e.0.line_number, e.1)?;
        }
        plus_more_errors_line(f, "  ", error.line_errors.len())?;

        for e in &error.subroutine_errors {
            fmt_indented_channel_errors(f, e, &error.file_name, true)?;
        }
        for e in &error.channel_errors {
            fmt_indented_channel_errors(f, e, &error.file_name, false)?;
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
    let n_errors = error.errors.len();
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

    for e in error.errors.iter().take(SFX_MML_ERROR_LIMIT) {
        writeln!(f, "    {}:{}:{} {}", file_name, e.0.line_number, e.0.line_char, e.1)?;
    }
    plus_more_errors_line(f, "    ", n_errors)?;

    Ok(())
}

pub struct SongTooLargeErrorIndentedDisplay<'a>(&'a SongTooLargeError);

impl Display for SongTooLargeErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let e = &self.0;
        write!(
            f,
            concat![
                "Song is too large by {} bytes!",
                "\n  common data (samples + sfx): {:>6} bytes",
                "\n  song_data: {:>24} bytes",
                "\n  echo buffer: {:>22} bytes"
            ],
            e.too_large_by, e.common_data_size, e.song_data_size, e.echo_buffer_size
        )
    }
}

impl SongError {
    pub fn multiline_display(&self) -> SongErrorIndentedDisplay {
        SongErrorIndentedDisplay(self)
    }
}

impl Display for SongErrorIndentedDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            SongError::MmlError(e) => MmlCompileErrorsIndentedDisplay(e).fmt(f),
            e => e.fmt(f),
        }
    }
}

impl SongTooLargeError {
    pub fn multiline_display(&self) -> SongTooLargeErrorIndentedDisplay {
        SongTooLargeErrorIndentedDisplay(self)
    }
}
pub struct SongErrorIndentedDisplay<'a>(&'a SongError);
