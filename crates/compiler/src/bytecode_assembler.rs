//! Audio driver bytecode assembler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, EarlyReleaseMinTicks, EarlyReleaseTicks, LoopCount,
    PlayNoteTicks, PlayPitchPitch, PortamentoSlideTicks, PortamentoVelocity, RelativeEchoFeedback,
    RelativeFirCoefficient, State, VibratoPitchOffsetPerTick,
};
use crate::data::{InstrumentOrSample, UniqueNamesList};
use crate::driver_constants::FIR_FILTER_SIZE;
use crate::echo::{EchoFeedback, FirCoefficient, FirTap};
use crate::envelope::{Adsr, Gain, OptionalGain, TempGain};
use crate::errors::{BytecodeError, ChannelError, ValueError};
use crate::invert_flags::parse_invert_flag_arguments;
use crate::notes::Note;
use crate::subroutines::SubroutineStore;
use crate::time::TickCounter;
use crate::value_newtypes::{
    parse_i8wh, I8WithByteHexValueNewType, SignedValueNewType, UnsignedValueNewType,
};

pub use crate::bytecode::{BcTerminator, BytecodeContext};

// Some `Bytecode` methods do not return a Result.
// The macro in `BytecodeAssembler::parse_line()` cannot determine the method's value.
// This trait will wrap () into Ok(()) so `BytecodeAssembler::parse_line()` can compile.
trait BytecodeResultWrapper {
    fn wrap(self) -> Result<(), ChannelError>;
}

impl BytecodeResultWrapper for () {
    fn wrap(self) -> Result<(), ChannelError> {
        Ok(())
    }
}

impl BytecodeResultWrapper for Result<(), BytecodeError> {
    fn wrap(self) -> Result<(), ChannelError> {
        match self {
            Ok(()) => Ok(()),
            Err(e) => Err(ChannelError::BytecodeError(e)),
        }
    }
}

fn no_arguments(args: &[&str]) -> Result<(), ChannelError> {
    if args.is_empty() {
        Ok(())
    } else {
        Err(ChannelError::InvalidNumberOfArguments(0))
    }
}

fn one_argument<'a>(args: &[&'a str]) -> Result<&'a str, ChannelError> {
    if args.len() == 1 {
        Ok(args[0])
    } else {
        Err(ChannelError::InvalidNumberOfArguments(1))
    }
}

fn two_arguments<'a>(args: &[&'a str]) -> Result<(&'a str, &'a str), ChannelError> {
    if args.len() == 2 {
        Ok((args[0], args[1]))
    } else {
        Err(ChannelError::InvalidNumberOfArguments(2))
    }
}

#[allow(dead_code)]
fn three_arguments<'a>(args: &[&'a str]) -> Result<(&'a str, &'a str, &'a str), ChannelError> {
    if args.len() == 3 {
        Ok((args[0], args[1], args[2]))
    } else {
        Err(ChannelError::InvalidNumberOfArguments(3))
    }
}

fn four_arguments<'a>(
    args: &[&'a str],
) -> Result<(&'a str, &'a str, &'a str, &'a str), ChannelError> {
    if args.len() == 4 {
        Ok((args[0], args[1], args[2], args[3]))
    } else {
        Err(ChannelError::InvalidNumberOfArguments(4))
    }
}

fn five_arguments<'a>(
    args: &[&'a str],
) -> Result<(&'a str, &'a str, &'a str, &'a str, &'a str), ChannelError> {
    if args.len() == 5 {
        Ok((args[0], args[1], args[2], args[3], args[4]))
    } else {
        Err(ChannelError::InvalidNumberOfArguments(5))
    }
}

fn one_svnt_argument<T>(args: &[&str]) -> Result<T, ChannelError>
where
    T: SignedValueNewType,
{
    let arg = one_argument(args)?;
    parse_svnt(arg)
}

fn one_svnt_argument_allow_zero<T>(args: &[&str]) -> Result<T, ChannelError>
where
    T: SignedValueNewType,
{
    let arg = one_argument(args)?;
    parse_svnt_allow_zero(arg)
}

fn one_uvnt_argument<T>(args: &[&str]) -> Result<T, ChannelError>
where
    T: UnsignedValueNewType,
{
    let arg = one_argument(args)?;
    parse_uvnt(arg)
}

fn two_uvnt_arguments<T, U>(args: &[&str]) -> Result<(T, U), ChannelError>
where
    T: UnsignedValueNewType,
    U: UnsignedValueNewType,
{
    let (arg1, arg2) = two_arguments(args)?;

    Ok((parse_uvnt(arg1)?, parse_uvnt(arg2)?))
}

fn three_uvnt_arguments<T, U, V>(args: &[&str]) -> Result<(T, U, V), ChannelError>
where
    T: UnsignedValueNewType,
    U: UnsignedValueNewType,
    V: UnsignedValueNewType,
{
    let (arg1, arg2, arg3) = three_arguments(args)?;

    Ok((parse_uvnt(arg1)?, parse_uvnt(arg2)?, parse_uvnt(arg3)?))
}

fn two_svnt_arguments<T, U>(args: &[&str]) -> Result<(T, U), ChannelError>
where
    T: SignedValueNewType,
    U: SignedValueNewType,
{
    let (arg1, arg2) = two_arguments(args)?;

    Ok((parse_svnt(arg1)?, parse_svnt(arg2)?))
}

fn svnt_and_uvnt_arguments<T, U>(args: &[&str]) -> Result<(T, U), ChannelError>
where
    T: SignedValueNewType,
    U: UnsignedValueNewType,
{
    let (arg1, arg2) = two_arguments(args)?;

    Ok((parse_svnt(arg1)?, parse_uvnt(arg2)?))
}

fn uvnt_and_svnt_arguments<T, U>(args: &[&str]) -> Result<(T, U), ChannelError>
where
    T: UnsignedValueNewType,
    U: SignedValueNewType,
{
    let (arg1, arg2) = two_arguments(args)?;

    Ok((parse_uvnt(arg1)?, parse_svnt(arg2)?))
}

fn ticks_no_keyoff_argument(args: &[&str]) -> Result<BcTicksNoKeyOff, ChannelError> {
    let arg = one_argument(args)?;

    Ok(BcTicksNoKeyOff::try_from(parse_u32(arg)?)?)
}

fn ticks_keyoff_argument(args: &[&str]) -> Result<BcTicksKeyOff, ChannelError> {
    let arg = one_argument(args)?;

    Ok(BcTicksKeyOff::try_from(parse_u32(arg)?)?)
}

fn play_note_argument(args: &[&str]) -> Result<(Note, PlayNoteTicks), ChannelError> {
    let (note, key_off, ticks) = match args.len() {
        2 => (args[0], "", args[1]),
        3 => (args[0], args[1], args[2]),
        _ => return Err(ChannelError::InvalidNumberOfArgumentsRange(2, 3)),
    };
    let note = Note::parse_bytecode_argument(note)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((note, ticks))
}

fn uvnt_and_play_note_ticks_argument<T>(args: &[&str]) -> Result<(T, PlayNoteTicks), ChannelError>
where
    T: UnsignedValueNewType,
{
    let (a, key_off, ticks) = match args.len() {
        2 => (args[0], "", args[1]),
        3 => (args[0], args[1], args[2]),
        _ => return Err(ChannelError::InvalidNumberOfArgumentsRange(2, 3)),
    };
    let a = parse_uvnt(a)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((a, ticks))
}

fn vibrato_depth_and_play_note_argument(
    args: &[&str],
) -> Result<(VibratoPitchOffsetPerTick, Note, PlayNoteTicks), ChannelError> {
    let (depth, note, key_off, ticks) = match args.len() {
        3 => (args[0], args[1], "", args[2]),
        4 => (args[0], args[1], args[2], args[3]),
        _ => return Err(ChannelError::InvalidNumberOfArgumentsRange(3, 4)),
    };

    let depth = parse_uvnt(depth)?;
    let note = Note::parse_bytecode_argument(note)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((depth, note, ticks))
}

fn portamento_argument(
    args: &[&str],
) -> Result<(Note, PortamentoVelocity, PlayNoteTicks), ChannelError> {
    let (note, key_off, velocity, ticks) = four_arguments(args)?;

    let note = Note::parse_bytecode_argument(note)?;
    let velocity = parse_svnt(velocity)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((note, velocity, ticks))
}

fn portamento_calc_argument(
    args: &[&str],
) -> Result<(Note, PortamentoSlideTicks, PlayNoteTicks), ChannelError> {
    let (note, key_off, slide_ticks, ticks) = four_arguments(args)?;

    let note = Note::parse_bytecode_argument(note)?;
    let slide_ticks = parse_uvnt(slide_ticks)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((note, slide_ticks, ticks))
}

fn portamento_pitch_argument(
    args: &[&str],
) -> Result<(PlayPitchPitch, PortamentoVelocity, PlayNoteTicks), ChannelError> {
    let (target, key_off, velocity, ticks) = four_arguments(args)?;

    let target = parse_uvnt(target)?;
    let velocity = parse_svnt(velocity)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((target, velocity, ticks))
}

fn portamento_pitch_calc_argument(
    args: &[&str],
) -> Result<(PlayPitchPitch, PortamentoSlideTicks, PlayNoteTicks), ChannelError> {
    let (target, key_off, slide_ticks, ticks) = four_arguments(args)?;

    let target = parse_uvnt(target)?;
    let slide_ticks = parse_uvnt(slide_ticks)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((target, slide_ticks, ticks))
}

fn adsr_argument(args: &[&str]) -> Result<Adsr, ChannelError> {
    let args = four_arguments(args)?;

    Ok(Adsr::try_from_strs(args.0, args.1, args.2, args.3)?)
}

fn gain_argument(args: &[&str]) -> Result<Gain, ChannelError> {
    let arg = one_argument(args)?;

    Ok(arg.parse()?)
}

fn temp_gain_argument(args: &[&str]) -> Result<TempGain, ChannelError> {
    let arg = one_argument(args)?;

    Ok(arg.parse()?)
}

fn temp_gain_and_wait_arguments(
    args: &[&str],
) -> Result<(TempGain, BcTicksNoKeyOff), ChannelError> {
    let (gain, length) = two_arguments(args)?;

    Ok((
        gain.parse()?,
        BcTicksNoKeyOff::try_from(parse_u32(length)?)?,
    ))
}

fn temp_gain_and_rest_arguments(args: &[&str]) -> Result<(TempGain, BcTicksKeyOff), ChannelError> {
    let (gain, length) = two_arguments(args)?;

    Ok((gain.parse()?, BcTicksKeyOff::try_from(parse_u32(length)?)?))
}

fn instrument_and_adsr_argument<'a>(args: &[&'a str]) -> Result<(&'a str, Adsr), ChannelError> {
    let (inst, a1, a2, a3, a4) = five_arguments(args)?;

    let adsr = Adsr::try_from_strs(a1, a2, a3, a4)?;

    Ok((inst, adsr))
}

fn instrument_and_gain_argument<'a>(args: &[&'a str]) -> Result<(&'a str, Gain), ChannelError> {
    let (inst, gain) = two_arguments(args)?;
    let gain = gain.parse()?;

    Ok((inst, gain))
}

fn early_release_arguments(
    args: &[&str],
) -> Result<(EarlyReleaseTicks, EarlyReleaseMinTicks, OptionalGain), ChannelError> {
    match args.len() {
        2 => Ok((
            parse_uvnt(args[0])?,
            parse_uvnt(args[1])?,
            OptionalGain::NONE,
        )),
        3 => Ok((
            parse_uvnt(args[0])?,
            parse_uvnt(args[1])?,
            OptionalGain::try_from_str_forbid_none_and_raw(args[2])?,
        )),
        _ => Err(ChannelError::InvalidNumberOfArgumentsRange(2, 3)),
    }
}

fn optional_loop_count_argument(args: &[&str]) -> Result<Option<LoopCount>, ChannelError> {
    match args.len() {
        0 => Ok(None),
        1 => Ok(Some(parse_uvnt(args[0])?)),
        _ => Err(ChannelError::InvalidNumberOfArgumentsRange(0, 1)),
    }
}

fn echo_feedback_argument(args: &[&str]) -> Result<EchoFeedback, ChannelError> {
    let feedback = one_argument(args)?;

    Ok(parse_i8wh(feedback)?)
}

fn adjust_echo_feedback_limit_arguments(
    args: &[&str],
) -> Result<(RelativeEchoFeedback, EchoFeedback), ChannelError> {
    let (rel, limit) = two_arguments(args)?;
    Ok((parse_svnt(rel)?, parse_i8wh(limit)?))
}

fn set_fir_tap_arguments(args: &[&str]) -> Result<(FirTap, FirCoefficient), ChannelError> {
    let (tap, value) = two_arguments(args)?;

    Ok((parse_uvnt(tap)?, parse_i8wh(value)?))
}

fn adjust_fir_tap_limit_arguments(
    args: &[&str],
) -> Result<(FirTap, RelativeFirCoefficient, FirCoefficient), ChannelError> {
    let (tap, rel, limit) = three_arguments(args)?;
    Ok((parse_uvnt(tap)?, parse_svnt(rel)?, parse_i8wh(limit)?))
}

fn fir_filter_argument(args: &[&str]) -> Result<[FirCoefficient; FIR_FILTER_SIZE], ChannelError> {
    if args.len() == FIR_FILTER_SIZE {
        let mut out = [FirCoefficient::ZERO; FIR_FILTER_SIZE];
        assert_eq!(out.len(), args.len());
        for (f, s) in out.iter_mut().zip(args.iter()) {
            *f = parse_i8wh(s)?;
        }
        Ok(out)
    } else {
        Err(ChannelError::InvalidNumberOfArguments(
            FIR_FILTER_SIZE as u8,
        ))
    }
}

fn parse_play_note_ticks(ticks: &str, key_off: &str) -> Result<PlayNoteTicks, ChannelError> {
    let is_slur = match key_off {
        "" => false,
        "keyoff" => false,

        "no_keyoff" => true,
        "nko" => true,
        "slur_next" => true,
        "sn" => true,

        _ => return Err(ChannelError::InvalidKeyoffArgument(key_off.to_owned())),
    };

    let ticks = parse_u32(ticks)?;
    Ok(PlayNoteTicks::try_from_is_slur(ticks, is_slur)?)
}

fn parse_u32(s: &str) -> Result<u32, ValueError> {
    match s.bytes().next() {
        Some(b'$') => match u32::from_str_radix(&s[1..], 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(s.to_owned())),
        },
        _ => match s.parse() {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseUnsigned(s.to_owned())),
        },
    }
}

fn parse_i32(src: &str, missing_sign_err: &ValueError) -> Result<i32, ValueError> {
    if let Some(s) = src.strip_prefix("+$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else if let Some(s) = src.strip_prefix("-$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(-i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else {
        match src.bytes().next() {
            Some(b'-') | Some(b'+') => match src.parse() {
                Ok(i) => Ok(i),
                Err(_) => Err(ValueError::CannotParseSigned(src.to_owned())),
            },
            _ => Err(missing_sign_err.clone()),
        }
    }
}

pub(crate) fn parse_i32_allow_zero(
    src: &str,
    missing_sign_err: &ValueError,
) -> Result<i32, ValueError> {
    if let Some(s) = src.strip_prefix("+$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else if let Some(s) = src.strip_prefix("-$") {
        match i32::from_str_radix(s, 16) {
            Ok(i) => Ok(-i),
            Err(_) => Err(ValueError::CannotParseHex(src.to_owned())),
        }
    } else {
        match src.bytes().next() {
            Some(b'-') | Some(b'+') => match src.parse() {
                Ok(i) => Ok(i),
                Err(_) => Err(ValueError::CannotParseSigned(src.to_owned())),
            },
            _ => match src.parse::<i32>() {
                Ok(0) => Ok(0),
                _ => Err(missing_sign_err.clone()),
            },
        }
    }
}

/// Parse UnsignedValueNewType
fn parse_uvnt<T>(s: &str) -> Result<T, ChannelError>
where
    T: UnsignedValueNewType,
{
    Ok(parse_u32(s)?.try_into()?)
}

/// Parse SignedValueNewType
fn parse_svnt<T>(s: &str) -> Result<T, ChannelError>
where
    T: SignedValueNewType,
{
    Ok(parse_i32(s, &T::MISSING_SIGN_ERROR)?.try_into()?)
}

/// Parse SignedValueNewType
fn parse_svnt_allow_zero<T>(s: &str) -> Result<T, ChannelError>
where
    T: SignedValueNewType,
{
    Ok(parse_i32_allow_zero(s, &T::MISSING_SIGN_ERROR)?.try_into()?)
}

pub fn parse_asm_line(bc: &mut Bytecode, line: &str) -> Result<(), ChannelError> {
    // Strip comments
    let line = match line.split_once(';') {
        Some((l, _comment)) => l,
        None => line,
    };
    let line = line.trim();

    if line.is_empty() {
        return Ok(());
    }

    let (instruction, arguments) = line
        .split_once(|c: char| c.is_ascii_whitespace())
        .unwrap_or((line, ""));

    let arguments: Vec<&str> = if arguments.contains(',') {
        arguments.split(',').map(|s| s.trim()).collect()
    } else {
        arguments.split_ascii_whitespace().collect()
    };

    macro_rules! parse_args_and_execute {
        ($name:ident, $n_args:tt, $arg_parser:ident, $method:ident) => {{
            parse_args_and_execute!($method, $n_args, $arg_parser)
        }};

        ($name:ident, 0, $arg_parser:ident) => {{
            let () = $arg_parser(&arguments)?;
            BytecodeResultWrapper::wrap(bc.$name())
        }};

        ($name:ident, 1, $arg_parser:ident) => {{
            let a = $arg_parser(&arguments)?;
            BytecodeResultWrapper::wrap(bc.$name(a))
        }};

        ($name:ident, 2, $arg_parser:ident) => {{
            let (a1, a2) = $arg_parser(&arguments)?;
            BytecodeResultWrapper::wrap(bc.$name(a1, a2))
        }};

        ($name:ident, 3, $arg_parser:ident) => {{
            let (a1, a2, a3) = $arg_parser(&arguments)?;
            BytecodeResultWrapper::wrap(bc.$name(a1, a2, a3))
        }};

        ($name:ident, 4, $arg_parser:ident) => {{
            let (a1, a2, a3, a4) = $arg_parser(&arguments)?;
            BytecodeResultWrapper::wrap(bc.$name(a1, a2, a3, a4))
        }};
    }

    macro_rules! build_match {
            ($($name:ident $n_args:tt $arg_parser:ident $($method:ident)?, ) *) => {
                match instruction {
                    $(
                        stringify![$name] => {
                            parse_args_and_execute!($name, $n_args, $arg_parser $(, $method)?)
                        }
                    )*

                   _ => return Err(ChannelError::UnknownInstruction(instruction.to_owned())),
                }
            }
        }

    build_match!(
       wait 1 ticks_no_keyoff_argument,
       rest 1 ticks_keyoff_argument,

       play_note 2 play_note_argument,
       play_pitch 2 uvnt_and_play_note_ticks_argument,
       play_noise 2 uvnt_and_play_note_ticks_argument,
       disable_noise 0 no_arguments,

       portamento 3 portamento_argument,
       portamento_calc 3 portamento_calc_argument,
       portamento_pitch 3 portamento_pitch_argument,
       portamento_pitch_calc 3 portamento_pitch_calc_argument,
       set_vibrato_depth_and_play_note 3 vibrato_depth_and_play_note_argument,

       set_vibrato 2 two_uvnt_arguments,
       set_vibrato_with_delay 3 three_uvnt_arguments,
       disable_vibrato 0 no_arguments,

       set_instrument 1 one_argument set_instrument_str,
       set_instrument_and_adsr 2 instrument_and_adsr_argument set_instrument_and_adsr_str,
       set_instrument_and_gain 2 instrument_and_gain_argument set_instrument_and_gain_str,

       set_adsr 1 adsr_argument,
       set_gain 1 gain_argument,

       set_temp_gain 1 temp_gain_argument,
       set_temp_gain_and_wait 2 temp_gain_and_wait_arguments,
       set_temp_gain_and_rest 2 temp_gain_and_rest_arguments,

       reuse_temp_gain 0 no_arguments,
       reuse_temp_gain_and_wait 1 ticks_no_keyoff_argument,
       reuse_temp_gain_and_rest 1 ticks_keyoff_argument,

       disable_early_release 0 no_arguments,
       set_early_release 3 early_release_arguments,

       set_transpose 1 one_svnt_argument_allow_zero,
       adjust_transpose 1 one_svnt_argument,
       disable_transpose 0 no_arguments,

       set_detune 1 one_svnt_argument_allow_zero,
       disable_detune 0 no_arguments,

       adjust_volume 1 one_svnt_argument,
       set_volume 1 one_uvnt_argument,
       adjust_pan 1 one_svnt_argument,
       set_pan 1 one_uvnt_argument,
       set_pan_and_volume 2 two_uvnt_arguments,

       set_channel_invert 1 parse_invert_flag_arguments,

       volume_slide 2 svnt_and_uvnt_arguments,
       tremolo 2 two_uvnt_arguments,
       pan_slide 2 svnt_and_uvnt_arguments,
       panbrello 2 two_uvnt_arguments,

       enable_pmod 0 no_arguments,
       disable_pmod 0 no_arguments,

       enable_echo 0 no_arguments,
       disable_echo 0 no_arguments,

       start_loop 1 optional_loop_count_argument,
       skip_last_loop 0 no_arguments,
       end_loop 1 optional_loop_count_argument,

       call_subroutine_and_disable_vibrato 1 one_argument call_subroutine_and_disable_vibrato_str,
       call_subroutine 1 one_argument call_subroutine_str,

       set_song_tick_clock 1 one_uvnt_argument,

       set_echo_volume 1 one_uvnt_argument,
       set_stereo_echo_volume 2 two_uvnt_arguments,
       adjust_echo_volume 1 one_svnt_argument,
       adjust_stereo_echo_volume 2 two_svnt_arguments,
       set_echo_feedback 1 echo_feedback_argument,
       adjust_echo_feedback 1 one_svnt_argument,
       adjust_echo_feedback_limit 2 adjust_echo_feedback_limit_arguments,
       set_fir_filter 1 fir_filter_argument,
       set_fir_tap 2 set_fir_tap_arguments,
       adjust_fir_tap 2 uvnt_and_svnt_arguments,
       adjust_fir_tap_limit 3 adjust_fir_tap_limit_arguments,
       set_echo_invert 1 parse_invert_flag_arguments,
       set_echo_delay 1 one_uvnt_argument,

       keyon_next_note 0 no_arguments,
    )
}

pub(crate) const CALL_SUBROUTINE: &str = "call_subroutine";
pub(crate) const CALL_SUBROUTINE_AND_DISABLE_VIBRATO: &str = "call_subroutine_and_disable_vibrato";

pub(crate) const SET_TRANSPOSE: &str = "set_transpose";
pub(crate) const ADJUST_TRANSPOSE: &str = "adjust_transpose";

pub struct BytecodeAssembler<'a> {
    bc: Bytecode<'a>,
}

impl BytecodeAssembler<'_> {
    pub fn new<'a>(
        inst_map: &'a UniqueNamesList<InstrumentOrSample>,
        subroutines: &'a dyn SubroutineStore,
        context: BytecodeContext,
    ) -> BytecodeAssembler<'a> {
        BytecodeAssembler {
            bc: Bytecode::new(context, inst_map, subroutines),
        }
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.bc.get_tick_counter()
    }

    pub fn bytecode(self, terminator: BcTerminator) -> Result<(Vec<u8>, State), ChannelError> {
        match self.bc.bytecode(terminator) {
            Ok(b) => Ok(b),
            Err((e, _)) => Err(ChannelError::BytecodeError(e)),
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), ChannelError> {
        parse_asm_line(&mut self.bc, line)
    }
}
