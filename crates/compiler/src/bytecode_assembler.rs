//! Audio driver bytecode assembler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, EarlyReleaseTicks, LoopCount, PitchOffsetPerTick,
    PlayNoteTicks, PortamentoVelocity, State, SubroutineId,
};
use crate::data::{InstrumentOrSample, UniqueNamesList};
use crate::envelope::{Adsr, Gain};
use crate::errors::{BytecodeAssemblerError, BytecodeError, ValueError};
use crate::notes::Note;
use crate::time::TickCounter;
use crate::value_newtypes::ValueNewType;

pub use crate::bytecode::{BcTerminator, BytecodeContext};

use std::collections::HashMap;

// Some `Bytecode` methods do not return a Result.
// The macro in `BytecodeAssembler::parse_line()` cannot determine the method's value.
// This trait will wrap () into Ok(()) so `BytecodeAssembler::parse_line()` can compile.
trait BytecodeResultWrapper {
    fn wrap(self) -> Result<(), BytecodeAssemblerError>;
}

impl BytecodeResultWrapper for () {
    fn wrap(self) -> Result<(), BytecodeAssemblerError> {
        Ok(())
    }
}

impl BytecodeResultWrapper for Result<(), BytecodeError> {
    fn wrap(self) -> Result<(), BytecodeAssemblerError> {
        match self {
            Ok(()) => Ok(()),
            Err(e) => Err(BytecodeAssemblerError::BytecodeError(e)),
        }
    }
}

fn no_arguments(args: &[&str]) -> Result<(), BytecodeAssemblerError> {
    if args.is_empty() {
        Ok(())
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(0))
    }
}

fn one_argument<'a>(args: &[&'a str]) -> Result<&'a str, BytecodeAssemblerError> {
    if args.len() == 1 {
        Ok(args[0])
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(1))
    }
}

fn two_arguments<'a>(args: &[&'a str]) -> Result<(&'a str, &'a str), BytecodeAssemblerError> {
    if args.len() == 2 {
        Ok((args[0], args[1]))
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(2))
    }
}

#[allow(dead_code)]
fn three_arguments<'a>(
    args: &[&'a str],
) -> Result<(&'a str, &'a str, &'a str), BytecodeAssemblerError> {
    if args.len() == 3 {
        Ok((args[0], args[1], args[2]))
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(3))
    }
}

fn four_arguments<'a>(
    args: &[&'a str],
) -> Result<(&'a str, &'a str, &'a str, &'a str), BytecodeAssemblerError> {
    if args.len() == 4 {
        Ok((args[0], args[1], args[2], args[3]))
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(4))
    }
}

fn five_arguments<'a>(
    args: &[&'a str],
) -> Result<(&'a str, &'a str, &'a str, &'a str, &'a str), BytecodeAssemblerError> {
    if args.len() == 5 {
        Ok((args[0], args[1], args[2], args[3], args[4]))
    } else {
        Err(BytecodeAssemblerError::InvalidNumberOfArguments(5))
    }
}

fn one_vnt_argument<T>(args: &[&str]) -> Result<T, BytecodeAssemblerError>
where
    T: ValueNewType,
{
    let arg = one_argument(args)?;
    Ok(T::try_from_str(arg)?)
}

fn two_vnt_arguments<T, U>(args: &[&str]) -> Result<(T, U), BytecodeAssemblerError>
where
    T: ValueNewType,
    U: ValueNewType,
{
    let (arg1, arg2) = two_arguments(args)?;

    Ok((T::try_from_str(arg1)?, U::try_from_str(arg2)?))
}

fn ticks_no_keyoff_argument(args: &[&str]) -> Result<BcTicksNoKeyOff, BytecodeAssemblerError> {
    let arg = one_argument(args)?;

    Ok(BcTicksNoKeyOff::try_from(parse_u32(arg)?)?)
}

fn ticks_keyoff_argument(args: &[&str]) -> Result<BcTicksKeyOff, BytecodeAssemblerError> {
    let arg = one_argument(args)?;

    Ok(BcTicksKeyOff::try_from(parse_u32(arg)?)?)
}

fn play_note_argument(args: &[&str]) -> Result<(Note, PlayNoteTicks), BytecodeAssemblerError> {
    let (note, key_off, ticks) = match args.len() {
        2 => (args[0], "", args[1]),
        3 => (args[0], args[1], args[2]),
        _ => return Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(2, 3)),
    };
    let note = Note::parse_bytecode_argument(note)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((note, ticks))
}

fn vibrato_depth_and_play_note_argument(
    args: &[&str],
) -> Result<(PitchOffsetPerTick, Note, PlayNoteTicks), BytecodeAssemblerError> {
    let (depth, note, key_off, ticks) = match args.len() {
        3 => (args[0], args[1], "", args[2]),
        4 => (args[0], args[1], args[2], args[3]),
        _ => return Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(3, 4)),
    };

    let depth = PitchOffsetPerTick::try_from_str(depth)?;
    let note = Note::parse_bytecode_argument(note)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    Ok((depth, note, ticks))
}

fn portamento_argument(
    args: &[&str],
) -> Result<(Note, PortamentoVelocity, PlayNoteTicks), BytecodeAssemblerError> {
    let (note, key_off, velocity, ticks) = four_arguments(args)?;

    let note = Note::parse_bytecode_argument(note)?;
    let ticks = parse_play_note_ticks(ticks, key_off)?;

    match velocity.chars().next() {
        Some('+') => 1,
        Some('-') => -1,
        _ => return Err(BytecodeAssemblerError::NoDirectionInPortamentoVelocity),
    };
    let velocity = PortamentoVelocity::try_from_str(velocity)?;

    Ok((note, velocity, ticks))
}

fn adsr_argument(args: &[&str]) -> Result<Adsr, BytecodeAssemblerError> {
    let args = four_arguments(args)?;

    Ok(Adsr::try_from_strs(args.0, args.1, args.2, args.3)?)
}

fn gain_argument(args: &[&str]) -> Result<Gain, BytecodeAssemblerError> {
    let arg = one_argument(args)?;

    Ok(arg.parse()?)
}

fn gain_and_wait_arguments(
    args: &[&str],
) -> Result<(Gain, BcTicksNoKeyOff), BytecodeAssemblerError> {
    let (gain, length) = two_arguments(args)?;

    Ok((
        gain.parse()?,
        BcTicksNoKeyOff::try_from(parse_u32(length)?)?,
    ))
}

fn gain_and_rest_arguments(args: &[&str]) -> Result<(Gain, BcTicksKeyOff), BytecodeAssemblerError> {
    let (gain, length) = two_arguments(args)?;

    Ok((gain.parse()?, BcTicksKeyOff::try_from(parse_u32(length)?)?))
}

fn instrument_and_adsr_argument<'a>(
    args: &[&'a str],
) -> Result<(&'a str, Adsr), BytecodeAssemblerError> {
    let (inst, a1, a2, a3, a4) = five_arguments(args)?;

    let adsr = Adsr::try_from_strs(a1, a2, a3, a4)?;

    Ok((inst, adsr))
}

fn instrument_and_gain_argument<'a>(
    args: &[&'a str],
) -> Result<(&'a str, Gain), BytecodeAssemblerError> {
    let (inst, gain) = two_arguments(args)?;
    let gain = gain.parse()?;

    Ok((inst, gain))
}

fn early_release_arguments(
    args: &[&str],
) -> Result<(EarlyReleaseTicks, Gain), BytecodeAssemblerError> {
    match args.len() {
        1 => Ok((EarlyReleaseTicks::try_from_str(args[0])?, Gain::new(0))),
        2 => Ok((EarlyReleaseTicks::try_from_str(args[0])?, args[1].parse()?)),
        _ => Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(1, 2)),
    }
}

fn optional_loop_count_argument(
    args: &[&str],
) -> Result<Option<LoopCount>, BytecodeAssemblerError> {
    match args.len() {
        0 => Ok(None),
        1 => Ok(Some(LoopCount::try_from_str(args[0])?)),
        _ => Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(0, 1)),
    }
}

fn parse_play_note_ticks(
    ticks: &str,
    key_off: &str,
) -> Result<PlayNoteTicks, BytecodeAssemblerError> {
    let is_slur = match key_off {
        "" => false,
        "keyoff" => false,

        "no_keyoff" => true,
        "nko" => true,
        "slur_next" => true,
        "sn" => true,

        _ => {
            return Err(BytecodeAssemblerError::InvalidKeyoffArgument(
                key_off.to_owned(),
            ))
        }
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

pub fn parse_asm_line(bc: &mut Bytecode, line: &str) -> Result<(), BytecodeAssemblerError> {
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

                   _ => return Err(BytecodeAssemblerError::UnknownInstruction(instruction.to_owned())),
                }
            }
        }

    build_match!(
       wait 1 ticks_no_keyoff_argument,
       rest 1 ticks_keyoff_argument,

       play_note 2 play_note_argument,
       portamento 3 portamento_argument,
       set_vibrato_depth_and_play_note 3 vibrato_depth_and_play_note_argument,

       set_vibrato 2 two_vnt_arguments,
       disable_vibrato 0 no_arguments,

       set_instrument 1 one_argument set_instrument_str,
       set_instrument_and_adsr 2 instrument_and_adsr_argument set_instrument_and_adsr_str,
       set_instrument_and_gain 2 instrument_and_gain_argument set_instrument_and_gain_str,

       set_adsr 1 adsr_argument,
       set_gain 1 gain_argument,

       set_temp_gain 1 gain_argument,
       set_temp_gain_and_wait 2 gain_and_wait_arguments,
       set_temp_gain_and_rest 2 gain_and_rest_arguments,

       reuse_temp_gain 0 no_arguments,
       reuse_temp_gain_and_wait 1 ticks_no_keyoff_argument,
       reuse_temp_gain_and_rest 1 ticks_keyoff_argument,

       disable_early_release 0 no_arguments,
       set_early_release 2 early_release_arguments,

       adjust_volume 1 one_vnt_argument,
       set_volume 1 one_vnt_argument,
       adjust_pan 1 one_vnt_argument,
       set_pan 1 one_vnt_argument,
       set_pan_and_volume 2 two_vnt_arguments,

       enable_echo 0 no_arguments,
       disable_echo 0 no_arguments,

       start_loop 1 optional_loop_count_argument,
       skip_last_loop 0 no_arguments,
       end_loop 1 optional_loop_count_argument,

       call_subroutine_and_disable_vibrato 1 one_argument call_subroutine_and_disable_vibrato_str,
       call_subroutine 1 one_argument call_subroutine_str,

       set_song_tick_clock 1 one_vnt_argument,
    )
}

pub(crate) const CALL_SUBROUTINE: &str = "call_subroutine";
pub(crate) const CALL_SUBROUTINE_AND_DISABLE_VIBRATO: &str = "call_subroutine_and_disable_vibrato";

pub struct BytecodeAssembler<'a> {
    bc: Bytecode<'a>,
}

impl BytecodeAssembler<'_> {
    pub fn new<'a>(
        inst_map: &'a UniqueNamesList<InstrumentOrSample>,
        subroutines: Option<&'a HashMap<&'a str, SubroutineId>>,
        context: BytecodeContext,
    ) -> BytecodeAssembler<'a> {
        BytecodeAssembler {
            bc: Bytecode::new(context, inst_map, subroutines),
        }
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.bc.get_tick_counter()
    }

    pub fn bytecode(
        self,
        terminator: BcTerminator,
    ) -> Result<(Vec<u8>, State), BytecodeAssemblerError> {
        match self.bc.bytecode(terminator) {
            Ok(b) => Ok(b),
            Err((e, _)) => Err(BytecodeAssemblerError::BytecodeError(e)),
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), BytecodeAssemblerError> {
        parse_asm_line(&mut self.bc, line)
    }
}
