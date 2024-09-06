//! Audio driver bytecode assembler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{
    BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, InstrumentId, LoopCount, PitchOffsetPerTick,
    PlayNoteTicks, PortamentoVelocity, SubroutineId,
};
use crate::data::{InstrumentOrSample, UniqueNamesList};
use crate::envelope::{Adsr, Gain};
use crate::errors::{BytecodeAssemblerError, BytecodeError, ValueError};
use crate::notes::Note;
use crate::time::TickCounter;
use crate::value_newtypes::ValueNewType;

pub use crate::bytecode::{BcTerminator, BytecodeContext};

use std::collections::HashMap;

pub type SubroutinesMap<'a> = HashMap<&'a str, SubroutineId>;

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

pub struct BytecodeAssembler<'a, 'b> {
    bc: Bytecode,
    inst_map: &'a UniqueNamesList<InstrumentOrSample>,
    subroutines: Option<&'b SubroutinesMap<'b>>,
}

impl BytecodeAssembler<'_, '_> {
    pub fn new<'a, 'b>(
        inst_map: &'a UniqueNamesList<InstrumentOrSample>,
        subroutines: Option<&'b SubroutinesMap<'b>>,
        context: BytecodeContext,
    ) -> BytecodeAssembler<'a, 'b> {
        BytecodeAssembler {
            bc: Bytecode::new(context),
            inst_map,
            subroutines,
        }
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.bc.get_tick_counter()
    }

    pub fn bytecode(self, terminator: BcTerminator) -> Result<Vec<u8>, BytecodeAssemblerError> {
        match self.bc.bytecode(terminator) {
            Ok(b) => Ok(b),
            Err((e, _)) => Err(BytecodeAssemblerError::BytecodeError(e)),
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), BytecodeAssemblerError> {
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
            ($name:ident, 0, $arg_parser:ident) => {{
                let () = self.$arg_parser(&arguments)?;
                BytecodeResultWrapper::wrap(self.bc.$name())
            }};

            ($name:ident, 1, $arg_parser:ident) => {{
                let a = self.$arg_parser(&arguments)?;
                BytecodeResultWrapper::wrap(self.bc.$name(a))
            }};

            ($name:ident, 2, $arg_parser:ident) => {{
                let (a1, a2) = self.$arg_parser(&arguments)?;
                BytecodeResultWrapper::wrap(self.bc.$name(a1, a2))
            }};

            ($name:ident, 3, $arg_parser:ident) => {{
                let (a1, a2, a3) = self.$arg_parser(&arguments)?;
                BytecodeResultWrapper::wrap(self.bc.$name(a1, a2, a3))
            }};

            ($name:ident, 4, $arg_parser:ident) => {{
                let (a1, a2, a3, a4) = self.$arg_parser(&arguments)?;
                BytecodeResultWrapper::wrap(self.bc.$name(a1, a2, a3, a4))
            }};
        }

        macro_rules! build_match {
            ($($name:ident $n_args:tt $arg_parser:ident , ) *) => {
                match instruction {
                    $(
                        stringify![$name] => {
                            parse_args_and_execute!($name, $n_args, $arg_parser)
                        }
                    )*

                   _ => return Err(BytecodeAssemblerError::UnknownInstruction(instruction.to_owned())),
                }
            }
        }

        build_match!(
           wait 1 ticks_no_keyoff_argument,
           rest_keyoff 1 rest_keyoff_argument,

           play_note 2 play_note_argument,
           portamento 3 portamento_argument,
           set_vibrato_depth_and_play_note 3 vibrato_depth_and_play_note_argument,

           set_vibrato 2 two_vnt_arguments,
           disable_vibrato 0 no_arguments,

           set_instrument 1 instrument_argument,
           set_instrument_and_adsr 2 instrument_and_adsr_argument,
           set_instrument_and_gain 2 instrument_and_gain_argument,

           set_adsr 1 adsr_argument,
           set_gain 1 gain_argument,

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

           call_subroutine_and_disable_vibrato 2 subroutine_argument,
           call_subroutine 2 subroutine_argument,

           set_song_tick_clock 1 one_vnt_argument,
        )
    }

    fn no_arguments(&self, args: &[&str]) -> Result<(), BytecodeAssemblerError> {
        if args.is_empty() {
            Ok(())
        } else {
            Err(BytecodeAssemblerError::InvalidNumberOfArguments(0))
        }
    }

    fn one_vnt_argument<T>(&self, args: &[&str]) -> Result<T, BytecodeAssemblerError>
    where
        T: ValueNewType,
    {
        let arg = one_argument(args)?;
        Ok(T::try_from_str(arg)?)
    }

    fn two_vnt_arguments<T, U>(&self, args: &[&str]) -> Result<(T, U), BytecodeAssemblerError>
    where
        T: ValueNewType,
        U: ValueNewType,
    {
        let (arg1, arg2) = two_arguments(args)?;

        Ok((T::try_from_str(arg1)?, U::try_from_str(arg2)?))
    }

    fn ticks_no_keyoff_argument(
        &self,
        args: &[&str],
    ) -> Result<BcTicksNoKeyOff, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        Ok(BcTicksNoKeyOff::try_from(parse_u32(arg)?)?)
    }

    fn rest_keyoff_argument(&self, args: &[&str]) -> Result<BcTicksKeyOff, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        Ok(BcTicksKeyOff::try_from(parse_u32(arg)?)?)
    }

    fn play_note_argument(
        &self,
        args: &[&str],
    ) -> Result<(Note, PlayNoteTicks), BytecodeAssemblerError> {
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
        &self,
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
        &self,
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

    fn adsr_argument(&self, args: &[&str]) -> Result<Adsr, BytecodeAssemblerError> {
        let args = four_arguments(args)?;

        Ok(Adsr::try_from_strs(args.0, args.1, args.2, args.3)?)
    }

    fn gain_argument(&self, args: &[&str]) -> Result<Gain, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        Ok(arg.parse()?)
    }

    fn _find_instrument(&self, arg: &str) -> Result<InstrumentId, BytecodeAssemblerError> {
        match self.inst_map.get_with_index(arg) {
            Some((i, _inst)) => Ok(InstrumentId::try_from(i)?),
            None => Err(BytecodeAssemblerError::UnknownInstrument(arg.to_owned())),
        }
    }

    fn instrument_argument(&self, args: &[&str]) -> Result<InstrumentId, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        self._find_instrument(arg)
    }

    fn instrument_and_adsr_argument(
        &self,
        args: &[&str],
    ) -> Result<(InstrumentId, Adsr), BytecodeAssemblerError> {
        let (inst, a1, a2, a3, a4) = five_arguments(args)?;

        let inst = self._find_instrument(inst)?;
        let adsr = Adsr::try_from_strs(a1, a2, a3, a4)?;

        Ok((inst, adsr))
    }

    fn instrument_and_gain_argument(
        &self,
        args: &[&str],
    ) -> Result<(InstrumentId, Gain), BytecodeAssemblerError> {
        let (inst, gain) = two_arguments(args)?;

        let inst = self._find_instrument(inst)?;
        let gain = gain.parse()?;

        Ok((inst, gain))
    }

    fn subroutine_argument<'a>(
        &self,
        args: &[&'a str],
    ) -> Result<(&'a str, SubroutineId), BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        match self.subroutines {
            Some(subroutines) => match subroutines.get(arg) {
                Some(s) => Ok((arg, *s)),
                None => Err(BytecodeAssemblerError::UnknownSubroutine(arg.to_owned())),
            },
            None => Err(BytecodeAssemblerError::UnknownSubroutine(arg.to_owned())),
        }
    }

    fn optional_loop_count_argument(
        &self,
        args: &[&str],
    ) -> Result<Option<LoopCount>, BytecodeAssemblerError> {
        match args.len() {
            0 => Ok(None),
            1 => Ok(Some(LoopCount::try_from_str(args[0])?)),
            _ => Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(0, 1)),
        }
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
