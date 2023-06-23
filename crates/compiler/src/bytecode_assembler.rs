//! Audio driver bytecode assembler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{Bytecode, InstrumentId, SubroutineId};
use crate::data::Instrument;
use crate::envelope::{Adsr, Gain};
use crate::errors::{BytecodeAssemblerError, BytecodeError};
use crate::notes::Note;
use crate::time::{TickClock, TickCounter};

use std::collections::HashMap;

pub type InstrumentsMap<'a> = HashMap<&'a str, InstrumentId>;

#[allow(clippy::ptr_arg)]
pub fn build_instruments_map(instruments: &Vec<Instrument>) -> InstrumentsMap {
    // No bounds checking or duplicate name tests in this function.
    //
    // This is OK as Bytecode only uses the id, not instrument data.
    instruments
        .iter()
        .enumerate()
        .map(|(i, inst)| {
            (
                inst.name.as_str(),
                InstrumentId::new(u8::try_from(i).unwrap_or(0)),
            )
        })
        .collect()
}

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
    instruments: &'a InstrumentsMap<'a>,
    subroutines: Option<&'b SubroutinesMap<'b>>,
}

impl BytecodeAssembler<'_, '_> {
    pub fn new<'a, 'b>(
        instruments: &'a InstrumentsMap<'a>,
        subroutines: Option<&'b SubroutinesMap<'b>>,
        is_subroutine: bool,
        is_sound_effect: bool,
    ) -> BytecodeAssembler<'a, 'b> {
        BytecodeAssembler {
            bc: Bytecode::new(is_subroutine, is_sound_effect),
            instruments,
            subroutines,
        }
    }

    pub fn get_tick_counter(&self) -> TickCounter {
        self.bc.get_tick_counter()
    }

    pub fn get_bytecode(&self) -> Result<&[u8], BytecodeAssemblerError> {
        match self.bc.get_bytecode() {
            Ok(b) => Ok(b),
            Err(e) => Err(BytecodeAssemblerError::BytecodeError(e)),
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
           rest 1 length_argument,
           rest_keyoff 1 length_argument,

           play_note 3 play_note_argument,
           portamento 4 portamento_argument,
           set_vibrato_depth_and_play_note 4 vibrato_depth_and_play_note_argument,

           set_vibrato 2 two_u32_arguments,
           disable_vibrato 0 no_arguments,

           set_instrument 1 instrument_argument,
           set_instrument_and_adsr 2 instrument_and_adsr_argument,
           set_instrument_and_gain 2 instrument_and_gain_argument,

           set_adsr 1 adsr_argument,
           set_gain 1 gain_argument,

           adjust_volume 1 i32_argument,
           set_volume 1 u32_argument,
           adjust_pan 1 i32_argument,
           set_pan 1 u32_argument,
           set_pan_and_volume 2 two_u32_arguments,

           enable_echo 0 no_arguments,
           disable_echo 0 no_arguments,

           start_loop 1 u32_argument,
           skip_last_loop 0 no_arguments,
           end_loop 0 no_arguments,

           call_subroutine 1 subroutine_argument,
           return_from_subroutine 0 no_arguments,

           end 0 no_arguments,
           disable_channel 0 no_arguments,

           set_song_tick_clock 1 tick_clock_argument,
        )
    }

    fn no_arguments(&self, args: &[&str]) -> Result<(), BytecodeAssemblerError> {
        if args.is_empty() {
            Ok(())
        } else {
            Err(BytecodeAssemblerError::InvalidNumberOfArguments(0))
        }
    }

    fn length_argument(&self, args: &[&str]) -> Result<TickCounter, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        parse_length(arg)
    }

    fn play_note_argument(
        &self,
        args: &[&str],
    ) -> Result<(Note, bool, TickCounter), BytecodeAssemblerError> {
        let (note, key_off, tick_counter) = match args.len() {
            2 => (
                Note::parse_bytecode_argument(args[0])?,
                true,
                parse_length(args[1])?,
            ),
            3 => (
                Note::parse_bytecode_argument(args[0])?,
                parse_key_off(args[1])?,
                parse_length(args[2])?,
            ),
            _ => return Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(2, 3)),
        };

        Ok((note, key_off, tick_counter))
    }

    fn vibrato_depth_and_play_note_argument(
        &self,
        args: &[&str],
    ) -> Result<(u32, Note, bool, TickCounter), BytecodeAssemblerError> {
        let (depth, note, key_off, tick_counter) = match args.len() {
            3 => (
                parse_u32(args[0])?,
                Note::parse_bytecode_argument(args[1])?,
                true,
                parse_length(args[2])?,
            ),
            4 => (
                parse_u32(args[0])?,
                Note::parse_bytecode_argument(args[1])?,
                parse_key_off(args[2])?,
                parse_length(args[3])?,
            ),
            _ => return Err(BytecodeAssemblerError::InvalidNumberOfArgumentsRange(3, 4)),
        };

        Ok((depth, note, key_off, tick_counter))
    }

    fn portamento_argument(
        &self,
        args: &[&str],
    ) -> Result<(Note, bool, i32, TickCounter), BytecodeAssemblerError> {
        let (note, key_off, velocity, length) = four_arguments(args)?;

        let note = Note::parse_bytecode_argument(note)?;
        let key_off = parse_key_off(key_off)?;
        let tick_counter = parse_length(length)?;

        let direction: i32 = match velocity.chars().next() {
            Some('+') => 1,
            Some('-') => -1,
            _ => {
                return Err(BytecodeAssemblerError::InvalidPortamentoVelocity(
                    velocity.to_owned(),
                ))
            }
        };

        if velocity.is_empty() {
            return Err(BytecodeAssemblerError::InvalidPortamentoVelocity(
                velocity.to_owned(),
            ));
        }
        let vel_number = &velocity[1..];

        let velocity: i32 = match vel_number.parse() {
            Ok(i) => i,
            Err(_) => {
                return Err(BytecodeAssemblerError::InvalidPortamentoVelocity(
                    velocity.to_owned(),
                ))
            }
        };

        Ok((note, key_off, velocity * direction, tick_counter))
    }

    fn adsr_argument(&self, args: &[&str]) -> Result<Adsr, BytecodeAssemblerError> {
        let args = four_arguments(args)?;

        Ok(Adsr::from_strs(args.0, args.1, args.2, args.3)?)
    }

    fn gain_argument(&self, args: &[&str]) -> Result<Gain, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        let gain = Gain::from_str(arg)?;

        Ok(gain)
    }

    fn _find_instrument(&self, arg: &str) -> Result<InstrumentId, BytecodeAssemblerError> {
        match self.instruments.get(arg) {
            Some(i) => Ok(*i),
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
        let adsr = Adsr::from_strs(a1, a2, a3, a4)?;

        Ok((inst, adsr))
    }

    fn instrument_and_gain_argument(
        &self,
        args: &[&str],
    ) -> Result<(InstrumentId, Gain), BytecodeAssemblerError> {
        let (inst, gain) = two_arguments(args)?;

        let inst = self._find_instrument(inst)?;
        let gain = Gain::from_str(gain)?;

        Ok((inst, gain))
    }

    fn subroutine_argument(&self, args: &[&str]) -> Result<SubroutineId, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        match self.subroutines {
            Some(subroutines) => match subroutines.get(arg) {
                Some(s) => Ok(*s),
                None => Err(BytecodeAssemblerError::UnknownSubroutine(arg.to_owned())),
            },
            // ::TODO is this correct?::
            None => Err(BytecodeAssemblerError::UnknownSubroutine(arg.to_owned())),
        }
    }

    fn tick_clock_argument(&self, args: &[&str]) -> Result<TickClock, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        Ok(TickClock::new_from_str(arg)?)
    }

    fn u32_argument(&self, args: &[&str]) -> Result<u32, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        parse_u32(arg)
    }

    fn i32_argument(&self, args: &[&str]) -> Result<i32, BytecodeAssemblerError> {
        let arg = one_argument(args)?;

        parse_i32(arg)
    }

    fn two_u32_arguments(&self, args: &[&str]) -> Result<(u32, u32), BytecodeAssemblerError> {
        let (arg1, arg2) = two_arguments(args)?;

        Ok((parse_u32(arg1)?, parse_u32(arg2)?))
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

fn parse_key_off(key_off: &str) -> Result<bool, BytecodeAssemblerError> {
    match key_off {
        "keyoff" => Ok(true),

        "no_keyoff" => Ok(false),
        "nko" => Ok(false),
        "slur_next" => Ok(false),
        "sn" => Ok(false),

        _ => Err(BytecodeAssemblerError::InvalidKeyoffArgument(
            key_off.to_owned(),
        )),
    }
}

fn parse_length(length: &str) -> Result<TickCounter, BytecodeAssemblerError> {
    match length.parse() {
        Ok(i) => Ok(TickCounter::new(i)),
        Err(_) => Err(BytecodeAssemblerError::CannotParseTickCounter(
            length.to_owned(),
        )),
    }
}

fn parse_u32(s: &str) -> Result<u32, BytecodeAssemblerError> {
    match s.parse() {
        Ok(i) => Ok(i),
        Err(_) => Err(BytecodeAssemblerError::CannotParseUnsigned(s.to_owned())),
    }
}

fn parse_i32(s: &str) -> Result<i32, BytecodeAssemblerError> {
    match s.parse() {
        Ok(i) => Ok(i),
        Err(_) => Err(BytecodeAssemblerError::CannotParseSigned(s.to_owned())),
    }
}
