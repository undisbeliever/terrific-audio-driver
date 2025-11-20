//! Command parser functions

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::Command;

use crate::bytecode::InstrumentId;
use crate::command_compiler::commands::SubroutineCallType;
use crate::data::{self, UniqueNamesList};
use crate::envelope::Envelope;
use crate::errors::{BytecodeError, ChannelError};
use crate::identifier::ChannelId;
use crate::subroutines::SubroutineNameMap;
use crate::{bytecode_assembler, Transpose};

fn parse_set_instrument_asm<'a>(
    name: &str,
    envelope: Option<Envelope>,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<Command<'a>, ChannelError> {
    match data_instruments.get_with_index(name) {
        Some((i, _inst)) => match InstrumentId::try_from(i) {
            Ok(inst) => Ok(Command::SetInstrumentAsm(inst, envelope)),
            Err(_) => Err(BytecodeError::InvalidInstrumentId.into()),
        },
        None => Err(BytecodeError::UnknownInstrument(name.to_owned()).into()),
    }
}

pub fn parse_call_subroutine_command<'a>(
    id: &str,
    call_type: SubroutineCallType,
    subroutines: &dyn SubroutineNameMap,
    channel_id: ChannelId,
) -> Result<Command<'a>, ChannelError> {
    match subroutines.find_subroutine_index(id) {
        Some(index) => Ok(Command::CallSubroutine(index, call_type)),

        None => match channel_id {
            ChannelId::Channel(_) | ChannelId::Subroutine(_) | ChannelId::SoundEffect => {
                Err(ChannelError::CannotFindSubroutine(id.to_owned()))
            }
            ChannelId::MmlPrefix => Err(ChannelError::CannotCallSubroutineInAnMmlPrefix),
        },
    }
}

pub fn parse_bytecode_asm_instruction<'a>(
    asm: &'a str,
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    subroutines: &dyn SubroutineNameMap,
    channel_id: ChannelId,
) -> Result<Command<'a>, ChannelError> {
    use bytecode_assembler::split_asm_args;

    match asm.split_once(|c: char| c.is_ascii_whitespace()) {
        Some((bytecode_assembler::CALL_SUBROUTINE, arg)) => parse_call_subroutine_command(
            arg.trim_start(),
            SubroutineCallType::Asm,
            subroutines,
            channel_id,
        ),
        Some((bytecode_assembler::CALL_SUBROUTINE_AND_DISABLE_VIBRATO, arg)) => {
            parse_call_subroutine_command(
                arg.trim_start(),
                SubroutineCallType::AsmDisableVibrato,
                subroutines,
                channel_id,
            )
        }
        Some((bytecode_assembler::START_LOOP, arg)) => {
            let a = bytecode_assembler::parse_uvnt(arg.trim_start())?;
            Ok(Command::StartLoop(Some(a), Default::default()))
        }
        Some((bytecode_assembler::END_LOOP, arg)) => {
            let a = bytecode_assembler::parse_uvnt(arg.trim_start())?;
            Ok(Command::EndLoop(Some(a), Default::default()))
        }
        Some((bytecode_assembler::SET_INSTRUMENT, arg)) => {
            parse_set_instrument_asm(arg.trim_start(), None, data_instruments)
        }
        Some((bytecode_assembler::SET_INSTRUMENT_AND_ADSR, arg)) => {
            let (name, a) = bytecode_assembler::instrument_and_adsr_argument(&split_asm_args(arg))?;
            parse_set_instrument_asm(name, Some(Envelope::Adsr(a)), data_instruments)
        }
        Some((bytecode_assembler::SET_INSTRUMENT_AND_GAIN, arg)) => {
            let (name, g) = bytecode_assembler::instrument_and_gain_argument(&split_asm_args(arg))?;
            parse_set_instrument_asm(name, Some(Envelope::Gain(g)), data_instruments)
        }
        Some((bytecode_assembler::SET_TRANSPOSE, arg)) => {
            let t = bytecode_assembler::parse_svnt_allow_zero(arg.trim_start())?;
            Ok(Command::SetTranspose(t))
        }
        Some((bytecode_assembler::ADJUST_TRANSPOSE, arg)) => {
            let t = bytecode_assembler::parse_svnt(arg.trim_start())?;
            Ok(Command::AdjustTranspose(t))
        }
        Some(_) => Ok(Command::BytecodeAsm(asm)),

        // Instructions with no arguments
        None => match asm {
            bytecode_assembler::START_LOOP => Ok(Command::StartLoop(None, Default::default())),
            bytecode_assembler::SKIP_LAST_LOOP => Ok(Command::SkipLastLoop),
            bytecode_assembler::END_LOOP => Ok(Command::EndLoop(None, Default::default())),
            bytecode_assembler::DISABLE_TRANSPOSE => Ok(Command::SetTranspose(Transpose::ZERO)),
            _ => Ok(Command::BytecodeAsm(asm)),
        },
    }
}
