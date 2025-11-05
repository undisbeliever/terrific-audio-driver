//! Command analyser

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::{ChannelCommands, Command};
use super::subroutines::SubroutineCommandsWithCompileOrder;

use crate::command_compiler::commands::CommandWithPos;
use crate::driver_constants::N_MUSIC_CHANNELS;

pub struct AnalysedCommands<'a> {
    pub(super) subroutines: SubroutineCommandsWithCompileOrder<'a>,
    pub(super) uses_driver_transpose: bool,
}

pub fn analyse<'a>(
    subroutines: SubroutineCommandsWithCompileOrder<'a>,
    channels: Option<&[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
) -> AnalysedCommands<'a> {
    let test_commands = |commands: &[CommandWithPos<'a>]| -> bool {
        for c in commands {
            match c.command() {
                Command::SetTranspose(_) => return true,
                Command::AdjustTranspose(_) => return true,
                _ => (),
            }
        }
        false
    };

    let uses_driver_transpose = subroutines
        .compile_iter()
        .any(|s| test_commands(&s.commands))
        | channels.is_some_and(|channels| {
            channels
                .iter()
                .flatten()
                .any(|c| test_commands(&c.commands))
        });

    AnalysedCommands {
        subroutines,
        uses_driver_transpose,
    }
}
