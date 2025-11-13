//! Command analyser

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::{ChannelCommands, Command};
use super::subroutines::SubroutineCommandsWithCompileOrder;

use crate::command_compiler::commands::{CommandWithPos, LoopAnalysis};
use crate::command_compiler::subroutines::SubroutineBitArray;
use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, MAX_SUBROUTINES, N_MUSIC_CHANNELS,
};
use crate::Transpose;

pub struct AnalysedCommands<'a> {
    pub(super) subroutines: SubroutineCommandsWithCompileOrder<'a>,
    pub(super) channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
    pub(super) subroutines_called_with_transpose: SubroutineBitArray,
}

const MAX_STACK_LOOPS: usize = BC_CHANNEL_STACK_SIZE / BC_STACK_BYTES_PER_LOOP;

struct AnalysisStackItem<'a> {
    start_loop_analysis: &'a mut LoopAnalysis,

    start_loop_transpose: Option<bool>,
    skip_last_loop_transpose: Option<bool>,
}

fn analyse_loop_commands<'a>(
    commands: &mut [CommandWithPos<'a>],
    subroutine_transpose_state: &[Option<bool>; MAX_SUBROUTINES],
) -> LoopAnalysis {
    // ::SHOULD find a way to avoid this allocation::
    let mut stack = Vec::with_capacity(MAX_STACK_LOOPS);

    let mut driver_transpose = None;

    let mut song_loop_analysis = None;

    for c in commands {
        match c.command_mut() {
            Command::StartLoop(_, a) => {
                stack.push(AnalysisStackItem {
                    start_loop_analysis: a,
                    start_loop_transpose: driver_transpose,
                    skip_last_loop_transpose: None,
                });
                driver_transpose = None;
            }
            Command::SkipLastLoop => {
                if let Some(s) = stack.last_mut() {
                    s.skip_last_loop_transpose = driver_transpose;
                }
            }
            Command::EndLoop(_, end_loop_analysis) => {
                if let Some(s) = stack.pop() {
                    *end_loop_analysis = LoopAnalysis {
                        driver_transpose: s.skip_last_loop_transpose,
                    };
                    *s.start_loop_analysis = LoopAnalysis {
                        // Do not disable transpose at the start of a loop
                        driver_transpose: match driver_transpose {
                            Some(true) => Some(true),
                            Some(false) => None,
                            None => None,
                        },
                    };

                    driver_transpose = s
                        .skip_last_loop_transpose
                        .or(driver_transpose)
                        .or(s.start_loop_transpose);
                }
            }
            Command::SetLoopPoint(a) => {
                song_loop_analysis = Some(a);
            }
            Command::SetTranspose(Transpose::ZERO) => driver_transpose = Some(false),
            Command::SetTranspose(_) => driver_transpose = Some(true),
            Command::AdjustTranspose(_) => driver_transpose = Some(true),
            Command::CallSubroutine(i, _) => {
                if let Some(t) = subroutine_transpose_state[usize::from(*i)] {
                    driver_transpose = Some(t);
                }
            }
            _ => (),
        }
    }

    if let Some(a) = song_loop_analysis {
        *a = LoopAnalysis { driver_transpose };
    }

    LoopAnalysis { driver_transpose }
}

fn analyse_subroutine_calls(
    called_with_transpose: bool,
    commands: &[CommandWithPos<'_>],
    subroutine_transpose_state: &[Option<bool>; MAX_SUBROUTINES],
    subroutines_called_with_transpose: &mut SubroutineBitArray,
) {
    let mut driver_transpose = called_with_transpose;

    for c in commands {
        match c.command() {
            Command::StartLoop(_, a) | Command::EndLoop(_, a) | Command::SetLoopPoint(a) => {
                if let Some(t) = a.driver_transpose {
                    driver_transpose = t;
                }
            }
            Command::SetTranspose(Transpose::ZERO) => driver_transpose = false,
            Command::SetTranspose(_) => driver_transpose = true,
            Command::AdjustTranspose(_) => driver_transpose = true,
            Command::CallSubroutine(i, _) => {
                if driver_transpose {
                    subroutines_called_with_transpose.set_bit(*i);
                }

                if let Some(t) = subroutine_transpose_state[usize::from(*i)] {
                    driver_transpose = t;
                }
            }
            _ => (),
        }
    }
}

fn analyse_song_subroutine_calls(
    subroutines: &SubroutineCommandsWithCompileOrder<'_>,
    channels: Option<&[Option<ChannelCommands<'_>>; N_MUSIC_CHANNELS]>,
    subroutine_transpose_state: &[Option<bool>; MAX_SUBROUTINES],
) -> SubroutineBitArray {
    let mut subroutines_called_with_transpose = SubroutineBitArray::new_all_clear();

    // Processed in reverse compile order

    if let Some(channels) = channels {
        for c in channels.iter().rev().flatten() {
            analyse_subroutine_calls(
                false,
                &c.commands,
                subroutine_transpose_state,
                &mut subroutines_called_with_transpose,
            );
        }
    }

    for s in subroutines.rev_compile_iter() {
        analyse_subroutine_calls(
            subroutines_called_with_transpose.get_bit(s.index),
            &s.commands,
            subroutine_transpose_state,
            &mut subroutines_called_with_transpose,
        );
    }

    subroutines_called_with_transpose
}

pub fn analyse<'a>(
    subroutines: SubroutineCommandsWithCompileOrder<'a>,
    channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
) -> AnalysedCommands<'a> {
    let mut subroutine_transpose_state = [None; MAX_SUBROUTINES];

    let subroutines = {
        let mut subroutines = subroutines;

        for i in 0..subroutines.len() {
            let s = subroutines.get_compile_order(i);
            s.analysis = analyse_loop_commands(&mut s.commands, &subroutine_transpose_state);
            subroutine_transpose_state[usize::from(s.index)] = s.analysis.driver_transpose;
        }
        subroutines
    };

    let channels = {
        let mut channels = channels;
        if let Some(channels) = &mut channels {
            for c in channels.iter_mut().flatten() {
                analyse_loop_commands(&mut c.commands, &subroutine_transpose_state);
            }
        }
        channels
    };

    let subroutines_called_with_transpose =
        analyse_song_subroutine_calls(&subroutines, channels.as_ref(), &subroutine_transpose_state);

    AnalysedCommands {
        subroutines,
        channels,
        subroutines_called_with_transpose,
    }
}
