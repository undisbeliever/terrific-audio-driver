//! Command analyser

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::{ChannelCommands, Command};
use super::subroutines::SubroutineCommandsWithCompileOrder;

use crate::command_compiler::commands::{
    CommandWithPos, InstrumentAnalysis, LoopAnalysis, SoundEffectCommands,
};
use crate::command_compiler::subroutines::SubroutineBitArray;
use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, MAX_SUBROUTINES, N_MUSIC_CHANNELS,
};
use crate::sound_effects::CompiledSfxSubroutines;
use crate::Transpose;

pub struct AnalysedCommands<'a> {
    pub(super) subroutines: SubroutineCommandsWithCompileOrder<'a>,
    pub(super) channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
    pub(super) subroutines_called_with_transpose: SubroutineBitArray,

    pub(crate) subroutine_analysis: [LoopAnalysis; MAX_SUBROUTINES],
}

pub struct AnalysedSoundEffectCommands<'a>(pub(super) SoundEffectCommands<'a>);

const MAX_STACK_LOOPS: usize = BC_CHANNEL_STACK_SIZE / BC_STACK_BYTES_PER_LOOP;

struct AnalysisStackItem<'a> {
    start_loop_analysis: &'a mut LoopAnalysis,

    start_loop_instrument: Option<InstrumentAnalysis>,
    skip_last_loop_instrument: Option<InstrumentAnalysis>,

    start_loop_transpose_active: Option<bool>,
    skip_last_loop_transpose_active: Option<bool>,
}

fn analyse_loop_commands<'a>(
    commands: &mut [CommandWithPos<'a>],
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
) -> LoopAnalysis {
    // ::SHOULD find a way to avoid this allocation::
    let mut stack = Vec::with_capacity(MAX_STACK_LOOPS);

    let mut instrument_set = false;
    let mut instrument = None;
    let mut driver_transpose_active = None;

    let mut song_loop_analysis = None;

    for c in commands {
        match c.command_mut() {
            Command::StartLoop(_, a) => {
                stack.push(AnalysisStackItem {
                    start_loop_analysis: a,
                    start_loop_instrument: instrument,
                    skip_last_loop_instrument: None,
                    start_loop_transpose_active: driver_transpose_active,
                    skip_last_loop_transpose_active: None,
                });
                instrument = None;
                driver_transpose_active = None;
            }
            Command::SkipLastLoop => {
                if let Some(s) = stack.last_mut() {
                    s.skip_last_loop_instrument = instrument;
                    s.skip_last_loop_transpose_active = driver_transpose_active;
                }
            }
            Command::EndLoop(_, end_loop_analysis) => {
                if let Some(s) = stack.pop() {
                    *end_loop_analysis = LoopAnalysis {
                        instrument: s.skip_last_loop_instrument,
                        driver_transpose_active: s.skip_last_loop_transpose_active,
                    };
                    *s.start_loop_analysis = LoopAnalysis {
                        instrument,

                        // Do not disable transpose at the start of a loop
                        driver_transpose_active: match driver_transpose_active {
                            Some(true) => Some(true),
                            Some(false) => None,
                            None => None,
                        },
                    };

                    instrument = s
                        .skip_last_loop_instrument
                        .or(instrument)
                        .or(s.start_loop_instrument);

                    driver_transpose_active = s
                        .skip_last_loop_transpose_active
                        .or(driver_transpose_active)
                        .or(s.start_loop_transpose_active);
                }
            }
            Command::SetLoopPoint(a) => {
                song_loop_analysis = Some(a);
            }

            Command::SetSubroutineInstrumentHint(i, _) => {
                if !instrument_set {
                    debug_assert!(instrument.is_none());
                    instrument = Some(InstrumentAnalysis::Hint(*i));
                }
                instrument_set = true;
            }
            Command::SetInstrument(i, _) | Command::SetInstrumentAsm(i, _) => {
                instrument = Some(InstrumentAnalysis::Set(*i));
                instrument_set = true;
            }

            Command::SetTranspose(Transpose::ZERO) => driver_transpose_active = Some(false),
            Command::SetTranspose(_) => driver_transpose_active = Some(true),
            Command::AdjustTranspose(_) => driver_transpose_active = Some(true),

            Command::CallSubroutine(i, _) => {
                let s = subroutine_analysis[usize::from(*i)];

                match s.instrument {
                    Some(InstrumentAnalysis::Set(i)) => {
                        instrument = Some(InstrumentAnalysis::Set(i));
                        instrument_set = true;
                    }
                    Some(InstrumentAnalysis::Hint(i)) => {
                        if !instrument_set {
                            debug_assert!(instrument.is_none());
                            instrument = Some(InstrumentAnalysis::Hint(i));
                        }
                        instrument_set = true;
                    }
                    None => (),
                }
                if let Some(t) = s.driver_transpose_active {
                    driver_transpose_active = Some(t);
                }
            }
            _ => (),
        }
    }

    if let Some(a) = song_loop_analysis {
        *a = LoopAnalysis {
            instrument,
            driver_transpose_active,
        };
    }

    LoopAnalysis {
        instrument,
        driver_transpose_active,
    }
}

fn analyse_subroutine_calls(
    called_with_transpose: bool,
    commands: &[CommandWithPos<'_>],
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
    subroutines_called_with_transpose: &mut SubroutineBitArray,
) {
    let mut driver_transpose_active = called_with_transpose;

    for c in commands {
        match c.command() {
            Command::StartLoop(_, a) | Command::EndLoop(_, a) | Command::SetLoopPoint(a) => {
                if let Some(t) = a.driver_transpose_active {
                    driver_transpose_active = t;
                }
            }
            Command::SetTranspose(Transpose::ZERO) => driver_transpose_active = false,
            Command::SetTranspose(_) => driver_transpose_active = true,
            Command::AdjustTranspose(_) => driver_transpose_active = true,
            Command::CallSubroutine(i, _) => {
                if driver_transpose_active {
                    subroutines_called_with_transpose.set_bit(*i);
                }

                if let Some(t) = subroutine_analysis[usize::from(*i)].driver_transpose_active {
                    driver_transpose_active = t;
                }
            }
            _ => (),
        }
    }
}

fn analyse_song_subroutine_calls(
    subroutines: &SubroutineCommandsWithCompileOrder<'_>,
    channels: Option<&[Option<ChannelCommands<'_>>; N_MUSIC_CHANNELS]>,
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
) -> SubroutineBitArray {
    let mut subroutines_called_with_transpose = SubroutineBitArray::new_all_clear();

    // Processed in reverse compile order

    if let Some(channels) = channels {
        for c in channels.iter().rev().flatten() {
            analyse_subroutine_calls(
                false,
                &c.commands,
                subroutine_analysis,
                &mut subroutines_called_with_transpose,
            );
        }
    }

    for s in subroutines.rev_compile_iter() {
        analyse_subroutine_calls(
            subroutines_called_with_transpose.get_bit(s.index),
            &s.commands,
            subroutine_analysis,
            &mut subroutines_called_with_transpose,
        );
    }

    subroutines_called_with_transpose
}

pub fn analyse_sound_effect_commands<'a>(
    sfx: SoundEffectCommands<'a>,
    sfx_subroutines: &CompiledSfxSubroutines,
) -> AnalysedSoundEffectCommands<'a> {
    let mut sfx = sfx;

    analyse_loop_commands(
        &mut sfx.commands.commands,
        sfx_subroutines.subroutine_analysis_array(),
    );

    AnalysedSoundEffectCommands(sfx)
}

pub(crate) fn blank_subroutine_analysis_array() -> [LoopAnalysis; 255] {
    [LoopAnalysis {
        instrument: None,
        driver_transpose_active: None,
    }; MAX_SUBROUTINES]
}

pub fn analyse<'a>(
    subroutines: SubroutineCommandsWithCompileOrder<'a>,
    channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
) -> AnalysedCommands<'a> {
    let mut subroutine_analysis = [LoopAnalysis {
        instrument: None,
        driver_transpose_active: None,
    }; MAX_SUBROUTINES];

    let subroutines = {
        let mut subroutines = subroutines;

        for i in 0..subroutines.len() {
            let s = subroutines.get_compile_order(i);
            let a = analyse_loop_commands(&mut s.commands, &subroutine_analysis);

            subroutine_analysis[usize::from(s.index)] = a;
            s.analysis = a;
        }
        subroutines
    };

    let channels = {
        let mut channels = channels;
        if let Some(channels) = &mut channels {
            for c in channels.iter_mut().flatten() {
                analyse_loop_commands(&mut c.commands, &subroutine_analysis);
            }
        }
        channels
    };

    let subroutines_called_with_transpose =
        analyse_song_subroutine_calls(&subroutines, channels.as_ref(), &subroutine_analysis);

    AnalysedCommands {
        subroutines,
        channels,
        subroutines_called_with_transpose,
        subroutine_analysis,
    }
}
