//! Command analyser

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::{ChannelCommands, Command};
use super::subroutines::SubroutineCommandsWithCompileOrder;

use crate::bytecode::{LoopCount, RelativeTranspose};
use crate::command_compiler::commands::{
    CommandWithPos, InstrumentAnalysis, LoopAnalysis, SkipLastLoopAnalysis, SoundEffectCommands,
};
use crate::driver_constants::{
    BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP, MAX_SUBROUTINES, N_MUSIC_CHANNELS,
};
use crate::sound_effects::CompiledSfxSubroutines;
use crate::Transpose;

pub struct AnalysedCommands<'a> {
    pub(super) subroutines: SubroutineCommandsWithCompileOrder<'a>,
    pub(super) channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
    pub(super) transpose_at_subroutine_start: [TransposeStartRange; MAX_SUBROUTINES],

    pub(crate) subroutine_analysis: [LoopAnalysis; MAX_SUBROUTINES],
}

pub struct AnalysedSoundEffectCommands<'a>(pub(super) SoundEffectCommands<'a>);

const MAX_STACK_LOOPS: usize = BC_CHANNEL_STACK_SIZE / BC_STACK_BYTES_PER_LOOP;

// Transpose analysis is a state not a range as the start-loop state is unknown inside subroutine calls.
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct TransposeAnalysis(TransposeState);

impl TransposeAnalysis {
    pub const BLANK: Self = Self(TransposeState::Unset);
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SkipLastLoopTransposeAnalysis {
    start_of_second_last_loop: TransposeState,
    and_then: TransposeState,
}

impl SkipLastLoopTransposeAnalysis {
    pub const BLANK: Self = Self {
        start_of_second_last_loop: TransposeState::Unset,
        and_then: TransposeState::Unset,
    };
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RelativeTransposeState(i16);

impl RelativeTransposeState {
    const MIN: i16 = -255;
    const MAX: i16 = 255;

    fn new(a: i8) -> Self {
        Self(a.into())
    }

    fn checked_add(self, a: Self) -> Option<Self> {
        match self.0.checked_add(a.0) {
            Some(o @ Self::MIN..=Self::MAX) => Some(Self(o)),
            _ => None,
        }
    }

    fn check_add_to_known(self, a: i8) -> Option<i8> {
        (self.0 + i16::from(a)).try_into().ok()
    }

    fn checked_mul(self, m: i16) -> Option<Self> {
        match self.0.checked_mul(m) {
            Some(o @ Self::MIN..=Self::MAX) => Some(Self(o)),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub(crate) enum TransposeState {
    #[default]
    Unset,
    Set(i8),
    Adjust(RelativeTransposeState),
    Overflow,
}

impl TransposeState {
    #[must_use]
    fn add_adjust(self, adjust: RelativeTransposeState) -> Self {
        match self {
            Self::Unset => Self::Adjust(adjust),
            Self::Set(t) => match adjust.check_add_to_known(t) {
                Some(t) => Self::Set(t),
                None => Self::Overflow,
            },
            Self::Adjust(a) => match a.checked_add(adjust) {
                Some(t) => Self::Adjust(t),
                None => Self::Overflow,
            },
            Self::Overflow => Self::Overflow,
        }
    }

    #[must_use]
    fn loop_analysis(self, loop_count: i16) -> TransposeState {
        match self {
            TransposeState::Unset => TransposeState::Unset,
            TransposeState::Set(s) => TransposeState::Set(s),
            TransposeState::Adjust(a) => match a.checked_mul(loop_count) {
                Some(a) => TransposeState::Adjust(a),
                None => TransposeState::Overflow,
            },
            TransposeState::Overflow => TransposeState::Overflow,
        }
    }

    #[must_use]
    fn apply(self, o: Self) -> Self {
        match o {
            Self::Unset => self,
            Self::Set(s) => Self::Set(s),
            Self::Adjust(adjust) => self.add_adjust(adjust),
            Self::Overflow => Self::Overflow,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TransposeRange {
    Set { min: i8, max: i8 },
    Overflow,
}

impl TransposeRange {
    pub const DISABLED: Self = Self::Set { min: 0, max: 0 };

    pub fn new(t: TransposeStartRange) -> Self {
        t.0
    }

    pub fn is_active(&self) -> bool {
        self != &Self::Set { min: 0, max: 0 }
    }

    pub fn set(&mut self, t: Transpose) {
        *self = Self::Set {
            min: t.as_i8(),
            max: t.as_i8(),
        }
    }

    pub fn adjust(&mut self, a: RelativeTranspose) {
        match self {
            Self::Set { min, max } => {
                *self = match (a.as_i8().checked_add(*min), a.as_i8().checked_add(*max)) {
                    (Some(min), Some(max)) => Self::Set { min, max },
                    _ => Self::Overflow,
                }
            }
            Self::Overflow => (),
        }
    }

    #[must_use]
    fn loop_set(&self, t: i8) -> Self {
        match self {
            &Self::Set { min, max } => Self::Set {
                min: std::cmp::min(min, t),
                max: std::cmp::max(max, t),
            },
            Self::Overflow => Self::Overflow,
        }
    }

    #[must_use]
    fn loop_adjust(&self, a: RelativeTransposeState) -> Self {
        match self {
            &Self::Set { min, max } => match (a.check_add_to_known(min), a.check_add_to_known(max))
            {
                (Some(amin), Some(amax)) => Self::Set {
                    min: std::cmp::min(min, amin),
                    max: std::cmp::max(max, amax),
                },
                _ => Self::Overflow,
            },
            Self::Overflow => Self::Overflow,
        }
    }

    #[must_use]
    fn end_loop_adjust(&self, a: RelativeTransposeState) -> Self {
        match self {
            Self::Set { min, max } => {
                match (a.check_add_to_known(*min), a.check_add_to_known(*max)) {
                    (Some(min), Some(max)) => Self::Set { min, max },
                    _ => Self::Overflow,
                }
            }
            Self::Overflow => Self::Overflow,
        }
    }

    pub fn start_loop(&mut self, analysis: TransposeAnalysis) {
        match analysis.0 {
            TransposeState::Unset => (),
            TransposeState::Set(t) => *self = self.loop_set(t),
            TransposeState::Adjust(t) => *self = self.loop_adjust(t),
            TransposeState::Overflow => *self = TransposeRange::Overflow,
        }
    }

    pub fn skip_last_loop(
        &mut self,
        analysis: SkipLastLoopTransposeAnalysis,
        start_loop_range: Self,
    ) {
        let tr = match analysis.start_of_second_last_loop {
            TransposeState::Unset => start_loop_range,
            TransposeState::Set(t) => start_loop_range.loop_set(t),
            TransposeState::Adjust(t) => start_loop_range.loop_adjust(t),
            TransposeState::Overflow => TransposeRange::Overflow,
        };
        *self = match analysis.and_then {
            TransposeState::Unset => tr,
            TransposeState::Set(t) => TransposeRange::Set { min: t, max: t },
            TransposeState::Adjust(t) => tr.end_loop_adjust(t),
            TransposeState::Overflow => TransposeRange::Overflow,
        };
    }

    pub fn end_loop(&mut self, analysis: TransposeAnalysis, start_loop_range: Self) {
        match analysis.0 {
            TransposeState::Unset => (),
            TransposeState::Set(t) => *self = Self::Set { min: t, max: t },
            TransposeState::Adjust(t) => *self = start_loop_range.end_loop_adjust(t),
            TransposeState::Overflow => *self = TransposeRange::Overflow,
        }
    }

    pub fn subroutine_call(&mut self, analysis: TransposeAnalysis) {
        match analysis.0 {
            TransposeState::Unset => (),
            TransposeState::Set(t) => *self = Self::Set { min: t, max: t },
            TransposeState::Adjust(t) => *self = self.end_loop_adjust(t),
            TransposeState::Overflow => *self = TransposeRange::Overflow,
        }
    }

    pub fn song_loop(&mut self, analysis: TransposeAnalysis) {
        match analysis.0 {
            TransposeState::Unset => (),
            TransposeState::Set(t) => *self = self.loop_set(t),
            TransposeState::Adjust(_) => *self = TransposeRange::Overflow,
            TransposeState::Overflow => *self = TransposeRange::Overflow,
        }
    }

    fn merge(&mut self, tr: Self) {
        *self = match (*self, tr) {
            (
                TransposeRange::Set {
                    min: amin,
                    max: amax,
                },
                TransposeRange::Set {
                    min: bmin,
                    max: bmax,
                },
            ) => Self::Set {
                min: std::cmp::min(amin, bmin),
                max: std::cmp::max(amax, bmax),
            },

            (TransposeRange::Overflow, TransposeRange::Set { .. })
            | (TransposeRange::Set { .. }, TransposeRange::Overflow)
            | (TransposeRange::Overflow, TransposeRange::Overflow) => TransposeRange::Overflow,
        };
    }
}

struct AnalysisStackItem<'a> {
    start_loop_analysis: &'a mut LoopAnalysis,
    skip_last_loop_analysis: Option<&'a mut SkipLastLoopAnalysis>,

    loop_count: Option<LoopCount>,

    start_loop_instrument: Option<InstrumentAnalysis>,
    start_loop_transpose: TransposeState,

    skip_last_loop_instrument: Option<InstrumentAnalysis>,
    skip_last_loop_transpose: Option<TransposeState>,
}

fn analyse_loop_commands<'a>(
    commands: &mut [CommandWithPos<'a>],
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
) -> LoopAnalysis {
    // ::SHOULD find a way to avoid this allocation::
    let mut stack = Vec::with_capacity(MAX_STACK_LOOPS);

    let mut instrument_set = false;
    let mut instrument = None;
    let mut transpose = TransposeState::Unset;

    let mut song_loop_analysis = None;

    for c in commands {
        match c.command_mut() {
            Command::StartLoop(loop_count, a) => {
                stack.push(AnalysisStackItem {
                    start_loop_analysis: a,
                    skip_last_loop_analysis: None,

                    loop_count: *loop_count,

                    start_loop_instrument: instrument,
                    start_loop_transpose: transpose,

                    skip_last_loop_instrument: None,
                    skip_last_loop_transpose: None,
                });
                instrument = None;
                transpose = TransposeState::Unset;
            }
            Command::SkipLastLoop(analysis) => {
                if let Some(s) = stack.last_mut() {
                    if s.skip_last_loop_analysis.is_none() {
                        // Set here in the event there is no `EndLoop` commend
                        analysis.transpose.and_then = transpose;

                        s.skip_last_loop_analysis = Some(analysis);

                        s.skip_last_loop_instrument = instrument;
                        s.skip_last_loop_transpose = Some(transpose);
                    }
                }
            }
            Command::EndLoop(loop_count, end_loop_analysis) => {
                if let Some(s) = stack.pop() {
                    debug_assert_eq!(
                        s.skip_last_loop_analysis.is_some(),
                        s.skip_last_loop_analysis.is_some()
                    );

                    let loop_count = s
                        .loop_count
                        .or(*loop_count)
                        .unwrap_or(LoopCount::MIN)
                        .to_i16();

                    let start_of_last_loop_transpose = transpose.loop_analysis(loop_count - 1);

                    if let Some(skip_last_loop_analysis) = s.skip_last_loop_analysis {
                        skip_last_loop_analysis.transpose.start_of_second_last_loop =
                            if loop_count > 2 {
                                transpose.loop_analysis(loop_count - 2)
                            } else {
                                // Commands after `SkipLastLoop` are only processed once
                                TransposeState::Unset
                            }
                    }

                    let end_loop_transpose = start_of_last_loop_transpose
                        .apply(s.skip_last_loop_transpose.unwrap_or(transpose));

                    *s.start_loop_analysis = LoopAnalysis {
                        instrument,
                        transpose: TransposeAnalysis(start_of_last_loop_transpose),
                    };

                    *end_loop_analysis = LoopAnalysis {
                        instrument: s.skip_last_loop_instrument,
                        transpose: TransposeAnalysis(end_loop_transpose),
                    };

                    instrument = s
                        .skip_last_loop_instrument
                        .or(instrument)
                        .or(s.start_loop_instrument);

                    transpose = s.start_loop_transpose.apply(end_loop_transpose);
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

            Command::SetTranspose(t) => {
                transpose = TransposeState::Set(t.as_i8());
            }
            Command::AdjustTranspose(a) => {
                transpose = transpose.add_adjust(RelativeTransposeState::new(a.as_i8()));
            }

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

                transpose = transpose.apply(s.transpose.0);
            }
            _ => (),
        }
    }

    if let Some(a) = song_loop_analysis {
        *a = LoopAnalysis {
            instrument,
            transpose: TransposeAnalysis(transpose),
        };
    }

    LoopAnalysis {
        instrument,
        transpose: TransposeAnalysis(transpose),
    }
}

fn analyse_subroutine_calls(
    transpose_range: TransposeStartRange,
    commands: &[CommandWithPos<'_>],
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
    transpose_at_suboutine_call: &mut [TransposeStartRange; MAX_SUBROUTINES],
) {
    let mut transpose_range = transpose_range.0;
    let mut stack = Vec::new();

    for c in commands {
        match c.command() {
            Command::StartLoop(_, a) => {
                stack.push((transpose_range, false));
                transpose_range.start_loop(a.transpose);
            }
            Command::SkipLastLoop(a) => {
                if let Some((start_loop_range, skip_last_loop)) = stack.last_mut() {
                    if !*skip_last_loop {
                        *skip_last_loop = true;
                        transpose_range.skip_last_loop(a.transpose, *start_loop_range);
                    }
                }
            }
            Command::EndLoop(_, a) => {
                if let Some((start_loop_range, _)) = stack.pop() {
                    transpose_range.end_loop(a.transpose, start_loop_range);
                }
            }

            Command::SetTranspose(t) => transpose_range.set(*t),
            Command::AdjustTranspose(a) => transpose_range.adjust(*a),

            Command::CallSubroutine(i, _) => {
                transpose_at_suboutine_call[usize::from(*i)]
                    .0
                    .merge(transpose_range);

                transpose_range.subroutine_call(subroutine_analysis[usize::from(*i)].transpose);
            }

            Command::SetLoopPoint(a) => {
                transpose_range.song_loop(a.transpose);
            }
            _ => (),
        }
    }
}

/// The driver transpose range at the start of the subroutine or channel
#[derive(Copy, Clone)]
pub struct TransposeStartRange(TransposeRange);

impl TransposeStartRange {
    pub const DISABLED: Self = Self(TransposeRange::DISABLED);
}

fn analyse_song_subroutine_calls(
    subroutines: &SubroutineCommandsWithCompileOrder<'_>,
    channels: Option<&[Option<ChannelCommands<'_>>; N_MUSIC_CHANNELS]>,
    subroutine_analysis: &[LoopAnalysis; MAX_SUBROUTINES],
) -> [TransposeStartRange; MAX_SUBROUTINES] {
    let mut subroutine_call_transpose_range =
        [const { TransposeStartRange(TransposeRange::DISABLED) }; MAX_SUBROUTINES];

    // Processed in reverse compile order

    if let Some(channels) = channels {
        for c in channels.iter().rev().flatten() {
            analyse_subroutine_calls(
                TransposeStartRange::DISABLED,
                &c.commands,
                subroutine_analysis,
                &mut subroutine_call_transpose_range,
            );
        }
    }

    for s in subroutines.rev_compile_iter() {
        analyse_subroutine_calls(
            subroutine_call_transpose_range[usize::from(s.index)],
            &s.commands,
            subroutine_analysis,
            &mut subroutine_call_transpose_range,
        );
    }

    subroutine_call_transpose_range
}

pub fn analyse_sound_effect_commands<'a>(
    sfx: SoundEffectCommands<'a>,
    sfx_subroutines: &CompiledSfxSubroutines,
) -> AnalysedSoundEffectCommands<'a> {
    let mut sfx = sfx;

    analyse_loop_commands(
        &mut sfx.commands,
        sfx_subroutines.subroutine_analysis_array(),
    );

    AnalysedSoundEffectCommands(sfx)
}

pub(crate) fn blank_subroutine_analysis_array() -> [LoopAnalysis; 255] {
    [LoopAnalysis {
        instrument: None,
        transpose: TransposeAnalysis::BLANK,
    }; MAX_SUBROUTINES]
}

pub fn analyse<'a>(
    subroutines: SubroutineCommandsWithCompileOrder<'a>,
    channels: Option<[Option<ChannelCommands<'a>>; N_MUSIC_CHANNELS]>,
) -> AnalysedCommands<'a> {
    let mut subroutine_analysis = [LoopAnalysis {
        instrument: None,
        transpose: TransposeAnalysis::BLANK,
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

    let transpose_at_subroutine_start =
        analyse_song_subroutine_calls(&subroutines, channels.as_ref(), &subroutine_analysis);

    AnalysedCommands {
        subroutines,
        channels,
        transpose_at_subroutine_start,
        subroutine_analysis,
    }
}
