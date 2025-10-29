//! MML bytecode generator

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::identifier::IdentifierStr;
use super::tokenizer::MmlTokens;
use super::{ChannelId, MmlSoundEffect, CHANNEL_NAMES};

use crate::data::{self, UniqueNamesList};
use crate::echo::EchoEdl;
use crate::mml::command_parser::parse_mml_tokens;
use crate::mml::metadata::GlobalSettings;
use crate::mml::{CursorTracker, MmlPrefixData, MAX_MML_PREFIX_TICKS};
use crate::songs::{BytecodePos, SongBcTracking};

use crate::bytecode::{BcTerminator, BytecodeContext, SubroutineId};
use crate::command_compiler::channel_bc_generator::{ChannelBcGenerator, MpState};
use crate::command_compiler::commands::{
    Command, CommandWithPos, MmlInstrument, SubroutineCallType,
};
use crate::errors::{ChannelError, ErrorWithPos, MmlChannelError};
use crate::pitch_table::PitchTable;
use crate::songs::Channel;
use crate::sound_effects::{CompiledSfxSubroutines, MAX_SFX_TICKS};
use crate::subroutines::{FindSubroutineResult, NoSubroutines, Subroutine, SubroutineStore};
use crate::time::TickCounterWithLoopFlag;

use std::collections::HashMap;

struct SongSubroutines<'a> {
    vec: Vec<Subroutine>,
    id_map: HashMap<IdentifierStr<'a>, Option<SubroutineId>>,
    name_map: &'a HashMap<IdentifierStr<'a>, usize>,
}

impl SubroutineStore for SongSubroutines<'_> {
    fn get(&self, index: usize) -> Option<&Subroutine> {
        self.vec.get(index)
    }

    fn find_subroutine<'a, 'b>(&'a self, name: &'b str) -> FindSubroutineResult<'b>
    where
        'a: 'b,
    {
        let name = IdentifierStr::from_str(name);

        match self.id_map.get(&name) {
            Some(Some(s)) => FindSubroutineResult::Found(s),
            Some(None) => {
                // Subroutine has been compiled, but it contains an error
                FindSubroutineResult::NotCompiled
            }
            None => match self.name_map.get(&name) {
                Some(_) => FindSubroutineResult::Recussion,
                None => FindSubroutineResult::NotFound,
            },
        }
    }
}

pub struct MmlSongBytecodeGenerator<'a> {
    song_data: Vec<u8>,

    global_settings: &'a GlobalSettings,
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    mml_instruments: &'a Vec<MmlInstrument>,
    mml_instrument_map: HashMap<IdentifierStr<'a>, usize>,

    max_edl: EchoEdl,

    subroutines: SongSubroutines<'a>,

    is_song: bool,

    first_channel_bc_offset: Option<u16>,
    cursor_tracker: CursorTracker,

    bytecode_tracker: Vec<BytecodePos>,
}

impl<'a> MmlSongBytecodeGenerator<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        global_settings: &'a GlobalSettings,
        pitch_table: &'a PitchTable,
        mml_file: &'a str,
        data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        instruments: &'a Vec<MmlInstrument>,
        instrument_map: HashMap<IdentifierStr<'a>, usize>,
        subroutine_name_map: &'a HashMap<IdentifierStr<'a>, usize>,
        max_edl: EchoEdl,
        header_size: usize,
        is_song: bool,
    ) -> Self {
        Self {
            song_data: vec![0; header_size],
            global_settings,
            pitch_table,
            mml_file,
            data_instruments,
            mml_instruments: instruments,
            mml_instrument_map: instrument_map,

            is_song,

            max_edl,
            subroutines: SongSubroutines {
                vec: Vec::new(),
                id_map: HashMap::new(),
                name_map: subroutine_name_map,
            },

            first_channel_bc_offset: None,
            cursor_tracker: CursorTracker::new(),
            bytecode_tracker: Vec::new(),
        }
    }

    pub(crate) fn take_sfx_subroutine_data(self) -> CompiledSfxSubroutines {
        assert!(!self.is_song);

        CompiledSfxSubroutines::new(self.song_data, self.subroutines.vec, self.cursor_tracker)
    }

    pub(crate) fn take_data(self) -> (Vec<u8>, Vec<Subroutine>, SongBcTracking) {
        assert!(self.is_song);

        (
            self.song_data,
            self.subroutines.vec,
            SongBcTracking {
                bytecode: self.bytecode_tracker,
                cursor_tracker: self.cursor_tracker,
                first_channel_bc_offset: self.first_channel_bc_offset.unwrap_or(u16::MAX),
            },
        )
    }

    fn parse_and_compile_tail_call<'b>(
        commands: &'b [CommandWithPos],
        gen: &mut ChannelBcGenerator,
        bytecode_tracker: &mut Vec<BytecodePos>,
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) -> Option<&'b CommandWithPos> {
        let (last, remaining) = commands.split_last()?;

        for c in remaining {
            Self::_compile_command(c, gen, bytecode_tracker, errors);
        }

        match last.command() {
            Command::CallSubroutine(..) => Some(last),
            _ => {
                Self::_compile_command(last, gen, bytecode_tracker, errors);
                None
            }
        }
    }

    fn compile_commands(
        commands: &[CommandWithPos],
        gen: &mut ChannelBcGenerator,
        bytecode_tracker: &mut Vec<BytecodePos>,
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) {
        for c in commands {
            Self::_compile_command(c, gen, bytecode_tracker, errors);
        }
    }

    fn _compile_command(
        c: &CommandWithPos,
        gen: &mut ChannelBcGenerator,
        bytecode_tracker: &mut Vec<BytecodePos>,
        errors: &mut Vec<ErrorWithPos<ChannelError>>,
    ) {
        match gen.process_command(c) {
            Ok(()) => (),
            Err(e) => errors.push(ErrorWithPos(c.pos().clone(), e)),
        }

        bytecode_tracker.push(BytecodePos {
            bc_end_pos: gen
                .bytecode()
                .get_bytecode_len()
                .try_into()
                .unwrap_or(0xffff),
            char_index: c.pos().index_start,
        });
    }

    pub fn parse_and_compile_subroutione(
        &mut self,
        identifier: IdentifierStr<'a>,
        tokens: MmlTokens,
        song_uses_driver_transpose: bool,
    ) -> Result<(), MmlChannelError> {
        // Index in SongData, not mml file
        let song_subroutine_index = self.subroutines.vec.len().try_into().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        let (commands, mut errors) = parse_mml_tokens(
            ChannelId::Subroutine(song_subroutine_index),
            tokens,
            &self.mml_instrument_map,
            &self.subroutines,
            self.global_settings,
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.mml_file,
            self.data_instruments,
            self.mml_instruments,
            &self.subroutines,
            match self.is_song {
                true => BytecodeContext::SongSubroutine {
                    max_edl: self.max_edl,
                },
                false => BytecodeContext::SfxSubroutine,
            },
            song_uses_driver_transpose,
        );

        let tail_call = Self::parse_and_compile_tail_call(
            &commands.commands,
            &mut gen,
            &mut self.bytecode_tracker,
            &mut errors,
        );

        // ::TODO refactor and move into ChannelBcGenerator::
        let (terminator, tail_call_end_pos) = match (
            &gen.mp_state(),
            gen.bytecode().get_state().vibrato.is_active(),
        ) {
            (MpState::Mp(_), true) | (MpState::Disabled, true) => {
                // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                // less Audio-RAM then `disable_vibraro` + `goto_relative``
                if let Some(tc) = tail_call {
                    Self::_compile_command(tc, &mut gen, &mut self.bytecode_tracker, &mut errors);
                }
                (BcTerminator::ReturnFromSubroutineAndDisableVibrato, None)
            }
            (MpState::Mp(_), false) | (MpState::Disabled, false) | (MpState::Manual, _) => {
                match tail_call {
                    Some(tc) => match tc.command() {
                        Command::CallSubroutine(
                            s,
                            SubroutineCallType::Mml | SubroutineCallType::Asm,
                        ) => {
                            let sub = self.subroutines.vec.get(*s).unwrap();
                            (
                                BcTerminator::TailSubroutineCall(
                                    sub.bytecode_offset.into(),
                                    &sub.subroutine_id,
                                ),
                                Some(tc.end_pos()),
                            )
                        }
                        Command::CallSubroutine(_, SubroutineCallType::AsmDisableVibrato) => {
                            // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                            // less Audio-RAM then `disable_vibraro` + `goto_relative``
                            Self::_compile_command(
                                tc,
                                &mut gen,
                                &mut self.bytecode_tracker,
                                &mut errors,
                            );
                            (BcTerminator::ReturnFromSubroutine, None)
                        }
                        _ => panic!("tail_call is not a CallSubroutine command"),
                    },
                    None => (BcTerminator::ReturnFromSubroutine, None),
                }
            }
        };

        assert!(gen.loop_point().is_none());

        let (bc, mut tick_tracker) = gen.take_bytecode_and_tick_tracker();

        let (bc_data, bc_state) = match bc.bytecode(terminator) {
            Ok((d, s)) => (d, Some(s)),
            Err((e, d)) => {
                errors.push(ErrorWithPos(
                    commands.end_pos_range(),
                    ChannelError::BytecodeError(e),
                ));
                (d, None)
            }
        };
        self.song_data = bc_data;

        if let (Some(end_pos), Some(bc_state)) = (tail_call_end_pos, &bc_state) {
            tick_tracker.push(
                end_pos,
                TickCounterWithLoopFlag {
                    ticks: bc_state.tick_counter,
                    in_loop: false,
                },
            );
        }

        match (errors.is_empty(), bc_state) {
            (true, Some(bc_state)) => {
                let changes_song_tempo = !bc_state.tempo_changes.is_empty();
                let subroutine_id = SubroutineId::new(song_subroutine_index, bc_state);

                self.subroutines
                    .id_map
                    .insert(identifier, Some(subroutine_id.clone()));

                self.subroutines.vec.push(Subroutine {
                    identifier: identifier.to_owned(),
                    bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                    subroutine_id,
                    changes_song_tempo,
                    tick_tracker,
                });

                Ok(())
            }
            _ => {
                self.subroutines.id_map.insert(identifier, None);

                Err(MmlChannelError {
                    identifier: identifier.to_owned(),
                    errors,
                })
            }
        }
    }

    pub fn parse_and_compile_song_channel(
        &mut self,
        tokens: MmlTokens,
        channel_index: usize,
        song_uses_driver_transpose: bool,
    ) -> Result<Channel, MmlChannelError> {
        assert!(self.is_song);

        let identifier = IdentifierStr::try_from_name(CHANNEL_NAMES[channel_index]).unwrap();
        assert!(identifier.as_str().len() == 1);
        let channel_char = identifier.as_str().chars().next().unwrap();
        let channel_index = channel_index.try_into().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        if self.first_channel_bc_offset.is_none() {
            self.first_channel_bc_offset = sd_start_index.try_into().ok();
        }

        let (commands, mut errors) = parse_mml_tokens(
            ChannelId::Channel(channel_char),
            tokens,
            &self.mml_instrument_map,
            &self.subroutines,
            self.global_settings,
            &mut self.cursor_tracker,
        );

        let mut gen = ChannelBcGenerator::new(
            song_data,
            self.pitch_table,
            self.mml_file,
            self.data_instruments,
            self.mml_instruments,
            &self.subroutines,
            BytecodeContext::SongChannel {
                index: channel_index,
                max_edl: self.max_edl,
            },
            song_uses_driver_transpose,
        );

        Self::compile_commands(
            &commands.commands,
            &mut gen,
            &mut self.bytecode_tracker,
            &mut errors,
        );

        let loop_point = gen.loop_point();
        let tick_counter = gen.bytecode().get_tick_counter();

        let terminator = match gen.loop_point() {
            None => BcTerminator::DisableChannel,
            Some(lp) => {
                if lp.tick_counter == tick_counter {
                    errors.push(ErrorWithPos(
                        commands.end_pos_range(),
                        ChannelError::NoTicksAfterLoopPoint,
                    ));
                }
                BcTerminator::Goto(lp.bytecode_offset)
            }
        };

        let (bc, tick_tracker) = gen.take_bytecode_and_tick_tracker();

        let (bc_data, bc_state) = match bc.bytecode(terminator) {
            Ok((b, s)) => (b, Some(s)),
            Err((e, b)) => {
                errors.push(ErrorWithPos(
                    commands.end_pos_range(),
                    ChannelError::BytecodeError(e),
                ));
                (b, None)
            }
        };
        self.song_data = bc_data;

        match (errors.is_empty(), bc_state) {
            (true, Some(bc_state)) => Ok(Channel {
                name: identifier.as_str().chars().next().unwrap(),
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                loop_point,
                tick_counter: bc_state.tick_counter,
                max_stack_depth: bc_state.max_stack_depth,
                tick_tracker,
                tempo_changes: bc_state.tempo_changes,
            }),
            _ => Err(MmlChannelError {
                identifier: identifier.to_owned(),
                errors,
            }),
        }
    }
}

pub fn parse_and_compile_sound_effect(
    mml_file: &str,
    tokens: MmlTokens,
    pitch_table: &PitchTable,
    mml_instruments: &[MmlInstrument],
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    instruments_map: &HashMap<IdentifierStr, usize>,
    sfx_subroutines: &CompiledSfxSubroutines,
) -> Result<MmlSoundEffect, Vec<ErrorWithPos<ChannelError>>> {
    let mut cursor_tracker = CursorTracker::new();

    let (commands, mut errors) = parse_mml_tokens(
        ChannelId::SoundEffect,
        tokens,
        instruments_map,
        sfx_subroutines,
        &GlobalSettings::default(),
        &mut cursor_tracker,
    );

    let mut gen = ChannelBcGenerator::new(
        Vec::new(),
        pitch_table,
        mml_file,
        data_instruments,
        mml_instruments,
        sfx_subroutines,
        BytecodeContext::SoundEffect,
        // ::TODO detect driver transpose in sound effects::
        false,
    );

    for c in &commands.commands {
        match gen.process_command(c) {
            Ok(()) => (),
            Err(e) => errors.push(ErrorWithPos(c.pos().clone(), e)),
        }
    }

    let tick_counter = gen.bytecode().get_tick_counter();

    assert!(gen.loop_point().is_none());

    let (bc, tick_tracker) = gen.take_bytecode_and_tick_tracker();

    let bytecode = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            errors.push(ErrorWithPos(
                commands.end_pos_range(),
                ChannelError::BytecodeError(e),
            ));
            b
        }
    };

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            commands.end_pos_range(),
            ChannelError::TooManySfxTicks(tick_counter),
        ));
    }

    if errors.is_empty() {
        Ok(MmlSoundEffect {
            bytecode,
            tick_counter,

            tick_tracker,
            cursor_tracker,
        })
    } else {
        Err(errors)
    }
}

pub fn parse_and_compile_mml_prefix(
    mml_prefix: &str,
    tokens: MmlTokens,
    pitch_table: &PitchTable,
    mml_instruments: &[MmlInstrument],
    data_instruments: &UniqueNamesList<data::InstrumentOrSample>,
    instruments_map: &HashMap<IdentifierStr, usize>,
) -> Result<MmlPrefixData, Vec<ErrorWithPos<ChannelError>>> {
    let mut cursor_tracker = CursorTracker::new();

    let (commands, mut errors) = parse_mml_tokens(
        ChannelId::MmlPrefix,
        tokens,
        instruments_map,
        &NoSubroutines(),
        &GlobalSettings::default(),
        // ::TODO remove cursor tracker here::
        &mut cursor_tracker,
    );

    let mut gen = ChannelBcGenerator::new(
        Vec::new(),
        pitch_table,
        mml_prefix,
        data_instruments,
        mml_instruments,
        &NoSubroutines(),
        BytecodeContext::MmlPrefix,
        true,
    );

    for c in &commands.commands {
        match gen.process_command(c) {
            Ok(()) => (),
            Err(e) => errors.push(ErrorWithPos(c.pos().clone(), e)),
        }
    }

    let tick_counter = gen.bytecode().get_tick_counter();

    assert!(gen.loop_point().is_none());

    let (bc, _) = gen.take_bytecode_and_tick_tracker();

    let bytecode = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            errors.push(ErrorWithPos(
                commands.end_pos_range(),
                ChannelError::BytecodeError(e),
            ));
            b
        }
    };

    if tick_counter > MAX_MML_PREFIX_TICKS {
        errors.push(ErrorWithPos(
            commands.end_pos_range(),
            ChannelError::TooManyTicksInMmlPrefix(tick_counter),
        ))
    }

    if errors.is_empty() {
        Ok(MmlPrefixData { bytecode })
    } else {
        Err(errors)
    }
}
