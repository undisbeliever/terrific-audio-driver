//! MML bytecode generator

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::command_parser::{MmlCommandWithPos, Parser};
use super::identifier::IdentifierStr;
use super::tokenizer::MmlTokens;
use super::{ChannelId, MmlSoundEffect, Section, CHANNEL_NAMES};

#[cfg(feature = "mml_tracking")]
use super::note_tracking::CursorTracker;
use crate::data::{self, UniqueNamesList};
use crate::echo::EchoEdl;
use crate::mml::{MmlPrefixData, MAX_MML_PREFIX_TICKS};
#[cfg(feature = "mml_tracking")]
use crate::songs::{BytecodePos, SongBcTracking};

use crate::bytecode::{BcTerminator, BytecodeContext, SubroutineId};
use crate::channel_bc_generator::{
    ChannelBcGenerator, Command, MmlInstrument, MpState, SubroutineCallType,
};
use crate::errors::{ChannelError, ErrorWithPos, MmlChannelError};
use crate::pitch_table::PitchTable;
use crate::songs::Channel;
use crate::sound_effects::{CompiledSfxSubroutines, MAX_SFX_TICKS};
use crate::subroutines::{FindSubroutineResult, NoSubroutines, Subroutine, SubroutineStore};
use crate::time::{ZenLen, DEFAULT_ZENLEN};

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

    default_zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    mml_file: &'a str,
    data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
    sections: &'a [Section],
    mml_instruments: &'a Vec<MmlInstrument>,
    mml_instrument_map: HashMap<IdentifierStr<'a>, usize>,

    max_edl: EchoEdl,

    subroutines: SongSubroutines<'a>,

    is_song: bool,

    #[cfg(feature = "mml_tracking")]
    first_channel_bc_offset: Option<u16>,
    #[cfg(feature = "mml_tracking")]
    cursor_tracker: CursorTracker,
    #[cfg(feature = "mml_tracking")]
    bytecode_tracker: Vec<BytecodePos>,
}

impl<'a> MmlSongBytecodeGenerator<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        default_zenlen: ZenLen,
        pitch_table: &'a PitchTable,
        mml_file: &'a str,
        data_instruments: &'a UniqueNamesList<data::InstrumentOrSample>,
        sections: &'a [Section],
        instruments: &'a Vec<MmlInstrument>,
        instrument_map: HashMap<IdentifierStr<'a>, usize>,
        subroutine_name_map: &'a HashMap<IdentifierStr<'a>, usize>,
        max_edl: EchoEdl,
        header_size: usize,
        is_song: bool,
    ) -> Self {
        Self {
            song_data: vec![0; header_size],
            default_zenlen,
            pitch_table,
            mml_file,
            data_instruments,
            sections,
            mml_instruments: instruments,
            mml_instrument_map: instrument_map,

            is_song,

            max_edl,
            subroutines: SongSubroutines {
                vec: Vec::new(),
                id_map: HashMap::new(),
                name_map: subroutine_name_map,
            },

            #[cfg(feature = "mml_tracking")]
            first_channel_bc_offset: None,
            #[cfg(feature = "mml_tracking")]
            cursor_tracker: CursorTracker::new(),
            #[cfg(feature = "mml_tracking")]
            bytecode_tracker: Vec::new(),
        }
    }

    pub(crate) fn take_sfx_subroutine_data(self) -> CompiledSfxSubroutines {
        assert!(!self.is_song);

        CompiledSfxSubroutines::new(
            self.song_data,
            self.subroutines.vec,
            #[cfg(feature = "mml_tracking")]
            self.cursor_tracker,
        )
    }

    #[cfg(feature = "mml_tracking")]
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

    #[cfg(not(feature = "mml_tracking"))]
    pub(crate) fn take_data(self) -> (Vec<u8>, Vec<Subroutine>) {
        assert!(self.is_song);

        (self.song_data, self.subroutines.vec)
    }

    fn parse_and_compile_tail_call(
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) -> Option<MmlCommandWithPos> {
        let mut next = parser.next();

        while let Some(c) = next {
            // Must process the command immediately or else the mml_tracking
            // after an end-loop or bytecode-asm MmlCommand will be incorrect.

            match &c.command() {
                Command::CallSubroutine(..) => {
                    let after = parser.next();

                    if after.is_none() {
                        return Some(c);
                    } else {
                        Self::_compile_command(
                            c,
                            parser,
                            gen,
                            #[cfg(feature = "mml_tracking")]
                            bytecode_tracker,
                        );
                    }
                    next = after;
                }
                _ => {
                    Self::_compile_command(
                        c,
                        parser,
                        gen,
                        #[cfg(feature = "mml_tracking")]
                        bytecode_tracker,
                    );

                    next = parser.next();
                }
            }
        }

        None
    }

    fn parse_and_compile(
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) {
        while let Some(c) = parser.next() {
            Self::_compile_command(
                c,
                parser,
                gen,
                #[cfg(feature = "mml_tracking")]
                bytecode_tracker,
            );
        }
    }

    fn _compile_command(
        c: MmlCommandWithPos,
        parser: &mut Parser,
        gen: &mut ChannelBcGenerator,
        #[cfg(feature = "mml_tracking")] bytecode_tracker: &mut Vec<BytecodePos>,
    ) {
        match gen.process_command(c.command()) {
            Ok(()) => (),
            Err(e) => parser.add_error_range(c.pos().clone(), e),
        }

        match c.command() {
            Command::EndLoop(_) | Command::BytecodeAsm(_) => {
                parser.set_tick_counter(gen.bytecode().get_tick_counter_with_loop_flag());
            }
            _ => (),
        }

        #[cfg(feature = "mml_tracking")]
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
    ) -> Result<(), MmlChannelError> {
        // Index in SongData, not mml file
        let song_subroutine_index = self.subroutines.vec.len().try_into().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        let mut parser = Parser::new(
            ChannelId::Subroutine(song_subroutine_index),
            tokens,
            &self.mml_instrument_map,
            &self.subroutines,
            self.default_zenlen,
            None, // No sections in subroutines
            #[cfg(feature = "mml_tracking")]
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
        );

        let tail_call = Self::parse_and_compile_tail_call(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        // ::TODO refactor and move into ChannelBcGenerator::
        let terminator = match (
            &gen.mp_state(),
            gen.bytecode().get_state().vibrato.is_active(),
        ) {
            (MpState::Mp(_), true) | (MpState::Disabled, true) => {
                // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                // less Audio-RAM then `disable_vibraro` + `goto_relative``
                if let Some(tc) = tail_call {
                    Self::_compile_command(
                        tc,
                        &mut parser,
                        &mut gen,
                        #[cfg(feature = "mml_tracking")]
                        &mut self.bytecode_tracker,
                    );
                }
                BcTerminator::ReturnFromSubroutineAndDisableVibrato
            }
            (MpState::Mp(_), false) | (MpState::Disabled, false) | (MpState::Manual, _) => {
                match tail_call {
                    Some(tc) => match tc.command() {
                        Command::CallSubroutine(
                            s,
                            SubroutineCallType::Mml | SubroutineCallType::Asm,
                        ) => {
                            let sub = self.subroutines.vec.get(*s).unwrap();
                            BcTerminator::TailSubroutineCall(
                                sub.bytecode_offset.into(),
                                &sub.subroutine_id,
                            )
                        }
                        Command::CallSubroutine(_, SubroutineCallType::AsmDisableVibrato) => {
                            // `call_subroutine_and_disable_vibrato` + `return_from_subroutine` uses
                            // less Audio-RAM then `disable_vibraro` + `goto_relative``
                            Self::_compile_command(
                                tc,
                                &mut parser,
                                &mut gen,
                                #[cfg(feature = "mml_tracking")]
                                &mut self.bytecode_tracker,
                            );
                            BcTerminator::ReturnFromSubroutine
                        }
                        _ => panic!("tail_call is not a CallSubroutine command"),
                    },
                    None => BcTerminator::ReturnFromSubroutine,
                }
            }
        };

        let last_pos = parser.peek_pos();

        assert!(gen.loop_point().is_none());

        let (bc_data, bc_state) = match gen.take_bytecode().bytecode(terminator) {
            Ok((d, s)) => (d, Some(s)),
            Err((e, d)) => {
                parser.add_error_range(last_pos.to_range(1), ChannelError::BytecodeError(e));
                (d, None)
            }
        };
        self.song_data = bc_data;

        let (_, errors) = parser.finalize();

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
    ) -> Result<Channel, MmlChannelError> {
        assert!(self.is_song);

        let identifier = IdentifierStr::try_from_name(CHANNEL_NAMES[channel_index]).unwrap();
        assert!(identifier.as_str().len() == 1);
        let channel_char = identifier.as_str().chars().next().unwrap();
        let channel_index = channel_index.try_into().unwrap();

        let song_data = std::mem::take(&mut self.song_data);
        let sd_start_index = song_data.len();

        #[cfg(feature = "mml_tracking")]
        if self.first_channel_bc_offset.is_none() {
            self.first_channel_bc_offset = sd_start_index.try_into().ok();
        }

        let mut parser = Parser::new(
            ChannelId::Channel(channel_char),
            tokens,
            &self.mml_instrument_map,
            &self.subroutines,
            self.default_zenlen,
            Some(self.sections),
            #[cfg(feature = "mml_tracking")]
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
        );

        Self::parse_and_compile(
            &mut parser,
            &mut gen,
            #[cfg(feature = "mml_tracking")]
            &mut self.bytecode_tracker,
        );

        let last_pos = parser.peek_pos();
        let loop_point = gen.loop_point();
        let tick_counter = gen.bytecode().get_tick_counter();

        let terminator = match gen.loop_point() {
            None => BcTerminator::DisableChannel,
            Some(lp) => {
                if lp.tick_counter == tick_counter {
                    parser
                        .add_error_range(last_pos.to_range(1), ChannelError::NoTicksAfterLoopPoint);
                }
                BcTerminator::Goto(lp.bytecode_offset)
            }
        };

        let (bc_data, bc_state) = match gen.take_bytecode().bytecode(terminator) {
            Ok((b, s)) => (b, Some(s)),
            Err((e, b)) => {
                parser.add_error_range(last_pos.to_range(1), ChannelError::BytecodeError(e));
                (b, None)
            }
        };
        self.song_data = bc_data;

        let (section_tick_counters, errors) = parser.finalize();

        match (errors.is_empty(), bc_state) {
            (true, Some(bc_state)) => Ok(Channel {
                name: identifier.as_str().chars().next().unwrap(),
                bytecode_offset: sd_start_index.try_into().unwrap_or(u16::MAX),
                loop_point,
                tick_counter: bc_state.tick_counter,
                max_stack_depth: bc_state.max_stack_depth,
                section_tick_counters,
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
    #[cfg(feature = "mml_tracking")]
    let mut cursor_tracker = CursorTracker::new();

    let mut parser = Parser::new(
        ChannelId::SoundEffect,
        tokens,
        instruments_map,
        sfx_subroutines,
        DEFAULT_ZENLEN,
        None, // No sections in sound effect
        #[cfg(feature = "mml_tracking")]
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
    );

    while let Some(c) = parser.next() {
        match gen.process_command(c.command()) {
            Ok(()) => (),
            Err(e) => parser.add_error_range(c.pos().clone(), e),
        }
        if matches!(c.command(), Command::EndLoop(_)) {
            parser.set_tick_counter(gen.bytecode().get_tick_counter_with_loop_flag());
        }
    }

    let last_pos = parser.peek_pos();
    let tick_counter = gen.bytecode().get_tick_counter();

    assert!(gen.loop_point().is_none());

    let bytecode = match gen.take_bytecode().bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            parser.add_error_range(last_pos.to_range(1), ChannelError::BytecodeError(e));
            b
        }
    };

    let (_, mut errors) = parser.finalize();

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            last_pos.to_range(1),
            ChannelError::TooManySfxTicks(tick_counter),
        ));
    }

    if errors.is_empty() {
        Ok(MmlSoundEffect {
            bytecode,
            tick_counter,

            #[cfg(feature = "mml_tracking")]
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
    #[cfg(feature = "mml_tracking")]
    let mut cursor_tracker = CursorTracker::new();

    let mut parser = Parser::new(
        ChannelId::MmlPrefix,
        tokens,
        instruments_map,
        &NoSubroutines(),
        DEFAULT_ZENLEN,
        None, // No sections in sound effect
        // ::TODO remove cursor tracker here::
        #[cfg(feature = "mml_tracking")]
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
    );

    while let Some(c) = parser.next() {
        match gen.process_command(c.command()) {
            Ok(()) => (),
            Err(e) => parser.add_error_range(c.pos().clone(), e),
        }
        if matches!(c.command(), Command::EndLoop(_)) {
            parser.set_tick_counter(gen.bytecode().get_tick_counter_with_loop_flag());
        }
    }

    let last_pos = parser.peek_pos();
    let tick_counter = gen.bytecode().get_tick_counter();

    assert!(gen.loop_point().is_none());

    let bytecode = match gen.take_bytecode().bytecode(BcTerminator::DisableChannel) {
        Ok((b, _)) => b,
        Err((e, b)) => {
            parser.add_error_range(last_pos.to_range(1), ChannelError::BytecodeError(e));
            b
        }
    };

    let (_, mut errors) = parser.finalize();

    if tick_counter > MAX_MML_PREFIX_TICKS {
        errors.push(ErrorWithPos(
            last_pos.to_range(1),
            ChannelError::TooManyTicksInMmlPrefix(tick_counter),
        ))
    }

    if errors.is_empty() {
        Ok(MmlPrefixData { bytecode })
    } else {
        Err(errors)
    }
}
