//! Sound Effects compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{opcodes, BcTerminator, BytecodeContext};
use crate::bytecode_assembler::BytecodeAssembler;
use crate::command_compiler::channel_bc_generator::{self, CommandCompiler};
use crate::command_compiler::subroutines::subroutine_compile_order;
use crate::data::{
    DefaultSfxFlags, InstrumentOrSample, Name, UniqueNamesList, UniqueSoundEffectExportOrder,
};
use crate::driver_constants::{
    COMMON_DATA_BYTES_PER_SFX_SUBROUTINE, COMMON_DATA_BYTES_PER_SOUND_EFFECT, MAX_COMMON_DATA_SIZE,
};
use crate::echo::EchoEdl;
use crate::errors::{
    ChannelError, CombineSoundEffectsError, ErrorWithPos, OtherSfxError, SfxSubroutineErrors,
    SoundEffectError, SoundEffectErrorList, SoundEffectsFileError,
};
use crate::file_pos::{blank_file_range, split_lines};
use crate::identifier::ChannelId;
use crate::mml::note_tracking::{CommandTickTracker, CursorTracker};
use crate::mml::{self, CursorTrackerGetter};
use crate::pitch_table::PitchTable;
use crate::sfx_file::SoundEffectsFile;
use crate::subroutines::{CompiledSubroutines, SubroutineNameMap, SubroutineState};
use crate::time::{TickClock, TickCounter};

use std::collections::HashMap;
use std::time::Duration;

// Offset to add to the duration field of SfxHeader
// Should prevent `sfx_remainingTicks` from underflowing in the audio-driver.
const SFX_HEADER_DURATION_OFFSET: u32 = 32;

pub const MAX_SFX_TICKS: TickCounter = TickCounter::new(0x7fff - SFX_HEADER_DURATION_OFFSET);

const COMMENT_CHAR: char = ';';

#[derive(Debug, Clone, PartialEq)]
pub struct SfxSubroutinesMml(pub String);

#[derive(Debug)]
pub struct CompiledSfxSubroutines {
    data: Vec<u8>,

    subroutines: CompiledSubroutines,

    name_map: HashMap<String, usize>,

    cursor_tracker: CursorTracker,
}

impl CompiledSfxSubroutines {
    pub fn blank() -> Self {
        Self {
            data: Vec::new(),
            subroutines: CompiledSubroutines::new_blank(),
            name_map: HashMap::new(),
            cursor_tracker: CursorTracker::new(),
        }
    }

    pub fn subroutines(&self) -> &CompiledSubroutines {
        &self.subroutines
    }

    pub fn cad_data_len(&self) -> usize {
        self.data.len() + self.subroutines.len() * COMMON_DATA_BYTES_PER_SFX_SUBROUTINE
    }

    pub fn cursor_tracker(&self) -> &CursorTracker {
        &self.cursor_tracker
    }

    pub(crate) fn bytecode_data(&self) -> &[u8] {
        &self.data
    }

    fn bytecode_addr_table_iter(&self, addr: u16) -> impl Iterator<Item = u16> + '_ {
        assert!(self.data.len() + usize::from(addr) < MAX_COMMON_DATA_SIZE);

        self.subroutines.iter().map(move |s| match s {
            (_, SubroutineState::Compiled(s)) => s.bytecode_offset + addr,
            _ => panic!("subroutine has errors"),
        })
    }

    pub(crate) fn sfx_subroutines_l_iter(&self, addr: u16) -> impl Iterator<Item = u8> + '_ {
        self.bytecode_addr_table_iter(addr)
            .map(|o| o.to_le_bytes()[0])
    }

    pub(crate) fn sfx_subroutines_h_iter(&self, addr: u16) -> impl Iterator<Item = u8> + '_ {
        self.bytecode_addr_table_iter(addr)
            .map(|o| o.to_le_bytes()[1])
    }
}

impl SubroutineNameMap for CompiledSfxSubroutines {
    fn find_subroutine_index(&self, name: &str) -> Option<u8> {
        self.name_map.get(name).and_then(|&i| i.try_into().ok())
    }
}

impl CursorTrackerGetter for CompiledSfxSubroutines {
    fn cursor_tracker(&self) -> Option<&CursorTracker> {
        Some(&self.cursor_tracker)
    }

    fn tick_tracker_for_channel(&self, channel: ChannelId) -> Option<&CommandTickTracker> {
        match channel {
            ChannelId::Subroutine(i) => match self.subroutines.get_compiled(i) {
                Some(s) => Some(&s.tick_tracker),
                _ => None,
            },
            ChannelId::SoundEffect => None,
            ChannelId::Channel(_) => None,
            ChannelId::MmlPrefix => None,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct SfxFlags {
    pub one_channel: Option<bool>,
    pub interruptible: Option<bool>,
}

#[derive(Debug)]
pub struct CompiledSoundEffect {
    data: Vec<u8>,
    flags: SfxFlags,
    tick_counter: TickCounter,

    cursor_tracker: Option<(CursorTracker, CommandTickTracker)>,
}

impl CompiledSoundEffect {
    pub fn bytecode(&self) -> &[u8] {
        &self.data
    }

    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn duration(&self) -> Duration {
        self.tick_counter().to_duration(TickClock::SFX_TICK_CLOCK)
    }
}

impl CursorTrackerGetter for CompiledSoundEffect {
    fn cursor_tracker(&self) -> Option<&CursorTracker> {
        self.cursor_tracker.as_ref().map(|(c, _)| c)
    }

    fn tick_tracker_for_channel(&self, channel: ChannelId) -> Option<&CommandTickTracker> {
        match channel {
            ChannelId::SoundEffect => self.cursor_tracker.as_ref().map(|(_, t)| t),
            ChannelId::Channel(_) | ChannelId::Subroutine(_) | ChannelId::MmlPrefix => None,
        }
    }
}

fn compile_mml_sound_effect(
    sfx: &str,
    data_instruments: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
    sfx_subroutines: &CompiledSfxSubroutines,
    flags: SfxFlags,
) -> Result<CompiledSoundEffect, SoundEffectErrorList> {
    let s = mml::parse_sound_effect(sfx, data_instruments, sfx_subroutines)?;

    channel_bc_generator::compile_sound_effect(
        &s.commands,
        &s.instruments,
        data_instruments,
        pitch_table,
        &sfx_subroutines.subroutines,
        s.errors,
    )
    .map(|(data, tick_counter, tick_tracker)| CompiledSoundEffect {
        data,
        tick_counter,
        flags,
        cursor_tracker: Some((s.mml_tracker, tick_tracker)),
    })
}

pub fn compile_bytecode_sound_effect(
    sfx: &str,
    instruments: &UniqueNamesList<InstrumentOrSample>,
    subroutines: &CompiledSfxSubroutines,
    flags: SfxFlags,
) -> Result<CompiledSoundEffect, SoundEffectErrorList> {
    let mut errors = Vec::new();

    let mut bc = BytecodeAssembler::new(
        instruments,
        subroutines.subroutines(),
        subroutines,
        BytecodeContext::SoundEffect,
    );

    let mut last_line_range = blank_file_range();

    for line in split_lines(sfx) {
        let line = line.trim_start().trim_comments(COMMENT_CHAR);

        if !line.text.is_empty() {
            last_line_range = line.range();

            match bc.parse_line(line.text) {
                Ok(()) => (),
                Err(e) => errors.push(ErrorWithPos(line.range(), e)),
            }
        }
    }

    let tick_counter = bc.get_tick_counter();

    let out = match bc.bytecode(BcTerminator::DisableChannel) {
        Ok(out) => Some(out),
        Err(e) => {
            errors.push(ErrorWithPos(last_line_range.clone(), e));
            None
        }
    };

    if tick_counter.is_zero() {
        errors.push(ErrorWithPos(
            last_line_range.clone(),
            ChannelError::NoTicksInSoundEffect,
        ));
    }

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            last_line_range,
            ChannelError::TooManySfxTicks(tick_counter),
        ));
    }

    if errors.is_empty() {
        let (data, bc_state) = out.unwrap();

        Ok(CompiledSoundEffect {
            data,
            flags,
            tick_counter: bc_state.tick_counter,
            cursor_tracker: None,
        })
    } else {
        Err(SoundEffectErrorList::BytecodeErrors(errors))
    }
}

pub fn compile_sfx_subroutines(
    subroutines: &SfxSubroutinesMml,
    data_instruments: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<CompiledSfxSubroutines, SfxSubroutineErrors> {
    let s = mml::parse_sfx_subroutines(&subroutines.0, data_instruments)?;

    let mut compiler = CommandCompiler::new(
        0,
        pitch_table,
        data_instruments,
        &s.instruments,
        EchoEdl::MIN,
        false,
        // ::TODO detect driver transpose in subroutines::
        false,
    );
    let mut errors = s.errors;

    let subroutines = subroutine_compile_order(s.subroutines);

    let subroutines = compiler.compile_subroutines(subroutines, &mut errors);

    if errors.is_empty() {
        let (data, _) = compiler.take_data();

        Ok(CompiledSfxSubroutines {
            data,
            name_map: subroutines
                .iter()
                .enumerate()
                .map(|(i, (name, _))| (name.as_str().to_owned(), i))
                .collect(),
            subroutines,
            cursor_tracker: s.mml_tracker,
        })
    } else {
        Err(SfxSubroutineErrors::SubroutineErrors(errors))
    }
}

pub fn compile_sound_effects_file(
    sfx_file: &SoundEffectsFile,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<(CompiledSfxSubroutines, HashMap<Name, CompiledSoundEffect>), SoundEffectsFileError> {
    let subroutines = match compile_sfx_subroutines(&sfx_file.subroutines, inst_map, pitch_table) {
        Ok(s) => s,
        Err(e) => {
            return Err(SoundEffectsFileError {
                path: sfx_file.path.clone(),
                file_name: sfx_file.file_name.clone(),
                subroutine_errors: Some(e),
                errors: Vec::new(),
            })
        }
    };

    let mut sound_effects = HashMap::with_capacity(sfx_file.sound_effects.len());

    let mut errors = Vec::new();

    for sfx in &sfx_file.sound_effects {
        let mut other_errors = Vec::new();

        let name = match sfx.name.parse::<Name>() {
            Ok(name) => {
                if sound_effects.contains_key(&name) {
                    other_errors.push(OtherSfxError::DuplicateName(name.clone()));
                }
                name
            }
            Err(_) => {
                other_errors.push(OtherSfxError::InvalidName(sfx.name.clone()));
                "sfx".parse().unwrap()
            }
        };

        let r = match &sfx.sfx {
            SoundEffectText::BytecodeAssembly(text) => {
                compile_bytecode_sound_effect(text, inst_map, &subroutines, sfx.flags.clone())
            }
            SoundEffectText::Mml(text) => compile_mml_sound_effect(
                text,
                inst_map,
                pitch_table,
                &subroutines,
                sfx.flags.clone(),
            ),
        };
        match r {
            Ok(s) => {
                sound_effects.insert(name, s);
            }
            Err(e) => {
                errors.push(SoundEffectError {
                    sfx_name: sfx.name.clone(),
                    sfx_line_no: sfx.line_no + 1,
                    other_errors,
                    errors: e,
                });
            }
        }
    }

    if errors.is_empty() {
        Ok((subroutines, sound_effects))
    } else {
        Err(SoundEffectsFileError {
            path: sfx_file.path.clone(),
            file_name: sfx_file.file_name.clone(),
            subroutine_errors: None,
            errors,
        })
    }
}

pub(crate) struct SfxHeader {
    one_channel_flag: bool,
    offset: u16,
    duration_and_interrupt_flag: u16,
}

pub struct CombinedSoundEffectsData {
    pub(crate) n_high_priority_sfx: u8,
    pub(crate) low_priority_index: u8,
    pub(crate) sfx_header: Vec<SfxHeader>,
    pub(crate) sfx_data: Vec<u8>,
}

impl CombinedSoundEffectsData {
    pub fn sfx_data_size(&self) -> usize {
        self.sfx_header.len() * COMMON_DATA_BYTES_PER_SOUND_EFFECT + self.sfx_data.len()
    }

    pub(crate) fn last_offset(&self) -> Option<u16> {
        self.sfx_header.last().map(|h| h.offset)
    }

    pub(crate) fn sfx_header_addr_and_one_channel_flag_l_iter(
        &self,
        sfx_data_addr: u16,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header
            .iter()
            .map(move |h| (h.offset + sfx_data_addr).to_le_bytes()[0])
    }

    pub(crate) fn sfx_header_addr_and_one_channel_flag_h_iter(
        &self,
        sfx_data_addr: u16,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header.iter().map(move |h| {
            const ONE_CHANNEL_BIT: u8 = 7;
            let flags = u8::from(h.one_channel_flag) << ONE_CHANNEL_BIT;

            let addr_h = (h.offset + sfx_data_addr).to_le_bytes()[1];
            assert!(
                addr_h & (1 << ONE_CHANNEL_BIT) == 0,
                "addr_h must not set one-channel flag"
            );

            addr_h | flags
        })
    }

    pub(crate) fn sfx_header_duration_and_interrupt_flag_l_iter(
        &self,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header
            .iter()
            .map(|h| (h.duration_and_interrupt_flag).to_le_bytes()[0])
    }

    pub(crate) fn sfx_header_duration_and_interrupt_flag_h_iter(
        &self,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header
            .iter()
            .map(|h| (h.duration_and_interrupt_flag).to_le_bytes()[1])
    }
}

pub trait CompiledSfxMap {
    fn is_empty(&self) -> bool;
    fn get(&self, name: &Name) -> Option<&CompiledSoundEffect>;
}

impl CompiledSfxMap for HashMap<Name, CompiledSoundEffect> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn get(&self, name: &Name) -> Option<&CompiledSoundEffect> {
        self.get(name)
    }
}

pub trait SfxExportOrder {
    fn n_sound_effects(&self) -> usize;
    fn export_order(&self) -> &[Name];
    fn n_high_priority_sfx(&self) -> usize;
    fn low_priority_index(&self) -> usize;
}

impl SfxExportOrder for UniqueSoundEffectExportOrder {
    fn n_sound_effects(&self) -> usize {
        self.export_order.len()
    }

    fn export_order(&self) -> &[Name] {
        self.export_order.list()
    }

    fn n_high_priority_sfx(&self) -> usize {
        self.n_high_priority_sfx
    }

    fn low_priority_index(&self) -> usize {
        self.low_priority_index
    }
}

pub fn combine_sound_effects(
    sfx_map: &impl CompiledSfxMap,
    sfx_export_order: &impl SfxExportOrder,
    default_flags: DefaultSfxFlags,
) -> Result<CombinedSoundEffectsData, CombineSoundEffectsError> {
    let export_order = sfx_export_order.export_order();

    if sfx_map.is_empty() && !export_order.is_empty() {
        return Err(CombineSoundEffectsError::NoSoundEffectFile);
    }

    let n_high_priority_sfx = match sfx_export_order.n_high_priority_sfx().try_into() {
        Ok(i) => i,
        Err(_) => return Err(CombineSoundEffectsError::InvalidNumberOfHighPrioritySfx),
    };

    let low_priority_index = match sfx_export_order.low_priority_index().try_into() {
        Ok(i) => i,
        Err(_) => return Err(CombineSoundEffectsError::InvalidLowPriorityIndex),
    };

    if n_high_priority_sfx > low_priority_index {
        return Err(CombineSoundEffectsError::InvalidLowPriorityIndex);
    }

    let mut sfx_header = Vec::with_capacity(sfx_export_order.n_sound_effects());
    let mut sfx_data = Vec::new();

    let mut missing = Vec::new();

    for name in export_order {
        match sfx_map.get(name) {
            Some(s) => {
                const FLAG_MASK: u16 = 1 << 15;

                let t = s.tick_counter.value();
                assert!(t <= FLAG_MASK as u32);
                const _: () = assert!(MAX_SFX_TICKS.value() < FLAG_MASK as u32);

                let offset = u16::try_from(sfx_data.len()).unwrap_or(0xffff);
                let duration = u16::try_from(t + SFX_HEADER_DURATION_OFFSET).unwrap();

                assert!(duration < FLAG_MASK);

                let interruptible_flag =
                    match s.flags.interruptible.unwrap_or(default_flags.interruptible) {
                        true => FLAG_MASK,
                        false => 0,
                    };

                sfx_header.push(SfxHeader {
                    one_channel_flag: s.flags.one_channel.unwrap_or(default_flags.one_channel),
                    offset,
                    duration_and_interrupt_flag: duration | interruptible_flag,
                });
                sfx_data.extend(s.bytecode());
            }
            None => missing.push(name.as_str().to_owned()),
        }
    }

    if missing.is_empty() {
        Ok(CombinedSoundEffectsData {
            n_high_priority_sfx,
            low_priority_index,
            sfx_header,
            sfx_data,
        })
    } else {
        Err(CombineSoundEffectsError::MissingSoundEffects(missing))
    }
}

pub fn blank_compiled_sound_effects() -> CombinedSoundEffectsData {
    CombinedSoundEffectsData {
        n_high_priority_sfx: 0,
        low_priority_index: u8::MAX,
        sfx_header: Vec::new(),
        sfx_data: Vec::new(),
    }
}

/// Create a `CombinedSoundEffectsData` that contains a single blank sound-effect.
/// Used by the GUI to allocate a block of Audio-RAM that the audio-thread can write sound-effect bytecode to.
pub(crate) fn tad_gui_sfx_buffer(buffer_size: usize) -> CombinedSoundEffectsData {
    CombinedSoundEffectsData {
        n_high_priority_sfx: 0,
        low_priority_index: u8::MAX,
        sfx_header: vec![SfxHeader {
            one_channel_flag: false,
            offset: 0,
            duration_and_interrupt_flag: MAX_SFX_TICKS.value().try_into().unwrap(),
        }],
        sfx_data: vec![opcodes::DISABLE_CHANNEL; buffer_size],
    }
}

// Sound effect input
// ==================

#[derive(Clone, Debug, PartialEq)]
pub enum SoundEffectText {
    BytecodeAssembly(String),
    Mml(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SoundEffectInput {
    pub name: Name,
    pub flags: SfxFlags,
    pub sfx: SoundEffectText,
}

pub fn compile_sound_effect_input(
    input: &SoundEffectInput,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
    subroutines: &CompiledSfxSubroutines,
) -> Result<CompiledSoundEffect, SoundEffectError> {
    let r = match &input.sfx {
        SoundEffectText::BytecodeAssembly(text) => {
            compile_bytecode_sound_effect(text, inst_map, subroutines, input.flags.clone())
        }
        SoundEffectText::Mml(text) => compile_mml_sound_effect(
            text,
            inst_map,
            pitch_table,
            subroutines,
            input.flags.clone(),
        ),
    };
    match r {
        Ok(o) => Ok(o),
        Err(e) => Err(SoundEffectError {
            sfx_name: input.name.as_str().to_owned(),
            sfx_line_no: 1,
            // tad-gui ensures the name is valid and unique
            other_errors: Vec::new(),
            errors: e,
        }),
    }
}
