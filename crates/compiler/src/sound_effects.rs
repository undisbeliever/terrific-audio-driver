//! Sound Effects compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::{opcodes, BcTerminator, BytecodeContext};
use crate::bytecode_assembler::BytecodeAssembler;
use crate::data::{InstrumentOrSample, Name, UniqueNamesList, UniqueSoundEffectExportOrder};
use crate::driver_constants::{COMMON_DATA_BYTES_PER_SOUND_EFFECT, SFX_TICK_CLOCK};
use crate::errors::{
    BytecodeAssemblerError, CombineSoundEffectsError, ErrorWithPos, OtherSfxError,
    SoundEffectError, SoundEffectErrorList, SoundEffectsFileError,
};
use crate::file_pos::{blank_file_range, split_lines};
use crate::mml;
use crate::pitch_table::PitchTable;
use crate::sfx_file::SoundEffectsFile;
use crate::time::{TickClock, TickCounter};

use std::collections::HashMap;
use std::time::Duration;

pub const MAX_SFX_TICKS: TickCounter = TickCounter::new(0x7fff);

const COMMENT_CHAR: char = ';';

#[derive(Debug)]
enum SfxData {
    Mml(mml::MmlSoundEffect),
    BytecodeAssembly(Vec<u8>),
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct SfxFlags {
    pub interruptible: Option<bool>,
}

#[derive(Debug)]
pub struct CompiledSoundEffect {
    data: SfxData,
    flags: SfxFlags,
    tick_counter: TickCounter,
}

impl CompiledSoundEffect {
    pub fn bytecode(&self) -> &[u8] {
        match &self.data {
            SfxData::BytecodeAssembly(s) => s,
            SfxData::Mml(s) => s.bytecode(),
        }
    }

    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }

    pub fn duration(&self) -> Duration {
        self.tick_counter()
            .to_duration(TickClock::try_from(SFX_TICK_CLOCK).unwrap())
    }

    #[cfg(feature = "mml_tracking")]
    pub fn cursor_tracker(&self) -> Option<&mml::CursorTracker> {
        match &self.data {
            SfxData::BytecodeAssembly(_) => None,
            SfxData::Mml(s) => Some(s.cursor_tracker()),
        }
    }
}

fn compile_mml_sound_effect(
    sfx: &str,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
    flags: SfxFlags,
) -> Result<CompiledSoundEffect, SoundEffectErrorList> {
    match mml::compile_sound_effect(sfx, inst_map, pitch_table) {
        Ok(o) => Ok(CompiledSoundEffect {
            tick_counter: o.tick_counter(),
            flags,
            data: SfxData::Mml(o),
        }),
        Err(errors) => Err(errors),
    }
}

fn compile_bytecode_sound_effect(
    sfx: &str,
    instruments: &UniqueNamesList<InstrumentOrSample>,
    flags: SfxFlags,
) -> Result<CompiledSoundEffect, SoundEffectErrorList> {
    let mut errors = Vec::new();

    let mut bc = BytecodeAssembler::new(instruments, None, BytecodeContext::SoundEffect);

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
            BytecodeAssemblerError::NoTicksInSoundEffect,
        ));
    }

    if tick_counter > MAX_SFX_TICKS {
        errors.push(ErrorWithPos(
            last_line_range,
            BytecodeAssemblerError::TooManySfxTicks(tick_counter),
        ));
    }

    if errors.is_empty() {
        Ok(CompiledSoundEffect {
            data: SfxData::BytecodeAssembly(out.unwrap()),
            flags,
            tick_counter,
        })
    } else {
        Err(SoundEffectErrorList::BytecodeErrors(errors))
    }
}

pub fn compile_sound_effects_file(
    sfx_file: &SoundEffectsFile,
    inst_map: &UniqueNamesList<InstrumentOrSample>,
    pitch_table: &PitchTable,
) -> Result<HashMap<Name, CompiledSoundEffect>, SoundEffectsFileError> {
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
                compile_bytecode_sound_effect(text, inst_map, sfx.flags.clone())
            }
            SoundEffectText::Mml(text) => {
                compile_mml_sound_effect(text, inst_map, pitch_table, sfx.flags.clone())
            }
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
        Ok(sound_effects)
    } else {
        Err(SoundEffectsFileError {
            path: sfx_file.path.clone(),
            file_name: sfx_file.file_name.clone(),
            errors,
        })
    }
}

pub(crate) struct SfxHeader {
    pub(crate) offset: u16,
    pub(crate) duration_and_interrupt_flag: u16,
}

pub struct CombinedSoundEffectsData {
    pub(crate) low_priority_index: u8,
    pub(crate) sfx_header: Vec<SfxHeader>,
    pub(crate) sfx_data: Vec<u8>,
}

impl CombinedSoundEffectsData {
    pub fn sfx_data_size(&self) -> usize {
        self.sfx_header.len() * COMMON_DATA_BYTES_PER_SOUND_EFFECT + self.sfx_data.len()
    }

    pub(crate) fn sfx_header_addr_l_iter(
        &self,
        sfx_data_addr: u16,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header
            .iter()
            .map(move |h| (h.offset + sfx_data_addr).to_le_bytes()[0])
    }

    pub(crate) fn sfx_header_addr_h_iter(
        &self,
        sfx_data_addr: u16,
    ) -> impl Iterator<Item = u8> + '_ {
        self.sfx_header
            .iter()
            .map(move |h| (h.offset + sfx_data_addr).to_le_bytes()[1])
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
    fn low_priority_index(&self) -> usize;
}

impl SfxExportOrder for UniqueSoundEffectExportOrder {
    fn n_sound_effects(&self) -> usize {
        self.export_order.len()
    }

    fn export_order(&self) -> &[Name] {
        self.export_order.list()
    }

    fn low_priority_index(&self) -> usize {
        self.low_priority_index
    }
}

pub fn combine_sound_effects(
    sfx_map: &impl CompiledSfxMap,
    sfx_export_order: &impl SfxExportOrder,
) -> Result<CombinedSoundEffectsData, CombineSoundEffectsError> {
    let export_order = sfx_export_order.export_order();

    if sfx_map.is_empty() {
        return Err(CombineSoundEffectsError::NoSoundEffectFiles);
    }

    let low_priority_index = match sfx_export_order.low_priority_index().try_into() {
        Ok(i) => i,
        Err(_) => return Err(CombineSoundEffectsError::InvalidLowPriorityIndex),
    };

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
                let ticks = u16::try_from(t).unwrap();

                let interruptible_flag = match s.flags.interruptible.unwrap_or(true) {
                    true => FLAG_MASK,
                    false => 0,
                };

                sfx_header.push(SfxHeader {
                    offset,
                    duration_and_interrupt_flag: ticks | interruptible_flag,
                });
                sfx_data.extend(s.bytecode());
            }
            None => missing.push(name.as_str().to_owned()),
        }
    }

    if missing.is_empty() {
        Ok(CombinedSoundEffectsData {
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
        low_priority_index: u8::MAX,
        sfx_header: Vec::new(),
        sfx_data: Vec::new(),
    }
}

/// Create a `CombinedSoundEffectsData` that contains a single blank sound-effect.
/// Used by the GUI to allocate a block of Audio-RAM that the audio-thread can write sound-effect bytecode to.
pub fn tad_gui_sfx_data(buffer_size: usize) -> CombinedSoundEffectsData {
    CombinedSoundEffectsData {
        low_priority_index: u8::MAX,
        sfx_header: vec![SfxHeader {
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
) -> Result<CompiledSoundEffect, SoundEffectError> {
    let r = match &input.sfx {
        SoundEffectText::BytecodeAssembly(text) => {
            compile_bytecode_sound_effect(text, inst_map, input.flags.clone())
        }
        SoundEffectText::Mml(text) => {
            compile_mml_sound_effect(text, inst_map, pitch_table, input.flags.clone())
        }
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
