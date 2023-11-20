//! MML bytecode generator

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::command_parser::{
    ManualVibrato, MmlCommand, MpVibrato, PanCommand, Parser, PortamentoSpeed, VolumeCommand,
};
use super::identifier::Identifier;
use super::instruments::{EnvelopeOverride, MmlInstrument};
use super::{IdentifierStr, Section};

use crate::bytecode::{
    BcTerminator, BcTicksKeyOff, BcTicksNoKeyOff, Bytecode, LoopCount, PitchOffsetPerTick,
    PlayNoteTicks, PortamentoVelocity, SubroutineId,
};
use crate::errors::{MmlChannelError, MmlError, ValueError};
use crate::file_pos::Line;
use crate::notes::{Note, SEMITONES_PER_OCTAVE};
use crate::pitch_table::PitchTable;
use crate::time::{TickClock, TickCounter, TickCounterWithLoopFlag, ZenLen};

use std::cmp::max;
use std::collections::HashMap;

pub const MAX_BROKEN_CHORD_NOTES: usize = 128;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoopPoint {
    pub bytecode_offset: usize,
    pub tick_counter: TickCounter,
}

#[cfg(feature = "mml_tracking")]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BytecodePos {
    // Position (within the channel/subroutine) at the end of the bytecode instruction.
    pub bc_end_pos: u16,
    // Character index within the input file
    pub char_index: u32,
}

#[derive(Debug, PartialEq)]
pub struct ChannelData {
    identifier: Identifier,

    bytecode: Vec<u8>,
    loop_point: Option<LoopPoint>,

    tick_counter: TickCounter,
    last_instrument: Option<usize>,

    // Some if this channel is a subroutine
    pub(super) bc_subroutine: Option<SubroutineId>,

    section_tick_counters: Vec<TickCounterWithLoopFlag>,
    pub(super) tempo_changes: Vec<(TickCounter, TickClock)>,

    #[cfg(feature = "mml_tracking")]
    pub(crate) bc_tracking: Vec<BytecodePos>,
}

impl ChannelData {
    pub fn identifier(&self) -> &Identifier {
        &self.identifier
    }
    pub fn bytecode(&self) -> &[u8] {
        &self.bytecode
    }
    pub fn loop_point(&self) -> Option<LoopPoint> {
        self.loop_point
    }
    pub fn tick_counter(&self) -> TickCounter {
        self.tick_counter
    }
    pub fn section_tick_counters(&self) -> &[TickCounterWithLoopFlag] {
        &self.section_tick_counters
    }
}

#[derive(Clone, PartialEq)]
enum MpState {
    Disabled,
    Manual,
    Mp(MpVibrato),
}

struct SkipLastLoopState {
    instrument: Option<usize>,
    prev_slurred_note: Option<Note>,
    vibrato: Option<ManualVibrato>,
}

struct ChannelBcGenerator<'a> {
    pitch_table: &'a PitchTable,
    instruments: &'a Vec<MmlInstrument>,
    subroutines: Option<&'a Vec<ChannelData>>,

    bc: Bytecode,

    tempo_changes: Vec<(TickCounter, TickClock)>,

    instrument: Option<usize>,
    prev_slurred_note: Option<Note>,

    mp: MpState,
    vibrato: Option<ManualVibrato>,

    skip_last_loop_state: Option<SkipLastLoopState>,

    loop_point: Option<LoopPoint>,

    show_missing_set_instrument_error: bool,
}

impl ChannelBcGenerator<'_> {
    fn new<'a>(
        pitch_table: &'a PitchTable,
        instruments: &'a Vec<MmlInstrument>,
        subroutines: Option<&'a Vec<ChannelData>>,
        is_subroutine: bool,
    ) -> ChannelBcGenerator<'a> {
        ChannelBcGenerator {
            pitch_table,
            instruments,
            subroutines,
            bc: Bytecode::new(is_subroutine, false),
            tempo_changes: Vec::new(),
            instrument: None,
            prev_slurred_note: None,
            mp: MpState::Disabled,
            vibrato: None,
            skip_last_loop_state: None,
            loop_point: None,
            show_missing_set_instrument_error: !is_subroutine,
        }
    }

    fn instrument_from_index(&self, i: usize) -> &MmlInstrument {
        // `i` should always be valid
        match self.instruments.get(i) {
            Some(inst) => inst,
            None => panic!("invalid instrument index"),
        }
    }

    fn test_note(&mut self, note: Note) -> Result<(), MmlError> {
        match self.instrument {
            Some(i) => {
                let inst = self.instrument_from_index(i);
                if note >= inst.first_note && note <= inst.last_note {
                    Ok(())
                } else {
                    Err(MmlError::NoteOutOfRange(
                        note,
                        inst.first_note,
                        inst.last_note,
                    ))
                }
            }
            None => {
                if self.show_missing_set_instrument_error {
                    self.show_missing_set_instrument_error = false;
                    Err(MmlError::CannotPlayNoteBeforeSettingInstrument)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn calculate_vibrato_for_note(
        &self,
        mp: &MpVibrato,
        note: Note,
    ) -> Result<ManualVibrato, MmlError> {
        if mp.depth_in_cents == 0 {
            return Err(MmlError::MpDepthZero);
        }
        let inst = match self.instrument {
            Some(index) => self.instrument_from_index(index),
            None => return Err(MmlError::CannotUseMpWithoutInstrument),
        };

        let pitch = self.pitch_table.pitch_for_note(inst.instrument_id, note);

        // Calculate the minimum and maximum pitches of the vibrato.
        // This produces more accurate results when cents is very large (ie, 400)
        let pow = f64::from(mp.depth_in_cents) / f64::from(SEMITONES_PER_OCTAVE as u32 * 100);
        let p1 = f64::from(pitch) * 2.0_f64.powf(-pow);
        let p2 = f64::from(pitch) * 2.0_f64.powf(pow);

        let qwt = u32::from(mp.quarter_wavelength_ticks.as_u8());

        let po_per_tick = f64::round((p2 - p1) / f64::from(qwt * 2));
        let po_per_tick = if po_per_tick > 1.0 { po_per_tick } else { 1.0 };

        if po_per_tick > u32::MAX.into() {
            return Err(MmlError::MpPitchOffsetTooLarge(u32::MAX));
        }
        let po_per_tick = po_per_tick as u32;

        match po_per_tick.try_into() {
            Ok(po) => Ok(ManualVibrato {
                quarter_wavelength_ticks: mp.quarter_wavelength_ticks,
                pitch_offset_per_tick: po,
            }),
            Err(_) => Err(MmlError::MpPitchOffsetTooLarge(po_per_tick)),
        }
    }

    fn split_play_note_length(
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(PlayNoteTicks, TickCounter), ValueError> {
        let l = length.value();

        if !is_slur && l <= BcTicksKeyOff::MAX {
            return Ok((
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(l)?),
                TickCounter::new(0),
            ));
        }

        // The play_note instruction requires keyoff.
        let last_min = if is_slur {
            BcTicksNoKeyOff::MIN
        } else {
            BcTicksKeyOff::MIN
        };
        const MAX: u32 = BcTicksNoKeyOff::MAX;

        let pn = {
            if l <= MAX {
                l
            } else if l >= MAX + last_min {
                MAX
            } else {
                MAX - 1
            }
        };

        Ok((
            PlayNoteTicks::NoKeyOff(BcTicksNoKeyOff::try_from(pn)?),
            TickCounter::new(l - pn),
        ))
    }

    fn play_note_with_mp(
        &mut self,
        note: Note,
        length: TickCounter,
        is_slur: bool,
    ) -> Result<(), MmlError> {
        let (pn_length, rest) = Self::split_play_note_length(length, is_slur)?;

        self.test_note(note)?;

        self.prev_slurred_note = if is_slur { Some(note) } else { None };

        match &self.mp {
            MpState::Manual => {
                self.bc.play_note(note, pn_length);
            }
            MpState::Disabled => {
                const POPT: PitchOffsetPerTick = PitchOffsetPerTick::new(0);

                let vibrato_disabled = match self.vibrato {
                    None => true,
                    Some(v) => v.pitch_offset_per_tick == POPT,
                };

                if vibrato_disabled {
                    self.bc.play_note(note, pn_length);
                } else {
                    self.bc
                        .set_vibrato_depth_and_play_note(POPT, note, pn_length);

                    if let Some(v) = &mut self.vibrato {
                        v.pitch_offset_per_tick = POPT;
                    }
                }
            }
            MpState::Mp(mp) => {
                let cv = self.calculate_vibrato_for_note(mp, note)?;

                if self.vibrato == Some(cv) {
                    self.bc.play_note(note, pn_length);
                } else {
                    match self.vibrato {
                        Some(sv) if sv.quarter_wavelength_ticks == cv.quarter_wavelength_ticks => {
                            self.bc.set_vibrato_depth_and_play_note(
                                cv.pitch_offset_per_tick,
                                note,
                                pn_length,
                            );
                        }
                        _ => {
                            self.bc
                                .set_vibrato(cv.pitch_offset_per_tick, cv.quarter_wavelength_ticks);
                            self.bc.play_note(note, pn_length);
                        }
                    }

                    self.vibrato = Some(cv);
                }
            }
        }

        self.rest_after_play_note(rest, is_slur)
    }

    fn rest_after_play_note(&mut self, length: TickCounter, is_slur: bool) -> Result<(), MmlError> {
        if length.is_zero() {
            return Ok(());
        }
        if is_slur {
            return self.rest(length);
        }

        self.prev_slurred_note = None;

        let mut remaining_ticks = length.value();

        const MAX_REST: u32 = BcTicksNoKeyOff::MAX;
        const MAX_FINAL_REST: u32 = BcTicksKeyOff::MAX;
        const MIN_FINAL_REST: u32 = BcTicksKeyOff::MIN;
        const _: () = assert!(MIN_FINAL_REST > 1);

        while remaining_ticks > MAX_FINAL_REST {
            let l = if remaining_ticks >= MAX_REST + MIN_FINAL_REST {
                MAX_REST
            } else {
                MAX_REST - 1
            };
            self.bc.rest(BcTicksNoKeyOff::try_from(l).unwrap());
            remaining_ticks -= l;
        }

        self.bc
            .rest_keyoff(BcTicksKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    fn rest(&mut self, length: TickCounter) -> Result<(), MmlError> {
        let mut remaining_ticks = length.value();

        let rest_length = BcTicksNoKeyOff::try_from(BcTicksNoKeyOff::MAX).unwrap();
        const _: () = assert!(BcTicksNoKeyOff::MIN == 1);

        while remaining_ticks > rest_length.ticks() {
            self.bc.rest(rest_length);
            remaining_ticks -= rest_length.ticks();
        }

        self.bc.rest(BcTicksNoKeyOff::try_from(remaining_ticks)?);

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn portamento(
        &mut self,
        note1: Note,
        note2: Note,
        is_slur: bool,
        speed_override: Option<PortamentoSpeed>,
        total_length: TickCounter,
        delay_length: TickCounter,
        tie_length: TickCounter,
    ) -> Result<(), MmlError> {
        assert!(delay_length < total_length);

        self.test_note(note1)?;
        self.test_note(note2)?;

        // Play note1 (if required)
        let note1_length = {
            if self.prev_slurred_note != Some(note1) {
                let note_1_length = max(TickCounter::new(1), delay_length);
                let (pn_length, rest) = Self::split_play_note_length(note_1_length, true)?;

                self.bc.play_note(note1, pn_length);
                self.rest_after_play_note(rest, true)?;
                note_1_length
            } else if !delay_length.is_zero() {
                self.rest(delay_length)?;
                delay_length
            } else {
                TickCounter::new(0)
            }
        };

        let portamento_length =
            TickCounter::new(total_length.value().wrapping_sub(note1_length.value()));
        if portamento_length.is_zero() {
            return Err(MmlError::PortamentoDelayTooLong);
        }

        let velocity = match speed_override {
            Some(speed) => {
                if note1 < note2 {
                    i32::from(speed.as_u8())
                } else {
                    -i32::from(speed.as_u8())
                }
            }
            None => {
                let inst = match self.instrument {
                    Some(index) => self.instrument_from_index(index),
                    None => return Err(MmlError::PortamentoRequiresInstrument),
                };
                let p1: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note1)
                    .into();
                let p2: i32 = self
                    .pitch_table
                    .pitch_for_note(inst.instrument_id, note2)
                    .into();

                let ticks = i32::try_from(portamento_length.value()).unwrap();

                (p2 - p1) / ticks
            }
        };
        let velocity = PortamentoVelocity::try_from(velocity)?;

        let (p_length, p_rest) =
            Self::split_play_note_length(tie_length + portamento_length, is_slur)?;
        self.bc.portamento(note2, velocity, p_length);

        self.prev_slurred_note = if is_slur { Some(note2) } else { None };

        self.rest_after_play_note(p_rest, is_slur)
    }

    fn broken_chord(
        &mut self,
        notes: &[Note],
        total_length: TickCounter,
        note_length: PlayNoteTicks,
    ) -> Result<(), MmlError> {
        self.prev_slurred_note = None;

        if notes.is_empty() {
            return Err(MmlError::NoNotesInBrokenChord);
        }
        if notes.len() > MAX_BROKEN_CHORD_NOTES {
            return Err(MmlError::TooManyNotesInBrokenChord(notes.len()));
        }
        let n_notes: u32 = notes.len().try_into().unwrap();

        for n in notes {
            self.test_note(*n)?;
        }

        let expected_tick_counter = self.bc.get_tick_counter() + total_length;

        let total_ticks = total_length.value();

        // Number of ticks in the last note played outside the loop (if any).
        let mut last_note_ticks = total_ticks % note_length.ticks();

        // If tie is true, a keyoff is required after the loop.
        if note_length.is_slur() && last_note_ticks == 0 {
            last_note_ticks += note_length.ticks();
        }

        if last_note_ticks != 0 && last_note_ticks < BcTicksKeyOff::MIN {
            last_note_ticks = BcTicksKeyOff::MIN;
        }
        let last_note_ticks = last_note_ticks;

        if total_ticks < last_note_ticks {
            return Err(MmlError::BrokenChordTotalLengthTooShort);
        }
        let notes_in_loop = (total_ticks - last_note_ticks) / note_length.ticks();

        let break_point = usize::try_from(notes_in_loop % n_notes).unwrap();
        let has_break_point: bool = break_point != 0;

        let n_loops = (notes_in_loop / n_notes) + u32::from(has_break_point);

        if n_loops < 2 {
            return Err(MmlError::BrokenChordTotalLengthTooShort);
        }

        let n_loops = LoopCount::try_from(n_loops)?;

        self.bc.start_loop(Some(n_loops))?;

        for (i, n) in notes.iter().enumerate() {
            if i == break_point && i != 0 {
                self.bc.skip_last_loop()?;
            }
            self.bc.play_note(*n, note_length);
        }

        self.bc.end_loop(None)?;

        if last_note_ticks > 0 {
            // The last note to play is always a keyoff note.
            self.bc.play_note(
                notes[break_point],
                PlayNoteTicks::KeyOff(BcTicksKeyOff::try_from(last_note_ticks)?),
            )
        }

        if self.bc.get_tick_counter() != expected_tick_counter {
            return Err(MmlError::BrokenChordTickCountMismatch(
                expected_tick_counter,
                self.bc.get_tick_counter(),
            ));
        }

        Ok(())
    }

    fn set_instrument(&mut self, inst_index: usize) -> Result<(), MmlError> {
        if self.instrument == Some(inst_index) {
            return Ok(());
        }
        let inst = self.instrument_from_index(inst_index);
        let old_inst = self.instrument.map(|i| self.instrument_from_index(i));

        let i_id = inst.instrument_id;

        match old_inst {
            Some(old) if old.instrument_id == i_id => {
                // Instrument_id unchanged
                if inst.envelope_override != old.envelope_override {
                    match inst.envelope_override {
                        EnvelopeOverride::None => self.bc.set_instrument(i_id),
                        EnvelopeOverride::Adsr(adsr) => self.bc.set_adsr(adsr),
                        EnvelopeOverride::Gain(gain) => self.bc.set_gain(gain),
                    }
                }
            }
            _ => match inst.envelope_override {
                EnvelopeOverride::None => self.bc.set_instrument(i_id),
                EnvelopeOverride::Adsr(adsr) => self.bc.set_instrument_and_adsr(i_id, adsr),
                EnvelopeOverride::Gain(gain) => self.bc.set_instrument_and_gain(i_id, gain),
            },
        }

        self.instrument = Some(inst_index);

        Ok(())
    }

    fn call_subroutine(&mut self, s_id: SubroutineId) -> Result<(), MmlError> {
        // CallSubroutine commands should only be created if the channel can call a subroutine.
        // `s_id` should always be valid
        let sub: &ChannelData = match self.subroutines {
            Some(s) => match s.get(s_id.as_usize()) {
                Some(s) => s,
                None => panic!("invalid SubroutineId"),
            },
            None => panic!("subroutines is None"),
        };

        // Calling a subroutine disables manual vibrato
        self.vibrato = None;
        if self.mp == MpState::Manual {
            self.mp = MpState::Disabled;
        }

        if let Some(inst) = sub.last_instrument {
            self.instrument = Some(inst);
        }

        self.bc.call_subroutine(s_id)?;

        Ok(())
    }

    fn set_manual_vibrato(&mut self, v: Option<ManualVibrato>) {
        self.mp = MpState::Manual;
        match v {
            Some(v) => {
                self.vibrato = Some(v);
                self.bc
                    .set_vibrato(v.pitch_offset_per_tick, v.quarter_wavelength_ticks);
            }
            None => {
                self.vibrato = None;
                self.bc.disable_vibrato();
            }
        }
    }

    fn set_song_tick_clock(&mut self, tick_clock: TickClock) -> Result<(), MmlError> {
        let tc = (self.bc.get_tick_counter(), tick_clock);
        self.tempo_changes.push(tc);

        self.bc.set_song_tick_clock(tick_clock)?;
        Ok(())
    }

    fn process_command(&mut self, command: &MmlCommand) -> Result<(), MmlError> {
        match command {
            MmlCommand::NoCommand => (),

            &MmlCommand::SetLoopPoint => {
                if self.loop_point.is_some() {
                    return Err(MmlError::LoopPointAlreadySet);
                }
                self.loop_point = Some(LoopPoint {
                    bytecode_offset: self.bc.get_bytecode_len(),
                    tick_counter: self.bc.get_tick_counter(),
                })
            }

            &MmlCommand::SetInstrument(inst_index) => {
                self.set_instrument(inst_index)?;
            }

            &MmlCommand::CallSubroutine(s_id) => {
                self.call_subroutine(s_id)?;
            }

            &MmlCommand::SetManualVibrato(v) => {
                self.set_manual_vibrato(v);
            }

            &MmlCommand::SetMpVibrato(mp) => match mp {
                Some(mp) => self.mp = MpState::Mp(mp),
                None => self.mp = MpState::Disabled,
            },

            &MmlCommand::Rest(length) => {
                self.rest(length)?;
            }

            &MmlCommand::PlayNote {
                note,
                length,
                is_slur,
            } => {
                self.play_note_with_mp(note, length, is_slur)?;
            }

            &MmlCommand::PlayQuantizedNote {
                note,
                length: _,
                key_on_length,
                rest,
            } => {
                self.play_note_with_mp(note, key_on_length, false)?;
                self.rest(rest)?;
            }

            &MmlCommand::Portamento {
                note1,
                note2,
                is_slur,
                speed_override,
                total_length,
                delay_length,
                tie_length,
            } => {
                self.portamento(
                    note1,
                    note2,
                    is_slur,
                    speed_override,
                    total_length,
                    delay_length,
                    tie_length,
                )?;
            }

            MmlCommand::BrokenChord {
                notes,
                total_length,
                note_length,
            } => {
                self.broken_chord(notes, *total_length, *note_length)?;
            }

            MmlCommand::StartLoop => {
                self.bc.start_loop(None)?;
            }

            MmlCommand::SkipLastLoop => {
                self.bc.skip_last_loop()?;

                self.skip_last_loop_state = Some(SkipLastLoopState {
                    instrument: self.instrument,
                    prev_slurred_note: self.prev_slurred_note,
                    vibrato: self.vibrato,
                });
            }

            &MmlCommand::EndLoop(loop_count) => {
                self.bc.end_loop(Some(loop_count))?;

                if let Some(s) = &self.skip_last_loop_state {
                    self.instrument = s.instrument;
                    self.prev_slurred_note = s.prev_slurred_note;
                    self.vibrato = s.vibrato;
                }
                self.skip_last_loop_state = None;
            }

            &MmlCommand::ChangePanAndOrVolume(pan, volume) => match (pan, volume) {
                (Some(PanCommand::Absolute(p)), Some(VolumeCommand::Absolute(v))) => {
                    self.bc.set_pan_and_volume(p, v);
                }
                (pan, volume) => {
                    match volume {
                        Some(VolumeCommand::Absolute(v)) => self.bc.set_volume(v),
                        Some(VolumeCommand::Relative(v)) => self.bc.adjust_volume(v),
                        None => (),
                    }
                    match pan {
                        Some(PanCommand::Absolute(p)) => self.bc.set_pan(p),
                        Some(PanCommand::Relative(p)) => self.bc.adjust_pan(p),
                        None => (),
                    }
                }
            },

            &MmlCommand::SetEcho(e) => {
                if e {
                    self.bc.enable_echo();
                } else {
                    self.bc.disable_echo();
                }
            }

            &MmlCommand::SetSongTempo(bpm) => {
                self.set_song_tick_clock(bpm.to_tick_clock()?)?;
            }
            &MmlCommand::SetSongTickClock(tick_clock) => {
                self.set_song_tick_clock(tick_clock)?;
            }
        }

        Ok(())
    }
}

pub struct MmlBytecodeGenerator<'a, 'b> {
    default_zenlen: ZenLen,
    pitch_table: &'a PitchTable,
    sections: &'a [Section],
    instruments: &'a Vec<MmlInstrument>,
    instrument_map: HashMap<IdentifierStr<'a>, usize>,

    subroutines: Option<&'b Vec<ChannelData>>,
    subroutine_map: Option<HashMap<IdentifierStr<'b>, SubroutineId>>,
}

impl<'a, 'b> MmlBytecodeGenerator<'a, 'b> {
    pub fn new(
        default_zenlen: ZenLen,
        pitch_table: &'a PitchTable,
        sections: &'a [Section],
        instruments: &'a Vec<MmlInstrument>,
        instrument_map: HashMap<IdentifierStr<'a>, usize>,
    ) -> Self {
        Self {
            default_zenlen,
            pitch_table,
            sections,
            instruments,
            instrument_map,
            subroutines: None,
            subroutine_map: None,
        }
    }

    // Should only be called when all subroutines have been compiled.
    pub fn set_subroutines(&mut self, subroutines: &'b Vec<ChannelData>) {
        let subroutine_map = subroutines
            .iter()
            .map(|s| (s.identifier().as_ref(), s.bc_subroutine.unwrap()))
            .collect();

        self.subroutines = Some(subroutines);
        self.subroutine_map = Some(subroutine_map);
    }

    pub fn parse_and_compile_mml_channel(
        &self,
        lines: &[Line],
        identifier: Identifier,
        subroutine_index: Option<u8>,
    ) -> Result<ChannelData, MmlChannelError> {
        let sections = if subroutine_index.is_none() {
            // channel
            Some(self.sections)
        } else {
            // subroutine (no section tracking)
            None
        };

        let mut parser = Parser::new(
            lines,
            &self.instrument_map,
            self.subroutine_map.as_ref(),
            self.default_zenlen,
            sections,
        );

        #[cfg(feature = "mml_tracking")]
        let mut bc_tracking = Vec::new();

        // Cannot have subroutine_index and subroutines list at the same time.
        if subroutine_index.is_some() {
            assert!(
                self.subroutines.is_none(),
                "Cannot set `subroutine_index` and `subroutines` vec at the same time"
            );
        }

        let mut gen = ChannelBcGenerator::new(
            self.pitch_table,
            self.instruments,
            self.subroutines,
            subroutine_index.is_some(),
        );

        while let Some(c) = parser.next() {
            match gen.process_command(c.command()) {
                Ok(()) => (),
                Err(e) => parser.add_error_range(c.pos().clone(), e),
            }

            if matches!(c.command(), MmlCommand::EndLoop(_)) {
                parser.set_tick_counter(gen.bc.get_tick_counter_with_loop_flag());
            }

            #[cfg(feature = "mml_tracking")]
            bc_tracking.push(BytecodePos {
                bc_end_pos: gen.bc.get_bytecode_len().try_into().unwrap_or(0xffff),
                char_index: c.pos().index_start,
            });
        }

        let last_pos = *parser.peek_pos();

        let tick_counter = gen.bc.get_tick_counter();
        let max_nested_loops = gen.bc.get_max_nested_loops();

        let terminator = match (subroutine_index, gen.loop_point) {
            (Some(_), Some(_)) => {
                panic!("Loop point not allowed in subroutine")
            }
            (None, None) => BcTerminator::DisableChannel,
            (Some(_), None) => BcTerminator::ReturnFromSubroutine,
            (None, Some(lp)) => {
                if lp.tick_counter == tick_counter {
                    parser.add_error_range(last_pos.to_range(1), MmlError::NoTicksAfterLoopPoint);
                }
                BcTerminator::LoopChannel
            }
        };

        let bytecode = match gen.bc.bytecode(terminator) {
            Ok(b) => b,
            Err(e) => {
                parser.add_error_range(last_pos.to_range(1), MmlError::BytecodeError(e));
                Vec::new()
            }
        };

        let bc_subroutine =
            subroutine_index.map(|si| SubroutineId::new(si, tick_counter, max_nested_loops));

        let (section_tick_counters, errors) = parser.finalize();

        if errors.is_empty() {
            Ok(ChannelData {
                identifier,
                bytecode,
                loop_point: gen.loop_point,
                tick_counter,
                last_instrument: gen.instrument,
                bc_subroutine,
                section_tick_counters,
                tempo_changes: gen.tempo_changes,

                #[cfg(feature = "mml_tracking")]
                bc_tracking,
            })
        } else {
            Err(MmlChannelError { identifier, errors })
        }
    }
}
