//! Common audio data compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::bytecode::GOTO_RELATIVE_INSTRUCTION_SIZE;
use crate::driver_constants::{
    addresses, AUDIO_RAM_SIZE, COMMON_DATA_BYTES_PER_DIR, COMMON_DATA_BYTES_PER_INSTRUMENT,
    COMMON_DATA_BYTES_PER_PITCH, COMMON_DATA_BYTES_PER_SFX_SUBROUTINE,
    COMMON_DATA_BYTES_PER_SOUND_EFFECT, COMMON_DATA_DIR_TABLE_OFFSET, COMMON_DATA_HEADER_SIZE,
    MAX_COMMON_DATA_SIZE, MAX_DIR_ITEMS, MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_PITCHES,
    MAX_SFX_DATA_ADDR, MAX_SFX_SUBROUTINES, MAX_SOUND_EFFECTS,
};
use crate::envelope::Envelope;
use crate::errors::{CommonAudioDataError, CommonAudioDataErrors, SfxCannotFitInSfxBuffer};
use crate::opcodes;
use crate::samples::SampleAndInstrumentData;
use crate::sound_effects::{
    tad_gui_sfx_buffer, CombinedSoundEffectsData, CompiledSfxSubroutines, CompiledSoundEffect,
};

use std::ops::Range;
use std::sync::OnceLock;

#[derive(Clone)]
pub struct CommonAudioData {
    data: Vec<u8>,
    n_dir_items: usize,
    n_instruments_and_samples: usize,
    instruments_soa_offset: usize,

    sfx_data_and_tables: Range<u16>,
    sfx_soa: Range<u16>,
    sfx_bc_data: Range<u16>,

    pitch_table_addr: u16,
    instruments_soa_addr: u16,
    brr_data_addr: u16,
    min_song_data_addr: u16,
}

impl std::fmt::Debug for CommonAudioData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CommonAudioData {} bytes", self.data.len())
    }
}

impl CommonAudioData {
    const DIR_TABLE_ADDR: u16 = addresses::COMMON_DATA + COMMON_DATA_DIR_TABLE_OFFSET as u16;

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn dir_addr_range(&self) -> Range<u16> {
        Self::DIR_TABLE_ADDR..self.sfx_data_and_tables.start
    }

    pub fn sfx_data_and_tables_range(&self) -> Range<u16> {
        self.sfx_data_and_tables.clone()
    }

    /// Does not include sfx subroutines
    pub fn sfx_bytecode_addr_range(&self) -> Range<u16> {
        self.sfx_bc_data.clone()
    }

    pub fn pitch_table_addr_range(&self) -> Range<u16> {
        self.pitch_table_addr..self.instruments_soa_addr
    }

    pub fn instruments_soa_addr_range(&self) -> Range<u16> {
        self.instruments_soa_addr..self.brr_data_addr
    }

    /// Caution: Includes padding byte
    pub fn brr_addr_range(&self) -> Range<u16> {
        self.brr_data_addr..self.min_song_data_addr
    }

    pub fn min_song_data_addr(&self) -> u16 {
        self.min_song_data_addr
    }

    pub fn aram_size(&self) -> u16 {
        self.min_song_data_addr - addresses::COMMON_DATA
    }

    pub fn instruments_and_samples_size(&self) -> u16 {
        let sfx = &self.sfx_data_and_tables;
        let sfx_size = sfx.end - sfx.start;

        self.aram_size() - sfx_size
    }

    pub fn n_dir_items(&self) -> usize {
        self.n_dir_items
    }

    pub fn n_instruments_and_samples(&self) -> usize {
        self.n_instruments_and_samples
    }

    pub fn dir_table_start_iter(&self) -> impl Iterator<Item = u16> + '_ {
        let n_dir_items: usize = self.n_dir_items;
        let range = Range {
            start: COMMON_DATA_DIR_TABLE_OFFSET,
            end: COMMON_DATA_DIR_TABLE_OFFSET + n_dir_items * COMMON_DATA_BYTES_PER_DIR,
        };

        let dir_table = &self.data[range];
        (0..n_dir_items).map(|i| u16::from_le_bytes([dir_table[i * 4], dir_table[i * 4 + 1]]))
    }

    pub fn instruments_soa_data(&self) -> &[u8] {
        let start = self.instruments_soa_offset;
        let end = start + self.n_instruments_and_samples * COMMON_DATA_BYTES_PER_INSTRUMENT;
        &self.data[start..end]
    }

    pub fn instruments_soa_scrn(&self) -> &[u8] {
        let start = self.instruments_soa_offset;
        let end = start + self.n_instruments_and_samples;
        &self.data[start..end]
    }

    pub fn sound_effect_addresses(&self) -> Vec<u16> {
        let r = &self.sfx_soa;
        let r = Range {
            start: (r.start - addresses::COMMON_DATA).into(),
            end: (r.end - addresses::COMMON_DATA).into(),
        };
        let sfx_soa = &self.data[r];

        let n_sfx = sfx_soa.len() / 4;

        let sfx_addr_and_one_channel_flag_l = &sfx_soa[..n_sfx];
        let sfx_addr_and_one_channel_flag_h = &sfx_soa[n_sfx..n_sfx * 2];

        std::iter::zip(
            sfx_addr_and_one_channel_flag_l,
            sfx_addr_and_one_channel_flag_h,
        )
        .map(|(&l, &h)| u16::from_le_bytes([l, h]) & 0x7fff)
        .collect()
    }

    pub fn pitch_table_data(&self) -> (&[u8], &[u8]) {
        let r = self.pitch_table_addr_range();
        let start: usize = (r.start - addresses::COMMON_DATA).into();
        let end: usize = (r.end - addresses::COMMON_DATA).into();
        let mid = (start + end) / 2;

        (&self.data[start..mid], &self.data[mid..end])
    }

    pub fn instrument_envelope(&self, instrument_id: u8) -> Option<Envelope> {
        let i1 = self.instruments_soa_offset
            + 2 * self.n_instruments_and_samples
            + usize::from(instrument_id);
        let i2 = self.instruments_soa_offset
            + 3 * self.n_instruments_and_samples
            + usize::from(instrument_id);

        if i2 < self.data.len() {
            Some(Envelope::from_engine_value(self.data[i1], self.data[i2]))
        } else {
            None
        }
    }
}

pub fn build_common_audio_data(
    samples_and_instruments: &SampleAndInstrumentData,
    sfx_subroutines: &CompiledSfxSubroutines,
    sound_effects: &CombinedSoundEffectsData,
) -> Result<CommonAudioData, CommonAudioDataErrors> {
    const CAD_ADDR: usize = addresses::COMMON_DATA as usize;

    let mut errors = Vec::new();

    let n_pitches = samples_and_instruments.pitch_table.n_pitches;
    let n_instruments_and_samples = samples_and_instruments.n_instruments;
    let n_dir_items = samples_and_instruments.brr_directory_offsets.len();
    let n_sound_effects = sound_effects.sfx_header.len();
    let n_sfx_subroutines = sfx_subroutines.subroutines().len();
    let sfx_subroutine_data = sfx_subroutines.bytecode_data();

    if n_instruments_and_samples > MAX_INSTRUMENTS_AND_SAMPLES {
        errors.push(CommonAudioDataError::TooManyInstrumentsAndSamples(
            n_instruments_and_samples,
        ));
    }
    if n_dir_items > MAX_DIR_ITEMS {
        errors.push(CommonAudioDataError::TooManyBrrSamples(n_dir_items));
    }
    if n_sfx_subroutines > MAX_SFX_SUBROUTINES.into() {
        errors.push(CommonAudioDataError::TooManySfxSubroutines(
            n_sfx_subroutines,
        ));
    }
    if n_sound_effects > MAX_SOUND_EFFECTS {
        errors.push(CommonAudioDataError::TooManySoundEffects(n_sound_effects));
    }

    let header_size = COMMON_DATA_HEADER_SIZE + n_dir_items * COMMON_DATA_BYTES_PER_DIR;

    let common_data_size = header_size
        + n_sfx_subroutines * COMMON_DATA_BYTES_PER_SFX_SUBROUTINE
        + sfx_subroutine_data.len()
        + n_sound_effects * COMMON_DATA_BYTES_PER_SOUND_EFFECT
        + sound_effects.sfx_data.len()
        + n_pitches * COMMON_DATA_BYTES_PER_PITCH
        + n_instruments_and_samples * COMMON_DATA_BYTES_PER_INSTRUMENT
        + samples_and_instruments.brr_data.len();

    if common_data_size > MAX_COMMON_DATA_SIZE {
        errors.push(CommonAudioDataError::CommonAudioDataTooLarge(
            common_data_size,
        ));
    }

    // Sound effect bytecode should be immediately after the BRR directory
    // as the MSB of `soundEffects_addrAndOneChannelFlag_l` is used as a flag
    // and all SFX addresses must be <= MAX_SFX_DATA_ADDR.
    let sfx_bc_addr = CAD_ADDR + header_size;
    let sfx_sub_bc_addr = sfx_bc_addr + sound_effects.sfx_data.len();
    let sfx_soa_addr = sfx_sub_bc_addr + sfx_subroutine_data.len();
    let sfx_sub_table_addr = sfx_soa_addr + n_sound_effects * COMMON_DATA_BYTES_PER_SOUND_EFFECT;
    let pitch_table_addr =
        sfx_sub_table_addr + n_sfx_subroutines * COMMON_DATA_BYTES_PER_SFX_SUBROUTINE;
    let instruments_soa_addr = pitch_table_addr + n_pitches * COMMON_DATA_BYTES_PER_PITCH;
    let brr_data_addr =
        instruments_soa_addr + n_instruments_and_samples * COMMON_DATA_BYTES_PER_INSTRUMENT;

    assert_eq!(
        brr_data_addr + samples_and_instruments.brr_data.len(),
        common_data_size + CAD_ADDR
    );

    if let Some(last_offset) = sound_effects.last_offset() {
        let last_sfx_addr = sfx_bc_addr + usize::from(last_offset);
        if last_sfx_addr > MAX_SFX_DATA_ADDR {
            errors.push(CommonAudioDataError::SoundEffectDataTooLarge {
                by: last_sfx_addr - MAX_SFX_DATA_ADDR,
            });
        }
    }

    if !errors.is_empty() {
        return Err(CommonAudioDataErrors { errors });
    }
    drop(errors);

    const _: () = assert!(MAX_COMMON_DATA_SIZE < u16::MAX as usize);
    assert!(CAD_ADDR + common_data_size < u16::MAX.into());

    assert!(samples_and_instruments.instruments_scrn.len() == n_instruments_and_samples);
    assert!(
        samples_and_instruments
            .pitch_table
            .instruments_pitch_offset_len()
            == n_instruments_and_samples
    );
    assert!(samples_and_instruments.instruments_adsr1.len() == n_instruments_and_samples);
    assert!(samples_and_instruments.instruments_adsr2_or_gain.len() == n_instruments_and_samples);

    let mut out = Vec::with_capacity(common_data_size);

    let mut push_ptr = |start, n_elements, i| {
        let addr = u16::try_from(start + n_elements * i).unwrap();
        out.extend(addr.to_le_bytes());
    };

    push_ptr(pitch_table_addr, n_pitches, 0); // pitchTable_l
    push_ptr(pitch_table_addr, n_pitches, 1); // pitchTable_h

    push_ptr(instruments_soa_addr, n_instruments_and_samples, 0); // instruments_scrn
    push_ptr(instruments_soa_addr, n_instruments_and_samples, 1); // instruments_pitchOffset
    push_ptr(instruments_soa_addr, n_instruments_and_samples, 2); // instruments_adsr1
    push_ptr(instruments_soa_addr, n_instruments_and_samples, 3); // instruments_adsr2OrGain

    push_ptr(sfx_sub_table_addr, n_sfx_subroutines, 0); // sfxSubroutines_l
    push_ptr(sfx_sub_table_addr, n_sfx_subroutines, 1); // sfxSubroutines_h

    push_ptr(sfx_soa_addr, n_sound_effects, 0); // soundEffects_addrAndOneChannelFlag_l
    push_ptr(sfx_soa_addr, n_sound_effects, 1); // soundEffects_addrAndOneChannelFlag_h
    push_ptr(sfx_soa_addr, n_sound_effects, 2); // soundEffects_durationAndInterruptFlag_l
    push_ptr(sfx_soa_addr, n_sound_effects, 3); // soundEffects_durationAndInterruptFlag_h

    out.push(0); // _padding1
    out.push(n_sound_effects.try_into().unwrap());
    out.push(sound_effects.n_high_priority_sfx);
    out.push(sound_effects.low_priority_index);

    let brr_data_addr = u16::try_from(brr_data_addr).unwrap();

    // sfx table
    let sfx_bc_addr = u16::try_from(sfx_bc_addr).unwrap();
    let sfx_sub_bc_addr = u16::try_from(sfx_sub_bc_addr).unwrap();
    let sfx_soa_addr = u16::try_from(sfx_soa_addr).unwrap();
    let pitch_table_addr = u16::try_from(pitch_table_addr).unwrap();

    let sfx_data_and_tables = sfx_bc_addr..pitch_table_addr;
    let sfx_bc_data = sfx_bc_addr..sfx_sub_bc_addr;
    let sfx_soa = sfx_soa_addr..pitch_table_addr;

    assert_eq!(out.len(), COMMON_DATA_HEADER_SIZE);

    for o in &samples_and_instruments.brr_directory_offsets {
        out.extend((o.start + brr_data_addr).to_le_bytes());
        out.extend((o.loop_point + brr_data_addr).to_le_bytes());
    }

    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_bc_addr));
    out.extend(&sound_effects.sfx_data);

    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_sub_bc_addr));
    out.extend(sfx_subroutine_data);

    // soundEffects SoA
    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_soa_addr));
    out.extend(sound_effects.sfx_header_addr_and_one_channel_flag_l_iter(sfx_bc_addr));
    out.extend(sound_effects.sfx_header_addr_and_one_channel_flag_h_iter(sfx_bc_addr));
    out.extend(sound_effects.sfx_header_duration_and_interrupt_flag_l_iter());
    out.extend(sound_effects.sfx_header_duration_and_interrupt_flag_h_iter());

    // SFX subroutine table
    assert_eq!(out.len() + CAD_ADDR, sfx_sub_table_addr);
    out.extend(sfx_subroutines.sfx_subroutines_l_iter(sfx_sub_bc_addr));
    out.extend(sfx_subroutines.sfx_subroutines_h_iter(sfx_sub_bc_addr));

    // pitch table
    assert!(n_pitches <= MAX_N_PITCHES);
    assert_eq!(out.len() + CAD_ADDR, usize::from(pitch_table_addr));
    out.extend(samples_and_instruments.pitch_table.pitch_table_l());
    out.extend(samples_and_instruments.pitch_table.pitch_table_h());

    // instruments SoA
    assert_eq!(out.len() + CAD_ADDR, instruments_soa_addr);
    let instruments_soa_addr = u16::try_from(instruments_soa_addr).unwrap();
    let instruments_soa_offset = out.len();
    out.extend(&samples_and_instruments.instruments_scrn);
    out.extend(
        samples_and_instruments
            .pitch_table
            .instruments_pitch_offset(),
    );
    out.extend(&samples_and_instruments.instruments_adsr1);
    out.extend(&samples_and_instruments.instruments_adsr2_or_gain);

    out.extend(&samples_and_instruments.brr_data);

    assert_eq!(out.len(), common_data_size);

    // Loader can only load a multiple of 2 bytes
    let song_data_addr =
        u16::try_from(CAD_ADDR + common_data_size + (common_data_size % 2)).unwrap();

    assert!(song_data_addr % 2 == 0);

    Ok(CommonAudioData {
        data: out,
        n_dir_items,
        n_instruments_and_samples,
        instruments_soa_offset,

        sfx_data_and_tables,
        sfx_soa,
        sfx_bc_data,

        pitch_table_addr,
        instruments_soa_addr,
        brr_data_addr,
        min_song_data_addr: song_data_addr,
    })
}

/// Common audio data with samples and sfx_subroutines but without compiled sound effect bytecode.
/// Intended to be used in the GUI and allow the GUI to insert sound-effect bytecode into
/// Audio-RAM without interrupting the currently playing song.
///
/// The sfx bytecode is stored in between the common-audio-data and the song-data in Audio-RAM.
///
/// Because the audio driver forbids sfx addresses >= 0x8000, a `goto_relative` instruction is
/// used to jump outside the common-audio-data.
///
/// To ensure a `play_sound_effect` IO command will never play an uninitialised Audio-RAM, the
/// `goto_relative` instruction will be inserted when the CompiledSoundEffect is loaded into
/// audio-RAM using [`ArcCadWithSfxBufferInAram::load_sfx()`].
#[derive(Debug)]
pub struct CommonAudioDataWithSfxBuffer(CommonAudioData);

impl CommonAudioDataWithSfxBuffer {
    pub fn common_data(&self) -> &CommonAudioData {
        &self.0
    }
}

pub fn build_cad_with_sfx_buffer(
    samples_and_instruments: &SampleAndInstrumentData,
    sfx_subroutines: &CompiledSfxSubroutines,
) -> Result<CommonAudioDataWithSfxBuffer, CommonAudioDataErrors> {
    static SFX_DATA: OnceLock<CombinedSoundEffectsData> = OnceLock::new();
    let sound_effects = SFX_DATA.get_or_init(|| tad_gui_sfx_buffer(GOTO_RELATIVE_INSTRUCTION_SIZE));

    Ok(CommonAudioDataWithSfxBuffer(build_common_audio_data(
        samples_and_instruments,
        sfx_subroutines,
        sound_effects,
    )?))
}

pub struct SfxBufferInAram {
    /// Address of the 3 byte `goto_relative` instruction in common-audio-data
    cad_sfx_bc: Range<usize>,
    /// Offset between the GOTO_RELATIVE instruction and the `sfx_bufer`
    goto_offset: u16,
    /// Address of the sfx buffer
    sfx_buffer: Range<u16>,
}

impl SfxBufferInAram {
    pub fn new(
        cad: &CommonAudioDataWithSfxBuffer,
        song_addr: u16,
    ) -> Result<Self, SfxCannotFitInSfxBuffer> {
        let cad_sfx_bc = cad.0.sfx_bytecode_addr_range();
        let sfx_buffer_addr = cad.0.min_song_data_addr();

        assert_eq!(cad_sfx_bc.len(), 3);

        if sfx_buffer_addr < song_addr {
            Ok(Self {
                cad_sfx_bc: usize::from(cad_sfx_bc.start)..usize::from(cad_sfx_bc.end),
                goto_offset: sfx_buffer_addr.wrapping_sub(cad_sfx_bc.start + 2),
                sfx_buffer: sfx_buffer_addr..song_addr,
            })
        } else {
            Err(SfxCannotFitInSfxBuffer())
        }
    }

    pub fn test_sfx_fits_in_apuram(&self, sfx: &CompiledSoundEffect) -> bool {
        sfx.bytecode().len() < self.sfx_buffer.len()
    }

    pub fn load_sfx(
        &self,
        sfx: &CompiledSoundEffect,
        apuram: &mut [u8; AUDIO_RAM_SIZE],
    ) -> Result<(), SfxCannotFitInSfxBuffer> {
        let bc = sfx.bytecode();

        if bc.len() < self.sfx_buffer.len() {
            // Write `goto_relative` instruction.
            {
                let goto_instruction: [u8; GOTO_RELATIVE_INSTRUCTION_SIZE] = [
                    opcodes::GOTO_RELATIVE,
                    self.goto_offset.to_le_bytes()[0],
                    self.goto_offset.to_le_bytes()[1],
                ];

                apuram[self.cad_sfx_bc.clone()].copy_from_slice(&goto_instruction);
            }

            let bc_addr = usize::from(self.sfx_buffer.start);
            apuram[bc_addr..bc_addr + bc.len()].copy_from_slice(bc);

            Ok(())
        } else {
            // Replace cad sfx bytecode with `disable_channel` instructions,
            // to prevent ensure the old sfx bytecode from playing.
            apuram[self.cad_sfx_bc.clone()]
                .copy_from_slice(&[opcodes::DISABLE_CHANNEL; GOTO_RELATIVE_INSTRUCTION_SIZE]);

            Err(SfxCannotFitInSfxBuffer())
        }
    }
}
