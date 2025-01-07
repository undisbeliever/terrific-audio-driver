//! Common audio data compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::{
    addresses, COMMON_DATA_BYTES_PER_DIR, COMMON_DATA_BYTES_PER_INSTRUMENT,
    COMMON_DATA_BYTES_PER_PITCH, COMMON_DATA_BYTES_PER_SFX_SUBROUTINE,
    COMMON_DATA_BYTES_PER_SOUND_EFFECT, COMMON_DATA_DIR_TABLE_OFFSET, COMMON_DATA_HEADER_SIZE,
    MAX_COMMON_DATA_SIZE, MAX_DIR_ITEMS, MAX_INSTRUMENTS_AND_SAMPLES, MAX_N_PITCHES,
    MAX_SFX_DATA_ADDR, MAX_SFX_SUBROUTINES, MAX_SOUND_EFFECTS,
};
use crate::errors::{CommonAudioDataError, CommonAudioDataErrors};
use crate::samples::SampleAndInstrumentData;
use crate::sound_effects::{CombinedSoundEffectsData, CompiledSfxSubroutines};

use std::ops::Range;

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
    song_data_addr: u16,
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
        self.brr_data_addr..self.song_data_addr
    }

    pub fn song_data_addr(&self) -> u16 {
        self.song_data_addr
    }

    pub fn aram_size(&self) -> u16 {
        self.song_data_addr - addresses::COMMON_DATA
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

    let sfx_sub_bc_addr = CAD_ADDR + header_size;
    let sfx_sub_table_addr = sfx_sub_bc_addr + sfx_subroutine_data.len();
    let sfx_bc_addr = sfx_sub_table_addr + n_sfx_subroutines * COMMON_DATA_BYTES_PER_SFX_SUBROUTINE;
    let sfx_soa_addr = sfx_bc_addr + sound_effects.sfx_data.len();
    let pitch_table_addr = sfx_soa_addr + n_sound_effects * COMMON_DATA_BYTES_PER_SOUND_EFFECT;
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
    let _disable_errors = errors;

    const _: () = assert!(MAX_COMMON_DATA_SIZE < u16::MAX as usize);
    assert!(CAD_ADDR + common_data_size < u16::MAX.into());

    assert!(samples_and_instruments.instruments_scrn.len() == n_instruments_and_samples);
    assert!(
        samples_and_instruments
            .pitch_table
            .instruments_pitch_offset
            .len()
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

    let sfx_sub_bc_addr = u16::try_from(sfx_sub_bc_addr).unwrap();
    // sfx table
    let sfx_bc_addr = u16::try_from(sfx_bc_addr).unwrap();
    let sfx_soa_addr = u16::try_from(sfx_soa_addr).unwrap();
    let pitch_table_addr = u16::try_from(pitch_table_addr).unwrap();

    let sfx_data_and_tables = sfx_sub_bc_addr..pitch_table_addr;
    let sfx_bc_data = sfx_bc_addr..sfx_soa_addr;
    let sfx_soa = sfx_soa_addr..pitch_table_addr;

    assert_eq!(out.len(), COMMON_DATA_HEADER_SIZE);

    for o in &samples_and_instruments.brr_directory_offsets {
        out.extend((o.start + brr_data_addr).to_le_bytes());
        out.extend((o.loop_point + brr_data_addr).to_le_bytes());
    }

    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_sub_bc_addr));
    out.extend(sfx_subroutine_data);

    assert_eq!(out.len() + CAD_ADDR, sfx_sub_table_addr);
    out.extend(sfx_subroutines.sfx_subroutines_l_iter(sfx_sub_bc_addr));
    out.extend(sfx_subroutines.sfx_subroutines_h_iter(sfx_sub_bc_addr));

    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_bc_addr));
    out.extend(&sound_effects.sfx_data);

    // soundEffects SoA
    assert_eq!(out.len() + CAD_ADDR, usize::from(sfx_soa_addr));
    out.extend(sound_effects.sfx_header_addr_and_one_channel_flag_l_iter(sfx_bc_addr));
    out.extend(sound_effects.sfx_header_addr_and_one_channel_flag_h_iter(sfx_bc_addr));
    out.extend(sound_effects.sfx_header_duration_and_interrupt_flag_l_iter());
    out.extend(sound_effects.sfx_header_duration_and_interrupt_flag_h_iter());

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
    out.extend(&samples_and_instruments.pitch_table.instruments_pitch_offset);
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
        song_data_addr,
    })
}
