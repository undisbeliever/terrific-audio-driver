//! Common audio data compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::{
    addresses, COMMON_DATA_BYTES_PER_DIR, COMMON_DATA_BYTES_PER_INSTRUMENTS,
    COMMON_DATA_BYTES_PER_SOUND_EFFECT, COMMON_DATA_HEADER_SIZE, MAX_COMMON_DATA_SIZE,
    MAX_DIR_ITEMS, MAX_INSTRUMENTS_AND_SAMPLES, MAX_SOUND_EFFECTS,
};
use crate::errors::{CommonAudioDataError, CommonAudioDataErrors};
use crate::samples::SampleAndInstrumentData;
use crate::sound_effects::CombinedSoundEffectsData;

#[derive(Clone)]
pub struct CommonAudioData(Vec<u8>);

impl CommonAudioData {
    pub fn data(&self) -> &[u8] {
        &self.0
    }
}

pub fn build_common_audio_data(
    samples_and_instruments: &SampleAndInstrumentData,
    sound_effects: &CombinedSoundEffectsData,
) -> Result<CommonAudioData, CommonAudioDataErrors> {
    let mut errors = Vec::new();

    let n_instruments_and_samples = samples_and_instruments.n_instruments;
    let n_dir_items = samples_and_instruments.brr_directory_offsets.len();
    let n_sound_effects = sound_effects.sfx_offsets.len();

    if n_instruments_and_samples > MAX_INSTRUMENTS_AND_SAMPLES {
        errors.push(CommonAudioDataError::TooManyInstrumentsAndSamples(
            n_instruments_and_samples,
        ));
    }
    if n_dir_items > MAX_DIR_ITEMS {
        errors.push(CommonAudioDataError::TooManyBrrSamples(n_dir_items));
    }
    if n_sound_effects > MAX_SOUND_EFFECTS {
        errors.push(CommonAudioDataError::TooManySoundEffects(n_sound_effects));
    }

    let header_size = COMMON_DATA_HEADER_SIZE
        + n_dir_items * COMMON_DATA_BYTES_PER_DIR
        + n_instruments_and_samples * COMMON_DATA_BYTES_PER_INSTRUMENTS
        + n_sound_effects * COMMON_DATA_BYTES_PER_SOUND_EFFECT;

    let common_data_size =
        header_size + samples_and_instruments.brr_data.len() + sound_effects.sfx_data.len();

    if common_data_size > MAX_COMMON_DATA_SIZE {
        errors.push(CommonAudioDataError::CommonAudioDataTooLarge(
            common_data_size,
        ));
    }

    if !errors.is_empty() {
        return Err(CommonAudioDataErrors { errors });
    }
    let _disable_errors = errors;

    const _: () = assert!(MAX_COMMON_DATA_SIZE < u16::MAX as usize);
    assert!(usize::from(addresses::COMMON_DATA) + common_data_size < u16::MAX.into());

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

    let brr_data_addr: u16 = addresses::COMMON_DATA + u16::try_from(header_size).unwrap();
    let sfx_data_addr: u16 =
        brr_data_addr + u16::try_from(samples_and_instruments.brr_data.len()).unwrap();

    let mut out = Vec::with_capacity(common_data_size);

    out.push(u8::try_from(n_dir_items).unwrap());
    out.push(u8::try_from(n_instruments_and_samples).unwrap());
    out.push(u8::try_from(n_sound_effects).unwrap());
    out.push(0); // padding

    out.extend(samples_and_instruments.pitch_table.table_data_l);
    out.extend(samples_and_instruments.pitch_table.table_data_h);

    assert_eq!(out.len(), COMMON_DATA_HEADER_SIZE);

    for o in &samples_and_instruments.brr_directory_offsets {
        out.extend((o.start + brr_data_addr).to_le_bytes());
        out.extend((o.loop_point + brr_data_addr).to_le_bytes());
    }

    // instruments SoA
    out.extend(&samples_and_instruments.instruments_scrn);
    out.extend(&samples_and_instruments.pitch_table.instruments_pitch_offset);
    out.extend(&samples_and_instruments.instruments_adsr1);
    out.extend(&samples_and_instruments.instruments_adsr2_or_gain);

    // soundEffects_l
    for o in &sound_effects.sfx_offsets {
        // This should never panic, `o` < sfx_data.len().
        let addr = u16::try_from(*o).unwrap() + sfx_data_addr;
        out.push(addr.to_le_bytes()[0]);
    }

    // soundEffects_h
    for o in &sound_effects.sfx_offsets {
        let addr = u16::try_from(*o).unwrap() + sfx_data_addr;
        out.push(addr.to_le_bytes()[1]);
    }

    assert_eq!(out.len(), header_size);

    out.extend(&samples_and_instruments.brr_data);
    out.extend(&sound_effects.sfx_data);

    assert_eq!(out.len(), common_data_size);

    Ok(CommonAudioData(out))
}
