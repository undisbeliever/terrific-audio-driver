//! Common audio data compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::{
    COMMON_DATA_BYTES_PER_DIR, COMMON_DATA_BYTES_PER_INSTRUMENTS,
    COMMON_DATA_BYTES_PER_SOUND_EFFECT, COMMON_DATA_HEADER_ADDR, COMMON_DATA_HEADER_SIZE,
    MAX_COMMON_DATA_SIZE, MAX_INSTRUMENTS, MAX_SOUND_EFFECTS,
};
use crate::errors::{CommonAudioDataError, CommonAudioDataErrors};
use crate::samples::{build_sample_and_instrument_data, SampleAndInstrumentData};
use crate::sound_effects::{compile_sound_effects_file, CompiledSoundEffects, SoundEffectsFile};
use crate::MappingsFile;

pub fn build_common_audio_data(
    samples: &SampleAndInstrumentData,
    sound_effects: &CompiledSoundEffects,
) -> Result<Vec<u8>, CommonAudioDataErrors> {
    let mut errors = Vec::new();

    let n_instruments = samples.n_instruments;
    let n_dir_items = samples.brr_directory_offsets.len();
    let n_sound_effects = sound_effects.sfx_offsets.len();

    if n_instruments > MAX_INSTRUMENTS {
        errors.push(CommonAudioDataError::TooManyInstruments(n_instruments));
    }
    if n_dir_items > MAX_INSTRUMENTS {
        errors.push(CommonAudioDataError::TooManySamples(n_dir_items));
    }
    if n_sound_effects > MAX_SOUND_EFFECTS {
        errors.push(CommonAudioDataError::TooManySoundEffects(n_sound_effects));
    }

    let header_size = COMMON_DATA_HEADER_SIZE
        + n_dir_items * COMMON_DATA_BYTES_PER_DIR
        + n_instruments * COMMON_DATA_BYTES_PER_INSTRUMENTS
        + n_sound_effects * COMMON_DATA_BYTES_PER_SOUND_EFFECT;

    let common_data_size = header_size + samples.brr_data.len() + sound_effects.sfx_data.len();

    if common_data_size > MAX_COMMON_DATA_SIZE {
        errors.push(CommonAudioDataError::CommonAudioDataTooLarge(
            common_data_size,
        ));
    }

    if !errors.is_empty() {
        return Err(errors);
    }
    let _disable_errors = errors;

    const _: () = assert!(MAX_COMMON_DATA_SIZE < u16::MAX as usize);
    assert!(usize::from(COMMON_DATA_HEADER_ADDR) + common_data_size < u16::MAX.into());

    assert!(samples.instruments_scrn.len() == n_instruments);
    assert!(samples.pitch_table.instruments_pitch_offset.len() == n_instruments);
    assert!(samples.instruments_adsr1.len() == n_instruments);
    assert!(samples.instruments_adsr2_or_gain.len() == n_instruments);

    let brr_data_addr: u16 = COMMON_DATA_HEADER_ADDR + u16::try_from(header_size).unwrap();
    let sfx_data_addr: u16 = brr_data_addr + u16::try_from(samples.brr_data.len()).unwrap();

    let mut out = Vec::with_capacity(common_data_size);

    out.push(u8::try_from(n_dir_items).unwrap());
    out.push(u8::try_from(n_instruments).unwrap());
    out.push(u8::try_from(n_sound_effects).unwrap());
    out.push(0); // padding

    out.extend(samples.pitch_table.table_data_l);
    out.extend(samples.pitch_table.table_data_h);

    assert_eq!(out.len(), COMMON_DATA_HEADER_SIZE);

    for o in &samples.brr_directory_offsets {
        out.extend((o.start + brr_data_addr).to_le_bytes());
        out.extend((o.loop_point + brr_data_addr).to_le_bytes());
    }

    // instruments SoA
    out.extend(&samples.instruments_scrn);
    out.extend(&samples.pitch_table.instruments_pitch_offset);
    out.extend(&samples.instruments_adsr1);
    out.extend(&samples.instruments_adsr2_or_gain);

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

    out.extend(&samples.brr_data);
    out.extend(&sound_effects.sfx_data);

    assert_eq!(out.len(), common_data_size);

    Ok(out)
}

pub fn compile_common_audio_data(
    mappings: &MappingsFile,
    sfx_file: &SoundEffectsFile,
) -> Result<Vec<u8>, CommonAudioDataErrors> {
    let samples = build_sample_and_instrument_data(mappings);
    let sound_effects = compile_sound_effects_file(sfx_file, mappings);

    if let (Ok(samples), Ok(sound_effects)) = (&samples, &sound_effects) {
        build_common_audio_data(samples, sound_effects)
    } else {
        let mut errors = Vec::new();
        if let Err(e) = samples {
            errors.push(CommonAudioDataError::SampleError(e));
        }
        if let Err(e) = sound_effects {
            errors.push(CommonAudioDataError::SoundEffectError(e));
        }
        Err(errors)
    }
}
