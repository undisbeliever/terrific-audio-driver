//! Song compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::data::UniqueNamesMappingsFile;
use crate::driver_constants::{
    MAX_SONG_DATA_SIZE, MAX_SUBROUTINES, N_MUSIC_CHANNELS, SONG_HEADER_SIZE,
};
use crate::errors::SongError;
use crate::mml::MmlData;
use crate::{build_pitch_table, parse_mml};

const NULL_OFFSET: u16 = 0xffff_u16;

fn validate_data_size(data: &[u8], expected_size: usize) -> Result<(), SongError> {
    if data.len() == expected_size {
        Ok(())
    } else {
        Err(SongError::DataSizeMismatch(data.len(), expected_size))
    }
}

pub fn song_data(mml_data: &MmlData) -> Result<Vec<u8>, SongError> {
    let metadata = mml_data.metadata();
    let channels = mml_data.channels();
    let subroutines = mml_data.subroutines();

    let echo_buffer = &metadata.echo_buffer;

    if channels.is_empty() {
        return Err(SongError::NoMusicChannels);
    }

    assert!(channels.len() < N_MUSIC_CHANNELS);
    assert!(subroutines.len() < MAX_SUBROUTINES);

    let channel_data_size: usize = channels.iter().map(|c| c.bytecode().len()).sum();
    let subroutine_data_size: usize = subroutines.iter().map(|s| s.bytecode().len()).sum();

    let header_size = SONG_HEADER_SIZE + subroutines.len() * 2;
    let total_size = header_size + channel_data_size + subroutine_data_size;

    if total_size > MAX_SONG_DATA_SIZE {
        return Err(SongError::SongIsTooLarge(total_size));
    }
    const _: () = assert!(MAX_SONG_DATA_SIZE < u16::MAX as usize);

    let mut out = Vec::with_capacity(header_size);
    {
        let mut data_offset: u16 = header_size.try_into().unwrap();

        // Channel data
        for i in 0..N_MUSIC_CHANNELS {
            let (start_offset, loop_offset) = match channels.get(i) {
                Some(c) => {
                    let c_size: u16 = c.bytecode().len().try_into().unwrap();

                    let loop_offset: u16 = match c.loop_point() {
                        Some(lp) => {
                            if lp.bytecode_offset >= c_size.into() {
                                return Err(SongError::InvalidMmlData);
                            }
                            u16::try_from(lp.bytecode_offset).unwrap()
                        }
                        None => NULL_OFFSET,
                    };

                    let offset = data_offset;
                    data_offset += c_size;

                    (offset, loop_offset)
                }
                None => (NULL_OFFSET, NULL_OFFSET),
            };

            out.extend(start_offset.to_le_bytes());
            out.extend(loop_offset.to_le_bytes());
        }

        // Echo buffer settings
        out.push(echo_buffer.edl.as_u8());
        for f in echo_buffer.fir {
            out.extend(f.to_le_bytes());
        }
        out.extend(echo_buffer.feedback.to_le_bytes());
        out.extend(echo_buffer.echo_volume.to_le_bytes());

        // Tick timer
        out.push(metadata.tick_clock.as_u8());

        // Subroutine table
        out.push(subroutines.len().try_into().unwrap());

        for s in subroutines {
            out.extend(data_offset.to_le_bytes());

            let s_size: u16 = s.bytecode().len().try_into().unwrap();
            data_offset += s_size;
        }

        validate_data_size(&out, header_size)?;
    }

    for c in channels {
        out.extend(c.bytecode());
    }
    for s in subroutines {
        out.extend(s.bytecode());
    }

    validate_data_size(&out, total_size)?;

    Ok(out)
}

pub fn compile_song(
    mml_text: &str,
    file_name: &str,
    mappings: &UniqueNamesMappingsFile,
) -> Result<Vec<u8>, SongError> {
    let instruments = &mappings.instruments;

    let pitch_table = match build_pitch_table(instruments) {
        Ok(pt) => pt,
        Err(e) => return Err(SongError::PitchTableError(mappings.file_name.clone(), e)),
    };

    let mml = match parse_mml(mml_text, &mappings.instruments, &pitch_table) {
        Ok(mml) => mml,
        Err(e) => return Err(SongError::MmlError(file_name.to_string(), e)),
    };

    song_data(&mml)
}
