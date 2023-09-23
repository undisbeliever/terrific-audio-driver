//! Song compiler

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::driver_constants::{
    AUDIO_RAM_SIZE, COMMON_DATA_ADDR, MAX_SONG_DATA_SIZE, MAX_SUBROUTINES, N_MUSIC_CHANNELS,
    SONG_HEADER_SIZE,
};
use crate::errors::{SongError, SongTooLargeError};
use crate::mml::{MetaData, MmlData};

const NULL_OFFSET: u16 = 0xffff_u16;

fn validate_data_size(data: &[u8], expected_size: usize) -> Result<(), SongError> {
    if data.len() == expected_size {
        Ok(())
    } else {
        Err(SongError::DataSizeMismatch(data.len(), expected_size))
    }
}

pub struct SongData {
    metadata: MetaData,
    data: Vec<u8>,
}

impl SongData {
    pub fn metadata(&self) -> &MetaData {
        &self.metadata
    }
    pub fn data(&self) -> &[u8] {
        &self.data
    }
}

pub fn song_data(mml_data: MmlData) -> Result<SongData, SongError> {
    let metadata = mml_data.metadata();
    let channels = mml_data.channels();
    let subroutines = mml_data.subroutines();

    let echo_buffer = &metadata.echo_buffer;

    if channels.is_empty() {
        return Err(SongError::NoMusicChannels);
    }

    assert!(channels.len() <= N_MUSIC_CHANNELS);
    assert!(subroutines.len() <= MAX_SUBROUTINES);

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

    Ok(SongData {
        metadata: mml_data.take_metadata(),
        data: out,
    })
}

pub fn validate_song_size(
    song: &SongData,
    common_data_size: usize,
) -> Result<(), SongTooLargeError> {
    let song_data_size = song.data().len();
    let echo_buffer_size = song.metadata().echo_buffer.edl.buffer_size();

    // Loader can only transfer data that is a multiple of 2 bytes
    let common_data_size = common_data_size + (common_data_size % 2);
    let song_data_size = song_data_size + (song_data_size % 2);

    let total_size = common_data_size + song_data_size + echo_buffer_size;

    let end_addr = usize::from(COMMON_DATA_ADDR) + total_size;

    if end_addr <= AUDIO_RAM_SIZE {
        Ok(())
    } else {
        let too_large_by = end_addr - AUDIO_RAM_SIZE;
        Err(SongTooLargeError {
            too_large_by,
            common_data_size,
            song_data_size,
            echo_buffer_size,
        })
    }
}
