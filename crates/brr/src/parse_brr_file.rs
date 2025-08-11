//! A simple mono PCM wave file decoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use ::std::fmt::Display;

use crate::{
    BrrSample, BRR_HEADER_END_FLAG, BRR_HEADER_LOOP_FLAG, BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK,
};

#[derive(Debug, Clone)]
pub enum ParseError {
    Empty,
    InvalidFileSize,
    FileTooLarge,
    EndFlagNotSetInLastBlock,
    SampleEndsEarly,
    MissingLoopPoint,
    BrrSampleNotLooping,
    InvalidLoopPointHeaderBytes,
    InvalidLoopPointSamples,
    LoopPointOutOfRange(usize, usize),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Empty => write!(f, "BRR file is empty"),
            ParseError::InvalidFileSize => write!(f, "BRR file size is invalid"),
            ParseError::FileTooLarge => write!(f, "BRR file is too large"),
            ParseError::EndFlagNotSetInLastBlock => {
                write!(f, "End flag not set in the last BRR block")
            }
            ParseError::SampleEndsEarly => write!(f, "Sample ends too early"),
            ParseError::MissingLoopPoint => write!(f, "Missing loop point"),
            ParseError::BrrSampleNotLooping => write!(f, "BRR sample not looping"),
            ParseError::InvalidLoopPointHeaderBytes => write!(
                f,
                "Invalid loop point offset in BRR file (not a multiple of {})",
                BYTES_PER_BRR_BLOCK
            ),
            ParseError::InvalidLoopPointSamples => write!(
                f,
                "Invalid loop point (not a multiple of {})",
                SAMPLES_PER_BLOCK
            ),
            ParseError::LoopPointOutOfRange(lp, max) => {
                write!(f, "Loop point out of bounds ({lp}, max: {max}")
            }
        }
    }
}

#[derive(Clone)]
pub struct ValidBrrFile {
    brr_data: Vec<u8>,
    loop_flag: bool,
    loop_offset: Option<u16>,
}

impl ValidBrrFile {
    pub fn brr_data(&self) -> &[u8] {
        &self.brr_data
    }

    /// If loop_point is Some and the BRR File contains a loop header, loop_point will override
    /// the BRR loop header.
    pub fn into_brr_sample(self, loop_point: Option<usize>) -> Result<BrrSample, ParseError> {
        let loop_offset = match (self.loop_offset, loop_point) {
            (_, Some(lp)) => Some(loop_point_to_loop_offset(lp, self.brr_data.len())?),
            (Some(lo), None) => Some(lo),
            (None, None) => None,
        };

        match self.loop_flag {
            true => {
                if loop_offset.is_none() {
                    return Err(ParseError::MissingLoopPoint);
                }
            }
            false => {
                if loop_offset.is_some() {
                    return Err(ParseError::BrrSampleNotLooping);
                }
            }
        }

        Ok(BrrSample {
            loop_offset,
            brr_data: self.brr_data,
        })
    }
}

pub fn parse_brr_file(input: &[u8]) -> Result<ValidBrrFile, ParseError> {
    let (lo_in_file, brr_data) = match input.len() % BYTES_PER_BRR_BLOCK {
        0 => {
            // No two-byte header, use loop_offset.
            (None, input)
        }
        2 => {
            // BRR file has a two byte loop point header
            if input.len() <= 2 {
                return Err(ParseError::Empty);
            }

            let (lo, brr_data) = input.split_at(2);
            let lo = u16::from_le_bytes([lo[0], lo[1]]);

            (Some(lo), brr_data)
        }
        _ => {
            return Err(ParseError::InvalidFileSize);
        }
    };

    if brr_data.is_empty() {
        return Err(ParseError::Empty);
    }
    if brr_data.len() % BYTES_PER_BRR_BLOCK != 0 {
        return Err(ParseError::InvalidFileSize);
    }
    if brr_data.len() > u16::MAX.into() {
        return Err(ParseError::FileTooLarge);
    }

    let mut rblocks = brr_data.rchunks_exact(BYTES_PER_BRR_BLOCK);

    // safe - rblocks is non-empty
    let last_block_header = rblocks.next().unwrap()[0];

    if last_block_header & BRR_HEADER_END_FLAG == 0 {
        return Err(ParseError::EndFlagNotSetInLastBlock);
    }

    // Process remaining blocks
    for block in rblocks {
        let block_header = block[0];
        if block_header & BRR_HEADER_END_FLAG != 0 {
            return Err(ParseError::SampleEndsEarly);
        }
    }

    let loop_flag = last_block_header & BRR_HEADER_LOOP_FLAG != 0;

    let loop_offset = match (lo_in_file, loop_flag) {
        (Some(lo), true) => {
            let max_loop_offset = brr_data.len() - BYTES_PER_BRR_BLOCK;
            if lo % BYTES_PER_BRR_BLOCK as u16 != 0 {
                return Err(ParseError::InvalidLoopPointHeaderBytes);
            }
            if usize::from(lo) > max_loop_offset {
                return Err(ParseError::LoopPointOutOfRange(lo.into(), max_loop_offset));
            }

            Some(lo)
        }
        // Ignore loop offset if the BRR file does not loop
        _ => None,
    };

    Ok(ValidBrrFile {
        loop_flag,
        loop_offset,
        brr_data: brr_data.to_vec(),
    })
}

fn loop_point_to_loop_offset(lp: usize, brr_data_size: usize) -> Result<u16, ParseError> {
    if lp % SAMPLES_PER_BLOCK != 0 {
        return Err(ParseError::InvalidLoopPointSamples);
    }

    let max_loop_point = brr_data_size / BYTES_PER_BRR_BLOCK * SAMPLES_PER_BLOCK;
    if lp >= max_loop_point {
        return Err(ParseError::LoopPointOutOfRange(lp, max_loop_point));
    }

    assert!(brr_data_size < u16::MAX.into());
    Ok(u16::try_from(lp / SAMPLES_PER_BLOCK * BYTES_PER_BRR_BLOCK).unwrap())
}
