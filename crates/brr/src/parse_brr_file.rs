//! A simple mono PCM wave file decoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use ::std::fmt::Display;

use crate::{BrrSample, BRR_HEADER_END_FLAG, BRR_HEADER_LOOP_FLAG, BYTES_PER_BRR_BLOCK};

#[derive(Debug)]
pub enum ParseError {
    Empty,
    InvalidFileSize,
    FileTooLarge,
    EndFlagNotSetInLastBlock,
    SampleEndsEarly,
    TwoLoopPoints,
    MissingLoopPoint,
    BrrSampleNotLooping,
    InvalidLoopPoint,
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
            ParseError::TwoLoopPoints => write!(f, "BRR file has a loop point header"),
            ParseError::MissingLoopPoint => write!(f, "Missing loop point"),
            ParseError::BrrSampleNotLooping => write!(f, "BRR sample not looping"),
            ParseError::InvalidLoopPoint => write!(
                f,
                "Invalid loop point (not a multiple of {})",
                BYTES_PER_BRR_BLOCK
            ),
            ParseError::LoopPointOutOfRange(lp, max) => {
                write!(f, "Loop point out of bounds ({lp}, max: {max}")
            }
        }
    }
}

pub fn parse_brr_file(input: &[u8], loop_offset: Option<usize>) -> Result<BrrSample, ParseError> {
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

    // safe - brr_data is
    let last_block_header = rblocks.next().unwrap()[0];

    if last_block_header & BRR_HEADER_END_FLAG == 0 {
        return Err(ParseError::EndFlagNotSetInLastBlock);
    }

    for block in rblocks {
        let block_header = block[0];
        if block_header & BRR_HEADER_END_FLAG != 0 {
            return Err(ParseError::SampleEndsEarly);
        }
    }

    let loop_offset = if last_block_header & BRR_HEADER_LOOP_FLAG != 0 {
        // Loop flag set
        let lo = match (loop_offset, lo_in_file) {
            (Some(lo), None) => lo,
            (None, Some(lo)) => lo.into(),
            (Some(_), Some(_)) => return Err(ParseError::TwoLoopPoints),
            (None, None) => return Err(ParseError::MissingLoopPoint),
        };

        let max_loop_offset = brr_data.len() - BYTES_PER_BRR_BLOCK;
        if lo % BYTES_PER_BRR_BLOCK != 0 {
            return Err(ParseError::InvalidLoopPoint);
        }
        if lo > max_loop_offset {
            return Err(ParseError::LoopPointOutOfRange(lo, max_loop_offset));
        }

        let lo = u16::try_from(lo).unwrap();

        Some(lo)
    } else {
        // Loop flag clear
        if loop_offset.is_some() {
            return Err(ParseError::BrrSampleNotLooping);
        }

        // Ignore loop offset in BRR file (even if it exists)
        None
    };

    Ok(BrrSample {
        loop_offset,
        brr_data: brr_data.to_vec(),
    })
}
