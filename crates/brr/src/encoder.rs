//! BRR encoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::BitAnd;

use crate::{
    BrrFilter, BrrSample, BRR_HEADER_END_FLAG, BRR_HEADER_LOOP_FLAG, BYTES_PER_BRR_BLOCK,
    SAMPLES_PER_BLOCK,
};

const MAX_SHIFT: u8 = 12;

const I4_MIN: i32 = -8;
const I4_MAX: i32 = 7;

#[derive(Debug, Clone)]
pub enum EncodeError {
    NoSamples,
    InvalidNumberOfSamples,
    TooManySamples,
    InvalidLoopPoint,
    LoopPointTooLarge(usize, usize),
    DupeBlockHackNotAllowedWithLoopPoint,
    DupeBlockHackNotAllowedWithLoopResetsFilter,
    DupeBlockHackTooLarge,
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncodeError::NoSamples => write!(f, "no samples"),
            EncodeError::InvalidNumberOfSamples => write!(
                f,
                "number of samples is not a multiple of {SAMPLES_PER_BLOCK}"
            ),
            EncodeError::TooManySamples => write!(f, "too many samples"),
            EncodeError::InvalidLoopPoint => {
                write!(f, "loop_point is not a multiple of {SAMPLES_PER_BLOCK}")
            }
            EncodeError::LoopPointTooLarge(lp, s_len) => write!(
                f,
                "loop_point is too large ({}, max {})",
                lp,
                s_len - SAMPLES_PER_BLOCK
            ),
            EncodeError::DupeBlockHackNotAllowedWithLoopPoint => {
                write!(f, "dupe_block_hack not allowed when loop_point is set")
            }
            EncodeError::DupeBlockHackNotAllowedWithLoopResetsFilter => {
                write!(
                    f,
                    "dupe_block_hack does nothing when loop_resets_filter is set"
                )
            }
            EncodeError::DupeBlockHackTooLarge => write!(f, "dupe_block_hack value is too large"),
        }
    }
}

struct BrrBlock {
    filter: BrrFilter,
    shift: u8,
    // signed 4-bit values
    nibbles: [i8; 16],
    decoded_samples: [i16; SAMPLES_PER_BLOCK],
}

fn build_block(
    samples: &[i16; SAMPLES_PER_BLOCK],
    shift: u8,
    filter: BrrFilter,
    filter_fn: fn(i32, i32) -> i32,
    prev1: i16,
    prev2: i16,
) -> BrrBlock {
    assert!(shift <= MAX_SHIFT);

    let mut nibbles = [0; SAMPLES_PER_BLOCK];
    let mut decoded_samples = [0; SAMPLES_PER_BLOCK];

    let div: i32 = 1 << shift;

    let mut p1 = prev1 as i32;
    let mut p2 = prev2 as i32;

    for (i, s) in samples.iter().enumerate() {
        let s: i32 = (*s).into();

        let offset = filter_fn(p1, p2);

        let n = ((s - offset) / div).clamp(I4_MIN, I4_MAX);
        let s = n * div + offset;

        // Sample is clamped to 16 bit and then clipped to 15 bit.
        // (source: Anomie's S-DSP Doc)
        //
        // The clamp/clip values are left-shifted by 1 as `build_block()` encodes 16 bit samples.

        // clamp to 16 bit
        let s = s.clamp(i32::from(i16::MIN) << 1, i32::from(i16::MAX) << 1);
        // clip to 15 bit
        let s = s.bitand(0x7fff_i32 << 1);
        let s = s as i16; // truncate

        p2 = p1;
        p1 = s as i32;

        nibbles[i] = n.try_into().unwrap();
        decoded_samples[i] = s;
    }

    BrrBlock {
        filter,
        shift,
        nibbles,
        decoded_samples,
    }
}

fn filter0(_p1: i32, _p2: i32) -> i32 {
    0
}
fn filter1(p1: i32, _p2: i32) -> i32 {
    p1 * 15 / 16
}
fn filter2(p1: i32, p2: i32) -> i32 {
    (p1 * 61 / 32) - (p2 * 15 / 16)
}
fn filter3(p1: i32, p2: i32) -> i32 {
    (p1 * 115 / 64) - (p2 * 13 / 16)
}

fn calc_squared_error(block: &BrrBlock, samples: &[i16; SAMPLES_PER_BLOCK]) -> i64 {
    assert!(block.decoded_samples.len() == samples.len());

    let mut square_error = 0;

    for (b, s) in block.decoded_samples.iter().zip(samples) {
        let delta = i64::from(*b) - i64::from(*s);

        square_error += delta * delta;
    }

    square_error
}

fn find_best_block(samples: &[i16; SAMPLES_PER_BLOCK], prev1: i16, prev2: i16) -> BrrBlock {
    let mut best_block = None;
    let mut best_block_score = i64::MAX;

    let mut test_filter = |filter, filter_fn| {
        for shift in 0..=MAX_SHIFT {
            let block = build_block(samples, shift, filter, filter_fn, prev1, prev2);

            let score = calc_squared_error(&block, samples);
            if score < best_block_score {
                best_block = Some(block);
                best_block_score = score;
            }
        }
    };

    test_filter(BrrFilter::Filter0, filter0);
    test_filter(BrrFilter::Filter1, filter1);
    test_filter(BrrFilter::Filter2, filter2);
    test_filter(BrrFilter::Filter3, filter3);

    best_block.unwrap()
}

fn find_best_block_filter(
    samples: &[i16; SAMPLES_PER_BLOCK],
    filter: BrrFilter,
    prev1: i16,
    prev2: i16,
) -> BrrBlock {
    let test_filter = |filter, filter_fn| {
        (0..=MAX_SHIFT)
            .map(|shift| build_block(samples, shift, filter, filter_fn, prev1, prev2))
            .min_by_key(|block| calc_squared_error(block, samples))
            .unwrap()
    };

    match filter {
        BrrFilter::Filter0 => test_filter(BrrFilter::Filter0, filter0),
        BrrFilter::Filter1 => test_filter(BrrFilter::Filter1, filter1),
        BrrFilter::Filter2 => test_filter(BrrFilter::Filter2, filter2),
        BrrFilter::Filter3 => test_filter(BrrFilter::Filter3, filter3),
    }
}

// Loop flag only set if end_flag is set.
fn encode_block(block: BrrBlock, end_flag: bool, loop_flag: bool) -> [u8; BYTES_PER_BRR_BLOCK] {
    assert!(block.shift <= MAX_SHIFT);

    let mut out = [0; BYTES_PER_BRR_BLOCK];

    // Header
    let mut header = ((block.shift & 0xf) << 4) | ((block.filter.as_u8()) << 2);
    if end_flag {
        header |= BRR_HEADER_END_FLAG;

        if loop_flag {
            header |= BRR_HEADER_LOOP_FLAG;
        }
    }

    out[0] = header;

    for (i, o) in out.iter_mut().skip(1).enumerate() {
        let nibble0 = block.nibbles[i * 2].to_le_bytes()[0];
        let nibble1 = block.nibbles[i * 2 + 1].to_le_bytes()[0];

        *o = ((nibble0 & 0xf) << 4) | (nibble1 & 0xf);
    }

    out
}

pub fn encode_brr(
    samples: &[i16],
    loop_offset: Option<usize>,
    dupe_block_hack: Option<usize>,
    loop_filter: Option<BrrFilter>,
) -> Result<BrrSample, EncodeError> {
    if samples.is_empty() {
        return Err(EncodeError::NoSamples);
    }

    if samples.len() % SAMPLES_PER_BLOCK != 0 {
        return Err(EncodeError::InvalidNumberOfSamples);
    }

    if samples.len() > u16::MAX.into() {
        return Err(EncodeError::TooManySamples);
    }

    let (loop_flag, loop_block, loop_offset) = match (loop_offset, dupe_block_hack) {
        (None, None) => (false, usize::MAX, None),
        (Some(lp), None) => {
            if lp % SAMPLES_PER_BLOCK != 0 {
                return Err(EncodeError::InvalidLoopPoint);
            }
            if lp >= samples.len() {
                return Err(EncodeError::LoopPointTooLarge(lp, samples.len()));
            }

            let loop_block = lp / SAMPLES_PER_BLOCK;

            // safe, `samples.len() is <= u16::MAX`
            let loop_offset = u16::try_from(loop_block * BYTES_PER_BRR_BLOCK).unwrap();

            (true, loop_block, Some(loop_offset))
        }
        (None, Some(dbh)) => {
            if dbh > 64 {
                return Err(EncodeError::DupeBlockHackTooLarge);
            }

            if loop_filter == Some(BrrFilter::Filter0) {
                return Err(EncodeError::DupeBlockHackNotAllowedWithLoopResetsFilter);
            }

            let loop_block = dbh;
            let loop_offset = u16::try_from(dbh * BYTES_PER_BRR_BLOCK).unwrap();

            (true, loop_block, Some(loop_offset))
        }
        (Some(_), Some(_)) => {
            return Err(EncodeError::DupeBlockHackNotAllowedWithLoopPoint);
        }
    };

    let n_blocks = samples.len() / SAMPLES_PER_BLOCK + dupe_block_hack.unwrap_or(0);
    let last_block_index = n_blocks - 1;

    let mut brr_data = Vec::with_capacity(n_blocks * BYTES_PER_BRR_BLOCK);

    let mut prev1 = 0;
    let mut prev2 = 0;

    for (i, samples) in samples
        .chunks_exact(SAMPLES_PER_BLOCK)
        .cycle()
        .take(n_blocks)
        .enumerate()
    {
        let block = if i == 0 {
            // The first block always uses filter 0
            find_best_block_filter(samples.try_into().unwrap(), BrrFilter::Filter0, 0, 0)
        } else if i == loop_block {
            match loop_filter {
                None => find_best_block(samples.try_into().unwrap(), prev1, prev2),
                Some(loop_filter) => {
                    find_best_block_filter(samples.try_into().unwrap(), loop_filter, prev1, prev2)
                }
            }
        } else {
            find_best_block(samples.try_into().unwrap(), prev1, prev2)
        };

        prev1 = block.decoded_samples[SAMPLES_PER_BLOCK - 1];
        prev2 = block.decoded_samples[SAMPLES_PER_BLOCK - 2];

        brr_data.extend(encode_block(block, i == last_block_index, loop_flag));
    }

    if let Some(lo) = loop_offset {
        assert!(usize::from(lo) < brr_data.len());
    }

    Ok(BrrSample {
        loop_offset,
        brr_data,
    })
}
