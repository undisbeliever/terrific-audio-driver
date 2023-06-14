//! BRR encoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    BrrSample, BRR_HEADER_END_FLAG, BRR_HEADER_LOOP_FLAG, BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK,
};

const MAX_SHIFT: u8 = 12;

const I4_MIN: i32 = -8;
const I4_MAX: i32 = 7;

#[derive(Debug)]
pub enum EncodeError {
    NoSamples,
    InvalidNumberOfSamples,
    TooManySamples,
    InvalidLoopPoint,
    LoopPointTooLarge(usize, usize),
    DupeBlockHankNotAllowedWithLoopPoint,
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
            EncodeError::DupeBlockHankNotAllowedWithLoopPoint => {
                write!(f, "dupe_block_hack not allowed when loop_point is set")
            }
            EncodeError::DupeBlockHackTooLarge => write!(f, "dupe_block_hack value is too large"),
        }
    }
}

struct BrrBlock {
    filter: u8,
    shift: u8,
    // signed 4-bit values
    nibbles: [i8; 16],
    decoded_samples: [i16; SAMPLES_PER_BLOCK],
}

fn build_block(
    samples: &[i16; SAMPLES_PER_BLOCK],
    shift: u8,
    filter_int: u8,
    filter: fn(i32, i32) -> i32,
    prev1: i16,
    prev2: i16,
) -> BrrBlock {
    assert!(shift <= MAX_SHIFT);
    assert!(filter_int <= 3);

    let mut nibbles = [0; SAMPLES_PER_BLOCK];
    let mut decoded_samples = [0; SAMPLES_PER_BLOCK];

    let div: i32 = 1 << shift;

    let mut p1 = prev1 as i32;
    let mut p2 = prev2 as i32;

    for (i, s) in samples.iter().enumerate() {
        let s: i32 = (*s).into();

        let offset = filter(p1, p2);

        let n = ((s - offset) / div).clamp(I4_MIN, I4_MAX);
        let s = n * div + offset;

        // clamp to an i16
        let s: i16 = s
            .clamp(i16::MIN.into(), i16::MAX.into())
            .try_into()
            .unwrap();

        p2 = p1;
        p1 = s as i32;

        nibbles[i] = n.try_into().unwrap();
        decoded_samples[i] = s;
    }

    BrrBlock {
        filter: filter_int,
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

fn find_best_block(
    samples: &[i16; SAMPLES_PER_BLOCK],
    use_filters: bool,
    prev1: i16,
    prev2: i16,
) -> BrrBlock {
    let mut best_block = None;
    let mut best_block_score = i64::MAX;

    let mut test_filter = |filter_int, filter| {
        for shift in 0..=MAX_SHIFT {
            let block = build_block(samples, shift, filter_int, filter, prev1, prev2);

            let score = calc_squared_error(&block, samples);
            if score < best_block_score {
                best_block = Some(block);
                best_block_score = score;
            }
        }
    };

    test_filter(0, filter0);

    if use_filters {
        test_filter(1, filter1);
        test_filter(2, filter2);
        test_filter(3, filter3);
    }

    best_block.unwrap()
}

// Loop flag only set if end_flag is set.
fn encode_block(block: BrrBlock, end_flag: bool, loop_flag: bool) -> [u8; BYTES_PER_BRR_BLOCK] {
    assert!(block.shift <= MAX_SHIFT);

    let mut out = [0; BYTES_PER_BRR_BLOCK];

    // Header
    let mut header = ((block.shift & 0xf) << 4) | ((block.filter & 0x3) << 2);
    if end_flag {
        header |= BRR_HEADER_END_FLAG;

        if loop_flag {
            header |= BRR_HEADER_LOOP_FLAG;
        }
    }

    out[0] = header;

    for i in 1..BYTES_PER_BRR_BLOCK {
        let s = (i - 1) * 2;

        let nibble0 = block.nibbles[s + 0].to_le_bytes()[0];
        let nibble1 = block.nibbles[s + 1].to_le_bytes()[0];

        out[i] = ((nibble0 & 0xf) << 4) | (nibble1 & 0xf);
    }

    out
}

pub fn encode_brr(
    samples: &[i16],
    loop_point_samples: Option<usize>,
    dupe_block_hack: Option<usize>,
) -> Result<BrrSample, EncodeError> {
    if samples.len() == 0 {
        return Err(EncodeError::NoSamples);
    }

    if samples.len() % SAMPLES_PER_BLOCK != 0 {
        return Err(EncodeError::InvalidNumberOfSamples);
    }

    if samples.len() > u16::MAX.into() {
        return Err(EncodeError::TooManySamples);
    }

    let (loop_flag, loop_offset) = match (loop_point_samples, dupe_block_hack) {
        (None, None) => (false, None),
        (Some(lp), None) => {
            if lp % SAMPLES_PER_BLOCK != 0 {
                return Err(EncodeError::InvalidLoopPoint);
            }
            if lp >= samples.len() {
                return Err(EncodeError::LoopPointTooLarge(lp, samples.len()));
            }

            // safe, `samples.len() is <= u16::MAX`
            let loop_offset = u16::try_from(lp / SAMPLES_PER_BLOCK * BYTES_PER_BRR_BLOCK).unwrap();

            (true, Some(loop_offset))
        }
        (None, Some(dbh)) => {
            if dbh > 64 {
                return Err(EncodeError::DupeBlockHackTooLarge);
            }

            let loop_offset = u16::try_from(dbh * BYTES_PER_BRR_BLOCK).unwrap();

            (true, Some(loop_offset))
        }
        (Some(_), Some(_)) => {
            return Err(EncodeError::DupeBlockHankNotAllowedWithLoopPoint);
        }
    };

    let n_blocks = samples.len() / SAMPLES_PER_BLOCK + dupe_block_hack.unwrap_or(0);

    let mut brr_data = Vec::new();
    brr_data.reserve(n_blocks * BYTES_PER_BRR_BLOCK);

    let mut prev1 = 0;
    let mut prev2 = 0;

    let last_block_index = n_blocks - 1;

    for (i, samples) in samples
        .chunks_exact(SAMPLES_PER_BLOCK)
        .cycle()
        .take(n_blocks)
        .enumerate()
    {
        let block = if i == 0 {
            // The first block always uses filter 0
            find_best_block(samples.try_into().unwrap(), false, 0, 0)
        } else {
            find_best_block(samples.try_into().unwrap(), true, prev1, prev2)
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
