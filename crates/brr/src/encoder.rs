//! BRR encoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::decoder::{filter0, filter1, filter2, filter3, I15Sample};
use crate::{
    BlockNumber, BrrFilter, BrrSample, SampleNumber, BRR_HEADER_END_FLAG, BRR_HEADER_LOOP_FLAG,
    BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK,
};

const MAX_SAMPLES: usize = u16::MAX as usize;

const MAX_SHIFT: u8 = 12;

const I4_MIN: i32 = -8;
const I4_MAX: i32 = 7;

#[derive(Debug, Clone)]
pub enum EncodeError {
    NoSamples,
    InvalidNumberOfSamples(usize),
    TooManySamples(usize),
    InvalidLoopPointSamples(SampleNumber),
    LoopPointTooLarge(SampleNumber, usize),
    DupeBlockHackNotAllowedWithLoopPoint,
    DupeBlockHackNotAllowedWithLoopResetsFilter,
    DupeBlockHackTooLarge,
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncodeError::NoSamples => write!(f, "no samples"),
            EncodeError::InvalidNumberOfSamples(s) => write!(
                f,
                "number of samples ({s}) is not a multiple of {SAMPLES_PER_BLOCK}"
            ),
            EncodeError::TooManySamples(s) => {
                write!(f, "too many samples ({s} samples, max {MAX_SAMPLES}))")
            }
            EncodeError::InvalidLoopPointSamples(lp) => {
                write!(
                    f,
                    "loop_point ({}) is not a multiple of {SAMPLES_PER_BLOCK}",
                    lp.0
                )
            }
            EncodeError::LoopPointTooLarge(lp, s_len) => write!(
                f,
                "loop_point is too large ({}, max {})",
                lp.0,
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

trait Scorer {
    // BRR encoder will select the lowest scoring sample
    fn score_sample(source: I15Sample, decoded: I15Sample, prev: (I15Sample, I15Sample)) -> i32;

    // BRR encoder will select the lowest scoring block
    fn score_block(
        samples: &[I15Sample; SAMPLES_PER_BLOCK],
        block: &BrrBlock,
        prev: (I15Sample, I15Sample),
    ) -> i64;
}

struct SquaredError {}

impl SquaredError {
    fn sample_squared_error(s1: I15Sample, s2: I15Sample) -> i32 {
        let delta = s1.value() - s2.value();
        delta * delta
    }
}

impl Scorer for SquaredError {
    fn score_sample(source: I15Sample, decoded: I15Sample, _prev: (I15Sample, I15Sample)) -> i32 {
        Self::sample_squared_error(source, decoded)
    }

    fn score_block(
        samples: &[I15Sample; SAMPLES_PER_BLOCK],
        block: &BrrBlock,
        _prev: (I15Sample, I15Sample),
    ) -> i64 {
        assert!(block.decoded_samples.len() == samples.len());

        let mut square_error = 0;

        for (b, s) in block.decoded_samples.iter().zip(samples) {
            square_error += i64::from(Self::sample_squared_error(*b, *s));
        }

        square_error
    }
}

struct SquaredErrorAvoidGaussianOverflow {}

impl Scorer for SquaredErrorAvoidGaussianOverflow {
    fn score_sample(source: I15Sample, decoded: I15Sample, prev: (I15Sample, I15Sample)) -> i32 {
        const MAX_NEGATIVE_PENALTY_1: i32 = 100_000_000;
        const MAX_NEGATIVE_PENALTY_2: i32 = 1_000_000_000;

        const _MAX_DELTA: i32 = (i16::MAX as i32 >> 1) - (i16::MIN as i32 >> 1);
        const _: () = assert!(MAX_NEGATIVE_PENALTY_1 + (_MAX_DELTA * _MAX_DELTA) < i32::MAX);
        const _: () = assert!(MAX_NEGATIVE_PENALTY_2 + (_MAX_DELTA * _MAX_DELTA) < i32::MAX);

        let penalty = match (decoded, prev.0, prev.1) {
            (I15Sample::MIN, I15Sample::MIN, I15Sample::MIN) => return i32::MAX,
            (I15Sample::MIN, I15Sample::MIN, _) => MAX_NEGATIVE_PENALTY_2,
            (I15Sample::MIN, _, _) => MAX_NEGATIVE_PENALTY_1,
            (_, _, _) => 0,
        };

        let delta = source.value() - decoded.value();
        delta * delta + penalty
    }

    fn score_block(
        samples: &[I15Sample; SAMPLES_PER_BLOCK],
        block: &BrrBlock,
        prev: (I15Sample, I15Sample),
    ) -> i64 {
        assert!(block.decoded_samples.len() == samples.len());

        let mut error = 0;

        let mut prev = prev;

        for (s, b) in samples.iter().zip(block.decoded_samples) {
            error += i64::from(Self::score_sample(*s, b, prev));

            prev.1 = prev.0;
            prev.0 = b;
        }

        error
    }
}

struct BrrBlock {
    filter: BrrFilter,
    shift: u8,
    // signed 4-bit values
    nibbles: [i8; 16],
    decoded_samples: [I15Sample; SAMPLES_PER_BLOCK],
}

fn build_block<S: Scorer>(
    samples: &[I15Sample; SAMPLES_PER_BLOCK],
    shift: u8,
    filter: BrrFilter,
    filter_fn: fn(I15Sample, I15Sample) -> i32,
    prev1: I15Sample,
    prev2: I15Sample,
) -> BrrBlock {
    assert!(shift <= MAX_SHIFT);

    let mut nibbles = [0; SAMPLES_PER_BLOCK];
    let mut decoded_samples: [I15Sample; SAMPLES_PER_BLOCK] = Default::default();

    let div: i32 = 1 << shift;

    let mut prev1 = prev1;
    let mut prev2 = prev2;

    for (i, s) in samples.iter().enumerate() {
        let offset = filter_fn(prev1, prev2);

        // Using division instead of `>> shift` to round towards 0 when s is negative
        let n = (((s.value() - offset) << 1) / div).clamp(I4_MIN, I4_MAX);

        // `n` might not be the best value for `s`.
        // Use squared-error to determine if `n`, `n-1` or `n+1` is the better value to use
        let (n, d) = (n - 1..=n + 1)
            .filter(|n| (I4_MIN..=I4_MAX).contains(n))
            .map(|n| {
                // Decode nibble (no shift out-of-range test required)
                let d = I15Sample::clamp_and_clip(((n << shift) >> 1) + offset);
                (n, d)
            })
            .min_by_key(|(_, d)| S::score_sample(*s, *d, (prev1, prev2)))
            .unwrap();

        prev2 = prev1;
        prev1 = d;

        nibbles[i] = n.try_into().unwrap();
        decoded_samples[i] = d;
    }

    BrrBlock {
        filter,
        shift,
        nibbles,
        decoded_samples,
    }
}

fn find_best_block<S: Scorer>(
    samples: &[I15Sample; SAMPLES_PER_BLOCK],
    prev1: I15Sample,
    prev2: I15Sample,
) -> BrrBlock {
    let mut best_block = None;
    let mut best_block_score = i64::MAX;

    let mut test_filter = |filter, filter_fn| {
        for shift in 0..=MAX_SHIFT {
            let block = build_block::<S>(samples, shift, filter, filter_fn, prev1, prev2);

            let score = S::score_block(samples, &block, (prev1, prev2));
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

fn find_best_block_filter<S: Scorer>(
    samples: &[I15Sample; SAMPLES_PER_BLOCK],
    filter: BrrFilter,
    prev1: I15Sample,
    prev2: I15Sample,
) -> BrrBlock {
    let test_filter = |filter, filter_fn| {
        (0..=MAX_SHIFT)
            .map(|shift| build_block::<S>(samples, shift, filter, filter_fn, prev1, prev2))
            .min_by_key(|block| S::score_block(samples, block, (prev1, prev2)))
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

fn encode_brr_with_scorer<S: Scorer>(
    samples: &[i16],
    loop_offset: Option<SampleNumber>,
    dupe_block_hack: Option<BlockNumber>,
    loop_filter: Option<BrrFilter>,
) -> Result<BrrSample, EncodeError> {
    if samples.is_empty() {
        return Err(EncodeError::NoSamples);
    }

    if samples.len() % SAMPLES_PER_BLOCK != 0 {
        return Err(EncodeError::InvalidNumberOfSamples(samples.len()));
    }

    if samples.len() > MAX_SAMPLES {
        return Err(EncodeError::TooManySamples(samples.len()));
    }

    let (loop_flag, loop_block, loop_offset) = match (loop_offset, dupe_block_hack) {
        (None, None) => (false, BlockNumber(usize::MAX), None),
        (Some(lp), None) => {
            if lp.0 % SAMPLES_PER_BLOCK != 0 {
                return Err(EncodeError::InvalidLoopPointSamples(lp));
            }
            if lp.0 >= samples.len() {
                return Err(EncodeError::LoopPointTooLarge(lp, samples.len()));
            }

            let loop_block = BlockNumber(lp.0 / SAMPLES_PER_BLOCK);

            // safe, `samples.len() is <= u16::MAX`
            let loop_offset = u16::try_from(loop_block.0 * BYTES_PER_BRR_BLOCK).unwrap();

            (true, loop_block, Some(loop_offset))
        }
        (None, Some(dbh)) => {
            if dbh.0 > 64 {
                return Err(EncodeError::DupeBlockHackTooLarge);
            }

            if loop_filter == Some(BrrFilter::Filter0) {
                return Err(EncodeError::DupeBlockHackNotAllowedWithLoopResetsFilter);
            }

            let loop_block = dbh;
            let loop_offset = u16::try_from(dbh.0 * BYTES_PER_BRR_BLOCK).unwrap();

            (true, loop_block, Some(loop_offset))
        }
        (Some(_), Some(_)) => {
            return Err(EncodeError::DupeBlockHackNotAllowedWithLoopPoint);
        }
    };

    let n_blocks = samples.len() / SAMPLES_PER_BLOCK + dupe_block_hack.map(|bn| bn.0).unwrap_or(0);
    let last_block_index = BlockNumber(n_blocks - 1);

    let mut brr_data = Vec::with_capacity(n_blocks * BYTES_PER_BRR_BLOCK);

    let mut prev1 = I15Sample::default();
    let mut prev2 = I15Sample::default();

    for (bn, samples) in samples
        .chunks_exact(SAMPLES_PER_BLOCK)
        .cycle()
        .take(n_blocks)
        .enumerate()
        .map(|(i, s)| (BlockNumber(i), s))
    {
        let samples: [i16; SAMPLES_PER_BLOCK] = samples.try_into().unwrap();
        let samples = samples.map(I15Sample::from_sample);

        let block = if bn == BlockNumber(0) {
            // The first block always uses filter 0
            find_best_block_filter::<S>(&samples, BrrFilter::Filter0, prev1, prev2)
        } else if bn == loop_block {
            match loop_filter {
                None => find_best_block::<S>(&samples, prev1, prev2),
                Some(loop_filter) => {
                    find_best_block_filter::<S>(&samples, loop_filter, prev1, prev2)
                }
            }
        } else {
            find_best_block::<S>(&samples, prev1, prev2)
        };

        prev1 = block.decoded_samples[SAMPLES_PER_BLOCK - 1];
        prev2 = block.decoded_samples[SAMPLES_PER_BLOCK - 2];

        brr_data.extend(encode_block(block, bn == last_block_index, loop_flag));
    }

    if let Some(lo) = loop_offset {
        assert!(usize::from(lo) < brr_data.len());
    }

    Ok(BrrSample {
        loop_offset,
        brr_data,
    })
}

pub enum Evaluator {
    SquaredError,
    SquaredErrorAvoidGaussianOverflow,
}

pub const DEFAULT_EVALUATOR: Evaluator = Evaluator::SquaredErrorAvoidGaussianOverflow;

pub fn encode_brr(
    samples: &[i16],
    evaluator: Evaluator,
    loop_offset: Option<SampleNumber>,
    dupe_block_hack: Option<BlockNumber>,
    loop_filter: Option<BrrFilter>,
) -> Result<BrrSample, EncodeError> {
    match evaluator {
        Evaluator::SquaredError => encode_brr_with_scorer::<SquaredError>(
            samples,
            loop_offset,
            dupe_block_hack,
            loop_filter,
        ),
        Evaluator::SquaredErrorAvoidGaussianOverflow => {
            encode_brr_with_scorer::<SquaredErrorAvoidGaussianOverflow>(
                samples,
                loop_offset,
                dupe_block_hack,
                loop_filter,
            )
        }
    }
}

#[cfg(test)]
#[allow(clippy::bool_assert_comparison)]
mod test_avoid_gaussian_overflow {
    use super::*;

    fn _test(input: &[i16]) {
        let no_avoid = encode_brr(input, Evaluator::SquaredError, None, None, None).unwrap();

        let with_avoid = encode_brr(
            input,
            Evaluator::SquaredErrorAvoidGaussianOverflow,
            None,
            None,
            None,
        )
        .unwrap();

        // Confirm input would glitch
        assert_eq!(no_avoid.test_for_gaussian_overflow_glitch_autoloop(), true);

        assert_eq!(
            with_avoid.test_for_gaussian_overflow_glitch_autoloop(),
            false
        );
    }

    #[test]
    fn square_wave() {
        // Created using audacity's tone generator (Square, no alias, 0.95 amplitude)
        _test(&[
            0, 25623, 31830, 31316, 31479, 31409, 31444, 31423, 31439, 31422, 31446, 31408, 31479,
            31316, 31831, 25621, 2, -25624, -31830, -31314, -31483, -31405, -31447, -31420, -31443,
            -31418, -31450, -31403, -31484, -31312, -31834, -25619,
        ]);
    }

    #[test]
    fn one_block() {
        const N: i16 = i16::MIN;

        _test(&[0, 0, 0, 0, 0, 0, 0, 0, N, N, N, N, N, N, N, N]);
    }

    #[test]
    fn glitch_with_prev1() {
        const N: i16 = i16::MIN;

        const INPUT: [i16; SAMPLES_PER_BLOCK * 2] = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, N, N, N, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ];

        assert_eq!(INPUT[SAMPLES_PER_BLOCK - 2], 0);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK - 1], N);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK], N);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK + 1], N);

        _test(&INPUT);
    }

    #[test]
    fn glitch_with_prev1_and_prev2() {
        const N: i16 = i16::MIN;

        const INPUT: [i16; SAMPLES_PER_BLOCK * 2] = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, N, N, N, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ];

        assert_eq!(INPUT[SAMPLES_PER_BLOCK - 3], 0);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK - 2], N);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK - 1], N);
        assert_eq!(INPUT[SAMPLES_PER_BLOCK], N);

        _test(&INPUT);
    }
}

#[cfg(test)]
mod test_decoded_samples {
    use crate::decoder::decode_brr_block;

    use super::*;

    /// Encodes a block of samples using all 4 filters and tests if `BrrBlock::decoded_samples` matches `decode_brr_block()`
    fn _test(p2: i16, p1: i16, input: [i16; 16]) {
        const ALL_FILTERS: [BrrFilter; 4] = [
            BrrFilter::Filter0,
            BrrFilter::Filter1,
            BrrFilter::Filter2,
            BrrFilter::Filter3,
        ];

        let i15_input = input.map(I15Sample::from_sample);
        let i15_p1 = I15Sample::from_sample(p1);
        let i15_p2 = I15Sample::from_sample(p2);

        for filter in ALL_FILTERS {
            let best_block =
                find_best_block_filter::<SquaredError>(&i15_input, filter, i15_p1, i15_p2);
            let brr_block_samples = best_block.decoded_samples.map(I15Sample::to_sample);

            let brr_block = encode_block(best_block, false, false);
            let decoded_samples = decode_brr_block(&brr_block, p1, p2);

            assert_eq!(brr_block_samples, decoded_samples, "ERROR: {:?}", input);
        }
    }

    #[test]
    fn linear() {
        // (i - 6) / 10 * 0.8 * i16::MAX
        #[rustfmt::skip]
        _test(
            -20970,
            -18349,
            [-15728, -13106, -10485, -7864, -5242, -2621, 0, 2621, 5242, 7864, 10485, 13106, 15728, 18349, 20970, 23592]
        );
    }

    #[test]
    fn sine() {
        // sin(tau * i / 16) * 0.95 * i16::MAX
        #[rustfmt::skip]
        _test(
            -22011,
            -11912,
            [0, 11912, 22011, 28759, 31128, 28759, 22011, 11912, 0, -11912, -22011, -28759, -31128, -28759, -22011, -11912],
        );
    }

    /// Tests a sample that glitches if there is no 15-bit wrapping
    #[test]
    fn wrapping_test() {
        #[rustfmt::skip]
        _test(
            -820,
            -800,
            [
                -450, -450,  800,  6000,  30000,  32000,  400,  200,
                 400,  450, -800, -6000, -30000, -32000, -400, -200,
            ],
        );
    }
}
