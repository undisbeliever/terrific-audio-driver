//! BRR encoder and decoder warning tests

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::decoder::I15Sample;
use crate::SAMPLES_PER_BLOCK;

// CAUTION: Assumes samples decoded by I15Sample.
pub(crate) fn test_for_gaussian_overflow_glitch(
    prev1: i16,
    prev2: i16,
    decoded_samples: &[i16; SAMPLES_PER_BLOCK],
) -> bool {
    const MIN: i16 = I15Sample::MIN.to_sample();

    let mut c: u32 = match (prev2, prev1) {
        (MIN, MIN) => 2,
        (_, MIN) => 1,
        (_, _) => 0,
    };

    for s in decoded_samples {
        if *s == MIN {
            c += 1;
            if c == 3 {
                return true;
            }
        } else {
            c = 0;
        }
    }

    false
}

#[cfg(test)]
#[allow(clippy::bool_assert_comparison)]
mod guassian_overflow_detector_tests {
    use super::*;

    const MIN: i16 = I15Sample::MIN.to_sample();
    const MAX: i16 = i16::MAX;

    #[test]
    fn test_for_gaussian_overflow_bug_false() {
        // Almost max negative
        let amn = I15Sample::clamp_and_clip(-16383);
        assert_eq!(amn.value(), I15Sample::MIN.value() + 1);
        let amn = amn.to_sample();

        assert_eq!(test_for_gaussian_overflow_glitch(0, 0, &[0; 16]), false);
        assert_eq!(
            test_for_gaussian_overflow_glitch(MAX, MAX, &[MAX; 16]),
            false
        );

        assert_eq!(
            test_for_gaussian_overflow_glitch(
                amn,
                MIN,
                &[MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn, MIN]
            ),
            false
        );

        assert_eq!(
            test_for_gaussian_overflow_glitch(
                MIN,
                MIN,
                &[amn, MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn, MIN, MIN, amn]
            ),
            false
        );
    }

    #[test]
    fn test_for_gaussian_overflow_bug_true() {
        assert_eq!(test_for_gaussian_overflow_glitch(0, 0, &[MIN; 16]), true);

        assert_eq!(
            test_for_gaussian_overflow_glitch(
                0,
                0,
                &[MIN, MIN, MIN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ),
            true
        );

        assert_eq!(
            test_for_gaussian_overflow_glitch(
                0,
                0,
                &[0, 0, 0, 0, 0, 0, MIN, MIN, MIN, 0, 0, 0, 0, 0, 0, 0]
            ),
            true
        );

        assert_eq!(
            test_for_gaussian_overflow_glitch(
                0,
                0,
                &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, MIN, MIN, MIN]
            ),
            true
        );
    }

    #[test]
    fn test_for_gaussian_overflow_bug_true_prev1_prev2() {
        assert_eq!(
            test_for_gaussian_overflow_glitch(
                MIN,
                MIN,
                &[MIN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ),
            true
        );
    }

    #[test]
    fn test_for_gaussian_overflow_bug_true_prev1() {
        assert_eq!(
            test_for_gaussian_overflow_glitch(
                MIN,
                0,
                &[MIN, MIN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ),
            true
        );
    }
}
