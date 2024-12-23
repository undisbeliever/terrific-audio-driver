// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn test_early_release() {
    assert_line_matches_bytecode("q0", &["disable_early_release"]);
    assert_line_matches_bytecode("q10", &["set_early_release 10 1"]);
    assert_line_matches_bytecode("q16,D15", &["set_early_release 16 1 D15"]);
}

#[test]
fn test_early_release_deduplication() {
    assert_line_matches_line("q0 q0 q0", "q0");

    assert_line_matches_line(
        "q0 q20 q20 q21 q15,I10 q15,I20 q15,I20 q0",
        "q0 q20 q21 q15,I10 q15,I20 q0",
    );

    assert_line_matches_bytecode(
        "q3,D5 [a]5 q3,D8",
        &[
            "set_early_release 3 1 D5",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            "set_early_release 3 1 D8",
        ],
    );

    assert_line_matches_bytecode(
        "q1 [a]5 q1",
        &[
            "set_early_release 1 1",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            // q1 deduplicated
        ],
    );

    assert_line_matches_bytecode(
        "q1 [q1 a : q10 b]5 q2",
        &[
            "set_early_release 1 1",
            "start_loop",
            "set_early_release 1 1",
            "play_note a4 24",
            "skip_last_loop",
            "set_early_release 10 1",
            "play_note b4 24",
            "end_loop 5",
            "set_early_release 2 1",
        ],
    );

    assert_line_matches_bytecode(
        "q1 [q1 a : q10 b]5 q1",
        &[
            "set_early_release 1 1",
            "start_loop",
            "set_early_release 1 1",
            "play_note a4 24",
            "skip_last_loop",
            "set_early_release 10 1",
            "play_note b4 24",
            "end_loop 5",
            // q1 deduplicated
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s q8,E20 a

A @1 !s q10,E15 a
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "set_early_release 10 1 E15",
            "play_note a4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s q8,E20 a

A @1 !s q8,E20 a
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            // q8,E20 deduplicated
            "play_note a4 24",
        ],
    );
}

#[test]
fn test_early_release_with_blank_min_ticks() {
    assert_line_matches_bytecode("q10,,D15", &["set_early_release 10 1 D15"]);
}

#[test]
fn test_early_release_with_min_ticks() {
    assert_line_matches_bytecode("q10,6", &["set_early_release 10 6"]);
    assert_line_matches_bytecode("q16,6,D15", &["set_early_release 16 6 D15"]);
}

#[test]
fn test_early_release_with_min_ticks_deduplication() {
    assert_line_matches_line("q10,6 q10,6 q10,6", "q10,6");
    assert_line_matches_line("q20,8,D15 q20,8,D15 q20,8,D15", "q20,8,D15");

    assert_line_matches_line("q8,E24 q8,1,E24", "q8,E24");
    assert_line_matches_line("q8,1,E24 q8,E24", "q8,1,E24");

    // No need to test everything.  1, 2 & 3 argument
    // This test assumes test_early_release_deduplication() passes.
}

#[test]
fn test_early_release_raw_gain_is_err() {
    assert_error_in_mml_line("q10,,15", 5, ValueError::NoOptionalGainMode.into());
    assert_error_in_mml_line("q10,2,15", 6, ValueError::NoOptionalGainMode.into());
}

#[test]
fn test_early_release_comma_f0_gain_is_err() {
    assert_error_in_mml_line("q10,F0", 5, ValueError::OptionalGainCannotBeZero.into());
    assert_error_in_mml_line("q10,2,F0", 7, ValueError::OptionalGainCannotBeZero.into());
}
