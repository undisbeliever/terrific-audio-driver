// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn set_vibrato() {
    assert_line_matches_bytecode(
        "~23,4 a ~0",
        &["set_vibrato 23 4", "play_note a4 24", "disable_vibrato"],
    );
}

#[test]
fn mp_vibrato() {
    assert_line_matches_bytecode(
        "MP2,4 a b MP0 c",
        &[
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "set_vibrato_depth_and_play_note 0 c4 24",
        ],
    );
}

#[test]
fn mp_vibrato_after_start_of_loop() {
    assert_line_matches_bytecode(
        "MP40,2 a [a a b MP30,5 c]4",
        &[
            "set_vibrato 42 2",
            "play_note a4 24",
            "start_loop",
            // Vibrato state is unknown
            "set_vibrato 42 2",
            "play_note a4 24",
            // Vibrato unchanged
            "play_note a4 24",
            "set_vibrato_depth_and_play_note 47 b4 24",
            "set_vibrato 7 5",
            "play_note c4 24",
            "end_loop 4",
        ],
    );
}

/// Test that the vibrato state is correctly tracked after a *skip last loop* command.
#[test]
fn mp0_after_skip_last_loop() {
    assert_line_matches_bytecode(
        "[MP2,4 a b : MP0 c]4 d",
        &[
            "start_loop",
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "skip_last_loop",
            "set_vibrato_depth_and_play_note 0 c4 24",
            "end_loop 4",
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );

    assert_line_matches_bytecode(
        "[MP2,4 a b : [ MP0 c : d ]2 ]3 d",
        &[
            "start_loop",
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_vibrato_depth_and_play_note 0 c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );
}

#[test]
fn subroutine_vibrato_bugfix() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s ~250,4
A @1 c !s d e
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            "play_note d4 24",
            "play_note e4 24",
        ],
    );
}

#[test]
fn vibrato_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 c ~50,4 !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "set_vibrato 50 4",
            "call_subroutine s",
            "play_note d4 24",
        ],
    );
}

#[test]
fn vibrato_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 c !s ~50,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            // set vibrato instructions are not deduplicated
            "set_vibrato 50 4",
            "play_note d4 24",
        ],
    );
}

#[test]
fn mp_vibrato_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1
A @1 MP20,4 c !s c
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 6 c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d
A @1 MP20,4 c !s d d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
            // vibrato active and depth unchanged
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,6 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,6 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );
}

#[test]
fn mp_vibrato_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1
A @1 c !s MP20,4 c
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato unchanged
            "set_vibrato 6 4",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato unchanged
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d
A @1 c !s MP20,4 d d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
            // vibrato active and depth unchanged
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,6 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,6 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );
}

#[test]
fn mp0_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 MP0 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            // subroutine vibrato overrides MP0
            "play_note d4 24",
        ],
    );
}

#[test]
fn mp0_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 c !s MP0 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );
}

#[test]
fn mp0_at_end_of_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d MP0
A @1 ~25,5 c !s MP0 d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 25 5",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d MP0
A @1 ~25,10 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 25 10",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );
}
