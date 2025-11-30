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
fn set_vibrato_with_delay() {
    assert_line_matches_bytecode(
        "~12,5,20 a ~0",
        &[
            "set_vibrato_with_delay 12 5 20",
            "play_note a4 24",
            "disable_vibrato",
        ],
    );

    // Also test bytecode will correctly optimise a `set_vibrato_with_delay a b 0` to `set_vibrato a b`.
    assert_line_matches_line_and_bytecode("~13,6,0", "~13,6", &["set_vibrato_with_delay 13 6 0"]);
    assert_line_matches_line_and_bytecode("~14,7,0", "~14,7", &["set_vibrato 14 7"]);

    assert_line_matches_bytecode("~1,1,255", &["set_vibrato_with_delay 1 1 255"]);

    assert_one_error_in_mml_line(
        "~1,1,256",
        6,
        ValueError::VibratoDelayTicksOutOfRange(256).into(),
    );

    assert_one_error_in_mml_line("~1,1,", 5, ValueError::NoVibratoDelayTicks.into());
}

#[test]
fn mp_vibrato_with_delay() {
    assert_line_matches_bytecode(
        "MP2,4,8 a b MP0 c",
        &[
            "set_vibrato_with_delay 1 4 8",
            "play_note a4 24",
            "play_note b4 24",
            "set_vibrato_depth_and_play_note 0 c4 24",
        ],
    );

    assert_line_matches_bytecode(
        "MP20,4,8 a b MP0 c MP20,4,8 d",
        &[
            "set_vibrato_with_delay 10 4 8",
            "play_note a4 24",
            "set_vibrato_depth_and_play_note 12 b4 24",
            "set_vibrato_depth_and_play_note 0  c4 24",
            "set_vibrato_depth_and_play_note 7  d4 24",
        ],
    );

    assert_line_matches_bytecode(
        "MP2,4,8 a b MP2,4,9 c d MP0 c MP20,4,9 d",
        &[
            "set_vibrato_with_delay 1 4 8",
            "play_note a4 24",
            "play_note b4 24",
            "set_vibrato_with_delay 1 4 9",
            "play_note c4 24",
            "play_note d4 24",
            "set_vibrato_depth_and_play_note 0 c4 24",
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_line_matches_bytecode(
        "MP2,4,255 a",
        &["set_vibrato_with_delay 1 4 255", "play_note a4 24"],
    );

    assert_one_error_in_mml_line(
        "MP2,4,256 a",
        7,
        ValueError::VibratoDelayTicksOutOfRange(256).into(),
    );

    assert_one_error_in_mml_line("MP2,4,", 6, ValueError::NoVibratoDelayTicks.into());
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

#[test]
fn l_in_vibrato() {
    assert_line_matches_line("~20,l2", "~20,48");
    assert_line_matches_line("~30,l3", "~30,32");
    assert_line_matches_line("~40,l3,l8", "~40,32,12");

    assert_line_matches_line("C192 ~40,l2", "~40,96");
    assert_line_matches_line("C192 ~50,l4", "~50,48");
    assert_line_matches_line("C192 ~50,l8.", "~50,36");

    assert_line_matches_line("C192 ~60,l8,l24", "~60,24,8");
    assert_line_matches_line("C192 ~70,l8,l24..", "~70,24,14");
}

#[test]
fn l_in_mp_vibrato() {
    assert_line_matches_line("MP20,l2 c", "MP20,48 c");
    assert_line_matches_line("MP30,l3 c", "MP30,32 c");
    assert_line_matches_line("MP30,l3,l8 c", "MP30,32,12 c");

    assert_line_matches_line("C192 MP40,l2 c", "MP40,96 c%48");
    assert_line_matches_line("C192 MP50,l4 c", "MP50,48 c%48");
    assert_line_matches_line("C192 MP60,l8. c", "MP60,36 c%48");

    assert_line_matches_line("C192 MP60,l12,l24 c", "MP60,16,8 c%48");
    assert_line_matches_line("C192 MP60,l12,l24.. c", "MP60,16,14 c%48");
}

#[test]
fn instrument_tuning_loop_analysis() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@14 f1000_o4
@13 f1000_o3_o5

A MP20,2 @14 [c @13 d]2 e
"##,
        &[
            "set_instrument f1000_o4",
            "start_loop",
            "set_vibrato 6, 2",
            "play_note c4 24",
            "set_instrument f1000_o3_o5",
            "set_vibrato_depth_and_play_note 7 d4 24",
            "end_loop 2",
            "set_vibrato_depth_and_play_note 8 e4 24",
        ],
    );

    assert_one_error_in_channel_a_mml(
        r##"
@14 f1000_o4
@24 f2000_o4

A MP20,2 @14 [c @24 d]2 e
"##,
        15,
        ChannelError::CannotUseMpWithUnknownInstrumentTuning,
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@14 f1000_o4
@13 f1000_o3_o5
@24 f2000_o4

A MP20,2 @14 [c @24 d : @13 e]2 f
"##,
        &[
            "set_instrument f1000_o4",
            "start_loop",
            "set_vibrato 6, 2",
            "play_note c4 24",
            "set_instrument f2000_o4",
            "set_vibrato_depth_and_play_note 3 d4 24",
            "skip_last_loop",
            "set_instrument f1000_o3_o5",
            "set_vibrato_depth_and_play_note 8 e4 24",
            "end_loop 2",
            "set_vibrato_depth_and_play_note 4 f4 24",
        ],
    );

    assert_one_error_in_channel_a_mml(
        r##"
@14 f1000_o4
@13 f1000_o3_o5
@24 f2000_o4

A MP20,2 @14 [c @13 d : @24 e]2 f
"##,
        15,
        ChannelError::CannotUseMpWithUnknownInstrumentTuning,
    );

    // Nested loop instrument tuning analysis is tested in portamento module
}

#[test]
fn mp_vibrato_prev_slurred_note_assert_panic_bugfix() {
    // Found with cargo-fuzz
    assert_line_matches_bytecode(
        "e & ~1,7 MP0 a",
        &[
            "play_note e4 no_keyoff 24",
            "set_vibrato 1 7",
            "set_vibrato_depth_and_play_note 0 a4 24",
        ],
    );

    assert_line_matches_bytecode(
        "MP20,3 c d MP 40,3 e f & MP0 g",
        &[
            "set_vibrato 8 3",
            "play_note c4 keyoff 24",
            "set_vibrato_depth_and_play_note 9  d4 24",
            "set_vibrato_depth_and_play_note 21 e4 24",
            "set_vibrato_depth_and_play_note 22 f4 no_keyoff 24",
            "set_vibrato_depth_and_play_note 0  g4 24",
        ],
    );
}
