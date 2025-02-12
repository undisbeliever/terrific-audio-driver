// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn set_instrument() {
    let mml = r##"
@0 dummy_instrument
@1 inst_with_adsr
@2 inst_with_gain

A @0 @1 @2
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "set_instrument dummy_instrument",
            "set_instrument inst_with_adsr",
            "set_instrument inst_with_gain",
        ],
    );
}

/// Test instruments with the same InstrumentId do not emit a set_instrument instruction
#[test]
fn deduplicate_set_instrument_instrument_ids() {
    let mml = r##"
@0 dummy_instrument
@1 dummy_instrument
@2 dummy_instrument
@o inst_with_adsr

A @0 @0 @0
A @1 @2
A @o @0 @1 @2
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "set_instrument dummy_instrument",
            "set_instrument inst_with_adsr",
            "set_instrument dummy_instrument",
        ],
    );
}

#[test]
fn set_instrument_and_envelope() {
    let mml = format!(
        r##"
@0 dummy_instrument
@1 dummy_instrument adsr 1 2 3 4
@a inst_with_adsr
@aa inst_with_adsr adsr {EXAMPLE_ADSR_STR}
@ab inst_with_adsr adsr 1 2 3 4
@ag inst_with_adsr gain 24

@g1 inst_with_gain
@g2 inst_with_gain gain {EXAMPLE_GAIN_STR}
@g3 inst_with_gain adsr 3 4 5 6

A @0 @1
A @a @aa @ab @ag
A @0 @ab @a
A @0 @ag @ab
A @0 @g1 @g2 @g3 @g1
"##
    );

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            // Line 1
            "set_instrument dummy_instrument",
            "set_adsr 1 2 3 4",
            // Line 2
            "set_instrument inst_with_adsr",
            "set_adsr 1 2 3 4",
            "set_gain 24",
            // Line 3
            "set_instrument dummy_instrument",
            "set_instrument_and_adsr inst_with_adsr 1 2 3 4",
            "set_instrument inst_with_adsr",
            // Line 4
            "set_instrument dummy_instrument",
            "set_instrument_and_gain inst_with_adsr 24",
            "set_adsr 1 2 3 4",
            // Line 5
            "set_instrument dummy_instrument",
            "set_instrument inst_with_gain",
            "set_adsr 3 4 5 6",
            "set_instrument inst_with_gain",
        ],
    );
}

#[test]
fn set_adsr_and_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        &format!(
            r##"
@a inst_with_adsr
@aa inst_with_adsr adsr 1 2 3 4
@ag inst_with_adsr gain 24

@g inst_with_gain
@ga inst_with_gain adsr 5 6 7 8
@gg inst_with_gain gain 48

A @a  A{EXAMPLE_ADSR_COMMENTS_STR} A{EXAMPLE_ADSR_COMMENTS_STR}
A @aa A1,2,3,4
A @ag G24 G100 A1,2,3,4 A1,2,3,4 A5,6,7,8 A5,6,7,8

A @g  G{EXAMPLE_GAIN_STR} G{EXAMPLE_GAIN_STR}
A @ga G10 G10 A5,6,7,8 G20
A @gg G48 G48 A5,6,7,8 G30
"##
        ),
        &[
            // Line 1
            "set_instrument inst_with_adsr",
            // Line 2
            "set_adsr 1 2 3 4",
            // Line 3
            "set_gain 24",
            "set_gain 100",
            "set_adsr 1 2 3 4",
            "set_adsr 5 6 7 8",
            // Line 4
            "set_instrument inst_with_gain",
            // Line 5
            "set_adsr 5 6 7 8",
            "set_gain 10",
            "set_adsr 5 6 7 8",
            "set_gain 20",
            // Line 6
            "set_gain 48",
            "set_adsr 5 6 7 8",
            "set_gain 30",
        ],
    );
}

#[test]
fn set_gain() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@g inst_with_gain
@gf inst_with_gain gain F 1
@gd inst_with_gain gain D 2
@ge inst_with_gain gain E 3
@gi inst_with_gain gain I 4
@gb inst_with_gain gain B 5

A @g   @gf @gd @ge @gi @gb
A G$20 GF6 GD7 GE8 GI9 GB10
"##,
        &[
            // Line 1
            "set_instrument inst_with_gain",
            "set_gain $01",
            "set_gain $82",
            "set_gain $a3",
            "set_gain $c4",
            "set_gain $e5",
            // Line 2
            "set_gain $20",
            "set_gain $06",
            "set_gain $87",
            "set_gain $a8",
            "set_gain $c9",
            "set_gain $ea",
        ],
    );
}

#[test]
fn hexadecimal_in_mml_instrument_envelope() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@a1 dummy_instrument adsr $f $7 $7 $1f
@a2 dummy_instrument adsr $d 5 2 $b
@a3 dummy_instrument adsr 12 4 5 21

@g1 dummy_instrument gain $2b
@g2 dummy_instrument gain $e4
@g3 dummy_instrument gain 42

A @a1 @a2 @a3 @g1 @g2 @g3
"##,
        &[
            "set_instrument_and_adsr dummy_instrument 15 7 7 31",
            "set_adsr $d 5 2 $b",
            "set_adsr $c $4 $5 $15",
            "set_gain 43",
            "set_gain $e4",
            "set_gain $2a",
        ],
    );
}

#[test]
fn set_instrument_after_set_adsr() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 inst_with_adsr

A @1 A1,2,3,4 @1
"##,
        &[
            "set_instrument inst_with_adsr",
            "set_adsr 1 2 3 4",
            "set_instrument inst_with_adsr",
        ],
    );
}

#[test]
fn set_instrument_after_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 inst_with_gain

A @1 GI5 @1
"##,
        &[
            "set_instrument inst_with_gain",
            "set_gain I5",
            "set_instrument inst_with_gain",
        ],
    );
}

/// Test instrument is is correctly tracked after a *skip last loop* command.
/// Assumes `test_set_instrument_merge_instrument_ids()` passes
#[test]
fn set_instrument_deduplication_after_skip_last_loop_1() {
    let mml = r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : @d b ]2 @d
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : [ @d b : c]2 ]3 @d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 2",
            "end_loop 3",
            "set_instrument dummy_instrument",
        ],
    );
}

/// Test instrument is is correctly tracked after a *skip last loop* command.
/// Assumes `test_set_instrument_merge_instrument_ids()` passes
#[test]
fn set_instrument_deduplication_after_skip_last_loop_2() {
    let mml = r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : @d b ]2 @a
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "end_loop 2",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : [ @d b : c ]2 ] 3 @a
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 2",
            "end_loop 3",
        ],
    );
}

/// Test ADSR envelope is correctly tracked across loops
#[test]
fn set_adsr_dedeuplication_after_skip_last_loop() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a A 5,6,7,8 b : A 15,7,7,31 c ]2 A 5,6,7,8 d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "skip_last_loop",
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "end_loop 2",
            // No set_adsr instruction
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a A 5,6,7,8 b : [ A 15,7,7,31 c : d]2 ]3 A 5,6,7,8 e
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            // No set_adsr instruction
            "play_note e4 24",
        ],
    );
}

/// Test GAIN envelope is correctly tracked across loops
#[test]
fn set_gain_dedeuplication_after_skip_last_loop() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a G10 b : G20 c ]2 G10 d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_gain 10",
            "play_note b4 24",
            "skip_last_loop",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 2",
            // No set_gain instruction
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a G10 b : [ G20 c : d ]2 ]3 G10 e
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_gain 10",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_gain 20",
            "play_note c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            // No set_gain instruction
            "play_note e4 24",
        ],
    );
}

#[test]
fn set_instrument_after_start_of_loop() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [@1 a]3 @1 b
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 a [@1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [[a]2 @1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [[@1 a]2 @1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "end_loop 2",
            // no set_instrument instruction
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );
}

#[test]
fn set_adsr_after_start_of_loop() {
    assert_line_matches_bytecode(
        "A1,2,3,4 [A1,2,3,4 a]3 b",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [A1,2,3,4 a A5,6,7,8 b]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [[a]2 A1,2,3,4 b A5,6,7,8 c]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_adsr 1 2 3 4",
            "play_note b4 24",
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [[A1,2,3,4 a]2 A1,2,3,4 b A5,6,7,8 c]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "end_loop 2",
            // no set_adsr instruction
            "play_note b4 24",
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            "end_loop 3",
        ],
    );
}

#[test]
fn set_gain_after_start_of_loop() {
    assert_line_matches_bytecode(
        "G10 [G10 a]3 b",
        &[
            "set_gain 10",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [G10 a G20 b]3",
        &[
            "set_gain 10",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "set_gain 20",
            "play_note b4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [[a]2 G10 b G20 c]3",
        &[
            "set_gain 10",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_gain 10",
            "play_note b4 24",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [[G10 a]2 G10 b G20 c]3",
        &[
            "set_gain 10",
            "start_loop",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "end_loop 2",
            // no set_gain instruction
            "play_note b4 24",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 3",
        ],
    );
}

#[test]
fn set_instrument_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument
@1 dummy_instrument_2

!s @1 a

A @0 !s b @0 c
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note b4 24",
            "set_instrument dummy_instrument",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s @0 a

A !s b @0 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_instrument` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn set_adsr_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument adsr 1 1 1 1

!s @0 a

A !s b A2,2,2,2 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            "set_adsr 2 2 2 2",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument adsr 2 2 2 2

!s @0 a

A !s b A2,2,2,2 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_adsr 2 2 2 2` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn set_gain_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument gain F100

!s @0 a

A !s b GF50 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            "set_gain F50",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument gain F100

!s @0 a

A !s b GF100 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_gain F100` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn nested_loops_and_envelope_bugfig() {
    assert_line_matches_bytecode(
        "[A15,7,7,31 c [ d ]2 : GF127 ]3 GF127",
        &[
            "start_loop",
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "start_loop",
            "play_note d4 24",
            "end_loop 2",
            "skip_last_loop",
            "set_gain F127",
            "end_loop 3",
            // envelope is ADSR at end of loop
            "set_gain F127",
        ],
    );
}

#[test]
fn nested_loops_and_envelope_2() {
    assert_line_matches_bytecode(
        "A15,7,7,31 [[d]2 A15,7,7,31 c : GF127 ]3 GF127",
        &[
            "set_adsr 15 7 7 31",
            "start_loop",
            "start_loop",
            "play_note d4 24",
            "end_loop 2",
            // envelope is unknown
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "skip_last_loop",
            "set_gain F127",
            "end_loop 3",
            // envelope is ADSR at end of loop
            "set_gain F127",
        ],
    );
}

#[test]
fn nested_loops_and_envelope_3() {
    assert_line_matches_bytecode(
        "GF127 [GF127 [d]2 GF127 c : GF64 ]3 GF64",
        &[
            "set_gain F127",
            "start_loop",
            // Gain is unknown
            "set_gain F127",
            "start_loop",
            "play_note d4 24",
            "end_loop 2",
            // skip GF127, envelope is known
            "play_note c4 24",
            "skip_last_loop",
            "set_gain F64",
            "end_loop 3",
            // envelope is G127 at end of loop
            "set_gain F64",
        ],
    );

    assert_line_matches_bytecode(
        "GF127 [GF127 [d]2 GF127 c : GF64 ]3 GF127",
        &[
            "set_gain F127",
            "start_loop",
            // Gain is unknown
            "set_gain F127",
            "start_loop",
            "play_note d4 24",
            "end_loop 2",
            // skip GF127, envelope is known
            "play_note c4 24",
            "skip_last_loop",
            "set_gain F64",
            "end_loop 3",
            // skip GF127, envelope is G127 at end of loop
        ],
    );
}

#[test]
fn nested_loops_and_envelope_4() {
    assert_line_matches_bytecode(
        "GF127 [GF127 [d : e]2 GF127 f : GF64 ]3 GF64",
        &[
            "set_gain F127",
            "start_loop",
            // Gain is unknown
            "set_gain F127",
            "start_loop",
            "play_note d4 24",
            "skip_last_loop",
            "play_note e4 24",
            "end_loop 2",
            // skip GF127, envelope is known
            "play_note f4 24",
            "skip_last_loop",
            "set_gain F64",
            "end_loop 3",
            // envelope is G127 at end of loop
            "set_gain F64",
        ],
    );

    assert_line_matches_bytecode(
        "GF127 [GF127 [d : e]2 GF127 f : GF64 ]3 GF127",
        &[
            "set_gain F127",
            "start_loop",
            // Gain is unknown
            "set_gain F127",
            "start_loop",
            "play_note d4 24",
            "skip_last_loop",
            "play_note e4 24",
            "end_loop 2",
            // skip GF127, envelope is known
            "play_note f4 24",
            "skip_last_loop",
            "set_gain F64",
            "end_loop 3",
            // skip GF127, envelope is G127 at end of loop
        ],
    );
}

#[test]
fn instrument_hint_and_song_loop_panic_bugfix() {
    // "hint in song-loop" panic in compiler::bytecode::InstrumentState::demote_to_song_loop()
    // found using rust-fuzz
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s ?@0 a

A r !s L r
"##,
        5,
        BytecodeError::SubroutineInstrumentHintNoInstrumentSet.into(),
    );
}
