// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn manual_detune() {
    assert_line_matches_bytecode("D+1 D0", &["set_detune +1", "disable_detune"]);
    // Testing `set_detune 0` is allowed in bytecode assembly
    assert_line_matches_bytecode("D+1 D0", &["set_detune +1", "set_detune 0"]);

    assert_line_matches_bytecode("D+10", &["set_detune +10"]);
    assert_line_matches_bytecode("D-10", &["set_detune -10"]);
    assert_line_matches_bytecode("D+300", &["set_detune +300"]);
    assert_line_matches_bytecode("D-300", &["set_detune -300"]);

    assert_line_matches_bytecode_bytes(
        "D+1 D0",
        &[opcodes::SET_DETUNE_P8, 1, opcodes::SET_DETUNE_P8, 0],
    );

    assert_line_matches_bytecode_bytes("D+10", &[opcodes::SET_DETUNE_P8, 10]);
    assert_line_matches_bytecode_bytes("D-10", &[opcodes::SET_DETUNE_N8, 0xf6]);
    assert_line_matches_bytecode_bytes("D+300", &[opcodes::SET_DETUNE_I16, 0x2c, 0x01]);
    assert_line_matches_bytecode_bytes("D-300", &[opcodes::SET_DETUNE_I16, 0xd4, 0xfe]);

    // Test limits
    assert_line_matches_bytecode("D+16383", &["set_detune +$3fff"]);
    assert_line_matches_bytecode("D-16383", &["set_detune -$3fff"]);
    assert_error_in_mml_line(
        "D+16384",
        1,
        ValueError::DetuneValueOutOfRange(16384).into(),
    );
    assert_error_in_mml_line(
        "D-16384",
        1,
        ValueError::DetuneValueOutOfRange(-16384).into(),
    );

    assert_error_in_mml_line("D", 1, ValueError::NoDetuneValue.into());
    assert_error_in_mml_line("D8", 1, ValueError::NoDetuneValueSign.into());
}

#[test]
fn detune_deduplication() {
    // Channel starts with detune set to 0
    assert_line_matches_line("D0 D0 D0", "");

    assert_line_matches_line(
        "D0 D+20 D+20 D+21 D-15 D-15 D+5 D+20 D0",
        "D0 D+20      D+21 D-15      D+5 D+20 D0",
    );

    assert_line_matches_bytecode(
        "D+25 [a]5 D-30",
        &[
            "set_detune +25",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            "set_detune -30",
        ],
    );

    assert_line_matches_bytecode(
        "D+25 [a]5 D+25",
        &[
            "set_detune +25",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            // D+25 deduplicated
        ],
    );

    assert_line_matches_bytecode(
        "D+10 a [D+25 b]5 D+25",
        &[
            "set_detune +10",
            "play_note a4 24",
            "start_loop",
            "set_detune +25",
            "play_note b4 24",
            "end_loop 5",
            // D+25 deduplicated
        ],
    );

    assert_line_matches_bytecode(
        "D+300 [D+300 a : D-40 b]5 D-40",
        &[
            "set_detune +300",
            "start_loop",
            "set_detune +300",
            "play_note a4 24",
            "skip_last_loop",
            "set_detune -40",
            "play_note b4 24",
            "end_loop 5",
            "set_detune -40",
        ],
    );

    assert_line_matches_bytecode(
        "D+300 [D+300 a : D-40 b]5 D+300",
        &[
            "set_detune +300",
            "start_loop",
            "set_detune +300",
            "play_note a4 24",
            "skip_last_loop",
            "set_detune -40",
            "play_note b4 24",
            "end_loop 5",
            // D+300 deduplicated
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s D+10 a

A @1 D+20 !s D+20 a
"##,
        &[
            "set_instrument dummy_instrument",
            "set_detune +20",
            "call_subroutine s",
            "set_detune +20",
            "play_note a4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s D+10 a

A @1 D+20 !s D+10 a
"##,
        &[
            "set_instrument dummy_instrument",
            "set_detune +20",
            "call_subroutine s",
            // D+10 deduplicated
            "play_note a4 24",
        ],
    );

    // Also tests D0 in a subroutine is not deduplicated
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s D0 a

A @1 D+10 !s D0 a
"##,
        &[
            "set_instrument dummy_instrument",
            "set_detune +10",
            "call_subroutine s",
            // D0 deduplicated
            "play_note a4 24",
        ],
    );

    // Test call subroutine does not change detune
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s a

A @1 D+30 !s D+30 a
"##,
        &[
            "set_instrument dummy_instrument",
            "set_detune +30",
            "call_subroutine s",
            // D+30 deduplicated
            "play_note a4 24",
        ],
    );
}

#[test]
fn detune_cents() {
    assert_line_matches_bytecode("MD+50 a", &["set_detune +106", "play_note a4 24"]);

    // Test deduplication
    assert_line_matches_bytecode(
        "MD-50 a a",
        &["set_detune -103", "play_note a4 24", "play_note a4 24"],
    );

    // From mml-syntax.md
    assert_line_matches_line("MD+20 c d e", "D+25 c D+28 d D+31 e");

    assert_line_matches_bytecode(
        "MD+25 a > a > a",
        &[
            "set_detune +52",
            "play_note a4 24",
            "set_detune +105",
            "play_note a5 24",
            "set_detune +210",
            "play_note a6 24",
        ],
    );

    assert_line_matches_bytecode(
        "MD-5 a > a > a",
        &[
            "set_detune -10",
            "play_note a4 24",
            "set_detune -21",
            "play_note a5 24",
            "set_detune -42",
            "play_note a6 24",
        ],
    );

    // MD affects portamento
    assert_line_matches_bytecode(
        "MD-30 o5 {dg}2",
        &[
            "set_detune -83",
            "play_note d5 no_keyoff 1",
            "set_detune -110",
            "portamento g5 keyoff +34 47",
        ],
    );

    // MD affects broken chords
    assert_line_matches_line_and_bytecode(
        "MD+25 {{dgf}}",
        "[D+35 d%1 & : D+47 g%1 & D+42 f%1 &]8 D+47 g%2",
        &[
            "start_loop 8",
            "set_detune +35",
            "play_note d4 no_keyoff 1",
            "skip_last_loop",
            "set_detune +47",
            "play_note g4 no_keyoff 1",
            "set_detune +42",
            "play_note f4 no_keyoff 1",
            "end_loop",
            "set_detune +47",
            "play_note g4 keyoff 2",
        ],
    );

    // MD0 disables manual detune immediatley
    assert_line_matches_bytecode(
        "D+22 c MD0 r d",
        &[
            "set_detune +22",
            "play_note c4 24",
            "disable_detune",
            "rest 24",
            "play_note d4 24",
        ],
    );

    // MD0 disables detune immediatley
    assert_line_matches_bytecode(
        "MD+75 c MD0 r d",
        &[
            "set_detune +95",
            "play_note c4 24",
            "disable_detune",
            "rest 24",
            "play_note d4 24",
        ],
    );

    // `D0` disables detune_cents
    assert_line_matches_bytecode(
        "MD-75 c D0 r d",
        &[
            "set_detune -91",
            "play_note c4 24",
            "disable_detune",
            "rest 24",
            "play_note d4 24",
        ],
    );

    // `D` disables detune_cents
    assert_line_matches_bytecode(
        "MD-75 c D+80 r d",
        &[
            "set_detune -91",
            "play_note c4 24",
            "set_detune +80",
            "rest 24",
            "play_note d4 24",
        ],
    );

    // Test limits
    assert_line_matches_bytecode("MD+600", &[]);
    assert_line_matches_bytecode("MD-600", &[]);
    assert_error_in_mml_line("MD+601", 1, ValueError::DetuneCentsOutOfRange(601).into());
    assert_error_in_mml_line("MD-601", 1, ValueError::DetuneCentsOutOfRange(-601).into());

    assert_error_in_mml_line("MD", 1, ValueError::NoDetuneCents.into());
    assert_error_in_mml_line("MD100", 1, ValueError::NoDetuneCentsSign.into());
}

#[test]
fn small_detune_cents() {
    assert_line_matches_bytecode(
        "MD+1 o4 c o2 d e o3 f",
        &[
            "set_detune +1",
            "play_note c4 24",
            "set_detune 0",
            "play_note d2 24",
            "play_note e2 24",
            "set_detune +1",
            "play_note f3 24",
        ],
    );
}

/// Tests detune changes MP vibrato
#[test]
fn manual_detune_with_mp() {
    assert_line_matches_bytecode("MP200,6 c", &["set_vibrato 41 6", "play_note c4 24"]);

    assert_line_matches_bytecode(
        "D+200 MP200,6 c",
        &["set_detune +200", "set_vibrato 45 6", "play_note c4 24"],
    );
    assert_line_matches_bytecode(
        "D-200 MP200,6 c",
        &["set_detune -200", "set_vibrato 37 6", "play_note c4 24"],
    );
}

/// Tests detune-cents changes MP vibrato
#[test]
fn detune_cents_with_mp() {
    assert_line_matches_bytecode(
        "MD+600 MP200,6 c",
        &["set_detune +888", "set_vibrato 58 6", "play_note c4 24"],
    );
    assert_line_matches_bytecode(
        "MD-600 MP200,6 c",
        &["set_detune -628", "set_vibrato 29 6", "play_note c4 24"],
    );
}
