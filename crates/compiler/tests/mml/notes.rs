// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn c_major_scale() {
    assert_line_matches_bytecode(
        "c d e f g a b",
        &[
            "play_note c4 24",
            "play_note d4 24",
            "play_note e4 24",
            "play_note f4 24",
            "play_note g4 24",
            "play_note a4 24",
            "play_note b4 24",
        ],
    );
}

#[test]
fn play_midi_note_number() {
    assert_line_matches_line("c d e f g a b", "n60 n62 n64 n65 n67 n69 n71");
}

#[test]
fn play_sample() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@sample sample

A @sample

A s s0 s1 s2
A s,8 s1,2 s2,4
A s,%2 s1,%4 s2,%6

A s^8 s&
A s1^8 s1&
A s2,16^8 s2,16 &
"##,
        &[
            "set_instrument sample",
            // line 1
            "play_note 0 24",
            "play_note 0 24",
            "play_note 1 24",
            "play_note 2 24",
            // line 2
            "play_note 0 12",
            "play_note 1 48",
            "play_note 2 24",
            // line 3
            "play_note 0 2",
            "play_note 1 4",
            "play_note 2 6",
            // line 4
            "play_note 0 36",
            "play_note 0 slur_next 24",
            // line 5
            "play_note 1 36",
            "play_note 1 slur_next 24",
            // line 6
            "play_note 2 18",
            "play_note 2 slur_next 6",
        ],
    );
}

#[test]
#[rustfmt::skip]
fn set_octave() {
    assert_line_matches_bytecode("a",             &["play_note a4 24"]);
    assert_line_matches_bytecode("o2 o5 b",       &["play_note b5 24"]);
    assert_line_matches_bytecode("o6 c < c << c", &["play_note c6 24", "play_note c5 24", "play_note c3 24"]);
    assert_line_matches_bytecode("o2 d > d >> d", &["play_note d2 24", "play_note d3 24", "play_note d5 24"]);
}

#[test]
fn compare_accidentals_with_midi_note_numbers() {
    assert_line_matches_line("o4 e e- e-- e--- e----", "n64 n63 n62 n61 n60");
    assert_line_matches_line("o4 e e+ e++ e+++ e++++", "n64 n65 n66 n67 n68");
}

#[test]
fn too_many_accidentals() {
    let lots_of_plusses = "+".repeat(1024);
    let lots_of_minuses = "+".repeat(1024);

    assert_one_error_in_mml_line(
        &format!("c{lots_of_plusses}"),
        1,
        ValueError::InvalidNote.into(),
    );

    assert_one_error_in_mml_line(
        &format!("c{lots_of_minuses}"),
        1,
        ValueError::InvalidNote.into(),
    );

    assert_one_error_in_mml_line(
        &format!("c{lots_of_plusses}{lots_of_minuses}"),
        1,
        ValueError::InvalidNote.into(),
    );
}

#[test]
#[rustfmt::skip]
fn note_length() {
    assert_line_matches_bytecode("a", &["play_note a4 24"]);
    assert_line_matches_bytecode(
        "a1 b3 c4 d8 e16",
        &["play_note a4 96", "play_note b4 32", "play_note c4 24", "play_note d4 12", "play_note e4 6"],
    );
    assert_line_matches_bytecode(
        "l8 a l16 b l8 c8 d32 e.",
        &["play_note a4 12", "play_note b4 6", "play_note c4 12", "play_note d4 3", "play_note e4 18"],
    );
    assert_line_matches_bytecode(
        "a. b.. c...",
        &["play_note a4 36", "play_note b4 42", "play_note c4 45"],
    );
    assert_line_matches_bytecode(
        "a1. b4. c8. d8..",
        &["play_note a4 144", "play_note b4 36", "play_note c4 18", "play_note d4 21"],
    );
    assert_line_matches_bytecode(
        "l4. a",
        &["play_note a4 36"]
    );
    assert_line_matches_bytecode("a%9  b%21", &["play_note a4 9", "play_note b4 21"]);
    assert_line_matches_bytecode(
        "l%9 a l%42 b c.",
        &["play_note a4 9", "play_note b4 42", "play_note c4 63"]
    );
}

#[test]
fn lots_of_dots_in_length() {
    // Test "attempt to add with overflow" panic is fixed
    let lots_of_dots = ".".repeat(512);
    assert_line_matches_line(&format!("l1{lots_of_dots} a"), "a%190");
    assert_line_matches_line(&format!("a{lots_of_dots}"), "a%46");
    assert_line_matches_line(&format!("a ^{lots_of_dots}"), "a%70");
    assert_line_matches_line(&format!("a &{lots_of_dots}"), "a%70");
    assert_line_matches_line(&format!("r2{lots_of_dots}"), "r%94");
    assert_line_matches_line(&format!("w3{lots_of_dots}"), "w%63");
    assert_line_matches_line(
        &format!("{{ab}}1{lots_of_dots},2{lots_of_dots}"),
        "{ab}%190,%94",
    );
    assert_line_matches_line(
        &format!("{{{{ab}}}}1{lots_of_dots},8{lots_of_dots}"),
        "{{ab}}%190,%22",
    );

    // Confirm %255 the largest default length in ticks
    assert_one_error_in_mml_line("l%256", 1, ValueError::InvalidDefaultLength.into());

    assert_line_matches_line(
        "l%255 a...................................................................................",
        "a%502"
    );

    // Confirm 255 is the largest ZenLen
    assert_one_error_in_mml_line("C256", 1, ValueError::ZenLenOutOfRange(256).into());

    assert_line_matches_line(
        "C255 a1...................................................................................",
        "a%502",
    );

    assert_line_matches_line(
        "C255 l1................................................................................... a....................................................................................",
        "a%997"
    );

    assert_line_matches_line(
        "C255 l1.............................................. {ab}%1024,1.....................................................",
        "{ab}%1024,%502",
    );

    assert_line_matches_line(
        "C255 l1.............................................. {ab}%1024,.....................................................",
        "{ab}%1024,%997",
    );
}

#[test]
fn slur() {
    assert_line_matches_bytecode(
        "a & b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );

    assert_line_matches_bytecode(
        "a & b8 & c16 & d32",
        &[
            "play_note a4 no_keyoff 24",
            "play_note b4 no_keyoff 12",
            "play_note c4 no_keyoff 6",
            "play_note d4 keyoff 3",
        ],
    );

    assert_line_matches_bytecode(
        "a & >b",
        &["play_note a4 no_keyoff 24", "play_note b5 keyoff 24"],
    );

    assert_line_matches_bytecode(
        "a < & b",
        &["play_note a4 no_keyoff 24", "play_note b3 keyoff 24"],
    );

    merge_mml_commands_test(
        "a || & b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );

    merge_mml_commands_test(
        "a & || b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );
}

#[test]
fn tie() {
    assert_line_matches_bytecode("a^ b", &["play_note a4 48", "play_note b4 24"]);
    assert_line_matches_bytecode("a^2 b", &["play_note a4 72", "play_note b4 24"]);
    assert_line_matches_bytecode("a^%1 b", &["play_note a4 25", "play_note b4 24"]);

    merge_mml_commands_test("a8 ^ || b", &["play_note a4 36", "play_note b4 24"]);
    merge_mml_commands_test("a8. || ^ b", &["play_note a4 42", "play_note b4 24"]);

    // Alternative tie syntax
    merge_mml_commands_test("a || &4 b", &["play_note a4 48", "play_note b4 24"]);
    merge_mml_commands_test("a &%1 || b", &["play_note a4 25", "play_note b4 24"]);
}

#[test]
fn long_note() {
    // `wait` can rest for 1 to 256 ticks.
    // `rest` can rest for 2 to 257 tick.
    // The last rest in a wait-rest chain must be 257 ticks to prevent interference with early-release

    assert_line_matches_bytecode("a%256", &["play_note a4 keyoff 256"]);
    assert_line_matches_bytecode("a%257", &["play_note a4 keyoff 257"]);

    assert_line_matches_bytecode("a%258", &["play_note a4 no_keyoff 1", "rest 257"]);

    assert_line_matches_bytecode("a%512", &["play_note a4 no_keyoff 255", "rest 257"]);
    assert_line_matches_bytecode("a%513", &["play_note a4 no_keyoff 256", "rest 257"]);
    assert_line_matches_bytecode(
        "a%514",
        &["play_note a4 no_keyoff 256", "wait 1", "rest 257"],
    );

    assert_line_matches_bytecode(
        "a%600",
        &["play_note a4 no_keyoff 256", "wait 87", "rest 257"],
    );
}

#[test]
fn long_slurred_note() {
    // `wait` can rest for 1 to 256 ticks.

    assert_line_matches_bytecode("a%256 &", &["play_note a4 no_keyoff 256"]);
    assert_line_matches_bytecode("a%257 &", &["play_note a4 no_keyoff 256", "wait 1"]);
    assert_line_matches_bytecode("a%258 &", &["play_note a4 no_keyoff 256", "wait 2"]);

    assert_line_matches_bytecode("a%512 &", &["play_note a4 no_keyoff 256", "wait 256"]);
    assert_line_matches_bytecode(
        "a%513 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 1"],
    );
    assert_line_matches_bytecode(
        "a%514 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 2"],
    );

    assert_line_matches_bytecode(
        "a%600 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 88"],
    );
}

#[test]
fn note_too_long_errors() {
    assert_one_error_in_mml_line(
        "c%65536",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "c%65536 &",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );
}

#[test]
fn tied_note_command_ticks_overflow_error() {
    assert_one_error_in_mml_line(
        "c ^%65536",
        4,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "c ^%60000 ^%60000",
        12,
        ValueError::CommandTicksOverflow.into(),
    );

    assert_one_error_in_mml_line(
        "c &%60000 &%60000",
        12,
        ValueError::CommandTicksOverflow.into(),
    );
}
