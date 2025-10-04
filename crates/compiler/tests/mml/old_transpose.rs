// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

fn line_contains_transpose(line: &str) -> bool {
    line.contains("_+") | line.contains("_-") | line.contains("__+") | line.contains("__-")
}

fn _assert_ot_line_matches_line(transpose_line: &str, no_transpose_line: &str) {
    assert!(line_contains_transpose(transpose_line));
    assert!(!line_contains_transpose(no_transpose_line));

    let mml1 = [
        "#OldTranspose\n@1 dummy_instrument\nA @1 o4\nA ",
        transpose_line,
    ]
    .concat();

    let mml2 = ["@1 dummy_instrument\nA @1 o4\nA ", no_transpose_line].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    assert_eq!(
        mml_bytecode(&mml_data1),
        mml_bytecode(&mml_data2),
        "Testing {transpose_line:?} against MML"
    );

    let mml1 = [
        "#OldTranspose\n@1 dummy_instrument\nA !s\n!s @1 o4\nA ",
        transpose_line,
    ]
    .concat();

    let mml2 = ["@1 dummy_instrument\nA !s\n!s @1 o4\nA ", no_transpose_line].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    assert_eq!(
        subroutine_bytecode(&mml_data1, 0),
        subroutine_bytecode(&mml_data2, 0),
        "Testing {transpose_line:?} against MML"
    );
}

#[test]
fn transpose() {
    _assert_ot_line_matches_line("_+2 d e f", "e f+ g");
    _assert_ot_line_matches_line("_-2 d e f", "c d d+");

    _assert_ot_line_matches_line("_+3 c d e", "c+++ d+++ e+++");
    _assert_ot_line_matches_line("_-3 c d e", "c--- d--- e---");

    // Relative transpose
    _assert_ot_line_matches_line("__+2 d e f", "e f+ g");
    _assert_ot_line_matches_line("__-2 d e f", "c d d+");

    _assert_ot_line_matches_line("__+2 __+2 def", "_M+4 def");
    _assert_ot_line_matches_line("__-2 __-2 def", "_M-4 def");

    _assert_ot_line_matches_line("__+8 __-2 def", "_M+6 def");
    _assert_ot_line_matches_line("__-8 __+2 def", "_M-6 def");

    // transpose overrides relative transpose
    _assert_ot_line_matches_line("_-5 _-10 def", "_M-10 def");
    _assert_ot_line_matches_line("_-5 _+10 def", "_M+10 def");

    _assert_ot_line_matches_line("__+5 _-10 def", "_M-10 def");
    _assert_ot_line_matches_line("__-5 _+10 def", "_M+10 def");
}

#[test]
fn transpose_in_loops() {
    _assert_ot_line_matches_line("[ _+2 d : e ]3 f", "[ e : f+ ]3 g");
    _assert_ot_line_matches_line("[ _-2 d : e ]3 f", "[ c : d ]3 d+");

    _assert_ot_line_matches_line("[ _+3 c : d ]4 e", "[ c+++ : d+++ ]4 e+++");
    _assert_ot_line_matches_line("[ _-3 c : d ]4 e", "[ c--- : d--- ]4 e---");

    // Relative transpose
    _assert_ot_line_matches_line("[ __+2 d : e ]5 f", "[ e : f+ ]5 g");
    _assert_ot_line_matches_line("[ __-2 d : e ]5 f", "[ c : d  ]5 d+");

    _assert_ot_line_matches_line("[ __+2 __+2 d : e ]6 f", "_M+4 [d:e]6 f");
    _assert_ot_line_matches_line("[ __-2 __-2 d : e ]6 f", "_M-4 [d:e]6 f");

    _assert_ot_line_matches_line("[ __+8 __-2 d : e ]7 f", "_M+6 [d:e]7 f");
    _assert_ot_line_matches_line("[ __-8 __+2 d : e ]7 f", "_M-6 [d:e]7 f");

    // transpose overrides relative transpose
    _assert_ot_line_matches_line("[ _-5 _-10 d : e ]8 f", "_M-10 [d:e]8 f");
    _assert_ot_line_matches_line("[ _-5 _+10 d : e ]8 f", "_M+10 [d:e]8 f");

    _assert_ot_line_matches_line("[ __+5 _-10 d : e ]9 f", "_M-10 [d:e]9 f");
    _assert_ot_line_matches_line("[ __-5 _+10 d : e ]9 f", "_M+10 [d:e]9 f");

    // Nested loops
    _assert_ot_line_matches_line(
        "[ _+1 [c _+2 d : _-3 e _-4 ]2 f : g _+5 ]3 a",
        "[[c+ d++ : e---]2 f---- : g---- ]3 a+++++",
    );
    _assert_ot_line_matches_line(
        "[ __+1 [c __+2 d : __-1 e __-3 ]2 f : g __-1 ]3 a",
        "[[c+ d+++ : e++]2 f- : g- ]3 a--",
    );
}

#[test]
fn transpose_in_subroutine() {
    let mml = r##"
#OldTranspose

@0 dummy_instrument

!s1 ?@0 _+1 {cd}2

A @0 _-1 !s1 c
"##;

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s1",
            "play_note b3 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &["play_note c+4 no_keyoff 1", "portamento d+4 keyoff +6 47"],
    );
}

#[test]
fn relative_transpose_in_subroutine() {
    let mml = r##"
#OldTranspose

@0 dummy_instrument

!s1 ?@0 __-2 {cd}2

A @0 __-2 !s1 c
"##;

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s1",
            "play_note b-3 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &["play_note b-3 no_keyoff 1", "portamento c4 keyoff +5 47"],
    );
}
