// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn transpose() {
    assert_line_matches_line("_+2 d e f", "e f+ g");
    assert_line_matches_line("_-2 d e f", "c d d+");

    assert_line_matches_line("_+3 c d e", "c+++ d+++ e+++");
    assert_line_matches_line("_-3 c d e", "c--- d--- e---");

    // Relative transpose
    assert_line_matches_line("__+2 d e f", "e f+ g");
    assert_line_matches_line("__-2 d e f", "c d d+");

    assert_line_matches_line("__+2 __+2 def", "_+4 def");
    assert_line_matches_line("__-2 __-2 def", "_-4 def");

    assert_line_matches_line("__+8 __-2 def", "_+6 def");
    assert_line_matches_line("__-8 __+2 def", "_-6 def");

    // transpose overrides relative transpose
    assert_line_matches_line("_-5 _-10 def", "_-10 def");
    assert_line_matches_line("_-5 _+10 def", "_+10 def");

    assert_line_matches_line("__+5 _-10 def", "_-10 def");
    assert_line_matches_line("__-5 _+10 def", "_+10 def");
}

#[test]
fn transpose_in_loops() {
    assert_line_matches_line("[ _+2 d : e ]3 f", "[ e : f+ ]3 g");
    assert_line_matches_line("[ _-2 d : e ]3 f", "[ c : d ]3 d+");

    assert_line_matches_line("[ _+3 c : d ]4 e", "[ c+++ : d+++ ]4 e+++");
    assert_line_matches_line("[ _-3 c : d ]4 e", "[ c--- : d--- ]4 e---");

    // Relative transpose
    assert_line_matches_line("[ __+2 d : e ]5 f", "[ e : f+ ]5 g");
    assert_line_matches_line("[ __-2 d : e ]5 f", "[ c : d  ]5 d+");

    assert_line_matches_line("[ __+2 __+2 d : e ]6 f", "_+4 [d:e]6 f");
    assert_line_matches_line("[ __-2 __-2 d : e ]6 f", "_-4 [d:e]6 f");

    assert_line_matches_line("[ __+8 __-2 d : e ]7 f", "_+6 [d:e]7 f");
    assert_line_matches_line("[ __-8 __+2 d : e ]7 f", "_-6 [d:e]7 f");

    // transpose overrides relative transpose
    assert_line_matches_line("[ _-5 _-10 d : e ]8 f", "_-10 [d:e]8 f");
    assert_line_matches_line("[ _-5 _+10 d : e ]8 f", "_+10 [d:e]8 f");

    assert_line_matches_line("[ __+5 _-10 d : e ]9 f", "_-10 [d:e]9 f");
    assert_line_matches_line("[ __-5 _+10 d : e ]9 f", "_+10 [d:e]9 f");

    // Nested loops
    assert_line_matches_line(
        "[ _+1 [c _+2 d : _-3 e _-4 ]2 f : g _+5 ]3 a",
        "[[c+ d++ : e---]2 f---- : g---- ]3 a+++++",
    );
    assert_line_matches_line(
        "[ __+1 [c __+2 d : __-1 e __-3 ]2 f : g __-1 ]3 a",
        "[[c+ d+++ : e++]2 f- : g- ]3 a--",
    );
}

#[test]
fn transpose_in_subroutine() {
    let mml = r##"
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
