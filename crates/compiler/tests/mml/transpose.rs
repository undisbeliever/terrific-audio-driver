// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn master_transpose() {
    assert_line_matches_line("_M+2 d e f", "e f+ g");
    assert_line_matches_line("_M-2 d e f", "c d d+");

    assert_line_matches_line("_M+3 c d e", "c+++ d+++ e+++");
    assert_line_matches_line("_M-3 c d e", "c--- d--- e---");

    // Relative transpose
    assert_line_matches_line("__M+2 d e f", "e f+ g");
    assert_line_matches_line("__M-2 d e f", "c d d+");

    assert_line_matches_line("__M+2 __M+2 def", "_M+4 def");
    assert_line_matches_line("__M-2 __M-2 def", "_M-4 def");

    assert_line_matches_line("__M+8 __M-2 def", "_M+6 def");
    assert_line_matches_line("__M-8 __M+2 def", "_M-6 def");

    // transpose overrides relative transpose
    assert_line_matches_line("_M-5 _M-10 def", "_M-10 def");
    assert_line_matches_line("_M-5 _M+10 def", "_M+10 def");

    assert_line_matches_line("__M+5 _M-10 def", "_M-10 def");
    assert_line_matches_line("__M-5 _M+10 def", "_M+10 def");

    // disable transpose
    assert_line_matches_line("_M+2 d __M+1 e _M0 f", "e g f");
    assert_line_matches_line("_M-2 d __M-1 e _M0 f", "c d- f");
}

#[test]
fn master_transpose_in_loops() {
    assert_line_matches_line("[ _M+2 d : e ]3 f", "[ e : f+ ]3 g");
    assert_line_matches_line("[ _M-2 d : e ]3 f", "[ c : d ]3 d+");

    assert_line_matches_line("[ _M+3 c : d ]4 e", "[ c+++ : d+++ ]4 e+++");
    assert_line_matches_line("[ _M-3 c : d ]4 e", "[ c--- : d--- ]4 e---");

    // Relative transpose
    assert_line_matches_line("[ __M+2 d : e ]5 f", "[ e : f+ ]5 g");
    assert_line_matches_line("[ __M-2 d : e ]5 f", "[ c : d  ]5 d+");

    assert_line_matches_line("[ __M+2 __M+2 d : e ]6 f", "_M+4 [d:e]6 f");
    assert_line_matches_line("[ __M-2 __M-2 d : e ]6 f", "_M-4 [d:e]6 f");

    assert_line_matches_line("[ __M+8 __M-2 d : e ]7 f", "_M+6 [d:e]7 f");
    assert_line_matches_line("[ __M-8 __M+2 d : e ]7 f", "_M-6 [d:e]7 f");

    // transpose overrides relative transpose
    assert_line_matches_line("[ _M-5 _M-10 d : e ]8 f", "_M-10 [d:e]8 f");
    assert_line_matches_line("[ _M-5 _M+10 d : e ]8 f", "_M+10 [d:e]8 f");

    assert_line_matches_line("[ __M+5 _M-10 d : e ]9 f", "_M-10 [d:e]9 f");
    assert_line_matches_line("[ __M-5 _M+10 d : e ]9 f", "_M+10 [d:e]9 f");

    // Nested loops
    assert_line_matches_line(
        "[ _M+1 [c _M+2 d : _M-3 e _M-4 ]2 f : g _M+5 ]3 a",
        "[[c+ d++ : e---]2 f---- : g---- ]3 a+++++",
    );
    assert_line_matches_line(
        "[ __M+1 [c __M+2 d : __M-1 e __M-3 ]2 f : g __M-1 ]3 a",
        "[[c+ d+++ : e++]2 f- : g- ]3 a--",
    );
}

#[test]
fn master_transpose_in_subroutine() {
    let mml = r##"
@0 dummy_instrument

!s1 ?@0 _M+1 {cd}2

A @0 _M-1 !s1 c
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
fn relative_master_transpose_in_subroutine() {
    let mml = r##"
@0 dummy_instrument

!s1 ?@0 __M-2 {cd}2

A @0 __M-2 !s1 c
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

#[test]
fn set_transpose_asm_disables_portamento_velocity_calculation() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 \asm { set_transpose +1 } {cd}4

A @0 {cd}4 !s1 {cd}4
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
            "call_subroutine s1",
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 {cd}4

A @0 {cd}4 !s1 {cd}4 \asm { set_transpose -1 }
"##,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
        ],
    );
}

#[test]
fn adjust_transpose_asm_disables_portamento_velocity_calculation() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 \asm { adjust_transpose +1 } {cd}4

A @0 {cd}4 !s1 {cd}4
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
            "call_subroutine s1",
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 {cd}4

A @0 {cd}4 !s1 {cd}4 \asm { adjust_transpose -1 }
"##,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 22 23",
        ],
    );
}

#[test]
fn set_transpose_asm_disables_mp_vibraro() {
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 \asm { set_transpose +1 } c

A @0 MP100,2 c !s1 c
"##,
        6,
        ChannelError::MpVibratoInSongWithTranspose,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 \asm { set_transpose +1 } !s1

"##,
        "!s1",
        5,
        ChannelError::MpVibratoInSongWithTranspose,
    );
}
