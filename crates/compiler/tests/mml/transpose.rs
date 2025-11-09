// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn channel_transpose() {
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
fn channel_transpose_in_loops() {
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
fn channel_transpose_in_subroutine() {
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
fn relative_channel_transpose_in_subroutine() {
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
fn transpose_header() {
    let mml = r##"
#Transpose +5

@0 dummy_instrument

!s ?@0 c __M-2 c _M0 c

A @0 !s c __M-2 c _M0 c
B @0 !s c __M+2 c _M0 c
"##;

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note f4 24",
            "play_note d+4 24",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_b_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note f4 24",
            "play_note g4 24",
            "play_note c4 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &["play_note f4 24", "play_note d+4 24", "play_note c4 24"],
    );
}

// Driver Transpose
// ================

#[test]
fn set_transpose() {
    assert_line_matches_bytecode(
        "c _+2 c",
        &[
            "play_note c4 keyoff 24",
            "set_transpose +2",
            "play_note c4 keyoff 24",
        ],
    );
    assert_line_matches_bytecode(
        "c _-3 c",
        &[
            "play_note c4 keyoff 24",
            "set_transpose -3",
            "play_note c4 keyoff 24",
        ],
    );

    assert_line_matches_bytecode(
        "c _0 c",
        &[
            "play_note c4 keyoff 24",
            "disable_transpose",
            "play_note c4 keyoff 24",
        ],
    );

    assert_one_error_in_mml_line("_ c", 1, ValueError::NoTranspose.into());
    assert_one_error_in_mml_line("_5", 1, ValueError::NoTransposeSign.into());

    assert_line_matches_bytecode("_-128", &["set_transpose -128"]);
    assert_one_error_in_mml_line("_-129", 1, ValueError::TransposeOutOfRange(-129).into());

    assert_line_matches_bytecode("_+127", &["set_transpose +127"]);
    assert_one_error_in_mml_line("_+128", 1, ValueError::TransposeOutOfRange(128).into());
}

#[test]
fn adjust_transpose() {
    assert_line_matches_bytecode(
        "c __-4 c",
        &[
            "play_note c4 keyoff 24",
            "adjust_transpose -4",
            "play_note c4 keyoff 24",
        ],
    );
    assert_line_matches_bytecode(
        "c __-5 c",
        &[
            "play_note c4 keyoff 24",
            "adjust_transpose -5",
            "play_note c4 keyoff 24",
        ],
    );

    assert_one_error_in_mml_line("__ c", 1, ValueError::NoRelativeTranspose.into());
    assert_one_error_in_mml_line("__5", 1, ValueError::NoRelativeTransposeSign.into());

    assert_line_matches_bytecode("__-128", &["adjust_transpose -128"]);
    assert_one_error_in_mml_line(
        "__-129",
        1,
        ValueError::RelativeTransposeOutOfRange(-129).into(),
    );

    assert_line_matches_bytecode("__+127", &["adjust_transpose +127"]);
    assert_one_error_in_mml_line(
        "__+128",
        1,
        ValueError::RelativeTransposeOutOfRange(128).into(),
    );
}

// MP Vibrato
// ----------

#[test]
fn set_transpose_mml_disables_mp_vibraro() {
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 _+1 c

A @0 MP100,2 c !s1 c
"##,
        20,
        ChannelError::MpVibratoInSongWithTranspose,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 _-1 !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoInSongWithTranspose,
    );
}

#[test]
fn adjust_transpose_mml_disables_mp_vibraro() {
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 __+1 c

A @0 MP100,2 c !s1 c
"##,
        20,
        ChannelError::MpVibratoInSongWithTranspose,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 __-1 !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoInSongWithTranspose,
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
        20,
        ChannelError::MpVibratoInSongWithTranspose,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 \asm { set_transpose +1 } !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoInSongWithTranspose,
    );
}

#[test]
fn adjust_transpose_asm_disables_mp_vibraro() {
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 \asm { adjust_transpose +1 } c

A @0 MP100,2 c !s1 c
"##,
        20,
        ChannelError::MpVibratoInSongWithTranspose,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 \asm { adjust_transpose -1 } !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoInSongWithTranspose,
    );
}

// Portamento
// ----------

const PORTAMENTO: &str = "portamento d4 keyoff +12 23";
const PORTAMENTO_CALC: &str = "portamento_calc d4 keyoff 22 23";

struct TransposeMmlCommand {
    mml: &'static str,
    asm: &'static str,
}

#[rustfmt::skip]
const ALL_TRANSPOSE_MML_COMMANDS: &[TransposeMmlCommand] = &[
    TransposeMmlCommand { mml: "_+1", asm: "set_transpose +1" },
    TransposeMmlCommand { mml: "_-1", asm: "set_transpose -1" },
    TransposeMmlCommand { mml: "__+2", asm: "adjust_transpose +2" },
    TransposeMmlCommand { mml: "__-2", asm: "adjust_transpose -2" },
    TransposeMmlCommand { mml: "\\asm { set_transpose +3 }", asm: "set_transpose +3" },
    TransposeMmlCommand { mml: "\\asm { set_transpose -3 }", asm: "set_transpose -3" },
    TransposeMmlCommand { mml: "\\asm { adjust_transpose +4 }", asm: "adjust_transpose +4" },
    TransposeMmlCommand { mml: "\\asm { adjust_transpose -4 }", asm: "adjust_transpose -4" },
];

#[test]
fn driver_transpose_in_subroutine_disables_portamento_velocity_calculation() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

!s1 ___ c

A @0 {cd} !s1 {cd}
"##
        .replace("___", transpose.mml);

        assert_mml_channel_a_matches_bytecode(
            &mml,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
            ],
        );
    }
}

#[test]
fn driver_transpose_disabled_in_subroutine() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

!s1 _0 c

A @0 ___ {cd} !s1 {cd}
"##
        .replace("___", transpose.mml);

        assert_mml_channel_a_matches_bytecode(
            &mml,
            &[
                "set_instrument dummy_instrument",
                transpose.asm,
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
            ],
        );
    }
}

#[test]
fn subroutine_called_with_driver_transpose() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

!s1 @0 {cd}4
!s2 @0 {cd}4

A !s1 ___ !s2
"##
        .replace("___", transpose.mml);

        // Called without transpose
        assert_mml_subroutine_matches_bytecode(
            &mml,
            0,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
            ],
        );

        // Called with transpose
        assert_mml_subroutine_matches_bytecode(
            &mml,
            1,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
            ],
        );
    }
}

#[test]
fn nested_subroutines_and_driver_transpose() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

!s0 @0 {cd}
!s1 @0 {cd} !s0 r
!s2 @0 \asm {call_subroutine s1} r

!s3 @0 {cd}
!s4 @0 {cd} !s3 r

A !s1 ___ !s2 _0 !s4
"##
        .replace("___", transpose.mml);

        // Called with transpose
        assert_mml_subroutine_matches_bytecode(
            &mml,
            0,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
            ],
        );
        assert_mml_subroutine_matches_bytecode(
            &mml,
            1,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "call_subroutine s0",
                "rest 24",
            ],
        );
        assert_mml_subroutine_matches_bytecode(
            &mml,
            2,
            &[
                "set_instrument dummy_instrument",
                "call_subroutine s1",
                "rest 24",
            ],
        );

        // called without transpose
        assert_mml_subroutine_matches_bytecode(
            &mml,
            3,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
            ],
        );
        assert_mml_subroutine_matches_bytecode(
            &mml,
            4,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
                "call_subroutine s3",
                "rest 24",
            ],
        );
    }
}

#[test]
fn driver_transpose_in_loop() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        assert_line_matches_bytecode(
            &"{cd} [{cd} ___]2 {cd}".replace("___", transpose.mml),
            &[
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                transpose.asm,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
            ],
        );

        assert_line_matches_bytecode(
            &"___ {cd} [{cd} _0]2 {cd}".replace("___", transpose.mml),
            &[
                transpose.asm,
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "disable_transpose",
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
            ],
        );
    }
}

#[test]
fn disable_driver_transpose_in_loop() {
    assert_line_matches_bytecode(
        "{cd} [{cd} _0]2",
        &[
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            // ::TODO detect and deduplicate `_0` commands::
            "disable_transpose",
            "end_loop 2",
        ],
    );

    assert_line_matches_bytecode(
        "_+1 {cd} [{cd} _0]2 {cd}",
        &[
            "set_transpose +1",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "disable_transpose",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );
}

#[test]
fn skip_last_loop_and_driver_transpose_in_loop() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        assert_line_matches_bytecode(
            &"[{cd} : ___]2 {cd}".replace("___", transpose.mml),
            &[
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "skip_last_loop",
                transpose.asm,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
            ],
        );

        assert_line_matches_bytecode(
            &"[{cd} _0 : ___]2 {cd}".replace("___", transpose.mml),
            &[
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "disable_transpose",
                "skip_last_loop",
                transpose.asm,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
            ],
        );
    }
}

#[test]
fn loop_contains_driver_transpose_subroutine() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

!s1 ___ c

A @0 [ {cd} !s1 ]2
"##
        .replace("___", transpose.mml);

        assert_mml_channel_a_matches_bytecode(
            &mml,
            &[
                "set_instrument dummy_instrument",
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "call_subroutine s1",
                "end_loop 2",
            ],
        );
    }
}

#[test]
fn driver_transpose_in_nested_loop() {
    assert_line_matches_bytecode(
        "[{cd} [{cd} __-1]2 _0]3",
        &[
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose -1",
            "end_loop 2",
            "disable_transpose",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "[[{cd} : _0]2 {cd} __+2]3",
        &[
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "skip_last_loop",
            "disable_transpose",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "adjust_transpose +2",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "[[{cd} _+1 : _0 {cd}]2 {cd} _-1]3",
        &[
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "set_transpose +1",
            "skip_last_loop",
            "disable_transpose",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "set_transpose -1",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "[[{cd} _+1 : _0 {cd}]2 {cd} _0]3",
        &[
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "set_transpose +1",
            "skip_last_loop",
            "disable_transpose",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "disable_transpose",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "_+1 [[{cd} _+1 : _0 {cd}]2 {cd} _0]3",
        &[
            "set_transpose +1",
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "set_transpose +1",
            "skip_last_loop",
            "disable_transpose",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "disable_transpose",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "[[{cd} _0 : _+1 {cd}]2 {cd}]3",
        &[
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "disable_transpose",
            "skip_last_loop",
            "set_transpose +1",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "_+1 [[{cd} _0 : {cd}]2 {cd}]3",
        &[
            "set_transpose +1",
            "start_loop",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "disable_transpose",
            "skip_last_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 3",
        ],
    );
}

#[test]
fn transpose_and_song_loop() {
    for transpose in ALL_TRANSPOSE_MML_COMMANDS {
        let mml = r##"
@0 dummy_instrument

A @0 {cd} L {cd} ___ r
"##
        .replace("___", transpose.mml);

        assert_mml_channel_a_matches_looping_bytecode(
            &mml,
            &[
                "set_instrument dummy_instrument",
                "play_note c4 no_keyoff 1",
                PORTAMENTO,
                // Loop point
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                transpose.asm,
                "rest 24",
            ],
        );
    }
}
