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
        ChannelError::MpVibratoWithDriverTransposeActive,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 _-1 !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoWithDriverTransposeActive,
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
        ChannelError::MpVibratoWithDriverTransposeActive,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 __-1 !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoWithDriverTransposeActive,
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
        ChannelError::MpVibratoWithDriverTransposeActive,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 \asm { set_transpose +1 } !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoWithDriverTransposeActive,
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
        ChannelError::MpVibratoWithDriverTransposeActive,
    );

    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 MP100,2 c

A @0 \asm { adjust_transpose -1 } !s1

"##,
        "!s1",
        13,
        ChannelError::MpVibratoWithDriverTransposeActive,
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
const SET_TRANSPOSE_MML_COMMANDS: &[TransposeMmlCommand] = &[
    TransposeMmlCommand { mml: "_+1", asm: "set_transpose +1" },
    TransposeMmlCommand { mml: "_-1", asm: "set_transpose -1" },
    TransposeMmlCommand { mml: "\\asm { set_transpose +3 }", asm: "set_transpose +3" },
    TransposeMmlCommand { mml: "\\asm { set_transpose -3 }", asm: "set_transpose -3" },
];

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
fn disable_driver_transpose_asm_in_loop() {
    assert_line_matches_bytecode(
        "{cd} [{cd} \\asm {disable_transpose} ]2",
        &[
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "disable_transpose",
            "end_loop 2",
        ],
    );

    assert_line_matches_bytecode(
        "_+1 {cd} [{cd} \\asm {disable_transpose} ]2 {cd}",
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

    assert_line_matches_bytecode(
        "_+1 {cd} [{cd} \\asm { set_transpose 0 } ]2 {cd}",
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
    for transpose in SET_TRANSPOSE_MML_COMMANDS {
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

#[test]
fn driver_transpose_asm_in_asm_loop() {
    assert_line_matches_bytecode(
        "_+1 \\asm { start_loop | disable_transpose | wait 24 | end_loop 2 } {cd}",
        &[
            "set_transpose +1",
            "start_loop",
            "disable_transpose",
            "wait 24",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "\\asm { start_loop | set_transpose +1 | wait 24 | end_loop 2 } {cd}",
        &[
            "start_loop",
            "set_transpose +1",
            "wait 24",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
        ],
    );

    assert_line_matches_bytecode(
        "\\asm { start_loop | adjust_transpose -1 | wait 24 | end_loop 2 } {cd}",
        &[
            "start_loop",
            "adjust_transpose -1",
            "wait 24",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
        ],
    );

    assert_line_matches_bytecode(
        "\\asm { start_loop | adjust_transpose +1 | wait 24 | skip_last_loop | disable_transpose | rest 24 | end_loop 2 } {cd}",
        &[
            "start_loop",
            "adjust_transpose +1",
            "wait 24",
            "skip_last_loop",
            "disable_transpose",
            "rest 24",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
        ],
    );

    assert_line_matches_bytecode(
        "\\asm { start_loop | disable_transpose | wait 24 | skip_last_loop | adjust_transpose +1 | rest 24 | end_loop 2 } {cd}",
        &[
            "start_loop",
            "disable_transpose",
            "wait 24",
            "skip_last_loop",
            "adjust_transpose +1",
            "rest 24",
            "end_loop 2",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );
}

#[test]
fn adjust_driver_transpose_to_zero_test() {
    // Use portamento instructions to detect if the analyser can track relative transpose command

    assert_line_matches_bytecode(
        "_+5 {cd} __-5 {cd}",
        &[
            "set_transpose +5",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose -5",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "_-5 {cd} __+5 {cd}",
        &[
            "set_transpose -5",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose +5",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );
}

#[test]
fn adjust_driver_transpose_in_loop_zero_test() {
    assert_line_matches_bytecode(
        "[__+3 {cd}]10 __-30 {cd}",
        &[
            "start_loop",
            "adjust_transpose +3",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "end_loop 10",
            // transpose is -30
            "adjust_transpose -30",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "_+5 [__-5 {cd} : __+5]10 {cd}",
        &[
            "set_transpose +5",
            "start_loop",
            "adjust_transpose -5",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "skip_last_loop",
            "adjust_transpose +5",
            "end_loop 10",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "__-10 [{cd} __+2]5 {cd}",
        &[
            "adjust_transpose -10",
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose +2",
            "end_loop 5",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "_+30 [__-7 {cd}]7 [{cd} __+1]19 {cd}",
        &[
            "set_transpose +30",
            "start_loop",
            "adjust_transpose -7",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "end_loop 7",
            // transpose is -19
            "start_loop",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose +1",
            "end_loop 19",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "[{cd} __+10 {cd} _-5]10 __+5 {cd}",
        &[
            "start_loop",
            // transpose is 0 or 5
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose +10",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "set_transpose -5",
            "end_loop 10",
            // transpose is -5
            "adjust_transpose +5",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "[{cd} __+10 {cd} : _-5]10 __-5 {cd}",
        &[
            "start_loop",
            // transpose is 0 or 5
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "adjust_transpose +10",
            "play_note c4 no_keyoff 1",
            PORTAMENTO_CALC,
            "skip_last_loop",
            "set_transpose -5",
            "end_loop 10",
            // transpose is +5
            "adjust_transpose -5",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );

    assert_line_matches_bytecode(
        "[ {cd} [[__+3 r : __+5]7]2 : [ __-6 r ]17 {cd} ]50 r __-102 {cd}",
        &[
            "start_loop",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "start_loop",
            "start_loop",
            "adjust_transpose +3",
            "rest 24",
            "skip_last_loop",
            "adjust_transpose +5",
            "end_loop 7",
            "end_loop 2",
            // transpose is 102
            "skip_last_loop",
            "start_loop",
            "adjust_transpose -6",
            "rest 24",
            "end_loop 17",
            // transpose is 0
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
            "end_loop 50",
            // transpose is 102
            "rest 24",
            "adjust_transpose -102",
            "play_note c4 no_keyoff 1",
            PORTAMENTO,
        ],
    );
}

#[test]
fn transpose_overflow_errors() {
    assert_one_error_in_mml_line(
        "_+127 __+1 r",
        7,
        BytecodeError::DriverTransposeOverflows.into(),
    );
    assert_one_error_in_mml_line(
        "_-128 __-1 r",
        7,
        BytecodeError::DriverTransposeOverflows.into(),
    );

    assert_one_error_in_mml_line(
        "_+127 __-64 r __-64 r __-64 r __-64",
        31,
        BytecodeError::DriverTransposeOverflows.into(),
    );

    assert_one_error_in_mml_line(
        "_-127 __+64 r __+64 r __+64 r __+64",
        31,
        BytecodeError::DriverTransposeOverflows.into(),
    );
}

#[test]
fn transpose_overflow_in_loop_errors() {
    assert_one_error_in_mml_line(
        "_+127 [__-4 r]64",
        8,
        BytecodeError::DriverTransposeOverflowsInLoop.into(),
    );

    assert_one_error_in_mml_line(
        "_-127 [__+4 r]64",
        8,
        BytecodeError::DriverTransposeOverflowsInLoop.into(),
    );

    assert_one_error_in_mml_line(
        "[__+100 : _+27 r]2 __+1 r",
        20,
        BytecodeError::DriverTransposeOverflows.into(),
    );
    assert_one_error_in_mml_line(
        "[__-100 : _-28 r]2 __-1 r",
        20,
        BytecodeError::DriverTransposeOverflows.into(),
    );

    assert_line_matches_bytecode(
        "[[__+10 r]10 _+27]2",
        &[
            "start_loop",
            "start_loop",
            "adjust_transpose +10",
            "rest 24",
            "end_loop 10",
            "set_transpose +27",
            "end_loop 2",
            // Transpose is +127
        ],
    );
    assert_one_error_in_mml_line(
        "[[__+10 r]10 _+28]2",
        3,
        BytecodeError::DriverTransposeOverflowsInLoop.into(),
    );

    assert_line_matches_bytecode(
        "[[__-10 r]10 _-28]2",
        &[
            "start_loop",
            "start_loop",
            "adjust_transpose -10",
            "rest 24",
            "end_loop 10",
            "set_transpose -28",
            "end_loop 2",
            // Transpose is -128
        ],
    );
    assert_one_error_in_mml_line(
        "[[__-10 r]10 _-29]2",
        3,
        BytecodeError::DriverTransposeOverflowsInLoop.into(),
    );
}

#[test]
fn transpose_overflow_errors_in_song_loop() {
    assert_one_error_in_mml_line(
        "r L __+1 r",
        5,
        BytecodeError::DriverTransposeOverflows.into(),
    );
    assert_one_error_in_mml_line(
        "r L __-1 r",
        5,
        BytecodeError::DriverTransposeOverflows.into(),
    );

    // Transpose after `L` is known
    assert_one_error_in_mml_line(
        "__+127 __+1 r L _+1 r __+1",
        8,
        BytecodeError::DriverTransposeOverflows.into(),
    );
    assert_one_error_in_mml_line(
        "__-128 __-1 r L _-1 r __-1",
        8,
        BytecodeError::DriverTransposeOverflows.into(),
    );

    // Transpose does not overflow
    assert_looping_line_matches_bytecode(
        "r L __+1 r _+1",
        &[
            "rest 24",
            "adjust_transpose +1",
            "rest 24",
            "set_transpose +1",
        ],
    );

    // Transpose does not overflow
    assert_looping_line_matches_bytecode(
        "r L __-1 r _-1",
        &[
            "rest 24",
            "adjust_transpose -1",
            "rest 24",
            "set_transpose -1",
        ],
    );
}

#[test]
fn transposed_note_out_of_range_error_tests() {
    assert_one_error_in_mml_line(
        "_+1 o6 b",
        8,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("b6"),
            transpose: 1..=1,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    assert_one_error_in_mml_line(
        "_-1 o2 c",
        8,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c2"),
            transpose: -1..=-1,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    assert_one_error_in_mml_line(
        "[__+7 o3 c]8",
        10,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c3"),
            transpose: 7..=56,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    assert_one_error_in_mml_line(
        "[__-3 c : __-5]5",
        7,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: -35..=-3,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_range_in_subroutine_with_known_instrument() {
    // Instrument is known, transpose is known
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s @1 o3 _-21 c

A !s
"##,
        "!s",
        15,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c3"),
            transpose: -21..=-21,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    // Called with transpose disabled
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s @1 [__+7 o3 c]8

A !s
"##,
        "!s",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c3"),
            transpose: 7..=56,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_range_in_subroutine_call_with_known_instrument() {
    // Instrument is known, transpose is not known
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s @1 c

A _+4 !s _+36 !s _-12 !s __-12 !s
"##,
        "!s",
        7,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: -24..=36,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    // Instrument is known, subroutine adjusts transpose
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s @1 o5 c __-12

A _+4 [ !s r ]5
"##,
        "!s",
        10,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c5"),
            transpose: -44..=4,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_range_in_subroutine_call_with_unknown_instrument() {
    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s o3c o4c o5c

A @1 _+26 !s
"##,
        11,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c3")..=note("c5"),
            transpose: 26..=26,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s o3c o4c o5c

A @1 _-17 !s
"##,
        11,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c3")..=note("c5"),
            transpose: -17..=-17,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    // transpose inst_range
    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s o3c o4c o5c

A @1 _+10 [ __-7 !s ]5
"##,
        18,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c3")..=note("c5"),
            transpose: -25..=3,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );

    // subroutine modifies transpose
    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s o5 c __-12

A @1 _+4 [ !s ]5
"##,
        12,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c5")..=note("c5"),
            transpose: -44..=4,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_inst_range_in_subroutine_call_with_known_and_unknown_instruments() {
    // Tests:
    //  * Subroutine with known and unknown instrument notes
    //  * Test only unknown notes are in the error message
    //  * Test instrument after subroutine call is correct
    //  * Only the middle call is out-of-inst_range

    assert_one_error_in_channel_a_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s o4cd @135 o4c o5c

A @14 _+2 !s @14 _-3 !s !s @135 _-3 !s
"##,
        22,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c4")..=note("d4"),
            transpose: -3..=-3,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s o4cd @135 o4c o5c

A @14 _+2 [@14 !s __-3]2 @135 !s !s _-0 [@135 !s __-3]2
"##,
        16,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            notes: note("c4")..=note("d4"),
            transpose: -1..=2,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    // unknown instrument notes OK, known instrument notes out of range
    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s o5cd @14 o4 c

A [@135 __-1 !s r ]6
"##,
        "!s",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: -6..=-1,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    // unknown instrument notes OK, known instrument notes out of range
    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s o4cd @14 o4 c

A [@135 __+1 !s r ]18
"##,
        "!s",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: 1..=18,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_range_in_subroutine_sets_instrument_in_loop() {
    assert_mml_subroutine_matches_bytecode(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s [c @14]2

A [@135 __+1 !s r ]11
"##,
        0,
        &[
            "start_loop",
            "play_note c4 24",
            "set_instrument f1000_o4",
            "end_loop 2",
        ],
    );

    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s [c @14]2

A [@135 __+1 !s r ]12
"##,
        "!s",
        5,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: 1..=12,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_out_of_inst_range_in_nested_subroutine_call_with_known_and_unknown_instruments()
{
    assert_one_error_in_channel_a_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o4d @135 o4c o5c
!s1 o4c !s0 __+1 !s0

A @14 _+2 !s1 @14 _-3 !s1 !s1 @135 _-3 !s1
"##,
        23,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            // Only the first `o4c` and `o4d` is played with an unknown instrument
            notes: note("c4")..=note("d4"),
            transpose: -3..=-3,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o4c @135 o4c o5c
!s1 o4d !s0 __-1 !s0

A @14 _+2 [@14 !s1 __-3]2 @135 !s1 !s1 _-0 [@135 !s1 __-3]2
"##,
        16,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            // Only the first `c4` and `d4` is played with an unknown instrument
            notes: note("c4")..=note("d4"),
            transpose: -2..=2,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    // unknown instrument notes OK, known instrument notes out of range
    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o5c @14 o4 c
!s1 __-1 !s0

A [@135 !s1 r ]6
"##,
        "!s0",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: -6..=-1,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    // unknown instrument notes OK, known instrument notes out of range
    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o4c @14 o4 c
!s1 !s0 __+2

A [@135 !s1 r ]18
"##,
        "!s0",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: 0..=34,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    // unknown instrument notes OK, known instrument notes out of range
    assert_one_subroutine_error_in_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o4c @14 o4 c
!s1 __+1 !s0

A [@135 !s1 r ]18
"##,
        "!s0",
        16,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: 1..=18,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@14  f1000_o4
@135 f1000_o3_o5

!s0 o4d o5d
!s1 o4c @135 !s0 !s0 r

A @14 _+2 !s1 @14 _-3 !s1 !s1 @135 _-3 !s1
"##,
        23,
        BytecodeError::TransposedSubroutineNotesOutOfRange {
            // Only the first `o4c` in `s1` is invoked with an unknown instrument
            notes: note("c4")..=note("c4"),
            transpose: -3..=-3,
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );
}

#[test]
fn transposed_note_overflow_error_tests() {
    assert_one_error_in_mml_line(
        "_+47 c",
        6,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: 47..=47,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
    assert_one_error_in_mml_line(
        "_+48 c",
        6,
        BytecodeError::TransposedNoteOverflow(note("c4"), 48..=48).into(),
    );

    assert_one_error_in_mml_line(
        "_-48 c",
        6,
        BytecodeError::TransposedNoteOutOfRange {
            note: note("c4"),
            transpose: -48..=-48,
            inst_range: note("c2")..=note("b6"),
        }
        .into(),
    );
    assert_one_error_in_mml_line(
        "_-49 c",
        6,
        BytecodeError::TransposedNoteOverflow(note("c4"), -49..=-49).into(),
    );

    assert_one_error_in_mml_line(
        "[__+10 c]10",
        8,
        BytecodeError::TransposedNoteOverflow(note("c4"), 10..=100).into(),
    );

    assert_one_error_in_mml_line(
        "_+12 [__-8 c]12",
        12,
        BytecodeError::TransposedNoteOverflow(note("c4"), -84..=4).into(),
    );

    assert_one_error_in_mml_line(
        "[ c [[__+3 c : __+5]7]2 : [ __-6 r ]17 r ]50 r __-102 c",
        12,
        BytecodeError::TransposedNoteOverflow(note("c4"), 3..=102).into(),
    );
    assert_one_error_in_mml_line(
        "[ c [[__+3 r : __+5]7]2 : [ __-6 c ]17 r ]50 r __-102 c",
        34,
        BytecodeError::TransposedNoteOverflow(note("c4"), 0..=96).into(),
    );
    assert_one_error_in_mml_line(
        "[ c [[__+3 r : __+5]7]2 : [ __-6 r ]17 r ]50 c __-102 c",
        46,
        BytecodeError::TransposedNoteOverflow(note("c4"), 102..=102).into(),
    );
}

#[test]
fn transposed_note_overflow_error_in_subroutine() {
    // Instrument is known, transpose is not known
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s @1 c

A _+6 !s _+80 !s _-60 !s
"##,
        "!s",
        7,
        BytecodeError::TransposedNoteOverflow(note("c4"), -60..=80).into(),
    );

    // Instrument is known, transpose is not known
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument
@2 dummy_instrument_2

!s0 @1 c
!s1 @2 !s0 r

A _+6 !s1 _+80 !s1 _-60 !s1
"##,
        "!s0",
        8,
        BytecodeError::TransposedNoteOverflow(note("c4"), -60..=80).into(),
    );
}

#[test]
fn looping_song_ends_with_transpose_state_adjust0_bugfix() {
    // Bug: A looping song channel that ends with Adjust(0) transpose state will erroneously
    // output a transpose error.

    assert_looping_line_matches_bytecode(
        "c L __-12 c __+12 c",
        &[
            "play_note c4 24",
            "adjust_transpose -12",
            "play_note c4 24",
            "adjust_transpose +12",
            "play_note c4 24",
        ],
    );

    assert_looping_line_matches_bytecode(
        "c L [__-12 c]2 [__+6 c]4 c",
        &[
            "play_note c4 24",
            "start_loop",
            "adjust_transpose -12",
            "play_note c4 24",
            "end_loop 2",
            "start_loop",
            "adjust_transpose +6",
            "play_note c4 24",
            "end_loop 4",
            "play_note c4 24",
        ],
    );
}
