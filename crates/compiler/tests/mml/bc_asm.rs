// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn test_bc_asm_in_mml_oneline() {
    assert_line_matches_bytecode(
        r"c \asm { play_note d4 10 | play_note e4 20 } f g",
        &[
            "play_note c4 24",
            "play_note d4 10",
            "play_note e4 20",
            "play_note f4 24",
            "play_note g4 24",
        ],
    );
}

#[test]
fn test_bc_asm_in_mml_multiline() {
    assert_mml_channel_a_matches_bytecode(
        r###"
A \asm {
    set_instrument dummy_instrument ; comment 1
    play_note a4 no_keyoff 5
} b
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 5",
            "play_note b4 24",
        ],
    );
}

#[test]
fn test_multiline_asm_then_comments() {
    assert_mml_channel_a_matches_bytecode(
        r###"
A \asm {
    set_instrument dummy_instrument ; comment 1
    play_note a4 no_keyoff 5
} b ; comment afer \asm line
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 5",
            "play_note b4 24",
        ],
    );
}

#[test]
fn test_bc_asm_call_subroutine_in_subroutine() {
    let sd = compile_mml(
        r##"
!s \asm { call_subroutine    t } r ; No tail call optimisation
!t r

A !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();

    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 2);
    assert_eq!(s.subroutine_id.tick_counter().value(), 24 * 2);

    assert_eq!(
        bc,
        &[
            opcodes::CALL_SUBROUTINE,
            0,
            opcodes::REST,
            23,
            opcodes::RETURN_FROM_SUBROUTINE
        ]
    );
}

/// Testing if loops in an `\asm` block are self-contained breaks tail-call-optimsation
/// on subroutines that end with a subroutine call in an `\asm` block.
//
// ::TODO find a way to add tail call optimsation to subrotuines that end in `\asm { call_subroutine <name> }`::
#[test]
fn test_bc_asm_no_tail_call_optimsation() {
    let sd = compile_mml(
        r##"
!s \asm { call_subroutine    t }
!t r

A !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();

    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 2);
    assert_eq!(s.subroutine_id.tick_counter().value(), 24);

    assert_eq!(
        bc,
        [opcodes::CALL_SUBROUTINE, 0, opcodes::RETURN_FROM_SUBROUTINE]
    );
}

#[test]
fn test_bc_asm_call_subroutine_and_disable_vibrato_in_subroutine() {
    let sd = compile_mml(
        r##"
!s \asm { call_subroutine_and_disable_vibrato    t }
!t r

A !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();

    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 2);
    assert_eq!(s.subroutine_id.tick_counter().value(), 24);

    assert_eq!(
        bc,
        &[
            // Not a tail call
            opcodes::CALL_SUBROUTINE_AND_DISABLE_VIBRATO,
            0,
            opcodes::RETURN_FROM_SUBROUTINE
        ]
    );
}

#[test]
fn test_bc_asm_in_mml_loop() {
    assert_line_matches_bytecode(
        r"[ \asm { play_note d4 10 | play_note e4 20 } ]5",
        &[
            "start_loop 5",
            "play_note d4 10",
            "play_note e4 20",
            "end_loop",
        ],
    );
}

#[test]
fn test_bc_asm_loop() {
    assert_line_matches_bytecode(
        r"c \asm { start_loop 3 | play_note d5 10 | end_loop } e",
        &[
            "play_note c4 24",
            "start_loop 3",
            "play_note d5 10",
            "end_loop",
            "play_note e4 24",
        ],
    );
}

#[test]
fn test_bc_asm_skip_last_loop() {
    assert_line_matches_bytecode(
        r"c \asm { start_loop 3 | play_note d5 10 | skip_last_loop | play_note e5 20 | end_loop } f",
        &[
            "play_note c4 24",
            "start_loop 3",
            "play_note d5 10",
            "skip_last_loop",
            "play_note e5 20",
            "end_loop",
            "play_note f4 24",
        ],
    );
}

#[test]
fn test_missing_end_loop_in_bc_asm_err() {
    assert_error_in_mml_line(
        r"\asm{start_loop} a ]2",
        16,
        BytecodeError::MissingEndLoopInAsmBlock.into(),
    );
}

#[test]
fn test_missing_start_loop_in_bc_asm_err() {
    assert_error_in_mml_line(
        r"[ a \asm{ end_loop 2}",
        11,
        BytecodeError::MissingStartLoopInAsmBlock.into(),
    );
}

#[test]
fn test_skip_last_loop_outside_asm_loop_err() {
    assert_error_in_mml_line(
        r"[ c \asm{ skip_last_loop } c ]2",
        11,
        BytecodeError::CannotModifyLoopOutsideAsmBlock.into(),
    );
}

#[test]
fn test_mml_bc_asm_repeated_channel_is_only_processed_once() {
    assert_mml_channel_a_matches_bytecode(
        r###"
@0 dummy_instrument

AAAAAAAAAAA \asm { set_instrument dummy_instrument | play_note a4 24 }
"###,
        &["set_instrument dummy_instrument", "play_note a4 24"],
    );
}

/// Test the bytecode is repeated 4 times if there are 4 different channels on a single MML line
#[test]
fn test_mml_bc_asm_with_multiple_channels_on_one_line() {
    let mml = r###"
@0 dummy_instrument

ADEF \asm { set_instrument dummy_instrument | play_note a4 24 }
"###;
    let bc_asm = &["set_instrument dummy_instrument", "play_note a4 24"];

    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data.instruments_and_samples,
        mml.subroutines(),
        BcTerminator::DisableChannel,
        BytecodeContext::SongChannel(0),
    )
    .repeat(4);

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

#[test]
fn test_set_instrument_after_bc_asm() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 \asm { set_instrument dummy_instrument} b @1 c
"##,
        &[
            "set_instrument dummy_instrument",
            // Not optimised out
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "set_instrument dummy_instrument_2",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

A \asm { set_instrument dummy_instrument } b @0 c
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note b4 24",
            // `set_instrument` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn test_adsr_after_bc_asm() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

A @0 \asm { set_adsr 2 2 2 2 } b A2,2,2,2 c
"##,
        &[
            "set_instrument dummy_instrument",
            "set_adsr 2 2 2 2",
            "play_note b4 24",
            // set_adsr optimised out
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
A \asm { set_instrument_and_adsr dummy_instrument 2 2 2 2 } b A2,2,2,2 c
"##,
        &[
            "set_instrument_and_adsr dummy_instrument 2 2 2 2",
            "play_note b4 24",
            // set_adsr optimised out
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument adsr 2 2 2 2

A @0 \asm { set_adsr 2 2 2 2 } b A3,3,3,3 c
"##,
        &[
            "set_instrument_and_adsr dummy_instrument 2 2 2 2",
            // Not optimised out
            "set_adsr 2 2 2 2",
            "play_note b4 24",
            "set_adsr 3 3 3 3",
            "play_note c4 24",
        ],
    );
}

#[test]
fn test_gain_after_bc_asm() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

A @0 \asm { set_gain F127 } b GF127 c
"##,
        &[
            "set_instrument dummy_instrument",
            "set_gain F127",
            "play_note b4 24",
            // set_gain optimised out
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
A \asm { set_instrument_and_gain dummy_instrument I30 } b GI30 c
"##,
        &[
            "set_instrument_and_gain dummy_instrument I30",
            "play_note b4 24",
            // set_gain optimised out
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument gain 10

A @0 \asm { set_gain 10 } b G20 c
"##,
        &[
            "set_instrument_and_gain dummy_instrument 10",
            // Not optimised out
            "set_gain 10",
            "play_note b4 24",
            "set_gain 20",
            "play_note c4 24",
        ],
    );
}

#[test]
fn test_prev_slurred_note_after_bc_asm() {
    // see `test_skip_last_loop_prev_slurred_note()`

    assert_line_matches_bytecode(
        r"\asm { play_note g4 no_keyoff 24 } {ab},,10",
        &[
            "play_note g4 no_keyoff 24",
            // Previous note is g4 and NOT slurred
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
        ],
    );

    assert_line_matches_bytecode(
        r"\asm { play_note a4 no_keyoff 24 } {ab},,10",
        &[
            "play_note a4 no_keyoff 24",
            // Previous slurred note is a4
            "portamento b4 keyoff +10 24",
        ],
    );
}

#[test]
fn test_hexadecimal_in_mml_bc_asm() {
    assert_mml_channel_a_matches_bytecode(
        r##"
A \asm {
    set_instrument dummy_instrument
    set_vibrato $f $2
    set_volume $5A
    set_pan $40

    adjust_volume -$14
    adjust_pan +$14

    play_note d5 no_keyoff $30
    set_temp_gain_and_wait D6 $18
    play_note e5 no_keyoff $48
    set_temp_gain I6
    play_note d5 no_keyoff $30
    set_temp_gain_and_rest E18 $48
}"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 15 2",
            "set_volume 90",
            "set_pan 64",
            "adjust_volume -20",
            "adjust_pan +20",
            "play_note d5 no_keyoff 48",
            "set_temp_gain_and_wait D6 24",
            "play_note e5 no_keyoff 72",
            "set_temp_gain I6",
            "play_note d5 no_keyoff 48",
            "set_temp_gain_and_rest E18 72",
        ],
    );
}
