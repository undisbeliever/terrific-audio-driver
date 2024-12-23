// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn loops() {
    assert_line_matches_bytecode("[a]4", &["start_loop 4", "play_note a4 24", "end_loop"]);

    assert_line_matches_bytecode(
        "[ab:c]256",
        &[
            "start_loop 256",
            "play_note a4 24",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop",
        ],
    );

    assert_line_matches_bytecode(
        "[a [b]2 c]4",
        &[
            "start_loop",
            "play_note a4 24",
            "start_loop",
            "play_note b4 24",
            "end_loop 2",
            "play_note c4 24",
            "end_loop 4",
        ],
    );

    assert_line_matches_bytecode(
        "[a [b]2 : c]4",
        &[
            "start_loop",
            "play_note a4 24",
            "start_loop",
            "play_note b4 24",
            "end_loop 2",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 4",
        ],
    );

    assert_line_matches_bytecode(
        "[[[a]7]8]9",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 7",
            "end_loop 8",
            "end_loop 9",
        ],
    );
}

#[test]
fn no_tick_instructions_allowed_with_skip_last_loop() {
    assert_line_matches_bytecode(
        "[a : V-5 ]6",
        &[
            "start_loop",
            "play_note a4 24",
            "skip_last_loop",
            "adjust_volume -5",
            "end_loop 6",
        ],
    );

    assert_line_matches_bytecode(
        "[V+5 : b ]6",
        &[
            "start_loop",
            "adjust_volume +5",
            "skip_last_loop",
            "play_note b4 24",
            "end_loop 6",
        ],
    );
}

#[test]
fn note_range_after_skip_last_loop_bugfix() {
    assert_one_error_in_channel_a_mml(
        r##"
@d dummy_instrument
@oof only_octave_four

A [ @oof c : @d d]2 o6 e
"##,
        24,
        BytecodeError::NoteOutOfRange(note("e6"), note("c4")..=note("b4")).into(),
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@d dummy_instrument
@oof only_octave_four

A [ @d c : @oof d]2 o6 e
"##,
        &[
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "skip_last_loop",
            "set_instrument only_octave_four",
            "play_note d4 24",
            "end_loop 2",
            "play_note e6 24",
        ],
    );
}

#[test]
fn loop_errors() {
    assert_one_error_in_mml_line("[ ]3", 3, BytecodeError::NoTicksInLoop.into());
    assert_one_error_in_mml_line("[ V+5 ]3", 7, BytecodeError::NoTicksInLoop.into());
    assert_one_error_in_mml_line("[ V+5 : V-5 ]3", 13, BytecodeError::NoTicksInLoop.into());

    assert_one_error_in_mml_line(
        "[ : c ]3",
        3,
        BytecodeError::NoInstructionsBeforeSkipLastLoop.into(),
    );

    assert_one_error_in_mml_line(
        "[ c : ]3",
        7,
        BytecodeError::NoInstructionsAfterSkipLastLoop.into(),
    );
}

#[test]
fn max_loops() {
    assert_line_matches_bytecode(
        "[[[[[[[a]11]12]13]14]15]16]17",
        &[
            "start_loop 17",
            "start_loop 16",
            "start_loop 15",
            "start_loop 14",
            "start_loop 13",
            "start_loop 12",
            "start_loop 11",
            "play_note a4 24",
            "end_loop",
            "end_loop",
            "end_loop",
            "end_loop",
            "end_loop",
            "end_loop",
            "end_loop",
        ],
    );
}

#[test]
fn too_many_loops() {
    assert_one_error_in_mml_line(
        "[[[[[[[[a]11]12]13]14]15]16]17]18",
        8,
        ChannelError::BytecodeError(BytecodeError::StackOverflowInStartLoop(8 * 3)),
    );
}

// Test that a note out of range error does not emit a NoTicksInLoop nor NoTicksAfterLoopPoint error.
#[test]
fn only_one_error_for_out_of_range_note_in_loop() {
    assert_one_error_in_mml_line(
        "[ o7 a ]2",
        6,
        BytecodeError::NoteOutOfRange(note("a7"), note("c2")..=note("b6")).into(),
    );

    assert_one_error_in_mml_line(
        "[ c : o7 a ]2",
        10,
        BytecodeError::NoteOutOfRange(note("a7"), note("c2")..=note("b6")).into(),
    );
}
