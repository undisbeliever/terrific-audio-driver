// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

const INSTRUMENT_DEFINITIONS: &str = r##"
@12  f1000_o2
@14  f1000_o4
@15  f1000_o5
@135 f1000_o3_o5
@24  f1000_o4
@235 f2000_o3_o5
@34  f3000_o4
"##;

fn assert_nr_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = format!("{INSTRUMENT_DEFINITIONS}\nA {mml_line}");

    assert_mml_channel_a_matches_bytecode(&mml, bc_asm);
}

fn assert_one_error_in_nr_line(mml_line: &str, line_char: u32, expected_error: ChannelError) {
    let mml = format!("{INSTRUMENT_DEFINITIONS}\nA {mml_line}");

    assert_one_error_in_channel_a_mml(&mml, line_char + 2, expected_error);
}

#[test]
fn out_of_range_note_test() {
    assert_one_error_in_nr_line(
        "@14 o4c o5c o4c",
        11,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 o4c o5c @15 o5c",
        11,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 o4c @15 o5c o4c",
        19,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@235 o4c o5c @24 o5c",
        20,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );
}

#[test]
fn valid_note_range_in_loop() {
    assert_nr_line_matches_bytecode(
        "@14 [o4c @235]2 o3c o4c o5c",
        &[
            "set_instrument f1000_o4",
            "start_loop",
            "play_note c4 24",
            "set_instrument f2000_o3_o5",
            "end_loop 2",
            "play_note c3 24",
            "play_note c4 24",
            "play_note c5 24",
        ],
    );

    assert_nr_line_matches_bytecode(
        "@235 [o4c @14]2 o4c",
        &[
            "set_instrument f2000_o3_o5",
            "start_loop",
            "play_note c4 24",
            "set_instrument f1000_o4",
            "end_loop 2",
            "play_note c4 24",
        ],
    );

    assert_nr_line_matches_bytecode(
        "[@15 o5c]2 o5c",
        &[
            "start_loop",
            "set_instrument f1000_o5",
            "play_note c5 24",
            "end_loop 2",
            "play_note c5 24",
        ],
    );

    assert_nr_line_matches_bytecode(
        "@14 [o4c @15 o5c : @14 o4c]2 o5c",
        &[
            "set_instrument f1000_o4",
            "start_loop",
            "play_note c4 24",
            "set_instrument f1000_o5",
            "play_note c5 24",
            "skip_last_loop",
            "set_instrument f1000_o4",
            "play_note c4 24",
            "end_loop 2",
            "play_note c5 24",
        ],
    );
}

#[test]
fn out_of_range_notes_after_loop() {
    assert_one_error_in_nr_line(
        "@14 [o4c @15 o5c @14 o4c]2 o5c",
        30,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 [o4c @15 o5c : @14 o4c]2 o4c",
        32,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 [o4c @235 o3c o4c o5c]2 o2c o3c o4c o5c",
        31,
        BytecodeError::NoteOutOfRange(note("c2"), note("c3")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@15 [o5c [@34 o4c]2 @135 o5c]2 o6c",
        34,
        BytecodeError::NoteOutOfRange(note("c6"), note("c3")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o4c : @24 o4c]2 o3c o4c",
        25,
        BytecodeError::NoteOutOfRange(note("c3"), note("c4")..=note("b4")).into(),
    );
}

#[test]
fn out_of_range_notes_inside_loop() {
    assert_one_error_in_nr_line(
        "@15 [o5c o3c @235 o3c o4c o5c]2 o3c o4c o5c",
        12,
        BytecodeError::NoteOutOfRange(note("c3"), note("c5")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o4c o3c @24 o4c]2 o4c",
        13,
        BytecodeError::NoteOutOfRange(note("c3"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o4c o3c : @24 o4c]2 o4c",
        13,
        BytecodeError::NoteOutOfRange(note("c3"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o4c o3c : @24 o4c]2 o4c",
        13,
        BytecodeError::NoteOutOfRange(note("c3"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@15 [o5c @235 o6c : @15 o5c]2 o3c o4c o5c",
        17,
        BytecodeError::NoteOutOfRange(note("c6"), note("c3")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o5c [o4c : @235 o3c o4c o5c]2 @15 o5c]3 o5c",
        14,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o5c [o4c @14 o4c : @235 o3c o4c o5c]2 o4c @15 o5c]3 o5c",
        14,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );
}

#[test]
fn all_loop_instruments_out_of_range() {
    assert_one_error_in_nr_line(
        "@14 [o4c @15 o5c]2 o5c",
        8,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c4")).into(),
    );

    assert_one_error_in_nr_line(
        "@15 [o5c @135 o4c : @14 o4c]2 o3c o4c o5c",
        8,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c5")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 [o4c [@14 o4c @15 o5c]2 ]2 o5c",
        8,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c4")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 [o4c [o4c @14 o4c : @15 o5c]2 ]2 o4c",
        13,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c4")).into(),
    );

    assert_one_error_in_nr_line(
        "@135 [o5c [o5c [o4c : @12 o2c]2 @15 o5c]3 @235]4 o3c o4c o5c",
        19,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c4")).into(),
    );
}

#[test]
fn song_loop() {
    assert_one_error_in_nr_line(
        "@135 o3c L o4c o5c @15 o5c",
        14,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 L o4c o5c @135 o3c o4c o5c",
        13,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );

    assert_one_error_in_nr_line(
        "@14 L o4c @15 o5c",
        9,
        BytecodeError::NoteOutOfRangeEmptyNoteRange(note("c4")).into(),
    );
}

#[test]
fn subroutine() {
    assert_one_channel_error_in_mml(
        r##"
@14  f1000_o4
@15  f1000_o5

!s @14 o4c

A @15 !s o4c o5c o4c
"##,
        "A",
        16,
        BytecodeError::NoteOutOfRange(note("c5"), note("c4")..=note("b4")).into(),
    );

    assert_one_channel_error_in_mml(
        r##"
@135  f1000_o3_o5
@15   f1000_o5

!s @15 o5c

A @135 o3c o4c o5c [o5c o4c !s o5c]2 o5c
"##,
        "A",
        27,
        BytecodeError::NoteOutOfRange(note("c4"), note("c5")..=note("b5")).into(),
    );
}
