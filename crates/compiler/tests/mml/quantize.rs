// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn coarse_quantization() {
    // Cannot use `assert_mml_matches_mml`.
    // There is a single rest tick at the end of a play_note instruction
    assert_line_matches_bytecode("Q1 c%80", &["play_note c4 11", "rest 69"]);
    assert_line_matches_bytecode("Q2 c%80", &["play_note c4 21", "rest 59"]);
    assert_line_matches_bytecode("Q3 c%80", &["play_note c4 31", "rest 49"]);
    assert_line_matches_bytecode("Q4 c%80", &["play_note c4 41", "rest 39"]);
    assert_line_matches_bytecode("Q5 c%80", &["play_note c4 51", "rest 29"]);
    assert_line_matches_bytecode("Q6 c%80", &["play_note c4 61", "rest 19"]);
    assert_line_matches_bytecode("Q7 c%80", &["play_note c4 71", "rest  9"]);
    assert_line_matches_bytecode("Q8 c%80", &["play_note c4 80"]);

    assert_line_matches_bytecode("Q4 c", &["play_note c4 13", "rest 11"]);

    merge_mml_commands_test("Q4 || c%100 r%100", &["play_note c4 51", "rest 149"]);
    merge_mml_commands_test("Q4 c%100 || r%100", &["play_note c4 51", "rest 149"]);

    // Test with tie
    merge_mml_commands_test("Q4 c%100 || ^ %100", &["play_note c4 101", "rest 99"]);
    merge_mml_commands_test("Q4 c%100 || & %100", &["play_note c4 101", "rest 99"]);

    // Test with tie and rest
    merge_mml_commands_test(
        "Q2 c%50 ^%50 || r%50 r%50",
        &["play_note c4 26", "rest 174"],
    );
    merge_mml_commands_test(
        "Q6 c%70 & %30 || r%50 r%50",
        &["play_note c4 76", "rest 124"],
    );
    merge_mml_commands_test(
        "Q6 c%70 & %30 || r%50 r%50 r%257",
        &["play_note c4 76", "rest 257", "rest 124"],
    );

    assert_line_matches_bytecode(
        "Q4 c%70 r%600",
        &["play_note c4 36", "rest 257", "rest 257", "rest 120"],
    );

    assert_line_matches_bytecode(
        "Q4 c Q8 d Q6 e",
        &[
            "play_note c4 13",
            "rest 11",
            "play_note d4 24",
            "play_note e4 19",
            "rest 5",
        ],
    );
}

#[test]
fn fine_quantisation() {
    // Cannot use `assert_mml_matches_mml`.
    // There is a single rest tick at the end of a play_note instruction

    assert_line_matches_bytecode("Q%1  c%80", &["play_note c4 2", "rest 78"]);

    assert_line_matches_bytecode("Q%10 c%80", &["play_note c4 4", "rest 76"]);
    assert_line_matches_bytecode("Q%128 c%80", &["play_note c4 41", "rest 39"]);
    assert_line_matches_bytecode("Q%$c0 c%80", &["play_note c4 61", "rest 19"]);

    assert_line_matches_bytecode("Q%249  c%80", &["play_note c4 78", "rest 2"]);
    assert_line_matches_bytecode("Q%250  c%80", &["play_note c4 79", "wait 1"]);

    assert_line_matches_bytecode("Q%252  c%80", &["play_note c4 79", "wait 1"]);
    assert_line_matches_bytecode("Q%253  c%80", &["play_note c4 80"]);

    assert_line_matches_bytecode("Q%255  c%80", &["play_note c4 80"]);
}

#[test]
fn quantization_of_short_note_then_rest_bugfix() {
    // The rest notes were erroniously dropped and ignored

    assert_line_matches_bytecode("Q8 c%6 r%6", &["play_note c4 6", "rest 6"]);

    assert_line_matches_bytecode("Q%255  c%100 r%100", &["play_note c4 100", "rest 100"]);
}

#[test]
fn quantization_of_1_tick_note_panic_bugfix() {
    assert_error_in_mml_line("Q4 c%1", 4, ChannelError::NoteIsTooShort);
}

#[test]
fn quantize_with_temp_gain() {
    assert_line_matches_line("Q4,$84 c", "c8 & GT$84 r8");

    assert_line_matches_line("Q4,F12 c", "c8 & GFT12 r8");
    assert_line_matches_line("Q4,D3 c", "c8 & GDT3 r8");
    assert_line_matches_line("Q4,E4 c", "c8 & GET4 r8");
    assert_line_matches_line("Q4,I5 c", "c8 & GIT5 r8");
    assert_line_matches_line("Q4,B6 c", "c8 & GBT6 r8");

    assert_line_matches_line("Q%0,$84 c", "c%1 & GT$84 r%23");

    assert_line_matches_line("Q%250,$84 c2", "c%46 & GT$84 r%2");
    assert_line_matches_line("Q%251,$84 c2", "c%48");

    assert_line_matches_line("Q4,D10    c d d f2. r", "Q%128,D10 c d d f2. r");

    assert_line_matches_bytecode(
        "Q6,E4 c d8 e8 f",
        &[
            "play_note c4 slur_next 18",
            "set_temp_gain_and_rest E4 6",
            "play_note d4 slur_next 9",
            "reuse_temp_gain_and_rest 3",
            "play_note e4 slur_next 9",
            "reuse_temp_gain_and_rest 3",
            "play_note f4 slur_next 18",
            "reuse_temp_gain_and_rest 6",
        ],
    );

    assert_line_matches_bytecode("Q1,E4 c%2", &["play_note c4 2"]);

    // From mml-syntax.md
    assert_line_matches_line("Q4,D10 c4", "c8 & GDT10 r8");
}

#[test]
fn quantise_comma_0_gain_is_error() {
    assert_error_in_mml_line("Q2,0", 4, ValueError::OptionalGainCannotBeZero.into());
    assert_error_in_mml_line("Q2,F0", 4, ValueError::OptionalGainCannotBeZero.into());
}

#[test]
fn quantized_portamento() {
    // Only testing portamento with a speed override

    assert_line_matches_bytecode(
        "Q4 {df},,10",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 12",
            "rest 11",
        ],
    );

    assert_line_matches_bytecode(
        "Q4 {df}2,,10",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 24",
            "rest 23",
        ],
    );

    assert_line_matches_bytecode(
        "Q4 d& {df},,10",
        &[
            "play_note d4 no_keyoff 24",
            "portamento f4 keyoff +10 13",
            "rest 11",
        ],
    );

    assert_line_matches_bytecode(
        "Q4 {df},,10 ^",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 24",
            "rest 23",
        ],
    );

    assert_line_matches_bytecode(
        "Q4,D8 {df},,10 ^2",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 no_keyoff +10 35",
            "set_temp_gain_and_rest D8 36",
        ],
    );

    assert_line_matches_bytecode(
        "GDT8 Q4,D8 {df},,10 ^2",
        &[
            "set_temp_gain D8",
            "play_note d4 no_keyoff 1",
            "portamento f4 no_keyoff +10 35",
            "reuse_temp_gain_and_rest 36",
        ],
    );

    assert_line_matches_line_and_bytecode(
        "Q4 {df},,10 & b",
        "{df},,10 & Q4 b",
        &[
            // portamento is not quantized
            "play_note d4 no_keyoff 1",
            "portamento f4 no_keyoff +10 23",
            // play note is quantized
            "play_note b4 13",
            "rest 11",
        ],
    );

    assert_line_matches_line("Q4 {df}4,8", "d8 & Q4 {df}8");
    assert_line_matches_line("Q4 {df}4,8", "Q4 d8 & {df}8");

    assert_line_matches_line_and_bytecode(
        "Q4 {df}4,8,15",
        "Q4 d8 & {df}8,,15",
        &[
            "play_note d4 no_keyoff 12",
            "portamento f4 keyoff +15 7",
            "rest 5",
        ],
    );

    assert_line_matches_line("Q4 {a > c}2", "Q4 {a _+12 c}2");
    assert_line_matches_line("Q4 {o3 c o4 c}2", "Q4 {< c > c}2");

    assert_line_matches_bytecode(
        "Q4 {df},,10 r%90 r%90 ^%90 r%90",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 12",
            // 360 + 11 ticks of rest
            "rest 257",
            "rest 114",
        ],
    );

    assert_line_matches_bytecode(
        "Q4,E15 {df},,10 r%90 r%90 ^%90 r%90",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 no_keyoff +10 11",
            "set_temp_gain_and_rest E15 12",
            // 360 ticks of rest
            "rest 257",
            "rest 103",
        ],
    );
}
