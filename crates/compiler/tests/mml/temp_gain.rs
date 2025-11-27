// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn set_temp_gain() {
    assert_line_matches_bytecode("GFT$12", &["set_temp_gain 18"]);
    assert_line_matches_bytecode("GT0", &["set_temp_gain 0"]);

    assert_line_matches_bytecode("GFT127", &["set_temp_gain 127"]);

    assert_line_matches_bytecode("GDT1", &["set_temp_gain $81"]);
    assert_line_matches_bytecode("GET2", &["set_temp_gain $a2"]);
    assert_line_matches_bytecode("GIT3", &["set_temp_gain $c3"]);
    assert_line_matches_bytecode("GBT4", &["set_temp_gain $e4"]);

    assert_line_matches_bytecode(
        "GIT8 c4 & GDT5 d4 & GBT3 e4 & GET5 f4",
        &[
            "set_temp_gain I8",
            "play_note c4 no_keyoff 24",
            "set_temp_gain D5",
            "play_note d4 no_keyoff 24",
            "set_temp_gain B3",
            "play_note e4 no_keyoff 24",
            "set_temp_gain E5",
            "play_note f4 24",
        ],
    );
}

#[test]
fn gft0_is_error() {
    assert_one_error_in_mml_line("GFT0", 1, ValueError::F0TempGain.into());
}

#[test]
fn set_temp_gain_and_rest() {
    assert_line_matches_bytecode("GDT12 r", &["set_temp_gain_and_rest D12 24"]);

    merge_mml_commands_test("GDT12 || r8", &["set_temp_gain_and_rest D12 12"]);
    merge_mml_commands_test("GDT12 || r8.", &["set_temp_gain_and_rest D12 18"]);
    merge_mml_commands_test("GDT12 r ^ || ^ ^", &["set_temp_gain_and_rest D12 96"]);
    merge_mml_commands_test("GDT12 r || ^1", &["set_temp_gain_and_rest D12 120"]);
    merge_mml_commands_test(
        "GDT12 r || r",
        &["set_temp_gain_and_rest D12 24", "rest 24"],
    );

    assert_line_matches_bytecode("GDT12 r r2.", &["set_temp_gain_and_rest D12 24", "rest 72"]);

    merge_mml_commands_test(
        "GDT12 r r || r r",
        &["set_temp_gain_and_rest D12 24", "rest 72"],
    );

    assert_line_matches_bytecode("GDT12 r%256", &["set_temp_gain_and_rest D12 256"]);
    assert_line_matches_bytecode("GDT12 r%257", &["set_temp_gain_and_rest D12 257"]);
    assert_line_matches_bytecode("GDT12 r%258", &["set_temp_gain_and_rest D12 258"]);
    assert_line_matches_bytecode("GDT12 r%259", &["set_temp_gain_and_rest D12 259"]);

    assert_line_matches_bytecode("GDT12 r%1000", &["set_temp_gain_and_rest D12 1000"]);

    merge_mml_commands_test(
        "GDT12 r%700 r%200 || r%500",
        &["set_temp_gain_and_rest D12 700", "rest 700"],
    );

    assert_line_matches_bytecode(
        "GDT12 r%10000 r%2000",
        &["set_temp_gain_and_rest D12 10000", "rest 2000"],
    );

    assert_line_matches_bytecode(
        "GIT1 c4 & GDT2 w4 GBT3 d4 & GET4 w4",
        &[
            "set_temp_gain I1",
            "play_note c4 no_keyoff 24",
            "set_temp_gain_and_wait D2 24",
            "set_temp_gain B3",
            "play_note d4 no_keyoff 24",
            "set_temp_gain_and_wait E4 24",
        ],
    );
}

#[test]
fn set_temp_gain_and_wait() {
    assert_line_matches_bytecode("GDT12 w", &["set_temp_gain_and_wait D12 24"]);

    merge_mml_commands_test("GDT12 || w8", &["set_temp_gain_and_wait D12 12"]);
    merge_mml_commands_test("GDT12 || w8.", &["set_temp_gain_and_wait D12 18"]);

    merge_mml_commands_test("GDT12 w w || w w", &["set_temp_gain_and_wait D12 96"]);
    merge_mml_commands_test("GDT12 w ^ ^ || w", &["set_temp_gain_and_wait D12 96"]);
    merge_mml_commands_test("GDT12 w || ^1", &["set_temp_gain_and_wait D12 120"]);

    assert_line_matches_bytecode("GDT12 w%256", &["set_temp_gain_and_wait D12 256"]);
    assert_line_matches_bytecode("GDT12 w%257", &["set_temp_gain_and_wait D12 257"]);

    assert_line_matches_bytecode("GDT12 w%1000", &["set_temp_gain_and_wait D12 1000"]);
    assert_line_matches_bytecode("GDT12 w%8000", &["set_temp_gain_and_wait D12 8000"]);

    assert_line_matches_bytecode(
        "GIT1 c4 & GDT2 w4 GBT3 d4 & GET4 w4",
        &[
            "set_temp_gain I1",
            "play_note c4 no_keyoff 24",
            "set_temp_gain_and_wait D2 24",
            "set_temp_gain B3",
            "play_note d4 no_keyoff 24",
            "set_temp_gain_and_wait E4 24",
        ],
    );
}

#[test]
fn reuse_temp_gain() {
    assert_line_matches_bytecode(
        "GT100 a GT b",
        &[
            "set_temp_gain 100",
            "play_note a4 24",
            "reuse_temp_gain",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "GIT20 a GIT20 b",
        &[
            "set_temp_gain I20",
            "play_note a4 24",
            "reuse_temp_gain",
            "play_note b4 24",
        ],
    );
}

#[test]
fn set_gain_does_not_affect_temp_gain_reuse() {
    assert_line_matches_bytecode(
        "GIT10 c GI20 d GIT10 e",
        &[
            "set_temp_gain I10",
            "play_note c4 24",
            "set_gain I20",
            "play_note d4 24",
            "reuse_temp_gain",
            "play_note e4 24",
        ],
    );
}

#[test]
fn reuse_temp_gain_and_wait() {
    assert_line_matches_bytecode(
        "a & GT100 w | b & GT w",
        &[
            "play_note a4 no_keyoff 24",
            "set_temp_gain_and_wait 100 24",
            "play_note b4 no_keyoff 24",
            "reuse_temp_gain_and_wait 24",
        ],
    );

    assert_line_matches_bytecode(
        "a & GDT10 w | b & GDT10 w",
        &[
            "play_note a4 no_keyoff 24",
            "set_temp_gain_and_wait D10 24",
            "play_note b4 no_keyoff 24",
            "reuse_temp_gain_and_wait 24",
        ],
    );
}

#[test]
fn reuse_temp_gain_and_rest() {
    assert_line_matches_bytecode(
        "a & GT100 r | b & GT r",
        &[
            "play_note a4 no_keyoff 24",
            "set_temp_gain_and_rest 100 24",
            "play_note b4 no_keyoff 24",
            "reuse_temp_gain_and_rest 24",
        ],
    );

    assert_line_matches_bytecode(
        "a & GDT10 r | b & GDT10 r",
        &[
            "play_note a4 no_keyoff 24",
            "set_temp_gain_and_rest D10 24",
            "play_note b4 no_keyoff 24",
            "reuse_temp_gain_and_rest 24",
        ],
    );
}

#[test]
fn reuse_temp_gain_after_loop() {
    assert_line_matches_bytecode(
        "[GIT10 c : GIT20 d]3 GIT10 e",
        &[
            "start_loop",
            "set_temp_gain I10",
            "play_note c4 24",
            "skip_last_loop",
            "set_temp_gain I20",
            "play_note d4 24",
            "end_loop 3",
            "reuse_temp_gain",
            "play_note e4 24",
        ],
    );

    assert_line_matches_bytecode(
        "[GIT10 c : GIT20 d]3 GIT20 e",
        &[
            "start_loop",
            "set_temp_gain I10",
            "play_note c4 24",
            "skip_last_loop",
            "set_temp_gain I20",
            "play_note d4 24",
            "end_loop 3",
            // After the skip-last-loop, temp-GAIN is GI10
            "set_temp_gain I20",
            "play_note e4 24",
        ],
    );

    assert_line_matches_bytecode(
        "[GIT10 c GIT20 d]3 GIT20 e",
        &[
            "start_loop",
            "set_temp_gain I10",
            "play_note c4 24",
            "set_temp_gain I20",
            "play_note d4 24",
            "end_loop 3",
            "reuse_temp_gain",
            "play_note e4 24",
        ],
    );

    // prev temp GAIN is unknown at the start of a loop
    assert_line_matches_bytecode(
        "GT10 [GT10 c]3 GT10 d",
        &[
            "set_temp_gain 10",
            "start_loop",
            "set_temp_gain 10",
            "play_note c4 24",
            "end_loop 3",
            "reuse_temp_gain",
            "play_note d4 24",
        ],
    );
}

#[test]
fn reuse_temp_gain_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s GT20 b

A @1 GT10 !s GT20 b
"##,
        &[
            "set_instrument dummy_instrument",
            "set_temp_gain 10",
            "call_subroutine s",
            "reuse_temp_gain",
            "play_note b4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s GT20 b

A @1 GT10 !s GT10 b
"##,
        &[
            "set_instrument dummy_instrument",
            "set_temp_gain 10",
            "call_subroutine s",
            "set_temp_gain 10",
            "play_note b4 24",
        ],
    );
}
