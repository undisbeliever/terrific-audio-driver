// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn rest() {
    // The last rest in a wait-rest chain must be 257 ticks to prevent interference with early-release

    assert_line_matches_bytecode("r", &["rest 24"]);
    assert_line_matches_bytecode("r.", &["rest 36"]);
    assert_line_matches_bytecode("r8", &["rest 12"]);
    assert_line_matches_bytecode("r8.", &["rest 18"]);

    assert_line_matches_bytecode("r%30", &["rest 30"]);

    assert_line_matches_bytecode("r%256", &["rest 256"]);
    assert_line_matches_bytecode("r%257", &["rest 257"]);
    assert_line_matches_bytecode("r%258", &["rest 258"]);
    assert_line_matches_bytecode("r%512", &["rest 512"]);
    assert_line_matches_bytecode("r%513", &["rest 513"]);
    assert_line_matches_bytecode("r%514", &["rest 514"]);
    assert_line_matches_bytecode("r%600", &["rest 600"]);

    // User expects a keyoff after the first rest command
    merge_mml_commands_test("r || r", &["rest 24", "rest 24"]);
    merge_mml_commands_test("r || r8", &["rest 24", "rest 12"]);
    merge_mml_commands_test("r%30||r%20", &["rest 30", "rest 20"]);

    merge_mml_commands_test("r r || r", &["rest 24", "rest 48"]);
    merge_mml_commands_test("r r || r8", &["rest 24", "rest 36"]);
    merge_mml_commands_test("r%30 r%30||r%20", &["rest 30", "rest 50"]);

    assert_line_matches_bytecode("r%300 r%256", &["rest 300", "rest 256"]);

    // It does not matter if subsequent rests send keyoff events or not, no note is playing
    merge_mml_commands_test("r%300 || r%300", &["rest 300", "rest 300"]);
    assert_line_matches_bytecode("r%300 r%100 r%100 r%100", &["rest 300", "rest 300"]);

    // From `mml-syntax.md`
    assert_line_matches_line("r4 r8 r8", "r4 r4");
}

#[test]
fn rest_tie() {
    // used by midi2smw.

    assert_line_matches_bytecode("r^^^", &["rest 96"]);

    assert_line_matches_bytecode("r^8", &["rest 36"]);
    assert_line_matches_bytecode("r8^", &["rest 36"]);
    assert_line_matches_bytecode("r8^16", &["rest 18"]);

    assert_line_matches_bytecode("r2^4^8", &["rest 84"]);

    assert_line_matches_bytecode("r%5^%7", &["rest 12"]);

    merge_mml_commands_test("r || ^ 8", &["rest 36"]);
}

#[test]
fn wait() {
    assert_line_matches_bytecode("w", &["wait 24"]);
    assert_line_matches_bytecode("w.", &["wait 36"]);
    assert_line_matches_bytecode("w8", &["wait 12"]);
    assert_line_matches_bytecode("w8.", &["wait 18"]);

    assert_line_matches_bytecode("w%30", &["wait 30"]);

    assert_line_matches_bytecode("w%256", &["wait 256"]);
    assert_line_matches_bytecode("w%257", &["wait 257"]);
    assert_line_matches_bytecode("w%512", &["wait 512"]);
    assert_line_matches_bytecode("w%600", &["wait 600"]);

    merge_mml_commands_test("w || w", &["wait 48"]);
    merge_mml_commands_test("w || w8", &["wait 36"]);
    merge_mml_commands_test("w%30||w%20", &["wait 50"]);

    merge_mml_commands_test("w%300 || w%400 w%500", &["wait 1200"]);
    merge_mml_commands_test("w%300 w%400 || w%500", &["wait 1200"]);

    // From `mml-syntax.md`
    assert_line_matches_line("w4 w8 w8", "w2");
}

#[test]
fn wait_tie() {
    assert_line_matches_bytecode("w^^^", &["wait 96"]);

    assert_line_matches_bytecode("w^8", &["wait 36"]);
    assert_line_matches_bytecode("w8^", &["wait 36"]);
    assert_line_matches_bytecode("w8^16", &["wait 18"]);

    assert_line_matches_bytecode("w2^4^8", &["wait 84"]);

    assert_line_matches_bytecode("w%5^%7", &["wait 12"]);

    merge_mml_commands_test("w || ^ 8", &["wait 36"]);
}

#[test]
fn large_wait() {
    // Test wait tick-counter threashold
    assert_line_matches_bytecode("w%768", &["wait 768"]);
    assert_line_matches_bytecode("w%769", &["wait 769"]);

    assert_line_matches_bytecode("w%25600", &["wait 25600"]);
    assert_line_matches_bytecode("w%25601", &["wait 25601"]);

    assert_line_matches_bytecode("w%$ffff", &["wait 65535"]);

    // A random prime number
    assert_line_matches_bytecode("w%7039", &["wait 7039"]);

    // Test no compile errors when loop stack is full
    assert_line_matches_bytecode(
        "[[[[[[[ w%1024 ]2]3]4]5]6]7]8",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "wait 1024",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    assert_line_matches_line("w1 w1 w1 w1 w1 w1 w1 w1 w1 w1", "w%960");
}

#[test]
fn large_rest() {
    // Test rest tick-counter threashold
    assert_line_matches_bytecode("r%1024", &["rest 1024"]);
    assert_line_matches_bytecode("r%1025", &["rest 1025"]);

    assert_line_matches_bytecode("r%1026", &["rest 1026"]);
    assert_line_matches_bytecode("r%1277", &["rest 1277"]);

    assert_line_matches_bytecode("r%25809", &["rest 25809"]);
    assert_line_matches_bytecode("r%25810", &["rest 25810"]);
    assert_line_matches_bytecode("r%25811", &["rest 25811"]);
    assert_line_matches_bytecode("r%25858", &["rest 25858"]);

    assert_line_matches_bytecode("r%$ffff", &["rest 65535"]);

    assert_line_matches_bytecode("r%5494", &["rest 5494"]);

    assert_line_matches_bytecode(
        "[[[[[[[ r%1281 ]2]3]4]5]6]7]8",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "rest 1281",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );
}

#[test]
fn merged_large_rests() {
    // Test rest tick-counter threashold
    assert_line_matches_bytecode("r%2 r%771", &["rest 2", "rest 771"]);
    assert_line_matches_bytecode("r%2 r%772", &["rest 2", "rest 772"]);

    assert_line_matches_bytecode("r%2 r%1028", &["rest 2", "rest 1028"]);
    assert_line_matches_bytecode("r%2 r%1286", &["rest 2", "rest 1286"]);

    assert_line_matches_bytecode("r%2 r%25700", &["rest 2", "rest 25700"]);
    assert_line_matches_bytecode("r%2 r%25701", &["rest 2", "rest 25701"]);
    assert_line_matches_bytecode("r%2 r%25702", &["rest 2", "rest 25702"]);

    assert_line_matches_bytecode("r%2 r%$ffff", &["rest 2", "rest 65535"]);

    // A random prime number
    assert_line_matches_bytecode("r%2 r%6553", &["rest 2", "rest 6553"]);

    merge_mml_commands_test("r%300 || r%400 r%500", &["rest 300", "rest 900"]);
    merge_mml_commands_test("r%300 r%400 || r%500", &["rest 300", "rest 900"]);

    assert_line_matches_bytecode(
        "[[[[[[[ r%2 r%1028 ]2]3]4]5]6]7]8",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "rest 2",
            "rest 1028",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    assert_line_matches_bytecode(
        "[[[[[[[ r%1537 r%1028 ]2]3]4]5]6]7]8",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "rest 1537",
            "rest 1028",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    assert_line_matches_bytecode("r%2000 r%1000", &["rest 2000", "rest 1000"]);

    assert_line_matches_line("r1 r1 r1 r1 r1 r1 r1 r1 r1 r1", "r1 r%864")
}

#[test]
fn rest_after_keyoff_note() {
    assert_line_matches_bytecode("a r", &["play_note a4 24", "rest 24"]);
    assert_line_matches_bytecode("a r r", &["play_note a4 24", "rest 48"]);
    assert_line_matches_bytecode("a w r", &["play_note a4 24", "wait 24", "rest 24"]);

    merge_mml_commands_test("a r8 || ^8^8", &["play_note a4 24", "rest 36"]);

    assert_line_matches_bytecode(
        "a2 b3 r8 r16 c32",
        &[
            "play_note a4 48",
            "play_note b4 32",
            "rest 18",
            "play_note c4 3",
        ],
    );

    assert_line_matches_bytecode("a r%1", &["play_note a4 24", "wait 1"]);
    assert_line_matches_bytecode("a r%2", &["play_note a4 24", "rest 2"]);

    assert_line_matches_bytecode("a%50 r%500", &["play_note a4 50", "rest 500"]);
    assert_line_matches_bytecode("a%600 r%600", &["play_note a4 keyoff 600", "rest 600"]);

    merge_mml_commands_test("a%300 || r%400 r%500", &["play_note a4 300", "rest 900"]);
    merge_mml_commands_test("a%300 r%400 || r%500", &["play_note a4 300", "rest 900"]);

    assert_line_matches_bytecode("a%2561 r%2570", &["play_note a4 keyoff 2561", "rest 2570"]);
}

// The rest after a slurred note must not be merged with successive rests
#[test]
fn rest_after_surred_note() {
    assert_line_matches_bytecode("a & r", &["play_note a4 no_keyoff 24", "rest 24"]);

    assert_line_matches_bytecode(
        "a & r r",
        &["play_note a4 no_keyoff 24", "rest 24", "rest 24"],
    );

    merge_mml_commands_test("a & r8 || ^8^8", &["play_note a4 no_keyoff 24", "rest 36"]);

    assert_line_matches_bytecode(
        "a & r%2 r%2",
        &["play_note a4 no_keyoff 24", "rest 2", "rest 2"],
    );

    assert_line_matches_bytecode("a%50 & r%500", &["play_note a4 no_keyoff 50", "rest 500"]);

    assert_line_matches_bytecode(
        "a%600 & r%600 r%600",
        &["play_note a4 no_keyoff 600", "rest 600", "rest 600"],
    );

    assert_line_matches_bytecode(
        "a%2560 & r%2561 r%2561",
        &["play_note a4 no_keyoff 2560", "rest 2561", "rest 2561"],
    );

    merge_mml_commands_test(
        "a%300 & || r%400 r%500",
        &["play_note a4 no_keyoff 300", "rest 400", "rest 500"],
    );
    merge_mml_commands_test(
        "a%300 & r%400 || r%500",
        &["play_note a4 no_keyoff 300", "rest 400", "rest 500"],
    );
}

#[test]
fn merge_rests_newlines() {
    let mml = r##"
@0 dummy_instrument

A @0 r4
A r4
A r4
A r4
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &["set_instrument dummy_instrument", "rest 24", "rest 72"],
    );
}

// Found with cargo-fuzz.
// This line takes 165ms to compile on my PC for a release build
// and 7.5 seconds for a debug build.
#[test]
fn large_noloop_rest_and_waits_slow_to_compile_bugfix() {
    let dummy_data = dummy_data();

    let start = std::time::Instant::now();

    let _ = compiler::songs::compile_mml_song(
        "ABCDEFGH [[[[[[[[w%999999999 r%999999999",
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let duration = start.elapsed();

    assert!(
        duration.as_millis() < 2,
        "MML compiler too slow {duration:?}"
    );
}

#[test]
fn wait_too_long_errors() {
    assert_one_error_in_mml_line(
        "w%65536",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "w%65536",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "w ^%65536",
        4,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "w%60000 ^%60000",
        10,
        ValueError::CommandTicksOverflow.into(),
    );
}

#[test]
fn rest_too_long_errors() {
    assert_one_error_in_mml_line(
        "r%65536",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "r%65536",
        2,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "r ^%65536",
        4,
        ValueError::CommandTicksOutOfRange(65536).into(),
    );

    assert_one_error_in_mml_line(
        "r r%60000 ^%60000",
        12,
        ValueError::CommandTicksOverflow.into(),
    );
}
