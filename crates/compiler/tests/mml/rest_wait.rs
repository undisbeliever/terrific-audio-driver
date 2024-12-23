// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn test_rests() {
    // The last rest in a wait-rest chain must be 257 ticks to prevent interference with early-release

    assert_line_matches_bytecode("r", &["rest 24"]);
    assert_line_matches_bytecode("r.", &["rest 36"]);
    assert_line_matches_bytecode("r8", &["rest 12"]);
    assert_line_matches_bytecode("r8.", &["rest 18"]);

    assert_line_matches_bytecode("r%30", &["rest 30"]);

    assert_line_matches_bytecode("r%256", &["rest 256"]);
    assert_line_matches_bytecode("r%257", &["rest 257"]);
    assert_line_matches_bytecode("r%258", &["wait 1", "rest 257"]);
    assert_line_matches_bytecode("r%512", &["wait 255", "rest 257"]);
    assert_line_matches_bytecode("r%513", &["wait 256", "rest 257"]);
    assert_line_matches_bytecode("r%514", &["wait 256", "wait 1", "rest 257"]);
    assert_line_matches_bytecode("r%600", &["wait 256", "wait 87", "rest 257"]);

    // User expects a keyoff after the first rest command
    merge_mml_commands_test("r || r", &["rest 24", "rest 24"]);
    merge_mml_commands_test("r || r8", &["rest 24", "rest 12"]);
    merge_mml_commands_test("r%30||r%20", &["rest 30", "rest 20"]);

    merge_mml_commands_test("r r || r", &["rest 24", "rest 48"]);
    merge_mml_commands_test("r r || r8", &["rest 24", "rest 36"]);
    merge_mml_commands_test("r%30 r%30||r%20", &["rest 30", "rest 50"]);

    assert_line_matches_bytecode("r%300 r%256", &["wait 43", "rest 257", "rest 256"]);

    // It does not matter if subsequent rests send keyoff events or not, no note is playing
    merge_mml_commands_test(
        "r%300 || r%300",
        &["wait 43", "rest 257", "rest 257", "rest 43"],
    );
    assert_line_matches_bytecode(
        "r%300 r%100 r%100 r%100",
        &["wait 43", "rest 257", "rest 257", "rest 43"],
    );

    // From `mml-syntax.md`
    assert_line_matches_line("r4 r8 r8", "r4 r4");
}

#[test]
fn test_rest_tie() {
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
fn test_waits() {
    assert_line_matches_bytecode("w", &["wait 24"]);
    assert_line_matches_bytecode("w.", &["wait 36"]);
    assert_line_matches_bytecode("w8", &["wait 12"]);
    assert_line_matches_bytecode("w8.", &["wait 18"]);

    assert_line_matches_bytecode("w%30", &["wait 30"]);

    assert_line_matches_bytecode("w%256", &["wait 256"]);
    assert_line_matches_bytecode("w%257", &["wait 256", "wait 1"]);
    assert_line_matches_bytecode("w%512", &["wait 256", "wait 256"]);
    assert_line_matches_bytecode("w%600", &["wait 256", "wait 256", "wait 88"]);

    merge_mml_commands_test("w || w", &["wait 48"]);
    merge_mml_commands_test("w || w8", &["wait 36"]);
    merge_mml_commands_test("w%30||w%20", &["wait 50"]);

    // From `mml-syntax.md`
    assert_line_matches_line("w4 w8 w8", "w2");
}

#[test]
fn test_wait_tie() {
    assert_line_matches_bytecode("w^^^", &["wait 96"]);

    assert_line_matches_bytecode("w^8", &["wait 36"]);
    assert_line_matches_bytecode("w8^", &["wait 36"]);
    assert_line_matches_bytecode("w8^16", &["wait 18"]);

    assert_line_matches_bytecode("w2^4^8", &["wait 84"]);

    assert_line_matches_bytecode("w%5^%7", &["wait 12"]);

    merge_mml_commands_test("w || ^ 8", &["wait 36"]);
}

#[test]
fn test_wait_loop() {
    // Test wait tick-counter threashold
    assert_line_matches_bytecode("w%768", &["wait 256", "wait 256", "wait 256"]);

    assert_line_matches_line_and_bytecode(
        "w%769",
        "[w%256]3 w%1",
        &["start_loop 3", "wait 256", "end_loop", "wait 1"],
    );

    assert_line_matches_line_and_bytecode(
        "w%25600",
        "[w%256]100",
        &["start_loop 100", "wait 256", "end_loop"],
    );

    assert_line_matches_line_and_bytecode(
        "w%25601",
        "[w%256]100 w%1",
        &["start_loop 100", "wait 256", "end_loop", "wait 1"],
    );

    assert!(512 * 195 + 159 == 99999);
    assert_line_matches_line_and_bytecode(
        "w%99999",
        "[w%512]195 w%159",
        &[
            "start_loop 195",
            "wait 256",
            "wait 256",
            "end_loop",
            "wait 159",
        ],
    );

    // A random prime number
    assert!(251 * 28 + 11 == 7039);
    assert_line_matches_line_and_bytecode(
        "w%7039",
        "[w%251]28 w%11",
        &["start_loop 28", "wait 251", "end_loop", "wait 11"],
    );

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
            "wait 256",
            "wait 256",
            "wait 256",
            "wait 256",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    // From `mml-syntax.md`
    assert_line_matches_line("w1 w1 w1 w1 w1 w1 w1 w1 w1 w1", "[w%240]4");
}

#[test]
fn test_rest_loop() {
    // Test rest tick-counter threashold
    assert_line_matches_bytecode("r%1024", &["wait 256", "wait 256", "wait 255", "rest 257"]);
    assert_line_matches_bytecode("r%1025", &["wait 256", "wait 256", "wait 256", "rest 257"]);

    assert_line_matches_line_and_bytecode(
        "r%1026",
        "[w%256]3 w%1 r%257",
        &["start_loop 3", "wait 256", "end_loop", "wait 1", "rest 257"],
    );

    assert!(255 * 4 + 257 == 1277);
    assert_line_matches_line_and_bytecode(
        "r%1277",
        "[w%255]4 r%257",
        &["start_loop 4", "wait 255", "end_loop", "rest 257"],
    );

    assert!(255 * 100 + 52 + 257 == 25809);
    assert_line_matches_line_and_bytecode(
        "r%25809",
        "[w%255]100 w%52 r%257",
        &[
            "start_loop 100",
            "wait 255",
            "end_loop",
            "wait 52",
            "rest 257",
        ],
    );

    assert!(253 * 101 + 257 == 25810);
    assert_line_matches_line_and_bytecode(
        "r%25810",
        "[w%253]101 r%257",
        &["start_loop 101", "wait 253", "end_loop", "rest 257"],
    );

    assert_line_matches_line_and_bytecode(
        "r%25811",
        "[w%255]100 w%54 r%257",
        &[
            "start_loop 100",
            "wait 255",
            "end_loop",
            "wait 54",
            "rest 257",
        ],
    );

    assert_line_matches_line_and_bytecode(
        "r%25858",
        "[w%256]100 w%1 r%257",
        &[
            "start_loop 100",
            "wait 256",
            "end_loop",
            "wait 1",
            "rest 257",
        ],
    );

    assert!(512 * 195 + 257 == 100097);
    assert_line_matches_line_and_bytecode(
        "r%100097",
        "[w%512]195 r%257",
        &[
            "start_loop 195",
            "wait 256",
            "wait 256",
            "end_loop",
            "rest 257",
        ],
    );

    // A random prime number for the wait portion
    // 5237 is prime
    assert!(249 * 21 + 8 == 5237);
    assert!(5237 + 257 == 5494);
    assert_line_matches_line_and_bytecode(
        "r%5494",
        "[w%249]21 w%8 r%257",
        &[
            "start_loop 21",
            "wait 249",
            "end_loop",
            "wait 8",
            "rest 257",
        ],
    );

    // Test no compile errors when loop stack is full
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
            "wait 256",
            "wait 256",
            "wait 256",
            "wait 256",
            "rest 257",
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
fn test_merged_rest_loop() {
    // Test rest tick-counter threashold
    assert_line_matches_bytecode(
        "r%2 r%771",
        &[
            "rest 2", // second rest
            "rest 257", "rest 257", "rest 257",
        ],
    );

    assert_line_matches_line_and_bytecode(
        "r%2 r%772",
        "r%2 [r%193]4",
        &["rest 2", "start_loop 4", "rest 193", "end_loop"],
    );

    // Test that RestLoop remainder of 0 works
    assert!(1028 % 257 == 0);
    assert_line_matches_line_and_bytecode(
        "r%2 r%1028",
        "r%2 [r%257]4",
        &["rest 2", "start_loop 4", "rest 257", "end_loop"],
    );

    // Test that RestLoop remainder of 1 is skipped
    assert!(1286 % 257 == 1);
    assert!(214 * 6 + 2 == 1286);
    assert_line_matches_line_and_bytecode(
        "r%2 r%1286",
        "r%2 [r%214]6 r%2",
        &["rest 2", "start_loop 6", "rest 214", "end_loop", "rest 2"],
    );

    assert!(25700 % 257 == 0);
    assert!(257 * 100 == 25700);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25700",
        "r%2 [r%257]100",
        &["rest 2", "start_loop 100", "rest 257", "end_loop"],
    );

    assert!(25701 % 257 == 1);
    assert!(254 * 101 + 47 == 25701);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25701",
        "r%2 [r%254]101 r%47",
        &[
            "rest 2",
            "start_loop 101",
            "rest 254",
            "end_loop",
            "rest 47",
        ],
    );

    assert!(25702 % 257 == 2);
    assert!(181 * 142 == 25702);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25702",
        "r%2 [r%181]142",
        &["rest 2", "start_loop 142", "rest 181", "end_loop"],
    );

    assert!(512 * 195 + 159 == 99999);
    assert_line_matches_line_and_bytecode(
        "r%2 r%99999",
        "r%2 [r%257 r%255]195 r%159",
        &[
            "rest 2",
            "start_loop 195",
            "rest 257",
            "rest 255",
            "end_loop",
            "rest 159",
        ],
    );

    // A random prime number for wait part of the first rest
    assert!(9743 == 10000 - 257);
    assert_line_matches_line_and_bytecode(
        "r%10000",
        "[w%256]38 w%15 r%257",
        &[
            "start_loop 38",
            "wait 256",
            "end_loop",
            "wait 15",
            "rest 257",
        ],
    );

    // A random prime number
    assert!(242 * 27 + 19 == 6553);
    assert_line_matches_line_and_bytecode(
        "r%2 r%6553",
        "r%2 [r%242]27 r%19",
        &["rest 2", "start_loop 27", "rest 242", "end_loop", "rest 19"],
    );

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
            // second rest
            "rest 257",
            "rest 257",
            "rest 257",
            "rest 257",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    // Test no compile errors when loop stack is full
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
            // first rest
            "wait 256",
            "wait 256",
            "wait 256",
            "wait 256",
            "wait 256",
            "rest 257",
            // second rest
            "rest 257",
            "rest 257",
            "rest 257",
            "rest 257",
            "end_loop 2",
            "end_loop 3",
            "end_loop 4",
            "end_loop 5",
            "end_loop 6",
            "end_loop 7",
            "end_loop 8",
        ],
    );

    // Test both rests are converted to loops
    assert!(249 * 7 == 2000 - 257);
    assert!(250 * 4 == 1000);
    assert_line_matches_line_and_bytecode(
        "r%2000 r%1000",
        "[w%249]7 r%257 [r%250]4",
        &[
            "start_loop 7",
            "wait 249",
            "end_loop",
            "rest 257",
            "start_loop 4",
            "rest 250",
            "end_loop",
        ],
    );

    // From `mml_syntax.md`
    assert_line_matches_line("r1 r1 r1 r1 r1 r1 r1 r1 r1 r1", "r1 [r%216]4")
}

#[test]
fn test_rest_after_keyoff_note() {
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

    assert_line_matches_bytecode("a%50 r%500", &["play_note a4 50", "rest 257", "rest 243"]);

    assert_line_matches_bytecode(
        "a%600 r%600",
        &[
            "play_note a4 no_keyoff 256",
            "wait 87",
            "rest 257",
            // rest
            "rest 257",
            "rest 257",
            "rest 86",
        ],
    );

    assert_line_matches_bytecode(
        "a%2561 r%2570",
        &[
            "play_note a4 no_keyoff 256",
            "start_loop 8",
            "wait 256",
            "end_loop",
            "rest 257",
            // rest
            "start_loop 10",
            "rest 257",
            "end_loop",
        ],
    );
}

// The rest after a slurred note must not be merged with successive rests
#[test]
fn test_rest_after_surred_note() {
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

    assert_line_matches_bytecode(
        "a%50 & r%500",
        &["play_note a4 no_keyoff 50", "wait 243", "rest 257"],
    );

    assert_line_matches_bytecode(
        "a%600 & r%600 r%600",
        &[
            "play_note a4 no_keyoff 256",
            "wait 256",
            "wait 88",
            // rest 1
            "wait 256",
            "wait 87",
            "rest 257",
            // rest 2
            "wait 256",
            "wait 87",
            "rest 257",
        ],
    );

    assert_line_matches_bytecode(
        "a%2560 & r%2561 r%2561",
        &[
            "play_note a4 no_keyoff 256",
            "start_loop 9",
            "wait 256",
            "end_loop",
            // rest 1
            "start_loop 9",
            "wait 256",
            "end_loop",
            "rest 257",
            // rest 2
            "start_loop 9",
            "wait 256",
            "end_loop",
            "rest 257",
        ],
    );
}

#[test]
fn test_merge_rests_newlines() {
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
