//! MML tests

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use compiler::bytecode_assembler::BcTerminator;
use compiler::data;
use compiler::data::{Name, TextFile, UniqueNamesList};
use compiler::driver_constants::{
    BC_CHANNEL_STACK_OFFSET, BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP,
    BC_STACK_BYTES_PER_SUBROUTINE_CALL, MAX_SUBROUTINES,
};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::errors::{BytecodeError, MmlError, SongError};
use compiler::mml;
use compiler::notes::Octave;
use compiler::pitch_table::{build_pitch_table, PitchTable};
use compiler::songs::{SongData, Subroutine};
use compiler::{bytecode_assembler, opcodes};

use std::fmt::Write;

const SAMPLE_FREQ: f64 = 500.0;

const EXAMPLE_ADSR_STR: &str = "12 1 1 16";
const EXAMPLE_ADSR_COMMENTS_STR: &str = "12,1,1,16";
const EXAMPLE_ADSR: Adsr = match Adsr::try_new(12, 1, 1, 16) {
    Ok(v) => v,
    Err(_) => panic!("Invalid Adsr"),
};

const EXAMPLE_GAIN_STR: &str = "127";
const EXAMPLE_GAIN: Gain = Gain::new(127);

#[test]
fn test_c_major_scale() {
    assert_line_matches_bytecode(
        "c d e f g a b",
        &[
            "play_note c4 24",
            "play_note d4 24",
            "play_note e4 24",
            "play_note f4 24",
            "play_note g4 24",
            "play_note a4 24",
            "play_note b4 24",
        ],
    );
}

#[test]
fn test_play_midi_note_number() {
    assert_line_matches_line("c d e f g a b", "n60 n62 n64 n65 n67 n69 n71");
}

#[test]
fn test_play_sample() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@sample sample

A @sample

A s s0 s1 s2
A s,8 s1,2 s2,4
A s,%2 s1,%4 s2,%6

A s^8 s&
A s1^8 s1&
A s2,16^8 s2,16 &
"##,
        &[
            "set_instrument sample",
            // line 1
            "play_note 0 24",
            "play_note 0 24",
            "play_note 1 24",
            "play_note 2 24",
            // line 2
            "play_note 0 12",
            "play_note 1 48",
            "play_note 2 24",
            // line 3
            "play_note 0 2",
            "play_note 1 4",
            "play_note 2 6",
            // line 4
            "play_note 0 36",
            "play_note 0 slur_next 24",
            // line 5
            "play_note 1 36",
            "play_note 1 slur_next 24",
            // line 6
            "play_note 2 18",
            "play_note 2 slur_next 6",
        ],
    );
}

#[test]
#[rustfmt::skip]
fn test_octave() {
    assert_line_matches_bytecode("a",             &["play_note a4 24"]);
    assert_line_matches_bytecode("o2 o5 b",       &["play_note b5 24"]);
    assert_line_matches_bytecode("o6 c < c << c", &["play_note c6 24", "play_note c5 24", "play_note c3 24"]);
    assert_line_matches_bytecode("o2 d > d >> d", &["play_note d2 24", "play_note d3 24", "play_note d5 24"]);
}

#[test]
fn test_accidentals_against_midi_note_numbers() {
    assert_line_matches_line("o4 e e- e-- e--- e----", "n64 n63 n62 n61 n60");
    assert_line_matches_line("o4 e e+ e++ e+++ e++++", "n64 n65 n66 n67 n68");
}

#[test]
#[rustfmt::skip]
fn test_note_length() {
    assert_line_matches_bytecode("a", &["play_note a4 24"]);
    assert_line_matches_bytecode(
        "a1 b3 c4 d8 e16",
        &["play_note a4 96", "play_note b4 32", "play_note c4 24", "play_note d4 12", "play_note e4 6"],
    );
    assert_line_matches_bytecode(
        "l8 a l16 b l8 c8 d32 e.",
        &["play_note a4 12", "play_note b4 6", "play_note c4 12", "play_note d4 3", "play_note e4 18"],
    );
    assert_line_matches_bytecode(
        "a. b.. c...",
        &["play_note a4 36", "play_note b4 42", "play_note c4 45"],
    );
    assert_line_matches_bytecode(
        "a1. b4. c8. d8..",
        &["play_note a4 144", "play_note b4 36", "play_note c4 18", "play_note d4 21"],
    );
    assert_line_matches_bytecode(
        "l4. a",
        &["play_note a4 36"]
    );
    assert_line_matches_bytecode("a%9  b%21", &["play_note a4 9", "play_note b4 21"]);
    assert_line_matches_bytecode(
        "l%9 a l%42 b c.",
        &["play_note a4 9", "play_note b4 42", "play_note c4 63"]
    );
}

#[test]
#[rustfmt::skip]
fn test_transpose() {
    assert_line_matches_line("_+2 d e f", "e f+ g");
    assert_line_matches_line("_-2 d e f", "c d d+");

    assert_line_matches_line("_+3 c d e", "c+++ d+++ e+++");
    assert_line_matches_line("_-3 c d e", "c--- d--- e---");

    // Relative transpose
    assert_line_matches_line("__+2 d e f", "e f+ g");
    assert_line_matches_line("__-2 d e f", "c d d+");

    assert_line_matches_line("__+2 __+2 def", "_+4 def");
    assert_line_matches_line("__-2 __-2 def", "_-4 def");

    assert_line_matches_line("__+8 __-2 def", "_+6 def");
    assert_line_matches_line("__-8 __+2 def", "_-6 def");

    // transpose overrides relative transpose
    assert_line_matches_line("_-5 _-10 def", "_-10 def");
    assert_line_matches_line("_-5 _+10 def", "_+10 def");

    assert_line_matches_line("_+5 _-10 def", "_-10 def");
    assert_line_matches_line("_-5 _+10 def", "_+10 def");
}

#[test]
fn test_slur() {
    assert_line_matches_bytecode(
        "a & b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );

    assert_line_matches_bytecode(
        "a & b8 & c16 & d32",
        &[
            "play_note a4 no_keyoff 24",
            "play_note b4 no_keyoff 12",
            "play_note c4 no_keyoff 6",
            "play_note d4 keyoff 3",
        ],
    );

    assert_line_matches_bytecode(
        "a & >b",
        &["play_note a4 no_keyoff 24", "play_note b5 keyoff 24"],
    );

    assert_line_matches_bytecode(
        "a < & b",
        &["play_note a4 no_keyoff 24", "play_note b3 keyoff 24"],
    );

    merge_mml_commands_test(
        "a || & b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );

    merge_mml_commands_test(
        "a & || b",
        &["play_note a4 no_keyoff 24", "play_note b4 keyoff 24"],
    );
}

#[test]
fn test_tie() {
    assert_line_matches_bytecode("a^ b", &["play_note a4 48", "play_note b4 24"]);
    assert_line_matches_bytecode("a^2 b", &["play_note a4 72", "play_note b4 24"]);
    assert_line_matches_bytecode("a^%1 b", &["play_note a4 25", "play_note b4 24"]);

    merge_mml_commands_test("a8 ^ || b", &["play_note a4 36", "play_note b4 24"]);
    merge_mml_commands_test("a8. || ^ b", &["play_note a4 42", "play_note b4 24"]);

    // Alternative tie syntax
    merge_mml_commands_test("a || &4 b", &["play_note a4 48", "play_note b4 24"]);
    merge_mml_commands_test("a &%1 || b", &["play_note a4 25", "play_note b4 24"]);
}

#[test]
fn test_quantization() {
    // Cannot use `assert_mml_matches_mml`.
    // There is a single rest tick at the end of a play_note instruction
    assert_line_matches_bytecode("Q1 c%80", &["play_note c4 11", "rest_keyoff 69"]);
    assert_line_matches_bytecode("Q2 c%80", &["play_note c4 21", "rest_keyoff 59"]);
    assert_line_matches_bytecode("Q3 c%80", &["play_note c4 31", "rest_keyoff 49"]);
    assert_line_matches_bytecode("Q4 c%80", &["play_note c4 41", "rest_keyoff 39"]);
    assert_line_matches_bytecode("Q5 c%80", &["play_note c4 51", "rest_keyoff 29"]);
    assert_line_matches_bytecode("Q6 c%80", &["play_note c4 61", "rest_keyoff 19"]);
    assert_line_matches_bytecode("Q7 c%80", &["play_note c4 71", "rest_keyoff  9"]);
    assert_line_matches_bytecode("Q8 c%80", &["play_note c4 80"]);

    assert_line_matches_bytecode("Q4 c", &["play_note c4 13", "rest_keyoff 11"]);

    merge_mml_commands_test("Q4 || c%100 r%100", &["play_note c4 51", "rest_keyoff 149"]);
    merge_mml_commands_test("Q4 c%100 || r%100", &["play_note c4 51", "rest_keyoff 149"]);

    // Test with tie
    merge_mml_commands_test(
        "Q4 c%100 || ^ %100",
        &["play_note c4 101", "rest_keyoff 99"],
    );
    merge_mml_commands_test(
        "Q4 c%100 || & %100",
        &["play_note c4 101", "rest_keyoff 99"],
    );

    // Test with tie and rest
    merge_mml_commands_test(
        "Q2 c%50 ^%50 || r%50 r%50",
        &["play_note c4 26", "rest_keyoff 174"],
    );
    merge_mml_commands_test(
        "Q6 c%70 & %30 || r%50 r%50",
        &["play_note c4 76", "rest_keyoff 124"],
    );
    merge_mml_commands_test(
        "Q6 c%70 & %30 || r%50 r%50 r%257",
        &["play_note c4 76", "rest_keyoff 257", "rest_keyoff 124"],
    );

    assert_line_matches_bytecode(
        "Q4 c%70 r%600",
        &[
            "play_note c4 36",
            "rest_keyoff 257",
            "rest_keyoff 257",
            "rest_keyoff 120",
        ],
    );

    assert_line_matches_bytecode(
        "Q4 c Q8 d Q6 e",
        &[
            "play_note c4 13",
            "rest_keyoff 11",
            "play_note d4 24",
            "play_note e4 19",
            "rest_keyoff 5",
        ],
    );
}

#[test]
fn play_long_note() {
    // `rest` can rest for 1 to 256 ticks.
    // `rest_keyoff` can rest for 2 to 257 tick.

    assert_line_matches_bytecode("a%256", &["play_note a4 keyoff 256"]);
    assert_line_matches_bytecode("a%257", &["play_note a4 keyoff 257"]);
    assert_line_matches_bytecode("a%258", &["play_note a4 no_keyoff 256", "rest_keyoff 2"]);

    assert_line_matches_bytecode("a%512", &["play_note a4 no_keyoff 256", "rest_keyoff 256"]);
    assert_line_matches_bytecode("a%513", &["play_note a4 no_keyoff 256", "rest_keyoff 257"]);
    assert_line_matches_bytecode(
        "a%514",
        &["play_note a4 no_keyoff 256", "rest 256", "rest_keyoff 2"],
    );

    assert_line_matches_bytecode(
        "a%600",
        &["play_note a4 no_keyoff 256", "rest 256", "rest_keyoff 88"],
    );
}

#[test]
fn play_long_slurred_note() {
    // `rest` can rest for 1 to 256 ticks.
    // `rest_keyoff` can rest for 2 to 257 tick.

    assert_line_matches_bytecode("a%256 &", &["play_note a4 no_keyoff 256"]);
    assert_line_matches_bytecode("a%257 &", &["play_note a4 no_keyoff 256", "rest 1"]);
    assert_line_matches_bytecode("a%258 &", &["play_note a4 no_keyoff 256", "rest 2"]);

    assert_line_matches_bytecode("a%512 &", &["play_note a4 no_keyoff 256", "rest 256"]);
    assert_line_matches_bytecode(
        "a%513 &",
        &["play_note a4 no_keyoff 256", "rest 256", "rest 1"],
    );
    assert_line_matches_bytecode(
        "a%514 &",
        &["play_note a4 no_keyoff 256", "rest 256", "rest 2"],
    );

    assert_line_matches_bytecode(
        "a%600 &",
        &["play_note a4 no_keyoff 256", "rest 256", "rest 88"],
    );
}

#[test]
fn test_rests() {
    assert_line_matches_bytecode("r", &["rest_keyoff 24"]);
    assert_line_matches_bytecode("r.", &["rest_keyoff 36"]);
    assert_line_matches_bytecode("r8", &["rest_keyoff 12"]);
    assert_line_matches_bytecode("r8.", &["rest_keyoff 18"]);

    assert_line_matches_bytecode("r%30", &["rest_keyoff 30"]);

    assert_line_matches_bytecode("r%256", &["rest_keyoff 256"]);
    assert_line_matches_bytecode("r%257", &["rest_keyoff 257"]);
    assert_line_matches_bytecode("r%258", &["rest 256", "rest_keyoff 2"]);
    assert_line_matches_bytecode("r%512", &["rest 256", "rest_keyoff 256"]);
    assert_line_matches_bytecode("r%513", &["rest 256", "rest_keyoff 257"]);
    assert_line_matches_bytecode("r%514", &["rest 256", "rest 256", "rest_keyoff 2"]);
    assert_line_matches_bytecode("r%600", &["rest 256", "rest 256", "rest_keyoff 88"]);

    // User expects a keyoff after the first rest command
    merge_mml_commands_test("r || r", &["rest_keyoff 24", "rest_keyoff 24"]);
    merge_mml_commands_test("r || r8", &["rest_keyoff 24", "rest_keyoff 12"]);
    merge_mml_commands_test("r%30||r%20", &["rest_keyoff 30", "rest_keyoff 20"]);

    merge_mml_commands_test("r r || r", &["rest_keyoff 24", "rest_keyoff 48"]);
    merge_mml_commands_test("r r || r8", &["rest_keyoff 24", "rest_keyoff 36"]);
    merge_mml_commands_test("r%30 r%30||r%20", &["rest_keyoff 30", "rest_keyoff 50"]);

    assert_line_matches_bytecode(
        "r%300 r%256",
        &["rest 256", "rest_keyoff 44", "rest_keyoff 256"],
    );

    // It does not matter if subsequent rests send keyoff events or not, no note is playing
    merge_mml_commands_test(
        "r%300 || r%300",
        &[
            "rest 256",
            "rest_keyoff 44",
            "rest_keyoff 257",
            "rest_keyoff 43",
        ],
    );
    assert_line_matches_bytecode(
        "r%300 r%100 r%100 r%100",
        &[
            "rest 256",
            "rest_keyoff 44",
            "rest_keyoff 257",
            "rest_keyoff 43",
        ],
    );

    // From `mml-syntax.md`
    assert_line_matches_line("r4 r8 r8", "r4 r4");
}

#[test]
fn test_rest_tie() {
    // used by midi2smw.

    assert_line_matches_bytecode("r^^^", &["rest_keyoff 96"]);

    assert_line_matches_bytecode("r^8", &["rest_keyoff 36"]);
    assert_line_matches_bytecode("r8^", &["rest_keyoff 36"]);
    assert_line_matches_bytecode("r8^16", &["rest_keyoff 18"]);

    assert_line_matches_bytecode("r2^4^8", &["rest_keyoff 84"]);

    assert_line_matches_bytecode("r%5^%7", &["rest_keyoff 12"]);

    merge_mml_commands_test("r || ^ 8", &["rest_keyoff 36"]);
}

#[test]
fn test_waits() {
    assert_line_matches_bytecode("w", &["rest 24"]);
    assert_line_matches_bytecode("w.", &["rest 36"]);
    assert_line_matches_bytecode("w8", &["rest 12"]);
    assert_line_matches_bytecode("w8.", &["rest 18"]);

    assert_line_matches_bytecode("w%30", &["rest 30"]);

    assert_line_matches_bytecode("w%256", &["rest 256"]);
    assert_line_matches_bytecode("w%257", &["rest 256", "rest 1"]);
    assert_line_matches_bytecode("w%512", &["rest 256", "rest 256"]);
    assert_line_matches_bytecode("w%600", &["rest 256", "rest 256", "rest 88"]);

    merge_mml_commands_test("w || w", &["rest 48"]);
    merge_mml_commands_test("w || w8", &["rest 36"]);
    merge_mml_commands_test("w%30||w%20", &["rest 50"]);

    // From `mml-syntax.md`
    assert_line_matches_line("w4 w8 w8", "w2");
}

#[test]
fn test_wait_tie() {
    assert_line_matches_bytecode("w^^^", &["rest 96"]);

    assert_line_matches_bytecode("w^8", &["rest 36"]);
    assert_line_matches_bytecode("w8^", &["rest 36"]);
    assert_line_matches_bytecode("w8^16", &["rest 18"]);

    assert_line_matches_bytecode("w2^4^8", &["rest 84"]);

    assert_line_matches_bytecode("w%5^%7", &["rest 12"]);

    merge_mml_commands_test("w || ^ 8", &["rest 36"]);
}

#[test]
fn test_wait_loop() {
    // Test wait tick-counter threashold
    assert_line_matches_bytecode("w%768", &["rest 256", "rest 256", "rest 256"]);

    assert_line_matches_line_and_bytecode(
        "w%769",
        "[w%256]3 w%1",
        &["start_loop 3", "rest 256", "end_loop", "rest 1"],
    );

    assert_line_matches_line_and_bytecode(
        "w%25600",
        "[w%256]100",
        &["start_loop 100", "rest 256", "end_loop"],
    );

    assert_line_matches_line_and_bytecode(
        "w%25601",
        "[w%256]100 w%1",
        &["start_loop 100", "rest 256", "end_loop", "rest 1"],
    );

    assert!(512 * 195 + 159 == 99999);
    assert_line_matches_line_and_bytecode(
        "w%99999",
        "[w%512]195 w%159",
        &[
            "start_loop 195",
            "rest 256",
            "rest 256",
            "end_loop",
            "rest 159",
        ],
    );

    // A random prime number
    assert!(251 * 28 + 11 == 7039);
    assert_line_matches_line_and_bytecode(
        "w%7039",
        "[w%251]28 w%11",
        &["start_loop 28", "rest 251", "end_loop", "rest 11"],
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
            "rest 256",
            "rest 256",
            "rest 256",
            "rest 256",
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
    assert_line_matches_bytecode("r%768", &["rest 256", "rest 256", "rest_keyoff 256"]);
    assert_line_matches_bytecode("r%769", &["rest 256", "rest 256", "rest_keyoff 257"]);

    assert_line_matches_line_and_bytecode(
        "r%770",
        "[w%256]3 r%2",
        &["start_loop 3", "rest 256", "end_loop", "rest_keyoff 2"],
    );

    // Test that RestLoop remainder of 0 is skipped
    assert!(1024 % 256 == 0);
    assert!(204 * 5 + 4 == 1024);
    assert_line_matches_line_and_bytecode(
        "r%1024",
        "[w%204]5 r%4",
        &["start_loop 5", "rest 204", "end_loop", "rest_keyoff 4"],
    );

    // Test that RestLoop remainder of 1 is skipped
    assert!(1025 % 256 == 1);
    assert!(170 * 6 + 5 == 1025);
    assert_line_matches_line_and_bytecode(
        "r%1025",
        "[w%170]6 r%5",
        &["start_loop 6", "rest 170", "end_loop", "rest_keyoff 5"],
    );

    assert!(25600 % 256 == 0);
    assert!(253 * 101 + 47 == 25600);
    assert_line_matches_line_and_bytecode(
        "r%25600",
        "[w%253]101 r%47",
        &["start_loop 101", "rest 253", "end_loop", "rest_keyoff 47"],
    );

    assert!(25601 % 256 == 1);
    assert!(253 * 101 + 48 == 25601);
    assert_line_matches_line_and_bytecode(
        "r%25601",
        "[w%253]101 r%48",
        &["start_loop 101", "rest 253", "end_loop", "rest_keyoff 48"],
    );

    assert_line_matches_line_and_bytecode(
        "r%25602",
        "[w%256]100 r%2",
        &["start_loop 100", "rest 256", "end_loop", "rest_keyoff 2"],
    );

    assert!(512 * 195 + 159 == 99999);
    assert_line_matches_line_and_bytecode(
        "r%99999",
        "[w%512]195 r%159",
        &[
            "start_loop 195",
            "rest 256",
            "rest 256",
            "end_loop",
            "rest_keyoff 159",
        ],
    );

    // A random prime number
    assert!(249 * 21 + 8 == 5237);
    assert_line_matches_line_and_bytecode(
        "r%5237",
        "[w%249]21 r%8",
        &["start_loop 21", "rest 249", "end_loop", "rest_keyoff 8"],
    );

    // Test no compile errors when loop stack is full
    assert_line_matches_bytecode(
        "[[[[[[[ r%1024 ]2]3]4]5]6]7]8",
        &[
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "start_loop",
            "rest 256",
            "rest 256",
            "rest 256",
            "rest_keyoff 256",
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
            "rest_keyoff 2",
            // second rest
            "rest_keyoff 257",
            "rest_keyoff 257",
            "rest_keyoff 257",
        ],
    );

    assert_line_matches_line_and_bytecode(
        "r%2 r%772",
        "r%2 [r%193]4",
        &[
            "rest_keyoff 2",
            "start_loop 4",
            "rest_keyoff 193",
            "end_loop",
        ],
    );

    // Test that RestLoop remainder of 0 works
    assert!(1028 % 257 == 0);
    assert_line_matches_line_and_bytecode(
        "r%2 r%1028",
        "r%2 [r%257]4",
        &[
            "rest_keyoff 2",
            "start_loop 4",
            "rest_keyoff 257",
            "end_loop",
        ],
    );

    // Test that RestLoop remainder of 1 is skipped
    assert!(1286 % 257 == 1);
    assert!(214 * 6 + 2 == 1286);
    assert_line_matches_line_and_bytecode(
        "r%2 r%1286",
        "r%2 [r%214]6 r%2",
        &[
            "rest_keyoff 2",
            "start_loop 6",
            "rest_keyoff 214",
            "end_loop",
            "rest_keyoff 2",
        ],
    );

    assert!(25700 % 257 == 0);
    assert!(257 * 100 == 25700);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25700",
        "r%2 [r%257]100",
        &[
            "rest_keyoff 2",
            "start_loop 100",
            "rest_keyoff 257",
            "end_loop",
        ],
    );

    assert!(25701 % 257 == 1);
    assert!(254 * 101 + 47 == 25701);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25701",
        "r%2 [r%254]101 r%47",
        &[
            "rest_keyoff 2",
            "start_loop 101",
            "rest_keyoff 254",
            "end_loop",
            "rest_keyoff 47",
        ],
    );

    assert!(25702 % 257 == 2);
    assert!(181 * 142 == 25702);
    assert_line_matches_line_and_bytecode(
        "r%2 r%25702",
        "r%2 [r%181]142",
        &[
            "rest_keyoff 2",
            "start_loop 142",
            "rest_keyoff 181",
            "end_loop",
        ],
    );

    assert!(512 * 195 + 159 == 99999);
    assert_line_matches_line_and_bytecode(
        "r%2 r%99999",
        "r%2 [r%257 r%255]195 r%159",
        &[
            "rest_keyoff 2",
            "start_loop 195",
            "rest_keyoff 257",
            "rest_keyoff 255",
            "end_loop",
            "rest_keyoff 159",
        ],
    );

    // A random prime number
    assert!(242 * 27 + 19 == 6553);
    assert_line_matches_line_and_bytecode(
        "r%2 r%6553",
        "r%2 [r%242]27 r%19",
        &[
            "rest_keyoff 2",
            "start_loop 27",
            "rest_keyoff 242",
            "end_loop",
            "rest_keyoff 19",
        ],
    );

    // Test no compile errors when loop stack is full
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
            "rest_keyoff 2",
            // second rest
            "rest_keyoff 257",
            "rest_keyoff 257",
            "rest_keyoff 257",
            "rest_keyoff 257",
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
    assert!(166 * 6 + 4 == 1000);
    assert!(250 * 4 == 1000);
    assert_line_matches_line_and_bytecode(
        "r%1000 r%1000",
        "[w%166]6 r%4 [r%250]4",
        &[
            "start_loop 6",
            "rest 166",
            "end_loop",
            "rest_keyoff 4",
            "start_loop 4",
            "rest_keyoff 250",
            "end_loop",
        ],
    );

    // From `mml_syntax.md`
    assert_line_matches_line("r1 r1 r1 r1 r1 r1 r1 r1 r1 r1", "r1 [r%216]4")
}

#[test]
fn test_loops() {
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
fn test_max_loops() {
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
fn test_too_many_loops() {
    assert_error_in_mml_line(
        "[[[[[[[[a]11]12]13]14]15]16]17]18",
        8,
        MmlError::BytecodeError(BytecodeError::StackOverflowInStartLoop(8 * 3)),
    );
}

#[test]
fn test_max_loops_with_subroutine() {
    let subroutine_stack_depth = 3 * BC_STACK_BYTES_PER_LOOP;
    let channel_a_stack_depth =
        subroutine_stack_depth + BC_STACK_BYTES_PER_SUBROUTINE_CALL + 3 * BC_STACK_BYTES_PER_LOOP;

    // Assert this is the maximum depth
    assert!(channel_a_stack_depth + BC_STACK_BYTES_PER_LOOP > BC_CHANNEL_STACK_OFFSET);
    assert!(channel_a_stack_depth + BC_STACK_BYTES_PER_SUBROUTINE_CALL > BC_CHANNEL_STACK_OFFSET);

    // Cannot test with asm, no easy way to implement a "call_subroutine s" bytecode.
    // Instead, this test will confirm it compiles with no errors and the correct stack depth

    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s [[[a]11]12]13

A @0
A [[[ !s ]14]15]16
"##,
        &dummy_data(),
    );

    let subroutine = &mml.subroutines()[0];
    let channel_a = mml.channels()[0].as_ref().unwrap();

    assert_eq!(
        subroutine.subroutine_id.max_stack_depth().to_u32(),
        u32::try_from(subroutine_stack_depth).unwrap()
    );

    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
}

#[test]
fn test_too_many_loops_with_subroutine_call() {
    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s [[[[[[[a]11]12]13]14]15]16]17

A @0
A !s
"##,
        3,
        MmlError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            23,
        )),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s a

A @0
A [[[[[[[ !s ]11]12]13]14]15]16]17
"##,
        11,
        MmlError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            23,
        )),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s [[[a]11]12]13

A @0
A [[[[[ !s ]14]15]16]17]18
"##,
        9,
        MmlError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            26,
        )),
    );
}

#[test]
fn test_max_subroutines() {
    const N_SUBROUTINES: u32 = MAX_SUBROUTINES as u32;

    let mut mml = String::new();
    writeln!(mml, "@0 dummy_instrument").unwrap();

    write!(mml, "A ").unwrap();
    for i in 1..=N_SUBROUTINES {
        write!(mml, " !s{i}").unwrap();
    }
    writeln!(mml).unwrap();

    for i in 1..=N_SUBROUTINES {
        writeln!(mml, "!s{i} a").unwrap();
    }

    let song = compile_mml(&mml, &dummy_data());
    let channel_a = song.channels()[0].as_ref().unwrap();

    assert_eq!(song.subroutines().len(), N_SUBROUTINES as usize);
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
    );
    assert_eq!(channel_a.tick_counter.value(), 24 * N_SUBROUTINES);
}

#[test]
fn test_max_subroutines_with_nesting() {
    const N_SUBROUTINES: u32 = MAX_SUBROUTINES as u32;

    let mut mml = String::new();
    writeln!(mml, "@0 dummy_instrument").unwrap();

    write!(mml, "A ").unwrap();
    for i in 1..=N_SUBROUTINES {
        write!(mml, " !{i}").unwrap();
    }
    writeln!(mml).unwrap();

    for i in 1..=N_SUBROUTINES {
        if i % 3 == 0 {
            writeln!(mml, "!{i} a !1 a").unwrap();
        } else if i % 5 == 0 {
            writeln!(mml, "!{i} b !3 b").unwrap();
        } else {
            writeln!(mml, "!{i} c").unwrap();
        }
    }

    let song = compile_mml(&mml, &dummy_data());
    let channel_a = song.channels()[0].as_ref().unwrap();

    assert_eq!(song.subroutines().len(), N_SUBROUTINES as usize);
    for s in song.subroutines() {
        let i: u32 = s.identifier.as_str().parse().unwrap();

        let stack_depth = if i % 3 == 0 {
            BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
        } else if i % 5 == 0 {
            2 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
        } else {
            0
        };

        assert_eq!(
            s.subroutine_id.max_stack_depth().to_u32(),
            stack_depth,
            "subroutine max_stack_depth mismatch for !{i}"
        );
    }

    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        3 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
    );
}

/// Testing for tail call optimisation by checking stack depth
#[test]
fn test_tail_call_1() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();

    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 0);
    assert_eq!(s.subroutine_id.tick_counter().value(), 24);

    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);
}

/// Testing for tail call optimisation by checking stack depth
#[test]
fn test_tail_call_2() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!tco r !t      ; expect tail-call optimsation
!no_tco !t r   ; expect no tail-call optimisation

!t r

A @0 !tco !no_tco
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "tco").unwrap();
    assert_eq!(s.identifier.as_str(), "tco");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 0);
    assert_eq!(s.subroutine_id.tick_counter().value(), 48);
    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);

    let (s, bc) = get_subroutine_and_bytecode(&sd, "no_tco").unwrap();
    assert_eq!(s.identifier.as_str(), "no_tco");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 2);
    assert_eq!(s.subroutine_id.tick_counter().value(), 48);
    assert_eq!(
        bc[bc.len().checked_sub(1).unwrap()],
        opcodes::RETURN_FROM_SUBROUTINE
    );
}

/// Testing that tail call optimisation is disabled if the subroutine ends with MP vibracto.
///
/// Using `call_subroutine_and_disable_vibrato ; return_from_subroutine` (3 bytes)
/// uses less Audio-RAM then `disable_vibrato ; goto_relative` (4 bytes).
///
/// Testing for the existance of tail call optimisation by checking stack depth.
#[test]
fn test_tail_call_mp() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s @0 MP20,5 a !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();
    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 2);
    assert_eq!(s.subroutine_id.tick_counter().value(), 48);
    assert_eq!(
        bc[bc.len().checked_sub(1).unwrap()],
        opcodes::RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO
    );
}

/// Testing that manual vibrato does not affect tail call optimisation.
///
/// Testing for the existance of tail call optimisation by checking stack depth.
#[test]
fn test_tail_call_manual_vibrato() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s @0 ~50,5 a !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();

    assert_eq!(s.identifier.as_str(), "s");
    assert_eq!(s.subroutine_id.max_stack_depth().to_u32(), 0);
    assert_eq!(s.subroutine_id.tick_counter().value(), 48);
    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);
}

#[test]
fn test_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 a
!s2 b
!s3 c

A @0 !s1 !s2 !s3
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s1",
            "call_subroutine s2",
            "call_subroutine s3",
        ],
    );
}

#[test]
fn test_set_instrument_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument
@1 dummy_instrument_2

!s @1 a

A @0 !s b @0 c
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note b4 24",
            "set_instrument dummy_instrument",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s @0 a

A !s b @0 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_instrument` is optimised out
            "play_note c4 24",
        ],
    );
}
#[test]
fn test_adsr_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument adsr 1 1 1 1

!s @0 a

A !s b A2,2,2,2 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            "set_adsr 2 2 2 2",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument adsr 2 2 2 2

!s @0 a

A !s b A2,2,2,2 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_adsr 2 2 2 2` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn test_gain_after_call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument gain F100

!s @0 a

A !s b GF50 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            "set_gain F50",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument gain F100

!s @0 a

A !s b GF100 c
"##,
        &[
            "call_subroutine s",
            "play_note b4 24",
            // `set_gain F100` is optimised out
            "play_note c4 24",
        ],
    );
}

#[test]
fn test_prev_slurred_note_after_subroutine_call() {
    // see `test_skip_last_loop_prev_slurred_note()`

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s a

A @0 a& !s {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 24",
            "call_subroutine s",
            // Previous note is a4 and NOT slurred
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s a&

A @0 g !s {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note g4 24",
            "call_subroutine s",
            // Previous slurred note is a4
            "portamento b4 keyoff +10 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s r10
A @0 a& !s {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 24",
            "call_subroutine s",
            // s will rest and key-off
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s w10
A @0 a& !s {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 24",
            "call_subroutine s",
            // s does not key-off and does not play a note
            // Previous slurred note is still a4
            "portamento b4 keyoff +10 24",
        ],
    );
}

#[test]
fn test_nested_subroutines() {
    // Cannot test with asm, no easy way to implement a "call_subroutine s" bytecode.
    // Instead, this test will confirm it compiles with no errors and the correct stack depth

    let _test = |mml| {
        let mml = compile_mml(mml, &dummy_data());
        let channel_a = mml.channels()[0].as_ref().unwrap();
        assert_eq!(
            channel_a.max_stack_depth.to_u32(),
            3 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
        );
        assert_eq!(channel_a.tick_counter.value(), 24 * 2 * 3,);
    };

    _test(
        r##"
@0 dummy_instrument

!s1 a !s2 a
!s2 b !s3 b
!s3 c c

A @0 !s1
"##,
    );

    _test(
        r##"
@0 dummy_instrument

!s3 c c
!s2 b !s3 b
!s1 a !s2 a

A @0 !s1
"##,
    );

    _test(
        r##"
@0 dummy_instrument

!s2 b !s3 b
!s3 c c
!s1 a !s2 a

A @0 !s1
"##,
    );
}

#[test]
fn test_nested_subroutines_recursion() {
    assert_subroutine_err_in_mml(
        r##"
@0 dummy_instrument

!s a !s a

A @0 !s
"##,
        &[(
            "s",
            &[MmlError::CannotCallSubroutineRecursion("s".to_owned())],
        )],
    );
}

#[test]
fn test_nested_subroutines_recursion2() {
    assert_subroutine_err_in_mml(
        r##"
@0 dummy_instrument

!s1 !s2 a
!s2 !s3 b
!s3 !s1 c

A @0 !s1
"##,
        &[
            (
                "s1",
                &[MmlError::CannotCallSubroutineRecursion("s2".to_owned())],
            ),
            (
                "s2",
                &[MmlError::CannotCallSubroutineRecursion("s3".to_owned())],
            ),
            // No error in !s3.  It is compiled last
        ],
    );
}

#[test]
fn test_nested_subroutines_with_missing1() {
    assert_subroutine_err_in_mml(
        r##"
@0 dummy_instrument

!s !s2 a

A @0 !s
"##,
        &[("s", &[MmlError::CannotFindSubroutine("s2".to_owned())])],
    );
}

#[test]
fn test_nested_subroutines_with_missing2() {
    assert_subroutine_err_in_mml(
        r##"
@0 dummy_instrument

!s1 !s2 a
!s2 !missing b
!s3 !s1 c
!s4 d

A @0 !s1 !s3
"##,
        &[(
            "s2",
            &[MmlError::CannotFindSubroutine("missing".to_owned())],
        )],
    );
}

#[test]
fn test_nested_subroutines_stack_depth_limit() {
    let channel_a_stack_depth =
        3 * BC_STACK_BYTES_PER_LOOP + 6 * BC_STACK_BYTES_PER_SUBROUTINE_CALL;
    assert_eq!(channel_a_stack_depth, BC_CHANNEL_STACK_SIZE);

    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s1 [ !s2 ]2
!s2 [ !s3 ]2
!s3 [ !s4 ]2
!s4 !s5 d
!s5 !s6 e
!s6 f

A @0 !s1
"##,
        &dummy_data(),
    );

    let channel_a = mml.channels()[0].as_ref().unwrap();
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
    assert_eq!(channel_a.tick_counter.value(), (24 * 3) * 2 * 2 * 2);

    // Same subroutines but shuffled
    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s5 !s6 e
!s2 [ !s3 ]2
!s6 f
!s3 [ !s4 ]2
!s4 !s5 d
!s1 [ !s2 ]2

A @0 !s1
"##,
        &dummy_data(),
    );

    let channel_a = mml.channels()[0].as_ref().unwrap();
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
    assert_eq!(channel_a.tick_counter.value(), (24 * 3) * 2 * 2 * 2);
}

#[test]
fn test_nested_subroutines_stack_overflow() {
    let stack_depth = 11 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32;
    assert!(stack_depth > BC_CHANNEL_STACK_SIZE as u32);

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 !s2 r
!s2 !s3 r
!s3 !s4 r
!s4 !s5 r
!s5 !s6 r
!s6 !s7 r
!s7 !s8 r
!s8 !s9 r
!s9 !s10 r
!s10 !s11 r
!s11 a

A @0 !s1
"##,
        6,
        MmlError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s1".to_owned(),
            stack_depth,
        )),
    );
}

#[test]
fn test_broken_chord() {
    // From mml_symtax.md
    assert_line_matches_line("{{ceg}}", "[c%1 & : e%1 & g%1 &]8 e%2");
    assert_line_matches_line("{{ce-g}}2,32", "[c32 & e-32 & g32 &]5 c32");
    assert_line_matches_line("{{c}}4,,0", "[c%2]12");
    assert_line_matches_line("{{de}}4,,0", "[d%2 e%2]6");
    assert_line_matches_line("{{fg}}4,%3,0", "[f%3 g%3]4");
    assert_line_matches_line("{{ab}}4,16,0", "[a16 b16]2");

    // Test octave/transpose changes::
    assert_line_matches_line("{{c o3 e > g}}", "[c%1& : o3 e%1& > g%1&]8 o3 e%2");
    assert_line_matches_line("{{c _+2 e < g}}", "[c%1& : _+2 e%1& < g%1&]8 o4 e%2");
    assert_line_matches_line("{{c __-2 e g}}", "[c%1& : _-2 e%1& g%1&]8 e%2");
}

#[test]
fn test_portamento() {
    // Only testing portamento with a speed override

    assert_line_matches_bytecode(
        "{df},,10",
        &["play_note d4 no_keyoff 1", "portamento f4 keyoff +10 23"],
    );

    assert_line_matches_bytecode(
        "{df}2,,10",
        &["play_note d4 no_keyoff 1", "portamento f4 keyoff +10 47"],
    );

    assert_line_matches_bytecode(
        "d& {df},,10",
        &["play_note d4 no_keyoff 24", "portamento f4 keyoff +10 24"],
    );

    assert_line_matches_bytecode(
        "d {df},,10",
        &[
            "play_note d4 24",
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 23",
        ],
    );

    assert_line_matches_bytecode(
        "e& {df},,10",
        &[
            "play_note e4 no_keyoff 24",
            "play_note d4 no_keyoff  1",
            "portamento f4 keyoff +10 23",
        ],
    );

    assert_line_matches_bytecode(
        "{df},,10 & b",
        &[
            "play_note d4 no_keyoff 1",
            "portamento f4 no_keyoff +10 23",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "{df},,10 ^",
        &["play_note d4 no_keyoff 1", "portamento f4 keyoff +10 47"],
    );
    assert_line_matches_bytecode(
        "{df},,10 &8",
        &["play_note d4 no_keyoff 1", "portamento f4 keyoff +10 35"],
    );
    assert_line_matches_bytecode(
        "d8 & {df},,15 &8",
        &["play_note d4 no_keyoff 12", "portamento f4 keyoff +15 36"],
    );
    assert_line_matches_bytecode(
        "d8 & {df},,15 ^8 &",
        &[
            "play_note d4 no_keyoff 12",
            "portamento f4 no_keyoff +15 36",
        ],
    );

    assert_line_matches_line("{df}4,8", "d8 & {df}8");
    assert_line_matches_line("{df}4,8,15", "d8 & {df}8,,15");

    assert_line_matches_line("{a > c}2", "{a _+12 c}2");
    assert_line_matches_line("{o3 c o4 c}2", "{< c > c}2");
}

#[test]
fn test_vibrato() {
    assert_line_matches_bytecode(
        "~23,4 a ~0",
        &["set_vibrato 23 4", "play_note a4 24", "disable_vibrato"],
    );
}

#[test]
fn test_mp_vibrato() {
    assert_line_matches_bytecode(
        "MP2,4 a b MP0 c",
        &[
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "set_vibrato_depth_and_play_note 0 c4 24",
        ],
    );
}

#[test]
fn test_subroutine_vibrato_bugfix() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s ~250,4
A @1 c !s d e
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            "play_note d4 24",
            "play_note e4 24",
        ],
    );
}

#[test]
fn test_vibrato_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 c ~50,4 !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "set_vibrato 50 4",
            "call_subroutine s",
            "play_note d4 24",
        ],
    );
}

#[test]
fn test_vibrato_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 c !s ~50,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            // set vibrato instructions are not deduplicated
            "set_vibrato 50 4",
            "play_note d4 24",
        ],
    );
}

#[test]
fn test_mp_vibrato_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1
A @1 MP20,4 c !s c
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 6 c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d
A @1 MP20,4 c !s d d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
            // vibrato active and depth unchanged
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,6 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,6 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 MP20,4 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 6 4",
            "play_note c4 24",
            "call_subroutine_and_disable_vibrato s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );
}

#[test]
fn test_mp_vibrato_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1
A @1 c !s MP20,4 c
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato unchanged
            "set_vibrato 6 4",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato unchanged
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d
A @1 c !s MP20,4 d d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
            // vibrato active and depth unchanged
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,4 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,6 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~50,6 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 50 and quarter_wavelength 6
            "set_vibrato 7 4",
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );
}

#[test]
fn test_mp0_before_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 MP0 c !s d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            // subroutine vibrato overrides MP0
            "play_note d4 24",
        ],
    );
}

#[test]
fn test_mp0_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 ~7,4 d
A @1 c !s MP0 d
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 7 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );
}

#[test]
fn test_mp0_at_end_of_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d MP0
A @1 ~25,5 c !s MP0 d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 25 5",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
!s @1 MP20,4 d MP0
A @1 ~25,10 c !s MP20,4 d
"##,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 25 10",
            "play_note c4 24",
            "call_subroutine s",
            // subroutine returns with vibrato depth 0 and quarter_wavelength 4
            "set_vibrato_depth_and_play_note 7 d4 24",
        ],
    );
}

#[test]
fn test_volume() {
    assert_line_matches_bytecode("v1", &["set_volume 16"]);
    assert_line_matches_bytecode("v8", &["set_volume 128"]);
    assert_line_matches_bytecode("v16", &["set_volume 255"]);

    assert_line_matches_bytecode("v+1", &["adjust_volume +16"]);
    assert_line_matches_bytecode("v+2", &["adjust_volume +32"]);
    assert_line_matches_bytecode("v+7", &["adjust_volume +112"]);
    assert_line_matches_bytecode("v-3", &["adjust_volume -48"]);
    assert_line_matches_bytecode("v-4", &["adjust_volume -64"]);
    assert_line_matches_bytecode("v-8", &["adjust_volume -128"]);

    assert_line_matches_bytecode("V0", &["set_volume 0"]);
    assert_line_matches_bytecode("V42", &["set_volume 42"]);
    assert_line_matches_bytecode("V255", &["set_volume 255"]);

    assert_line_matches_bytecode("V-20", &["adjust_volume -20"]);
    assert_line_matches_bytecode("V-40", &["adjust_volume -40"]);
    assert_line_matches_bytecode("V+60", &["adjust_volume +60"]);
    assert_line_matches_bytecode("V+70", &["adjust_volume +70"]);
}

#[test]
fn test_pan() {
    assert_line_matches_bytecode("p0", &["set_pan 0"]);
    assert_line_matches_bytecode("p64", &["set_pan 64"]);
    assert_line_matches_bytecode("p128", &["set_pan 128"]);

    assert_line_matches_bytecode("p+16", &["adjust_pan +16"]);
    assert_line_matches_bytecode("p+32", &["adjust_pan +32"]);
    assert_line_matches_bytecode("p-48", &["adjust_pan -48"]);
    assert_line_matches_bytecode("p-64", &["adjust_pan -64"]);
}

#[test]
fn test_merge_pan() {
    merge_mml_commands_test("p1 p2 || p3 p4", &["set_pan 4"]);

    merge_mml_commands_test("p0 || p+5 p+6", &["set_pan 11"]);
    merge_mml_commands_test("p100 || p+100", &["set_pan 128"]);
    merge_mml_commands_test("p0 p+5 || p10", &["set_pan 10"]);
    merge_mml_commands_test("p+1 || p+2 p+3", &["adjust_pan +6"]);

    merge_mml_commands_test("p120 || p-10 p-20", &["set_pan 90"]);
    merge_mml_commands_test("p50 || p-100", &["set_pan 0"]);
    merge_mml_commands_test("p10 p-5 || p10", &["set_pan 10"]);
    merge_mml_commands_test("p-4 || p-5 p-6", &["adjust_pan -15"]);
}

#[test]
fn test_merge_coarse_volume() {
    merge_mml_commands_test("v1 v2 || v3 v4", &["set_volume 64"]);

    merge_mml_commands_test("v0 v+5 || v+6", &["set_volume 176"]);
    merge_mml_commands_test("v12 || v+7", &["set_volume 255"]);
    merge_mml_commands_test("v0 || v+5 v10", &["set_volume 160"]);
    merge_mml_commands_test("v+1 v+2 || v+3", &["adjust_volume +96"]);

    merge_mml_commands_test("v15 || v-5 v-2", &["set_volume 128"]);
    merge_mml_commands_test("v3 || v-5", &["set_volume 0"]);
    merge_mml_commands_test("v0 v-5 || v10", &["set_volume 160"]);
    merge_mml_commands_test("v-1 || v-2 v-3", &["adjust_volume -96"]);
}

#[test]
fn test_merge_fine_volume() {
    merge_mml_commands_test("V1 V2 || V3 V4", &["set_volume 4"]);

    merge_mml_commands_test("V0 V+5 || V+6", &["set_volume 11"]);
    merge_mml_commands_test("V180 || V+120", &["set_volume 255"]);
    merge_mml_commands_test("V0 V+5 || V10", &["set_volume 10"]);
    merge_mml_commands_test("V+1 || V+2 V+3", &["adjust_volume +6"]);

    merge_mml_commands_test("V120 V-10 || V-20", &["set_volume 90"]);
    merge_mml_commands_test("V50 || V-100", &["set_volume 0"]);
    merge_mml_commands_test("V10 V-5 || V10", &["set_volume 10"]);
    merge_mml_commands_test("V-4 || V-5 V-6", &["adjust_volume -15"]);
}

#[test]
fn test_merge_pan_and_volume() {
    merge_mml_commands_test("p0 || v5", &["set_pan_and_volume 0 80"]);
    merge_mml_commands_test("v6 || p128", &["set_pan_and_volume 128 96"]);
    merge_mml_commands_test("p30 || V40", &["set_pan_and_volume 30 40"]);
    merge_mml_commands_test("V80 || p90", &["set_pan_and_volume 90 80"]);

    merge_mml_commands_test("p10 || V+5", &["adjust_volume +5", "set_pan 10"]);
    merge_mml_commands_test("V10 || p+5", &["set_volume 10", "adjust_pan +5"]);

    merge_mml_commands_test("p-10 || V5", &["set_volume 5", "adjust_pan -10"]);
    merge_mml_commands_test("V-10 || p5", &["adjust_volume -10", "set_pan 5"]);

    merge_mml_commands_test("p-10 || V+5", &["adjust_volume +5", "adjust_pan -10"]);
    merge_mml_commands_test("V-10 || p+5", &["adjust_volume -10", "adjust_pan +5"]);
}

#[test]
fn test_large_adjust_volume() {
    assert_line_matches_bytecode("V+127", &["adjust_volume +127"]);
    assert_line_matches_bytecode("V+128", &["adjust_volume +127", "adjust_volume +1"]);
    assert_line_matches_bytecode("V+200", &["adjust_volume +127", "adjust_volume +73"]);
    assert_line_matches_bytecode("V+100 V+100", &["adjust_volume +127", "adjust_volume +73"]);

    assert_line_matches_bytecode("v+12", &["adjust_volume +127", "adjust_volume +65"]);
    assert_line_matches_bytecode("v+6 v+6", &["adjust_volume +127", "adjust_volume +65"]);

    assert_line_matches_bytecode("V-128", &["adjust_volume -128"]);
    assert_line_matches_bytecode("V-129", &["adjust_volume -128", "adjust_volume -1"]);
    assert_line_matches_bytecode("V-200", &["adjust_volume -128", "adjust_volume -72"]);
    assert_line_matches_bytecode("V-100 V-100", &["adjust_volume -128", "adjust_volume -72"]);

    assert_line_matches_bytecode("v-12", &["adjust_volume -128", "adjust_volume -64"]);
    assert_line_matches_bytecode("v-6 v-6", &["adjust_volume -128", "adjust_volume -64"]);

    assert_line_matches_bytecode("V+254", &["adjust_volume +127", "adjust_volume +127"]);
    assert_line_matches_bytecode("V+255", &["set_volume 255"]);
    assert_line_matches_bytecode("V+400", &["set_volume 255"]);
    assert_line_matches_bytecode("V+200 V+200", &["set_volume 255"]);

    assert_line_matches_bytecode("V-254", &["adjust_volume -128", "adjust_volume -126"]);
    assert_line_matches_bytecode("V-255", &["set_volume 0"]);
    assert_line_matches_bytecode("V-400", &["set_volume 0"]);
    assert_line_matches_bytecode("V-200 V-200", &["set_volume 0"]);

    assert_line_matches_bytecode("v+16", &["set_volume 255"]);
    assert_line_matches_bytecode("v-16", &["set_volume 0"]);

    assert_line_matches_bytecode("V+200 V+200 V-10", &["set_volume 245"]);
    assert_line_matches_bytecode("V-200 V-200 V+10", &["set_volume 10"]);
}

// Tests if a large relative pan command turns into an absolute pan command
#[test]
fn test_large_adjust_pan() {
    assert_line_matches_bytecode("p+127", &["adjust_pan +127"]);
    assert_line_matches_bytecode("p+128", &["set_pan 128"]);
    assert_line_matches_bytecode("p+200", &["set_pan 128"]);
    assert_line_matches_bytecode("p+100 p+100", &["set_pan 128"]);

    assert_line_matches_bytecode("p-127", &["adjust_pan -127"]);
    assert_line_matches_bytecode("p-128", &["set_pan 0"]);
    assert_line_matches_bytecode("p-200", &["set_pan 0"]);
    assert_line_matches_bytecode("p-100 p-100", &["set_pan 0"]);

    assert_line_matches_bytecode("p+100 p+100 p-10", &["set_pan 118"]);
    assert_line_matches_bytecode("p-100 p-100 p+10", &["set_pan 10"]);
}

#[test]
fn test_set_instrument() {
    let mml = r##"
@0 dummy_instrument
@1 inst_with_adsr
@2 inst_with_gain

A @0 @1 @2
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "set_instrument dummy_instrument",
            "set_instrument inst_with_adsr",
            "set_instrument inst_with_gain",
        ],
    );
}

/// Test instruments with the same InstrumentId do not emit a set_instrument instruction
#[test]
fn test_set_instrument_merge_instrument_ids() {
    let mml = r##"
@0 dummy_instrument
@1 dummy_instrument
@2 dummy_instrument
@o inst_with_adsr

A @0 @0 @0
A @1 @2
A @o @0 @1 @2
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "set_instrument dummy_instrument",
            "set_instrument inst_with_adsr",
            "set_instrument dummy_instrument",
        ],
    );
}

#[test]
fn test_set_instrument_and_envelope() {
    let mml = format!(
        r##"
@0 dummy_instrument
@1 dummy_instrument adsr 1 2 3 4
@a inst_with_adsr
@aa inst_with_adsr adsr {EXAMPLE_ADSR_STR}
@ab inst_with_adsr adsr 1 2 3 4
@ag inst_with_adsr gain 24

@g1 inst_with_gain
@g2 inst_with_gain gain {EXAMPLE_GAIN_STR}
@g3 inst_with_gain adsr 3 4 5 6

A @0 @1
A @a @aa @ab @ag
A @0 @ab @a
A @0 @ag @ab
A @0 @g1 @g2 @g3 @g1
"##
    );

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            // Line 1
            "set_instrument dummy_instrument",
            "set_adsr 1 2 3 4",
            // Line 2
            "set_instrument inst_with_adsr",
            "set_adsr 1 2 3 4",
            "set_gain 24",
            // Line 3
            "set_instrument dummy_instrument",
            "set_instrument_and_adsr inst_with_adsr 1 2 3 4",
            "set_instrument inst_with_adsr",
            // Line 4
            "set_instrument dummy_instrument",
            "set_instrument_and_gain inst_with_adsr 24",
            "set_adsr 1 2 3 4",
            // Line 5
            "set_instrument dummy_instrument",
            "set_instrument inst_with_gain",
            "set_adsr 3 4 5 6",
            "set_instrument inst_with_gain",
        ],
    );
}

#[test]
fn test_set_adsr_and_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        &format!(
            r##"
@a inst_with_adsr
@aa inst_with_adsr adsr 1 2 3 4
@ag inst_with_adsr gain 24

@g inst_with_gain
@ga inst_with_gain adsr 5 6 7 8
@gg inst_with_gain gain 48

A @a  A{EXAMPLE_ADSR_COMMENTS_STR} A{EXAMPLE_ADSR_COMMENTS_STR}
A @aa A1,2,3,4
A @ag G24 G100 A1,2,3,4 A1,2,3,4 A5,6,7,8 A5,6,7,8

A @g  G{EXAMPLE_GAIN_STR} G{EXAMPLE_GAIN_STR}
A @ga G10 G10 A5,6,7,8 G20
A @gg G48 G48 A5,6,7,8 G30
"##
        ),
        &[
            // Line 1
            "set_instrument inst_with_adsr",
            // Line 2
            "set_adsr 1 2 3 4",
            // Line 3
            "set_gain 24",
            "set_gain 100",
            "set_adsr 1 2 3 4",
            "set_adsr 5 6 7 8",
            // Line 4
            "set_instrument inst_with_gain",
            // Line 5
            "set_adsr 5 6 7 8",
            "set_gain 10",
            "set_adsr 5 6 7 8",
            "set_gain 20",
            // Line 6
            "set_gain 48",
            "set_adsr 5 6 7 8",
            "set_gain 30",
        ],
    );
}

#[test]
fn test_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@g inst_with_gain
@gf inst_with_gain gain F 1
@gd inst_with_gain gain D 2
@ge inst_with_gain gain E 3
@gi inst_with_gain gain I 4
@gb inst_with_gain gain B 5

A @g   @gf @gd @ge @gi @gb
A G$20 GF6 GD7 GE8 GI9 GB10
"##,
        &[
            // Line 1
            "set_instrument inst_with_gain",
            "set_gain $01",
            "set_gain $82",
            "set_gain $a3",
            "set_gain $c4",
            "set_gain $e5",
            // Line 2
            "set_gain $20",
            "set_gain $06",
            "set_gain $87",
            "set_gain $a8",
            "set_gain $c9",
            "set_gain $ea",
        ],
    );
}

#[test]
fn test_instrument_envelope_hex() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@a1 dummy_instrument adsr $f $7 $7 $1f
@a2 dummy_instrument adsr $d 5 2 $b
@a3 dummy_instrument adsr 12 4 5 21

@g1 dummy_instrument gain $2b
@g2 dummy_instrument gain $e4
@g3 dummy_instrument gain 42

A @a1 @a2 @a3 @g1 @g2 @g3
"##,
        &[
            "set_instrument_and_adsr dummy_instrument 15 7 7 31",
            "set_adsr $d 5 2 $b",
            "set_adsr $c $4 $5 $15",
            "set_gain 43",
            "set_gain $e4",
            "set_gain $2a",
        ],
    );
}

#[test]
fn test_parse_hex() {
    assert!(96 / 0xc == 8);
    assert_line_matches_bytecode("c$c", &["play_note c4 8"]);
    assert_line_matches_bytecode("c$c.", &["play_note c4 12"]);
    assert_line_matches_bytecode("c%$ba", &["play_note c4 186"]);
    assert_line_matches_bytecode("A $f,$7,$7,$1f", &["set_adsr 15 7 7 31"]);
    assert_line_matches_bytecode("G $7f", &["set_gain 127"]);
}

#[test]
fn test_set_instrument_after_set_adsr() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 inst_with_adsr

A @1 A1,2,3,4 @1
"##,
        &[
            "set_instrument inst_with_adsr",
            "set_adsr 1 2 3 4",
            "set_instrument inst_with_adsr",
        ],
    );
}

#[test]
fn test_set_instrument_after_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 inst_with_gain

A @1 GI5 @1
"##,
        &[
            "set_instrument inst_with_gain",
            "set_gain I5",
            "set_instrument inst_with_gain",
        ],
    );
}

#[test]
fn test_echo() {
    assert_line_matches_bytecode("E", &["enable_echo"]);
    assert_line_matches_bytecode("E1", &["enable_echo"]);
    assert_line_matches_bytecode("E0", &["disable_echo"]);
}

#[test]
fn test_set_song_tempo() {
    let tc = f64::round(8000.0 * 60.0 / f64::from(48 * 80)) as u32;
    let bc = format!("set_song_tick_clock {tc}");

    assert_line_matches_bytecode("t80", &[&bc]);
}

#[test]
fn test_set_song_tick_clock() {
    assert_line_matches_bytecode("T64", &["set_song_tick_clock 64"]);
    assert_line_matches_bytecode("T255", &["set_song_tick_clock 255"]);
    assert_line_matches_bytecode("T90", &["set_song_tick_clock 90"]);
}

/// Test instrument is is correctly tracked after a *skip last loop* command.
/// Assumes `test_set_instrument_merge_instrument_ids()` passes
#[test]
fn test_skip_last_loop_set_instrument_merge_1() {
    let mml = r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : @d b ]2 @d
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : [ @d b : c]2 ]3 @d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 2",
            "end_loop 3",
            "set_instrument dummy_instrument",
        ],
    );
}

/// Test instrument is is correctly tracked after a *skip last loop* command.
/// Assumes `test_set_instrument_merge_instrument_ids()` passes
#[test]
fn test_skip_last_loop_set_instrument_merge_2() {
    let mml = r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : @d b ]2 @a
"##
    .to_string();

    assert_mml_channel_a_matches_bytecode(
        &mml,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "end_loop 2",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@d dummy_instrument
@a inst_with_adsr

A [ @a a : [ @d b : c ]2 ] 3 @a
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "skip_last_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 2",
            "end_loop 3",
        ],
    );
}

/// Test ADSR envelope is correctly tracked across loops
#[test]
fn test_skip_last_loop_set_adsr() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a A 5,6,7,8 b : A 15,7,7,31 c ]2 A 5,6,7,8 d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "skip_last_loop",
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "end_loop 2",
            // No set_adsr instruction
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a A 5,6,7,8 b : [ A 15,7,7,31 c : d]2 ]3 A 5,6,7,8 e
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_adsr 15 7 7 31",
            "play_note c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            // No set_adsr instruction
            "play_note e4 24",
        ],
    );
}

/// Test GAIN envelope is correctly tracked across loops
#[test]
fn test_skip_last_loop_set_gain() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a G10 b : G20 c ]2 G10 d
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_gain 10",
            "play_note b4 24",
            "skip_last_loop",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 2",
            // No set_gain instruction
            "play_note d4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@i inst_with_adsr

A [ @i a G10 b : [ G20 c : d ]2 ]3 G10 e
"##,
        &[
            "start_loop",
            "set_instrument inst_with_adsr",
            "play_note a4 24",
            "set_gain 10",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_gain 20",
            "play_note c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            // No set_gain instruction
            "play_note e4 24",
        ],
    );
}

/// Test that the vibrato state is correctly tracked after a *skip last loop* command.
#[test]
fn test_skip_last_loop_vibrato() {
    assert_line_matches_bytecode(
        "[MP2,4 a b : MP0 c]4 d",
        &[
            "start_loop",
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "skip_last_loop",
            "set_vibrato_depth_and_play_note 0 c4 24",
            "end_loop 4",
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );

    assert_line_matches_bytecode(
        "[MP2,4 a b : [ MP0 c : d ]2 ]3 d",
        &[
            "start_loop",
            "set_vibrato 1 4",
            "play_note a4 24",
            "play_note b4 24",
            "skip_last_loop",
            "start_loop",
            "set_vibrato_depth_and_play_note 0 c4 24",
            "skip_last_loop",
            "play_note d4 24",
            "end_loop 2",
            "end_loop 3",
            "set_vibrato_depth_and_play_note 0 d4 24",
        ],
    );
}

/// Test if `last_slurred_note` is correctly tracked after a *skip last loop* command.
/// Assumes `test_portamento()` passes
#[test]
fn test_skip_last_loop_prev_slurred_note() {
    assert_line_matches_bytecode(
        "[d& : b]4 {df},,10",
        &[
            "start_loop",
            "play_note d4 no_keyoff 24",
            "skip_last_loop",
            "play_note b4 24",
            "end_loop 4",
            // Previous slurred note is d4
            "portamento f4 keyoff +10 24",
        ],
    );

    assert_line_matches_bytecode(
        "[d& : [b : c]2 ]3 {df},,10",
        &[
            "start_loop",
            "play_note d4 no_keyoff 24",
            "skip_last_loop",
            "start_loop",
            "play_note b4 24",
            "skip_last_loop",
            "play_note c4 24",
            "end_loop 2",
            "end_loop 3",
            // Previous slurred note is d4
            "portamento f4 keyoff +10 24",
        ],
    );
}

/// Test if `last_slurred_note` is correctly tracked after a *skip last loop* command.
/// Assumes `test_portamento()` passes
#[test]
fn test_skip_last_loop_prev_slurred_note_2() {
    assert_line_matches_bytecode(
        "[w16 : d&]4 {df},,10",
        &[
            "start_loop",
            "rest 6",
            "skip_last_loop",
            "play_note d4 no_keyoff 24",
            "end_loop 4",
            // Previous slurred note is d4
            "portamento f4 keyoff +10 24",
        ],
    );

    assert_line_matches_bytecode(
        "[w : [w : d&]2 ]3 {df},,10",
        &[
            "start_loop",
            "rest 24",
            "skip_last_loop",
            "start_loop",
            "rest 24",
            "skip_last_loop",
            "play_note d4 no_keyoff 24",
            "end_loop 2",
            "end_loop 3",
            // Previous slurred note is d4
            "portamento f4 keyoff +10 24",
        ],
    );
}

/// Test the first portamento after a `start_loop` does not use prev_slurred_note
/// Assumes `test_portamento()` passes
#[test]
fn test_prev_slurred_note_after_start_loop() {
    assert_line_matches_bytecode(
        "d& [{df},,10 c]3",
        &[
            "play_note d4 no_keyoff 24",
            "start_loop",
            // Previous slurred note is unknown
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 23",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "d& [w16]4 {df},,10",
        &[
            "play_note d4 no_keyoff 24",
            "start_loop",
            "rest 6",
            "end_loop 4",
            // Previous slurred note is unknown
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 23",
        ],
    );

    assert_line_matches_bytecode(
        "d& [w16 : w]4 {df},,10",
        &[
            "play_note d4 no_keyoff 24",
            "start_loop",
            "rest 6",
            "skip_last_loop",
            "rest 24",
            "end_loop 4",
            // Previous slurred note is unknown
            "play_note d4 no_keyoff 1",
            "portamento f4 keyoff +10 23",
        ],
    );
}

#[test]
fn test_set_instrument_start_of_loop() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [@1 a]3 @1 b
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 a [@1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [[a]2 @1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument
@2 inst_with_adsr

A @1 [[@1 a]2 @1 b @2 c]3
"##,
        &[
            "set_instrument dummy_instrument",
            "start_loop",
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "end_loop 2",
            // no set_instrument instruction
            "play_note b4 24",
            "set_instrument inst_with_adsr",
            "play_note c4 24",
            "end_loop 3",
        ],
    );
}

#[test]
fn test_set_adsr_start_of_loop() {
    assert_line_matches_bytecode(
        "A1,2,3,4 [A1,2,3,4 a]3 b",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [A1,2,3,4 a A5,6,7,8 b]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "set_adsr 5 6 7 8",
            "play_note b4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [[a]2 A1,2,3,4 b A5,6,7,8 c]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_adsr 1 2 3 4",
            "play_note b4 24",
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "A1,2,3,4 [[A1,2,3,4 a]2 A1,2,3,4 b A5,6,7,8 c]3",
        &[
            "set_adsr 1 2 3 4",
            "start_loop",
            "start_loop",
            "set_adsr 1 2 3 4",
            "play_note a4 24",
            "end_loop 2",
            // no set_adsr instruction
            "play_note b4 24",
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            "end_loop 3",
        ],
    );
}

#[test]
fn test_set_gain_start_of_loop() {
    assert_line_matches_bytecode(
        "G10 [G10 a]3 b",
        &[
            "set_gain 10",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "end_loop 3",
            "play_note b4 24",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [G10 a G20 b]3",
        &[
            "set_gain 10",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "set_gain 20",
            "play_note b4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [[a]2 G10 b G20 c]3",
        &[
            "set_gain 10",
            "start_loop",
            "start_loop",
            "play_note a4 24",
            "end_loop 2",
            "set_gain 10",
            "play_note b4 24",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 3",
        ],
    );

    assert_line_matches_bytecode(
        "G10 [[G10 a]2 G10 b G20 c]3",
        &[
            "set_gain 10",
            "start_loop",
            "start_loop",
            "set_gain 10",
            "play_note a4 24",
            "end_loop 2",
            // no set_gain instruction
            "play_note b4 24",
            "set_gain 20",
            "play_note c4 24",
            "end_loop 3",
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
        &[
            "set_instrument dummy_instrument",
            "rest_keyoff 24",
            "rest_keyoff 72",
        ],
    );
}

#[test]
fn test_change_whole_note_length_command() {
    assert_line_matches_bytecode(
        "c d16 C192 c d16",
        &[
            "play_note c4 24",
            "play_note d4  6",
            "play_note c4 48",
            "play_note d4 12",
        ],
    );
}

/// Tests the merge instrument/envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn test_set_instrument_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument    adsr 1 2 3 4
@1 dummy_instrument_2  adsr 5 6 7 8

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument_and_adsr dummy_instrument_2 5 6 7 8",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument    gain I10
@1 dummy_instrument_2  gain E20

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument_and_gain dummy_instrument I10",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument_and_gain dummy_instrument_2 E20",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  adsr 1 2 3 4
@1 dummy_instrument  adsr 5 6 7 8
@2 dummy_instrument_2

A @0 a @0 b L @1 c @1 d @2 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND adsr after loop)
            "set_instrument_and_adsr dummy_instrument 5 6 7 8",
            "play_note c4 24",
            // @1 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  adsr 1 2 3 4
@1 dummy_instrument  adsr 5 6 7 8
@2 dummy_instrument_2

A @0 a @0 b L A 5,6,7,8 c @1 d @1 e @2 f
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND adsr after loop)
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            // Instrument is unknown after loop
            "set_instrument_and_adsr dummy_instrument 5 6 7 8",
            "play_note d4 24",
            // @1 optimised out
            "play_note e4 24",
            "set_instrument dummy_instrument_2",
            "play_note f4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  gain I10
@1 dummy_instrument  gain E20
@2 dummy_instrument_2

A @0 a @0 b L @1 c @1 d @2 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND gain after loop)
            "set_instrument_and_gain dummy_instrument E20",
            "play_note c4 24",
            // @1 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  gain I10
@1 dummy_instrument  gain E20
@2 dummy_instrument_2

A @0 a @0 b L GI10 c @1 d @1 e @2 f
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND gain after loop)
            "set_gain I10",
            "play_note c4 24",
            // Instrument unknwon after loop
            "set_instrument_and_gain dummy_instrument E20",
            "play_note d4 24",
            // @1 optimised out
            "play_note e4 24",
            "set_instrument dummy_instrument_2",
            "play_note f4 24",
        ],
    );
}

/// Tests the merge ADSR envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn test_set_adsr_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument adsr 1 2 3 4

A @0 a A1,2,3,4 b L A1,2,3,4 c A1,2,3,4 d A5,6,7,8 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // A1,2,3,4 optimised out
            "play_note b4 24",
            // Loop
            "set_adsr 1 2 3 4",
            "play_note c4 24",
            // A1,2,3,4 optimised out
            "play_note d4 24",
            "set_adsr 5 6 7 8",
            "play_note e4 24",
        ],
    );
}

/// Tests the merge GAIN envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn test_set_gain_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument gain E24

A @0 a GE24 b L GE24 c GE24 d GF127 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument E24",
            "play_note a4 24",
            // GE24 optimised out
            "play_note b4 24",
            // Loop
            "set_gain E24",
            "play_note c4 24",
            // GE24 optimised out
            "play_note d4 24",
            "set_gain F127",
            "play_note e4 24",
        ],
    );
}

#[test]
fn test_set_loop_point_in_loop_is_err() {
    assert_error_in_mml_line("[a b L c]5", 6, MmlError::CannotSetLoopPointInALoop);
}

// ----------------------------------------------------------------------------------------------

/// Tests MML commands will still be merged if there are a change MML state command in between
/// the two commands I want to test.
fn merge_mml_commands_test(mml_line: &str, bc_asm: &[&str]) {
    // The inc/dec octave commands must return to the original octave
    // The transpose commands must return to a transpose of 0
    const MML_TO_INSERT: [&str; 10] = [
        "", "l4", "o4", "> <", "> <", "_+2 __-2", "_-4 __+4", "|", "| | | |",
        // Newline
        "\nA ",
    ];
    const MATCH_SYMBOL: &str = "||";

    if mml_line.matches(MATCH_SYMBOL).count() != 1 {
        panic!("mml_line requires at ONE {MATCH_SYMBOL}");
    }

    for command in MML_TO_INSERT {
        let ml = mml_line.replace(MATCH_SYMBOL, command);
        assert_line_matches_bytecode(&ml, bc_asm);
    }
}

// ----------------------------------------------------------------------------------------------

fn get_subroutine_and_bytecode<'a>(
    sd: &'a SongData,
    name: &str,
) -> Option<(&'a Subroutine, &'a [u8])> {
    let s = sd
        .subroutines()
        .iter()
        .find(|s| s.identifier.as_str() == name)?;

    let end = match sd.subroutines().get(s.subroutine_id.as_usize() + 1) {
        Some(s) => s.bytecode_offset,
        None => {
            sd.channels()
                .iter()
                .flatten()
                .next()
                .unwrap()
                .bytecode_offset
        }
    };

    let bc_range = usize::from(s.bytecode_offset)..usize::from(end);

    Some((s, &sd.data()[bc_range]))
}

fn mml_bytecode(mml: &SongData) -> &[u8] {
    let song_data = mml.data();

    let start: usize = mml.channels()[0].as_ref().unwrap().bytecode_offset.into();

    let end = match &mml.channels()[1] {
        Some(c) => c.bytecode_offset.into(),
        None => song_data.len(),
    };

    &song_data[start..end]
}

fn assert_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml = compile_mml(&mml, &dd);
    let bc_asm = assemble_channel_bytecode(
        &bc_asm,
        &dd.instruments_and_samples,
        &[],
        BcTerminator::DisableChannel,
    );

    assert_eq!(
        mml_bytecode(&mml),
        bc_asm,
        "Testing {mml_line:?} against bytecode"
    );
}

fn assert_line_matches_line(mml_line1: &str, mml_line2: &str) {
    let mml1 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line1].concat();
    let mml2 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line2].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    assert_eq!(
        mml_bytecode(&mml_data1),
        mml_bytecode(&mml_data2),
        "Testing {mml_line1:?} against MML"
    );
}

fn assert_line_matches_line_and_bytecode(mml_line1: &str, mml_line2: &str, bc_asm: &[&str]) {
    let mml1 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line1].concat();
    let mml2 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line2].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    let mml1_bc = mml_bytecode(&mml_data1);

    assert_eq!(
        mml1_bc,
        mml_bytecode(&mml_data2),
        "Testing {mml_line1:?} against MML"
    );

    let bc_asm = assemble_channel_bytecode(
        &bc_asm,
        &dd.instruments_and_samples,
        &[],
        BcTerminator::DisableChannel,
    );

    assert_eq!(mml1_bc, bc_asm, "Testing {mml_line1:?} against bytecode");
}

fn assert_mml_channel_a_matches_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data.instruments_and_samples,
        mml.subroutines(),
        BcTerminator::DisableChannel,
    );

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

fn assert_mml_channel_a_matches_looping_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let channel_a = mml.channels()[0].as_ref().unwrap();

    let loop_point = match channel_a.loop_point {
        Some(lp) => lp.bytecode_offset - usize::from(channel_a.bytecode_offset),
        None => panic!("No loop point in MML"),
    };

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data.instruments_and_samples,
        mml.subroutines(),
        BcTerminator::Goto(loop_point),
    );

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

fn assert_error_in_mml_line(mml_line: &str, line_char: u32, expected_error: MmlError) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    assert_err_in_channel_a_mml(&mml, line_char + 2, expected_error);
}

fn assert_err_in_channel_a_mml(mml: &str, line_char: u32, expected_error: MmlError) {
    let dummy_data = dummy_data();

    let r = mml::compile_mml(
        &TextFile {
            contents: mml.to_string(),
            path: None,
            file_name: "".to_owned(),
        },
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let valid = match &r {
        Err(SongError::MmlError(e)) => {
            if e.channel_errors.len() == 1
                && e.line_errors.is_empty()
                && e.subroutine_errors.is_empty()
            {
                let c = e.channel_errors.first().unwrap();
                match c.errors.len() {
                    1 => {
                        let e = c.errors.first().unwrap();
                        e.0.line_char() == line_char && e.1 == expected_error
                    }
                    _ => false,
                }
            } else {
                false
            }
        }
        _ => false,
    };

    if !valid {
        panic!("expected a single {expected_error:?} error on line_char {line_char}\nInput: {mml:?}\nResult: {r:?}")
    }
}

fn assert_subroutine_err_in_mml(mml: &str, expected_errors: &[(&str, &[MmlError])]) {
    let dummy_data = dummy_data();

    let r = mml::compile_mml(
        &TextFile {
            contents: mml.to_string(),
            path: None,
            file_name: "".to_owned(),
        },
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let matches_err = match &r {
        Err(SongError::MmlError(e)) => {
            e.subroutine_errors.len() == expected_errors.len()
                && e.subroutine_errors.iter().all(|subroutine_error| {
                    let expected = expected_errors
                        .iter()
                        .find(|e| e.0 == subroutine_error.identifier.as_str());
                    match expected {
                        Some((_id, expected)) => {
                            let iter1 = subroutine_error.errors.iter().map(|e| &e.1);
                            let iter2 = expected.iter();

                            expected.len() == subroutine_error.errors.len()
                                && iter1.zip(iter2).all(|(i1, i2)| i1 == i2)
                        }
                        None => false,
                    }
                })
        }
        _ => false,
    };

    if !matches_err {
        panic!("Subroutine error mismatch:\nInput: {mml:?}\nExpected: {expected_errors:?}\nResult: {r:?}")
    }
}

fn compile_mml(mml: &str, dummy_data: &DummyData) -> SongData {
    mml::compile_mml(
        &TextFile {
            contents: mml.to_string(),
            path: None,
            file_name: "".to_owned(),
        },
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    )
    .unwrap()
}

fn assemble_channel_bytecode(
    bc_asm: &[&str],
    inst_map: &UniqueNamesList<data::InstrumentOrSample>,
    subroutines: &[Subroutine],
    terminator: BcTerminator,
) -> Vec<u8> {
    let subroutines = subroutines
        .iter()
        .map(|s| (s.identifier.as_str(), s.subroutine_id))
        .collect();

    let mut bc = bytecode_assembler::BytecodeAssembler::new(
        inst_map,
        Some(&subroutines),
        bytecode_assembler::BytecodeContext::SongChannel,
    );

    for line in bc_asm {
        bc.parse_line(line).unwrap();
    }

    bc.bytecode(terminator).unwrap().to_owned()
}

struct DummyData {
    instruments_and_samples: UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: PitchTable,
}

fn dummy_data() -> DummyData {
    const SF: f64 = SAMPLE_FREQ;

    #[rustfmt::skip]
    let instruments_and_samples = data::validate_instrument_and_sample_names([
        dummy_instrument("dummy_instrument", SF, 2, 6, Envelope::Gain(Gain::new(0))),
        dummy_instrument("dummy_instrument_2", SF, 2, 6, Envelope::Gain(Gain::new(0))),
        dummy_instrument("inst_with_adsr",   SF, 2, 6, Envelope::Adsr(EXAMPLE_ADSR)),
        dummy_instrument("inst_with_gain",   SF, 2, 6, Envelope::Gain(EXAMPLE_GAIN)),
    ].iter(),
        [
            data::Sample{
                name: "sample".parse().unwrap(),
                source: Default::default(),
                loop_setting: data::LoopSetting::None,
                sample_rates: vec![32000, 16000, 18000],
                envelope: Envelope::Gain(EXAMPLE_GAIN),
                comment: None,
            },
        ].iter(),
    ).unwrap();

    let pitch_table = build_pitch_table(&instruments_and_samples).unwrap();

    DummyData {
        instruments_and_samples,
        pitch_table,
    }
}

fn dummy_instrument(
    name: &str,
    freq: f64,
    first_octave: u32,
    last_octave: u32,
    envelope: Envelope,
) -> data::Instrument {
    data::Instrument {
        name: Name::try_from(name.to_owned()).unwrap(),
        source: Default::default(),
        freq,
        loop_setting: data::LoopSetting::LoopWithFilter(0),
        first_octave: Octave::try_new(first_octave).unwrap(),
        last_octave: Octave::try_new(last_octave).unwrap(),
        envelope,
        comment: None,
    }
}
