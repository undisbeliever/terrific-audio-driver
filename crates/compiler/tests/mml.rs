//! MML tests

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::bytecode_assembler;
use compiler::data;
use compiler::data::{Name, TextFile, UniqueNamesList};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::mml;
use compiler::notes::Octave;
use compiler::pitch_table::{build_pitch_table, PitchTable};
use compiler::songs::SongData;

const SAMPLE_FREQ: f64 = 500.0;

const EXAMPLE_ADSR_STR: &str = "12 1 1 16";
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
A @0 @g1 @g2 @g3
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

fn mml_bytecode(mml: &SongData) -> &[u8] {
    let song_data = mml.data();

    let start = mml.channels()[0].bytecode_offset.into();

    let end = if mml.channels().len() == 1 {
        song_data.len()
    } else {
        mml.channels()[1].bytecode_offset.into()
    };

    &song_data[start..end]
}

fn assert_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml = compile_mml(&mml, &dd);
    let bc_asm = assemble_channel_bytecode(&bc_asm, &dd.instruments);

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

fn assert_mml_channel_a_matches_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(bc_asm, &dummy_data.instruments);

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

fn compile_mml(mml: &str, dummy_data: &DummyData) -> SongData {
    mml::compile_mml(
        &TextFile {
            contents: mml.to_string(),
            path: None,
            file_name: "".to_owned(),
        },
        None,
        &dummy_data.instruments,
        &dummy_data.pitch_table,
    )
    .unwrap()
}

fn assemble_channel_bytecode(
    bc_asm: &[&str],
    instruments: &UniqueNamesList<data::Instrument>,
) -> Vec<u8> {
    let mut bc = bytecode_assembler::BytecodeAssembler::new(
        instruments,
        None,
        bytecode_assembler::BytecodeContext::SongChannel,
    );

    for line in bc_asm {
        bc.parse_line(line).unwrap();
    }

    bc.bytecode(bytecode_assembler::BcTerminator::DisableChannel)
        .unwrap()
        .to_owned()
}

struct DummyData {
    instruments: UniqueNamesList<data::Instrument>,
    pitch_table: PitchTable,
}

fn dummy_data() -> DummyData {
    const SF: f64 = SAMPLE_FREQ;

    #[rustfmt::skip]
    let instruments = data::validate_instrument_names(vec![
        dummy_instrument("dummy_instrument", SF, 2, 6, Envelope::Gain(Gain::new(0))),
        dummy_instrument("inst_with_adsr",   SF, 2, 6, Envelope::Adsr(EXAMPLE_ADSR)),
        dummy_instrument("inst_with_gain",   SF, 2, 6, Envelope::Gain(EXAMPLE_GAIN)),
    ]).unwrap();

    let pitch_table = build_pitch_table(&instruments).unwrap();

    DummyData {
        instruments,
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
