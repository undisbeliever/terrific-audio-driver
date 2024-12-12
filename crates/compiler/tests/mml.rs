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
use compiler::errors::{BytecodeError, ChannelError, SongError, ValueError};
use compiler::mml;
use compiler::notes::{Note, Octave};
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
fn test_play_pitch() {
    assert_line_matches_bytecode("P$1000", &["play_pitch $1000 keyoff 24"]);
    assert_line_matches_bytecode("l8 P$2000", &["play_pitch $2000 keyoff 12"]);

    assert_line_matches_bytecode("P256 &", &["play_pitch 256 no_keyoff 24"]);

    assert_line_matches_bytecode("P600,1", &["play_pitch 600 96"]);
    assert_line_matches_bytecode("P200,16", &["play_pitch 200 6"]);

    assert_line_matches_bytecode("P20,4..", &["play_pitch 20 42"]);
    assert_line_matches_bytecode("P20,4...", &["play_pitch 20 45"]);
    assert_line_matches_bytecode("P20...", &["play_pitch 20 45"]);

    assert_line_matches_bytecode(
        "P$1000,%600",
        &["play_pitch $1000 no_keyoff 256", "wait 87", "rest 257"],
    );
    assert_line_matches_bytecode(
        "P$1000,%600 &",
        &["play_pitch $1000 no_keyoff 256", "wait 256", "wait 88"],
    );

    assert_line_matches_bytecode("P$1000 r r r", &["play_pitch $1000 keyoff 24", "rest 72"]);
    assert_line_matches_bytecode(
        "P$1000 & r r",
        &["play_pitch $1000 no_keyoff 24", "rest 24", "rest 24"],
    );

    assert_line_matches_bytecode("Q3 P$1000,%80", &["play_pitch $1000 keyoff 31", "rest 49"]);

    assert_line_matches_bytecode(
        "Q3 P$1000,%80 r%50 r%50",
        &["play_pitch $1000 keyoff 31", "rest 149"],
    );

    assert_line_matches_bytecode(
        "Q3,D8 P$1000,%80 r%10 r%20",
        &[
            "play_pitch $1000 no_keyoff 30",
            "set_temp_gain_and_rest D8 50",
            "rest 30",
        ],
    );

    // Q not applied to slurred notes
    assert_line_matches_bytecode(
        "Q3 P$1000,%80 & r%10 r%20",
        &["play_pitch $1000 no_keyoff 80", "rest 10", "rest 20"],
    );

    assert_line_matches_bytecode("P0", &["play_pitch 0 keyoff 24"]);
    assert_line_matches_bytecode("P$3fff", &["play_pitch 16383 keyoff 24"]);

    assert_error_in_mml_line(
        "P$4000",
        1,
        ValueError::PlayPitchPitchOutOfRange(0x4000).into(),
    );

    assert_error_in_mml_line("P,4", 1, ValueError::NoPlayPitchPitch.into());

    assert_error_in_mml_line("P c", 1, ValueError::NoPlayPitchPitch.into());

    assert_error_in_mml_line("P$1000,", 7, ChannelError::NoLengthAfterComma);
}

#[test]
fn test_play_noise() {
    assert_line_matches_bytecode("N15", &["play_noise 15 keyoff 24"]);
    assert_line_matches_bytecode("N15 &", &["play_noise 15 no_keyoff 24"]);

    assert_line_matches_bytecode("N16,1", &["play_noise 16 keyoff 96"]);
    assert_line_matches_bytecode("N16,16", &["play_noise 16 keyoff 6"]);

    assert_line_matches_bytecode("N17,4..", &["play_noise 17 keyoff 42"]);
    assert_line_matches_bytecode("N18,4...", &["play_noise 18 keyoff 45"]);
    assert_line_matches_bytecode("N19...", &["play_noise 19 keyoff 45"]);

    assert_line_matches_bytecode("N$0c", &["play_noise 12 keyoff 24"]);
    assert_line_matches_bytecode("N$1f,2", &["play_noise 31 keyoff 48"]);

    assert_line_matches_bytecode(
        "N1,%600",
        &["play_noise 1 no_keyoff 256", "wait 87", "rest 257"],
    );
    assert_line_matches_bytecode(
        "N2,%600 &",
        &["play_noise 2 no_keyoff 256", "wait 256", "wait 88"],
    );

    assert_line_matches_bytecode("N30 r r r", &["play_noise 30 keyoff 24", "rest 72"]);
    assert_line_matches_bytecode(
        "N15 & r r",
        &["play_noise 15 no_keyoff 24", "rest 24", "rest 24"],
    );

    assert_line_matches_bytecode("Q3 N8,%80", &["play_noise 8 keyoff 31", "rest 49"]);

    assert_line_matches_bytecode(
        "Q3 N5,%80 r%50 r%50",
        &["play_noise 5 keyoff 31", "rest 149"],
    );

    assert_line_matches_bytecode(
        "Q3,D8 N10,%80 r%10 r%20",
        &[
            "play_noise 10 no_keyoff 30",
            "set_temp_gain_and_rest D8 50",
            "rest 30",
        ],
    );

    // Q not applied to slurred notes
    assert_line_matches_bytecode(
        "Q3 N20,%80 & r%10 r%20",
        &["play_noise 20 no_keyoff 80", "rest 10", "rest 20"],
    );

    assert_line_matches_bytecode("N0", &["play_noise 0 keyoff 24"]);
    assert_line_matches_bytecode("N31", &["play_noise 31 keyoff 24"]);

    assert_error_in_mml_line("N32", 1, ValueError::NoiseFrequencyOutOfRange(32).into());

    assert_error_in_mml_line("N,4", 1, ValueError::NoNoiseFrequency.into());

    assert_error_in_mml_line("N c", 1, ValueError::NoNoiseFrequency.into());

    assert_error_in_mml_line("N5,", 3, ChannelError::NoLengthAfterComma);
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
fn test_too_many_accidentals() {
    let lots_of_plusses = "+".repeat(1024);
    let lots_of_minuses = "+".repeat(1024);

    assert_error_in_mml_line(
        &format!("c{lots_of_plusses}"),
        1,
        ValueError::InvalidNote.into(),
    );

    assert_error_in_mml_line(
        &format!("c{lots_of_minuses}"),
        1,
        ValueError::InvalidNote.into(),
    );

    assert_error_in_mml_line(
        &format!("c{lots_of_plusses}{lots_of_minuses}"),
        1,
        ValueError::InvalidNote.into(),
    );
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
fn test_lots_of_dots_in_length() {
    // Test "attempt to add with overflow" panic is fixed
    let lots_of_dots = ".".repeat(512);
    assert_line_matches_line(&format!("l1{lots_of_dots} a"), "a%190");
    assert_line_matches_line(&format!("a{lots_of_dots}"), "a%46");
    assert_line_matches_line(&format!("a ^{lots_of_dots}"), "a%70");
    assert_line_matches_line(&format!("a &{lots_of_dots}"), "a%70");
    assert_line_matches_line(&format!("r2{lots_of_dots}"), "r%94");
    assert_line_matches_line(&format!("w3{lots_of_dots}"), "w%63");
    assert_line_matches_line(
        &format!("{{ab}}1{lots_of_dots},2{lots_of_dots}"),
        "{ab}%190,%94",
    );
    assert_line_matches_line(
        &format!("{{{{ab}}}}1{lots_of_dots},8{lots_of_dots}"),
        "{{ab}}%190,%22",
    );

    // Confirm %255 the largest default length in ticks
    assert_error_in_mml_line("l%256", 1, ValueError::InvalidDefaultLength.into());

    assert_line_matches_line(
        "l%255 a...................................................................................",
        "a%502"
    );

    // Confirm 255 is the largest ZenLen
    assert_error_in_mml_line("C256", 1, ValueError::ZenLenOutOfRange(256).into());

    assert_line_matches_line(
        "C255 a1...................................................................................",
        "a%502",
    );

    assert_line_matches_line(
        "C255 l1................................................................................... a....................................................................................",
        "a%997"
    );

    assert_line_matches_line(
        "C255 l1.............................................. {ab}%1024,1.....................................................",
        "{ab}%1024,%502",
    );

    assert_line_matches_line(
        "C255 l1.............................................. {ab}%1024,.....................................................",
        "{ab}%1024,%997",
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
fn test_quantization() {
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
fn test_fine_quantisation() {
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
fn bugfix_quantization_of_short_note_then_rest() {
    // The rest notes were erroniously dropped and ignored

    assert_line_matches_bytecode("Q8 c%6 r%6", &["play_note c4 6", "rest 6"]);

    assert_line_matches_bytecode("Q%255  c%100 r%100", &["play_note c4 100", "rest 100"]);
}

#[test]
fn bugfix_quantization_of_1_tick_note_panic() {
    assert_error_in_mml_line("Q4 c%1", 4, ChannelError::NoteIsTooShort);
}

#[test]
fn test_quantize_with_temp_gain() {
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
fn test_quantise_comma_0_gain_is_err() {
    assert_error_in_mml_line("Q2,0", 4, ValueError::OptionalGainCannotBeZero.into());
    assert_error_in_mml_line("Q2,F0", 4, ValueError::OptionalGainCannotBeZero.into());
}

#[test]
fn test_early_release() {
    assert_line_matches_bytecode("q0", &["disable_early_release"]);
    assert_line_matches_bytecode("q10", &["set_early_release 10 1"]);
    assert_line_matches_bytecode("q16,D15", &["set_early_release 16 1 D15"]);
}

#[test]
fn test_early_release_deduplication() {
    assert_line_matches_line("q0 q0 q0", "q0");

    assert_line_matches_line(
        "q0 q20 q20 q21 q15,I10 q15,I20 q15,I20 q0",
        "q0 q20 q21 q15,I10 q15,I20 q0",
    );

    assert_line_matches_bytecode(
        "q3,D5 [a]5 q3,D8",
        &[
            "set_early_release 3 1 D5",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            "set_early_release 3 1 D8",
        ],
    );

    assert_line_matches_bytecode(
        "q1 [a]5 q1",
        &[
            "set_early_release 1 1",
            "start_loop",
            "play_note a4 24",
            "end_loop 5",
            // q1 deduplicated
        ],
    );

    assert_line_matches_bytecode(
        "q1 [q1 a : q10 b]5 q2",
        &[
            "set_early_release 1 1",
            "start_loop",
            "set_early_release 1 1",
            "play_note a4 24",
            "skip_last_loop",
            "set_early_release 10 1",
            "play_note b4 24",
            "end_loop 5",
            "set_early_release 2 1",
        ],
    );

    assert_line_matches_bytecode(
        "q1 [q1 a : q10 b]5 q1",
        &[
            "set_early_release 1 1",
            "start_loop",
            "set_early_release 1 1",
            "play_note a4 24",
            "skip_last_loop",
            "set_early_release 10 1",
            "play_note b4 24",
            "end_loop 5",
            // q1 deduplicated
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s q8,E20 a

A @1 !s q10,E15 a
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "set_early_release 10 1 E15",
            "play_note a4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@1 dummy_instrument

!s q8,E20 a

A @1 !s q8,E20 a
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            // q8,E20 deduplicated
            "play_note a4 24",
        ],
    );
}

#[test]
fn test_early_release_with_blank_min_ticks() {
    assert_line_matches_bytecode("q10,,D15", &["set_early_release 10 1 D15"]);
}

#[test]
fn test_early_release_with_min_ticks() {
    assert_line_matches_bytecode("q10,6", &["set_early_release 10 6"]);
    assert_line_matches_bytecode("q16,6,D15", &["set_early_release 16 6 D15"]);
}

#[test]
fn test_early_release_with_min_ticks_deduplication() {
    assert_line_matches_line("q10,6 q10,6 q10,6", "q10,6");
    assert_line_matches_line("q20,8,D15 q20,8,D15 q20,8,D15", "q20,8,D15");

    assert_line_matches_line("q8,E24 q8,1,E24", "q8,E24");
    assert_line_matches_line("q8,1,E24 q8,E24", "q8,1,E24");

    // No need to test everything.  1, 2 & 3 argument
    // This test assumes test_early_release_deduplication() passes.
}

#[test]
fn test_early_release_raw_gain_is_err() {
    assert_error_in_mml_line("q10,,15", 5, ValueError::NoOptionalGainMode.into());
    assert_error_in_mml_line("q10,2,15", 6, ValueError::NoOptionalGainMode.into());
}

#[test]
fn test_early_release_comma_f0_gain_is_err() {
    assert_error_in_mml_line("q10,F0", 5, ValueError::OptionalGainCannotBeZero.into());
    assert_error_in_mml_line("q10,2,F0", 7, ValueError::OptionalGainCannotBeZero.into());
}

#[test]
fn play_long_note() {
    // `wait` can rest for 1 to 256 ticks.
    // `rest` can rest for 2 to 257 tick.
    // The last rest in a wait-rest chain must be 257 ticks to prevent interference with early-release

    assert_line_matches_bytecode("a%256", &["play_note a4 keyoff 256"]);
    assert_line_matches_bytecode("a%257", &["play_note a4 keyoff 257"]);

    assert_line_matches_bytecode("a%258", &["play_note a4 no_keyoff 1", "rest 257"]);

    assert_line_matches_bytecode("a%512", &["play_note a4 no_keyoff 255", "rest 257"]);
    assert_line_matches_bytecode("a%513", &["play_note a4 no_keyoff 256", "rest 257"]);
    assert_line_matches_bytecode(
        "a%514",
        &["play_note a4 no_keyoff 256", "wait 1", "rest 257"],
    );

    assert_line_matches_bytecode(
        "a%600",
        &["play_note a4 no_keyoff 256", "wait 87", "rest 257"],
    );
}

#[test]
fn play_long_slurred_note() {
    // `wait` can rest for 1 to 256 ticks.

    assert_line_matches_bytecode("a%256 &", &["play_note a4 no_keyoff 256"]);
    assert_line_matches_bytecode("a%257 &", &["play_note a4 no_keyoff 256", "wait 1"]);
    assert_line_matches_bytecode("a%258 &", &["play_note a4 no_keyoff 256", "wait 2"]);

    assert_line_matches_bytecode("a%512 &", &["play_note a4 no_keyoff 256", "wait 256"]);
    assert_line_matches_bytecode(
        "a%513 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 1"],
    );
    assert_line_matches_bytecode(
        "a%514 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 2"],
    );

    assert_line_matches_bytecode(
        "a%600 &",
        &["play_note a4 no_keyoff 256", "wait 256", "wait 88"],
    );
}

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
fn test_no_tick_instructions_and_skip_last_loop() {
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
fn test_loop_errors() {
    assert_error_in_mml_line("[ ]3", 3, BytecodeError::NoTicksInLoop.into());
    assert_error_in_mml_line("[ V+5 ]3", 7, BytecodeError::NoTicksInLoop.into());
    assert_error_in_mml_line("[ V+5 : V-5 ]3", 13, BytecodeError::NoTicksInLoop.into());

    assert_error_in_mml_line(
        "[ : c ]3",
        3,
        BytecodeError::NoInstructionsBeforeSkipLastLoop.into(),
    );

    assert_error_in_mml_line(
        "[ c : ]3",
        7,
        BytecodeError::NoInstructionsAfterSkipLastLoop.into(),
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
        ChannelError::BytecodeError(BytecodeError::StackOverflowInStartLoop(8 * 3)),
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
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
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
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
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
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
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

    write!(mml, "A @0 ").unwrap();
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

    write!(mml, "A @0 ").unwrap();
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
            &[ChannelError::CannotCallSubroutineRecursion("s".to_owned())],
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
                &[ChannelError::CannotCallSubroutineRecursion("s2".to_owned())],
            ),
            (
                "s2",
                &[ChannelError::CannotCallSubroutineRecursion("s3".to_owned())],
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
        &[("s", &[ChannelError::CannotFindSubroutine("s2".to_owned())])],
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
            &[ChannelError::CannotFindSubroutine("missing".to_owned())],
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
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s1".to_owned(),
            stack_depth,
        )),
    );
}

#[test]
fn test_subroutine_call_no_instrument_err() {
    assert_err_in_channel_a_mml(
        r##"
!s c d e f

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_err_in_channel_a_mml(
        r##"
!s c d e f

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s a @0 b c d

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 r !s2 r
!s2 a

A !s2
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    // test tail call
    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 r !s2
!s2 a

A !s2
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );
}

#[test]
fn test_subroutine_call_note_range_errors() {
    let inst_range = Note::first_note_for_octave(Octave::try_from(2).unwrap())
        ..=Note::last_note_for_octave(Octave::try_from(6).unwrap());

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o7 f d c e

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("c7")..=note("f7"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o4 f d o7 e c

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("d4")..=note("e7"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o1 a o4 c d

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("d4"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2
!s2 o5 c d e

A @0 !s1
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("e5"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2 o4 d e
!s2 @0 b

A @0 !s1
"##,
        6,
        // Only o1 a is played with an unknown instrument
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("a1"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    // Test tail call
    assert_err_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2
!s2 o4 b

A @0 !s1
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("b4"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );
}

#[test]
fn test_nested_subroutine_no_instrument_bug() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!n1 @0 c d e
!n2 b

!s !n1 !n2

A !s
"##,
        &["call_subroutine s"],
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
fn test_quantized_portamento() {
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

#[test]
fn test_portamento_speed() {
    // Calculate pitch velocity for an `{o4 a o5 a}` portamento slide.
    // `pitch_slide_ticks` does NOT include the key-off tick
    // Using floats to ensure the i32 rounding in bc_generator is correct.
    fn pv(pitch_slide_ticks: u32) -> u32 {
        const A4_PITCH: u32 = 0x0e14;
        const A5_PITCH: u32 = 0x1c29;

        let v = f64::from(A5_PITCH - A4_PITCH) / f64::from(pitch_slide_ticks);

        v.round() as u32
    }

    assert_line_matches_line_and_bytecode(
        "{o4 a > a}%64",
        // -1 for play-note a4, -1 for key-off
        &format!("{{o4 a o5 a}}%64,,{}", pv(64 - 2)),
        &[
            "play_note a4 no_keyoff 1",
            &format!("portamento a5 keyoff +{} 63", pv(64 - 2)),
        ],
    );

    assert_line_matches_line(
        "{o4 a > a}%64,%10",
        // -10 for delay, -1 for key-off
        &format!("{{o4 a o5 a}}%64,%10,{}", pv(64 - 11)),
    );

    assert_line_matches_line(
        "{o4 a > a}%32 & b",
        // -1 for play-note a4, no-key-off
        &format!("{{o4 a o5 a}}%32,,{} & b", pv(32 - 1)),
    );

    assert_line_matches_line(
        "{o4 a > a}%32,%10 & b",
        // -10 for delay, no-key-off
        &format!("{{o4 a o5 a}}%32,%10,{} & b", pv(32 - 10)),
    );

    assert_line_matches_line(
        "{o4 a > a}%32 ^ %32",
        // -1 for play-note a4, no key-off (after pitch slide ends)
        &format!("{{o4 a o5 a}}%64,,{}", pv(32 - 1)),
    );

    assert_line_matches_line(
        "{o4 a > a}%32 & %32 & g",
        // -1 for play-note a4, no key-off
        &format!("{{o4 a o5 a}}%64,,{} & g", pv(32 - 1)),
    );

    assert_line_matches_line(
        "{o4 a > a}%32,%10 ^ %32",
        // -10 for delay
        &format!("{{o4 a o5 a}}%64,%10,{}", pv(32 - 10)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%40",
        // no play-note, -1 for key-off
        &format!("o4 a & {{o4 a o5 a}}%40,,{}", pv(40 - 1)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%40,%15",
        // -15 for play-note delay, -1 for key-off
        &format!("o4 a & {{o4 a o5 a}}%40,%15,{}", pv(40 - 16)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%40 & b",
        // no play-note, no key-off
        &format!("o4 a & {{o4 a o5 a}}%40,,{} & b", pv(40)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%40,%15 & b",
        // -15 for play-note delay, no key-off
        &format!("o4 a & {{o4 a o5 a}}%40,%15,{} & b", pv(40 - 15)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%60,%30 ^ %40",
        // -30 for delay
        &format!("o4 a & {{o4 a o5 a}}%100,%30,{}", pv(60 - 30)),
    );

    assert_line_matches_line(
        "o4 a & {o4 a > a}%60,%30 ^ %40 & g",
        // -30 for delay
        &format!("o4 a & {{o4 a o5 a}}%100,%30,{} & g", pv(60 - 30)),
    );

    assert_line_matches_line(
        "e & {o4 a > a}%100",
        // -1 for play-note, -1 for key-off
        &format!("e & {{o4 a o5 a}}%100,,{}", pv(100 - 2)),
    );

    assert_line_matches_line(
        "e & {o4 a > a}%100,%20",
        // -20 for play-note delay, -1 for key-off
        &format!("e & {{o4 a o5 a}}%100,%20,{}", pv(100 - 21)),
    );

    assert_line_matches_line(
        "e & {o4 a > a}%100,%20 ^ %40",
        // -20 for delay
        &format!("e & {{o4 a o5 a}}%140,%20,{}", pv(100 - 20)),
    );

    // Testing negative portamento velocity
    assert_line_matches_line_and_bytecode(
        "{o5 a < a}%64",
        // -1 for play-note a4, -1 for key-off
        &format!("{{o5 a o4 a}}%64,,{}", pv(64 - 2)),
        &[
            "play_note a5 no_keyoff 1",
            &format!("portamento a4 keyoff -{} 63", pv(64 - 2)),
        ],
    );

    assert_line_matches_line(
        "o5 a & {o5 a o4 a}%40 & b",
        // no play-note, no key-off
        &format!("o5 a & {{o5 a < a}}%40,,{} & b", pv(40)),
    );

    assert_line_matches_line(
        "e & {o5 a o4 a}%100,%20",
        // -20 for play-note delay, -1 for key-off
        &format!("e & {{o5 a < a}}%100,%20,{}", pv(100 - 21)),
    );
}

#[test]
fn test_portamento_err() {
    assert_error_in_mml_line("{c g}4,4", 8, ChannelError::InvalidPortamentoDelay);
    assert_error_in_mml_line("l2 {c g},2", 10, ChannelError::InvalidPortamentoDelay);

    assert_error_in_mml_line("{c g}4,,0", 1, ValueError::PortamentoVelocityZero.into());
    assert_error_in_mml_line(
        "{c g}4,,800",
        9,
        ValueError::PortamentoSpeedOutOfRange(800).into(),
    );

    assert_error_in_mml_line("{c g}%0", 1, ChannelError::PortamentoTooShort);
    assert_error_in_mml_line("{c g}%1", 1, ChannelError::PortamentoTooShort);
    assert_error_in_mml_line("{c g}%10,%9", 1, ChannelError::PortamentoTooShort);

    assert_error_in_mml_line("{c c}4", 1, ValueError::PortamentoVelocityZero.into());
    assert_error_in_mml_line(
        "{c > c}16",
        1,
        ValueError::PortamentoVelocityOutOfRange(536).into(),
    );

    // Tests if the TryFromIntError panic in ChannelBcGenerator::portamento() has been fixed
    assert_error_in_mml_line("{c g}%$ffffffff", 1, ChannelError::PortamentoTooLong);
    assert_error_in_mml_line("{c g}%16387", 1, ChannelError::PortamentoTooLong);
    assert_error_in_mml_line("c & {c g}%16385 & a", 5, ChannelError::PortamentoTooLong);
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
fn test_mp_vibrato_start_of_loop() {
    assert_line_matches_bytecode(
        "MP40,2 a [a a b MP30,5 c]4",
        &[
            "set_vibrato 42 2",
            "play_note a4 24",
            "start_loop",
            // Vibrato state is unknown
            "set_vibrato 42 2",
            "play_note a4 24",
            // Vibrato unchanged
            "play_note a4 24",
            "set_vibrato_depth_and_play_note 47 b4 24",
            "set_vibrato 7 5",
            "play_note c4 24",
            "end_loop 4",
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
fn test_px_pan() {
    assert_line_matches_bytecode("px0", &["set_pan 64"]);
    assert_line_matches_bytecode("px-64", &["set_pan 0"]);
    assert_line_matches_bytecode("px+64", &["set_pan 128"]);

    assert_error_in_mml_line("px-65", 1, ValueError::PxPanOutOfRange(-65).into());
    assert_error_in_mml_line("px+65", 1, ValueError::PxPanOutOfRange(65).into());

    assert_line_matches_bytecode("px+16", &["set_pan 80"]);
    assert_line_matches_bytecode("px+32", &["set_pan 96"]);
    assert_line_matches_bytecode("px-48", &["set_pan 16"]);
    assert_line_matches_bytecode("px-15", &["set_pan 49"]);
}

#[test]
fn test_merge_pan_px() {
    merge_mml_commands_test("px+1 px-2 || px+3 px-4", &["set_pan 60"]);
    merge_mml_commands_test("px-1 px+2 || px-3 px+4", &["set_pan 68"]);

    merge_mml_commands_test("px-20 || p+5 p+6", &["set_pan 55"]);
    merge_mml_commands_test("px+40 || p+100", &["set_pan 128"]);
    merge_mml_commands_test("px-50 p+5 || px+10", &["set_pan 74"]);
    merge_mml_commands_test("p+1 || px+20 p-40", &["set_pan 44"]);
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
fn test_volume_slide() {
    assert_line_matches_bytecode("vs+2,8", &["volume_slide +32 8"]);
    assert_line_matches_bytecode("vs-4,16", &["volume_slide -$40 16"]);
    assert_line_matches_bytecode("vs+16,20", &["volume_slide +255 20"]);
    assert_line_matches_bytecode("vs-16,20", &["volume_slide -255 20"]);

    assert_line_matches_bytecode("Vs+20,50", &["volume_slide +20 50"]);
    assert_line_matches_bytecode("Vs-30,80", &["volume_slide -30 80"]);

    // 0x10ff / 16 = 0x10f
    assert_line_matches_bytecode_bytes("vs+1,16", &[opcodes::VOLUME_SLIDE_UP, 16, 0x0f, 0x01]);
    // 0x30ff / 32 = 0x187
    assert_line_matches_bytecode_bytes("vs+3,32", &[opcodes::VOLUME_SLIDE_UP, 32, 0x87, 0x01]);
    // 0x31ff / 14 = 0x392
    assert_line_matches_bytecode_bytes("Vs +49,14", &[opcodes::VOLUME_SLIDE_UP, 14, 0x92, 0x03]);
    // (0x100 * 111 + 0xff) / 256 = 111
    assert_line_matches_bytecode_bytes("Vs+111,256", &[opcodes::VOLUME_SLIDE_UP, 0, 111, 0]);

    assert_line_matches_bytecode_bytes("vs-7,1", &[opcodes::VOLUME_SLIDE_DOWN, 1, 0xff, 0x70]);
    // 0x80ff / 40 = 0x339
    assert_line_matches_bytecode_bytes("vs-8,40", &[opcodes::VOLUME_SLIDE_DOWN, 40, 0x39, 0x03]);
    // (0x100 * 200 + 0xff) / 140 = 0x16f
    assert_line_matches_bytecode_bytes(
        "Vs-200,140",
        &[opcodes::VOLUME_SLIDE_DOWN, 140, 0x6f, 0x01],
    );
    // (0x100 * 222 + 0xff) / 256 = 222
    assert_line_matches_bytecode_bytes("Vs -222,256", &[opcodes::VOLUME_SLIDE_DOWN, 0, 222, 0]);

    assert_error_in_mml_line(
        "vs+17,100",
        1,
        ValueError::CoarseVolumeSlideOutOfRange(17).into(),
    );
    assert_error_in_mml_line(
        "Vs+256,100",
        1,
        ValueError::VolumeSlideAmountOutOfRange(256).into(),
    );
    assert_error_in_mml_line(
        "vs-17,100",
        1,
        ValueError::CoarseVolumeSlideOutOfRange(-17).into(),
    );
    assert_error_in_mml_line(
        "Vs-256,100",
        1,
        ValueError::VolumeSlideAmountOutOfRange(-256).into(),
    );

    assert_error_in_mml_line(
        "vs+4,0",
        6,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "Vs+100,0",
        8,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "vs-4,0",
        6,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "Vs-100,0",
        8,
        ValueError::VolumeSlideTicksOutOfRange(0).into(),
    );

    assert_error_in_mml_line(
        "vs+4,257",
        6,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_error_in_mml_line(
        "Vs+100,257",
        8,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_error_in_mml_line(
        "vs-4,257",
        6,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );
    assert_error_in_mml_line(
        "Vs-100,257",
        8,
        ValueError::VolumeSlideTicksOutOfRange(257).into(),
    );

    assert_error_in_mml_line("vs+10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_error_in_mml_line("vs-10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_error_in_mml_line("Vs+10", 1, ValueError::NoCommaVolumeSlideTicks.into());
    assert_error_in_mml_line("Vs-10", 1, ValueError::NoCommaVolumeSlideTicks.into());
}

#[test]
fn test_tremolo() {
    assert_line_matches_bytecode("v~2,4", &["tremolo 32 4"]);
    assert_line_matches_bytecode("v~7,8", &["tremolo 112 8"]);
    assert_line_matches_bytecode("v~8,8", &["tremolo 127 8"]);

    assert_line_matches_bytecode("V~20,10", &["tremolo 20 10"]);
    assert_line_matches_bytecode("V~30,20", &["tremolo 30 20"]);

    // 0x107f / 10 = 0x1a6
    assert_line_matches_bytecode_bytes("v~1,10", &[opcodes::TREMOLO, 10, 0xa6, 0x01]);
    // 0x307f / 8 = 0x60f
    assert_line_matches_bytecode_bytes("v~3,8", &[opcodes::TREMOLO, 8, 0x0f, 0x06]);

    // 0x287f / 6 = 0x6bf
    assert_line_matches_bytecode_bytes("V~40,6", &[opcodes::TREMOLO, 6, 0xbf, 0x06]);

    // 0x7f7f / 127 = 0x101 (largest values)
    assert_line_matches_bytecode_bytes("V~127,127", &[opcodes::TREMOLO, 127, 0x01, 0x01]);

    assert_error_in_mml_line(
        "v~0,10",
        1,
        ValueError::CoarseTremoloAmplitudeOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "v~9,10",
        1,
        ValueError::CoarseTremoloAmplitudeOutOfRange(9).into(),
    );
    assert_error_in_mml_line(
        "V~0,10",
        1,
        ValueError::TremoloAmplitudeOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "V~128,10",
        1,
        ValueError::TremoloAmplitudeOutOfRange(128).into(),
    );

    assert_error_in_mml_line(
        "v~4,0",
        5,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "V~100,0",
        7,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(0).into(),
    );

    assert_error_in_mml_line(
        "v~4,128",
        5,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(128).into(),
    );
    assert_error_in_mml_line(
        "V~100,128",
        7,
        ValueError::TremoloQuarterWavelengthTicksOutOfRange(128).into(),
    );

    assert_error_in_mml_line("v~3", 1, ValueError::NoCommaQuarterWavelength.into());
    assert_error_in_mml_line("V~10", 1, ValueError::NoCommaQuarterWavelength.into());
}

#[test]
fn test_pan_slide() {
    assert_line_matches_bytecode("ps+15,8", &["pan_slide +15 8"]);
    assert_line_matches_bytecode("ps-30,16", &["pan_slide -30 16"]);

    assert_line_matches_bytecode("ps+128,50", &["pan_slide +128 50"]);
    assert_line_matches_bytecode("ps-128,256", &["pan_slide -128 256"]);

    // 0x14ff / 14 = 0x17f
    assert_line_matches_bytecode_bytes("ps +20,14", &[opcodes::PAN_SLIDE_UP, 14, 0x7f, 0x01]);
    // 0x80ff / 60 = 0x226
    assert_line_matches_bytecode_bytes("ps +128,60", &[opcodes::PAN_SLIDE_UP, 60, 0x26, 0x02]);
    // (0x100 * 64 + 0xff) / 256 = 64
    assert_line_matches_bytecode_bytes("ps+64,256", &[opcodes::PAN_SLIDE_UP, 0, 64, 0]);

    // 0x37ff / 10 = 0x599
    assert_line_matches_bytecode_bytes("ps-55,10", &[opcodes::PAN_SLIDE_DOWN, 10, 0x99, 0x05]);
    // (0x100 * 30 + 0xff) / 256 = 30
    assert_line_matches_bytecode_bytes("ps -30,256", &[opcodes::PAN_SLIDE_DOWN, 0, 30, 0]);

    assert_error_in_mml_line(
        "ps+129,100",
        1,
        ValueError::PanSlideAmountOutOfRange(129).into(),
    );
    assert_error_in_mml_line(
        "ps-129,100",
        1,
        ValueError::PanSlideAmountOutOfRange(-129).into(),
    );

    assert_error_in_mml_line("ps+20,0", 7, ValueError::PanSlideTicksOutOfRange(0).into());
    assert_error_in_mml_line("ps-40,0", 7, ValueError::PanSlideTicksOutOfRange(0).into());

    assert_error_in_mml_line(
        "ps+50,257",
        7,
        ValueError::PanSlideTicksOutOfRange(257).into(),
    );
    assert_error_in_mml_line(
        "ps-60,257",
        7,
        ValueError::PanSlideTicksOutOfRange(257).into(),
    );

    assert_error_in_mml_line("ps+10", 1, ValueError::NoCommaPanSlideTicks.into());
    assert_error_in_mml_line("ps-10", 1, ValueError::NoCommaPanSlideTicks.into());
}

#[test]
fn test_panbrello() {
    assert_line_matches_bytecode("p~20,10", &["panbrello 20 10"]);
    assert_line_matches_bytecode("p~30,20", &["panbrello 30 20"]);

    // 0x3c7f / 5 = 0xc19
    assert_line_matches_bytecode_bytes("p~60,5", &[opcodes::PANBRELLO, 5, 0x19, 0x0c]);

    // 0x177f / 17 = 0x161
    assert_line_matches_bytecode_bytes("p~23,17", &[opcodes::PANBRELLO, 17, 0x61, 0x01]);

    // 0x407f / 127 = 0x82 (largest values)
    assert_line_matches_bytecode_bytes("p~64,127", &[opcodes::PANBRELLO, 127, 0x82, 0x00]);

    assert_error_in_mml_line(
        "p~0,10",
        1,
        ValueError::PanbrelloAmplitudeOutOfRange(0).into(),
    );
    assert_error_in_mml_line(
        "p~65,10",
        1,
        ValueError::PanbrelloAmplitudeOutOfRange(65).into(),
    );

    assert_error_in_mml_line(
        "p~10,0",
        6,
        ValueError::PanbrelloQuarterWavelengthTicksOutOfRange(0).into(),
    );

    assert_error_in_mml_line(
        "p~20,128",
        6,
        ValueError::PanbrelloQuarterWavelengthTicksOutOfRange(128).into(),
    );

    assert_error_in_mml_line("p~10", 1, ValueError::NoCommaQuarterWavelength.into());
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
fn test_temp_gain() {
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
fn test_gft0_is_err() {
    assert_error_in_mml_line("GFT0", 1, ValueError::F0TempGain.into());
}

#[test]
fn test_temp_gain_rest() {
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
    assert_line_matches_bytecode("GDT12 r%258", &["set_temp_gain_and_wait D12 1", "rest 257"]);
    assert_line_matches_bytecode("GDT12 r%259", &["set_temp_gain_and_wait D12 2", "rest 257"]);

    assert_line_matches_bytecode(
        "GDT12 r%1000",
        &[
            "set_temp_gain_and_wait D12 256",
            "wait 256",
            "wait 231",
            "rest 257",
        ],
    );

    merge_mml_commands_test(
        "GDT12 r%700 r%200 || r%500",
        &[
            "set_temp_gain_and_wait D12 256",
            "wait 187",
            "rest 257",
            "rest 257",
            "rest 257",
            "rest 186",
        ],
    );

    assert_line_matches_bytecode(
        "GDT12 r%10000 r%2000",
        &[
            "set_temp_gain_and_wait D12 256",
            "start_loop 53",
            "wait 179",
            "end_loop",
            "rest 257",
            // second rest
            "start_loop 8",
            "rest 250",
            "end_loop",
        ],
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
fn test_temp_gain_wait() {
    assert_line_matches_bytecode("GDT12 w", &["set_temp_gain_and_wait D12 24"]);

    merge_mml_commands_test("GDT12 || w8", &["set_temp_gain_and_wait D12 12"]);
    merge_mml_commands_test("GDT12 || w8.", &["set_temp_gain_and_wait D12 18"]);

    merge_mml_commands_test("GDT12 w w || w w", &["set_temp_gain_and_wait D12 96"]);
    merge_mml_commands_test("GDT12 w ^ ^ || w", &["set_temp_gain_and_wait D12 96"]);
    merge_mml_commands_test("GDT12 w || ^1", &["set_temp_gain_and_wait D12 120"]);

    assert_line_matches_bytecode("GDT12 w%256", &["set_temp_gain_and_wait D12 256"]);
    assert_line_matches_bytecode("GDT12 w%257", &["set_temp_gain_and_wait D12 256", "wait 1"]);
    assert_line_matches_bytecode(
        "GDT12 w%1000",
        &[
            "set_temp_gain_and_wait D12 256",
            "wait 256",
            "wait 256",
            "wait 232",
        ],
    );

    assert_line_matches_bytecode(
        "GDT12 w%8000",
        &[
            "set_temp_gain_and_wait D12 256",
            "start_loop 32",
            "wait 242",
            "end_loop",
        ],
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
fn test_reuse_temp_gain() {
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
fn test_set_gain_does_not_affect_temp_gain_reuse() {
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
fn test_reuse_temp_gain_and_wait() {
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
fn test_reuse_temp_gain_and_rest() {
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
fn test_reuse_temp_gain_loop() {
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
fn test_reuse_temp_gain_after_call_subroutine() {
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
fn test_mp0_after_skip_last_loop() {
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
            "wait 6",
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
            "wait 24",
            "skip_last_loop",
            "start_loop",
            "wait 24",
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
            "wait 6",
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
            "wait 6",
            "skip_last_loop",
            "wait 24",
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
        &["set_instrument dummy_instrument", "rest 24", "rest 72"],
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
    assert_error_in_mml_line("[a b L c]5", 6, ChannelError::CannotSetLoopPointInALoop);
}

#[test]
fn test_mp_vibrato_with_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument

A @0 MP20,2 a L a
"###,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 21 2",
            "play_note a4 24",
            // Loop
            // This set_vibrato instruction is not optimised out.
            "set_vibrato 21 2",
            "play_note a4 24",
        ],
    );
}

#[test]
fn test_mml_repeated_channel_is_only_processed_once() {
    assert_mml_channel_a_matches_bytecode(
        r###"
@0 dummy_instrument

AAAAAAAAAAA @0 a
"###,
        &["set_instrument dummy_instrument", "play_note a4 24"],
    );
}

/// Test the bytecode is repeated 4 times if there are 4 different channels on a single MML line
#[test]
fn test_mml_with_multiple_channels_on_one_line() {
    let mml = r###"
@0 dummy_instrument

ADEF @0 a
"###;
    let bc_asm = &["set_instrument dummy_instrument", "play_note a4 24"];

    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data.instruments_and_samples,
        mml.subroutines(),
        BcTerminator::DisableChannel,
    )
    .repeat(4);

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

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
fn test_note_range_after_skip_last_loop_bugfix() {
    assert_err_in_channel_a_mml(
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
fn test_note_range_after_subroutine_call() {
    assert_err_in_channel_a_mml(
        r##"
@d dummy_instrument
@oof only_octave_four

!s @oof c4a

A @d o6c !s o4c o6d o4e
"##,
        19,
        BytecodeError::NoteOutOfRange(note("d6"), note("c4")..=note("b4")).into(),
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

// Test that a note out of range error does not emit a NoTicksInLoop nor NoTicksAfterLoopPoint error.
#[test]
fn test_only_one_error_for_out_of_range_note_in_loop() {
    assert_error_in_mml_line(
        "[ o7 a ]2",
        6,
        BytecodeError::NoteOutOfRange(note("a7"), note("c2")..=note("b6")).into(),
    );

    assert_error_in_mml_line(
        "[ c : o7 a ]2",
        10,
        BytecodeError::NoteOutOfRange(note("a7"), note("c2")..=note("b6")).into(),
    );
}

#[test]
fn test_utf8_in_mml() {
    assert_eq!("´".len(), 2);
    assert_error_in_mml_line("´", 1, ChannelError::UnknownCharacters(1));

    assert_error_in_mml_line("´´´´´", 1, ChannelError::UnknownCharacters(5));

    // Google translate for "this is an error"
    assert_error_in_mml_line("これはエラーです", 1, ChannelError::UnknownCharacters(8));

    assert_eq!("⚠".len(), 3);
    assert_error_in_mml_line("⚠☹⛔", 1, ChannelError::UnknownCharacters(3));

    assert_eq!("𝅘𝅥𝅮".len(), 4);
    assert_error_in_mml_line("𝅘𝅥𝅮𝅘𝅥𝅮𝅘𝅥𝅮𝅗𝅥𝅗𝅥𝅘𝅥𝅮𝅘𝅥𝅮𝅘𝅥𝅮", 1, ChannelError::UnknownCharacters(8));
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

fn assert_line_matches_bytecode_bytes(mml_line: &str, bc: &[u8]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();

    let dd = dummy_data();
    let mml = compile_mml(&mml, &dd);
    let mml_bc = mml_bytecode(&mml);

    assert_eq!(&mml_bc[..2], [opcodes::SET_INSTRUMENT, 0]);
    assert_eq!(mml_bc.last(), Some(&opcodes::DISABLE_CHANNEL));
    assert_eq!(
        &mml_bc[2..(mml_bc.len() - 1)],
        bc,
        "Testing {mml_line:?} against bytecode bytes"
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

fn assert_error_in_mml_line(mml_line: &str, line_char: u32, expected_error: ChannelError) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    assert_err_in_channel_a_mml(&mml, line_char + 2, expected_error);
}

fn assert_err_in_channel_a_mml(mml: &str, line_char: u32, expected_error: ChannelError) {
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

fn assert_subroutine_err_in_mml(mml: &str, expected_errors: &[(&str, &[ChannelError])]) {
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
        .map(|s| (s.identifier.as_str(), s.subroutine_id.clone()))
        .collect();

    let mut bc = bytecode_assembler::BytecodeAssembler::new(
        inst_map,
        Some(&subroutines),
        bytecode_assembler::BytecodeContext::SongChannel,
    );

    for line in bc_asm {
        bc.parse_line(line).unwrap();
    }

    bc.bytecode(terminator).unwrap().0
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
        dummy_instrument("only_octave_four", SF, 4, 4, Envelope::Gain(Gain::new(0))),
    ].iter(),
        [
            data::Sample{
                name: "sample".parse().unwrap(),
                source: Default::default(),
                loop_setting: data::LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
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
        evaluator: Default::default(),
        ignore_gaussian_overflow: false,
        first_octave: Octave::try_new(first_octave).unwrap(),
        last_octave: Octave::try_new(last_octave).unwrap(),
        envelope,
        comment: None,
    }
}

fn note(note: &str) -> Note {
    Note::parse_bytecode_argument(note).unwrap()
}
