// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn note() {
    assert_line_matches_bytecode(
        "K0 c d8 e%20 & e%40",
        &[
            "play_note c4 no_keyoff 24",
            "keyon_next_note",
            "play_note d4 no_keyoff 12",
            "keyon_next_note",
            "play_note e4 no_keyoff 20",
            "play_note e4 no_keyoff 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode("K0 c r", &["play_note c4 no_keyoff 24", "rest 24"]);
}

#[test]
fn long_note() {
    assert_line_matches_bytecode(
        "K0 c%600",
        &[
            "play_note c4 no_keyoff 256",
            "wait 256",
            "wait 88",
            "keyon_next_note",
        ],
    );
}

#[test]
fn pitch() {
    assert_line_matches_bytecode(
        "K0 P$1000 P$1200,8 P$1400,%20 & P$1800,%40",
        &[
            "play_pitch $1000 no_keyoff 24",
            "keyon_next_note",
            "play_pitch $1200 no_keyoff 12",
            "keyon_next_note",
            "play_pitch $1400 no_keyoff 20",
            "play_pitch $1800 no_keyoff 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode("K0 P$1000 r", &["play_pitch $1000 no_keyoff 24", "rest 24"]);
}

#[test]
fn long_pitch() {
    assert_line_matches_bytecode(
        "K0 P$1000,%600",
        &[
            "play_pitch $1000 no_keyoff 256",
            "wait 256",
            "wait 88",
            "keyon_next_note",
        ],
    );
}

#[test]
fn pitch_frequency() {
    assert_line_matches_bytecode(
        "K0 PF500 PF250,8 PF1000,%20 & PF1500,%40",
        &[
            "play_pitch $1000 no_keyoff 24",
            "keyon_next_note",
            "play_pitch $0800 no_keyoff 12",
            "keyon_next_note",
            "play_pitch $2000 no_keyoff 20",
            "play_pitch $3000 no_keyoff 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode("K0 PF500 r", &["play_pitch $1000 no_keyoff 24", "rest 24"]);
}

#[test]
fn long_pitch_frequency() {
    assert_line_matches_bytecode(
        "K0 PF500,%600",
        &[
            "play_pitch $1000 no_keyoff 256",
            "wait 256",
            "wait 88",
            "keyon_next_note",
        ],
    );
}

#[test]
fn sample() {
    assert_line_matches_bytecode(
        "K0 s40 s41,8 s42,%20 & s43,%40",
        &[
            "play_note 40 no_keyoff 24",
            "keyon_next_note",
            "play_note 41 no_keyoff 12",
            "keyon_next_note",
            "play_note 42 no_keyoff 20",
            "play_note 43 no_keyoff 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode("K0 s40 r", &["play_note 40 no_keyoff 24", "rest 24"]);
}

#[test]
fn long_sample() {
    assert_line_matches_bytecode(
        "K0 s40,%600",
        &[
            "play_note 40 no_keyoff 256",
            "wait 256",
            "wait 88",
            "keyon_next_note",
        ],
    );
}

#[test]
fn noise() {
    assert_line_matches_bytecode(
        "K0 N3 N4,8 N5,%20 & N6,%40",
        &[
            "play_noise 3 no_keyoff 24",
            "keyon_next_note",
            "play_noise 4 no_keyoff 12",
            "keyon_next_note",
            "play_noise 5 no_keyoff 20",
            "play_noise 6 no_keyoff 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode("K0 N7 r", &["play_noise 7 no_keyoff 24", "rest 24"]);
}

#[test]
fn long_noise() {
    assert_line_matches_bytecode(
        "K0 N20,%600",
        &[
            "play_noise 20 no_keyoff 256",
            "wait 256",
            "wait 88",
            "keyon_next_note",
        ],
    );
}

#[test]
fn portamento() {
    assert_line_matches_bytecode(
        "K0 {cd} {de}8 & {ef}%40",
        &[
            "play_note c4 no_keyoff 1",
            "portamento d4 no_keyoff +12 23",
            "keyon_next_note",
            "play_note d4 no_keyoff 1",
            "portamento e4 no_keyoff +27 11",
            // &
            "portamento f4 no_keyoff +4 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode(
        "K0 {cd} r",
        &[
            "play_note c4 no_keyoff 1",
            "portamento d4 no_keyoff +12 23",
            "rest 24",
        ],
    );
}

#[test]
fn portamento_pitch() {
    assert_line_matches_bytecode(
        "K0 {P$1000 P$1500} {P$1500 P$1000}8 & {P$1000 P$1200}%40",
        &[
            "play_pitch $1000 no_keyoff 1",
            "portamento_pitch $1500 no_keyoff +58 23",
            "keyon_next_note",
            "play_pitch $1500 no_keyoff 1",
            "portamento_pitch $1000 no_keyoff -116 11",
            // &
            "portamento_pitch $1200 no_keyoff +13 40",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode(
        "K0 {P$2000 P$1000} r",
        &[
            "play_pitch $2000 no_keyoff 1",
            "portamento_pitch $1000 no_keyoff -186 23",
            "rest 24",
        ],
    );
}

#[test]
fn long_portamento() {
    assert_line_matches_bytecode(
        "K0 {o3 c o5 c}%600",
        &[
            "play_note c3 no_keyoff 1",
            "portamento c5 no_keyoff +5 256",
            "wait 256",
            "wait 87",
            "keyon_next_note",
        ],
    );
}

#[test]
fn broken_chord() {
    assert_line_matches_line_and_bytecode(
        "K0 {{ceg}}",
        "K0 [c%1 & : e%1 & g%1 &]8 e%2",
        &[
            "start_loop",
            "play_note c4 no_keyoff 1",
            "skip_last_loop",
            "play_note e4 no_keyoff 1",
            "play_note g4 no_keyoff 1",
            "end_loop 8",
            "play_note e4 no_keyoff 2",
            "keyon_next_note",
        ],
    );

    assert_line_matches_bytecode(
        "K0 {{ceg}} r",
        &[
            "start_loop",
            "play_note c4 no_keyoff 1",
            "skip_last_loop",
            "play_note e4 no_keyoff 1",
            "play_note g4 no_keyoff 1",
            "end_loop 8",
            "play_note e4 no_keyoff 2",
            "keyon_next_note",
            "rest 24",
        ],
    );
}

#[test]
fn broken_chord_no_tie() {
    assert_line_matches_line_and_bytecode(
        "K0 {{ceg}}2,,0",
        "K0 [c%2 e%2 g%2]8",
        &[
            "start_loop",
            "play_note c4 no_keyoff 2",
            "keyon_next_note",
            "play_note e4 no_keyoff 2",
            "keyon_next_note",
            "play_note g4 no_keyoff 2",
            "keyon_next_note",
            "end_loop 8",
        ],
    );

    assert_line_matches_bytecode(
        "K0 {{ceg}}2,,0 r",
        &[
            "start_loop",
            "play_note c4 no_keyoff 2",
            "keyon_next_note",
            "play_note e4 no_keyoff 2",
            "keyon_next_note",
            "play_note g4 no_keyoff 2",
            "keyon_next_note",
            "end_loop 8",
            "rest 24",
        ],
    );
}

#[test]
fn note_then_wait() {
    assert_line_matches_bytecode(
        "K0 c w",
        &["play_note c4 no_keyoff 24", "keyon_next_note", "wait 24"],
    );
}

/// Quantize overrides no-keyoff
#[test]
fn coarse_quantization() {
    assert_line_matches_line_and_bytecode(
        "K0 Q1 c%80 Q8 d",
        "Q1 K0 c%80 Q8 d",
        &[
            "play_note c4 keyoff 11",
            "rest 69",
            // Q8
            "play_note d4 no_keyoff 24",
            "keyon_next_note",
        ],
    );
}

/// Quantize overrides no-keyoff
#[test]
fn coarse_quantization_with_temp_gain() {
    assert_line_matches_line_and_bytecode(
        "K0 Q1,D3 c%80 Q8 d",
        "Q1,D3 K0 c%80 Q8 d",
        &[
            "play_note c4 no_keyoff 10",
            "set_temp_gain_and_rest D3 70",
            // Q8
            "play_note d4 no_keyoff 24",
            "keyon_next_note",
        ],
    );
}

/// Quantize overrides no-keyoff
#[test]
fn fine_quantization() {
    assert_line_matches_line_and_bytecode(
        "K0 Q%1 c%80 Q8 d",
        "Q%1 K0 c%80 Q8 d",
        &[
            "play_note c4 keyoff 2",
            "rest 78",
            // Q8
            "play_note d4 no_keyoff 24",
            "keyon_next_note",
        ],
    );
}

#[test]
fn k0_note_k1_note() {
    assert_line_matches_bytecode(
        "K0 c K1 d",
        &[
            "play_note c4 no_keyoff 24",
            "keyon_next_note",
            "play_note d4 keyoff 24",
        ],
    );
}
