// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

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
fn test_enable_pitch_mod() {
    assert_channel_b_line_matches_bytecode("PM", &["enable_pmod"]);

    assert_one_err_in_mml(
        "A PM",
        "A",
        3,
        BytecodeError::PmodNotAllowedInChannelA.into(),
    );
    assert_one_err_in_mml(
        "G PM",
        "G",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
    assert_one_err_in_mml(
        "H PM",
        "H",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
}

#[test]
fn test_disable_pitch_mod() {
    assert_channel_b_line_matches_bytecode("PM0", &["disable_pmod"]);

    assert_one_err_in_mml(
        "A PM0",
        "A",
        3,
        BytecodeError::PmodNotAllowedInChannelA.into(),
    );
    assert_one_err_in_mml(
        "G PM0",
        "G",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
    assert_one_err_in_mml(
        "H PM0",
        "H",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
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
