// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::{time::Bpm, UnsignedValueNewType};

use crate::*;

#[test]
fn play_noise() {
    assert_line_matches_bytecode("N15", &["play_noise 15 keyoff 24"]);
    assert_line_matches_bytecode("N15 &", &["play_noise 15 no_keyoff 24"]);

    assert_line_matches_bytecode("N16,1", &["play_noise 16 keyoff 96"]);
    assert_line_matches_bytecode("N16,16", &["play_noise 16 keyoff 6"]);

    assert_line_matches_bytecode("N17,4..", &["play_noise 17 keyoff 42"]);
    assert_line_matches_bytecode("N18,4...", &["play_noise 18 keyoff 45"]);
    assert_line_matches_bytecode("N19...", &["play_noise 19 keyoff 45"]);

    assert_line_matches_bytecode("N$0c", &["play_noise 12 keyoff 24"]);
    assert_line_matches_bytecode("N$1f,2", &["play_noise 31 keyoff 48"]);

    assert_line_matches_bytecode("N1,%600", &["play_noise 1 keyoff 600"]);
    assert_line_matches_bytecode("N2,%600 &", &["play_noise 2 no_keyoff 600"]);

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

    assert_one_error_in_mml_line("N32", 1, ValueError::NoiseFrequencyOutOfRange(32).into());

    assert_one_error_in_mml_line("N,4", 1, ValueError::NoNoiseFrequency.into());

    assert_one_error_in_mml_line("N c", 1, ValueError::NoNoiseFrequency.into());

    assert_one_error_in_mml_line("N5,", 3, ChannelError::NoLengthAfterComma);
}

#[test]
fn disable_noise() {
    assert_line_matches_bytecode("N-", &["disable_noise"]);
}

#[test]
fn noise_without_instrument_error() {
    // also tests the error is only shown once
    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

A N1 N2,4 c @1 N3
"##,
        3,
        BytecodeError::CannotPlayNoiseBeforeSettingInstrument.into(),
    );
}

#[test]
fn noise_without_instrument_in_subroutine_error() {
    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s N20

A !s @1 !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@1 dummy_instrument

!s1 N30
!s2 !s1 @1 !s1

A !s2 @1 !s2
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );
}

#[test]
fn enable_pitch_mod() {
    assert_channel_b_line_matches_bytecode("PM", &["enable_pmod"]);

    assert_one_channel_error_in_mml(
        "A PM",
        "A",
        3,
        BytecodeError::PmodNotAllowedInChannelA.into(),
    );
    assert_one_channel_error_in_mml(
        "G PM",
        "G",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
    assert_one_channel_error_in_mml(
        "H PM",
        "H",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
}

#[test]
fn disable_pitch_mod() {
    assert_channel_b_line_matches_bytecode("PM0", &["disable_pmod"]);

    assert_one_channel_error_in_mml(
        "A PM0",
        "A",
        3,
        BytecodeError::PmodNotAllowedInChannelA.into(),
    );
    assert_one_channel_error_in_mml(
        "G PM0",
        "G",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
    assert_one_channel_error_in_mml(
        "H PM0",
        "H",
        3,
        BytecodeError::PmodNotAllowedInChannelsGH.into(),
    );
}

#[test]
fn set_channel_invert() {
    assert_line_matches_bytecode("i", &["set_channel_invert both"]);
    assert_line_matches_bytecode("iB", &["set_channel_invert both"]);
    assert_line_matches_bytecode("i0", &["set_channel_invert none"]);

    assert_line_matches_bytecode(
        "i0 iL iR iLR iM iML iMR iMLR",
        &[
            "set_channel_invert none",
            "set_channel_invert left",
            "set_channel_invert right",
            "set_channel_invert left right",
            "set_channel_invert mono",
            "set_channel_invert mono left",
            "set_channel_invert mono right",
            "set_channel_invert mono left right",
        ],
    );

    // B or 0 in multi-character invert is not allowed
    assert_one_error_in_mml_line("i0L", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_one_error_in_mml_line("iR0", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_one_error_in_mml_line("i0M", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_one_error_in_mml_line("iBR", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_one_error_in_mml_line("iBM", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_one_error_in_mml_line("iRB", 1, ValueError::InvalidMmlInvertFlags.into());

    // Repeating a L/R/M is not allowed
    assert_one_error_in_mml_line("iLRL", 1, ValueError::DuplicateMmlInvertFlag.into());
    assert_one_error_in_mml_line("iRRL", 1, ValueError::DuplicateMmlInvertFlag.into());
    assert_one_error_in_mml_line("iMM", 1, ValueError::DuplicateMmlInvertFlag.into());

    // There must be a space after invert
    assert_one_error_in_mml_line("ic", 1, ValueError::InvalidMmlInvertFlags.into());
    assert_line_matches_bytecode("i c", &["set_channel_invert both", "play_note c4 24"]);
}

#[test]
fn set_channel_invert_asm() {
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert none }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0x80],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert right }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xc0],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert left }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xa0],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert mono }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0x81],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert left right }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xe0],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert right left }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xe0],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert left right mono }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xe1],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert both }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xe1],
    );

    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert 0 }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0x80],
    );
    assert_line_matches_bytecode_bytes(
        r"\asm { set_channel_invert LR }",
        &[opcodes::SET_CHANNEL_OR_ECHO_INVERT, 0xe0],
    );

    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert unknown }",
        8,
        ValueError::UnknownInvertFlagStr("unknown".to_owned()).into(),
    );

    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert left both }",
        8,
        ValueError::InvalidMultiArgInvertFlag("both").into(),
    );
    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert none right }",
        8,
        ValueError::InvalidMultiArgInvertFlag("none").into(),
    );
    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert none both }",
        8,
        ValueError::InvalidMultiArgInvertFlag("none").into(),
    );

    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert LRL }",
        8,
        ValueError::DuplicateMmlInvertFlag.into(),
    );

    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert right right }",
        8,
        ValueError::DuplicateInvertFlag.into(),
    );
    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert left mono left }",
        8,
        ValueError::DuplicateInvertFlag.into(),
    );
    assert_one_error_in_mml_line(
        r"\asm { set_channel_invert mono mono }",
        8,
        ValueError::DuplicateInvertFlag.into(),
    );
}

#[test]
fn set_song_tempo() {
    let tc = f64::round(8000.0 * 60.0 / f64::from(48 * 80)) as u32;
    let bc = format!("set_song_tick_clock {tc}");

    assert_line_matches_bytecode("t80", &[&bc]);

    assert_line_matches_bytecode(
        "t40",
        &[&format!(
            "set_song_tick_clock {}",
            Bpm::MIN.to_tick_clock().unwrap().value()
        )],
    );
    assert_one_error_in_mml_line("t39", 1, ValueError::BpmOutOfRange(39).into());

    assert_line_matches_bytecode(
        "t157",
        &[&format!(
            "set_song_tick_clock {}",
            Bpm::MAX.to_tick_clock().unwrap().value()
        )],
    );
    assert_one_error_in_mml_line("t158", 1, ValueError::BpmOutOfRange(158).into());
}

#[test]
fn set_song_tick_clock() {
    const SET_SONG_TICK_CLOCK: u8 = 1;

    assert_line_matches_bytecode_bytes("T90", &[opcodes::MISCELLANEOUS, SET_SONG_TICK_CLOCK, 90]);

    assert_line_matches_bytecode_bytes("T255", &[opcodes::MISCELLANEOUS, SET_SONG_TICK_CLOCK, 255]);

    assert_line_matches_bytecode_bytes("T256", &[opcodes::MISCELLANEOUS, SET_SONG_TICK_CLOCK, 0]);
    assert_one_error_in_mml_line("T257", 1, ValueError::TickClockOutOfRange(257).into());

    assert_line_matches_bytecode_bytes("T64", &[opcodes::MISCELLANEOUS, SET_SONG_TICK_CLOCK, 64]);
    assert_one_error_in_mml_line("T63", 1, ValueError::TickClockOutOfRange(63).into());
}
