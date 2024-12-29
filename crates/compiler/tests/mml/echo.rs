// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn enable_echo() {
    assert_line_matches_bytecode("E", &["enable_echo"]);
}

#[test]
fn disable_echo() {
    assert_line_matches_bytecode("E1", &["enable_echo"]);
    assert_line_matches_bytecode("E0", &["disable_echo"]);
}

#[test]
fn set_echo_volume() {
    assert_line_matches_bytecode(r"\evol $12", &["set_echo_volume 18"]);

    assert_line_matches_bytecode(r"\evol 0", &["set_echo_volume 0"]);
    assert_line_matches_bytecode(r"\evol 127", &["set_echo_volume 127"]);

    assert_one_error_in_mml_line(
        r"\evol 128",
        7,
        ValueError::EchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn set_stereo_echo_volume() {
    assert_line_matches_bytecode(r"\evol 12,34", &["set_stereo_echo_volume 12, 34"]);

    // Use set_echo_volume is both arguments are the same
    assert_line_matches_bytecode_bytes(r"\evol $60,$60", &[opcodes::SET_ECHO_VOLUME, 0x60]);
    assert_line_matches_bytecode_bytes(r"\evol 7,8", &[opcodes::SET_STEREO_ECHO_VOLUME, 7, 8]);

    assert_line_matches_bytecode(r"\evol 127,0", &["set_stereo_echo_volume 127, 0"]);
    assert_line_matches_bytecode(r"\evol 0,127", &["set_stereo_echo_volume 0, 127"]);

    assert_one_error_in_mml_line(
        r"\evol 128,0",
        7,
        ValueError::EchoVolumeOutOfRange(128).into(),
    );
    assert_one_error_in_mml_line(
        r"\evol 0,128",
        9,
        ValueError::EchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn adjust_echo_volume() {
    assert_line_matches_bytecode(r"\evol +50", &["adjust_echo_volume +50"]);

    assert_line_matches_bytecode(r"\evol +0", &[]);
    assert_line_matches_bytecode(r"\evol -0", &[]);

    assert_line_matches_bytecode(r"\evol -127", &["adjust_echo_volume -127"]);
    assert_one_error_in_mml_line(
        r"\evol -128",
        7,
        ValueError::RelativeEchoVolumeOutOfRange(-128).into(),
    );

    assert_line_matches_bytecode(r"\evol +127", &["adjust_echo_volume +127"]);
    assert_one_error_in_mml_line(
        r"\evol +128",
        7,
        ValueError::RelativeEchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn adjust_stereo_echo_volume() {
    assert_line_matches_bytecode(r"\evol +50 , -80", &["adjust_stereo_echo_volume +50, -80"]);

    // Use adjust_echo_volume is both arguments are the same
    assert_line_matches_bytecode_bytes(r"\evol +20,+20", &[opcodes::ADJUST_ECHO_VOLUME, 20]);
    assert_line_matches_bytecode_bytes(
        r"\evol +30,+40",
        &[opcodes::ADJUST_STEREO_ECHO_VOLUME, 30, 40],
    );

    assert_line_matches_bytecode(r"\evol +10,+0", &["adjust_stereo_echo_volume +10, +0"]);
    assert_line_matches_bytecode(r"\evol -0,-10", &["adjust_stereo_echo_volume -0, -10"]);

    assert_line_matches_bytecode(r"\evol +0,+0", &[]);
    assert_line_matches_bytecode(r"\evol +0,-0", &[]);
    assert_line_matches_bytecode(r"\evol -0,+0", &[]);
    assert_line_matches_bytecode(r"\evol -0,-0", &[]);

    assert_line_matches_bytecode(r"\evol -127,-10", &["adjust_stereo_echo_volume -127, -10"]);
    assert_one_error_in_mml_line(
        r"\evol -128,-10",
        7,
        ValueError::RelativeEchoVolumeOutOfRange(-128).into(),
    );

    assert_line_matches_bytecode(r"\evol +127,+10", &["adjust_stereo_echo_volume +127, +10"]);
    assert_one_error_in_mml_line(
        r"\evol +128,+10",
        7,
        ValueError::RelativeEchoVolumeOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\evol -10,-127", &["adjust_stereo_echo_volume -10, -127"]);
    assert_one_error_in_mml_line(
        r"\evol -10,-128",
        11,
        ValueError::RelativeEchoVolumeOutOfRange(-128).into(),
    );

    assert_line_matches_bytecode(r"\evol +10,+127", &["adjust_stereo_echo_volume +10, +127"]);
    assert_one_error_in_mml_line(
        r"\evol +10,+128",
        11,
        ValueError::RelativeEchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn forbid_set_and_adjust_stereo_echo_volume() {
    assert_one_error_in_mml_line(
        r"\evol 10,+10",
        1,
        ChannelError::SetAndRelativeStereoEchoVolume,
    );
    assert_one_error_in_mml_line(
        r"\evol 20,-20",
        1,
        ChannelError::SetAndRelativeStereoEchoVolume,
    );
    assert_one_error_in_mml_line(
        r"\evol +30,30",
        1,
        ChannelError::RelativeAndSetStereoEchoVolume,
    );
    assert_one_error_in_mml_line(
        r"\evol -40,40",
        1,
        ChannelError::RelativeAndSetStereoEchoVolume,
    );
}
