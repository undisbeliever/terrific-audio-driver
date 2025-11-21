// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

const MIN_U32_TO_I32_ERROR: u32 = (i32::MAX as u32) + 1;

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

#[test]
fn set_echo_feedback() {
    assert_line_matches_bytecode(r"\efb 52", &["set_echo_feedback $34"]);
    assert_line_matches_bytecode(r"\efb $34", &["set_echo_feedback 52"]);

    assert_line_matches_bytecode(r"\efb 0", &["set_echo_feedback 0"]);

    assert_line_matches_bytecode(r"\efb -128", &["set_echo_feedback -128"]);
    assert_one_error_in_mml_line(
        r"\efb -129",
        6,
        ValueError::EchoFeedbackOutOfRange(-129).into(),
    );

    assert_line_matches_bytecode(r"\efb 127", &["set_echo_feedback 127"]);
    assert_one_error_in_mml_line(
        r"\efb 128",
        6,
        ValueError::EchoFeedbackOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\efb +127", &["set_echo_feedback +127"]);
    assert_one_error_in_mml_line(
        r"\efb +128",
        6,
        ValueError::EchoFeedbackOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line(
        r"\efb 2147483647",
        6,
        ValueError::EchoFeedbackOutOfRangeU32(i32::MAX.try_into().unwrap()).into(),
    );
    assert_one_error_in_mml_line(
        r"\efb 2147483648",
        6,
        ValueError::EchoFeedbackOutOfRangeU32(u32::try_from(i32::MAX).unwrap() + 1).into(),
    );

    assert_one_error_in_mml_line(r"\efb", 1, ValueError::NoEchoFeedback.into());
}

#[test]
fn increment_echo_feedback() {
    assert_line_matches_bytecode(r"\efb+ 50", &["adjust_echo_feedback +50"]);

    assert_line_matches_bytecode(r"\efb+ 0", &[]);

    assert_line_matches_bytecode(r"\efb+ 127", &["adjust_echo_feedback +127"]);
    assert_one_error_in_mml_line(
        r"\efb+ 128",
        7,
        ValueError::RelativeEchoFeedbackOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line(
        r"\efb+ 2147483647",
        7,
        ValueError::RelativeEchoFeedbackOutOfRange(i32::MAX).into(),
    );
    assert_one_error_in_mml_line(
        r"\efb+ 2147483648",
        7,
        ValueError::RelativeEchoFeedbackOutOfRangeU32(MIN_U32_TO_I32_ERROR).into(),
    );

    assert_one_error_in_mml_line(r"\efb+", 1, ValueError::NoRelativeEchoFeedback.into());
}

#[test]
fn decrement_echo_feedback() {
    assert_line_matches_bytecode(r"\efb- 50", &["adjust_echo_feedback -50"]);

    assert_line_matches_bytecode(r"\efb- 0", &[]);

    assert_line_matches_bytecode(r"\efb- 128", &["adjust_echo_feedback -128"]);
    assert_one_error_in_mml_line(
        r"\efb- 129",
        7,
        ValueError::RelativeEchoFeedbackOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(
        r"\efb- 2147483647",
        7,
        ValueError::RelativeEchoFeedbackOutOfRange(-i32::MAX).into(),
    );
    assert_one_error_in_mml_line(
        r"\efb- 2147483648",
        7,
        ValueError::RelativeEchoFeedbackOutOfRangeU32(MIN_U32_TO_I32_ERROR).into(),
    );

    assert_one_error_in_mml_line(r"\efb-", 1, ValueError::NoRelativeEchoFeedback.into());
}

#[test]
fn set_fir_filter() {
    assert_line_matches_bytecode(
        r"\fir { 1 2 3 4 5 6 7 8 }",
        &["set_fir_filter 1 2 3 4 5 6 7 8"],
    );
    assert_line_matches_bytecode(
        r"\fir { -1 -2 -3 -4 -5 -6 -7 -8 }",
        &["set_fir_filter -1 -2 -3 -4 -5 -6 -7 -8"],
    );
    assert_line_matches_bytecode(
        r"\fir { +1 +2 +3 +4 +5 +6 +7 +8 }",
        &["set_fir_filter +1 +2 +3 +4 +5 +6 +7 +8"],
    );

    assert_line_matches_bytecode(
        r"\fir { $00 $10 $20 $30 $40 $11 $12 $13 }",
        &["set_fir_filter 0 16 32 $30 $40 17 18 19"],
    );
}

#[test]
fn set_fir_filter_errors() {
    assert_one_error_in_mml_line(
        r"\fir { 0 0 0 0 127 128 0 0 }",
        20,
        ValueError::FirCoefficientOutOfRangeU32(128).into(),
    );
    assert_one_error_in_mml_line(
        r"\fir { 0 0 0 0 0 0 +127 +128 }",
        25,
        ValueError::FirCoefficientOutOfRange(128).into(),
    );
    assert_one_error_in_mml_line(
        r"\fir { 0 0 -128 -129 0 0 0 0 }",
        17,
        ValueError::FirCoefficientOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(
        r"\fir { 2147483647 0 0 0 0 0 0 0 }",
        8,
        ValueError::FirCoefficientOutOfRangeU32(i32::MAX.try_into().unwrap()).into(),
    );
    assert_one_error_in_mml_line(
        r"\fir { 2147483648 0 0 0 0 0 0 0 }",
        8,
        ValueError::FirCoefficientOutOfRangeU32(MIN_U32_TO_I32_ERROR).into(),
    );

    assert_one_error_in_mml_line(
        r"\fir { }",
        1,
        ChannelError::InvalidNumberOfFirCoefficients(0),
    );
    assert_one_error_in_mml_line(
        r"\fir { 0 }",
        1,
        ChannelError::InvalidNumberOfFirCoefficients(1),
    );
    assert_one_error_in_mml_line(
        r"\fir { 0 0 0 0 0 0 0 }",
        1,
        ChannelError::InvalidNumberOfFirCoefficients(7),
    );
    assert_one_error_in_mml_line(
        r"\fir { 0 0 0 0 0 0 0 0 0 }",
        1,
        ChannelError::InvalidNumberOfFirCoefficients(9),
    );

    assert_one_error_in_mml_line(r"\fir c", 1, ChannelError::NoBraceAfterFirFilter);
    assert_one_error_in_mml_line(
        r"\fir { 1 2 3 4 5 6 7 8",
        1,
        ChannelError::MissingEndFirFilter,
    );
    assert_one_error_in_mml_line(
        r"\fir { c 1 2 3 4 5 6 7 8 }",
        8,
        ChannelError::UnknownTokenInFirFilter,
    );
    assert_one_error_in_mml_line(
        r"\fir { 1 2 3 4 5 6 7 8 { }",
        24,
        ChannelError::UnknownTokenInFirFilter,
    );
}

#[test]
fn set_fir_tap() {
    assert_line_matches_bytecode(r"\ftap 0,15", &["set_fir_tap 0 +15"]);
    assert_line_matches_bytecode(r"\ftap 0,+15", &["set_fir_tap 0 15"]);
    assert_line_matches_bytecode(r"\ftap 0,-15", &["set_fir_tap 0 -15"]);
    assert_line_matches_bytecode(r"\ftap 0, $42", &["set_fir_tap 0 66"]);

    assert_line_matches_bytecode(r"\ftap 3,45", &["set_fir_tap 3 45"]);

    assert_line_matches_bytecode(r"\ftap 0,127", &["set_fir_tap 0 127"]);
    assert_one_error_in_mml_line(
        r"\ftap 0,128",
        9,
        ValueError::FirCoefficientOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap 0,+127", &["set_fir_tap 0 127"]);
    assert_one_error_in_mml_line(
        r"\ftap 0,+128",
        9,
        ValueError::FirCoefficientOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap 0,-128", &["set_fir_tap 0 -128"]);
    assert_one_error_in_mml_line(
        r"\ftap 0,-129",
        9,
        ValueError::FirCoefficientOutOfRange(-129).into(),
    );

    assert_line_matches_bytecode(r"\ftap 7,27", &["set_fir_tap 7 27"]);
    assert_one_error_in_mml_line(r"\ftap 8,27", 1, ValueError::FirTapOutOfRange(8).into());

    assert_one_error_in_mml_line(r"\ftap ,0", 1, ValueError::NoFirTap.into());
    assert_one_error_in_mml_line(r"\ftap 0,", 9, ValueError::NoFirCoefficient.into());
    assert_one_error_in_mml_line(r"\ftap 0", 8, ValueError::NoCommaFirCoefficient.into());
}

#[test]
fn increment_fir_tap() {
    assert_line_matches_bytecode(r"\ftap+ 0,15", &["adjust_fir_tap 0 +15"]);
    assert_line_matches_bytecode(r"\ftap+ 4,56", &["adjust_fir_tap 4 +56"]);

    assert_line_matches_bytecode(r"\ftap+ 2,0", &[]);

    assert_line_matches_bytecode(r"\ftap+ 0,127", &["adjust_fir_tap 0 +127"]);
    assert_one_error_in_mml_line(
        r"\ftap+ 0,128",
        10,
        ValueError::RelativeFirCoefficientOutOfRange(128).into(),
    );

    assert_one_error_in_mml_line(
        r"\ftap+ 0,2147483647",
        10,
        ValueError::RelativeFirCoefficientOutOfRange(i32::MAX).into(),
    );
    assert_one_error_in_mml_line(
        r"\ftap+ 0,2147483648",
        10,
        ValueError::RelativeFirCoefficientOutOfRangeU32(MIN_U32_TO_I32_ERROR).into(),
    );

    assert_line_matches_bytecode(r"\ftap+ 7,22", &["adjust_fir_tap 7 +22"]);
    assert_one_error_in_mml_line(r"\ftap+ 8,22", 1, ValueError::FirTapOutOfRange(8).into());

    assert_one_error_in_mml_line(r"\ftap+ ,20", 1, ValueError::NoFirTap.into());
    assert_one_error_in_mml_line(
        r"\ftap+ 0,",
        10,
        ValueError::NoRelativeFirCoefficient.into(),
    );
    assert_one_error_in_mml_line(
        r"\ftap+ 0",
        9,
        ValueError::NoCommaRelativeFirCoefficient.into(),
    );
}

#[test]
fn decrement_fir_tap() {
    assert_line_matches_bytecode(r"\ftap- 0,15", &["adjust_fir_tap 0 -15"]);
    assert_line_matches_bytecode(r"\ftap- 4,56", &["adjust_fir_tap 4 -56"]);

    assert_line_matches_bytecode(r"\ftap- 2,0", &[]);

    assert_line_matches_bytecode(r"\ftap- 0,128", &["adjust_fir_tap 0 -128"]);
    assert_one_error_in_mml_line(
        r"\ftap- 0,129",
        10,
        ValueError::RelativeFirCoefficientOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(
        r"\ftap- 0,2147483647",
        10,
        ValueError::RelativeFirCoefficientOutOfRange(-i32::MAX).into(),
    );
    assert_one_error_in_mml_line(
        r"\ftap- 0,2147483648",
        10,
        ValueError::RelativeFirCoefficientOutOfRangeU32(MIN_U32_TO_I32_ERROR).into(),
    );

    assert_line_matches_bytecode(r"\ftap- 7,22", &["adjust_fir_tap 7 -22"]);
    assert_one_error_in_mml_line(r"\ftap- 8,22", 1, ValueError::FirTapOutOfRange(8).into());

    assert_one_error_in_mml_line(r"\ftap- ,20", 1, ValueError::NoFirTap.into());
    assert_one_error_in_mml_line(
        r"\ftap- 0,",
        10,
        ValueError::NoRelativeFirCoefficient.into(),
    );
    assert_one_error_in_mml_line(
        r"\ftap- 0",
        9,
        ValueError::NoCommaRelativeFirCoefficient.into(),
    );
}

#[test]
fn increment_echo_feedback_with_limit() {
    assert_line_matches_bytecode(r"\efb+ 15,20", &["adjust_echo_feedback_limit +15 20"]);
    assert_line_matches_bytecode(r"\efb+ 56,-80", &["adjust_echo_feedback_limit +56 -80"]);

    assert_line_matches_bytecode(r"\efb+ 10,127", &["adjust_echo_feedback_limit +10 127"]);
    assert_one_error_in_mml_line(
        r"\efb+ 10,128",
        10,
        ValueError::EchoFeedbackOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\efb+ 10,+127", &["adjust_echo_feedback_limit +10 127"]);
    assert_one_error_in_mml_line(
        r"\efb+ 10,+128",
        10,
        ValueError::EchoFeedbackOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\efb+ 10,-128", &["adjust_echo_feedback_limit +10 -128"]);
    assert_one_error_in_mml_line(
        r"\efb+ 10,-129",
        10,
        ValueError::EchoFeedbackOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(r"\efb+ 10,", 9, ValueError::NoEchoFeedback.into());
}

#[test]
fn decrement_echo_feedback_with_limit() {
    assert_line_matches_bytecode(r"\efb- 15,20", &["adjust_echo_feedback_limit -15 20"]);
    assert_line_matches_bytecode(r"\efb- 56,-80", &["adjust_echo_feedback_limit -56 -80"]);

    assert_line_matches_bytecode(r"\efb- 10,127", &["adjust_echo_feedback_limit -10 127"]);
    assert_one_error_in_mml_line(
        r"\efb- 10,128",
        10,
        ValueError::EchoFeedbackOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\efb- 10,+127", &["adjust_echo_feedback_limit -10 127"]);
    assert_one_error_in_mml_line(
        r"\efb- 10,+128",
        10,
        ValueError::EchoFeedbackOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\efb- 10,-128", &["adjust_echo_feedback_limit -10 -128"]);
    assert_one_error_in_mml_line(
        r"\efb- 10,-129",
        10,
        ValueError::EchoFeedbackOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(r"\efb- 10,", 9, ValueError::NoEchoFeedback.into());
}

#[test]
fn increment_fir_tap_with_limit() {
    assert_line_matches_bytecode(r"\ftap+ 0,15,20", &["adjust_fir_tap_limit 0 +15 20"]);
    assert_line_matches_bytecode(r"\ftap+ 4,56,-80", &["adjust_fir_tap_limit 4 +56 -80"]);

    assert_line_matches_bytecode(r"\ftap+ 0,10,127", &["adjust_fir_tap_limit 0 +10 127"]);
    assert_one_error_in_mml_line(
        r"\ftap+ 0,10,128",
        13,
        ValueError::FirCoefficientOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap+ 0,10,+127", &["adjust_fir_tap_limit 0 +10 127"]);
    assert_one_error_in_mml_line(
        r"\ftap+ 0,10,+128",
        13,
        ValueError::FirCoefficientOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap+ 0,10,-128", &["adjust_fir_tap_limit 0 +10 -128"]);
    assert_one_error_in_mml_line(
        r"\ftap+ 0,10,-129",
        13,
        ValueError::FirCoefficientOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(r"\ftap+ 0,10,", 12, ValueError::NoFirCoefficient.into());
}

#[test]
fn decrement_fir_tap_with_limit() {
    assert_line_matches_bytecode(r"\ftap- 0,15,20", &["adjust_fir_tap_limit 0 -15 20"]);
    assert_line_matches_bytecode(r"\ftap- 4,56,-80", &["adjust_fir_tap_limit 4 -56 -80"]);

    assert_line_matches_bytecode(r"\ftap- 0,10,127", &["adjust_fir_tap_limit 0 -10 127"]);
    assert_one_error_in_mml_line(
        r"\ftap- 0,10,128",
        13,
        ValueError::FirCoefficientOutOfRangeU32(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap- 0,10,+127", &["adjust_fir_tap_limit 0 -10 127"]);
    assert_one_error_in_mml_line(
        r"\ftap- 0,10,+128",
        13,
        ValueError::FirCoefficientOutOfRange(128).into(),
    );

    assert_line_matches_bytecode(r"\ftap- 0,10,-128", &["adjust_fir_tap_limit 0 -10 -128"]);
    assert_one_error_in_mml_line(
        r"\ftap- 0,10,-129",
        13,
        ValueError::FirCoefficientOutOfRange(-129).into(),
    );

    assert_one_error_in_mml_line(r"\ftap- 0,10,", 12, ValueError::NoFirCoefficient.into());
}

#[test]
fn efb_tap_hex() {
    assert_line_matches_line(r"\efb $00", r"\efb 0");
    assert_line_matches_line(r"\efb $7f", r"\efb +127");
    assert_line_matches_line(r"\efb $80", r"\efb -128");
    assert_line_matches_line(r"\efb $c0", r"\efb -64");

    assert_line_matches_bytecode(r"\efb -64", &["set_echo_feedback $c0"]);
    assert_line_matches_bytecode(r"\efb+ 64,-64", &["adjust_echo_feedback_limit +64 $c0"]);

    assert_line_matches_line(r"\efb $FF", r"\efb -1");
    assert_one_error_in_mml_line(
        r"\efb $100",
        6,
        ValueError::EchoFeedbackHexOutOfRange(0x100).into(),
    );

    assert_line_matches_line(r"\efb+ 20,$FF", r"\efb+ 20,-1");
    assert_one_error_in_mml_line(
        r"\efb+ 20,$100",
        10,
        ValueError::EchoFeedbackHexOutOfRange(0x100).into(),
    );

    assert_line_matches_line(r"\efb- 20,$FF", r"\efb- 20,-1");
    assert_one_error_in_mml_line(
        r"\efb- 20,$100",
        10,
        ValueError::EchoFeedbackHexOutOfRange(0x100).into(),
    );
}

#[test]
fn fir_tap_hex() {
    assert_line_matches_line(r"\ftap 0,$00", r"\ftap 0,0");
    assert_line_matches_line(r"\ftap 0,$7f", r"\ftap 0,+127");
    assert_line_matches_line(r"\ftap 0,$80", r"\ftap 0,-128");
    assert_line_matches_line(r"\ftap 0,$c0", r"\ftap 0,-64");

    assert_line_matches_bytecode(r"\ftap 0,-64", &["set_fir_tap 0 $c0"]);
    assert_line_matches_bytecode(r"\ftap+ 0,64,-64", &["adjust_fir_tap_limit 0 +64 $c0"]);

    assert_line_matches_line(r"\ftap 0,$ff", r"\ftap 0,-1");
    assert_one_error_in_mml_line(
        r"\ftap 0,$100",
        9,
        ValueError::FirCoefficientHexOutOfRange(0x100).into(),
    );

    assert_line_matches_line(r"\ftap+ 0,20,$FF", r"\ftap+ 0,20,-1");
    assert_one_error_in_mml_line(
        r"\ftap+ 0,20,$100",
        13,
        ValueError::FirCoefficientHexOutOfRange(0x100).into(),
    );

    assert_line_matches_line(r"\ftap- 0,20,$FF", r"\ftap- 0,20,-1");
    assert_one_error_in_mml_line(
        r"\ftap- 0,20,$100",
        13,
        ValueError::FirCoefficientHexOutOfRange(0x100).into(),
    );
}

#[test]
fn fir_filter_hex() {
    assert_line_matches_line(
        r"\fir {$00 $20 $40 $7f  $80 $a0 $d0 $ff}",
        r"\fir {  0  32  64 127 -128 -96 -48  -1}",
    );

    assert_line_matches_bytecode(
        r"\fir { 0 32 64 127 -128 -96 -48 -1 }",
        &["set_fir_filter $00 $20 $40 $7f $80 $a0 $d0 $ff"],
    );

    assert_one_error_in_mml_line(
        r"\fir {$00 $00 $ff $100 $00 $00 $00 $00}",
        19,
        ValueError::FirCoefficientHexOutOfRange(0x100).into(),
    );
}

#[test]
fn set_echo_invert() {
    assert_line_matches_bytecode(r"\ei B", &["set_echo_invert both"]);
    assert_line_matches_bytecode(r"\ei 0", &["set_echo_invert none"]);
    assert_line_matches_bytecode(r"\ei L", &["set_echo_invert left"]);
    assert_line_matches_bytecode(r"\ei R", &["set_echo_invert right"]);
    assert_line_matches_bytecode(r"\ei M", &["set_echo_invert mono"]);
    assert_line_matches_bytecode(r"\ei LR", &["set_echo_invert left right"]);
    assert_line_matches_bytecode(r"\ei LMR", &["set_echo_invert left right mono"]);

    // Repeating a L/R/M is not allowed
    // Not testing all flags.
    // (`set_echo_invert` uses the same parsing/errors `set_channel_invert`)
    assert_one_error_in_mml_line(r"\ei LRL", 1, ValueError::DuplicateMmlInvertFlag.into());

    // echo invert flags are required
    assert_one_error_in_mml_line(r"\ei", 1, ValueError::NoInvertFlags.into());
    assert_one_error_in_mml_line(r"\ei c", 1, ValueError::InvalidMmlInvertFlags.into());
}

#[test]
fn set_echo_delay() {
    assert_line_matches_bytecode(r"\edl 0", &["set_echo_delay 0"]);

    assert_mml_channel_a_matches_bytecode(
        r#"
#MaxEchoLength 48

A \edl 48
"#,
        &["set_echo_delay 48"],
    );

    assert_mml_channel_a_matches_bytecode(
        r#"
#MaxEchoLength 240

A \edl 240
"#,
        &["set_echo_delay 240"],
    );

    assert_one_error_in_channel_a_mml(
        r#"
#MaxEchoLength 16

A \edl 32
"#,
        3,
        BytecodeError::EchoLengthLargerThanMaxEdl {
            edl: 2u8.try_into().unwrap(),
            max_edl: 1u8.try_into().unwrap(),
        }
        .into(),
    );
}
