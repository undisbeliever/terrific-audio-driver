// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn play_pitch() {
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
fn play_pitch_sample_rate() {
    assert_line_matches_line_and_bytecode(
        "PR32000 PR16000 PR64000 PR28000",
        "P$1000 P$0800 P$2000 P$0e00",
        &[
            "play_pitch $1000 keyoff 24",
            "play_pitch $0800 keyoff 24",
            "play_pitch $2000 keyoff 24",
            "play_pitch $0e00 keyoff 24",
        ],
    );

    assert_line_matches_line_and_bytecode("PR0", "P0", &["play_pitch 0 24"]);
    assert_line_matches_line_and_bytecode("PR127999", "P$3fff", &["play_pitch $3fff 24"]);

    assert_error_in_mml_line(
        "PR128000",
        1,
        ValueError::PlayPitchSampleRateOutOfRange(128000).into(),
    );
}

#[test]
fn play_pitch_freq() {
    assert_eq!(0x1000 * 600 / 500, 4915);
    assert_line_matches_line_and_bytecode("PF600", "P4915", &["play_pitch 4915 keyoff 24"]);

    assert_line_matches_line_and_bytecode("PF 0", "P 0", &["play_pitch 0 24"]);

    assert_line_matches_line("PF$123,%$30", "PF291,%48");

    assert_line_matches_line_and_bytecode(
        "PF500, %40 & PF1000,8.",
        "P 4096,%40 & P 8192,8.",
        &["play_pitch 4096 no_keyoff 40", "play_pitch 8192 18"],
    );

    assert_mml_channel_a_matches_bytecode(
        r#"
@1 f1000_o4
@2 f2000_o4

A @1 PF500 PF750 PF1000 PF2000 PF$0aaa
A @2 PF500 PF750 PF1000 PF2000 PF$0aaa
"#,
        &[
            "set_instrument f1000_o4",
            "play_pitch $0800 24",
            "play_pitch $0c00 24",
            "play_pitch $1000 24",
            "play_pitch $2000 24",
            "play_pitch $2bae 24",
            "set_instrument f2000_o4",
            "play_pitch $0400 24",
            "play_pitch $0600 24",
            "play_pitch $0800 24",
            "play_pitch $1000 24",
            "play_pitch $15d7 24",
        ],
    );

    assert_line_matches_line_and_bytecode("PF1999", "P$3ff8", &["play_pitch $3ff8 24"]);
    assert_error_in_mml_line(
        "PF2000",
        1,
        ValueError::CannotConvertPitchFrequency(
            PlayPitchFrequency::try_from(2000u32).unwrap(),
            0x4000,
        )
        .into(),
    );

    assert_error_in_mml_line(
        "PF16000",
        1,
        ValueError::CannotConvertPitchFrequency(
            PlayPitchFrequency::try_from(16000u32).unwrap(),
            0x20000,
        )
        .into(),
    );
    assert_error_in_mml_line(
        "PF16001",
        1,
        ValueError::PlayPitchFrequencyOutOfRange(16001).into(),
    );

    assert_error_in_mml_line("PF c", 1, ValueError::NoPlayPitchFrequency.into());

    assert_err_in_channel_a_mml(
        r#"
@s sample

A @s PF500
"#,
        6,
        ValueError::CannotConvertPitchFrequencySample.into(),
    );

    assert_one_subroutine_err_in_mml(
        r#"
@1 dummy_instrument

!s PF500
A @1 !s
"#,
        "!s",
        4,
        ValueError::CannotConvertPitchFrequencyUnknownInstrument.into(),
    );
}
