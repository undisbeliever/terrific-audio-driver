// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn broken_chord() {
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
fn play_pitch() {
    assert_line_matches_line(
        "{{P$1100 P$1200 P$1300}}",
        "[P$1100,%1 & : P$1200,%1 & P$1300,%1 &]8 P$1200,%2",
    );

    assert_line_matches_line("{{d P5678}}4,,0", "[d%2 P5678,%2]6");

    assert_line_matches_line("{{PR12000 PR18000}}4,16,0", "[PR12000,16 PR18000,16]2");

    assert_line_matches_line("{{P$3fff}},,0", "[P$3fff,%2]12");
    assert_error_in_mml_line(
        "{{P$4000 c}}",
        3,
        ValueError::PlayPitchPitchOutOfRange(0x4000).into(),
    );

    assert_line_matches_line("{{PR127999}},,0", "[PR127999,%2]12");
    assert_error_in_mml_line(
        "{{PR128000 c}}",
        3,
        ValueError::PlayPitchSampleRateOutOfRange(128000).into(),
    );
}

#[test]
fn play_pitch_freq() {
    assert_line_matches_line("{{PF700 PF1000}}4,16,0", "[PF700,16 PF1000,16]2");

    assert_error_in_mml_line(
        "{{PF16000}}",
        1,
        ValueError::CannotConvertPitchFrequency(
            PlayPitchFrequency::try_from(16000u32).unwrap(),
            0x20000,
        )
        .into(),
    );
    assert_error_in_mml_line(
        "{{PF16001 c}}",
        3,
        ValueError::PlayPitchFrequencyOutOfRange(16001).into(),
    );
}

#[test]
fn pitch_list_errors() {
    assert_error_in_mml_line("{{   ", 6, ChannelError::MissingEndBrokenChord);

    assert_error_in_mml_line("{{  }", 5, ChannelError::MissingEndBrokenChord);

    assert_error_in_mml_line("{{ [ a b }}", 4, ChannelError::InvalidPitchListSymbol);
}

// ::TODO broken chord argument error tests::
