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
    assert_one_error_in_mml_line(
        "{{P$4000 c}}",
        3,
        ValueError::PlayPitchPitchOutOfRange(0x4000).into(),
    );

    assert_line_matches_line("{{PR127999}},,0", "[PR127999,%2]12");
    assert_one_error_in_mml_line(
        "{{PR128000 c}}",
        3,
        ValueError::PlayPitchSampleRateOutOfRange(128000).into(),
    );
}

#[test]
fn play_pitch_freq() {
    assert_line_matches_line("{{PF700 PF1000}}4,16,0", "[PF700,16 PF1000,16]2");

    assert_one_error_in_mml_line(
        "{{PF16000}}",
        1,
        ValueError::CannotConvertPitchFrequency(
            PlayPitchFrequency::try_from(16000u32).unwrap(),
            0x20000,
        )
        .into(),
    );
    assert_one_error_in_mml_line(
        "{{PF16001 c}}",
        3,
        ValueError::PlayPitchFrequencyOutOfRange(16001).into(),
    );
}

#[test]
fn pitch_list_errors() {
    assert_one_error_in_mml_line("{{   ", 6, ChannelError::MissingEndBrokenChord);

    assert_one_error_in_mml_line("{{  }", 5, ChannelError::MissingEndBrokenChord);

    assert_one_error_in_mml_line("{{ [ a b }}", 4, ChannelError::InvalidPitchListSymbol);
}

// ::TODO broken chord argument error tests::

#[test]
fn slurred_broken_chord_tie1() {
    // from mml-syntax.md
    assert_line_matches_line("{{ceg}} &", "[c%1& e%1& g%1&]8");
    assert_line_matches_line("{{ce-g}}2,32 &", "[c32& : e-32& g32&]6");

    assert_line_matches_line("{{o2c o4c o3c}}%600 &", "[o2c%1& o4c%1& o3c%1&]200");
    assert_line_matches_line("{{o2c o4c o3c}}%700 &", "[o2c%1& : o4c%1& o3c%1&]234");

    assert_line_matches_line("{{ceg}}2,%5 &", "[c%5& e%5& g%5&]3 c%3&");
    assert_line_matches_line("{{ceg}}1,%5 &", "[c%5& : e%5& g%5&]7 e%1&");

    assert_line_matches_line(
        "{{a > c < b g}}%700,%8 &",
        "[o4a%8& o5c%8& o4b%8& : o4g%8&]22 o4g%4&",
    );

    assert_line_matches_line("{{PF700 PF1000}}4,16 &", "[PF700,16& PF1000,16&]2");
}

#[test]
fn slurred_broken_chord_tie0() {
    // from mml-syntax.md
    assert_line_matches_line("{{ceg}},,0 &", "[c%2 e%2 : g%2]4 g%2&");

    assert_line_matches_line("{{ce-g}}2,32,0 &", "[c32 e-32 g32]5 c32&");

    assert_line_matches_line(
        "{{o2c o4c o3c}}%600,,0 &",
        "[o2c%2 o4c%2 : o3c%2]100 o3c%2&",
    );
    assert_line_matches_line(
        "{{o2c o4c o3c}}%700,,0 &",
        "[o2c%2 : o4c%2 o3c%2]117 o4c%2&",
    );

    assert_line_matches_line("{{ceg}}2,%5,0 &", "[c%5 e%5 g%5]3 c%3&");
    assert_line_matches_line("{{ceg}}1,%5,0 &", "[c%5 : e%5 g%5]7 e%1&");

    assert_line_matches_line(
        "{{a > c < b g}}%700,%8,0 &",
        "[o4a%8 o5c%8 o4b%8 : o4g%8]22 o4g%4&",
    );

    assert_line_matches_line(
        "{{PF700 PF1000}}4,16,0 &",
        "[PF700,16 : PF1000,16]2 PF1000,16 &",
    );
}
