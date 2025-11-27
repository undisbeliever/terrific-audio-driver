// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn portamento() {
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

    assert_old_transpose_line_matches_line("{a > c}2", "{a _+12 c}2");
    assert_line_matches_line("{a > c}2", "{a _M+12 c}2");
    assert_line_matches_line("{o3 c o4 c}2", "{< c > c}2");

    assert_old_transpose_line_matches_line("{_+5 a __-3 c}2", "{_M+5 a __M-3 c}2");
}

#[test]
fn portamento_speed() {
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
fn portamento_with_speed_and_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g},,50

A @1 !s
"#,
        0,
        &["play_note c4 no_keyoff 1", "portamento g4 keyoff +50 23"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {g c},,50

A @1 !s
"#,
        0,
        &["play_note g4 no_keyoff 1", "portamento c4 keyoff -50 23"],
    );
}

#[test]
fn portamento_with_instrument_hint() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s ?@1 o4 {c g}

A @1 !s
"#,
        0,
        &["play_note c4 no_keyoff 1", "portamento g4 keyoff +49 23"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s ?@1 o4 {g c}

A @1 !s
"#,
        0,
        &["play_note g4 no_keyoff 1", "portamento c4 keyoff -49 23"],
    );
}

#[test]
fn portamento_errors() {
    assert_one_error_in_mml_line("{c g}4,4", 8, ChannelError::InvalidPortamentoDelay);
    assert_one_error_in_mml_line("l2 {c g},2", 10, ChannelError::InvalidPortamentoDelay);

    assert_one_error_in_mml_line(
        "{c g}4,,0",
        9,
        ValueError::PortamentoSpeedOutOfRange(0).into(),
    );
    assert_one_error_in_mml_line(
        "{c g}4,,800",
        9,
        ValueError::PortamentoSpeedOutOfRange(800).into(),
    );

    assert_one_error_in_mml_line("{c g}%0", 1, ChannelError::PortamentoTooShort);
    assert_one_error_in_mml_line("{c g}%1", 1, ChannelError::PortamentoTooShort);
    assert_one_error_in_mml_line("{c g}%10,%9", 1, ChannelError::PortamentoTooShort);

    assert_one_error_in_mml_line("{c c}4", 1, ValueError::PortamentoVelocityZero.into());
    assert_one_error_in_mml_line(
        "{c > c}16",
        1,
        ValueError::PortamentoVelocityOutOfRange(536).into(),
    );

    assert_one_error_in_mml_line("{c g}%$ffff", 1, ChannelError::PortamentoTooLong);

    // Tests if the TryFromIntError panic in ChannelBcGenerator::portamento() has been fixed
    assert_one_error_in_mml_line("{c g}%16387", 1, ChannelError::PortamentoTooLong);
    assert_one_error_in_mml_line("c & {c g}%16385 & a", 5, ChannelError::PortamentoTooLong);
}

#[test]
fn portamento_note_and_pitch_without_instrument_error() {
    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c P500},,50

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoNoteAndPitchWithoutInstrument,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {P500 c},,50

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoNoteAndPitchWithoutInstrument,
    );
}

#[test]
fn portamento_pitch() {
    assert_line_matches_bytecode(
        "{P1000 P1230}",
        &[
            "play_pitch 1000 no_keyoff 1",
            "portamento_pitch 1230 keyoff +10 23",
        ],
    );

    assert_line_matches_bytecode(
        "g {P$1500 P$1000}2,,20",
        &[
            "play_note g4 keyoff 24",
            "play_pitch $1500 no_keyoff 1",
            "portamento_pitch $1000 keyoff -20 47",
        ],
    );

    assert_line_matches_bytecode(
        "g & {P$1500 P$1000}2,,20",
        &[
            "play_note g4 no_keyoff 24",
            "play_pitch $1500 no_keyoff 1",
            "portamento_pitch $1000 keyoff -20 47",
        ],
    );

    assert_line_matches_bytecode(
        "P$1000 {P$1000 P$1300},,25",
        &[
            "play_pitch $1000 keyoff 24",
            "play_pitch $1000 no_keyoff 1",
            "portamento_pitch $1300 keyoff +25 23",
        ],
    );

    assert_line_matches_bytecode(
        "P$1000 & {P$1000 P$1300},,25",
        &[
            "play_pitch $1000 no_keyoff 24",
            "portamento_pitch $1300 keyoff +25 24",
        ],
    );

    assert_line_matches_bytecode(
        "P$1000 & {P$2000 P$2200}2,8",
        &[
            "play_pitch $1000 no_keyoff 24",
            "play_pitch $2000 no_keyoff 12",
            "portamento_pitch $2200 keyoff +15 36",
        ],
    );

    assert_line_matches_bytecode(
        "P$2000 & {P$2000 P$2200}2,8",
        &[
            "play_pitch $2000 no_keyoff 24",
            "wait 12",
            "portamento_pitch $2200 keyoff +15 36",
        ],
    );

    assert_line_matches_bytecode(
        "{g P$500}",
        &[
            "play_note g4 no_keyoff 1",
            "portamento_pitch $500 keyoff -88 23",
        ],
    );

    assert_line_matches_bytecode(
        "{P$500 g}",
        &["play_pitch $500 no_keyoff 1", "portamento g4 keyoff +88 23"],
    );
}

#[test]
fn portamento_pitch_sample_rate() {
    assert_line_matches_line_and_bytecode(
        "{PR32000 PR16000}",
        "{P$1000 P$0800}",
        &[
            "play_pitch $1000 no_keyoff 1",
            "portamento_pitch $0800 keyoff -93 23",
        ],
    );

    assert_line_matches_line_and_bytecode(
        "{PR64000 PR28000}",
        "{P$2000 P$0e00}",
        &[
            "play_pitch $2000 no_keyoff 1",
            "portamento_pitch $0e00 keyoff -209 23",
        ],
    );

    assert_line_matches_line("{PR0 PR127999}%1000", "{P0 P$3fff}%1000");

    assert_one_error_in_mml_line(
        "{PR128000 PR0}",
        2,
        ValueError::PlayPitchSampleRateOutOfRange(128000).into(),
    );
}

#[test]
fn portamento_pitch_freq() {
    assert_eq!((0x2000 - 0x1333 + 22 / 2) / 22, 149);
    assert_line_matches_line_and_bytecode(
        "{PF600 PF1000}",
        "{P$1333 P$2000}",
        &[
            "play_pitch $1333 no_keyoff 1",
            "portamento_pitch $2000 keyoff +149 23",
        ],
    );

    assert_eq!((0x199a - 0x2666 - 22 / 2) / 22, -149);
    assert_line_matches_line_and_bytecode(
        "{PF1200 PF800}",
        "{P$2666 P$199a}",
        &[
            "play_pitch $2666 no_keyoff 1",
            "portamento_pitch $199a keyoff -149 23",
        ],
    );

    assert_one_error_in_mml_line(
        "{PF0 PF16000}%1000",
        1,
        ValueError::CannotConvertPitchFrequency(
            PlayPitchFrequency::try_from(16000u32).unwrap(),
            0x20000,
        )
        .into(),
    );

    assert_one_error_in_mml_line(
        "c & {PF0 PF16001}",
        10,
        ValueError::PlayPitchFrequencyOutOfRange(16001).into(),
    );
}

#[test]
fn slurred_note_detune_then_portamento() {
    assert_line_matches_bytecode(
        "c & D+80 {c g}",
        &[
            "play_note c4 no_keyoff 24",
            // Must play a new note, the detune changed
            "set_detune +80",
            "play_note c4 no_keyoff 1",
            "portamento g4 keyoff +49 23",
        ],
    );

    assert_line_matches_bytecode(
        "MD+25 c & MD+50 {c g}",
        &[
            "set_detune +31",
            "play_note c4 no_keyoff 24",
            // Must play a new note, the detune changed
            "set_detune +63",
            "play_note c4 no_keyoff 1",
            "set_detune +94",
            "portamento g4 keyoff +50 23",
        ],
    );

    // D does not affect P
    assert_line_matches_bytecode(
        "P1000 & D+1000 {P1000 P1500}",
        &[
            "play_pitch 1000 no_keyoff 24",
            "set_detune +1000",
            "portamento_pitch 1500 keyoff +22 24",
        ],
    );

    // MD does not affect P
    assert_line_matches_bytecode(
        "P1000 & MD+20 {P1000 P1500}",
        &[
            "play_pitch 1000 no_keyoff 24",
            "portamento_pitch 1500 keyoff +22 24",
        ],
    );
}

#[test]
fn pitch_and_detune() {
    const A4_PITCH: u32 = 0x0e14;

    assert_line_matches_bytecode(
        "D+80 P1000 & {P1000 P1500}",
        &[
            "set_detune +80",
            "play_pitch 1000 no_keyoff 24",
            "portamento_pitch 1500 keyoff +22 24",
        ],
    );

    // MD does not affect P
    assert_line_matches_bytecode(
        "MD+80 P1000 & {P1000 P1500}",
        &[
            "play_pitch 1000 no_keyoff 24",
            "portamento_pitch 1500 keyoff +22 24",
        ],
    );

    // MD affects note, not pitch
    assert_eq!((A4_PITCH - 52) + 50 * (12 - 2), 4052);
    assert_line_matches_bytecode(
        "MD-25 {a P4052}%12",
        &[
            "set_detune -52",
            "play_note a4 no_keyoff 1",
            "portamento_pitch 4052 keyoff +50 11",
        ],
    );
    assert_line_matches_bytecode(
        "MD-25 {P4052 a}%12",
        &[
            "play_pitch 4052 no_keyoff 1",
            "set_detune -52",
            "portamento a4 keyoff -50 11",
        ],
    );

    // D affects note, not pitch
    assert_eq!((A4_PITCH + 80) - 60 * (12 - 2), 3084);
    assert_line_matches_bytecode(
        "D+80 {a P3084}%12",
        &[
            "set_detune +80",
            "play_note a4 no_keyoff 1",
            "portamento_pitch 3084 keyoff -60 11",
        ],
    );
    assert_line_matches_bytecode(
        "D+80 {P3084 a}%12",
        &[
            "set_detune +80",
            "play_pitch 3084 no_keyoff 1",
            "portamento a4 keyoff +60 11",
        ],
    );
}

#[test]
fn previous_slurred_note_and_pitch() {
    const _A4_PITCH: u32 = 0x0e14;

    assert_line_matches_bytecode(
        "a & {P$0e14 P$1000}",
        &[
            "play_note a4 no_keyoff 24",
            "portamento_pitch $1000 keyoff +21 24",
        ],
    );

    assert_line_matches_bytecode(
        "P$0e14 & {a P$1000}",
        &[
            "play_pitch $e14 no_keyoff 24",
            "portamento_pitch $1000 keyoff +21 24",
        ],
    );

    assert_line_matches_bytecode(
        "a & {P$0e14 P$1000}2,8",
        &[
            "play_note a4 no_keyoff 24",
            "wait 12",
            "portamento_pitch $1000 keyoff +14 36",
        ],
    );

    assert_line_matches_bytecode(
        "P$0e14 & {a P$1000}2,8",
        &[
            "play_pitch $e14 no_keyoff 24",
            "wait 12",
            "portamento_pitch $1000 keyoff +14 36",
        ],
    );
}

#[test]
fn previous_slurred_note_has_different_instrument() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

@f1000_o3_o5 f1000_o3_o5
@f2000_o3_o5 f2000_o3_o5

A @f1000_o4 a & @f1000_o3_o5 {ae}
A @f2000_o4 a & @f2000_o3_o5 {ae}
A @f1000_o4 a & @f2000_o4    {ae}
"##,
        &[
            "set_instrument f1000_o4",
            "play_note a4 no_keyoff 24",
            "set_instrument f1000_o3_o5",
            // Previous slurred note is a4 @f1000_o4 - same instrument tuning
            "portamento e4 keyoff -20 24",
            //
            // newline
            "set_instrument f2000_o4",
            "play_note a4 no_keyoff 24",
            "set_instrument f2000_o3_o5",
            // previous slurred note instruement is a4 @f2000_o4 - same instrument tuning
            "portamento e4 keyoff -10 24",
            //
            // newline
            "set_instrument f1000_o4",
            "play_note a4 no_keyoff 24",
            "set_instrument f2000_o4",
            // previous slurred note instruement has a different frequency
            "play_note a4 no_keyoff 1",
            "portamento e4 keyoff -10 23",
        ],
    );

    // Test without slurs
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

@f1000_o3_o5 f1000_o3_o5
@f2000_o3_o5 f2000_o3_o5

A @f1000_o4 a @f1000_o3_o5 {ae}
A @f2000_o4 a @f2000_o3_o5 {ae}
A @f1000_o4 a @f2000_o4    {ae}
"##,
        &[
            "set_instrument f1000_o4",
            "play_note a4 keyoff 24",
            "set_instrument f1000_o3_o5",
            // Previous note is not slurred
            "play_note a4 no_keyoff 1",
            "portamento e4 keyoff -21 23",
            //
            // newline
            "set_instrument f2000_o4",
            "play_note a4 keyoff 24",
            "set_instrument f2000_o3_o5",
            // Previous note is not slurred
            "play_note a4 no_keyoff 1",
            "portamento e4 keyoff -10 23",
            //
            // newline
            "set_instrument f1000_o4",
            "play_note a4 keyoff 24",
            "set_instrument f2000_o4",
            // previous note instruement has a different frequency
            "play_note a4 no_keyoff 1",
            "portamento e4 keyoff -10 23",
        ],
    );
}

// Test instrument is ignored if first portamento parameter is a P pitch
#[test]
fn previous_slurred_instrument_ignored_if_note1_is_pitch() {
    assert_eq!(0x1800 - 45 * 23, 0x13f5);
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

A @f1000_o4 P$1800 & @f2000_o4 { P$1800 P$13f5 }
"##,
        &[
            "set_instrument f1000_o4",
            "play_pitch $1800 no_keyoff 24",
            "set_instrument f2000_o4",
            // pitch matches, instrument not checked
            "portamento_pitch $13f5 keyoff -45 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

A @f1000_o4 P$1500 & @f2000_o4 { P$1500 g }
"##,
        &[
            "set_instrument f1000_o4",
            "play_pitch $1500 no_keyoff 24",
            "set_instrument f2000_o4",
            // pitch matches, instrument not checked
            "portamento g4 keyoff -199 24",
        ],
    );

    const _A4_PITCH: u32 = 0x070a;
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

A @f1000_o4 a & @f2000_o4 { P$70a g }
"##,
        &[
            "set_instrument f1000_o4",
            "play_note a4 no_keyoff 24",
            "set_instrument f2000_o4",
            // pitch matches, instrument not checked
            "portamento g4 keyoff -43 24",
        ],
    );

    // Also test detune
    assert_eq!(_A4_PITCH - 50, 0x6d8);
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

A D-50 @f1000_o4 a & @f2000_o4 { P$6d8 g }
"##,
        &[
            "set_detune -50",
            "set_instrument f1000_o4",
            "play_note a4 no_keyoff 24",
            "set_instrument f2000_o4",
            // pitch matches, instrument not checked
            "portamento g4 keyoff -43 24",
        ],
    );
}

/// Test if `last_slurred_note` is correctly tracked after a *skip last loop* command.
/// Assumes `test_portamento()` passes
#[test]
fn previous_slurred_note_after_skip_last_loop_1() {
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
fn previous_slurred_loop_after_skip_last_loop_2() {
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
fn previous_slurred_note_after_start_loop() {
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
fn previous_slurred_instrument_after_subroutine_call() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@f1000_o4 f1000_o4
@f2000_o4 f2000_o4

@f1000_o3_o5 f1000_o3_o5
@f2000_o3_o5 f2000_o3_o5

!sc a&

!s1           !sc w
!s2 @f2000_o4 !sc w

A @f1000_o4 g !s1 @f1000_o4 {ae}
A @f1000_o4 g !s2 @f1000_o4 {ae}

; Different instruments with instrument source frequencies
A @f1000_o4 g !s1 @f1000_o3_o5 {ae}
A @f1000_o4 g !s2 @f2000_o3_o5 {ae}
"##,
        &[
            "set_instrument f1000_o4",
            "play_note g4 24",
            "call_subroutine s1",
            // Previous slurred note is a4 f1000_o4
            // previous instruement is f1000_o4
            "portamento e4 keyoff -20 24",
            //
            // newline
            // previous instruement is f1000_o4
            "play_note g4 24",
            "call_subroutine s2",
            // previous instruement is f2000_o4
            "set_instrument f1000_o4",
            // Previous slurred note is a4 f2000_o4 (instrument frequency does not match)
            "play_note a4 no_keyoff 1",
            "portamento e4 keyoff -21 23",
            //
            // newline
            // previous instruement is f1000_o4
            "play_note g4 24",
            "call_subroutine s1",
            // previous instruement is f1000_o4
            "set_instrument f1000_o3_o5",
            // Previous slurred note is a4 f1000_o4 (instrument frequency matches)
            "portamento e4 keyoff -20 24",
            //
            // newline
            // previous instruement is f1000_o3_i5
            "set_instrument f1000_o4",
            "play_note g4 24",
            "call_subroutine s2",
            // previous instruement is f2000_o4
            "set_instrument f2000_o3_o5",
            // Previous slurred note is a4 f2000_o4 (instrument frequency matches)
            "portamento e4 keyoff -10 24",
        ],
    );
}

#[test]
fn previous_slurred_note_after_subroutine_call() {
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
fn previous_slurred_note_after_nested_subroutine_call() {
    // see `test_skip_last_loop_prev_slurred_note()`

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 a
!s2 !s1 w

A @0
A a& !s1 {ab},,10
A a& !s2 {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            // newline
            "play_note a4 no_keyoff 24",
            "call_subroutine s1",
            // Previous note is a4 and NOT slurred
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
            // newline
            "play_note a4 no_keyoff 24",
            "call_subroutine s2",
            // Previous note is a4 and NOT slurred
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 a&
!s2 !s1 w

A @0 g !s1 {ab},,10
A @0 g !s2 {ab},,10
"##,
        &[
            // newline
            "set_instrument dummy_instrument",
            "play_note g4 24",
            "call_subroutine s1",
            // Previous slurred note is a4
            "portamento b4 keyoff +10 24",
            // newline
            // instument unchanged
            "play_note g4 24",
            "call_subroutine s2",
            // Previous slurred note is a4
            "portamento b4 keyoff +10 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 r10
!s2 !s1 w

A @0 a& !s1 {ab},,10
A @0 a& !s2 {ab},,10
"##,
        &[
            // newline
            "set_instrument dummy_instrument",
            "play_note a4 no_keyoff 24",
            "call_subroutine s1",
            // s1 will rest and key-off
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
            // newline
            // instrument unchanged
            "play_note a4 no_keyoff 24",
            "call_subroutine s2",
            // s2 will rest and key-off
            "play_note a4 no_keyoff 1",
            "portamento b4 keyoff +10 23",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 w1
!s2 w2

A @0
A a& !s1 {ab},,10
A a& !s2 {ab},,10
"##,
        &[
            "set_instrument dummy_instrument",
            // newline
            "play_note a4 no_keyoff 24",
            "call_subroutine s1",
            // s1 does not key-off and does not play a note
            // Previous slurred note is still a4
            "portamento b4 keyoff +10 24",
            // newline
            "play_note a4 no_keyoff 24",
            "call_subroutine s2",
            // s2 does not key-off and does not play a note
            // Previous slurred note is still a4
            "portamento b4 keyoff +10 24",
        ],
    );
}

#[test]
fn one_pitch_portamento() {
    const _A4_PITCH: u32 = 0x0e14;

    assert_line_matches_bytecode(
        "a & {e}",
        &["play_note a4 no_keyoff 24", "portamento e4 keyoff -39 24"],
    );

    assert_line_matches_bytecode(
        "a & {e}8",
        &["play_note a4 no_keyoff 24", "portamento e4 keyoff -82 12"],
    );

    assert_line_matches_bytecode(
        "a & {e}3,8",
        &[
            "play_note a4 no_keyoff 24",
            "wait 12",
            "portamento e4 keyoff -48 20",
        ],
    );

    assert_line_matches_bytecode(
        "e & w2 {a}3,8",
        &[
            "play_note e4 no_keyoff 24",
            "wait 48",
            // portamento
            "wait 12",
            "portamento a4 keyoff +48 20",
        ],
    );

    assert_line_matches_bytecode(
        "a & {e}3 & {a}6 & {g}3",
        &[
            "play_note a4 no_keyoff 24",
            "portamento e4 no_keyoff -28 32",
            "portamento a4 no_keyoff +57 16",
            "portamento g4 keyoff -13 32",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s ?@1 e & {a}
A @1 !s
"#,
        0,
        &["play_note e4 no_keyoff 24", "portamento a4 keyoff +39 24"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s a & {e},,50
A @1 !s
"#,
        0,
        &["play_note a4 no_keyoff 24", "portamento e4 keyoff -50 24"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s e & {a},,50
A @1 !s
"#,
        0,
        &["play_note e4 no_keyoff 24", "portamento a4 keyoff +50 24"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s P$1000 & {P$2000}2
A @1 !s
"#,
        0,
        &[
            "play_pitch $1000 no_keyoff 24",
            "portamento_pitch $2000 keyoff +87 48",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s P$1000 & {P$2000}2,8
A @1 !s
"#,
        0,
        &[
            "play_pitch $1000 no_keyoff 24",
            "wait 12",
            "portamento_pitch $2000 keyoff +117 36",
        ],
    );
}

#[test]
fn one_pitch_portamento_errors() {
    assert_one_error_in_mml_line("{e}", 1, ChannelError::OneNotePortamentoNoPreviousNote);

    // Note is unknown at the start of a loop
    assert_one_error_in_mml_line(
        "a {e}3,8",
        3,
        ChannelError::OneNotePortamentoPreviousNoteIsNotSlurred,
    );

    // Previous note is noise
    assert_one_error_in_mml_line(
        "N10 & {e}",
        7,
        ChannelError::OneNotePortamentoPreviousNoteIsNoise,
    );

    // Previous note is unknown at the start of a loop
    assert_one_error_in_mml_line(
        "a & [{e} a&]3",
        6,
        ChannelError::OneNotePortamentoPreviousNoteIsUnknown,
    );

    // Previous note is unknown at the start of a loop
    assert_one_error_in_mml_line(
        "a & L {e} c",
        7,
        ChannelError::OneNotePortamentoPreviousNoteIsUnknown,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s {e},,35
A @1 !s
"#,
        "!s",
        4,
        ChannelError::OneNotePortamentoNoPreviousNote,
    );
}

#[test]
fn portamento_pitch_list_errors() {
    assert_one_error_in_mml_line("{   ", 5, ChannelError::MissingEndPortamento);

    assert_one_error_in_mml_line("{ }}", 3, ChannelError::MissingEndPortamento);

    assert_one_error_in_mml_line("{ [ a b }", 3, ChannelError::InvalidPitchListSymbol);

    assert_one_error_in_mml_line("{ a }", 1, ChannelError::OneNotePortamentoNoPreviousNote);
    assert_one_error_in_mml_line("{ a b c }", 1, ChannelError::PortamentoRequiresTwoPitches);
    assert_one_error_in_mml_line("{ a b c d }", 1, ChannelError::PortamentoRequiresTwoPitches);

    assert_one_error_in_mml_line(
        "{ P2000 }",
        1,
        ChannelError::OneNotePortamentoNoPreviousNote,
    );
    assert_one_error_in_mml_line(
        "{ P2000 P1000 P500 }",
        1,
        ChannelError::PortamentoRequiresTwoPitches,
    );

    assert_one_error_in_mml_line("{ a a }", 1, ValueError::PortamentoVelocityZero.into());
    assert_one_error_in_mml_line(
        "{ o5 a o5 a }",
        1,
        ValueError::PortamentoVelocityZero.into(),
    );
    assert_one_error_in_mml_line(
        "{ o5 a o3 >> a }",
        1,
        ValueError::PortamentoVelocityZero.into(),
    );
    assert_one_error_in_mml_line(
        "{ P512 P512 }",
        1,
        ValueError::PortamentoVelocityZero.into(),
    );
}

// ::TODO add portamento length error tests::

#[test]
fn portamento_note_tracking_bugfix_1() {
    // Confirm a note tracking buf that caused the 2nd and 3rd portamento to not key-on is fixed
    assert_line_matches_bytecode(
        "{cd} {cf} {cg}",
        &[
            "play_note c4 no_keyoff 1",
            "portamento d4 keyoff +12 23",
            "play_note c4 no_keyoff 1",
            "portamento f4 keyoff +33 23",
            "play_note c4 no_keyoff 1",
            "portamento g4 keyoff +49 23",
        ],
    );

    assert_eq!(4608, 0x1200);
    assert_line_matches_bytecode(
        "{P500 P4608} {P$1200 P$1800} {P$1800 P$1000}",
        &[
            "play_pitch 500 no_keyoff 1",
            "portamento_pitch 4608 keyoff +187 23",
            "play_pitch $1200 no_keyoff 1",
            "portamento_pitch $1800 keyoff +70 23",
            "play_pitch $1800 no_keyoff 1",
            "portamento_pitch $1000 keyoff -93 23",
        ],
    );
}

#[test]
fn portamento_note_tracking_bugfix_2() {
    assert_line_matches_bytecode(
        "{cd} & {df} & {fg}",
        &[
            "play_note c4 no_keyoff 1",
            "portamento d4 no_keyoff +11 23",
            "portamento f4 no_keyoff +19 24",
            "portamento g4 keyoff +15 24",
        ],
    );

    assert_eq!(4608, 0x1200);
    assert_line_matches_bytecode(
        "{P500 P4608} & {P$1200 P$1800} & {P$1800 P$1000}",
        &[
            "play_pitch 500 no_keyoff 1",
            "portamento_pitch 4608 no_keyoff +179 23",
            "portamento_pitch $1800 no_keyoff +64 24",
            "portamento_pitch $1000 keyoff -89 24",
        ],
    );
}

#[test]
fn portamento_panic_bugfix() {
    assert_one_error_in_mml_line("{c}3", 1, ChannelError::OneNotePortamentoNoPreviousNote);

    assert_one_error_in_mml_line("{}", 1, ChannelError::PortamentoRequiresTwoPitches);
}

#[test]
fn single_pitch_portamento_after_songloop_bugfix() {
    assert_mml_channel_a_matches_looping_bytecode(
        r##"
@1 dummy_instrument

A @1 c& {d} L c& {d} c& {d}"##,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 no_keyoff 24",
            "portamento d4 keyoff +11 24",
            // Song loop point
            "play_note c4 no_keyoff 24",
            "portamento d4 keyoff +11 24",
            "play_note c4 no_keyoff 24",
            "portamento d4 keyoff +11 24",
        ],
    );
}

#[test]
fn portamento_in_subroutine_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g}

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc g4 keyoff 22 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {g c}

A @1 !s
"#,
        0,
        &[
            "play_note g4 no_keyoff 1",
            "portamento_calc c4 keyoff 22 23",
        ],
    );
}

#[test]
fn portamento_pitch_pitch_in_subroutine_with_no_instrument() {
    // Confirm velocity is calculated by the compiler

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s {P$1000 P$1300}

A @1 !s
"#,
        0,
        &[
            "play_pitch $1000 no_keyoff 1",
            "portamento_pitch $1300 keyoff +35 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s {P$1300 P$1000}

A @1 !s
"#,
        0,
        &[
            "play_pitch $1300 no_keyoff 1",
            "portamento_pitch $1000 keyoff -35 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s P$1000 & {P$1000 P$1200}

A @1 !s
"#,
        0,
        &[
            "play_pitch $1000 no_keyoff 24",
            "portamento_pitch $1200 keyoff +22 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s P$1200 & {P$1200 P$1000}

A @1 !s
"#,
        0,
        &[
            "play_pitch $1200 no_keyoff 24",
            "portamento_pitch $1000 keyoff -22 24",
        ],
    );
}

#[test]
fn portamento_note_and_pitch_in_subroutine_with_no_instrument() {
    // confirm velocity is unknown

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s {c P$1000}

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_pitch_calc $1000 keyoff 22 23",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s {P$1000 c}

A @1 !s
"#,
        0,
        &[
            "play_pitch $1000 no_keyoff 1",
            "portamento_calc c4 keyoff 22 23",
        ],
    );
}

#[test]
fn portamento_in_subroutine_with_no_instrument_slide_length_too_long_test() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g}%257

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc g4 keyoff 255 256",
        ],
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c g}%258

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 256,
            tuning_unknown: true,
            transpose_active: false,
        },
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g}%456,%200

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 200",
            "portamento_calc g4 keyoff 255 256",
        ],
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c g}%457,%200

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 256,
            tuning_unknown: true,
            transpose_active: false,
        },
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g}%256 & c

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc g4 no_keyoff 255 255",
            "play_note c4 24",
        ],
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c g}%257 & c

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 256,
            tuning_unknown: true,
            transpose_active: false,
        },
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c g}%455,%200 & c

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 200",
            "portamento_calc g4 no_keyoff 255 255",
            "play_note c4 24",
        ],
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c g}%456,%200 & c

A @1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 256,
            tuning_unknown: true,
            transpose_active: false,
        },
    );
}

#[test]
fn portamento_calc_error_with_transpose_active() {
    assert_line_matches_bytecode(
        "_+1 {c g}%257",
        &[
            "set_transpose +1",
            "play_note c4 no_keyoff 1",
            "portamento_calc g4 keyoff 255 256",
        ],
    );

    assert_one_error_in_mml_line(
        "_+1 {c g}%258",
        5,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 256,
            tuning_unknown: false,
            transpose_active: true,
        },
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 @1 {c g}%1000 r

A @1 _+1 !s
"#,
        "!s",
        10,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 998,
            tuning_unknown: false,
            transpose_active: true,
        },
    );
}

#[test]
fn portamento_calc_error_with_unknown_instrument_and_transpose_active() {
    assert_one_error_in_mml_line(
        "_+1 [ {c g}%1000 \\asm { set_instrument f1000_o4 } ]4",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 998,
            tuning_unknown: true,
            transpose_active: true,
        },
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s o4 {c g}%1000 r

A @1 _+1 !s
"#,
        "!s",
        7,
        ChannelError::PortamentoCalcSlideTicksOutOfRange {
            ticks: 998,
            tuning_unknown: true,
            transpose_active: true,
        },
    );
}

#[test]
fn slurred_portamento_then_rest_in_subroutine_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd}3 & r8

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 no_keyoff 31 31",
            "rest 12",
        ],
    );
}

#[test]
fn slurred_portamento_then_wait_in_subroutine_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd}3 & w8

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 no_keyoff 31 31",
            "wait 12",
        ],
    );
}

#[test]
fn portamento_tie_within_subroutine_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd} ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 23 47",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd},8 ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 12",
            "portamento_calc d4 keyoff 12 36",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd}%100,%25 ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 25",
            "portamento_calc d4 keyoff 75 99",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%10 & {cd}%100,%25 ^%40

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 10",
            "wait 25",
            "portamento_calc d4 keyoff 75 115",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd}8 ^3

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 keyoff 11 43",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {cd}8 ^3 &

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 no_keyoff 11 43",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c & {e} ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 24",
            "portamento_calc e4 keyoff 24 48",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c3 & {e}3 ^16 ^16

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 32",
            "portamento_calc e4 keyoff 32 44",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%1 & {e}%1 ^%2

A @1 !s
"#,
        0,
        &["play_note c4 no_keyoff 1", "portamento_calc e4 keyoff 1 3"],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%10 & {e}%10,%3 ^%2

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 10",
            "wait 3",
            "portamento_calc e4 keyoff 7 9",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%1 & {e}%1 ^%1 &

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc e4 no_keyoff 1 2",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%10 & {e}%10,%3 ^%1 &

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 10",
            "wait 3",
            "portamento_calc e4 no_keyoff 7 8",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%10 & {e}%10,%3 ^%300

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 10",
            "wait 3",
            "portamento_calc e4 keyoff 7 307",
        ],
    );
}

#[test]
fn no_keyoff_portamento_tie_within_subroutine_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s K0 o4 {cd} ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 1",
            "portamento_calc d4 no_keyoff 23 47",
            "keyon_next_note",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s K0 o4 {cd}3,4 ^%600

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 24",
            "portamento_calc d4 no_keyoff 8 608",
            "keyon_next_note",
        ],
    );
}

#[test]
fn portamento_note_pitch_and_tie_with_no_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {P$800 c},8 ^

A @1 !s
"#,
        0,
        &[
            "play_pitch $800 no_keyoff 12",
            "portamento_calc c4 keyoff 12 36",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 {c P$1000},8 ^

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 12",
            "portamento_pitch_calc $1000 keyoff 12 36",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s o4 c%10 & {P$1800}%10,%3 ^%300

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 10",
            "wait 3",
            "portamento_pitch_calc $1800 keyoff 7 307",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument

!s K0 o4 {c P$2000}3,4 ^%600

A @1 !s
"#,
        0,
        &[
            "play_note c4 no_keyoff 24",
            "portamento_pitch_calc $2000 no_keyoff 8 608",
            "keyon_next_note",
        ],
    );
}

// Loop Analysis
// =============

mod loop_analysis {
    use crate::*;

    const PORTAMENTO_1: &str = "portamento d4 keyoff  +6 23";
    const PORTAMENTO_2: &str = "portamento d4 keyoff  +3 23";
    const PORTAMENTO_3: &str = "portamento d4 keyoff  +2 23";
    const PORTAMENTO_CALC: &str = "portamento_calc d4 keyoff 22 23";

    #[test]
    fn mml_loops() {
        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5

A @14 {cd} [ {cd} @13 ]2
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Same instrument tuning in loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o3_o5",
                "end_loop 2",
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4

A @14 {cd} [ {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // No instrument change in loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

A @14 {cd} [ {cd} @24 ]2
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Different instrument tuning in loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f2000_o4",
                "end_loop 2",
            ],
        );
    }

    #[test]
    fn skip_last_loop() {
        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5

A @14 {cd} [ {cd} @14 {cd} : @13 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Same instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "set_instrument f1000_o3_o5",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

A @14 {cd} [ {cd} @14 {cd} : @24 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Different instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

A @14 {cd} [ {cd} @24 {cd} : @14 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Same instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "skip_last_loop",
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
            ],
        );
    }

    #[test]
    fn loop_and_subroutine() {
        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5
@24 f2000_o4

!s1 @13 {cd}

A @14 {cd} [ {cd} @24 {cd} : !s1 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Instrument tuning unchanged at loop start
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "skip_last_loop",
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

!s1 @14 {cd}
!s2 @24 {cd}

A @14 {cd} [ {cd} !s1 {cd} : !s2 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Different instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "call_subroutine s2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

!s1 @14 {cd}
!s2 @24 {cd}

A @14 {cd} [ {cd} !s1 {cd} : !s2 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Different instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "call_subroutine s2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

!s1 @14 {cd}
!s2 @24 {cd}

A @14 {cd} [ {cd} !s2 {cd} : !s1 {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Same instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "call_subroutine s2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "skip_last_loop",
                "call_subroutine s1",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
            ],
        );
    }

    #[test]
    fn loop_and_instrument_asm() {
        assert_mml_channel_a_matches_bytecode(
            r##"
A \asm { set_instrument f1000_o4 }
A [ {cd} \asm { set_instrument_and_adsr f1000_o4 1 2 3 4 } {cd} : \asm { set_instrument_and_gain f2000_o4 F127 } {cd} ]2 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // Different instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument_and_adsr f1000_o4 1 2 3 4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "set_instrument_and_gain f2000_o4 F127",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
A \asm {
    set_instrument f1000_o4
    start_loop
        set_instrument_and_adsr f2000_o4 1 2 3 4
        rest 24
        set_instrument_and_gain f1000_o4 F127
        rest 24
    end_loop 2
}
A {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "start_loop",
                "set_instrument_and_adsr f2000_o4 1 2 3 4",
                "rest 24",
                "set_instrument_and_gain f1000_o4 F127",
                "rest 24",
                "end_loop 2",
                // Instrument is f1000_o4
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
A \asm {
    set_instrument f1000_o4
    start_loop
        set_instrument_and_adsr f2000_o4 1 2 3 4
        rest 24
        skip_last_loop
        set_instrument_and_gain f1000_o4 F127
        rest 24
    end_loop 2
}
A {cd}
"##,
            &[
                "set_instrument f1000_o4",
                "start_loop",
                "set_instrument_and_adsr f2000_o4 1 2 3 4",
                "rest 24",
                "skip_last_loop",
                "set_instrument_and_gain f1000_o4 F127",
                "rest 24",
                "end_loop 2",
                // Instrument is f2000_o4
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
            ],
        );
    }

    #[test]
    fn nested_loops() {
        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5

A @14 [ [ {cd} @13 {cd} ]2 @14 ]3 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // Same instrument tuning at start of loop
                "start_loop",
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o3_o5",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "set_instrument f1000_o4",
                "end_loop 3",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5
@24 f2000_o4

A @14 [ [ {cd} @14 {cd} @13 {cd} ]2 : @24 {cd} ]3 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // Different instrument tuning at start of loop
                "start_loop",
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o3_o5",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                "skip_last_loop",
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 3",
                // loop ends with @13
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

A @14 [ [ {cd} @14 {cd} : @24 {cd} ]2 {cd} ]3 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // Same instrument tuning at start of loop
                "start_loop",
                // Different instrument tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                // instrument @14 here
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 3",
                // loop ends with @14
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4
@34 f3000_o4

A @14 [ {cd} @24 [ {cd} @14 {cd} @24 {cd} ]2 {cd} : @34 {cd} ]3 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // @14, @34 tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f2000_o4",
                // Same tuning at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f2000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "end_loop 2",
                // instrument @24 here
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
                "skip_last_loop",
                "set_instrument f3000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_3,
                "end_loop 3",
                // loop ends with @24
                "play_note c4 no_keyoff 1",
                PORTAMENTO_2,
            ],
        );

        assert_mml_channel_a_matches_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5

A @14 [ {cd} [ {cd} @13 {cd} ]2 @13 {cd} : {cd} ]3 @13 {cd}
"##,
            &[
                "set_instrument f1000_o4",
                // @14, @13 at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // @14, @13 at start of loop
                "start_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o3_o5",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 2",
                // instrument @13 here
                // @13 optimised out
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "skip_last_loop",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "end_loop 3",
                // loop ends with @13
                // @13 optimised out
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
            ],
        );
    }

    #[test]
    fn song_loop() {
        assert_mml_channel_a_matches_looping_bytecode(
            r##"
@14 f1000_o4
@13 f1000_o3_o5

A @14 {cd} L {cd} @13 r
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Loop point - same instrument tuning
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                "set_instrument f1000_o3_o5",
                "rest 24",
            ],
        );

        assert_mml_channel_a_matches_looping_bytecode(
            r##"
@14 f1000_o4
@24 f2000_o4

A @14 {cd} L {cd} @24 r
"##,
            &[
                "set_instrument f1000_o4",
                "play_note c4 no_keyoff 1",
                PORTAMENTO_1,
                // Loop point - unknown instrument tuning
                "play_note c4 no_keyoff 1",
                PORTAMENTO_CALC,
                "set_instrument f2000_o4",
                "rest 24",
            ],
        );
    }
}
