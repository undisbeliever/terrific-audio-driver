// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::echo::{EchoBuffer, FirCoefficient};
use compiler::invert_flags::InvertFlags;
use compiler::mml::{GlobalSettings, MetaData};
use compiler::notes::KeySignature;
use compiler::time::{Bpm, TickClock, ZenLen};
use compiler::{Transpose, UnsignedValueNewType};

use crate::*;

#[test]
fn all_headers() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#Title song-title
#Game song-game
#Date song-date
#Composer song-composer
#Author song-author
#Copyright song-copyright
#License song-license
#ZenLen 192
#Transpose +2
#KeySignature +fc ; D Major
#OldTranspose
#MaxEchoLength 64
#EchoLength 32
#FirFilter 1 2 -3 -4 $5 $6 $77 $88
#DisableFirFilterLimit
#EchoFeedback 12
#EchoVolume 34 56
#EchoInvert R
#Timer 100
#SpcSongLength 300
#SpcFadeout 200

A r
"#,
        &dummy_data,
    );

    assert_eq!(
        s.metadata(),
        &MetaData {
            title: Some("song-title".to_owned()),
            game: Some("song-game".to_owned()),
            date: Some("song-date".to_owned()),
            composer: Some("song-composer".to_owned()),
            author: Some("song-author".to_owned()),
            copyright: Some("song-copyright".to_owned()),
            license: Some("song-license".to_owned()),
            echo_buffer: EchoBuffer {
                max_edl: (64u8 / 16).try_into().unwrap(),
                edl: (32u8 / 16).try_into().unwrap(),
                fir: [1, 2, -3, -4, 0x5, 0x6, 0x77, -120].map(|i: i8| FirCoefficient::new(i)),
                feedback: 12.try_into().unwrap(),
                echo_volume_l: 34u8.try_into().unwrap(),
                echo_volume_r: 56u8.try_into().unwrap(),
                invert: InvertFlags {
                    right: true,
                    left: false,
                    mono: false
                },
            },
            tick_clock: 100u16.try_into().unwrap(),
            mml_settings: GlobalSettings {
                zenlen: 192u8.try_into().unwrap(),
                channel_transpose: Transpose::new(2),
                signature: KeySignature::default()
                    .parse_signature_changes("+fc")
                    .unwrap(),
                old_transpose: true,
            },
            spc_song_length: Some(300),
            spc_fadeout_millis: Some(200),
        }
    );

    let s = compile_mml(
        r#"
#Tempo 144

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().tick_clock,
        Bpm::try_from(144u8).unwrap().to_tick_clock().unwrap()
    );
}

#[test]
fn all_headers_lower_camel_case() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#title song-title
#game song-game
#date song-date
#composer song-composer
#author song-author
#copyright song-copyright
#license song-license
#zenLen 192
#transpose +2
#keySignature +fc ; D Major
#oldTranspose
#maxEchoLength 64
#echoLength 32
#firFilter 1 2 -3 -4 $5 $6 $77 $88
#disableFirFilterLimit
#echoFeedback 12
#echoVolume 34 56
#echoInvert right
#timer 100
#spcSongLength 300
#spcFadeout 200

A r
"#,
        &dummy_data,
    );

    assert_eq!(
        s.metadata(),
        &MetaData {
            title: Some("song-title".to_owned()),
            game: Some("song-game".to_owned()),
            date: Some("song-date".to_owned()),
            composer: Some("song-composer".to_owned()),
            author: Some("song-author".to_owned()),
            copyright: Some("song-copyright".to_owned()),
            license: Some("song-license".to_owned()),
            echo_buffer: EchoBuffer {
                max_edl: (64u8 / 16).try_into().unwrap(),
                edl: (32u8 / 16).try_into().unwrap(),
                fir: [1, 2, -3, -4, 0x5, 0x6, 0x77, -120].map(|i: i8| FirCoefficient::new(i)),
                feedback: 12.try_into().unwrap(),
                echo_volume_l: 34u8.try_into().unwrap(),
                echo_volume_r: 56u8.try_into().unwrap(),
                invert: InvertFlags {
                    right: true,
                    left: false,
                    mono: false
                },
            },
            tick_clock: 100u16.try_into().unwrap(),
            mml_settings: GlobalSettings {
                zenlen: 192u8.try_into().unwrap(),
                channel_transpose: Transpose::new(2),
                signature: KeySignature::default()
                    .parse_signature_changes("+fc")
                    .unwrap(),
                old_transpose: true,
            },
            spc_song_length: Some(300),
            spc_fadeout_millis: Some(200),
        }
    );

    let s = compile_mml(
        r#"
#tempo 144

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().tick_clock,
        Bpm::try_from(144u8).unwrap().to_tick_clock().unwrap()
    );
}

#[test]
fn all_headers_lowercase() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#title song-title
#game song-game
#date song-date
#composer song-composer
#author song-author
#copyright song-copyright
#license song-license
#zenlen 192
#transpose +2
#keysignature +fc ; D Major
#oldtranspose
#maxecholength 64
#echolength 32
#firfilter 1 2 -3 -4 $5 $6 $77 $88
#disablefirfilterlimit
#echofeedback 12
#echovolume 34 56
#echoinvert right
#timer 100
#spcsonglength 300
#spcfadeout 200

A r
"#,
        &dummy_data,
    );

    assert_eq!(
        s.metadata(),
        &MetaData {
            title: Some("song-title".to_owned()),
            game: Some("song-game".to_owned()),
            date: Some("song-date".to_owned()),
            composer: Some("song-composer".to_owned()),
            author: Some("song-author".to_owned()),
            copyright: Some("song-copyright".to_owned()),
            license: Some("song-license".to_owned()),
            echo_buffer: EchoBuffer {
                max_edl: (64u8 / 16).try_into().unwrap(),
                edl: (32u8 / 16).try_into().unwrap(),
                fir: [1, 2, -3, -4, 0x5, 0x6, 0x77, -120].map(|i: i8| FirCoefficient::new(i)),
                feedback: 12.try_into().unwrap(),
                echo_volume_l: 34u8.try_into().unwrap(),
                echo_volume_r: 56u8.try_into().unwrap(),
                invert: InvertFlags {
                    right: true,
                    left: false,
                    mono: false
                },
            },
            tick_clock: 100u16.try_into().unwrap(),
            mml_settings: GlobalSettings {
                zenlen: 192u8.try_into().unwrap(),
                channel_transpose: Transpose::new(2),
                signature: KeySignature::default()
                    .parse_signature_changes("+fc")
                    .unwrap(),
                old_transpose: true,
            },
            spc_song_length: Some(300),
            spc_fadeout_millis: Some(200),
        }
    );

    let s = compile_mml(
        r#"
#tempo 144

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().tick_clock,
        Bpm::try_from(144u8).unwrap().to_tick_clock().unwrap()
    );
}

#[test]
fn duplicate_header_errors() {
    assert_one_header_error_in_mml(
        r#"
#Title one
#title two

A r
"#,
        3,
        MmlLineError::DuplicateHeader {
            name: "#title".to_owned(),
            line: 2,
        },
    );

    assert_one_header_error_in_mml(
        r#"
#title song-title

#EchoLength 32

#tempo 120

#echoLength 32

A r
"#,
        8,
        MmlLineError::DuplicateHeader {
            name: "#echoLength".to_owned(),
            line: 4,
        },
    );
}

#[test]
fn mono_echo_volume() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#EchoVolume 127

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().echo_buffer.echo_volume_l,
        127u32.try_into().unwrap()
    );
    assert_eq!(
        s.metadata().echo_buffer.echo_volume_r,
        127u32.try_into().unwrap()
    );

    assert_one_header_error_in_mml(
        r#"
#EchoVolume -1

A r
"#,
        2,
        ValueError::CannotParseUnsigned("-1".to_owned()).into(),
    );

    assert_one_header_error_in_mml(
        r#"
#EchoVolume 128

A r
"#,
        2,
        ValueError::EchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn stereo_echo_volume() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#EchoVolume 0 127

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().echo_buffer.echo_volume_l,
        0u32.try_into().unwrap()
    );
    assert_eq!(
        s.metadata().echo_buffer.echo_volume_r,
        127u32.try_into().unwrap()
    );

    assert_one_header_error_in_mml(
        r#"
#EchoVolume 64 -1

A r
"#,
        2,
        ValueError::CannotParseUnsigned("-1".to_owned()).into(),
    );

    assert_one_header_error_in_mml(
        r#"
#EchoVolume 64 128

A r
"#,
        2,
        ValueError::EchoVolumeOutOfRange(128).into(),
    );
}

#[test]
fn echo_volume_number_of_arguments_error() {
    assert_one_header_error_in_mml(
        r#"
#EchoVolume 1 2 3

A r
"#,
        2,
        MmlLineError::InvalidNumberOfEchoVolumeArguments,
    );
}

#[test]
fn echo_invert() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#EchoInvert mono

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().echo_buffer.invert,
        InvertFlags {
            right: false,
            left: false,
            mono: true
        }
    );

    let s = compile_mml(
        r#"
#EchoInvert mono left

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().echo_buffer.invert,
        InvertFlags {
            right: false,
            left: true,
            mono: true
        }
    );

    let s = compile_mml(
        r#"
#EchoInvert LR

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().echo_buffer.invert,
        InvertFlags {
            right: true,
            left: true,
            mono: false
        }
    );

    assert_one_header_error_in_mml(
        r#"
#EchoInvert unknown

A r
"#,
        2,
        ValueError::UnknownInvertFlagStr("unknown".to_owned()).into(),
    );
}

#[test]
fn max_edl() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#MaxEchoLength 48

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().echo_buffer.max_edl.to_length().value(), 48);

    // If MaxEchoLength is unused, use `#EchoLength`
    let s = compile_mml(
        r#"
#EchoLength 96

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().echo_buffer.max_edl.to_length().value(), 96);

    assert_one_header_error_in_mml(
        r#"
#EchoLength 96
#MaxEchoLength 32

A r
"#,
        2,
        ValueError::EchoEdlLargerThanMaxEdl {
            edl: 6u8.try_into().unwrap(),
            max_edl: 2u8.try_into().unwrap(),
        }
        .into(),
    );
}

#[test]
fn timer() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#Timer 64

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().tick_clock, TickClock::MIN);

    assert_one_header_error_in_mml(
        r#"
#Timer 63

A r
"#,
        2,
        ValueError::TickClockOutOfRange(63).into(),
    );

    let s = compile_mml(
        r#"
#Timer 256

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().tick_clock, TickClock::MAX);

    assert_one_header_error_in_mml(
        r#"
#Timer 257

A r
"#,
        2,
        ValueError::TickClockOutOfRange(257).into(),
    );
}

#[test]
fn zenlen() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#ZenLen 192

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().mml_settings.zenlen, 192u8.try_into().unwrap());

    let s = compile_mml(
        r#"
#ZenLen 4

A r1
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().mml_settings.zenlen, ZenLen::MIN);

    assert_one_header_error_in_mml(
        r#"
#ZenLen 3

A r
"#,
        2,
        ValueError::ZenLenOutOfRange(3).into(),
    );

    let s = compile_mml(
        r#"
#ZenLen 255

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().mml_settings.zenlen, ZenLen::MAX);

    assert_one_header_error_in_mml(
        r#"
#ZenLen 256

A r
"#,
        2,
        ValueError::ZenLenOutOfRange(256).into(),
    );

    assert_one_header_error_in_mml(
        r#"
#ZenLen not-a-number

A r
"#,
        2,
        ValueError::CannotParseUnsigned("not-a-number".to_owned()).into(),
    );
}

#[test]
fn zenlen_alt_name() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
#Zenlen 192

A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().mml_settings.zenlen, 192u8.try_into().unwrap());
}

#[test]
fn transpose() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.channel_transpose,
        Transpose::new(0)
    );

    let s = compile_mml(
        r#"
#Transpose 0

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.channel_transpose,
        Transpose::new(0)
    );

    assert_one_header_error_in_mml(
        r#"
#Transpose 5

A r
"#,
        2,
        ValueError::NoTransposeSign.into(),
    );

    let s = compile_mml(
        r#"
#Transpose +127

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.channel_transpose,
        Transpose::new(127)
    );

    assert_one_header_error_in_mml(
        r#"
#Transpose +128

A r
"#,
        2,
        ValueError::TransposeOutOfRange(128).into(),
    );

    let s = compile_mml(
        r#"
#Transpose -128

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.channel_transpose,
        Transpose::new(-128)
    );

    assert_one_header_error_in_mml(
        r#"
#Transpose -129

A r
"#,
        2,
        ValueError::TransposeOutOfRange(-129).into(),
    );

    assert_one_header_error_in_mml(
        r#"
#Transpose +not-a-number

A r
"#,
        2,
        ValueError::CannotParseSigned("+not-a-number".to_owned()).into(),
    );
}

#[test]
fn key_signature() {
    let dummy_data = dummy_data();

    let s = compile_mml(
        r#"
A r
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().mml_settings.signature, KeySignature::default());

    let s = compile_mml(
        r#"
#KeySignature -bea

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.signature,
        KeySignature::default()
            .parse_signature_changes("-bea")
            .unwrap()
    );

    let s = compile_mml(
        r#"
#KeySignature -bea, +fc

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.signature,
        KeySignature::default()
            .parse_signature_changes("-bea")
            .unwrap()
            .parse_signature_changes("+fc")
            .unwrap()
    );

    let s = compile_mml(
        r#"
#KeySignature - b e a, =b

A r
"#,
        &dummy_data,
    );
    assert_eq!(
        s.metadata().mml_settings.signature,
        KeySignature::default()
            .parse_signature_changes("-ea")
            .unwrap()
    );

    assert_one_header_error_in_mml(
        r#"
#KeySignature b

A r
"#,
        2,
        ValueError::NoKeySignatureSign.into(),
    );

    assert_one_header_error_in_mml(
        r#"
#KeySignature -be, +, +fc

A r
"#,
        2,
        ValueError::NoTonesInKeySignature.into(),
    );

    assert_one_header_error_in_mml(
        r#"
#KeySignature +abcdefg, +abcdefg, =h

A r
"#,
        2,
        ValueError::InvalidKeySignatureTone('h').into(),
    );

    assert_one_header_error_in_mml(
        r#"
#KeySignature + b   e, + f e, = =

A r
"#,
        2,
        ValueError::InvalidKeySignatureTone('=').into(),
    );
}
