// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::invert_flags::InvertFlags;
use compiler::time::{TickClock, ZenLen};
use compiler::UnsignedValueNewType;

use crate::*;

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
    assert_eq!(s.metadata().zenlen, 192u8.try_into().unwrap());

    let s = compile_mml(
        r#"
#ZenLen 4

A r1
"#,
        &dummy_data,
    );
    assert_eq!(s.metadata().zenlen, ZenLen::MIN);

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
    assert_eq!(s.metadata().zenlen, ZenLen::MAX);

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
    assert_eq!(s.metadata().zenlen, 192u8.try_into().unwrap());
}
