// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

use compiler::identifier::{ChannelId, MusicChannelIndex};
use compiler::mml::{find_cursor_state, line_start_ticks};

const CHANNEL_A: ChannelId = ChannelId::Channel(MusicChannelIndex::CHANNEL_A);
const CHANNEL_B: ChannelId = ChannelId::Channel(MusicChannelIndex::CHANNEL_B);
const SUB_1: ChannelId = ChannelId::Subroutine(0);
const SUB_2: ChannelId = ChannelId::Subroutine(1);

fn compile(mml: &str) -> SongData {
    let env = Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap());

    let instruments_and_samples = data::validate_instrument_and_sample_names(
        [
            dummy_instrument("sine", 500.0, 1, 6, env),
            dummy_instrument("square", 500.0, 1, 6, env),
        ]
        .iter(),
        [].iter(),
    )
    .unwrap();

    let pitch_table = build_pitch_table(&instruments_and_samples).unwrap();

    compile_mml(
        &mml,
        &DummyData {
            instruments_and_samples,
            pitch_table,
        },
    )
}

fn cursor_ticks(sd: &SongData, char_index: u32) -> Option<(ChannelId, u32, bool)> {
    find_cursor_state(sd, char_index)
        .map(|(channel, tc, _)| (channel, tc.ticks.value(), tc.in_loop))
}

fn line_start(sd: &SongData, char_index: u32) -> Option<(ChannelId, u32, bool)> {
    line_start_ticks(sd, char_index).map(|(channel, tc)| (channel, tc.ticks.value(), tc.in_loop))
}

#[test]
fn merged_ties() {
    let sd = compile(
        r##"
@1 sine
A @1 c^ c8 ^8 c16^16 l%8
A ^%16^
"##,
    );

    assert_eq!(cursor_ticks(&sd, 14), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 15), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 16), Some((CHANNEL_A, 48, false)));

    assert_eq!(cursor_ticks(&sd, 17), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 19), Some((CHANNEL_A, 60, false)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 60, false)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 72, false)));

    assert_eq!(cursor_ticks(&sd, 25), Some((CHANNEL_A, 72, false)));
    assert_eq!(cursor_ticks(&sd, 26), Some((CHANNEL_A, 78, false)));
    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 78, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 84, false)));

    // new line
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 84, false)));

    assert_eq!(cursor_ticks(&sd, 36), Some((CHANNEL_A, 84, false)));
    assert_eq!(cursor_ticks(&sd, 37), Some((CHANNEL_A, 84, false)));
    assert_eq!(cursor_ticks(&sd, 39), Some((CHANNEL_A, 84, false)));
    assert_eq!(cursor_ticks(&sd, 40), Some((CHANNEL_A, 100, false)));

    assert_eq!(cursor_ticks(&sd, 41), Some((CHANNEL_A, 108, false)));

    assert_eq!(cursor_ticks(&sd, 42), None);

    assert_eq!(line_start(&sd, 20), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 38), Some((CHANNEL_A, 84, false)));
}

#[test]
fn note_rests() {
    let sd = compile(
        r##"
@1 sine
A @1 c & rr r
A r%15
A r2
"##,
    );

    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 19), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 20), Some((CHANNEL_A, 72, false)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 72, false)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 96, false)));

    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 96, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 111, false)));

    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 111, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 159, false)));

    assert_eq!(line_start(&sd, 10), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 29), Some((CHANNEL_A, 96, false)));
    assert_eq!(line_start(&sd, 33), Some((CHANNEL_A, 111, false)));
}

#[test]
fn note_waits() {
    let sd = compile(
        r##"
@1 sine
A @1 c & ww w
A w%15
A w2
"##,
    );

    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 19), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 20), Some((CHANNEL_A, 72, false)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 72, false)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 96, false)));

    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 96, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 111, false)));

    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 111, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 159, false)));

    assert_eq!(line_start(&sd, 10), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 29), Some((CHANNEL_A, 96, false)));
    assert_eq!(line_start(&sd, 33), Some((CHANNEL_A, 111, false)));
}

#[test]
fn merged_rests() {
    let sd = compile(
        r##"
@1 sine
A @1 r 4 rr r
A r%15
A r2
"##,
    );

    assert_eq!(cursor_ticks(&sd, 16), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 17), Some((CHANNEL_A, 24, false)));

    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 19), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 20), Some((CHANNEL_A, 72, false)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 72, false)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 96, false)));

    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 96, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 111, false)));

    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 111, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 159, false)));

    assert_eq!(line_start(&sd, 10), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 29), Some((CHANNEL_A, 96, false)));
    assert_eq!(line_start(&sd, 33), Some((CHANNEL_A, 111, false)));
}

#[test]
fn merge_waits() {
    let sd = compile(
        r##"
@1 sine
A @1 w 4 ww w
A w%15
A w2
"##,
    );

    assert_eq!(cursor_ticks(&sd, 16), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 17), Some((CHANNEL_A, 24, false)));

    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 19), Some((CHANNEL_A, 48, false)));
    assert_eq!(cursor_ticks(&sd, 20), Some((CHANNEL_A, 72, false)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 72, false)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 96, false)));

    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 96, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 111, false)));

    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 111, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 159, false)));

    assert_eq!(line_start(&sd, 10), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 29), Some((CHANNEL_A, 96, false)));
    assert_eq!(line_start(&sd, 33), Some((CHANNEL_A, 111, false)));
}

#[test]
fn loops() {
    let sd = compile(
        r##"
@1 square
A @1 [ [[ c8 ]3 ]40]5
"##,
    );

    assert_eq!(cursor_ticks(&sd, 16), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 17), Some((CHANNEL_A, 0, true)));

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 0, true)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 0, true)));
    assert_eq!(cursor_ticks(&sd, 23), Some((CHANNEL_A, 12, true)));

    assert_eq!(cursor_ticks(&sd, 25), Some((CHANNEL_A, 12, true)));
    assert_eq!(cursor_ticks(&sd, 26), Some((CHANNEL_A, 36, true)));

    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 36, true)));
    assert_eq!(cursor_ticks(&sd, 30), Some((CHANNEL_A, 1440, true)));

    assert_eq!(cursor_ticks(&sd, 31), Some((CHANNEL_A, 1440, true)));
    assert_eq!(cursor_ticks(&sd, 32), Some((CHANNEL_A, 7200, false)));

    assert_eq!(cursor_ticks(&sd, 33), None);
}

#[test]
fn portamento() {
    let sd = compile(
        r##"
@1 sine
A @1 {cd} {ce}8 & w {ce}&r {ce}r8 {ce}%20,8,20 ^rr
"##,
    );

    assert_eq!(cursor_ticks(&sd, 17), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 18), Some((CHANNEL_A, 24, false)));

    assert_eq!(cursor_ticks(&sd, 23), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 24), Some((CHANNEL_A, 36, false)));

    assert_eq!(cursor_ticks(&sd, 27), Some((CHANNEL_A, 36, false)));
    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_A, 60, false)));

    assert_eq!(cursor_ticks(&sd, 32), Some((CHANNEL_A, 60, false)));
    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 84, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 84, false)));
    assert_eq!(cursor_ticks(&sd, 35), Some((CHANNEL_A, 108, false)));

    assert_eq!(cursor_ticks(&sd, 39), Some((CHANNEL_A, 108, false)));
    assert_eq!(cursor_ticks(&sd, 40), Some((CHANNEL_A, 132, false)));
    assert_eq!(cursor_ticks(&sd, 41), Some((CHANNEL_A, 132, false)));
    assert_eq!(cursor_ticks(&sd, 42), Some((CHANNEL_A, 144, false)));

    assert_eq!(cursor_ticks(&sd, 49), Some((CHANNEL_A, 144, false)));
    assert_eq!(cursor_ticks(&sd, 50), Some((CHANNEL_A, 164, false)));

    assert_eq!(cursor_ticks(&sd, 56), Some((CHANNEL_A, 164, false)));
    assert_eq!(cursor_ticks(&sd, 57), Some((CHANNEL_A, 188, false)));
    assert_eq!(cursor_ticks(&sd, 58), Some((CHANNEL_A, 212, false)));
    assert_eq!(cursor_ticks(&sd, 59), Some((CHANNEL_A, 236, false)));
}

#[test]
fn broken_chord() {
    let sd = compile(
        r##"
@1 sine
A @1 {{cde}} {{de}}16 & c
"##,
    );

    assert_eq!(cursor_ticks(&sd, 20), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 24, false)));

    assert_eq!(cursor_ticks(&sd, 29), Some((CHANNEL_A, 24, false)));
    assert_eq!(cursor_ticks(&sd, 30), Some((CHANNEL_A, 30, false)));

    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 30, false)));
    assert_eq!(cursor_ticks(&sd, 34), Some((CHANNEL_A, 54, false)));
}

#[test]
fn multiple_channels() {
    let sd = compile(
        r##"
@1 sine
A r [
AB @1 ccc
B r
A ]2
"##,
    );

    assert_eq!(cursor_ticks(&sd, 21), Some((CHANNEL_A, 24, true)));
    assert_eq!(cursor_ticks(&sd, 22), Some((CHANNEL_A, 48, true)));

    assert_eq!(cursor_ticks(&sd, 27), Some((CHANNEL_B, 72, false)));
    assert_eq!(cursor_ticks(&sd, 28), Some((CHANNEL_B, 96, false)));

    assert_eq!(cursor_ticks(&sd, 32), Some((CHANNEL_A, 96, true)));
    assert_eq!(cursor_ticks(&sd, 33), Some((CHANNEL_A, 168, false)));

    assert_eq!(line_start(&sd, 12), Some((CHANNEL_A, 0, false)));
    assert_eq!(line_start(&sd, 23), Some((CHANNEL_A, 24, true)));
    assert_eq!(line_start(&sd, 26), Some((CHANNEL_B, 72, false)));
    assert_eq!(line_start(&sd, 33), Some((CHANNEL_A, 96, true)));
}

#[test]
fn subroutines() {
    let sd = compile(
        r##"
@1 sine
!s1 c
!s2 c8
!s1 !s2
A @1 !s2 !s1
"##,
    );

    assert_eq!(cursor_ticks(&sd, 13), Some((SUB_1, 0, false)));
    assert_eq!(cursor_ticks(&sd, 14), Some((SUB_1, 24, false)));

    assert_eq!(cursor_ticks(&sd, 20), Some((SUB_2, 0, false)));
    assert_eq!(cursor_ticks(&sd, 21), Some((SUB_2, 12, false)));

    assert_eq!(cursor_ticks(&sd, 28), Some((SUB_1, 24, false)));
    assert_eq!(cursor_ticks(&sd, 29), Some((SUB_1, 36, false)));

    assert_eq!(cursor_ticks(&sd, 37), Some((CHANNEL_A, 0, false)));
    assert_eq!(cursor_ticks(&sd, 38), Some((CHANNEL_A, 12, false)));
    assert_eq!(cursor_ticks(&sd, 41), Some((CHANNEL_A, 12, false)));
    assert_eq!(cursor_ticks(&sd, 42), Some((CHANNEL_A, 48, false)));

    assert_eq!(line_start(&sd, 12), Some((SUB_1, 0, false)));
    assert_eq!(line_start(&sd, 20), Some((SUB_2, 0, false)));
    assert_eq!(line_start(&sd, 29), Some((SUB_1, 24, false)));
    assert_eq!(line_start(&sd, 38), Some((CHANNEL_A, 0, false)));
}
