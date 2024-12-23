// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

/// Tests the merge instrument/envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn set_instrument_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument    adsr 1 2 3 4
@1 dummy_instrument_2  adsr 5 6 7 8

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument_and_adsr dummy_instrument_2 5 6 7 8",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument    gain I10
@1 dummy_instrument_2  gain E20

A @0 a @0 b L @0 c @0 d @1 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop
            "set_instrument_and_gain dummy_instrument I10",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument_and_gain dummy_instrument_2 E20",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  adsr 1 2 3 4
@1 dummy_instrument  adsr 5 6 7 8
@2 dummy_instrument_2

A @0 a @0 b L @1 c @1 d @2 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND adsr after loop)
            "set_instrument_and_adsr dummy_instrument 5 6 7 8",
            "play_note c4 24",
            // @1 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  adsr 1 2 3 4
@1 dummy_instrument  adsr 5 6 7 8
@2 dummy_instrument_2

A @0 a @0 b L A 5,6,7,8 c @1 d @1 e @2 f
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND adsr after loop)
            "set_adsr 5 6 7 8",
            "play_note c4 24",
            // Instrument is unknown after loop
            "set_instrument_and_adsr dummy_instrument 5 6 7 8",
            "play_note d4 24",
            // @1 optimised out
            "play_note e4 24",
            "set_instrument dummy_instrument_2",
            "play_note f4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  gain I10
@1 dummy_instrument  gain E20
@2 dummy_instrument_2

A @0 a @0 b L @1 c @1 d @2 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND gain after loop)
            "set_instrument_and_gain dummy_instrument E20",
            "play_note c4 24",
            // @1 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument  gain I10
@1 dummy_instrument  gain E20
@2 dummy_instrument_2

A @0 a @0 b L GI10 c @1 d @1 e @2 f
"###,
        &[
            "set_instrument_and_gain dummy_instrument I10",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // Loop (must set instrument AND gain after loop)
            "set_gain I10",
            "play_note c4 24",
            // Instrument unknwon after loop
            "set_instrument_and_gain dummy_instrument E20",
            "play_note d4 24",
            // @1 optimised out
            "play_note e4 24",
            "set_instrument dummy_instrument_2",
            "play_note f4 24",
        ],
    );
}

/// Tests the merge ADSR envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn set_adsr_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument adsr 1 2 3 4

A @0 a A1,2,3,4 b L A1,2,3,4 c A1,2,3,4 d A5,6,7,8 e
"###,
        &[
            "set_instrument_and_adsr dummy_instrument 1 2 3 4",
            "play_note a4 24",
            // A1,2,3,4 optimised out
            "play_note b4 24",
            // Loop
            "set_adsr 1 2 3 4",
            "play_note c4 24",
            // A1,2,3,4 optimised out
            "play_note d4 24",
            "set_adsr 5 6 7 8",
            "play_note e4 24",
        ],
    );
}

/// Tests the merge GAIN envelope optimisation is disabled after a `L` set-loop-point command
#[test]
fn set_gain_after_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument gain E24

A @0 a GE24 b L GE24 c GE24 d GF127 e
"###,
        &[
            "set_instrument_and_gain dummy_instrument E24",
            "play_note a4 24",
            // GE24 optimised out
            "play_note b4 24",
            // Loop
            "set_gain E24",
            "play_note c4 24",
            // GE24 optimised out
            "play_note d4 24",
            "set_gain F127",
            "play_note e4 24",
        ],
    );
}

#[test]
fn set_instrument_after_song_loop_point_and_loop() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 a @0 b L [c]2 @0 c @0 d @1 e
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            // @0 optimised out
            "play_note b4 24",
            // `L` set song loop point
            "start_loop",
            "play_note c4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // @0 optimised out
            "play_note d4 24",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 c @0 L [d : e]2 @0 f @0 g
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // @0 optimised out
            // `L` set song loop point
            "start_loop",
            "play_note d4 24",
            "skip_last_loop",
            "play_note e4 24",
            "end_loop 2",
            "set_instrument dummy_instrument",
            "play_note f4 24",
            // @0 optimised out
            "play_note g4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument

A @0 c L [@0 d]2 @0 e
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // `L` set song loop point
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note d4 24",
            "end_loop 2",
            // @0 optimised out
            "play_note e4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument
@1 dummy_instrument_2

A @0 c L [@0 d : @1 e]2 @0 f
"###,
        &[
            "set_instrument dummy_instrument",
            "play_note c4 24",
            // `L` set song loop point
            "start_loop",
            "set_instrument dummy_instrument",
            "play_note d4 24",
            "skip_last_loop",
            "set_instrument dummy_instrument_2",
            "play_note e4 24",
            "end_loop 2",
            // @0 optimised out
            "play_note f4 24",
        ],
    );
}

#[test]
fn envelope_after_song_loop_point_and_loop() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument gain F80

A @0 c GF80 d L [e]2 GF80 f
"###,
        &[
            "set_instrument_and_gain dummy_instrument F80",
            "play_note c4 24",
            // F80 optimised out
            "play_note d4 24",
            // `L` set song loop point
            "start_loop",
            "play_note e4 24",
            "end_loop 2",
            "set_gain F80",
            "play_note f4 24",
        ],
    );

    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument gain F80

A @0 c GF80 d L [e : f]2 GF80 g
"###,
        &[
            "set_instrument_and_gain dummy_instrument F80",
            "play_note c4 24",
            // F80 optimised out
            "play_note d4 24",
            // `L` set song loop point
            "start_loop",
            "play_note e4 24",
            "skip_last_loop",
            "play_note f4 24",
            "end_loop 2",
            "set_gain F80",
            "play_note g4 24",
        ],
    );
}

#[test]
fn set_loop_point_in_loop_is_error() {
    assert_error_in_mml_line("[a b L c]5", 6, ChannelError::CannotSetLoopPointInALoop);
}

#[test]
fn mp_vibrato_before_set_loop_point() {
    assert_mml_channel_a_matches_looping_bytecode(
        r###"
@0 dummy_instrument

A @0 MP20,2 a L a
"###,
        &[
            "set_instrument dummy_instrument",
            "set_vibrato 21 2",
            "play_note a4 24",
            // Loop
            // This set_vibrato instruction is not optimised out.
            "set_vibrato 21 2",
            "play_note a4 24",
        ],
    );
}
