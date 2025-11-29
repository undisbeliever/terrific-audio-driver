// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn call_subroutine() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!s1 a
!s2 b
!s3 c

A @0 !s1 !s2 !s3
"##,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s1",
            "call_subroutine s2",
            "call_subroutine s3",
        ],
    );
}

#[test]
fn max_loops_with_subroutine() {
    let subroutine_stack_depth = 3 * BC_STACK_BYTES_PER_LOOP;
    let channel_a_stack_depth =
        subroutine_stack_depth + BC_STACK_BYTES_PER_SUBROUTINE_CALL + 3 * BC_STACK_BYTES_PER_LOOP;

    // Assert this is the maximum depth
    assert!(channel_a_stack_depth + BC_STACK_BYTES_PER_LOOP > BC_CHANNEL_STACK_OFFSET);
    assert!(channel_a_stack_depth + BC_STACK_BYTES_PER_SUBROUTINE_CALL > BC_CHANNEL_STACK_OFFSET);

    // Cannot test with asm, no easy way to implement a "call_subroutine s" bytecode.
    // Instead, this test will confirm it compiles with no errors and the correct stack depth

    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s [[[a]11]12]13

A @0
A [[[ !s ]14]15]16
"##,
        &dummy_data(),
    );

    let subroutine = &mml.subroutines().get_compiled(0).unwrap();
    let channel_a = mml.channels()[0].as_ref().unwrap();

    assert_eq!(
        subroutine.bc_state.max_stack_depth.to_u32(),
        u32::try_from(subroutine_stack_depth).unwrap()
    );

    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
}

#[test]
fn too_many_loops_with_subroutine_call() {
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s [[[[[[[a]11]12]13]14]15]16]17

A @0
A !s
"##,
        3,
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            23,
        )),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s a

A @0
A [[[[[[[ !s ]11]12]13]14]15]16]17
"##,
        11,
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            23,
        )),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s [[[a]11]12]13

A @0
A [[[[[ !s ]14]15]16]17]18
"##,
        9,
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s".to_owned(),
            26,
        )),
    );
}

#[test]
fn max_subroutines() {
    const N_SUBROUTINES: u32 = MAX_SUBROUTINES as u32;

    let mut mml = String::new();
    writeln!(mml, "@0 dummy_instrument").unwrap();

    write!(mml, "A @0 ").unwrap();
    for i in 1..=N_SUBROUTINES {
        write!(mml, " !s{i}").unwrap();
    }
    writeln!(mml).unwrap();

    for i in 1..=N_SUBROUTINES {
        writeln!(mml, "!s{i} a").unwrap();
    }

    let song = compile_mml(&mml, &dummy_data());
    let channel_a = song.channels()[0].as_ref().unwrap();

    assert_eq!(song.subroutines().len(), N_SUBROUTINES as usize);
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
    );
    assert_eq!(channel_a.tick_counter.value(), 24 * N_SUBROUTINES);
}

#[test]
fn max_subroutines_with_nesting() {
    const N_SUBROUTINES: u32 = MAX_SUBROUTINES as u32;

    let mut mml = String::new();
    writeln!(mml, "@0 dummy_instrument").unwrap();

    write!(mml, "A @0 ").unwrap();
    for i in 1..=N_SUBROUTINES {
        write!(mml, " !{i}").unwrap();
    }
    writeln!(mml).unwrap();

    for i in 1..=N_SUBROUTINES {
        if i % 3 == 0 {
            writeln!(mml, "!{i} a !1 a").unwrap();
        } else if i % 5 == 0 {
            writeln!(mml, "!{i} b !3 b").unwrap();
        } else {
            writeln!(mml, "!{i} c").unwrap();
        }
    }

    let song = compile_mml(&mml, &dummy_data());
    let channel_a = song.channels()[0].as_ref().unwrap();

    assert_eq!(song.subroutines().len(), N_SUBROUTINES as usize);
    for (s_name, s) in song.subroutines().iter() {
        let s = match s {
            SubroutineState::Compiled(s) => s,
            SubroutineState::CompileError => panic!(),
            SubroutineState::NotCompiled => panic!(),
        };

        let i: u32 = s_name.as_str().parse().unwrap();

        let stack_depth = if i % 3 == 0 {
            BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
        } else if i % 5 == 0 {
            2 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32
        } else {
            0
        };

        assert_eq!(
            s.bc_state.max_stack_depth.to_u32(),
            stack_depth,
            "subroutine max_stack_depth mismatch for !{i}"
        );
    }

    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        3 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
    );
}

/// Testing for tail call optimisation by checking stack depth
#[test]
fn tail_call_1() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();
    assert_eq!(s.bc_state.max_stack_depth.to_u32(), 0);
    assert_eq!(s.bc_state.tick_counter.value(), 24);

    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);
}

/// Testing for tail call optimisation by checking stack depth
#[test]
fn tail_call_2() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!tco r !t      ; expect tail-call optimsation
!no_tco !t r   ; expect no tail-call optimisation

!t r

A @0 !tco !no_tco
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "tco").unwrap();
    assert_eq!(s.bc_state.max_stack_depth.to_u32(), 0);
    assert_eq!(s.bc_state.tick_counter.value(), 48);
    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);

    let (s, bc) = get_subroutine_and_bytecode(&sd, "no_tco").unwrap();
    assert_eq!(s.bc_state.max_stack_depth.to_u32(), 2);
    assert_eq!(s.bc_state.tick_counter.value(), 48);
    assert_eq!(
        bc[bc.len().checked_sub(1).unwrap()],
        opcodes::RETURN_FROM_SUBROUTINE
    );
}

/// Testing that tail call optimisation is disabled if the subroutine ends with MP vibracto.
///
/// Using `call_subroutine_and_disable_vibrato ; return_from_subroutine` (3 bytes)
/// uses less Audio-RAM then `disable_vibrato ; goto_relative` (4 bytes).
///
/// Testing for the existance of tail call optimisation by checking stack depth.
#[test]
fn tail_call_mp() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s @0 MP20,5 a !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();
    assert_eq!(s.bc_state.max_stack_depth.to_u32(), 2);
    assert_eq!(s.bc_state.tick_counter.value(), 48);
    assert_eq!(
        bc[bc.len().checked_sub(1).unwrap()],
        opcodes::RETURN_FROM_SUBROUTINE_AND_DISABLE_VIBRATO
    );
}

/// Testing that manual vibrato does not affect tail call optimisation.
///
/// Testing for the existance of tail call optimisation by checking stack depth.
#[test]
fn tail_call_manual_vibrato() {
    let sd = compile_mml(
        r##"
@0 dummy_instrument

!s @0 ~50,5 a !t
!t r

A @0 !s
"##,
        &dummy_data(),
    );

    let (s, bc) = get_subroutine_and_bytecode(&sd, "s").unwrap();
    assert_eq!(s.bc_state.max_stack_depth.to_u32(), 0);
    assert_eq!(s.bc_state.tick_counter.value(), 48);
    assert_eq!(bc[bc.len().checked_sub(3).unwrap()], opcodes::GOTO_RELATIVE);
}

#[test]
fn nested_subroutines() {
    // Cannot test with asm, no easy way to implement a "call_subroutine s" bytecode.
    // Instead, this test will confirm it compiles with no errors and the correct stack depth

    let _test = |mml| {
        let mml = compile_mml(mml, &dummy_data());
        let channel_a = mml.channels()[0].as_ref().unwrap();
        assert_eq!(
            channel_a.max_stack_depth.to_u32(),
            3 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32,
        );
        assert_eq!(channel_a.tick_counter.value(), 24 * 2 * 3,);
    };

    _test(
        r##"
@0 dummy_instrument

!s1 a !s2 a
!s2 b !s3 b
!s3 c c

A @0 !s1
"##,
    );

    _test(
        r##"
@0 dummy_instrument

!s3 c c
!s2 b !s3 b
!s1 a !s2 a

A @0 !s1
"##,
    );

    _test(
        r##"
@0 dummy_instrument

!s2 b !s3 b
!s3 c c
!s1 a !s2 a

A @0 !s1
"##,
    );
}

#[test]
fn nested_subroutines_recursion_1() {
    assert_subroutine_errors_in_mml(
        r##"
@0 dummy_instrument

!s a !s a

A @0 !s
"##,
        &[(
            "s",
            &[ChannelError::CannotCallSubroutineRecursion("s".to_owned())],
        )],
    );
}

#[test]
fn nested_subroutines_recursion_2() {
    assert_subroutine_errors_in_mml(
        r##"
@0 dummy_instrument

!s1 !s2 a
!s2 !s3 b
!s3 !s1 c

A @0 !s1
"##,
        &[
            (
                "s1",
                &[ChannelError::CannotCallSubroutineRecursion("s2".to_owned())],
            ),
            (
                "s2",
                &[ChannelError::CannotCallSubroutineRecursion("s3".to_owned())],
            ),
            // No error in !s3.  It is compiled last
        ],
    );
}

#[test]
fn nested_subroutines_with_missing_1() {
    assert_subroutine_errors_in_mml(
        r##"
@0 dummy_instrument

!s !s2 a

A @0 !s
"##,
        &[("s", &[ChannelError::CannotFindSubroutine("s2".to_owned())])],
    );
}

#[test]
fn nested_subroutines_with_missing_2() {
    assert_subroutine_errors_in_mml(
        r##"
@0 dummy_instrument

!s1 !s2 a
!s2 !missing b
!s3 !s1 c
!s4 d

A @0 !s1 !s3
"##,
        &[(
            "s2",
            &[ChannelError::CannotFindSubroutine("missing".to_owned())],
        )],
    );
}

#[test]
fn nested_subroutines_stack_depth_limit() {
    let channel_a_stack_depth =
        3 * BC_STACK_BYTES_PER_LOOP + 6 * BC_STACK_BYTES_PER_SUBROUTINE_CALL;
    assert_eq!(channel_a_stack_depth, BC_CHANNEL_STACK_SIZE);

    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s1 [ !s2 ]2
!s2 [ !s3 ]2
!s3 [ !s4 ]2
!s4 !s5 d
!s5 !s6 e
!s6 f

A @0 !s1
"##,
        &dummy_data(),
    );

    let channel_a = mml.channels()[0].as_ref().unwrap();
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
    assert_eq!(channel_a.tick_counter.value(), (24 * 3) * 2 * 2 * 2);

    // Same subroutines but shuffled
    let mml = compile_mml(
        r##"
@0 dummy_instrument

!s5 !s6 e
!s2 [ !s3 ]2
!s6 f
!s3 [ !s4 ]2
!s4 !s5 d
!s1 [ !s2 ]2

A @0 !s1
"##,
        &dummy_data(),
    );

    let channel_a = mml.channels()[0].as_ref().unwrap();
    assert_eq!(
        channel_a.max_stack_depth.to_u32(),
        u32::try_from(channel_a_stack_depth).unwrap()
    );
    assert_eq!(channel_a.tick_counter.value(), (24 * 3) * 2 * 2 * 2);
}

#[test]
fn nested_subroutines_stack_overflow() {
    let stack_depth = 11 * BC_STACK_BYTES_PER_SUBROUTINE_CALL as u32;
    assert!(stack_depth > BC_CHANNEL_STACK_SIZE as u32);

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 !s2 r
!s2 !s3 r
!s3 !s4 r
!s4 !s5 r
!s5 !s6 r
!s6 !s7 r
!s7 !s8 r
!s8 !s9 r
!s9 !s10 r
!s10 !s11 r
!s11 a

A @0 !s1
"##,
        6,
        ChannelError::BytecodeError(BytecodeError::StackOverflowInSubroutineCall(
            "s1".to_owned(),
            stack_depth,
        )),
    );
}

#[test]
fn subroutine_call_no_instrument_error() {
    assert_one_error_in_channel_a_mml(
        r##"
!s c d e f

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
!s c d e f

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s a @0 b c d

A !s
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 r !s2 r
!s2 a

A !s2
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );

    // test tail call
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 r !s2
!s2 a

A !s2
"##,
        3,
        BytecodeError::SubroutinePlaysNotesWithNoInstrument.into(),
    );
}

#[test]
fn subroutine_call_note_range_errors() {
    let inst_range = Note::first_note_for_octave(Octave::try_from(2).unwrap())
        ..=Note::last_note_for_octave(Octave::try_from(6).unwrap());

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o7 f d c e

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("c7")..=note("f7"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o4 f d o7 e c

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("d4")..=note("e7"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s o1 a o4 c d

A @0 !s
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("d4"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2
!s2 o5 c d e

A @0 !s1
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("e5"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2 o4 d e
!s2 @0 b

A @0 !s1
"##,
        6,
        // Only o1 a is played with an unknown instrument
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("a1"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );

    // Test tail call
    assert_one_error_in_channel_a_mml(
        r##"
@0 dummy_instrument

!s1 o1 a !s2
!s2 o4 b

A @0 !s1
"##,
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("a1")..=note("b4"),
            inst_range: inst_range.clone(),
        }
        .into(),
    );
}

#[test]
fn nested_subroutine_no_instrument_bugfix() {
    assert_mml_channel_a_matches_bytecode(
        r##"
@0 dummy_instrument

!n1 @0 c d e
!n2 b

!s !n1 !n2

A !s
"##,
        &["call_subroutine s"],
    );
}

#[test]
fn subroutine_instrument_hint_1() {
    // Test MML with instrument hint has identical bytecode to MML with instrument set
    let mml = r#"
@1 f1000_o4

!s ?@1 c d & {dg} {dg} MP50,10 c ~6,4 MD+60 c D0
!s  @1 c d & {dg} {dg} MP50,10 c ~6,4 MD+60 c D0

A @1 !s
"#;

    let bc_asm: &[&str] = &[
        "play_note c4 keyoff 24",
        "play_note d4 no_keyoff 24",
        "portamento g4 keyoff +18 24",
        "play_note d4 no_keyoff 1",
        "portamento g4 keyoff +18 23",
        "set_vibrato 3 10",
        "play_note c4 keyoff 24",
        "set_vibrato 6 4",
        "set_detune +38",
        "play_note c4 keyoff 24",
        "disable_detune",
    ];
    let bc_asm = [bc_asm, &["set_instrument f1000_o4"], bc_asm].concat();

    assert_mml_subroutine_matches_bytecode(mml, 0, &bc_asm);
}

#[test]
fn subroutine_instrument_hint_2() {
    // Tests different instruments are accepted
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 f1000_o4
@2 f1000_o3_o5

!s ?@1 {dg}

A @1 !s @2 !s
"#,
        0,
        &["play_note d4 no_keyoff 1", "portamento g4 keyoff +18 23"],
    );

    // Test MML instruments with ADSR/GAIN are accepted
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 dummy_instrument adsr 10 2 2 20
@2 dummy_instrument gain F127

!s ?@1 c

A @1 !s @2 !s
"#,
        0,
        &["play_note c4 24"],
    );

    // Test nested subroutines
    // Also tests subroutine changes instrument
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 f1000_o4
@2 f1000_o3_o5
@3 f2000_o4

!s1 ?@1 c
!s2 !s1 {ge} @3 {fe}

A @1 !s1 @2 !s1
"#,
        1,
        &[
            "call_subroutine s1",
            "play_note g4 no_keyoff 1",
            "portamento e4 keyoff -12 23",
            "set_instrument f2000_o4",
            "play_note f4 no_keyoff 1",
            "portamento e4 keyoff -2 23",
        ],
    );
}

#[test]
fn subroutine_instrument_hint_then_set_same_instrument() {
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 f1000_o4

!s ?@1 {dg} @1 {gd}

A @1 !s
"#,
        0,
        &[
            "play_note d4 no_keyoff 1",
            "portamento g4 keyoff +18 23",
            "set_instrument f1000_o4",
            "play_note g4 no_keyoff 1",
            "portamento d4 keyoff -18 23",
        ],
    );

    // The previous test was passing because the envelope was unset.
    // This time the envelope is set to match the `f1000_o4`
    // envelope before the `@1` command.

    assert_mml_subroutine_matches_bytecode(
        r#"
; Default gain for `f1000_o4` is F0
@1 f1000_o4 gain F0

!s ?@1 GF0 {dg} @1 {gd}

A @1 !s
"#,
        0,
        &[
            "set_gain F0",
            "play_note d4 no_keyoff 1",
            "portamento g4 keyoff +18 23",
            "set_instrument f1000_o4",
            "play_note g4 no_keyoff 1",
            "portamento d4 keyoff -18 23",
        ],
    );

    // Repeat the test with a custom @ instrument envelope.
    assert_mml_subroutine_matches_bytecode(
        r#"
@1 f1000_o4 gain I15

!s ?@1 GI15 {dg} @1 {gd}

A @1 !s
"#,
        0,
        &[
            "set_gain I15",
            "play_note d4 no_keyoff 1",
            "portamento g4 keyoff +18 23",
            "set_instrument_and_gain f1000_o4 I15",
            "play_note g4 no_keyoff 1",
            "portamento d4 keyoff -18 23",
        ],
    );
}

#[test]
fn set_subroutine_instrument_hint_errors() {
    assert_one_error_in_mml_line(
        "?@1",
        1,
        ChannelError::InstrumentHintOnlyAllowedInSubroutines,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s ?@ c

A @1 !s
"#,
        "!s",
        4,
        ChannelError::NoInstrumentHint,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s ?@unknown c

A @1 !s
"#,
        "!s",
        4,
        ChannelError::CannotFindInstrument("unknown".to_owned()),
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s ?@1 ?@1

A @1 !s
"#,
        "!s",
        8,
        ChannelError::InstrumentHintAlreadySet,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s1 ?@1
!s2 !s1 ?@1

A @1 !s2
"#,
        "!s2",
        9,
        ChannelError::InstrumentHintAlreadySet,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s @1 ?@1

A @1 !s
"#,
        "!s",
        7,
        ChannelError::InstrumentHintInstrumentAlreadySet,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 dummy_instrument

!s1 @1
!s2 !s1 ?@1


A @1 !s2
"#,
        "!s2",
        9,
        ChannelError::InstrumentHintInstrumentAlreadySet,
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 f1000_o4
@sample sample

!s ?@sample s0

A @sample !s
"#,
        "!s",
        4,
        ChannelError::CannotSetInstrumentHintForSample,
    );

    assert_one_channel_error_in_mml(
        r#"
@1 dummy_instrument

!s ?@1

A !s
"#,
        "A",
        3,
        BytecodeError::SubroutineInstrumentHintNoInstrumentSet.into(),
    );

    assert_one_channel_error_in_mml(
        r#"
@1 dummy_instrument
@2 sample

!s ?@1

A @2 !s
"#,
        "A",
        6,
        BytecodeError::SubroutineInstrumentHintSampleMismatch.into(),
    );
}

#[test]
fn subroutine_instrument_hint_freq_errors() {
    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f2000_o4

!s ?@1 o4 {cd}

A @2 !s o4 g
"#,
        "A",
        6,
        BytecodeError::SubroutineInstrumentHintFrequencyMismatch {
            subroutine: InstrumentHintFreq::from_freq(1000.0),
            instrument: InstrumentHintFreq::from_freq(2000.0),
        }
        .into(),
    );

    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f2000_o4

!s ?@1 o4 c @2 d e

A @2 !s o4 c d e
"#,
        "A",
        6,
        BytecodeError::SubroutineInstrumentHintFrequencyMismatch {
            subroutine: InstrumentHintFreq::from_freq(1000.0),
            instrument: InstrumentHintFreq::from_freq(2000.0),
        }
        .into(),
    );

    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f2000_o4

!sc ?@1 o4 c
!sp !sc {cd} @2

A @2 !sp
"#,
        "A",
        6,
        BytecodeError::SubroutineInstrumentHintFrequencyMismatch {
            subroutine: InstrumentHintFreq::from_freq(1000.0),
            instrument: InstrumentHintFreq::from_freq(2000.0),
        }
        .into(),
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 f1000_o4
@2 f2000_o4

!s1 ?@2 o4 c
!s2 @1 !s1

A !s2 !s1
"#,
        "!s2",
        11,
        BytecodeError::SubroutineInstrumentHintFrequencyMismatch {
            subroutine: InstrumentHintFreq::from_freq(2000.0),
            instrument: InstrumentHintFreq::from_freq(1000.0),
        }
        .into(),
    );
}

#[test]
fn subroutine_instrument_hint_note_range_errors() {
    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f1000_o5

!s ?@1 o4 d e f

A @2 !s o5 f
"#,
        "A",
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("d4")..=note("f4"),
            inst_range: note("c5")..=note("b5"),
        }
        .into(),
    );

    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f1000_o5

!s ?@1 o4 d @2 o5 e f

A @2 !s o5 f
"#,
        "A",
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("d4")..=note("d4"),
            inst_range: note("c5")..=note("b5"),
        }
        .into(),
    );

    assert_one_channel_error_in_mml(
        r#"
@1 f1000_o4
@2 f1000_o5

!sc ?@1 o4 d
!sp !sc {fg} @2

A @2 !sp
"#,
        "A",
        6,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("d4")..=note("g4"),
            inst_range: note("c5")..=note("b5"),
        }
        .into(),
    );

    assert_one_subroutine_error_in_mml(
        r#"
@1 f1000_o4
@2 f1000_o5

!s1 ?@2 o5 {ef}
!s2 @1 !s1 {ef}

A !s2 !s1
"#,
        "!s2",
        8,
        BytecodeError::SubroutineNotesOutOfRange {
            subroutine_range: note("e5")..=note("f5"),
            inst_range: note("c4")..=note("b4"),
        }
        .into(),
    );
}

#[test]
fn subroutine_instrument_hint_and_loop() {
    assert_mml_channel_a_matches_bytecode(
        r#"
@1 f1000_o4

!s ?@1 {eg}

A [V+5 : @1 c]6 !s
"#,
        &[
            "start_loop",
            "adjust_volume +5",
            "skip_last_loop",
            "set_instrument f1000_o4",
            "play_note c4 24",
            "end_loop 6",
            "call_subroutine s",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r#"
@1 f1000_o4
@2 f2000_o4

!s ?@1 {eg}

A [@1 c : @2 d]6 !s
"#,
        &[
            "start_loop",
            "set_instrument f1000_o4",
            "play_note c4 24",
            "skip_last_loop",
            "set_instrument f2000_o4",
            "play_note d4 24",
            "end_loop 6",
            "call_subroutine s",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r#"
@1 f1000_o4
@2 f2000_o4

!s ?@1 {eg}

A [@1 !s : @2 c]6
"#,
        &[
            "start_loop",
            "set_instrument f1000_o4",
            "call_subroutine s",
            "skip_last_loop",
            "set_instrument f2000_o4",
            "play_note c4 24",
            "end_loop 6",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        r#"
@1 f1000_o4
@2 f2000_o4

!s1 ?@1 {eg}
!s2 ?@2 {eg}

A [ @1 c : [ V+5 [@1 !s1 : @2 c]6 !s1 @2 ]4 !s2 ]2 !s1
"#,
        &[
            "start_loop",
            "set_instrument f1000_o4",
            "play_note c4 24",
            "skip_last_loop",
            "start_loop",
            "adjust_volume +5",
            "start_loop",
            "set_instrument f1000_o4",
            "call_subroutine s1",
            "skip_last_loop",
            "set_instrument f2000_o4",
            "play_note c4 24",
            "end_loop 6",
            // instrument is @1 here (due to skip_last_loop)
            "call_subroutine s1",
            "set_instrument f2000_o4",
            "end_loop 4",
            // instrument is @2 here (no skip_last_loop)
            "call_subroutine s2",
            "end_loop 2",
            // instrument is @1 here (skip_last_loop)
            "call_subroutine s1",
        ],
    );
}

#[test]
fn note_range_after_subroutine_call() {
    assert_one_error_in_channel_a_mml(
        r##"
@d dummy_instrument
@oof only_octave_four

!s @oof c4a

A @d o6c !s o4c o6d o4e
"##,
        19,
        BytecodeError::NoteOutOfRange(note("d6"), note("c4")..=note("b4")).into(),
    );
}

#[test]
fn song_loop_point_in_nested_subroutine_panic_bugfix() {
    // found using rust-fuzz
    assert_one_subroutine_error_in_mml(
        r##"
@0 dummy_instrument

!s1 @0 r L r
!s2 !s1 r

A !s2
"##,
        "!s1",
        10,
        ChannelError::CannotSetLoopPoint,
    );
}

#[test]
fn tail_call_subroutine_with_compile_error_panic_bugfix() {
    let dummy_data = dummy_data();

    // found using rust-fuzz
    let r = compiler::songs::compile_mml_song(
        r##"
@0 dummy_instrument

!s1 !s2
!s2 {cP} @0

A !s2
"##,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );
    assert!(r.is_err());

    let r = compiler::songs::compile_mml_song(
        r##"
@0 dummy_instrument

!s1 !s2
!s2 !s3
!s3 !s4
!s4 !error
!error {cP} @0

A !s1
"##,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );
    assert!(r.is_err());
}

#[test]
fn self_recursion_loop_analysis_panic_bugfix() {
    // Found using rust-fuzz (manually minimised)
    // Panics on the `:` skip last loop command in !s2
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s1 !s1 @1
!s2 [ !s1 w : w ]2

A !s2
"##,
        "!s1",
        5,
        ChannelError::CannotCallSubroutineRecursion("s1".to_owned()),
    )
}

#[test]
fn subroutine_no_loop_end_loop_panic_bugfix() {
    // Found using cargo-fuzz
    assert_one_subroutine_error_in_mml(
        r##"
@1 dummy_instrument

!s ]2

A !s
"##,
        "!s",
        4,
        BytecodeError::NotInALoop.into(),
    )
}
