//! MML tests

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

mod bc_asm;
mod broken_chord;
mod cursor_song_tick_mapping;
mod detune;
mod early_release;
mod echo;
mod header;
mod instruments_and_envelope;
mod key_signature;
mod keyon_without_keyoff;
mod loops;
mod misc_instructions;
mod note_range_tracking;
mod notes;
mod old_transpose;
mod parsing;
mod pitch;
mod portamento;
mod quantize;
mod rest_wait;
mod song_loop_point;
mod subroutines;
mod temp_gain;
mod transpose;
mod vibrato;
mod volume_pan;

use compiler::bytecode_assembler::{BcTerminator, BytecodeContext};
use compiler::data::{Name, SampleNumber, UniqueNamesList};
use compiler::driver_constants::{
    BC_CHANNEL_STACK_OFFSET, BC_CHANNEL_STACK_SIZE, BC_STACK_BYTES_PER_LOOP,
    BC_STACK_BYTES_PER_SUBROUTINE_CALL, MAX_SUBROUTINES,
};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::errors::{BytecodeError, ChannelError, MmlLineError, SongError, ValueError};
use compiler::notes::{Note, Octave};
use compiler::pitch_table::{
    build_pitch_table, InstrumentHintFreq, PitchTable, PlayPitchFrequency,
};
use compiler::songs::SongData;
use compiler::subroutines::{CompiledSubroutines, Subroutine, SubroutineNameMap, SubroutineState};
use compiler::{bytecode_assembler, data, opcodes};

use std::fmt::Write;

const _: () = assert!(
    cfg!(feature = "test"),
    "The MML tests require the \"test\" feature in the compiler crate"
);

const SAMPLE_FREQ: f64 = 500.0;

const EXAMPLE_ADSR_STR: &str = "12 1 1 16";
const EXAMPLE_ADSR_COMMENTS_STR: &str = "12,1,1,16";
const EXAMPLE_ADSR: Adsr = match Adsr::try_new(12, 1, 1, 16) {
    Ok(v) => v,
    Err(_) => panic!("Invalid Adsr"),
};

const EXAMPLE_GAIN_STR: &str = "127";
const EXAMPLE_GAIN: Gain = Gain::new(127);

/// Tests MML commands will still be merged if there are a change MML state command in between
/// the two commands I want to test.
fn merge_mml_commands_test(mml_line: &str, bc_asm: &[&str]) {
    // The inc/dec octave commands must return to the original octave
    // The transpose commands must return to a transpose of 0
    const MML_TO_INSERT: [&str; 12] = [
        "",
        "l4",
        "o4",
        "> <",
        "> <",
        "_M+2 __M-2",
        "_M-4 __M+4",
        "_+2 __-2",
        "_-4 __+4",
        "|",
        "| | | |",
        // Newline
        "\nA ",
    ];
    const MATCH_SYMBOL: &str = "||";

    assert!(
        !mml_line.contains("_-") && !mml_line.contains("_+"),
        "line cannot contain a `_` or `__` transpose command"
    );

    if mml_line.matches(MATCH_SYMBOL).count() != 1 {
        panic!("mml_line requires at ONE {MATCH_SYMBOL}");
    }

    for command in MML_TO_INSERT {
        let ml = mml_line.replace(MATCH_SYMBOL, command);

        assert_old_transpose_line_matches_bytecode(&ml, bc_asm);
    }
}

fn get_subroutine_and_bytecode<'a>(
    sd: &'a SongData,
    name: &str,
) -> Option<(&'a Subroutine, &'a [u8])> {
    let s = sd.subroutines().iter().find_map(|(s_name, s)| match s {
        SubroutineState::Compiled(s) => {
            if s_name.as_str() == name {
                Some(s)
            } else {
                None
            }
        }
        SubroutineState::CompileError => panic!(),
        SubroutineState::NotCompiled => panic!(),
    })?;

    let bc_range = usize::from(s.bytecode_offset)..usize::from(s.bytecode_end_offset);

    Some((s, &sd.data()[bc_range]))
}

fn mml_bytecode(mml: &SongData) -> &[u8] {
    let song_data = mml.data();

    let start: usize = mml.channels()[0].as_ref().unwrap().bytecode_offset.into();

    let end = match &mml.channels()[1] {
        Some(c) => c.bytecode_offset.into(),
        None => song_data.len(),
    };

    &song_data[start..end]
}

fn mml_channel_b_bytecode(mml: &SongData) -> &[u8] {
    let song_data = mml.data();

    let start: usize = mml.channels()[1].as_ref().unwrap().bytecode_offset.into();

    let end = match &mml.channels()[2] {
        Some(c) => c.bytecode_offset.into(),
        None => song_data.len(),
    };

    &song_data[start..end]
}

fn subroutine_bytecode(mml: &SongData, index: usize) -> &[u8] {
    let song_data = mml.data();

    let s = mml
        .subroutines()
        .get_compiled(index.try_into().unwrap())
        .unwrap();

    &song_data[usize::from(s.bytecode_offset)..usize::from(s.bytecode_end_offset)]
}

fn assert_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml = compile_mml(&mml, &dd);
    let bc_asm = assemble_channel_bytecode(
        &bc_asm,
        &dd,
        &CompiledSubroutines::new_blank(),
        BcTerminator::DisableChannel,
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(
        mml_bytecode(&mml),
        bc_asm,
        "Testing {mml_line:?} against bytecode"
    );
}

fn assert_old_transpose_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["#OldTranspose\n@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    assert_mml_channel_a_matches_bytecode(&mml, &bc_asm);
}

fn assert_channel_b_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["@1 dummy_instrument\nB @1 o4\nB ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml = compile_mml(&mml, &dd);
    let bc_asm = assemble_channel_bytecode(
        &bc_asm,
        &dd,
        &CompiledSubroutines::new_blank(),
        BcTerminator::DisableChannel,
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(
        mml_channel_b_bytecode(&mml),
        bc_asm,
        "Testing {mml_line:?} against bytecode"
    );
}

fn assert_line_matches_bytecode_bytes(mml_line: &str, bc: &[u8]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();

    let dd = dummy_data();
    let mml = compile_mml(&mml, &dd);
    let mml_bc = mml_bytecode(&mml);

    assert_eq!(&mml_bc[..2], [opcodes::SET_INSTRUMENT, 0]);
    assert_eq!(mml_bc.last(), Some(&opcodes::DISABLE_CHANNEL));
    assert_eq!(
        &mml_bc[2..(mml_bc.len() - 1)],
        bc,
        "Testing {mml_line:?} against bytecode bytes"
    );
}

fn assert_line_matches_line(mml_line1: &str, mml_line2: &str) {
    let mml1 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line1].concat();
    let mml2 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line2].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    assert_eq!(
        mml_bytecode(&mml_data1),
        mml_bytecode(&mml_data2),
        "Testing {mml_line1:?} against MML"
    );
}

fn assert_old_transpose_line_matches_line(mml_line1: &str, mml_line2: &str) {
    let mml1 = ["#OldTranspose\n@1 dummy_instrument\nA @1 o4\nA ", mml_line1].concat();
    let mml2 = ["#OldTranspose\n@1 dummy_instrument\nA @1 o4\nA ", mml_line2].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    assert_eq!(
        mml_bytecode(&mml_data1),
        mml_bytecode(&mml_data2),
        "Testing {mml_line1:?} against MML"
    );
}

fn assert_line_matches_line_and_bytecode(mml_line1: &str, mml_line2: &str, bc_asm: &[&str]) {
    let mml1 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line1].concat();
    let mml2 = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line2].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    let dd = dummy_data();

    let mml_data1 = compile_mml(&mml1, &dd);
    let mml_data2 = compile_mml(&mml2, &dd);

    let mml1_bc = mml_bytecode(&mml_data1);

    assert_eq!(
        mml1_bc,
        mml_bytecode(&mml_data2),
        "Testing {mml_line1:?} against MML"
    );

    let bc_asm = assemble_channel_bytecode(
        &bc_asm,
        &dd,
        &CompiledSubroutines::new_blank(),
        BcTerminator::DisableChannel,
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(mml1_bc, bc_asm, "Testing {mml_line1:?} against bytecode");
}

fn assert_mml_channel_a_matches_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data,
        mml.subroutines(),
        BcTerminator::DisableChannel,
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

fn assert_looping_line_matches_bytecode(mml_line: &str, bc_asm: &[&str]) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    let bc_asm = [&["set_instrument dummy_instrument"], bc_asm].concat();

    assert_mml_channel_a_matches_looping_bytecode(&mml, &bc_asm);
}

fn assert_mml_channel_a_matches_looping_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let channel_a = mml.channels()[0].as_ref().unwrap();

    let loop_point = match channel_a.loop_point {
        Some(lp) => lp.bytecode_offset - usize::from(channel_a.bytecode_offset),
        None => panic!("No loop point in MML"),
    };

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data,
        mml.subroutines(),
        BcTerminator::Goto(loop_point),
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

fn assert_mml_channel_b_matches_bytecode(mml: &str, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data,
        mml.subroutines(),
        BcTerminator::DisableChannel,
        BytecodeContext::UnitTestAssembly,
    );

    assert_eq!(mml_channel_b_bytecode(&mml), bc_asm);
}

fn assert_mml_subroutine_matches_bytecode(mml: &str, subroutine_index: usize, bc_asm: &[&str]) {
    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data,
        mml.subroutines(),
        BcTerminator::ReturnFromSubroutine,
        BytecodeContext::UnitTestAssemblySubroutine,
    );

    assert_eq!(subroutine_bytecode(&mml, subroutine_index), bc_asm);
}

fn assert_one_error_in_mml_line(mml_line: &str, line_char: u32, expected_error: ChannelError) {
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();
    assert_one_error_in_channel_a_mml(&mml, line_char + 2, expected_error);
}

fn assert_one_error_in_channel_a_mml(mml: &str, line_char: u32, expected_error: ChannelError) {
    assert_one_channel_error_in_mml(mml, "A", line_char, expected_error)
}

fn assert_one_channel_error_in_mml(
    mml: &str,
    identifier: &str,
    line_char: u32,
    expected_error: ChannelError,
) {
    let dummy_data = dummy_data();

    let r = compiler::songs::compile_mml_song(
        mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let valid = match &r {
        Err(SongError::MmlError(e)) => {
            if e.channel_errors.len() == 1
                && e.line_errors.is_empty()
                && e.subroutine_errors.is_empty()
            {
                let c = e.channel_errors.first().unwrap();
                match c.errors.len() {
                    1 => {
                        let e = c.errors.first().unwrap();
                        c.identifier.as_str() == identifier
                            && e.0.line_char() == line_char
                            && e.1 == expected_error
                    }
                    _ => false,
                }
            } else {
                false
            }
        }
        _ => false,
    };

    if !valid {
        panic!("expected a single {expected_error:?} error on {identifier}, line_char {line_char}\nInput: {mml:?}\nResult: {r:?}")
    }
}

fn assert_one_subroutine_error_in_mml(
    mml: &str,
    identifier: &str,
    line_char: u32,
    expected_error: ChannelError,
) {
    let dummy_data = dummy_data();

    let id_str = identifier
        .strip_prefix("!")
        .expect("identifier must start with !");

    let r = compiler::songs::compile_mml_song(
        mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let valid = match &r {
        Err(SongError::MmlError(e)) => {
            if e.subroutine_errors.len() == 1
                && e.line_errors.is_empty()
                && e.channel_errors.is_empty()
            {
                let c = e.subroutine_errors.first().unwrap();
                match c.errors.len() {
                    1 => {
                        let e = c.errors.first().unwrap();
                        c.identifier.as_str() == id_str
                            && e.0.line_char() == line_char
                            && e.1 == expected_error
                    }
                    _ => false,
                }
            } else {
                false
            }
        }
        _ => false,
    };

    if !valid {
        panic!("expected a single {expected_error:?} error on {identifier}, line_char {line_char}\nInput: {mml:?}\nResult: {r:?}")
    }
}

fn assert_subroutine_errors_in_mml(mml: &str, expected_errors: &[(&str, &[ChannelError])]) {
    let dummy_data = dummy_data();

    let r = compiler::songs::compile_mml_song(
        mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let matches_err = match &r {
        Err(SongError::MmlError(e)) => {
            e.subroutine_errors.len() == expected_errors.len()
                && e.subroutine_errors.iter().all(|subroutine_error| {
                    let expected = expected_errors
                        .iter()
                        .find(|e| e.0 == subroutine_error.identifier.as_str());
                    match expected {
                        Some((_id, expected)) => {
                            let iter1 = subroutine_error.errors.iter().map(|e| &e.1);
                            let iter2 = expected.iter();

                            expected.len() == subroutine_error.errors.len()
                                && iter1.zip(iter2).all(|(i1, i2)| i1 == i2)
                        }
                        None => false,
                    }
                })
        }
        _ => false,
    };

    if !matches_err {
        panic!("Subroutine error mismatch:\nInput: {mml:?}\nExpected: {expected_errors:?}\nResult: {r:?}")
    }
}

fn assert_one_header_error_in_mml(mml: &str, line_number: u32, expected_error: MmlLineError) {
    let dummy_data = dummy_data();

    let r = compiler::songs::compile_mml_song(
        mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    );

    let valid = match &r {
        Err(SongError::MmlError(e)) => {
            if e.line_errors.len() == 1
                && e.channel_errors.is_empty()
                && e.subroutine_errors.is_empty()
            {
                let e = e.line_errors.first().unwrap();

                e.0.line_number() == line_number && e.1 == expected_error
            } else {
                false
            }
        }
        _ => false,
    };

    if !valid {
        panic!("expected a single {expected_error:?} error on line {line_number}\nInput: {mml:?}\nResult: {r:?}")
    }
}

fn compile_mml(mml: &str, dummy_data: &DummyData) -> SongData {
    compiler::songs::compile_mml_song(
        mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    )
    .unwrap()
}

fn compile_mml_line(mml_line: &str) -> Result<SongData, SongError> {
    let dummy_data = dummy_data();
    let mml = ["@1 dummy_instrument\nA @1 o4\nA ", mml_line].concat();

    compiler::songs::compile_mml_song(
        &mml,
        "",
        None,
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
    )
}

struct SubroutineNameSearcher<'a>(&'a CompiledSubroutines);

impl SubroutineNameMap for SubroutineNameSearcher<'_> {
    fn find_subroutine_index(&self, name: &str) -> Option<u8> {
        self.0
            .iter()
            .enumerate()
            .find_map(|(s_index, (s_name, _))| {
                if s_name.as_str() == name {
                    s_index.try_into().ok()
                } else {
                    None
                }
            })
    }
}

fn assemble_channel_bytecode(
    bc_asm: &[&str],
    dummy_data: &DummyData,
    subroutines: &CompiledSubroutines,
    terminator: BcTerminator,
    context: BytecodeContext,
) -> Vec<u8> {
    let subroutine_name_map = SubroutineNameSearcher(subroutines);

    let mut bc = bytecode_assembler::BytecodeAssembler::new(
        &dummy_data.instruments_and_samples,
        &dummy_data.pitch_table,
        subroutines,
        &subroutine_name_map,
        context,
    );

    for line in bc_asm {
        bc.parse_line(line).unwrap();
    }

    bc.bytecode(terminator).unwrap().0
}

struct DummyData {
    instruments_and_samples: UniqueNamesList<data::InstrumentOrSample>,
    pitch_table: PitchTable,
}

fn dummy_data() -> DummyData {
    const SF: f64 = SAMPLE_FREQ;

    #[rustfmt::skip]
    let instruments_and_samples = data::validate_instrument_and_sample_names([
        dummy_instrument("dummy_instrument", SF, 2, 6, Envelope::Gain(Gain::new(0))),
        dummy_instrument("dummy_instrument_2", SF, 2, 6, Envelope::Gain(Gain::new(0))),
        dummy_instrument("inst_with_adsr",   SF, 2, 6, Envelope::Adsr(EXAMPLE_ADSR)),
        dummy_instrument("inst_with_gain",   SF, 2, 6, Envelope::Gain(EXAMPLE_GAIN)),
        dummy_instrument("only_octave_four", SF, 4, 4, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f1000_o2", 3000.0, 2, 2, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f1000_o4", 1000.0, 4, 4, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f1000_o5", 1000.0, 5, 5, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f2000_o4", 2000.0, 4, 4, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f1000_o3_o5", 1000.0, 3, 5, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f2000_o3_o5", 2000.0, 3, 5, Envelope::Gain(Gain::new(0))),
        dummy_instrument("f3000_o4", 3000.0, 4, 4, Envelope::Gain(Gain::new(0))),
        dummy_note_instrument("f1000_d5_g5", 3000.0, "d5", "g5", Envelope::Gain(Gain::new(0))),
    ].iter(),
        [
            data::Sample{
                name: "sample".parse().unwrap(),
                source: Default::default(),
                loop_setting: data::LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
                sample_rates: vec![32000, 16000, 18000],
                envelope: Envelope::Gain(EXAMPLE_GAIN),
                comment: None,
            },
        ].iter(),
    ).unwrap();

    let pitch_table = build_pitch_table(&instruments_and_samples).unwrap();

    DummyData {
        instruments_and_samples,
        pitch_table,
    }
}

fn dummy_instrument(
    name: &str,
    freq: f64,
    first_octave: u32,
    last_octave: u32,
    envelope: Envelope,
) -> data::Instrument {
    data::Instrument {
        name: Name::try_from(name.to_owned()).unwrap(),
        source: Default::default(),
        freq,
        loop_setting: data::LoopSetting::LoopWithFilter(SampleNumber(0)),
        evaluator: Default::default(),
        ignore_gaussian_overflow: false,
        note_range: data::InstrumentNoteRange::Octave {
            first: Octave::try_new(first_octave).unwrap(),
            last: Octave::try_new(last_octave).unwrap(),
        },
        envelope,
        comment: None,
    }
}

fn dummy_note_instrument(
    name: &str,
    freq: f64,
    first_note: &str,
    last_note: &str,
    envelope: Envelope,
) -> data::Instrument {
    data::Instrument {
        name: Name::try_from(name.to_owned()).unwrap(),
        source: Default::default(),
        freq,
        loop_setting: data::LoopSetting::LoopWithFilter(SampleNumber(0)),
        evaluator: Default::default(),
        ignore_gaussian_overflow: false,
        note_range: data::InstrumentNoteRange::Note {
            first: Note::parse_bytecode_argument(first_note).unwrap(),
            last: Note::parse_bytecode_argument(last_note).unwrap(),
        },
        envelope,
        comment: None,
    }
}

fn note(note: &str) -> Note {
    Note::parse_bytecode_argument(note).unwrap()
}
