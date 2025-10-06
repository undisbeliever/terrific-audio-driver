// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn key_signature_errors() {
    assert_one_error_in_mml_line("_{+}", 1, ValueError::NoTonesInKeySignature.into());
    assert_one_error_in_mml_line("_{-}", 1, ValueError::NoTonesInKeySignature.into());
    assert_one_error_in_mml_line("_{=}", 1, ValueError::NoTonesInKeySignature.into());

    assert_one_error_in_mml_line("_{+h}", 1, ValueError::InvalidKeySignatureTone('h').into());
    assert_one_error_in_mml_line("_{--}", 1, ValueError::InvalidKeySignatureTone('-').into());
    assert_one_error_in_mml_line("_{=!}", 1, ValueError::InvalidKeySignatureTone('!').into());

    assert_one_error_in_mml_line("_{fc}", 1, ValueError::NoKeySignatureSign.into());

    assert_one_error_in_mml_line("_{+fc", 1, ChannelError::MissingEndKeySignature);
}

#[test]
fn key_signature_note_out_of_range() {
    assert_one_error_in_mml_line("_{-c} o0 c", 10, ValueError::InvalidNote.into());
    assert_one_error_in_mml_line("_{+b} o7 b", 10, ValueError::InvalidNote.into());
}

#[test]
fn key_signature() {
    assert_line_matches_line("_{+abcdefgab} cdefgab", "c+d+e+f+g+a+b+");
    assert_line_matches_line("_{-abcdefgab} cdefgab", "c-d-e-f-g-a-b-");
    assert_line_matches_line("_{=abcdefgab} cdefgab", "cdefgab");

    // From mml-syntax
    assert_line_matches_line("_{+fcg} ab>cdefga", "a b > c+ d e f+ g+ a");
    assert_line_matches_line("_{-be} b>cdefgab", "b- > c d e- f g a b-");
    assert_line_matches_line("_{+fc} cdef _{=fc} cdef", "c+ d e f+ | c d e f");

    assert_line_matches_line("_{+ aaa\taa aa\taa} cdefgab", "cdefga+b");

    assert_line_matches_line("_{+fc} cdefgab", "c+ d e f+ g a b");
    assert_line_matches_line("_{-bea} cdefgab", "c d e- f g a- b-");
    assert_line_matches_line("_{-bea} cdefgab", "c d e- f g a- b-");

    assert_line_matches_line(
        "_{+fcg} cdefgab _{=fcg} cdefgab",
        "c+ d e f+ g+ a b | cdefgab",
    );
    assert_line_matches_line(
        "_{-abe} cdefgab _{=eba} cdefgab",
        "c d e- f g a- b- | cdefgab",
    );

    assert_line_matches_line("_{+fcgd} _{-cde} cdefgab", "c- d- e- f+ g+ a b");
}

#[test]
fn natural_ignores_key_signature() {
    assert_line_matches_line("_{+fcg} c=defgab", "c d e f+ g+ a b");
    assert_line_matches_line("_{-be} c=d=e=f=g=a=b=", "c d e f g a b");

    assert_line_matches_line("_{+fcg} f=- c=-- g=---", "f- c-- g---");
    assert_line_matches_line("_{+fcg} f= c=+ g=++", "f c+ g++");

    assert_line_matches_line("_{-bea} b=+ e=++a=+++", "b+ e++ a+++");
    assert_line_matches_line("_{-bea} b= e=- a=--", "b e- a--");

    assert_line_matches_line("_M-2 _{+c} c c=", "c- c--");
    assert_line_matches_line("_{+c} _M-2 c c=", "c- c--");

    // from mml-syntax.md
    assert_line_matches_line("_{+fc} c c= c=- c=+", "c+ c c- c+");
}

#[test]
fn key_signature_loops() {
    assert_line_matches_line("[ efc : _{+fc} efc ]4 f", "[ efc : ef+c+ ]4 f+");
    assert_line_matches_line("[ abc : _{-eb} abc ]4 e", "[ abc : ab-c ]4 e-");
}

#[test]
fn key_signature_and_channel_transpose() {
    assert_line_matches_line("_M+2 c _{+c} c", "c++ c+++");
    assert_line_matches_line("_M-3 d _{-d} d", "d--- d----");
}

#[test]
fn key_signature_portamento() {
    assert_line_matches_line(
        "_{+fc} {fc} {cd} {ef} {ga} {bc}",
        "{f+c+} {c+d} {ef+} {ga} {bc+}",
    );
    assert_line_matches_line(
        "_{-bea} {be} {cd} {ef} {ga} {bc}",
        "{b-e-} {cd} {e-f} {ga-} {b-c}",
    );
    assert_line_matches_line(
        "_{+ac} _{-ce} _{=c} {ca} {ae} {ea} {ac}",
        "{ca+} {a+e-} {e-a+} {a+c}",
    );
}

#[test]
fn key_signature_broken_chord() {
    assert_line_matches_line("_{+fc} {{cdefgab}}", "{{c+ d e f+ g a b}}");
    assert_line_matches_line("_{-bea} {{cdefgab}}", "{{c d e- f g a- b-}}");
}

#[test]
fn key_signature_and_accidentals() {
    assert_line_matches_line("_{+fc} c-d-e-f-g-a-b-", "c-+ d- e- f-+ g- a- b-");
    assert_line_matches_line("_{-bea} c+d+e+f+g+a+b+", "c+ d+ e+- f+ g+ a+- b+-");

    assert_line_matches_line(
        "_{+abcdefgab} c--d--e--f--g--a--b--",
        "c- d- e- f- g- a- b-",
    );
    assert_line_matches_line(
        "_{-abcdefgab} c++d++e++f++g++a++b++",
        "c+ d+ e+ f+ g+ a+ b+",
    );
}

#[test]
fn key_signature_subroutines() {
    let mml = r##"
@1 dummy_instrument

!s _{+fc} afc _{-ac} afc _{=c} afc

A @1 _{+fc} afc !s _{-ac} afc !s _{=c} afc
"##;
    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &[
            "play_note a4 24",
            "play_note f+4 24",
            "play_note c+4 24",
            //
            "play_note a-4 24",
            "play_note f+4 24",
            "play_note c-4 24",
            //
            "play_note a-4 24",
            "play_note f+4 24",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "play_note a4 24",
            "play_note f+4 24",
            "play_note c+4 24",
            "call_subroutine s",
            "play_note a-4 24",
            "play_note f+4 24",
            "play_note c-4 24",
            "call_subroutine s",
            "play_note a-4 24",
            "play_note f+4 24",
            "play_note c4 24",
        ],
    );
}

#[test]
fn key_signature_header_1() {
    let mml = r##"
#KeySignature +fe

@0 dummy_instrument

!s ?@0 fec

A @0 !s fec
B @0 !s fec
"##;

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note f+4 24",
            "play_note e+4 24",
            "play_note c4 24",
        ],
    );

    assert_mml_channel_b_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note f+4 24",
            "play_note e+4 24",
            "play_note c4 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &["play_note f+4 24", "play_note e+4 24", "play_note c4 24"],
    );
}

#[test]
fn key_signature_header_2() {
    let mml = r##"
#KeySignature +abcdefg, - a b, = b

@0 dummy_instrument

!s ?@0 abc _{-c} c

A @0 !s abc _{-c} c
B @0 !s abc _{-c} c
"##;

    assert_mml_channel_a_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note a-4 24",
            "play_note b4 24",
            "play_note c+4 24",
            "play_note c-4 24",
        ],
    );

    assert_mml_channel_b_matches_bytecode(
        mml,
        &[
            "set_instrument dummy_instrument",
            "call_subroutine s",
            "play_note a-4 24",
            "play_note b4 24",
            "play_note c+4 24",
            "play_note c-4 24",
        ],
    );

    assert_mml_subroutine_matches_bytecode(
        mml,
        0,
        &[
            "play_note a-4 24",
            "play_note b4 24",
            "play_note c+4 24",
            "play_note c-4 24",
        ],
    );
}
