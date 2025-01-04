// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::*;

#[test]
fn hexadecimal_numbers() {
    assert!(96 / 0xc == 8);
    assert_line_matches_bytecode("c$c", &["play_note c4 8"]);
    assert_line_matches_bytecode("c$c.", &["play_note c4 12"]);
    assert_line_matches_bytecode("c%$ba", &["play_note c4 186"]);
    assert_line_matches_bytecode("A $f,$7,$7,$1f", &["set_adsr 15 7 7 31"]);
    assert_line_matches_bytecode("G $7f", &["set_gain 127"]);
}

#[test]
fn change_whole_note_length_command() {
    assert_line_matches_bytecode(
        "c d16 C192 c d16",
        &[
            "play_note c4 24",
            "play_note d4  6",
            "play_note c4 48",
            "play_note d4 12",
        ],
    );
}

#[test]
fn mml_repeated_channel_is_only_processed_once() {
    assert_mml_channel_a_matches_bytecode(
        r###"
@0 dummy_instrument

AAAAAAAAAAA @0 a
"###,
        &["set_instrument dummy_instrument", "play_note a4 24"],
    );
}

/// Test the bytecode is repeated 4 times if there are 4 different channels on a single MML line
#[test]
fn mml_with_multiple_channels_on_one_line() {
    let mml = r###"
@0 dummy_instrument

ADEF @0 a
"###;
    let bc_asm = &["set_instrument dummy_instrument", "play_note a4 24"];

    let dummy_data = dummy_data();

    let mml = compile_mml(mml, &dummy_data);

    let bc_asm = assemble_channel_bytecode(
        bc_asm,
        &dummy_data.instruments_and_samples,
        mml.subroutines(),
        BcTerminator::DisableChannel,
        BytecodeContext::SongChannel { index: 0 },
    )
    .repeat(4);

    assert_eq!(mml_bytecode(&mml), bc_asm);
}

#[test]
fn utf8_in_mml() {
    assert_eq!("´".len(), 2);
    assert_one_error_in_mml_line("´", 1, ChannelError::UnknownCharacters(1));

    assert_one_error_in_mml_line("´´´´´", 1, ChannelError::UnknownCharacters(5));

    // Google translate for "this is an error"
    assert_one_error_in_mml_line("これはエラーです", 1, ChannelError::UnknownCharacters(8));

    assert_eq!("⚠".len(), 3);
    assert_one_error_in_mml_line("⚠☹⛔", 1, ChannelError::UnknownCharacters(3));

    assert_eq!("𝅘𝅥𝅮".len(), 4);
    assert_one_error_in_mml_line("𝅘𝅥𝅮𝅘𝅥𝅮𝅘𝅥𝅮𝅗𝅥𝅗𝅥𝅘𝅥𝅮𝅘𝅥𝅮𝅘𝅥𝅮", 1, ChannelError::UnknownCharacters(8));
}
