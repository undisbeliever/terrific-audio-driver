//! spc700asm integration tests

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use spc700asm::{
    assemble,
    errors::{AssemblerError, ConstexprError, ExpressionError, FileParserError, LineNo},
};

fn l(line: u32) -> LineNo {
    LineNo(line)
}

#[test]
fn variables() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.varbank zeropage  $0000..100+28
.varbank firstpage FP_START..FP_END

FP_START = $0100
FP_END   = $0200

N_CHANNELS = 10

.vars zeropage
    unsigned_byte : u8
    signed_byte : s8
.endvars

.vars firstpage
    pointer     : ptr
    byte_array  : [u8 : N_CHANNELS]
    word_array  : [u16:4]
    word_array2 : [  u16  : 2 + 2 ]
.endvars

.vars zeropage
    unsigned_word : u16
    signed_word : s16
.endvars
"##,
    )?;

    assert_eq!(c.banks[0].name, "zeropage");
    assert_eq!(c.banks[0].range, 0x0000..0x0080);
    assert_eq!(c.banks[0].pos, 0x06);

    assert_eq!(c.banks[1].name, "firstpage");
    assert_eq!(c.banks[1].range, 0x0100..0x0200);
    assert_eq!(c.banks[1].pos, 0x100 + 2 + 10 + 2 * 2 * 4);

    assert_eq!(c.symbols["unsigned_byte"], 0x00);
    assert_eq!(c.symbols["signed_byte"], 0x01);

    assert_eq!(c.symbols["unsigned_word"], 0x02);
    assert_eq!(c.symbols["unsigned_word.l"], 0x02);
    assert_eq!(c.symbols["unsigned_word.h"], 0x03);

    assert_eq!(c.symbols["signed_word"], 0x04);
    assert_eq!(c.symbols["signed_word.l"], 0x04);
    assert_eq!(c.symbols["signed_word.h"], 0x05);

    assert_eq!(c.symbols["pointer"], 0x100);
    assert_eq!(c.symbols["pointer.l"], 0x100);
    assert_eq!(c.symbols["pointer.h"], 0x101);

    assert_eq!(c.symbols["byte_array"], 0x102);

    assert_eq!(c.symbols["word_array"], 0x10c);
    assert_eq!(c.symbols["word_array.l"], 0x10c);
    assert_eq!(c.symbols["word_array.h"], 0x10d);

    assert_eq!(c.symbols["word_array2"], 0x114);
    assert_eq!(c.symbols["word_array2.l"], 0x114);
    assert_eq!(c.symbols["word_array2.h"], 0x115);

    assert_eq!(c.output, &[]);

    Ok(())
}

#[test]
fn invalid_varbank_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.varbank bad1
.varbank bad2  $200..
.varbank bad3  $200..$200
.varbank bad4  unknown..unknown+1
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(2), FileParserError::InvalidBankSyntax.into()),
            (l(3), FileParserError::InvalidBankSyntax.into()),
            (
                l(4),
                AssemblerError::InvalidVarBankRange(0x200, 0x200).into()
            ),
            (l(5), ConstexprError::UnknownValue("unknown").into()),
            (l(5), ConstexprError::UnknownValue("unknown+1").into()),
        ]
    );

    Ok(())
}

#[test]
fn invalid_var_lines_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.varbank zeropage $00..$f0

.vars zeropage
    unknown_type : unknown_type
    invalid_array : [
    invalid_array : [ u8 ]
    invalid_array : [ u8 : ]
    invalid_array : [ u8 : UNKNOWN ]
    too_large : [ u16 : $10000 ]
    too_large : [ u16 : $8001 ]
    two_errors : [ ut : UNKNOWN ]
    multi_array : [[ u8 ; 2 ] ; 2]
    multi_array : [ [ u8 ; 2 ]
.endvars
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(5), AssemblerError::UnknownType("unknown_type").into()),
            (l(6), AssemblerError::InvalidArraySyntax.into()),
            (l(7), AssemblerError::InvalidArraySyntax.into()),
            (
                l(8),
                ConstexprError::InvalidU16("", ExpressionError::SyntaxError).into()
            ),
            (l(9), ConstexprError::UnknownValue("UNKNOWN").into()),
            (l(10), ConstexprError::U16OutOfRange("$10000").into()),
            (l(11), AssemblerError::ArrayTooLarge.into()),
            (l(12), AssemblerError::UnknownType("ut").into()),
            (l(12), ConstexprError::UnknownValue("UNKNOWN").into()),
            (l(13), AssemblerError::CannotNestArrays.into()),
            (l(14), AssemblerError::CannotNestArrays.into()),
        ]
    );

    Ok(())
}

#[test]
fn overflow_vars_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.varbank small_bank  $200..$202

.vars small_bank
    byte1 : u8
    byte2 : u8
    byte3 : u8
.endvars
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(l(7), AssemblerError::VarBankOverflows.into())]
    );

    Ok(())
}
