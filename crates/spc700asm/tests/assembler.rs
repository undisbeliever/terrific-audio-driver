//! spc700asm integration tests

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use spc700asm::{
    assemble,
    errors::{
        AssemblerError, AssertError, ConstexprError, ExpressionError, FileParserError, LineNo,
        OutputError, SymbolError,
    },
};

fn l(line: u32) -> LineNo {
    LineNo(line)
}

#[test]
fn variables() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $200..$300

FP_START = $0100
FP_END   = $0200

.varbank zeropage  $0000..100+28
.varbank firstpage FP_START..FP_END

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

    assert_eq!(c.var_banks[0].name, "zeropage");
    assert_eq!(c.var_banks[0].range, 0x0000..0x0080);
    assert_eq!(c.var_banks[0].pos, 0x06);

    assert_eq!(c.var_banks[1].name, "firstpage");
    assert_eq!(c.var_banks[1].range, 0x0100..0x0200);
    assert_eq!(c.var_banks[1].pos, 0x100 + 2 + 10 + 2 * 2 * 4);

    assert_eq!(c.sym("unsigned_byte"), 0x00);
    assert_eq!(c.sym("signed_byte"), 0x01);

    assert_eq!(c.sym("unsigned_word"), 0x02);
    assert_eq!(c.sym("unsigned_word.l"), 0x02);
    assert_eq!(c.sym("unsigned_word.h"), 0x03);

    assert_eq!(c.sym("signed_word"), 0x04);
    assert_eq!(c.sym("signed_word.l"), 0x04);
    assert_eq!(c.sym("signed_word.h"), 0x05);

    assert_eq!(c.sym("pointer"), 0x100);
    assert_eq!(c.sym("pointer.l"), 0x100);
    assert_eq!(c.sym("pointer.h"), 0x101);

    assert_eq!(c.sym("byte_array"), 0x102);

    assert_eq!(c.sym("word_array"), 0x10c);
    assert_eq!(c.sym("word_array.l"), 0x10c);
    assert_eq!(c.sym("word_array.h"), 0x10d);

    assert_eq!(c.sym("word_array2"), 0x114);
    assert_eq!(c.sym("word_array2.l"), 0x114);
    assert_eq!(c.sym("word_array2.h"), 0x115);

    assert_eq!(c.output, &[]);

    Ok(())
}

#[test]
fn invalid_varbank_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.codebank $200..$300

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
            (l(4), FileParserError::InvalidBankSyntax.into()),
            (l(5), FileParserError::InvalidBankSyntax.into()),
            (
                l(6),
                AssemblerError::InvalidVarBankRange(0x200, 0x200).into()
            ),
            (l(7), ConstexprError::UnknownValue("unknown").into()),
            (l(7), ConstexprError::UnknownValue("unknown+1").into()),
        ]
    );

    Ok(())
}

#[test]
fn invalid_var_lines_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.codebank $200..$300

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
            (l(7), AssemblerError::UnknownType("unknown_type").into()),
            (l(8), AssemblerError::InvalidArraySyntax.into()),
            (l(9), AssemblerError::InvalidArraySyntax.into()),
            (
                l(10),
                ConstexprError::InvalidU16("", ExpressionError::SyntaxError).into()
            ),
            (l(11), ConstexprError::UnknownValue("UNKNOWN").into()),
            (l(12), ConstexprError::U16OutOfRange("$10000").into()),
            (l(13), AssemblerError::ArrayTooLarge.into()),
            (l(14), AssemblerError::UnknownType("ut").into()),
            (l(14), ConstexprError::UnknownValue("UNKNOWN").into()),
            (l(15), AssemblerError::CannotNestArrays.into()),
            (l(16), AssemblerError::CannotNestArrays.into()),
        ]
    );

    Ok(())
}

#[test]
fn overflow_vars_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.codebank $200..$300
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
        &[(l(8), AssemblerError::VarBankOverflows.into())]
    );

    Ok(())
}

#[test]
fn structs() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $200..$300
.varbank zeropage  $0000..$0100

N = 8

.struct Inner
    byte    : u8
    pointer : ptr
    array   : [u16 : N]
    byte2   : u8
.endstruct

.struct Outer
    b : u8
    i : Inner
    a : [Inner : 2]
    end: u8
.endstruct

.vars zeropage
    inner : Inner
    outer_array : [Outer : 2]
    end: u8
.endvars
"##,
    )?;

    assert_eq!(c.sym("inner"), 0);
    assert_eq!(c.sym("inner.byte"), 0);
    assert_eq!(c.sym("inner.pointer"), 1);
    assert_eq!(c.sym("inner.pointer.l"), 1);
    assert_eq!(c.sym("inner.pointer.h"), 2);
    assert_eq!(c.sym("inner.array"), 3);
    assert_eq!(c.sym("inner.array.l"), 3);
    assert_eq!(c.sym("inner.array.h"), 4);
    assert_eq!(c.sym("inner.byte2"), 19);

    assert_eq!(c.sym("outer_array"), 20);
    assert_eq!(c.sym("outer_array.b"), 20);
    assert_eq!(c.sym("outer_array.i"), 21);
    assert_eq!(c.sym("outer_array.i.byte"), 21);
    assert_eq!(c.sym("outer_array.i.pointer"), 22);
    assert_eq!(c.sym("outer_array.i.pointer.l"), 22);
    assert_eq!(c.sym("outer_array.i.pointer.h"), 23);
    assert_eq!(c.sym("outer_array.i.array"), 24);
    assert_eq!(c.sym("outer_array.i.array.l"), 24);
    assert_eq!(c.sym("outer_array.i.array.h"), 25);
    assert_eq!(c.sym("outer_array.i.byte2"), 40);
    assert_eq!(c.sym("outer_array.a"), 41);
    assert_eq!(c.sym("outer_array.a.byte"), 41);
    assert_eq!(c.sym("outer_array.a.pointer"), 42);
    assert_eq!(c.sym("outer_array.a.pointer.l"), 42);
    assert_eq!(c.sym("outer_array.a.pointer.h"), 43);
    assert_eq!(c.sym("outer_array.a.array"), 44);
    assert_eq!(c.sym("outer_array.a.array.l"), 44);
    assert_eq!(c.sym("outer_array.a.array.h"), 45);
    assert_eq!(c.sym("outer_array.a.byte2"), 60);
    assert_eq!(c.sym("outer_array.end"), 81);

    assert_eq!(c.sym("end"), 20 + 62 * 2);

    Ok(())
}

#[test]
fn struct_errors() {
    let e = assemble(
        r##"
.codebank $200..$300

.struct s
    field : u8
    field : u16
    invalid name : u16
    l8   : [u8 : UNKNOWN]
    l9  : [unknown : 2]
    l10  : [u8 : $10000]
    l11  : [u16 : $9000]
    l12  : [u8 : 2
    l13  : u8 : 2]
.endstruct

.struct s
.endstruct

.struct invalid struct name
.endstruct
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(6), AssemblerError::DuplicateField("field").into()),
            (
                l(7),
                AssemblerError::InvalidFieldName("invalid name").into()
            ),
            (l(8), ConstexprError::UnknownValue("UNKNOWN").into()),
            (l(9), AssemblerError::UnknownType("unknown").into()),
            (l(10), ConstexprError::U16OutOfRange("$10000").into()),
            (l(11), AssemblerError::ArrayTooLarge.into()),
            (l(12), AssemblerError::InvalidArraySyntax.into()),
            (l(13), AssemblerError::UnknownType("u8 : 2]").into()),
            (l(16), AssemblerError::DuplicateStruct("s").into()),
            (l(16), AssemblerError::EmptyStruct.into()),
            (
                l(19),
                AssemblerError::InvalidStructName("invalid struct name").into()
            ),
            (l(19), AssemblerError::EmptyStruct.into()),
        ]
    );
}

#[test]
fn global_asm_test() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $200..$300

Label:
    bra Label

Label2: inc A

    mov A, #FORWARD_REFERENCED_CONSTANT & $ff

    bra End
End:

FORWARD_REFERENCED_CONSTANT = End + 2
"##,
    )?;

    assert_eq!(c.sym("Label"), 0x200);
    assert_eq!(c.sym("Label2"), 0x202);
    assert_eq!(c.sym("End"), 0x207);
    assert_eq!(c.sym("FORWARD_REFERENCED_CONSTANT"), 0x209);

    assert_eq!(&c.output, &[0x2f, -2i8 as u8, 0xbc, 0xe8, 9, 0x2f, 0]);

    Ok(())
}

#[test]
fn forward_referenced_constant_is_err() {
    let e = assemble(
        r##"
.codebank $200..$300

FORWARD_REFERENCED_CONSTANT = End + 2
    .db 0
End:
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(
            l(4),
            AssemblerError::ConstantHasUnknownValue("End + 2").into()
        )]
    );
}

#[test]
fn unknown_symbol_asm_test() -> Result<(), Box<dyn std::error::Error>> {
    let e = assemble(
        r##"
.codebank $200..$300

    mov A, #UNKNOWN + 1
    mov missing_var, A
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (
                LineNo(4),
                OutputError::ExpressionHasUnknownValue("UNKNOWN + 1").into()
            ),
            (
                LineNo(5),
                OutputError::ExpressionHasUnknownValue("missing_var").into()
            ),
        ]
    );

    Ok(())
}

#[test]
fn code_too_large_error() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $1000..$1004

    inc A
    inc A
    inc A
"##,
    )?;
    assert_eq!(c.output, vec![0xbc; 3]);

    let e = assemble(
        r##"
.codebank $1000..$1004

    inc A
    inc A
    inc A
    inc A
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(
            l(0),
            AssemblerError::CodeTooLarge(0x1000..0x1004, 0x1004).into()
        )]
    );

    Ok(())
}

#[test]
fn proc() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

zpTmp1 = 0
zpTmp2 = 1

.proc subroutine
_tmp1 = zpTmp1
_tmp2 = zpTmp2

    mov A, _tmp1
    mov _tmp2, A

    call subroutine2

    ret
.endproc

.proc subroutine2
    ret
.endproc
"##,
    )?;

    assert_eq!(c.sym("subroutine"), 0x200);
    assert_eq!(c.sym("subroutine2"), 0x208);

    assert_eq!(c.sym("subroutine._tmp1"), c.sym("zpTmp1"));
    assert_eq!(c.sym("subroutine._tmp2"), c.sym("zpTmp2"));

    assert_eq!(
        c.output,
        &[
            0xe4, 0, 0xc4, 1, 0x3f, 0x08, 0x02, 0x6f, // subroutine
            0x6f, // subroutine2
        ]
    );

    Ok(())
}

#[test]
fn proc_const_scoping_test() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

name = 1

.proc proc1
    name = 2

    mov A, #name
    ret
.endproc

.proc proc2
    mov A, #name
    ret

    name = 3
.endproc

    mov A, #name
    stop
"##,
    )?;

    assert_eq!(
        c.output,
        &[
            0xe8, 2, 0x6f, // proc1
            0xe8, 3, 0x6f, // proc2
            0xe8, 1, 0xff, // global
        ]
    );

    Ok(())
}

#[test]
fn proc_label_scoping_test() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

Label:

.proc proc1
    .dw Label
Label:
    ret
.endproc

.proc proc2
Label:
    .dw Label
    ret
.endproc

.proc proc3
    .dw Label
    ret
.endproc

Global:
    .dw Label * 2
    ret
"##,
    )?;

    assert_eq!(c.sym("Label"), 0x0200);
    assert_eq!(c.sym("proc1.Label"), 0x0202);
    assert_eq!(c.sym("proc2.Label"), 0x0203);

    assert_eq!(
        c.output,
        &[
            0x02, 0x02, 0x6f, // proc1
            0x03, 0x02, 0x6f, // proc2
            0x00, 0x02, 0x6f, // proc3
            0x00, 0x04, 0x6f, // global
        ]
    );

    Ok(())
}

#[test]
fn duplicate_proc_name_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.proc subroutine
    ret
.endproc

.proc subroutine
    ret
.endproc
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(
            l(8),
            AssemblerError::CannotOpenProc("subroutine", SymbolError::DuplicateSymbol).into()
        )]
    );
}

#[test]
fn empty_proc_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.proc subroutine
.endproc
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(5), AssemblerError::EmptyProc.into(),)]);
}

#[test]
fn inline() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

zpTmp1 = 0
zpTmp2 = 1

.inline first_inline
_tmp1 = zpTmp1
_tmp2 = zpTmp2

    mov A, _tmp1
    mov _tmp2, A
.endinline

Global:
    inc A
    first_inline
    ret

.proc InProc
    inc X
    second_inline
    inc X
    ret
.endproc

; Forward referenced inline proc
.inline second_inline
    inc Y
.endinline

"##,
    )?;

    assert_eq!(c.sym("first_inline"), 0x201);
    assert_eq!(c.sym("InProc"), 0x206);
    assert_eq!(c.sym("second_inline"), 0x207);

    assert_eq!(c.sym("first_inline._tmp1"), c.sym("zpTmp1"));
    assert_eq!(c.sym("first_inline._tmp2"), c.sym("zpTmp2"));

    assert_eq!(
        c.output,
        &[
            0xbc, // Global
            0xe4, 0, 0xc4, 1,    // first_inline
            0x6f, // Global
            0x3d, // InProc
            0xfc, // second_inline
            0x3d, 0x6f // InProc
        ]
    );

    Ok(())
}

#[test]
fn nested_inline_calls() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

.inline inner
    constant = 0

    mov A, #constant
.endinline

.inline outer
    constant = 1

    mov A, #constant * 10
    inner
    mov A, #constant
.endinline

.inline outer_outer
    mov A, #constant * 10
    outer
    mov A, #constant

    constant = 2
.endinline

    outer_outer
    stop
"##,
    )?;

    assert_eq!(c.sym("outer_outer"), 0x200);
    assert_eq!(c.sym("outer"), 0x202);
    assert_eq!(c.sym("inner"), 0x204);

    assert_eq!(c.sym("outer_outer.constant"), 2);
    assert_eq!(c.sym("outer.constant"), 1);
    assert_eq!(c.sym("inner.constant"), 0);

    assert_eq!(
        c.output,
        &[
            0xe8, 20, // outer_outer
            0xe8, 10, // outer
            0xe8, 0, // inner
            0xe8, 1, // outer
            0xe8, 2, // outer_outer
            0xff
        ]
    );

    Ok(())
}

#[test]
fn inline_scoping_test() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

name = 1

    mov A, #name
    inline1
    stop

.inline inline1
    name = 2

    mov A, #name
    inline2
.endinline

; Forward referenced inline and inner constant
.inline inline2
    mov A, #name

    name = 3
.endinline
"##,
    )?;

    assert_eq!(
        c.output,
        &[
            0xe8, 1, // global
            0xe8, 2, // inline1
            0xe8, 3,    // inline2
            0xff, // stop
        ]
    );

    Ok(())
}

#[test]
fn duplicate_inline_name_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.inline inline
    inc A
.endinline

.inline inline
    inc A
.endinline

inline
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(l(8), AssemblerError::DuplicateInline("inline").into())]
    );
}

#[test]
fn empty_inline_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.inline inline_subroutine
.endinline

.proc subroutine
    inline_subroutine
    ret
.endproc
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(5), AssemblerError::EmptyInline.into())]);
}

#[test]
fn args_in_inline_call_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.inline inline1
    inc A
.endinline

    inline1 invalid
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(l(8), AssemblerError::CannotUseArgumentsInInlineCall.into())]
    );
}

#[test]
fn confirm_inline_only_used_once() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.inline inline1
    inc A
.endinline

.inline inline2
    inc X
.endinline

inline1
inline1

.proc sub
    inline2
    inline2
.endproc
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(13), AssemblerError::CanOnlyUseInlineOnce.into()),
            (l(17), AssemblerError::CanOnlyUseInlineOnce.into()),
        ]
    );
}

#[test]
fn unused_inline_is_error() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.inline inline1
    inc A
.endinline

.inline inline2
    inc X
.endinline

.inline inline3
    inc Y
.endinline

inline1
inline3
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(l(8), AssemblerError::UnusedInline("inline2").into())]
    );
}

#[test]
fn nesting_inline_and_proc_is_error() {
    let e = assemble(
        r##".codebank $0200..$0300
.inline inline
    .proc inner
        inc A
    .endproc
    inc A
.endinline

inline
"##,
    )
    .err()
    .unwrap();
    assert_eq!(
        e.errors(),
        [
            (l(3), FileParserError::CannotNestInlinesOrProcs.into()),
            (l(5), FileParserError::EndProcInInline.into()),
        ]
    );

    let e = assemble(
        r##".codebank $0200..$0300
.proc proc
    .inline inner
        inc A
    .endinline
    inc A
.endproc
"##,
    )
    .err()
    .unwrap();
    assert_eq!(
        e.errors(),
        [
            (l(3), FileParserError::CannotNestInlinesOrProcs.into()),
            (l(5), FileParserError::EndInlineInProc.into()),
        ]
    );
}

#[test]
fn incorrect_proc_and_inline_end_err() {
    let e = assemble(
        r##".codebank $0200..$0300
inline

.inline inline
    inc A
.endproc
"##,
    )
    .err()
    .unwrap();
    assert_eq!(
        e.errors(),
        [
            (l(6), FileParserError::EndProcInInline.into()),
            (l(4), FileParserError::NoEndInline.into()),
        ]
    );

    let e = assemble(
        r##".codebank $0200..$0300
.proc proc
    inc A
.endinline
"##,
    )
    .err()
    .unwrap();
    assert_eq!(
        e.errors(),
        [
            (l(4), FileParserError::EndInlineInProc.into()),
            (l(2), FileParserError::NoEndProc.into()),
        ]
    );
}

#[test]
fn db_statements() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

    .db 0, 1, 2, %011

    .proc Inner
        .db (Inner.Label * 2) & $ff, Label & $ff, 6, 7
    Label:
    .endproc

        .db Label & $ff
    Label:
"##,
    )?;

    assert_eq!(c.sym("Inner.Label"), 0x0208);
    assert_eq!(c.sym("Label"), 0x0209);

    assert_eq!(
        c.output,
        &[
            0, 1, 2, 3, // first line
            16, 8, 6, 7, // Inner.Label
            9, // global Label
        ]
    );

    Ok(())
}

#[test]
fn dw_statements() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

    .dw 0, $1234

    .proc Inner
        .dw Inner.Label * 2, Label
    Label:
    .endproc

        .dw Label
    Label:
"##,
    )?;

    assert_eq!(c.sym("Inner.Label"), 0x0208);
    assert_eq!(c.sym("Label"), 0x020a);

    assert_eq!(
        c.output,
        &[
            0x00, 0x00, 0x34, 0x12, // first line
            0x10, 0x04, 0x08, 0x02, // Inner
            0x0a, 0x02, // global Label
        ]
    );

    Ok(())
}

#[test]
fn function_tables() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

.ftdef bytecode
    bc_one
    bc_two
    bc_three
.endftdef

.proc bc_one
    .db 0, 1
.endproc

FunctionTable:
    .functiontable bytecode

.proc bc_two
    .db 2
.endproc

bc_three:
    .db 3, 4
"##,
    )?;

    assert_eq!(c.sym("bc_one"), 0x200);
    assert_eq!(c.sym("FunctionTable"), 0x202);
    assert_eq!(c.sym("bc_two"), 0x208);
    assert_eq!(c.sym("bc_three"), 0x209);

    assert_eq!(
        c.output,
        &[
            0, 1, // bc_one
            0x00, 0x02, // bc_one addr
            0x08, 0x02, // bc_two addr
            0x09, 0x02, // bc_three addr
            2,    // bc_two
            3, 4, // bc_three
        ]
    );

    Ok(())
}

#[test]
fn function_table_errors() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.ftdef invalid name
.endftdef

.ftdef bytecode
    invalid function
.endftdef

.ftdef bytecode
    valid_name
.endftdef

.ftdef bytecode
    valid_name
.endftdef

    .functiontable unused
    .functiontable bytecode
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        [
            (l(4), AssemblerError::InvalidFtName("invalid name").into()),
            (
                l(8),
                AssemblerError::InvalidFtFunction("invalid function").into()
            ),
            (l(15), AssemblerError::DuplicateFtdef("bytecode").into()),
            (l(19), AssemblerError::FtdefNotFound("unused").into()),
            (
                l(20),
                OutputError::ExpressionHasUnknownValue("valid_name").into()
            ),
        ]
    );
}

#[test]
fn function_table_not_allowed_in_proc_or_inline() {
    let e = assemble(
        r##"
.codebank $0200..$0300

.proc name
    .functiontable bytecode
    inline_proc
.endproc

.inline inline_proc
    .functiontable bytecode
    .db 0
.endinline
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        [
            (l(5), FileParserError::FunctionTableInProc.into()),
            (l(10), FileParserError::FunctionTableInProc.into()),
        ]
    );
}

#[test]
fn asserts() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $0200..$0300

CONST = 100

.assert CONST % 10 == 0
.assert CONST > 99
.assert CONST < 101

.assert proc.INNER_CONST == -50
.assert proc + 1 == next_proc
.assert proc == next_proc - 1

.proc proc
    INNER_CONST = -50

    .assert INNER_CONST != 0
    .db 0

    .assert PC == next_proc
.endproc

.proc next_proc
    .assert proc.INNER_CONST == -50
    .db 1
.endproc

.assert proc + 1 == next_proc
.assert proc == next_proc - 1

.assert PC == next_proc + 1
.assert PC == $202
"##,
    )?;

    assert_eq!(c.sym("proc"), 0x200);
    assert_eq!(c.sym("next_proc"), 0x201);
    assert_eq!(c.output, &[0, 1]);

    Ok(())
}

#[test]
fn assert_failures() {
    let e = assemble(
        r##"
.codebank $0200..$0300

CONST = 100

.assert CONST == 101
.assert CONST == 100
.assert CONST < 100

.proc proc
    INNER_CONST = 2

    .assert INNER_CONST != 2
    .db 0

    .assert PC == next_proc
.endproc

    .db 0

.proc next_proc
    .db 0
.endproc

.assert PC == $203 ; passes
.assert PC < $200  ; fails

.assert 1 + 2 + 3 ; fails (not a conditional)
.assert UNKNOWN == UNKNOWN ; fails
.assert 1 + (2 ; fails (expression error)
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(6), AssertError::AssertFailure("CONST == 101").into()),
            (l(8), AssertError::AssertFailure("CONST < 100").into()),
            (l(13), AssertError::AssertFailure("INNER_CONST != 2").into()),
            (l(16), AssertError::AssertFailure("PC == next_proc").into()),
            (l(26), AssertError::AssertFailure("PC < $200").into()),
            (l(28), AssertError::NoConditional("1 + 2 + 3").into()),
            (
                l(29),
                AssertError::UnknownSymbol("UNKNOWN == UNKNOWN").into()
            ),
            (
                l(30),
                AssertError::ExpressionError("1 + (2", ExpressionError::UnmatchedParenthesis)
                    .into()
            ),
        ]
    );
}

#[test]
fn cannot_access_pc_outside_asserts() {
    let e = assemble(
        r##"
.codebank $0200..$0300

    .dw PC
    mov A, PC+1
    inline_proc

.proc proc
    .dw PC+2
    mov A, PC+3
.endproc

.inline inline_proc
    .dw PC+4
    mov A, PC+5
.endinline
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[
            (l(4), OutputError::ExpressionHasUnknownValue("PC").into()),
            (l(5), OutputError::ExpressionHasUnknownValue("PC+1").into()),
            (l(14), OutputError::ExpressionHasUnknownValue("PC+4").into()),
            (l(15), OutputError::ExpressionHasUnknownValue("PC+5").into()),
            (l(9), OutputError::ExpressionHasUnknownValue("PC+2").into()),
            (l(10), OutputError::ExpressionHasUnknownValue("PC+3").into()),
        ]
    );
}

#[test]
fn multiple_code_banks_is_error() {
    let e = assemble(
        r##"
.codebank $200..$300
.codebank $300..$400
"##,
    )
    .err()
    .unwrap();

    assert_eq!(
        e.errors(),
        &[(l(3), AssemblerError::MultipleCodeBankStatements.into())]
    );
}

#[test]
fn no_code_bank_set_before_proc_error() {
    let e = assemble(
        r##"
.proc p1
    ret
.endproc

    ; Test only a single CodeBankNotSet error
    .proc p2
        ret
    .endproc

    mov A, #0
    .db 0

    .ftdef ft
        p1
    .endftdef
    .functiontable ft
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(2), AssemblerError::CodeBankNotSet.into())]);
}

#[test]
fn no_code_bank_set_before_asm_error() {
    let e = assemble(
        r##"
    mov A, #0

    ; Test only a single CodeBankNotSet error
    mov A, #0
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(2), AssemblerError::CodeBankNotSet.into())]);
}

#[test]
fn no_code_bank_set_before_functiontable_error() {
    let e = assemble(
        r##"
    .ftdef ft
        label
    .endftdef
    .functiontable ft

    ; Test only a single CodeBankNotSet error
    .functiontable ft
label:
    ret
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(5), AssemblerError::CodeBankNotSet.into())]);
}

#[test]
fn no_code_bank_set_before_inline_error() {
    let e = assemble(
        r##"
    .inline i1
        mov A, #0
    .endinline
    i1

    ; Test only 1 CodeBankNoSetError
    .inline i2
        mov A, #0
    .endinline
    i2
"##,
    )
    .err()
    .unwrap();

    assert_eq!(e.errors(), &[(l(5), AssemblerError::CodeBankNotSet.into())]);
}
