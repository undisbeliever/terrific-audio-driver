//! spc700asm integration tests

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use spc700asm::{
    assemble,
    errors::{
        AssemblerError, ConstexprError, ExpressionError, FileParserError, LineNo, OutputError,
        SymbolError,
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
fn global_asm_test() -> Result<(), Box<dyn std::error::Error>> {
    let c = assemble(
        r##"
.codebank $200..$300

FORWARD_REFERENCED_CONSTANT = End + 2

Label:
    bra Label

Label2: inc A

    mov A, #FORWARD_REFERENCED_CONSTANT & $ff

    bra End
End:
"##,
    )?;

    assert_eq!(c.symbols["Label"], 0x200);
    assert_eq!(c.symbols["Label2"], 0x202);
    assert_eq!(c.symbols["End"], 0x207);
    assert_eq!(c.symbols["FORWARD_REFERENCED_CONSTANT"], 0x209);

    assert_eq!(&c.output, &[0x2f, -2i8 as u8, 0xbc, 0xe8, 9, 0x2f, 0]);

    Ok(())
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
                OutputError::ExpressionHasUnknownValue("UNKNOWN + 1".to_string()).into()
            ),
            (
                LineNo(5),
                OutputError::ExpressionHasUnknownValue("missing_var".to_string()).into()
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

    assert_eq!(c.symbols["subroutine"], 0x200);
    assert_eq!(c.symbols["subroutine2"], 0x208);

    assert_eq!(c.symbols["subroutine._tmp1"], c.symbols["zpTmp1"]);
    assert_eq!(c.symbols["subroutine._tmp2"], c.symbols["zpTmp2"]);

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
fn proc_scoping_test() -> Result<(), Box<dyn std::error::Error>> {
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

    assert_eq!(c.symbols["first_inline"], 0x201);
    assert_eq!(c.symbols["InProc"], 0x206);
    assert_eq!(c.symbols["second_inline"], 0x207);

    assert_eq!(c.symbols["first_inline._tmp1"], c.symbols["zpTmp1"]);
    assert_eq!(c.symbols["first_inline._tmp2"], c.symbols["zpTmp2"]);

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

    assert_eq!(c.symbols["outer_outer"], 0x200);
    assert_eq!(c.symbols["outer"], 0x202);
    assert_eq!(c.symbols["inner"], 0x204);

    assert_eq!(c.symbols["outer_outer.constant"], 2);
    assert_eq!(c.symbols["outer.constant"], 1);
    assert_eq!(c.symbols["inner.constant"], 0);

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
