//! Splits an assembly file into sections

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{FileErrors, LineNo},
    file_loader::SplitLines,
    string::split_first_word,
};

pub struct CodeBankStatement<'a> {
    pub line_no: LineNo,
    pub start_expr: &'a str,
    pub end_expr: &'a str,
}

pub struct VarBankStatement<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub start_expr: &'a str,
    pub end_expr: &'a str,
}

pub struct Var<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub var_type: &'a str,
}

pub struct StructSection<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub fields: Vec<Var<'a>>,
}

pub struct VarsSection<'a> {
    pub line_no: LineNo,
    pub bank: &'a str,
    pub variables: Vec<Var<'a>>,
}

pub enum AsmLine<'a> {
    Constant { name: &'a str, expr: &'a str },
    Label(&'a str),
    Instruction(&'a str, &'a str),
    Db(&'a str),
    Dw(&'a str),
    Assert(&'a str),
}

pub struct Procedure<'a> {
    pub line_no: LineNo,
    pub end_line_no: LineNo,
    pub name: &'a str,
    pub lines: Vec<(LineNo, AsmLine<'a>)>,
}

pub struct FunctionTableDef<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub functions: Vec<(LineNo, &'a str)>,
}

pub enum GlobalAsm<'a> {
    CodeBank(CodeBankStatement<'a>),
    VarBank(VarBankStatement<'a>),
    Struct(StructSection<'a>),
    Vars(VarsSection<'a>),
    FunctionTableDef(FunctionTableDef<'a>),
    FunctionTable(LineNo, &'a str),
    Procedure(Procedure<'a>),
    AsmLine(LineNo, AsmLine<'a>),
}

pub struct AsmFile<'a> {
    pub inlines: Vec<Procedure<'a>>,
    pub assembly: Vec<GlobalAsm<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum FileParserError<'a> {
    InvalidCodeBankSyntax,
    InvalidBankSyntax,
    InvalidStructLine,
    NoEndStruct,
    InvalidVarLine,
    NoEndVars,
    InvalidCommand(&'a str),
    NoEndProc,
    NoEndInline,
    EndInlineInProc,
    EndProcInInline,
    CannotNestInlinesOrProcs,
    InvalidFunctionTableDefLine,
    NoEndftdef,
    FunctionTableInProc,
    CannotNestIncludes,
}

fn parse_code_bank<'a>(
    line_no: LineNo,
    args: &'a str,
) -> Result<CodeBankStatement<'a>, FileParserError<'a>> {
    match args.split_once("..") {
        Some((start_expr, end_expr)) if !start_expr.is_empty() && !end_expr.is_empty() => {
            Ok(CodeBankStatement {
                line_no,
                start_expr,
                end_expr,
            })
        }
        _ => Err(FileParserError::InvalidCodeBankSyntax),
    }
}

fn parse_var_bank<'a>(
    line_no: LineNo,
    args: &'a str,
) -> Result<VarBankStatement<'a>, FileParserError<'a>> {
    let (name, range) = split_first_word(args);

    match range.split_once("..") {
        Some((start_expr, end_expr)) if !start_expr.is_empty() && !end_expr.is_empty() => {
            Ok(VarBankStatement {
                line_no,
                name,
                start_expr,
                end_expr,
            })
        }
        _ => Err(FileParserError::InvalidBankSyntax),
    }
}

fn parse_struct<'a>(
    var_line_no: LineNo,
    var_bank: &'a str,
    it: &mut impl Iterator<Item = (LineNo, &'a str)>,
    errors: &mut FileErrors<'a>,
) -> StructSection<'a> {
    let mut out = StructSection {
        line_no: var_line_no,
        name: var_bank,
        fields: Vec::new(),
    };

    for (line_no, line) in it {
        if !line.starts_with(".") {
            match line.split_once(':') {
                Some((name, var_type)) => out.fields.push(Var {
                    line_no,
                    name: name.trim(),
                    var_type: var_type.trim(),
                }),
                None => errors.push(line_no, FileParserError::InvalidStructLine),
            }
        } else if line == ".endstruct" {
            return out;
        } else {
            errors.push(line_no, FileParserError::InvalidStructLine)
        }
    }

    errors.push(var_line_no, FileParserError::NoEndStruct);
    out
}

fn parse_var_line<'a>(line_no: LineNo, line: &'a str) -> Result<Var<'a>, FileParserError<'a>> {
    match line.split_once(':') {
        Some((name, var_type)) => Ok(Var {
            line_no,
            name: name.trim(),
            var_type: var_type.trim(),
        }),
        None => Err(FileParserError::InvalidVarLine),
    }
}

fn parse_vars<'a>(
    var_line_no: LineNo,
    var_bank: &'a str,
    it: &mut impl Iterator<Item = (LineNo, &'a str)>,
    errors: &mut FileErrors<'a>,
) -> VarsSection<'a> {
    let mut out = VarsSection {
        line_no: var_line_no,
        bank: var_bank,
        variables: Vec::new(),
    };

    for (line_no, line) in it {
        if !line.starts_with(".") {
            match parse_var_line(line_no, line) {
                Ok(v) => out.variables.push(v),
                Err(e) => errors.push(line_no, e),
            }
        } else if line == ".endvars" {
            return out;
        } else {
            errors.push(line_no, FileParserError::InvalidVarLine)
        }
    }

    errors.push(var_line_no, FileParserError::NoEndVars);
    out
}

fn parse_ftdef<'a>(
    line_no: LineNo,
    name: &'a str,
    it: &mut impl Iterator<Item = (LineNo, &'a str)>,
    errors: &mut FileErrors<'a>,
) -> FunctionTableDef<'a> {
    let mut out = FunctionTableDef {
        line_no,
        name,
        functions: Vec::new(),
    };

    for (line_no, line) in it {
        if !line.starts_with(".") {
            out.functions.push((line_no, line));
        } else if line == ".endftdef" {
            return out;
        } else {
            errors.push(line_no, FileParserError::InvalidFunctionTableDefLine)
        }
    }

    errors.push(line_no, FileParserError::NoEndftdef);
    out
}

fn parse_asm_line_after_label<'a>(
    line_no: LineNo,
    first_word: &'a str,
    arguments: &'a str,
    errors: &mut FileErrors<'a>,
    f: &mut dyn FnMut(LineNo, AsmLine<'a>),
) {
    match first_word {
        ".db" => f(line_no, AsmLine::Db(arguments)),
        ".dw" => f(line_no, AsmLine::Dw(arguments)),
        ".assert" => f(line_no, AsmLine::Assert(arguments)),
        ".include" => errors.push(line_no, FileParserError::CannotNestIncludes),
        fw if first_word.starts_with(".") => {
            errors.push(line_no, FileParserError::InvalidCommand(first_word))
        }
        _ => {
            if let Some(expr) = arguments.strip_prefix("=") {
                f(
                    line_no,
                    AsmLine::Constant {
                        name: first_word,
                        expr: expr.trim_start(),
                    },
                )
            } else {
                f(line_no, AsmLine::Instruction(first_word, arguments))
            }
        }
    }
}

pub fn parse_asm_line<'a>(
    line_no: LineNo,
    first_word: &'a str,
    arguments: &'a str,
    errors: &mut FileErrors<'a>,
    f: &mut dyn FnMut(LineNo, AsmLine<'a>),
) {
    if let Some(label) = first_word.strip_suffix(":") {
        f(line_no, AsmLine::Label(label));

        if !arguments.is_empty() {
            let (first_word, arguments) = split_first_word(arguments);
            parse_asm_line_after_label(line_no, first_word, arguments, errors, f);
        }
    } else {
        parse_asm_line_after_label(line_no, first_word, arguments, errors, f);
    }
}

enum ProcType {
    Proc,
    Inline,
}

fn parse_proc_or_inline<'a>(
    line_no: LineNo,
    name: &'a str,
    proc_type: ProcType,
    it: &mut impl Iterator<Item = (LineNo, &'a str)>,
    errors: &mut FileErrors<'a>,
) -> Procedure<'a> {
    let mut out = Procedure {
        line_no,
        end_line_no: LineNo(0, 0),
        name,
        lines: Vec::new(),
    };

    for (line_no, line) in it {
        let (first_word, arguments) = split_first_word(line);

        match first_word {
            ".endproc" => match proc_type {
                ProcType::Proc => {
                    out.end_line_no = line_no;
                    return out;
                }
                ProcType::Inline => errors.push(line_no, FileParserError::EndProcInInline),
            },
            ".endinline" => match proc_type {
                ProcType::Inline => {
                    out.end_line_no = line_no;
                    return out;
                }
                ProcType::Proc => errors.push(line_no, FileParserError::EndInlineInProc),
            },
            ".proc" | ".inline" => errors.push(line_no, FileParserError::CannotNestInlinesOrProcs),
            ".functiontable" => errors.push(line_no, FileParserError::FunctionTableInProc),
            _ => parse_asm_line(line_no, first_word, arguments, errors, &mut |ln, l| {
                out.lines.push((ln, l))
            }),
        }
    }

    errors.push(
        line_no,
        match proc_type {
            ProcType::Proc => FileParserError::NoEndProc,
            ProcType::Inline => FileParserError::NoEndInline,
        },
    );
    out
}

pub fn parse_file<'a, 's>(lines: SplitLines<'s>, errors: &mut FileErrors<'s>) -> AsmFile<'s>
where
    'a: 's,
{
    let mut out = AsmFile {
        inlines: Vec::new(),
        assembly: Vec::new(),
    };

    let mut it = lines.into_iter();

    while let Some((line_no, line)) = it.next() {
        let (command, args) = split_first_word(line);

        match command {
            ".codebank" => match parse_code_bank(line_no, args) {
                Ok(b) => out.assembly.push(GlobalAsm::CodeBank(b)),
                Err(e) => errors.push(line_no, e),
            },
            ".varbank" => match parse_var_bank(line_no, args) {
                Ok(b) => out.assembly.push(GlobalAsm::VarBank(b)),
                Err(e) => errors.push(line_no, e),
            },
            ".struct" => out.assembly.push(GlobalAsm::Struct(parse_struct(
                line_no, args, &mut it, errors,
            ))),
            ".vars" => out
                .assembly
                .push(GlobalAsm::Vars(parse_vars(line_no, args, &mut it, errors))),
            ".ftdef" => out.assembly.push(GlobalAsm::FunctionTableDef(parse_ftdef(
                line_no, args, &mut it, errors,
            ))),
            ".proc" => out.assembly.push(GlobalAsm::Procedure(parse_proc_or_inline(
                line_no,
                args,
                ProcType::Proc,
                &mut it,
                errors,
            ))),
            ".inline" => out.inlines.push(parse_proc_or_inline(
                line_no,
                args,
                ProcType::Inline,
                &mut it,
                errors,
            )),
            ".functiontable" => out.assembly.push(GlobalAsm::FunctionTable(line_no, args)),
            _ => parse_asm_line(line_no, command, args, errors, &mut |n, l| {
                out.assembly.push(GlobalAsm::AsmLine(n, l))
            }),
        }
    }

    out
}
