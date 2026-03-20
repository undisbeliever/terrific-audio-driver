//! Splits an assembly file into sections

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{FileErrors, LineNo},
    string::{split_first_word, strip_comment},
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

pub struct Constant<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub expr: &'a str,
}

pub struct Var<'a> {
    pub line_no: LineNo,
    pub name: &'a str,
    pub var_type: &'a str,
}

pub struct VarsSection<'a> {
    pub line_no: LineNo,
    pub bank: &'a str,
    pub variables: Vec<Var<'a>>,
}

pub enum AsmLine<'a> {
    Label(&'a str),
    Instruction(&'a str, &'a str),
}

pub struct Procedure<'a> {
    pub line_no: LineNo,
    pub end_line_no: LineNo,
    pub name: &'a str,
    pub constants: Vec<Constant<'a>>,
    pub lines: Vec<(LineNo, AsmLine<'a>)>,
}

pub enum AsmOrProc<'a> {
    AsmLine(LineNo, AsmLine<'a>),
    Procedure(Procedure<'a>),
}

pub struct AsmFile<'a> {
    pub global_constants: Vec<Constant<'a>>,

    pub code_bank: Option<CodeBankStatement<'a>>,
    pub var_banks: Vec<VarBankStatement<'a>>,

    pub vars: Vec<VarsSection<'a>>,

    pub assembly: Vec<AsmOrProc<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum FileParserError<'a> {
    InvalidCodeBankSyntax,
    MultipleCodeBankStatements,
    InvalidBankSyntax,
    InvalidVarLine,
    NoEndVars,
    InvalidCommand(&'a str),
    NoEndProc,
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

pub fn parse_asm_line_after_label<'a>(
    line_no: LineNo,
    first_word: &'a str,
    arguments: &'a str,
    errors: &mut FileErrors<'a>,
    f: &mut dyn FnMut(LineNo, AsmLine<'a>),
) {
    match first_word {
        fw if first_word.starts_with(".") => {
            errors.push(line_no, FileParserError::InvalidCommand(first_word))
        }
        _ => f(line_no, AsmLine::Instruction(first_word, arguments)),
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

fn parse_proc<'a>(
    line_no: LineNo,
    name: &'a str,
    it: &mut impl Iterator<Item = (LineNo, &'a str)>,
    errors: &mut FileErrors<'a>,
) -> Procedure<'a> {
    let mut out = Procedure {
        line_no,
        end_line_no: LineNo(0),
        name,
        constants: Vec::new(),
        lines: Vec::new(),
    };

    for (line_no, line) in it {
        let (first_word, arguments) = split_first_word(line);

        match first_word {
            ".endproc" => {
                out.end_line_no = line_no;
                return out;
            }
            _ => {
                if let Some(expr) = arguments.strip_prefix("=") {
                    out.constants.push(Constant {
                        line_no,
                        name: first_word,
                        expr: expr.trim_start(),
                    });
                } else {
                    parse_asm_line(line_no, first_word, arguments, errors, &mut |ln, l| {
                        out.lines.push((ln, l))
                    });
                }
            }
        }
    }

    errors.push(line_no, FileParserError::NoEndProc);

    out
}

pub fn parse_file<'a>(file: &'a str, errors: &mut FileErrors<'a>) -> AsmFile<'a> {
    let mut out = AsmFile {
        global_constants: Vec::new(),
        code_bank: None,
        var_banks: Vec::new(),
        vars: Vec::new(),
        assembly: Vec::new(),
    };
    let mut it = file.lines().enumerate().filter_map(|(i, s)| {
        let s = strip_comment(s);
        if !s.is_empty() {
            Some((LineNo(u32::try_from(i + 1).unwrap_or(u32::MAX)), s))
        } else {
            None
        }
    });

    while let Some((line_no, line)) = it.next() {
        let (command, args) = split_first_word(line);

        match command {
            ".codebank" => {
                if out.code_bank.is_none() {
                    match parse_code_bank(line_no, args) {
                        Ok(b) => out.code_bank = Some(b),
                        Err(e) => errors.push(line_no, e),
                    }
                } else {
                    errors.push(line_no, FileParserError::MultipleCodeBankStatements)
                }
            }
            ".varbank" => match parse_var_bank(line_no, args) {
                Ok(b) => out.var_banks.push(b),
                Err(e) => errors.push(line_no, e),
            },
            ".vars" => out.vars.push(parse_vars(line_no, args, &mut it, errors)),
            ".proc" => out.assembly.push(AsmOrProc::Procedure(parse_proc(
                line_no, args, &mut it, errors,
            ))),
            _ => {
                if let Some(expr) = args.strip_prefix("=") {
                    out.global_constants.push(Constant {
                        line_no,
                        name: command,
                        expr: expr.trim_start(),
                    });
                } else {
                    parse_asm_line(line_no, command, args, errors, &mut |n, l| {
                        out.assembly.push(AsmOrProc::AsmLine(n, l))
                    });
                }
            }
        }
    }

    out
}
