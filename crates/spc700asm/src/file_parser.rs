//! Splits an assembly file into sections

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{FileErrors, LineNo},
    string::{split_first_word, strip_comment},
};

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

pub struct AsmFile<'a> {
    pub global_constants: Vec<Constant<'a>>,

    pub var_banks: Vec<VarBankStatement<'a>>,

    pub vars: Vec<VarsSection<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum FileParserError {
    InvalidBankSyntax,
    InvalidVarLine,
    NoEndVars,
}

fn parse_var_bank<'a>(
    line_no: LineNo,
    args: &'a str,
) -> Result<VarBankStatement<'a>, FileParserError> {
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

fn parse_var_line<'a>(line_no: LineNo, line: &'a str) -> Result<Var<'a>, FileParserError> {
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
    errors: &mut FileErrors,
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

pub fn parse_file<'a>(file: &'a str, errors: &mut FileErrors) -> AsmFile<'a> {
    let mut out = AsmFile {
        global_constants: Vec::new(),
        var_banks: Vec::new(),
        vars: Vec::new(),
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
            ".varbank" => match parse_var_bank(line_no, args) {
                Ok(b) => out.var_banks.push(b),
                Err(e) => errors.push(line_no, e),
            },
            ".vars" => out.vars.push(parse_vars(line_no, args, &mut it, errors)),
            _ => {
                if let Some(expr) = args.strip_prefix("=") {
                    out.global_constants.push(Constant {
                        line_no,
                        name: command,
                        expr: expr.trim_start(),
                    });
                } else {
                    // ::TODO asm line::
                }
            }
        }
    }

    out
}
