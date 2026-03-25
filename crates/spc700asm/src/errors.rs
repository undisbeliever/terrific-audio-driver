//! Common error functions

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use anstyle::{AnsiColor, Color, Style};
use std::path::Path;

use crate::instructions::InstructionError;
pub use crate::{
    assembler::AssemblerError,
    evaluator::{ConstexprError, ExpressionError},
    file_parser::FileParserError,
    state::{AssertError, OutputError, SymbolError},
};

#[derive(Debug, PartialEq)]
pub enum FileError<'s> {
    FileParser(FileParserError<'s>),
    Constexpr(ConstexprError<'s>),
    Assember(AssemblerError<'s>),
    Instruction(InstructionError<'s>),
    Symbol(SymbolError<'s>),
    Output(OutputError<'s>),
    Assert(AssertError<'s>),
}

impl<'s> From<FileParserError<'s>> for FileError<'s> {
    fn from(v: FileParserError<'s>) -> Self {
        Self::FileParser(v)
    }
}

impl<'s> From<ConstexprError<'s>> for FileError<'s> {
    fn from(v: ConstexprError<'s>) -> Self {
        Self::Constexpr(v)
    }
}

impl<'s> From<AssemblerError<'s>> for FileError<'s> {
    fn from(v: AssemblerError<'s>) -> Self {
        Self::Assember(v)
    }
}

impl<'s> From<InstructionError<'s>> for FileError<'s> {
    fn from(v: InstructionError<'s>) -> Self {
        Self::Instruction(v)
    }
}

impl<'s> From<SymbolError<'s>> for FileError<'s> {
    fn from(v: SymbolError<'s>) -> Self {
        Self::Symbol(v)
    }
}

impl<'s> From<OutputError<'s>> for FileError<'s> {
    fn from(v: OutputError<'s>) -> Self {
        Self::Output(v)
    }
}

impl<'s> From<AssertError<'s>> for FileError<'s> {
    fn from(v: AssertError<'s>) -> Self {
        Self::Assert(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LineNo(pub u32);

#[derive(Debug)]
pub struct FileErrors<'s>(Vec<(LineNo, FileError<'s>)>);

impl<'s> FileErrors<'s> {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn push(&mut self, line_no: LineNo, error: impl Into<FileError<'s>>) {
        self.0.push((line_no, error.into()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn errors(&self) -> &[(LineNo, FileError<'s>)] {
        &self.0
    }

    pub fn color_display<'a>(&'a self, source: &'a Path) -> ColoredFileErrorDisplay<'a> {
        ColoredFileErrorDisplay(self, source)
    }
}

impl std::fmt::Display for FileErrors<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.0 {
            writeln!(f, "line {}: {:?}", e.0 .0, e.1)?;
        }
        Ok(())
    }
}

impl std::error::Error for FileErrors<'_> {}

pub struct ColoredFileErrorDisplay<'a>(&'a FileErrors<'a>, &'a Path);

impl std::fmt::Display for ColoredFileErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const RESET: anstyle::Reset = anstyle::Reset;
        const ERROR: Style = Style::new()
            .bold()
            .fg_color(Some(Color::Ansi(AnsiColor::Red)));
        const FILE: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow)));

        let errors = &self.0 .0;
        let file_name = &self.1.display();

        writeln!(f, "{RESET}{ERROR}Error assembling {file_name}{RESET}")?;
        for e in errors {
            writeln!(f, "    {FILE}{file_name}:{}{RESET}: {:?}", e.0 .0, e.1)?;
        }
        Ok(())
    }
}
