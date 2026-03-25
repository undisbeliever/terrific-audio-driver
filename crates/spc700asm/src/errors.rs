//! Common error functions

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

pub use crate::{
    assembler::AssemblerError,
    evaluator::{ConstexprError, ExpressionError},
    file_parser::FileParserError,
    state::{AssertError, OutputError, SymbolError},
};
use crate::{
    file_loader::{AsmFileWithIncludes, LoadAssemblyError},
    instructions::InstructionError,
};

use anstyle::{AnsiColor, Color, Style};

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
pub struct LineNo(pub u16, pub u32);

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

    pub fn color_display<'a>(
        &'a self,
        file: &'a AsmFileWithIncludes,
    ) -> ColoredFileErrorDisplay<'a> {
        ColoredFileErrorDisplay(self, file)
    }
}

impl std::fmt::Display for FileErrors<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (line_no, e) in &self.0 {
            writeln!(f, "file{} line {}: {:?}", line_no.0, line_no.1, e)?;
        }
        Ok(())
    }
}

impl std::error::Error for FileErrors<'_> {}

pub struct ColoredFileErrorDisplay<'a>(&'a FileErrors<'a>, &'a AsmFileWithIncludes);

impl std::fmt::Display for ColoredFileErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const RESET: anstyle::Reset = anstyle::Reset;
        const ERROR: Style = Style::new()
            .bold()
            .fg_color(Some(Color::Ansi(AnsiColor::Red)));
        const FILE: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow)));

        let errors = &self.0 .0;
        let af = &self.1;

        writeln!(
            f,
            "{RESET}{ERROR}Error assembling {}{RESET}",
            af.asm_filename()
        )?;
        for (l, e) in errors {
            writeln!(f, "    {FILE}{}:{}{RESET}: {:?}", af.filename(l.0), l.1, e)?;
        }
        Ok(())
    }
}

pub struct ColoredLoadAssemblyErrorDisplay<'a>(pub(crate) &'a LoadAssemblyError);

impl std::fmt::Display for ColoredLoadAssemblyErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const RESET: anstyle::Reset = anstyle::Reset;
        const ERROR: Style = Style::new()
            .bold()
            .fg_color(Some(Color::Ansi(AnsiColor::Red)));
        const FILE: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow)));

        match self.0 {
            LoadAssemblyError::CannotLoadFile(path, e) => {
                writeln!(
                    f,
                    "{RESET}{ERROR}Error loading {}{RESET}: {:?}",
                    path.display(),
                    e
                )
            }
            LoadAssemblyError::TooManyIncludes(path) => {
                writeln!(
                    f,
                    "{RESET}{ERROR}Error loading {path}{RESET}: Too many .includes",
                )
            }
            LoadAssemblyError::IncludeErrors(path, items) => {
                writeln!(f, "{RESET}{ERROR}Error loading {path}{RESET}:")?;
                for (l, e) in items {
                    writeln!(f, "    {FILE}{}:{}{RESET}: {:?}", path, l.1, e)?;
                }
                Ok(())
            }
        }
    }
}
