//! Common error functions

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

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
    Expression(ExpressionError),
    Constexpr(ConstexprError<'s>),
    Assember(AssemblerError<'s>),
    Instruction(InstructionError<'s>),
    Symbol(SymbolError),
    Output(OutputError),
    Assert(AssertError<'s>),
}

impl<'s> From<FileParserError<'s>> for FileError<'s> {
    fn from(v: FileParserError<'s>) -> Self {
        Self::FileParser(v)
    }
}

impl From<ExpressionError> for FileError<'_> {
    fn from(v: ExpressionError) -> Self {
        Self::Expression(v)
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

impl<'s> From<SymbolError> for FileError<'s> {
    fn from(v: SymbolError) -> Self {
        Self::Symbol(v)
    }
}

impl<'s> From<OutputError> for FileError<'s> {
    fn from(v: OutputError) -> Self {
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
}

impl std::fmt::Display for FileErrors<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.0 {
            // ::TODO use display instead of format::
            write!(f, "line {}: {:?}", e.0 .0, e.1)?;
        }
        Ok(())
    }
}

impl std::error::Error for FileErrors<'_> {}
