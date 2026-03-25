//! Assembler file loader and line splitter

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{ColoredLoadAssemblyErrorDisplay, LineNo},
    string::strip_comment,
};

use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

pub const MAX_FILE_SIZE: u64 = 2 * 1024 * 1024;
pub const MAX_INCLUDES: usize = 100;

#[derive(Debug)]
pub enum LoadFileError {
    OpenError(std::io::Error),
    ReadError(std::io::Error),
    FileTooLarge,
    Utf8Error,
    InvalidAsciiControlCharacter,
}

impl std::fmt::Display for LoadFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadFileError::OpenError(e) => write!(f, "error opening file: {e}"),
            LoadFileError::ReadError(e) => write!(f, "error reading file: {e}"),
            LoadFileError::FileTooLarge => write!(f, "file is too large"),
            LoadFileError::Utf8Error => write!(f, "not a valid UTF-8 file"),
            LoadFileError::InvalidAsciiControlCharacter => {
                write!(f, "file contains an invalid ASCII control character")
            }
        }
    }
}

#[derive(Debug)]
pub enum IncludeError {
    InvalidSyntax,
    DuplicateInclude(String),
    LoadError(String, LoadFileError),
}

impl std::fmt::Display for IncludeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IncludeError::InvalidSyntax => write!(f, "invalid syntax"),
            IncludeError::DuplicateInclude(n) => write!(f, "duplicate include file: {n}"),
            IncludeError::LoadError(n, e) => write!(f, "cannot load {n}: {e}"),
        }
    }
}

#[derive(Debug)]
pub enum LoadAssemblyError {
    CannotLoadFile(PathBuf, LoadFileError),
    TooManyIncludes(String),
    IncludeErrors(String, Vec<(LineNo, IncludeError)>),
}

impl LoadAssemblyError {
    pub fn color_display(&self) -> ColoredLoadAssemblyErrorDisplay<'_> {
        ColoredLoadAssemblyErrorDisplay(self)
    }
}

pub struct IncludeFile {
    inc_src: String,
    file_number: u16,
    contents: String,
}

pub struct AsmFileWithIncludes {
    file_name: String,
    contents: String,
    includes: Vec<IncludeFile>,
}

impl AsmFileWithIncludes {
    pub(crate) fn asm_filename(&self) -> &str {
        &self.file_name
    }

    pub(crate) fn filename(&self, file_number: u16) -> &str {
        if file_number == 0 {
            &self.file_name
        } else {
            self.includes
                .get(usize::from(file_number - 1))
                .map(|i| i.inc_src.as_ref())
                .unwrap_or("FILE")
        }
    }
}

pub fn load_asm_file_and_includes(path: &Path) -> Result<AsmFileWithIncludes, LoadAssemblyError> {
    let contents =
        load_file(path).map_err(|e| LoadAssemblyError::CannotLoadFile(path.to_owned(), e))?;

    let parent = path.parent().unwrap();
    let file_name = file_name_string(path);

    let mut inc_errors = Vec::new();
    let mut includes = Vec::new();

    for (line_no, line) in contents.lines().enumerate() {
        let line = line.trim_start();
        let line_no = LineNo(0, (line_no + 1).try_into().unwrap_or(0));

        match parse_include_line(line) {
            Some(Ok(inc_src)) => {
                let inc_src = inc_src.to_owned();
                let inc_path = parent.join(&inc_src);

                if includes.len() > MAX_INCLUDES {
                    return Err(LoadAssemblyError::TooManyIncludes(file_name));
                }

                if !includes.iter().any(|i: &IncludeFile| i.inc_src == inc_path) {
                    match load_file(&inc_path) {
                        Ok(contents) => includes.push(IncludeFile {
                            inc_src,
                            file_number: u16::try_from(includes.len() + 1).unwrap(),
                            contents,
                        }),
                        Err(e) => inc_errors.push((line_no, IncludeError::LoadError(inc_src, e))),
                    }
                } else {
                    inc_errors.push((line_no, IncludeError::DuplicateInclude(inc_src)))
                }
            }
            Some(Err(e)) => inc_errors.push((line_no, e)),
            None => (),
        }
    }

    if inc_errors.is_empty() {
        Ok(AsmFileWithIncludes {
            file_name,
            contents,
            includes,
        })
    } else {
        Err(LoadAssemblyError::IncludeErrors(file_name, inc_errors))
    }
}

fn parse_include_line(line: &str) -> Option<Result<&str, IncludeError>> {
    let arg = line.strip_prefix(".include")?;

    if arg.bytes().next().unwrap_or(0).is_ascii_whitespace() {
        match strip_comment(arg)
            .strip_prefix("\"")
            .and_then(|s| s.strip_suffix("\""))
        {
            Some(file_name) => Some(Ok(file_name)),
            None => Some(Err(IncludeError::InvalidSyntax)),
        }
    } else {
        None
    }
}

fn file_name_string(path: &Path) -> String {
    path.file_name()
        .and_then(|p| p.to_str())
        .unwrap_or("file")
        .to_owned()
}

fn load_file(path: &Path) -> Result<String, LoadFileError> {
    let file = match File::open(path) {
        Ok(file) => file,
        Err(e) => return Err(LoadFileError::OpenError(e)),
    };

    let mut buffer = Vec::new();

    match file.take(MAX_FILE_SIZE).read_to_end(&mut buffer) {
        Ok(n_bytes_read) => {
            if n_bytes_read >= MAX_FILE_SIZE as usize {
                return Err(LoadFileError::FileTooLarge);
            }
        }
        Err(e) => return Err(LoadFileError::ReadError(e)),
    };

    // Forbid ASCII control characters that are not whitespace
    if buffer
        .iter()
        .any(|&c| c.is_ascii_control() && !c.is_ascii_whitespace())
    {
        return Err(LoadFileError::InvalidAsciiControlCharacter);
    }

    String::from_utf8(buffer).map_err(|_| LoadFileError::Utf8Error)
}

pub struct SplitLines<'s>(Vec<(LineNo, &'s str)>);

impl<'s> SplitLines<'s> {
    pub fn into_iter(self) -> std::vec::IntoIter<(LineNo, &'s str)> {
        self.0.into_iter()
    }
}

fn line_iter(file_number: u16, contents: &str) -> impl Iterator<Item = (LineNo, &str)> {
    contents.lines().enumerate().filter_map(move |(i, s)| {
        let s = strip_comment(s);
        if !s.is_empty() {
            Some((
                LineNo(file_number, u32::try_from(i + 1).unwrap_or(u32::MAX)),
                s,
            ))
        } else {
            None
        }
    })
}

pub fn split_str_lines<'s>(contents: &'s str) -> SplitLines<'s> {
    SplitLines(line_iter(0, contents).collect())
}

pub fn split_file_lines(file: &AsmFileWithIncludes) -> SplitLines<'_> {
    const _: () = assert!(MAX_INCLUDES < u16::MAX as usize);
    assert!(file.includes.len() <= MAX_INCLUDES);

    let mut out = Vec::new();

    for (line_no, line) in line_iter(0, &file.contents) {
        match parse_include_line(line) {
            Some(Ok(inc_src)) => {
                let inc = file
                    .includes
                    .iter()
                    .find(|inc| inc.inc_src == inc_src)
                    .unwrap();

                out.extend(line_iter(inc.file_number, &inc.contents));
            }
            None => out.push((line_no, line)),
            Some(Err(_)) => panic!(),
        }
    }

    SplitLines(out)
}
