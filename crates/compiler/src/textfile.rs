//! Text file loader

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::FileError,
    path::{ParentPathBuf, SourcePathBuf},
};

use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

pub const MAX_FILE_SIZE: u32 = 5 * 1024 * 1024;

pub struct TextFile {
    pub path: Option<PathBuf>,
    pub file_name: String,
    pub contents: String,
}

pub fn load_text_file_with_limit(
    source: &SourcePathBuf,
    parent_path: &ParentPathBuf,
) -> Result<TextFile, FileError> {
    _load_text_file_with_limit(&source.to_path(parent_path), source.file_name().to_owned())
}

pub fn load_text_file_with_limit_path(path: &Path) -> Result<TextFile, FileError> {
    let file_name = path
        .file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string();

    _load_text_file_with_limit(path, file_name)
}

pub fn _load_text_file_with_limit(path: &Path, file_name: String) -> Result<TextFile, FileError> {
    let file = match File::open(path) {
        Ok(file) => file,
        Err(e) => return Err(FileError::OpenError(file_name, e)),
    };

    // Reading into a buffer as reading exactly `MAX_FILE_SIZE` bytes might output a valid UTF-8 string.
    let mut buffer = Vec::new();

    match file.take(MAX_FILE_SIZE.into()).read_to_end(&mut buffer) {
        Ok(n_bytes_read) => {
            if n_bytes_read >= MAX_FILE_SIZE as usize {
                return Err(FileError::FileTooLarge(file_name));
            }
        }
        Err(e) => return Err(FileError::ReadError(file_name, e)),
    };

    // Forbid ASCII control characters that are not whitespace
    if buffer
        .iter()
        .any(|&c| c.is_ascii_control() && !c.is_ascii_whitespace())
    {
        return Err(FileError::InvalidAsciiControlCharacter(file_name));
    }

    match String::from_utf8(buffer) {
        Ok(contents) => {
            // Normalize line endings
            // Fixes compiler errors
            // Prevents FLTK TextEditor glitches in Linux
            let contents = contents.replace("\r\n", "\n");

            Ok(TextFile {
                path: Some(path.to_owned()),
                file_name,
                contents,
            })
        }
        Err(_) => Err(FileError::Utf8Error(file_name)),
    }
}
