//! FilePos

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

// Having a cap on MML text length ensures all positions can fit inside a i32 (for use in fltk).
pub const MAX_MML_TEXT_LENGTH: usize = 512 * 1024;

#[derive(Debug, Copy, Clone)]
pub struct FilePos {
    pub(crate) line_number: u32,
    pub(crate) line_char: u32,
    pub(crate) char_index: u32,
}
const _: () = assert!(
    // Using i32 as fltk uses i32 for indexing in TextBuffer
    MAX_MML_TEXT_LENGTH < i32::MAX as usize,
    "Cannot use u32 in FilePos"
);

impl FilePos {
    pub fn char_index(&self) -> u32 {
        self.char_index
    }
}

pub struct Line<'a> {
    pub text: &'a str,
    pub position: FilePos,
}

pub struct LineSplitter<'a> {
    remaining: &'a str,
    line_no: u32,
    char_index: u32,
}

pub fn split_lines(s: &str) -> LineSplitter {
    assert!(s.len() < i32::MAX as usize);

    LineSplitter {
        remaining: s,
        line_no: 0,
        char_index: 0,
    }
}

impl<'a> Iterator for LineSplitter<'a> {
    type Item = Line<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.remaining.is_empty() {
            let (line, remaining) = match self.remaining.split_once('\n') {
                Some((line, remaining)) => (line, remaining),
                None => (self.remaining, ""),
            };
            self.line_no += 1;

            let line_len: u32 = line.len().try_into().unwrap();
            let line = line.trim();

            let pos = FilePos {
                line_number: self.line_no,
                line_char: line_len - u32::try_from(line.len()).unwrap(),
                char_index: self.char_index,
            };

            self.remaining = remaining;
            self.char_index += line_len + 1;

            Some(Line {
                text: line,
                position: pos,
            })
        } else {
            None
        }
    }
}
