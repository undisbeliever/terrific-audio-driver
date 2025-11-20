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

    pub(crate) fn to_range(self, byte_length: u32) -> FilePosRange {
        FilePosRange {
            line_number: self.line_number,
            line_char: self.line_char,
            index_start: self.char_index,
            index_end: self.char_index + byte_length,
        }
    }

    pub(crate) fn to_range_pos(self, p: &FilePos) -> FilePosRange {
        if p.char_index > self.char_index && p.line_number == self.line_number {
            FilePosRange {
                line_number: self.line_number,
                line_char: self.line_char,
                index_start: self.char_index,
                index_end: p.char_index,
            }
        } else {
            self.to_range(1)
        }
    }

    // ASSUMES: `s` is the prefix at this file position
    pub(crate) fn to_range_str_len(self, s: &str) -> FilePosRange {
        let s_len = s.len().try_into().unwrap();
        self.to_range(s_len)
    }

    fn advance(&self, char_count: u32, byte_length: usize) -> FilePos {
        FilePos {
            line_number: self.line_number,
            line_char: self.line_char + char_count,
            char_index: self.char_index + u32::try_from(byte_length).unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilePosRange {
    pub(crate) line_number: u32,
    pub(crate) line_char: u32,
    pub(crate) index_start: u32,
    pub(crate) index_end: u32,
}

impl FilePosRange {
    pub fn line_number(&self) -> u32 {
        self.line_number
    }
    pub fn line_char(&self) -> u32 {
        self.line_char
    }
    pub fn index_start(&self) -> u32 {
        self.index_start
    }
    pub fn index_end(&self) -> u32 {
        self.index_end
    }
}

pub fn blank_file_range() -> FilePosRange {
    FilePosRange {
        line_number: 0,
        line_char: 0,
        index_start: 0,
        index_end: 0,
    }
}

pub fn blank_file_pos() -> FilePos {
    FilePos {
        line_number: 0,
        line_char: 0,
        char_index: 0,
    }
}

#[derive(Clone)]
pub struct Line<'a> {
    pub text: &'a str,
    pub position: FilePos,
}

// Cannot `Copy` a `std::ops::Range<u32>`.
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub struct LineIndexRange {
    pub start: u32,
    pub end: u32,
}

impl Line<'_> {
    pub fn range(&self) -> FilePosRange {
        let line_length = self.text.len().try_into().unwrap();
        self.position.to_range(line_length)
    }

    pub fn end_pos(&self) -> u32 {
        let line_length: u32 = self.text.len().try_into().unwrap();
        self.position.char_index() + line_length
    }

    pub fn index_range(&self) -> LineIndexRange {
        let line_length: u32 = self.text.len().try_into().unwrap();
        let start = self.position.char_index;

        LineIndexRange {
            start,
            end: start + line_length,
        }
    }

    pub fn trim_start(self) -> Self {
        let pos = self.position;

        let mut char_count = 0;
        for (index, c) in self.text.char_indices() {
            if !c.is_whitespace() {
                let text = &self.text[index..];

                return Line {
                    text,
                    position: pos.advance(char_count, index),
                };
            }
            char_count += 1;
        }

        Line {
            text: "",
            position: FilePos {
                line_number: pos.line_number,
                line_char: pos.line_char + char_count,
                char_index: pos.char_index + u32::try_from(self.text.len()).unwrap(),
            },
        }
    }

    pub fn trim_comments(self, comment_char: char) -> Self {
        match self.text.split_once(comment_char) {
            Some((text, _comment)) => Self {
                text: text.trim_end(),
                ..self
            },
            None => self,
        }
    }

    pub fn split_once(&self) -> Option<(&str, Line<'_>)> {
        match self
            .text
            .char_indices()
            .enumerate()
            .find(|(_, (_, c))| c.is_ascii_whitespace())
        {
            Some((char_count, (index, _))) => {
                let char_count: u32 = char_count.try_into().unwrap();
                let (first, rem) = self.text.split_at(index);

                let l = Line {
                    text: rem,
                    position: self.position.advance(char_count, index),
                };
                let l = l.trim_start();

                Some((first, l))
            }
            None => None,
        }
    }
}

pub struct LineSplitter<'a> {
    remaining: &'a str,
    line_no: u32,
    char_index: u32,
}

pub fn blank_line_splitter() -> LineSplitter<'static> {
    LineSplitter {
        remaining: "",
        line_no: 0,
        char_index: 0,
    }
}

pub fn split_lines(s: &str) -> LineSplitter<'_> {
    assert!(s.len() < i32::MAX as usize);

    LineSplitter {
        remaining: s,
        line_no: 0,
        char_index: 0,
    }
}

impl LineSplitter<'_> {
    pub fn file_pos(&self) -> FilePos {
        FilePos {
            line_number: self.line_no,
            line_char: 0,
            char_index: self.char_index,
        }
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
