//! MML Note Tracking

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use super::command_parser::State;
use super::ChannelId;

use crate::file_pos::{FilePos, LineIndexRange};
use crate::time::TickCounterWithLoopFlag;

use std::ops::{Range, RangeInclusive};

#[derive(Debug, Clone, PartialEq)]
pub struct Cursor {
    pub char_index: u32,
    pub ticks: TickCounterWithLoopFlag,
    pub state: State,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrackingLine {
    channel: ChannelId,
    char_indexes: RangeInclusive<u32>,
    data_range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CursorTracker {
    data: Vec<Cursor>,
    lines: Vec<TrackingLine>,
}

impl CursorTracker {
    pub(crate) fn new() -> Self {
        Self {
            data: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub(crate) fn new_line(
        &mut self,
        channel: ChannelId,
        line_range: LineIndexRange,
        ticks: TickCounterWithLoopFlag,
        state: State,
    ) {
        if let Some(l) = self.lines.last_mut() {
            l.data_range.end = self.data.len();
        }
        self.lines.push(TrackingLine {
            channel,
            char_indexes: line_range.start..=line_range.end,
            data_range: self.data.len()..self.data.len(),
        });

        self.data.push(Cursor {
            char_index: line_range.start,
            ticks,
            state,
        });
    }

    pub(crate) fn add(&mut self, p: &FilePos, ticks: TickCounterWithLoopFlag, state: State) {
        self.data.push(Cursor {
            char_index: p.char_index(),
            ticks,
            state,
        });
    }

    pub(crate) fn end_channel(&mut self) {
        if let Some(l) = self.lines.last_mut() {
            l.data_range.end = self.data.len();
        }
    }

    pub fn find(&self, char_index: u32) -> Option<(ChannelId, &Cursor)> {
        let line = self
            .lines
            .iter()
            .find(|l| l.char_indexes.contains(&char_index))?;
        let data = self.data.get(line.data_range.clone())?;

        let char_index = char_index + 1;
        let i = match data.binary_search_by_key(&char_index, |cur| cur.char_index) {
            Ok(i) => i,
            Err(i) => i,
        };
        let cursor = data.get(i.saturating_sub(1))?;

        Some((line.channel, cursor))
    }

    pub fn find_line_start(&self, char_index: u32) -> Option<(ChannelId, &Cursor)> {
        let line = self
            .lines
            .iter()
            .find(|l| l.char_indexes.contains(&char_index))?;
        let first = self.data.get(line.data_range.start)?;

        Some((line.channel, first))
    }
}
