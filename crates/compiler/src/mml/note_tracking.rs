//! MML Note Tracking

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use super::command_parser::State;
use super::ChannelId;

use crate::file_pos::{FilePos, LineIndexRange};
use crate::songs::Channel;
use crate::time::{TickCounter, TickCounterWithLoopFlag};

use std::ops::{Range, RangeInclusive};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct CommandTickTracker(Vec<CommandTickTrackerItem>);

impl CommandTickTracker {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn push(&mut self, cursor_pos: u32, ticks: TickCounterWithLoopFlag) {
        self.0.push(CommandTickTrackerItem { cursor_pos, ticks })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CommandTickTrackerItem {
    pub(crate) cursor_pos: u32,
    pub(crate) ticks: TickCounterWithLoopFlag,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cursor {
    pub char_index: u32,
    /// Tick offset after the previous command
    pub tick_offset: TickCounter,
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
        tick_offset: TickCounter,
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
            tick_offset,
            state,
        });
    }

    pub(crate) fn add(&mut self, p: &FilePos, tick_offset: TickCounter, state: State) {
        self.data.push(Cursor {
            char_index: p.char_index(),
            tick_offset,
            state,
        });
    }

    pub(crate) fn end_channel(&mut self) {
        if let Some(l) = self.lines.last_mut() {
            l.data_range.end = self.data.len();
        }
    }
}

pub trait CursorTrackerGetter {
    fn cursor_tracker(&self) -> Option<&CursorTracker>;
    fn tick_tracker_for_channel(&self, channel: ChannelId) -> Option<&CommandTickTracker>;
}

fn calc_ticks(
    channel_tracking: &CommandTickTracker,
    char_index: u32,
    cursor: &Cursor,
) -> Option<TickCounterWithLoopFlag> {
    let (command_pos, command_tick) = match channel_tracking
        .0
        .binary_search_by_key(&char_index, |t| t.cursor_pos)
    {
        Ok(0) | Err(0) => (0, Default::default()),
        Ok(i) | Err(i) => {
            let t = channel_tracking.0.get(i - 1)?;
            (t.cursor_pos, t.ticks)
        }
    };

    if command_pos < cursor.char_index {
        Some(TickCounterWithLoopFlag {
            ticks: command_tick.ticks + cursor.tick_offset,
            in_loop: command_tick.in_loop,
        })
    } else {
        Some(command_tick)
    }
}

pub fn find_cursor_state(
    compiled_data: &dyn CursorTrackerGetter,
    char_index: u32,
) -> Option<(ChannelId, TickCounterWithLoopFlag, &State)> {
    let tracker = compiled_data.cursor_tracker()?;

    let line = tracker
        .lines
        .iter()
        .find(|l| l.char_indexes.contains(&char_index))?;
    let cursor_data = tracker.data.get(line.data_range.clone())?;

    let char_index = char_index + 1;
    let i = match cursor_data.binary_search_by_key(&char_index, |cur| cur.char_index) {
        Ok(i) => i,
        Err(i) => i,
    };
    let cursor = cursor_data.get(i.saturating_sub(1))?;
    let channel_tracking = compiled_data.tick_tracker_for_channel(line.channel)?;
    let ticks = calc_ticks(channel_tracking, char_index, cursor)?;

    Some((line.channel, ticks, &cursor.state))
}

pub fn line_start_ticks(
    compiled_data: &dyn CursorTrackerGetter,
    char_index: u32,
) -> Option<(ChannelId, TickCounterWithLoopFlag)> {
    let tracker = compiled_data.cursor_tracker()?;

    let line = tracker
        .lines
        .iter()
        .find(|l| l.char_indexes.contains(&char_index))?;
    let line_start = tracker.data.get(line.data_range.start)?;

    let channel_tracking = compiled_data.tick_tracker_for_channel(line.channel)?;
    let ticks = calc_ticks(channel_tracking, line_start.char_index, line_start)?;

    Some((line.channel, ticks))
}

pub fn section_end_ticks(
    tracker: &CursorTracker,
    channel: &Channel,
    char_index: u32,
) -> Option<TickCounterWithLoopFlag> {
    let channel_id = ChannelId::Channel(channel.channel_index);

    let line = tracker
        .lines
        .iter()
        .find(|l| l.channel == channel_id && *l.char_indexes.start() >= char_index)?;
    let line_start = tracker.data.get(line.data_range.start)?;

    calc_ticks(&channel.tick_tracker, line_start.char_index, line_start)
}
