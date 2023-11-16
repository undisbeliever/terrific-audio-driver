//! MML tick count table

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{ChannelData, MmlData};

use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::time::TickCounter;

use std::cmp::{max, min};

const MIN_NAME_COLUMN_WIDTH: usize = 15;
const MAX_NAME_COLUMN_WIDTH: usize = 100;

#[derive(Default, Clone, Debug)]
pub struct ChannelTickCount {
    ticks: TickCounter,
    in_loop: bool,
}

#[derive(Debug)]
pub struct SectionTickCount {
    name: String,
    start_line: u32,
    end_line: u32,
    channels: [ChannelTickCount; N_MUSIC_CHANNELS],
}

#[derive(Debug)]
pub struct MmlTickCountTable {
    channel_names: Vec<String>,
    name_column_width: usize,
    sections: Vec<SectionTickCount>,

    loop_points: Option<[Option<TickCounter>; N_MUSIC_CHANNELS]>,
}

impl SectionTickCount {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn start_line(&self) -> u32 {
        self.start_line
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LineTickCounter {
    pub line_number: u32,
    pub ticks: TickCounter,
    pub in_loop: bool,
}

impl From<LineTickCounter> for ChannelTickCount {
    fn from(v: LineTickCounter) -> Self {
        Self {
            ticks: v.ticks,
            in_loop: v.in_loop,
        }
    }
}

fn loop_points(mml_channels: &[ChannelData]) -> Option<[Option<TickCounter>; N_MUSIC_CHANNELS]> {
    let mut loop_points: [Option<TickCounter>; N_MUSIC_CHANNELS] = Default::default();

    for (lp, c) in loop_points.iter_mut().zip(mml_channels) {
        *lp = c.loop_point().map(|l| l.tick_counter);
    }

    if loop_points.iter().any(|l| l.is_some()) {
        Some(loop_points)
    } else {
        None
    }
}

pub fn build_tick_count_table(mml_data: &MmlData) -> MmlTickCountTable {
    assert!(mml_data.channels().len() <= N_MUSIC_CHANNELS);

    let mml_channels = mml_data.channels();
    let mml_sections = mml_data.sections();

    let channel_names = mml_channels
        .iter()
        .map(|c| c.identifier().as_str().to_owned())
        .collect();

    let mut channels_end: [ChannelTickCount; N_MUSIC_CHANNELS] = Default::default();
    for (ci, channel) in mml_channels.iter().enumerate() {
        channels_end[ci] = ChannelTickCount {
            ticks: channel.tick_counter(),
            in_loop: channel.loop_point().is_some(),
        };
    }
    let channels_end = channels_end;

    let mut sections: Vec<SectionTickCount> = mml_sections
        .iter()
        .enumerate()
        .map(|(i, s)| SectionTickCount {
            name: s.name().to_owned(),
            start_line: s.line_number(),
            end_line: match mml_sections.get(i + 1) {
                Some(next_section) => next_section.line_number() - 1,
                None => u32::MAX,
            },
            channels: Default::default(),
        })
        .collect();

    for (ci, channel) in mml_channels.iter().enumerate() {
        // Assumes lines is sorted by line_number.
        let mut lines = channel.line_tick_counters();

        for s in sections.iter_mut() {
            let pp = lines.partition_point(|l| l.line_number <= s.end_line);

            if pp < lines.len() {
                s.channels[ci] = lines[pp].into();
                lines = &lines[pp..];
            } else {
                s.channels[ci] = channels_end[ci].clone();
                lines = &[];
            }
        }
    }

    if sections.is_empty() {
        sections.push(SectionTickCount {
            name: "MML".to_owned(),
            start_line: 0,
            end_line: u32::MAX,
            channels: channels_end,
        })
    }
    let sections = sections;

    let name_column_width = sections
        .iter()
        .map(|s| s.name.chars().count())
        .max()
        .unwrap_or(0);

    MmlTickCountTable {
        channel_names,
        name_column_width: max(MIN_NAME_COLUMN_WIDTH, name_column_width),
        sections,
        loop_points: loop_points(mml_channels),
    }
}

impl std::fmt::Display for MmlTickCountTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n_channels = self.channel_names.len();
        assert!(n_channels <= N_MUSIC_CHANNELS);

        let name_width = min(MAX_NAME_COLUMN_WIDTH, self.name_column_width);
        const TC_WIDTH: usize = 9;

        write!(f, "{:width$} |", "", width = name_width)?;
        for c_name in self.channel_names.iter().take(n_channels) {
            write!(f, " Channel {:<width$}|", c_name, width = TC_WIDTH - 7)?;
        }
        writeln!(f)?;

        for s in &self.sections {
            write!(f, "{:width$} |", s.name, width = name_width)?;
            for c in s.channels.iter().take(n_channels) {
                let lc = if c.in_loop { '+' } else { ' ' };

                write!(f, " {:>width$}{}|", c.ticks.value(), lc, width = TC_WIDTH)?;
            }
            writeln!(f)?;
        }

        if let Some(loop_points) = self.loop_points {
            write!(f, "{:width$} |", "Loop Point", width = name_width)?;
            for lp in loop_points.iter().take(n_channels) {
                if let Some(lp) = lp {
                    write!(f, " {:>width$} |", lp.value(), width = TC_WIDTH)?;
                } else {
                    write!(f, " {:width$} |", "", width = TC_WIDTH)?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
