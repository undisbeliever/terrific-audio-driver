//! MML tick count table

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::bc_generator::LoopPoint;
use super::{Identifier, MmlData};

use crate::time::TickCounter;
use crate::{driver_constants::N_MUSIC_CHANNELS, time::TickCounterWithLoopFlag};

use std::cmp::{max, min};

const MIN_NAME_COLUMN_WIDTH: usize = 15;
const MAX_NAME_COLUMN_WIDTH: usize = 100;

// ::TODO move into SongData::
#[derive(Debug)]
pub struct MmlTickCountTableChannel {
    name: Identifier,
    sections: Vec<TickCounterWithLoopFlag>,
    ticks: TickCounter,
    loop_point: Option<LoopPoint>,
}

// ::TODO move into SongData::
#[derive(Debug)]
pub struct MmlTickCountTable {
    section_names: Vec<String>,
    name_column_width: usize,
    channels: [Option<MmlTickCountTableChannel>; N_MUSIC_CHANNELS],
}

pub fn build_tick_count_table(mml_data: &MmlData) -> MmlTickCountTable {
    assert!(mml_data.channels().len() <= N_MUSIC_CHANNELS);

    let mml_channels = mml_data.channels();
    let mml_sections = mml_data.sections();

    let mut channels: [Option<MmlTickCountTableChannel>; N_MUSIC_CHANNELS] = Default::default();
    for (ci, channel) in mml_channels.iter().enumerate() {
        channels[ci] = Some(MmlTickCountTableChannel {
            name: channel.identifier().clone(),
            sections: channel.section_tick_counters().to_vec(),
            ticks: channel.tick_counter(),
            loop_point: channel.loop_point(),
        });
    }

    let mut section_names: Vec<String> = mml_sections.iter().map(|s| s.name.clone()).collect();
    if section_names.is_empty() {
        section_names = vec!["MML".to_string()];
    }

    let name_column_width = section_names
        .iter()
        .map(|s| s.chars().count())
        .max()
        .unwrap_or(0);

    MmlTickCountTable {
        section_names,
        name_column_width: max(MIN_NAME_COLUMN_WIDTH, name_column_width),
        channels,
    }
}

impl std::fmt::Display for MmlTickCountTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n_channels = self.channels.iter().filter(|c| c.is_some()).count();

        let has_loop_point = self
            .channels
            .iter()
            .any(|c| c.as_ref().is_some_and(|c| c.loop_point.is_some()));

        assert!(n_channels <= N_MUSIC_CHANNELS);

        let name_width = min(MAX_NAME_COLUMN_WIDTH, self.name_column_width);
        const TC_WIDTH: usize = 9;

        write!(f, "{:width$} |", "", width = name_width)?;
        for c in self.channels.iter().filter_map(|c| c.as_ref()) {
            let c_name = c.name.as_str();
            write!(f, " Channel {:<width$}|", c_name, width = TC_WIDTH - 7)?;
        }
        writeln!(f)?;

        for (i, s_name) in self.section_names.iter().enumerate() {
            write!(f, "{:width$} |", s_name, width = name_width)?;
            for c in self.channels.iter().take(n_channels) {
                if let Some(c) = c.as_ref() {
                    let (lc, ticks) = if let Some(s) = c.sections.get(i) {
                        let lc = if s.in_loop { '+' } else { ' ' };
                        (lc, &s.ticks)
                    } else {
                        (' ', &c.ticks)
                    };
                    write!(f, " {:>width$}{}|", ticks.value(), lc, width = TC_WIDTH)?;
                }
            }
            writeln!(f)?;
        }

        if has_loop_point {
            write!(f, "{:width$} |", "Loop Point", width = name_width)?;
            for c in self.channels.iter().take(n_channels) {
                if let Some(lp) = c.as_ref().and_then(|c| c.loop_point) {
                    let tc = lp.tick_counter;
                    write!(f, " {:>width$} |", tc.value(), width = TC_WIDTH)?;
                } else {
                    write!(f, " {:width$} |", "", width = TC_WIDTH)?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
