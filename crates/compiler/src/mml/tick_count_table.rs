//! MML tick count table

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::mml::note_tracking::{section_end_ticks, CursorTrackerGetter};
use crate::songs::{Channel, SongData};

const MIN_NAME_COLUMN_WIDTH: usize = 15;
const MAX_NAME_COLUMN_WIDTH: usize = 100;

pub struct MmlTickCountTable<'a>(pub &'a SongData);

impl std::fmt::Display for MmlTickCountTable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let channels: Vec<&Channel> = self
            .0
            .channels()
            .iter()
            .filter_map(|c| c.as_ref())
            .collect();
        let sections = self.0.sections();

        let cursor_tracker = match self.0.cursor_tracker() {
            Some(t) => t,
            None => return Ok(()),
        };

        assert!(channels.len() <= N_MUSIC_CHANNELS);

        let name_width = sections
            .iter()
            .map(|s| s.name.chars().count())
            .max()
            .unwrap_or(0)
            .clamp(MIN_NAME_COLUMN_WIDTH, MAX_NAME_COLUMN_WIDTH);

        let has_loop_point = channels.iter().any(|c| c.loop_point.is_some());

        const TC_WIDTH: usize = 9;

        write!(f, "{:width$} |", "", width = name_width)?;
        for c in &channels {
            let c_name = c.name;
            write!(f, " Channel {:<width$}|", c_name, width = TC_WIDTH - 7)?;
        }
        writeln!(f)?;

        if sections.is_empty() {
            write!(f, "{:width$} |", "MML", width = name_width)?;

            for c in &channels {
                let tc = c.tick_counter;
                write!(f, " {:>width$} |", tc.value(), width = TC_WIDTH)?;
            }
            writeln!(f)?;
        } else {
            for (i, s) in sections.iter().enumerate() {
                let target_char_index = sections.get(i + 1).map(|s| s.char_index);

                write!(f, "{:width$} |", s.name, width = name_width)?;

                for c in &channels {
                    // ::MAYDO optimise (this is O(mn) and could be made O(n))::
                    let (lc, ticks) = match target_char_index
                        .and_then(|t| section_end_ticks(cursor_tracker, c, t))
                    {
                        Some(t) => {
                            let lc = if t.in_loop { '+' } else { ' ' };
                            (lc, t.ticks)
                        }
                        None => (' ', c.tick_counter),
                    };

                    write!(f, " {:>width$}{}|", ticks.value(), lc, width = TC_WIDTH)?;
                }
                writeln!(f)?;
            }
        }

        if has_loop_point {
            write!(f, "{:width$} |", "Loop Point", width = name_width)?;
            for c in &channels {
                if let Some(lp) = c.loop_point {
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
