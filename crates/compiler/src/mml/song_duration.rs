//! Song duration calculations

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::bc_generator::ChannelData;
use super::MetaData;

use crate::time::{TickClock, TickCounter, TIMER_HZ};

use std::time::Duration;

pub fn calc_song_duration(
    metadata: &MetaData,
    subroutines: &[ChannelData],
    channels: &[ChannelData],
) -> Option<Duration> {
    let set_song_tick_in_subroutine = subroutines.iter().any(|c| !c.tempo_changes.is_empty());

    if set_song_tick_in_subroutine {
        return None;
    }

    let total_ticks: u32 = channels
        .iter()
        .map(|c| c.tick_counter().value())
        .max()
        .unwrap_or(0);

    let mut tempo_changes: Vec<(TickCounter, TickClock)> = channels
        .iter()
        .flat_map(|c| &c.tempo_changes)
        .cloned()
        .collect();
    tempo_changes.sort_by_key(|(tc, _tempo)| tc.value());

    let mut out: u64 = 0;
    let mut prev_ticks = 0;
    let mut prev_clock = metadata.tick_clock;

    for (ticks, clock) in tempo_changes {
        let ticks = ticks.value();

        let section_ticks = ticks - prev_ticks;
        out += u64::from(section_ticks) * u64::from(prev_clock.as_u8());

        prev_ticks = ticks;
        prev_clock = clock;
    }

    let remaining_ticks = total_ticks - prev_ticks;
    out += u64::from(remaining_ticks) * u64::from(prev_clock.as_u8());

    const _: () = assert!(1_000_000 % TIMER_HZ == 0);
    const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

    Some(Duration::from_micros(out * MICRO_MUL))
}
