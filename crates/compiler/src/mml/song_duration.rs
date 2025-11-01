//! Song duration calculations

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::MetaData;

use crate::driver_constants::N_MUSIC_CHANNELS;
use crate::songs::Channel;
use crate::subroutines::CompiledSubroutines;
use crate::time::{TickClock, TickCounter, TIMER_HZ};
use crate::UnsignedValueNewType;

use std::time::Duration;

// ::TODO move into Song::
pub fn calc_song_duration(
    metadata: &MetaData,
    channels: &[Option<Channel>; N_MUSIC_CHANNELS],
    subroutines: &CompiledSubroutines,
) -> Option<Duration> {
    let set_song_tick_in_subroutine = subroutines.iter_compiled().any(|s| s.changes_song_tempo);

    if set_song_tick_in_subroutine {
        return None;
    }

    let total_ticks: u32 = channels
        .iter()
        .filter_map(|c| c.as_ref())
        .map(|c| c.tick_counter.value())
        .max()
        .unwrap_or(0);

    let mut tempo_changes: Vec<(TickCounter, TickClock)> = channels
        .iter()
        .filter_map(|c| c.as_ref())
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
        out += u64::from(section_ticks) * u64::from(prev_clock.value());

        prev_ticks = ticks;
        prev_clock = clock;
    }

    let remaining_ticks = total_ticks - prev_ticks;
    out += u64::from(remaining_ticks) * u64::from(prev_clock.value());

    const _: () = assert!(1_000_000 % TIMER_HZ == 0);
    const MICRO_MUL: u64 = 1_000_000 / TIMER_HZ as u64;

    Some(Duration::from_micros(out * MICRO_MUL))
}
