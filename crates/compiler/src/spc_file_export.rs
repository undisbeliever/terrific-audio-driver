//! .spc file export

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{AudioMode, LoaderDataType};
use crate::errors::LoadSongError;
use crate::songs::{MetaData, SongData};
use crate::tad_apu::{load_song_to_apu, ApuEmulator};

const SPC_FILE_SIZE: usize = 0x10200;

const AUDIO_RAM_SIZE: usize = 1 << 16;

pub const MAX_SONG_LENGTH: u32 = 999;
pub const MAX_FADEOUT_MILLIS: u32 = 99999;

pub const DEFAULT_FADEOUT: u32 = 0;

pub const S_DSP_FLG_REGISTER: usize = 0x6C;
pub const S_DSP_ESA_REGISTER: usize = 0x6D;
pub const S_DSP_EDL_REGISTER: usize = 0x7D;

// Reset: soft reset, disable echo writes, mute all channels.
pub const S_DSP_FLG_RESET: u8 = 0b11100000;

fn write_id666_tag(out: &mut [u8], addr: usize, size: usize, s: Option<&str>) {
    // ::TODO check if .spc audio programs can read UTF-8 ::

    if let Some(s) = s {
        if s.is_ascii() {
            if s.len() <= size {
                out[addr..addr + s.len()].copy_from_slice(s.as_bytes());
            } else {
                out[addr..addr + size].copy_from_slice(&s.as_bytes()[0..size]);
            }
        }
    }
}

// Will write 0 if `value` cannot fit in `size`
fn write_id666_number_safe(out: &mut [u8], addr: usize, size: usize, value: u64) {
    let value = if value < 10_u64.saturating_pow(size.try_into().unwrap()) {
        value
    } else {
        0
    };

    let s = format!("{:>0width$}", value, width = size);
    out[addr..addr + size].copy_from_slice(s.as_bytes());
}

fn song_length(duration: Option<Duration>, metadata: &MetaData) -> u64 {
    if let Some(sl) = metadata.spc_song_length {
        return sl.into();
    }

    match duration {
        Some(d) => {
            let max_edl = metadata.echo_buffer.max_edl;
            let rounded_up = d + max_edl.to_duration() + Duration::from_millis(999);
            rounded_up.as_secs()
        }
        None => 0,
    }
}

struct SpcFile(Box<[u8; SPC_FILE_SIZE]>);

impl ApuEmulator for SpcFile {
    fn apuram_mut(&mut self) -> &mut [u8; AUDIO_RAM_SIZE] {
        (&mut self.0[0x100..0x10100]).try_into().unwrap()
    }

    fn reset(&mut self, registers: crate::tad_apu::ResetRegisters) {
        self.0[0x25..0x27].copy_from_slice(&registers.pc.to_le_bytes());
        self.0[0x27] = registers.a;
        self.0[0x28] = registers.x;
        self.0[0x29] = registers.y;
        self.0[0x2a] = registers.psw;
        self.0[0x2b] = registers.sp;

        // Disable echo writes
        self.0[0x10100 + S_DSP_FLG_REGISTER] = S_DSP_FLG_RESET;

        self.0[0x10100 + S_DSP_ESA_REGISTER] = registers.esa;
        self.0[0x10100 + S_DSP_EDL_REGISTER] = registers.edl;
    }

    // Unused functions

    fn apuram(&self) -> &[u8; AUDIO_RAM_SIZE] {
        unimplemented!()
    }

    fn program_counter(&self) -> u16 {
        unimplemented!()
    }

    fn write_smp_register(&mut self, _addr: u8, _value: u8) {
        unimplemented!()
    }
}

pub fn export_spc_file(
    common_audio_data: &CommonAudioData,
    song_data: &SongData,
) -> Result<Box<[u8]>, LoadSongError> {
    let mut spc_file = SpcFile(Box::new([0; SPC_FILE_SIZE]));

    // Update Header
    {
        let header = &mut spc_file.0[0..0x100];
        let metadata = song_data.metadata();
        let song_duration = song_data.duration();

        header[0..33].copy_from_slice(b"SNES-SPC700 Sound File Data v0.30");
        header[0x21] = 26;
        header[0x22] = 26;
        header[0x23] = 26; // ID666 tag
        header[0x24] = 30; // file version

        // SPC700 and DSP registers set in `load_song_to_apu` via `SpcFile::reset()`

        // Title of song
        write_id666_tag(header, 0x2e, 0x20, metadata.title.as_deref());
        // Game title
        write_id666_tag(header, 0x4e, 0x20, metadata.game.as_deref());
        // Artist of song
        write_id666_tag(header, 0xb1, 0x20, metadata.author.as_deref());

        // Song length in seconds
        let song_length = song_length(song_duration, metadata);
        write_id666_number_safe(header, 0xa9, 3, song_length);

        // Fadeout length in milliseconds
        let fadeout_length = metadata
            .spc_fadeout_millis
            .unwrap_or(DEFAULT_FADEOUT)
            .into();
        write_id666_number_safe(header, 0xac, 5, fadeout_length);
    }

    load_song_to_apu(
        &mut spc_file,
        common_audio_data,
        song_data,
        common_audio_data.min_song_data_addr(),
        LoaderDataType {
            play_song: true,
            audio_mode: AudioMode::Surround,
            reset_global_volumes: true,
        },
    )?;

    Ok(spc_file.0)
}
