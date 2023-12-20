//! .spc file export

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use crate::audio_driver;
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{addresses, LoaderDataType};
use crate::errors::ExportSpcFileError;
use crate::mml::MetaData;
use crate::songs::SongData;

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
            let echo_edl = metadata.echo_buffer.edl;
            let rounded_up = d + echo_edl.to_duration() + Duration::from_millis(999);
            rounded_up.as_secs()
        }
        None => 0,
    }
}

const _: () = assert!(
    audio_driver::AUDIO_DRIVER.len() + (addresses::DRIVER_CODE as usize)
        <= (addresses::COMMON_DATA as usize)
);

pub fn export_spc_file(
    common_audio_data: &CommonAudioData,
    song_data: &SongData,
) -> Result<Vec<u8>, ExportSpcFileError> {
    let common_audio_data = common_audio_data.data();
    let metadata = song_data.metadata();
    let song_duration = song_data.duration();
    let song_data = song_data.data();

    let echo_edl = metadata.echo_buffer.edl;

    let song_data_addr = usize::from(addresses::COMMON_DATA)
        + common_audio_data.len()
        + (common_audio_data.len() % 2);
    let song_end_addr = song_data_addr + song_data.len();
    let echo_buffer_size = echo_edl.buffer_size();

    if song_end_addr + echo_buffer_size > AUDIO_RAM_SIZE {
        return Err(ExportSpcFileError::TooMuchData {
            common: common_audio_data.len(),
            song: song_data.len(),
            echo: echo_buffer_size,
        });
    }

    let song_data_addr = u16::try_from(song_data_addr).unwrap();

    // song_data_addr should be even, the loader will always transfer an even number of bytes.
    assert!(song_data_addr % 2 == 0);
    assert!(song_end_addr <= echo_edl.echo_buffer_addr().into());

    let mut out = vec![0; SPC_FILE_SIZE];

    // Header
    {
        let header = &mut out[0..0x100];

        header[0..33].copy_from_slice(b"SNES-SPC700 Sound File Data v0.30");
        header[0x21] = 26;
        header[0x22] = 26;
        header[0x23] = 26; // ID666 tag
        header[0x24] = 30; // file version

        // SPC700 Registers
        // PC
        header[0x25..0x27].copy_from_slice(&addresses::DRIVER_CODE.to_le_bytes());
        // SP
        header[0x2b] = 0xff;
        // Ignoring remaining registers

        // ID666 tags
        // ::TODO add more ID666 tags::
        // ::TODO parse date from `metadata`::

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

    // Audio-RAM contents
    {
        const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

        let spc_ram = &mut out[0x100..0x10100];
        let mut write_spc_ram = |addr: u16, data: &[u8]| {
            let addr = usize::from(addr);
            spc_ram[addr..addr + data.len()].copy_from_slice(data);
        };

        write_spc_ram(addresses::DRIVER_CODE, audio_driver::AUDIO_DRIVER);
        write_spc_ram(addresses::COMMON_DATA, common_audio_data);

        write_spc_ram(song_data_addr, song_data);

        write_spc_ram(addresses::SONG_PTR, &song_data_addr.to_le_bytes());

        // Set the loader flags
        spc_ram[LOADER_DATA_TYPE_ADDR] = LoaderDataType {
            stereo_flag: true,
            play_song: true,
            skip_echo_buffer_reset: true,
        }
        .driver_value();

        // Replace loader with a `STOP` instructions
        spc_ram[usize::from(addresses::LOADER)] = 0xff;
    }

    // S-DSP registers
    {
        let dsp_registers = &mut out[0x10100..0x10180];

        // Disable echo writes
        dsp_registers[S_DSP_FLG_REGISTER] = S_DSP_FLG_RESET;

        // Setup the echo buffer
        dsp_registers[S_DSP_ESA_REGISTER] = echo_edl.esa_register();
        dsp_registers[S_DSP_EDL_REGISTER] = echo_edl.as_u8();
    }

    Ok(out)
}
