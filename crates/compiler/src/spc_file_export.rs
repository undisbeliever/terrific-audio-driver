//! .spc file export

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_driver;
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{
    COMMON_DATA_ADDR, DRIVER_CODE_ADDR, DRIVER_LOADER_ADDR, DRIVER_SONG_PTR_ADDR,
};
use crate::errors::ExportSpcFileError;
use crate::songs::SongData;

const SPC_FILE_SIZE: usize = 0x10200;

const AUDIO_RAM_SIZE: usize = 1 << 16;

pub const S_DSP_ESA_REGISTER: usize = 0x6D;
pub const S_DSP_EDL_REGISTER: usize = 0x7D;

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

const _: () = assert!(
    audio_driver::AUDIO_DRIVER.len() + (DRIVER_CODE_ADDR as usize) <= (COMMON_DATA_ADDR as usize)
);

pub fn export_spc_file(
    common_audio_data: &CommonAudioData,
    song_data: SongData,
) -> Result<Vec<u8>, ExportSpcFileError> {
    let common_audio_data = common_audio_data.data();
    let metadata = song_data.metadata();
    let song_data = song_data.data();

    let echo_edl = metadata.echo_buffer.edl;

    let song_data_addr =
        usize::from(COMMON_DATA_ADDR) + common_audio_data.len() + (common_audio_data.len() % 2);
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
        header[0x25..0x27].copy_from_slice(&DRIVER_CODE_ADDR.to_le_bytes());
        // SP
        header[0x2b] = 0xff;
        // Ignoring remaining registers

        // ID666 tags
        // ::TODO add more ID666 tags::
        // ::TODO add song length in seconds::
        // ::TODO parse date from `metadata`::

        // Title of song
        write_id666_tag(header, 0x2e, 0x20, metadata.title.as_deref());
        // Artist of song
        write_id666_tag(header, 0xb1, 0x20, metadata.author.as_deref());
    }

    // Audio-RAM contents
    {
        let spc_ram = &mut out[0x100..0x10100];
        let mut write_spc_ram = |addr: u16, data: &[u8]| {
            let addr = usize::from(addr);
            spc_ram[addr..addr + data.len()].copy_from_slice(data);
        };

        write_spc_ram(DRIVER_SONG_PTR_ADDR, &song_data_addr.to_le_bytes());
        write_spc_ram(DRIVER_CODE_ADDR, audio_driver::AUDIO_DRIVER);
        write_spc_ram(COMMON_DATA_ADDR, common_audio_data);
        write_spc_ram(song_data_addr, song_data);

        // Replace loader with a `STOP` instructions
        spc_ram[usize::from(DRIVER_LOADER_ADDR)] = 0xff;
    }

    // S-DSP registers
    {
        let dsp_registers = &mut out[0x10100..0x10180];

        // Setup the echo buffer
        dsp_registers[S_DSP_ESA_REGISTER] = echo_edl.esa_register();
        dsp_registers[S_DSP_EDL_REGISTER] = echo_edl.as_u8();
    }

    Ok(out)
}
