//! Terrific Audio Driver APU loader
//!
//! CAUTION: This loader patches out the loader and the echo buffer clear code

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::audio_driver;
use crate::common_audio_data::CommonAudioData;
use crate::driver_constants::{addresses, LoaderDataType, AUDIO_RAM_SIZE};
use crate::errors::LoadSongError;
use crate::songs::SongData;

use std::ops::Range;

const SPC700_JMP_INSTRUCTION: u8 = 0x5f;
const SPC700_STOP_INSTRUCTION: u8 = 0xff;

pub struct ResetRegisters {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub psw: u8,
    pub sp: u8,
    /// S-DSP Echo Start Address register
    pub esa: u8,
    /// S-DSP Echo Delay register
    pub edl: u8,
}

pub trait ApuEmulator {
    fn apuram(&self) -> &[u8; AUDIO_RAM_SIZE];
    fn program_counter(&self) -> u16;

    fn apuram_mut(&mut self) -> &mut [u8; AUDIO_RAM_SIZE];
    fn write_smp_register(&mut self, addr: u8, value: u8);
    fn reset(&mut self, registers: ResetRegisters);
}

fn patch_out_code_block_with_jmp(
    apuram: &mut [u8; AUDIO_RAM_SIZE],
    block: Range<u16>,
    jump_addr: u16,
) {
    assert!(block.len() > 3);

    let jmp_range = usize::from(block.start)..usize::from(block.start + 3);
    let stop_range = usize::from(block.start + 3)..usize::from(block.end);

    let jmp_addr = jump_addr.to_le_bytes();
    apuram[jmp_range].copy_from_slice(&[SPC700_JMP_INSTRUCTION, jmp_addr[0], jmp_addr[1]]);

    apuram[stop_range].fill(SPC700_STOP_INSTRUCTION);
}

fn load_apu(
    apu: &mut impl ApuEmulator,
    common_audio_data: &CommonAudioData,
    song_data: &[u8],
    song_addr: u16,
    esa: u8,
    edl: u8,
    flags: LoaderDataType,
) -> Result<(), LoadSongError> {
    const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

    const _: () = assert!(
        audio_driver::AUDIO_DRIVER.len() + (addresses::DRIVER_CODE as usize)
            <= (addresses::COMMON_DATA as usize)
    );

    let song_end_addr = usize::from(song_addr) + song_data.len();
    let echo_addr = usize::from(esa) * 0x100;

    if song_addr < common_audio_data.min_song_data_addr() || song_end_addr > echo_addr {
        return Err(LoadSongError::InvalidSongAddress);
    }

    let spc_ram = apu.apuram_mut();

    let mut write_spc_ram = |addr: u16, data: &[u8]| {
        let addr = usize::from(addr);
        spc_ram[addr..addr + data.len()].copy_from_slice(data);
    };

    write_spc_ram(addresses::DRIVER_CODE, audio_driver::AUDIO_DRIVER);
    write_spc_ram(addresses::COMMON_DATA, common_audio_data.data());

    write_spc_ram(song_addr, song_data);

    write_spc_ram(addresses::SONG_PTR, &song_addr.to_le_bytes());

    // Clear echo buffer
    spc_ram[echo_addr..].fill(0);

    // Set the loader flags
    spc_ram[LOADER_DATA_TYPE_ADDR] = flags.driver_value();

    // Replace loader with a `STOP` instructions
    spc_ram[usize::from(addresses::LOADER)] = SPC700_STOP_INSTRUCTION;

    // Patch out the echo buffer clear code
    patch_out_code_block_with_jmp(
        spc_ram,
        addresses::MAIN_CLEAR_ECHO_BUFFER_START..addresses::MAIN_CLEAR_ECHO_BUFFER_END,
        addresses::MAIN_CLEAR_ECHO_BUFFER_END,
    );

    apu.reset(ResetRegisters {
        pc: addresses::DRIVER_CODE,
        a: 0,
        x: 0,
        y: 0,
        psw: 0,
        sp: 0xff,
        esa,
        edl,
    });

    Ok(())
}

pub fn load_cad_and_blank_song_to_apu(
    apu: &mut impl ApuEmulator,
    common_audio_data: &CommonAudioData,
    song_addr: u16,
    flags: LoaderDataType,
) -> Result<(), LoadSongError> {
    load_apu(
        apu,
        common_audio_data,
        audio_driver::BLANK_SONG,
        song_addr,
        0xff,
        0,
        flags,
    )
}

pub fn load_song_to_apu(
    apu: &mut impl ApuEmulator,
    common_audio_data: &CommonAudioData,
    song: &SongData,
    song_addr: u16,
    flags: LoaderDataType,
) -> Result<(), LoadSongError> {
    let echo_buffer = &song.metadata().echo_buffer;

    let min_song_end_addr = usize::from(common_audio_data.min_song_data_addr()) + song.data().len();
    let echo_buffer_size = echo_buffer.buffer_size();

    if min_song_end_addr + echo_buffer_size > AUDIO_RAM_SIZE {
        return Err(LoadSongError::TooMuchData {
            common: common_audio_data.data().len(),
            song: song.data().len(),
            echo: echo_buffer_size,
        });
    }

    load_apu(
        apu,
        common_audio_data,
        song.data(),
        song_addr,
        song.metadata().echo_buffer.esa_register(),
        song.metadata().echo_buffer.edl_register(),
        flags,
    )
}
