//! TAD emulator

// SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::ops::{Deref, Range};

use compiler::bytecode_interpreter::SongInterpreter;
use compiler::common_audio_data::{ArcCadWithSfxBufferInAram, CommonAudioData};
use compiler::driver_constants::{
    addresses, io_commands, LoaderDataType, AUDIO_RAM_SIZE, FIRST_SFX_CHANNEL, IO_COMMAND_I_MASK,
    IO_COMMAND_MASK, N_CHANNELS, N_MUSIC_CHANNELS, N_SFX_CHANNELS,
};
use compiler::errors::LoadSongError;
use compiler::songs::SongData;
use compiler::sound_effects::CompiledSoundEffect;
use compiler::tad_apu::{self, ApuEmulator};
use compiler::Pan;

struct EmuWrapper(shvc_sound_emu::ShvcSoundEmu);

impl tad_apu::ApuEmulator for EmuWrapper {
    #[inline]
    fn apuram(&self) -> &[u8; compiler::driver_constants::AUDIO_RAM_SIZE] {
        self.0.apuram()
    }

    #[inline]
    fn program_counter(&self) -> u16 {
        self.0.program_counter()
    }

    fn reset(&mut self, r: tad_apu::ResetRegisters) {
        self.0.reset(shvc_sound_emu::ResetRegisters {
            pc: r.pc,
            a: r.a,
            x: r.x,
            y: r.y,
            psw: r.psw,
            sp: r.sp,
            esa: r.esa,
            edl: r.edl,
        })
    }

    #[inline]
    fn apuram_mut(&mut self) -> &mut [u8; compiler::driver_constants::AUDIO_RAM_SIZE] {
        self.0.apuram_mut()
    }

    #[inline]
    fn write_smp_register(&mut self, addr: u8, value: u8) {
        self.0.write_smp_register(addr, value)
    }
}

pub struct TadEmulator {
    emu: EmuWrapper,
    song_addr: Option<u16>,
    previous_command: u8,
}

impl TadEmulator {
    pub const AUDIO_BUFFER_SIZE: usize = shvc_sound_emu::ShvcSoundEmu::AUDIO_BUFFER_SIZE;
    pub const AUDIO_BUFFER_SAMPLES: usize = shvc_sound_emu::ShvcSoundEmu::AUDIO_BUFFER_SAMPLES;

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        // No IPL ROM
        let iplrom = [0; 64];

        Self {
            emu: EmuWrapper(shvc_sound_emu::ShvcSoundEmu::new(&iplrom)),
            song_addr: None,
            previous_command: 0,
        }
    }

    pub fn fill_apuram(&mut self, byte: u8) {
        self.emu.0.apuram_mut().fill(byte);
        self.song_addr = None;
    }

    pub fn unload_song(&mut self) {
        self.song_addr = None;
    }

    pub fn load_cad_and_blank_song(
        &mut self,
        cad: &CommonAudioData,
        song_addr: Option<u16>,
        stereo_flag: bool,
    ) -> Result<(), LoadSongError> {
        let song_addr = song_addr.unwrap_or_else(|| cad.min_song_data_addr());

        self.unload_song();

        tad_apu::load_cad_and_blank_song_to_apu(
            &mut self.emu,
            cad,
            song_addr,
            LoaderDataType {
                stereo_flag,
                play_song: true,
            },
        )?;

        self.song_addr = Some(song_addr);
        self.previous_command = 0;

        Ok(())
    }

    pub fn load_song(
        &mut self,
        cad: &CommonAudioData,
        song: &SongData,
        song_addr: Option<u16>,
        stereo_flag: bool,
    ) -> Result<(), LoadSongError> {
        let song_addr = song_addr.unwrap_or_else(|| cad.min_song_data_addr());

        self.unload_song();

        tad_apu::load_song_to_apu(
            &mut self.emu,
            cad,
            song,
            song_addr,
            LoaderDataType {
                stereo_flag,
                play_song: true,
            },
        )?;

        self.song_addr = Some(song_addr);
        self.previous_command = 0;

        Ok(())
    }

    pub fn load_song_skip<CAD, SD>(
        &mut self,
        cad: &CommonAudioData,
        song: &SongData,
        song_addr: u16,
        stereo_flag: bool,
        bc_interpreter: &Option<SongInterpreter<CAD, SD>>,
        music_channels_mask: u8,
    ) -> Result<(), LoadSongError>
    where
        CAD: Deref<Target = CommonAudioData>,
        SD: Deref<Target = SongData>,
    {
        self.unload_song();

        tad_apu::load_song_to_apu(
            &mut self.emu,
            cad,
            song,
            song_addr,
            LoaderDataType {
                stereo_flag,
                play_song: false,
            },
        )?;

        // Wait for the audio-driver to finish initialization
        while !addresses::MAIN_LOOP_CODE_RANGE.contains(&self.emu.program_counter()) {
            self.emu.0.emulate();
        }

        if let Some(bci) = &bc_interpreter {
            bci.write_to_emulator(&mut self.emu);
        }

        self.set_music_channels_mask(music_channels_mask);

        // Unpause the audio driver
        const _: () = assert!(io_commands::UNPAUSE != 0);
        self.emu.0.write_io_ports([io_commands::UNPAUSE, 0, 0, 0]);

        self.previous_command = io_commands::UNPAUSE;
        self.song_addr = Some(song_addr);

        Ok(())
    }

    pub fn set_music_channels_mask(&mut self, mask: u8) {
        let apuram = self.emu.apuram_mut();

        apuram[addresses::IO_MUSIC_CHANNELS_MASK as usize] = mask;
        // Keyoff muted channels
        apuram[addresses::KEYOFF_SHADOW_MUSIC as usize] |= mask ^ 0xff;
    }

    pub fn apuram(&self) -> &[u8; AUDIO_RAM_SIZE] {
        self.emu.0.apuram()
    }

    pub fn is_song_loaded(&self) -> bool {
        self.song_addr.is_some()
    }

    /// Panics if no song or CommonAudioData is loaded
    pub fn emulate(&mut self) -> &[i16; Self::AUDIO_BUFFER_SIZE] {
        assert!(self.is_song_loaded());

        self.emu.0.emulate()
    }

    pub fn is_io_command_acknowledged(&self) -> bool {
        self.is_song_loaded() && self.emu.0.read_io_ports()[0] == self.previous_command
    }

    pub fn try_send_io_command(&mut self, command: u8, param1: u8, param2: u8) -> bool {
        if self.is_io_command_acknowledged() {
            let command = ((self.previous_command ^ u8::MAX) & IO_COMMAND_I_MASK)
                | (command & IO_COMMAND_MASK);

            self.emu.0.write_io_ports([command, param1, param2, 0]);
            self.previous_command = command;

            true
        } else {
            false
        }
    }

    /// If this function returns false: Multiple `emulate() ; try_load_sfx_buffer_and_play_sfx()`
    /// calls might be needed to load and play the sound effect.
    ///
    /// Assumes cad is the common-audio-data loaded in memory
    #[must_use]
    pub fn try_load_sfx_buffer_and_play_sfx(
        &mut self,
        cad: &ArcCadWithSfxBufferInAram,
        sfx: &CompiledSoundEffect,
        pan: Pan,
    ) -> bool {
        const COMMON_DATA_ADDR_H: u8 = (addresses::COMMON_DATA >> 8) as u8;
        const SFX_SOA_INSTRUCTION_PTR_H: Range<usize> = Range {
            start: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + FIRST_SFX_CHANNEL,
            end: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + FIRST_SFX_CHANNEL + N_SFX_CHANNELS,
        };

        let apuram = self.emu.apuram_mut();

        let active_sfx_channels = apuram[SFX_SOA_INSTRUCTION_PTR_H]
            .iter()
            .any(|&pc_h| pc_h > COMMON_DATA_ADDR_H);

        if active_sfx_channels {
            // sfx_buffer can only only one sound effect at a time
            self.try_send_io_command(io_commands::STOP_SOUND_EFFECTS, 0, pan.as_u8());

            false
        } else {
            match cad.load_sfx(sfx, apuram) {
                Ok(()) => self.try_send_io_command(io_commands::PLAY_SOUND_EFFECT, 0, pan.as_u8()),
                Err(_) => false,
            }
        }
    }

    pub fn get_music_state(&self) -> Option<MusicState> {
        const ALL_CHANNELS_INSTRUCTION_PTR_H_RANGE: Range<usize> = Range {
            start: addresses::CHANNEL_INSTRUCTION_PTR_H as usize,
            end: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + N_CHANNELS,
        };
        const COMMON_DATA_ADDR_H: u8 = (addresses::COMMON_DATA >> 8) as u8;
        const STC: usize = addresses::SONG_TICK_COUNTER as usize;

        let song_addr = self.song_addr?;

        let apuram = self.emu.apuram();

        let any_channels_active = apuram[ALL_CHANNELS_INSTRUCTION_PTR_H_RANGE]
            .iter()
            .any(|&inst_ptr_h| inst_ptr_h > COMMON_DATA_ADDR_H);

        if !any_channels_active {
            return None;
        }

        let music_channels_mask = apuram[addresses::IO_MUSIC_CHANNELS_MASK as usize];

        let read_offsets = |addr_l: u16, addr_h: u16| -> [Option<u16>; N_MUSIC_CHANNELS] {
            std::array::from_fn(|i| {
                const _: () = assert!(N_MUSIC_CHANNELS <= 8);

                if music_channels_mask & (1 << i) != 0 {
                    let word = u16::from_le_bytes([
                        apuram[usize::from(addr_l) + i],
                        apuram[usize::from(addr_h) + i],
                    ]);
                    word.checked_sub(song_addr)
                } else {
                    None
                }
            })
        };

        Some(MusicState {
            song_tick_counter: u16::from_le_bytes(apuram[STC..STC + 2].try_into().unwrap()),
            voice_instruction_ptrs: read_offsets(
                addresses::CHANNEL_INSTRUCTION_PTR_L,
                addresses::CHANNEL_INSTRUCTION_PTR_H,
            ),
        })
    }
}

pub struct MusicState {
    pub song_tick_counter: u16,
    pub voice_instruction_ptrs: [Option<u16>; N_MUSIC_CHANNELS],
}
