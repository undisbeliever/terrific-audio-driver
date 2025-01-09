//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use brr::{BrrSample, SAMPLES_PER_BLOCK};
use compiler::audio_driver;
use compiler::bytecode_interpreter;
use compiler::bytecode_interpreter::Emulator;
use compiler::bytecode_interpreter::SongInterpreter;
use compiler::common_audio_data::CommonAudioData;
use compiler::driver_constants::AUDIO_RAM_SIZE;
use compiler::driver_constants::N_CHANNELS;
use compiler::driver_constants::N_DSP_VOICES;
use compiler::driver_constants::N_MUSIC_CHANNELS;
use compiler::driver_constants::SONG_HEADER_ECHO_EDL;
use compiler::driver_constants::{
    addresses, io_commands, LoaderDataType, FIRST_SFX_CHANNEL, IO_COMMAND_I_MASK, IO_COMMAND_MASK,
    N_SFX_CHANNELS,
};
use compiler::mml::MmlPrefixData;
use compiler::songs::{blank_song, SongData};
use compiler::sound_effects::CompiledSoundEffect;
use compiler::time::TickCounter;
use compiler::Pan;

use sdl2::Sdl;
use shvc_sound_emu::ShvcSoundEmu;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};

use std::ops::Range;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::Duration;

use crate::compiler_thread::CommonAudioDataWithSfx;
use crate::compiler_thread::ItemId;
use crate::sfx_export_order::SfxId;
use crate::GuiMessage;

/// Sample rate to run the audio driver at
const APU_SAMPLE_RATE: i32 = 32040;

/// Sample rate to run BRR samples at
const BRR_SAMPLE_RATE: i32 = 32000;

/// Approximate number of samples to play a looping BRR sample for
const LOOPING_BRR_SAMPLE_SAMPLES: usize = 24000;

// Amount of Audio-RAM (in common-audio-data) to allocate to sound effects
pub const SFX_BUFFER_SIZE: usize = 128;

#[derive(Debug, Clone, Copy)]
pub struct MusicChannelsMask(pub u8);

impl MusicChannelsMask {
    pub const ALL: MusicChannelsMask = MusicChannelsMask(0xff);

    pub fn only_one_channel(channel_name: char) -> Self {
        const FIRST: u32 = 'A' as u32;
        const LAST: u32 = 'A' as u32 + N_DSP_VOICES as u32 - 1;

        let c = u32::from(channel_name);
        match c {
            FIRST..=LAST => {
                let shift = c - FIRST;
                MusicChannelsMask(1 << shift)
            }
            _ => MusicChannelsMask(0),
        }
    }

    pub fn only_one_channel_index(index: usize) -> Self {
        const LAST: usize = N_MUSIC_CHANNELS - 1;

        match index {
            0..=LAST => MusicChannelsMask(1 << index),
            _ => MusicChannelsMask(0),
        }
    }
}

enum SongSkip {
    None,
    Song(TickCounter),
    Subroutine(Option<MmlPrefixData>, u8, TickCounter),
}

#[derive(Debug)]
pub struct CommonAudioDataNoSfx(pub CommonAudioData);

#[derive(Debug)]
pub struct CommonAudioDataWithSfxBuffer(pub CommonAudioData);

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetStereoFlag(StereoFlag),

    // Stop audio and close the audio device
    StopAndClose,

    // Stops the audio if playing ItemId with
    CloseIfSongIdEquals(ItemId),

    Pause,
    PauseResume(ItemId),
    SetMusicChannels(ItemId, MusicChannelsMask),

    CommonAudioDataChanged(Option<Arc<CommonAudioDataNoSfx>>),
    CommonAudioDataSfxBufferChanged(Option<Arc<CommonAudioDataWithSfxBuffer>>),
    CommandAudioDataWithSfxChanged(Option<Arc<CommonAudioDataWithSfx>>),

    PlaySong(ItemId, Arc<SongData>, TickCounter, MusicChannelsMask),
    PlaySongSubroutine(
        ItemId,
        Arc<SongData>,
        Option<MmlPrefixData>,
        u8,
        TickCounter,
    ),
    PlaySoundEffectCommand(SfxId, Pan),
    PlaySongWithSfxBuffer(ItemId, Arc<SongData>, TickCounter),
    PlaySfxUsingSfxBuffer(Arc<CompiledSoundEffect>, Pan),
    PlaySample(CommonAudioData, Box<SongData>),

    PlayBrrSampleAt32Khz(Arc<BrrSample>),
}

#[derive(Debug, Copy, Clone)]
pub enum StereoFlag {
    Mono,
    Stereo,
}

/// A private token to ensure `AudioMessage::RingBufferConsumed` is only created by this module.
pub struct PrivateToken {
    #[allow(dead_code)]
    pub(self) private: (),
}

impl PrivateToken {
    pub(self) fn new() -> Self {
        Self { private: () }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AudioMonitorData {
    pub song_id: Option<ItemId>,
    pub voice_instruction_ptrs: [Option<u16>; N_MUSIC_CHANNELS],
    /// May not be valid.
    pub voice_return_inst_ptrs: [Option<u16>; N_MUSIC_CHANNELS],
}

impl AudioMonitorData {
    fn new(song_id: Option<ItemId>) -> Self {
        Self {
            song_id,
            voice_instruction_ptrs: Default::default(),
            voice_return_inst_ptrs: Default::default(),
        }
    }
}

#[derive(Clone)]
pub struct AudioMonitor {
    data: Arc<Mutex<Option<AudioMonitorData>>>,
}

impl AudioMonitor {
    fn new() -> Self {
        Self {
            data: Arc::new(Mutex::default()),
        }
    }

    fn set(&mut self, data: Option<AudioMonitorData>) {
        *self.data.lock().unwrap() = data;
    }

    pub fn get(&self) -> Option<AudioMonitorData> {
        match self.data.lock() {
            Ok(d) => d.clone(),
            Err(_) => None,
        }
    }
}

struct RingBuffer {
    sender: mpsc::Sender<AudioMessage>,

    buffer: [i16; Self::BUFFER_SIZE],
    read_cursor: usize,
    write_cursor: usize,
}

impl RingBuffer {
    const SDL_BUFFER_SAMPLES: usize = 2048;
    const EMU_BUFFER_SAMPLES: usize = ShvcSoundEmu::AUDIO_BUFFER_SAMPLES;
    const BUFFER_SAMPLES: usize = Self::SDL_BUFFER_SAMPLES + Self::EMU_BUFFER_SAMPLES * 2;

    const BUFFER_SIZE: usize = Self::BUFFER_SAMPLES * 2;
    const EMU_BUFFER_SIZE: usize = Self::EMU_BUFFER_SAMPLES * 2;

    fn new(sender: mpsc::Sender<AudioMessage>) -> Self {
        const _: () = assert!(RingBuffer::SDL_BUFFER_SAMPLES % RingBuffer::EMU_BUFFER_SAMPLES == 0);
        const _: () = assert!(
            RingBuffer::SDL_BUFFER_SAMPLES + RingBuffer::EMU_BUFFER_SAMPLES
                < RingBuffer::BUFFER_SAMPLES
        );

        Self {
            sender,
            buffer: [0; Self::BUFFER_SIZE],
            read_cursor: 0,
            write_cursor: Self::EMU_BUFFER_SIZE,
        }
    }

    /// Clears the buffer and reset the cursors.
    fn reset(&mut self) {
        self.buffer.fill(0);

        self.read_cursor = 0;
        self.write_cursor = Self::EMU_BUFFER_SIZE;
    }

    fn is_buffer_full(&self) -> bool {
        self.write_cursor / RingBuffer::EMU_BUFFER_SIZE
            == self.read_cursor / RingBuffer::EMU_BUFFER_SIZE
    }

    /// Returns true if the buffer is full
    fn add_chunk(&mut self, samples: &[i16; Self::EMU_BUFFER_SIZE]) -> bool {
        const _: () = assert!(RingBuffer::BUFFER_SIZE % RingBuffer::EMU_BUFFER_SIZE == 0);

        assert!(self.write_cursor % RingBuffer::EMU_BUFFER_SIZE == 0);

        let wc = self.write_cursor;
        let wc_end = wc + Self::EMU_BUFFER_SIZE;

        self.buffer[wc..wc_end].copy_from_slice(samples);

        self.write_cursor = if wc_end < self.buffer.len() {
            wc_end
        } else {
            0
        };

        self.is_buffer_full()
    }

    fn fill_remaining_with_silence(&mut self) {
        while !self.is_buffer_full() {
            assert!(self.write_cursor % RingBuffer::EMU_BUFFER_SIZE == 0);

            let wc = self.write_cursor;
            let wc_end = wc + Self::EMU_BUFFER_SIZE;

            self.buffer[wc..wc_end].fill(0);

            self.write_cursor = if wc_end < self.buffer.len() {
                wc_end
            } else {
                0
            };
        }
    }
}

impl AudioCallback for RingBuffer {
    type Channel = i16;

    fn callback(&mut self, out: &mut [i16]) {
        let rc = self.read_cursor;

        if rc + out.len() <= self.buffer.len() {
            let rc_end = rc + out.len();
            out.copy_from_slice(&self.buffer[rc..rc_end]);

            self.read_cursor = if rc_end < self.buffer.len() {
                rc_end
            } else {
                0
            }
        } else {
            let buffer1 = &self.buffer[rc..];
            let (out1, out2) = out.split_at_mut(buffer1.len());
            let buffer2 = &self.buffer[..out2.len()];

            out1.copy_from_slice(buffer1);
            out2.copy_from_slice(buffer2);

            self.read_cursor = buffer2.len();
        }

        let _ = self
            .sender
            .send(AudioMessage::RingBufferConsumed(PrivateToken::new()));
    }
}

// Returns true if any sound is output by the emulator
fn fill_ring_buffer_emu(emu: &mut TadEmu, playback: &mut AudioDevice<RingBuffer>) -> bool {
    // Do not emulate the next audio chunk if the ring buffer is full,
    // which can happen if an SDL audio callback occurs in the middle of the last `fill_ring_buffer()` call.
    //
    // In my opinion, the stuttering caused by this early return sounds better then skipping audio.
    if playback.lock().is_buffer_full() {
        return true;
    }

    let mut silence = true;

    loop {
        let emu_buffer = emu.emulate();
        if silence {
            silence &= emu_buffer.iter().all(|&b| b == 0);
        }
        let full = playback.lock().add_chunk(emu_buffer);
        if full {
            break;
        }
    }

    !silence
}

struct BrrSampleDecoder<'a> {
    sample: &'a BrrSample,
    sample_pos: usize,
    blocks_decoded: usize,
    blocks_to_decode: usize,
    prev1: i16,
    prev2: i16,
}

impl<'a> BrrSampleDecoder<'a> {
    fn new(sample: &'a BrrSample) -> Self {
        let blocks_to_decode = match sample.is_looping() {
            true => LOOPING_BRR_SAMPLE_SAMPLES / SAMPLES_PER_BLOCK,
            false => sample.n_brr_blocks(),
        };

        Self {
            sample,
            sample_pos: 0,
            blocks_to_decode,
            blocks_decoded: 0,
            prev1: 0,
            prev2: 0,
        }
    }

    fn is_finished(&self) -> bool {
        self.blocks_decoded > self.blocks_to_decode
    }

    fn fill_ring_buffer(&mut self, playback: &mut AudioDevice<RingBuffer>) {
        const BUF_LEN: usize = RingBuffer::EMU_BUFFER_SIZE;

        if playback.lock().is_buffer_full() {
            return;
        }

        let mut buf = [0; BUF_LEN];
        let mut is_done = self.is_finished();

        loop {
            if !is_done {
                self.sample_pos = self.sample.decode_into_buffer(
                    &mut buf,
                    self.sample_pos,
                    self.prev1,
                    self.prev2,
                );
                self.prev1 = buf[buf.len() - 1];
                self.prev2 = buf[buf.len() - 2];

                self.blocks_decoded += buf.len() / SAMPLES_PER_BLOCK;
                if self.blocks_decoded > self.blocks_to_decode {
                    // Fade out the audio
                    buf.iter_mut().enumerate().for_each(|(i, s)| {
                        let p = (BUF_LEN - i) as i32;
                        *s = i16::try_from(i32::from(*s) * p / (BUF_LEN as i32)).unwrap_or(0);
                    });
                    is_done = true;
                }
                let full = playback.lock().add_chunk(&buf);
                if full {
                    break;
                }
            } else {
                playback.lock().fill_remaining_with_silence();
                break;
            }
        }
    }
}

struct EmulatorWrapper<'a>(&'a mut ShvcSoundEmu);
impl bytecode_interpreter::Emulator for EmulatorWrapper<'_> {
    fn apuram_mut(&mut self) -> &mut [u8; 0x10000] {
        self.0.apuram_mut()
    }

    fn write_smp_register(&mut self, addr: u8, value: u8) {
        self.0.write_smp_register(addr, value);
    }

    fn program_counter(&self) -> u16 {
        self.0.program_counter()
    }
}

enum AudioDataState {
    NotLoaded,
    CommonDataOutOfDate, // Audio is still platying
    Sample(CommonAudioData, Box<SongData>),
    SongNoSfx(Arc<CommonAudioDataNoSfx>, Arc<SongData>),
    SongAndSfx(Arc<CommonAudioDataWithSfx>, Arc<SongData>),
    SongWithSfxBuffer(Arc<CommonAudioDataWithSfxBuffer>, Arc<SongData>),
}

enum SiCad {
    NoSfx(Arc<CommonAudioDataNoSfx>),
    SfxBuffer(Arc<CommonAudioDataWithSfxBuffer>),
    WithSfx(Arc<CommonAudioDataWithSfx>),
}

impl std::ops::Deref for SiCad {
    type Target = CommonAudioData;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::NoSfx(c) => &c.0,
            Self::WithSfx(c) => &c.common_audio_data,
            Self::SfxBuffer(c) => &c.0,
        }
    }
}

fn create_and_process_song_interpreter(
    audio_data: &AudioDataState,
    song_addr: u16,
    song_skip: SongSkip,
    stereo_flag: bool,
) -> Result<Option<SongInterpreter<SiCad, Arc<SongData>>>, ()> {
    let (cad, sd) = match audio_data {
        AudioDataState::NotLoaded => return Ok(None),
        AudioDataState::CommonDataOutOfDate => return Ok(None),
        AudioDataState::Sample(..) => return Ok(None),
        AudioDataState::SongNoSfx(cad, sd) => (SiCad::NoSfx(cad.clone()), sd.clone()),
        AudioDataState::SongAndSfx(cad, sd) => (SiCad::WithSfx(cad.clone()), sd.clone()),
        AudioDataState::SongWithSfxBuffer(cad, sd) => (SiCad::SfxBuffer(cad.clone()), sd.clone()),
    };

    match song_skip {
        SongSkip::None => Ok(Some(SongInterpreter::new(cad, sd, song_addr, stereo_flag))),
        SongSkip::Song(ticks) => {
            let mut si = SongInterpreter::new(cad, sd, song_addr, stereo_flag);
            if si.process_song_skip_ticks(ticks) {
                Ok(Some(si))
            } else {
                Ok(None)
            }
        }
        SongSkip::Subroutine(prefix, si, ticks) => {
            match SongInterpreter::new_song_subroutine(cad, sd, song_addr, prefix, si, stereo_flag)
            {
                Ok(mut si) => {
                    if si.process_song_skip_ticks(ticks) {
                        Ok(Some(si))
                    } else {
                        Ok(None)
                    }
                }
                Err(_) => Err(()),
            }
        }
    }
}

enum SfxQueue {
    None,
    TestSfx(Arc<CompiledSoundEffect>, Pan),
    PlaySfx(SfxId, Pan),
}

struct TadEmu {
    emu: ShvcSoundEmu,

    blank_song: Arc<SongData>,

    stereo_flag: StereoFlag,
    cad_no_sfx: Option<Arc<CommonAudioDataNoSfx>>,
    cad_with_sfx_buffer: Option<Arc<CommonAudioDataWithSfxBuffer>>,
    cad_with_sfx: Option<Arc<CommonAudioDataWithSfx>>,

    data_state: AudioDataState,
    song_id: Option<ItemId>,
    song_data_addr: Option<u16>,
    bc_interpreter: Option<SongInterpreter<SiCad, Arc<SongData>>>,

    previous_command: u8,
    sfx_queue: SfxQueue,
}

impl TadEmu {
    fn new() -> Self {
        // No IPL ROM
        let iplrom = [0; 64];

        Self {
            emu: ShvcSoundEmu::new(&iplrom),
            blank_song: Arc::new(blank_song()),
            stereo_flag: StereoFlag::Stereo,
            cad_no_sfx: None,
            cad_with_sfx_buffer: None,
            cad_with_sfx: None,
            data_state: AudioDataState::NotLoaded,
            bc_interpreter: None,
            song_id: None,
            song_data_addr: None,
            previous_command: 0,
            sfx_queue: SfxQueue::None,
        }
    }

    fn song_loaded(&self) -> bool {
        match self.data_state {
            AudioDataState::NotLoaded => false,

            AudioDataState::CommonDataOutOfDate
            | AudioDataState::Sample(..)
            | AudioDataState::SongNoSfx(..)
            | AudioDataState::SongAndSfx(..)
            | AudioDataState::SongWithSfxBuffer(..) => true,
        }
    }

    fn song_id(&self) -> Option<ItemId> {
        self.song_id
    }

    fn set_stereo_flag(&mut self, stereo_flag: StereoFlag) {
        self.stereo_flag = stereo_flag;
    }

    fn load_cad_no_sfx(&mut self, cad: Option<Arc<CommonAudioDataNoSfx>>) {
        self.cad_no_sfx = cad;
        if matches!(self.data_state, AudioDataState::SongWithSfxBuffer(..)) {
            self.data_state = AudioDataState::CommonDataOutOfDate;
        }
    }

    fn load_cad_with_sfx_buffer(&mut self, cad: Option<Arc<CommonAudioDataWithSfxBuffer>>) {
        self.cad_with_sfx_buffer = cad;
        if matches!(self.data_state, AudioDataState::SongWithSfxBuffer(..)) {
            self.data_state = AudioDataState::CommonDataOutOfDate;
        }
    }

    fn load_cad_with_sfx(&mut self, cad: Option<Arc<CommonAudioDataWithSfx>>) {
        self.cad_with_sfx = cad;
        if matches!(self.data_state, AudioDataState::SongAndSfx(..)) {
            self.data_state = AudioDataState::CommonDataOutOfDate;
        }
    }

    fn stop_song(&mut self) {
        self.data_state = AudioDataState::NotLoaded;
        self.song_id = None;
    }

    fn load_song(
        &mut self,
        song_id: ItemId,
        song: Arc<SongData>,
        skip: TickCounter,
        music_channels_mask: MusicChannelsMask,
    ) -> Result<(), ()> {
        let data = match (
            &self.cad_with_sfx,
            &self.cad_with_sfx_buffer,
            &self.cad_no_sfx,
        ) {
            (Some(c), _, _) => AudioDataState::SongAndSfx(c.clone(), song),
            (None, Some(c), _) => AudioDataState::SongWithSfxBuffer(c.clone(), song),
            (None, None, Some(c)) => AudioDataState::SongNoSfx(c.clone(), song),
            (None, None, None) => return Err(()),
        };
        self._load_song_into_memory(
            Some(song_id),
            data,
            SongSkip::Song(skip),
            music_channels_mask,
        )
    }

    fn load_song_subroutine(
        &mut self,
        song_id: ItemId,
        song: Arc<SongData>,
        prefix: Option<MmlPrefixData>,
        subroutine_index: u8,
        skip: TickCounter,
    ) -> Result<(), ()> {
        let data = match (&self.cad_with_sfx, &self.cad_with_sfx_buffer) {
            (Some(c), _) => AudioDataState::SongAndSfx(c.clone(), song),
            (None, Some(c)) => AudioDataState::SongWithSfxBuffer(c.clone(), song),
            (None, None) => return Err(()),
        };
        self._load_song_into_memory(
            Some(song_id),
            data,
            SongSkip::Subroutine(prefix, subroutine_index, skip),
            MusicChannelsMask::ALL,
        )
    }

    fn load_blank_song(&mut self) -> Result<(), ()> {
        let blank_song = &self.blank_song;
        let data = match (&self.cad_with_sfx, &self.cad_with_sfx_buffer) {
            (Some(c), _) => AudioDataState::SongAndSfx(c.clone(), blank_song.clone()),
            (None, Some(c)) => AudioDataState::SongWithSfxBuffer(c.clone(), blank_song.clone()),
            (None, None) => return Err(()),
        };
        self._load_song_into_memory(None, data, SongSkip::None, MusicChannelsMask::ALL)
    }

    fn load_song_with_sfx_buffer(
        &mut self,
        song_id: ItemId,
        song: Arc<SongData>,
        skip: TickCounter,
    ) -> Result<(), ()> {
        let data = match &self.cad_with_sfx_buffer {
            Some(c) => AudioDataState::SongWithSfxBuffer(c.clone(), song),
            None => return Err(()),
        };
        self._load_song_into_memory(
            Some(song_id),
            data,
            SongSkip::Song(skip),
            MusicChannelsMask::ALL,
        )
    }

    fn load_blank_song_with_sfx_buffer(&mut self) -> Result<(), ()> {
        let data = match &self.cad_with_sfx_buffer {
            Some(c) => AudioDataState::SongWithSfxBuffer(c.clone(), self.blank_song.clone()),
            None => return Err(()),
        };
        self._load_song_into_memory(None, data, SongSkip::None, MusicChannelsMask::ALL)
    }

    fn play_sample(
        &mut self,
        common_audio_data: CommonAudioData,
        song_data: Box<SongData>,
    ) -> Result<(), ()> {
        self._load_song_into_memory(
            None,
            AudioDataState::Sample(common_audio_data, song_data),
            SongSkip::None,
            MusicChannelsMask::ALL,
        )
    }

    fn _load_song_into_memory(
        &mut self,
        song_id: Option<ItemId>,
        data_state: AudioDataState,
        song_skip: SongSkip,
        music_channels_mask: MusicChannelsMask,
    ) -> Result<(), ()> {
        const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

        self.data_state = AudioDataState::NotLoaded;
        self.song_id = None;
        self.song_data_addr = None;
        self.bc_interpreter = None;

        self.sfx_queue = SfxQueue::None;

        let (common_audio_data, song) = match &data_state {
            AudioDataState::NotLoaded => return Err(()),
            AudioDataState::CommonDataOutOfDate => return Err(()),
            AudioDataState::Sample(cad, sd) => (cad, sd.as_ref()),
            AudioDataState::SongNoSfx(cad, sd) => (&cad.0, sd.as_ref()),
            AudioDataState::SongAndSfx(cad, sd) => (&cad.common_audio_data, sd.as_ref()),
            AudioDataState::SongWithSfxBuffer(cad, sd) => (&cad.0, sd.as_ref()),
        };

        let song_data = song.data();
        let common_data = common_audio_data.data();
        let echo_buffer = &song.metadata().echo_buffer;

        let stereo_flag = match self.stereo_flag {
            StereoFlag::Stereo => true,
            StereoFlag::Mono => false,
        };

        // Store song data at the end of the audio-RAM
        let song_size = song.song_aram_size().total_size();
        let song_data_addr = match AUDIO_RAM_SIZE.checked_sub(song_size) {
            Some(s) => s.try_into().unwrap(),
            None => return Err(()),
        };
        if song_data_addr < common_audio_data.min_song_data_addr() {
            return Err(());
        }

        self.bc_interpreter = create_and_process_song_interpreter(
            &data_state,
            song_data_addr,
            song_skip,
            stereo_flag,
        )?;

        let apuram = self.emu.apuram_mut();

        let mut write_spc_ram = |addr: u16, data: &[u8]| {
            let addr = usize::from(addr);
            apuram[addr..addr + data.len()].copy_from_slice(data);
        };

        // Load driver
        write_spc_ram(addresses::LOADER, audio_driver::LOADER);
        write_spc_ram(addresses::DRIVER_CODE, audio_driver::AUDIO_DRIVER);

        write_spc_ram(addresses::COMMON_DATA, common_data);
        write_spc_ram(addresses::SONG_PTR, &song_data_addr.to_le_bytes());
        write_spc_ram(song_data_addr, song_data);

        // Reset echo buffer
        let eb_start = usize::from(echo_buffer.buffer_addr());
        let eb_end = eb_start + echo_buffer.buffer_size();
        apuram[eb_start..eb_end].fill(0);

        // Set loader flags
        apuram[LOADER_DATA_TYPE_ADDR] = LoaderDataType {
            stereo_flag,
            play_song: false,
            skip_echo_buffer_reset: true,
        }
        .driver_value();

        // The echo buffer registers must be setup BEFORE the emulator processes instructions.
        // Otherwise the audio sounds weird.
        let (esa, edl) = match &self.bc_interpreter {
            Some(bci) => {
                apuram[usize::from(song_data_addr) + SONG_HEADER_ECHO_EDL] = bci.song_header_edl();

                (bci.esa_register(), bci.edl_register())
            }
            None => (echo_buffer.esa_register(), echo_buffer.edl_register()),
        };

        self.emu.reset(shvc_sound_emu::ResetRegisters {
            pc: addresses::DRIVER_CODE,
            a: 0,
            x: 0,
            y: 0,
            psw: 0,
            sp: 0xff,
            esa,
            edl,
        });

        // Wait for the audio-driver to finish initialization
        let mut emu_wrapper = EmulatorWrapper(&mut self.emu);
        while !emu_wrapper.is_pc_in_mainloop() {
            emu_wrapper.0.emulate();
        }

        if let Some(bci) = &self.bc_interpreter {
            bci.write_to_emulator(&mut emu_wrapper);
        }

        self.set_music_channels_mask(music_channels_mask);

        // Unpause the audio driver
        self.emu.write_io_ports([io_commands::UNPAUSE, 0, 0, 0]);

        self.previous_command = io_commands::UNPAUSE;

        self.data_state = data_state;
        self.song_id = song_id;
        self.song_data_addr = Some(song_data_addr);

        Ok(())
    }

    fn is_io_command_acknowledged(&self) -> bool {
        self.emu.read_io_ports()[0] == self.previous_command
    }

    fn try_send_io_command(&mut self, command: u8, param1: u8, param2: u8) {
        if self.is_io_command_acknowledged() {
            let command = ((self.previous_command ^ u8::MAX) & IO_COMMAND_I_MASK)
                | (command & IO_COMMAND_MASK);

            self.emu.write_io_ports([command, param1, param2, 0]);
            self.previous_command = command;
        }
    }

    fn set_music_channels_mask(&mut self, mask: MusicChannelsMask) {
        let apuram = self.emu.apuram_mut();

        apuram[addresses::IO_MUSIC_CHANNELS_MASK as usize] = mask.0;
        // Keyoff muted channels
        apuram[addresses::KEYOFF_SHADOW_MUSIC as usize] |= mask.0 ^ 0xff;
    }

    fn queue_sound_effect(&mut self, sfx_id: SfxId, pan: Pan) {
        if matches!(self.data_state, AudioDataState::SongAndSfx(..)) {
            self.sfx_queue = SfxQueue::PlaySfx(sfx_id, pan);
        } else {
            let r = self.load_blank_song().is_ok();
            if r {
                self.sfx_queue = SfxQueue::PlaySfx(sfx_id, pan);
            }
        }
    }

    fn queue_test_sfx(&mut self, sfx: Arc<CompiledSoundEffect>, pan: Pan) {
        if matches!(self.data_state, AudioDataState::SongWithSfxBuffer(..)) {
            self.sfx_queue = SfxQueue::TestSfx(sfx, pan);
        } else {
            let r = self.load_blank_song_with_sfx_buffer().is_ok();
            if r {
                self.sfx_queue = SfxQueue::TestSfx(sfx, pan);
            }
        }
    }

    fn process_sfx_queue(&mut self) {
        const COMMON_DATA_ADDR_H: u8 = (addresses::COMMON_DATA >> 8) as u8;
        const SFX_SOA_INSTRUCTION_PTR_H: Range<usize> = Range {
            start: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + FIRST_SFX_CHANNEL,
            end: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + FIRST_SFX_CHANNEL + N_SFX_CHANNELS,
        };

        match &self.sfx_queue {
            SfxQueue::None => (),
            SfxQueue::PlaySfx(sfx_id, pan) => {
                if self.is_io_command_acknowledged() {
                    self.try_send_io_command(
                        io_commands::PLAY_SOUND_EFFECT,
                        sfx_id.value(),
                        pan.as_u8(),
                    );
                    self.sfx_queue = SfxQueue::None;
                }
            }
            SfxQueue::TestSfx(sfx, pan) => {
                let common_data = match &self.data_state {
                    AudioDataState::SongWithSfxBuffer(cad, _) => cad.as_ref(),
                    _ => {
                        self.sfx_queue = SfxQueue::None;
                        return;
                    }
                };

                if !self.is_io_command_acknowledged() {
                    return;
                }

                if sfx.bytecode().len() >= SFX_BUFFER_SIZE {
                    self.sfx_queue = SfxQueue::None;
                    return;
                }

                let apuram = self.emu.apuram_mut();

                let active_sfx_channels = apuram[SFX_SOA_INSTRUCTION_PTR_H]
                    .iter()
                    .any(|&pc_h| pc_h > COMMON_DATA_ADDR_H);

                if active_sfx_channels {
                    // sfx_buffer can only onld one sound effect at a time
                    self.try_send_io_command(io_commands::STOP_SOUND_EFFECTS, 0, pan.as_u8());
                } else {
                    let bc_addr_range = common_data.0.sfx_bytecode_addr_range();
                    let bc_addr_range = bc_addr_range.start.into()..bc_addr_range.end.into();

                    let bc = sfx.bytecode();
                    let sfx_buffer = &mut apuram[bc_addr_range];
                    sfx_buffer[..bc.len()].copy_from_slice(bc);

                    self.try_send_io_command(io_commands::PLAY_SOUND_EFFECT, 0, pan.as_u8());
                    self.sfx_queue = SfxQueue::None;
                }
            }
        };
    }

    fn emulate(&mut self) -> &[i16; RingBuffer::EMU_BUFFER_SIZE] {
        const EMPTY_BUFFER: [i16; RingBuffer::EMU_BUFFER_SIZE] = [0; RingBuffer::EMU_BUFFER_SIZE];

        if !self.song_loaded() {
            return &EMPTY_BUFFER;
        }

        self.process_sfx_queue();

        self.emu.emulate()
    }

    /// Returns None if the song and sound effects have finished
    fn read_voice_positions(&mut self) -> Option<AudioMonitorData> {
        const ALL_CHANNELS_INSTRUCTION_PTR_H_RANGE: Range<usize> = Range {
            start: addresses::CHANNEL_INSTRUCTION_PTR_H as usize,
            end: addresses::CHANNEL_INSTRUCTION_PTR_H as usize + N_CHANNELS,
        };
        const COMMON_DATA_ADDR_H: u8 = (addresses::COMMON_DATA >> 8) as u8;

        let song_addr = self.song_data_addr?;

        let apuram = self.emu.apuram();

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

        let voice_instruction_ptrs = read_offsets(
            addresses::CHANNEL_INSTRUCTION_PTR_L,
            addresses::CHANNEL_INSTRUCTION_PTR_H,
        );

        let voice_return_inst_ptrs = match &mut self.bc_interpreter {
            Some(b) => {
                const STC: usize = addresses::SONG_TICK_COUNTER as usize;

                // Assumes number of ticks since the last read was < 256;
                let bc_tick_counter_l = b.tick_counter().value().to_le_bytes()[0];
                let emu_tick_counter_l = apuram[STC];

                let ticks_passed = emu_tick_counter_l.wrapping_sub(bc_tick_counter_l);
                if ticks_passed > 0 {
                    let v = b.process_ticks(TickCounter::new(ticks_passed.into()));
                    debug_assert!(v, "Bytecode interpreter timeout");
                }
                debug_assert_eq!(
                    b.tick_counter().value() as u16,
                    u16::from_le_bytes(apuram[STC..STC + 2].try_into().unwrap()),
                    "Bytecode interpreter desync"
                );

                let channels = b.channels();

                std::array::from_fn(|i| channels[i].as_ref().and_then(|c| c.topmost_return_pos))
            }
            None => Default::default(),
        };

        let any_channels_active = apuram[ALL_CHANNELS_INSTRUCTION_PTR_H_RANGE]
            .iter()
            .any(|&inst_ptr_h| inst_ptr_h > COMMON_DATA_ADDR_H);

        if any_channels_active {
            Some(AudioMonitorData {
                song_id: self.song_id,
                voice_instruction_ptrs,
                voice_return_inst_ptrs,
            })
        } else {
            None
        }
    }
}

enum PlayState {
    Paused,
    Running,
    PauseRequested,
    Pausing,
    SongFinished,
}

impl PlayState {
    fn timeout_until_close(&self) -> Duration {
        match self {
            Self::Paused => Duration::from_secs(2 * 60),
            _ => Duration::from_secs(5),
        }
    }
}

struct AudioThread {
    sender: mpsc::Sender<AudioMessage>,
    rx: mpsc::Receiver<AudioMessage>,

    gui_sender: fltk::app::Sender<GuiMessage>,
    monitor: AudioMonitor,

    sdl_context: Sdl,

    tad: TadEmu,
}

impl AudioThread {
    fn new(
        sender: mpsc::Sender<AudioMessage>,
        rx: mpsc::Receiver<AudioMessage>,
        gui_sender: fltk::app::Sender<GuiMessage>,
        monitor: AudioMonitor,
    ) -> Self {
        Self {
            sender,
            rx,
            gui_sender,
            monitor,
            sdl_context: sdl2::init().unwrap(),

            tad: TadEmu::new(),
        }
    }

    fn run(&mut self) {
        self.monitor.set(None);

        while let Ok(msg) = self.rx.recv() {
            let mut msg = Some(msg);
            while let Some(m) = msg {
                msg = self.process_message_no_audio(m);
            }
        }
    }

    fn send_started_song_message(&mut self, id: ItemId, song_data: Arc<SongData>) {
        // Must set the monitor data, audio timer stops when monitor data is None
        self.monitor.set(Some(AudioMonitorData::new(Some(id))));

        self.gui_sender
            .send(GuiMessage::AudioThreadStartedSong(id, song_data));
    }

    fn send_resume_song_message(&mut self, id: ItemId) {
        // Must set the monitor data, audio timer stops when monitor data is None
        self.monitor.set(Some(AudioMonitorData::new(Some(id))));

        self.gui_sender.send(GuiMessage::AudioThreadResumedSong(id));
    }

    // Process message while audio is not playing
    //
    // Returns Some if an AudioMessage could not be processed when playing a song or brr sample.
    #[must_use]
    fn process_message_no_audio(&mut self, msg: AudioMessage) -> Option<AudioMessage> {
        self.monitor.set(None);

        match msg {
            AudioMessage::CommonAudioDataChanged(data) => {
                self.tad.load_cad_no_sfx(data);
            }
            AudioMessage::CommonAudioDataSfxBufferChanged(data) => {
                self.tad.load_cad_with_sfx_buffer(data);
            }
            AudioMessage::CommandAudioDataWithSfxChanged(d) => {
                self.tad.load_cad_with_sfx(d);
            }
            AudioMessage::SetStereoFlag(sf) => {
                self.tad.set_stereo_flag(sf);

                // Disable unpause
                self.tad.stop_song();
            }

            AudioMessage::PlaySong(song_id, song, song_skip, channels_mask) => {
                if self
                    .tad
                    .load_song(song_id, song.clone(), song_skip, channels_mask)
                    .is_ok()
                {
                    self.send_started_song_message(song_id, song);
                    return self.play_song();
                }
            }
            AudioMessage::PlaySongSubroutine(song_id, song, prefix, si, skip) => {
                if self
                    .tad
                    .load_song_subroutine(song_id, song.clone(), prefix, si, skip)
                    .is_ok()
                {
                    self.send_started_song_message(song_id, song);
                    return self.play_song();
                }
            }
            AudioMessage::PlaySoundEffectCommand(id, pan) => {
                if self.tad.load_blank_song().is_ok() {
                    self.tad.queue_sound_effect(id, pan);
                    return self.play_song();
                }
            }
            AudioMessage::PlaySongWithSfxBuffer(song_id, song, song_skip) => {
                if self
                    .tad
                    .load_song_with_sfx_buffer(song_id, song.clone(), song_skip)
                    .is_ok()
                {
                    self.send_started_song_message(song_id, song);
                    return self.play_song();
                }
            }
            AudioMessage::PlaySfxUsingSfxBuffer(sfx_data, pan) => {
                if self.tad.load_blank_song_with_sfx_buffer().is_ok() {
                    self.tad.queue_test_sfx(sfx_data, pan);
                    return self.play_song();
                }
            }
            AudioMessage::PlaySample(common_data, song_data) => {
                if self.tad.play_sample(common_data, song_data).is_ok() {
                    return self.play_song();
                }
            }

            AudioMessage::PlayBrrSampleAt32Khz(brr_sample) => {
                return self.play_brr_sample(&brr_sample);
            }

            AudioMessage::PauseResume(id) => {
                if Some(id) == self.tad.song_id() {
                    return self.play_song();
                }
            }
            AudioMessage::SetMusicChannels(id, mask) => {
                if Some(id) == self.tad.song_id() {
                    self.tad.set_music_channels_mask(mask);
                }
            }

            AudioMessage::StopAndClose
            | AudioMessage::CloseIfSongIdEquals(_)
            | AudioMessage::Pause
            | AudioMessage::RingBufferConsumed(_) => (),
        }

        None
    }

    // Returns Some if the AudioMessage could not be processed
    #[must_use]
    fn play_song(&mut self) -> Option<AudioMessage> {
        if !self.tad.song_loaded() {
            return None;
        }

        let audio_subsystem = self.sdl_context.audio().unwrap();
        let desired_spec = AudioSpecDesired {
            freq: Some(APU_SAMPLE_RATE),
            channels: Some(2),
            samples: Some(RingBuffer::SDL_BUFFER_SAMPLES.try_into().unwrap()),
        };

        let mut playback = audio_subsystem
            .open_playback(None, &desired_spec, {
                |_spec| RingBuffer::new(self.sender.clone())
            })
            .unwrap();

        fill_ring_buffer_emu(&mut self.tad, &mut playback);

        let mut state = PlayState::Running;
        playback.resume();

        // Will exit the loop and close the audio device on timeout or channel disconnect.
        while let Ok(msg) = self.rx.recv_timeout(state.timeout_until_close()) {
            match msg {
                AudioMessage::StopAndClose => break,

                AudioMessage::CloseIfSongIdEquals(id) => {
                    if self.tad.song_id() == Some(id) {
                        break;
                    }
                }

                AudioMessage::RingBufferConsumed(_) => {
                    match state {
                        PlayState::Paused | PlayState::SongFinished => (),
                        PlayState::Running => {
                            let sound = fill_ring_buffer_emu(&mut self.tad, &mut playback);

                            // Detect when the song has finished playing.
                            //
                            // Must test if the emulator is outputting audio as the echo buffer
                            // feedback can output sound long after the song has finished.
                            let voices = self.tad.read_voice_positions();
                            if voices.is_some() {
                                self.monitor.set(voices);
                            } else if sound {
                                self.monitor
                                    .set(Some(AudioMonitorData::new(self.tad.song_id())));
                            } else {
                                // The song has finished
                                self.tad.stop_song();
                                state = PlayState::SongFinished;
                                playback.pause();

                                self.monitor.set(None);
                            }
                        }
                        PlayState::PauseRequested => {
                            playback.lock().fill_remaining_with_silence();
                            state = PlayState::Pausing;
                        }
                        PlayState::Pausing => {
                            state = PlayState::Paused;
                            playback.pause();

                            // Reset ring buffer to the beginning when playback is resumed.
                            playback.lock().reset();

                            self.monitor.set(None);
                        }
                    }
                }

                AudioMessage::CommonAudioDataChanged(data) => self.tad.load_cad_no_sfx(data),
                AudioMessage::CommonAudioDataSfxBufferChanged(data) => {
                    self.tad.load_cad_with_sfx_buffer(data)
                }
                AudioMessage::CommandAudioDataWithSfxChanged(data) => {
                    self.tad.load_cad_with_sfx(data)
                }

                AudioMessage::PlaySong(id, song, song_skip, channels_mask) => {
                    // Pause playback to prevent buffer overrun when tick_to_skip is large.
                    playback.pause();
                    playback.lock().reset();
                    match self
                        .tad
                        .load_song(id, song.clone(), song_skip, channels_mask)
                    {
                        Ok(()) => {
                            self.send_started_song_message(id, song);

                            state = PlayState::Running;
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = PlayState::PauseRequested;
                        }
                    }
                }

                AudioMessage::PlaySongSubroutine(id, song, prefix, si, skip) => {
                    // Pause playback to prevent buffer overrun when tick_to_skip is large.
                    playback.pause();
                    playback.lock().reset();
                    match self
                        .tad
                        .load_song_subroutine(id, song.clone(), prefix, si, skip)
                    {
                        Ok(()) => {
                            self.send_started_song_message(id, song);

                            state = PlayState::Running;
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = PlayState::PauseRequested;
                        }
                    }
                }

                AudioMessage::PlaySoundEffectCommand(id, pan) => match state {
                    PlayState::Running => {
                        self.tad.queue_sound_effect(id, pan);
                    }
                    PlayState::Paused
                    | PlayState::PauseRequested
                    | PlayState::Pausing
                    | PlayState::SongFinished => {
                        if self.tad.load_blank_song().is_ok() {
                            self.tad.queue_sound_effect(id, pan);
                            state = PlayState::Running;
                            playback.resume();
                        }
                    }
                },

                AudioMessage::PlaySongWithSfxBuffer(id, song, song_skip) => {
                    // Pause playback to prevent buffer overrun when tick_to_skip is large.
                    playback.pause();
                    playback.lock().reset();
                    match self
                        .tad
                        .load_song_with_sfx_buffer(id, song.clone(), song_skip)
                    {
                        Ok(()) => {
                            self.send_started_song_message(id, song);

                            state = PlayState::Running;
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = PlayState::PauseRequested;
                        }
                    }
                }

                AudioMessage::PlaySfxUsingSfxBuffer(sfx_data, pan) => match state {
                    PlayState::Running => {
                        self.tad.queue_test_sfx(sfx_data, pan);
                    }
                    PlayState::Paused
                    | PlayState::PauseRequested
                    | PlayState::Pausing
                    | PlayState::SongFinished => {
                        if self.tad.load_blank_song_with_sfx_buffer().is_ok() {
                            self.tad.queue_test_sfx(sfx_data, pan);
                            state = PlayState::Running;
                            playback.resume();
                        }
                    }
                },

                AudioMessage::PlaySample(common_data, song_data) => {
                    playback.pause();
                    playback.lock().reset();

                    match self.tad.play_sample(common_data, song_data) {
                        Ok(()) => {
                            state = PlayState::Running;
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = PlayState::PauseRequested;
                        }
                    }
                }

                AudioMessage::Pause => {
                    state = PlayState::PauseRequested;
                }

                AudioMessage::PauseResume(id) => {
                    match state {
                        PlayState::Running => {
                            state = PlayState::PauseRequested;
                        }
                        PlayState::Paused => {
                            // Resume playback if item_id is unchanged
                            if self.tad.song_id() == Some(id) {
                                state = PlayState::Running;
                                playback.resume();

                                self.send_resume_song_message(id);
                            }
                        }
                        PlayState::PauseRequested
                        | PlayState::Pausing
                        | PlayState::SongFinished => {
                            // Do not do anything
                        }
                    }
                }

                AudioMessage::SetMusicChannels(id, mask) => {
                    if Some(id) == self.tad.song_id() {
                        self.tad.set_music_channels_mask(mask);
                    }
                }

                // Cannot process these messages here.
                // Must reload the song when the stereo flag changes.
                // Must close `AudioDevice` to change the sample rate.
                m @ (AudioMessage::SetStereoFlag(_) | AudioMessage::PlayBrrSampleAt32Khz(_)) => {
                    return Some(m);
                }
            }
        }

        None
    }

    // Returns Some if the AudioMessage could not be processed
    #[must_use]
    fn play_brr_sample(&self, sample: &BrrSample) -> Option<AudioMessage> {
        const TIMEOUT: Duration = Duration::from_secs(1);

        let audio_subsystem = self.sdl_context.audio().unwrap();
        let desired_spec = AudioSpecDesired {
            freq: Some(BRR_SAMPLE_RATE),
            channels: Some(1),
            samples: Some((RingBuffer::SDL_BUFFER_SAMPLES * 2).try_into().unwrap()),
        };

        let mut playback = audio_subsystem
            .open_playback(None, &desired_spec, {
                |_spec| RingBuffer::new(self.sender.clone())
            })
            .unwrap();

        let mut decoder = BrrSampleDecoder::new(sample);
        let mut remaining_after_finished: i32 = 1;

        decoder.fill_ring_buffer(&mut playback);

        playback.resume();

        while let Ok(msg) = self.rx.recv_timeout(TIMEOUT) {
            match msg {
                AudioMessage::RingBufferConsumed(_) => {
                    if decoder.is_finished() {
                        // Must wait one more `RingBufferConsumed` message
                        remaining_after_finished -= 1;
                        if remaining_after_finished < 0 {
                            return None;
                        }
                    }
                    decoder.fill_ring_buffer(&mut playback);
                }
                m => {
                    return Some(m);
                }
            }
        }
        None
    }
}

pub fn create_audio_thread(
    gui_sender: fltk::app::Sender<GuiMessage>,
) -> (
    thread::JoinHandle<()>,
    mpsc::Sender<AudioMessage>,
    AudioMonitor,
) {
    let (sender, rx) = mpsc::channel();
    let monitor = AudioMonitor::new();

    let handler = thread::Builder::new()
        .name("audio_thread".into())
        .spawn({
            let s = sender.clone();
            let m = monitor.clone();
            move || AudioThread::new(s, rx, gui_sender, m).run()
        })
        .unwrap();

    (handler, sender, monitor)
}
