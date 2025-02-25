//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use brr::{BrrSample, SAMPLES_PER_BLOCK};
use compiler::bytecode_interpreter::SongInterpreter;
use compiler::common_audio_data::ArcCadWithSfxBufferInAram;
use compiler::common_audio_data::CommonAudioData;
use compiler::common_audio_data::CommonAudioDataWithSfxBuffer;
use compiler::driver_constants::AudioMode;
use compiler::driver_constants::{io_commands, AUDIO_RAM_SIZE, N_DSP_VOICES, N_MUSIC_CHANNELS};
use compiler::mml::MmlPrefixData;
use compiler::songs::{blank_song, SongData};
use compiler::sound_effects::CompiledSoundEffect;
use compiler::time::TickCounter;
use compiler::Pan;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::Sdl;

use std::sync::LockResult;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;
use std::sync::{mpsc, Arc, Mutex, RwLock};
use std::thread;
use std::time::Duration;

use crate::compiler_thread::CommonAudioDataWithSfx;
use crate::compiler_thread::ItemId;
use crate::sfx_export_order::SfxId;
use crate::GuiMessage;

pub const DEFAULT_AUDIO_MODE: AudioMode = AudioMode::Surround;

/// Sample rate to run the audio driver at
const APU_SAMPLE_RATE: i32 = 32040;

/// Sample rate to run BRR samples at
const BRR_SAMPLE_RATE: i32 = 32000;

/// Approximate number of samples to play a looping BRR sample for
const LOOPING_BRR_SAMPLE_SAMPLES: usize = 24000;

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

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetAudioMode(AudioMode),

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

#[derive(Default, Clone)]
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
    const EMU_BUFFER_SAMPLES: usize = tad_emu::TadEmulator::AUDIO_BUFFER_SAMPLES;
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
fn fill_ring_buffer_emu(emu: &mut TadState, playback: &mut AudioDevice<RingBuffer>) -> bool {
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

enum AudioDataState {
    NotLoaded,
    CommonDataOutOfDate, // Audio is still platying
    Sample(CommonAudioData, Box<SongData>),
    SongNoSfx(Arc<CommonAudioDataNoSfx>, Arc<SongData>),
    SongAndSfx(Arc<CommonAudioDataWithSfx>, Arc<SongData>),
    SongWithSfxBuffer(ArcCadWithSfxBufferInAram, Arc<SongData>),
}

pub enum SiCad {
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
            Self::SfxBuffer(c) => c.common_data(),
        }
    }
}

pub type AudioThreadSongInterpreter = SongInterpreter<SiCad, Arc<SongData>>;

// Holding the SongInterpreter in a Arc<RwLock> so the driver-state window
// can retrieve the exact CommonAudioData and SongData used by the currently playing song.
#[derive(Debug, Clone)]
pub struct SharedSongInterpreter(Option<ItemId>, Arc<RwLock<AudioThreadSongInterpreter>>);

impl SharedSongInterpreter {
    pub fn song_id(&self) -> Option<ItemId> {
        self.0
    }

    pub fn try_borrow(&self) -> LockResult<RwLockReadGuard<AudioThreadSongInterpreter>> {
        self.1.read()
    }

    // forbid GUI thread from modifying the song-interpreter
    fn try_borrow_mut(&self) -> LockResult<RwLockWriteGuard<AudioThreadSongInterpreter>> {
        self.1.write()
    }
}

fn create_and_process_song_interpreter(
    song_id: Option<ItemId>,
    audio_data: &AudioDataState,
    song_addr: u16,
    song_skip: SongSkip,
    audio_mode: AudioMode,
) -> Result<Option<SharedSongInterpreter>, ()> {
    let (cad, sd) = match audio_data {
        AudioDataState::NotLoaded => return Ok(None),
        AudioDataState::CommonDataOutOfDate => return Ok(None),
        AudioDataState::Sample(..) => return Ok(None),
        AudioDataState::SongNoSfx(cad, sd) => (SiCad::NoSfx(cad.clone()), sd.clone()),
        AudioDataState::SongAndSfx(cad, sd) => (SiCad::WithSfx(cad.clone()), sd.clone()),
        AudioDataState::SongWithSfxBuffer(cad, sd) => {
            (SiCad::SfxBuffer(cad.common_data().clone()), sd.clone())
        }
    };

    match song_skip {
        SongSkip::None => Ok(Some(SharedSongInterpreter(
            song_id,
            Arc::new(RwLock::new(SongInterpreter::new_zero(
                cad, sd, song_addr, audio_mode,
            ))),
        ))),
        SongSkip::Song(ticks) => {
            let mut si = SongInterpreter::new_zero(cad, sd, song_addr, audio_mode);
            if si.process_song_skip_ticks(ticks) {
                Ok(Some(SharedSongInterpreter(
                    song_id,
                    Arc::new(RwLock::new(si)),
                )))
            } else {
                Ok(None)
            }
        }
        SongSkip::Subroutine(prefix, si, ticks) => {
            match SongInterpreter::new_song_subroutine(cad, sd, song_addr, prefix, si, audio_mode) {
                Ok(mut si) => {
                    if si.process_song_skip_ticks(ticks) {
                        Ok(Some(SharedSongInterpreter(
                            song_id,
                            Arc::new(RwLock::new(si)),
                        )))
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

struct TadState {
    emu: tad_emu::TadEmulator,

    blank_song: Arc<SongData>,

    audio_mode: AudioMode,
    cad_no_sfx: Option<Arc<CommonAudioDataNoSfx>>,
    cad_with_sfx_buffer: Option<Arc<CommonAudioDataWithSfxBuffer>>,
    cad_with_sfx: Option<Arc<CommonAudioDataWithSfx>>,

    data_state: AudioDataState,
    song_id: Option<ItemId>,
    bc_interpreter: Option<SharedSongInterpreter>,

    sfx_queue: SfxQueue,
}

impl TadState {
    fn new() -> Self {
        Self {
            emu: tad_emu::TadEmulator::new(),
            blank_song: Arc::new(blank_song()),
            audio_mode: DEFAULT_AUDIO_MODE,
            cad_no_sfx: None,
            cad_with_sfx_buffer: None,
            cad_with_sfx: None,
            data_state: AudioDataState::NotLoaded,
            bc_interpreter: None,
            song_id: None,
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

    fn set_audio_mode(&mut self, mode: AudioMode) {
        self.audio_mode = mode;
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
            (None, Some(c), _) => Self::audio_state_sfx_buffer(c.clone(), song)?,
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
            (None, Some(c)) => Self::audio_state_sfx_buffer(c.clone(), song)?,
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
            (None, Some(c)) => Self::audio_state_sfx_buffer(c.clone(), blank_song.clone())?,
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
            Some(c) => Self::audio_state_sfx_buffer(c.clone(), song)?,
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
            Some(c) => Self::audio_state_sfx_buffer(c.clone(), self.blank_song.clone())?,
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

    fn audio_state_sfx_buffer(
        cad: Arc<CommonAudioDataWithSfxBuffer>,
        song: Arc<SongData>,
    ) -> Result<AudioDataState, ()> {
        match ArcCadWithSfxBufferInAram::new(cad, Self::calc_song_addr(&song)?) {
            Ok(o) => Ok(AudioDataState::SongWithSfxBuffer(o, song)),
            Err(_) => Err(()),
        }
    }

    fn calc_song_addr(song: &SongData) -> Result<u16, ()> {
        let song_size = song.song_aram_size().total_size();

        match AUDIO_RAM_SIZE.checked_sub(song_size) {
            Some(s) => Ok(s.try_into().unwrap()),
            None => Err(()),
        }
    }

    fn _load_song_into_memory(
        &mut self,
        song_id: Option<ItemId>,
        data_state: AudioDataState,
        song_skip: SongSkip,
        music_channels_mask: MusicChannelsMask,
    ) -> Result<(), ()> {
        self.data_state = AudioDataState::NotLoaded;
        self.song_id = None;
        self.bc_interpreter = None;

        self.sfx_queue = SfxQueue::None;

        let (common_audio_data, song) = match &data_state {
            AudioDataState::NotLoaded => return Err(()),
            AudioDataState::CommonDataOutOfDate => return Err(()),
            AudioDataState::Sample(cad, sd) => (cad, sd.as_ref()),
            AudioDataState::SongNoSfx(cad, sd) => (&cad.0, sd.as_ref()),
            AudioDataState::SongAndSfx(cad, sd) => (&cad.common_audio_data, sd.as_ref()),
            AudioDataState::SongWithSfxBuffer(cad, sd) => (cad.common_data_ref(), sd.as_ref()),
        };

        // Store song data at the end of the audio-RAM
        let song_data_addr = Self::calc_song_addr(song)?;
        if song_data_addr < common_audio_data.min_song_data_addr() {
            return Err(());
        }

        self.bc_interpreter = create_and_process_song_interpreter(
            song_id,
            &data_state,
            song_data_addr,
            song_skip,
            self.audio_mode,
        )?;

        self.emu
            .load_song_skip(
                common_audio_data,
                song,
                song_data_addr,
                self.audio_mode,
                self.bc_interpreter
                    .as_ref()
                    .map(|b| b.try_borrow_mut().unwrap())
                    .as_deref(),
                music_channels_mask.0,
            )
            .map_err(|_| ())?;

        self.data_state = data_state;
        self.song_id = song_id;

        Ok(())
    }

    fn set_music_channels_mask(&mut self, mask: MusicChannelsMask) {
        self.emu.set_music_channels_mask(mask.0);
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
        match &self.sfx_queue {
            SfxQueue::None => (),
            SfxQueue::PlaySfx(sfx_id, pan) => {
                if self.emu.is_io_command_acknowledged() {
                    self.emu.try_send_io_command(
                        io_commands::PLAY_SOUND_EFFECT,
                        sfx_id.value(),
                        pan.as_u8(),
                    );
                    self.sfx_queue = SfxQueue::None;
                }
            }
            SfxQueue::TestSfx(sfx, pan) => {
                let common_data = match &self.data_state {
                    AudioDataState::SongWithSfxBuffer(cad, _) => cad,
                    _ => {
                        self.sfx_queue = SfxQueue::None;
                        return;
                    }
                };

                if self
                    .emu
                    .try_load_sfx_buffer_and_play_sfx(common_data, sfx, *pan)
                {
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
        let s = self.emu.get_music_state()?;

        let voice_return_inst_ptrs = match &mut self.bc_interpreter {
            Some(b) => match b.try_borrow_mut() {
                Ok(mut b) => {
                    // Assumes number of ticks since the last read was < 256;
                    let bc_tick_counter_l = b.tick_counter().value().to_le_bytes()[0];
                    let emu_tick_counter_l = s.song_tick_counter.to_le_bytes()[0];

                    let ticks_passed = emu_tick_counter_l.wrapping_sub(bc_tick_counter_l);
                    if ticks_passed > 0 {
                        let v = b.process_ticks(TickCounter::new(ticks_passed.into()));
                        debug_assert!(v, "Bytecode interpreter timeout");
                    }
                    debug_assert_eq!(
                        b.tick_counter().value() as u16,
                        s.song_tick_counter,
                        "Bytecode interpreter desync"
                    );

                    let channels = b.channels();

                    std::array::from_fn(|i| channels[i].as_ref().and_then(|c| c.topmost_return_pos))
                }
                Err(_) => Default::default(),
            },
            None => Default::default(),
        };

        Some(AudioMonitorData {
            song_id: self.song_id,
            voice_instruction_ptrs: s.voice_instruction_ptrs,
            voice_return_inst_ptrs,
        })
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

    tad: TadState,
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

            tad: TadState::new(),
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

        self.gui_sender.send(GuiMessage::AudioThreadStartedSong(
            id,
            song_data,
            self.tad.bc_interpreter.clone(),
        ));
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
            AudioMessage::SetAudioMode(mode) => {
                self.tad.set_audio_mode(mode);

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
                m @ (AudioMessage::SetAudioMode(_) | AudioMessage::PlayBrrSampleAt32Khz(_)) => {
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
