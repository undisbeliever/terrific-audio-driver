//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

mod resampling_ring_buffer;
use self::resampling_ring_buffer::{
    resampling_ring_buffer, ResamplingRingBufConsumer, ResamplingRingBufProducer,
};

use brr::{BrrSample, SAMPLES_PER_BLOCK};
use compiler::bytecode_interpreter::SongInterpreter;
use compiler::common_audio_data::{CommonAudioData, SfxBufferInAram};
use compiler::driver_constants::AudioMode;
use compiler::driver_constants::{io_commands, AUDIO_RAM_SIZE, N_MUSIC_CHANNELS};
use compiler::identifier::MusicChannelIndex;
use compiler::mml::MmlPrefixData;
use compiler::songs::{blank_song, SongData};
use compiler::sound_effects::CompiledSoundEffect;
use compiler::time::TickCounter;
use compiler::Pan;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::Sdl;
use tad_emu::LagCounters;

use std::ops::Deref;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::LockResult;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;
use std::sync::{mpsc, Arc, Mutex, RwLock};
use std::thread;
use std::time::Duration;

use crate::compiler_thread::{
    CommonAudioDataNoSfx, CommonAudioDataWithSfx, CommonAudioDataWithSfxBuffer,
};
use crate::compiler_thread::{InstrumentAndSampleNames, ItemId};
use crate::sfx_export_order::SfxId;
use crate::GuiMessage;

pub const DEFAULT_AUDIO_MODE: AudioMode = AudioMode::Surround;

/// Sample rate to run the audio driver at
const APU_SAMPLE_RATE: u32 = 32040;

/// Sample rate to run BRR samples at
const BRR_SAMPLE_RATE: u32 = 32000;

const AUDIO_SAMPLE_RATE: u32 = 48000;
const AUDIO_BUFFER_SAMPLES: u16 = 1024;
const RING_BUFFER_SIZE: usize = 3072;

/// Approximate number of samples to play a looping BRR sample for
const LOOPING_BRR_SAMPLE_SAMPLES: usize = 24000;

#[derive(Debug, Clone, Copy)]
pub struct MusicChannelsMask(pub u8);

impl MusicChannelsMask {
    pub const ALL: MusicChannelsMask = MusicChannelsMask(0xff);

    pub fn only_one_channel(c: MusicChannelIndex) -> Self {
        MusicChannelsMask(1 << u8::from(c))
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

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetAudioMode(AudioMode),

    // Stop audio and close the audio device
    StopAndCloseDevice,

    // Stops the audio if playing ItemId with
    StopIfSongIdEquals(ItemId),

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
    PlaySample(Box<CommonAudioData>, Box<SongData>),

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

    pub lag_counters: LagCounters,
}

impl AudioMonitorData {
    fn new(song_id: Option<ItemId>) -> Self {
        Self {
            song_id,
            voice_instruction_ptrs: Default::default(),
            voice_return_inst_ptrs: Default::default(),
            lag_counters: Default::default(),
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

    pub fn get_lag_counters(&self) -> Option<LagCounters> {
        match self.data.lock() {
            Ok(d) => d.as_ref().map(|d| d.lag_counters.clone()),
            Err(_) => None,
        }
    }
}

pub struct SdlAudioCallback {
    ringbuf: ResamplingRingBufConsumer,
    sender: mpsc::Sender<AudioMessage>,
}

impl SdlAudioCallback {
    fn new(ringbuf: ResamplingRingBufConsumer, sender: mpsc::Sender<AudioMessage>) -> Self {
        Self { ringbuf, sender }
    }

    pub fn reset(&mut self) {
        self.ringbuf.clear();
    }
}

impl AudioCallback for SdlAudioCallback {
    type Channel = i16;

    fn callback(&mut self, out: &mut [i16]) {
        self.ringbuf.pop_slice(out);

        let _ = self
            .sender
            .send(AudioMessage::RingBufferConsumed(PrivateToken::new()));
    }
}

// Returns true if any sound is output by the emulator
fn fill_ring_buffer_emu(emu: &mut TadState, rb: &mut ResamplingRingBufProducer) -> bool {
    // Do not emulate the next audio chunk if the ring buffer is full,
    // which can happen if an SDL audio callback occurs in the middle of the last `fill_ring_buffer()` call.
    //
    // In my opinion, the stuttering caused by this early return sounds better then skipping audio.

    let mut silence = true;

    while rb.can_process() {
        let emu_buffer = emu.emulate();

        if silence {
            silence &= emu_buffer.iter().all(|&b| b == 0);
        }
        rb.process(emu_buffer);
    }

    !silence
}

struct BrrSampleDecoder {
    sample: Arc<BrrSample>,
    sample_pos: usize,
    blocks_decoded: usize,
    blocks_to_decode: usize,
    prev1: i16,
    prev2: i16,
}

impl BrrSampleDecoder {
    fn new(sample: Arc<BrrSample>) -> Self {
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

    fn fill_ring_buffer(&mut self, rb: &mut ResamplingRingBufProducer) {
        const BUF_LEN: usize = ResamplingRingBufProducer::INPUT_CHUNK_SIZE / 2;

        let mut buf = [0; BUF_LEN];
        let mut is_done = self.is_finished();

        while rb.can_process() {
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
                rb.process_mono(&buf);
            } else {
                rb.fill_with_silence();
                break;
            }
        }
    }
}

enum AudioDataState {
    NotLoaded,
    CommonDataOutOfDate, // Audio is still platying
    Sample(Box<CommonAudioData>, Box<SongData>),
    SongNoSfx(Arc<CommonAudioDataNoSfx>, Arc<SongData>),
    SongAndSfx(Arc<CommonAudioDataWithSfx>, Arc<SongData>),
    SongWithSfxBuffer(
        Arc<CommonAudioDataWithSfxBuffer>,
        SfxBufferInAram,
        Arc<SongData>,
    ),
}

pub enum SiCad {
    NoSfx(Arc<CommonAudioDataNoSfx>),
    SfxBuffer(Arc<CommonAudioDataWithSfxBuffer>),
    WithSfx(Arc<CommonAudioDataWithSfx>),
}

impl SiCad {
    pub fn instrument_and_sample_names(&self) -> &Arc<InstrumentAndSampleNames> {
        match self {
            Self::NoSfx(c) => &c.1,
            Self::WithSfx(c) => &c.instrument_and_sample_names,
            Self::SfxBuffer(c) => &c.1,
        }
    }
}

impl std::ops::Deref for SiCad {
    type Target = CommonAudioData;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::NoSfx(c) => &c.0,
            Self::WithSfx(c) => &c.common_audio_data,
            Self::SfxBuffer(c) => c.0.common_data(),
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

    pub fn try_borrow(&self) -> LockResult<RwLockReadGuard<'_, AudioThreadSongInterpreter>> {
        self.1.read()
    }

    // forbid GUI thread from modifying the song-interpreter
    fn try_borrow_mut(&self) -> LockResult<RwLockWriteGuard<'_, AudioThreadSongInterpreter>> {
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
        AudioDataState::SongWithSfxBuffer(cad, _, sd) => {
            (SiCad::SfxBuffer(cad.clone()), sd.clone())
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
    const EMU_BUFFER_SIZE: usize = tad_emu::TadEmulator::AUDIO_BUFFER_SIZE;

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
            (None, Some(c), _) => Self::audio_state_sfx_buffer(c, song)?,
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
            (None, Some(c)) => Self::audio_state_sfx_buffer(c, song)?,
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
            (None, Some(c)) => Self::audio_state_sfx_buffer(c, blank_song.clone())?,
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
            Some(c) => Self::audio_state_sfx_buffer(c, song)?,
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
            Some(c) => Self::audio_state_sfx_buffer(c, self.blank_song.clone())?,
            None => return Err(()),
        };
        self._load_song_into_memory(None, data, SongSkip::None, MusicChannelsMask::ALL)
    }

    fn play_sample(
        &mut self,
        common_audio_data: Box<CommonAudioData>,
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
        cad: &Arc<CommonAudioDataWithSfxBuffer>,
        song: Arc<SongData>,
    ) -> Result<AudioDataState, ()> {
        match SfxBufferInAram::new(&cad.0, Self::calc_song_addr(&song)?) {
            Ok(o) => Ok(AudioDataState::SongWithSfxBuffer(cad.clone(), o, song)),
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
            AudioDataState::Sample(cad, sd) => (cad.deref(), sd.as_ref()),
            AudioDataState::SongNoSfx(cad, sd) => (&cad.0, sd.as_ref()),
            AudioDataState::SongAndSfx(cad, sd) => (&cad.common_audio_data, sd.as_ref()),
            AudioDataState::SongWithSfxBuffer(cad, _, sd) => (cad.0.common_data(), sd.as_ref()),
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
                let buffer = match &self.data_state {
                    AudioDataState::SongWithSfxBuffer(_, b, _) => b,
                    _ => {
                        self.sfx_queue = SfxQueue::None;
                        return;
                    }
                };

                if self.emu.try_load_sfx_buffer_and_play_sfx(buffer, sfx, *pan) {
                    self.sfx_queue = SfxQueue::None;
                }
            }
        };
    }

    fn emulate(&mut self) -> &[i16; Self::EMU_BUFFER_SIZE] {
        const EMPTY_BUFFER: [i16; TadState::EMU_BUFFER_SIZE] = [0; TadState::EMU_BUFFER_SIZE];

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
            lag_counters: self.emu.lag_counters(),
        })
    }
}

#[derive(Clone, Copy, PartialEq)]
enum PlayState {
    Closed,
    Starting,
    Running,
    PauseRequested,
    Paused,
    StopRequested,
    Stopped,
}

impl PlayState {
    fn timeout_until_close(&self) -> Duration {
        match self {
            Self::Closed => Duration::MAX,
            Self::Paused | Self::Stopped => Duration::from_secs(2 * 60),
            _ => Duration::from_secs(5),
        }
    }
}

struct OpenAudioDevice {
    ringbuf: ResamplingRingBufProducer,
    playback: AudioDevice<SdlAudioCallback>,
}

struct GuiAudioDevice {
    sdl_context: Sdl,
    sender: mpsc::Sender<AudioMessage>,
    device: Option<OpenAudioDevice>,
    state: PlayState,
}

impl GuiAudioDevice {
    pub fn new(sdl_context: Sdl, sender: mpsc::Sender<AudioMessage>) -> Self {
        Self {
            sdl_context,
            sender,
            device: None,
            state: PlayState::Closed,
        }
    }

    fn state(&self) -> PlayState {
        self.state
    }

    fn is_playing(&self) -> bool {
        match self.state {
            PlayState::Starting | PlayState::Running => true,

            PlayState::Closed
            | PlayState::PauseRequested
            | PlayState::Paused
            | PlayState::StopRequested
            | PlayState::Stopped => false,
        }
    }

    fn open_or_restart(&mut self) {
        match &mut self.device {
            Some(d) => {
                // Reset ring buffer to the beginning
                d.playback.pause();
                d.playback.lock().reset();
            }
            None => {
                let audio_subsystem = self.sdl_context.audio().unwrap();
                let desired_spec = AudioSpecDesired {
                    freq: Some(AUDIO_SAMPLE_RATE as i32),
                    channels: Some(2),
                    samples: Some(AUDIO_BUFFER_SAMPLES),
                };

                let (ringbuf, rb_consumer) =
                    resampling_ring_buffer(RING_BUFFER_SIZE, APU_SAMPLE_RATE, AUDIO_SAMPLE_RATE);

                let playback = audio_subsystem
                    .open_playback(None, &desired_spec, |_spec| {
                        SdlAudioCallback::new(rb_consumer, self.sender.clone())
                    })
                    .unwrap();

                self.device = Some(OpenAudioDevice { ringbuf, playback });
            }
        }

        self.state = PlayState::Starting;

        let _ = self
            .sender
            .send(AudioMessage::RingBufferConsumed(PrivateToken::new()));
    }

    fn resume(&mut self) {
        match &mut self.device {
            Some(d) => {
                self.state = PlayState::Starting;
                d.playback.resume();
                let _ = self
                    .sender
                    .send(AudioMessage::RingBufferConsumed(PrivateToken::new()));
            }
            None => self.open_or_restart(),
        }
    }

    fn close_device(&mut self) {
        self.device = None;
        self.state = PlayState::Closed;
    }

    fn process(
        &mut self,
        _t: PrivateToken,
        mut f: impl FnMut(&mut ResamplingRingBufProducer) -> bool,
    ) {
        match &mut self.device {
            Some(d) => match self.state {
                PlayState::Closed => (),
                PlayState::Starting => {
                    f(&mut d.ringbuf);
                    d.playback.resume();
                    self.state = PlayState::Running;
                }
                PlayState::Running => {
                    let ended = f(&mut d.ringbuf);
                    if ended {
                        self.state = PlayState::StopRequested;
                    }
                }
                PlayState::PauseRequested => {
                    d.ringbuf.fill_with_silence();
                    d.playback.pause();
                    self.state = PlayState::Paused;
                }
                PlayState::Paused => {
                    d.ringbuf.fill_with_silence();
                }
                PlayState::StopRequested => {
                    d.ringbuf.fill_with_silence();
                    d.playback.pause();
                    self.state = PlayState::Stopped;
                }
                PlayState::Stopped => {
                    d.ringbuf.fill_with_silence();
                }
            },
            None => {
                self.state = PlayState::Closed;
            }
        }
    }

    fn pause_if_running(&mut self) {
        match self.state {
            PlayState::Starting | PlayState::Running => {
                self.state = PlayState::PauseRequested;
            }
            PlayState::Closed
            | PlayState::PauseRequested
            | PlayState::Paused
            | PlayState::StopRequested
            | PlayState::Stopped => (),
        }
    }

    // NOTE: Does not close the device
    fn stop(&mut self) {
        if self.device.is_some() {
            self.state = PlayState::StopRequested;
        }
    }
}

fn send_started_song_message(
    id: ItemId,
    song_data: Arc<SongData>,
    tad: &TadState,
    monitor: &mut AudioMonitor,
    gui_sender: &fltk::app::Sender<GuiMessage>,
) {
    // Must set the monitor data, audio timer stops when monitor data is None
    monitor.set(Some(AudioMonitorData::new(Some(id))));

    gui_sender.send(GuiMessage::AudioThreadStartedSong(
        id,
        song_data,
        tad.bc_interpreter.clone(),
    ));
}

fn send_resume_song_message(
    id: ItemId,
    monitor: &mut AudioMonitor,
    gui_sender: &fltk::app::Sender<GuiMessage>,
) {
    // Must set the monitor data, audio timer stops when monitor data is None
    monitor.set(Some(AudioMonitorData::new(Some(id))));
    gui_sender.send(GuiMessage::AudioThreadResumedSong(id));
}

fn audio_thread(
    sender: mpsc::Sender<AudioMessage>,
    rx: mpsc::Receiver<AudioMessage>,
    gui_sender: fltk::app::Sender<GuiMessage>,
    monitor: AudioMonitor,
) {
    let mut device = GuiAudioDevice::new(sdl2::init().unwrap(), sender.clone());
    let mut monitor = monitor;
    let mut tad = TadState::new();

    let mut brr_sample: Option<BrrSampleDecoder> = None;

    loop {
        let msg = match rx.recv_timeout(device.state().timeout_until_close()) {
            Ok(m) => m,
            Err(RecvTimeoutError::Timeout) => AudioMessage::StopAndCloseDevice,
            Err(RecvTimeoutError::Disconnected) => break,
        };

        match msg {
            AudioMessage::RingBufferConsumed(t) => {
                device.process(t, |rb| match &mut brr_sample {
                    None => {
                        rb.set_input_sample_rate(APU_SAMPLE_RATE);

                        let sound = fill_ring_buffer_emu(&mut tad, rb);

                        // Detect when the song has finished playing.
                        //
                        // Must test if the emulator is outputting audio as the echo buffer
                        // feedback can output sound long after the song has finished.
                        let voices = tad.read_voice_positions();
                        if voices.is_some() {
                            monitor.set(voices);
                            false
                        } else if sound {
                            monitor.set(Some(AudioMonitorData::new(tad.song_id())));
                            false
                        } else {
                            // The song has finished
                            tad.stop_song();
                            monitor.set(None);
                            true
                        }
                    }
                    Some(b) => {
                        rb.set_input_sample_rate(BRR_SAMPLE_RATE);
                        b.fill_ring_buffer(rb);

                        let is_finished = b.is_finished();
                        if is_finished {
                            brr_sample = None;
                        }
                        is_finished
                    }
                });
            }

            AudioMessage::PlayBrrSampleAt32Khz(sample) => {
                brr_sample = Some(BrrSampleDecoder::new(sample));
                device.open_or_restart();
            }

            AudioMessage::CommonAudioDataChanged(data) => {
                tad.load_cad_no_sfx(data);
            }
            AudioMessage::CommonAudioDataSfxBufferChanged(data) => {
                tad.load_cad_with_sfx_buffer(data);
            }
            AudioMessage::CommandAudioDataWithSfxChanged(data) => {
                tad.load_cad_with_sfx(data);
            }

            AudioMessage::PlaySong(id, song, song_skip, channels_mask) => {
                // Pause playback to prevent buffer overrun when tick_to_skip is large.
                device.pause_if_running();

                match tad.load_song(id, song.clone(), song_skip, channels_mask) {
                    Ok(()) => {
                        send_started_song_message(id, song, &tad, &mut monitor, &gui_sender);
                        device.open_or_restart();
                    }
                    Err(()) => device.stop(),
                }
            }
            AudioMessage::PlaySongSubroutine(id, song, prefix, si, skip) => {
                // Pause playback to prevent buffer overrun when tick_to_skip is large.
                device.pause_if_running();

                match tad.load_song_subroutine(id, song.clone(), prefix, si, skip) {
                    Ok(()) => {
                        send_started_song_message(id, song, &tad, &mut monitor, &gui_sender);
                        device.open_or_restart();
                    }
                    Err(()) => device.stop(),
                }
            }
            AudioMessage::PlaySoundEffectCommand(id, pan) => match device.is_playing() {
                true => tad.queue_sound_effect(id, pan),
                false => {
                    if tad.load_blank_song().is_ok() {
                        tad.queue_sound_effect(id, pan);
                        device.open_or_restart();
                    }
                }
            },
            AudioMessage::PlaySongWithSfxBuffer(id, song, song_skip) => {
                // Pause playback to prevent buffer overrun when tick_to_skip is large.
                device.pause_if_running();

                match tad.load_song_with_sfx_buffer(id, song.clone(), song_skip) {
                    Ok(()) => {
                        send_started_song_message(id, song, &tad, &mut monitor, &gui_sender);
                        device.open_or_restart();
                    }
                    Err(()) => device.stop(),
                }
            }
            AudioMessage::PlaySfxUsingSfxBuffer(sfx_data, pan) => match device.is_playing() {
                true => tad.queue_test_sfx(sfx_data, pan),
                false => {
                    if tad.load_blank_song_with_sfx_buffer().is_ok() {
                        tad.queue_test_sfx(sfx_data, pan);
                        device.open_or_restart();
                    }
                }
            },
            AudioMessage::PlaySample(common_data, song_data) => {
                device.pause_if_running();

                match tad.play_sample(common_data, song_data) {
                    Ok(()) => device.open_or_restart(),
                    Err(()) => device.stop(),
                }
            }

            AudioMessage::Pause => {
                device.pause_if_running();
            }
            AudioMessage::PauseResume(id) => {
                match device.state() {
                    PlayState::Starting | PlayState::Running => device.pause_if_running(),
                    PlayState::Closed | PlayState::PauseRequested | PlayState::Paused => {
                        // Resume playback if item_id is unchanged
                        if tad.song_id() == Some(id) {
                            device.resume();
                            send_resume_song_message(id, &mut monitor, &gui_sender);
                        }
                    }
                    PlayState::StopRequested | PlayState::Stopped => {
                        // Do not do anything
                    }
                }
            }
            AudioMessage::StopIfSongIdEquals(id) => {
                if tad.song_id() == Some(id) {
                    tad.stop_song();
                    device.stop();
                    monitor.set(None);
                }
            }
            AudioMessage::StopAndCloseDevice => {
                device.close_device();
            }

            AudioMessage::SetMusicChannels(id, mask) => {
                if Some(id) == tad.song_id() {
                    tad.set_music_channels_mask(mask);
                }
            }
            AudioMessage::SetAudioMode(mode) => {
                tad.set_audio_mode(mode);

                // Disable unpause
                tad.stop_song();
                device.stop();
            }
        }
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
            move || audio_thread(s, rx, gui_sender, m)
        })
        .unwrap();

    (handler, sender, monitor)
}
