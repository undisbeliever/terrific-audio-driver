//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use compiler::audio_driver;
use compiler::common_audio_data::CommonAudioData;
use compiler::driver_constants::{addresses, LoaderDataType};
use compiler::songs::SongData;

use shvc_sound_emu::ShvcSoundEmu;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};

use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::Duration;

use crate::compiler_thread::ItemId;
use crate::GuiMessage;

const APU_SAMPLE_RATE: i32 = 32040;

pub const N_VOICES: usize = 8;

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetStereoFlag(StereoFlag),

    // Stop audio and close the audio device
    StopAndClose,

    Pause,
    PauseResume(ItemId),

    CommonAudioDataChanged(Option<CommonAudioData>),
    PlaySong(ItemId, Arc<SongData>),
    PlaySample(ItemId, CommonAudioData, Arc<SongData>),
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

#[derive(Debug, Clone)]
pub struct AudioMonitorData {
    pub item_id: Option<ItemId>,
    pub voice_instruction_ptrs: [Option<u16>; N_VOICES],
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

    fn is_buffer_full(&self) -> bool {
        self.write_cursor / RingBuffer::EMU_BUFFER_SIZE
            == self.read_cursor / RingBuffer::EMU_BUFFER_SIZE
    }

    /// Safety: Panics if the ring buffer is full
    ///
    /// Returns true if the buffer is full
    fn add_chunk(&mut self, samples: &[i16; Self::EMU_BUFFER_SIZE]) -> bool {
        const _: () = assert!(RingBuffer::BUFFER_SIZE % RingBuffer::EMU_BUFFER_SIZE == 0);

        if self.is_buffer_full() {
            panic!("RingBuffer is full");
        }
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

    /// Safety: Panics if the ring buffer is full
    ///
    /// Fills the ring buffer with silence
    fn fill_with_silence(&mut self) {
        if self.is_buffer_full() {
            panic!("RingBuffer is full");
        }

        self.buffer.fill(0);

        // write_cursor must be aligned by EMU_BUFFER_SIZE.
        let read_chunk_id = self.read_cursor / RingBuffer::EMU_BUFFER_SIZE;
        self.write_cursor = read_chunk_id * RingBuffer::EMU_BUFFER_SIZE;
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
fn fill_ring_buffer(emu: &mut ShvcSoundEmu, playback: &mut AudioDevice<RingBuffer>) -> bool {
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

fn load_song(
    emu: &mut ShvcSoundEmu,
    common_data: &CommonAudioData,
    song: &SongData,
    stereo_flag: StereoFlag,
) -> Result<(), ()> {
    const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

    let song_data = song.data();
    let common_data = common_data.data();
    let edl = &song.metadata().echo_buffer.edl;

    let song_data_addr =
        usize::from(addresses::COMMON_DATA) + common_data.len() + (common_data.len() % 2);

    let song_data_addr = match u16::try_from(song_data_addr) {
        Ok(a) => a,
        Err(_) => return Err(()),
    };

    let apuram = emu.apuram_mut();

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
    let eb_start = usize::from(song.metadata().echo_buffer.edl.echo_buffer_addr());
    let eb_end = eb_start + edl.buffer_size();
    apuram[eb_start..eb_end].fill(0);

    // Set stereo flag and skip the echo buffer reset delay
    apuram[LOADER_DATA_TYPE_ADDR] = match stereo_flag {
        StereoFlag::Stereo => LoaderDataType::StereoSongDataSkipEchoBufferReset as u8,
        StereoFlag::Mono => LoaderDataType::MonoSongDataSkipEchoBufferReset as u8,
    };

    emu.set_echo_buffer_size(edl.esa_register(), edl.as_u8());

    emu.set_spc_registers(addresses::DRIVER_CODE, 0, 0, 0, 0, 0xff);

    Ok(())
}

fn read_voice_positions(emu: &ShvcSoundEmu) -> [Option<u16>; N_VOICES] {
    let apuram = emu.apuram();

    let mut out = [None; N_VOICES];

    let song_addr = u16::from_le_bytes([
        apuram[usize::from(addresses::SONG_PTR)],
        apuram[usize::from(addresses::SONG_PTR) + 1],
    ]);

    for (i, o) in out.iter_mut().enumerate() {
        let addr = u16::from_le_bytes([
            apuram[usize::from(addresses::CHANNEL_INSTRUCTION_PTR_L) + i],
            apuram[usize::from(addresses::CHANNEL_INSTRUCTION_PTR_H) + i],
        ]);

        if addr > song_addr {
            *o = Some(addr - song_addr);
        }
    }

    out
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

    emu: ShvcSoundEmu,

    stereo_flag: StereoFlag,
    common_audio_data: Option<CommonAudioData>,
    item_id: Option<ItemId>,
}

impl AudioThread {
    fn new(
        sender: mpsc::Sender<AudioMessage>,
        rx: mpsc::Receiver<AudioMessage>,
        gui_sender: fltk::app::Sender<GuiMessage>,
        monitor: AudioMonitor,
    ) -> Self {
        // No IPL ROM
        let iplrom = [0; 64];

        Self {
            sender,
            rx,
            gui_sender,
            monitor,

            emu: ShvcSoundEmu::new(&iplrom),

            stereo_flag: StereoFlag::Stereo,
            common_audio_data: None,
            item_id: None,
        }
    }

    fn run(&mut self) {
        while self.wait_for_play_song_message() {
            self.play();
        }
    }

    fn send_started_song_message(&mut self, id: ItemId, song_data: Arc<SongData>) {
        // Must set the monitor data, audio timer stops when monitor data is None
        self.monitor.set(Some(AudioMonitorData {
            item_id: Some(id),
            voice_instruction_ptrs: Default::default(),
        }));

        self.gui_sender
            .send(GuiMessage::AudioThreadStartedSong(id, song_data));
    }

    fn send_resume_song_message(&mut self, id: ItemId) {
        // Must set the monitor data, audio timer stops when monitor data is None
        self.monitor.set(Some(AudioMonitorData {
            item_id: Some(id),
            voice_instruction_ptrs: Default::default(),
        }));

        self.gui_sender.send(GuiMessage::AudioThreadResumedSong(id));
    }

    // Returns true if a song was loaded into the emulator
    fn wait_for_play_song_message(&mut self) -> bool {
        self.monitor.set(None);

        while let Ok(msg) = self.rx.recv() {
            match msg {
                AudioMessage::CommonAudioDataChanged(data) => {
                    self.common_audio_data = data;
                }
                AudioMessage::SetStereoFlag(sf) => {
                    self.stereo_flag = sf;
                }

                AudioMessage::PlaySong(song_id, song) => {
                    if let Some(common) = &self.common_audio_data {
                        if load_song(&mut self.emu, common, &song, self.stereo_flag).is_ok() {
                            self.send_started_song_message(song_id, song);
                            self.item_id = Some(song_id);
                            return true;
                        }
                    }
                }
                AudioMessage::PlaySample(id, common_data, song_data) => {
                    if load_song(&mut self.emu, &common_data, &song_data, self.stereo_flag).is_ok()
                    {
                        self.item_id = Some(id);
                        return true;
                    }
                }

                AudioMessage::PauseResume(id) => {
                    if Some(id) == self.item_id {
                        return true;
                    }
                }

                AudioMessage::StopAndClose
                | AudioMessage::Pause
                | AudioMessage::RingBufferConsumed(_) => (),
            }
        }

        false
    }

    fn play(&mut self) {
        if self.item_id.is_none() {
            return;
        }

        let sdl_context = sdl2::init().unwrap();
        let audio_subsystem = sdl_context.audio().unwrap();
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

        fill_ring_buffer(&mut self.emu, &mut playback);

        let mut state = PlayState::Running;
        playback.resume();

        // Will exit the loop and close the audio device on timeout or channel disconnect.
        while let Ok(msg) = self.rx.recv_timeout(state.timeout_until_close()) {
            match msg {
                AudioMessage::StopAndClose => break,

                AudioMessage::RingBufferConsumed(_) => {
                    match state {
                        PlayState::Paused | PlayState::SongFinished => (),
                        PlayState::Running => {
                            let sound = fill_ring_buffer(&mut self.emu, &mut playback);

                            // Detect when the song has finished playing.
                            //
                            // Must test if the emulator is outputting audio as the echo buffer
                            // feedback can output sound long after the song has finished.
                            let voices = read_voice_positions(&self.emu);
                            if sound || voices.iter().any(Option::is_some) {
                                self.monitor.set(Some(AudioMonitorData {
                                    item_id: self.item_id,
                                    voice_instruction_ptrs: voices,
                                }));
                            } else {
                                // The song has finished
                                self.item_id = None;
                                state = PlayState::SongFinished;
                                playback.pause();

                                self.monitor.set(None);
                            }
                        }
                        PlayState::PauseRequested => {
                            playback.lock().fill_with_silence();
                            state = PlayState::Pausing;
                        }
                        PlayState::Pausing => {
                            // Fill the ring buffer with silence
                            playback.lock().fill_with_silence();
                            state = PlayState::Paused;
                            playback.pause();

                            self.monitor.set(None);
                        }
                    }
                }

                AudioMessage::CommonAudioDataChanged(data) => {
                    self.common_audio_data = data;
                }

                AudioMessage::PlaySong(id, song) => {
                    if let Some(common_data) = &self.common_audio_data {
                        match load_song(&mut self.emu, common_data, &song, self.stereo_flag) {
                            Ok(()) => {
                                self.send_started_song_message(id, song);

                                state = PlayState::Running;
                                self.item_id = Some(id);
                                playback.resume();
                            }
                            Err(()) => {
                                // Stop playback
                                state = PlayState::PauseRequested;
                                self.item_id = None
                            }
                        }
                    } else {
                        // Stop playback
                        state = PlayState::PauseRequested;
                        self.item_id = None
                    }
                }

                AudioMessage::PlaySample(id, common_data, song_data) => {
                    match load_song(&mut self.emu, &common_data, &song_data, self.stereo_flag) {
                        Ok(()) => {
                            state = PlayState::Running;
                            self.item_id = Some(id);
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = PlayState::PauseRequested;
                            self.item_id = None
                        }
                    }
                }

                AudioMessage::Pause => {
                    state = PlayState::PauseRequested;
                    self.item_id = None;
                }

                AudioMessage::PauseResume(id) => {
                    match state {
                        PlayState::Running => {
                            state = PlayState::PauseRequested;
                        }
                        PlayState::Paused => {
                            // Resume playback if item_id is unchanged
                            if self.item_id == Some(id) {
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

                AudioMessage::SetStereoFlag(sf) => {
                    self.stereo_flag = sf;
                }
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
            move || AudioThread::new(s, rx, gui_sender, m).run()
        })
        .unwrap();

    (handler, sender, monitor)
}
