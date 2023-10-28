//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use compiler::audio_driver;
use compiler::common_audio_data::CommonAudioData;
use compiler::driver_constants::{
    LoaderDataType, COMMON_DATA_ADDR, DRIVER_CODE_ADDR, DRIVER_LOADER_ADDR,
    DRIVER_LOADER_DATA_TYPE_ADDR, DRIVER_SONG_PTR_ADDR,
};
use compiler::songs::SongData;

use shvc_sound_emu::ShvcSoundEmu;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};

use std::sync::mpsc;
use std::thread;

use crate::compiler_thread::ItemId;

const APU_SAMPLE_RATE: i32 = 32040;

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetStereoFlag(StereoFlag),

    // Stop audio and close the audio device
    StopAndClose,

    Pause,
    PauseResume(ItemId),

    CommonAudioDataChanged(Option<CommonAudioData>),
    PlaySong(ItemId, SongData),
    PlaySample(ItemId, CommonAudioData, SongData),
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

fn fill_ring_buffer(emu: &mut ShvcSoundEmu, playback: &mut AudioDevice<RingBuffer>) {
    loop {
        let buffer = emu.emulate();

        let full = playback.lock().add_chunk(buffer);
        if full {
            break;
        }
    }
}

fn load_song(
    emu: &mut ShvcSoundEmu,
    common_data: &CommonAudioData,
    song: &SongData,
    stereo_flag: StereoFlag,
) -> Result<(), ()> {
    const LOADER_DATA_TYPE_ADDR: usize = DRIVER_LOADER_DATA_TYPE_ADDR as usize;

    let song_data = song.data();
    let common_data = common_data.data();
    let edl = &song.metadata().echo_buffer.edl;

    let song_data_addr =
        usize::from(COMMON_DATA_ADDR) + common_data.len() + (common_data.len() % 2);

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
    write_spc_ram(DRIVER_LOADER_ADDR, audio_driver::LOADER);
    write_spc_ram(DRIVER_CODE_ADDR, audio_driver::AUDIO_DRIVER);

    write_spc_ram(COMMON_DATA_ADDR, common_data);
    write_spc_ram(DRIVER_SONG_PTR_ADDR, &song_data_addr.to_le_bytes());
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

    emu.set_spc_registers(DRIVER_CODE_ADDR, 0, 0, 0, 0, 0xff);

    Ok(())
}

enum PlayState {
    Paused,
    Running,
    PauseRequested,
    Pausing,
}

struct AudioThread {
    sender: mpsc::Sender<AudioMessage>,
    rx: mpsc::Receiver<AudioMessage>,

    emu: ShvcSoundEmu,

    stereo_flag: StereoFlag,
    common_audio_data: Option<CommonAudioData>,
    item_id: Option<ItemId>,
}

impl AudioThread {
    fn new(sender: mpsc::Sender<AudioMessage>, rx: mpsc::Receiver<AudioMessage>) -> Self {
        // No IPL ROM
        let iplrom = [0; 64];

        Self {
            sender,
            rx,
            emu: ShvcSoundEmu::new(&iplrom),

            stereo_flag: StereoFlag::Stereo,
            common_audio_data: None,
            item_id: None,
        }
    }

    fn run(&mut self) {
        loop {
            self.wait_for_play_song_message();
            self.play();
        }
    }

    fn wait_for_play_song_message(&mut self) {
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
                            self.item_id = Some(song_id);
                            return;
                        }
                    }
                }
                AudioMessage::PlaySample(id, common_data, song_data) => {
                    if load_song(&mut self.emu, &common_data, &song_data, self.stereo_flag).is_ok()
                    {
                        self.item_id = Some(id);
                        return;
                    }
                }

                AudioMessage::StopAndClose
                | AudioMessage::Pause
                | AudioMessage::PauseResume(_)
                | AudioMessage::RingBufferConsumed(_) => (),
            }
        }
        panic!("mpsc::Reciever::recv() returned None");
    }

    fn play(&mut self) {
        assert!(self.item_id.is_some());

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

        while let Ok(msg) = self.rx.recv() {
            match msg {
                AudioMessage::StopAndClose => break,

                AudioMessage::RingBufferConsumed(_) => {
                    match state {
                        PlayState::Paused => (),
                        PlayState::Running => {
                            fill_ring_buffer(&mut self.emu, &mut playback);

                            // ::TODO detect when the song has finished::
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
                            }
                        }
                        PlayState::PauseRequested | PlayState::Pausing => {
                            // Do not do anything
                        }
                    }
                }

                AudioMessage::SetStereoFlag(sf) => {
                    self.stereo_flag = sf;
                }
            }
        }

        playback.pause();
    }
}

pub fn create_audio_thread() -> (thread::JoinHandle<()>, mpsc::Sender<AudioMessage>) {
    let (sender, rx) = mpsc::channel();

    let handler = thread::Builder::new()
        .name("audio_thread".into())
        .spawn({
            let s = sender.clone();
            move || AudioThread::new(s, rx).run()
        })
        .unwrap();

    (handler, sender)
}
