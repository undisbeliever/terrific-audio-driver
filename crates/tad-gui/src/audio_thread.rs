//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use compiler::audio_driver;
use compiler::driver_constants::{
    COMMON_DATA_ADDR, DRIVER_CODE_ADDR, DRIVER_LOADER_ADDR, DRIVER_SONG_PTR_ADDR,
};
use compiler::{CommonAudioData, SongData};
use shvc_sound_emu::ShvcSoundEmu;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};

use std::sync::mpsc;
use std::thread;

use crate::compiler_thread::ItemId;

const APU_SAMPLE_RATE: i32 = 32040;

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    Stop,
    PauseResume(ItemId),

    CommonAudioDataChanged(Option<CommonAudioData>),
    PlaySong(ItemId, SongData),
    PlaySample(ItemId, CommonAudioData, SongData),
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

        Self {
            sender,
            buffer: [0; Self::BUFFER_SIZE],
            read_cursor: 0,
            write_cursor: Self::SDL_BUFFER_SAMPLES,
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
) -> Result<(), ()> {
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

    emu.set_echo_buffer_size(edl.esa_register(), edl.as_u8());

    // ::TODO reset echo buffer position::
    emu.set_spc_registers(DRIVER_CODE_ADDR, 0, 0, 0, 0, 0xff);

    Ok(())
}

enum State {
    Paused,
    Running,
    PauseRequested,
    Pausing,
}

fn wait_for_play_song_message(
    rx: &mpsc::Receiver<AudioMessage>,
) -> (ShvcSoundEmu, Option<CommonAudioData>, Option<ItemId>) {
    let mut common_audio_data: Option<CommonAudioData> = None;

    while let Ok(msg) = rx.recv() {
        match msg {
            AudioMessage::CommonAudioDataChanged(data) => {
                common_audio_data = data;
            }
            AudioMessage::PlaySong(song_id, song) => {
                if let Some(common) = &common_audio_data {
                    let mut emu = ShvcSoundEmu::new();
                    emu.power(false);

                    if load_song(&mut emu, common, &song).is_ok() {
                        return (emu, common_audio_data, Some(song_id));
                    }
                }
            }
            AudioMessage::PlaySample(id, common_data, song_data) => {
                let mut emu = ShvcSoundEmu::new();
                emu.power(false);

                if load_song(&mut emu, &common_data, &song_data).is_ok() {
                    return (emu, None, Some(id));
                }
            }
            _ => (),
        }
    }
    panic!("mpsc::Reciever::recv() returned None");
}

fn audio_thread(rx: mpsc::Receiver<AudioMessage>, sender: mpsc::Sender<AudioMessage>) {
    let (mut emu, mut common_audio_data, mut item_id) = wait_for_play_song_message(&rx);

    let sdl_context = sdl2::init().unwrap();
    let audio_subsystem = sdl_context.audio().unwrap();
    let desired_spec = AudioSpecDesired {
        freq: Some(APU_SAMPLE_RATE),
        channels: Some(2),
        samples: Some(RingBuffer::SDL_BUFFER_SAMPLES.try_into().unwrap()),
    };

    let mut playback = audio_subsystem
        .open_playback(None, &desired_spec, {
            let s = sender;
            move |_spec| RingBuffer::new(s)
        })
        .unwrap();

    let mut state = State::Running;
    playback.resume();

    while let Ok(msg) = rx.recv() {
        match msg {
            AudioMessage::RingBufferConsumed(_) => {
                match state {
                    State::Paused => (),
                    State::Running => {
                        fill_ring_buffer(&mut emu, &mut playback);

                        // ::TODO detect when the song has finished::
                    }
                    State::PauseRequested => {
                        playback.lock().fill_with_silence();
                        state = State::Pausing;
                    }
                    State::Pausing => {
                        // Fill the ring buffer with silence
                        playback.lock().fill_with_silence();
                        state = State::Paused;
                        playback.pause();
                    }
                }
            }

            AudioMessage::CommonAudioDataChanged(data) => {
                common_audio_data = data;
            }

            AudioMessage::PlaySong(id, song) => {
                if let Some(common_data) = &common_audio_data {
                    playback.lock();

                    match load_song(&mut emu, common_data, &song) {
                        Ok(()) => {
                            state = State::Running;
                            item_id = Some(id);
                            playback.resume();
                        }
                        Err(()) => {
                            // Stop playback
                            state = State::PauseRequested;
                            item_id = None
                        }
                    }
                } else {
                    // Stop playback
                    state = State::PauseRequested;
                    item_id = None
                }
            }

            AudioMessage::PlaySample(id, common_data, song_data) => {
                match load_song(&mut emu, &common_data, &song_data) {
                    Ok(()) => {
                        state = State::Running;
                        item_id = Some(id);
                        playback.resume();
                    }
                    Err(()) => {
                        // Stop playback
                        state = State::PauseRequested;
                        item_id = None
                    }
                }
            }

            AudioMessage::Stop => {
                state = State::PauseRequested;
                item_id = None;
            }

            AudioMessage::PauseResume(id) => {
                match state {
                    State::Running => {
                        state = State::PauseRequested;
                    }
                    State::Paused => {
                        // Resume playback if item_id is unchanged
                        if item_id == Some(id) {
                            state = State::Running;
                            playback.resume();
                        }
                    }
                    State::PauseRequested | State::Pausing => {
                        // Do not do anything
                    }
                }
            }
        }
    }

    playback.pause();
}

pub fn create_audio_thread() -> (thread::JoinHandle<()>, mpsc::Sender<AudioMessage>) {
    let (sender, rx) = mpsc::channel();

    let handler = thread::Builder::new()
        .name("audio_thread".into())
        .spawn({
            let s = sender.clone();
            move || audio_thread(rx, s)
        })
        .unwrap();

    (handler, sender)
}
