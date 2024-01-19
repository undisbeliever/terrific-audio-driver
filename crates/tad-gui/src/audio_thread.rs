//! Audio thread

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use brr::{BrrSample, SAMPLES_PER_BLOCK};
use compiler::audio_driver;
use compiler::bytecode_interpreter;
use compiler::common_audio_data::CommonAudioData;
use compiler::driver_constants::{addresses, io_commands, LoaderDataType};
use compiler::songs::SongData;
use compiler::time::TickCounter;

use sdl2::Sdl;
use shvc_sound_emu::ShvcSoundEmu;

extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};

use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::Duration;

use crate::compiler_thread::ItemId;
use crate::GuiMessage;

/// Sample rate to run the audio driver at
const APU_SAMPLE_RATE: i32 = 32040;

/// Sample rate to run BRR samples at
const BRR_SAMPLE_RATE: i32 = 32000;

/// Approximate number of samples to play a looping BRR sample for
const LOOPING_BRR_SAMPLE_SAMPLES: usize = 24000;

pub const N_VOICES: usize = 8;

#[derive(Debug, Clone, Copy)]
pub struct ChannelsMask(pub u8);

impl ChannelsMask {
    pub const ALL: ChannelsMask = ChannelsMask(0xff);

    pub fn only_one_channel(channel_name: char) -> Self {
        const FIRST: u32 = 'A' as u32;
        const LAST: u32 = 'A' as u32 + N_VOICES as u32 - 1;

        let c = u32::from(channel_name);
        match c {
            FIRST..=LAST => {
                let shift = c - FIRST;
                ChannelsMask(1 << shift)
            }
            _ => ChannelsMask(0),
        }
    }
}

pub enum AudioMessage {
    RingBufferConsumed(PrivateToken),

    SetStereoFlag(StereoFlag),

    // Stop audio and close the audio device
    StopAndClose,

    Pause,
    PauseResume(ItemId),

    CommonAudioDataChanged(Option<CommonAudioData>),
    PlaySong(ItemId, Arc<SongData>, Option<TickCounter>, ChannelsMask),
    PlaySample(ItemId, CommonAudioData, Arc<SongData>),

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
    pub item_id: Option<ItemId>,
    pub voice_instruction_ptrs: [Option<u16>; N_VOICES],
    /// May not be valid.
    pub voice_return_inst_ptrs: [Option<u16>; N_VOICES],
}

impl AudioMonitorData {
    fn new(item_id: Option<ItemId>) -> Self {
        Self {
            item_id,
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
fn fill_ring_buffer_emu(emu: &mut ShvcSoundEmu, playback: &mut AudioDevice<RingBuffer>) -> bool {
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

    fn write_dsp_register(&mut self, addr: u8, value: u8) {
        self.0.write_dsp_register(addr, value);
    }

    fn write_smp_register(&mut self, addr: u8, value: u8) {
        self.0.write_smp_register(addr, value);
    }
}

fn load_song(
    emu: &mut ShvcSoundEmu,
    common_audio_data: &CommonAudioData,
    song: &SongData,
    stereo_flag: StereoFlag,
    ticks_to_skip: Option<TickCounter>,
    enabled_channels_mask: ChannelsMask,
) -> Result<(), ()> {
    const LOADER_DATA_TYPE_ADDR: usize = addresses::LOADER_DATA_TYPE as usize;

    emu.power(true);

    let song_data = song.data();
    let common_data = common_audio_data.data();
    let edl = &song.metadata().echo_buffer.edl;

    let stereo_flag = match stereo_flag {
        StereoFlag::Stereo => true,
        StereoFlag::Mono => false,
    };

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

    // Set loader flags
    apuram[LOADER_DATA_TYPE_ADDR] = LoaderDataType {
        stereo_flag,
        play_song: false,
        skip_echo_buffer_reset: true,
    }
    .driver_value();

    emu.set_echo_buffer_size(edl.esa_register(), edl.as_u8());

    emu.set_spc_registers(addresses::DRIVER_CODE, 0, 0, 0, 0, 0xff);

    // Wait for the audio-driver to finish initialization
    emu.emulate();

    if let Some(ticks_to_skip) = ticks_to_skip {
        if ticks_to_skip.value() > 0 {
            // Cannot intrepret song in the compiler thread, the `stereo_flag` is unknown.
            let o = bytecode_interpreter::interpret_song(
                song,
                common_audio_data,
                stereo_flag,
                song_data_addr,
                ticks_to_skip,
            );
            if let Some(o) = o {
                o.write_to_emulator(&mut EmulatorWrapper(emu));
            }
        }
    }

    set_enabled_channels_mask(emu, enabled_channels_mask);

    // Unpause the audio driver
    emu.write_io_ports([io_commands::UNPAUSE, 0, 0, 0]);

    Ok(())
}

fn set_enabled_channels_mask(emu: &mut ShvcSoundEmu, mask: ChannelsMask) {
    let apuram = emu.apuram_mut();

    apuram[addresses::ENABLED_CHANNELS_MASK as usize] = mask.0;
}

fn read_voice_positions(emu: &ShvcSoundEmu, item_id: Option<ItemId>) -> Option<AudioMonitorData> {
    let apuram = emu.apuram();

    let song_addr = u16::from_le_bytes([
        apuram[usize::from(addresses::SONG_PTR)],
        apuram[usize::from(addresses::SONG_PTR) + 1],
    ]);

    let enabled_channels_mask = apuram[addresses::ENABLED_CHANNELS_MASK as usize];

    let read_offsets = |addr_l: u16, addr_h: u16| -> [Option<u16>; N_VOICES] {
        std::array::from_fn(|i| {
            const _: () = assert!(N_VOICES <= 8);

            if enabled_channels_mask & (1 << i) != 0 {
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

    if voice_instruction_ptrs.iter().any(Option::is_some) {
        let voice_return_inst_ptrs = read_offsets(
            addresses::CHANNEL_RETURN_INST_PTR_L,
            addresses::CHANNEL_RETURN_INST_PTR_H,
        );

        Some(AudioMonitorData {
            item_id,
            voice_instruction_ptrs,
            voice_return_inst_ptrs,
        })
    } else {
        None
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
            sdl_context: sdl2::init().unwrap(),

            emu: ShvcSoundEmu::new(&iplrom),

            stereo_flag: StereoFlag::Stereo,
            common_audio_data: None,
            item_id: None,
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
                self.common_audio_data = data;
            }
            AudioMessage::SetStereoFlag(sf) => {
                self.stereo_flag = sf;

                // Disable unpause
                self.item_id = None;
            }

            AudioMessage::PlaySong(song_id, song, ticks_to_skip, channels_mask) => {
                if let Some(common) = &self.common_audio_data {
                    if load_song(
                        &mut self.emu,
                        common,
                        &song,
                        self.stereo_flag,
                        ticks_to_skip,
                        channels_mask,
                    )
                    .is_ok()
                    {
                        self.send_started_song_message(song_id, song);
                        self.item_id = Some(song_id);
                        return self.play_song();
                    }
                }
            }
            AudioMessage::PlaySample(id, common_data, song_data) => {
                if load_song(
                    &mut self.emu,
                    &common_data,
                    &song_data,
                    self.stereo_flag,
                    None,
                    ChannelsMask::ALL,
                )
                .is_ok()
                {
                    self.item_id = Some(id);
                    return self.play_song();
                }
            }

            AudioMessage::PlayBrrSampleAt32Khz(brr_sample) => {
                return self.play_brr_sample(&brr_sample);
            }

            AudioMessage::PauseResume(id) => {
                if Some(id) == self.item_id {
                    return self.play_song();
                }
            }

            AudioMessage::StopAndClose
            | AudioMessage::Pause
            | AudioMessage::RingBufferConsumed(_) => (),
        }

        None
    }

    // Returns Some if the AudioMessage could not be processed
    #[must_use]
    fn play_song(&mut self) -> Option<AudioMessage> {
        self.item_id?;

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

        fill_ring_buffer_emu(&mut self.emu, &mut playback);

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
                            let sound = fill_ring_buffer_emu(&mut self.emu, &mut playback);

                            // Detect when the song has finished playing.
                            //
                            // Must test if the emulator is outputting audio as the echo buffer
                            // feedback can output sound long after the song has finished.
                            let voices = read_voice_positions(&self.emu, self.item_id);
                            if voices.is_some() {
                                self.monitor.set(voices);
                            } else if sound {
                                self.monitor.set(Some(AudioMonitorData::new(self.item_id)));
                            } else {
                                // The song has finished
                                self.item_id = None;
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

                AudioMessage::CommonAudioDataChanged(data) => {
                    self.common_audio_data = data;
                }

                AudioMessage::PlaySong(id, song, ticks_to_skip, channels_mask) => {
                    if let Some(common_data) = &self.common_audio_data {
                        // Pause playback to prevent buffer overrun when tick_to_skip is large.
                        playback.pause();
                        playback.lock().reset();
                        match load_song(
                            &mut self.emu,
                            common_data,
                            &song,
                            self.stereo_flag,
                            ticks_to_skip,
                            channels_mask,
                        ) {
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
                    match load_song(
                        &mut self.emu,
                        &common_data,
                        &song_data,
                        self.stereo_flag,
                        None,
                        ChannelsMask::ALL,
                    ) {
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
