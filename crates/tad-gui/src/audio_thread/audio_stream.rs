//! cpal audio stream manager

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::assertions_on_constants)]

use crate::audio_thread::{
    resampling_ring_buffer::{resampling_ring_buffer, ResamplingRingBufProducer},
    AudioMessage, PrivateToken, APU_SAMPLE_RATE,
};

use std::sync::mpsc;

use cpal::{
    traits::{DeviceTrait, HostTrait, StreamTrait},
    DefaultStreamConfigError, StreamConfig,
};

#[cfg(not(windows))]
const DEFAULT_AUDIO_SAMPLE_RATE: u32 = 48000;

pub enum OpenStreamError {
    BuildStreamError {
        sample_rate: u32,
        err: cpal::BuildStreamError,
    },
    BackendError(cpal::BackendSpecificError),
    NoOutputDevices,
}

impl OpenStreamError {
    pub fn multiline_message(&self) -> String {
        match self {
            OpenStreamError::BuildStreamError { sample_rate, err } => {
                format!("Cannot open a 16-bit stereo {sample_rate}Hz output stream.\n{err}")
            }
            OpenStreamError::BackendError(e) => format!("Cannot open an output audio stream.\n{e}"),
            OpenStreamError::NoOutputDevices => "No output audio devices found.".to_owned(),
        }
    }
}

impl std::fmt::Display for OpenStreamError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenStreamError::BuildStreamError { sample_rate, err } => {
                write!(
                    f,
                    "cannot build a {sample_rate}Hz stereo i16 output stream: {err}"
                )
            }
            OpenStreamError::BackendError(e) => e.fmt(f),
            OpenStreamError::NoOutputDevices => write!(f, "no output devices found"),
        }
    }
}

pub enum AudioStreamError {
    DeviceNotAvailable,
    OtherError(cpal::BackendSpecificError),
}

pub struct DeviceHost(cpal::Host);

impl DeviceHost {
    pub fn new() -> Self {
        Self(cpal::default_host())
    }

    // On windows the cpal sample rate must match the shared audio device's sample rate.
    //
    // https://github.com/RustAudio/cpal/issues/593
    #[cfg(windows)]
    pub fn open_stream(
        &mut self,
        sender: mpsc::Sender<AudioMessage>,
    ) -> Result<OpenAudioStream, OpenStreamError> {
        let device = self
            .0
            .default_output_device()
            .ok_or(OpenStreamError::NoOutputDevices)?;

        open_audio_stream_at_default_sample_rate(&device, sender)
    }

    // On undisbeliever's Debian Linux pipewrire setup cpal reports the default sample rate as
    // 44100Hz despite pipewire reporting a default clock rate of 48000Hz.
    //
    // Trying 48000Hz first and if that fails try the default sample rate.
    #[cfg(not(windows))]
    pub fn open_stream(
        &mut self,
        sender: mpsc::Sender<AudioMessage>,
    ) -> Result<OpenAudioStream, OpenStreamError> {
        let device = self
            .0
            .default_output_device()
            .ok_or(OpenStreamError::NoOutputDevices)?;

        match open_audio_stream(&device, DEFAULT_AUDIO_SAMPLE_RATE, sender.clone()) {
            Ok(s) => Ok(s),
            Err(_) => open_audio_stream_at_default_sample_rate(&device, sender),
        }
    }
}

pub struct OpenAudioStream {
    ringbuf: ResamplingRingBufProducer,
    stream: cpal::Stream,
}

impl OpenAudioStream {
    pub fn pause(&mut self) {
        let _ = self.stream.pause();
    }

    pub fn play(&mut self) -> Result<(), AudioStreamError> {
        self.stream.play().map_err(|e| match e {
            cpal::PlayStreamError::DeviceNotAvailable => AudioStreamError::DeviceNotAvailable,
            cpal::PlayStreamError::BackendSpecific { err } => AudioStreamError::OtherError(err),
        })
    }

    pub fn ringbuf_mut(&mut self) -> &mut ResamplingRingBufProducer {
        &mut self.ringbuf
    }
}

fn open_audio_stream_at_default_sample_rate(
    device: &cpal::Device,
    sender: mpsc::Sender<AudioMessage>,
) -> Result<OpenAudioStream, OpenStreamError> {
    match device.default_output_config() {
        Ok(c) => open_audio_stream(device, c.sample_rate().0, sender),
        Err(DefaultStreamConfigError::DeviceNotAvailable)
        | Err(DefaultStreamConfigError::StreamTypeNotSupported) => {
            Err(OpenStreamError::NoOutputDevices)
        }
        Err(DefaultStreamConfigError::BackendSpecific { err }) => {
            Err(OpenStreamError::BackendError(err))
        }
    }
}

fn open_audio_stream(
    device: &cpal::Device,
    sample_rate: u32,
    sender: mpsc::Sender<AudioMessage>,
) -> Result<OpenAudioStream, OpenStreamError> {
    let cpal_buffer_size = (sample_rate / 50).next_power_of_two();
    let ring_buffer_size = (cpal_buffer_size as usize) * 4;

    let config = StreamConfig {
        channels: 2,
        sample_rate: cpal::SampleRate(sample_rate),
        buffer_size: cpal::BufferSize::Fixed(cpal_buffer_size),
    };

    let (mut ringbuf, rb_consumer) =
        resampling_ring_buffer(ring_buffer_size, APU_SAMPLE_RATE, sample_rate);

    ringbuf.fill_with_silence();

    match device.build_output_stream(
        &config,
        {
            let s = sender.clone();
            let mut rb = rb_consumer;
            move |buf: &mut [i16], _cb: &cpal::OutputCallbackInfo| {
                rb.pop_slice(buf);
                let _ = s.send(AudioMessage::RingBufferConsumed(PrivateToken::new()));
            }
        },
        {
            let s = sender;
            move |e| {
                eprintln!("Audio stream error: {e}");
                let _ = s.send(AudioMessage::StopAndCloseDevice);
            }
        },
        None, // No timeout
    ) {
        Ok(stream) => Ok(OpenAudioStream { ringbuf, stream }),
        Err(err) => Err(OpenStreamError::BuildStreamError { sample_rate, err }),
    }
}
