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
    BuildStreamError, StreamConfig,
};

const AUDIO_SAMPLE_RATE: u32 = 48000;
const AUDIO_BUFFER_SAMPLES: u32 = 1024;

const RING_BUFFER_SIZE: usize = 4096;

pub enum AudioStreamError {
    DeviceNotAvailable,
    OtherError(cpal::BackendSpecificError),
}

pub struct DeviceHost(cpal::Host);

impl DeviceHost {
    pub fn new() -> Self {
        Self(cpal::default_host())
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

pub fn open_audio_stream(
    host: &DeviceHost,
    sender: mpsc::Sender<AudioMessage>,
) -> Result<OpenAudioStream, BuildStreamError> {
    let device = host
        .0
        .default_output_device()
        .expect("no output device available");

    let config = StreamConfig {
        channels: 2,
        sample_rate: cpal::SampleRate(AUDIO_SAMPLE_RATE),
        buffer_size: cpal::BufferSize::Fixed(AUDIO_BUFFER_SAMPLES),
    };

    let (mut ringbuf, rb_consumer) =
        resampling_ring_buffer(RING_BUFFER_SIZE, APU_SAMPLE_RATE, AUDIO_SAMPLE_RATE);

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
        Err(e) => Err(e),
    }
}
