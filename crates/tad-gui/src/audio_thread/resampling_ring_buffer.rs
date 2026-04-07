//! A resampling ring buffer

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use ringbuf::{
    traits::{Consumer, Observer, Producer, Split},
    wrap::FrozenProd,
    Arc, CachingCons, CachingProd, HeapRb,
};

struct HermiteInterpolator(f64, f64, f64, f64);

impl HermiteInterpolator {
    fn new() -> Self {
        Self(0.0, 0.0, 0.0, 0.0)
    }

    fn push(&mut self, value: f64) {
        *self = Self(self.1, self.2, self.3, value);
    }

    /// Hermite interpolation
    ///
    /// Formulas adapted from Interpolation methods by Paul Bourke
    /// https://paulbourke.net/miscellaneous/interpolation/
    /// with a tension value of 0 and a bias value of 0.
    fn interpolate(&self, mu: f64) -> f64 {
        let mu2 = mu * mu;
        let mu3 = mu2 * mu;

        let m0 = (self.1 - self.0) / 2.0 + (self.2 - self.1) / 2.0;
        let m1 = (self.2 - self.1) / 2.0 + (self.3 - self.2) / 2.0;

        let a0 = 2.0 * mu3 - 3.0 * mu2 + 1.0;
        let a1 = mu3 - 2.0 * mu2 + mu;
        let a2 = mu3 - mu2;
        let a3 = -2.0 * mu3 + 3.0 * mu2;

        a0 * self.1 + a1 * m0 + a2 * m1 + a3 * self.2
    }
}

pub struct ResamplingRingBufProducer {
    ringbuf: FrozenProd<Arc<HeapRb<i16>>>,

    input_sample_rate: u32,
    output_sample_rate: u32,
    ratio: f64,

    // The minimum number of vacant samples required before a `resample()` call.
    min_vacant_samples: usize,

    mu: f64,
    left: HermiteInterpolator,
    right: HermiteInterpolator,
}

impl ResamplingRingBufProducer {
    // Number of stereo samples
    pub const INPUT_CHUNK_SIZE: usize = 512;

    fn new(
        ringbuf: CachingProd<Arc<HeapRb<i16>>>,
        input_sample_rate: u32,
        output_sample_rate: u32,
    ) -> Self {
        let min_vacant_samples =
            Self::calc_min_vacant_samples(input_sample_rate, output_sample_rate);

        assert!(min_vacant_samples > Self::INPUT_CHUNK_SIZE);
        assert!(min_vacant_samples * 3 < ringbuf.capacity().into());

        Self {
            ringbuf: ringbuf.freeze(),
            input_sample_rate,
            output_sample_rate,
            ratio: f64::from(input_sample_rate) / f64::from(output_sample_rate),
            min_vacant_samples,
            mu: 0.0,
            left: HermiteInterpolator::new(),
            right: HermiteInterpolator::new(),
        }
    }

    pub fn set_input_sample_rate(&mut self, input_sample_rate: u32) {
        self.input_sample_rate = input_sample_rate;
        self.ratio = f64::from(input_sample_rate) / f64::from(self.output_sample_rate);
        self.min_vacant_samples =
            Self::calc_min_vacant_samples(input_sample_rate, self.output_sample_rate);
    }

    fn calc_min_vacant_samples(input_sample_rate: u32, output_sample_rate: u32) -> usize {
        assert!(input_sample_rate < output_sample_rate);
        assert!((8000..=256000).contains(&input_sample_rate));
        assert!((8000..=256000).contains(&output_sample_rate));

        const _: () = assert!(
            ResamplingRingBufProducer::INPUT_CHUNK_SIZE * 256000 / 8000 < i16::MAX as usize
        );

        // +4 to ensure the whole input chunk is read
        usize::try_from(
            (Self::INPUT_CHUNK_SIZE as u64 + 4) * u64::from(output_sample_rate)
                / u64::from(input_sample_rate),
        )
        .unwrap()
    }

    #[must_use]
    pub fn can_process(&self) -> bool {
        self.ringbuf.fetch();

        self.ringbuf.vacant_len() >= self.min_vacant_samples
    }

    // MUST call `Self::can_process()` before `process()`
    pub fn process(&mut self, samples: &[i16; Self::INPUT_CHUNK_SIZE]) {
        assert!(self.ringbuf.vacant_len() >= self.min_vacant_samples);

        debug_assert!(self.input_sample_rate < self.output_sample_rate);
        let mut it = samples.iter();
        while let (Some(&left), Some(&right)) = (it.next(), it.next()) {
            self.left.push(left.into());
            self.right.push(right.into());

            while self.mu < 1.0 {
                self.ringbuf.push_slice(&[
                    // Rust 1.45 and later do saturating casts when converting float to int
                    self.left.interpolate(self.mu) as i16,
                    self.right.interpolate(self.mu) as i16,
                ]);

                self.mu += self.ratio;
            }

            self.mu -= 1.0;
        }

        debug_assert!(self.mu >= 0.0 && self.mu < 1.0);

        self.ringbuf.commit();
    }

    // MUST call `Self::can_process()` before `process_mono()`
    pub fn process_mono(&mut self, samples: &[i16; Self::INPUT_CHUNK_SIZE / 2]) {
        assert!(self.ringbuf.vacant_len() >= self.min_vacant_samples);

        debug_assert!(self.input_sample_rate < self.output_sample_rate);
        for &s in samples {
            self.left.push(s.into());
            self.right.push(s.into());

            while self.mu < 1.0 {
                self.ringbuf.push_slice(&[
                    // Rust 1.45 and later do saturating casts when converting float to int
                    self.left.interpolate(self.mu) as i16,
                    self.right.interpolate(self.mu) as i16,
                ]);

                self.mu += self.ratio;
            }

            self.mu -= 1.0;
        }

        debug_assert!(self.mu >= 0.0 && self.mu < 1.0);

        self.ringbuf.commit();
    }

    pub fn fill_with_silence(&mut self) {
        self.left = HermiteInterpolator::new();
        self.right = HermiteInterpolator::new();

        self.ringbuf.fetch();
        self.ringbuf.push_iter(std::iter::repeat(0));
        self.ringbuf.commit();
    }
}

pub struct ResamplingRingBufConsumer(CachingCons<Arc<HeapRb<i16>>>);

impl ResamplingRingBufConsumer {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn pop_slice(&mut self, out: &mut [i16]) {
        let read = self.0.pop_slice(out);

        if read != out.len() {
            out[read..].fill(0);
            eprintln!("Resampling Ring Buffer overrun");
        }
    }
}

pub fn resampling_ring_buffer(
    buffer_size: usize,
    input_sample_rate: u32,
    output_sample_rate: u32,
) -> (ResamplingRingBufProducer, ResamplingRingBufConsumer) {
    let (producer, consumer) = HeapRb::<i16>::new(buffer_size).split();

    (
        ResamplingRingBufProducer::new(producer, input_sample_rate, output_sample_rate),
        ResamplingRingBufConsumer(consumer),
    )
}
