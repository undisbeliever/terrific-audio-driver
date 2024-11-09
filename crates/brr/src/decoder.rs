//! BRR decoder

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{BRR_HEADER_END_FLAG, BYTES_PER_BRR_BLOCK, SAMPLES_PER_BLOCK};

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct I15Sample {
    value: i16,
}

impl I15Sample {
    pub const MIN: I15Sample = I15Sample {
        value: i16::MIN >> 1,
    };

    pub fn value(self) -> i32 {
        self.value.into()
    }

    // Sample is clamped to 16 bit, then cliped to 15 bit.
    // (source: Anomie's S-DSP Doc)
    pub fn clamp_and_clip(value: i32) -> Self {
        let value = value.clamp(i16::MIN.into(), i16::MAX.into());
        let value = i16::try_from(value).unwrap();

        // clip to 15 bit (without changing the sign)
        let value = value << 1;
        let value = value >> 1;

        Self { value }
    }

    pub fn from_sample(s: i16) -> Self {
        Self { value: s >> 1 }
    }

    pub const fn to_sample(self) -> i16 {
        self.value << 1
    }
}

pub fn filter0(_p1: I15Sample, _p2: I15Sample) -> i32 {
    0
}

pub fn filter1(p1: I15Sample, _p2: I15Sample) -> i32 {
    let p1: i32 = p1.value.into();

    // Formula from Anomie's S-DSP Doc
    p1 + ((-p1) >> 4)
}

pub fn filter2(p1: I15Sample, p2: I15Sample) -> i32 {
    let p1: i32 = p1.value.into();
    let p2: i32 = p2.value.into();

    // Formula from Anomie's S-DSP Doc
    (p1 << 1) + ((-((p1 << 1) + p1)) >> 5) - p2 + (p2 >> 4)
}

pub fn filter3(p1: I15Sample, p2: I15Sample) -> i32 {
    let p1: i32 = p1.value.into();
    let p2: i32 = p2.value.into();

    // Formula from Anomie's S-DSP Doc
    (p1 << 1) + ((-(p1 + (p1 << 2) + (p1 << 3))) >> 6) - p2 + (((p2 << 1) + p2) >> 4)
}

fn nibble_to_i32(n: u8) -> i32 {
    let n = n & 0xf;
    if n & 0b1000 == 0 {
        // n is positive
        n.into()
    } else {
        // n is negative, sign extend
        let n = n | 0b11110000;
        i8::from_le_bytes([n]).into()
    }
}

fn decode_brr_block_data(
    data: &[u8; SAMPLES_PER_BLOCK / 2],
    p1: I15Sample,
    p2: I15Sample,
    shift: u8,
    filter: impl Fn(I15Sample, I15Sample) -> i32,
) -> [i16; SAMPLES_PER_BLOCK] {
    let mut p1 = p1;
    let mut p2 = p2;

    std::array::from_fn(|i| {
        let nibble = if (i & 1) == 0 {
            data[i / 2] >> 4
        } else {
            data[i / 2]
        };

        let s = if shift <= 12 {
            (nibble_to_i32(nibble) << shift) >> 1
        } else {
            // Shift out of range
            if nibble & 0b1000 == 0 {
                0
            } else {
                -2048
            }
        };

        let s = s + filter(p1, p2);

        p2 = p1;
        p1 = I15Sample::clamp_and_clip(s);

        p1.to_sample()
    })
}

pub fn decode_brr_block(
    brr_block: &[u8; BYTES_PER_BRR_BLOCK],
    previous_sample_1: i16,
    previous_sample_2: i16,
) -> [i16; SAMPLES_PER_BLOCK] {
    let p1 = I15Sample::from_sample(previous_sample_1);
    let p2 = I15Sample::from_sample(previous_sample_2);

    let header = brr_block[0];
    let data = brr_block[1..].try_into().unwrap();

    let shift = header >> 4;
    let filter = (header >> 2) & 0b11;

    match filter {
        0 => decode_brr_block_data(data, p1, p2, shift, filter0),
        1 => decode_brr_block_data(data, p1, p2, shift, filter1),
        2 => decode_brr_block_data(data, p1, p2, shift, filter2),
        3 => decode_brr_block_data(data, p1, p2, shift, filter3),
        _ => panic!(),
    }
}

pub fn decode_brr_data(brr_data: &[u8]) -> Vec<i16> {
    let n_blocks = brr_data.len() / BYTES_PER_BRR_BLOCK;

    let mut out = Vec::with_capacity(n_blocks * SAMPLES_PER_BLOCK);

    let mut p1 = 0;
    let mut p2 = 0;

    for block in brr_data.chunks_exact(BYTES_PER_BRR_BLOCK) {
        let samples = decode_brr_block(block.try_into().unwrap(), p1, p2);

        p1 = samples[SAMPLES_PER_BLOCK - 1];
        p2 = samples[SAMPLES_PER_BLOCK - 2];

        out.extend(samples);

        if block[0] & BRR_HEADER_END_FLAG != 0 {
            break;
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests decode_brr_data using a sine BRR sample
    #[test]
    fn sine() {
        assert_eq!(
            // BRR sample created using wav2brr
            decode_brr_data(&[
                0xc0, 0x00, 0x12, 0x23, 0x44, 0x55, 0x66, 0x77, 0x77, 0x9c, 0x5e, 0x00, 0x00, 0x00,
                0xf0, 0xf0, 0xe0, 0xff, 0x7c, 0xad, 0xbb, 0xcb, 0xbb, 0xcc, 0xbd, 0xcd, 0xdd, 0x79,
                0xbb, 0xac, 0xbc, 0xbd, 0xdd, 0xee, 0xf0, 0xf0
            ]),
            // Sample decoded using split700's brr2wav
            &[
                0, 0, 4096, 8192, 8192, 12288, 16384, 16384, 20480, 20480, 24576, 24576, 28672,
                28672, 28672, 28672, 30784, 30994, 30680, 29944, 28876, 27556, 26052, 24422, 22202,
                20050, 17474, 15106, 11920, 9144, 6232, 3256, 18, -2998, -6044, -9068, -11898,
                -14654, -17306, -19832, -22088, -24090, -25982, -27500, -28818, -29826, -30564,
                -31072, -31220, -31024, -30640, -29836, -28792, -27428, -25934, -24110, -22032,
                -19782, -17312, -14714, -11948, -8982, -6050, -3114
            ]
        );
    }

    /// Tests decode_brr_data using a BRR sample that uses BRR overflow
    #[test]
    fn noise() {
        assert_eq!(
            // A Manually created BRR noise sample from undisbeliever's unnamed-snes-game.
            // Uses BRR overflow to generate noise.
            // Uses filter 0 and 2.
            decode_brr_data(&[
                0xb0, 0x00, 0x00, 0x00, 0x1d, 0x3a, 0x31, 0xcf, 0xbc, 0xb8, 0x04, 0x1e, 0x20, 0xe1,
                0xcb, 0x4f, 0xc3, 0xd6, 0xb8, 0xc3, 0xd5, 0x2d, 0x35, 0x4c, 0xf0, 0xa4, 0xf2, 0xb8,
                0x14, 0xc2, 0x1d, 0xec, 0x2e, 0x42, 0xdf, 0x1e, 0xbb, 0x5f, 0x1f, 0x3a, 0xbc, 0xf4,
                0x4e, 0x2f, 0xcb
            ]),
            // Sample decoded using split700's brr2wav
            &[
                0, 0, 0, 0, 0, 0, 2048, -6144, 6144, -12288, 6144, 2048, -8192, -2048, -10240,
                -8192, -6016, 4404, 16082, 22430, 31774, -25998, 0, 26420, -23366, 0, 30096,
                -10214, 9656, -31412, 0, -23800, 11974, -14256, 20990, -1920, -19244, 24508, -2,
                -12742, -16098, -26936, 27230, -2, 27714, -4516, 28896, -2126, -29096, 20254,
                -7844, -29848, 18038, -9314, 26772, -13964, 17912, -22398, 14236, -13306, 20678,
                -15694, 18280, -20074, 20370, -9936, 29544, -1952, -25276, 6894, 26596, -29494, 0,
                -29694, 17122, -9156, -29410, 16006, -15648, 10460
            ]
        )
    }

    /// Tests decode_brr_block using a manually created sample that uses all 4 filters
    #[test]
    fn test_filters() {
        #[rustfmt::skip]
        #[allow(clippy::identity_op)]
        const BRR_DATA: [u8; 4 * BYTES_PER_BRR_BLOCK] = [
            (10 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            ( 9 << 4) | (1 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            ( 8 << 4) | (2 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            ( 7 << 4) | (3 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
        ];

        assert_eq!(
            decode_brr_data(&BRR_DATA),
            // BRR_DATA decoded using split700's brr2wav
            &[
                0, 1024, 2048, 3072, 4096, 5120, 6144, 7168, -8192, -7168, -6144, -5120, -4096,
                -3072, -2048, -1024, -960, -388, 660, 2154, 4066, 6370, 9042, 12060, 7210, 3174,
                -98, -2652, -4536, -5790, -6454, -6564, -6464, -5914, -4702, -2652, 376, 4482,
                9724, 16126, 19574, 20400, 18998, 15808, 11298, 5948, 234, -5388, -9874, -13240,
                -15514, -16738, -16962, -16242, -14638, -12212, -11076, -10878, -11318, -12140,
                -13134, -14124, -14966, -15548
            ]
        );
    }

    /// Tests decode_brr_block using a BRR sample that uses a shift value that is out of range
    #[test]
    fn test_shift_out_of_range() {
        #[rustfmt::skip]
        #[allow(clippy::identity_op)]
        const BRR_DATA: [u8; 4 * BYTES_PER_BRR_BLOCK] = [
            (12 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (13 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (14 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (15 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
        ];

        #[rustfmt::skip]
        assert_eq!(
            decode_brr_data(&BRR_DATA),
            // BRR_DATA decoded using split700's brr2wav
            &[
                0, 4096, 8192, 12288, 16384, 20480, 24576, 28672, -32768, -28672, -24576, -20480, -16384, -12288, -8192, -4096,
                0, 0, 0, 0, 0, 0, 0, 0, -4096, -4096, -4096, -4096, -4096, -4096, -4096, -4096,
                0, 0, 0, 0, 0, 0, 0, 0, -4096, -4096, -4096, -4096, -4096, -4096, -4096, -4096,
                0, 0, 0, 0, 0, 0, 0, 0, -4096, -4096, -4096, -4096, -4096, -4096, -4096, -4096,
            ]
        );
    }

    /// Tests decode_brr_block using a BRR sample that uses a shift value that is out of range
    #[test]
    fn test_shift_out_of_range2() {
        #[rustfmt::skip]
        #[allow(clippy::identity_op)]
        const BRR_DATA: [u8; 4 * BYTES_PER_BRR_BLOCK] = [
            (15 << 4) | (0 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (15 << 4) | (1 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (15 << 4) | (2 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            (15 << 4) | (3 << 2), 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
        ];

        assert_eq!(
            decode_brr_data(&BRR_DATA),
            // BRR_DATA decoded using split700's brr2wav
            [
                0, 0, 0, 0, 0, 0, 0, 0, -4096, -4096, -4096, -4096, -4096, -4096, -4096, -4096,
                -3840, -3600, -3376, -3166, -2970, -2786, -2612, -2450, -6394, -10092, -13558,
                -16808, -19854, -22710, -25388, -27898, -29382, -29856, -29370, -27998, -25838,
                -23006, -19634, -15860, -15924, -19584, -26500, 29284, -2, -31554, 1290, 27942,
                -16378, 13402, -28150, 4062, 30168, -14630, 14734, -27176, 636, 19126, 29752,
                -31712, 0, 21670, -30694, 0
            ]
        );
    }
}
