//! A simple mono PCM wave file decoder

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use ::std::fmt::Display;
use ::std::io;

#[derive(Eq, PartialEq)]
pub struct MonoPcm16WaveFile {
    pub sample_rate: u32,
    pub samples: Vec<i16>,
}

#[derive(Debug)]
pub enum WavError {
    NotAWaveFile,
    WaveFileTooLarge,
    NotAPcmWaveFile,
    NoSamples,
    NotA16BitMonoWav,

    InvalidWaveFile,
    InvaidDataChunkSize,

    IoError(io::Error),
}

impl Display for WavError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WavError::NotAWaveFile => write!(f, "not a .wav file"),
            WavError::WaveFileTooLarge => write!(f, "wave file is too large"),
            WavError::NotAPcmWaveFile => write!(f, "not a PCM (uncompressed) wave file"),
            WavError::NoSamples => write!(f, "wave file is empty (no samples)"),

            WavError::NotA16BitMonoWav => write!(f, "not a 16-bit mono PCM wave file"),

            WavError::InvalidWaveFile => write!(f, "invalid wave file"),
            WavError::InvaidDataChunkSize => {
                write!(f, "invalid wave file: invalid data chunk size")
            }

            WavError::IoError(io_error) => io_error.fmt(f),
        }
    }
}

impl From<std::io::Error> for WavError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

const WAV_FORMAT_PCM_FORMAT: u16 = 1;

// The extended fields of a fmt chunk are ignored.
#[derive(Debug, Eq, PartialEq)]
struct FmtChunk {
    format_tag: u16,
    n_channels: u16,
    samples_per_second: u32,
    avg_bytes_per_second: u32,
    block_align: u16,
    bits_per_sample: u16,
}

#[derive(Debug, Eq, PartialEq)]
struct WaveFile {
    format: FmtChunk,
    data: Vec<u8>,
}

const WAVE_CHUNK_ID: [u8; 4] = [b'R', b'I', b'F', b'F'];
const WAVE_ID: [u8; 4] = [b'W', b'A', b'V', b'E'];
const FMT_CHUNK_ID: [u8; 4] = [b'f', b'm', b't', b' '];
const DATA_CHUNK_ID: [u8; 4] = [b'd', b'a', b't', b'a'];

fn parse_fmt_chunk(data: &[u8]) -> Result<FmtChunk, WavError> {
    // There are different versions of fmt chunk.
    if data.len() < 16 {
        // Invalid fmt chunk
        return Err(WavError::InvalidWaveFile);
    }

    let read_u16 = |o: usize| -> u16 { u16::from_le_bytes([data[o], data[o + 1]]) };
    let read_u32 =
        |o: usize| -> u32 { u32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]) };

    let format_tag = read_u16(0);
    let n_channels = read_u16(2);
    let samples_per_second = read_u32(4);
    let avg_bytes_per_second = read_u32(8);
    let block_align = read_u16(12);
    let bits_per_sample = read_u16(14);

    Ok(FmtChunk {
        format_tag,
        n_channels,
        samples_per_second,
        avg_bytes_per_second,
        block_align,
        bits_per_sample,
    })
}

fn read_four_bytes(reader: &mut impl io::Read) -> io::Result<[u8; 4]> {
    let mut data = [0; 4];
    reader.read_exact(&mut data)?;
    Ok(data)
}

fn read_wave_file(
    reader: &mut (impl io::Read + io::Seek),
    max_data_size: usize,
) -> Result<WaveFile, WavError> {
    if read_four_bytes(reader)? != WAVE_CHUNK_ID {
        return Err(WavError::NotAWaveFile);
    }

    let _file_size = read_four_bytes(reader)?;

    if read_four_bytes(reader)? != WAVE_ID {
        return Err(WavError::NotAWaveFile);
    }

    // fmt chunk is always after WAVE chunk
    let format = {
        let chunk_id = read_four_bytes(reader)?;
        let chunk_size = u32::from_le_bytes(read_four_bytes(reader)?);

        if chunk_id != FMT_CHUNK_ID {
            return Err(WavError::InvalidWaveFile);
        }

        // This should not happen
        if chunk_size >= 100 {
            return Err(WavError::InvalidWaveFile);
        }

        let chunk_data = {
            let mut v = vec![0; chunk_size.try_into().unwrap()];
            reader.read_exact(&mut v)?;
            v
        };
        let fmt = parse_fmt_chunk(chunk_data.as_slice())?;

        if fmt.format_tag != WAV_FORMAT_PCM_FORMAT {
            return Err(WavError::NotAPcmWaveFile);
        }

        fmt
    };

    let mut data = Vec::new();

    loop {
        let mut chunk_id = [0; 4];

        let n = reader.read(&mut chunk_id)?;
        if n == 0 {
            // End of file
            break;
        }

        if n != chunk_id.len() {
            return Err(WavError::InvalidWaveFile);
        }

        let chunk_size = u32::from_le_bytes(read_four_bytes(reader)?);

        match chunk_id {
            DATA_CHUNK_ID => {
                let start = data.len();
                let chunk_size = usize::try_from(chunk_size).unwrap();

                if data.len() + chunk_size > max_data_size {
                    return Err(WavError::WaveFileTooLarge);
                }

                data.resize(data.len() + chunk_size, 0);
                reader.read_exact(&mut data[start..])?;
            }

            FMT_CHUNK_ID => {
                // A wave file has only one fmt chunk
                return Err(WavError::InvalidWaveFile);
            }

            _ => {
                // Ignore unknown chunks
                reader.seek(io::SeekFrom::Current(chunk_size.try_into().unwrap()))?;
            }
        }
    }

    if data.is_empty() {
        return Err(WavError::NoSamples);
    }

    Ok(WaveFile { format, data })
}

fn is_fmt_16_bit_mono_pcm(fmt: &FmtChunk) -> bool {
    fmt.format_tag == WAV_FORMAT_PCM_FORMAT
        && fmt.n_channels == 1
        && fmt.block_align == 2
        && fmt.bits_per_sample == 16
}

pub fn read_16_bit_mono_wave_file(
    reader: &mut (impl io::Read + io::Seek),
    max_samples: usize,
) -> Result<MonoPcm16WaveFile, WavError> {
    let wav = read_wave_file(reader, max_samples * 2)?;

    if !is_fmt_16_bit_mono_pcm(&wav.format) {
        return Err(WavError::NotA16BitMonoWav);
    }

    if wav.data.len() % 2 != 0 {
        return Err(WavError::InvaidDataChunkSize);
    }

    // Convert data to i16 vector
    let mut samples = Vec::with_capacity(wav.data.len() / 2);

    samples.extend(
        wav.data
            .chunks_exact(2)
            .map(|c| i16::from_le_bytes([c[0], c[1]])),
    );

    Ok(MonoPcm16WaveFile {
        sample_rate: wav.format.samples_per_second,
        samples,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // 96000Hz, 32-bit stereo PCM wave file (1 sample of silence)
    // Created with audacity
    const STEREO_96000_32_BIT_PCM: [u8; 52] = [
        0x52, 0x49, 0x46, 0x46, 0x2c, 0x00, 0x00, 0x00, 0x57, 0x41, 0x56, 0x45, 0x66, 0x6d, 0x74,
        0x20, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x00, 0x77, 0x01, 0x00, 0x00, 0xb8,
        0x0b, 0x00, 0x08, 0x00, 0x20, 0x00, 0x64, 0x61, 0x74, 0x61, 0x08, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];
    const STEREO_96000_32_BIT_PCM_DATA_SIZE: usize = 8;

    // Created using audacity's Tone Generator
    //  Mono, 16-bit PCM
    //  Waveform: Sine, Frequency: 3200, Amplitude: 0.8, Duration: 10 samples
    const MONO_32000_16_BIT_PCM: [u8; 64] = [
        0x52, 0x49, 0x46, 0x46, 0x38, 0x00, 0x00, 0x00, 0x57, 0x41, 0x56, 0x45, 0x66, 0x6d, 0x74,
        0x20, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00, 0xfa,
        0x00, 0x00, 0x02, 0x00, 0x10, 0x00, 0x64, 0x61, 0x74, 0x61, 0x14, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x30, 0x3c, 0x65, 0x61, 0x61, 0x61, 0x32, 0x3c, 0x00, 0x00, 0xce, 0xc3, 0x9f, 0x9e,
        0x9a, 0x9e, 0xd2, 0xc3,
    ];
    // Samples read using python `wave` and `struct` modules.
    const MONO_32000_16_BIT_SAMPLES: [i16; 10] = [
        0, 15408, 24933, 24929, 15410, 0, -15410, -24929, -24934, -15406,
    ];

    #[test]
    fn test_read_wave_file() {
        let wav = read_wave_file(&mut io::Cursor::new(STEREO_96000_32_BIT_PCM), 100).unwrap();

        assert_eq!(
            wav,
            WaveFile {
                format: FmtChunk {
                    format_tag: WAV_FORMAT_PCM_FORMAT,
                    n_channels: 2,
                    samples_per_second: 96000,
                    avg_bytes_per_second: 96000 * 2 * 4,
                    block_align: 8,
                    bits_per_sample: 32,
                },
                data: vec![0; STEREO_96000_32_BIT_PCM_DATA_SIZE]
            }
        )
    }

    #[test]
    fn test_empty_wave_file() {
        // Test assumes the `file_size` header is not read/verified.

        // Remove the DATA chunk from the end of the wav file
        let end = STEREO_96000_32_BIT_PCM.len() - STEREO_96000_32_BIT_PCM_DATA_SIZE - 8;
        let clipped_wav = &STEREO_96000_32_BIT_PCM[0..end];

        let r = read_wave_file(&mut io::Cursor::new(clipped_wav), 100);

        assert!(matches!(r, Err(WavError::NoSamples)));
    }

    #[test]
    fn test_read_wave_file_max_data_size() {
        let data_size = MONO_32000_16_BIT_SAMPLES.len() * 2;

        let r = read_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), 1000);
        assert!(matches!(r, Ok(_)));

        let r = read_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), data_size);
        assert!(matches!(r, Ok(_)));

        let r = read_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), data_size - 1);
        assert!(matches!(r, Err(WavError::WaveFileTooLarge)));

        let r = read_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), 0);
        assert!(matches!(r, Err(WavError::WaveFileTooLarge)));
    }

    #[test]
    fn test_skip_metadata() {
        // Tests that `read_wave_file` will correctly skip over metadata.

        // Created using audacity (2 samples of silence, Title=Title)
        const WAV_WITH_METADATA: [u8; 120] = [
            0x52, 0x49, 0x46, 0x46, 0x70, 0x00, 0x00, 0x00, 0x57, 0x41, 0x56, 0x45, 0x66, 0x6d,
            0x74, 0x20, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x7d, 0x00, 0x00,
            0x00, 0xfa, 0x00, 0x00, 0x02, 0x00, 0x10, 0x00, 0x64, 0x61, 0x74, 0x61, 0x04, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4c, 0x49, 0x53, 0x54, 0x12, 0x00, 0x00, 0x00,
            0x49, 0x4e, 0x46, 0x4f, 0x49, 0x4e, 0x41, 0x4d, 0x06, 0x00, 0x00, 0x00, 0x54, 0x69,
            0x74, 0x6c, 0x65, 0x00, 0x69, 0x64, 0x33, 0x20, 0x26, 0x00, 0x00, 0x00, 0x49, 0x44,
            0x33, 0x04, 0x00, 0x40, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x0c, 0x01, 0x20,
            0x05, 0x04, 0x50, 0x5d, 0x4d, 0x3e, 0x54, 0x49, 0x54, 0x32, 0x00, 0x00, 0x00, 0x06,
            0x00, 0x00, 0x00, 0x54, 0x69, 0x74, 0x6c, 0x65,
        ];

        let wav = read_wave_file(&mut io::Cursor::new(WAV_WITH_METADATA), 100).unwrap();

        assert_eq!(
            wav,
            WaveFile {
                format: FmtChunk {
                    format_tag: WAV_FORMAT_PCM_FORMAT,
                    n_channels: 1,
                    samples_per_second: 32000,
                    avg_bytes_per_second: 32000 * 2,
                    block_align: 2,
                    bits_per_sample: 16,
                },
                data: vec![0; 4]
            }
        )
    }

    #[test]
    fn test_not_16_bit_mono_wav() {
        let r = read_16_bit_mono_wave_file(&mut io::Cursor::new(STEREO_96000_32_BIT_PCM), 100);

        assert!(matches!(r, Err(WavError::NotA16BitMonoWav)));
    }

    #[test]
    fn test_read_16_bit_mono_wave_file() {
        let wav =
            read_16_bit_mono_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), 100).unwrap();

        assert!(
            wav == MonoPcm16WaveFile {
                sample_rate: 32000,
                samples: MONO_32000_16_BIT_SAMPLES.to_vec(),
            }
        );
    }

    #[test]
    fn test_read_16_bit_mono_wave_file_max_data_size() {
        let n_samples = MONO_32000_16_BIT_SAMPLES.len();

        let r = read_16_bit_mono_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), 1000);
        assert!(matches!(r, Ok(_)));

        let r = read_16_bit_mono_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), n_samples);
        assert!(matches!(r, Ok(_)));

        let r =
            read_16_bit_mono_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), n_samples - 1);
        assert!(matches!(r, Err(WavError::WaveFileTooLarge)));

        let r = read_16_bit_mono_wave_file(&mut io::Cursor::new(MONO_32000_16_BIT_PCM), 0);
        assert!(matches!(r, Err(WavError::WaveFileTooLarge)));
    }
}
