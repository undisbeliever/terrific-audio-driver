//! binary data exporter for embedding to a game

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

mod ca65;
pub use self::ca65::Ca65Exporter;

use crate::audio_driver;
use crate::common_audio_data::CommonAudioData;
use crate::data::UniqueNamesProjectFile;
use crate::driver_constants::MAX_N_SONGS;
use crate::errors::ExportError;
use crate::songs::SongData;

use std::ops::Range;
use std::path::Path;

extern crate relative_path;
use relative_path::{PathExt, RelativePathBuf};

pub const MAX_BIN_FILE: usize = 4 * 1024 * 1024;
pub const BLANK_SONG_NAME: &str = "BLANK";

#[derive(Clone, Copy)]
pub enum MemoryMapMode {
    LoRom,
    HiRom,
}

impl MemoryMapMode {
    pub fn bank_start(self) -> usize {
        match self {
            Self::LoRom => 0x8000,
            Self::HiRom => 0x0000,
        }
    }

    pub fn bank_size(self) -> usize {
        match self {
            Self::LoRom => 0x8000,
            Self::HiRom => 0x10000,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::LoRom => "LoRom",
            Self::HiRom => "HiRom",
        }
    }
}

pub struct MemoryMap {
    pub(crate) mode: MemoryMapMode,
    pub(crate) segment_prefix: String,
    pub(crate) first_segment_number: usize,
}

impl MemoryMap {
    fn valid_segment_char(c: char) -> bool {
        // PVSnesLib examples contains sections starting with a dot
        c.is_ascii_alphanumeric() || c == '_' || c == '.'
    }

    pub fn try_new(mode: MemoryMapMode, first_segment: &str) -> Result<MemoryMap, ExportError> {
        if first_segment.contains(|c| !Self::valid_segment_char(c)) {
            return Err(ExportError::InvalidSegmentName(first_segment.to_owned()));
        }

        let prefix = first_segment.trim_end_matches(|c: char| c.is_ascii_digit());
        if prefix.len() == first_segment.len() {
            return Err(ExportError::NoSegmentNumberSuffix(first_segment.to_owned()));
        }

        let first_segment_number = first_segment[prefix.len()..].parse().unwrap();

        Ok(MemoryMap {
            mode,
            segment_prefix: prefix.to_owned(),
            first_segment_number,
        })
    }
}

pub struct ExportedBinFile {
    data: Vec<u8>,
    n_songs: usize,
}

impl ExportedBinFile {
    pub const LOADER_OFFSET: usize = 0;
    pub const LOADER_SIZE: usize = audio_driver::LOADER.len();
    pub const AUDIO_DRIVER_OFFSET: usize = Self::LOADER_OFFSET + Self::LOADER_SIZE;
    pub const AUDIO_DRIVER_SIZE: usize = audio_driver::AUDIO_DRIVER.len();
    pub const BLANK_SONG_OFFSET: usize = Self::AUDIO_DRIVER_OFFSET + Self::AUDIO_DRIVER_SIZE;
    pub const BLANK_SONG_SIZE: usize = audio_driver::BLANK_SONG.len();
    pub const DATA_TABLE_OFFSET: usize = Self::BLANK_SONG_OFFSET + Self::BLANK_SONG_SIZE;
    pub const DATA_TABLE_ELEMENT_SIZE: usize = 3;
    pub const DATA_TABLE_FOOTER_SIZE: usize = 2;

    pub const DATA_TABLE_DOCSTRING: &'static str =
        "[u24 ; N_DATA_ITEMS] - table of offsets within the binary file";
    pub const DATA_TABLE_FOOTER_DOCSTRING: &'static str =
        "u16 footer - 16 bit clipped binary file size (used to determine the size of the last item)";

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn n_songs(&self) -> usize {
        self.n_songs
    }

    pub fn data_table_size(&self) -> usize {
        (self.n_songs + 1) * Self::DATA_TABLE_ELEMENT_SIZE + Self::DATA_TABLE_FOOTER_SIZE
    }
}

pub fn export_bin_file(
    common_audio_data: &CommonAudioData,
    songs: &[SongData],
) -> Result<ExportedBinFile, ExportError> {
    if songs.len() > MAX_N_SONGS {
        return Err(ExportError::TooManySongs(songs.len()));
    }

    let data_table_size = 3 * (1 + songs.len()) + 2;

    let bin_file_size = ExportedBinFile::DATA_TABLE_OFFSET
        + data_table_size
        + common_audio_data.data().len()
        + songs.iter().map(|s| s.data().len()).sum::<usize>();

    if bin_file_size > MAX_BIN_FILE {
        return Err(ExportError::BinFileTooLarge(bin_file_size));
    }

    let data_table_range = Range {
        start: ExportedBinFile::DATA_TABLE_OFFSET,
        end: ExportedBinFile::DATA_TABLE_OFFSET + data_table_size,
    };

    let mut bin_file = Vec::with_capacity(bin_file_size);

    bin_file.extend(audio_driver::LOADER);
    bin_file.extend(audio_driver::AUDIO_DRIVER);
    bin_file.extend(audio_driver::BLANK_SONG);

    // Add space for the data table
    assert!(bin_file.len() == data_table_range.start);
    bin_file.resize(data_table_range.end, 0);

    let mut data_table: Vec<u8> = Vec::with_capacity(data_table_size);
    let mut add_data = |d: &[u8]| {
        assert!(d.len() < u16::MAX.into());

        let offset_le_bytes = u32::try_from(bin_file.len()).unwrap().to_le_bytes();
        let u24_offset = &offset_le_bytes[..3];

        data_table.extend(u24_offset);
        bin_file.extend(d);
    };

    // Populate with data
    add_data(common_audio_data.data());
    for s in songs {
        add_data(s.data());
    }

    // Add data table footer (16 bit clipped value of the data size)
    let data_table_footer: u16 = (bin_file.len() & usize::from(u16::MAX)).try_into().unwrap();
    data_table.extend(data_table_footer.to_le_bytes());

    assert!(data_table.len() == data_table_size);
    bin_file[data_table_range].copy_from_slice(&data_table);

    let out = ExportedBinFile {
        data: bin_file,
        n_songs: songs.len(),
    };
    assert!(out.data.len() == bin_file_size);
    assert!(out.data_table_size() == data_table_size);

    Ok(out)
}

pub struct BinIncludePath(pub(crate) RelativePathBuf);

pub fn bin_include_path(
    asm_filename: &Path,
    bin_filename: &Path,
) -> Result<BinIncludePath, ExportError> {
    match bin_filename.to_str() {
        Some(s) => {
            if s.contains(|c| c == '"' || c == '\'') {
                return Err(ExportError::BinPathContainsQuotes);
            }
        }
        None => return Err(ExportError::BinPathNotUtf8),
    }

    match asm_filename.parent() {
        Some(parent) => match bin_filename.relative_to(parent) {
            Ok(r) => Ok(BinIncludePath(r)),
            Err(e) => Err(ExportError::BinPathRelativeToError(e)),
        },
        None => Err(ExportError::NoBinPathParent),
    }
}

pub trait Exporter {
    fn generate_include_file(pf: UniqueNamesProjectFile) -> Result<String, std::fmt::Error>;

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &MemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error>;
}
