//! binary data exporter for embedding to a game

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

pub mod ca65;
pub mod pvsneslib;
pub mod tass64;

pub use self::ca65::{Ca65Exporter, Ca65MemoryMap};
pub use self::pvsneslib::{PvExporter, PvMemoryMap};
pub use self::tass64::{Tass64Exporter, Tass64MemoryMap};

use crate::audio_driver;
use crate::common_audio_data::CommonAudioData;
use crate::data::UniqueNamesProjectFile;
use crate::driver_constants::MAX_N_SONGS;
use crate::errors::{ExportError, ExportSegmentType};
use crate::songs::SongData;

use std::ops::Range;
use std::path::Path;

extern crate relative_path;
use relative_path::{PathExt, RelativePathBuf};

pub const MAX_BIN_FILE: usize = 4 * 1024 * 1024;
pub const BLANK_SONG_NAME: &str = "BLANK";

#[derive(Clone, Copy, PartialEq)]
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
        "[u24 ; N_DATA_ITEMS] - table of PRG ROM offsets (from the start of the first Audio Data segment)";
    pub const DATA_TABLE_FOOTER_DOCSTRING: &'static str =
        "u16 footer - 16 bit clipped `bin_data_offset + bin_file.len()` (used to determine the size of the last item)";

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

fn export_bin_file(
    common_audio_data: &CommonAudioData,
    songs: &[SongData],
    bin_data_offset: usize,
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

        let offset_le_bytes = u32::try_from(bin_data_offset + bin_file.len())
            .unwrap()
            .to_le_bytes();
        let u24_offset = &offset_le_bytes[..3];

        data_table.extend(u24_offset);
        bin_file.extend(d);
    };

    // Populate with data
    add_data(common_audio_data.data());
    for s in songs {
        add_data(s.data());
    }

    // Add data table footer (16 bit clipped value end of binary file)
    let data_table_footer = bin_data_offset + bin_file.len();
    let data_table_footer: u16 = (data_table_footer & usize::from(u16::MAX))
        .try_into()
        .unwrap();
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
            if s.contains(['"', '\'']) {
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
    type MemoryMap;

    fn generate_include_file(pf: UniqueNamesProjectFile) -> Result<String, std::fmt::Error>;

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &Self::MemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error>;

    fn bin_data_offset(memory_map: &Self::MemoryMap) -> usize;

    fn export_bin_file(
        common_audio_data: &CommonAudioData,
        songs: &[SongData],
        memory_map: &Self::MemoryMap,
    ) -> Result<ExportedBinFile, ExportError> {
        export_bin_file(common_audio_data, songs, Self::bin_data_offset(memory_map))
    }
}

pub enum SuffixType {
    Integer,
    LowerHex,
    UpperHex,
}

#[derive(Debug, PartialEq)]
enum SegmentPrefix {
    Integer(String, usize),
    LowerHex(String, usize),
    UpperHex(String, usize),
}

impl SegmentPrefix {
    fn split_hex(s: &str, hex_match: fn(u8) -> bool) -> Result<(String, usize), ()> {
        match s.len().checked_sub(2).and_then(|i| s.split_at_checked(i)) {
            Some((prefix, hex)) => {
                if !hex.bytes().all(hex_match) {
                    return Err(());
                }

                match usize::from_str_radix(hex, 16) {
                    Ok(n) => Ok((prefix.to_owned(), n)),
                    Err(_) => Err(()),
                }
            }
            None => Err(()),
        }
    }

    fn valid_segment_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_' || c == '.'
    }

    pub fn try_from(
        first_segment: &str,
        suffix_type: SuffixType,
        stype: ExportSegmentType,
    ) -> Result<SegmentPrefix, ExportError> {
        if first_segment.contains(|c| !Self::valid_segment_char(c)) {
            return Err(ExportError::InvalidSegmentName(
                stype,
                first_segment.to_owned(),
            ));
        }

        match suffix_type {
            SuffixType::Integer => {
                let prefix = first_segment.trim_end_matches(|c: char| c.is_ascii_digit());
                if prefix.len() != first_segment.len() {
                    let number = first_segment[prefix.len()..].parse().unwrap();
                    Ok(SegmentPrefix::Integer(prefix.to_owned(), number))
                } else {
                    Err(ExportError::NoSegmentNumberSuffix(
                        stype,
                        first_segment.to_owned(),
                    ))
                }
            }

            SuffixType::LowerHex => {
                match Self::split_hex(first_segment, |c| {
                    c.is_ascii_digit() | matches!(c, b'a'..=b'f')
                }) {
                    Ok((prefix, n)) => Ok(SegmentPrefix::LowerHex(prefix, n)),
                    Err(()) => Err(ExportError::NoSegmentLowerHexSuffix(
                        stype,
                        first_segment.to_owned(),
                    )),
                }
            }

            SuffixType::UpperHex => {
                match Self::split_hex(first_segment, |c| {
                    c.is_ascii_digit() | matches!(c, b'A'..=b'F')
                }) {
                    Ok((prefix, n)) => Ok(SegmentPrefix::UpperHex(prefix, n)),
                    Err(()) => Err(ExportError::NoSegmentUpperHexSuffix(
                        stype,
                        first_segment.to_owned(),
                    )),
                }
            }
        }
    }

    pub fn index(&self, index: usize) -> Segment {
        Segment {
            segment: self,
            index,
        }
    }
}

struct Segment<'a> {
    segment: &'a SegmentPrefix,
    index: usize,
}

impl std::fmt::Display for Segment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.segment {
            SegmentPrefix::Integer(prefix, start) => {
                write!(f, "{prefix}{}", start + self.index)
            }
            SegmentPrefix::LowerHex(prefix, start) => {
                write!(f, "{prefix}{:02x}", start + self.index)
            }
            SegmentPrefix::UpperHex(prefix, start) => {
                write!(f, "{prefix}{:02X}", start + self.index)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn segment_prefix_integer() {
        assert_eq!(
            SegmentPrefix::try_from("name 2", SuffixType::Integer, ExportSegmentType::Section),
            Err(ExportError::InvalidSegmentName(
                ExportSegmentType::Section,
                "name 2".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name", SuffixType::Integer, ExportSegmentType::Section),
            Err(ExportError::NoSegmentNumberSuffix(
                ExportSegmentType::Section,
                "name".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name892", SuffixType::Integer, ExportSegmentType::Section)
                .unwrap()
                .index(10)
                .to_string(),
            "name902"
        );
    }

    #[test]
    fn segment_prefix_lower_hex() {
        assert_eq!(
            SegmentPrefix::try_from("name-c1", SuffixType::LowerHex, ExportSegmentType::Section),
            Err(ExportError::InvalidSegmentName(
                ExportSegmentType::Section,
                "name-c1".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name_", SuffixType::LowerHex, ExportSegmentType::Section),
            Err(ExportError::NoSegmentLowerHexSuffix(
                ExportSegmentType::Section,
                "name_".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name_C1", SuffixType::LowerHex, ExportSegmentType::Section),
            Err(ExportError::NoSegmentLowerHexSuffix(
                ExportSegmentType::Section,
                "name_C1".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name_c1", SuffixType::LowerHex, ExportSegmentType::Section)
                .unwrap()
                .index(10)
                .to_string(),
            "name_cb"
        );
    }

    #[test]
    fn segment_prefix_upper_hex() {
        assert_eq!(
            SegmentPrefix::try_from("name-ab", SuffixType::UpperHex, ExportSegmentType::Section),
            Err(ExportError::InvalidSegmentName(
                ExportSegmentType::Section,
                "name-ab".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("name", SuffixType::UpperHex, ExportSegmentType::Section),
            Err(ExportError::NoSegmentUpperHexSuffix(
                ExportSegmentType::Section,
                "name".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("nameab", SuffixType::UpperHex, ExportSegmentType::Section),
            Err(ExportError::NoSegmentUpperHexSuffix(
                ExportSegmentType::Section,
                "nameab".to_owned()
            ))
        );
        assert_eq!(
            SegmentPrefix::try_from("nameAB", SuffixType::UpperHex, ExportSegmentType::Section)
                .unwrap()
                .index(20)
                .to_string(),
            "nameBF"
        );
    }
}
