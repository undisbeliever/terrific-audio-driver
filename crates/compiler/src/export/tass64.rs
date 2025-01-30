//! tass64 enum and asm file generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{
    BinIncludePath, ExportedBinFile, Exporter, MemoryMapMode, SegmentPrefix, SuffixType,
    BLANK_SONG_NAME,
};

use crate::data::UniqueNamesProjectFile;
use crate::driver_constants::TAD_IO_VERSION;
use crate::errors::{ExportError, ExportSegmentType};
use crate::sound_effects::SfxExportOrder;

use std::fmt::Write;

pub struct Tass64MemoryMap {
    mode: MemoryMapMode,
    prefix: SegmentPrefix,
}

impl Tass64MemoryMap {
    pub fn try_new(
        mode: MemoryMapMode,
        first_section: &str,
        suffix_type: SuffixType,
    ) -> Result<Tass64MemoryMap, ExportError> {
        Ok(Self {
            mode,
            prefix: SegmentPrefix::try_from(
                first_section,
                suffix_type,
                ExportSegmentType::Section,
            )?,
        })
    }
}

pub struct Tass64Exporter;

#[rustfmt::skip::macros(writeln)]
impl Exporter for Tass64Exporter {
    type MemoryMap = Tass64MemoryMap;

    fn bin_data_offset(memory_map: &Self::MemoryMap) -> usize {
        match memory_map.mode {
            MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM_SIZE,
            MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM_SIZE,
        }
    }

    fn generate_include_file(pf: UniqueNamesProjectFile) -> Result<String, std::fmt::Error> {
        let sfx = &pf.sfx_export_order;

        let mut out = String::with_capacity(4096);

        write!(
            out,
            concat![
                ";; Song and Sound Effect 64tass enums for {}.\n",
                ";;\n",
                ";; Automatically generated using `tad-compiler 64tass-enums` or `tad-compiler tass64-export`.\n",
                ";;\n",
                ";; This file MUST BE recreated if song list or sound effect export order changes.\n",
                ";;\n",
                "\n",
                "\n",
            ],
            pf.file_name
        )?;

        writeln!(out, "LAST_SONG_ID = {}", pf.last_song_id())?;
        writeln!(out, "N_SOUND_EFFECTS = {}", sfx.export_order.len())?;
        writeln!(out)?;

        writeln!(out, ";; Song enum.")?;
        writeln!(out, ";; Input argument for `Tad_LoadSong`")?;
        writeln!(out, "Song .block")?;
        if !pf.songs.map().contains_key(BLANK_SONG_NAME) {
            writeln!(out, "  {} = 0 ; blank (silent) song", BLANK_SONG_NAME)?;
        } else {
            writeln!(out, "  ; song_id 0 is blank (silent) song")?;
        }
        for (i, s) in pf.songs.list().iter().enumerate() {
            writeln!(out, "  {} = {}", s.name, i + 1)?;
        }
        writeln!(out, ".endblock")?;

        writeln!(out, "\n")?;

        writeln!(out, ";; Sound Effects enum.")?;
        writeln!(out, ";; Input argument for `Tad_QueueSoundEffect` and `Tad_QueuePannedSoundEffect`")?;
        writeln!(out, "SFX .block")?;
        for (i, s) in sfx.export_order.list().iter().enumerate() {
            if i == sfx.low_priority_index() {
                writeln!(out, "  ; low-priority sound effects")?;
            } else if i == sfx.n_high_priority_sfx() {
                writeln!(out, "  ; normal-priority sound effects")?;
            } else if i == 0 {
                writeln!(out, "  ; high-priority sound effects")?;
            }
            writeln!(out, "  {} = {}", s, i)?;
        }
        writeln!(out, ".endblock")?;

        Ok(out)
    }

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &Tass64MemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error> {
        let incbin_path = &bin_include_path.0;
        let bank_name = memory_map.mode.name();
        let bank_size = memory_map.mode.bank_size();
        let bank_start = memory_map.mode.bank_start();
        let n_data_items = bin_data.n_songs() + 1;

        let load_audio_data_size = Self::bin_data_offset(memory_map);
        let n_banks = (load_audio_data_size + bin_data.data().len()).div_ceil(bank_size);

        assert!(n_banks > 0);
        assert!(n_data_items < u8::MAX.into());
        assert!(
            load_audio_data_size + ExportedBinFile::DATA_TABLE_OFFSET + bin_data.data_table_size()
                < bank_size,
            "data table does not fit inside a single bank"
        );

        let mut out = String::with_capacity(4096);

        out += ASM_HEADER;

        // Add an assert to ensure TAD_IO_VERSION in the audio driver matches the one is `tad-process.inc`
        writeln!(out, ".cerror TAD_IO_VERSION != {}, \"TAD_IO_VERSION in audio driver does not match TAD_IO_VERSION in the 64tass-api\"", TAD_IO_VERSION)?;
        writeln!(out)?;

        for block_number in 0..n_banks {
            let section = memory_map.prefix.index(block_number);

            writeln!(out, "\n.section {}", memory_map.prefix.index(block_number))?;
            writeln!(
                out,
                "  .cerror (* & $ffff) != ${bank_start:04x}, \"{section} does not start at the beginning of a {bank_name} bank (${bank_start:04x}) \", *",
            )?;

            match block_number {
                0 => {
                    out += match memory_map.mode {
                        MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM,
                        MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM,
                    };
                    writeln!(out, ".cerror (* - LoadAudioData) != {load_audio_data_size}, \"LoadAudioData size mismatch\"")?;
                    writeln!(out)?;
                }
                1.. => {
                    let prev_section = memory_map.prefix.index(block_number - 1);
                    writeln!(
                        out,
                        "  .cerror (* >> 16) != ({FIRST_BLOCK} >> 16) + {block_number}, \"{section} section must point to the bank immediatly after {prev_section}\"",
                    )?;
                }
            }

            let incbin_offset = match block_number {
                0 => 0,
                1.. => block_number * bank_size - load_audio_data_size,
            };
            let block_size = match block_number {
                0 => bank_size - load_audio_data_size,
                1.. => bank_size,
            };
            let remaining_bytes = bin_data.data().len() - incbin_offset;
            assert!(remaining_bytes > 0);

            if remaining_bytes > block_size {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .binary \"{incbin_path}\", ${incbin_offset:x}, ${block_size:x}")?;
                writeln!(out, "  .cerror (* & $ffff) != $0000, \"Not at the end of a bank ({section})\", *")?;
            } else {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .binary \"{incbin_path}\", ${incbin_offset:x}")?;
                writeln!(out, "  .cerror (* - {BLOCK_PREFIX}{block_number}) != ${remaining_bytes:x}, \"{incbin_path} file size does not match binary size in the assembly file\"")?;
            }
            writeln!(out, ".endsection")?;
        }
        writeln!(out)?;

        writeln!(out, "Tad_Loader_Bin = {} + {}", FIRST_BLOCK, ExportedBinFile::LOADER_OFFSET)?;
        writeln!(out, "Tad_Loader_SIZE = {}", ExportedBinFile::LOADER_SIZE)?;
        writeln!(out, "Tad_AudioDriver_Bin = {} + {}", FIRST_BLOCK, ExportedBinFile::AUDIO_DRIVER_OFFSET)?;
        writeln!(out, "Tad_AudioDriver_SIZE = {}", ExportedBinFile::AUDIO_DRIVER_SIZE)?;
        writeln!(out)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_DOCSTRING)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_FOOTER_DOCSTRING)?;
        writeln!(out, "Tad_DataTable = {} + {}", FIRST_BLOCK, ExportedBinFile::DATA_TABLE_OFFSET)?;
        writeln!(out, "Tad_DataTable_SIZE = {}", bin_data.data_table_size())?;
        writeln!(out)?;
        writeln!(out, "N_DATA_ITEMS = {}", n_data_items)?;
        writeln!(out, "AUDIO_DATA_BANK = `{FIRST_BLOCK}")?;
        writeln!(out)?;

        writeln!(out, ".cerror (Tad_DataTable >> 16) != ((Tad_DataTable + Tad_DataTable_SIZE) >> 16), \"Tad_DataTable does not fit in a single bank\"")?;
        writeln!(out)?;

        Ok(out)
    }
}

const BLOCK_PREFIX: &str = "_Tad_AudioData_";
const FIRST_BLOCK: &str = "_Tad_AudioData_0";

// Licensing the LoadSongData under The Unlicense so anyone is free edit and use it without worrying about licensing.

// SPDX-SnippetBegin

// SDPX—SnippetName: Terrific Audio Driver 64tass LoadAudioData callback
// SPDX-License-Identifier: Unlicense

const ASM_HEADER: &str = r##";; Terrific Audio Driver LoadSongData and audio driver/data .incbin statements.
;;
;; This file is automatically generated by `tad-compiler 64tass-export`.
;;
;; It MUST BE recreated if the audio project has changed (including samples, sound effects and songs).
;;

; SPDX-License-Identifier: Unlicense
;
; This is free and unencumbered software released into the public domain.
;
; Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in
; source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any
; means.
;
; In jurisdictions that recognize copyright laws, the author or authors of this software dedicate any
; and all copyright interest in the software to the public domain. We make this dedication for the
; benefit of the public at large and to the detriment of our heirs and successors. We intend this
; dedication to be an overt act of relinquishment in perpetuity of all present and future rights to
; this software under copyright law.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
; NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
; For more information, please refer to <http://unlicense.org/>


.cpu "65816"

"##;

const LOAD_AUDIO_DATA_LOROM_SIZE: usize = 51;
const LOAD_AUDIO_DATA_LOROM: &str = r##"
;; LoadAudioData callback (LOROM mapping)
;;
;; Called using JSL (return with RTL)
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
.as
.xl
.dpage ?
.databank ?
LoadAudioData .proc
    .cerror TAD_MEMORY_MAP != "LOROM", "TAD_MEMORY_MAP is not LOROM"

    .cerror N_DATA_ITEMS <= 1, "No common audio data"

    cmp     #N_DATA_ITEMS
    bcc     _ValidInput
        ; return false
        clc
        rtl
_ValidInput:

    rep     #$30
.al
    and     #$ff

    pha

    asl     a
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda     Tad_DataTable+3,x
    sec
    sbc     Tad_DataTable,x
    tay

    lda     Tad_DataTable,x
    cmp     #$8000
    ora     #$8000
    sta     1,s

    ; carry = bit 0 of bank byte

    sep     #$20
.as
    lda     Tad_DataTable+2,x
    rol     a
    clc
    adc     #AUDIO_DATA_BANK

    plx

    sec
    rtl
.endproc

"##;

const LOAD_AUDIO_DATA_HIROM_SIZE: usize = 43;
const LOAD_AUDIO_DATA_HIROM: &str = r##"
;; LoadAudioData callback (HIROM mapping)
;;
;; Called using JSL (return with RTL)
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
.as
.xl
.dpage ?
.databank ?
LoadAudioData .proc
    .cerror TAD_MEMORY_MAP != "HIROM", "TAD_MEMORY_MAP is not HIROM"

    .cerror N_DATA_ITEMS <= 1, "No common audio data"

    cmp     #N_DATA_ITEMS
    bcc     _ValidInput
        ; return false
        clc
        rtl
_ValidInput:

    rep     #$30
.al
    and     #$ff

    pha

    asl     a
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda     Tad_DataTable+3,x
    sec
    sbc     Tad_DataTable,x
    tay

    lda     Tad_DataTable,x
    sta     1,s

    sep     #$21
.as
    lda     Tad_DataTable+2,x
    ; carry set
    adc     #(AUDIO_DATA_BANK - 1) & $ff

    plx

    sec
    rtl
.endproc

"##;

// SPDX-SnippetEnd
