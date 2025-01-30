//! PVSnesLib enum and asm file generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{BinIncludePath, ExportedBinFile, Exporter, MemoryMapMode, BLANK_SONG_NAME};

use crate::data::UniqueNamesProjectFile;
use crate::errors::ExportError;
use crate::sound_effects::SfxExportOrder;

use std::cmp::min;
use std::fmt::Write;

// The upcoming PVSnesLib HIROM memory feature moves libtcc to BANK 1.
pub const FIRST_VALID_BANK: u8 = 2;

pub struct PvMemoryMap {
    mode: MemoryMapMode,
    first_bank: u8,
}

impl PvMemoryMap {
    pub fn try_new(mode: MemoryMapMode, first_bank: u8) -> Result<PvMemoryMap, ExportError> {
        if first_bank < FIRST_VALID_BANK {
            return Err(ExportError::PvSnesLibInvalidFirstBank);
        }

        Ok(PvMemoryMap { mode, first_bank })
    }
}

pub struct PvExporter;

#[rustfmt::skip::macros(writeln)]
impl Exporter for PvExporter {
    type MemoryMap = PvMemoryMap;

    fn bin_data_offset(_memory_map: &Self::MemoryMap) -> usize {
        0
    }

    fn generate_include_file(pf: UniqueNamesProjectFile) -> Result<String, std::fmt::Error> {
        let sfx = &pf.sfx_export_order;

        let mut out = String::with_capacity(4096);

        write!(
            out,
            concat![
                "// Song and Sound Effect enums for {}.\n",
                "//\n",
                "// Automatically generated using `tad-compiler pv-enums` or `tad-compiler pv-export`.\n",
                "//\n",
                "// This file MUST BE recreated if song list or sound effect export order changes.\n",
                "//\n",
                "\n",
                "\n#ifndef TAD_AUDIO_ENUMS__INCLUDE_H_",
                "\n",
                "\n",
            ],
            pf.file_name
        )?;

        writeln!(out, "#define LAST_SONG_ID {}", pf.last_song_id())?;
        writeln!(out, "#define N_SOUND_EFFECTS {}", sfx.export_order.len())?;
        writeln!(out)?;

        writeln!(out, "// Song enum.")?;
        writeln!(out, "// Input argument for `tad_loadSong`")?;
        writeln!(out, "enum Song {{")?;
        if !pf.songs.map().contains_key(BLANK_SONG_NAME) {
            writeln!(out, "  SONG_{} = 0, // blank (silent) song", BLANK_SONG_NAME)?;
        } else {
            writeln!(out, "  // song_id 0 is blank (silent) song")?;
        }
        for (i, s) in pf.songs.list().iter().enumerate() {
            writeln!(out, "  SONG_{} = {},", s.name, i + 1)?;
        }
        writeln!(out, "}};")?;

        writeln!(out, "\n")?;

        if !sfx.export_order.is_empty() {
            writeln!(out, "// Sound Effects enum.")?;
            writeln!(out, "// Input argument for `tad_queueSoundEffect` and `tad_queuePannedSoundEffect`")?;
            writeln!(out, "enum SFX {{")?;
            for (i, s) in sfx.export_order.list().iter().enumerate() {
                if i == sfx.low_priority_index() {
                    writeln!(out, "  // low-priority sound effects")?;
                } else if i == sfx.n_high_priority_sfx() {
                    writeln!(out, "  // normal-priority sound effects")?;
                } else if i == 0 {
                    writeln!(out, "  // high-priority sound effects")?;
                }
                writeln!(out, "  SFX_{} = {},", s, i)?;
            }
            writeln!(out, "}};")?;
        }

        writeln!(out)?;
        writeln!(out, "#endif")?;
        writeln!(out)?;

        Ok(out)
    }

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &PvMemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error> {
        let incbin_path = &bin_include_path.0;
        let bank_size = memory_map.mode.bank_size();
        let n_banks = bin_data.data().len().div_ceil(bank_size);

        let n_data_items = bin_data.n_songs() + 1;

        assert!(n_banks > 0);
        assert!(bin_data.n_songs + 1 < u8::MAX.into());
        assert!(
            ExportedBinFile::DATA_TABLE_OFFSET + bin_data.data_table_size() < bank_size,
            "data table does not fit inside a single bank"
        );

        let mut out = String::with_capacity(4096);

        out += ASM_HEADER;

        writeln!(out, "Tad_Loader_SIZE = {}", ExportedBinFile::LOADER_SIZE)?;
        writeln!(out, "Tad_AudioDriver_SIZE = {}", ExportedBinFile::AUDIO_DRIVER_SIZE)?;
        writeln!(out)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_DOCSTRING)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_FOOTER_DOCSTRING)?;
        writeln!(out, "Tad_DataTable_SIZE = {}", bin_data.data_table_size())?;
        writeln!(out)?;
        writeln!(out, "N_DATA_ITEMS = {}", n_data_items)?;
        writeln!(out)?;

        // I cannot export the first byte as a single block and `.export` custom `Tad_AudioData__0 + n` constants.
        // For some unknown reason the bank byte is missing.
        {
            const BLOCK_NUMBER: usize = 0;
            let bank_number = usize::from(memory_map.first_bank) + BLOCK_NUMBER;
            let first_block_offset =
                ExportedBinFile::DATA_TABLE_OFFSET + bin_data.data_table_size();
            let first_block_size = min(
                bank_size - first_block_offset,
                bin_data.data().len() - first_block_offset,
            );

            writeln!(out, ".section \"loadAudioData_{}\" FORCE PRIORITY 100 BANK {} ORG 0", BLOCK_NUMBER, bank_number)?;

            let mut incbin_label = |label: &str, incbin_offset: usize, to_read: usize| {
                writeln!(out, "  {label}: .incbin \"{incbin_path}\" SKIP ${incbin_offset:x} READ ${to_read:x}")
            };
            incbin_label(
                "Tad_Loader_Bin",
                ExportedBinFile::LOADER_OFFSET,
                ExportedBinFile::LOADER_SIZE,
            )?;
            incbin_label(
                "Tad_AudioDriver_Bin",
                ExportedBinFile::AUDIO_DRIVER_OFFSET,
                ExportedBinFile::AUDIO_DRIVER_SIZE,
            )?;
            incbin_label(
                "Tad_DataTable",
                ExportedBinFile::DATA_TABLE_OFFSET,
                bin_data.data_table_size(),
            )?;
            incbin_label(FIRST_BLOCK, first_block_offset, first_block_size)?;

            writeln!(out, ".ends")?;
        }

        for block_number in 1..n_banks {
            let bank_number = usize::from(memory_map.first_bank) + block_number;
            writeln!(out, ".section \"loadAudioData_{}\" FORCE PRIORITY 100 BANK {} ORG 0", block_number, bank_number)?;

            let incbin_offset = block_number * bank_size;
            let remaining_bytes = bin_data.data().len() - incbin_offset;
            assert!(remaining_bytes > 0);

            if remaining_bytes > bank_size {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .incbin \"{incbin_path}\" SKIP ${incbin_offset:x} READ ${bank_size:x}")?;
            } else {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .incbin \"{incbin_path}\" SKIP ${incbin_offset:x}")?;
                // Cannot use .assert here: INPUT_ERROR: .ASSERT needs immediate data.
            }

            writeln!(out, ".ends")?;
        }
        writeln!(out)?;

        writeln!(out, "AUDIO_DATA_BANK = :{FIRST_BLOCK}")?;
        writeln!(out)?;

        out += match memory_map.mode {
            MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM,
            MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM,
        };

        // Cannot use .assert to validate the memory map: INPUT_ERROR: .ASSERT needs immediate data.

        Ok(out)
    }
}

const BLOCK_PREFIX: &str = "Tad_AudioData__";
const FIRST_BLOCK: &str = "Tad_AudioData__0";

// Licensing the loadSongData under The Unlicense so anyone is free edit and use it without worrying about licensing.

// SPDX-SnippetBegin

// SDPX—SnippetName: Terrific Audio Driver PVSnesLib loadAudioData() callback
// SPDX-License-Identifier: Unlicense

const ASM_HEADER: &str = r##";; Terrific Audio Driver loadSongData() and audio driver/data .incbin statements.
;;
;; This file is automatically generated by `tad-compiler pv-export`.
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


.export Tad_Loader_SIZE, Tad_AudioDriver_SIZE, Tad_BlankSong_SIZE

"##;

const LOAD_AUDIO_DATA_LOROM: &str = r##"

.ifdef HIROM
    .fail "The loadAudioData() in this file was written for LOROM.  Please switch to LOROM memory-map or recreate this file using `tad-compiler pv-export --hirom`"
.endif

.ifndef LOROM
    .fail "Unknown memory map.  Please confirm hdr.asm is using LOROM mapping and .define LOROM in hdr.asm"
.endif

.section "loadAudioData_LOROM" SUPERFREE

;; LoadAudioData callback (LOROM mapping)
;;
;; Called using the pvSnesLib ABI
;;
;; void loadAudioData(u8 id)
.index 16
loadAudioData:
    .assert N_DATA_ITEMS >= 1

    php
    rep     #$10
    sep     #$20
.accu 8
.index 16
    phx
    ; Does not use Y

    lda     7,s
    cmp     #N_DATA_ITEMS
    bcs     @Return
        ; MUST NOT USE Y

        rep     #$30
    .accu 16
    .index 16
        and     #$ff

        pha
            asl
            ; carry clear
            adc     1,s
            tax
        pla

        ; Calculate data size
        ; ASSUMES data size > 0 and <= $ffff
        lda.l   Tad_DataTable+3,x
        sec
        sbc.l   Tad_DataTable,x
        sta.w   loadAudioData_out.size

        lda.l   Tad_DataTable,x
        cmp     #$8000
        ora     #$8000
        sta.w   loadAudioData_out.data

        ; carry = bit 0 of bank byte

        sep     #$20
    .accu 8
        lda.l   Tad_DataTable+2,x
        rol
        clc
        adc     #AUDIO_DATA_BANK
        sta.w   loadAudioData_out.data + 2

@Return:
    plx
    plp
    rtl
.ends

"##;

const LOAD_AUDIO_DATA_HIROM: &str = r##"

.ifdef LOROM
    .fail "The loadAudioData() in this file was written for HIROM.  Please switch to HIROM memory-map or recreate this file using `tad-compiler pv-export --lorom`"
.endif

.ifndef HIROM
    .fail "Unknown memory map.  Please confirm hdr.asm is using HIROM mapping and .define HIROM in hdr.asm"
.endif

.section "loadAudioData_HIROM" SUPERFREE

;; LoadAudioData callback (HIROM mapping)
;;
;; Called using the pvSnesLib ABI
;;
;; void loadAudioData(u8 id)
loadAudioData:
    .assert N_DATA_ITEMS >= 1

    php
    rep     #$10
    sep     #$20
.accu 8
.index 16
    phx
    ; Does not use Y


    lda     7,s
    cmp     #N_DATA_ITEMS
    bcs     @Return
        ; MUST NOT USE Y

        rep     #$30
    .accu 16
    .index 16
        and     #$ff

        pha
            asl
            ; carry clear
            adc     1,s
            tax
        pla

        ; Calculate data size
        ; ASSUMES data size > 0 and <= $ffff
        lda.l   Tad_DataTable+3,x
        sec
        sbc.l   Tad_DataTable,x
        sta.w   loadAudioData_out.size

        lda.l   Tad_DataTable,x
        sta.w   loadAudioData_out.data

        sep     #$21
    .accu 8
        lda.l   Tad_DataTable+2,x
        ; carry set
        adc     #(AUDIO_DATA_BANK - 1) & $ff
        sta.w   loadAudioData_out.data + 2

@Return:
    plx
    plp
    rtl
.ends

"##;

// SPDX-SnippetEnd
