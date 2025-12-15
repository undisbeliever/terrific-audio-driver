//! asar enum and asm file generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{BinIncludePath, ExportedBinFile, Exporter, MemoryMapMode, BLANK_SONG_NAME};

use crate::data::UniqueNamesProjectFile;
use crate::driver_constants::TAD_IO_VERSION;
use crate::sound_effects::SfxExportOrder;

use std::fmt::Write;

pub struct AsarMemoryMap {
    mode: MemoryMapMode,
}

impl AsarMemoryMap {
    pub fn new(mode: MemoryMapMode) -> Self {
        Self { mode }
    }
}

pub struct AsarExporter;

#[rustfmt::skip::macros(writeln)]
impl Exporter for AsarExporter {
    type MemoryMap = AsarMemoryMap;

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
                ";; Song and Sound Effect asar enums for {}.\n",
                ";;\n",
                ";; Automatically generated using `tad-compiler asar-enums` or `tad-compiler asar-export`.\n",
                ";;\n",
                ";; This file MUST BE recreated if song list or sound effect export order changes.\n",
                ";;\n",
                "\n",
                "\n",
            ],
            pf.file_name
        )?;

        writeln!(out, "!LAST_SONG_ID = {}", pf.last_song_id())?;
        writeln!(out, "!N_SOUND_EFFECTS = {}", sfx.export_order.len())?;
        writeln!(out)?;

        writeln!(out, ";; Song enum.")?;
        writeln!(out, ";; Input argument for `Tad_LoadSong`")?;
        if !pf.songs.map().contains_key(BLANK_SONG_NAME) {
            writeln!(out, "  !Song_{} = 0 ; blank (silent) song", BLANK_SONG_NAME)?;
        } else {
            writeln!(out, "  ; song_id 0 is blank (silent) song")?;
        }
        for (i, s) in pf.songs.list().iter().enumerate() {
            writeln!(out, "  !Song_{} = {}", s.name, i + 1)?;
        }

        writeln!(out, "\n")?;

        writeln!(out, ";; Sound Effects enum.")?;
        writeln!(out, ";; Input argument for `Tad_QueueSoundEffect` and `Tad_QueuePannedSoundEffect`")?;
        for (i, s) in sfx.export_order.list().iter().enumerate() {
            if i == sfx.low_priority_index() {
                writeln!(out, "; low-priority sound effects")?;
            } else if i == sfx.n_high_priority_sfx() {
                writeln!(out, "; normal-priority sound effects")?;
            } else if i == 0 {
                writeln!(out, "; high-priority sound effects")?;
            }
            writeln!(out, "  !SFX_{} = {}", s, i)?;
        }

        Ok(out)
    }

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &AsarMemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error> {
        let incbin_path = &bin_include_path.0;
        let incbin_size = bin_data.data().len();
        let bank_size = memory_map.mode.bank_size();
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
        writeln!(out, "assert !TAD_IO_VERSION == {}, \"TAD_IO_VERSION in audio driver does not match TAD_IO_VERSION in the asar-api\"", TAD_IO_VERSION)?;

        writeln!(out, "\n!TAD_N_DATA_ITEMS = {n_data_items}")?;

        out += match memory_map.mode {
            MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM,
            MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM,
        };

        writeln!(out, r##"assert pc()-LoadAudioData == {load_audio_data_size}, "LoadAudioData size mismatch"


assert filesize("{incbin_path}") == {incbin_size}, "{incbin_path} file size does not match binary size in the assembly file"


check bankcross off

Tad_AudioData:

Tad_Loader_Bin:
    incbin "{incbin_path}":0..{}
Tad_Loader_SIZE = {}

Tad_AudioDriver_Bin:
    incbin "{incbin_path}":{}..{}
Tad_AudioDriver_SIZE = {}

;; [u24 ; !TAD_N_DATA_ITEMS] - table of PRG ROM offsets (from the start of the first Audio Data segment)
;; u16 footer - 16 bit clipped `bin_data_offset + bin_file.len()` (used to determine the size of the last item)
Tad_DataTable:
    incbin "{incbin_path}":{}..{incbin_size}

Tad_AudioData_End:

check bankcross full
"##,
    ExportedBinFile::LOADER_SIZE,
    ExportedBinFile::LOADER_SIZE,
    ExportedBinFile::AUDIO_DRIVER_OFFSET,
    ExportedBinFile::AUDIO_DRIVER_OFFSET + ExportedBinFile::AUDIO_DRIVER_SIZE,
    ExportedBinFile::AUDIO_DRIVER_SIZE,
    ExportedBinFile::DATA_TABLE_OFFSET
)?;

        Ok(out)
    }
}

// Licensing the LoadSongData under The Unlicense so anyone is free edit and use it without worrying about licensing.

// SPDX-SnippetBegin

// SDPX—SnippetName: Terrific Audio Driver 64tass LoadAudioData callback
// SPDX-License-Identifier: Unlicense

const ASM_HEADER: &str = r##";; Terrific Audio Driver LoadSongData and audio driver/data .incbin statements.
;;
;; This file is automatically generated by `tad-compiler asar-export`.
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

"##;

const LOAD_AUDIO_DATA_LOROM_SIZE: usize = 51;
const LOAD_AUDIO_DATA_LOROM: &str = r##"
assert bank(pc())&$7f <= $40, "pc() not in a LOROM bank"
assert pc()&$ffff == $8000, "pc() does not start at the beginning of a LOROM bank"

;; LoadAudioData callback (LOROM mapping)
;;
;; Called using JSL (return with RTL)
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
;; A8
;; I16
;; DP unknown
;; DB unknown
LoadAudioData: {
    assert defined("LOROM"), "memory map is not LOROM"
    assert !TAD_N_DATA_ITEMS > 1, "No common audio data"

    cmp.b   #!TAD_N_DATA_ITEMS
    bcc     +
        ; return false
        clc
        rtl
    +

    rep     #$30
; A16
    and.w   #$ff

    pha

    asl
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda.l   Tad_DataTable+3,x
    sec
    sbc.l   Tad_DataTable,x
    tay

    lda.l   Tad_DataTable,x
    cmp.w   #$8000
    ora.w   #$8000
    sta     1,s

    ; carry = bit 0 of bank byte

    sep     #$20
; A8
    lda.l   Tad_DataTable+2,x
    rol
    clc
    adc.b   #bank(Tad_DataTable)

    plx

    sec
    rtl
}
"##;

const LOAD_AUDIO_DATA_HIROM_SIZE: usize = 43;
const LOAD_AUDIO_DATA_HIROM: &str = r##"
assert bank(pc())|$80 >= $c0, "pc() not in a HIROM bank"
assert pc()&$ffff == $0000, "pc() does not start at the beginning of a HIROM bank"

;; LoadAudioData callback (HIROM mapping)
;;
;; Called using JSL (return with RTL)
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
;; A8
;; I16
;; DP unknown
;; DB unknown
LoadAudioData: {
    assert defined("HIROM"), "memory map is not HIROM"
    assert !TAD_N_DATA_ITEMS > 1, "No common audio data"

    cmp.b   #!TAD_N_DATA_ITEMS
    bcc     +
        ; return false
        clc
        rtl
    +

    rep     #$30
; A16
    and.w   #$ff

    pha

    asl
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda.l   Tad_DataTable+3,x
    sec
    sbc.l   Tad_DataTable,x
    tay

    lda.l   Tad_DataTable,x
    sta     1,s

    sep     #$21
; A8
    lda.l   Tad_DataTable+2,x
    ; carry set
    adc.b   #(bank(Tad_DataTable)-1)&$ff

    plx

    sec
    rtl
}
"##;

// SPDX-SnippetEnd
