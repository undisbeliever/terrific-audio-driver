//! ca65 enum and asm file generator

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{BinIncludePath, ExportedBinFile, Exporter, MemoryMap, MemoryMapMode, BLANK_SONG_NAME};

use crate::data::UniqueNamesProjectFile;
use crate::driver_constants::TAD_IO_VERSION;

use std::fmt::Write;

pub struct Ca65Exporter {}

#[rustfmt::skip::macros(writeln)]
impl Exporter for Ca65Exporter {
    fn generate_include_file(pf: UniqueNamesProjectFile) -> Result<String, std::fmt::Error> {
        let mut out = String::with_capacity(4096);

        write!(
            out,
            concat![
                ";; Song and Sound Effect ca65 enums for {}.\n",
                ";;\n",
                ";; Automatically generated using `tad-compiler ca65-enums` or `tad-compiler ca65-export`.\n",
                ";;\n",
                ";; This file MUST BE recreated if song list or sound effect export order changes.\n",
                ";;\n",
                "\n",
                "\n",
            ],
            pf.file_name
        )?;

        writeln!(out, "LAST_SONG_ID = {}", pf.last_song_id())?;
        writeln!(out, "N_SOUND_EFFECTS = {}", pf.sound_effects.len())?;
        writeln!(out)?;

        writeln!(out, ";; Song enum.")?;
        writeln!(out, ";; Input argument for `Tad_LoadSong`")?;
        writeln!(out, ".enum Song")?;
        if !pf.songs.map().contains_key(BLANK_SONG_NAME) {
            writeln!(out, "  {} = 0 ; blank (silent) song", BLANK_SONG_NAME)?;
        } else {
            writeln!(out, "  ; song_id 0 is blank (silent) song")?;
        }
        for (i, s) in pf.songs.list().iter().enumerate() {
            writeln!(out, "  {} = {}", s.name, i + 1)?;
        }
        writeln!(out, ".endenum")?;

        writeln!(out, "\n")?;

        writeln!(out, ";; Sound Effects enum.")?;
        writeln!(out, ";; Input argument for `Tad_QueueSoundEffect` and `Tad_QueuePannedSoundEffect`")?;
        writeln!(out, ".enum SFX")?;
        for (i, s) in pf.sound_effects.list().iter().enumerate() {
            writeln!(out, "  {} = {}", s, i)?;
        }
        writeln!(out, ".endenum")?;

        Ok(out)
    }

    fn generate_asm_file(
        bin_data: &ExportedBinFile,
        memory_map: &MemoryMap,
        bin_include_path: &BinIncludePath,
    ) -> Result<String, std::fmt::Error> {
        let incbin_path = &bin_include_path.0;
        let bank_name = memory_map.mode.name();
        let bank_size = memory_map.mode.bank_size();
        let bank_start = memory_map.mode.bank_start();
        let n_banks = (bin_data.data().len() + (bank_size - 1)) / bank_size;

        let n_data_items = bin_data.n_songs() + 1;

        assert!(n_banks > 0);
        assert!(bin_data.n_songs + 1 < u8::MAX.into());
        assert!(
            ExportedBinFile::DATA_TABLE_OFFSET + bin_data.data_table_size() < bank_size,
            "data table does not fit inside a single bank"
        );

        let mut out = String::with_capacity(4096);

        out += ASM_HEADER;

        writeln!(out, ".export Tad_Loader_Bin := {} + {}", FIRST_BLOCK, ExportedBinFile::LOADER_OFFSET)?;
        writeln!(out, ".export Tad_Loader_SIZE = {}", ExportedBinFile::LOADER_SIZE)?;
        writeln!(out, ".export Tad_AudioDriver_Bin := {} + {}", FIRST_BLOCK, ExportedBinFile::AUDIO_DRIVER_OFFSET)?;
        writeln!(out, ".export Tad_AudioDriver_SIZE = {}", ExportedBinFile::AUDIO_DRIVER_SIZE)?;
        writeln!(out, ".export Tad_BlankSong_Bin := {} + {}", FIRST_BLOCK, ExportedBinFile::BLANK_SONG_OFFSET)?;
        writeln!(out, ".export Tad_BlankSong_SIZE = {}", ExportedBinFile::BLANK_SONG_SIZE)?;
        writeln!(out)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_DOCSTRING)?;
        writeln!(out, ";; {}", ExportedBinFile::DATA_TABLE_FOOTER_DOCSTRING)?;
        writeln!(out, "Tad_DataTable := {} + {}", FIRST_BLOCK, ExportedBinFile::DATA_TABLE_OFFSET)?;
        writeln!(out, "Tad_DataTable_SIZE = {}", bin_data.data_table_size())?;
        writeln!(out)?;
        writeln!(out, "N_DATA_ITEMS = {}", n_data_items)?;
        writeln!(out, "AUDIO_DATA_BANK = .bankbyte({FIRST_BLOCK})")?;
        writeln!(out)?;

        // Add an assert to ensure TAD_IO_VERSION in the audio driver matches the one is `tad-audio.s`
        writeln!(out, ".import TAD_IO_VERSION")?;
        writeln!(out, ".assert TAD_IO_VERSION = {}, lderror, \"TAD_IO_VERSION in audio driver does not match TAD_IO_VERSION in tad-audio.s\"", TAD_IO_VERSION)?;
        writeln!(out)?;

        for block_number in 0..n_banks {
            writeln!(out, "\n.segment \"{}{}\"", memory_map.segment_prefix, memory_map.first_segment_number + block_number)?;

            let incbin_offset = block_number * bank_size;
            let remaining_bytes = bin_data.data().len() - incbin_offset;
            assert!(remaining_bytes > 0);

            if remaining_bytes > bank_size {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .incbin \"{incbin_path}\", ${incbin_offset:x}, ${bank_size:x}")?;
            } else {
                writeln!(out, "  {BLOCK_PREFIX}{block_number}: .incbin \"{incbin_path}\", ${incbin_offset:x}")?;
                writeln!(out, "  .assert .sizeof({BLOCK_PREFIX}{block_number}) = ${remaining_bytes:x}, error, \"{incbin_path} file size does not match binary size in the assembly file\"")?;
            }
        }
        writeln!(out)?;

        let load_audio_data_size = match memory_map.mode {
            MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM_SIZE,
            MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM_SIZE,
        };

        if (bin_data.data().len() + load_audio_data_size) / bank_size > n_banks {
            writeln!(out, "\n.segment \"{}{}\"", memory_map.segment_prefix, memory_map.first_segment_number + n_banks)?;
        }

        out += match memory_map.mode {
            MemoryMapMode::LoRom => LOAD_AUDIO_DATA_LOROM,
            MemoryMapMode::HiRom => LOAD_AUDIO_DATA_HIROM,
        };
        writeln!(out, ".assert .sizeof(LoadAudioData) = {load_audio_data_size}, error")?;
        writeln!(out)?;

        for i in 0..n_banks {
            writeln!(
                out,
                ".assert .loword({BLOCK_PREFIX}{i}) = ${bank_start:04x}, lderror, \"{BLOCK_PREFIX}{i} does not start at the beginning of a {bank_name} bank (${bank_start:04x})\""
            )?;
        }
        writeln!(out)?;

        for i in 1..n_banks {
            writeln!(
                out,
                ".assert .bankbyte({BLOCK_PREFIX}{i}) = .bankbyte({FIRST_BLOCK}) + {i}, lderror, \"{}{} segment must point to the bank immediatly after {}{}\"",
                memory_map.segment_prefix,
                memory_map.first_segment_number + i,
                memory_map.segment_prefix,
                memory_map.first_segment_number + i - 1
            )?;
        }
        if n_banks > 1 {
            writeln!(out)?;
        }

        writeln!(out, ".assert .bankbyte(Tad_DataTable) = .bankbyte(Tad_DataTable + Tad_DataTable_SIZE), lderror, \"Tad_DataTable does not fit in a single bank\"")?;

        Ok(out)
    }
}

const BLOCK_PREFIX: &str = "__Tad_AudioData_";
const FIRST_BLOCK: &str = "__Tad_AudioData_0";

// Licensing the LoadSongData under The Unlicense so anyone is free edit and use it without worrying about licensing.

// SPDX-SnippetBegin

// SDPX—SnippetName: Terrific Audio Driver ca65 LoadAudioData callback
// SPDX-License-Identifier: Unlicense

const ASM_HEADER: &str = r##";; Terrific Audio Driver LoadSongData and audio driver/data .incbin statements.
;;
;; This file is automatically generated by `tad-compiler ca65-export`.
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


.setcpu "65816"

.export LoadAudioData: Far

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
.a8
.i16
;; DB unknown
.proc LoadAudioData
    .assert .not .defined(HIROM), error, "HIROM is defined in code designed for LOROM"

    .assert N_DATA_ITEMS > 1, error, "No common audio data"

    cmp     #N_DATA_ITEMS
    bcc     @ValidInput
        ; return false
        clc
        rtl
@ValidInput:

    rep     #$30
.a16
    and     #$ff

    pha

    asl
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda     f:Tad_DataTable+3,x
    sec
    sbc     f:Tad_DataTable,x
    tay

    lda     f:Tad_DataTable,x
    cmp     #$8000
    ora     #$8000
    sta     1,s

    ; carry = bit 0 of bank byte

    sep     #$20
.a8
    lda     f:Tad_DataTable+2,x
    rol
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
.a8
.i16
;; DB unknown
.proc LoadAudioData
    .assert .not .defined(LOROM), error, "LOROM is defined in code designed for HIROM"

    .assert N_DATA_ITEMS > 1, error, "No common audio data"

    cmp     #N_DATA_ITEMS
    bcc     @ValidInput
        ; return false
        clc
        rtl
@ValidInput:

    rep     #$30
.a16
    and     #$ff

    pha

    asl
    ; carry clear
    adc     1,s
    tax

    ; Calculate data size
    ; ASSUMES data size > 0 and <= $ffff
    lda     f:Tad_DataTable+3,x
    sec
    sbc     f:Tad_DataTable,x
    tay

    lda     f:Tad_DataTable,x
    sta     1,s

    sep     #$21
.a8
    lda     f:Tad_DataTable+2,x
    ; carry set
    adc     #(AUDIO_DATA_BANK - 1) & $ff

    plx

    sec
    rtl
.endproc

"##;

// SPDX-SnippetEnd
