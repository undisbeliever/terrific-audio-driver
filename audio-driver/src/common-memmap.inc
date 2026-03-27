; Common addresses, sizes and banks used by the loader and the audio driver.
;
; SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
; SPDX-License-Identifier: Zlib
;
; Copyright © 2023 Marcus Rowe <undisbeliever@gmail.com>
;
; This software is provided 'as-is', without any express or implied warranty.  In
; no event will the authors be held liable for any damages arising from the use of
; this software.
;
; Permission is granted to anyone to use this software for any purpose, including
; commercial applications, and to alter it and redistribute it freely, subject to
; the following restrictions:
;
;      1. The origin of this software must not be misrepresented; you must not
;         claim that you wrote the original software. If you use this software in
;         a product, an acknowledgment in the product documentation would be
;         appreciated but is not required.
;
;      2. Altered source versions must be plainly marked as such, and must not be
;         misrepresented as being the original software.
;
;      3. This notice may not be removed or altered from any source distribution.


; LOADER_ADDR must match LOADER_ADDR in the 65816 APIs
LOADER_ADDR = $200
LOADER_SIZE =  116 ; MUST be even

; ::SHOULDDO shrink stack size::
STACK_SIZE = $20
STACK_BOTTOM_ADDR = $200 - STACK_SIZE

BC_CHANNEL_STACK_SIZE = 21
BC_CHANNEL_STACK_OFFSET = 6

LAST_VAR_PAGE_SIZE = (BC_CHANNEL_STACK_SIZE + 12) * 10

; The -28 ensures the pitch-table and BRR directory is page aligned
; `COMMON_DATA_ADDR` MUST be an even address (The loader only works with even addresses).
;
; If this constant is modified, the following must also be updated:
;   * `addresses::COMMON_DATA` in `crates/compiler/src/driver_constants.rs`
;   * The Tad_AudioDriver_SIZE assert in `ca65-api/tad-audio.s`
;   * The Tad_AudioDriver_SIZE assert in `64tass-api/tad-process.inc`
COMMON_DATA_ADDR = $1100 - 28

; `CODE_ADDR` MUST be an even address (The loader only works with even addresses)
CODE_ADDR = $200 + LOADER_SIZE
CODE_END_ADDR = COMMON_DATA_ADDR - LAST_VAR_PAGE_SIZE


.varbank zeropage           $0000..$00eb
.varbank loader_zeropage    $00eb..$00f0
.varbank firstpage          $0100..STACK_BOTTOM_ADDR
.varbank stack              STACK_BOTTOM_ADDR..$0200
.varbank lastpage           CODE_END_ADDR..COMMON_DATA_ADDR


; Variables used by loader or used across song loads
.vars loader_zeropage
    ; Global volumes.
    ;
    ; Stored here so they are unchanged when a new song or common-audio-data is loaded.
    ;
    ; If global-volume is 0, the channel volume is unchanged.
    globalVolume_music : u8
    globalVolume_sfx : u8

    ; The address of the song data
    songPtr :  ptr

    ; The `LoaderDataType` value sent to the loader.
    ;
    ; Used as a Mono/Stereo flag.
    ;
    ; Set by the loader:
    ;   Advantages: Simpler and uses less code space (no IoCommand for to set/clear stereoFlag).
    ;   Disadvantages: Stereo flag is not set until a song is loaded.
    loaderDataType : u8
.endvars

