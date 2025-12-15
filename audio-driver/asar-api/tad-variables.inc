; tad-variables.inc
; =================
;
; Terrific Audio Driver 65816 API variables.
;
;
; This file is is an asar port from the Terrific Audio Driver bass API.
;
; SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
; SPDX-License-Identifier: Zlib
;
; Copyright © 2024 Marcus Rowe <undisbeliever@gmail.com>
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


; ================
; Public variables
; ================

; Must be in lowram
struct Tad $7e0200
    ; `TadFlags` bitfield
    ; (see `TadFlags`)
    .flags: skip 1

    ; Mono/Stereo/Surround audio mode
    ; (`TadAudioMode` enum)
    .audioMode: skip 1
endstruct


; ---------------------------------------
; Queue 4 - The next sound effect to play
; ---------------------------------------

; should be zeropage, must be in lowram
struct TadSfxQueue $00
    .sfx: skip 1
    .pan: skip 1
endstruct



; =================
; Private Variables
; =================

; Must be in lowram
struct TadPrivate $7e0202
    ; The current audio driver state
    ; (`TadState` enum)
    .state: skip 1

    ; Number of bytes to transfer per `Process` call
    ;
    ; MUST be > 0
    .bytesToTransferPerFrame: skip 2

    ; The previous `TadIO_ToScpu.COMMAND_PORT` sent to the S-SMP audio driver.
    .previousCommand: skip 1


; ---------------------------------------------------
; Queue 1 - remaining data to transfer into Audio-RAM
; ---------------------------------------------------
    ; A far pointer to the remaining data to transfer
    .dataToTransfer_addr: skip 2
    .dataToTransfer_bank: skip 1

    ; The remaining number of bytes to transfer
    .dataToTransfer_size: skip 2

    ; The previous value written to the loader spinLock
    .dataToTransfer_prevSpinLock: skip 1


; ----------------------------------------------
; Queue 2 - The next song to load into Audio-RAM
; ----------------------------------------------
    ; The next song to load into Audio-RAM
    ; Used by the `WAITING_FOR_LOADER_*` states
    ; If this value is 0 or an invalid song, a blank silent song will be loaded instead.
    .nextSong: skip 1


; ------------------------------------------------------
; Queue 3 - The next command to send to the audio driver
; ------------------------------------------------------
    ; The next `TadCommand` to send to the audio driver.
    ; If this value is negative, the queue is empty.
    .nextCommand_id: skip 1

    ; The two parameters of the next command (if any)
    .nextCommand_parameter0: skip 1
    .nextCommand_parameter1: skip 1
endstruct

