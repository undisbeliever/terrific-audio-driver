;; asar example ROM

; SPDX-FileCopyrightText: © 2025 Marcus Rowe <undisbeliever@gmail.com>
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


; Zeropage variables
org $00
    joypadCurrent: skip 2
    joypadPressed: skip 2

    ; TadSfxQueue can be in zeropage
    TadSfxQueue.sfx: skip 1
    TadSfxQueue.pan: skip 1

    assert pc() < $100


; Lowram variables
org $7e0100
    ; `TadFlags` bitfield
    ; (see `TadFlags`)
    Tad.flags: skip 1

    ; Mono/Stereo/Surround audio mode
    ; (`TadAudioMode` enum)
    Tad.audioMode: skip 1


    TadPrivate.state: skip 1
    TadPrivate.bytesToTransferPerFrame: skip 2
    TadPrivate.previousCommand: skip 1

    TadPrivate.dataToTransfer_addr: skip 2
    TadPrivate.dataToTransfer_bank: skip 1
    TadPrivate.dataToTransfer_size: skip 2
    TadPrivate.dataToTransfer_prevSpinLock: skip 1

    TadPrivate.nextSong: skip 1

    TadPrivate.nextCommand_id: skip 1
    TadPrivate.nextCommand_parameter0: skip 1
    TadPrivate.nextCommand_parameter1: skip 1

    assert pc() < $7ef180


; Stack
; =====
org $1f80
    STACK_BOTTOM:   skip $80
    STACK_TOP:

    assert pc() == $2000



