; asar example ROM

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


arch 65816
optimize dp always
optimize address mirrors


; Setup TAD defines

!LOROM = 1

; Testing custom default values
!TAD_DEFAULT_FLAGS               = !TadFlags_PLAY_SONG_IMMEDIATELY|!TadFlags_RESET_GLOBAL_VOLUMES_ON_SONG_START
!TAD_DEFAULT_AUDIO_MODE          = !TadAudioMode_SURROUND
!TAD_DEFAULT_TRANSFER_PER_FRAME  = 700


incsrc "registers.inc"
incsrc "variables.inc"
incsrc "../../tad-constants.inc"
incsrc "../gen/audio.inc"


lorom

; Pad ROM to 256KiB
org $87ffff
    db 0


; Bank $80
org $808000
    incsrc "reset.inc"
    incsrc "break-isr.inc"
    incsrc "main.inc"

    dpbase 0
    bank $80
    incsrc "../../tad-process.inc"
    incsrc "../../tad-control.inc"

    assert pc() < $80ffb0


org $80ffb0
    incsrc "snes-header.inc"


org $818000
    incsrc "../gen/audio-data-lorom.inc"

