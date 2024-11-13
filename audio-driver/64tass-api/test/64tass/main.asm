; Test to confirm the ca65 and 64tass API is identical (64tass side)

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


.cpu "65816"


; Memory map

* = 0                   ; reset ROM offset
.logical $808000        ; start address
    .dsection Code
        .cerror * > $80ffff, "Bank 80 overflow"
.here


; RAM map
START_TOP    = $1fff
STACK_BOTTOM = $1f80

* = $0000
.dsection Zeropage
    .cerror * > $100, "zeropage overflow"

* = $7e0200
.dsection Lowram
    .cerror * > $7e2000, "lowram section overflow"


.dpage 0
.databank ?

.section Lowram
    .include "../../tad-variables-private.inc"
.send

.section Zeropage
    .include "../../tad-variables-public.inc"
.send

.section Code
    .include "../../tad-process.inc"

    .include "../../tad-code.inc"
.send


.section Code
    ; 64tass will auto-remove unreferenced `.proc` blocks
    ;
    ; Referencing them in this function table will unsure they are included in the output ROM
    TadSubroutines:
        .long Tad_Init
        .long Tad_Process
        .long Tad_FinishLoadingData
        .long Tad_QueueCommand
        .long Tad_QueueCommandOverride
        .long Tad_QueuePannedSoundEffect
        .long Tad_QueueSoundEffect
        .long Tad_LoadSong
        .long Tad_LoadSongIfChanged
        .long Tad_GetSong
        .long Tad_ReloadCommonAudioData
        .long Tad_SetMono
        .long Tad_SetStereo
        .long Tad_GetStereoFlag
        .long Tad_SongsStartImmediately
        .long Tad_SongsStartPaused
        .long Tad_SetTransferSize
        .long Tad_IsLoaderActive
        .long Tad_IsSongLoaded
        .long Tad_IsSfxPlaying
        .long Tad_IsSongPlaying
.send


Tad_Loader_Bin          =   $1234
Tad_BlankSong_Bin       =   $5678
Tad_AudioDriver_Bin     =   $9abc
LoadAudioData           = $80def0

Tad_Loader_SIZE         =  116
Tad_AudioDriver_SIZE    = 2048
Tad_BlankSong_SIZE      =   29


