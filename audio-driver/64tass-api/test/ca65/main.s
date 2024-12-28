; Test to confirm the ca65 and 64tass API is identical (ca65 side)

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


.include "../../../ca65-api/tad-audio.inc"


.code

; 64tass will auto-remove unreferenced `.proc` blocks
;
; Referencing them in this function table will unsure they are included in the output ROM
TadSubroutines:
    .faraddr Tad_Init
    .faraddr Tad_Process
    .faraddr Tad_FinishLoadingData
    .faraddr Tad_QueueCommand
    .faraddr Tad_QueueCommandOverride
    .faraddr Tad_QueuePannedSoundEffect
    .faraddr Tad_QueueSoundEffect
    .faraddr Tad_LoadSong
    .faraddr Tad_LoadSongIfChanged
    .faraddr Tad_GetSong
    .faraddr Tad_ReloadCommonAudioData
    .faraddr Tad_SetMono
    .faraddr Tad_SetStereo
    .faraddr Tad_GetStereoFlag
    .faraddr Tad_SongsStartImmediately
    .faraddr Tad_SongsStartPaused
    .faraddr Tad_SetTransferSize
    .faraddr Tad_IsLoaderActive
    .faraddr Tad_IsSongLoaded
    .faraddr Tad_IsSfxPlaying
    .faraddr Tad_IsSongPlaying


.export Tad_Loader_Bin          =   $1234
.export Tad_BlankSong_Bin       =   $5678
.export Tad_AudioDriver_Bin     =   $9abc
.export LoadAudioData           = $80def0

.export Tad_Loader_SIZE         =  116
.export Tad_AudioDriver_SIZE    = 2048
.export Tad_BlankSong_SIZE      =   30


