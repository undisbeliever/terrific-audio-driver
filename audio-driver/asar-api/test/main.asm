; Test to confirm the ca65 and asar API is identical (asar side)

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

incsrc "../tad-variables.inc"
incsrc "../tad-constants.inc"

lorom
org $808000
    incsrc "../tad-process.inc"
    incsrc "../tad-control.inc"


; 64tass will auto-remove unreferenced `.proc` blocks
;
; Referencing them in this function table will unsure they are included in the output ROM
TadSubroutines:
    dl  Tad_Init
    dl  Tad_Process
    dl  Tad_FinishLoadingData
    dl  Tad_QueueCommand
    dl  Tad_QueueCommandOverride
    dl  Tad_QueuePannedSoundEffect
    dl  Tad_QueueSoundEffect
    dl  Tad_LoadSong
    dl  Tad_LoadSongIfChanged
    dl  Tad_GetSong
    dl  Tad_ReloadCommonAudioData
    dl  Tad_SongsStartImmediately
    dl  Tad_SongsStartPaused
    dl  Tad_GlobalVolumesResetOnSongStart
    dl  Tad_GlobalVolumesPersist
    dl  Tad_SetTransferSize
    dl  Tad_IsLoaderActive
    dl  Tad_IsSongLoaded
    dl  Tad_IsSfxPlaying
    dl  Tad_IsSongPlaying

TadConstants:
    dw  !TAD_MAX_PAN
    dw  !TAD_CENTER_PAN
    dw  !TAD_MIN_TICK_CLOCK

    db  !TadCommand_PAUSE
    db  !TadCommand_PAUSE_MUSIC_PLAY_SFX
    db  !TadCommand_PLAY_SOUND_EFFECT
    db  !TadCommand_STOP_SOUND_EFFECTS
    db  !TadCommand_SET_MAIN_VOLUME
    db  !TadCommand_SET_MUSIC_CHANNELS
    db  !TadCommand_SET_SONG_TIMER
    db  !TadCommand_SET_GLOBAL_MUSIC_VOLUME
    db  !TadCommand_SET_GLOBAL_SFX_VOLUME
    db  !TadCommand_SET_GLOBAL_VOLUMES

    db  !TadAudioMode_MONO
    db  !TadAudioMode_STEREO
    db  !TadAudioMode_SURROUND

    db  !TadFlags_RELOAD_COMMON_AUDIO_DATA
    db  !TadFlags_PLAY_SONG_IMMEDIATELY
    db  !TadFlags_RESET_GLOBAL_VOLUMES_ON_SONG_START


Tad_Loader_Bin          =   $1234
Tad_AudioDriver_Bin     =   $9abc
LoadAudioData           = $80def0

Tad_Loader_SIZE         =  116
Tad_AudioDriver_SIZE    = 2048


