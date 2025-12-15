; tad-constants.inc
; =================
;
; Terrific Audio Driver 65816 API public constants.
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


; MUST match `audio-driver/src/io-commands.wiz`
; enum TadCommand
    !TadCommand_PAUSE = 0
    !TadCommand_PAUSE_MUSIC_PLAY_SFX = 2
    !TadCommand_UNPAUSE = 4
    !TadCommand_PLAY_SOUND_EFFECT = 6
    !TadCommand_STOP_SOUND_EFFECTS = 8
    !TadCommand_SET_MAIN_VOLUME = 10
    !TadCommand_SET_MUSIC_CHANNELS = 12
    !TadCommand_SET_SONG_TIMER = 14
    !TadCommand_SET_GLOBAL_MUSIC_VOLUME = 16
    !TadCommand_SET_GLOBAL_SFX_VOLUME = 18
    !TadCommand_SET_GLOBAL_VOLUMES = 20


; Maximum pan value (100% to the right)
!TAD_MAX_PAN = 128

; Center pan value (default if pan is not specified)
!TAD_CENTER_PAN = !TAD_MAX_PAN/2

; Minimum tick clock value for the `TadCommand::SET_SONG_TIMER` command.
!TAD_MIN_TICK_CLOCK = 64



; TadFlags namespace
    !TadFlags_RELOAD_COMMON_AUDIO_DATA = $80
    !TadFlags_PLAY_SONG_IMMEDIATELY = $40
    !TadFlags_RESET_GLOBAL_VOLUMES_ON_SONG_START = $20

    ; A mask for the flags that are sent to the loader
    !TadFlags__ALL_FLAGS = $e0


; enum TadAudioMode
    !TadAudioMode_MONO = 0
    !TadAudioMode_STEREO = 1
    !TadAudioMode_SURROUND = 2

!TAD_N_AUDIO_MODES = 3


; Default values
; ==============

if not(defined("TAD_DEFAULT_FLAGS"))
    ; Default TAD flags
    ; MUST NOT set RELOAD_COMMON_AUDIO_DATA
    !TAD_DEFAULT_FLAGS = !TadFlags_PLAY_SONG_IMMEDIATELY
endif

if not(defined("TAD_DEFAULT_AUDIO_MODE"))
    ; Starting audio mode
    !TAD_DEFAULT_AUDIO_MODE = !TadAudioMode_MONO
endif

if not(defined("TAD_DEFAULT_TRANSFER_PER_FRAME"))
    ; Default number of bytes to transfer to Audio-RAM per `Process` call.
    ;
    ; MUST be between !TAD_MIN_TRANSFER_PER_FRAME and !TAD_MAX_TRANSFER_PER_FRAME
    !TAD_DEFAULT_TRANSFER_PER_FRAME = 256
endif

