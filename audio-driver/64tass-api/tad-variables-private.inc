; TAD 64tass API private variables
;
; These variables MUST NOT be modified outside of the TAD API
;
; These variables must be placed somewhere in the lowram segment
; ($0000 - $1fff or $7e0000-$7e1ffff).
;
; You do not need to place these variables in zeropage.


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


.cerror (* >= $2000) && ((* < $7e0000) || (* >= $7e2000)), "tad-variables-private.inc is not in lowram"


; Minimum transfer size accepted by `Tad_SetTransferSize`
;
; MUST BE > 0
TAD_MIN_TRANSFER_PER_FRAME = 32

; Maximum transfer size accepted by `Tad_SetTransferSize`
;
; The loader can transfer ~849 bytes per 60Hz frame SlowROM or FastROM
TAD_MAX_TRANSFER_PER_FRAME = 800

; Default number of bytes to transfer to Audio-RAM per `Tad_Process` call.
;
; MUST BE > 0
TAD_DEFAULT_TRANSFER_PER_FRAME = 256


TadState .block
    NULL                                = $00
    ; Waiting for loader to send the ready signal.
    WAITING_FOR_LOADER                  = $7c
    ; Loading common audio data.
    LOADING_COMMON_AUDIO_DATA           = $7d
    ; Loading a song and the TadLoaderDataType.PLAY_SONG_FLAG was clear.
    LOADING_SONG_DATA_PAUSED            = $7e
    ; Loading a song and the TadLoaderDataType.PLAY_SONG_FLAG was set.
    LOADING_SONG_DATA_PLAY              = $7f
    ; Song is loaded into Audio-RAM and the audio driver is paused.
    ; No play-sound-effect commands will be sent when the driver is paused.
    PAUSED                              = $80
    ; Song is loaded into Audio-RAM and the audio driver is playing sfx (song paused).
    PLAYING_SFX                         = $81
    ; Song is loaded into Audio-RAM and the audio driver is playing the song.
    PLAYING                             = $82
.endblock
TAD__FIRST_LOADING_STATE      = TadState.LOADING_COMMON_AUDIO_DATA
TAD__FIRST_LOADING_SONG_STATE = TadState.LOADING_SONG_DATA_PAUSED


TadFlags .block 
    ; The mono/stereo flag
    ;  * If set, the next song will be played in stereo.
    ;  * If clear, the next song will be played in mono.
    ; Default: Mono
    STEREO                   = TadLoaderDataType.STEREO_FLAG

    ; Determines if the song is played immediately after loading into Audio-RAM
    ;  * If set, the audio driver will play the song after the next song is loaded into Audio-RAM
    ;  * If clear, the audio driver will be paused after the next song is loaded into Audio-RAM
    ; Default: Set
    PLAY_SONG_IMMEDIATELY    = TadLoaderDataType.PLAY_SONG_FLAG

    ; If set the *common audio data* will be loaded into Audio-RAM the next time a song is requested.
    ;
    ; This flag is cleared after the *common audio data* is loaded into Audio-RAM
    RELOAD_COMMON_AUDIO_DATA = 1 << 0


    ; A mask for the flags that are sent to the loader
    _LOADER_MASK = STEREO | PLAY_SONG_IMMEDIATELY
.endblock



; The current audio driver state
; (see `TadState`)
Tad_state .byte ?

; `TadFlags` bitfield
; (see `TadFlags`)
Tad_flags .byte ?

; Number of bytes to transfer per `Tad_Process` call
;
; MUST be > 0
Tad_bytesToTransferPerFrame .word ?

; The previous `IO_ToScpu.COMMAND_PORT` sent to the S-SMP audio driver.
Tad_previousCommand .byte ?


; ---------------------------------------------------
; Queue 1 - remaining data to transfer into Audio-RAM
; ---------------------------------------------------
    ; A far pointer to the remaining data to transfer
    Tad_dataToTransfer_addr .word ?
    Tad_dataToTransfer_bank .byte ?

    ; The remaining number of bytes to transfer
    Tad_dataToTransfer_size .word ?

    ; The previous value written to the loader spinLock
    Tad_dataToTransfer_prevSpinLock .byte ?


; ----------------------------------------------
; Queue 2 - The next song to load into Audio-RAM
; ----------------------------------------------
    ; The next song to load into Audio-RAM
    ; Used by the `WAITING_FOR_LOADER` state
    ; If this value is 0 or an invalid song, a blank silent song will be loaded instead.
    Tad_nextSong .byte ?


; ------------------------------------------------------
; Queue 3 - The next command to send to the audio driver
; ------------------------------------------------------
    ; The next `Command` to send to the audio driver.
    ; MUST NOT be PLAY_SOUND_EFFECT_COMMAND.
    ; If this value is negative, the queue is empty.
    Tad_nextCommand_id .byte ?

    ; The parameter of the next next command (if any)
    Tad_nextCommand_parameter .byte ?


