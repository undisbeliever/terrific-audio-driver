; TAD 64tass API subroutines

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



; =========
; Constants
; =========

; Minimum tick clock value for the `TadCommand::SET_SONG_TEMPO` command.
TAD_MIN_TICK_CLOCK = 64



; Terrific Audio Driver IO commands
;
; MUST match `audio-driver/src/io-commands.wiz`
TadCommand .block
    ; Pause the song and sound effect channels.
    ; * IO commands will still be executed when the audio-driver is paused.
    ; * The audio driver starts paused unless the `LoaderDataType.PLAY_SONG_BIT` is set.
    PAUSE = 0

    ; Pauses the music channels, sound effects will continue to play.
    ; Unpauses the sound effects channels.
    PAUSE_MUSIC_PLAY_SFX = 2

    ; Unpauses the music and sound effect channels.
    UNPAUSE = 4

    ; Play a sound effect
    ;  * parameter0: sound effect id
    ;  * parameter1: pan (0-128) (not bounds checked)
    ;
    ; The sound-effect might not play if both sound effect channels are active.
    ; See the Sound Effects documentation for more details
    ;
    ; CAUTION: a PLAY_SOUND_EFFECT command has a higher priority than the sound-effect queue.
    PLAY_SOUND_EFFECT = 6

    ; Stop all active sound effects
    STOP_SOUND_EFFECTS = 8

    ; Set the main volume
    ;  * parameter0: signed i8 volume
    ;
    ; Disabled channels will be keyed-off.
    ;
    ; NOTE: The main volume is reset whenever a new song is loaded.
    SET_MAIN_VOLUME = 10

    ; Enables or disables music channels.
    ;  * parameter0: A bitmask of the 8 music channels that can send key-on events
    ;
    ; NOTE: The music channels bitmask is reset whenever a new song is loaded.
    SET_MUSIC_CHANNELS = 12

    ; Set the song tempo.
    ;  * parameter0: The new S-DSP TIMER_0 register value
    ;    (MUST be >= TAD_MIN_TICK_CLOCK 64, is bounds checked)
    ;
    ; NOTE: The song can still change the tempo.
    SET_SONG_TEMPO = 14

    ; Set the global music volume
    ;  * parameter0: the music volume.
    ;    A 255 music-volume will not modify the channel volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    SET_GLOBAL_MUSIC_VOLUME = 16

    ; Set the global sound-effect volume
    ;  * parameter0: the music volume.
    ;    A 255 sfx-volume will not modify the channel volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    SET_GLOBAL_SFX_VOLUME = 18

    ; Set the global music and sound-effect volumes
    ;  * parameter0: the music volume.
    ;  * parameter1: the sound-effect volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    SET_GLOBAL_VOLUMES = 20
.endblock



; IN: A = command
; IN: X = first parameter
; IN: Y = second parameter
; OUT: Carry set if command added to queue
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_QueueCommand .proc
    bit     Tad_nextCommand_id
    bpl     _ReturnFalse
        ; command queue is empty
    _WriteCommand:
        and     #TadIO_ToDriver.COMMAND_MASK
        sta     Tad_nextCommand_id

        txa
        sta     Tad_nextCommand_parameter0

        tya
        sta     Tad_nextCommand_parameter1

        ; return true
        sec
        rts

_ReturnFalse:
    clc
    rts
.endproc



; IN: A = command
; IN: X = first parameter
; IN: Y = second parameter
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_QueueCommandOverride := Tad_QueueCommand._WriteCommand



; IN: A = sfx id
; IN: X = pan
.as
; I unknown
.databank TAD_DB_LOWRAM
; KEEP: X, Y
Tad_QueuePannedSoundEffect .proc
    cmp     Tad_sfxQueue_sfx
    bcs     _EndIf
        sta     Tad_sfxQueue_sfx

        txa
        sta     Tad_sfxQueue_pan

_EndIf:
    rts
.endproc



; IN: A = sfx_id
.as
; I unknown
.databank TAD_DB_LOWRAM
; KEEP: X, Y
Tad_QueueSoundEffect .proc
    cmp     Tad_sfxQueue_sfx
    bcs     _EndIf
        sta     Tad_sfxQueue_sfx

        lda     #TAD_CENTER_PAN
        sta     Tad_sfxQueue_pan
_EndIf:
    rts
.endproc



; IN: A = song_id
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_LoadSong .proc
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.NULL)
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.WAITING_FOR_LOADER_COMMON)
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.WAITING_FOR_LOADER_SONG)
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.LOADING_COMMON_AUDIO_DATA)

    sta     Tad_nextSong

    lda     #TadFlags.RELOAD_COMMON_AUDIO_DATA
    trb     Tad_flags
    beq     _SongRequested
        ; Common audio data requested
        lda     #TadState.WAITING_FOR_LOADER_COMMON
        bra     _SetStateAndSwitchToLoader

_SongRequested:
    lda     Tad_state
    cmp     #TAD__FIRST_LOADING_SONG_STATE
    bcc     _Return
        ; TadState is not NULL, WAITING_FOR_LOADER_* or LOADING_COMMON_AUDIO_DATA

        lda     #TadState.WAITING_FOR_LOADER_SONG

    _SetStateAndSwitchToLoader:
        sta     Tad_state

        ; Assert it is safe to send a switch-to-loader command when the loader is waiting for a READY signal
        .cerror !(TadIO_ToDriver.SWITCH_TO_LOADER != TadIO_Loader_Init.LOADER_READY_H)
        .cerror !(TadIO_ToDriver.SWITCH_TO_LOADER_PORT == TadIO_Loader_Init.READY_PORT_H)

        ; Send a *switch-to-loader* command to the audio-driver or loader
        lda     #TadIO_ToDriver.SWITCH_TO_LOADER
    .databank ?
        sta     TadIO_ToDriver.SWITCH_TO_LOADER_PORT
    .databank TAD_DB_LOWRAM
_Return:
    rts
.endproc



; IN: A = song_id
; OUT: carry set if `Tad_LoadSong` was called
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_LoadSongIfChanged .proc
    cmp     Tad_nextSong
    beq     +
        jsr     Tad_LoadSong
        sec
        rts
    +
    clc
    rts
.endproc


; OUT: A = The song_id used in the last `Tad_LoadSong` call.
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_GetSong .proc
    ; `Tad_nextSong` is only written to in `Tad_Init` and `Tad_LoadSong`.
    lda     Tad_nextSong
    rts
.endproc


.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_ReloadCommonAudioData .proc
    lda     #TadFlags.RELOAD_COMMON_AUDIO_DATA
    tsb     Tad_flags
    rts
.endproc


.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_SongsStartImmediately .proc
    lda     #TadFlags.PLAY_SONG_IMMEDIATELY
    tsb     Tad_flags
rts
.endproc



.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_SongsStartPaused .proc
    lda     #TadFlags.PLAY_SONG_IMMEDIATELY
    trb     Tad_flags
    rts
.endproc


; IN: X = new `Tad_bytesToTransferPerFrame` value
; A unknown
.xl
.databank TAD_DB_LOWRAM
Tad_SetTransferSize .proc
    cpx     #TAD_MAX_TRANSFER_PER_FRAME
    bcc     +
        ldx     #TAD_MAX_TRANSFER_PER_FRAME
    +
    cpx     #TAD_MIN_TRANSFER_PER_FRAME
    bcs     +
        ldx     #TAD_MIN_TRANSFER_PER_FRAME
    +
    stx     Tad_bytesToTransferPerFrame

    rts
.endproc



; OUT: carry set if state is LOADING_*
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_IsLoaderActive .proc
    #TadPrivate_IsLoaderActive
    rts
.endproc



; OUT: carry set if state is PAUSED, PLAYING_SFX or PLAYING
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_IsSongLoaded .proc
    .cerror !(TadState.PLAYING_SFX > TadState.PAUSED)
    .cerror !(TadState.PLAYING > TadState.PAUSED)
    ; Assumes PLAYING is the last state

    lda     Tad_state
    cmp     #TadState.PAUSED
    rts
.endproc



; OUT: carry set if state is PLAYING_SFX or PLAYING
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_IsSfxPlaying .proc
    .cerror !(TadState.PLAYING > TadState.PLAYING_SFX)
    ; Assumes PLAYING is the last state

    lda     Tad_state
    cmp     #TadState.PLAYING_SFX
    rts
.endproc



; OUT: carry set if state is PLAYING
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_IsSongPlaying .proc
    ; Assumes PLAYING is the last state

    lda     Tad_state
    cmp     #TadState.PLAYING
    rts
.endproc

