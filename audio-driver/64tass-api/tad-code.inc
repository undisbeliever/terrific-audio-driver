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

    ; SHOULD NOT USE PLAY_SOUND_EFFECT in `queue_command`
    ; (command queue only holds 1 parameter byte and PLAY_SOUND_EFFECT has 2 parameters)

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
.endblock



; IN: A = command
; IN: X = parameter
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
        sta     Tad_nextCommand_parameter

        ; return true
        sec
        rts

_ReturnFalse:
    clc
    rts
.endproc



; IN: A = command
; IN: X = parameter
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
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.WAITING_FOR_LOADER)
    .cerror !(TAD__FIRST_LOADING_SONG_STATE > TadState.LOADING_COMMON_AUDIO_DATA)

    sta     Tad_nextSong

    lda     Tad_state
    cmp     #TAD__FIRST_LOADING_SONG_STATE
    bcc     +
        ; TadState is not NULL, WAITING_FOR_LOADER or LOADING_COMMON_AUDIO_DATA

        ; Send a *switch-to-loader* command to the audio-driver or loader
        lda     #TadIO_ToDriver.SWITCH_TO_LOADER
    .databank ?
        sta     TadIO_ToDriver.SWITCH_TO_LOADER_PORT
    .databank TAD_DB_LOWRAM

        lda     #TadState.WAITING_FOR_LOADER
        sta     Tad_state
    +
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
Tad_SetMono .proc
    lda     #TadFlags.STEREO
    trb     Tad_flags
    rts
.endproc


.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_SetStereo .proc
    lda     #TadFlags.STEREO
    tsb     Tad_flags
    rts
.endproc


; OUT: carry set if stereo
.as
; I unknown
.databank TAD_DB_LOWRAM
Tad_GetStereoFlag .proc
    .cerror !(TadFlags.STEREO == $80)
    lda     Tad_flags
    cmp     #TadFlags.STEREO
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

