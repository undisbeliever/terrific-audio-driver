;; Terrific Audio Driver ca65 API unit tests

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


.include "../../../tad-audio.inc"

.p816
.smart

.export RunTests

.import TadPrivate_nextCommand_id, TadPrivate_nextCommand_parameter0, TadPrivate_nextCommand_parameter1


.bss
    testIndex:  .res 2
    counter:    .res 2


    _queueSfxTest_id:   .res 1
    _queueSfxTest_pan:  .res 2

.code


;; Test the ca65 API
;;
;; ASSUMES: `Tad_Init` has been called.
;;
;; Breaks if a test fails
.a8
.i16
;; DB unknown
.proc RunTests
    phb

    pea     $7e22
    plb
; DB = $22
    jsr     __DoTests

    plb
; DB = $7e
    jsr     __DoTests

    plb
    rts
.endproc


; IN: X = first test
.a8
.i16
; DB = $7e or DB access registers
.proc __DoTests
    ldx     #0

    @Loop:
        stx     testIndex


        ; Reset TAD State
        ldx     #256
        jsr     Tad_SetTransferSize
        jsr     Tad_SongsStartImmediately
        ; Switch to a blank song
        lda     #0
        jsr     Tad_LoadSong
        jsr     _FinishLoading
        jsr     _Wait


        ldx     testIndex
        jsr     (.loword(TestTable),x)

        sep     #$20
        rep     #$10
    .a8
    .i16

        ldx     testIndex
        inx
        inx
        cpx     #TestTable_SIZE
        bcc     @Loop

    rts
.endproc


TestTable:
    .addr   TestFinishLoadingData
    .addr   TestFinishLoadingData2
    .addr   TestLoadSong
    .addr   TestLoadSongWhileLoaderActive
    .addr   TestLoadSongWhileLoaderActive2
    .addr   TestLoadSongWhileLoadingCommonAudioData
    .addr   TestReloadCommonAudioDataImmediatelyAfterLoadSong
    .addr   TestLoadSongRestartsLoaderIfReloadCommonAudioDataIsSet
    .addr   TestLoadSongIfChanged
    .addr   TestGetSong
    .addr   TestQueueCommand
    .addr   TestQueueCommandOverride
    .addr   TestQueueCommandIdIsMasked
    .addr   TestQueueCommandWithTwoParameters_i16
    .addr   TestQueueCommandWithTwoParameters_i8
    .addr   TestPauseCommand1
    .addr   TestPauseCommand2
    .addr   TestPauseCommand3
    .addr   TestPauseMusicPlaySfxCommand1
    .addr   TestPauseMusicPlaySfxCommand2
    .addr   TestPauseMusicPlaySfxCommand3
    .addr   TestUnpauseCommand1
    .addr   TestUnpauseCommand2
    .addr   TestUnpauseCommand3
    .addr   TestQueuePannedSoundEffect
    .addr   TestQueueSoundEffect
    .addr   TestSfxQueueAfterLoadSong
    .addr   TestSfxQueueAfterPlaySoundEffectCommand
    .addr   TestSfxQueueWithOnlyMusicPaused
    .addr   TestCommandAndSfxQueuePriority
    .addr   TestCommandAndSfxQueueEmptyAfterSongLoad
    .addr   TestQueuePannedSoundEffectKeepsXY16
    .addr   TestQueuePannedSoundEffectKeepsXY8
    .addr   TestQueueSoundEffectKeepsXY16
    .addr   TestQueueSoundEffectKeepsXY8
    .addr   TestSongStartsImmediately
    .addr   TestSongStartPaused
    .addr   TestSetTransferSize
    .addr   TestFlagFunctions
TestTable_SIZE = * - TestTable


.macro assert_u8_var_eq var1, var2
    .assert .asize = 8, error

    .local @Pass
        pha

        lda     var1
        cmp     var2
        beq     @Pass
            brk     0
    @Pass:
        pla
.endmacro


.macro assert_a_eq value
    .local @Pass
        cmp     #value
        beq     @Pass
            brk     0
    @Pass:
.endmacro


.macro assert_x_eq value
    .local @Pass
        cpx     #value
        beq     @Pass
            brk     0
    @Pass:
.endmacro


.macro assert_y_eq value
    .local @Pass
        cpy     #value
        beq     @Pass
            brk     0
    @Pass:
.endmacro


.macro assert_carry subroutine, carry_value
    .local @Pass

    .if .xmatch({carry_value}, true)
        jsr     subroutine
        bcs     @Pass
    .elseif .xmatch({carry_value}, false)
        jsr     subroutine
        bcc     @Pass
    .else
        .error "Unknown assert value"
    .endif
        ; Branch not taken
        ; Assert failed
        brk     0
    @Pass:
.endmacro


; Also tests `Tad_ReloadCommonAudioData`
.a8
.i16
;; DB access lowram
.proc TestFinishLoadingData
    lda     #1
    jsr     Tad_LoadSong

    jsr     _WaitForLoader

    jsl     Tad_FinishLoadingData

    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded,  true

    rts
.endproc


; Also tests `Tad_ReloadCommonAudioData`
.a8
.i16
;; DB access lowram
.proc TestFinishLoadingData2
    jsr     Tad_ReloadCommonAudioData

    lda     #1
    jsr     Tad_LoadSong

    jsr     _WaitForLoader

    jsl     Tad_FinishLoadingData

    ; Common audio data loaded
    ; State = WAITING_FOR_LOADER
    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded,  false

    jsr     _WaitForLoader

    jsl     Tad_FinishLoadingData

    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded,  true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestLoadSong
    assert_carry  Tad_IsSongLoaded,  true

    lda     #0
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded,  false

    jsr     _WaitForLoader

    ; Loading blank song, it should only require 1 Tad_Process call to load it
    jsl     Tad_Process
    assert_carry  Tad_IsSongLoaded,   true
    assert_carry  Tad_IsLoaderActive, false

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestLoadSongWhileLoaderActive
    assert_carry  Tad_IsSongLoaded,  true

    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded,  false

    jsr     _WaitForLoader
    jsl     Tad_Process
    jsl     Tad_Process
    jsl     Tad_Process

    assert_carry  Tad_IsLoaderActive, true

    lda     #0
    jsr     Tad_LoadSong

    assert_carry  Tad_IsLoaderActive, false

    jsr     _WaitForLoader

    assert_carry  Tad_IsLoaderActive, true

    jsr     _FinishLoading

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestLoadSongWhileLoaderActive2
    assert_carry  Tad_IsSongLoaded,  true

    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded,  false

    jsr     _Wait
    lda     #0
    jsr     Tad_LoadSong

    jsr     _Wait
    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded,  false

    jsr     _WaitForLoader
    jsr     _FinishLoading

    rts
.endproc


;; Tests that `Tad_LoadSong` does not restart the loader if the loader is loading common audio data.
.a8
.i16
;; DB access lowram
.proc TestLoadSongWhileLoadingCommonAudioData

    jsr     Tad_ReloadCommonAudioData

    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded,  false
    assert_carry  Tad_IsLoaderActive, false

    jsr     _WaitForLoader
    jsl     Tad_Process
    jsl     Tad_Process
    jsl     Tad_Process

    assert_carry  Tad_IsLoaderActive, true

    lda     #1
    jsr     Tad_LoadSong

    ; Test `Tad_LoadSong` did not switch to `WAITING_FOR_LOADER` state
    assert_carry  Tad_IsLoaderActive, true

    rts
.endproc


;; Tests that `Tad_Process` does not clear the RELOAD_COMMON_AUDIO_DATA flag.
.a8
.i16
;; DB access lowram
.proc TestReloadCommonAudioDataImmediatelyAfterLoadSong

    jsr     Tad_ReloadCommonAudioData

    lda     #1
    jsr     Tad_LoadSong
    ; RELOAD_COMMON_AUDIO_DATA flag is cleared by Tad_LoadSong

    jsr     Tad_ReloadCommonAudioData


    jsr     _FinishLoading

    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded, true


    ; RELOAD_COMMON_AUDIO_DATA flag is set
    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded, false
    assert_carry  Tad_IsLoaderActive, false
    jsr     _WaitForLoader
    jsl     Tad_FinishLoadingData

    ; Test the loader is waiting for song data
    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded, false

    jsr     _WaitForLoader
    jsl     Tad_FinishLoadingData
    assert_carry  Tad_IsSongLoaded, true

    rts
.endproc


;; Tests that `Tad_LoadSong` restarts the loader if RELOAD_COMMON_AUDIO_DATA flag is set
.a8
.i16
;; DB access lowram
.proc TestLoadSongRestartsLoaderIfReloadCommonAudioDataIsSet

    assert_carry  Tad_IsSongLoaded,  true

    lda     #1
    jsr     Tad_LoadSong

    assert_carry  Tad_IsSongLoaded, false

    jsr     _WaitForLoader
    jsl     Tad_Process
    jsl     Tad_Process
    jsl     Tad_Process

    assert_carry  Tad_IsLoaderActive, true


    ; Reload CAD in the middle of loading a song
    jsr     Tad_ReloadCommonAudioData
    lda     #1
    jsr     Tad_LoadSong
    assert_carry  Tad_IsLoaderActive, false


    jsr     _WaitForLoader
    jsl     Tad_Process
    jsl     Tad_Process

    assert_carry  Tad_IsLoaderActive, true

    ; Test loader still active when loading song data while loader is loading CAD
    lda     #1
    jsr     Tad_LoadSong
    assert_carry  Tad_IsLoaderActive, true


    ; Test that the loader is reset if `RELOAD_COMMON_AUDIO_DATA` is set on a `Tad_LoadSong` call
    jsr     Tad_ReloadCommonAudioData
    lda     #1
    jsr     Tad_LoadSong
    assert_carry  Tad_IsLoaderActive, false


    ; Test `Tad_LoadSong` works when loader is waiting for the ready byte
    jsr     _Wait
    jsr     _Wait
    jsr     _Wait
    assert_carry  Tad_IsLoaderActive, false

    jsr     Tad_ReloadCommonAudioData
    lda     #1
    jsr     Tad_LoadSong
    assert_carry  Tad_IsSongLoaded, false
    assert_carry  Tad_IsLoaderActive, false

    ; Finish loading the CAD
    jsr     _WaitForLoader
    jsl     Tad_FinishLoadingData

    ; Test the loader is waiting for song data
    assert_carry  Tad_IsLoaderActive, false
    assert_carry  Tad_IsSongLoaded, false

    jsr     _WaitForLoader
    jsl     Tad_FinishLoadingData
    assert_carry  Tad_IsSongLoaded, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestLoadSongIfChanged
    ; Test harness loads song 0 before starting this test
    assert_carry    Tad_IsSongPlaying, true

    lda     #0
    assert_carry    Tad_LoadSongIfChanged, false

    ; song_id has not changed, song is still playing
    assert_carry    Tad_IsSongLoaded, true
    assert_carry    Tad_IsSongPlaying, true


    ; Using song_id $22 as it is invalid and not 0.
    ; (A blank song will be loaded, but the last song_id will be $22)
    lda     #$22
    assert_carry    Tad_LoadSongIfChanged, true

    ; song_id changed, Song has stopped
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSongLoaded, false

    ; TAD is waiting for the drive to switch to the loader
    assert_carry    Tad_IsLoaderActive, false


    ; Testing if `Tad_LoadSongIfChanged` while a song is loading
    lda     #$22
    assert_carry    Tad_LoadSongIfChanged, false

    lda     #$44
    assert_carry    Tad_LoadSongIfChanged, true


    jsr     _FinishLoading
    assert_carry    Tad_IsSongPlaying, true


    lda     #$44
    assert_carry    Tad_LoadSongIfChanged, false

    ; song_id has not changed, song is still playing
    assert_carry    Tad_IsLoaderActive, false
    assert_carry    Tad_IsSongLoaded, true
    assert_carry    Tad_IsSongPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestGetSong
    lda     #$22
    jsr     Tad_LoadSong

    jsr     Tad_GetSong
    assert_a_eq($22)

    lda     #$44
    jsr     Tad_LoadSongIfChanged

    jsr     _FinishLoading

    jsr     Tad_GetSong
    assert_a_eq($44)

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestQueueCommand
    ; Using PAUSE and UNPAUSE commands as they change the 65816-side state

    assert_carry    Tad_IsSongPlaying, true


    lda     #TadCommand::PAUSE
    assert_carry    Tad_QueueCommand, true

    jsr     _Wait
    jsl     Tad_Process

    ; Assert PAUSE command changed state
    assert_carry    Tad_IsSongPlaying, false


    ; Queue is empty
    lda     #TadCommand::UNPAUSE
    assert_carry    Tad_QueueCommand, true

    ; Assert paused/playing state unchanged
    assert_carry    Tad_IsSongPlaying, false

    ; Command queue contains an UNPAUSE command

    ; Queue is full
    lda     #TadCommand::STOP_SOUND_EFFECTS
    assert_carry    Tad_QueueCommand, false

    ; Queue is full
    lda     #TadCommand::SET_MAIN_VOLUME
    ldx     #128
    assert_carry    Tad_QueueCommand, false

    ; Process command
    jsr     _Wait
    jsl     Tad_Process

    ; Assert UNPAUSE command changed state
    assert_carry    Tad_IsSongPlaying, true


    ; Test `Tad_QueueCommand` can handle an 8 bit index
    sep     #$30
.a8
.i8
    lda     #TadCommand::SET_MAIN_VOLUME
    ldx     #20
    assert_carry    Tad_QueueCommand, true

    ; Queue is full
    lda     #TadCommand::SET_MUSIC_CHANNELS
    ldx     #0
    assert_carry    Tad_QueueCommand, false

    ; Queue is full
    lda     #TadCommand::PAUSE
    assert_carry    Tad_QueueCommand, false

    rep     #$10
.i16

    ; Process command
    jsr     _Wait
    jsl     Tad_Process

    ; Command was SET_MAIN_VOLUME
    ; Confirm playing state unchanged
    assert_carry    Tad_IsSongPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestQueueCommandOverride
    ; Using PAUSE and UNPAUSE commands as they change the 65816-side state

    assert_carry    Tad_IsSongPlaying, true

    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride

    jsr     _Wait
    jsl     Tad_Process

    ; Assert PAUSE command changed state
    assert_carry    Tad_IsSongPlaying, false


    lda     #TadCommand::SET_MAIN_VOLUME
    ldx     #20
    jsr     Tad_QueueCommandOverride

    ; Test QueueCommand will not fill the queue
    lda     #TadCommand::PAUSE
    assert_carry    Tad_QueueCommand, false

    lda     #TadCommand::UNPAUSE
    jsr     Tad_QueueCommandOverride

    ; Assert paused/playing state unchanged
    assert_carry    Tad_IsSongPlaying, false

    ; Command queue contains an UNPAUSE command
    jsr     _Wait
    jsl     Tad_Process

    ; Assert UNPAUSE command changed state
    assert_carry    Tad_IsSongPlaying, true


    ; Test `Tad_QueueCommandOverride` can handle an 8 bit index
    sep     #$30
.a8
.i8
    lda     #TadCommand::SET_MAIN_VOLUME
    ldx     #20
    jsr     Tad_QueueCommandOverride

    lda     #TadCommand::SET_MUSIC_CHANNELS
    ldx     #0
    jsr     Tad_QueueCommandOverride

    lda     #TadCommand::STOP_SOUND_EFFECTS
    jsr     Tad_QueueCommandOverride

    rep     #$10
.i16

    ; Process command
    jsr     _Wait
    jsl     Tad_Process

    ; Command was STOP_SOUND_EFFECTS
    ; Confirm playing state unchanged
    assert_carry    Tad_IsSongPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestQueueCommandIdIsMasked
    ; Test command id has been masked
    lda     #$ff
    assert_carry    Tad_QueueCommand, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #%11110

    ; Clear command id
    lda     #0
    jsr     Tad_QueueCommandOverride
    assert_u8_var_eq TadPrivate_nextCommand_id, #%0

    ; Test command id has changed and is masked
    lda     #$ff
    assert_carry    Tad_QueueCommandOverride, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #%11110

    ; Test command id is unchanged
    lda     #0
    assert_carry    Tad_QueueCommand, false
    assert_u8_var_eq TadPrivate_nextCommand_id, #%11110

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc   TestQueueCommandWithTwoParameters_i16
    stz     TadPrivate_nextCommand_parameter0
    stz     TadPrivate_nextCommand_parameter1


    lda     #TadCommand::SET_GLOBAL_VOLUMES
    ldx     #$ff12
    ldy     #$ff34
    assert_carry    Tad_QueueCommand, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #TadCommand::SET_GLOBAL_VOLUMES
    assert_u8_var_eq TadPrivate_nextCommand_parameter0, #$12
    assert_u8_var_eq TadPrivate_nextCommand_parameter1, #$34


    lda     #TadCommand::PLAY_SOUND_EFFECT
    ldx     #$ffaa
    ldy     #$ffbb
    assert_carry    Tad_QueueCommandOverride, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #TadCommand::PLAY_SOUND_EFFECT
    assert_u8_var_eq TadPrivate_nextCommand_parameter0, #$aa
    assert_u8_var_eq TadPrivate_nextCommand_parameter1, #$bb

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc   TestQueueCommandWithTwoParameters_i8
    sep     #$30
.i8
    stz     TadPrivate_nextCommand_parameter0
    stz     TadPrivate_nextCommand_parameter1


    lda     #TadCommand::SET_GLOBAL_VOLUMES
    ldx     #$12
    ldy     #$34
    assert_carry    Tad_QueueCommand, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #TadCommand::SET_GLOBAL_VOLUMES
    assert_u8_var_eq TadPrivate_nextCommand_parameter0, #$12
    assert_u8_var_eq TadPrivate_nextCommand_parameter1, #$34


    lda     #TadCommand::PLAY_SOUND_EFFECT
    ldx     #$aa
    ldy     #$bb
    assert_carry    Tad_QueueCommandOverride, true
    assert_u8_var_eq TadPrivate_nextCommand_id, #TadCommand::PLAY_SOUND_EFFECT
    assert_u8_var_eq TadPrivate_nextCommand_parameter0, #$aa
    assert_u8_var_eq TadPrivate_nextCommand_parameter1, #$bb

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseCommand1
    assert_carry    Tad_IsSongPlaying, true


    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseCommand2
    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false


    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseCommand3
    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true


    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseMusicPlaySfxCommand1
    assert_carry    Tad_IsSongPlaying, true


    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseMusicPlaySfxCommand2
    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false


    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestPauseMusicPlaySfxCommand3
    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true


    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestUnpauseCommand1
    assert_carry    Tad_IsSongPlaying, true


    lda     #TadCommand::UNPAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, true
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestUnpauseCommand2
    lda     #TadCommand::PAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false


    lda     #TadCommand::UNPAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, true
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc


.a8
.i16
;; DB access lowram
.proc TestUnpauseCommand3
    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true


    lda     #TadCommand::UNPAUSE
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process

    assert_carry    Tad_IsSongPlaying, true
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc



;; Test Tad_QueuePannedSoundEffect
.a8
.i16
;; DB access lowram
.proc TestQueuePannedSoundEffect
    assert_carry    Tad_IsSongPlaying, true

    lda     #$fe
    ldx     #1
    jsr     _QueuePannedSoundEffect_AssertSuccess

    lda     #10
    ldx     #2
    jsr     _QueuePannedSoundEffect_AssertSuccess

    lda     #11
    ldx     #3
    jsr     _QueuePannedSoundEffect_AssertFail

    lda     #9
    ldx     #4
    jsr     _QueuePannedSoundEffect_AssertSuccess

    lda     #10
    ldx     #5
    jsr     _QueuePannedSoundEffect_AssertFail


    lda     #$fe
    ldx     #6
    jsr     _QueuePannedSoundEffect_AssertFail

    jsr     _Wait
    jsl     Tad_Process

    ; PLAY_SOUND_EFFECT command sent to the audio driver

    lda     #$fe
    ldx     #7
    jsr     _QueuePannedSoundEffect_AssertSuccess

    ; Clear SFX queue so it doesn't interfere with the next test
    jsr     _Wait
    jsl     Tad_Process

    rts
.endproc



;; Test Tad_QueueSoundEffect
.a8
.i16
;; DB access lowram
.proc TestQueueSoundEffect
    assert_carry    Tad_IsSongPlaying, true

    lda     #$fe
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #10
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #11
    jsr     _QueueSoundEffect_AssertFail

    lda     #9
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #10
    jsr     _QueueSoundEffect_AssertFail


    lda     #$fe
    jsr     _QueueSoundEffect_AssertFail

    jsr     _Wait
    jsl     Tad_Process

    ; PLAY_SOUND_EFFECT command sent to the audio driver

    lda     #$fe
    jsr     _QueueSoundEffect_AssertSuccess

    ; Clear SFX queue so it doesn't interfere with the next test
    jsr     _Wait
    jsl     Tad_Process

    rts
.endproc



.a8
.i16
;; DB access lowram
.proc TestSfxQueueAfterLoadSong
    lda     #42
    sta     Tad_sfxQueue_sfx
    sta     Tad_sfxQueue_pan

    lda     #0
    jsr     Tad_LoadSong
    jsr     _FinishLoading

    ; Assert queue has been reset
    assert_u8_var_eq   Tad_sfxQueue_sfx, #$ff
    assert_u8_var_eq   Tad_sfxQueue_pan, #$ff

    rts
.endproc



.a8
.i16
;; DB access lowram
.proc   TestSfxQueueAfterPlaySoundEffectCommand
    lda     #42
    sta     Tad_sfxQueue_sfx
    sta     Tad_sfxQueue_pan

    jsr     _Wait
    jsl     Tad_Process
    ; play_sound_effect command sent to the audio driver

    ; Assert queue has been reset
    assert_u8_var_eq   Tad_sfxQueue_sfx, #$ff
    assert_u8_var_eq   Tad_sfxQueue_pan, #$ff

    rts
.endproc



.a8
.i16
;; DB access lowram
.proc TestSfxQueueWithOnlyMusicPaused
    lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
    jsr     Tad_QueueCommandOverride
    jsr     _Wait
    jsl     Tad_Process
    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, true

    lda     #10
    jsr     _QueueSoundEffect_AssertSuccess

    jsr     _Wait
    jsl     Tad_Process
    ; play_sound_effect command sent to the audio driver

    assert_carry Tad_IsSfxPlaying, true

    ; Assert sfx queue has been reset (a play_sound_effect command was sent to the audio driver)
    assert_u8_var_eq   Tad_sfxQueue_sfx, #$ff
    assert_u8_var_eq   Tad_sfxQueue_pan, #$ff

    rts
.endproc



;; Tests Command queue has a higher priority then the SFX queue
.a8
.i16
;; DB access lowram
.proc TestCommandAndSfxQueuePriority
    assert_carry    Tad_IsSongPlaying, true

    lda     #0
    jsr     _QueueSoundEffect_AssertSuccess

    jsr     _Wait
    jsl     Tad_Process
    ; play_sound_effect command sent to the audio driver

    lda     #10
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #TadCommand::STOP_SOUND_EFFECTS
    assert_carry    Tad_QueueCommand, true

    jsr     _Wait
    jsl     Tad_Process
    ; STOP_SOUND_EFFECTS command sent to the audio driver

    ; Sound effect queue is unchanged

    ; Test sfx cannot be added to the queue
    lda     #20
    jsr     _QueueSoundEffect_AssertFail

    jsr     _Wait
    jsl     Tad_Process
    ; play_sound_effect command sent to the audio driver

    ; Test sfx can be added to the queue
    lda     #100
    jsr     _QueueSoundEffect_AssertSuccess

    ; Clear SFX queue so it doesn't interfere with the next test
    jsr     _Wait
    jsl     Tad_Process

    rts
.endproc



.a8
.i16
;; DB access lowram
.proc TestCommandAndSfxQueueEmptyAfterSongLoad
    lda     #0
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #TadCommand::STOP_SOUND_EFFECTS
    assert_carry    Tad_QueueCommand, true


    ; Load a song that requires multiple `Tad_Process` calls to load
    lda     #1
    jsr     Tad_LoadSong

    jsr     _WaitForLoader

    jsl     Tad_Process
    jsl     Tad_Process

    ; Test SFX queue is unchanged while the loader is active
    lda     #100
    jsr     _QueueSoundEffect_AssertFail

    ; Test command queue is unchanged while the loader is active
    lda     #TadCommand::STOP_SOUND_EFFECTS
    assert_carry    Tad_QueueCommand, false


    jsr     _FinishLoading


    ; Test the two queue are now empty by trying to populate them

    lda     #$fe
    jsr     _QueueSoundEffect_AssertSuccess

    lda     #TadCommand::STOP_SOUND_EFFECTS
    assert_carry    Tad_QueueCommand, true


    ; Clear the two queues so they don't interfere with the next test
    jsr     _Wait
    jsl     Tad_Process

    jsr     _Wait
    jsl     Tad_Process

    rts
.endproc



;; Tests that `Tad_QueuePannedSoundEffect` does not modify the X/Y registers
;; and can be called with a 16 bit index
.a8
.i16
;; DB access lowram
.proc TestQueuePannedSoundEffectKeepsXY16
    assert_carry    Tad_IsSongPlaying, true

    lda     #10
    ldx     #1234
    ldy     #5678
    jsr     _QueuePannedSoundEffect_AssertSuccess

    assert_x_eq     1234
    assert_y_eq     5678


    lda     #20
    ldx     #$fedc
    ldy     #$ba98
    jsr     _QueuePannedSoundEffect_AssertFail

    assert_x_eq     $fedc
    assert_y_eq     $ba98

    rts
.endproc



;; Tests that `Tad_QueuePannedSoundEffect` does not modify the X/Y registers
;; and can be called with a 8 bit index
.a8
.i16
;; DB access lowram
.proc TestQueuePannedSoundEffectKeepsXY8
    sep     #$10
.i8

    assert_carry    Tad_IsSongPlaying, true

    lda     #100
    ldx     #12
    ldy     #34
    jsr     _QueuePannedSoundEffect_AssertSuccess

    assert_x_eq     12
    assert_y_eq     34


    lda     #200
    ldx     #$fe
    ldy     #$dc
    jsr     _QueuePannedSoundEffect_AssertFail

    assert_x_eq     $fe
    assert_y_eq     $dc

    rep     #$10
.i16
    rts
.endproc



;; Tests that `Tad_QueueSoundEffect` does not modify the X/Y registers
;; and can be called with a 16 bit index
.a8
.i16
;; DB access lowram
.proc TestQueueSoundEffectKeepsXY16
    assert_carry    Tad_IsSongPlaying, true

    lda     #10
    ldx     #1234
    ldy     #5678
    jsr     _QueueSoundEffect_AssertSuccess

    assert_x_eq     1234
    assert_y_eq     5678


    lda     #20
    ldx     #$fedc
    ldy     #$ba98
    jsr     _QueueSoundEffect_AssertFail

    assert_x_eq     $fedc
    assert_y_eq     $ba98

    rts
.endproc



;; Tests that `Tad_QueueSoundEffect` does not modify the X/Y registers
;; and can be called with a 8 bit index
.a8
.i16
;; DB access lowram
.proc TestQueueSoundEffectKeepsXY8
    sep     #$10
.i8

    assert_carry    Tad_IsSongPlaying, true

    lda     #100
    ldx     #12
    ldy     #34
    jsr     _QueueSoundEffect_AssertSuccess

    assert_x_eq     12
    assert_y_eq     34


    lda     #200
    ldx     #$fe
    ldy     #$dc
    jsr     _QueueSoundEffect_AssertFail

    assert_x_eq     $fe
    assert_y_eq     $dc

    rep     #$10
.i16
    rts
.endproc



; Does NOT test if the PlaySongImmediately flag is sent to the loader
.a8
.i16
;; DB access lowram
.proc TestSongStartsImmediately
    jsr     Tad_SongsStartImmediately

    lda     #0
    jsr     Tad_LoadSong
    jsr     _FinishLoading

    assert_carry    Tad_IsSongPlaying, true
    assert_carry    Tad_IsSfxPlaying, true

    rts
.endproc



; Does NOT test if the PlaySongImmediately flag is sent to the loader
.a8
.i16
;; DB access lowram
.proc TestSongStartPaused
    jsr     Tad_SongsStartPaused

    lda     #0
    jsr     Tad_LoadSong
    jsr     _FinishLoading

    assert_carry    Tad_IsSongPlaying, false
    assert_carry    Tad_IsSfxPlaying, false

    rts
.endproc



.a8
.i16
;; DB access lowram
.proc TestSetTransferSize
    SONG_ID = 1
    DATA_SIZE = DummySongData_SIZE
    .assert DATA_SIZE = 2000, error

    MIN_TRANSFER = 32
    MAX_TRANSFER = 800

    assert_carry    Tad_IsSongLoaded, true


    ; Also tests `loader` rounds up transfer size to 100
    ldx     #99
    jsr     Tad_SetTransferSize

    jsr     __CountTransfers
    assert_a_eq     20


    ldx     #250
    jsr     Tad_SetTransferSize

    jsr     __CountTransfers
    assert_a_eq     8


    ; Test `Tad_SetTransferSize` enforces minimum value
    ldx     #10
    jsr     Tad_SetTransferSize

    jsr     __CountTransfers
    assert_a_eq     (DATA_SIZE + MIN_TRANSFER - 1) / MIN_TRANSFER


    ; Test `Tad_SetTransferSize` enforces maximum value
    ldx     #$6000
    jsr     Tad_SetTransferSize

    jsr     __CountTransfers
    assert_a_eq     (DATA_SIZE + MAX_TRANSFER - 1) / MAX_TRANSFER

    rts


    .proc __CountTransfers
        lda     #SONG_ID
        jsr     Tad_LoadSong

        jsr     _WaitForLoader

        stz     counter

        @Loop:
            jsl     Tad_Process
            inc     counter

            jsr     Tad_IsSongLoaded
            bcc     @Loop

       lda  counter
       rts
    .endproc
.endproc


.proc TestFlagFunctions
    stz     Tad_flags
    jsr     Tad_ReloadCommonAudioData
    assert_u8_var_eq Tad_flags, #TadFlags::RELOAD_COMMON_AUDIO_DATA

    ; No function to clear TadFlags::RELOAD_COMMON_AUDIO_DATA

    stz     Tad_flags
    jsr     Tad_SongsStartImmediately
    assert_u8_var_eq Tad_flags, #TadFlags::PLAY_SONG_IMMEDIATELY

    lda     #$ff
    sta     Tad_flags
    jsr     Tad_SongsStartPaused
    assert_u8_var_eq Tad_flags, #$ff ^ TadFlags::PLAY_SONG_IMMEDIATELY

    stz     Tad_flags
    jsr     Tad_GlobalVolumesResetOnSongStart
    assert_u8_var_eq Tad_flags, #TadFlags::RESET_GLOBAL_VOLUMES_ON_SONG_START

    lda     #$ff
    sta     Tad_flags
    jsr     Tad_GlobalVolumesPersist
    assert_u8_var_eq Tad_flags, #$ff ^ TadFlags::RESET_GLOBAL_VOLUMES_ON_SONG_START

    rts
.endproc


;; Waits for a bit.
;;
;; This delay should be long enough for the audio-driver to process any pending IO commands.
.a8
.i16
;; DB unknown
.proc _Wait
    ldy     #9
    @OuterLoop:
        ldx     #10000
        @InnerLoop:
            dex
            bne     @InnerLoop
        dey
        bne     @OuterLoop
    rts
.endproc


;; Repeatedly calls `Tad_Process` if the song is not loaded into Audio-RAM
.a8
.i16
;; DB access lowram
.proc _FinishLoading
    @Loop:
        jsr     Tad_IsSongLoaded
        bcs     @Return
            jsl     Tad_Process
            bra     @Loop
@Return:

    ; assert state is PAUSED or PLAYING
    assert_carry  Tad_IsSongLoaded,   true
    assert_carry  Tad_IsLoaderActive, false

    rts
.endproc


;; Asserts state is WAITING_FOR_LOADER.
;; Repeatedly calls `Tad_Process` if the loader is active
.a8
.i16
;; DB access lowram
.proc _WaitForLoader
    ; assert state is WAITING_FOR_LOADER
    assert_carry  Tad_IsSongLoaded,   false
    assert_carry  Tad_IsLoaderActive, false

    @Loop:
        jsl     Tad_Process

        jsr     Tad_IsLoaderActive
        bcc     @Loop

    ; assert state is LOADING_COMMON_AUDIO_DATA
    assert_carry  Tad_IsLoaderActive, true
    assert_carry  Tad_IsSongLoaded,  false

    rts
.endproc


;; IN: A = sfx id
.a8
;; I unknown
;; DB access lowram
.proc _QueuePannedSoundEffect_AssertSuccess
    ; save function arguments
    pha
        sta     _queueSfxTest_id

        txa
        sta     _queueSfxTest_pan
    pla

    ; Assumes `Tad_QueuePannedSoundEffect` is false (from the cmp) if `sfx_id` was added to the queue
    ; NOTE: This is undocumented behaviour
    assert_carry    Tad_QueuePannedSoundEffect, false

    ; test SFX queue matches function arguments
    assert_u8_var_eq   Tad_sfxQueue_sfx, _queueSfxTest_id
    assert_u8_var_eq   Tad_sfxQueue_pan, _queueSfxTest_pan

    rts
.endproc


;; IN: A = sfx id
.a8
;; I unknown
;; DB access lowram
.proc _QueuePannedSoundEffect_AssertFail
    ; save SFX queue
    pha
        lda     Tad_sfxQueue_sfx
        sta     _queueSfxTest_id

        lda     Tad_sfxQueue_pan
        sta     _queueSfxTest_pan
    pla

    ; Assumes `Tad_QueuePannedSoundEffect` is false (from the cmp) if `sfx_id` was added to the queue
    ; NOTE: This is undocumented behaviour
    assert_carry    Tad_QueuePannedSoundEffect, true

    ; test SFX queue unchanged
    assert_u8_var_eq   Tad_sfxQueue_sfx, _queueSfxTest_id
    assert_u8_var_eq   Tad_sfxQueue_pan, _queueSfxTest_pan

    rts
.endproc


;; IN: A = sfx id
.a8
;; I unknown
;; DB access lowram
.proc _QueueSoundEffect_AssertSuccess
    ; save subroutine argument
    sta     _queueSfxTest_id

    ; Assumes `Tad_QueueSoundEffect` is false (from the cmp) if `sfx_id` was added to the queue
    ; NOTE: This is undocumented behaviour
    assert_carry    Tad_QueueSoundEffect, false

    ; test SFX queue matches input
    assert_u8_var_eq   Tad_sfxQueue_sfx, _queueSfxTest_id
    assert_u8_var_eq   Tad_sfxQueue_pan, #TAD_CENTER_PAN

    rts
.endproc


;; IN: A = sfx id
.a8
;; I unknown
;; DB access lowram
.proc _QueueSoundEffect_AssertFail
    ; save SFX queue
    pha
        lda     Tad_sfxQueue_sfx
        sta     _queueSfxTest_id

        lda     Tad_sfxQueue_pan
        sta     _queueSfxTest_pan
    pla

    ; Assumes `Tad_QueueSoundEffect` is false (from the cmp) if `sfx_id` was added to the queue
    ; NOTE: This is undocumented behaviour
    assert_carry    Tad_QueueSoundEffect, true

    ; test SFX queue unchanged
    assert_u8_var_eq   Tad_sfxQueue_sfx, _queueSfxTest_id
    assert_u8_var_eq   Tad_sfxQueue_pan, _queueSfxTest_pan

    rts
.endproc



;; ===========
;; Binary Data
;; ===========

.export Tad_Loader_Bin, Tad_Loader_SIZE
.export Tad_AudioDriver_Bin, Tad_AudioDriver_SIZE

Tad_Loader_Bin: .incbin "../../../loader.bin"
Tad_Loader_SIZE = .sizeof(Tad_Loader_Bin)

Tad_AudioDriver_Bin: .incbin "../../../audio-driver.bin"
Tad_AudioDriver_SIZE = .sizeof(Tad_AudioDriver_Bin)


;; ================
;; Dummy Audio Data
;; ================
;;
;; Serves three purposes:
;;
;;   1. Tests the loader correctly handles bank wrapping.
;;      This cannot be tested with 65816 code.
;;      To test bank wrapping you will need to:
;;        * Use Mesen's Memory Viewer to track the reads to `DummyCommonAudioData_Part1`,
;;          `DummyCommonAudioData_Part2`, `DummySongData_Part1` and `DummySongData_Part2`.
;;        * Set a breakpoint on `__Tad_Loader_GotoNextBank`
;;   2. Tests the API without invoking `tad-compiler`
;;   3. Enforces a fixed song-data size for the `Tad_SetTransferSize` and `Tad_LoadSong` tests.


.export LoadAudioData : far

;; LoadAudioData callback
;;
;; Called using JSL (return with RTL)
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
.a8
.i16
;; DB access registers
.proc LoadAudioData
    ; Ensure LoadAudioData returns an even and an odd address
    ; to test if `_Tad_Loader_TransferData` correctly handles odd and even addresses.
    .assert DummyCommonAudioData & 1 = 0, lderror
    .assert DummySongData        & 1 = 1, lderror

    cmp     #0
    bne     :+
        ldx     #.loword(DummyCommonAudioData)
        lda     #.bankbyte(DummyCommonAudioData)
        ldy     #DummyCommonAudioData_SIZE

        sec
        rtl
    :

    cmp     #1
    bne     :+
        ldx     #.loword(DummySongData)
        lda     #.bankbyte(DummySongData)
        ldy     #DummySongData_SIZE

        sec
        rtl
    :

    clc
    rtl
.endproc


.ifdef LOROM
    BANK_START =  $8000
    BANK_SIZE  =  $8000
.endif
.ifdef HIROM
    BANK_START =  $0000
    BANK_SIZE  = $10000
.endif


_COMMON_AUDIO_DATA_PART1_SIZE = 1500
_COMMON_AUDIO_DATA_PART2_SIZE = 1500

_SONG_DATA_PART1_SIZE =  999
_SONG_DATA_PART2_SIZE = 1001

.segment "RODATA1"
    .res   BANK_SIZE - _COMMON_AUDIO_DATA_PART1_SIZE, $ff

DummyCommonAudioData_Part1:
    .res   _COMMON_AUDIO_DATA_PART1_SIZE, $01
DummyCommonAudioData_Part1End:

.segment "RODATA2"
DummyCommonAudioData_Part2:
    .res   _COMMON_AUDIO_DATA_PART2_SIZE, $01
DummyCommonAudioData_Part2End:

    .res   BANK_SIZE - _COMMON_AUDIO_DATA_PART2_SIZE - _SONG_DATA_PART1_SIZE, $ff

DummySongData_Part1:
    .res   _SONG_DATA_PART1_SIZE, $02
DummySongData_Part1End:

.segment "RODATA3"
DummySongData_Part2:
    .res   _SONG_DATA_PART2_SIZE, $02
DummySongData_Part2End:

    .res   BANK_SIZE - _SONG_DATA_PART2_SIZE, $ff


.assert .bankbyte(DummyCommonAudioData_Part2) = .bankbyte(DummyCommonAudioData_Part1) + 1, lderror
.assert .bankbyte(DummySongData_Part2) = .bankbyte(DummySongData_Part1) + 1, lderror

.assert .loword(DummyCommonAudioData_Part1End) = $0000, lderror
.assert .loword(DummySongData_Part1End) = $0000, lderror

.assert .loword(DummyCommonAudioData_Part2) = BANK_START, lderror
.assert .loword(DummySongData_Part2) = BANK_START, lderror


DummyCommonAudioData = DummyCommonAudioData_Part1
DummySongData = DummySongData_Part1

DummyCommonAudioData_SIZE = _COMMON_AUDIO_DATA_PART1_SIZE + _COMMON_AUDIO_DATA_PART1_SIZE
DummySongData_SIZE = _SONG_DATA_PART1_SIZE + _SONG_DATA_PART2_SIZE


