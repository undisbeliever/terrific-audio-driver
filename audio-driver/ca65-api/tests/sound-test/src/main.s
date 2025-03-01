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


.define ROM_NAME "CA65 API SOUND TEST"
VERSION     = 0
ROM_SIZE    = 2
REGION      = REGION__Japan
CART_TYPE   = CART_TYPE__RomOnly
ROM_SPEED   = ROM_SPEED__Slow


.include "text-buffer.inc"
.include "../../_common/registers.inc"
.include "../../_common/snes-header.inc"
.include "../../../tad-audio.inc"

.include "../gen/audio.inc"


.p816
.smart

.export Main, NmiHandler, IrqHandler, CopHandler

.exportzp zpTmpByte, zpTmpWord, zpTmpFarPtr


; break-isr.s
.import BreakHandler, EmptyHandler

; text-buffer.s
.import TextBuffer_SetupPpu, TextBuffer_VBlank

; dma-forceblank.s
.import DmaToCgram_ForceBlank


;; =========
;; CONSTANTS
;; =========

.export TEXT_BUFFER_VRAM_MAP_WADDR   : absolute = $0000
.export TEXT_BUFFER_VRAM_TILES_WADDR : absolute = $1000


;; =========
;; VARIABLES
;; =========

.zeropage
    zpTmpByte:          .res 1
    zpTmpWord:          .res 2
    zpTmpFarPtr:        .res 3

    joypadCurrent:      .res 2
    joypadPressed:      .res 2

    ;; Index into function tables
    menuPos:            .res 2

    ;; Selected channel bitfield
    ;; A maximum ONE bit must be set in this variable.
    selectedChannelMask: .res 1

    .scope Menu
        song:               .res 1
        sfx:                .res 1
        sfxPan:             .res 1
        mainVolume:         .res 1
        musicVolume:        .res 1
        sfxVolume:          .res 1
        timerOverride:      .res 1
        channelMask:        .res 1
        audioMode:          .res 1
        songStartsFlag:     .res 1
        resetVolumesFlag:   .res 1
    .endscope


;; ==========
;; SOUND TEST
;; ==========

N_SELECTED_CHARS_TO_HIGHLIGHT = 28

CURSOR_XPOS         = 2
MENU_LABEL_XPOS     = CURSOR_XPOS + 2
VAR_XPOS            = 32 - 7
CHANNEL_MASK_XPOS   = VAR_XPOS - 5

STATE_XPOS          = 23
STATE_YPOS          = 2

MENU_YPOS           = 3

N_MENU_ITEMS        = 15
LAST_MENU_INDEX     = (N_MENU_ITEMS - 1) * 2

CHANNEL_MASK_MENU_POS = 7 * 2


.rodata

SingleSpaceString:  .byte " ", 0
CursorString:       .byte ">", 0


;; Location of the cursor (within the text buffer) for each menu item
;;
;; Used for drawing the cursor and highlighting the selected item
;;
;; [u16 ; N_MENU_ITEMS]
BufferIndexTable:
    .macro _BufferIndexEntry_ name, yPos
        name = MENU_YPOS + yPos
        .word TextBuffer_PosToIndex(CURSOR_XPOS, name)
    .endmacro

    _BufferIndexEntry_ PLAY_SONG_YPOS,           0
    _BufferIndexEntry_ PLAY_SFX_YPOS,            2
    _BufferIndexEntry_ SFX_PAN_YPOS,             3
    _BufferIndexEntry_ MAIN_VOLUME_YPOS,         5
    _BufferIndexEntry_ MUSIC_VOLUME_YPOS,        7
    _BufferIndexEntry_ SFX_VOLUME_YPOS,          8
    _BufferIndexEntry_ OVERRIDE_TIMER_YPOS,     10
    _BufferIndexEntry_ CHANNEL_MASK_YPOS,       11
    _BufferIndexEntry_ AUDIO_MODE_YPOS,         13
    _BufferIndexEntry_ SONG_STARTS_YPOS,        14
    _BufferIndexEntry_ RESET_VOLUMES_YPOS,      15
    _BufferIndexEntry_ STOP_SOUND_EFFECTS_YPOS, 17
    _BufferIndexEntry_ PAUSE_UNPAUSE_YPOS,      19
    _BufferIndexEntry_ PAUSE_MUSIC_AND_SFX,     21
    _BufferIndexEntry_ RELOAD_COMMON_DATA_YPOS, 23

.assert * - BufferIndexTable = N_MENU_ITEMS * 2, error

;; List of labels
;; [*str ; N_MENU_ITEMS]
TextTable:
    .repeat N_MENU_ITEMS, i
        .word   .loword(.ident(.sprintf("MenuLabel_%02d", i)))
    .endrepeat

MenuLabel_00: .byte "PLAY SONG", 0
MenuLabel_01: .byte "PLAY SFX (L/R)", 0
MenuLabel_02: .byte "SFX PAN", 0
MenuLabel_03: .byte "MAIN VOLUME", 0
MenuLabel_04: .byte "MUSIC VOLUME", 0
MenuLabel_05: .byte "SFX VOLUME", 0
MenuLabel_06: .byte "OVERRIDE TIMER", 0
MenuLabel_07: .byte "MUSIC CHANNELS", 0
MenuLabel_08: .byte "", 0
MenuLabel_09: .byte "", 0
MenuLabel_10: .byte "", 0
MenuLabel_11: .byte "STOP SOUND EFFECTS (X)", 0
MenuLabel_12: .byte "PAUSE / UNPAUSE (START)", 0
MenuLabel_13: .byte "PAUSE MUSIC AND SFX", 0
MenuLabel_14: .byte "RELOAD COMMON AUDIO DATA", 0


.code


;; The subroutine to call if the menu item is selected.
;;
;; Called if the user presses B or Y
MenuProcessFunctions:
    .addr   Menu_PlaySong_Process
    .addr   Menu_PlaySfx_Process
    .addr   Menu_SfxPan_Process
    .addr   Menu_MainVolume_Process
    .addr   Menu_MusicVolume_Process
    .addr   Menu_SfxVolume_Process
    .addr   Menu_OverrideTimer_Process
    .addr   MenuChannelMask_Process
    .addr   Menu_AudioMode_Process
    .addr   Menu_SongStartsFlag_Process
    .addr   Menu_ResetVolumesFlag_Process
    .addr   Menu_Null_Process
    .addr   Menu_Null_Process
    .addr   Menu_Null_Process
    .addr   Menu_Null_Process
.assert * - MenuProcessFunctions = N_MENU_ITEMS * 2, error


;; The subroutine to call if the menu item is not selected
;;
;; Called once per frame.
MenuActionFunctions:
    .addr   Menu_PlaySong_Action
    .addr   Menu_PlaySfx_Action
    .addr   Menu_SfxPan_Action
    .addr   Menu_MainVolume_Action
    .addr   Menu_MusicVolume_Action
    .addr   Menu_SfxVolume_Action
    .addr   Menu_OverrideTimer_Action
    .addr   MenuChannelMask_Action
    .addr   Menu_AudioMode_Action
    .addr   Menu_SongStartsFlag_Action
    .addr   Menu_ResetVolumesFlag_Action
    .addr   Menu_StopSoundEffects_Action
    .addr   Menu_PauseUnPauseMusic_Action
    .addr   Menu_PauseMusicAndSfx_Action
    .addr   Menu_ReloadCommonAudioData_Action
.assert * - MenuActionFunctions = N_MENU_ITEMS * 2, error


.code


.a8
.i16
;; DB = $7e
.proc SoundTest_Init
    ldx     #0
    stx     menuPos

    stz     selectedChannelMask


    lda     #0
    jsr     _SetMenuPos

    lda     #0
    jsr     _SetSong

    lda     #0
    jsr     _SetSfx

    lda     #TAD_CENTER_PAN
    jsr     _SetSfxPan

    lda     #$7f
    jsr     _SetMainVolume

    lda     #255
    jsr     _SetMusicVolume

    lda     #255
    jsr     _SetSfxVolume

    lda     #100
    jsr     _SetTimerOverride

    lda     #TadAudioMode::SURROUND
    jsr     _SetAudioMode

    lda     #1
    jsr     _SetSongStartsFlag

    lda     #0
    jsr     _SetResetVolumesFlag

    lda     #$ff
    jsr     _SetChannelMask


    ldx     #0

    @Loop:
        phx

        rep     #$30
    .a16
        lda     f:BufferIndexTable,x
        inc
        inc     ; increment 2 spaces after the cursor
        tay

        lda     f:TextTable,x
        tax

        sep     #$20
    .a8
        jsr     TextBuffer_PrintString_rodata

        plx
        inx
        inx
        cpx     #LAST_MENU_INDEX + 1
        bcc     @Loop

    TextBuffer_PrintLiteral CHANNEL_MASK_XPOS, CHANNEL_MASK_YPOS, "01234567"

    lda     #PALETTE_STATE
    ldx     #TextBuffer_PosToIndex(STATE_XPOS, STATE_YPOS)
    ldy     #8
    jsr     TextBuffer_SetPaletteBlock

    rts
.endproc


.a8
.i16
;; DB = $7e
.proc SoundTest_Process
    jsr     _SoundTest_PrintState

    ; Reset variables when songs are loaded
    jsr     Tad_IsSongLoaded
    bcs     :+
        lda     #$ff
        jsr     _SetMainVolume

        lda     #$ff
        jsr     _SetChannelMask
    :

    jsr     SoundTest_ProcessJoypad

    rts
.endproc


.a8
.i16
;; DB = $7e
.proc SoundTest_ProcessJoypad
    lda     joypadPressed + 1
    bit     #JOYPAD_H_UP
    beq     :+
        lda     menuPos
        dec
        dec
        jmp     _SetMenuPos
    :
    bit     #JOYPAD_H_DOWN
    beq     :+
        lda     menuPos
        inc
        inc
        jmp     _SetMenuPos
    :

    lda     joypadPressed + 1
    bit     #JOYPAD_H_START
    beq     :+
        jmp     Menu_PauseUnPauseMusic_Action
    :

    lda     joypadPressed + 0
    bit     #JOYPAD_L_X
    beq     :+
        jmp     Menu_StopSoundEffects_Action
    :

    lda     joypadPressed + 0
    bit     #JOYPAD_L_L | JOYPAD_L_R
    beq     :+
        jmp     PlaySfxUsingCommand
    :


    ldx     menuPos

    .assert JOYPAD_H_B = JOYPAD_L_A, error
    lda     joypadPressed
    ora     joypadPressed + 1
    .assert JOYPAD_H_B = $80, error
    bpl     :+
        ; Goto Menu_*_Action function if B or A pressed
        jmp     (.loword(MenuActionFunctions), x)
    :
        ; Otherwise goto Menu_*_Process function
        jmp     (.loword(MenuProcessFunctions), x)
.endproc


;; IN: A = new menu position
.a8
.i16
;; DB = $7e
.proc _SetMenuPos
    pha

        ; Clear old cursor
        jsr     _CalcCursorBufferIndex
        ldx     #.loword(SingleSpaceString)
        jsr     TextBuffer_PrintString_rodata

        ; unhighlight old selection
        jsr     _CalcCursorBufferIndex
        tyx
        ldy     #N_SELECTED_CHARS_TO_HIGHLIGHT
        lda     #PALETTE_NORMAL
        jsr     TextBuffer_SetPaletteBlock

    pla
    and     #$fe
    bpl     :+
        lda     #LAST_MENU_INDEX
    :
    cmp     #LAST_MENU_INDEX + 1
    bcc     :+
        lda     #0
    :

    .assert .sizeof(menuPos) = 2, error
    sta     menuPos
    stz     menuPos + 1


    ; Draw new cursor cursor
    jsr     _CalcCursorBufferIndex
    ldx     #.loword(CursorString)
    jsr     TextBuffer_PrintString_rodata

    ; Highlight new selection
    jsr     _CalcCursorBufferIndex
    tyx
    ldy     #N_SELECTED_CHARS_TO_HIGHLIGHT
    lda     #PALETTE_SELECTED
    jsr     TextBuffer_SetPaletteBlock

    jsr     _UpdateChannelMask

    rts
.endproc


.a8
.i16
;; DB = $7e
.proc _SoundTest_PrintState
    jsr     Tad_IsSongPlaying
    bcc     :+
        TextBuffer_PrintLiteral STATE_XPOS, STATE_YPOS, "PLAYING"
        rts
    :
    jsr     Tad_IsSfxPlaying
    bcc     :+
        TextBuffer_PrintLiteral STATE_XPOS, STATE_YPOS, "SFX    "
        rts
    :
    jsr     Tad_IsSongLoaded
    bcc     :+
        TextBuffer_PrintLiteral STATE_XPOS, STATE_YPOS, "PAUSED "
        rts
    :
    jsr     Tad_IsLoaderActive
    bcc     :+
        TextBuffer_PrintLiteral STATE_XPOS, STATE_YPOS, "LOADING"
        rts
    :
    ; Else
        TextBuffer_PrintLiteral STATE_XPOS, STATE_YPOS, "......."
        rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PlaySong_Process
    lda     Menu::song
    jsr     _AdjustWithDpad_Slow
    jmp     _SetSong
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PlaySong_Action
    lda     Menu::song
    jsr     Tad_LoadSong

    lda     Menu::resetVolumesFlag
    beq     :+
        lda     #255
        jsr     _SetMusicVolume

        lda     #255
        jsr     _SetSfxVolume
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PlaySfx_Process
    lda     Menu::sfx
    jsr     _AdjustWithDpad_Slow
    jmp     _SetSfx
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PlaySfx_Action
    lda     Menu::sfx
    ldx     Menu::sfxPan
    jmp     Tad_QueuePannedSoundEffect
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_SfxPan_Process
    lda     Menu::sfxPan
    jsr     _AdjustWithDpad_Fast
    jmp     _SetSfxPan
.endproc

Menu_SfxPan_Action = Menu_PlaySfx_Action


.a8
.i16
;; DB = $7e
.proc PlaySfxUsingCommand
    lda     #TadCommand::PLAY_SOUND_EFFECT
    ldx     Menu::sfx
    ldy     Menu::sfxPan
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_MainVolume_Process
    lda     Menu::mainVolume
    jsr     _AdjustWithDpad_Fast
    bcc     :+
        jsr     _SetMainVolume

        lda     #TadCommand::SET_MAIN_VOLUME
        ldx     Menu::mainVolume
        jmp     Tad_QueueCommandOverride
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_MainVolume_Action
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_MusicVolume_Process
    lda     Menu::musicVolume
    jsr     _AdjustWithDpad_Fast
    bcc     :+
        jsr     _SetMusicVolume

        lda     #TadCommand::SET_GLOBAL_MUSIC_VOLUME
        ldx     Menu::musicVolume
        jmp     Tad_QueueCommandOverride
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_MusicVolume_Action
    ; Test set-global-volumes IO command

    lda     #128
    jsr     _SetMusicVolume

    lda     #255
    jsr     _SetSfxVolume

    lda     #TadCommand::SET_GLOBAL_VOLUMES
    ldx     Menu::musicVolume
    ldy     Menu::sfxVolume
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_SfxVolume_Process
    lda     Menu::sfxVolume
    jsr     _AdjustWithDpad_Fast
    bcc     :+
        jsr     _SetSfxVolume

        lda     #TadCommand::SET_GLOBAL_SFX_VOLUME
        ldx     Menu::sfxVolume
        jmp     Tad_QueueCommandOverride
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_SfxVolume_Action
    ; Test set-global-volumes IO command

    lda     #255
    jsr     _SetMusicVolume

    lda     #128
    jsr     _SetSfxVolume

    lda     #TadCommand::SET_GLOBAL_VOLUMES
    ldx     Menu::musicVolume
    ldy     Menu::sfxVolume
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_OverrideTimer_Process
    lda     Menu::timerOverride
    jsr     _AdjustWithDpad_0Max_Fast
    jmp     _SetTimerOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_OverrideTimer_Action
    lda     #TadCommand::SET_SONG_TIMER
    ldx     Menu::timerOverride
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc MenuChannelMask_Process
    lda     joypadPressed + 1
    bit     #JOYPAD_H_LEFT
    beq     :+
        lsr     selectedChannelMask
        bne     :+
            lda     #$80
            sta     selectedChannelMask
    :
    bit     #JOYPAD_H_RIGHT
    beq     :+
        asl     selectedChannelMask
        bne     :+
            lda     #1
            sta     selectedChannelMask
    :

    jmp     _UpdateChannelMask
.endproc


.a8
.i16
;: DB = $7e
.proc MenuChannelMask_Action
    lda     Menu::channelMask
    eor     selectedChannelMask

    jsr     _SetChannelMask

    lda     #TadCommand::SET_MUSIC_CHANNELS
    ldx     Menu::channelMask
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_AudioMode_Process
    lda     Menu::audioMode
    jsr     _AdjustWithDpad_Slow
    bcc     :+
        jmp     _SetAudioMode
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_AudioMode_Action
    lda     Menu::audioMode
    inc
    jmp     _SetAudioMode
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_SongStartsFlag_Process
    lda     Menu::songStartsFlag
    jsr     _AdjustWithDpad_Bool
    bcc     :+
        jmp     _SetSongStartsFlag
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_SongStartsFlag_Action
    lda     Menu::songStartsFlag
    inc
    jmp     _SetSongStartsFlag
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_ResetVolumesFlag_Process
    lda     Menu::resetVolumesFlag
    jsr     _AdjustWithDpad_Bool
    bcc     :+
        jmp     _SetResetVolumesFlag
    :
    rts
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_ResetVolumesFlag_Action
    lda     Menu::resetVolumesFlag
    inc
    jmp     _SetResetVolumesFlag
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_StopSoundEffects_Action
    lda     #TadCommand::STOP_SOUND_EFFECTS
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PauseUnPauseMusic_Action
    jsr     Tad_IsSongPlaying
    bcc     :+
        lda     #TadCommand::PAUSE_MUSIC_PLAY_SFX
        bra     :++
    :
        lda     #TadCommand::UNPAUSE
    :
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_PauseMusicAndSfx_Action
    lda     #TadCommand::PAUSE
    jmp     Tad_QueueCommandOverride
.endproc


.a8
.i16
;: DB = $7e
.proc Menu_ReloadCommonAudioData_Action
    jmp     Tad_ReloadCommonAudioData
.endproc



.a8
.i16
;; DB = $7e
.proc Menu_Null_Process
    rts
.endproc


;; OUT: Y = buffer index
.a8
.i16
;; DB = $7e
.proc _CalcCursorBufferIndex
    rep     #$30
.a16
    ldx     menuPos
    lda     f:BufferIndexTable,x
    tay

    sep     #$20
.a8
    rts
.endproc


;; IN: A - value
;; OUT: A - new value
;; OUT: carry set if value changed
.a8
.i16
;; DB = $7e
.proc _AdjustWithDpad_Slow
    tay

    lda     joypadPressed + 1
    bit     #JOYPAD_H_LEFT
    beq     :++
        tya
        beq     :+
            dec
        :

        sec
        rts
    :
    bit     #JOYPAD_H_RIGHT
    beq     :++
        tya
        inc
        bne     :+
            dec
        :

        sec
        rts
    :

    tya
    clc
    rts
.endproc


;; IN: A - value
;; OUT: A - new value
;; OUT: carry set if value changed
.a8
.i16
;; DB = $7e
.proc _AdjustWithDpad_Fast
    tay

    lda     joypadCurrent + 1
    bit     #JOYPAD_H_LEFT
    beq     :++
        tya
        beq     :+
            dec
        :

        sec
        rts
    :
    bit     #JOYPAD_H_RIGHT
    beq     :++
        tya
        inc
        bne     :+
            dec
        :

        sec
        rts
    :

    tya
    clc
    rts
.endproc


;; IN: A - value
;; OUT: A - new value
;; OUT: carry set if value changed
.a8
.i16
;; DB = $7e
.proc _AdjustWithDpad_0Max_Fast
    tay

    lda     joypadCurrent + 1
    bit     #JOYPAD_H_LEFT
    beq     :++
        tya
        cmp     #1
        beq     :+
            dec
        :

        sec
        rts
    :
    bit     #JOYPAD_H_RIGHT
    beq     :++
        tya
        beq     :+
            inc
        :

        sec
        rts
    :

    tya
    clc
    rts
.endproc


;; IN: A - value
;; OUT: A - new value
;; OUT: carry set if value changed
.a8
.i16
;; DB = $7e
.proc _AdjustWithDpad_Bool
    tay

    lda     joypadPressed + 1
    bit     #JOYPAD_H_LEFT | JOYPAD_H_RIGHT
    beq     :+
        tya
        inc
        and     #1

        sec
        rts
    :
        ; else
        tya

        clc
        rts
.endproc


.macro _SetVarFn_ name, yPos, var, min, max
    ;; IN: A = new value
    .a8
    .i16
    ;; DB = $7e
    .proc name
        cmp     #min
        bcs     :+
            lda     #min
        :
        cmp     #.max(max, min)
        bcc     :+
            lda     #.max(max, min)
        :

        sta     Menu::var

        ldy     #TextBuffer_PosToIndex(VAR_XPOS, yPos)
        jmp     TextBuffer_PrintPadded_A8
    .endproc
.endmacro

_SetVarFn_  _SetSong,           PLAY_SONG_YPOS,         song,           0,                  LAST_SONG_ID
_SetVarFn_  _SetSfx,            PLAY_SFX_YPOS,          sfx,            0,                  N_SOUND_EFFECTS - 1
_SetVarFn_  _SetSfxPan,         SFX_PAN_YPOS,           sfxPan,         0,                  TAD_MAX_PAN
_SetVarFn_  _SetMainVolume,     MAIN_VOLUME_YPOS,       mainVolume,     0,                  $7f
_SetVarFn_  _SetMusicVolume,    MUSIC_VOLUME_YPOS,      musicVolume,    0,                  $ff
_SetVarFn_  _SetSfxVolume,      SFX_VOLUME_YPOS,        sfxVolume,      0,                  $ff

;; IN: A = new value
.a8
.i16
;; DB = $7e
.proc _SetTimerOverride
    cmp     #0
    beq     :+
        cmp     #TAD_MIN_TICK_CLOCK
        bcs     :+
            lda     #TAD_MIN_TICK_CLOCK
    :
    sta     Menu::timerOverride

    ldy     #TextBuffer_PosToIndex(VAR_XPOS, OVERRIDE_TIMER_YPOS)
    jmp     TextBuffer_PrintPadded_A8
.endproc



;; IN: A = new value
.a8
.i16
;; DB = $7e
.proc _SetAudioMode
    MAX = 2

    cmp     #$80
    bcc     :+
       lda     #MAX
    :
    cmp     #MAX + 1
    bcc     :+
        lda     #0
    :
    sta     Menu::audioMode

    sta     Tad_audioMode


    cmp     #TadAudioMode::SURROUND
    bne     :+
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, AUDIO_MODE_YPOS, "SURROUND"
        rts
    :
    cmp     #TadAudioMode::STEREO
    bne     :+
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, AUDIO_MODE_YPOS, "STEREO  "
        rts
    :

    TextBuffer_PrintLiteral MENU_LABEL_XPOS, AUDIO_MODE_YPOS, "MONO    "
    rts
.endproc


;; IN: A = new value
.a8
.i16
;; DB = $7e
.proc _SetSongStartsFlag
    and     #1
    sta     Menu::songStartsFlag

    beq     :+
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, SONG_STARTS_YPOS, "SONGS START IMMEDIATELY"
        jmp     Tad_SongsStartImmediately
    :
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, SONG_STARTS_YPOS, "SONGS START PAUSED     "
        jmp     Tad_SongsStartPaused
.endproc


;; IN: A = new value
.a8
.i16
;; DB = $7e
.proc _SetResetVolumesFlag
    and     #1
    sta     Menu::resetVolumesFlag

    beq     :+
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, RESET_VOLUMES_YPOS, "VOLUMES RESET ON SONG LOAD"
        jmp     Tad_GlobalVolumesResetOnSongStart
    :
        TextBuffer_PrintLiteral MENU_LABEL_XPOS, RESET_VOLUMES_YPOS, "GLOBAL VOLUMES PERSIST    "
        jmp     Tad_GlobalVolumesPersist
.endproc


;; IN: A = new value
.a8
.i16
;; DB = $7e
.proc _SetChannelMask
    sta     Menu::channelMask

; Fallthrough
    .assert * = _UpdateChannelMask, error
.endproc


.a8
.i16
;; DB = $7e
.proc _UpdateChannelMask
_cm := zpTmpByte
_sm := zpTmpWord

START_INDEX = TextBuffer_PosToIndex(CHANNEL_MASK_XPOS, CHANNEL_MASK_YPOS)
END_INDEX = TextBuffer_PosToIndex(CHANNEL_MASK_XPOS + 8, CHANNEL_MASK_YPOS)

.assert START_INDEX < TextBuffer_attrBuffer_SIZE, error
.assert END_INDEX < TextBuffer_attrBuffer_SIZE, error

    lda     Menu::channelMask
    sta     _cm

    lda     selectedChannelMask
    bne     :+
        lda     #1
        sta     selectedChannelMask
    :
    sta     _sm

    ldx     #START_INDEX
    @Loop:
        ; Check if channel is selected
        lsr     _sm
        bcc     @NotSelected

        lda     menuPos
        cmp     #CHANNEL_MASK_MENU_POS
        bne     @NotSelected
            ; Channel and menu are seleected
            lsr     _cm
            bcc     :+
                lda     #PALETTE_SELECTED_AND_ENABLED_CHANNEL << 2
                bra     :++
            :
                lda     #PALETTE_SELECTED_AND_DISABLED_CHANNEL << 2
            :

            bra     @EndIf

        @NotSelected:
            ; Channel or menu are not selected
            lsr     _cm
            bcc     :+
                lda     #PALETTE_ENABLED_CHANNEL << 2
                bra     :++
            :
                lda     #PALETTE_DISABLED_CHANNEL << 2
            :
        @EndIf:

        sta     TextBuffer_attrBuffer,x

        inx
        cpx     #END_INDEX
        bcc     @Loop


    stz     TextBuffer_attrBufferChangedIfZero

    rts
.endproc



;; ====
;; MAIN
;; ====

.code

.a8
.i16
;; DB = $80
.proc Main
    jsl     Tad_Init


    ; Reset TextBuffer and copy tiles/map to VRAM
    jsr     TextBuffer_SetupPpu

    ; Mode 0
    stz     BGMODE

    .assert TEXT_BUFFER_VRAM_MAP_WADDR = 0, error
    stz     BG1SC

    lda     #TEXT_BUFFER_VRAM_TILES_WADDR >> 12
    sta     BG12NBA

    lda     #1
    sta     TM

    ; Copy palette to CGRAM
    stz     CGADD
    ldx     #0

    ldx     #.loword(Palette)
    lda     #.bankbyte(Palette)
    ldy     #Palette_SIZE
    jsr     DmaToCgram_ForceBlank


    lda     #NMITIMEN_VBLANK | NMITIMEN_JOYPAD
    sta     NMITIMEN

    lda     #15
    sta     INIDISP


    lda     #$7e
    pha
    plb
; DB = $7e

    jsr     SoundTest_Init

    MainLoop:
        jsr     WaitFrame

        jsl     Tad_Process
        jsr     SoundTest_Process

        bra     MainLoop
.endproc



;; ==========
;; Interrupts
;; ==========


; This ROM does not use IRQ or COP interrupts.
IrqHandler = BreakHandler
CopHandler = BreakHandler


;; Code to execute during a non lag-frame VBlank
;;
;; A8
;; I16
;; DB = $80
.macro VBlank
    TextBuffer_VBlank


    ; Read Joypad

    ; Wait for auto-joy to complete
    lda     #1
    :
        bit     HVBJOY
        bne :-

    rep     #$30
.a16

    lda     JOY1
    bit     #$f
    beq     :+
        ; Ignore input if this is not a standard controller
        lda     #0
    :

    ; Swap A and joypadCurrent
    tay
    lda     joypadCurrent
    sty     joypadCurrent

    ; Calculate newly pressed buttons
    eor     #$ffff
    and     joypadCurrent
    sta     joypadPressed

    sep     #$20
.a8
.endmacro

.include "../../_common/nmi-isr.inc"


;; ======
;; RODATA
;; ======

.rodata

.define SnesRgb(r, g, b) ((r) | ((g) << 5) | ((b) << 10))

PALETTE_NORMAL                        = 0
PALETTE_SELECTED                      = 1
PALETTE_STATE                         = 2
PALETTE_ENABLED_CHANNEL               = PALETTE_NORMAL
PALETTE_DISABLED_CHANNEL              = 3
PALETTE_SELECTED_AND_ENABLED_CHANNEL  = 4
PALETTE_SELECTED_AND_DISABLED_CHANNEL = 5

Palette:
    .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb(25, 25, 25)
    .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb(31, 31,  0)
    .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb( 0, 28,  0)
    .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb( 7,  7,  7)
    .word   0, 0, SnesRgb(15, 15,  0), SnesRgb(31, 31, 31)
    .word   0, 0, SnesRgb(15, 15,  0), SnesRgb( 0,  0,  0)

Palette_SIZE = * - Palette


