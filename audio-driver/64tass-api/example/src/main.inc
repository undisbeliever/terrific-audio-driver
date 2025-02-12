; 64tass example ROM

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


IrqHandler = BreakHandler
CopHandler = BreakHandler
UnusedInterruptHandler = BreakHandler


.as
.xl
.dpage 0
.databank 0
Main .proc
    jsl     Tad_Init

    ; Default flags, audio-mode and transfer size set in `_example.asm`.


    ; Enable white screen

    ; Set backdrop
    stz     CGADD
    lda     #$ff
    sta     CGDATA
    sta     CGDATA

    lda     #15
    sta     INIDISP


    ; Enable VBlank interrupts and auto-joypad reading
    lda     RDNMI
    lda     #NMITIMEN_VBLANK | NMITIMEN_JOYPAD
    sta     NMITIMEN

    lda     #1
    jsr     Tad_LoadSong


    MainLoop:
        ; Wait for auto-joy to start
        lda     #1
        -
            bit     HVBJOY
            beq -
        ; Wait for auto-joy to complete
        -
            bit     HVBJOY
            bne -

        ; Read joypad and update joypadPressed
        rep     #$30
    .al
    .xl
        lda     JOY1
        bit     #$f
        beq     +
            ; Ignore input if this is not a standard controller
            lda     #0
        +
        ; Swap A and joypadCurrent
        tay
        lda     joypadCurrent
        sty     joypadCurrent

        ; Calculate newly pressed buttons
        eor     #$ffff
        and     joypadCurrent
        sta     joypadPressed

        sep     #$20
    .as

        ; Call a function in the function table if a button has been pressed
        lda     joypadPressed
        bit     #JOYPAD_L_L
        beq     +
            lda     #0
            ldx     #0
            jsr     Tad_QueuePannedSoundEffect
        +

        lda     joypadPressed
        bit     #JOYPAD_L_R
        beq     +
            lda     #0
            ldx     #TAD_MAX_PAN
            jsr     Tad_QueuePannedSoundEffect
        +

        lda     joypadPressed
        bit     #JOYPAD_L_X
        beq     +
            lda     #0
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed
        bit     #JOYPAD_L_A
        beq     +
            lda     #1
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_B
        beq     +
            lda     #2
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_Y
        beq     +
            lda     #3
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_UP
        beq     +
            lda     #4
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_RIGHT
        beq     +
            lda     #5
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_DOWN
        beq     +
            lda     #6
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_LEFT
        beq     +
            lda     #7
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed + 1
        bit     #JOYPAD_H_START
        beq     EndStart
            jsr     Tad_IsSongPlaying
            bcc     +
                lda     #TadCommand.PAUSE_MUSIC_PLAY_SFX
                bra     ++
            +
                lda     #TadCommand.UNPAUSE
            +
            jsr     Tad_QueueCommand
    EndStart:

        lda     joypadPressed + 1
        bit     #JOYPAD_H_SELECT
        beq     EndSelect
            jsr     Tad_GetSong
            inc     a
            cmp     #LAST_SONG_ID + 1
            bcc     +
                lda     #1
            +
            jsr     Tad_LoadSong
    EndSelect:

        jsl     Tad_Process

        jmp     MainLoop
.endproc


NmiHandler .proc
    rti
.endproc

