; asar TAD example

; SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
; SPDX-License-Identifier: Zlib
;
; Copyright © 2024 Marcus Rowe <undisbeliever@gmail.com>
;
; This software is provided -is', without any express or implied warranty.  In
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
;      2. Altered source versions must be plainly marked such, and must not be
;         misrepresented being the original software.
;
;      3. This notice may not be removed or altered from any source distribution.


dpbase 0
bank $80
; A8
; I16
Main: {
    jsl     Tad_Init

    ; Default flags, audio-mode and transfer size set in `_examplem`.


    ; Enable white screen

    ; Set backdrop
    stz     CGADD
    lda.b   #$ff
    sta     CGDATA
    sta     CGDATA

    lda.b   #15
    sta     INIDISP


    ; Enable VBlank interrupts and auto-joypad reading
    lda     RDNMI
    lda.b   #!NMITIMEN_VBLANK|!NMITIMEN_JOYPAD
    sta     NMITIMEN

    lda.b   #1
    jsr     Tad_LoadSong


    MainLoop:
        ; Wait for auto-joy to start
        lda.b   #1
        -
            bit     HVBJOY
            beq -
        ; Wait for auto-joy to complete
        -
            bit     HVBJOY
            bne -

        ; Read joypad and update joypadPressed
        rep     #$30
    ; A16

        lda     JOY1
        bit.w   #$f
        beq     +
            ; Ignore input if this is not a standard controller
            lda.w   #0
        +
        ; Swap A and joypadCurrent
        tay
        lda     joypadCurrent
        sty     joypadCurrent

        ; Calculate newly pressed buttons
        eor.w   #$ffff
        and     joypadCurrent
        sta     joypadPressed

        sep     #$20
    ; A8

        ; Call a function in the function table if a button  been pressed
        lda     joypadPressed
        bit.b   #!JOYPAD_L_L
        beq     +
            lda.b   #0
            ldx.w   #0
            jsr     Tad_QueuePannedSoundEffect
        +

        lda     joypadPressed
        bit.b   #!JOYPAD_L_R
        beq     +
            lda.b   #0
            ldx.w   #!TAD_MAX_PAN
            jsr     Tad_QueuePannedSoundEffect
        +

        lda     joypadPressed
        bit.b   #!JOYPAD_L_X
        beq     +
            lda.b   #0
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed
        bit.b   #!JOYPAD_L_A
        beq     +
            lda.b   #1
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_B
        beq     +
            lda.b   #2
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_Y
        beq     +
            lda.b   #3
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_UP
        beq     +
            lda.b   #4
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_RIGHT
        beq     +
            lda.b   #5
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_DOWN
        beq     +
            lda.b   #6
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_LEFT
        beq     +
            lda.b   #7
            jsr     Tad_QueueSoundEffect
        +

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_START
        beq     EndStart
            jsr     Tad_IsSongPlaying
            bcc     +
                lda.b   #!TadCommand_PAUSE_MUSIC_PLAY_SFX
                bra     ++
            +
                lda.b   #!TadCommand_UNPAUSE
            ++
            jsr     Tad_QueueCommand
    EndStart:

        lda     joypadPressed+1
        bit.b   #!JOYPAD_H_SELECT
        beq     EndSelect
            jsr     Tad_GetSong
            inc     a
            cmp.b   #!LAST_SONG_ID+1
            bcc     +
                lda.b   #1
            +
            jsr     Tad_LoadSong
    EndSelect:

        jsl     Tad_Process

        jmp     MainLoop
}


NmiHandler: {
    rti
}


IrqHandler:
CopHandler: {
    jml BreakHandler
}

