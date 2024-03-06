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


.define ROM_NAME "TAD CA65 API TESTS"
VERSION     = 0
ROM_SIZE    = 2
REGION      = REGION__Japan
CART_TYPE   = CART_TYPE__RomOnly
ROM_SPEED   = ROM_SPEED__Slow


.include "../../_common/snes-header.inc"
.include "../../_common/registers.inc"
.include "../../../tad-audio.inc"


.p816
.smart

.export Main

.import BreakHandler
.import RunTests
.import ResetRegisters


.define SnesRgb(r, g, b) ((r) | ((g) << 5) | ((b) << 10))

TESTING_COLOR = SnesRgb(31, 31, 0)
FAIL_COLOR    = SnesRgb(31,  0, 0)
SUCCESS_COLOR = SnesRgb( 0, 31, 0)


.code


;; VBlank interrupt ISR
;;
;; Pauses execution for a bit.
;; Tests if the audio driver and loader can handle being interrupted.
;;
;; A unknown
;; I unknown
;; DB unknown
;; D unknown
.proc NmiHandler
    rep     #$30
.i16
    phx

    ; MUST ONLY USE X

    ; Loop counter chosen to pause execution until a few scanlines after VBlank ends.
    ldx     #1600
    @Loop:
        dex
        bne     @Loop

    plx

    rti
.endproc


; This test does not use IRQ interrupts.
IrqHandler             = BreakHandler
CopHandler             = BreakHandler



.a8
.i16
;; DB = $80
.proc Main
    ; Set backdrop
    stz     CGADD
    lda     #.lobyte(TESTING_COLOR)
    sta     CGDATA
    lda     #.hibyte(TESTING_COLOR)
    sta     CGDATA

    lda     #15
    sta     INIDISP

    lda     #$7f
    pha
    plb
; DB = $7f
    jsl     Tad_Init

    phk
    plb
; DB = $80


    ; Enable NMI interrupts
    lda     #$80
    sta     NMITIMEN

    jsr     RunTests


    ; Tests passed
    ; Wait until VBlank
    wai

    stz     CGADD
    lda     #.lobyte(SUCCESS_COLOR)
    sta     CGDATA
    lda     #.hibyte(SUCCESS_COLOR)
    sta     CGDATA

    MainLoop:
        ; Run the tests again and again and again
        jsr     RunTests

        bra     MainLoop
.endproc


