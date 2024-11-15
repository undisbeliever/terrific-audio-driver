; 64tass red screen of death Break ISR
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

;; Break ISR - Show a red screen on crash or `brk`
;;
;; emulation flag unknown
;; A unknown
;; I unknown
.dpage ?
.databank ?
BreakHandler .proc
    .cerror (`*) != $80, "BreakHandler not in bank $80"

COLOR = 31 ; RED

    ; Switch to native mode
    clc
    xce

    rep     #$30
.al
.xl
    lda     #0
    tcd
; D = 0

    sep     #$20
.as
    phk
    plb
; DB = $80
.databank 0


    jsr     ResetRegisters

    ; Interrupts are disabled

    ; Set backdrop
    stz     CGADD
    lda     #<COLOR
    sta     CGDATA
    lda     #>COLOR
    sta     CGDATA

    lda     #15
    sta     INIDISP

    ; Loop forever
    -
        wai
        bra     -
.endproc

