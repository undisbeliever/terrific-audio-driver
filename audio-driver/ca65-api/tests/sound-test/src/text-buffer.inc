;; A basic test buffer

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


.p816

; subroutines
.import TextBuffer_SetupPpu
.import TextBuffer_SetPaletteBlock
.import TextBuffer_PrintString, TextBuffer_PrintString_rodata
.import TextBuffer_PrintPadded_A8, TextBuffer_PrintPadded_X16


; variables
.import TextBuffer_charBufferChangedIfZero
.import TextBuffer_charBuffer : far
.import TextBuffer_charBuffer_SIZE

.import TextBuffer_attrBufferChangedIfZero
.import TextBuffer_attrBuffer : far
.import TextBuffer_attrBuffer_SIZE


; external constants
.global TEXT_BUFFER_VRAM_MAP_WADDR, TEXT_BUFFER_VRAM_TILES_WADDR


;; Copies the text buffer to VRAM if it is dirty
;;
;; TIMING: Vertical-blank
;; Uses DMA channel 0
;;
;;
;; DB access registers
.macro TextBuffer_VBlank
    .local SkipCharBuffer, SkipAttrBuffer

    .assert .asize = 8, error, "A is not 8"
    .assert .isize = 16, error, "I is not 16"

    lda     TextBuffer_charBufferChangedIfZero
    bne     SkipCharBuffer
        ; Set VMAIN to low-byte access
        stz     VMAIN

        ldx     #TEXT_BUFFER_VRAM_MAP_WADDR
        stx     VMADD

        ; one registers to VMDATAL
        ldx     #($00) | (.lobyte(VMDATAL) << 8)
        stx     DMAP0

        ldx     #.loword(TextBuffer_charBuffer)
        stx     A1T0
        lda     #.bankbyte(TextBuffer_charBuffer)
        sta     A1B0

        ldx     #TextBuffer_charBuffer_SIZE
        stx     DAS0

        lda     #1
        sta     MDMAEN

        ; A is non-zero
        sta     TextBuffer_charBufferChangedIfZero
SkipCharBuffer:

    lda     TextBuffer_attrBufferChangedIfZero
    bne     SkipAttrBuffer
        ; Set VMAIN to high-byte access
        lda     #$80
        sta     VMAIN

        ldx     #TEXT_BUFFER_VRAM_MAP_WADDR
        stx     VMADD

        ; one registers to VMDATAL
        ldx     #($00) | (.lobyte(VMDATAH) << 8)
        stx     DMAP0

        ldx     #.loword(TextBuffer_attrBuffer)
        stx     A1T0
        lda     #.bankbyte(TextBuffer_attrBuffer)
        sta     A1B0

        ldx     #TextBuffer_attrBuffer_SIZE
        stx     DAS0

        lda     #1
        sta     MDMAEN

        ; A is non-zero
        sta     TextBuffer_attrBufferChangedIfZero
SkipAttrBuffer:

.endmacro


;; Converts a x/y position to a buffer index
.define TextBuffer_PosToIndex(xPos, yPos) ( (((xPos) & 31) + ((yPos & 31) * 32)) )


;; Print a string literal
;;
;; IN: xPos - text buffer x position
;; IN: yPos - test buffer y position
;; IN: str  - the text to print
;;
;; DB access lowram
.macro TextBuffer_PrintLiteral xPos, yPos, str
    .assert .asize = 8, error, "A is not 8"
    .assert .isize = 16, error, "I is not 16"

    .local String

    .pushseg
        .rodata
        String:
            .byte str, 0
    .popseg

    ldy     #TextBuffer_PosToIndex(xPos, yPos)
    ldx     #.loword(String)
    jsr     TextBuffer_PrintString_rodata
.endmacro

