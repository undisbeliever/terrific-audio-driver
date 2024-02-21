;; A basic text buffer

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
.smart

; subroutines
.export TextBuffer_SetupPpu
.export TextBuffer_SetPaletteBlock
.export TextBuffer_PrintString, TextBuffer_PrintString_rodata
.export TextBuffer_PrintPadded_A8, TextBuffer_PrintPadded_X16

; variables
.export TextBuffer_charBufferChangedIfZero
.export TextBuffer_charBuffer : far
.export TextBuffer_charBuffer_SIZE

.export TextBuffer_attrBufferChangedIfZero
.export TextBuffer_attrBuffer : far
.export TextBuffer_attrBuffer_SIZE


.import DmaToVramL_ForceBlank, DmaFillToVramH_ForceBlank, DmaFillToVram_ForceBlank

.include "../../_common/registers.inc"


;; =========
;; CONSTANTS
;; =========

BUFFER_WIDTH = 32
BUFFER_HEIGHT = 28

BUFFER_SIZE = BUFFER_WIDTH * BUFFER_HEIGHT

;; VRAM word address of the tilemap
.import TEXT_BUFFER_VRAM_MAP_WADDR

;; VRAM word address of the 2bpp tiles
.import TEXT_BUFFER_VRAM_TILES_WADDR


;; =========
;; VARIABLES
;; =========

.importzp zpTmpWord, zpTmpFarPtr


.bss

;; If zero, `TextBuffer_charBuffer` has been changed and needs to be uploaded to VRAM
TextBuffer_charBufferChangedIfZero: .res 1

;; If zero, `TextBuffer_attrBuffer` has been changed and needs to be uploaded to VRAM
TextBuffer_attrBufferChangedIfZero: .res 1


.segment "WRAM7E"

;; character buffer
;; MUST BE WRITTEN USING FAR ADDRESSING
;; ([u8 ; BUFFER_WIDTH * BUFFER_HEIGHT])
TextBuffer_charBuffer: .res BUFFER_SIZE
TextBuffer_charBuffer_SIZE = .sizeof(TextBuffer_charBuffer)

;; attribute buffer
;; MUST BE WRITTEN USING FAR ADDRESSING
;; ([u8 ; BUFFER_WIDTH * BUFFER_HEIGHT])
TextBuffer_attrBuffer: .res BUFFER_SIZE
TextBuffer_attrBuffer_SIZE = .sizeof(TextBuffer_attrBuffer)


charBuffer       := TextBuffer_charBuffer
charBufferChangedIfZero  := TextBuffer_charBufferChangedIfZero

attrBuffer       := TextBuffer_attrBuffer
attrBufferChangedIfZero  := TextBuffer_attrBufferChangedIfZero


;; ==========
;; PUBLIC API
;; ==========

.code

;; Initialise the text buffer and setup the PPU
;;
;; TIMING: No active HDMA channels, force-blank
;; Uses DMA channel 0
;;
;; NOTE: Does not setup Palette or BG mode
.a8
.i16
;; DB access registers
.proc TextBuffer_SetupPpu
    ldx     #TEXT_BUFFER_VRAM_TILES_WADDR + FONT_TILE_OFFSET * 8
    stx     VMADD

    ldx     #.loword(Font_1bpp)
    lda     #.bankbyte(Font_1bpp)
    ldy     #Font_1bpp_SIZE
    jsr     DmaToVramL_ForceBlank


    ldx     #TEXT_BUFFER_VRAM_TILES_WADDR
    stx     VMADD

    ; Clear the first FONT_TILE_OFFSET tiles
    .assert FONT_TILE_OFFSET > 0, error
    lda     #$00
    ldy     #FONT_TILE_OFFSET * 16
    jsr     DmaFillToVram_ForceBlank

    ; Set the high byte of all font tiles to 1
    lda     #$ff
    ldy     #Font_1bpp_SIZE
    jsr     DmaFillToVramH_ForceBlank


    ; Clear tilemap in VRAM
    ldx     #TEXT_BUFFER_VRAM_MAP_WADDR
    stx     VMADD

    lda     #0
    ldy     #BUFFER_SIZE * 2
    jmp     DmaFillToVram_ForceBlank


; Fallthrough
    .assert * = TextBuffer_Clear, lderror
.endproc



;; Clear the character and attribute buffers
;;
.a8
.i16
;; DB access lowram
.proc TextBuffer_Clear
    rep     #$30
.a16

    ; Clear charBuffer and attrBuffer
    .assert charBuffer + .sizeof(TextBuffer_charBuffer) = attrBuffer, error
    .assert .sizeof(TextBuffer_charBuffer) = .sizeof(TextBuffer_attrBuffer), error
    lda     #0
    ldx     #(BUFFER_SIZE - 1) * 2
    :
        sta     f:charBuffer,x
        dex
        dex
        bpl     :-

    sep     #$20
.a8

    stz     charBufferChangedIfZero
    stz     attrBufferChangedIfZero

    rts
.endproc



;; Set the palette for a block of text
;;
;; IN: A - palette
;; IN: X - start buffer index
;; IN: Y - number of characters
.a8
.i16
;; DB access lowram
.proc TextBuffer_SetPaletteBlock
    ; convert palette to tilemap attribute
    and     #7
    asl
    asl

    @Loop:
        cpx     #BUFFER_SIZE
        bcs     @Break

        sta     f:attrBuffer,x
        inx

        dey
        bne     @Loop

@Break:

    stz     attrBufferChangedIfZero

    rts
.endproc



;; Print a string that is in the .rodata segment
;;
;; IN: X - string addr (inside .rodata segment)
;; IN: Y - text buffer index
;;
;; DB access lowram
.proc TextBuffer_PrintString_rodata
    lda     #RODATA_BANK

; fallthrough
    .assert * = TextBuffer_PrintString, error
.endproc



;; Print a string
;;
;; IN: A:X - string far address
;; IN: Y - text buffer index
;;
;; DB access lowram
.proc TextBuffer_PrintString
    stx     zpTmpFarPtr
    sta     zpTmpFarPtr + 2

    tyx

    ldy     #0
    @Loop:
        lda     [zpTmpFarPtr],y
        beq     @Break

        iny

        cpx     #BUFFER_SIZE
        bcc     :+
            ldx     #0
        :

        sec
        sbc     #FONT_ASCII_OFFSET
        sta     f:charBuffer,x
        inx

        bra     @Loop

@Break:
    stz     charBufferChangedIfZero

    rts
.endproc



;; Print an unsigned 8 bit variable (with padding)
;;
;; IN: A = unsigned 8 bit value to print
;; IN: Y = buffer index
;;
.a8
.i16
;; DB access lowram
.proc TextBuffer_PrintPadded_A8
    rep     #$30
.a16
    and     #$ff
    tax

    sep     #$20
.a8
    lda     #3
    bra     __TextBuffer_PrintPadded_X16
.endproc



;; Print an unsigned 16 bit variable (with padding)
;;
;; IN: X = unsigned 16 bit value to print
;; IN: Y = buffer index
;;
.a8
.i16
;; DB access lowram
.proc TextBuffer_PrintPadded_X16
    lda     #5

; fallthrough
    .assert * = __TextBuffer_PrintPadded_X16, error
.endproc



;; Print a 16 bit variable (with space padding)
;;
;; IN: A = number of characters to print
;; IN: Y = buffer index
;; IN: X = unsigned number to print
;;
.a8
.i16
;; DB access lowram
.proc __TextBuffer_PrintPadded_X16
_indexBeforePadding := zpTmpWord

    phx

    iny
    sty     _indexBeforePadding

    rep     #$31
.a16

    ; carry clear
    and     #$ff
    adc     _indexBeforePadding
    dec
    tax

    pla

    .a16
    ; A = digits
    ; X = buffer Pos
    @Loop:
        sta     f:WRDIV

        sep     #$20
    .a8
        lda     #10
        sta     f:WRDIVB

        ; Must wait 16 cycles

        ; Test for underflow error and double check `charBuffer` write is in bounds
        dex                     ; 2
        cpx     #BUFFER_SIZE    ; 3
        bcs     @Return         ; 2
        nop                     ; 2

        lda     #FONT_ZERO_CHAR ; 2
        clc                     ; 2
        adc     f:RDMPYL        ; +3 from instruction = 16

        sta     f:charBuffer,x

        rep     #$20
    .a16
        lda     f:RDDIV
        bne     @Loop


    sep     #$20
.a8
    ; Draw padding (if required)
    cpx     _indexBeforePadding
    bcc     @SkipPadding
        lda     #FONT_TILE_OFFSET
        @PaddingLoop:
            dex
            sta     f:charBuffer,x

            cpx     _indexBeforePadding
            bcs     @PaddingLoop

@SkipPadding:

@Return:
    stz     charBufferChangedIfZero

    rts
.endproc



;; ====
;; DATA
;; ====

.rodata
    RODATA_BANK = .bankbyte(*)

    .include "font.inc"

