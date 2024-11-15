;; ResetHandler and ResetRegisters
;;
;; https://snes.nesdev.org/wiki/Init_code

.cerror (ResetHandler >> 16) != $80, "ResetHandler MUST be in bank $80"


; SPDX-SnippetBegin
; SDPX—SnippetName: SNESdev Wiki Init code
; SPDX-License-Identifier: CC0-1.0

;; Reset handler.
;;
;; Sets the CPU registers, clears Work-RAM, resets the registers and jumps to Main.
;;
;; `ResetHandler` is from the SNESdev wiki, CC0 licensed https://snes.nesdev.org/wiki/Init_code
;;
;; A unknown
;; X unknown
;; DB unknown
.dpage ?
.databank ?
ResetHandler .proc
    jml _FastRomReset       ; Change Program Bank to a FastROM bank
_FastRomReset:

    clc
    xce                     ; Switch to native mode

    ; Reset Status Flags
    ;   n (negative flag) clear
    ;   v (overflow flag) clear
    ;   m (memory size)   set   - 8 bit Accumulator
    ;   x (index size)    clear - 16 bit Index registers
    ;   d (decimal mode)  clear - decimal mode disabled
    ;   i (irq disable)   set   - IRQ interrupts disabled
    ;   z (zero flag)     clear
    ;   c (carry flag)    clear
    rep #$ff
    sep #$24

; 8 bit accumulator
; 16 bit index


    ldx #STACK_BOTTOM
    txs                     ; Set up Stack Pointer

    pea 0
    pld                     ; Reset Direct Page register to 0
.dpage 0

    phk
    plb                     ; Set Data Bank to Program Bank
                            ; addr addressing mode can now access PPU/MMIO/DMA registers if PB is $00 - $3F or $80 - $BF
.databank $0


    stz NMITIMEN            ; Disable interrupts
    stz HDMAEN              ; Disable HDMA

    lda #$8f
    sta INIDISP             ; Disable screen


    ; Fill Work-RAM with zeros using two 64KiB fixed address DMA transfers to WMDATA
    stz WMADDL
    stz WMADDM
    stz WMADDH              ; Set VMDATA address

    lda #$08
    sta DMAP0               ; Fixed address transfer to a byte register

    lda #WMDATA & $ff
    sta BBAD0               ; DMA transfer to WMDATA

    ldx #<>WorkRamResetByte
    stx A1T0
    lda #`WorkRamResetByte
    sta A1B0                ; Set DMA source to `WorkRamResetByte`

    ldx #0
    stx DAS0                ; Transfer size = 64KiB

    lda #1
    sta MDMAEN              ; First DMA Transfer

    ; x = 0
    stx DAS0                ; Transfer size = 64KiB

    ; a = 1
    sta MDMAEN              ; Second DMA Transfer


    ; Reset PPU
    jsr ResetRegisters

    jml Main                ; Jump to Main


; Byte value to use when clearing Work-RAM
; (can be moved to any ROM bank)
WorkRamResetByte:
    .byte 0
.endproc

; SPDX-SnippetEnd



;; Reset the registers
;;
;; NOTE: Does not reset the DMA registers
;;
;; Registers to reset taken from the SNESdev wiki: https://snes.nesdev.org/wiki/Init_code
.as
.xl
.databank 0
ResetRegisters .proc
; Must reset first
    stz     NMITIMEN    ; disable Interrupts
    stz     HDMAEN      ; disable HDMA

    lda     #$8f
    sta     INIDISP     ; enable Force Blank, full brightness

; CPU registers
    stz     MEMSEL      ; set to 1 if FastROM

    lda     #$ff
    sta     WRIO

; Objects
    stz     OBJSEL
    stz     OAMADDL
    stz     OAMADDH     ; disable OAM priority rotation

; Backgrounds
    stz     BGMODE
    stz     MOSAIC

    stz     BG1SC
    stz     BG2SC
    stz     BG3SC
    stz     BG4SC

    stz     BG12NBA
    stz     BG34NBA

; Scroll Registers
    lda     #$ff

    stz     BG1HOFS
    stz     BG1HOFS     ; set horizontal offset to 0
    sta     BG1VOFS
    sta     BG1VOFS     ; set vertical offset to -1
    stz     BG2HOFS
    stz     BG2HOFS
    sta     BG2VOFS
    sta     BG2VOFS
    stz     BG3HOFS
    stz     BG3HOFS
    sta     BG3VOFS
    sta     BG3VOFS
    stz     BG4HOFS
    stz     BG4HOFS
    sta     BG4VOFS
    sta     BG4VOFS

; VRAM Registers
    lda     #$80
    sta     VMAIN       ; VRAM word access, increment by 1, no remapping

; Mode 7
    stz     M7SEL       ; no flipping or screen repeat

    ; Identity matrix
    lda     #1
    stz     M7A
    sta     M7A
    stz     M7B
    stz     M7B
    stz     M7C
    stz     M7C
    stz     M7D
    sta     M7D
    stz     M7X
    stz     M7X
    stz     M7Y
    stz     M7Y

; Windows
    stz     W12SEL
    stz     W34SEL
    stz     WOBJSEL
    stz     WH0
    stz     WH1
    stz     WH2
    stz     WH3
    stz     WBGLOG
    stz     WOBJLOG

; Layer Enable
    stz     TM
    stz     TS
    stz     TMW
    stz     TSW

; Color Math
    stz     CGWSEL      ; everywhere
    stz     CGADSUB

    lda     #$e0
    sta     COLDATA     ; set Fixed color data to black

; Misc
    stz     SETINI

    rts
.endproc


