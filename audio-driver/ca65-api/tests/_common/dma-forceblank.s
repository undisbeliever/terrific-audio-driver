;; Force-Blank DMA functions

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


.export DmaToCgram_ForceBlank
.export DmaToVramL_ForceBlank, DmaFillToVramH_ForceBlank, DmaFillToVram_ForceBlank


.include "registers.inc"

.importzp zpTmpByte


;; TIMING: No active HDMA channels, force-blank
;; Uses DMA channel 0
;;
;; IN: CGADD - destination address
;; IN: A:X - address
;; IN: Y - size
.a8
.i16
;; DB access registers
.proc DmaToCgram_ForceBlank
    stx     A1T0
    sta     A1B0
    sty     DAS0

    ; one register to CGDATA
    ldx     #DMAP_TRANSFER_WRITE_TWICE | (.lobyte(CGDATA) << 8)
    stx     DMAP0

    lda     #1
    sta     MDMAEN

    rts
.endproc


;; TIMING: No active HDMA channels, force-blank
;; Uses DMA channel 0
;;
;; IN: VMADD - destination word address
;; IN: A:X - address
;; IN: Y - size
.a8
.i16
;; DB access registers
.proc DmaToVramL_ForceBlank
    stx     A1T0
    sta     A1B0
    sty     DAS0

    ; Set VMAIN to low-byte access
    stz     VMAIN

    ; one register to VMDATAL
    ldx     #DMAP_TRANSFER_ONE | (.lobyte(VMDATAL) << 8)
    stx     DMAP0

    lda     #1
    sta     MDMAEN

    rts
.endproc


;; TIMING: No active HDMA channels, force-blank
;; Uses DMA channel 0
;;
;; IN: VMADD - destination word address
;; IN: A - fill value
;; IN: Y - size
.a8
.i16
;; DB access registers
.proc DmaFillToVramH_ForceBlank
    sta     zpTmpByte

    ldx     #zpTmpByte
    stx     A1T0
    stz     A1B0

    sty     DAS0

    ; Set VMAIN to high-byte access
    lda     #$80
    sta     VMAIN

    ; one fixed register to VMDATAH
    ldx     #DMAP_FIXED | DMAP_TRANSFER_ONE | (.lobyte(VMDATAH) << 8)
    stx     DMAP0

    lda     #1
    sta     MDMAEN

    rts
.endproc


;; TIMING: No active HDMA channels, force-blank
;; Uses DMA channel 0
;;
;; IN: VMADD - destination word address
;; IN: A - fill value
;; IN: Y - size
.a8
.i16
;; DB access registers
.proc DmaFillToVram_ForceBlank
    sta     zpTmpByte

    ldx     #zpTmpByte
    stx     A1T0
    stz     A1B0

    sty     DAS0

    ; Set VMAIN to word access
    lda     #$80
    sta     VMAIN

    ; two registers to VMDATAH
    ldx     #DMAP_FIXED | DMAP_TRANSFER_TWO | (.lobyte(VMDATA) << 8)
    stx     DMAP0

    lda     #1
    sta     MDMAEN

    rts
.endproc
