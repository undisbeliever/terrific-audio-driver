;; A basic NMI ISR with lag-frame detection
;;
;; TO USE:
;;   * Create a macro called `VBlank` (Invoked with A8, I16, D = 0, DB = $00)
;;   * Include `nmi-isr.inc` AFTER the `VBlank` macro
;;
;; You MUST call `WaitFrame` at the end of the frame.  The `VBlank` macro will not run
;; unless `WaitFrame` was called.
;;

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

.export WaitFrame


.bss
    ;; Used to prevent the VBlank Interrupt from writing to the PPU during a lag frame
    vBlankFlag: .res 1


.code

;; Signals to the VBlank Interrupt the frame is completed and waits until the start of the next frame.
;;
;; A8
;; I16
;; DB access lowram
.proc WaitFrame
    lda     #1
    sta     vBlankFlag

    ; Loop until `vBlankFlag` cleared
    Loop:
        wai
        lda     vBlankFlag
        bne     Loop

    rts
.endproc


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
    .assert .bankbyte(*) = $80, lderror, "NmiHandler is not in a FastROM bank"
    jml     @Fast
@Fast:

    rep #$38            ; 16 bit A, 16 bit I, decimal mode disabled
.a16
.i16
    pha
    phx
    phy
    phb
    phd

    ; Reset DB and DP registers

    .assert .bankbyte(*) & $7f = 0, lderror, "NmiIsr is not in bank $00 or $80"
    phk
    plb
; DB = 0x80 or 0x00 (can access Low-RAM, PPU registers, DMA registers and MMIO registers)

    lda     #0
    tcd
; DP = 0

    sep     #$20
.a8

    lda     vBlankFlag
    bne     VBlankMacro
        jmp     SkipVblankMacro

    VBlankMacro:
        VBlank

        .assert .asize = 8, error, "VBlank macro must exit with 8 bit A"
        stz     vBlankFlag
SkipVblankMacro:

    rep #$30
.a16
.i16

    ; Restore CPU registers
    .assert .asize = 16 && .isize = 16, error, "Invalid register sizes"
    pld
    plb
    ply
    plx
    pla

    rti
.endproc

