;; SNES Header
;;
;; This header is intended to be included in an assembly file.
;; To configure the header, the following defines and constants must be created before `snes-header.inc` is included:
;;
;; Defines:
;;  * A `HIROM` or `LOROM` define to select the memory mapping
;;  * `ROM_NAME` (max 21 characters)
;;
;; Constants:
;;  * VERSION   - 8 bit version
;;  * ROM_SIZE  - ROM size in MegaBits
;;  * REGION    - Region byte (see `REGION__*` constants)
;;  * CART_TYPE - Cartridge type (see `CART_TYPE__*` constants)
;;  * ROM_SPEED - SlowROM or FastROM bit (see `ROM_SPEED__*` constants)

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


.global ResetHandler, NmiHandler, IrqHandler, BreakHandler, CopHandler

;; Unused interrupt ISR
;; CAUTION: This ISR is used in emulation mode.
.global UnusedInterruptHandler


REGION__Japan = $00
REGION__US = $01
REGION__Europe = $02


CART_TYPE__RomOnly = $00
CART_TYPE__RomRam  = $01
CART_TYPE__RomSram = $02


ROM_SPEED__Slow = $00
ROM_SPEED__Fast = $10


.scope SnesHeader
    .if .defined(LOROM) && .defined(HIROM)
        .error "Cannot use HIROM and LOROM at the same time"
    .elseif .defined(LOROM)
        MAPPER = $20
    .elseif .defined(HIROM)
        MAPPER = $21
    .else
        .error "Missing HIROM or LOROM define"
    .endif

    __HEADER_MAP_MODE = MAPPER | (::ROM_SPEED & $10)


    ; ROM_SIZE = Rom size in megabits
    .if ::ROM_SIZE <= 1
        __HEADER_ROM_SIZE = $7
    .elseif ::ROM_SIZE <= 2
        __HEADER_ROM_SIZE = $8
    .elseif ::ROM_SIZE <= 4
        __HEADER_ROM_SIZE = $9
    .elseif ::ROM_SIZE <= 8
        __HEADER_ROM_SIZE = $a
    .elseif ::ROM_SIZE <= 16
        __HEADER_ROM_SIZE = $b
    .elseif ::ROM_SIZE <= 32
        __HEADER_ROM_SIZE = $c
    .elseif ::ROM_SIZE <= 64
        __HEADER_ROM_SIZE = $d
    .else
        .error "Invalid ROM_SIZE"
    .endif


    SP = $20    ; Space character

    .segment "SNESHEADER"

    SnesHeader:
        .assert * & $7fffff = $00ffb0, lderror, "SNESHEADER is in the wrong location"

        .byte  SP, SP               ; $ffb0 - 2 Digit marker code (blank - unlicensed)
        .byte  SP, SP, SP, SP       ; $ffb2 - 4 Character game code (blank - unlicensed)
        .byte  0, 0, 0, 0, 0, 0, 0  ; $ffb6 - fixed value (7 bytes)

        .byte  0                    ; $ffbd - expansion RAM size
        .byte  0                    ; $ffbe - special version
        .byte  0                    ; $ffbf - cartridge type sub-number

    @RomName:
        .byte  .sprintf("% 21s", ROM_NAME) ; $ffc0 - ROM name (21 characters)
            .if * - @RomName <> 21
                .error "ROM_NAME is too long (max 21 characters)"
            .endif

    .assert .loword(*) = $ffd5, lderror

        .byte  __HEADER_MAP_MODE    ; $ffd5 - map mode
        .byte  ::CART_TYPE          ; $ffd6 - cartridge type
        .byte  __HEADER_ROM_SIZE    ; $ffd7 - ROM size
        .byte  0                    ; $ffd8 - RAM size
        .byte  ::REGION             ; $ffd9 - Destination code
        .byte  $33                  ; $ffda - fixed value
        .byte  ::VERSION            ; $ffdb - ROM version
        .word  $aaaa                ; $ffdc - checksum compliment
        .word  $5555                ; $ffde - checksum

    ; Native Interrupt Vectors
        .addr 0                                 ; $ffe0
        .addr 0                                 ; $ffe2
        .addr .loword(CopHandler)               ; $ffe4 - native cop
        .addr .loword(BreakHandler)             ; $ffe6 - native brk
        .addr .loword(UnusedInterruptHandler)   ; $ffe8 - native abort
        .addr .loword(NmiHandler)               ; $ffea - native nmi
        .addr 0                                 ; $ffec
        .addr .loword(IrqHandler)               ; $ffee - native irq

    ; Emulated Interrupt Vectors
        .addr 0                                 ; $fff0
        .addr 0                                 ; $fff2
        .addr .loword(UnusedInterruptHandler)   ; $fff4 - emu cop
        .addr 0                                 ; $fff6
        .addr .loword(UnusedInterruptHandler)   ; $fff8 - emu abort
        .addr .loword(UnusedInterruptHandler)   ; $fffa - emu nmi
        .addr .loword(ResetHandler)             ; $fffc - emu reset
        .addr .loword(UnusedInterruptHandler)   ; $fffe - emu brk/irq
    SnesHeaderEnd:

        .assert .loword(SnesHeaderEnd) = $0000, lderror
.endscope

.code


