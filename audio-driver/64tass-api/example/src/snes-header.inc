;; SNES Header
;;
;; To configure the header `snes-header.inc` will access the following constants:
;;
;; Strings:
;;  * `MEMORY_MAP` - "LOROM" or "HIROM"
;;  * `ROM_NAME` - (max 21 characters)
;;
;; Integer constants:
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


REGION__Japan = $00
REGION__US = $01
REGION__Europe = $02


CART_TYPE__RomOnly = $00
CART_TYPE__RomRam  = $01
CART_TYPE__RomSram = $02


ROM_SPEED__Slow = $00
ROM_SPEED__Fast = $10


.section SnesHeader

SnesHeader .block
    _MAPPER = { "HIROM": $21, "LOROM": $20 }[MEMORY_MAP]

    _HEADER_MAP_MODE = _MAPPER | (ROM_SPEED & $10)


    ; ROM_SIZE = Rom size in megabits
    .if ROM_SIZE <= 1
        _HEADER_ROM_SIZE = $7
    .elsif ROM_SIZE <= 2
        _HEADER_ROM_SIZE = $8
    .elsif ROM_SIZE <= 4
        _HEADER_ROM_SIZE = $9
    .elsif ROM_SIZE <= 8
        _HEADER_ROM_SIZE = $a
    .elsif ROM_SIZE <= 16
        _HEADER_ROM_SIZE = $b
    .elsif ROM_SIZE <= 32
        _HEADER_ROM_SIZE = $c
    .elsif ROM_SIZE <= 64
        _HEADER_ROM_SIZE = $d
    .else
        .error "Invalid ROM_SIZE ", ROM_SIZE
    .endif


    _SP = $20    ; Space character

        .cerror * & $7fffff != $00ffb0, lderror, "SnesHeader is in the wrong location"

        .byte  _SP, _SP             ; $ffb0 - 2 Digit marker code (blank - unlicensed)
        .byte  _SP, _SP, _SP, _SP   ; $ffb2 - 4 Character game code (blank - unlicensed)
        .byte  0, 0, 0, 0, 0, 0, 0  ; $ffb6 - fixed value (7 bytes)

        .byte  0                    ; $ffbd - expansion RAM size
        .byte  0                    ; $ffbe - special version
        .byte  0                    ; $ffbf - cartridge type sub-number

    _RomName:
    .encode
        .cdef " ~", 32
        .text  format("%-21s", ROM_NAME)   ; $ffc0 - ROM name (21 characters)
           .cerror * - _RomName != 21, "ROM_NAME is too long (max 21 characters)"
    .endencode

        .byte  _HEADER_MAP_MODE         ; $ffd5 - map mode
        .byte  CART_TYPE                ; $ffd6 - cartridge type
        .byte  _HEADER_ROM_SIZE         ; $ffd7 - ROM size
        .byte  0                        ; $ffd8 - RAM size
        .byte  REGION                   ; $ffd9 - Destination code
        .byte  $33                      ; $ffda - fixed value
        .byte  VERSION                  ; $ffdb - ROM version
        .word  $aaaa                    ; $ffdc - checksum compliment
        .word  $5555                    ; $ffde - checksum

    ; Native Interrupt Vectors
        .word 0                         ; $ffe0
        .word 0                         ; $ffe2
        .word <>CopHandler              ; $ffe4 - native cop
        .word <>BreakHandler            ; $ffe6 - native brk
        .word <>UnusedInterruptHandler  ; $ffe8 - native abort
        .word <>NmiHandler              ; $ffea - native nmi
        .word 0                         ; $ffec
        .word <>IrqHandler              ; $ffee - native irq

    ; Emulated Interrupt Vectors
        .word 0                         ; $fff0
        .word 0                         ; $fff2
        .word <>UnusedInterruptHandler  ; $fff4 - emu cop
        .word 0                         ; $fff6
        .word <>UnusedInterruptHandler  ; $fff8 - emu abort
        .word <>UnusedInterruptHandler  ; $fffa - emu nmi
        .word <>ResetHandler            ; $fffc - emu reset
        .word <>UnusedInterruptHandler  ; $fffe - emu brk/irq

        .cerror (* & $ffff) != $0000, "Invalid header size"
.endblock

.send

