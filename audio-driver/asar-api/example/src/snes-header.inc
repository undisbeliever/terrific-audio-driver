; SNES Header

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


!REGION__Japan = $00
!REGION__US = $01
!REGION__Europe = $02


!CART_TYPE__RomOnly = $00
!CART_TYPE__RomRam  = $01
!CART_TYPE__RomSram = $02


!ROM_SPEED__Slow = $00
!ROM_SPEED__Fast = $10


SnesHeader: {
    assert pc()&$7fffff == $00ffb0, "SnesHeader is in the wrong location"

        db  "  "                 ; $ffb0 - 2 Digit marker code (blank - unlicensed)
        db  "    "               ; $ffb2 - 4 Character game code (blank - unlicensed)
        db  0, 0, 0, 0, 0, 0, 0  ; $ffb6 - fixed value (7 bytes)

        db  0                    ; $ffbd - expansion RAM size
        db  0                    ; $ffbe - special version
        db  0                    ; $ffbf - cartridge type sub-number

    .RomName:
        db "ASAR TAD EXAMPLE     "
    assert pc()&$7fffff == $ffd5, "ROM_NAME is not 21 characters long"

    ; $ffd5 - map mode
    if defined("LOROM")
        db  $20|!ROM_SPEED__Slow
    elseif defined("HIROM")
        db  $21|!ROM_SPEED__Slow
    endif

        db  !CART_TYPE__RomOnly     ; $ffd6 - cartridge type
        db  log2(2)                 ; $ffd7 - ROM size
        db  0                       ; $ffd8 - RAM size
        db  !REGION__Japan          ; $ffd9 - Destination code
        db  $33                     ; $ffda - fixed value
        db  0                       ; $ffdb - ROM version
        dw  $aaaa                   ; $ffdc - checksum compliment
        dw  $5555                   ; $ffde - checksum

    ; Native Interrupt Vectors
        dw 0                        ; $ffe0
        dw 0                        ; $ffe2
        dw CopHandler               ; $ffe4 - native cop
        dw BreakHandler             ; $ffe6 - native brk
        dw UnusedInterruptHandler   ; $ffe8 - native abort
        dw NmiHandler               ; $ffea - native nmi
        dw 0                        ; $ffec
        dw IrqHandler               ; $ffee - native irq

    ; Emulated Interrupt Vectors
        dw 0                        ; $fff0
        dw 0                        ; $fff2
        dw UnusedInterruptHandler   ; $fff4 - emu cop
        dw 0                        ; $fff6
        dw UnusedInterruptHandler   ; $fff8 - emu abort
        dw UnusedInterruptHandler   ; $fffa - emu nmi
        dw ResetHandler             ; $fffc - emu reset
        ;dw UnusedInterruptHandler   ; $fffe - emu brk/irq

    assert pc()&$0000 == $0000
}

