; LOROM memory map

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


; 2Mbits = 256KiB
ROM_SIZE = 2


* = 0                   ; ROM offset
.logical $808000        ; start address
    .dsection Code
        .cerror * > $80ffb0, "Bank 80 overflow by ", * - $80ffb0
.here

* = $007fb0             ; ROM offset
.logical $80ffb0        ; start address
    .dsection SnesHeader
        .cerror * != $810000, "Missing header"
.here

* = $008000             ; ROM offset
.logical $818000        ; start address
    .dsection AudioData0
        .cerror * > $820000, "Bank 81 overflow by ", * - $820000
.here

* = $010000             ; ROM offset
.logical $828000        ; start address
    .dsection AudioData1
        .cerror * > $830000, "Bank 82 overflow by ", * - $830000
.here

* = $018000             ; ROM offset
.logical $838000        ; start address
    .dsection AudioData2
        .cerror * > $840000, "Bank 83 overflow by ", * - $840000
.here

* = $020000             ; ROM offset
.logical $848000        ; start address
    .dsection AudioData3
        .cerror * > $850000, "Bank 84 overflow by ", * - $850000
.here

* = $028000             ; ROM offset
.logical $858000        ; start address
    .dsection AudioData4
        .cerror * > $860000, "Bank 85 overflow by ", * - $860000
.here

* = $030000             ; ROM offset
.logical $868000        ; start address
    .dsection AudioData5
        .cerror * > $870000, "Bank 86 overflow by ", * - $870000
.here

* = $038000             ; ROM offset
.logical $878000        ; start address
    .dsection AudioData6
        .cerror * > $880000, "Bank 87 overflow by ", * - $880000
.here

; Padding
* = $03ffff
    .byte 0


; RAM map
START_TOP    = $1fff
STACK_BOTTOM = $1f80

* = $0000
.dsection Zeropage
    .cerror * > $100, "zeropage overflow"

* = $0100
.dsection Lowram
    .cerror * > STACK_BOTTOM, "lowram section overflow"

* = $7e2000
.dsection Wram7e
    .cerror * > $7effff, "wram7e section overflow"

* = $7f0000
.dsection Wram7f
    .cerror * > $7fffff, "wram7f section overflow"


* = 0


