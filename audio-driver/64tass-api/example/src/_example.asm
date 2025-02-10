; TAS 64tass example

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


ROM_NAME    = "TAD 64TASS EXAMPLE"
VERSION     = 0
REGION      = REGION__Japan
ROM_SPEED   = ROM_SPEED__Fast
CART_TYPE   = CART_TYPE__RomOnly


.cpu "65816"
.autsiz

.weak
    HIROM = 0
    LOROM = 0
.endweak

.if LOROM != 0
    MEMORY_MAP = "LOROM"
    TAD_MEMORY_MAP = "LOROM"
    .include "memory-map-lorom.inc"

.elsif HIROM != 0
    MEMORY_MAP = "HIROM"
    TAD_MEMORY_MAP = "HIROM"
    .include "memory-map-hirom.inc"

.else
    .error "Unknown memory map"
.endif


; Variables

.section Zeropage
    joypadCurrent .word ?
    joypadPressed .word ?

    .include "../../tad-zeropage.inc"
.send

.section Lowram
    .include "../../tad-lowram.inc"
.send


; Includes

.dpage 0
.databank ?

.include "registers.inc"
.include "snes-header.inc"

.include "../gen/audio.inc"

.section Code
    .include "reset.inc"
    .include "break-isr.inc"
    .include "main.inc"

    .include "../../tad-process.inc"
    .include "../../tad-code.inc"
.send

.if HIROM != 0
    .include "../gen/audio-data-hirom.inc"
.elsif LOROM != 0
    .include "../gen/audio-data-lorom.inc"
.else
    .error "Unknown memory map"
.endif

