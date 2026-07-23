; Common addresses, sizes and banks used by the loader and the audio driver.
;
; SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
; SPDX-License-Identifier: Zlib
;
; Copyright © 2023 Marcus Rowe <undisbeliever@gmail.com>
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


; LOADER_ADDR must match LOADER_ADDR in the 65816 APIs
LOADER_ADDR = $200

SHARED_ZEROPAGE_ADDR = $00eb

; ::SHOULDDO shrink stack size::
STACK_SIZE = $20
STACK_BOTTOM_ADDR = $200 - STACK_SIZE

BC_CHANNEL_STACK_SIZE = 21
BC_CHANNEL_STACK_OFFSET = 6

LAST_VAR_PAGE_SIZE = (BC_CHANNEL_STACK_SIZE + 11) * 10

; The -28 ensures the pitch-table and BRR directory is page aligned
; `COMMON_DATA_ADDR` MUST be an even address (The loader only works with even addresses).
;
; If this constant is modified, the following should also be updated:
;   * The Tad_AudioDriver_SIZE assert in `ca65-api/tad-audio.s`
;   * The Tad_AudioDriver_SIZE assert in `64tass-api/tad-process.inc`
COMMON_DATA_ADDR = $1100 - 24

LAST_VAR_PAGE_ADDR = COMMON_DATA_ADDR - LAST_VAR_PAGE_SIZE


.varbank zeropage           $0000..SHARED_ZEROPAGE_ADDR
.varbank loader_zeropage    SHARED_ZEROPAGE_ADDR..$00f0
.varbank firstpage          $0100..STACK_BOTTOM_ADDR
.varbank stack              STACK_BOTTOM_ADDR..$0200
.varbank lastvarpage        LAST_VAR_PAGE_ADDR..COMMON_DATA_ADDR


.codebank LOADER_ADDR..LAST_VAR_PAGE_ADDR


