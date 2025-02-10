; TAD 64tass API zeropage variables
;
; These variables can be modified outside of the TAD API
;
; These variables must be placed somewhere in the lowram segment
; ($0000 - $1fff or $7e0000-$7e1ffff).
;
; These variables should be placed in zeropage, but it is not required.


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


.cerror (* >= $2000) && ((* < $7e0000) || (* >= $7e2000)), "tad-zeropage.inc is not in lowram"


; Maximum pan value (100% to the right)
TAD_MAX_PAN = 128

; Center pan value (default if pan is not specified)
TAD_CENTER_PAN = TAD_MAX_PAN / 2


; ---------------------------------------
; Queue 4 - The next sound effect to play
; ---------------------------------------
    ; The next sound effect id to send to the audio driver.
    ;
    ; If `Tad_sfxQueue_sfx == $ff` then the queue is considered empty.
    Tad_sfxQueue_sfx .byte ?

    ; The pan value to play `Tad_sfxQueue_sfx` with.
    ;
    ; If this value is greater than `TAD_MAX_PAN`, then `TAD_CENTER_PAN` will be used instead.
    Tad_sfxQueue_pan .byte ?

