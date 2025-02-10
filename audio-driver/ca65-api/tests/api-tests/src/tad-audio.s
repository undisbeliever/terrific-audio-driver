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


.if .defined(LOROM)
    ; Move TAD_PROCESS_SEGMENT to a different bank.
    .define TAD_PROCESS_SEGMENT "CODE7"
.elseif .defined(HIROM)
    ; Not enough banks to move TAD_PROCESS_SEGMENT to the last bank.
    .define TAD_CODE_SEGMENT    "CODE"
    .define TAD_PROCESS_SEGMENT "CODE1"
.endif

.include "../../../tad-audio.s"


; Export command queue so tests can read it
.export TadPrivate_nextCommand_id, TadPrivate_nextCommand_parameter0, TadPrivate_nextCommand_parameter1

