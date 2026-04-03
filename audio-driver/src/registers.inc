; S-SMP and S-DSP register definitions
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


; Register addresses from the snesdev wiki:
;   * https://snes.nesdev.org/wiki/S-SMP#Registers
;   * https://snes.nesdev.org/wiki/S-DSP_registers


; Global DSP Registers
; ====================
    DSP_MVOL_L  = $0c   ; Left channel main volume (signed)
    DSP_MVOL_R  = $1c   ; Right channel main volume (signed)
    DSP_EVOL_L  = $2c   ; Left channel echo volume (signed)
    DSP_EVOL_R  = $3c   ; Right channel echo volume (signed)
    DSP_KON     = $4c   ; Key on
    DSP_KOFF    = $5c   ; Key off
    DSP_FLG     = $6c   ; Flags
    DSP_ENDX    = $7c   ; end of sample flag for each channel (read only)
    DSP_EFB     = $0d   ; Echo feedback (signed)
    DSP_PMON    = $2d   ; Pitch modulation enable flags
    DSP_NON     = $3d   ; Noise enable flags
    DSP_EON     = $4d   ; Echo enable flags
    DSP_DIR     = $5d   ; Sample table address (page number (addr / $100))
    DSP_ESA     = $6d   ; Start of echo memory (page number (addr / $100))
    DSP_EDL     = $7d   ; Echo delay time

    ; Echo filter coefficients
    DSP_C0      = $0f
    DSP_C1      = $1f
    DSP_C2      = $2f
    DSP_C3      = $3f
    DSP_C4      = $4f
    DSP_C5      = $5f
    DSP_C6      = $6f
    DSP_C7      = $7f


; Voice DSP Registers
; ===================

; DSP address = `$10 * voice + voice_register
    VxVOL_L     = $0    ; Left channel volume (signed)
    VxVOL_R     = $1    ; Right channel volume (signed)
    VxPITCH_L   = $2    ; Sample pitch low byte
    VxPITCH_H   = $3    ; Sample pitch high byte (6 bits)
    VxSCRN      = $4    ; Sample source
    VxADSR1     = $5    ; ADSR settings
    VxADSR2     = $6    ; ADSR settings
    VxGAIN      = $7    ; Gain settings

    VxENVX      = $8    ; 7 bit ASDR/GAIN envelope (read only)
    VxOUTX      = $9    ; Signed 8 bit current sample value (read only)


; DSP register constants
; ======================
    DSP_FLG__SOFT_RESET         = $80
    DSP_FLG__MUTE_ALL           = $40
    DSP_FLG__ECHO_DISABLE       = $20
    DSP_FLG__NOISE_FREQ_MASK    = $1f

    VxADSR1_ENABLE_BIT  = 7


; S-SMP registers
; ===============
    ; $f0 is an undocumented test register

    CONTROL     = $f1
    DSPADDR     = $f2
    DSPDATA     = $f3

    CPUIO0      = $f4
    CPUIO1      = $f5
    CPUIO2      = $f6
    CPUIO3      = $f7

    T0TARGET    = $fa
    T1TARGET    = $fb
    T2TARGET    = $fc

    T0OUT       = $fd
    T1OUT       = $fe
    T2OUT       = $ff


; S-SMP register constants
; ========================
    CONTROL__ENABLE_TIMER_0 = 1 << 0
    CONTROL__ENABLE_TIMER_1 = 1 << 1
    CONTROL__ENABLE_TIMER_2 = 1 << 2
    CONTROL__RESET_PORTS_01 = 1 << 4
    CONTROL__RESET_PORTS_23 = 1 << 5
    CONTROL__ENABLE_IPL     = 1 << 7

    TIMER_0_HZ =  8000
    TIMER_1_HZ =  8000
    TIMER_2_HZ = 64000

