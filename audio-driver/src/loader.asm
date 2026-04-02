; Terrific Audio Driver Data Loader
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


.include "registers.inc"
.include "io-commands.inc"
.include "common-memmap.inc"


.codebank LOADER_ADDR..(LOADER_ADDR + LOADER_SIZE)


; Loader can only write to even addresses
.assert LOADER_ADDR % 2 == 0
.assert CODE_ADDR % 2 == 0

; Confirm LoaderIO and DriverIO use the same switch to loader signal
.assert DriverIO__SWITCH_TO_LOADER_PORT == LoaderIO_TransferToLoader__SPINLOCK_PORT
.assert DriverIO__SWITCH_TO_LOADER_BIT == LoaderIO_TransferToLoader__SPINLOCK_SWITCH_TO_LOADER_BIT


.proc loader
    ; MUST be the first thing in the loader_code bank
    .assert PC == LOADER_ADDR

    ; Setup stack
    mov X, #$ff
    mov SP, X

    ; Set Direct Page to 0
    clrp
.p0

    ; Soft reset the DSP
    mov DSPADDR, #DSP_FLG
    mov DSPDATA, #DSP_FLG__SOFT_RESET | DSP_FLG__MUTE_ALL | DSP_FLG__ECHO_DISABLE

    ; Clear IO ports
    mov CONTROL, #CONTROL__RESET_PORTS_01 | CONTROL__RESET_PORTS_23

    ; Tell the S-CPU the S-SMP is running the loader
    mov A, #DriverIO__MODE_LOADER
    mov DriverIO__MODE_PORT, A

    ; Send 'ready' signal
    .assert LoaderIO__LOADER_READY_L == DriverIO__MODE_LOADER
    .assert LoaderIO_InitToScpu__READY_PORT_L + 1 == LoaderIO_InitToScpu__READY_PORT_H
    ; A = LoaderIO__LOADER_READY_L
    mov Y, #LoaderIO__LOADER_READY_H
    movw LoaderIO_InitToScpu__READY_PORT_L, YA

    ; Wait for S-CPU to respond to the ready signal
    InitSpinloop:
        cmpw YA, LoaderIO_InitToLoader__READY_PORT_L
        bne InitSpinloop


    ; retrieve LoaderDataType from S-CPU
    mov Y, LoaderIO_InitToLoader__DATA_TYPE_PORT
    mov loaderDataType, Y

    ; Get the address to store the data
    .assert LoaderDataType__CODE == 0
    .assert LoaderDataType__SONG_DATA_BIT == 7
    bne DtNotZero
        ; Clear global music and sfx volume
        .assert globalVolume_music + 1 == globalVolume_sfx
        mov A, Y
        movw globalVolume_music, YA

        mov A, #lobyte(CODE_ADDR)
        mov Y, #hibyte(CODE_ADDR)

        bra DtEndIf

    DtNotZero:
        bmi DtIsSong

        mov A, #lobyte(COMMON_DATA_ADDR)
        mov Y, #hibyte(COMMON_DATA_ADDR)

        bra DtEndIf

    DtIsSong:
        ; Assumes `_songPtr` is even
        movw YA, songPtr
    DtEndIf:

    ; Write the high byte of the address to the two `mov !abs+Y, A` instructions.
    mov STA_1 + 2, Y
    mov STA_2 + 2, Y

    mov Y, A

    ; Acknowledge LoaderDataType
    ; And override the `RD` ready signal.
    mov LoaderIO_InitAckToScpu__ACK_PORT, #LoaderIO_InitAckToScpu__ACK_VALUE

    mov X, #LoaderIO__LOADER_READY_H

    ; Have to use a manual loop so the above code has access to the `STA_1` and `STA_2` labels.
    Loop:
        ; Y = low byte of data address
        ; X = last value written to IO port 3

        ; Wait until S-CPU has written the data to the IO ports
        SpinLoop:
            cmp X, LoaderIO_TransferToLoader__SPINLOCK_PORT
            beq SpinLoop

        ; Check if this data is correct, and get the byte to send back to the S-CPU
        mov X, LoaderIO_TransferToLoader__SPINLOCK_PORT
        ; Transfer is complete when spinlock is negative.
        bmi EndLoop


        ; Read data from the IO ports and store to memory
        mov A, LoaderIO_TransferToLoader__DATA_PORT_L
    STA_1:
        mov $ff00 + Y, A

        mov A, LoaderIO_TransferToLoader__DATA_PORT_H

        ; Acknowledge data (S-CPU will start loading new data)
        mov LoaderIO_TransferToScpu__SPINLOCK_ACK_PORT, X

        ; This code is executed while the S-CPU loads the next data into ports 0 & 1
        inc Y
    STA_2:
        mov $ff00 + Y, A
        inc Y

        ; Increment high byte of stores when Y wraps
        bne Loop
            inc STA_1 + 2
            inc STA_2 + 2
            bra  Loop
    EndLoop:

    ; Y = low byte of data address

    ; Acknowledge end of data
    ; (x is negative)
    mov LoaderIO_TransferToScpu__SPINLOCK_ACK_PORT, X

    ; Restart loader if the SWITCH_TO_LOADER_BIT is set
    mov A, X
    and A, #(1 << LoaderIO_TransferToLoader__SPINLOCK_SWITCH_TO_LOADER_BIT)
    bne loader


    ; Set songPtr if dataType is CAD
    mov X, loaderDataType
    cmp X, #LoaderDataType__COMMON_DATA
    bne NotCommonData
        ; Save current data address in `songPtr`
        mov A, Y
        mov Y, STA_1 + 2
        movw songPtr, YA
    NotCommonData:

    ; Restart loader if LoaderDataType__SONG_DATA_BIT is set
    .assert LoaderDataType__SONG_DATA_BIT == 7
    ; X = loaderDataType
    mov A, X
    bpl loader


    ; Fallthrough into `main` in `audio-driver.asm`
    .assert PC == CODE_ADDR
.endproc

