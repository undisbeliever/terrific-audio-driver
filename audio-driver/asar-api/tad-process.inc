; tad-process.inc
; ===============
;
; Terrific Audio Driver Initialisation and processing subroutines
;
;
; This file is is an asar port of the Terrific Audio Driver bass API.
;
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
;


assert bank(TadPrivate.state)&$7f < $40 || bank(TadPrivate.state) == $7e, "Tad variables are not in lowram"
assert (TadPrivate.state&$ffff) < $2000, "Tad variables are not in lowram"

assert bank(TadPrivate.nextCommand_parameter1)&$7f < $40 || bank(TadPrivate.nextCommand_parameter1) == $7e, "Tad variables are not in lowram"
assert (TadPrivate.nextCommand_parameter1&$ffff) < $2000, "Tad variables are not in lowram"

; The `TadSfxQueue` is handled via `optimize dp` and `optimise address`


assert datasize(TadPrivate.bytesToTransferPerFrame) >= 2
assert datasize(TadPrivate.dataToTransfer_addr) >= 2
assert datasize(TadPrivate.dataToTransfer_size) >= 2


; =========
; CONSTANTS
; =========


; Address to store the loader (in Audio-RAM).
; Address (in Audio-RAM) to execute after loading the Loader.
; MUST match LOADER_ADDR in `audio-driver/src/common_memmap.wiz`.
!TAD_LOADER_ARAM_ADDR = $0200


; Minimum transfer size accepted by `SetTransferSize`
;
; MUST BE > 0
!TAD_MIN_TRANSFER_PER_FRAME = 32

; Maximum transfer size accepted by `SetTransferSize`
;
; The loader can transfer ~849 bytes per 60Hz frame SlowROM or FastROM
!TAD_MAX_TRANSFER_PER_FRAME = 800


; ========
; IO Ports
; ========

; APU IO communication ports
TAD_APUIO0 = $2140
TAD_APUIO1 = $2141
TAD_APUIO2 = $2142
TAD_APUIO3 = $2143


; IO communication protocol version.
;
; Used by `tad-compiler ca65-export` to verify the IO protocol in `tad-audio.s` matches the audio-driver.
;
; This constant MUST be increased if `LOADER_ADDR` or the IO Communication protocol changes.
!TAD_IO_VERSION = 20



; MUST match `audio-driver/src/io-commands.wiz`
; enum TadIO_ToDriver
    ; The command to execute.
    ;
    ;      iiicccci
    ;         cccc = command
    ;            i = command id, MUST be different on every command.
    ;                Used to detect when a new command has been sent to the driver.
    ;
    ; NOTES:
    ;  * The command will only be execute if the `command` byte has changed.
    ;  * This value MUST be written last.
    ;  * The command and parameter bytes MUST NOT change unless the previous command
    ;    has been acknowledged.
    !TadIO_ToDriver_COMMAND_PORT = $2140 ; APUIO0

    !TadIO_ToDriver_COMMAND_MASK   = %00011110
    !TadIO_ToDriver_COMMAND_I_MASK = %11100001

    ; The first command parameter port
    !TadIO_ToDriver_PARAMETER0_PORT = $2141 ; APUIO1

    ; The second command parameter port
    !TadIO_ToDriver_PARAMETER1_PORT = $2142 ; APUIO2


    ; Writing `SWITCH_TO_LOADER` to this port should stop execution and start the loader.
    ;
    ; If the audio-driver is running; if the `SWITCH_TO_LOADER_BIT` is set,
    ; the audio driver will stop and execute the loader.
    ;
    ; If the loader is in the middle of a transfer and both the `SWITCH_TO_LOADER_BIT`
    ; and MSB (bit 7) bits are set, the loader will restart.
    !TadIO_ToDriver_SWITCH_TO_LOADER_PORT = $2143 ; APUIO3

    !TadIO_ToDriver_SWITCH_TO_LOADER_BIT = 5
    !TadIO_ToDriver_SWITCH_TO_LOADER = $a0


; MUST match `audio-driver/src/io-commands.wiz`
; enum TadIO_ToScpu
    ; Audio driver command acknowledgment.
    ;
    ; Acknowledgment of the `ToDriver.command` byte.  Not used in the loader.
    ;
    ; After the command has been processed, the `IO.ToDriver.command` value will be written to this port.
    !TadIO_ToScpu_COMMAND_ACK_PORT = $2140 ; APUIO0


    ; The mode the S-SMP is currently executing.
    ;
    ; Used by both the loader and the audio-driver.
    ;
    ; NOTE: The IPL sets this value after at has cleared the zero-page.
    ;       Do not read this value immediately after reset.
    ;       Make sure enough time has passed for the IPL to set IO Port 1
    ;       to $bb before reading this port.
    !TadIO_ToScpu_MODE_PORT = $2141 ; APUIO1

    ; The S-SMP is at the start of the IPL, waiting for the ready signal.
    !TadIO_ToScpu_MODE_IPL = $bb

    ; The S-SMP is running the loader.
    !TadIO_ToScpu_MODE_LOADER = $4c ; 'L', Loader.LOADER_READY_L

    ; The S-SMP is running the audio-driver.
    !TadIO_ToScpu_MODE_AUDIO_DRIVER = $61 ; 'a'



; MUST match `audio-driver/src/io-commands.wiz`
; enum TadLoaderDataType
    !TadLoaderDataType_CODE        = 0
    !TadLoaderDataType_COMMON_DATA = 1

    !TadLoaderDataType_SONG_DATA_FLAG            = $80
    !TadLoaderDataType_PLAY_SONG_FLAG            = $40
    !TadLoaderDataType_RESET_GLOBAL_VOLUMES_FLAG = $20

    !TadLoaderDataType_STEREO_FLAG               = $02
    !TadLoaderDataType_SURROUND_FLAG             = $01



; MUST match `audio-driver/src/io-commands.wiz`
; enum TadIO_Loader_Init
    !TadIO_Loader_Init_LOADER_DATA_TYPE_PORT = $2141 ; APUIO1
    !TadIO_Loader_Init_READY_PORT_L          = $2142 ; APUIO2
    !TadIO_Loader_Init_READY_PORT_H          = $2143 ; APUIO3

    !TadIO_Loader_Init_READY_PORT_HL         = $2142 ; APUIO2 & APUIO3

    !TadIO_Loader_Init_LOADER_READY_L = %01001100  ; 'L'
    !TadIO_Loader_Init_LOADER_READY_H = %01000100  ; 'D'
    !TadIO_Loader_Init_LOADER_READY_HL = $444c     ; 'LD'


; MUST match `audio-driver/src/io-commands.wiz`
; enum TadIO_Loader
    !TadIO_Loader_DATA_PORT_L   = $2141 ; APUIO1
    !TadIO_Loader_DATA_PORT_H   = $2142 ; APUIO2
    !TadIO_Loader_SPINLOCK_PORT = $2143 ; APUIO3

    ; The spinlock value when the audio driver starts playing a song
    !TadIO_Loader_SPINLOCK_INIT_VALUE = 0

    ; Only the lower 4 bits of the spinlock should be set while sending data to the loader
    !TadIO_Loader_SPINLOCK_MASK = $0f

    ; Signal to the loader that the transfer has completed.
    !TadIO_Loader_SPINLOCK_COMPLETE = $80

    ; If this value is written to the spinlock, the loader will restart;
    !TadIO_Loader_SPINLOCK_SWITCH_TO_LOADER = !TadIO_ToDriver_SWITCH_TO_LOADER


; enum TadState
    !TadState_NULL                                = $00
    ; Waiting for loader to send the ready signal before loading common-audio-data
    !TadState_WAITING_FOR_LOADER_COMMON           = $7b
    ; Waiting for loader to send the ready signal before loading song data
    !TadState_WAITING_FOR_LOADER_SONG             = $7c
    ; Loading common audio data.
    !TadState_LOADING_COMMON_AUDIO_DATA           = $7d
    ; Loading a song and the !TadLoaderDataType_PLAY_SONG_FLAG was clear.
    !TadState_LOADING_SONG_DATA_PAUSED            = $7e
    ; Loading a song and the !TadLoaderDataType_PLAY_SONG_FLAG was set.
    !TadState_LOADING_SONG_DATA_PLAY              = $7f
    ; Song is loaded into Audio-RAM and the audio driver is paused.
    ; No play-sound-effect commands will be sent when the driver is paused.
    !TadState_PAUSED                              = $80
    ; Song is loaded into Audio-RAM and the audio driver is playing sfx (song paused).
    !TadState_PLAYING_SFX                         = $81
    ; Song is loaded into Audio-RAM and the audio driver is playing the song.
    !TadState_PLAYING                             = $82

!TAD__FIRST_WAITING_STATE      = !TadState_WAITING_FOR_LOADER_COMMON
!TAD__FIRST_LOADING_STATE      = !TadState_LOADING_COMMON_AUDIO_DATA
!TAD__FIRST_LOADING_SONG_STATE = !TadState_LOADING_SONG_DATA_PAUSED


; ==================
; Loader subroutines
; ==================


; Transfer and execute Loader using the IPL
;
; REQUIRES: S-SMP reset and no data has been written to it yet
;
; This macro MUST only be called once.  There is no way to reset the S-SMP and restart the IPL.
;
; A8
; I16
; DB access registers
macro TadPrivate_Loader_TransferLoaderViaIpl()
    ; Clear start command port (just in case APUIO0 has $cc in it)
    ; SOURCE: `blarggapu.s` from lorom-template, originally written by blargg (Shay Green)
    stz.w   TAD_APUIO0

    ; Wait for ready signal
    ldy.w   #$bbaa
    -
        cpy.w   TAD_APUIO0
        bne     -

    ldx.w   #!TAD_LOADER_ARAM_ADDR
    lda.b   #$cc
    stx.w   TAD_APUIO2          ; destination ARAM address
    sta.w   TAD_APUIO1          ; non-zero = write data to address
    sta.w   TAD_APUIO0          ; New data command (non-zero and APUIO0 + more than 2, or $cc on the first transfer)

    ; Wait for a response from the IPL
    -
        cmp.w   TAD_APUIO0
        bne     -


    ; Transfer the data
    sep     #$30
; I8
    ldx.b   #0
    .IplLoop:
        ; Send the next byte to the IPL
        lda.l   Tad_Loader_Bin,x
        sta.w   TAD_APUIO1

        ; Tell the IPL the next byte is ready
        stx.w   TAD_APUIO0

        ; Wait for a response form the IPL
        -
            cpx.w   TAD_APUIO0
            bne     -

        inx
        cpx.b   #Tad_Loader_SIZE
        bcc     .IplLoop

    rep     #$10
; I16

    ; Send an execute program command to the IPL
    ldx.w   #!TAD_LOADER_ARAM_ADDR
    stx.w   TAD_APUIO2              ; A-RAM address
    stz.w   TAD_APUIO1              ; zero = execute program at A-RAM address
    lda.b   #Tad_Loader_SIZE+2
    sta.w   TAD_APUIO0              ; New data command (must be +2 the previous APUIO0 write)
endmacro



; Sends a TadLoaderDataType byte to the loader if the loader is ready
;
; Assumes loader just started OR a `SWITCH_TO_LOADER` message was sent to the audio driver/loader.
;
; IN: A = TadLoaderDataType value
; OUT: carry = loader is ready and TadLoaderDataType sent
;
; A8
; I16
; DB access registers
TadPrivate_Loader_CheckReadyAndSendLoaderDataType: {
    ; Test if the loader is ready
    ldx.w   #!TadIO_Loader_Init_LOADER_READY_HL
    cpx.w   !TadIO_Loader_Init_READY_PORT_HL
    bne     .ReturnFalse
        ; Send the ready signal and the TadLoaderDataType
        sta.w   !TadIO_Loader_Init_LOADER_DATA_TYPE_PORT

        lda.b   #!TadIO_Loader_Init_LOADER_READY_L
        sta.w   !TadIO_Loader_Init_READY_PORT_L

        lda.b   #!TadIO_Loader_Init_LOADER_READY_H
        sta.w   !TadIO_Loader_Init_READY_PORT_H

        ; The S-CPU must wait for the loader to write 0 to the spinlock before transferring data.
        stz.w   TadPrivate.dataToTransfer_prevSpinLock

        ; return true
        sec
        rts

.ReturnFalse:
    clc
    rts
}



; Set the data transfer queue
;
; IN: A:X = far address
; IN: Y = size
; A8
; I16
; DB access registers
TadPrivate_Loader_SetDataToTransfer: {
    stx.w   TadPrivate.dataToTransfer_addr
    sta.w   TadPrivate.dataToTransfer_bank
    sty.w   TadPrivate.dataToTransfer_size

    rts
}



; Transfer data to the audio loader.
;
; ASSUMES: `check_ready_and_send_loader_data_type` and `set_data_to_transfer` were previously called.
;
; NOTE: This function may read one byte past the end of the transfer queue.
;
; OUT: carry set if all data in the transfer queue was sent to Audio-RAM.
;
; A8
; I16
; DB access lowram
TadPrivate_Loader_TransferData: {
    ; Early exit if the loader is not ready
    ;
    ; This test doubles as a lock for the previous transfer.
    ;
    ; This also prevents a freeze in `process()` if the loader has crashed/glitched.
    ; (`finish_loading_data()` will freeze if the loader has crashed/glitched.
    lda.w   TadPrivate.dataToTransfer_prevSpinLock
    cmp.l   !TadIO_Loader_SPINLOCK_PORT
    bne     .ReturnFalse

    phd
    phb

    rep     #$30
; A16

    ; Calculate number of words to read
    lda.w   TadPrivate.dataToTransfer_size
    cmp.w   TadPrivate.bytesToTransferPerFrame
    bcc     +
        lda.w   TadPrivate.bytesToTransferPerFrame
    +
    inc     ; required
    lsr

    ; Prevent corrupting all of Audio-RAM if number of words == 0
    bne     +
        inc
    +
    ; Store word to read in X
    tax

    ; Reverse subtract TadPrivate.dataToTransfer_size (with clamping)
    asl                             ; convert number of words to number of bytes
    eor.w   #$ffff
    sec
    adc.w   TadPrivate.dataToTransfer_size
    bcs     +
        lda.w   #0
    +
    sta.w   TadPrivate.dataToTransfer_size


    lda.w   #$2100
    tcd
; D = $2100

    sep     #$20
; A8

    lda.w   TadPrivate.dataToTransfer_bank
    ldy.w   TadPrivate.dataToTransfer_addr

    pha
    plb
; DB = TadPrivate.dataToTransfer_bank

    .Loop:
        ; x = number of words remaining
        ; y = data address (using y to force addr,y addressing mode)

        lda.w   0,y
        sta.b   !TadIO_Loader_DATA_PORT_L&$ff

        ; The bank overflow test must be done here as `TadPrivate.dataToTransfer_addr` might point to an odd memory address.
        iny
        beq     .BankOverflow_1
    .BankOverflow_1_Resume:

        lda.w   0,y
        sta.b   !TadIO_Loader_DATA_PORT_H&$ff

        ; Increment this spinloack value
        ;
        ; The upper 4 bits of the spinlock must be clear'
        ; Cannot be 0.  Zero is used to spinlock the loader init before this loop starts
        ;               (see Loader Step 3 in `terrific-audio-driver/audio-driver/src/io-commands.wiz`)

        assert ($ffff&7)+1 < !TadIO_Loader_SPINLOCK_MASK
        tya             ; y = address of data, it should always increment by 2
        and.b   #7
        inc
        sta.b   !TadIO_Loader_SPINLOCK_PORT&$ff

        iny
        beq     .BankOverflow_2
    .BankOverflow_2_Resume:

        dex
        beq     .EndLoop

        ; Spinloop until the S-SMP has acknowledged the data
        -
            cmp.b   !TadIO_Loader_SPINLOCK_PORT&$ff
            bne     -

        bra     .Loop
.EndLoop:

    plb
    pld
; DB restored
; D = 0

    sty.w   TadPrivate.dataToTransfer_addr
    sta.w   TadPrivate.dataToTransfer_prevSpinLock


    ldy.w   TadPrivate.dataToTransfer_size
    bne     .ReturnFalse
        ; End of data transfer

        ; Wait for Loader to acknowledge the last write
        -
            cmp.l   !TadIO_Loader_SPINLOCK_PORT
            bne     -

        ; No more data to transfer
        lda.b   #!TadIO_Loader_SPINLOCK_COMPLETE
        sta.l   !TadIO_Loader_SPINLOCK_PORT

        sec
        rts

.ReturnFalse:
    clc
    rts


.BankOverflow_1:
    jsr     TadPrivate_Loader_GotoNextBank
    bra     .BankOverflow_1_Resume

.BankOverflow_2:
    ; Must save/restore A, it holds the spinlock
    pha
        jsr     TadPrivate_Loader_GotoNextBank
    pla
    bra     .BankOverflow_2_Resume
}


; Advance to the next bank
;
; MUST only be called to TadPrivate.Loader_TransferData
;
; ASSUMES: Y = 0 (Y addr overflowed to 0)
;
; IN: Y = 0
; IN: DB = TadPrivate.dataToTransfer_bank
;
; OUT: Y = new address
; OUT: DB = new bank
;
; KEEP: X
; A8
; I16
; DB = TadPrivate.dataToTransfer_bank
TadPrivate_Loader_GotoNextBank: {
    phb
    pla

    inc
    sta.l   TadPrivate.dataToTransfer_bank

    pha
    plb
; DB = new TadPrivate.dataToTransfer_bank value

    ; MUST NOT CHANGE X

    ; Y = 0
    if defined("LOROM")
        and.b   #$fe
        cmp.b   #$7e
        beq     +
            ; Bank is not Work-RAM
            ldy.w   #$8000
        +
    elseif defined("HIROM")
        and.b   #$7f
        cmp.b   #$40
        bcs     +
            ; Bank is a register bank
            ; set Y to the first ROM address
            ldy.w   #$8000
        +
    else
        error "Unknown memory map.  Missing `LOROM` or `HIROM` define."
    endif

    ; Y = 0 or $8000
    rts
}


; OUT: carry set if state is LOADING_*
; A8
macro TadPrivate_IsLoaderActive()
    assert !TadState_NULL < !TAD__FIRST_LOADING_STATE
    assert !TadState_WAITING_FOR_LOADER_COMMON < !TAD__FIRST_LOADING_STATE
    assert !TadState_WAITING_FOR_LOADER_SONG < !TAD__FIRST_LOADING_STATE
    assert (!TadState_PAUSED&$7f) < !TAD__FIRST_LOADING_STATE
    assert (!TadState_PLAYING&$7f) < !TAD__FIRST_LOADING_STATE

    lda.w   TadPrivate.state
    and.b   #$7f
    cmp.b   #!TAD__FIRST_LOADING_STATE
endmacro


; ==========
; Public API
; ==========

; -------------------------------
; TAD_PROCESS_SEGMENT subroutines
; -------------------------------


; Initialises the audio driver:
;
;  * Loads the loader into Audio-RAM
;  * Loads the audio driver into Audio-RAM
;  * Sets the song to 0 (silence)
;  * Resets variables
;  * Initialises `flags` and `audioMode`
;  * Queues a common audio data transfer
;
; This function will require multiple frames of execution time.
;
; REQUIRES: S-SMP reset
;
; TIMING:
;  * Should be called more than 40 scanlines after reset
;  * MUST be called ONCE
;     * Calling `Init` more than once will hardlock.
;  * MUST be called with INTERRUPTS DISABLED
;  * MUST be called while the S-SMP is running the IPL.
;  * MUST be called after the `LoadAudioData` callback is setup (if necessary)
;  * `Init` MUST be called before any other TAD subroutine.
;
; Called with JSL long addressing (returns with RTL).
; A8
; I16
; DB unknown
; DP = 0
Tad_Init: {
    phb

    lda.b   #$80
    pha
    plb
; DB = $80

    %TadPrivate_Loader_TransferLoaderViaIpl()


    ; Set default settings
    assert (!TAD_DEFAULT_FLAGS)&!TadFlags_RELOAD_COMMON_AUDIO_DATA == 0
    assert (!TAD_DEFAULT_FLAGS)&!TadFlags__ALL_FLAGS == (!TAD_DEFAULT_FLAGS)
    assert (!TAD_DEFAULT_AUDIO_MODE) >= 0 && (!TAD_DEFAULT_AUDIO_MODE) < !TAD_N_AUDIO_MODES

    assert Tad_flags+1 == Tad_audioMode
    ldx.w   #(!TAD_DEFAULT_FLAGS)|((!TAD_DEFAULT_AUDIO_MODE)<<8)
    stx.w   Tad_flags

    ldx.w   #!TAD_DEFAULT_TRANSFER_PER_FRAME
    stx.w   TadPrivate.bytesToTransferPerFrame


    lda.b   #Tad_AudioDriver_Bin>>16
    ldx.w   #Tad_AudioDriver_Bin&$ffff
    ldy.w   #Tad_AudioDriver_SIZE
    jsr     TadPrivate_Loader_SetDataToTransfer

    lda.b   #$ff
    sta.w   TadPrivate.nextCommand_id
    sta     TadSfxQueue.sfx

    stz.w   TadPrivate.nextSong

    .DataTypeLoop:
        lda.b   #!TadLoaderDataType_CODE
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     .DataTypeLoop

    .TransferLoop:
        jsr     TadPrivate_Loader_TransferData
        bcc     .TransferLoop

    lda.b   #!TadState_WAITING_FOR_LOADER_COMMON
    sta.w   TadPrivate.state

    plb
; DB restored
    rtl
}


; Sends a command to the audio driver.
;
; REQUIRES: state == PAUSED or state == PLAYING.
; REQUIRES: The previous command has been processed by the audio-driver.
; REQUIRES: `TadPrivate.nextCommand_id` is not a play-sound-effect command.
; REQUIRES: `TadPrivate.nextCommand_id` is a valid comma.
;
; IN: Y = TadPrivate.nextCommand_id
; A8
; I8
; DB access lowram
macro TadPrivate_Process_SendCommand()
    lda.w   TadPrivate.nextCommand_parameter0
    sta.l   !TadIO_ToDriver_PARAMETER0_PORT

    lda.w   TadPrivate.nextCommand_parameter1
    sta.l   !TadIO_ToDriver_PARAMETER1_PORT

    lda.w   TadPrivate.previousCommand
    and.b   #!TadIO_ToDriver_COMMAND_I_MASK    ; Clear the non i bits of the command
    eor.b   #!TadIO_ToDriver_COMMAND_I_MASK    ; Flip the i bits
    ora.w   TadPrivate.nextCommand_id          ; Set the c bits
    sta.l   !TadIO_ToDriver_COMMAND_PORT
    sta.w   TadPrivate.previousCommand

    cpy.b   #!TadCommand_UNPAUSE+1
    bcs     .NotPauseOrPlay
        ; Change state if the command is a pause or play command
        assert !TadCommand_PAUSE == 0
        assert !TadCommand_PAUSE_MUSIC_PLAY_SFX == 2
        assert !TadCommand_UNPAUSE == 4
        assert (!TadCommand_PAUSE>>1)&3|$80 == !TadState_PAUSED
        assert (!TadCommand_PAUSE_MUSIC_PLAY_SFX>>1)&3|$80 == !TadState_PLAYING_SFX
        assert (!TadCommand_UNPAUSE>>1)&3|$80 == !TadState_PLAYING
        lsr
        and.b   #3
        ora.b   #$80
        sta.w   TadPrivate.state
.NotPauseOrPlay:

    ; Reset command queue
    lda.b   #$ff
    sta.w   TadPrivate.nextCommand_id
endmacro



; Send a play-sound-effect command to the audio driver.
;
; REQUIRES: state == PLAYING
; REQUIRES: The previous command has been processed by the audio-driver.
;
; IN: A = TadSfxQueue.sfx
;
; A8
; I8
; DB access lowram
; DP = 0
macro TadPrivate_Process_SendSfxCommand()
    ; parameter 0 = sfx_id
    sta.l   !TadIO_ToDriver_PARAMETER0_PORT

    ; parameter 1 = pan
    lda     TadSfxQueue.pan
    cmp.b   #!TAD_MAX_PAN+1
    bcc     +
        lda.b   #!TAD_CENTER_PAN
    +
    sta.l   !TadIO_ToDriver_PARAMETER1_PORT

    ; Send play-sound-effect command
    lda.w   TadPrivate.previousCommand
    and.b   #!TadIO_ToDriver_COMMAND_I_MASK            ; Clear the non i bits of the command
    eor.b   #!TadIO_ToDriver_COMMAND_I_MASK            ; Flip the i bits
    ora.b   #!TadCommand_PLAY_SOUND_EFFECT             ; Set the c bits

    sta.l   !TadIO_ToDriver_COMMAND_PORT
    sta.w   TadPrivate.previousCommand

    ; Reset the SFX queue
    ldy.b   #$ff
    sty     TadSfxQueue.sfx
    sty     TadSfxQueue.pan
endmacro



; Processes the next queue.
;
; This function will do one of the following, depending on the state:
;  * Transfer data to the Audio-RAM
;  * Wait for the loader and call the `LoadAudioData` callback when the loader is ready to receive new data
;  * Send a command to the audio driver
;  * Send a play-sound effect command to the audio driver
;
; NOTE: The command and sound-effect queues will be reset after a new song is loaded into Audio-RAM.
;
; TIMING:
;  * MUST be called after `Init`.
;  * Should be called once per frame.
;  * MUST NOT be called in an interrupt ISR.
;
; Called with JSL long addressing (returns with RTL).
; A8
; I16
; DB access lowram
; DP = 0
Tad_Process: {
    assert !TadState_PAUSED == $80
    assert !TadState_PLAYING > $80
    lda.w   TadPrivate.state
    bpl     .NotLoaded
        ; Playing or paused state
        sep     #$10
    ; I8
        tax

        lda.w   TadPrivate.previousCommand
        cmp.l   !TadIO_ToScpu_COMMAND_ACK_PORT
        bne     .Return_I8
            ; Previous command has been processed

            ; Check command queue
            ldy.w   TadPrivate.nextCommand_id
            bpl     .SendCommand

            ; X = TadPrivate.state
            assert !TadState_PAUSED < $81
            assert !TadState_PLAYING >= $81
            assert !TadState_PLAYING_SFX >= $81
            dex
            bpl     .Return_I8
                ; Playing state
                lda     TadSfxQueue.sfx
                cmp.b   #$ff
                beq     .Return_I8
                    %TadPrivate_Process_SendSfxCommand()

        .Return_I8:
            rep     #$10
        ; I16
            rtl

        ; A8
        ; I8
        .SendCommand:
            %TadPrivate_Process_SendCommand()
            rep     #$10
        ; I16
            rtl

    .NotLoaded:
        ; Song is not loaded into Audio-RAM

        ; Test if state is WAITING_FOR_LOADER_* or LOADING_*
        assert !TAD__FIRST_LOADING_STATE > !TAD__FIRST_WAITING_STATE
        assert !TAD__FIRST_LOADING_STATE == !TadState_WAITING_FOR_LOADER_SONG+1
        cmp.b   #!TAD__FIRST_LOADING_STATE
        bcs     TadPrivate_Process_Loading
        cmp.b   #!TAD__FIRST_WAITING_STATE
        bcs     TadPrivate_Process_WaitingForLoader

    ; TadState is null
    rtl
}



; Process the WAITING_FOR_LOADER_* states
;
; return using RTL
; A8
; I16
; DB access lowram
TadPrivate_Process_WaitingForLoader: { ; RTL
    phb

    ; Setting DB to access registers as it:
    ;  * Simplifies `TadPrivate.Loader_CheckReadyAndSendLoaderDataType`
    ;  * Ensures `LoadAudioData` is called with a fixed data bank
    ;    (NOTE: `LoadAudioData` is tagged `DB access registers`)
    lda.b   #$80
    pha
    plb
; DB = $80

    lda.w   TadPrivate.state
    cmp.b   #!TadState_WAITING_FOR_LOADER_COMMON
    bne     .SongData
        ; Common audio data
        lda.b   #!TadLoaderDataType_COMMON_DATA
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     .Return

        lda.b   #!TadState_LOADING_COMMON_AUDIO_DATA
        pha

        lda.b   #0
        bra     .LoadData

    .SongData:
        ; Songs

        ; flags MUST NOT have the stereo/surround loader flag set
        assert !TadFlags__ALL_FLAGS&!TadLoaderDataType_STEREO_FLAG == 0
        assert !TadFlags__ALL_FLAGS&!TadLoaderDataType_SURROUND_FLAG == 0

        ; SONG_DATA_FLAG must always be sent and it also masks the RELOAD_COMMON_AUDIO_DATA flag in TadLoaderDataType
        assert !TadFlags_RELOAD_COMMON_AUDIO_DATA == !TadLoaderDataType_SONG_DATA_FLAG

        assert !TadFlags_PLAY_SONG_IMMEDIATELY == !TadLoaderDataType_PLAY_SONG_FLAG
        assert !TadFlags_RESET_GLOBAL_VOLUMES_ON_SONG_START == !TadLoaderDataType_RESET_GLOBAL_VOLUMES_FLAG

        ; Clear unused TAD flags
        lda.b   #$ff^!TadFlags__ALL_FLAGS
        trb.w   Tad_flags

        ; Convert `audioMode` to TadLoaderDataType and combine with TadFlags
        assert ((0+1)&3) == !TadLoaderDataType_SURROUND_FLAG
        assert ((1+1)&3) == !TadLoaderDataType_STEREO_FLAG
        assert ((2+1)&3) == !TadLoaderDataType_STEREO_FLAG|!TadLoaderDataType_SURROUND_FLAG
        lda.w   Tad_audioMode
        inc
        and.b   #3

        ora.w   Tad_flags
        ora.b   #!TadLoaderDataType_SONG_DATA_FLAG
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     .Return

        ; Determine next state
        assert !TadFlags_PLAY_SONG_IMMEDIATELY == $40
        assert !TadState_LOADING_SONG_DATA_PAUSED+1 == !TadState_LOADING_SONG_DATA_PLAY
        lda.w   Tad_flags
        asl
        asl
        lda.b   #0
        ; carry = PLAY_SONG_IMMEDIATELY flag
        adc.b   #!TadState_LOADING_SONG_DATA_PAUSED
        pha

        ; Load next song
        lda.w   TadPrivate.nextSong
        beq     .UseBlankSong

.LoadData:
    jsl     LoadAudioData
    bcs     +
        ; LoadAudioData returned false
    .UseBlankSong:
        ; The blank song is a single zero byte.
        ; ::HACK use the 3rd byte of `ldy #1` (which is `0x00`) for the blank song data::
        ldy.w   #1
    ._AfterBlankSongData
        lda.b   #(._AfterBlankSongData-1)>>16
        ldx.w   #(._AfterBlankSongData-1)
    +

    ; STACK holds next state
    ; A:X = data address
    ; Y = data size
    jsr     TadPrivate_Loader_SetDataToTransfer

    ; Must set state AFTER the `LoadAudioData` call.
    ; `LoadAudioData` might call `FinishLoadingData`.
    pla
    sta.w   TadPrivate.state

.Return:
    plb
; DB restored
    rtl
}



; Process the LOADING_* states
;
; return using RTL
; A8
; I16
; DB access lowram
; DP = 0
TadPrivate_Process_Loading: { ; RTL
    jsr     TadPrivate_Loader_TransferData
    bcc     .Return
        ; Data loaded successfully
        lda.w   TadPrivate.state
        cmp.b   #!TadState_LOADING_COMMON_AUDIO_DATA
        bne     .Song
            ; Common audio data was just transferred
            ; Loader is still active
            lda.b   #!TadState_WAITING_FOR_LOADER_SONG
            bra     .EndIf

        .Song:
            ; song data was loaded into Audio-RAM
            ; Loader has finished, audio driver is now active

            stz.w   TadPrivate.previousCommand

            ; Reset command and SFX queues
            lda.b   #$ff
            sta.w   TadPrivate.nextCommand_id
            sta     TadSfxQueue.sfx
            sta     TadSfxQueue.pan

            ; Use `TadPrivate.state` to determine if the song is playing or paused.
            ; Cannot use `flags` as it may have changed after the `TadLoaderDataType` was sent to
            ; the loader (while the song was loaded).
            assert ((!TadState_LOADING_SONG_DATA_PAUSED&1)<<1)|$80 == !TadState_PAUSED
            assert ((!TadState_LOADING_SONG_DATA_PLAY&1)<<1)|$80 == !TadState_PLAYING
            lda.w   TadPrivate.state
            and.b   #1
            asl
            ora.b   #$80

        ; A = new state
    .EndIf:
        sta.w   TadPrivate.state

.Return:
    rtl
}



; Finish loading the data into audio-RAM.
;
; `Tad_FinishLoadingData` will not transfer data if the state is `WAITING_FOR_LOADER`.
; It will only transfer data if the loader is in the middle of transferring data
; (when `Tad_IsLoaderActive` returns true).
;
; This function can be safely called by `LoadAudioData`.
;
; This function may require multiple frames of execution time.
;
; Called with JSL long addressing (returns with RTL).
; A8
; I16
; DB access lowram
Tad_FinishLoadingData: {
    .Loop:
        %TadPrivate_IsLoaderActive()
        bcc     .EndLoop
            jsl     TadPrivate_Process_Loading
        bra     .Loop
    .EndLoop:

    rtl
}
