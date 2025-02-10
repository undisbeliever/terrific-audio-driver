; TAD 64tass API init, process and loader subroutines
;
; MUST NOT CALL TadPrivate_* functions outside of the TAD API.

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


.cpu "65816"
.autsiz

TAD_DB_LOWRAM      = TadPrivate_state >> 16
TAD_DB_REGISTERS   = 0


; ===========
; Binary Data
; ===========
;
; These 3 files MUST be embedded (using `.incbin`) into the ROM if the developer uses a custom
; `LoadAudioData` callback.
;

.cerror Tad_Loader_SIZE < 64 || Tad_Loader_SIZE > 128, "Invalid Tad_Loader_Bin size"
.cerror (Tad_Loader_Bin >> 16) != ((Tad_Loader_Bin + Tad_Loader_SIZE) >> 16), "Tad_Loader_Bin does not fit inside a single bank"

.cerror Tad_AudioDriver_SIZE < $600 || Tad_AudioDriver_SIZE > $d00, "Invalid Tad_AudioDriver_Bin size"
; `Tad_AudioDriver_Bin` can cross bank boundaries



; =========
; CONSTANTS
; =========

.weak
    TAD_MEMORY_MAP = ""
.endweak

.cerror TAD_MEMORY_MAP != "LOROM" && TAD_MEMORY_MAP != "HIROM", "Unknown memory map, set TAD_MEMORY_MAP to the string LOROM or HIROM"


; Address to store the loader (in Audio-RAM).
; Address (in Audio-RAM) to execute after loading the Loader.
; MUST match LOADER_ADDR in `audio-driver/src/common_memmap.wiz`.
TAD_LOADER_ARAM_ADDR = $0200


; ========
; IO Ports
; ========

; IO communication protocol version.
;
; Used by `tad-compiler ca65-export` to verify the IO protocol in `tad-audio.s` matches the audio-driver.
;
; This constant MUST be increased if `LOADER_ADDR` or the IO Communication protocol changes.
TAD_IO_VERSION = 20


; MUST match `audio-driver/src/io-commands.wiz`
TadIO_ToDriver .block
    ; The command to execute.
    ;
    ;      iii0ccci
    ;          ccc = command
    ;            0 = reserved for future expansion
    ;            i = command id, MUST be different on every command.
    ;                Used to detect when a new command has been sent to the driver.
    ;
    ; NOTES:
    ;  * The command will only be execute if the `command` byte has changed.
    ;  * This value MUST be written last.
    ;  * The command and parameter bytes MUST NOT change unless the previous command
    ;    has been acknowledged.
    COMMAND_PORT = $2140 ; APUIO0

    COMMAND_MASK   = %00011110
    COMMAND_I_MASK = %11100001

    ; The first command parameter port
    PARAMETER0_PORT = $2141 ; APUIO1

    ; The second command parameter port
    PARAMETER1_PORT = $2142 ; APUIO2


    ; Writing `SWITCH_TO_LOADER` to this port should stop execution and start the loader.
    ;
    ; If the audio-driver is running; if the `SWITCH_TO_LOADER_BIT` is set,
    ; the audio driver will stop and execute the loader.
    ;
    ; If the loader is in the middle of a transfer and both the `SWITCH_TO_LOADER_BIT`
    ; and MSB (bit 7) bits are set, the loader will restart.
    SWITCH_TO_LOADER_PORT = $2143 ; APUIO3

    SWITCH_TO_LOADER_BIT = 5
    SWITCH_TO_LOADER = $80 | (1 << SWITCH_TO_LOADER_BIT)
.endblock


; MUST match `audio-driver/src/io-commands.wiz`
TadIO_ToScpu .block
    ; Audio driver command acknowledgment.
    ;
    ; Acknowledgment of the `ToDriver.command` byte.  Not used in the loader.
    ;
    ; After the command has been processed, the `IO.ToDriver.command` value will be written to this port.
    COMMAND_ACK_PORT = $2140 ; APUIO0


    ; The mode the S-SMP is currently executing.
    ;
    ; Used by both the loader and the audio-driver.
    ;
    ; NOTE: The IPL sets this value after at has cleared the zero-page.
    ;       Do not read this value immediately after reset.
    ;       Make sure enough time has passed for the IPL to set IO Port 1
    ;       to $bb before reading this port.
    MODE_PORT = $2141 ; APUIO1

    ; The S-SMP is at the start of the IPL, waiting for the ready signal.
    MODE_IPL = $bb

    ; The S-SMP is running the loader.
    MODE_LOADER = $4c ; 'L', Loader.LOADER_READY_L

    ; The S-SMP is running the audio-driver.
    MODE_AUDIO_DRIVER = $61 ; 'a'
.endblock


; MUST match `audio-driver/src/io-commands.wiz`
TadLoaderDataType .block
    CODE        = 0
    COMMON_DATA = 1

    SONG_DATA_FLAG            = 1 << 7
    PLAY_SONG_FLAG            = 1 << 6
    RESET_GLOBAL_VOLUMES_FLAG = 1 << 5

    STEREO_FLAG               = 1 << 1
    SURROUND_FLAG             = 1 << 0
.endblock


; MUST match `audio-driver/src/io-commands.wiz`
TadIO_Loader_Init .block
    LOADER_DATA_TYPE_PORT = $2141 ; APUIO1
    READY_PORT_L          = $2142 ; APUIO2
    READY_PORT_H          = $2143 ; APUIO3

    READY_PORT_HL         = $2142 ; APUIO2 & APUIO3

    LOADER_READY_L = %01001100  ; 'L'
    LOADER_READY_H = %01000100  ; 'D'
    LOADER_READY_HL = LOADER_READY_L | (LOADER_READY_H << 8)
.endblock


; MUST match `audio-driver/src/io-commands.wiz`
TadIO_Loader .block
    DATA_PORT_L   = $2141 ; APUIO1
    DATA_PORT_H   = $2142 ; APUIO2
    SPINLOCK_PORT = $2143 ; APUIO3

    ; The spinlock value when the audio driver starts playing a song
    SPINLOCK_INIT_VALUE = 0

    ; Only the lower 4 bits of the spinlock should be set while sending data to the loader
    SPINLOCK_MASK = $0f

    ; Signal to the loader that the transfer has completed.
    SPINLOCK_COMPLETE = $80

    ; If this value is written to the spinlock, the loader will restart;
    SPINLOCK_SWITCH_TO_LOADER = TadIO_ToDriver.SWITCH_TO_LOADER
.endblock



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
TadPrivate_Loader_TransferLoaderViaIpl .macro

APUIO0 = $2140
APUIO1 = $2141
APUIO2 = $2142
APUIO3 = $2143

    ; Clear start command port (just in case APUIO0 has $cc in it)
    ; SOURCE: `blarggapu.s` from lorom-template, originally written by blargg (Shay Green)
    stz     APUIO0,b

    ; Wait for ready signal
    ldy     #$bbaa
    -
        cpy     APUIO0
        bne     -

    ldx     #TAD_LOADER_ARAM_ADDR
    lda     #$cc
    stx     APUIO2,b            ; destination ARAM address
    sta     APUIO1,b            ; non-zero = write data to address
    sta     APUIO0,b            ; New data command (non-zero and APUIO0 + more than 2, or $cc on the first transfer)

    ; Wait for a response from the IPL
    -
        cmp     APUIO0,b
        bne     -


    ; Transfer the data
    .cerror !(Tad_Loader_SIZE < $ff), "Cannot fit Tad_Loader_SIZE in an 8 bit index"

    sep     #$30
.xs
    ldx     #0
    _IplLoop:
        ; Send the next byte to the IPL
    .databank ?
        lda     Tad_Loader_Bin,x
    .databank TAD_DB_REGISTERS
        sta     APUIO1,b

        ; Tell the IPL the next byte is ready
        stx     APUIO0,b

        ; Wait for a response form the IPL
        -
            cpx     APUIO0,b
            bne     -

        inx
        cpx     #Tad_Loader_SIZE
        bcc     _IplLoop

    rep     #$10
.xl

    ; Send an execute program command to the IPL
    ldx     #TAD_LOADER_ARAM_ADDR
    stx     APUIO2,b                ; A-RAM address
    stz     APUIO1,b                ; zero = execute program at A-RAM address
    lda     #Tad_Loader_SIZE + 2
    sta     APUIO0,b                ; New data command (must be +2 the previous APUIO0 write)
.endmacro



; Sends a TadLoaderDataType byte to the loader if the loader is ready
;
; Assumes loader just started OR a `SWITCH_TO_LOADER` message was sent to the audio driver/loader.
;
; IN: A = TadLoaderDataType value
; OUT: carry = loader is ready and TadLoaderDataType sent
.as
.xl
.databank TAD_DB_REGISTERS
TadPrivate_Loader_CheckReadyAndSendLoaderDataType .proc
    ; Test if the loader is ready
    ldx     #TadIO_Loader_Init.LOADER_READY_HL
    cpx     TadIO_Loader_Init.READY_PORT_HL
    bne     _ReturnFalse
        ; Send the ready signal and the TadLoaderDataType
        sta     TadIO_Loader_Init.LOADER_DATA_TYPE_PORT

        lda     #TadIO_Loader_Init.LOADER_READY_L
        sta     TadIO_Loader_Init.READY_PORT_L

        lda     #TadIO_Loader_Init.LOADER_READY_H
        sta     TadIO_Loader_Init.READY_PORT_H

        ; The S-CPU must wait for the loader to write 0 to the spinlock before transferring data.
    .databank TAD_DB_LOWRAM
        stz     TadPrivate_dataToTransfer_prevSpinLock

        ; return true
        sec
        rts

_ReturnFalse:
    clc
    rts
.endproc



; IN: A:X = far address
; IN: Y = size
.as
.xl
.databank TAD_DB_REGISTERS
TadPrivate_Loader_SetDataToTransfer .proc
    .databank TAD_DB_LOWRAM
    stx     TadPrivate_dataToTransfer_addr
    sta     TadPrivate_dataToTransfer_bank
    sty     TadPrivate_dataToTransfer_size

    rts
.endproc



; Transfer data to the audio loader.
;
; ASSUMES: `check_ready_and_send_loader_data_type` and `set_data_to_transfer` were previously called.
;
; NOTE: This function may read one byte past the end of the transfer queue.
;
; OUT: carry set if all data in the transfer queue was sent to Audio-RAM.
;
.as
.xl
.databank TAD_DB_LOWRAM
TadPrivate_Loader_TransferData .proc
    ; Early exit if the loader is not ready
    ;
    ; This test doubles as a lock for the previous transfer.
    ;
    ; This also prevents a freeze in `process()` if the loader has crashed/glitched.
    ; (`finish_loading_data()` will freeze if the loader has crashed/glitched.
    lda     TadPrivate_dataToTransfer_prevSpinLock
    cmp     TadIO_Loader.SPINLOCK_PORT
    bne     _ReturnFalse

    phd
    phb

    rep     #$30
.al

    ; Calculate number of words to read
    lda     TadPrivate_dataToTransfer_size
    cmp     TadPrivate_bytesToTransferPerFrame
    bcc     +
        lda     TadPrivate_bytesToTransferPerFrame
    +
    inc     a       ; required
    lsr     a

    ; Prevent corrupting all of Audio-RAM if number of words == 0
    bne     +
        inc     a
    +
    ; Store word to read in X
    tax

    ; Reverse subtract TadPrivate_dataToTransfer_size (with clamping)
    asl     a                       ; convert number of words to number of bytes
    eor     #$ffff
    sec
    adc     TadPrivate_dataToTransfer_size
    bcs     +
        lda     #0
    +
    sta     TadPrivate_dataToTransfer_size


    lda     #$2100
    tcd
; D = $2100

    sep     #$20
.as

    lda     TadPrivate_dataToTransfer_bank
    ldy     TadPrivate_dataToTransfer_addr

    pha
    plb
; DB = TadPrivate_dataToTransfer_bank
.databank ?
; NOT USING `.dpage`
; I do not see a way to restore `.dpage` after `PLD`

    _Loop:
        ; x = number of words remaining
        ; y = data address (using y to force addr,y addressing mode)

        ; DB = TadPrivate_dataToTransfer_bank (unknown)
        ; DP = $2100

        lda     0,b,y
        sta     #(TadIO_Loader.DATA_PORT_L & $ff),d

        ; The bank overflow test must be done here as `TadPrivate_dataToTransfer_addr` might point to an odd memory address.
        iny
        beq     _BankOverflow_1
    _BankOverflow_1_Resume:

        lda     0,b,y
        sta     #(TadIO_Loader.DATA_PORT_H & $ff),d

        ; Increment this spinloack value
        ;
        ; The upper 4 bits of the spinlock must be clear'
        ; Cannot be 0.  Zero is used to spinlock the loader init before this loop starts
        ;               (see Loader Step 3 in `terrific-audio-driver/audio-driver/src/io-commands.wiz`)

        .cerror !(($ffff & 7) + 1 < TadIO_Loader.SPINLOCK_MASK)
        tya             ; y = address of data, it should always increment by 2
        and     #7
        inc     a
        sta     #(TadIO_Loader.SPINLOCK_PORT & $ff),d

        iny
        beq     _BankOverflow_2
    _BankOverflow_2_Resume:

        dex
        beq     _EndLoop

        ; Spinloop until the S-SMP has acknowledged the data
        -
            cmp     #(TadIO_Loader.SPINLOCK_PORT & $ff),d
            bne     -

        bra     _Loop
_EndLoop:

    plb
    pld
.databank TAD_DB_LOWRAM
; D restored

    sty     TadPrivate_dataToTransfer_addr
    sta     TadPrivate_dataToTransfer_prevSpinLock


    ldy     TadPrivate_dataToTransfer_size
    bne     _ReturnFalse
        ; End of data transfer

        ; Wait for Loader to acknowledge the last write
        -
            cmp     TadIO_Loader.SPINLOCK_PORT
            bne     -

        ; No more data to transfer
        lda     #TadIO_Loader.SPINLOCK_COMPLETE
        sta     TadIO_Loader.SPINLOCK_PORT

        sec
        rts

_ReturnFalse:
    clc
    rts


.databank ?
_BankOverflow_1:
    jsr     TadPrivate_Loader_GotoNextBank__
    bra     _BankOverflow_1_Resume

_BankOverflow_2:
    ; Must save/restore A, it holds the spinlock
    pha
        jsr     TadPrivate_Loader_GotoNextBank__
    pla
    bra     _BankOverflow_2_Resume
.endproc


; Advance to the next bank
;
; MUST only be called to TadPrivate_Loader_TransferData
;
; ASSUMES: Y = 0 (Y addr overflowed to 0)
;
; IN: Y = 0
; IN: DB = TadPrivate_dataToTransfer_bank
;
; OUT: Y = new address
; OUT: DB = new bank
;
; KEEP: X
; DB = TadPrivate_dataToTransfer_bank
.as
.xl
.databank ?
TadPrivate_Loader_GotoNextBank__ .proc
    phb
    pla

    inc     a
    sta     TadPrivate_dataToTransfer_bank

    pha
    plb
; DB = new TadPrivate_dataToTransfer_bank value

    ; MUST NOT CHANGE X

    ; Y = 0
    .if TAD_MEMORY_MAP == "LOROM"
        and     #$fe
        cmp     #$7e
        beq     +
            ; Bank is not Work-RAM
            ldy     #$8000
        +
    .elsif TAD_MEMORY_MAP == "HIROM"
        and     #$7f
        cmp     #$40
        bcs     +
            ; Bank is a register bank
            ; set Y to the first ROM address
            ldy     #$8000
        +
    .else
        .error "Unknown memory map."
    .endif

    ; Y = 0 or $8000
    rts
.endproc


; OUT: carry set if state is LOADING_*
TadPrivate_IsLoaderActive .macro
    .xs

    .cerror !(TadState.NULL < TAD__FIRST_LOADING_STATE)
    .cerror !(TadState.WAITING_FOR_LOADER_COMMON < TAD__FIRST_LOADING_STATE)
    .cerror !(TadState.WAITING_FOR_LOADER_SONG < TAD__FIRST_LOADING_STATE)
    .cerror !((TadState.PAUSED & $7f) < TAD__FIRST_LOADING_STATE)
    .cerror !((TadState.PLAYING & $7f) < TAD__FIRST_LOADING_STATE)

    lda     TadPrivate_state
    and     #$7f
    cmp     #TAD__FIRST_LOADING_STATE
.endmacro


; ==========
; Public API
; ==========


; JSL/RTL subroutine
.as
.xl
.databank ?
; Called with JSL (far addressing)
Tad_Init .proc
    phb

    lda     #$80
    pha
    plb
.databank TAD_DB_REGISTERS

    #TadPrivate_Loader_TransferLoaderViaIpl

.databank TAD_DB_LOWRAM

    stz     Tad_audioMode

    lda     #TadFlags.PLAY_SONG_IMMEDIATELY
    sta     Tad_flags

    ldx     #TAD_DEFAULT_TRANSFER_PER_FRAME
    stx     TadPrivate_bytesToTransferPerFrame

    lda     #`Tad_AudioDriver_Bin
    ldx     #<>Tad_AudioDriver_Bin
    ldy     #Tad_AudioDriver_SIZE
    jsr     TadPrivate_Loader_SetDataToTransfer

    lda     #$ff
    sta     TadPrivate_nextCommand_id
    sta     Tad_sfxQueue_sfx

    stz     TadPrivate_nextSong

    _DataTypeLoop:
        lda     #TadLoaderDataType.CODE
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     _DataTypeLoop

    _TransferLoop:
        jsr     TadPrivate_Loader_TransferData
        bcc     _TransferLoop

    lda     #TadState.WAITING_FOR_LOADER_COMMON
    sta     TadPrivate_state

    plb
.databank ?
    rtl
.endproc


; Sends a command to the audio driver.
;
; REQUIRES: state == PAUSED or state == PLAYING.
; REQUIRES: The previous command has been processed by the audio-driver.
; REQUIRES: `TadPrivate_nextCommand_id` is not a play-sound-effect command.
; REQUIRES: `TadPrivate_nextCommand_id` is a valid comma.
;
; IN: Y = TadPrivate_nextCommand_id
.as
.xs
.databank TAD_DB_LOWRAM
TadPrivate_Process_SendCommand .macro
    lda     TadPrivate_nextCommand_parameter0
    sta     TadIO_ToDriver.PARAMETER0_PORT

    lda     TadPrivate_nextCommand_parameter1
    sta     TadIO_ToDriver.PARAMETER1_PORT

    lda     TadPrivate_previousCommand
    and     #TadIO_ToDriver.COMMAND_I_MASK    ; Clear the non i bits of the command
    eor     #TadIO_ToDriver.COMMAND_I_MASK    ; Flip the i bits
    ora     TadPrivate_nextCommand_id         ; Set the c bits
    sta     TadIO_ToDriver.COMMAND_PORT
    sta     TadPrivate_previousCommand

    cpy     #TadCommand.UNPAUSE + 1
    bcs     _NotPauseOrPlay
        ; Change state if the command is a pause or play command
        .cerror !(TadCommand.PAUSE == 0)
        .cerror !(TadCommand.PAUSE_MUSIC_PLAY_SFX == 2)
        .cerror !(TadCommand.UNPAUSE == 4)
        .cerror !((TadCommand.PAUSE >> 1) & 3 | $80 == TadState.PAUSED)
        .cerror !((TadCommand.PAUSE_MUSIC_PLAY_SFX >> 1) & 3 | $80 == TadState.PLAYING_SFX)
        .cerror !((TadCommand.UNPAUSE >> 1) & 3 | $80 == TadState.PLAYING)
        lsr     a
        and     #3
        ora     #$80
        sta     TadPrivate_state
_NotPauseOrPlay:

    ; Reset command queue
    lda     #$ff
    sta     TadPrivate_nextCommand_id
.endmacro



; Send a play-sound-effect command to the audio driver.
;
; REQUIRES: state == PLAYING
; REQUIRES: The previous command has been processed by the audio-driver.
;
; IN: A = Tad_sfxQueue_sfx
;
; A8
; I8
TadPrivate_Process_SendSfxCommand .macro
    ; parameter 0 = sfx_id
    sta     TadIO_ToDriver.PARAMETER0_PORT

    ; parameter 1 = pan
    lda     Tad_sfxQueue_pan
    cmp     #TAD_MAX_PAN + 1
    bcc     +
        lda     #TAD_CENTER_PAN
    +
    sta     TadIO_ToDriver.PARAMETER1_PORT

    ; Send play-sound-effect command
    lda     TadPrivate_previousCommand
    and     #TadIO_ToDriver.COMMAND_I_MASK            ; Clear the non i bits of the command
    eor     #TadIO_ToDriver.COMMAND_I_MASK            ; Flip the i bits
    ora     #TadCommand.PLAY_SOUND_EFFECT             ; Set the c bits

    sta     TadIO_ToDriver.COMMAND_PORT
    sta     TadPrivate_previousCommand

    ; Reset the SFX queue
    ldy     #$ff
    sty     Tad_sfxQueue_sfx
    sty     Tad_sfxQueue_pan
.endmacro



; JSL/RTL subroutine
.as
.xl
.databank TAD_DB_LOWRAM
; Called with JSL (far addressing)
Tad_Process .proc
    .cerror !(TadState.PAUSED == $80)
    .cerror !(TadState.PLAYING > $80)

    lda     TadPrivate_state
    bpl     _NotLoaded
        ; Playing or paused state
        sep     #$10
    .xs
        tax

        lda     TadPrivate_previousCommand
        cmp     TadIO_ToScpu.COMMAND_ACK_PORT
        bne     _Return_I8
            ; Previous command has been processed

            ; Check command queue
            ldy     TadPrivate_nextCommand_id
            bpl     _SendCommand

            ; X = TadPrivate_state
            .cerror !(TadState.PAUSED < $81)
            .cerror !(TadState.PLAYING >= $81)
            .cerror !(TadState.PLAYING_SFX >= $81)
            dex
            bpl     _Return_I8
                ; Playing state
                lda     Tad_sfxQueue_sfx
                cmp     #$ff
                beq     _Return_I8
                    #TadPrivate_Process_SendSfxCommand

        _Return_I8:
            rep     #$10
        .xl
            rtl

        .as
        .xs
        _SendCommand:
            #TadPrivate_Process_SendCommand
            rep     #$10
        .xl
            rtl

    _NotLoaded:
        ; Song is not loaded into Audio-RAM

        ; Test if state is WAITING_FOR_LOADER_* or LOADING_*
        .cerror !(TAD__FIRST_LOADING_STATE > TAD__FIRST_WAITING_STATE)
        .cerror !(TAD__FIRST_LOADING_STATE == TadState.WAITING_FOR_LOADER_SONG + 1)

        cmp     #TAD__FIRST_LOADING_STATE
        bcs     TadPrivate_Process_Loading__
        cmp     #TAD__FIRST_WAITING_STATE
        bcs     TadPrivate_Process_WaitingForLoader__

    ; TadState is null
    rtl
.endproc



; Process the WAITING_FOR_LOADER_* states
;
; return using RTL
.as
.xl
.databank TAD_DB_LOWRAM
TadPrivate_Process_WaitingForLoader__ .proc ; RTL
    phb

    ; Setting DB to access registers as it:
    ;  * Simplifies `TadPrivate_Loader_CheckReadyAndSendLoaderDataType`
    ;  * Ensures `LoadAudioData` is called with a fixed data bank
    ;    (NOTE: `LoadAudioData` is tagged `DB access registers`)
    lda     #$80
    pha
    plb
; DB = $80

    lda     TadPrivate_state
    cmp     #TadState.WAITING_FOR_LOADER_COMMON
    bne     _SongData
        ; Common audio data
        lda     #TadLoaderDataType.COMMON_DATA
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     _Return

        lda     #TadState.LOADING_COMMON_AUDIO_DATA
        pha

        lda     #0
        bra     _LoadData

    _SongData:
        ; Songs

        _ALL_FLAGS = TadFlags.RELOAD_COMMON_AUDIO_DATA | TadFlags.PLAY_SONG_IMMEDIATELY | TadFlags.RESET_GLOBAL_VOLUMES_ON_SONG_START

        ; Tad_flags MUST NOT have the stereo/surround loader flag set
        .cerror !(_ALL_FLAGS & TadLoaderDataType.STEREO_FLAG == 0)
        .cerror !(_ALL_FLAGS & TadLoaderDataType.SURROUND_FLAG == 0)

        ; SONG_DATA_FLAG must always be sent and it also masks the RELOAD_COMMON_AUDIO_DATA flag in TadLoaderDataType
        .cerror !(TadFlags.RELOAD_COMMON_AUDIO_DATA == TadLoaderDataType.SONG_DATA_FLAG)

        .cerror !(TadFlags.PLAY_SONG_IMMEDIATELY == TadLoaderDataType.PLAY_SONG_FLAG)
        .cerror !(TadFlags.RESET_GLOBAL_VOLUMES_ON_SONG_START == TadLoaderDataType.RESET_GLOBAL_VOLUMES_FLAG)

        ; Clear unused TAD flags
        lda     #$ff ^ _ALL_FLAGS
        trb     Tad_flags

        ; Convert `Tad_audioMode` to TadLoaderDataType
        .cerror !(((0 + 1) & 3) == TadLoaderDataType.SURROUND_FLAG) ; mono
        .cerror !(((1 + 1) & 3) == TadLoaderDataType.STEREO_FLAG)   ; stereo
        .cerror !(((2 + 1) & 3) == TadLoaderDataType.STEREO_FLAG | TadLoaderDataType.SURROUND_FLAG) ; surround
        lda     Tad_audioMode
        inc     a
        and     #3

        ora     Tad_flags
        ora     #TadLoaderDataType.SONG_DATA_FLAG
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     _Return

        ; Determine next state
        .cerror !(TadFlags.PLAY_SONG_IMMEDIATELY == $40)
        .cerror !(TadState.LOADING_SONG_DATA_PAUSED + 1 == TadState.LOADING_SONG_DATA_PLAY)
        lda     Tad_flags
        asl     a
        asl     a
        lda     #0
        ; carry = PLAY_SONG_IMMEDIATELY flag
        adc     #TadState.LOADING_SONG_DATA_PAUSED
        pha

        ; Load next song
        lda     TadPrivate_nextSong
        beq     _UseBlankSong

_LoadData:
    jsl     LoadAudioData
    bcs     +
        ; LoadAudioData returned false
    _UseBlankSong:
        ; The blank song is a single zero byte.
        ; ::HACK use the 3rd byte of `ldy #1` (which is `0x00`) for the blank song data::
        ldy     #1
        _BlankSongData: = * - 1
        lda     #`_BlankSongData
        ldx     #<>_BlankSongData
    +

    ; STACK holds next state
    ; A:X = data address
    ; Y = data size
    jsr     TadPrivate_Loader_SetDataToTransfer

    pla
    sta     TadPrivate_state

_Return:
    plb
; DB restored
    rtl
.endproc



; Process the LOADING_* states
;
; return using RTL
.as
.xl
.databank TAD_DB_LOWRAM
TadPrivate_Process_Loading__ .proc ; RTL
    jsr     TadPrivate_Loader_TransferData
    bcc     _Return
        ; Data loaded successfully
        lda     TadPrivate_state
        cmp     #TadState.LOADING_COMMON_AUDIO_DATA
        bne     _Song
            ; Common audio data was just transferred
            ; Loader is still active
            lda     #TadState.WAITING_FOR_LOADER_SONG
            bra     _EndIf

        _Song:
            ; song data was loaded into Audio-RAM
            ; Loader has finished, audio driver is now active

            stz     TadPrivate_previousCommand

            ; Reset command and SFX queues
            lda     #$ff
            sta     TadPrivate_nextCommand_id
            sta     Tad_sfxQueue_sfx
            sta     Tad_sfxQueue_pan

            ; Use `TadPrivate_state` to determine if the song is playing or paused.
            ; Cannot use `Tad_flags` as it may have changed after the `TadLoaderDataType` was sent to
            ; the loader (while the song was loaded).
            .cerror !(((TadState.LOADING_SONG_DATA_PAUSED & 1) << 1) | $80 == TadState.PAUSED)
            .cerror !(((TadState.LOADING_SONG_DATA_PLAY & 1) << 1) | $80 == TadState.PLAYING)
            lda     TadPrivate_state
            and     #1
            asl     a
            ora     #$80

        ; A = new state
    _EndIf:
        sta     TadPrivate_state

_Return:
    rtl
.endproc



; JSL/RTL subroutine
.as
.xl
.databank TAD_DB_LOWRAM
; Called with JSL (far addressing)
Tad_FinishLoadingData .proc
    _Loop:
        TadPrivate_IsLoaderActive
        bcc     _EndLoop
            jsl     TadPrivate_Process_Loading__
        bra     _Loop
    _EndLoop:

    rtl
.endproc

