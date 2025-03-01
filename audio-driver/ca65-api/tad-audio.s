;; Terrific Audio Driver ca65 API

; This file MUST be recompiled if the memory map changes.
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


.setcpu "65816"
.smart

; Ensure autoimport is disabled
.autoimport -


.export Tad_Init : far, Tad_Process : far, Tad_FinishLoadingData : far
.export Tad_QueueCommand, Tad_QueueCommandOverride
.export Tad_QueuePannedSoundEffect, Tad_QueueSoundEffect
.export Tad_LoadSong, Tad_LoadSongIfChanged, Tad_GetSong, Tad_ReloadCommonAudioData
.export Tad_SongsStartImmediately, Tad_SongsStartPaused
.export Tad_GlobalVolumesResetOnSongStart, Tad_GlobalVolumesPersist
.export Tad_SetTransferSize
.export Tad_IsLoaderActive, Tad_IsSongLoaded, Tad_IsSfxPlaying, Tad_IsSongPlaying

.exportzp Tad_sfxQueue_sfx, Tad_sfxQueue_pan
.export Tad_flags, Tad_audioMode


;; =======
;; DEFINES
;; =======

;; Memory Map
;; ----------
;;
;; `tad-audio.s` requires either a `LOROM` or `HIROM` symbol to determine the memory map used by the ROM.

.if .defined(LOROM) && .defined(HIROM)
    .error "Cannot use HIROM and LOROM at the same time"
.endif
.if ! (.defined(LOROM) || .defined(HIROM))
    .error "Unknown memory map: Missing LOROM or HIROM define"
.endif


;; Segments
;; --------
;;
;; The following optional defines are used to determine the segment to place the code in.
;;
;;  * `TAD_PROCESS_SEGMENT` defines the segment to store the subroutines that processes the queues
;;     and loads data into Audio-RAM (`Tad_Init`, `Tad_Process`, `Tad_FinishLoadingData`).
;;      * The exported subroutines in this segment are called using `JSL` long addressing.
;;      * If `TAD_PROCESS_SEGMENT` is undefined, `TAD_CODE_SEGMENT` is used.
;;
;;  * `TAD_CODE_SEGMENT` defines the segment to store the remaining subroutines.
;;      * The subroutines in this segment are called using `JSR` absolute addressing.
;;      * If `TAD_CODE_SEGMENT` is undefined, "CODE" will be used.
;;
;;
;; NOTE: Because ca65 only allows numbers in `-D name=value` command line arguments, the only
;; way to set these defines is to create a new source file that defines `TAD_CODE_SEGMENT`
;; and/or `TAD_PROCESS_SEGMENT` and then includes `tad-audio.s`.
;;
;; For example:
;;
;;      .define TAD_CODE_SEGMENT "CODE1"
;;      .define TAD_PROCESS_SEGMENT "CODE3"
;;
;;      .include "../terrific-audio-driver/audio-driver/ca65-api/tad-audio.s"
;;

.if .not .match({TAD_CODE_SEGMENT}, {""})
    .define TAD_CODE_SEGMENT "CODE"
.endif

.if .not .match({TAD_PROCESS_SEGMENT}, {""})
    .define TAD_PROCESS_SEGMENT TAD_CODE_SEGMENT
.endif




;; ===========
;; Binary Data
;; ===========
;;
;; These 3 files MUST be embedded (using `.incbin`) into the ROM if the developer uses a custom
;; `LoadAudioData` callback.
;;

;; Terrific Audio Driver spc700 Loader (loader.bin)
.import Tad_Loader_Bin
.importzp Tad_Loader_SIZE

;; Terrific Audio Driver spc700 driver (audio-driver.bin)
.import Tad_AudioDriver_Bin, Tad_AudioDriver_SIZE


.assert Tad_Loader_SIZE > 64 && Tad_Loader_SIZE < 128, lderror, "Invalid Tad_Loader_Bin size"
.assert .bankbyte(Tad_Loader_Bin) = .bankbyte(Tad_Loader_Bin + Tad_Loader_SIZE), lderror, "Tad_Loader_Bin does not fit inside a single bank"

.assert Tad_AudioDriver_SIZE > $600 && Tad_AudioDriver_SIZE < $d00, lderror, "Invalid Tad_AudioDriver_Bin size"
; `Tad_AudioDriver_Bin` can cross bank boundaries



;; =========
;; CALLBACKS
;; =========


;; LoadAudioData callback
;;
;; IN: A = 0 - Common audio data (MUST return carry set)
;; IN: A >= 1 - Song data (might be invalid)
;; OUT: Carry set if input (`A`) was valid
;; OUT: A:X = far address
;; OUT: Y = size
;;
;; Called with JSL long addressing (returns with RTL).
.a8
.i16
;; DB access registers
.import LoadAudioData: Far


;; =========
;; CONSTANTS
;; =========


;; Address to store the loader (in Audio-RAM).
;; Address (in Audio-RAM) to execute after loading the Loader.
;; MUST match LOADER_ADDR in `audio-driver/src/common_memmap.wiz`.
TAD_LOADER_ARAM_ADDR = $0200


;; Minimum transfer size accepted by `Tad_SetTransferSize`
;;
;; MUST BE > 0
TAD_MIN_TRANSFER_PER_FRAME = 32

;; Maximum transfer size accepted by `Tad_SetTransferSize`
;;
;; The loader can transfer ~849 bytes per 60Hz frame SlowROM or FastROM
TAD_MAX_TRANSFER_PER_FRAME = 800



;; ========
;; IO Ports
;; ========

;; IO communication protocol version.
;;
;; Used by `tad-compiler ca65-export` to verify the IO protocol in `tad-audio.s` matches the audio-driver.
;;
;; This constant MUST be increased if `LOADER_ADDR` or the IO Communication protocol changes.
.export TAD_IO_VERSION : abs = 20


; MUST match `audio-driver/src/io-commands.wiz`
.enum TadCommand
    PAUSE = 0
    PAUSE_MUSIC_PLAY_SFX = 2
    UNPAUSE = 4
    PLAY_SOUND_EFFECT = 6
    STOP_SOUND_EFFECTS = 8
    SET_MAIN_VOLUME = 10
    SET_MUSIC_CHANNELS = 12
    SET_SONG_TIMER = 14
    SET_GLOBAL_MUSIC_VOLUME = 16
    SET_GLOBAL_SFX_VOLUME = 18
    SET_GLOBAL_VOLUMES = 20
.endenum

TAD_MAX_PAN = 128
TAD_CENTER_PAN = TAD_MAX_PAN / 2


;; MUST match `audio-driver/src/io-commands.wiz`
.scope TadIO_ToDriver
    ;; The command to execute.
    ;;
    ;;      iiicccci
    ;;         cccc = command
    ;;            i = command id, MUST be different on every command.
    ;;                Used to detect when a new command has been sent to the driver.
    ;;
    ;; NOTES:
    ;;  * The command will only be execute if the `command` byte has changed.
    ;;  * This value MUST be written last.
    ;;  * The command and parameter bytes MUST NOT change unless the previous command
    ;;    has been acknowledged.
    COMMAND_PORT = $2140 ; APUIO0

    COMMAND_MASK   = %00011110
    COMMAND_I_MASK = %11100001

    ;; The first command parameter port
    PARAMETER0_PORT = $2141 ; APUIO1

    ;; The second command parameter port
    PARAMETER1_PORT = $2142 ; APUIO2


    ;; Writing `SWITCH_TO_LOADER` to this port should stop execution and start the loader.
    ;;
    ;; If the audio-driver is running; if the `SWITCH_TO_LOADER_BIT` is set,
    ;; the audio driver will stop and execute the loader.
    ;;
    ;; If the loader is in the middle of a transfer and both the `SWITCH_TO_LOADER_BIT`
    ;; and MSB (bit 7) bits are set, the loader will restart.
    SWITCH_TO_LOADER_PORT = $2143 ; APUIO3

    SWITCH_TO_LOADER_BIT = 5
    SWITCH_TO_LOADER = $80 | (1 << SWITCH_TO_LOADER_BIT)
.endscope


;; MUST match `audio-driver/src/io-commands.wiz`
.scope TadIO_ToScpu
    ;; Audio driver command acknowledgment.
    ;;
    ;; Acknowledgment of the `ToDriver.command` byte.  Not used in the loader.
    ;;
    ;; After the command has been processed, the `IO.ToDriver.command` value will be written to this port.
    COMMAND_ACK_PORT = $2140 ; APUIO0


    ;; The mode the S-SMP is currently executing.
    ;;
    ;; Used by both the loader and the audio-driver.
    ;;
    ;; NOTE: The IPL sets this value after at has cleared the zero-page.
    ;;       Do not read this value immediately after reset.
    ;;       Make sure enough time has passed for the IPL to set IO Port 1
    ;;       to $bb before reading this port.
    MODE_PORT = $2141 ; APUIO1

    ;; The S-SMP is at the start of the IPL, waiting for the ready signal.
    MODE_IPL = $bb

    ;; The S-SMP is running the loader.
    MODE_LOADER = $4c ; 'L', Loader.LOADER_READY_L

    ;; The S-SMP is running the audio-driver.
    MODE_AUDIO_DRIVER = $61 ; 'a'
.endscope


;; MUST match `audio-driver/src/io-commands.wiz`
.scope TadLoaderDataType
    CODE        = 0
    COMMON_DATA = 1

    SONG_DATA_FLAG            = 1 << 7
    PLAY_SONG_FLAG            = 1 << 6
    RESET_GLOBAL_VOLUMES_FLAG = 1 << 5

    STEREO_FLAG               = 1 << 1
    SURROUND_FLAG             = 1 << 0
.endscope


;; MUST match `audio-driver/src/io-commands.wiz`
.scope TadIO_Loader_Init
    LOADER_DATA_TYPE_PORT = $2141 ; APUIO1
    READY_PORT_L          = $2142 ; APUIO2
    READY_PORT_H          = $2143 ; APUIO3

    READY_PORT_HL         = $2142 ; APUIO2 & APUIO3

    LOADER_READY_L = %01001100  ; 'L'
    LOADER_READY_H = %01000100  ; 'D'
    LOADER_READY_HL = LOADER_READY_L | (LOADER_READY_H << 8)
.endscope


;; MUST match `audio-driver/src/io-commands.wiz`
.scope TadIO_Loader
    DATA_PORT_L   = $2141 ; APUIO1
    DATA_PORT_H   = $2142 ; APUIO2
    SPINLOCK_PORT = $2143 ; APUIO3

    ;; The spinlock value when the audio driver starts playing a song
    SPINLOCK_INIT_VALUE = 0

    ;; Only the lower 4 bits of the spinlock should be set while sending data to the loader
    SPINLOCK_MASK = $0f

    ;; Signal to the loader that the transfer has completed.
    SPINLOCK_COMPLETE = $80

    ;; If this value is written to the spinlock, the loader will restart;
    SPINLOCK_SWITCH_TO_LOADER = TadIO_ToDriver::SWITCH_TO_LOADER
.endscope


.enum TadState
    NULL                                = $00
    ;; Waiting for loader to send the ready signal before loading common-audio-data
    WAITING_FOR_LOADER_COMMON           = $7b
    ;; Waiting for loader to send the ready signal before loading song data
    WAITING_FOR_LOADER_SONG             = $7c
    ;; Loading common audio data.
    LOADING_COMMON_AUDIO_DATA           = $7d
    ;; Loading a song and the TadLoaderDataType::PLAY_SONG_FLAG was clear.
    LOADING_SONG_DATA_PAUSED            = $7e
    ;; Loading a song and the TadLoaderDataType::PLAY_SONG_FLAG was set.
    LOADING_SONG_DATA_PLAY              = $7f
    ;; Song is loaded into Audio-RAM and the audio driver is paused.
    ;; No play-sound-effect commands will be sent when the driver is paused.
    PAUSED                              = $80
    ;; Song is loaded into Audio-RAM and the audio driver is playing sfx (song paused).
    PLAYING_SFX                         = $81
    ;; Song is loaded into Audio-RAM and the audio driver is playing the song.
    PLAYING                             = $82
.endenum
TAD__FIRST_WAITING_STATE      = TadState::WAITING_FOR_LOADER_COMMON
TAD__FIRST_LOADING_STATE      = TadState::LOADING_COMMON_AUDIO_DATA
TAD__FIRST_LOADING_SONG_STATE = TadState::LOADING_SONG_DATA_PAUSED


.scope TadFlags
    RELOAD_COMMON_AUDIO_DATA = 1 << 7
    PLAY_SONG_IMMEDIATELY = 1 << 6
    RESET_GLOBAL_VOLUMES_ON_SONG_START = 1 << 5

    ;; A mask for the flags that are sent to the loader
    _ALL_FLAGS = RELOAD_COMMON_AUDIO_DATA | PLAY_SONG_IMMEDIATELY | RESET_GLOBAL_VOLUMES_ON_SONG_START
.endscope

.enum TadAudioMode
    MONO = 0
    STEREO = 1
    SURROUND = 2
.endenum

TAD_N_AUDIO_MODES = 3


;; Default values
;; ==============

; Using a single symbol to enable custom defaults as I am unable to detect if a `.define`
; exists using an if statement.
;
; I recommend using a `.define` for custom defaults so `TadFlags` and `TadAudioMode` values
; can be referenced before they are defined.
.ifndef TAD_CUSTOM_DEFAULTS
    ;; Default TAD flags
    ;; MUST NOT set RELOAD_COMMON_AUDIO_DATA
    TAD_DEFAULT_FLAGS = TadFlags::PLAY_SONG_IMMEDIATELY

    ;; Starting audio mode
    TAD_DEFAULT_AUDIO_MODE = TadAudioMode::MONO

    ;; Default number of bytes to transfer to Audio-RAM per `Tad_Process` call.
    ;;
    ;; MUST be between the TAD_MIN_TRANSFER_PER_FRAME and TAD_MAX_TRANSFER_PER_FRAME
    TAD_DEFAULT_TRANSFER_PER_FRAME = 256
.endif



;; =========
;; Variables
;; =========


.bss
    ;; The current audio driver state
    ;; (`TadState` enum)
    TadPrivate_state: .res 1

    ;; `TadFlags` bitfield
    ;; (see `TadFlags` namespace)
    Tad_flags: .res 1

    ;; Mono/Stereo/Surround audio mode
    ;; (`TadAudioMode` enum)
    Tad_audioMode: .res 1

    ;; Number of bytes to transfer per `Tad_Process` call
    ;;
    ;; MUST be > 0
    TadPrivate_bytesToTransferPerFrame: .res 2

    ;; The previous `TadIO_ToScpu::COMMAND_PORT` sent to the S-SMP audio driver.
    TadPrivate_previousCommand: .res 1


;; ---------------------------------------------------
;; Queue 1 - remaining data to transfer into Audio-RAM
;; ---------------------------------------------------
.bss
    ;; A far pointer to the remaining data to transfer
    TadPrivate_dataToTransfer_addr: .res 2
    TadPrivate_dataToTransfer_bank: .res 1

    ;; The remaining number of bytes to transfer
    TadPrivate_dataToTransfer_size: .res 2

    ;; The previous value written to the loader spinLock
    TadPrivate_dataToTransfer_prevSpinLock: .res 1


;; ----------------------------------------------
;; Queue 2 - The next song to load into Audio-RAM
;; ----------------------------------------------
.bss
    ;; The next song to load into Audio-RAM
    ;; Used by the `WAITING_FOR_LOADER_*` states
    ;; If this value is 0 or an invalid song, a blank silent song will be loaded instead.
    TadPrivate_nextSong: .res 1


;; ------------------------------------------------------
;; Queue 3 - The next command to send to the audio driver
;; ------------------------------------------------------
.bss
    ;; The next `TadCommand` to send to the audio driver.
    ;; If this value is negative, the queue is empty.
    TadPrivate_nextCommand_id: .res 1

    ;; The two parameters of the next command (if any)
    TadPrivate_nextCommand_parameter0: .res 1
    TadPrivate_nextCommand_parameter1: .res 1


;; ---------------------------------------
;; Queue 4 - The next sound effect to play
;; ---------------------------------------
.zeropage
    ;; see tad-audio.inc
    Tad_sfxQueue_sfx: .res 1
    Tad_sfxQueue_pan: .res 1


;; Memory Map Asserts
;; ==================
.bss
    .assert ((* > $100) && (* < $2000)) || ((* > $7e0100) && (* < $7e2000)), lderror, ".bss is not in lowram"



;; ==================
;; Loader subroutines
;; ==================

.segment TAD_PROCESS_SEGMENT


;; Transfer and execute Loader using the IPL
;;
;; REQUIRES: S-SMP reset and no data has been written to it yet
;;
;; This macro MUST only be called once.  There is no way to reset the S-SMP and restart the IPL.
;;
;; A8
;; I16
;; DB access registers
.macro TadPrivate_Loader_TransferLoaderViaIpl
    .assert .asize = 8, error
    .assert .isize = 16, error

APUIO0 = $2140
APUIO1 = $2141
APUIO2 = $2142
APUIO3 = $2143

    ; Clear start command port (just in case APUIO0 has $cc in it)
    ; SOURCE: `blarggapu.s` from lorom-template, originally written by blargg (Shay Green)
    stz     APUIO0

    ; Wait for ready signal
    ldy     #$bbaa
    :
        cpy     APUIO0
        bne     :-

    ldx     #TAD_LOADER_ARAM_ADDR
    lda     #$cc
    stx     APUIO2              ; destination ARAM address
    sta     APUIO1              ; non-zero = write data to address
    sta     APUIO0              ; New data command (non-zero and APUIO0 + more than 2, or $cc on the first transfer)

    ; Wait for a response from the IPL
    :
        cmp     APUIO0
        bne     :-


    ; Transfer the data
    .assert Tad_Loader_SIZE < $ff, error, "Cannot fit Tad_Loader_SIZE in an 8 bit index"

    sep     #$30
.i8
    ldx     #0
    @IplLoop:
        ; Send the next byte to the IPL
        lda     f:Tad_Loader_Bin,x
        sta     APUIO1

        ; Tell the IPL the next byte is ready
        stx     APUIO0

        ; Wait for a response form the IPL
        :
            cpx     APUIO0
            bne     :-

        inx
        cpx     #Tad_Loader_SIZE
        bcc     @IplLoop

    rep     #$10
.i16

    ; Send an execute program command to the IPL
    ldx     #TAD_LOADER_ARAM_ADDR
    stx     APUIO2                  ; A-RAM address
    stz     APUIO1                  ; zero = execute program at A-RAM address
    lda     #Tad_Loader_SIZE + 2
    sta     APUIO0                  ; New data command (must be +2 the previous APUIO0 write)
.endmacro



;; Sends a TadLoaderDataType byte to the loader if the loader is ready
;;
;; Assumes loader just started OR a `SWITCH_TO_LOADER` message was sent to the audio driver/loader.
;;
;; IN: A = TadLoaderDataType value
;; OUT: carry = loader is ready and TadLoaderDataType sent
;;
.a8
.i16
;; DB access registers
.proc TadPrivate_Loader_CheckReadyAndSendLoaderDataType
    ; Test if the loader is ready
    ldx     #TadIO_Loader_Init::LOADER_READY_HL
    cpx     TadIO_Loader_Init::READY_PORT_HL
    bne     ReturnFalse
        ; Send the ready signal and the TadLoaderDataType
        sta     TadIO_Loader_Init::LOADER_DATA_TYPE_PORT

        lda     #TadIO_Loader_Init::LOADER_READY_L
        sta     TadIO_Loader_Init::READY_PORT_L

        lda     #TadIO_Loader_Init::LOADER_READY_H
        sta     TadIO_Loader_Init::READY_PORT_H

        ; The S-CPU must wait for the loader to write 0 to the spinlock before transferring data.
        stz     TadPrivate_dataToTransfer_prevSpinLock

        ; return true
        sec
        rts

ReturnFalse:
    clc
    rts
.endproc



;; Set the data transfer queue
;;
;; IN: A:X = far address
;; IN: Y = size
.a8
.i16
;; DB access registers
.proc TadPrivate_Loader_SetDataToTransfer
    stx     TadPrivate_dataToTransfer_addr
    sta     TadPrivate_dataToTransfer_bank
    sty     TadPrivate_dataToTransfer_size

    rts
.endproc



;; Transfer data to the audio loader.
;;
;; ASSUMES: `check_ready_and_send_loader_data_type` and `set_data_to_transfer` were previously called.
;;
;; NOTE: This function may read one byte past the end of the transfer queue.
;;
;; OUT: carry set if all data in the transfer queue was sent to Audio-RAM.
;;
.a8
.i16
;; DB access lowram
.proc TadPrivate_Loader_TransferData
    ; Early exit if the loader is not ready
    ;
    ; This test doubles as a lock for the previous transfer.
    ;
    ; This also prevents a freeze in `process()` if the loader has crashed/glitched.
    ; (`finish_loading_data()` will freeze if the loader has crashed/glitched.
    lda     TadPrivate_dataToTransfer_prevSpinLock
    cmp     f:TadIO_Loader::SPINLOCK_PORT
    bne     @ReturnFalse

    phd
    phb

    rep     #$30
.a16

    ; Calculate number of words to read
    lda     TadPrivate_dataToTransfer_size
    cmp     TadPrivate_bytesToTransferPerFrame
    bcc     :+
        lda     TadPrivate_bytesToTransferPerFrame
    :
    inc     ; required
    lsr

    ; Prevent corrupting all of Audio-RAM if number of words == 0
    bne     :+
        inc
    :
    ; Store word to read in X
    tax

    ; Reverse subtract TadPrivate_dataToTransfer_size (with clamping)
    asl                             ; convert number of words to number of bytes
    eor     #$ffff
    sec
    adc     TadPrivate_dataToTransfer_size
    bcs     :+
        lda     #0
    :
    sta     TadPrivate_dataToTransfer_size


    lda     #$2100
    tcd
; D = $2100

    sep     #$20
.a8

    lda     TadPrivate_dataToTransfer_bank
    ldy     TadPrivate_dataToTransfer_addr

    pha
    plb
; DB = TadPrivate_dataToTransfer_bank

    @Loop:
        ; x = number of words remaining
        ; y = data address (using y to force addr,y addressing mode)

        lda     a:0,y
        sta     z:.lobyte(TadIO_Loader::DATA_PORT_L)

        ; The bank overflow test must be done here as `TadPrivate_dataToTransfer_addr` might point to an odd memory address.
        iny
        beq     @BankOverflow_1
    @BankOverflow_1_Resume:

        lda     a:0,y
        sta     z:.lobyte(TadIO_Loader::DATA_PORT_H)

        ; Increment this spinloack value
        ;
        ; The upper 4 bits of the spinlock must be clear'
        ; Cannot be 0.  Zero is used to spinlock the loader init before this loop starts
        ;               (see Loader Step 3 in `terrific-audio-driver/audio-driver/src/io-commands.wiz`)

        .assert ($ffff & 7) + 1 < TadIO_Loader::SPINLOCK_MASK, error
        tya             ; y = address of data, it should always increment by 2
        and     #7
        inc
        sta     z:.lobyte(TadIO_Loader::SPINLOCK_PORT)

        iny
        beq     @BankOverflow_2
    @BankOverflow_2_Resume:

        dex
        beq     @EndLoop

        ; Spinloop until the S-SMP has acknowledged the data
        :
            cmp     z:.lobyte(TadIO_Loader::SPINLOCK_PORT)
            bne     :-

        bra     @Loop
@EndLoop:

    plb
    pld
; DB restored
; D = 0

    sty     TadPrivate_dataToTransfer_addr
    sta     TadPrivate_dataToTransfer_prevSpinLock


    ldy     TadPrivate_dataToTransfer_size
    bne     @ReturnFalse
        ; End of data transfer

        ; Wait for Loader to acknowledge the last write
        :
            cmp     f:TadIO_Loader::SPINLOCK_PORT
            bne     :-

        ; No more data to transfer
        lda     #TadIO_Loader::SPINLOCK_COMPLETE
        sta     f:TadIO_Loader::SPINLOCK_PORT

        sec
        rts

@ReturnFalse:
    clc
    rts


@BankOverflow_1:
    jsr     TadPrivate_Loader_GotoNextBank
    bra     @BankOverflow_1_Resume

@BankOverflow_2:
    ; Must save/restore A, it holds the spinlock
    pha
        jsr     TadPrivate_Loader_GotoNextBank
    pla
    bra     @BankOverflow_2_Resume
.endproc


;; Advance to the next bank
;;
;; MUST only be called to TadPrivate_Loader_TransferData
;;
;; ASSUMES: Y = 0 (Y addr overflowed to 0)
;;
;; IN: Y = 0
;; IN: DB = TadPrivate_dataToTransfer_bank
;;
;; OUT: Y = new address
;; OUT: DB = new bank
;;
;; KEEP: X
.a8
.i16
;; DB = TadPrivate_dataToTransfer_bank
.proc TadPrivate_Loader_GotoNextBank
    phb
    pla

    inc
    sta     f:TadPrivate_dataToTransfer_bank

    pha
    plb
; DB = new TadPrivate_dataToTransfer_bank value

    ; MUST NOT CHANGE X

    ; Y = 0
    .if .defined(LOROM)
        and     #$fe
        cmp     #$7e
        beq     :+
            ; Bank is not Work-RAM
            ldy     #$8000
        :
    .elseif .defined(HIROM)
        and     #$7f
        cmp     #$40
        bcs     :+
            ; Bank is a register bank
            ; set Y to the first ROM address
            ldy     #$8000
        :
    .else
        .error "Unknown memory map"
    .endif

    ; Y = 0 or $8000
    rts
.endproc


;; OUT: carry set if state is LOADING_*
;; A8
.macro TadPrivate_IsLoaderActive
    .assert .asize = 8, error

    .assert TadState::NULL < TAD__FIRST_LOADING_STATE, error
    .assert TadState::WAITING_FOR_LOADER_COMMON < TAD__FIRST_LOADING_STATE, error
    .assert TadState::WAITING_FOR_LOADER_SONG < TAD__FIRST_LOADING_STATE, error
    .assert (TadState::PAUSED & $7f) < TAD__FIRST_LOADING_STATE, error
    .assert (TadState::PLAYING & $7f) < TAD__FIRST_LOADING_STATE, error

    lda     TadPrivate_state
    and     #$7f
    cmp     #TAD__FIRST_LOADING_STATE
.endmacro


;; ==========
;; Public API
;; ==========

;; -------------------------------
;; TAD_PROCESS_SEGMENT subroutines
;; -------------------------------

.segment TAD_PROCESS_SEGMENT


; JSL/RTL subroutine
.a8
.i16
; DB unknown
.proc Tad_Init : far
    phb

    lda     #$80
    pha
    plb
; DB = $80

    TadPrivate_Loader_TransferLoaderViaIpl


    ; Set default settings
    .assert (TAD_DEFAULT_FLAGS) & TadFlags::RELOAD_COMMON_AUDIO_DATA = 0, error, "RELOAD_COMMON_AUDIO_DATA flag must not be use in TAD_DEFAULT_FLAGS"
    .assert (TAD_DEFAULT_FLAGS) & TadFlags::_ALL_FLAGS = (TAD_DEFAULT_FLAGS), error, "Invalid TAD_DEFAULT_FLAGS"
    .assert (TAD_DEFAULT_AUDIO_MODE) >= 0 && (TAD_DEFAULT_AUDIO_MODE) < TAD_N_AUDIO_MODES, error, "Invalid TAD_DEFAULT_AUDIO_MODE"

    .assert Tad_flags + 1 = Tad_audioMode, error
    ldx     #(TAD_DEFAULT_FLAGS) | ((TAD_DEFAULT_AUDIO_MODE) << 8)
    stx     Tad_flags

    ldx     #TAD_DEFAULT_TRANSFER_PER_FRAME
    stx     TadPrivate_bytesToTransferPerFrame


    lda     #.bankbyte(Tad_AudioDriver_Bin)
    ldx     #.loword(Tad_AudioDriver_Bin)
    ldy     #Tad_AudioDriver_SIZE
    jsr     TadPrivate_Loader_SetDataToTransfer

    lda     #$ff
    sta     TadPrivate_nextCommand_id
    sta     Tad_sfxQueue_sfx

    stz     TadPrivate_nextSong

    @DataTypeLoop:
        lda     #TadLoaderDataType::CODE
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     @DataTypeLoop

    @TransferLoop:
        jsr     TadPrivate_Loader_TransferData
        bcc     @TransferLoop

    lda     #TadState::WAITING_FOR_LOADER_COMMON
    sta     TadPrivate_state

    plb
; DB restored
    rtl
.endproc


;; Sends a command to the audio driver.
;;
;; REQUIRES: state == PAUSED or state == PLAYING.
;; REQUIRES: The previous command has been processed by the audio-driver.
;; REQUIRES: `TadPrivate_nextCommand_id` is not a play-sound-effect command.
;; REQUIRES: `TadPrivate_nextCommand_id` is a valid comma.
;;
;; IN: Y = TadPrivate_nextCommand_id
.a8
.i8
;; DB access lowram
.macro TadPrivate_Process_SendCommand
    .assert .asize = 8, error
    .assert .isize = 8, error

    lda     TadPrivate_nextCommand_parameter0
    sta     f:TadIO_ToDriver::PARAMETER0_PORT

    lda     TadPrivate_nextCommand_parameter1
    sta     f:TadIO_ToDriver::PARAMETER1_PORT

    lda     TadPrivate_previousCommand
    and     #TadIO_ToDriver::COMMAND_I_MASK    ; Clear the non i bits of the command
    eor     #TadIO_ToDriver::COMMAND_I_MASK    ; Flip the i bits
    ora     TadPrivate_nextCommand_id          ; Set the c bits
    sta     f:TadIO_ToDriver::COMMAND_PORT
    sta     TadPrivate_previousCommand

    cpy     #TadCommand::UNPAUSE + 1
    bcs     @NotPauseOrPlay
        ; Change state if the command is a pause or play command
        .assert TadCommand::PAUSE = 0, error
        .assert TadCommand::PAUSE_MUSIC_PLAY_SFX = 2, error
        .assert TadCommand::UNPAUSE = 4, error
        .assert (TadCommand::PAUSE >> 1) & 3 | $80 = TadState::PAUSED, error
        .assert (TadCommand::PAUSE_MUSIC_PLAY_SFX >> 1) & 3 | $80 = TadState::PLAYING_SFX, error
        .assert (TadCommand::UNPAUSE >> 1) & 3 | $80 = TadState::PLAYING, error
        lsr
        and     #3
        ora     #$80
        sta     TadPrivate_state
@NotPauseOrPlay:

    ; Reset command queue
    lda     #$ff
    sta     TadPrivate_nextCommand_id
.endmacro



;; Send a play-sound-effect command to the audio driver.
;;
;; REQUIRES: state == PLAYING
;; REQUIRES: The previous command has been processed by the audio-driver.
;;
;; IN: A = Tad_sfxQueue_sfx
;;
;; A8
;; I8
;; DB access lowram
.macro TadPrivate_Process_SendSfxCommand
    .assert .asize = 8, error
    .assert .isize = 8, error

    ; parameter 0 = sfx_id
    sta     f:TadIO_ToDriver::PARAMETER0_PORT

    ; parameter 1 = pan
    lda     Tad_sfxQueue_pan
    cmp     #TAD_MAX_PAN + 1
    bcc     :+
        lda     #TAD_CENTER_PAN
    :
    sta     f:TadIO_ToDriver::PARAMETER1_PORT

    ; Send play-sound-effect command
    lda     TadPrivate_previousCommand
    and     #TadIO_ToDriver::COMMAND_I_MASK            ; Clear the non i bits of the command
    eor     #TadIO_ToDriver::COMMAND_I_MASK            ; Flip the i bits
    ora     #TadCommand::PLAY_SOUND_EFFECT             ; Set the c bits

    sta     f:TadIO_ToDriver::COMMAND_PORT
    sta     TadPrivate_previousCommand

    ; Reset the SFX queue
    ldy     #$ff
    sty     Tad_sfxQueue_sfx
    sty     Tad_sfxQueue_pan
.endmacro



; JSL/RTL subroutine
.a8
.i16
; DB access lowram
.proc Tad_Process : far
    .assert TadState::PAUSED = $80, error
    .assert TadState::PLAYING > $80, error
    lda     TadPrivate_state
    bpl     @NotLoaded
        ; Playing or paused state
        sep     #$10
    .i8
        tax

        lda     TadPrivate_previousCommand
        cmp     f:TadIO_ToScpu::COMMAND_ACK_PORT
        bne     @Return_I8
            ; Previous command has been processed

            ; Check command queue
            ldy     TadPrivate_nextCommand_id
            bpl     @SendCommand

            ; X = TadPrivate_state
            .assert TadState::PAUSED < $81, error
            .assert TadState::PLAYING >= $81, error
            .assert TadState::PLAYING_SFX >= $81, error
            dex
            bpl     @Return_I8
                ; Playing state
                lda     Tad_sfxQueue_sfx
                cmp     #$ff
                beq     @Return_I8
                    TadPrivate_Process_SendSfxCommand

        @Return_I8:
            rep     #$10
        .i16
            rtl

        .a8
        .i8
        @SendCommand:
            TadPrivate_Process_SendCommand
            rep     #$10
        .i16
            rtl

    @NotLoaded:
        ; Song is not loaded into Audio-RAM

        ; Test if state is WAITING_FOR_LOADER_* or LOADING_*
        .assert TAD__FIRST_LOADING_STATE > TAD__FIRST_WAITING_STATE, error
        .assert TAD__FIRST_LOADING_STATE = TadState::WAITING_FOR_LOADER_SONG + 1, error
        cmp     #TAD__FIRST_LOADING_STATE
        bcs     TadPrivate_Process_Loading
        cmp     #TAD__FIRST_WAITING_STATE
        bcs     TadPrivate_Process_WaitingForLoader

    ; TadState is null
    rtl
.endproc



;; Process the WAITING_FOR_LOADER_* states
;;
;; return using RTL
.a8
.i16
;; DB access lowram
.proc TadPrivate_Process_WaitingForLoader ; RTL
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
    cmp     #TadState::WAITING_FOR_LOADER_COMMON
    bne     @SongData
        ; Common audio data
        lda     #TadLoaderDataType::COMMON_DATA
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     @Return

        lda     #TadState::LOADING_COMMON_AUDIO_DATA
        pha

        lda     #0
        bra     @LoadData

    @SongData:
        ; Songs

        ; Tad_flags MUST NOT have the stereo/surround loader flag set
        .assert TadFlags::_ALL_FLAGS & TadLoaderDataType::STEREO_FLAG = 0, error
        .assert TadFlags::_ALL_FLAGS & TadLoaderDataType::SURROUND_FLAG = 0, error

        ; SONG_DATA_FLAG must always be sent and it also masks the RELOAD_COMMON_AUDIO_DATA flag in TadLoaderDataType
        .assert TadFlags::RELOAD_COMMON_AUDIO_DATA = TadLoaderDataType::SONG_DATA_FLAG, error, "Cannot hide RELOAD_COMMON_AUDIO_DATA TadFlag with SONG_DATA_FLAG"

        .assert TadFlags::PLAY_SONG_IMMEDIATELY = TadLoaderDataType::PLAY_SONG_FLAG, error
        .assert TadFlags::RESET_GLOBAL_VOLUMES_ON_SONG_START = TadLoaderDataType::RESET_GLOBAL_VOLUMES_FLAG, error

        ; Clear unused TAD flags
        lda     #$ff ^ TadFlags::_ALL_FLAGS
        trb     Tad_flags

        ; Convert `Tad_audioMode` to TadLoaderDataType and combine with TadFlags
        .assert ((0 + 1) & 3) = TadLoaderDataType::SURROUND_FLAG, error ; mono
        .assert ((1 + 1) & 3) = TadLoaderDataType::STEREO_FLAG, error ; stereo
        .assert ((2 + 1) & 3) = TadLoaderDataType::STEREO_FLAG | TadLoaderDataType::SURROUND_FLAG, error ; surround
        lda     Tad_audioMode
        inc
        and     #3

        ora     Tad_flags
        ora     #TadLoaderDataType::SONG_DATA_FLAG
        jsr     TadPrivate_Loader_CheckReadyAndSendLoaderDataType
        bcc     @Return

        ; Determine next state
        .assert TadFlags::PLAY_SONG_IMMEDIATELY = $40, error
        .assert TadState::LOADING_SONG_DATA_PAUSED + 1 = TadState::LOADING_SONG_DATA_PLAY, error
        lda     Tad_flags
        asl
        asl
        lda     #0
        ; carry = PLAY_SONG_IMMEDIATELY flag
        adc     #TadState::LOADING_SONG_DATA_PAUSED
        pha

        ; Load next song
        lda     TadPrivate_nextSong
        beq     @UseBlankSong

@LoadData:
    jsl     LoadAudioData
    bcs     :+
        ; LoadAudioData returned false
    @UseBlankSong:
        ; The blank song is a single zero byte.
        ; ::HACK use the 3rd byte of `ldy #1` (which is `0x00`) for the blank song data::
        ldy     #1
        @_BlankSongData = * - 1
        lda     #.bankbyte(@_BlankSongData)
        ldx     #.loword(@_BlankSongData)
    :

    ; STACK holds next state
    ; A:X = data address
    ; Y = data size
    jsr     TadPrivate_Loader_SetDataToTransfer

    ; Must set state AFTER the `LoadAudioData` call.
    ; `LoadAudioData` might call `Tad_FinishLoadingData`.
    pla
    sta     TadPrivate_state

@Return:
    plb
; DB restored
    rtl
.endproc



;; Process the LOADING_* states
;;
;; return using RTL
.a8
.i16
;; DB access lowram
.proc TadPrivate_Process_Loading ; RTL
    jsr     TadPrivate_Loader_TransferData
    bcc     @Return
        ; Data loaded successfully
        lda     TadPrivate_state
        cmp     #TadState::LOADING_COMMON_AUDIO_DATA
        bne     @Song
            ; Common audio data was just transferred
            ; Loader is still active
            lda     #TadState::WAITING_FOR_LOADER_SONG
            bra     @EndIf

        @Song:
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
            .assert ((TadState::LOADING_SONG_DATA_PAUSED & 1) << 1) | $80 = TadState::PAUSED, error
            .assert ((TadState::LOADING_SONG_DATA_PLAY & 1) << 1) | $80 = TadState::PLAYING, error
            lda     TadPrivate_state
            and     #1
            asl
            ora     #$80

        ; A = new state
    @EndIf:
        sta     TadPrivate_state

@Return:
    rtl
.endproc



; JSL/RTL subroutine
.a8
.i16
; DB access lowram
.proc Tad_FinishLoadingData : far
    @Loop:
        TadPrivate_IsLoaderActive
        bcc     @EndLoop
            jsl     TadPrivate_Process_Loading
        bra     @Loop
    @EndLoop:

    rtl
.endproc


;; ----------------------------
;; TAD_CODE_SEGMENT subroutines
;; ----------------------------

.segment TAD_CODE_SEGMENT


; IN: A = command
; IN: X = first parameter
; IN: Y = second parameter
; OUT: Carry set if command added to queue
.a8
; I unknown
; DB access lowram
.proc Tad_QueueCommand
    bit     TadPrivate_nextCommand_id
    bpl     ReturnFalse
        ; command queue is empty
    WriteCommand:
        and     #TadIO_ToDriver::COMMAND_MASK
        sta     TadPrivate_nextCommand_id

        txa
        sta     TadPrivate_nextCommand_parameter0

        tya
        sta     TadPrivate_nextCommand_parameter1

        ; return true
        sec
        rts

ReturnFalse:
    clc
    rts
.endproc


; IN: A = command
; IN: X = first parameter
; IN: Y = second parameter
.a8
; I unknown
; DB access lowram
Tad_QueueCommandOverride := Tad_QueueCommand::WriteCommand



; IN: A = sfx id
; IN: X = pan
.a8
; I unknown
; DB access lowram
; KEEP: X, Y
.proc Tad_QueuePannedSoundEffect
    cmp     Tad_sfxQueue_sfx
    bcs     @EndIf
        sta     Tad_sfxQueue_sfx

        txa
        sta     Tad_sfxQueue_pan

@EndIf:
    rts
.endproc



; IN: A = sfx_id
.a8
; I unknown
; DB access lowram
; KEEP: X, Y
.proc Tad_QueueSoundEffect
    cmp     Tad_sfxQueue_sfx
    bcs     @EndIf
        sta     Tad_sfxQueue_sfx

        lda     #TAD_CENTER_PAN
        sta     Tad_sfxQueue_pan
@EndIf:
    rts
.endproc



; IN: A = song_id
.a8
; I unknown
; DB access lowram
.proc Tad_LoadSong
    .assert TAD__FIRST_LOADING_SONG_STATE > TadState::NULL, error
    .assert TAD__FIRST_LOADING_SONG_STATE > TadState::WAITING_FOR_LOADER_COMMON, error
    .assert TAD__FIRST_LOADING_SONG_STATE > TadState::WAITING_FOR_LOADER_SONG, error
    .assert TAD__FIRST_LOADING_SONG_STATE > TadState::LOADING_COMMON_AUDIO_DATA, error


    sta     TadPrivate_nextSong

    lda     #TadFlags::RELOAD_COMMON_AUDIO_DATA
    trb     Tad_flags
    beq     @SongRequested
        ; Common audio data requested
        lda     #TadState::WAITING_FOR_LOADER_COMMON
        bra     @SetStateAndSwitchToLoader

@SongRequested:
    lda     TadPrivate_state
    cmp     #TAD__FIRST_LOADING_SONG_STATE
    bcc     @Return
        ; TadState is not NULL, WAITING_FOR_LOADER_* or LOADING_COMMON_AUDIO_DATA

        lda     #TadState::WAITING_FOR_LOADER_SONG

    @SetStateAndSwitchToLoader:
        sta     TadPrivate_state

        ; Assert it is safe to send a switch-to-loader command when the loader is waiting for a READY signal
        .assert TadIO_ToDriver::SWITCH_TO_LOADER <> TadIO_Loader_Init::LOADER_READY_H, error
        .assert TadIO_ToDriver::SWITCH_TO_LOADER_PORT = TadIO_Loader_Init::READY_PORT_H, error

        ; Send a *switch-to-loader* command to the audio-driver or loader
        lda     #TadIO_ToDriver::SWITCH_TO_LOADER
        sta     f:TadIO_ToDriver::SWITCH_TO_LOADER_PORT
@Return:
    rts
.endproc


; IN: A = song_id
; OUT: carry set if `Tad_LoadSong` was called
.a8
; I unknown
; DB access lowram
.proc Tad_LoadSongIfChanged
    cmp     TadPrivate_nextSong
    beq     :+
        jsr     Tad_LoadSong
        sec
        rts
    :
    clc
    rts
.endproc


;; OUT: A = The song_id used in the last `Tad_LoadSong` call.
.a8
; I unknown
; DB access lowram
.proc Tad_GetSong
    ; `TadPrivate_nextSong` is only written to in `Tad_Init` and `Tad_LoadSong`.
    lda     TadPrivate_nextSong
    rts
.endproc


.a8
; I unknown
; DB access lowram
.proc Tad_ReloadCommonAudioData
    lda     #TadFlags::RELOAD_COMMON_AUDIO_DATA
    tsb     Tad_flags
    rts
.endproc


.a8
; I unknown
; DB access lowram
.proc Tad_SongsStartImmediately
    lda     #TadFlags::PLAY_SONG_IMMEDIATELY
    tsb     Tad_flags
    rts
.endproc


.a8
; I unknown
; DB access lowram
.proc Tad_SongsStartPaused
    lda     #TadFlags::PLAY_SONG_IMMEDIATELY
    trb     Tad_flags
    rts
.endproc


.a8
; I unknown
; DB access lowram
.proc Tad_GlobalVolumesResetOnSongStart
    lda     #TadFlags::RESET_GLOBAL_VOLUMES_ON_SONG_START
    tsb     Tad_flags
    rts
.endproc


.a8
; I unknown
; DB access lowram
.proc Tad_GlobalVolumesPersist
    lda     #TadFlags::RESET_GLOBAL_VOLUMES_ON_SONG_START
    trb     Tad_flags
    rts
.endproc


; IN: X = new `TadPrivate_bytesToTransferPerFrame` value
; A unknown
.i16
; DB access lowram
.proc Tad_SetTransferSize
    cpx     #TAD_MAX_TRANSFER_PER_FRAME
    bcc     :+
        ldx     #TAD_MAX_TRANSFER_PER_FRAME
    :
    cpx     #TAD_MIN_TRANSFER_PER_FRAME
    bcs     :+
        ldx     #TAD_MIN_TRANSFER_PER_FRAME
    :
    stx     TadPrivate_bytesToTransferPerFrame

    rts
.endproc



; OUT: carry set if state is LOADING_*
.a8
; I unknown
; DB access lowram
.proc Tad_IsLoaderActive
    TadPrivate_IsLoaderActive
    rts
.endproc



; OUT: carry set if state is PAUSED, PLAYING_SFX or PLAYING
.a8
; I unknown
; DB access lowram
.proc Tad_IsSongLoaded
    .assert TadState::PLAYING_SFX > TadState::PAUSED, error
    .assert TadState::PLAYING > TadState::PAUSED, error
    ; Assumes PLAYING is the last state

    lda     TadPrivate_state
    cmp     #TadState::PAUSED
    rts
.endproc



; OUT: carry set if state is PLAYING_SFX or PLAYING
.a8
; I unknown
; DB access lowram
.proc Tad_IsSfxPlaying
    .assert TadState::PLAYING > TadState::PLAYING_SFX, error
    ; Assumes PLAYING is the last state

    lda     TadPrivate_state
    cmp     #TadState::PLAYING_SFX
    rts
.endproc



; OUT: carry set if state is PLAYING
.a8
; I unknown
; DB access lowram
.proc Tad_IsSongPlaying
    ; Assumes PLAYING is the last state

    lda     TadPrivate_state
    cmp     #TadState::PLAYING
    rts
.endproc


