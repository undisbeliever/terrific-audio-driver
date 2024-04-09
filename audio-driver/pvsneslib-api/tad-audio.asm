;; Terrific Audio Driver PVSnesLib API
;;
;; Adapted and ported from the Ca65 API.

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


.include "hdr.asm"


; ::TODO should I use .b .w .l suffixes to the instructions?::


;;
;; ===
;; ABI
;; ===
;;
;; Since the PVSnesLib/tcc ABI is not documented and with an abundance of caution,
;; all public functions in this file are called with the following ABI:
;;  * unknown mem size
;;  * unknown index size
;;  * DB unknown
;;  * D = 0
;;  * Callee saved index registers
;;      * X and Y are preserved after a function call.
;;      * A is volatile after a function call.
;;  * function arguments pushed on the stack.
;;  * function return value is stored in `tcc__r0`.
;;      * function return values are a maximum of 2 bytes
;;      * return values that are smaller than 2 bytes are promoted to 16 bits.
;;      * bool return values are returned as PVSNESLIB_TRUE ($00ff) or PVSNESLIB_FALSE ($0000)
;;        to match TRUE/FALSE in `pvsneslib/include/snes/snestypes.h`.
;;
;; Functions and variables that end in a double-underscore are private and MUST NOT BE CALLED
;; or MODIFIED outside of this file.
;;
;;
;; To ensure this ABI is met, all public functions (not ending with a double-underscore `__`) MUST:
;;  * Have a single exit/return point
;;  * Never return directly
;;  * Invoke a `__Push__*` macro at the start of the function
;;  * Invoke a `__PopReturn_*` macro at the end of the function
;;
;;
;; All functions and macros that end in a double-underscore `__` are called with a more
;; assembly-friendly ABI that is documented at the function definition.
;;  * Subroutine and macros exit with the same register sizes and stack pointer it started with.
;;     * EXCEPTION: `__Push__*` and `__PopReturn_*` macros
;;  * A, X, Y are clobbered, unless there is a KEEP tag in the subroutine documentation.
;;  * D = 0, unless otherwise marked
;;


;; Memory Map
;; ----------
;;
;; `tad-audio.asm` requires either a `LOROM` or `HIROM` .define to determine the memory map used by the ROM.

.if defined(LOROM) && defined(HIROM)
    .fail "Cannot use HIROM and LOROM at the same time"
.endif
.if ! (defined(LOROM) || defined(HIROM))
    .fail "Unknown memory map: Please .define LOROM or HIROM in hdr.asm"
.endif


;; ========
;; IO Ports
;; ========
;;
;; MUST match `audio-driver/src/io-commands.wiz`


;; Address to store the loader (in Audio-RAM).
;; Address (in Audio-RAM) to execute after loading the Loader.
;; MUST match LOADER_ADDR in `audio-driver/src/common_memmap.wiz`.
TAD_LOADER_ARAM_ADDR = $0200


TAD_Command__PAUSE = 0
TAD_Command__UNPAUSE = 2
TAD_Command__PLAY_SOUND_EFFECT_COMMAND = 4
TAD_Command__STOP_SOUND_EFFECTS = 8
TAD_Command__SET_MAIN_VOLUME = 10
TAD_Command__SET_ENABLED_CHANNELS = 12
TAD_Command__SET_SONG_TEMPO = 14

TAD_COMMAND_MASK   = %00001110
TAD_COMMAND_I_MASK = %11100001

MAX_PAN = 128
CENTER_PAN = MAX_PAN / 2


;; The command to execute.
;;
;;      iii0ccci
;;          ccc = command
;;            0 = reserved for future expansion
;;            i = command id, MUST be different on every command.
;;                Used to detect when a new command has been sent to the driver.
;;
;; NOTES:
;;  * The command will only be execute if the `command` byte has changed.
;;  * This value MUST be written last.
;;  * The command and parameter bytes MUST NOT change unless the previous command
;;    has been acknowledged.
TAD_IO_ToDriver__COMMAND_PORT = $2140 ; APUIO0

TAD_IO_ToDriver__N_COMMANDS = 8
TAD_IO_ToDriver__COMMAND_MASK   = %00001110
TAD_IO_ToDriver__COMMAND_I_MASK = %11100001

;; The first command parameter port
TAD_IO_ToDriver__PARAMETER0_PORT = $2141 ; APUIO1

;; The second command parameter port
TAD_IO_ToDriver__PARAMETER1_PORT = $2142 ; APUIO2


;; Writing `SWITCH_TO_LOADER` to this port should stop execution and start the loader.
;;
;; If the audio-driver is running; if the `SWITCH_TO_LOADER_BIT` is set,
;; the audio driver will stop and execute the loader.
;;
;; If the loader is in the middle of a transfer and both the `SWITCH_TO_LOADER_BIT`
;; and MSB (bit 7) bits are set, the loader will restart.
TAD_IO_ToDriver__SWITCH_TO_LOADER_PORT = $2143 ; APUIO3

TAD_IO_ToDriver__SWITCH_TO_LOADER_BIT = 5
TAD_IO_ToDriver__SWITCH_TO_LOADER = $80 | (1 << TAD_IO_ToDriver__SWITCH_TO_LOADER_BIT)


;; Audio driver command acknowledgment.
;;
;; Acknowledgment of the `ToDriver.command` byte.  Not used in the loader.
;;
;; After the command has been processed, the `IO.ToDriver.command` value will be written to this port.
TAD_IO_ToScpu__COMMAND_ACK_PORT = $2140 ; APUIO0


;; The mode the S-SMP is currently executing.
;;
;; Used by both the loader and the audio-driver.
;;
;; NOTE: The IPL sets this value after at has cleared the zero-page.
;;       Do not read this value immediately after reset.
;;       Make sure enough time has passed for the IPL to set IO Port 1
;;       to $bb before reading this port.
TAD_IO_ToScpu__MODE_PORT = $2141 ; APUIO1

;; The S-SMP is at the start of the IPL, waiting for the ready signal.
TAD_IO_ToScpu__MODE_IPL = $bb

;; The S-SMP is running the loader.
TAD_IO_ToScpu__MODE_LOADER = $4c ; 'L', Loader.LOADER_READY_L

;; The S-SMP is running the audio-driver.
TAD_IO_ToScpu__MODE_AUDIO_DRIVER = $61 ; 'a'


;; The `audio-driver.bin` file.
;; MUST be loaded first.
TAD_LoaderDataType__CODE        = 0

;; Common audio data.
;; Contains samples, pitch table and sound effects.
;; MUST be loaded after `LoaderDataType.CODE` and before song data.
TAD_LoaderDataType__COMMON_DATA = 1

;; Any value over `MIN_SONG_VALUE` will load song data
;; Song data MUST be loaded after `LoaderDataType.COMMON_DATA`.
TAD_LoaderDataType__MIN_SONG_VALUE = 2

;; If this bit is set, the song will be played in stereo.
;; If this bit is clear, the song will be played in mono.
;;
;; MUST NOT be set when loading code or common-audio-data.
TAD_LoaderDataType__STEREO_FLAG_BIT = 7
TAD_LoaderDataType__STEREO_FLAG = 1 << TAD_LoaderDataType__STEREO_FLAG_BIT

;; If this bit is set, the song will play after the echo buffer has been cleared.
;; If this bit is clear, the audio driver will start in a paused state.
;;
;; MUST NOT be set when loading code or common-audio-data.
TAD_LoaderDataType__PLAY_SONG_BIT = 6
TAD_LoaderDataType__PLAY_SONG_FLAG = 1 << TAD_LoaderDataType__PLAY_SONG_BIT


;; MUST match `audio-driver/src/io-commands.wiz`
TAD_IO_Loader_Init__LOADER_DATA_TYPE_PORT = $2141 ; APUIO1
TAD_IO_Loader_Init__READY_PORT_L          = $2142 ; APUIO2
TAD_IO_Loader_Init__READY_PORT_H          = $2143 ; APUIO3

TAD_IO_Loader_Init__READY_PORT_HL         = $2142 ; APUIO2 & APUIO3

TAD_IO_Loader_Init__LOADER_READY_L = %01001100  ; 'L'
TAD_IO_Loader_Init__LOADER_READY_H = %01000100  ; 'D'
TAD_IO_Loader_Init__LOADER_READY_HL = %0100010001001100 ; "LD"


;; MUST match `audio-driver/src/io-commands.wiz`
TAD_IO_Loader__DATA_PORT_L   = $2141 ; APUIO1
TAD_IO_Loader__DATA_PORT_H   = $2142 ; APUIO2
TAD_IO_Loader__SPINLOCK_PORT = $2143 ; APUIO3

;; The spinlock value when the audio driver starts playing a song
TAD_IO_Loader__SPINLOCK_INIT_VALUE = 0

;; Only the lower 4 bits of the spinlock should be set while sending data to the loader
TAD_IO_Loader__SPINLOCK_MASK = $0f

;; Signal to the loader that the transfer has completed.
TAD_IO_Loader__SPINLOCK_COMPLETE = $80

;; If this value is written to the spinlock, the loader will restart;
TAD_IO_Loader__SPINLOCK_SWITCH_TO_LOADER = TAD_IO_ToDriver__SWITCH_TO_LOADER



;; =========
;; CONSTANTS
;; =========

;; Minimum transfer size accepted by `tad_setTransferSize`
;;
;; MUST BE > 0
TAD_MIN_TRANSFER_PER_FRAME = 32

;; Maximum transfer size accepted by `tad_setTransferSize`
;;
;; The loader can transfer ~849 bytes per 60Hz frame SlowROM or FastROM
TAD_MAX_TRANSFER_PER_FRAME = 800

;; Default number of bytes to transfer to Audio-RAM per `tad_process` call.
;;
;; MUST BE > 0
TAD_DEFAULT_TRANSFER_PER_FRAME = 256    ; ::TODO should I decrement this value?::


;; ------
;; States
;; ------

TAD_State__NULL                         = $00

;; Waiting for loader to send the ready signal.
TAD_State__WAITING_FOR_LOADER           = $7c

;; Loading common audio data.
TAD_State__LOADING_COMMON_AUDIO_DATA    = $7d

;; Loading a song and the LoaderDataType::PLAY_SONG_FLAG was clear.
TAD_State__LOADING_SONG_DATA_PAUSED     = $7e

;; Loading a song and the LoaderDataType::PLAY_SONG_FLAG was set.
TAD_State__LOADING_SONG_DATA_PLAY       = $7f

;; Song is loaded into Audio-RAM and the audio driver is paused.
;; No play-sound-effect commands will be sent when the driver is paused.
TAD_State__PAUSED                       = $80

;; Song is loaded into Audio-RAM and the audio driver is playing the song.
TAD_State__PLAYING                      = $81

TAD_FIRST_LOADING_STATE      = TAD_State__LOADING_COMMON_AUDIO_DATA
TAD_FIRST_LOADING_SONG_STATE = TAD_State__LOADING_SONG_DATA_PAUSED


;; -----
;; Flags
;; -----


;; The mono/stereo flag
;;  * If set, the next song will be played in stereo.
;;  * If clear, the next song will be played in mono.
;; Default: Mono
TAD_Flags__STEREO                   = TAD_LoaderDataType__STEREO_FLAG

;; Determines if the song is played immediately after loading into Audio-RAM
;;  * If set, the audio driver will play the song after the next song is loaded into Audio-RAM
;;  * If clear, the audio driver will be paused after the next song is loaded into Audio-RAM
;; Default: Set
TAD_Flags__PLAY_SONG_IMMEDIATELY    = TAD_LoaderDataType__PLAY_SONG_FLAG

;; If set the *common audio data* will be loaded into Audio-RAM the next time a song is requested.
;;
;; This flag is cleared after the *common audio data* is loaded into Audio-RAM
TAD_Flags__RELOAD_COMMON_AUDIO_DATA = 1 << 0

;; A mask for the flags that are sent to the loader
TAD_Flags__LOADER_MASK = TAD_Flags__STEREO | TAD_Flags__PLAY_SONG_IMMEDIATELY


;; ============
;; State Macros
;; ============


;; OUT: carry set if state is LOADING_*
;; A8
;; I unknown
;; DB unknown
.macro __Tad_IsLoaderActive__a8_far_carry__
    .assert TAD_State__NULL < TAD_FIRST_LOADING_STATE
    .assert TAD_State__WAITING_FOR_LOADER < TAD_FIRST_LOADING_STATE
    .assert (TAD_State__PAUSED & $7f) < TAD_FIRST_LOADING_STATE
    .assert (TAD_State__PLAYING & $7f) < TAD_FIRST_LOADING_STATE

    lda.l   tad_state__
    and     #$7f
    cmp     #TAD_FIRST_LOADING_STATE
.endm


;; OUT: carry set if state is LOADING_*
;; A8
;; I unknown
;; DB = $80
.macro __Tad_IsLoaderActive__a8_db80_carry__
    .assert TAD_State__NULL < TAD_FIRST_LOADING_STATE
    .assert TAD_State__WAITING_FOR_LOADER < TAD_FIRST_LOADING_STATE
    .assert (TAD_State__PAUSED & $7f) < TAD_FIRST_LOADING_STATE
    .assert (TAD_State__PLAYING & $7f) < TAD_FIRST_LOADING_STATE

    lda.w   tad_state__
    and     #$7f
    cmp     #TAD_FIRST_LOADING_STATE
.endm



;; ====================================================
;; PVSnesLib interfacing function start and exit macros
;; ====================================================

// From `pvsneslib/include/snes/snestypes.h`
.define PVSNESLIB_TRUE  $ff
.define PVSNESLIB_FALSE $00


;; Push P to the stack and set mem register size to 8 bits.
;;
;; Functions that invoke this macro MUST NOT MODIFY the index registers.
;; Functions that invoke this macro MUST use the long addressing mode.
;;
.macro __Push__A8_noX_noY
    .redefine __FUNCTION_TYPE 0x00a80000
    .redefine _stack_arg_offset 5

        php
        sep     #$20
    .accu 8
.endm

;; Push P to the stack and set mem register size to 16 bits.
;;
;; Functions that invoke this macro MUST NOT MODIFY the index registers.
;; Functions that invoke this macro MUST use the long addressing mode.
;;
.macro __Push__A16_noX_noY
    .redefine __FUNCTION_TYPE 0x00a80000
    .redefine _stack_arg_offset 5

        php
        rep     #$20
    .accu 16
.endm

;; Restore the stack and return.
;;
;; MUST ONLY be used on functions that invoke `__Push__A8_noX_noY` or `__Push__A16_noX_noY`.
;; MUST ONLY be invoked once per function
.macro __PopReturn_noX_noY
    .assert __FUNCTION_TYPE == 0x00a80000

    .redefine __FUNCTION_TYPE == 0
    .redefine _stack_arg_offset 0xffff
        plp
        rtl
.endm

;; Convert a u8 to a PVSnesLib return value, restore the stack and return the u8 value.
;;
;; MUST ONLY be used on functions that invoke `__Push__A8_noX_noY` or `__Push__A16_noX_noY`.
;; MUST ONLY be called when .accu size is 8
;; MUST ONLY be invoked once per function
.macro __PopReturn_A8_noX_noY__u8_in_a
    rep     #$20
.accu 16
    and.w   #$00ff
    sta.b   tcc__r0

    __PopReturn_noX_noY
.endm

;; Convert the carry flag to a PVSnesLib bool, restore the stack and return the bool value.
;;
;; MUST ONLY be used on functions that invoke `__Push__A8_noX_noY` or `__Push__A16_noX_noY`.
;; MUST ONLY be invoked once per function
.macro __PopReturn_noX_noY__bool_in_carry
    rep     #$20
.accu 16
    bcc     +
        lda     #PVSNESLIB_TRUE
        bra     ++
    +
        lda     #PVSNESLIB_FALSE
    ++
    sta.b   tcc__r0

    __PopReturn_noX_noY
.endm

;; Convert the negative flag to a PVSnesLib bool, restore the stack and return the bool value.
;;
;; MUST ONLY be used on functions that invoke `__Push__A8_noX_noY` or `__Push__A16_noX_noY`.
;; MUST ONLY be invoked once per function
.macro __PopReturn_noX_noY__bool_in_negative
    rep     #$20
.accu 16
    bpl     +
        lda     #PVSNESLIB_TRUE
        bra     ++
    +
        lda     #PVSNESLIB_FALSE
    ++
    sta.b   tcc__r0

    __PopReturn_noX_noY
.endm



;; Push P, DB and index to the stack, switch to the Loader ABI (a8, i16, DB=$80).
;;
;; Functions that invoke this macro can modify the index registers.
;;
.macro __Push__A8_X16_Y16_DB_80
    .redefine __FUNCTION_TYPE 0xa8161680
    // Stack arg offset unknown (index size unknown)
    .redefine _stack_arg_offset 10

        php
        phb

        sep     #$20
        rep     #$10
    .accu 8
    .index 16
        phx
        phy

        lda     #0x80
        pha
        plb
    // DB = $80
.endm


;; Restore the stack and return.
;;
;; MUST ONLY be used on functions that invoke `__Push__A8_X16_Y16_DB_80`.
;; MUST ONLY be invoked once per function
;; MUST be invoked with a 16 bit Index
;;
;; I16
.macro __PopReturn_X16_Y16_DB_80
    .assert __FUNCTION_TYPE == 0xa8161680

    .redefine __FUNCTION_TYPE == 0
    .redefine _stack_arg_offset 0xffff
        ply
        plx

        plb
        plp

        rtl
.endm


;; ----------------------------
;; Macros to call tcc functions
;; ----------------------------


;; Call the loadAudioData callback using the tcc ABI.
;;
;; IN: A = loadAudioData id argument
;; PARAM: @_bytes_on_stack - the number of bytes currently on the stack
;;
;; OUT: carry set if input (`A`) was valid
;; OUT: A:X far pointer
;; OUT:   Y size
;;
;; old `DB` on stack
;; A8
;; I16
;; DB = 0x80
.macro __Call_loadAudioData__return_carry__
    ; Push loadAudioData argument to stack
    pha

    rep     #$30
.accu 16
.index 16
    ; Reset registers (just to be safe)
    lda     #0
    tax
    tay

    ; Retrieve old DB and P from stack
    .assert __FUNCTION_TYPE == 0xa8161680  ; Make sure `tad_process` invoked __Push__A8_X16_Y16_DB_80
    lda     @_bytes_on_stack + _stack_arg_offset - 5 + 1,s  ; +1 for the `pha` above
    pha
    plb
; DB unknown

    ; Reset size so `loadAudioData` does not need to set `loadAudioData_out.size` to 0 if `id` is invalid.
    stz.w   loadAudioData_out.size

    plp
; P unknown
; .accu unknown
; .index unknown

    jsl     loadAudioData

    rep     #$10
    sep     #$20
.accu 8
.index 16
    ; Replace loadAudioData argument with 0x80 for `plb` below
    lda     #$80
    sta     1,s

    lda.w   loadAudioData_out.data + 2
    ldx.w   loadAudioData_out.data
    ldy.w   loadAudioData_out.size

    plb
; DB = 0x80

    ; Return carry set if size is non-zero
    ; `cpy` is required as `plb` changes the zero flag
    ; Using 1 so this macro returns the carry flag (matching the ca65 loadAudioData API)
    cpy     #1
.endm


;; ============================================
;; Functions that send data to the audio driver
;; ============================================

.section "tad_processSegment" SUPERFREE


;; ------------------
;; Loader subroutines
;; ------------------
;;
;; These subroutines and macros ARE NOT called using the PVSnesLib ABI.

; DB = $80
.16bit


;; Transfer and execute Loader using the IPL
;;
;; REQUIRES: S-SMP reset and no data has been written to it yet
;;
;; This macro MUST only be called once.  There is no way to reset the S-SMP and restart the IPL.
;;
;; A8
;; I16
;; DB = $80
.macro _tad_loader_transferLoaderViaIpl__
APUIO0 = $2140
APUIO1 = $2141
APUIO2 = $2142
APUIO3 = $2143

    ; Clear start command port (just in case APUIO0 has $cc in it)
    ; SOURCE: `blarggapu.s` from lorom-template, originally written by blargg (Shay Green)
    stz     APUIO0

    ; Wait for ready signal
    ldy     #$bbaa
    -
        cpy     APUIO0
        bne     -

    ldx     #TAD_LOADER_ARAM_ADDR
    lda     #$cc
    stx     APUIO2              ; destination ARAM address
    sta     APUIO1              ; non-zero = write data to address
    sta     APUIO0              ; New data command (non-zero and APUIO0 + more than 2, or $cc on the first transfer)

    ; Wait for a response from the IPL
    -
        cmp     APUIO0
        bne     -

    sep     #$30
.index 8
    ldx     #0
    @IplLoop:
        ; Send the next byte to the IPL
        lda.l   Tad_Loader_Bin,x
        sta     APUIO1

        ; Tell the IPL the next byte is ready
        stx     APUIO0

        ; Wait for a response form the IPL
        -
            cpx     APUIO0
            bne     -

        inx
        cpx     #Tad_Loader_SIZE
        bcc     @IplLoop

    rep     #$10
.index 16

    ; Send an execute program command to the IPL
    ldx     #TAD_LOADER_ARAM_ADDR
    stx     APUIO2                  ; A-RAM address
    stz     APUIO1                  ; zero = execute program at A-RAM address
    lda     #Tad_Loader_SIZE + 2
    sta     APUIO0                  ; New data command (must be +2 the previous APUIO0 write)
.endm



;; Sends a LoaderDataType byte to the loader if the loader is ready
;;
;; Assumes loader just started OR a `SWITCH_TO_LOADER` message was sent to the audio driver/loader.
;;
;; IN: A = LoaderDataType value
;; OUT: carry = loader is ready and LoaderDataType sent
;;
.accu 8
.index 16
;; DB = $80
_tad_loader_checkReadyAndSendLoaderDataType__:
    ; Test if the loader is ready
    ldx     #TAD_IO_Loader_Init__LOADER_READY_HL
    cpx     TAD_IO_Loader_Init__READY_PORT_HL
    bne     @ReturnFalse
        ; Send the ready signal and the LoaderDataType
        sta     TAD_IO_Loader_Init__LOADER_DATA_TYPE_PORT

        lda     #TAD_IO_Loader_Init__LOADER_READY_L
        sta     TAD_IO_Loader_Init__READY_PORT_L

        lda     #TAD_IO_Loader_Init__LOADER_READY_H
        sta     TAD_IO_Loader_Init__READY_PORT_H

        ; The S-CPU must wait for the loader to write 0 to the spinlock before transferring data.
        stz     tad_dataToTransfer_prevSpinLock__

        ; return true
        sec
        rts

@ReturnFalse:
    clc
    rts



;; Set the data transfer queue
;;
;; IN: A:X = far address
;; IN: Y = size
.accu 8
.index 16
;; DB = $80
_tad_loader_setDataToTransfer__:
    stx     tad_dataToTransfer_addr__
    sta     tad_dataToTransfer_bank__
    sty     tad_dataToTransfer_size__

    rts



;; Transfer data to the audio loader.
;;
;; ASSUMES: `check_ready_and_send_loader_data_type` and `set_data_to_transfer` were previously called.
;;
;; NOTE: This function may read one byte past the end of the transfer queue.
;;
;; OUT: carry set if all data in the transfer queue was sent to Audio-RAM.
;;
.accu 8
.index 16
;; DB = $80
_tad_loader_transferData__:
    ; APUIO registers are accessed with direct-page addressing
    @__dp__DATA_PORT_L      = TAD_IO_Loader__DATA_PORT_L & 0xff
    @__dp__DATA_PORT_H      = TAD_IO_Loader__DATA_PORT_H & 0xff
    @__dp__SPINLOCK_PORT    = TAD_IO_Loader__SPINLOCK_PORT & 0xff

    ; Early exit if the loader is not ready
    ;
    ; This test doubles as a lock for the previous transfer.
    ;
    ; This also prevents a freeze in `process()` if the loader has crashed/glitched.
    ; (`finish_loading_data()` will freeze if the loader has crashed/glitched.
    lda     tad_dataToTransfer_prevSpinLock__
    cmp     TAD_IO_Loader__SPINLOCK_PORT
    bne     @ReturnFalse

    phd
    phb

    rep     #$30
.accu 16

    ; Calculate number of words to read
    lda     tad_dataToTransfer_size__
    cmp     tad_bytesToTransferPerFrame__
    bcc     +
        lda     tad_bytesToTransferPerFrame__
    +
    ina     ; required
    lsr

    ; Prevent corrupting all of Audio-RAM if number of words == 0
    bne     +
        ina
    +
    ; Store word to read in X
    tax

    ; Reverse subtract tad_dataToTransfer_size (with clamping)
    asl                             ; convert number of words to number of bytes
    eor     #$ffff
    sec
    adc     tad_dataToTransfer_size__
    bcs     +
        lda     #0
    +
    sta     tad_dataToTransfer_size__


    lda     #$2100
    tcd
; D = $2100

    sep     #$20
.accu 8

    lda     tad_dataToTransfer_bank__
    ldy     tad_dataToTransfer_addr__

    pha
    plb
; DB = tad_dataToTransfer_bank__

    @Loop:
        ; x = number of words remaining
        ; y = data address (using y to force addr,y addressing mode)

        lda.w   0,y
        sta.b   @__dp__DATA_PORT_L

        ; The bank overflow test must be done here as `tad_dataToTransfer_addr` might point to an odd memory address.
        iny
        beq     @BankOverflow_1
    @BankOverflow_1_Resume:

        lda.w   0,y
        sta.b   @__dp__DATA_PORT_H

        ; Increment this spinloack value
        ;
        ; The upper 4 bits of the spinlock must be clear'
        ; Cannot be 0.  Zero is used to spinlock the loader init before this loop starts
        ;               (see Loader Step 3 in `terrific-audio-driver/audio-driver/src/io-commands.wiz`)

        .assert ($ffff & 7) + 1 < TAD_IO_Loader__SPINLOCK_MASK
        tya             ; y = address of data, it should always increment by 2
        and     #7
        ina
        sta.b   @__dp__SPINLOCK_PORT

        iny
        beq     @BankOverflow_2
    @BankOverflow_2_Resume:

        dex
        beq     @EndLoop

        ; Spinloop until the S-SMP has acknowledged the data
        -
            cmp.b   @__dp__SPINLOCK_PORT
            bne     -

        bra     @Loop
@EndLoop:

    plb
    pld
; DB restored (0x80)
; D = 0

    sty     tad_dataToTransfer_addr__
    sta     tad_dataToTransfer_prevSpinLock__


    ldy     tad_dataToTransfer_size__
    bne     @ReturnFalse
        ; End of data transfer

        ; Wait for Loader to acknowledge the last write
        -
            cmp     TAD_IO_Loader__SPINLOCK_PORT
            bne     -

        ; No more data to transfer
        lda     #TAD_IO_Loader__SPINLOCK_COMPLETE
        sta     TAD_IO_Loader__SPINLOCK_PORT

        sec
        rts

@ReturnFalse:
    clc
    rts


@BankOverflow_1:
    jsr     _tad_loader_gotoNextBank__
    bra     @BankOverflow_1_Resume

@BankOverflow_2:
    ; Must save/restore A, it holds the spinlock
    pha
        jsr     _tad_loader_gotoNextBank__
    pla
    bra     @BankOverflow_2_Resume



;; Advance to the next bank
;;
;; MUST only be called to _tad_loader_transferData
;;
;; ASSUMES: Y = 0 (Y addr overflowed to 0)
;;
;; IN: Y = 0
;; IN: DB = tad_dataToTransfer_bank
;;
;; OUT: Y = new address
;; OUT: DB = new bank
;;
;; KEEP: X
.accu 8
.index 16
;; DB = tad_dataToTransfer_bank
;; D = $2100
_tad_loader_gotoNextBank__:
    phb
    pla

    ina
    sta.l   tad_dataToTransfer_bank__

    pha
    plb
; DB = new tad_dataToTransfer_bank value

    ; MUST NOT CHANGE X

    ; Y = 0
    .if defined(LOROM)
        and     #$fe
        cmp     #$7e
        beq     +
            ; Bank is not Work-RAM
            ldy     #$8000
        +
    .elif defined(HIROM)
        and     #$7f
        cmp     #$40
        bcs     +
            ; Bank is a register bank
            ; set Y to the first ROM address
            ldy     #$8000
        +
    .else
        .fail "Unknown memory map"
    .endif

    ; Y = 0 or $8000
    rts


;; --------------
;; Process macros
;; --------------

;; Sends a command to the audio driver.
;;
;; REQUIRES: state == PAUSED or state == PLAYING.
;; REQUIRES: The previous command has been processed by the audio-driver.
;; REQUIRES: `tad_nextCommand_id` is not a play-sound-effect command.
;; REQUIRES: `tad_nextCommand_id` is a valid comma.
;;
;; IN: Y = tad_nextCommand_id
;;
;; A8
;; I8
;; DB = $80
.macro _Tad_Process_SendCommand__
    lda     tad_nextCommand_parameter__
    sta     TAD_IO_ToDriver__PARAMETER0_PORT

    lda     tad_previousCommand__
    and     #TAD_IO_ToDriver__COMMAND_I_MASK    ; Clear the non i bits of the command
    eor     #TAD_IO_ToDriver__COMMAND_I_MASK    ; Flip the i bits
    ora     tad_nextCommand_id__            ; Set the c bits
    sta     TAD_IO_ToDriver__COMMAND_PORT
    sta     tad_previousCommand__

    cpy     #TAD_Command__UNPAUSE + 1
    bcs     @SC_NotPauseOrPlay
        ; Change state if the command is a pause or play command
        .assert TAD_Command__PAUSE == 0
        .assert TAD_Command__UNPAUSE == 2
        .assert (TAD_Command__PAUSE >> 1) & 1 | $80 == TAD_State__PAUSED
        .assert (TAD_Command__UNPAUSE >> 1) & 1 | $80 = TAD_State__PLAYING
        lsr
        and     #1
        ora     #$80
        sta     tad_state__
@SC_NotPauseOrPlay:

    ; Reset command queue
    lda     #$ff
    sta     tad_nextCommand_id__
.endm



;; Send a play-sound-effect command to the audio driver.
;;
;; REQUIRES: state == PLAYING
;; REQUIRES: The previous command has been processed by the audio-driver.
;;
;; IN: A = tad_sfxQueue
;;
;; A8
;; I8
;; DB = $80
.macro _Tad_Process_SendSfxCommand__
    ; parameter 0 = sfx_id
    sta     TAD_IO_ToDriver__PARAMETER0_PORT

    ; parameter 1 = pan
    lda     tad_sfxQueue_pan__
    cmp     #MAX_PAN + 1
    bcc     +
        lda     #CENTER_PAN
    +
    sta     TAD_IO_ToDriver__PARAMETER1_PORT

    ; Send play-sound-effect command
    lda     tad_previousCommand__
    and     #TAD_IO_ToDriver__COMMAND_I_MASK            ; Clear the non i bits of the command
    eor     #TAD_IO_ToDriver__COMMAND_I_MASK            ; Flip the i bits
    ora     #TAD_Command__PLAY_SOUND_EFFECT_COMMAND     ; Set the c bits

    sta     TAD_IO_ToDriver__COMMAND_PORT
    sta     tad_previousCommand__

    ; Reset the SFX queue
    ldy     #$ff
    sty     tad_sfxQueue__
.endm



;; Process the WAITING_FOR_LOADER state
;;
;; A8
;; I16
;; DB = $80
.macro _Tad_Process_WaitingForLoader__
    lda     tad_flags__
    bit     #TAD_Flags__RELOAD_COMMON_AUDIO_DATA
    beq     @WFL_SongData
        ; Common audio data
        lda     #TAD_LoaderDataType__COMMON_DATA
        jsr     _tad_loader_checkReadyAndSendLoaderDataType__
        bcc     @WFL_Return

        ; Clear the RELOAD_COMMON_AUDIO_DATA flag
        ;
        ; It is safe to do this before the data is loaded into audio-RAM.
        ; `tad_loadSong` will not restart the loader if state == LOADING_COMMON_AUDIO_DATA.
        lda     #TAD_Flags__RELOAD_COMMON_AUDIO_DATA
        trb     tad_flags__

        lda     #TAD_State__LOADING_COMMON_AUDIO_DATA
        pha

        lda     #0
        bra     @WFL_LoadData

    @WFL_SongData:
        ; Songs

        ; a = tad_flags
        and     #TAD_Flags__LOADER_MASK
        ora     #TAD_LoaderDataType__MIN_SONG_VALUE
        jsr     _tad_loader_checkReadyAndSendLoaderDataType__
        bcc     @WFL_Return

        ; Determine next state
        .assert TAD_Flags__PLAY_SONG_IMMEDIATELY == $40
        .assert TAD_State__LOADING_SONG_DATA_PAUSED + 1 == TAD_State__LOADING_SONG_DATA_PLAY
        lda     tad_flags__
        asl
        asl
        lda     #0
        ; carry = PLAY_SONG_IMMEDIATELY flag
        adc     #TAD_State__LOADING_SONG_DATA_PAUSED
        pha

        ; Load next song
        lda     tad_nextSong__
        beq     @WFL_UseBlankSong

@WFL_LoadData:
    ; STACK holds next state
    @_bytes_on_stack = 1
    __Call_loadAudioData__return_carry__
    bcs     +
        ; `loadAudioData` returned data with a non-zero size
    @WFL_UseBlankSong:
        lda     #:Tad_BlankSong_Bin
        ldx     #Tad_BlankSong_Bin
        ldy     #Tad_BlankSong_SIZE
    +

    ; STACK holds next state
    ; A:X = data address
    ; Y = data size
    jsr     _tad_loader_setDataToTransfer__

    pla
    sta     tad_state__
@WFL_Return:
.endm



;; Process the LOADING_* states
;;
;; REQUIRES: State = LOADING_*
;;
.accu 8
.index 16
;; DB = 0x80
_tad_process__loading__:
    jsr     _tad_loader_transferData__
    bcc     @Return
        ; Data loaded successfully
        lda     tad_state__
        cmp     #TAD_State__LOADING_COMMON_AUDIO_DATA
        bne     @Song
            ; Common audio data was just transferred
            ; Loader is still active
            lda     #TAD_State__WAITING_FOR_LOADER
            bra     @EndIf

        @Song:
            ; song data was loaded into Audio-RAM
            ; Loader has finished, audio driver is now active

            stz     tad_previousCommand__

            ; Reset command and SFX queues
            lda     #$ff
            sta     tad_nextCommand_id__
            sta     tad_sfxQueue__

            ; Use `tad_state` to determine if the song is playing or paused.
            ; Cannot use `tad_flags` as it may have changed after the `LoaderDataType` was sent to
            ; the loader (while the song was loaded).
            .assert TAD_State__LOADING_SONG_DATA_PAUSED & 1 | $80 == TAD_State__PAUSED
            .assert TAD_State__LOADING_SONG_DATA_PLAY & 1 | $80 == TAD_State__PLAYING
            lda     tad_state__
            and     #1
            ora     #$80

        ; A = new state
    @EndIf:
        sta     tad_state__

@Return:
    rts



;; Based off `tad_process` from the ca65 API, modified to jump to the end of the macro (single return point)
;;
;; A8
;; I16
;; DB = 0x80
.macro __Tad_Process__
    .assert TAD_State__PAUSED == $80
    .assert TAD_State__PLAYING > $80

    lda     tad_state__
    bpl     @NotLoaded
        ; Playing or paused state
        sep     #$10
    .index 8
        tax

        lda     tad_previousCommand__
        cmp     TAD_IO_ToScpu__COMMAND_ACK_PORT
        bne     @Return_I8
            ; Previous command has been processed

            ; Check command queue
            ldy     tad_nextCommand_id__
            bpl     @SendCommand

            ; X = tad_state
            .assert TAD_State__PLAYING == $81
            dex
            bpl     @Return_I8
                ; Playing state
                lda     tad_sfxQueue__
                cmp     #$ff
                beq     @Return_I8
                    _Tad_Process_SendSfxCommand__
                    bra     @Return_I8

        .accu 8
        .index 8
        @SendCommand:
            _Tad_Process_SendCommand__
        @Return_I8:
            rep     #$10
        .index 16
            jmp     @Return_I16

    .accu 8
    .index 16
    @NotLoaded:
        ; Song is not loaded into Audio-RAM

        ; Test if state is WAITING_FOR_LOADER or LOADING_*
        .assert TAD_FIRST_LOADING_STATE == TAD_State__WAITING_FOR_LOADER + 1
        cmp     #TAD_State__WAITING_FOR_LOADER
        beq     @WaitingForLoader
        bcc     @Return_I16
            @Loading:
                jsr     _tad_process__loading__
                bra     @Return_I16

            @WaitingForLoader:
                _Tad_Process_WaitingForLoader__

// A 8
// I 16
@Return_I16:
.endm



;; ----------
;; Public API
;; ----------

; void tad_init(void)
tad_init:
    __Push__A8_X16_Y16_DB_80
.accu 8
.index 16
// DB = $80

    _tad_loader_transferLoaderViaIpl__

    lda     #TAD_Flags__RELOAD_COMMON_AUDIO_DATA | TAD_Flags__PLAY_SONG_IMMEDIATELY
    sta     tad_flags__

    ldx     #TAD_DEFAULT_TRANSFER_PER_FRAME
    stx     tad_bytesToTransferPerFrame__

    lda     #:Tad_AudioDriver_Bin
    ldx     #Tad_AudioDriver_Bin
    ldy     #Tad_AudioDriver_SIZE
    jsr     _tad_loader_setDataToTransfer__

    lda     #$ff
    sta     tad_nextCommand_id__
    sta     tad_sfxQueue__

    stz     tad_nextSong__

    @DataTypeLoop:
        lda     #TAD_LoaderDataType__CODE
        jsr     _tad_loader_checkReadyAndSendLoaderDataType__
        bcc     @DataTypeLoop

    @TransferLoop:
        jsr     _tad_loader_transferData__
        bcc     @TransferLoop

    lda     #TAD_State__WAITING_FOR_LOADER
    sta     tad_state__

    __PopReturn_X16_Y16_DB_80



; void tad_process(void)
tad_process:
    __Push__A8_X16_Y16_DB_80
.accu 8
.index 16
// DB = $80

    __Tad_Process__

    __PopReturn_X16_Y16_DB_80



; void tad_finishLoadingData(void)
tad_finishLoadingData:
    __Push__A8_X16_Y16_DB_80
.accu 8
.index 16
// DB = $80

    @Loop:
        __Tad_IsLoaderActive__a8_db80_carry__
        bcc     @EndLoop
            jsr     _tad_process__loading__
        bra     @Loop
    @EndLoop:

    __PopReturn_X16_Y16_DB_80



.ends



;; ===================
;; All other functions
;; ===================
;;
;; Called using the PVSnesLib ABI, with DB unknown.
;; All subroutines in this section access variables using long addressing.

.24bit


.section "tad_queuePannedSoundEffect" SUPERFREE

; void tad_queuePannedSoundEffect(u8 sfx_id, u8 pan)
tad_queuePannedSoundEffect:
    __Push__A8_noX_noY
.accu 8

    lda     _stack_arg_offset + 0,s
    cmp.l   tad_sfxQueue__
    bcs     +
        sta.l   tad_sfxQueue__

        lda     _stack_arg_offset + 1,s
        sta.l   tad_sfxQueue_pan__
    +

    __PopReturn_noX_noY
.ends



.section "tad_queueSoundEffect" SUPERFREE

; void tad_queueSoundEffect(u8 sfx_id)
tad_queueSoundEffect:
    __Push__A8_noX_noY
.accu 8

    lda     _stack_arg_offset,s
    cmp.l   tad_sfxQueue__
    bcs     +
        sta.l   tad_sfxQueue__

        lda     #CENTER_PAN
        sta.l   tad_sfxQueue_pan__
    +

    __PopReturn_noX_noY
.ends



.section "tad_loadSong" SUPERFREE

; void tad_loadSong(u8 song_id)
tad_loadSong:
    .assert TAD_FIRST_LOADING_SONG_STATE > TAD_State__NULL
    .assert TAD_FIRST_LOADING_SONG_STATE > TAD_State__WAITING_FOR_LOADER
    .assert TAD_FIRST_LOADING_SONG_STATE > TAD_State__LOADING_COMMON_AUDIO_DATA

    __Push__A8_noX_noY
.accu 8

    lda     _stack_arg_offset,s
    sta.l   tad_nextSong__

    lda.l   tad_state__
    cmp     #TAD_FIRST_LOADING_SONG_STATE
    bcc     +
        ; State is not NULL, WAITING_FOR_LOADER or LOADING_COMMON_AUDIO_DATA

        ; Send a *switch-to-loader* command to the audio-driver or loader
        lda     #TAD_IO_ToDriver__SWITCH_TO_LOADER
        sta.l   TAD_IO_ToDriver__SWITCH_TO_LOADER_PORT

        lda     #TAD_State__WAITING_FOR_LOADER
        sta.l   tad_state__
    +

    __PopReturn_noX_noY
.ends



.section "tad_loadSongIfChanged" SUPERFREE

; bool tad_loadSongIfChanged(u8 song_id)
tad_loadSongIfChanged:

    __Push__A8_noX_noY
.accu 8

    lda     _stack_arg_offset,s
    cmp.l   tad_nextSong__
    bne     +
        lda     #PVSNESLIB_FALSE
        bra     ++
    +
        pha
        jsl     tad_loadSong
        pla

        lda     #PVSNESLIB_TRUE
    ++

    __PopReturn_A8_noX_noY__u8_in_a
.ends



.section "tad_setTransferSize" SUPERFREE

; void tad_setTransferSize(u16 transferSize)
tad_setTransferSize:
    __Push__A16_noX_noY
.accu 16

    lda     _stack_arg_offset,s
    cmp     #TAD_MAX_TRANSFER_PER_FRAME
    bcc     +
        lda     #TAD_MAX_TRANSFER_PER_FRAME
    +
    cmp     #TAD_MIN_TRANSFER_PER_FRAME
    bcs     +
        lda     #TAD_MIN_TRANSFER_PER_FRAME
    +
    sta.l   tad_bytesToTransferPerFrame__

    __PopReturn_noX_noY
.ends



;; -------------------------
;; Set/Clear Flags functions
;; -------------------------

.macro __Tad_FlagFunction args NAME, FLAG, STATE
    .section "\1" SUPERFREE
    ; void \1(void)
    \1:
        __Push__A8_noX_noY
    .accu 8

        lda.l   tad_flags__
        .if \3
            ora     #TAD_Flags__\2
        .else
            and     #~TAD_Flags__\2
        .endif
        sta.l   tad_flags__

        __PopReturn_noX_noY
    .ends
.endm

__Tad_FlagFunction tad_reloadCommonAudioData RELOAD_COMMON_AUDIO_DATA 1
__Tad_FlagFunction tad_setStereo             STEREO                   1
__Tad_FlagFunction tad_setMono               STEREO                   0
__Tad_FlagFunction tad_songsStartImmediately PLAY_SONG_IMMEDIATELY    1
__Tad_FlagFunction tad_songsStartPaused      PLAY_SONG_IMMEDIATELY    0



;; ------------------------
;; State Querying Functions
;; ------------------------


.section "tad_getStereoFlag" SUPERFREE
; bool tad_getStereoFlag(void)
tad_getStereoFlag:
    __Push__A8_noX_noY

    .assert TAD_Flags__STEREO == $80
    lda.l   tad_flags__

    __PopReturn_noX_noY__bool_in_negative
.ends


.section "tad_isLoaderActive" SUPERFREE

; bool tad_isLoaderActive(void)
tad_isLoaderActive:
    __Push__A8_noX_noY

    __Tad_IsLoaderActive__a8_far_carry__

    __PopReturn_noX_noY__bool_in_carry
.ends



.section "tad_isSongLoaded" SUPERFREE

; bool tad_isSongLoaded(void)
tad_isSongLoaded:
    __Push__A8_noX_noY

    .assert TAD_State__PLAYING > TAD_State__PAUSED
    lda.l   tad_state__
    cmp     #TAD_State__PAUSED

    __PopReturn_noX_noY__bool_in_carry
.ends



.section "tad_isSongPlaying" SUPERFREE

; bool tad_isSongPlaying(void)
tad_isSongPlaying:
    __Push__A8_noX_noY
.accu 8
    ; Assumes PLAYING is the last state
    lda.l   tad_state__
    cmp     #TAD_State__PLAYING

    __PopReturn_noX_noY__bool_in_carry
.ends



;; --------------------------
;; Queue IO Command Functions
;; --------------------------

.macro _Tad_QueueCommandFunction args NAME TYPE PARAMETER COMMAND_ID
    .section "\1" SUPERFREE
        \1:
            ; A size is unknown
            ;   in 8 bit mode, it reads `lda $cc` and `nop`
            ;   in 16 bit mode, it reads `lda #$eacc`, which is masked in tad__QueueCommand_with_param__
            lda.b   #TAD_Command__\4
            nop
            jmp.l   tad__Command_A__\2_\3__
    .ends
.endm

_Tad_QueueCommandFunction tad_queueCommand_pause                       Test        NoParameter     PAUSE
_Tad_QueueCommandFunction tad_queueCommand_unpause                     Test        NoParameter     UNPAUSE
_Tad_QueueCommandFunction tad_queueCommand_stopSoundEffects            Test        NoParameter     STOP_SOUND_EFFECTS
_Tad_QueueCommandFunction tad_queueCommand_setMainVolume               Test        WithParameter   SET_MAIN_VOLUME
_Tad_QueueCommandFunction tad_queueCommand_setEnabledChannels          Test        WithParameter   SET_ENABLED_CHANNELS
_Tad_QueueCommandFunction tad_queueCommand_setSongTempo                Test        WithParameter   SET_SONG_TEMPO

_Tad_QueueCommandFunction tad_queueCommandOverride_pause               Override    NoParameter     PAUSE
_Tad_QueueCommandFunction tad_queueCommandOverride_unpause             Override    NoParameter     UNPAUSE
_Tad_QueueCommandFunction tad_queueCommandOverride_stopSoundEffects    Override    NoParameter     STOP_SOUND_EFFECTS
_Tad_QueueCommandFunction tad_queueCommandOverride_setMainVolume       Override    WithParameter   SET_MAIN_VOLUME
_Tad_QueueCommandFunction tad_queueCommandOverride_setEnabledChannels  Override    WithParameter   SET_ENABLED_CHANNELS
_Tad_QueueCommandFunction tad_queueCommandOverride_setSongTempo        Override    WithParameter   SET_SONG_TEMPO


.section "tad__Command_A__Test_NoParameter__" SUPERFREE
; MUST NOT be called by tcc
;
; IN: A = command (bit 8 MUST be clear)
;
; OUT: tcc__r0 = bool
;
; A unknown
; I unknown
; DB unknown
tad__Command_A__Test_NoParameter__:
    __Push__A8_noX_noY
.accu 8

    ; Cannot use `bit tad_nextCommand_id__` here, DB is unknown
    ; Temporarily Save it in B
    xba

    lda.l   tad_nextCommand_id__
    bpl     @QueueFull
        xba
        sta.l   tad_nextCommand_id__

        rep     #$20
    .accu 16
        lda     #$ff
        bra     @EndIf

    .accu 8
    @QueueFull:
        rep     #$20
    .accu 16
        lda     #0

.accu 16
@EndIf:
    ; return bool in tcc__r0
    sta.b   tcc__r0

    __PopReturn_noX_noY
.ends


.section "tad__Command_A__Test_WithParameter__" SUPERFREE
; MUST NOT be called by tcc
;
; IN: A = command (bit 8 MUST be clear)
; IN: u8 parameter on stack
;
; OUT: tcc__r0 = bool
;
; A unknown
; I unknown
; DB unknown
tad__Command_A__Test_WithParameter__:
    __Push__A8_noX_noY

    sep     #$20
.accu 8

    ; Cannot use `bit tad_nextCommand_id__` here, DB is unknown
    ; Temporarily Save it in B
    xba

    lda.l   tad_nextCommand_id__
    bpl     @QueueFull
        xba
        sta.l   tad_nextCommand_id__

        lda     _stack_arg_offset,s
        sta.l   tad_nextCommand_parameter__

        rep     #$20
    .accu 16
        lda     #$ff
        bra     @EndIf

    .accu 8
    @QueueFull:
        rep     #$20
    .accu 16
        lda     #0

.accu 16
@EndIf:
    ; return bool in tcc__r0
    sta.b   tcc__r0

    __PopReturn_noX_noY
.ends



.section "tad__Command_A__Override_NoParameter__" SUPERFREE
; MUST NOT be called by tcc
;
; IN: A = command (bit 8 MUST be clear)
;
; OUT: tcc__r0 = bool
;
; A unknown
; I unknown
; DB unknown
tad__Command_A__Override_NoParameter__:
    __Push__A8_noX_noY
.accu 8

    sta.l   tad_nextCommand_id__

    __PopReturn_noX_noY
.ends



.section "tad__Command_A__Override_WithParameter__" SUPERFREE
; MUST NOT be called by tcc
;
; IN: A = command (bit 8 MUST be clear)
; IN: u8 parameter on stack
;
; OUT: tcc__r0 = bool
;
; A unknown
; I unknown
; DB unknown
tad__Command_A__Override_WithParameter__:
    __Push__A8_noX_noY

    sta.l   tad_nextCommand_id__

    lda     _stack_arg_offset,s
    sta.l   tad_nextCommand_parameter__

    __PopReturn_noX_noY
.ends



;; =========
;; Variables
;; =========

; Variables MUST BE last as I need to reset the `.bank` to 0 and there is no
; way to restore it back to the $00/$80/$c0/$40 used in `hdr.asm`

; Reset .base so `tad_variables` are mapped to the correct bank.
; (Required for HIROM mapping as they use base $40 or $c0, which erroneously map
; `tad_variables` to a non-lowram bank)
.base 0

; Storing variables in lowram ($000000 - $002000) so variables and APUIO registers can be
; accessed with the same DB Data Bank Register value.
;
; Using WINDOW to ensure the variables are not allocated to zeropage or stack addresses.
.ramsection "tad_variables" BANK $0 SLOT 1 WINDOW $0100 $1400
    ;; The current audio driver state
    ;; (`State` enum)
    tad_state__: db

    ;; `Flags` bitfield
    ;; (see `Flags` namespace)
    tad_flags__: db

    ;; Number of bytes to transfer per `tad_process` call
    ;;
    ;; MUST be > 0
    tad_bytesToTransferPerFrame__: dw

    ;; The previous `IO_ToScpu::COMMAND_PORT` sent to the S-SMP audio driver.
    tad_previousCommand__: db


;; ---------------------------------------------------
;; Queue 1 - remaining data to transfer into Audio-RAM
;; ---------------------------------------------------
    ;; A far pointer to the remaining data to transfer
    tad_dataToTransfer_addr__: dw
    tad_dataToTransfer_bank__: db

    ;; The remaining number of bytes to transfer
    tad_dataToTransfer_size__: dw

    ;; The previous value written to the loader spinLock
    tad_dataToTransfer_prevSpinLock__: db


;; ----------------------------------------------
;; Queue 2 - The next song to load into Audio-RAM
;; ----------------------------------------------
    ;; The next song to load into Audio-RAM
    ;; Used by the `WAITING_FOR_LOADER` state
    ;; If this value is 0 or an invalid song, a blank silent song will be loaded instead.
    tad_nextSong__: db


;; ------------------------------------------------------
;; Queue 3 - The next command to send to the audio driver
;; ------------------------------------------------------
    ;; The next `Command` to send to the audio driver.
    ;; MUST NOT be PLAY_SOUND_EFFECT_COMMAND.
    ;; If this value is negative, the queue is empty.
    tad_nextCommand_id__: db

    ;; The parameter of the next next command (if any)
    tad_nextCommand_parameter__: db


;; ---------------------------------------
;; Queue 4 - The next sound effect to play
;; ---------------------------------------
    ;; The sound effect to play next.
    ;;
    ;; Lower sound effect indexes take priority over higher sound effect indexes
    ;; (as defined by the project file sound effect export order).
    ;;
    ;; If `tad_sfxQueue == $ff`, then the queue is considered empty.
    tad_sfxQueue__ db

    ;; The pan value for the next sound effect to play.
    tad_sfxQueue_pan__: db
.ends


;; =======================
;; loadAudioData variables
;; =======================

;; This MUST MATCH `struct tad_audioData` in `tad-audio.h`
.struct Tad_AudioData
    data    ds 4
    size    dw
.endst

.ramsection "loadAudioData_out" bank $7e SLOT 2
    ;; The output of the `loadAudioData` callback.
    loadAudioData_out INSTANCEOF Tad_AudioData
.ends

