; tad-constants.inc
; =================
;
; Terrific Audio Driver 65816 API public constants.
;
;
; This file is is an asar port from the Terrific Audio Driver bass API.
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


; ================
; Public Constants
; ================

; MUST match `audio-driver/src/io-commands.wiz`
; enum TadCommand
    !TadCommand_PAUSE = 0
    !TadCommand_PAUSE_MUSIC_PLAY_SFX = 2
    !TadCommand_UNPAUSE = 4
    !TadCommand_PLAY_SOUND_EFFECT = 6
    !TadCommand_STOP_SOUND_EFFECTS = 8
    !TadCommand_SET_MAIN_VOLUME = 10
    !TadCommand_SET_MUSIC_CHANNELS = 12
    !TadCommand_SET_SONG_TIMER = 14
    !TadCommand_SET_GLOBAL_MUSIC_VOLUME = 16
    !TadCommand_SET_GLOBAL_SFX_VOLUME = 18
    !TadCommand_SET_GLOBAL_VOLUMES = 20


; Maximum pan value (100% to the right)
!TAD_MAX_PAN = 128

; Center pan value (default if pan is not specified)
!TAD_CENTER_PAN = !TAD_MAX_PAN/2

; Minimum tick clock value for the `TadCommand::SET_SONG_TIMER` command.
!TAD_MIN_TICK_CLOCK = 64



; TadFlags namespace
    !TadFlags_RELOAD_COMMON_AUDIO_DATA = $80
    !TadFlags_PLAY_SONG_IMMEDIATELY = $40
    !TadFlags_RESET_GLOBAL_VOLUMES_ON_SONG_START = $20

    ; A mask for the flags that are sent to the loader
    !TadFlags__ALL_FLAGS = $e0


; enum TadAudioMode
    !TadAudioMode_MONO = 0
    !TadAudioMode_STEREO = 1
    !TadAudioMode_SURROUND = 2

!TAD_N_AUDIO_MODES = 3


; Default values
; ==============

if not(defined("TAD_DEFAULT_FLAGS"))
    ; Default TAD flags
    ; MUST NOT set RELOAD_COMMON_AUDIO_DATA
    !TAD_DEFAULT_FLAGS = !TadFlags_PLAY_SONG_IMMEDIATELY
endif

if not(defined("TAD_DEFAULT_AUDIO_MODE"))
    ; Starting audio mode
    !TAD_DEFAULT_AUDIO_MODE = !TadAudioMode_MONO
endif

if not(defined("TAD_DEFAULT_TRANSFER_PER_FRAME"))
    ; Default number of bytes to transfer to Audio-RAM per `Process` call.
    ;
    ; MUST be between !TAD_MIN_TRANSFER_PER_FRAME and !TAD_MAX_TRANSFER_PER_FRAME
    !TAD_DEFAULT_TRANSFER_PER_FRAME = 256
endif


; =================
; Private constants
; =================


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

