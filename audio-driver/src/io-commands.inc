; S-CPU/SPC700 communication protocol definitions
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


; This constant MUST be increased if `LOADER_ADDR` or the IO Communication protocol changes.
TAD_IO_VERSION = 23


; Global ports
; ============
;
; These IO ports are used by both the loader and audio-driver.


; Loader spinlock and switch to loader command.
;
; 65816 to spc700:  This port serves multiple purposes:
;  1. (audio-driver and loader): Switch to the loader signal.
;     * If the audio driver is running: When bit 5 is set, the audio driver will
;       stop and execute the loader.
;     * If the loader is running: When bits 5 & 7 are set, the loader will restart.
;  2. Synchronise the start of the loader
;     * The port is set to "R" when the loader starts.
;     * The 65816 writes "R" to this port when initialising the loader
;     * The 65816 will wait until this port is 0 before sending data to the loader.
;  3. (loader only): Spinlock to synchronise loader transfers
;     * The loader will wait until the `TadIO__LOADER_PORT`'s value changes with
;      bit 7 clear before processing the next transfer.
;     * After the data has been read the value will be echoed to the 65816 over
;       `APUIO0` tell the 65816 S-CPU that IO ports 1-3 can be safely written to.
;  4. (loader only): Signal the end of transfer (bit 7 set and bit 5 clear).
;     * No data is written to audio-RAM.
;
; spc700 to 65816: This port serves 1 purpose:
;  1. The value that was written to this port is echoed by the spc700 to signal
;     to the 65816 that IO ports 1-3 can be safely written to.
;
; See: The *Loader Commands* section for more details of the spinlock signalling.
;
; Data format:
;      eslscccc
;          e    = end of transfer.
;          s    = Must only be set during the LoaderDataType loader initialisation
;          l    = switch to loader bit.
;          cccc = changing 4 bit spinlock value
TadIO__LOADER_PORT = CPUIO0
    TadIO__LOADER_PORT__END_OF_TRANSFER_BIT = 7
    TadIO__LOADER_PORT__SWITCH_TO_LOADER_BIT = 5

    TadIO__LOADER_PORT__END_OF_TRANSFER = $80
    TadIO__LOADER_PORT__SWITCH_TO_LOADER = $a0

    TadIO__LOADER_PORT__SPINLOCK_INIT_VALUE  = 0

    .assert LoaderIO__LOADER_READY_L & (1 << 6) != 0
    .assert LoaderIO__LOADER_READY_L & (1 << 4) != 0
    .assert LoaderIO__LOADER_READY_L & TadIO__LOADER_PORT__END_OF_TRANSFER == 0
    .assert LoaderIO__LOADER_READY_L & TadIO__LOADER_PORT__SWITCH_TO_LOADER == 0


; The mode the spc700 is currently executing (from spc700 to 65816)
;
; Used by both the loader and the audio-driver.
;
; NOTE: The IPL sets this value after at has cleared the zero-page.
;       Do not read this value immediately after reset.
;       Make sure enough time has passed for the IPL to set IO Port 1
;       to $bb before reading this port.
TadIO__MODE_PORT = CPUIO1
    ; The spc700 is running the IPL
    TadIO__MODE_PORT__IPL = $bb

    ; The S-SMP is running the loader.
    TadIO__MODE_PORT__LOADER = $4c ; 'L'

    ; The S-SMP is running the audio-driver.
    TadIO__MODE_PORT__AUDIO_DRIVER = $61 ; 'a'



; Loader Commands
; ===============

; LoaderDataType
; --------------
    ; Common audio data.
    ; Contains samples, pitch table and sound effects.
    ; MUST be loaded before the song data.
    LoaderDataType__COMMON_DATA = 0


    ; If this bit is clear, the LoaderDataType is common-audio-data.
    ; If this bit is set, the LoaderDataType is song-data.
    ;
    ; CAUTION: This is is also used for the RELOAD_COMMON_DATA flag in the 65816 APIs.
    LoaderDataType__SONG_DATA_BIT = 7

    ; If this bit is set, the song will play after the echo buffer has been cleared.
    ; If this bit is clear, the audio driver will start in a paused state.
    ;
    ; MUST NOT be set when loading code or common-audio-data.
    LoaderDataType__PLAY_SONG_BIT = 6

    ; If this bit is set, the global music and sound-effects volumes will be
    ; reset to the maximum volume when the song starts.
    LoaderDataType__RESET_GLOBAL_VOLUMES_BIT = 5


    ; If this bit is set, the song will be played in stereo.
    ; If this bit is clear, the song will be played in mono.
    ;
    ; MUST NOT be set when loading code or common-audio-data.
    LoaderDataType__STEREO_FLAG_BIT = 1

    ; If this bit is set, the left and right channels can be individually inverted.
    ; If this bit is clear, the left and right channels are only inverted if the mono invert flag is set.
    ;
    ; OPTIMISATION: Should be set if in MONO mode (STEREO_FLAG_BIT is clear).
    ;
    ; MUST NOT be set when loading code or common-audio-data.
    LoaderDataType__SURROUND_FLAG_BIT = 0



; Loader IO
; =========
    ; The loader ready signal.
    LoaderIO__LOADER_READY_L = %01010010 ; 'R'
    LoaderIO__LOADER_READY_H = %01001100 ; 'L'
        .assert LoaderIO__LOADER_READY_L & TadIO__LOADER_PORT__SWITCH_TO_LOADER == 0

    LoaderIO__READY_PORT_L = TadIO__LOADER_PORT
    LoaderIO__READY_PORT_H = TadIO__MODE_PORT

    LoaderIO__DATA_TYPE_PORT = CPUIO2

    LoaderIO__DATA_PORT_0 = CPUIO1
    LoaderIO__DATA_PORT_1 = CPUIO2
    LoaderIO__DATA_PORT_2 = CPUIO3


    ; Loader protocol
    ;
    ; Step 1: Ready signal is sent
    ;   * spc700 writes `LOADER_READY` to `READY_PORT_L` and `READY_PORT_H`
    ;   * 65816 waits until `READY_PORT_L` & `READY_PORT_H` == `LOADER_READY`
    ;
    ; Step 2: 65816 sends a `LoaderDataType` message.
    ;   * The 65816 MUST wait until the ready ports match `LOADER_READY`
    ;   * The 65816 writes the following in this order, 1 byte at a time:
    ;      1. Writes the `LoaderDataType` to `LoaderIO__DATA_TYPE_PORT`
    ;      2. Writes `LOADER_READY_L` to `READY_PORT_L`
    ;      3. Writes `LOADER_READY_H` to `READY_PORT_H`
    ;  * The spc700 waits until the `READY_PORT`s == `LOADER_READY`
    ;  * The spc700 initialises the loader transfer
    ;  * The spc700 writes `SPINLOCK_INIT_VALUE` to `TadIO__LOADER_PORT`.
    ;    The spinlock value is set to `TadIO__LOADER_PORT`.
    ;
    ; Step 3: The 65816 tells the loader how many transfers are required.
    ;  * The 65816 waits until `TadIO__LOADER_PORT` == spinlock value.
    ;  * The 65816 writes number of transfers (`data size / 3`) to `DATA_PORT_0`, `DATA_PORT_1`.
    ;  * The 65816 writes a new spinlock value to `TadIO__LOADER_PORT`.
    ;    The lower 4 bits of the spinlock MUST be clear.
    ;  * The spc700 waits until `TadIO__LOADER_PORT` has changed
    ;  * The spc700 updates loader addresses
    ;  * The spc700 echos the spinlock to `TadIO__LOADER_PORT`.
    ;
    ; Step 4: Data is transferred, 3 bytes at a time
    ;  * The data is interleaved.
    ;     * `DATA_PORT_0` is written to `addr + i`
    ;     * `DATA_PORT_1` is written to `addr + i + nTransfers`
    ;     * `DATA_PORT_2` is written to `addr + i + nTransfers * 2`
    ;  * The 65816 waits until `TadIO__LOADER_PORT` == spinlock value.
    ;  * The 65816 writes the data to `DATA_PORT_0`, `DATA_PORT_1` and `DATA_PORT_2`.
    ;  * The 65816 writes a different spinlock value to `TadIO__LOADER_PORT`.
    ;    The lower 4 bits of the spinlock MUST be clear.
    ;  * The spc700 waits until `TadIO__LOADER_PORT` has changed
    ;  * The spc700 reads the data bytes and copies it into Audio-RAM
    ;  * The spc700 will echo the spinlock value to `TadIO__LOADER_PORT`
    ;
    ; Step 4 is repeated until all data is written
    ;  * The number of step 4 transfers MUST EQUAL the value sent in Step 3.
    ;
    ; Step 5: An end-of-transfer signal is sent to the loader
    ;  * The 65816 waits until `TadIO__LOADER_PORT` == spinlock value.
    ;  * The 65816 writes `TadIO__LOADER_PORT__END_OF_TRANSFER` to `TadIO__LOADER_PORT`
    ;  * The spc700 notices the `TadIO__LOADER_PORT` MSB is set and ends the transfer.
    ;  * If `LoaderDataType` is song, the loader will switch to audio-driver mode,
    ;    otherwise the loader restarts.



; To Driver IO Commands
; =====================

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
    DriverIO__COMMAND_PORT = CPUIO3

    DriverIO__N_COMMANDS = 11
    DriverIO__COMMAND_MASK   = %00011110
    DriverIO__COMMAND_I_MASK = %11100001

    ; The two command parameter bytes
    DriverIO__PARAMETER_0_PORT = CPUIO1
    DriverIO__PARAMETER_1_PORT = CPUIO2


    ; Audio driver command acknowledgment.
    ;
    ; Acknowledgment of the `DriverIO__COMMAND_PORT` byte.  Not used in the loader.
    ;
    ; After the command has been processed, the `DriverIO__COMMAND_PORT` value
    ; will be written to this port.
    DriverIO__COMMAND_ACK_PORT = CPUIO3


    ; Writing `TadIO__LOADER_PORT__SWITCH_TO_LOADER` to `TadIO__LOADER_PORT`
    ; will stop the audio driver and execute the loader.
    ;
    ; If the audio-driver is running; if the `DriverIO__SWITCH_TO_LOADER_BIT` is set
    ; the audio driver will stop and execute the loader.
    ;
    ; If the loader is in the middle of a transfer and both the `DriverIO__SWITCH_TO_LOADER_BIT`
    ; and MSB (bit 7) bits are set, the loader will restart.


; IO Commands
; ===========

IoCommands__PAUSE = 0

; MUST match IoCommands in API and rust
.ftdef IoCommands
    ; Pause the song and sound effect channels.
    ; * Commands will still be executed when the audio-driver is paused.
    ; * The audio driver starts paused unless the `LoaderDataType__PLAY_SONG_BIT` is set.
    cmd__pause ; CMD 0

    ; Pauses the music channels, sound effects will continue to play.
    ; Unpauses the sound effects channels.
    cmd__pause_music_play_sfx ; CMD 2

    ; Unpauses the music and sound effect channels.
    cmd__unpause ; CMD 4

    ; Play a sound effect
    ;  * parameter0: sound effect id
    ;  * parameter1: pan (0-128) (not bounds checked)
    ;
    ; The sound-effect might not play if both sound effect channels are active.
    ; See the Sound Effects documentation for more details
    cmd__play_sound_effect ; CMD 6

    ; Stop all active sound effects
    cmd__stop_sound_effects ; CMD 8

    ; Set the main volume (`MVOL` register)
    ;  * parameter0: signed i8 volume
    ;
    ; NOTE: The main volume is reset whenever a new song is loaded.
    ;
    ; CAUTION: Echo volume is separate from main volume.
    ; The echo buffer will be unchanged and audible when main-volume is 0.
    ;
    ; CAUTION: The song can change the main volume.
    cmd__set_main_volume ; CMD 10

    ; Enables or disable music channels.
    ;  * parameter0: A bitmask of the 8 music channels that can send key-on events
    ;
    ; Disabled channels will be keyed-off.
    ;
    ; NOTE: The music channels bitmask is reset whenever a new song is loaded.
    cmd__set_music_channels ; CMD 12

    ; Set the song tick clock.
    ;  * parameter0: The new S-DSP TIMER_0 register value.
    ;    64 is fastest, 256 is slow, 0 is slowest.
    ;    (MUST be `0 || >= MIN_TICK_CLOCK 64`, is bounds checked)
    ;
    ; NOTE: The song can still change the song timer.
    cmd__set_song_timer ; CMD 14

    ; Set the global music volume
    ;  * parameter0: the music volume.
    ;    A 255 music-volume will not modify the channel volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd__set_global_music_volume ; CMD 16

    ; Set the global sound-effect volume
    ;  * parameter0: the sound-effect volume.
    ;    A 255 sfx-volume will not modify the channel volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd__set_sfx_volume ; CMD 18

    ; Set the global music and sound-effect volumes
    ;  * parameter0: the music volume.
    ;  * parameter1: the sound-effect volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd__set_global_volumes ; CMD 20
.endftdef

