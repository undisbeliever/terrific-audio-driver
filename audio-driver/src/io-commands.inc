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
TAD_IO_VERSION = 20


; Loader Commands
; ===============

; LoaderDataType
; --------------
    ; The `audio-driver.bin` file.
    ; MUST be loaded first.
    LoaderDataType__CODE        = 0

    ; Common audio data.
    ; Contains samples, pitch table and sound effects.
    ; MUST be loaded after `LoaderDataType.CODE` and before song data.
    LoaderDataType__COMMON_DATA = 1


    ; If this bit is clear, the LoaderDataType is CODE (zero) or COMMON_DATA (non-zero).
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



; Loader IO Commands
; ==================

    ; Port 0 is MUST be unused.
    ; MUST only be used for `ToDriver.command` and `ToScpu.command_ack`.

    ; The loader ready signal.
    ;
    ; To prevent conflicts with `Transfer_ToLoader.spinlock` or `ToDriver.switch_to_loader`:
    ;   * bit 7 must be clear (end of transfer bit)
    ;   * bit 6 should be set (so `Transfer_ToLoader.spinlock is never LOADER_READY if the counter MASK is used)
    ;   * bit 5 must be clear (`ToDriver.SWITCH_TO_LOADER_BIT`)
    LoaderIO__LOADER_READY_L = %01001100 ; 'L'
    LoaderIO__LOADER_READY_H = %01000100 ; 'D'


    ; Step 1: S-CPU waits until ready signal is set by the loader
        ; Signal to the S-CPU that the loader is ready to receive data.
        ; MUST wait until `ready` matches `LOADER_READY` before writing `InitToDriver` data.
        LoaderIO_InitToScpu__READY_PORT_L = CPUIO2
        LoaderIO_InitToScpu__READY_PORT_H = CPUIO3

    ; Step 2: S-CPU writes the following to the IO ports
        ; The LoaderDataType to transfer to audio-RAM.
        ; MUST be written first
        LoaderIO_InitToLoader__DATA_TYPE_PORT = CPUIO1

        ; Signal to the loader that the S-CPU is ready to receive data.
        LoaderIO_InitToLoader__READY_PORT_L = CPUIO2
        LoaderIO_InitToLoader__READY_PORT_H = CPUIO3


    ; Step 3: S-CPU waits until `Init_ToDriver` data is acknowledged
    ;         by waiting until `data_type_ack` is zero.
        LoaderIO_InitAckToScpu__ACK_VALUE = 0
        LoaderIO_InitAckToScpu__ACK_PORT = CPUIO3

    ; Step 4: S-CPU writes the next word to transfer to Audio-RAM to the following IO ports:
        ; Should be written one byte at a time
        LoaderIO_TransferToLoader__DATA_PORT_L = CPUIO1
        LoaderIO_TransferToLoader__DATA_PORT_H = CPUIO2

        ; Write a different value to this port to signal the data is ready.
        ;
        ; MUST be written last.
        ;
        ; Data format:
        ;      e0l0cccc
        ;          e    = end of transfer.
        ;          l    = switch to loader bit
        ;          cccc = incrementing 4 bit counter.
        ;
        ; If the MSB is set, no data is written to the Audio-RAM and the transfer is complete.
        ;
        ; If the *switch-to-loader* and *end-of-transfer* bits are set, the loader will restart.
        LoaderIO_TransferToLoader__SPINLOCK_PORT = CPUIO3

        LoaderIO_TransferToLoader__SPINLOCK_MASK = $0f
        LoaderIO_TransferToLoader__SPINLOCK_COMPLETE = $80
        LoaderIO_TransferToLoader__SPINLOCK_SWITCH_TO_LOADER_BIT = 5


    ; Step 5: S-CPU waits until `Transfer_ToLoader` is acknowledged
    ;         by waiting until `spinlock_ack` == `spinlock`.
        LoaderIO_TransferToScpu__SPINLOCK_ACK_PORT = CPUIO3

    ; Step 6: If there is more data to transfer, goto step 4

    ; Step 7: The S-CPU writes `Transfer_ToLoader.SPINLOCK_COMPLETE` to `Transfer_ToLoader.spinlock`.

    ; Step 8: The S-CPU waits until `Transfer_ToScpu.spinlock_ack == Transfer_ToScpu.SPINLOCK_COMPLETE`

    ; Step 9: If `LoaderDataType` is not song goto Step 1.
    ;         If `LoaderDataType` is a song, the audio driver is executed..


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
    DriverIO__COMMAND_PORT = CPUIO0

    DriverIO__N_COMMANDS = 11
    DriverIO__COMMAND_MASK   = %00011110
    DriverIO__COMMAND_I_MASK = %11100001

    ; The two command parameter bytes
    DriverIO__PARAMETER_0_PORT = CPUIO1
    DriverIO__PARAMETER_1_PORT = CPUIO2


    ; Writing `SWITCH_TO_LOADER` to this port should stop execution and start the loader.
    ;
    ; If the audio-driver is running; if the `DriverIO__SWITCH_TO_LOADER_BIT` is set
    ; the audio driver will stop and execute the loader.
    ;
    ; If the loader is in the middle of a transfer and both the `DriverIO__SWITCH_TO_LOADER_BIT`
    ; and MSB (bit 7) bits are set, the loader will restart.
    DriverIO__SWITCH_TO_LOADER_PORT = CPUIO3
    DriverIO__SWITCH_TO_LOADER_BIT = 5


    ; Audio driver command acknowledgment.
    ;
    ; Acknowledgment of the `ToDriver.command` byte.  Not used in the loader.
    ;
    ; After the command has been processed, the `IO.ToDriver.command` value will be written to this port.
    DriverIO__COMMAND_ACK_PORT = CPUIO0


    ; The mode the S-SMP is currently executing.
    ;
    ; Used by both the loader and the audio-driver.
    ;
    ; NOTE: The IPL sets this value after at has cleared the zero-page.
    ;       Do not read this value immediately after reset.
    ;       Make sure enough time has passed for the IPL to set IO Port 1
    ;       to $bb before reading this port.
    DriverIO__MODE_PORT = CPUIO1

    ; The S-SMP is at the start of the IPL, waiting for the ready signal.
    DriverIO__MODE_IPL = $bb

    ; The S-SMP is running the loader.
    DriverIO__MODE_LOADER = $4c;         ; 'L', Loader.LOADER_READY_L

    ; The S-SMP is running the audio-driver.
    DriverIO__MODE_AUDIO_DRIVER = $61 ;  ; 'a'


; IO Commands
; ===========

; MUST match IoCommands in API and rust
.ftdef IoCommands
    ; Pause the song and sound effect channels.
    ; * Commands will still be executed when the audio-driver is paused.
    ; * The audio driver starts paused unless the `LoaderDataType__PLAY_SONG_BIT` is set.
    cmd_Pause ; CMD 0

    ; Pauses the music channels, sound effects will continue to play.
    ; Unpauses the sound effects channels.
    cmd_PauseMusicPlaySfx ; CMD 2

    ; Unpauses the music and sound effect channels.
    cmd_Unpause ; CMD 4

    ; Play a sound effect
    ;  * parameter0: sound effect id
    ;  * parameter1: pan (0-128) (not bounds checked)
    ;
    ; The sound-effect might not play if both sound effect channels are active.
    ; See the Sound Effects documentation for more details
    cmd_PlaySoundEffect ; CMD 6

    ; Stop all active sound effects
    cmd_StopSoundEffect ; CMD 8

    ; Set the main volume (`MVOL` register)
    ;  * parameter0: signed i8 volume
    ;
    ; NOTE: The main volume is reset whenever a new song is loaded.
    cmd_SetMainVolume ; CMD 10

    ; Enables or disable music channels.
    ;  * parameter0: A bitmask of the 8 music channels that can send key-on events
    ;
    ; Disabled channels will be keyed-off.
    ;
    ; NOTE: The music channels bitmask is reset whenever a new song is loaded.
    cmd_SetMusicChannels ; CMD 12

    ; Set the song tick clock.
    ;  * parameter0: The new S-DSP TIMER_0 register value.
    ;    64 is fastest, 256 is slow, 0 is slowest.
    ;    (MUST be `0 || >= MIN_TICK_CLOCK 64`, is bounds checked)
    ;
    ; NOTE: The song can still change the song timer.
    cmd_SetSongTimer ; CMD 14

    ; Set the global music volume
    ;  * parameter0: the music volume.
    ;    A 255 music-volume will not modify the channel volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd_SetGlobalMusicVolume ; CMD 16

    ; Set the global sound-effect volume
    ;  * parameter0: the sound-effect volume.
    ;    A 255 sfx-volume will not modify the channel volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd_SetGlobalSfxVolume ; CMD 18

    ; Set the global music and sound-effect volumes
    ;  * parameter0: the music volume.
    ;  * parameter1: the sound-effect volume.
    ;
    ; CAUTION: This command does not change the echo volume.
    ;
    ; NOTE: Global volumes persist after a new song or common-audio-data is loaded.
    cmd_SetGlobalVolumes ; CMD 20
.endftdef

