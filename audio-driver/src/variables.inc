; Terrific Audio Driver variables and constants
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



; Must wait a minimum of 240ms after setting `EDL` and `ESA` before reading/writing the echo buffer.
;
; Added a little extra to be safe.
EDL_SLEEP_MS = 250

; Instead of setting TIMER_1 register to 8 and counting `EDL_SLEEP_MS` clocks, we can set TIMER_1 to `EDL_SLEEP_MS` and wait 8 clocks.
EDL_SLEEP_TIMER_1 = EDL_SLEEP_MS
EDL_SLEEP_COUNT   = TIMER_1_HZ / 1000

; Assert EDL_SLEEP_COUNT < 12


I8_MIN = -128
I8_MAX = 127


; Variables
; =========

FIRST_MUSIC_CHANNEL = 0
LAST_MUSIC_CHANNEL = N_MUSIC_CHANNELS - 1
FIRST_SFX_CHANNEL = LAST_MUSIC_CHANNEL + 1
LAST_SFX_CHANNEL = N_CHANNELS - 1


; Using separate values for slide and triangle to simplify the rust bytecode interpreter.
; See: `channelSoA_volEffect_direction`
VOL_PAN_EFFECT_SLIDE_UP      = $80
VOL_PAN_EFFECT_SLIDE_DOWN    = $81
VOL_PAN_EFFECT_TRIANGLE_UP   = $40
VOL_PAN_EFFECT_TRIANGLE_DOWN = $41


; Variables used by loader or used across song loads.
;
; They must not be cleared by the audio-driver.
.vars loader_zeropage
    ; Global volumes.
    ;
    ; Stored here so they are unchanged when a new song or common-audio-data is loaded.
    ;
    ; If global-volume is 0, the channel volume is unchanged.
    globalVolume_music : u8
    globalVolume_sfx : u8

    ; The address of the song data
    songPtr :  ptr

    ; The `LoaderDataType` value sent to the loader.
    ;
    ; Used as a Mono/Stereo flag.
    ;
    ; Set by the loader:
    ;   Advantages: Simpler and uses less code space (no IoCommand for to set/clear stereoFlag).
    ;   Disadvantages: Stereo flag is not set until a song is loaded.
    loaderDataType : u8
.endvars


.vars zeropage
    ; Temporary variables
    zpTmpWord : u16
        zpTmp  = zpTmpWord
        zpTmp2 = zpTmpWord + 1
    zpTmp3 : u8


    ; Increases once every song tick
    songTickCounter : u16


    ; Enabled or disabled music channels.
    ; A bitmask of the music channels that can send key-on events.
    ;
    ; This bitmask is set by the SET_MUSIC_CHANNELS IO comamnd, it is not `keyOnMask`.
    io_musicChannelsMask : u8


    ; The table of subroutine offsets
    ; (2x pointer to low-byte & high-byte tables)
    subroutineTable_l : ptr
    subroutineTable_h : ptr


    ; Lag detector
    ;
    ; The lag bit is set if the timer is non-zero on first timer counter read after processing.
    ;
    ; It's not a good lag detector, but it is better than the previous one.
    ;
    ; tad-gui will clear the LAG bits and turn it into a counter.
    lagDetector : u8
        LD_MUSIC_LAG_BIT = 0
        LD_SFX_LAG_BIT = 1

        LD_MUSIC_TEST_BIT = 6
        LD_SFX_TEST_BIT = 7


    ; bitmask - Set if S-DSP voice channel is music, clear if sfx.
    ; Used to disable music channel S-DSP writes that are used by sound effects.
    ; Bits 0-5 MUST be set.
    ; MUST ONLY be modified by `process_sfx_channels`.
    musicSfxChannelMask : u8

    ; bitmask - Set if S-DSP voice channel is sfx, clear if music
    ;           (opposite to `musicSfxChannelMask`).
    ; Used to enabled/disable noise and echo when sound effects channels are active.
    ; MUST ONLY be modified by `process_sfx_channels`.
    sfxMusicEchoNoiseMask : u8

    ; bitmask - Set if S-DSP voice channel is sfx.
    ; Used to disable S-DSP voice register writes for unused sfx channels.
    ; Used to determine the next available sound effect channel.
    activeSoundEffects : u8

    ; The `activeSoundEffects` value in the previous sfx tick.
    prevActiveSoundEffects : u8

    ; bitmask - Set if the sound effect is interruptible
    ; (only uses bits 6 & 7)
    sfx_interruptibleFlags : u8


    ; The remaining duration of the two sound effects in ticks
    sfx_remainingTicks : [u16 : N_SFX_CHANNELS]
        sfx_remainingTicks__ELEMENT_SIZE = 2

    ; Used to detect active one-channel sound effects.
    ; This value MUST BE $ff if the channel is disabled or the *one-channel* flag is clear.
    sfx_oneChannelSfxId : [u8 : N_SFX_CHANNELS]
        sfx_oneChannelSfxId__ELEMENT_SIZE = 1


    ; S-DSP voice channels dirty flags.
    ; Used to test if the virtualChannels should be written to the S-DSP voice registers.
    ; (using a bitset so they can be masked when channel ducking)
    voiceChannelsDirty_music : u8
    voiceChannelsDirty_sfx   : u8


    ; Shadow variables for KOFF DSP register
    keyOffShadow_music : u8
    keyOffShadow_sfx : u8

    ; Shadow variables for KON DSP register
    keyOnShadow_music : u8
    keyOnShadow_sfx : u8

    ; Shadow variable for the PMON S-DSP register
    pmonShadow : u8

    ; Shadow variable for the EON S-DSP register
    eonShadow_music : u8
    eonShadow_sfx : u8


    ; `keyOnShadow` mask.
    ;
    ; Each bit determines the channel bytecode should emit a key-on event.
    ;  * If set - the next play note instruction will set `keyOnShadow`.
    ;  * If clear - the channel is playing something and the play note instruction will
    ;               leave `keyOnShadow` unchanged (slurring the note).
    ; (8x bit array)
    keyOnMask_music : u8
    keyOnMask_sfx : u8


    ; Instruction pointer for the current channel.
    ; Only valid inside bytecode instructions.
    ; (pointer to bytecode instructions)
    instructionPtr : ptr


    ; Temporary variable for reading and writing `voiceChannelsDirty_*` bits.
    voiceChannelsDirty_tmp  : u8


    ; Temporary variable holding `globalVolume_music` or `globalVolume_sfx`
    globalVolume_tmp : u8


    ; Counter (in ticks) until the event
    ; (high byte in lastvarpage)
    channelSoA_countdownTimer_l : [u8 : N_CHANNELS]

    ; When negative the channel will key-off when early-release is activated
    ; or countdownTimer == 1.
    ;
    ; This flag is reset on KeyOff.
    ;
    ; NOTE: Only the MSB of this variable is tested
    channelSoA_keyoffMsbFlag : [u8 : N_CHANNELS]


    ; S-DSP voice register shadows (AKA Virtual Channels)
        channelSoA_virtualChannels_vol_l       : [u8 : N_CHANNELS]
        channelSoA_virtualChannels_vol_r       : [u8 : N_CHANNELS]
        channelSoA_virtualChannels_pitch_l     : [u8 : N_CHANNELS]
        channelSoA_virtualChannels_pitch_h     : [u8 : N_CHANNELS]
        channelSoA_virtualChannels_srcn        : [u8 : N_CHANNELS]
        channelSoA_virtualChannels_adsr1       : [u8 : N_CHANNELS]
        ; If the MSB of adsr1 is set, this value is written to the ADSR2 register.
        ; If the MSB of adsr1 is clear, this value is written to the GAIN register.
        channelSoA_virtualChannels_adsr2OrGain : [u8 : N_CHANNELS]

        ; Temporary GAIN
        ;
        ; If this variable is non-zero, it overrides `adsr1` and `adsr2OrGain`.
        ; Reset to 0 on key-off.
        channelSoA_virtualChannels_tempGain : [u8 : N_CHANNELS]


    ; The address of the next bytecode to execute.
    ;
    ; If the high-byte of the instructionPtr is 0, no bytecode will be executed.
    ;
    ; (The low-byte of instructionPtr is in lastvarpage)
    channelSoA_instructionPtr_h : [u8 : N_CHANNELS]


    ; The stack pointer to use in the `skip_last_loop_*` and `end_loop` instructions.
    ;
    ; Used to skip bounds checking when reading/modifying counter.
    ;
    ; CAUTION: Not equal to `stackPointer` if there is less than 2 or fewer bytes on the stack.
    ;
    ; MUST always be >= `BcStackIndexesEnd[x]` and <= `(BcStackIndexesStart[x] - 3)`
    ;
    ; (index into `bcStack`)
    channelSoA_loopStackPointer : [u8 : N_CHANNELS]


    ; Portamento direction
    ;  * zero - portamento disabled
    ;  *  > 0 - increase pitch
    ;  *  < 0 - decrease pitch
    channelSoA_portamento_direction : [i8 : N_CHANNELS]



    ; Vibrato PITCH offset per tick.
    ;
    ; If this value is 0, vibrato is disabled.
    ;
    ; (pitch change/tick)
    channelSoA_vibrato_pitchOffsetPerTick : [u8 : N_CHANNELS]

    ; Vibrato direction.
    ; The lsb controls the vibrato direction.
    channelSoA_vibrato_direction : [u8 : N_CHANNELS]


    ; Used to determine if vol_l and vol_r need recalculating.
    ; In the `process_channels` loop, bit 7 is left-shifted out and the volume is updated if set.
    ; (bit array)
    volShadowDirty_tmp : u8

    ; Used to determine if all music channels should be updated.
    ; Set in the SET_GLOBAL_MUSIC_VOLUME IO command.
    ;
    ; Must only contain $00 or $ff.
    ;
    ; CAUTION: These bits are in reverse order, compared to other *_music bit-arrays.
    volShadowDirty_music : u8

    ; The sound effects with invalid vol_l and vol_r shadow variables.
    ; Set in the PLAY_SOUND_EFFECT and SET_GLOBAL_SFX_VOLUME IO commands.
    ; CAUTION: bits 6 & 7 are swapped from other the other *_sfx bitsets
    ; (bit array)
    volShadowDirty_sfx : u8


    ; Controls which channel has ownership if the noise frequency
    ;
    ; VALUES:
    ;   * 0    = music channels
    ;   * $80 = sfx channel 9
    ;   * $40 = sfx channel 8
    ;
    ; This variable MUST not have bits 6 & 7 set at the same time
    ;
    ; To simplify timing, this variable is set at the start of `process_sfx_channels`.
    noiseLock : u8

    ; The last SFX voice-bit that encountered a `play_noise` instruction.
    ;
    ; VALUES:
    ;   * $80 = sfx channel 9
    ;   * $40 = sfx channel 8
    ;
    ; This variable MUST not have bits 6 & 7 set at the same time
    lastSfxPlayNoiseChannel : u8

    ; noise frequency register shadow variables
    ;
    ; These variables MUST ALWAYS be `<= DSP_FLG__NOISE_FREQ_MASK`.
    noiseFreq_music : u8
    noiseFreq_sfx : [u8: N_SFX_CHANNELS]


    ; NON DSP register shadow variables.
    ;
    ; Must only be edited by `process_*_channels`.
    ; Edit `pendingNon_*` instead.
    nonShadow_music : u8
    nonShadow_sfx : u8


; --- All zeropage variables after this point are not cleared in `main` ---
__EndZeropageClearAddr = nonShadow_sfx + 1


    ; The maximum echo delay in the song.
    ; Used to determine the size and location of the echo buffer in Audio-RAM.
    maxEdl : u8


    ; Used to determine if the S-DSP main-volume and echo registers need updating.
    ;
    ; (bit field)
    ;
    ; CAUTION: Also used to temporally store SongHeader.activeMusicChannels in the start of `Main`
    songGlobalsDirty : u8
        SONG_GLOBALS_DIRTY__FIR_FILTER_BIT = 7
        SONG_GLOBALS_DIRTY__CLEAR_FIR_BIT = 0   ; Clear FIR filter before writing FIR filter
        SONG_GLOBALS_DIRTY__ECHO_VOLUME_BIT = 6

        ; Not tested in `process_song_globals`
        ; Instead they will be written whenever songGlobalsDirty is non-zero
        SONG_GLOBALS_DIRTY__EFB_OR_MVOL_BIT = 5
        SONG_GLOBALS_DIRTY__EDL_BIT = 4


    ; Global song variables (Echo buffer settings)
    ;
    ; MUST be last, used to determine when zeropage is cleared.
    songGlobals : GlobalSongSettings


    ; Common audio data pointers.
    ; See `audio-driver/src/data-formats.inc` for more details about these variables.
    ;
    ; Also used to temporally store SongHeader fields after `SongHeader.globals` in the start of `Main`
    commonData : CommonDataPointers
.endvars


.vars firstpage
    ; Pending NON shadow variable changes.
    ;
    ; This value will be written to `nonShadow_*` at the start of the next tick.
    ;
    ; Delaying `pendingNon_*` NON writes fixes a bug where NON is set in the middle of the release envelope.
    pendingNon_music : u8
    pendingNon_sfx : u8

    ; Cached SFX mutedChannels parameter for `process_channels`.
    sfxMutedChannels : u8


; Channel variables that do not fit in zeropage

    ; Delay before vibrato is applied to the current note (using `ticksAfterNote`).
    ; (ticks)
    channelSoA_vibrato_delay : [u8 : N_CHANNELS]

    ; Countdown for vibrato direction change
    ; (ticks)
    channelSoA_vibrato_tickCounter : [u8 : N_CHANNELS]

    ; The vibrato tickCounter to reset to when changing direction (`quarterWavelengthInTicks*2`).
    channelSoA_vibrato_halfWavelength : [u8 : N_CHANNELS]

    ; The value to reset `vibrato_tickCounter` counter to when playing a note or starting a vibrato.
    ;
    ; This variable is equal to the `quarterWavelengthInTicks` parameter of the `set_vibrato` instruction
    ;
    ; Offsetting the start of the vibrato saves a comparison in `process_vibrato`.
    channelSoA_vibrato_tickCounterStart : [u8 : N_CHANNELS]


    ; i16 VxPITCH offset added to every play_note or portamento instruction
    channelSoA_detune_l : [u8 : N_CHANNELS]
    channelSoA_detune_h : [u8 : N_CHANNELS]


    ; Channel invert flags:
    ;      rl0000m0
    ;      r = invert right channel
    ;      l = invert left channel
    ;      m = invert both channels in mono mode
    channelSoA_invertFlags : [u8 : N_CHANNELS]


    ; Channel volume (0-255)
    channelSoA_volume : [u8 : N_CHANNELS]

    ; Fractional volume (used for volume effects)
    channelSoA_subVolume : [u8 : N_CHANNELS]

    ; Volume effect direction.
    ;  * zero - volume effect disabled
    ;  * bit 0 clear - increment volume
    ;  * bit 0 set - decrement volume
    channelSoA_volEffect_direction : [u8 : N_CHANNELS]

    ; Countdown ticks for volume effects
    ; (ticks)
    channelSoA_volEffect_counter : [u8 : N_CHANNELS]

    ; Volume offset per tick
    ; (8.8 unsigned fixed point)
    channelSoA_volEffect_offset_l : [u8 : N_CHANNELS]
    channelSoA_volEffect_offset_h : [u8 : N_CHANNELS]

    ; Tremolo half-wavelength in ticks.
    ; If 0, the volume effect is a volume slide.
    channelSoA_volEffect_halfWavelength : [u8 : N_CHANNELS]


    ; Channel pan (0-MAX_PAN)
    channelSoA_pan : [u8 : N_CHANNELS]

    ; See volEffect variables
    channelSoA_subPan : [u8 : N_CHANNELS]
    channelSoA_panEffect_direction : [u8 : N_CHANNELS]
    channelSoA_panEffect_counter : [u8 : N_CHANNELS]
    channelSoA_panEffect_offset_l : [u8 : N_CHANNELS]
    channelSoA_panEffect_offset_h : [u8 : N_CHANNELS]
    channelSoA_panEffect_halfWavelength : [u8 : N_CHANNELS]

    ; Early release comparison argument
    ; (the other 2 arguments are in lastvarpage)
    channelSoA_earlyRelease_cmp : [u8 : N_CHANNELS]


    ; The previous IO port command
    previousCommand : u8
.endvars


.vars lastvarpage
    ; Counter (in ticks) until the event
    ; (The low-byte is in zeropage)
    channelSoA_countdownTimer_h : [u8 : N_CHANNELS]

    ; Semitone offset for all play-note and portamento-note instructions.
    channelSoA_transpose : [u8 : N_CHANNELS]


    ; set_early_release instruction arguments
    channelSoA_earlyRelease_minTicks : [u8 : N_CHANNELS]
    channelSoA_earlyRelease_gain : [u8 : N_CHANNELS]


    ; The address of the next bytecode to execute
    ; (The high-byte of instructionPtr is in zeropage)
    channelSoA_instructionPtr_l : [u8 : N_CHANNELS]


    ; Portamento PITCH speed
    ; (pitch change/tick)
    channelSoA_portamento_speed : [u8 : N_CHANNELS]

    ; Portamento target pitch
    channelSoA_portamento_target_l : [u8 : N_CHANNELS]
    channelSoA_portamento_target_h : [u8 : N_CHANNELS]


    ; Stack pointer.
    ; The stack grows downwards,
    ; from highest address (BcStackIndexesStart[x]) to lowest address (BcStackIndexesEnd[x])
    ;
    ; CAUTION: Is out of bounds when the stack is empty.
    ; CAUTION: `bcStack` label is offset by BC_CHANNEL_STACK_OFFSET.
    ;
    ; MUST always be >= `BcStackIndexesEnd[x]` and <= `(BcStackIndexesStart[x]`
    ;
    ; (index into `bcStack`)
    channelSoA_stackPointer : [u8 : N_CHANNELS]


    ; The previous temp-GAIN used by the channel.
    channelSoA_prevTempGain : [u8 : N_CHANNELS]

    ; Number of ticks since the last play_note or portamento instruction.
    ; ::MAYDO move to zeropage::
    channelSoA_ticksAfterNote : [u8 : N_CHANNELS]



    ; The bytecode stack
    ;
    ; CAUTION: `bcStack` is offset by BC_CHANNEL_STACK_OFFSET to eliminate signed comparisons.
    __bcStack : [u8 : BC_CHANNEL_STACK_SIZE * N_CHANNELS]
        bcStack = __bcStack - BC_CHANNEL_STACK_OFFSET
.endvars


; common audio data is stored at a fixed address in Audio RAM
.varbank CAD COMMON_DATA_ADDR..COMMON_DATA_ADDR+COMMON_DATA_HEADER_SIZE+1
.vars CAD
    commonDataHeader : CommonDataHeader

    ; A label to the first byte of the BRR directory
    brrDirectory : u8
.endvars

