; Audio Driver
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
.include "data-formats.inc"
.include "io-commands.inc"
.include "common-memmap.inc"
.include "bytecode.inc"

.codebank CODE_ADDR..CODE_END_ADDR


; common audio data is stored at a fixed address in Audio RAM
.varbank CAD COMMON_DATA_ADDR..COMMON_DATA_ADDR+COMMON_DATA_HEADER_SIZE+1
.vars CAD
    commonDataHeader : CommonDataHeader

    ; A label to the first byte of the BRR directory
    brrDirectory : u8
.endvars
.assert brrDirectory - commonDataHeader == COMMON_DATA_HEADER_SIZE


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


.vars zeropage
    ; Temporary variables
    zpTmpWord : u16
        zpTmp  = zpTmpWord
        zpTmp2 = zpTmpWord + 1


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
        channelSoA_virtualChannels_scrn        : [u8 : N_CHANNELS]
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


    ; Used to determine if the S-DSP echo registers need updating.
    ;
    ; (bit field)
    ;
    ; CAUTION: Also used to temporally store SongHeader.activeMusicChannels in the start of `Main`
    echoDirty : u8
        ECHO_DIRTY__FIR_FILTER_BIT = 7
        ECHO_DIRTY__CLEAR_FIR_BIT = 0   ; Clear FIR filter before writing FIR filter
        ECHO_DIRTY__VOLUME_BIT = 6

        ; Not tested in `process_echo_registers`
        ; Instead they will be written whenever echoDirty is non-zero
        ECHO_DIRTY__FEEDBACK_BIT = 5
        ECHO_DIRTY__EDL_BIT = 4


    ; Echo buffer settings
    ;
    ; MUST be last, used to determine when zeropage is cleared.
    echo : EchoBufferSettings


    ; Common audio data pointers.
    ; See `audio-driver/src/data-formats.inc` for more details about these variables.
    ;
    ; Also used to temporally store SongHeader fields after `SongHeader.echo` in the start of `Main`
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

    ; Offset between the note to play and the pitch table.
    ;
    ; NOTE: Adding `instPitchOffset` to the note to play is allowed to an can overflow.
    channelSoA_instPitchOffset : [u8 : N_CHANNELS]

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


; Code
; ====

.proc main
    ; MUST be the first thing in the `code` bank (so the starting address is consistent across builds).
    .assert PC == CODE_ADDR

    mov X, #$ff
    mov SP, X
    clrp
.p0

    ; Tell the S-CPU the S-SMP is running the audio driver
    mov DriverIO__MODE_PORT, #DriverIO__MODE_AUDIO_DRIVER

    ; Reset the DSP
    mov A, #DSP_FLG
    mov Y, #DSP_FLG__SOFT_RESET | DSP_FLG__MUTE_ALL | DSP_FLG__ECHO_DISABLE
    movw DSPADDR, YA

    ; All keys off
    mov A, #DSP_KOFF
    mov Y, #$ff
    movw DSPADDR, YA


    ; Load song header into RAM.
    ; NOTE: If the song starts with a 0 byte, the song is blank and the header will be zeroed.

    ; Confirm `songHeader` will not be overridden by the zeropage clear
    .assert _songHeader > __EndZeropageClearAddr
    .assert _activeMusicChannels > __EndZeropageClearAddr
    .assert _songHeader + SONG_HEADER_SIZE < SHARED_ZEROPAGE_ADDR
    ; Confirm nSubroutines is the last field in the songHeader
    .assert offsetof(SongHeader, nSubroutines) + 1 == SONG_HEADER_SIZE

    _songHeader = echo - offsetof(SongHeader, echo)
    _activeMusicChannels = _songHeader + offsetof(SongHeader, activeMusicChannels)
    _songHeader_tickTimer = _songHeader + offsetof(SongHeader, tickTimer)
    _songHeader_nSubroutines = _songHeader + offsetof(SongHeader, nSubroutines)

    .assert offsetof(SongHeader, activeMusicChannels) == 0
    mov Y, #offsetof(SongHeader, activeMusicChannels)
    mov A, [songPtr] + Y
    mov _activeMusicChannels, A

    ; Copy SongHeader data to `_songHeader` if `_activeMusicChannels` is non-zero
    ; Otherwise, fill `_songHeader` with zeros.
    .assert offsetof(SongHeader, echo) == 1
    mov Y, #offsetof(SongHeader, echo)
    EchoCopyLoop:
        mov A, _activeMusicChannels
        beq SkipHeaderByte
            mov A, [songPtr] + Y
        SkipHeaderByte:

        mov _songHeader + Y, A

        inc Y
        cmp Y, #SONG_HEADER_SIZE
        bcc EchoCopyLoop


    ; Setup echo.
    ;
    ; This is done as soon as possible as we need to wait a while after setting EDL and ESA.
    ;
    ; Assumes ECHO write is disabled.

    ; Extract maximum EDL from EchoBufferSettings (high nibble of `echo.edl)
    ; and calculate echo start address
    ;
    ; maxEdl = (echo.edl >> 4) & $0f
    mov A, echo.edl
    xcn A
    and A, #$0f
    mov maxEdl, A

    ; a = $100 - a * (2048 / 256)
    ;
    ; Multiply by 8
    ; (XCN is safe, A has been masked)
    xcn A
    lsr A
    ; Negate A
    eor A, #$ff
    inc A

    bne     EdlNotZero
        ; Ensure echo buffer is at the end of audio-ram when EDL is 0.
        ; When EDL is 0, the echo buffer is 4 bytes in size.
        dec A
    EdlNotZero:

    ; set ESA (Echo start address??)
    mov DSPADDR, #DSP_ESA
    mov DSPDATA, A

    ; Save S-DSP ESA for later.
    push A


    ; Mask and clamp EDL
    mov A, echo.edl
    and A, #$0f
    cmp A, maxEdl
    bcc EdlOk
        mov A, maxEdl
    EdlOk:
    mov echo.edl, A

    ; Set EDL
    .assert DSP_EDL == DSP_ESA | (1 << 4)
    set1 DSPADDR, 4
    mov DSPDATA, A


    ; `EDL` does not take affect immediately.
    ; The S-DSP reads `EDL` when the echo buffer is full and wraps around.
    ;
    ; Since `EDL` starts in an uninitialised state, we must wait a minimum of 240ms before
    ; setting echo volume or enabling echo writes.
    ;
    ; Setup a timer here and enable echo writes at the end of driver initialization.
    ;
    ; Source: fullsnes https://problemkaputt.de/fullsnes.htm#snesapudspechoregisters

    ; Using timer1 as timer0 is setup before echo buffer clear.
    mov T1TARGET, #EDL_SLEEP_TIMER_1

    ; Reset and enable timer 0 (timers are reset on a transition from 0 to 1)
    mov CONTROL, #0
    mov CONTROL, #CONTROL__ENABLE_TIMER_1


    ; A copy of S-DSP `ESA` is on the stack.


    ; Disable echo
    mov Y, #0
    mov A, #DSP_EON
    movw DSPADDR, YA


    ; Set main volume to full volume
    mov Y, #$7f
    mov A, #DSP_MVOL_L
    movw DSPADDR, YA
    mov A, #DSP_MVOL_R
    movw DSPADDR, YA

    ; Set DSP directory table
    .assert lobyte(brrDirectory) == 0
    mov A, #DSP_DIR
    mov Y, #hibyte(brrDirectory)
    movw DSPADDR, YA


    ; Clear all zero-page variables before `__EndZeropageClearAddr` variables
    ; Assumes `songPtr` and `loaderDataType` is AFTER `zeropage` bank.
    mov A, #0
    mov X, #__EndZeropageClearAddr
    ZeropageClearLoop:
        dec X
        mov (X), A
        bne ZeropageClearLoop


    ; A = 0
    mov pendingNon_sfx, A
    mov pendingNon_music, A
    mov sfxMutedChannels, A

    ; Reset global volume if RESET_GLOBAL_VOLUMES_BIT flag is set
    bbc loaderDataType, LoaderDataType__RESET_GLOBAL_VOLUMES_BIT, NoResetGlobalVolume
        ; A = 0
        mov globalVolume_music, A
        mov globalVolume_sfx, A
    NoResetGlobalVolume:


    ; Setup music channels
    .assert N_MUSIC_CHANNELS == 8

    mov X, #N_MUSIC_CHANNELS - 1
    mov Y, #SONG_HEADER_SIZE
    SetupChannelsLoop:
        asl _activeMusicChannels
        bcc SkipChannel
            ; Calculate instructionPtr, `songPtr[y] + u16(songPtr)`
            ; and disable the channel if the addition overflows.

            mov A, [songPtr] + Y
            clrc
            adc A, songPtr
            mov channelSoA_instructionPtr_l + X, A
            inc Y

            mov A, [songPtr] + Y
            adc A, songPtr+1

            inc Y

            bcc ChannelOk
                ; Disable channel
                mov A, #0
        ChannelOk:
            mov channelSoA_instructionPtr_h + X, A
    SkipChannel:

        push Y

        mov A, #MAX_PAN / 2
        call __reset_channel

        pop Y

        dec X
        bpl SetupChannelsLoop

    ; Y = offset after songChannel addresses


    ; Calculate `subroutineTable` addresses.
    ; subroutineTable_l = songPtr + Y
    mov A, Y
    mov Y, #0
    addw YA, songPtr
    movw subroutineTable_l, YA

    ; subroutineTable_h = subroutineTable_l + _songHeader_nSubroutines
    mov A, _songHeader_nSubroutines
    mov Y, #0
    addw YA, subroutineTable_l
    movw subroutineTable_h, YA


    ; Setup TIMER 0
    mov T0TARGET, _songHeader_tickTimer


    ; No need to reset SFX channels.
    ; The sfx channels are disabled as `instructionPtr_h` is 0.


    ; Copy Common Audio Data header to zeropage
    mov X, #COMMON_DATA_POINTERS_SIZE - 1
    CopyCommonDataLoop:
        mov A, commonDataHeader.pointers + X
        mov commonData + X, A
        dec X
        bpl CopyCommonDataLoop


    ; If not in surround mode, set all `echo.invertFlags` bits to the mono-flag
    bbs loaderDataType, LoaderDataType__SURROUND_FLAG_BIT, IsSurround
        .assert INVERT_FLAGS__MONO & %1100_0001 == 0 ; Assert masking is more efficient than bpl or shifts
        mov A, echo.invertFlags
        and A, #INVERT_FLAGS__MONO
        beq NoMonoInvert
            mov A, #$ff
        NoMonoInvert:
        mov echo.invertFlags, A
    IsSurround:


    ; Reset bit-masks
    mov A, #$ff
    mov echoDirty, A
    mov io_musicChannelsMask, A
    mov musicSfxChannelMask, A
    mov volShadowDirty_music, A
    mov keyOnMask_music, A

    .assert N_SFX_CHANNELS == 2
    mov sfx_oneChannelSfxId+0, A
    mov sfx_oneChannelSfxId+1, A


    ; Pop ESA from the stack
    pop X


    ; Clear the echo buffer and wait until the echo buffer can be safely written to.
    ;
    ; In the GUI, spc-export and unit-tests this block of code is patched out
    ; and will be replaced with a `JMP ClearEchoBufferEnd : NOP ...`.
ClearEchoBufferStart:
        ; ::TODO remove (required in the `wiz` version to compile compiler crate)::
        nop

        ; Clear the echo buffer
        ; X = S-DSP ESA
        __clear_echo_buffer


        ; Wait until the echo buffer can be safely written to.
        ; (wait until `EDL_SLEEP_COUNT` ticks of TIMER_1)
        mov A, #EDL_SLEEP_COUNT - 1
        EchoBufferWait:
            setc
            sbc A, T1OUT
            bcs EchoBufferWait
ClearEchoBufferEnd:


    ; Set SFX timer
    mov T1TARGET, #SFX_TICK_CLOCK

    ; Timers are enabled and reset in `MainLoop`


    ; Enable echo write and unmute the DSP
    mov A, #DSP_FLG
    mov Y, #0
    movw DSPADDR, YA


    io__init


    ; fallthrough
    .assert PC == mainloop
.endproc


.proc mainloop
    ; Reset counters to ensure `maxTimerCounter` is accurate
    ; Timers and counters reset when the timer control bits transition from 0 to 1
    mov CONTROL, #0

    bbc loaderDataType, LoaderDataType__PLAY_SONG_BIT, NoStartSong
        mov CONTROL, #CONTROL__ENABLE_TIMER_0 | CONTROL__ENABLE_TIMER_1
    NoStartSong:

    ; Clear SMP `T0OUT` and `T1OUT` counters.
    ; Fixes a single audible song tick/note when LoaderDataType__PLAY_SONG_BIT is clear
    ; and the console has just powered-on.
    ; (The bug does not occur when the console is reset or when a new song is loaded).
    .assert T0OUT + 1 == T1OUT
    movw YA, T0OUT

    ; Process first song tick immediately
    ; So echo registers are setup before the first sfx tick
    bra ProcessMusic

    Loop:
        io__process_io_ports

        ; Song tick timer
        mov A, T0OUT
        bne ProcessMusic
            clr1 lagDetector, LD_MUSIC_TEST_BIT
            bra EndMusic

        ProcessMusic:
            call process_music_channels

            ; Set lag bit if `counter_0` was not 0 in the last loop.
            bbc lagDetector, LD_MUSIC_TEST_BIT, NoMusicLag
                set1 lagDetector, LD_MUSIC_LAG_BIT
            NoMusicLag:
            set1 lagDetector, LD_MUSIC_TEST_BIT
        EndMusic:

        ; SFX tick timer
        mov A, T1OUT
        bne ProcessSfx
            clr1 lagDetector, LD_SFX_TEST_BIT
            bra Loop

        ProcessSfx:
            process_sfx_channels

            ; Set lag bit if `counter_1` was not 0 in the last loop.
            bbc lagDetector, LD_SFX_TEST_BIT, NoSfxLag
                set1 lagDetector, LD_SFX_LAG_BIT
            NoSfxLag:
            set1 lagDetector, LD_SFX_TEST_BIT

            ; Loop forever
            jmp Loop
.endproc


; Clears the echo buffer.
;
; ASSUMES: IPL is disabled
; ASSUMES: X matches the `ESA` S-DSP register
;
; WARNING: Can clobber the entirely of Audio-RAM if `esa` is invalid.
;
; WARNING: self modifying code
;
; IN: X = S-DSP ESA
.inline __clear_echo_buffer
    ; X = page address of the start of the echo buffer
    ; Echo buffer ends at the end of memory.

    ; Assert the two `mov addr+Y, A` instructions are 3 bytes
    .assert STA_2 - STA_1 == 3
    .assert After_STA_2 - STA_2 == 3

    ; Set the high byte of the `mov !abs + Y, a` instructions.
    mov STA_1 + 2, X
    mov STA_2 + 2, X

    mov A, #0

    OuterLoop:
        mov Y, #$80

        InnerLoop:
            ; Write 2 bytes every loop.
            ; Writing 1 byte per loop is too slow (COUNTER_0 is 11 when it should be <=8 (TIMER_0=250)).
        STA_1:
            mov $0000 + Y, A
        STA_2:
            mov $0080 + Y, A
        After_STA_2:

            dbnz Y, InnerLoop

        ; Advance to the next page
        ; Repeat until at the end of Audio-RAM
        inc STA_1 + 2
        inc STA_2 + 2
        bne OuterLoop
.endinline


; Process music channels.
;
; Not inline for 2 reasons:
;   1. Used by the GUI to determine if the audio driver is processing music channels.
;   2. Will be required when I add loading ditties to the audio driver.
.proc process_music_channels
    incw songTickCounter

    .assert page(pendingNon_music) != 0
    mov A, pendingNon_music
    mov nonShadow_music, A

    ; Clears NON on the tick after key-off
    mov A, keyOffShadow_music
    tclr1 pendingNon_music

    ; Send KOFF commands to the DSP
    and A, musicSfxChannelMask
    mov Y, A
    mov A, #DSP_KOFF
    movw DSPADDR, YA


    process_echo_registers


    mov volShadowDirty_tmp, volShadowDirty_music

    mov Y, #0
    mov keyOffShadow_music, Y
    mov volShadowDirty_music, Y

    ; Set mutedChannels if SFX is playing noise
    .assert zpTmp == process_channels._mutedChannels
    ; Y = 0
    mov A, noiseLock
    beq NoNoise
        mov Y, nonShadow_music
    NoNoise:
    mov zpTmp, Y

    mov A, voiceChannelsDirty_music
    and A, musicSfxChannelMask

    mov globalVolume_tmp, globalVolume_music
    mov Y, #FIRST_MUSIC_CHANNEL + 1
    mov X, #LAST_MUSIC_CHANNEL + 1
    call process_channels


    mov voiceChannelsDirty_music, voiceChannelsDirty_tmp

    ret
.endproc


.inline process_sfx_channels
    .assert N_SFX_CHANNELS == 2
    BOTH_SFX_ACTIVE = (1 << 7) | (1 << 6)


    ; No need to mask sfx keyOff/keyOn events.
    ; The sfx channels do not emit key-off or key-on events when disabled.

    ; Delay echo and noise changes 1 tick to prevent noise/echo changing in
    ; the middle of the release envelope the channel is ducked or un-ducked.
    mov sfxMusicEchoNoiseMask, prevActiveSoundEffects

    mov A, activeSoundEffects
    mov prevActiveSoundEffects, A

    ; Update `musicSfxChannelMask`.
    ; Music channels can only write to the S-DSP when the sound effect is disabled AND there is no key-off event.
    ; Prevents an audio glitch when a music channel writes to a voice channel in the middle of the key-off release envelope.
    ;
    ; musicSfxChannelMask = a = (a | keyOffShadow_sfx) ^ $ff
    or  A, keyOffShadow_sfx
    eor A, #$ff
    mov musicSfxChannelMask, A

    ; Write keyOffShadow_sfx to the DSP
    mov A, #DSP_KOFF
    mov Y, keyOffShadow_sfx
    movw DSPADDR, YA


    mov Y, pendingNon_sfx
    cmp Y, nonShadow_sfx
    beq SfxNoiseUnchanged
        ; SFX noise changed

        mov nonShadow_sfx, Y

        mov A, Y
        bne SfxNoise
            ; Restore muted music channels on the next music tick when noiseLock is 0.
            or voiceChannelsDirty_music, nonShadow_music
            bra EndSfxNoiseIf

        SfxNoise:
            ; A Sound effect wants to play noise but a music voice is also playing noise
            ; and there is only one noise generator.
            ;
            ; To prevent a noise channel that doesn't have noiseLock from amplifying the
            ; noiseLock channel's noise, all channels that do not own the noiseLock will
            ; have their VxVOL zeroed.
            ;
            ; This is done in two places:
            ;   1. At the start of `process_sfx_channels` when a sound-effect
            ;      wants to play noise, muting music noise before SFX noise starts.
            ;      As an optimisation, this is only done if the previous SFX tick was not
            ;      playing noise.
            ;   2. When channelSoA_virtualChannels are written to the S-DSP.
            ;      `sfxMutedChannels` mutes SFX channels and
            ;      `nonShadow_music` mutes music channels (if `noiseLock` is non-zero).
            ;
            ;      This is required to:
            ;        * Mute any future play_noise instructions.
            ;        * Ignore any future volume changes
            ;      on channels that do not have the `noiseLock`.
            mov X, noiseLock
            bne EndNoiseLock
                mov A, nonShadow_music
                and A, musicSfxChannelMask
                beq EndNoiseLock
                    mov zpTmp, A

                    .assert VxVOL_L == 0
                    ; X = 0
                    mov A, X
                    mov Y, A

                    ; Write 0 to VxVOL_L and VxVOL_R on all channels with `zpTmp` bit set.
                    VolLoop:
                        lsr zpTmp
                        bcc SkipVol
                            movw DSPADDR, YA
                            inc A
                            movw DSPADDR, YA
                            dec A

                            clrc
                    SkipVol:
                        adc A, #$10

                        mov X, zpTmp
                        bne VolLoop

                    mov Y, pendingNon_sfx
            EndNoiseLock:

            mov A, #0
            cmp Y, #BOTH_SFX_ACTIVE
            bcc EndSfxNoiseIf
                ; Both channels want to play noise but there's only 1 noise channel
                ;
                ; Decide which channel plays noise using the interruptible flag
                ; and last SFX play_noise instruction.

                mov A, sfx_interruptibleFlags
                beq UseLastPlayNoiseChannel
                    ; 1 or 2 channels are interruptible.
                    ; Invert interruptible flags
                    eor A, #BOTH_SFX_ACTIVE
                    ; Test if both channels are uninterruptible
                    bne OneChannelUninterruptable
                        UseLastPlayNoiseChannel:
                            ; Both channels are interruptible or uninterruptible.
                            ; Use the SFX channel that last encountered a play_noise instruction.
                            mov A, lastSfxPlayNoiseChannel
                    OneChannelUninterruptable:

                mov Y, A
                ; Mute the other sfx channel
                ; (eor used to swap bits 6 & 7 (since only 1 bit is set)
                eor A, #BOTH_SFX_ACTIVE
        EndSfxNoiseIf:

        ; ::MAYDO replace with a MOVW write (If I can free 1 byte of zeropage)::
        mov noiseLock, Y
        mov sfxMutedChannels, A

        ; Mark muted SFX channel dirty to update VxVOL
        tset1 voiceChannelsDirty_sfx
    SfxNoiseUnchanged:


    ; Clears NON on the tick after key-off
    mov A, keyOffShadow_sfx
    tclr1 pendingNon_sfx

    mov volShadowDirty_tmp, volShadowDirty_sfx

    mov A, #0
    mov keyOffShadow_sfx, A
    mov volShadowDirty_sfx, A

    .assert page(sfxMutedChannels) != 0
    .assert zpTmp == process_channels._mutedChannels
    mov A, sfxMutedChannels
    mov zpTmp, A

    ; DSP voice register writes must be masked
    mov A, voiceChannelsDirty_sfx
    and A, activeSoundEffects

    mov globalVolume_tmp, globalVolume_sfx
    mov Y, #FIRST_SFX_CHANNEL + 1
    mov X, #LAST_SFX_CHANNEL + 1
    call process_channels

    mov voiceChannelsDirty_sfx, voiceChannelsDirty_tmp


    .assert N_SFX_CHANNELS == 2
    .assert sfx_remainingTicks__ELEMENT_SIZE == 2
    decw sfx_remainingTicks+0
    decw sfx_remainingTicks+2
.endinline


; Update the DSP voice registers, write to EON & KON, then execute channel bytecode.
;
; If a voice's `mutedChannels` and `voiceChannelsDirty` bit are both set, then
; 0 will be written to VxVOL.
;
; CAUTION: This function DOES NOT write to the `KOFF` S-DSP register.
;
; WARNING: self modifying code
;
; IN: A = voice channels to write to the DSP (bit array)
; IN: Y = last channel index + 1 (afterFirstChannel)
; IN: X = first channel index + 1 (afterLastChannel)
; IN: zpTmp = muted channels
; IN: volShadowDirty_tmp = volShadowDirty_music or volShadowDirty_sfx
; IN: globalVolume_tmp = globalVolume_music or globalVolume_sfx
;
; OUT: voiceChannelsDirty_tmp = DSP channels with pending writes for the next tick (bit array)
.proc process_channels
_mutedChannels = zpTmp

    ; Modify the `CMP X, #` instruction argument for the two do-while loops
    ; (faster and saves a byte of zeropage)
    mov _VoiceChannelLoop_CPX + 1, Y                ; afterFirstChannel
    mov _BytecodeAndEffectLoop_CPX + 1, X           ; afterLastChannel

    mov voiceChannelsDirty_tmp, A

    ; This loop MUST read the channels (and voiceChannelsDirty_tmp) in the opposite order
    ; to the loop below this one.
    ; That way I can populate `_voiceChannelsDirty_tmp` using `ROR` and read it with `ASL`.
    VoiceChannelLoop:
        dec X

        asl voiceChannelsDirty_tmp
        bcc VoiceRegistersUnchanged
            mov A, LastVoiceRegister + X

            ; Voice registers are written in reverse order so the ADSR2/GAIN voice register is
            ; written BEFORE the ADSR1 voice register.
            ;
            ; This prevents a race-condition that can occur when changing the envelope from ADSR to
            ; GAIN (or vice versa).  If the race-condition occurs, the previous ADSR2/GAIN value is
            ; erroneously used for a single sample.
            ;
            ; This race condition can affect multiple samples if the previous GAIN was a fixed
            ; value and thus audible to the listener.

            mov Y, channelSoA_virtualChannels_tempGain + X
            beq NoTempGain
                movw DSPADDR, YA
                mov Y, #0
                bra EndTempGain
            NoTempGain:
                ; Need to test which register (ADSR2 or GAIN) to write to.
                .assert VxADSR1_ENABLE_BIT == 7
                mov Y, channelSoA_virtualChannels_adsr1 + X
                bpl Gain
                    dec A
                Gain:

                ; Write to VxADSR2 or VxGAIN register
                mov Y, channelSoA_virtualChannels_adsr2OrGain + X
                movw DSPADDR, YA

                mov Y, channelSoA_virtualChannels_adsr1 + X
            EndTempGain:

            ; A = GAIN ($?7) or ADSR2 ($?6)
            .assert (VxGAIN  & %1111_0110) - 1 == VxADSR1
            .assert (VxADSR2 & %1111_0110) - 1 == VxADSR1
            and A, #%1111_0110
            dec A
            ; A = ADSR1 ($?5)
            movw DSPADDR, YA

            dec A
            mov Y, channelSoA_virtualChannels_scrn + X
            movw DSPADDR, YA

            dec A
            mov Y, channelSoA_virtualChannels_pitch_h + X
            movw DSPADDR, YA

            dec A
            mov Y, channelSoA_virtualChannels_pitch_l + X
            movw DSPADDR, YA

            ; Set VxVOL to 0 if _mutedChannels bit is set
            dec A
            mov Y, _mutedChannels
            bpl NotMuted
                mov Y, #0
                movw DSPADDR, YA

                bra EndMutedTest

            NotMuted:
                mov Y, channelSoA_virtualChannels_vol_r + X
                movw DSPADDR, YA

                mov Y, channelSoA_virtualChannels_vol_l + X
            EndMutedTest:
            dec A
            movw DSPADDR, YA

        VoiceRegistersUnchanged:

        asl _mutedChannels

        ; while x >= afterFirstChannel
    _VoiceChannelLoop_CPX:
        cmp X, #0
        bcs VoiceChannelLoop


    ; X = 0 or 8 (first channel index)


    ; Write eonShadow_* to the DSP
    ; MUST not use X
    ; y = a = ((eonShadow_music ^ eonShadow_sfx) & sfxMusicEchoNoiseMask) ^ eonShadow_music
    mov A, eonShadow_music
    eor A, eonShadow_sfx
    and A, sfxMusicEchoNoiseMask
    eor A, eonShadow_music
    mov Y, A
    mov A, #DSP_EON
    movw DSPADDR, YA

    ; Write nonShadow_* to the DSP
    ; MUST not use X
    ; y = a = (nonShadow_music & musicSfxChannelMask) | nonShadow_sfx
    mov A, nonShadow_music
    and A, musicSfxChannelMask
    or  A, nonShadow_sfx
    mov Y, A
    mov A, #DSP_NON
    movw DSPADDR, YA


    ; Setting noise frequency after voice registers are written
    ; so SFX noise frequency is set after SFX VxVOL is zeroed or restored.
    ;
    ; Comparing X with 0 is faster then splitting `process_channels` into two separate functions
    mov A, X
    bne SfxChannels
        ; processing music channels

        ; Write pmonShadow to PMON
        mov A, #DSP_PMON
        mov Y, pmonShadow
        movw DSPADDR, YA

        mov A, noiseLock
        bne NoMusicNoise
            ; Write noiseFreq_music to the DSP
            ; Assumes noiseFreq_music is <= DSP_FLG__NOISE_FREQ_MASK.
            mov A, #DSP_FLG
            mov Y, noiseFreq_music
            movw DSPADDR, YA
        NoMusicNoise:

        mov A, keyOnShadow_music
        and A, musicSfxChannelMask
        and A, io_musicChannelsMask
        mov Y, A

        ; X = 0
        mov keyOnShadow_music, X

        bra EndMusicOrSfxIf


    SfxChannels:
        ; processing sound-effect channels
        mov A, noiseLock
        beq NoSfxNoise
            .assert N_SFX_CHANNELS == 2
            bpl NoiseOnSfx0
                mov Y, noiseFreq_sfx+1
                bra EndSfxNoise
            NoiseOnSfx0:
                mov Y, noiseFreq_sfx+0
        EndSfxNoise:

            ; Write noise frequency to the DSP
            ; Assumes noiseFreq_sfx is <= DSP_FLG__NOISE_FREQ_MASK.
            mov A, #DSP_FLG
            movw DSPADDR, YA
        NoSfxNoise:

        mov Y, keyOnShadow_sfx
        mov keyOnShadow_sfx, #0
    EndMusicOrSfxIf:


    ; Y = keyOnShadow_*
    mov A, #DSP_KON
    movw DSPADDR, YA


    ; X = 0 or 8
    BytecodeAndEffectLoop:
        ; MUST NOT use `_konShadow` and `_afterFirstChannel` in this loop.
        ; The are clobbered by pitch effects and bytecode.

        lsr voiceChannelsDirty_tmp

        ; Vibrato is processed before bytecode.
        ; This delays all pitch changes 1 tick after a play-note instruction
        ; and ensures the first pitch on a play-note instruction is the requested note.
        process_vibrato


        ; Must saturate-increment on every tick.
        ; The note could be a part of a `play-note nokeyoff | rest` chain.
        mov A, channelSoA_ticksAfterNote + X
        inc A
        bne NoTicksOverflow
            dec A
        NoTicksOverflow:
        mov channelSoA_ticksAfterNote + X, A


        mov A, channelSoA_countdownTimer_h + X
        beq CountdownHIsZero
            ; Decrement `countdownTimer_h` if `dec countdownTimer_l` will underflow
            mov Y, channelSoA_countdownTimer_l + X
            bne NoCtHDec
                ; A = countdownTimer_h
                .assert page(channelSoA_countdownTimer_h) != 0
                dec A
                mov channelSoA_countdownTimer_h + X, A
            NoCtHDec:

            dec channelSoA_countdownTimer_l + X

            bra EndTimer


        CountdownHIsZero:
            ; countdown is > 0 and <= $ff
            dec channelSoA_countdownTimer_l + X
            beq CountdownZero
                ; Skip if keyoff flag is clear
                mov Y, channelSoA_keyoffMsbFlag + X
                bpl EndTimer

                ; Always key-off on the last sleep tick
                mov A, channelSoA_countdownTimer_l + X
                dec A
                beq KeyOff

                ; Test for early release
                cmp A, channelSoA_earlyRelease_cmp + X
                bcs EndTimer

                mov A, channelSoA_ticksAfterNote + X
                cmp A, channelSoA_earlyRelease_minTicks + X
                bcc EndTimer

                    ; Early release
                    mov A, channelSoA_earlyRelease_gain + X
                    beq KeyOff
                        mov channelSoA_virtualChannels_tempGain + X, A

                        set1 voiceChannelsDirty_tmp, 7
                        bra EndTimer

                KeyOff:
                    mov A, #0
                    ; Do not test countdownTimer or early-release on the next tick
                    mov channelSoA_keyoffMsbFlag + X, A
                    ; Disable temp gain
                    mov channelSoA_virtualChannels_tempGain + X, A

                    ; Keyoff note
                    mov A, ChannelVoiceBit + X
                    cmp X, #FIRST_SFX_CHANNEL
                    bcc WriteKeyOffMusicBits
                        tset1 keyOffShadow_sfx
                        tset1 keyOnMask_sfx
                        bra EndTimer

                    WriteKeyOffMusicBits:
                        tset1 keyOffShadow_music
                        tset1 keyOnMask_music
                        bra EndTimer

        CountdownZero:
            ; decremented countdown timer is 0

            mov Y, channelSoA_instructionPtr_h + X
            beq ChannelDisabled
                ; Clear key-off flag for safety
                lsr channelSoA_keyoffMsbFlag + X

                mov A, channelSoA_instructionPtr_l + X
                call process_bytecode

                movw YA, instructionPtr
                mov channelSoA_instructionPtr_l + X, A
                mov channelSoA_instructionPtr_h + X, Y

                ; Request S-DSP voice register update (set dirty bit).
                ; The bytecode most likely changed the virtual channel.
                ;
                ; The flag is set here (and once) instead of every instruction that changes
                ; `channelSoA_virtualChannels` to save a lot of code space.
                ;
                set1 voiceChannelsDirty_tmp, 7
            ChannelDisabled:
        EndTimer:


        mov A, channelSoA_volEffect_direction + X
        beq NoVolEffect
            process_volume_effects

            set1 volShadowDirty_tmp, 7
        NoVolEffect:

        mov A, channelSoA_panEffect_direction + X
        beq NoPanEffect
            process_pan_effects

            set1 volShadowDirty_tmp, 7
        NoPanEffect:


        ; Cannot do this after `process_bytecode` for 2 reasons:
        ;  1. Volume must be calculated on the first instruction of a sound-effect
        ;  2. Volume/pan slide/vibrato
        asl volShadowDirty_tmp
        bcc VolumeUnchanged
            update_vol_shadow

            set1 voiceChannelsDirty_tmp, 7
        VolumeUnchanged:

        ; Portamento is processed after bytecode to ensure pitch slide occurs
        ; on the same tick as the portamento instruction.
        process_portamento


        inc X

        ; while x < afterLastChannel
    _BytecodeAndEffectLoop_CPX:
        cmp X, #0
        bcs Return
            jmp BytecodeAndEffectLoop

Return:
    ret
.endproc


; Writes echo variables to the S-DSP echo registers if the `echoDirty` bits are set
.inline process_echo_registers
    mov A, echoDirty
    beq End
        .assert ECHO_DIRTY__FIR_FILTER_BIT == 7
        bpl FirUnchanged
            .assert DSP_C1 - DSP_C0 == $10
            .assert DSP_C7 < $80
            .assert DSP_C7 + $10 >= $80

            .assert ECHO_DIRTY__CLEAR_FIR_BIT == 0
            lsr A
            bcc NoClear
                ; Clear FIR filter
                clrc
                mov A, #DSP_C0
                mov Y, #0

                ClearFirLoop:
                    ; Y = 0
                    movw DSPADDR, YA

                    adc A, #$10
                    bpl ClearFirLoop
            NoClear:

            mov X, #0
            mov A, #DSP_C0
            ; carry clear
            FirLoop:
                mov Y, echo.firFilter + X
                movw DSPADDR, YA
                inc X

                adc A, #$10
                bpl FirLoop

            mov A, echoDirty
        FirUnchanged:

        .assert ECHO_DIRTY__VOLUME_BIT == 6
        asl A
        bpl EvolUnchanged
            mov DSPADDR, #DSP_EVOL_L

            mov A, echo.echoVolume_l

            ; Reading carry here saves a `CLC` instruction when averaging echo volumes
            mov1 C, loaderDataType, LoaderDataType__STEREO_FLAG_BIT
            bcs Stereo
                ; Mono

                ; Calculate average echo volume.
                ; Assumes echoVolume_l and echoVolume_r are both > 127.
                ; carry clear
                adc A, echo.echoVolume_r
                lsr A

                bbc echo.invertFlags, INVERT_FLAGS__MONO_BIT, NoMonoInvert
                    eor A, #$ff
                    inc A
                NoMonoInvert:
                mov DSPDATA, A

                bra EndMonoIf


            Stereo:
                bbc echo.invertFlags, INVERT_FLAGS__LEFT_BIT, NoLeftInvert
                    eor A, #$ff
                    inc A
                NoLeftInvert:
                mov DSPDATA, A

                mov A, echo.echoVolume_r
                bbc echo.invertFlags, INVERT_FLAGS__RIGHT_BIT, NoRightInvert
                    eor A, #$ff
                    inc A
                NoRightInvert:

            EndMonoIf:

            ; Write A to DSP_EVOL_R
            .assert DSP_EVOL_L | (1 << 4) == DSP_EVOL_R
            set1 DSPADDR, 4
            mov DSPDATA, A
        EvolUnchanged:

        ; Always update echo feedback
        mov DSPADDR, #DSP_EFB
        mov DSPDATA, echo.echoFeedback

        ; Always update echo delay
        mov DSPADDR, #DSP_EDL
        mov DSPDATA, echo.edl

        mov echoDirty, #0
    End:
.endinline



; IO Ports
; ========

.inline io__init
    ; ::TODO review these assumptions when implementing the sample swapping loader::
    ; Assumes audio driver starts paused.
    ; Assumes `DriverIO__COMMAND_ACK_PORT` is non-zero from the loader.
    .assert IoCommands__PAUSE == 0

    mov A, #IoCommands__PAUSE
    mov DriverIO__COMMAND_ACK_PORT, A
    mov previousCommand, A
.endinline


.inline io__process_io_ports
    ; If the S-CPU writes to an IO port at the same time S-SMP reads from the
    ; same IO port, the S-SMP will read invalid data.
    ;
    ; To detect this bug the IO-Port is read twice and the command is only
    ; processed if both reads are the same.
    ;
    ; Once the command byte has been read successfully, the two parameter bytes
    ; do not need to be double-read because:
    ;  * The S-CPU will wait until the command has been acknowledged before
    ;    writing a new command
    ;  * The parameter bytes are written before the command byte is written.

    ; Test if `DriverIO__COMMAND_PORT` has changed.
    mov A, DriverIO__COMMAND_PORT
    cmp A, previousCommand
    beq NoCommand
        ; Test if the second COMMAND_PORT read is equal to the first COMMAND_PORT read
        cbne DriverIO__COMMAND_PORT, NoCommand
            push A
            mov previousCommand, A

            mov Y, DriverIO__PARAMETER_0_PORT
            call _io__call_command

            ; Acknowledge command
            pop A
            mov DriverIO__COMMAND_ACK_PORT, A
    NoCommand:


    ; Execute the loader if the _SWITCH_TO_LOADER_BIT is set.
    bbc DriverIO__SWITCH_TO_LOADER_PORT, DriverIO__SWITCH_TO_LOADER_BIT, NoStl
        ; Read the IO port a second time just in case it was a glitch
        bbc DriverIO__SWITCH_TO_LOADER_PORT, DriverIO__SWITCH_TO_LOADER_BIT, NoStl
            jmp LOADER_ADDR
    NoStl:
.endinline


; NOTE: Does not acknowledge command
;
; IN: A = unpacked DriverIO__COMMAND_PORT
; IN: Y = DriverIO__PARAMETER_0_PORT
.proc _io__call_command
    .assert CommandFunctionTable_SIZE % 2 == 0
    .assert DriverIO__COMMAND_MASK >= CommandFunctionTable_SIZE - 2

    and A, #DriverIO__COMMAND_MASK
    cmp A, #CommandFunctionTable_SIZE
    bcs Return
        mov X, A
        jmp [CommandFunctionTable+X]

Return:
    ret
.endproc


CommandFunctionTable:
    .functiontable IoCommands
CommandFunctionTableEnd:
CommandFunctionTable_SIZE = CommandFunctionTableEnd - CommandFunctionTable



; Pause music and sound effects
.proc cmd__pause
    ; Disable timers
    mov CONTROL, #0

    ; Reset counters to fix a music/sfx channel playing a single tick after a `pause` IO command.
    ; (clears counter_0 and counter_1)
    .assert T0OUT + 1 == T1OUT
    movw YA, T0OUT

    ; key-off all channels
    mov Y, #$ff
    bra __cmd__keyoff
.endproc


.proc cmd__pause_music_play_sfx
    ; Disable music timer, enable sfx timer
    mov CONTROL, #CONTROL__ENABLE_TIMER_1

    ; Reset counter to fix music channel playing a single tick after a `pause_music_play_sfx` IO command.
    mov A, T0OUT

    mov Y, musicSfxChannelMask  ; key-off music channels

    ; fallthrough
    .assert PC == __cmd__keyoff
.endproc


; IN: Y = channels
.proc __cmd__keyoff
    mov A, #DSP_KOFF
    movw DSPADDR, YA

    ret
.endproc


.proc cmd__unpause
    ; Enable music and sfx timers
    mov CONTROL, #CONTROL__ENABLE_TIMER_0 | CONTROL__ENABLE_TIMER_1

    ret
.endproc


; IN: Y = volume
.proc cmd__set_main_volume
    ; Write Y to MVOL_L and MVOL_R DSP registers
    mov A, #DSP_MVOL_L
    movw DSPADDR, YA

    mov A, #DSP_MVOL_R
    movw DSPADDR, YA

    ret
.endproc


; IN: Y = channel mask
.proc cmd__set_music_channels
    mov io_musicChannelsMask, Y

    ; key-off muted channels
    mov A, Y
    eor A, #$ff
    tset1 keyOffShadow_music

    ret
.endproc


; IN: Y = timer
.proc cmd__set_song_timer
    ; if (y > 0 && y < MIN_TICK_CLOCK) y = MIN_TICK_CLOCK
    dec Y
    cmp Y, #MIN_TICK_CLOCK - 1
    bcs Valid
        mov Y, #MIN_TICK_CLOCK - 1
Valid:
    inc Y

    mov T0TARGET, Y

    ret
.endproc


; IN: music volume
.proc cmd__set_global_music_volume
    ; Increment volume so maximum volume is 0
    inc Y
    mov globalVolume_music, Y

    mov volShadowDirty_music, #$ff

    ret
.endproc


; IN Y: global music volume
.proc cmd__set_global_volumes
    call cmd__set_global_music_volume

    mov Y, DriverIO__PARAMETER_1_PORT

    ; fallthrough
    .assert PC == cmd__set_sfx_volume
.endproc


; IN: global SFX volume
.proc cmd__set_sfx_volume
    ; Increment volume so maximum volume is 0
    inc Y
    mov globalVolume_sfx, Y

    .assert N_SFX_CHANNELS == 2
    mov volShadowDirty_sfx, #%11000000

    ret
.endproc



.proc cmd__stop_sound_effects
    .assert N_SFX_CHANNELS == 2

    ; `bc__disable_channel` does not write to `channelSoA_instructionPtr`.
    ; (it changes `instructionPtr`)
    mov channelSoA_instructionPtr_h + FIRST_SFX_CHANNEL, #0
    mov channelSoA_instructionPtr_h + LAST_SFX_CHANNEL, #0

    mov X, #FIRST_SFX_CHANNEL
    call bc__disable_channel

    mov X, #LAST_SFX_CHANNEL
    jmp bc__disable_channel
.endproc


; IN: Y = sound effect ID
.proc cmd__play_sound_effect
    .assert N_SFX_CHANNELS == 2
    BOTH_SFX_ACTIVE = (1 << 7) | (1 << 6)

    cmp Y, commonDataHeader.nSoundEffects
    bcs Return
        cmp Y, sfx_oneChannelSfxId+0
        beq sfx__maybe_restart_sfx_0

        cmp Y, sfx_oneChannelSfxId+1
        beq sfx__maybe_restart_sfx_1

        mov A, activeSoundEffects
        cmp A, #BOTH_SFX_ACTIVE
        bcs sfx__both_channels_active

        ; At most 1 sound effect channel is active
        asl A
        bcc sfx__play_1
        bra sfx__play_sfx_0

Return:
    ret
.endproc


; IN: Y = sound effect ID
.proc sfx__both_channels_active
    .assert N_SFX_CHANNELS == 2
    BOTH_SFX_ACTIVE = (1 << 7) | (1 << 6)

    ; Both sound effect channels are active,

    ; Drop sfx_id if it is a low-priority sound effect
    cmp Y, commonDataHeader.firstLowPrioritySfx
    bcs Return

        ; if (zero && y < commonDataHeader.nHighPrioritySfx) || a >= BOTH_SFX_ACTIVE {
        mov A, sfx_interruptibleFlags
        bne OtherTest
            cmp Y, commonDataHeader.nHighPrioritySfx
            bcc BothChannels
        OtherTest:
            cmp A, #BOTH_SFX_ACTIVE
            bcc NotBothChannels

        BothChannels:
            ; Both sound effect channels active and either:
            ;   * both uninterruptible and sfx_id is high-priority
            ;   * both interruptible and sfx_id is normal priority
            ;
            ; check remaining ticks and drop the sound effect that will finish first.

            .assert N_SFX_CHANNELS == 2
            .assert sfx_remainingTicks__ELEMENT_SIZE == 2
            movw YA, sfx_remainingTicks+0
            cmpw YA, sfx_remainingTicks+2

            mov Y, DriverIO__PARAMETER_0_PORT

            bcc sfx__play_sfx_0
            bra sfx__play_1


        NotBothChannels:
            ; Play sound effect if a channel is interruptible
            ; A = sfx_interruptibleFlags
            asl A
            bcs sfx__play_1
            bmi sfx__play_sfx_0

Return:
    ret
.endproc

_PlaySfx_Return = sfx__both_channels_active.Return



; Restart sfx channel 0 if it is interruptible
; ASSUMES: sfx_id == sfx_oneChannelSfxId[0]
;
; IN: Y = sound effect ID
.proc sfx__maybe_restart_sfx_0
    bbc sfx_interruptibleFlags, 6, _PlaySfx_Return

    ; fallthrough
    .assert PC == sfx__play_sfx_0
.endproc


; IN: Y = sound effect ID
.proc sfx__play_sfx_0
    ; If this procedure is modified, `_PlaySFX_1` MUST ALSO be modified
    _I = 0
    _BIT = 8 - N_SFX_CHANNELS + _I

    .assert sfx_remainingTicks__ELEMENT_SIZE == 2
    .assert sfx_oneChannelSfxId__ELEMENT_SIZE == 1

    mov A, [commonData.soundEffects_durationAndInterruptFlag_l] + Y
    mov sfx_remainingTicks + _I*2, A

    mov A, [commonData.soundEffects_durationAndInterruptFlag_h] + Y
    mov sfx_remainingTicks + _I*2 + 1, A

    asl A
    mov1 sfx_interruptibleFlags, _BIT, C

    mov sfx_oneChannelSfxId + _I, Y

    mov A, [commonData.soundEffects_addrAndOneChannelFlag_h] + Y
    bmi NotOneChannel
        mov sfx_oneChannelSfxId + _I, #$ff
    NotOneChannel:

    mov X, #FIRST_SFX_CHANNEL
    bra sfx__play_sfx_on_channel
.endproc


; Restart sfx channel 1 if it is interruptible
; ASSUMES: sfx_id == sfx_oneChannelSfxId[1]
;
; IN: Y = sound effect ID
.proc sfx__maybe_restart_sfx_1
    bbc sfx_interruptibleFlags, 7, _PlaySfx_Return

    ; fallthrough
    .assert PC == sfx__play_1
.endproc


; IN: Y = sound effect ID
.proc sfx__play_1
    ; If this procedure is modified, `_PlaySFX_0` MUST ALSO be modified
    _I = 1
    _BIT = 8 - N_SFX_CHANNELS + _I

    .assert sfx_remainingTicks__ELEMENT_SIZE == 2
    .assert sfx_oneChannelSfxId__ELEMENT_SIZE == 1

    mov A, [commonData.soundEffects_durationAndInterruptFlag_l] + Y
    mov sfx_remainingTicks + _I*2, A

    mov A, [commonData.soundEffects_durationAndInterruptFlag_h] + Y
    mov sfx_remainingTicks + _I*2 + 1, A

    asl A
    mov1 sfx_interruptibleFlags, _BIT, C

    mov sfx_oneChannelSfxId + _I, Y

    mov A, [commonData.soundEffects_addrAndOneChannelFlag_h] + Y
    bmi NotOneChannel
        mov sfx_oneChannelSfxId + _I, #$ff
    NotOneChannel:

    mov X, #LAST_SFX_CHANNEL
    ; fallthrough
    .assert PC == sfx__play_sfx_on_channel
.endproc


; ASSUMES: sfx_id < commonData.nSoundEffects
; KEEP: X
;
; IN: A = addrAndOneChannelFlag_h
; IN: X = channelIndex
; IN: Y = sound effect ID
.proc sfx__play_sfx_on_channel
    and A, #$7f
    mov channelSoA_instructionPtr_h + X, A

    mov A, [commonData.soundEffects_addrAndOneChannelFlag_l] + Y
    mov channelSoA_instructionPtr_l + X, A

    mov A, ChannelVoiceBit + X
    ; Send a key-off event, the S-DSP voice channel might be active.
    tset1 keyOffShadow_sfx
    ; Set keyOnMask.  The next play-note instruction will queue a KON event.
    tset1 keyOnMask_sfx
    ; Disable echo
    tclr1 eonShadow_sfx

    ; Not disabling noise.
    ; Clearing pendingNon_sfx would disable noise in the middle of the release envelope.
    ; Noise will be disabled after the key-off release envelope.

    ; Enable sfx channel.
    tset1 activeSoundEffects

    ; Mark sfx channel volume dirty
    ; eor used to swap bits 6 & 7 (since only 1 bit is set)
    eor A, #%11_000000
    tset1 volShadowDirty_sfx


    ; Read pan parameter
    mov A, DriverIO__PARAMETER_1_PORT

    ; fallthrough
    .assert PC == __reset_channel
.endproc


; NOTE: Does not set:
;  * channelSoA_instructionPtr
;  * instrument/envelope
;  * virtual channels
;  * keyOnMask
;  * eonShadow
;  * nonShadow
;
; IN: X = channelIndex
; IN: A = pan
; KEEP: X
.proc __reset_channel
    ; MUST NOT use `commonData`.
    ; (It contains song header data, not common-audio data)

    setp
.p1
    ; MUST NOT access zeropage variables until direct_page is false.
    .assert inFirstpage(channelSoA_pan)
    mov channelSoA_pan + X, A

    .assert inFirstpage(channelSoA_volume)
    mov A, #STARTING_VOLUME
    mov channelSoA_volume + X, A


    mov A, #0

    ; Disable pan effects
    .assert inFirstpage(channelSoA_panEffect_direction)
    mov channelSoA_panEffect_direction + X, A

    ; Disable volume effects
    .assert inFirstpage(channelSoA_volEffect_direction)
    mov channelSoA_volEffect_direction + X, A

    ; Disable channel invert
    .assert inFirstpage(channelSoA_invertFlags)
    mov channelSoA_invertFlags + X, A

    ; Disable early-release
    .assert inFirstpage(channelSoA_earlyRelease_cmp)
    mov channelSoA_earlyRelease_cmp + X, A
        ; No need to reset `earlyRelease_minTicks`,
        ; (Early release is not active if `earlyRelease_cmp == 0`)

    ; Reset detune
    .assert inFirstpage(channelSoA_detune_l)
    .assert inFirstpage(channelSoA_detune_h)
    mov channelSoA_detune_l + X, A
    mov channelSoA_detune_h + X, A


    .assert page(channelSoA_transpose) != 0
    mov channelSoA_transpose + X, A

    clrp
.p0

    ; Disable temp-gain
    .assert inZeropage(channelSoA_virtualChannels_tempGain)
    mov channelSoA_virtualChannels_tempGain + X, A

    ; Disable portamento
    .assert inZeropage(channelSoA_portamento_direction)
    mov channelSoA_portamento_direction + X, A

    ; Disable vibrato
    .assert inZeropage(channelSoA_vibrato_pitchOffsetPerTick)
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A


    ; Immediately process bytecode on the next tick
    ; A = 0
    mov channelSoA_countdownTimer_h + X, A
    inc A
    ; A = 1
    mov channelSoA_countdownTimer_l + X, A


    mov A, BcStackIndexesStart + X
    mov channelSoA_stackPointer + X, A

    ; Ensures loopStackPointer is in-bounds if the first instruction is `end_loop`.
    setc
    sbc A, #3
    mov channelSoA_loopStackPointer + X, A

    ret
.endproc



; Volume and Pan
; ==============

; Process volume effect
;
; CAUTION: Does not set `volShadowDirty_tmp`.
; CAUTION: `channelSoA_volEffect_direction` must be non-zero
;
; IN: X = ChannelIndex
; IN: A = channelSoA_volEffect_direction
; KEEP: X
.inline process_volume_effects
    ; Warning spaghetti code: Optimised for code-size.

    setp
.p1

    lsr A

    mov A, channelSoA_subVolume + X
    mov Y, channelSoA_volume + X

    bcc Up
        ; down
        ; carry set
        sbc A, channelSoA_volEffect_offset_l + X
        mov channelSoA_subVolume + X, A

        mov A, Y
        sbc A, channelSoA_volEffect_offset_h + X

        bcs EndIf
            mov Y, #0
            bra DisableEffect

        UpOverflow:
            mov Y, #$ff

        DisableEffect:
            mov A, #0
            bra WriteDirection

    Up:
        ; up
        ; carry clear
        adc A, channelSoA_volEffect_offset_l + X
        mov channelSoA_subVolume + X, A

        mov A, Y
        adc A, channelSoA_volEffect_offset_h + X

        bcs UpOverflow
    EndIf:


    mov Y, A

    dec channelSoA_volEffect_counter + X
    bne EndCounterIf
        mov A, channelSoA_volEffect_halfWavelength + X
        beq WriteDirection
            mov channelSoA_volEffect_counter + X, A

            ; Change direction
            mov A, channelSoA_volEffect_direction + X
            eor A, #1

    WriteDirection:
        mov channelSoA_volEffect_direction + X, A
    EndCounterIf:

    mov channelSoA_volume + X, Y

    clrp
.p0

.endinline


; Process pan effect
;
; CAUTION: Does not set `volShadowDirty_tmp`.
; CAUTION: `channelSoA_panEffect_direction` must be non-zero
;
; IN: X = ChannelIndex
; IN: A = channelSoA_panEffect_direction
; KEEP: X
.inline process_pan_effects
    ; Warning spaghetti code: Optimised for code-size.

    setp
.p1

    lsr A

    mov Y, channelSoA_pan + X
    mov A, channelSoA_subPan + X

    bcc Up
        ; down
        ; carry set
        sbc A, channelSoA_panEffect_offset_l + X
        mov channelSoA_subPan + X, A

        mov A, Y
        sbc A, channelSoA_panEffect_offset_h + X

        bcs EndIf
            mov Y, #0
            bra DisableEffect

        UpOverflow:
            mov Y, #MAX_PAN

        DisableEffect:
            mov A, #0
            bra WriteDirection

    Up:
        ; up
        ; carry clear
        adc A, channelSoA_panEffect_offset_l + X
        mov channelSoA_subPan + X, A

        mov A, Y
        adc A, channelSoA_panEffect_offset_h + X

        bcs UpOverflow
        cmp A, #MAX_PAN + 1
        bcs UpOverflow
    EndIf:


    mov Y, A

    dec channelSoA_panEffect_counter + X
    bne EndCounterIf
        mov A, channelSoA_panEffect_halfWavelength + X
        beq WriteDirection
            mov channelSoA_panEffect_counter + X, A

            ; Change direction
            mov A, channelSoA_panEffect_direction + X
            eor A, #1
    WriteDirection:
        mov channelSoA_panEffect_direction + X, A
    EndCounterIf:

    mov channelSoA_pan + X, Y

    clrp
.p0

.endinline


; Updates vol_l and vol_r shadow variables.
;
; KEEP: X
.inline update_vol_shadow
    .assert page(channelSoA_volume) != 0
    mov A, channelSoA_volume + X
    mov Y, A

    ; If global-volume is 0, the channel volume is unchanged.
    mov A, globalVolume_tmp
    beq NoMul
        mul YA
    NoMul:
    ; Y = volume

    ; Reading carry here saves a `SEC` instruction when calculating VOL_R
    mov1 C, loaderDataType, LoaderDataType__STEREO_FLAG_BIT
    bcs Stereo
        ; Mono

        ; First shift converts 8-bit volume to 7-bit VOL
        ; Second shift ensure mono vol has same combined amplitude as stereo vol
        mov A, Y
        lsr A
        lsr A
        mov Y, A

        ; Test mono flag
        .assert INVERT_FLAGS__MONO & %1100_0001 == 0 ; Assert masking is more efficient than bpl or shifts
        mov A, channelSoA_invertFlags + X
        and A, #INVERT_FLAGS__MONO
        beq NoMonoInvert
            ; Invert Y
            mov A, Y
            eor A, #$ff
            inc A
            mov Y, A
        NoMonoInvert:

        mov channelSoA_virtualChannels_vol_r + X, Y
        ; vol_l set after if

        bra EndMonoIf

    Stereo:
        ; carry set

        mov zpTmp, Y

        ; Calculate right volume
        ; A pan value of `MAX_PAN` is 100% to the right.
        ; (MUST NOT USE CARRY)

        mov A, channelSoA_pan + X
        mul YA

        .assert INVERT_FLAGS__RIGHT_BIT == 7
        mov A, channelSoA_invertFlags + X
        bpl NoRightInvert
            ; Invert Y
            mov A, Y
            eor A, #$ff
            inc A
            mov Y, A
        NoRightInvert:

        mov channelSoA_virtualChannels_vol_r + X, Y

        ; Calculate left volume
        ; A pan value of `0` is 100% to the left.

        mov A, #MAX_PAN
        ; Carry set (MUL does not set carry)
        sbc A, channelSoA_pan + X
        mov Y, zpTmp
        mul YA

        .assert INVERT_FLAGS__LEFT_BIT == 6
        mov A, channelSoA_invertFlags + X
        asl A
        bpl NoLeftInvert
            ; Invert Y
            ; (have to invert Y, no 65816-style `bit invertFlags,x` instruction)
            mov A, Y
            eor A, #$ff
            inc A
            mov Y, A
        NoLeftInvert:
    EndMonoIf:

    mov channelSoA_virtualChannels_vol_l + X, Y
.endinline


; Pitch Effects
; =============


; IN: X = channelIndex
; KEEP: X
.inline process_vibrato
    ; Vibrato is not active during portamento.
    mov Y, channelSoA_portamento_direction + X
    bne NoVibrato

    ; Vibrato is disabled if pitch-offset-per-tick is 0
    mov Y, channelSoA_vibrato_pitchOffsetPerTick + X
    beq NoVibrato

    ; Vibrato is not active when ticksAfterNote < vibratoDelay
    mov A, channelSoA_ticksAfterNote + X
    cmp A, channelSoA_vibrato_delay + X
    bcc NoVibrato

        mov A, channelSoA_vibrato_direction + X
        lsr A
        bcs Subtract
            ; Add Y to pitch_l
            mov A, Y
            ; carry clear
            adc A, channelSoA_virtualChannels_pitch_l + X
            bcc WritePitchL
                inc channelSoA_virtualChannels_pitch_h + X
                bra WritePitchL

        Subtract:
            ; Subtract pitchOffsetPerTick from pitch_l
            ; (uses 1 less byte than reverse subtract Y, with the same CPU cycle count)
            mov A, channelSoA_virtualChannels_pitch_l + X
            ; carry set
            sbc A, channelSoA_vibrato_pitchOffsetPerTick + X

            bcs WritePitchL
                dec channelSoA_virtualChannels_pitch_h + X

    WritePitchL:
        mov channelSoA_virtualChannels_pitch_l + X, A

        setp
    .p1

        dec channelSoA_vibrato_tickCounter + X
        bne TcNotZero
            mov A, channelSoA_vibrato_halfWavelength + X
            mov channelSoA_vibrato_tickCounter + X, A

            clrp
        .p0
            inc channelSoA_vibrato_direction + X
        TcNotZero:

    ; PSW.p is unknown
        clrp
    .p0

        ; Set S-DSP voice registers dirty bit
        set1 voiceChannelsDirty_tmp, 7
    NoVibrato:
.endinline


; IN: X = channelIndex
; KEEP: X
.inline process_portamento
_target_h = zpTmp

    ; Warning: spaghetti code

    mov A, channelSoA_portamento_direction + X
    beq NoPortamento
        bpl Up
            ; portamento down

            ; Save target_h for later (no `cpy dp,x` instruction)
            mov A, channelSoA_portamento_target_h + X
            mov _target_h, A

            mov A, channelSoA_virtualChannels_pitch_l + X
            mov Y, channelSoA_virtualChannels_pitch_h + X

            ; Subtract speed from pitch
            setc
            sbc A, channelSoA_portamento_speed + X
            bcs Down_CarryClear
                dec Y
                ; Test for underflow
                bmi MetTarget
            Down_CarryClear:

            ; Test pitch against target
            cmp Y, _target_h
            bne Down_SkipCmpL
                cmp A, channelSoA_portamento_target_l + X
            Down_SkipCmpL:
            bcs WritePitch
            bra MetTarget


        Up:
            ; portamento up

            ; Save target_h for later (no `cpy dp,x` instruction)
            mov A, channelSoA_portamento_target_h + X
            mov _target_h, A

            mov A, channelSoA_virtualChannels_pitch_l + X
            mov Y, channelSoA_virtualChannels_pitch_h + X

            ; Add speed to pitch
            clrc
            adc A, channelSoA_portamento_speed + X
            bcc Up_NoCarry
                inc Y
                ; PITCH is 14 bits long, no overflow test required.
            Up_NoCarry:

            ; Test pitch against target
            cmp Y, _target_h
            bne Up_SkipCmpL
                cmp A, channelSoA_portamento_target_l + X
        Up_SkipCmpL:
            bcc WritePitch


        ; Pitch reached or exceeded target
    MetTarget:
        ; Disable portamento
        mov A, #0
        mov channelSoA_portamento_direction + X, A

        ; Restart vibrato (if active)
        ; A = 0
        mov channelSoA_vibrato_direction + X, A

        mov A, channelSoA_vibrato_tickCounterStart + X
        mov channelSoA_vibrato_tickCounter + X, A

        mov A, channelSoA_portamento_target_l + X
        mov Y, _target_h


    WritePitch:
        ; YA = new pitch
        mov channelSoA_virtualChannels_pitch_l + X, A
        mov channelSoA_virtualChannels_pitch_h + X, Y

        ; Set S-DSP voice registers dirty bit
        set1 voiceChannelsDirty_tmp, 7
    NoPortamento:
.endinline



; Bytecode
; ========
;
; bytecode functions MUST NOT modify X.
;
; bytecode functions MUST NOT modify S-DSP registers.


; See `process_bytecode_with_loader_test`
.proc _process_bytecode_with_loader_test__SecondBitTest
    ; Test the SWITCH_TO_LOADER_BIT a second time (just to be safe) before switching to the loader
    bbc DriverIO__SWITCH_TO_LOADER_PORT, DriverIO__SWITCH_TO_LOADER_BIT, process_bytecode
    jmp LOADER_ADDR
.endproc


; Set the bytecode `instructionPtr` and execute the next bytecode instruction while also
; testing the if `_SWITCH_TO_LOADER_BIT` bit is set.
;
; If the `_SWITCH_TO_LOADER_BIT` bit is set, the audio driver stops execution and the loader is
; started.  This should hopefully ensure the loader will be executed if the bytecode goes off the
; rails and never sleeps.
;
; IN: YA = new instructionPtr
; IN: X = channelIndex
; KEEP: X
.proc process_bytecode_with_loader_test
    bbs DriverIO__SWITCH_TO_LOADER_PORT, DriverIO__SWITCH_TO_LOADER_BIT, _process_bytecode_with_loader_test__SecondBitTest

    ; fallthrough
    .assert PC == process_bytecode
.endproc


; Set the bytecode `instructionPtr` and execute the next bytecode instruction.
;
; IN: YA = new instructionPtr
; IN: X = channelIndex
; KEEP: X
.proc process_bytecode
    movw instructionPtr, YA

    ; fallthrough
    .assert PC == process_next_bytecode
.endproc


; Execute the next bytecode instruction
;
; IN: X = channelIndex
; KEEP: X
.proc process_next_bytecode

    mov Y, #0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    cmp A, #FIRST_PLAY_NOTE_INSTRUCTION
    bcs _bc__play_note

    ; A is a non play-note bytecode
    asl A
    mov Y, A

    ; Push return address to the stack.
    ;
    ; This method uses the same number of CPU cycles (and the least amount of code space) compared to:
    ;   * `JMP[!abs+X]` plus saving/restoring X via a zeropage register (as there is no `TXY` or `TXY` instruction
    ;   * self modifying code (writing the return address of a JMP instruction)
    ;
    mov A, BytecodeInstructionTable + 1 + Y
    push A
    mov A, BytecodeInstructionTable + 0 + Y
    push A

    cmp Y, #FIRST_NO_ARGUMENT_INSTRUCTION_OPCODE * 2
    bcs NoParameters
        ; Instruction has a parameter
        mov Y, #0
        mov A, [instructionPtr] + Y
        incw instructionPtr
    NoParameters:

    ; Y MUST be 0 if the instruction has an argument

    ; Jump to the bytecode instruction routine that was just pushed onto the stack.
    ret
.endproc


; NOTE: BYTECODE INSTRUCTIONS MUST KEEP X
;
; NOTE: If the instruction has an argument, Y will be 0
;
; Bytecode instruction must either:
;    * Return after setting the countdown timer.
;    * Jump to `process_next_bytecode` to execute the next bytecode instruction.
;    * Jump to `process_bytecode` to change the bytecode instruction pointer and execute it.
;    * Jump to `process_bytecode_with_loader_test`.



; IN: A = high byte of the skip-last-loop argument
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__skip_last_loop_u16be
    mov zpTmpWord.h, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    bra _bc__skip_last_loop__bytesToSkip_l
.endproc


; IN: A = skip-last-loop argument
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__skip_last_loop_u8
    ; Zero high byte
    mov zpTmpWord.h, #0

    .assert PC == _bc__skip_last_loop__bytesToSkip_l
.endproc


; IN: A = low byte of skip-last-loop argument
; IN: zpTmpWord.h = high byte of skip-last-loop argument
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__skip_last_loop__bytesToSkip_l
    mov zpTmpWord.l, A

    ; The `skip_last_loop` instructions are here so I can `bne` into `process_next_bytecode`
    _bc__skip_last_loop__impl
.endproc


; IN: A = pitch offset per tick
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_vibrato_depth_and_play_note
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A

    ; Read first byte of `_play_note`
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    ; fallthrough
    .assert PC == _bc__play_note
.endproc


; IN: A = note and key-off bit
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc _bc__play_note
    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    ; calculate pitch table index
    clrc
    adc A, channelSoA_instPitchOffset + X
    clrc
    adc A, channelSoA_transpose + X
    mov Y, A

    ; Calculate voice pitch
    mov A, [commonData.pitchTable_l] + Y
    clrc
    adc A, channelSoA_detune_l + X
    mov channelSoA_virtualChannels_pitch_l + X, A

    mov A, [commonData.pitchTable_h] + Y
    adc A, channelSoA_detune_h + X

    ; Fallthough
    .assert PC == _bc__play_note_set_pitch_h_and_read_length
.endproc


; IN: A = pitch_h virtual channel value
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc _bc__play_note_set_pitch_h_and_read_length
    mov channelSoA_virtualChannels_pitch_h + X, A

    ; Get note length from the next byte in the instructionPtr
    mov Y, #0
    mov A, [instructionPtr] + Y
    bne ByteLength
        ; u16be, 3 byte length
        incw instructionPtr

        mov A, [instructionPtr] + Y
        mov channelSoA_countdownTimer_h + X, A
        incw instructionPtr

        mov A, [instructionPtr] + Y
    ByteLength:

    incw instructionPtr

    mov channelSoA_countdownTimer_l + X, A


    ; Y = 0
    ; disable portamento
    mov channelSoA_portamento_direction + X, Y

    ; Reset early release min-tick counter.
    ; Y = 0
    .assert page(channelSoA_ticksAfterNote) != 0
    mov A, Y
    mov channelSoA_ticksAfterNote + X, A

    ; Restart vibrato (if active)
    ; Y = 0
    mov channelSoA_vibrato_direction + X, Y

    mov A, channelSoA_vibrato_tickCounterStart + X
    mov channelSoA_vibrato_tickCounter + X, A


    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        ; X = SFX channel

        ; Disable noise
        tclr1 pendingNon_sfx

        ; Queue KON if the previous note has been KOFFed
        and A, keyOnMask_sfx
        tset1 keyOnShadow_sfx

        ; Disable KON the next time the note is played (slur the note)
        tclr1 keyOnMask_sfx

        ; return (do not execute the next bytecode and sleep)
        ret


    MusicChannel:
        tclr1 pendingNon_music

        and A, keyOnMask_music
        tset1 keyOnShadow_music

        tclr1 keyOnMask_music

        ret
.endproc


; IN: A = pitch_l
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__play_pitch
    ; play_pitch does not detune the pitch.
    ; Done to ensure VxPITCH is exactly what the song author wants and to avoid a save/restore carry.

    mov channelSoA_virtualChannels_pitch_l + X, A

    ; read pitch_h and key-off flag
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    bra _bc__play_note_set_pitch_h_and_read_length
.endproc


; IN: A = freq_and_keyoff
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__play_noise
    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    and A, #DSP_FLG__NOISE_FREQ_MASK
    mov Y, A

    ; Reset early release min-tick counter.
    mov A, #0
    mov channelSoA_ticksAfterNote + X, A


    ; y = noiseFreq
    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        ; X is SFX channel

        mov A, ChannelVoiceBit + X

        ; Set noise frequency
        mov noiseFreq_sfx - FIRST_SFX_CHANNEL + X, Y

        ; Enable noise
        mov lastSfxPlayNoiseChannel, A
        tset1 pendingNon_sfx

        ; queue KON if the previous note has been KOFFed
        and A, keyOnMask_sfx
        tset1 keyOnShadow_sfx

        ; Disable KON the next time the note is played (slur the note)
        tclr1 keyOnMask_sfx

        jmp _bc__read_length


    MusicChannel:
        mov noiseFreq_music, Y

        tset1 pendingNon_music

        and A, keyOnMask_music
        tset1 keyOnShadow_music

        tclr1 keyOnMask_music

        jmp _bc__read_length
.endproc


; IN: X = channelIndex
; KEEP: X
.proc bc__keyon_next_note
    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        tset1 keyOnMask_sfx
        jmp process_next_bytecode

    MusicChannel:
        tset1 keyOnMask_music
        jmp process_next_bytecode
.endproc


; IN: A = note and key-off flag
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__portamento_down
    mov Y, #-2 & $ff

    ; fallthrough
    .assert PC == bc__portamento_up
.endproc


; IN: A = note and key-off flag
; IN: X = channelIndex
; IN: Y = 0 (up) or -2 (down)
; KEEP: X
.proc bc__portamento_up
    inc Y
    ; Y = 1 or -1
    mov channelSoA_portamento_direction + X, Y

    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    ; calculate pitch table index
    clrc
    adc A, channelSoA_instPitchOffset + X
    clrc
    adc A, channelSoA_transpose + X
    mov Y, A

    ; Calculate target pitch
    mov A, [commonData.pitchTable_l] + Y
    clrc
    adc A, channelSoA_detune_l + X
    mov channelSoA_portamento_target_l + X, A

    mov A, [commonData.pitchTable_h] + Y
    adc A, channelSoA_detune_h + X

    mov Y, #0

    ; fallthrough
    .assert PC == _bc__portamento__read_speed_length
.endproc


; Assumes portamento_direction and portamento_target_l set
;
; IN: A = portamento target_h
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__portamento__read_speed_length
    mov channelSoA_portamento_target_h + X, A

    ; Speed
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr
    mov channelSoA_portamento_speed + X, A

    ; Reset early release min-tick counter.
    ; Y = 0
    mov A, Y
    mov channelSoA_ticksAfterNote + X, A

    jmp _bc__read_length__y0
.endproc


; IN: A = portamento target_l
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__portamento_pitch_down
    mov Y, #-2 & $ff

    ; fallthrough
    .assert PC == bc__portamento_pitch_up
.endproc


; IN: A = portamento target_l
; IN: X = channelIndex
; IN: Y = 0 (up) or -2 (down)
; KEEP: X
.proc bc__portamento_pitch_up
    inc Y
    ; Y = 1 or -1
    mov channelSoA_portamento_direction + X, Y

    mov channelSoA_portamento_target_l + X, A

    ; target_h and key-off flag
    mov Y, #0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    ; A = target_h
    ; Y = 0 (required)

    bra _bc__portamento__read_speed_length
.endproc


; IN: A = target pitch_l
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__portamento_pitch_calc
_tmp = zpTmpWord
_tmp_l = zpTmpWord.l

    mov channelSoA_portamento_target_l + X, A
    mov _tmp_l, A

    ; target_h and key-off flag
    mov A, [instructionPtr] + Y
    incw instructionPtr

    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    ; a = target_h
    jmp _bc__portamento_calc__read_lengths
.endproc


; IN: A = note and key-off flag
; IN: X = channelIndex
; unknown Y
; KEEP: X
.proc bc__portamento_calc
_tmp = zpTmpWord
_tmp_l = zpTmpWord.l

    lsr A
    ; carry = key-off note bit
    ; write carry to the key-off flag
    ror channelSoA_keyoffMsbFlag + X

    ; calculate pitch table index
    clrc
    adc A, channelSoA_instPitchOffset + X
    clrc
    adc A, channelSoA_transpose + X
    mov Y, A

    mov A, [commonData.pitchTable_l] + Y
    clrc
    adc A, channelSoA_detune_l + X
    mov channelSoA_portamento_target_l + X, A
    mov _tmp_l, A

    mov A, [commonData.pitchTable_h] + Y
    adc A, channelSoA_detune_h + X

    ; fallthrough
    .assert PC == _bc__portamento_calc__read_lengths
.endproc


; IN: zpTmpWord.l = low byte of target
; IN: A = high byte of target
; IN: X = channelIndex
; unknown Y
; KEEP:X
.proc _bc__portamento_calc__read_lengths
_tmp = zpTmpWord
_tmp_l = zpTmpWord.l
_tmp_h = zpTmpWord.h

    mov channelSoA_portamento_target_h + X, A
    mov _tmp_h, A

    ; Reset early release min-tick counter.
    mov A, #0
    mov channelSoA_ticksAfterNote + X, A

    ; slide length in ticks
    mov Y, A
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    push X
        push A
            ; Set down direction
            ; Y = 0
            dec Y
            ; Y = $ff
            mov channelSoA_portamento_direction + X, Y

            mov A, channelSoA_virtualChannels_pitch_l + X
            mov Y, channelSoA_virtualChannels_pitch_h + X

            subw YA, _tmp
            bpl NotNegative
                ; switch to up direction
                lsr channelSoA_portamento_direction + X

                ; negate YA
                movw _tmp, YA
                mov A, #0
                mov Y, A
                subw YA, _tmp
            NotNegative:
        pop X

        div YA, X
        bvc NoOverflow
            ; ares states overflow set if quotient >= 256 (even if output is invalid)
            ; ::TODO test an other emulators and real hardware::
            mov A, #$ff
            bra EndIf

        NoOverflow:
            ; Set speed to 1 of quotient is 0
            ; Required for long portamentos and low octaves
            mov Y, A
            bne EndIf
                inc A
        EndIf:
    pop X

    mov channelSoA_portamento_speed + X, A

    ; fallthrough
    .assert PC == _bc__read_length
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc _bc__read_length
    mov Y, #0

    ; fallthrough
    .assert PC == _bc__read_length__y0
.endproc


; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__read_length__y0
    ; Y = 0
    mov A, [instructionPtr] + Y
    bne OneByteLength
        ; u16be, 3 byte length
        incw instructionPtr

        mov A, [instructionPtr] + Y
        mov channelSoA_countdownTimer_h + X, A
        incw instructionPtr

        mov A, [instructionPtr] + Y
    OneByteLength:

    incw instructionPtr
    mov channelSoA_countdownTimer_l + X, A

    ; return (do not execute the next bytecode and sleep)
    ret
.endproc


; IN: A = pitch offset per tick
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_vibrato
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A

    ; Y = 0
    mov A, Y
    bra _bc__set_vibrato_delay_and_wavelength
.endproc


; IN: A = pitch offset per tick
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_vibrato_with_delay
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A

    ; Read vibrato-delay-in-ticks parameter
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    ; fallthrough
    .assert PC == _bc__set_vibrato_delay_and_wavelength
.endproc


; IN: A = vibrato delay
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__set_vibrato_delay_and_wavelength
    mov channelSoA_vibrato_delay + X, A

    ; Read quarterWavelengthInTicks parameter
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    ; Rest vibrato direction
    ; Y = 0
    mov channelSoA_vibrato_direction + X, Y

    mov channelSoA_vibrato_tickCounterStart + X, A
    mov channelSoA_vibrato_tickCounter + X, A

    asl A
    mov channelSoA_vibrato_halfWavelength + X, A

    jmp process_next_bytecode
.endproc


; IN: A = loop count
; IN: X = channel Index
; IN: Y = 0
; KEEP: X
.proc bc__start_loop
_loopCount = zpTmp

    mov _loopCount, A

    mov A, channelSoA_stackPointer + X
    setc
    sbc A, #3

    cmp A, BcStackIndexesEnd + X
    bcc bc__disable_channel


    mov channelSoA_stackPointer + X, A
    mov channelSoA_loopStackPointer + X, A


    mov Y, A

    mov A, _loopCount
    mov bcStack + 0 + Y, A

    mov A, instructionPtr.l
    mov bcStack + 1 + Y, A

    mov A, instructionPtr.h
    mov bcStack + 2 + Y, A


    jmp process_next_bytecode
.endproc


; IN: zpTmpWord = bytes to skip
; IN: X = channelIndex
; KEEP: X
; CAUTION: Will always jump
.inline _bc__skip_last_loop__impl
_bytesToSkip = zpTmpWord

    ; No need to bounds check `loopStackPointer`, it is always in bounds.
    mov Y, channelSoA_loopStackPointer + X

    mov A, bcStack+Y  ; loop counter
    dec A
    bne process_next_bytecode

    ; Assumes `y + 3` is a valid stackPointer
    mov A, Y
    clrc
    adc A, #3
    mov channelSoA_stackPointer + X, A

    cmp A, ValidLoopStackPointerTable + X
    bcs InvalidSp
        ; Only write to loopStackPointer if it is valid
        mov channelSoA_loopStackPointer + X, A
    InvalidSp:

    movw YA, _bytesToSkip
    addw YA, instructionPtr

    jmp process_bytecode_with_loader_test
.endinline


; IN: X = channelIndex
; KEEP: X
.proc bc__end_loop
    ; No need to bounds check `loopStackPointer`, it is always in bounds.
    mov Y, channelSoA_loopStackPointer + X

    mov A, bcStack + 0 + Y  ; loop counter
    dec A
    beq LastLoop
        mov bcStack + 0 + Y, A  ; loop counter

        mov A, bcStack + 1 + Y
        mov instructionPtr.l, A

        mov A, bcStack + 2 + Y
        mov instructionPtr.h, A

        jmp process_next_bytecode


    LastLoop:
        ; Assumes `y + 3` is a valid stackPointer
        mov A, Y
        clrc
        adc A, #3
        mov channelSoA_stackPointer + X, A

        cmp A, ValidLoopStackPointerTable + X
        bcs InvalidSp
            ; Only write to loopStackPointer if it is valid
            mov channelSoA_loopStackPointer + X, A
        InvalidSp:

        jmp process_next_bytecode
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__disable_channel
    ; Y may not be 0

    mov A, #$ff
    mov channelSoA_countdownTimer_h + X, A

    inc A
    ; A = 0

    ; No bytecode is processed if instructionPtr_h is 0
    mov instructionPtr.h, A

    ; Disable portamento
    mov channelSoA_portamento_direction + X, A

    ; Disable vibrato
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A


    ; Determine if this is a music or sound effect channel.
    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        ; X is a sound effect channel

        ; Send key-off event
        tset1 keyOffShadow_sfx

        ; Disable echo
        tclr1 eonShadow_sfx

        ; Not disabling noise.
        ; `pendingNon_sfx` will be cleared on the tick after Key-Off

        ; Disable sfx S-DSP writes.
        tclr1 activeSoundEffects

        ; Clear `sfx_oneChannelSfxId` so the sfx can be restarted if it is not interruptible.
        mov A, #$ff
        mov sfx_oneChannelSfxId - N_MUSIC_CHANNELS + X, A

        ; return (do not execute the next bytecode and sleep)
        ret


    MusicChannel:
        ; X is a music channel

        ; Send key-off event
        tset1 keyOffShadow_music

        ; return (do not execute the next bytecode and sleep)
        ret
.endproc


; IN: A = low byte of i16 offset
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__goto_relative
    push A

    ; Y = 0
    mov A, [instructionPtr] + Y
    mov Y, A

    pop A

    addw YA, instructionPtr

    jmp process_bytecode_with_loader_test
.endproc


; IN: A = subroutine Id
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__call_subroutine_and_disable_vibrato
    ; Disable vibrato
    ; Y = 0
    mov channelSoA_vibrato_pitchOffsetPerTick + X, Y

    ; fallthrough
    .assert PC == bc__call_subroutine
.endproc


; IN: A = subroutine Id
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__call_subroutine
_subroutineId = zpTmp

    mov _subroutineId, A

    mov A, channelSoA_stackPointer + X
    dec A
    dec A
    cmp A, BcStackIndexesEnd + X
    bcc bc__disable_channel

    mov channelSoA_stackPointer + X, A


    mov Y, A

    mov A, instructionPtr.l
    mov bcStack + 0 + Y, A

    mov A, instructionPtr.h
    mov bcStack + 1 + Y, A


    ; Add songPtr to subroutineTable[y] to get the subroutine address
    mov Y, _subroutineId

    cmp X, #FIRST_SFX_CHANNEL
    bcs SfxSubroutine
        ; carry clear

        mov A, [subroutineTable_l] + Y
        adc A, songPtr.l
        mov instructionPtr.l, A

        mov A, [subroutineTable_h] + Y
        adc A, songPtr.h
        mov instructionPtr.h, A

        jmp process_next_bytecode


    SfxSubroutine:
        mov A, [commonData.sfxSubroutines_l] + Y
        mov instructionPtr.l, A

        mov A, [commonData.sfxSubroutines_h] + Y
        mov instructionPtr.h, A

        jmp process_next_bytecode
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__return_from_subroutine_and_disable_vibrato
    ; Disable vibrato
    mov A, #0
    mov channelSoA_vibrato_pitchOffsetPerTick + X, A

    ; fallthrough
    .assert PC == bc__return_from_subroutine
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__return_from_subroutine
    mov A, channelSoA_stackPointer + X
    inc A
    cmp A, BcStackIndexesStart + X
    bcs bc__disable_channel
    inc A

    mov channelSoA_stackPointer + X, A

    cmp A, ValidLoopStackPointerTable + X
    bcs InvalidSp
        ; Only write to loopStackPointer if it is valid
        mov channelSoA_loopStackPointer + X, A
    InvalidSp:

    mov Y, A

    mov A, bcStack - 2 + Y
    mov instructionPtr.l, A

    mov A, bcStack - 1 + Y
    mov instructionPtr.h, A

    jmp process_next_bytecode
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__enable_echo
    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        tset1 eonShadow_sfx
        jmp process_next_bytecode

    MusicChannel:
        tset1 eonShadow_music
        jmp process_next_bytecode
.endproc


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__disable_echo
    mov A, ChannelVoiceBit + X

    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        tclr1 eonShadow_sfx
        jmp process_next_bytecode

    MusicChannel:
        tclr1 eonShadow_music
        jmp process_next_bytecode
.endproc


; IN: A = instrument ID
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_instrument
    ; disable temp GAIN
    ; Y = 0
    mov channelSoA_virtualChannels_tempGain + X, Y

    mov Y, A

    mov A, [commonData.instruments_pitchOffset] + Y
    mov channelSoA_instPitchOffset + X, A

    mov A, [commonData.instruments_scrn] + Y
    mov channelSoA_virtualChannels_scrn + X, A

    mov A, [commonData.instruments_adsr1] + Y
    mov channelSoA_virtualChannels_adsr1 + X, A

    mov A, [commonData.instruments_adsr2OrGain] + Y
    mov channelSoA_virtualChannels_adsr2OrGain + X, A

    jmp process_next_bytecode
.endproc


; IN: A = instrument ID
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_instrument_and_adsr_or_gain
    ; disable temp GAIN
    mov channelSoA_virtualChannels_tempGain + X, Y

    mov Y, A

    mov A, [commonData.instruments_pitchOffset] + Y
    mov channelSoA_instPitchOffset + X, A

    mov A, [commonData.instruments_scrn] + Y
    mov channelSoA_virtualChannels_scrn + X, A


    ; Read ADSR/GAIN from `instructionPtr`.
    mov Y, #0
    mov A, [instructionPtr] + Y
    mov channelSoA_virtualChannels_adsr1 + X, A
    incw instructionPtr

    mov A, [instructionPtr] + Y
    mov channelSoA_virtualChannels_adsr2OrGain + X, A
    incw instructionPtr

    jmp process_next_bytecode
.endproc


; IN: A = adsr1
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_adsr
    ; disable temp GAIN
    mov channelSoA_virtualChannels_tempGain + X, Y

    mov channelSoA_virtualChannels_adsr1 + X, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    mov channelSoA_virtualChannels_adsr2OrGain + X, A
    incw instructionPtr

    jmp process_next_bytecode
.endproc


; IN: A = gain
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_gain
    ; disable temp GAIN
    mov channelSoA_virtualChannels_tempGain + X, Y

    ; Y = 0
    mov channelSoA_virtualChannels_adsr1 + X, Y

    mov channelSoA_virtualChannels_adsr2OrGain + X, A

    jmp process_next_bytecode
.endproc


; IN: A = cmp
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_early_release_no_minimum
    mov channelSoA_earlyRelease_cmp + X, A

    ; Y = 0
    mov A, Y
    mov channelSoA_earlyRelease_minTicks + X, A

    bra _bc__set_early_release_gain
.endproc


; IN: A = cmp
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_early_release
    mov channelSoA_earlyRelease_cmp + X, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr
    mov channelSoA_earlyRelease_minTicks + X, A

    ; fallthrough
    .assert PC == _bc__set_early_release_gain
.endproc


; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__set_early_release_gain
    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    mov channelSoA_earlyRelease_gain + X, A

    jmp process_next_bytecode
.endproc


; IN: A = adjust
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__adjust_transpose
    clrc
    adc A, channelSoA_transpose + X

    ; fallthrough
    .assert PC == bc__set_transpose
.endproc


; IN: A = transpose
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_transpose
    mov channelSoA_transpose + X, A

    jmp process_next_bytecode
.endproc


; IN: A = detune_l
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_detune_i16
    mov channelSoA_detune_l + X, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    mov channelSoA_detune_h + X, A

    jmp process_next_bytecode
.endproc


; IN: A = detune_l
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_detune_n8
    mov Y, #$ff

    ; fallthrough
    .assert PC == bc__set_detune_p8
.endproc


; IN: A = detune_l
; IN: Y = 0 or $ff (detune_h)
; IN: X = channelIndex
; KEEP: X
.proc bc__set_detune_p8
    mov channelSoA_detune_l + X, A

    mov A, Y
    mov channelSoA_detune_h + X, A

    jmp process_next_bytecode
.endproc


; IN: A = delta
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__adjust_pan
    clrc

    mov Y, A
    bpl Positive
        ; carry clear, A is negative
        adc A, channelSoA_pan + X
        bcs InRange
            mov A, #0
        InRange:
        bra EndIf


    Positive:
        ; carry clear, A is positive
        clrc
        adc A, channelSoA_pan + X

        bcs SetMax
            cmp A, #MAX_PAN + 1
            bcc EndIf
            SetMax:
                mov A, #MAX_PAN
    EndIf:

    ; fallthrough
    .assert PC == bc__set_pan
.endproc


; IN: A = pan
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__set_pan
    mov channelSoA_pan + X, A

    ; Disable pan effects
    ; (Y is non-zero)
    mov A, #0
    mov channelSoA_panEffect_direction + X, A

    set1 volShadowDirty_tmp, 7

    jmp process_next_bytecode
.endproc


; IN: A = pan
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_pan_and_volume
    ; Y is 0

    mov channelSoA_pan + X, A

    ; Disable pan effects
    .assert page(channelSoA_panEffect_direction) != 0
    ; Y = 0
    mov A, Y
    mov channelSoA_panEffect_direction + X, A

    mov A, [instructionPtr] + Y
    incw instructionPtr

    bra bc__set_volume
.endproc


; IN: A = adjust
; IN: X = channelIndex
; IN: Y = 0
; KEEP:X
.proc bc__adjust_volume
    clrc

    mov Y, A
    bpl Positive
        ; carry clear, A is negative
        adc A, channelSoA_volume + X
        bcs InRange
            mov A, #0
        InRange:
        bra EndIf


    Positive:
        ; carry clear, A is positive
        adc A, channelSoA_volume + X
        bcc EndIf
            mov A, #$ff
        EndIf:

    ; fallthrough
    .assert PC == bc__set_volume
.endproc


; IN: A = volume
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__set_volume
    mov channelSoA_volume + X, A

    set1 volShadowDirty_tmp, 7

    ; Disable volume effects
    ; (Y is non-zero)
    mov A, #0
    mov channelSoA_volEffect_direction + X, A

    jmp process_next_bytecode
.endproc


; IN: A = flags
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_channel_or_echo_invert
    .assert INVERT_FLAGS__MONO_BIT != 0

    ; Have to bit-shift parameter, the spc700 has no `bit` instruction.
    asl A

    ; If not in surround mode, set all flags to the mono-flag
    bbs loaderDataType, LoaderDataType__SURROUND_FLAG_BIT, Surround
        ; MUST NOT modify carry

        and A, #INVERT_FLAGS__MONO
        beq NoMonoInvert
            mov A, #$7f << 1
        NoMonoInvert:
    Surround:

    bcc SetEchoInvert
        ; set channel invert
        mov channelSoA_invertFlags + X, A
        set1 volShadowDirty_tmp, 7

        jmp process_next_bytecode


    SetEchoInvert:
        ; set echo invert
        mov echo.invertFlags, A
        set1 echoDirty, ECHO_DIRTY__VOLUME_BIT

        jmp process_next_bytecode
.endproc


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__volume_slide_up
    .assert inFirstpage(channelSoA_volEffect_counter)

    setp
.p1
    mov channelSoA_volEffect_counter + X, A

    mov A, #VOL_PAN_EFFECT_SLIDE_UP
    ; Y (sub_volume) = 0
    bra __bc__volume_slide__dp_true__
.endproc


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.p0
.proc bc__volume_slide_down
    .assert inFirstpage(channelSoA_volEffect_counter)

    setp
.p1
    mov channelSoA_volEffect_counter + X, A

    mov A, #VOL_PAN_EFFECT_SLIDE_DOWN
    dec Y
    ; Y (sub_volume) = $ff

    ; fallthrough
    .assert PC == __bc__volume_slide__dp_true__
.endproc


; IN: A = direction
; IN: Y = subVolume
; IN: X = channelIndex
; direct_page = true
; KEEP: X
.p1
.proc __bc__volume_slide__dp_true__
    .assert inFirstpage(channelSoA_volEffect_direction)

    mov channelSoA_volEffect_direction + X, A

    ; halfWavelength
    mov A, #0

    ; fallthrough
    .assert PC == __bc__read_vol_effect_offset__dp_true__
.endproc


; IN: A = halfWavelength
; IN: X = channelIndex
; IN: Y = subVolume
; direct_page = true
; KEEP: X
; OUT: direct_page = false
.p1
.proc __bc__read_vol_effect_offset__dp_true__
    .assert inFirstpage(channelSoA_volEffect_halfWavelength)
    .assert inFirstpage(channelSoA_subVolume)

    mov channelSoA_volEffect_halfWavelength + X, A
    mov channelSoA_subVolume + X, Y

    clrp
.p0

    mov Y, #0

    mov A, [instructionPtr] + Y
    mov channelSoA_volEffect_offset_l + X, A
    incw instructionPtr

    mov A, [instructionPtr] + Y
    mov channelSoA_volEffect_offset_h + X, A
    incw instructionPtr

    jmp process_next_bytecode
.endproc


; IN: A = quarter waveLength
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__tremolo
    .assert inFirstpage(channelSoA_volEffect_counter)
    .assert inFirstpage(channelSoA_volEffect_direction)

    setp
.p1

    mov Y, #VOL_PAN_EFFECT_TRIANGLE_UP
    mov channelSoA_volEffect_direction + X, Y

    mov channelSoA_volEffect_counter + X, A

    ; halfWavelength
    asl A

    ; subVolume
    mov Y, #$7f

    bra __bc__read_vol_effect_offset__dp_true__
.endproc
.p0


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__pan_slide_up
    .assert inFirstpage(channelSoA_panEffect_counter)

    setp
.p1

    mov channelSoA_panEffect_counter+X, A

    mov A, #VOL_PAN_EFFECT_SLIDE_UP
    ; Y (subPan) = 0
    bra __bc__pan_slide__dp_true__
.endproc
.p0


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__pan_slide_down
    .assert inFirstpage(channelSoA_panEffect_counter)

    setp
.p1

    mov channelSoA_panEffect_counter+X, A

    mov A, #VOL_PAN_EFFECT_SLIDE_DOWN
    dec Y
    ; Y (subPan) = $ff

    ; fallthrough
    .assert PC == __bc__pan_slide__dp_true__
.endproc
.p0


; IN: A = direction
; IN: Y = subPan
; IN: X = channelIndex
; direct_page = true
; KEEP: X
.p1
.proc __bc__pan_slide__dp_true__
    .assert inFirstpage(channelSoA_panEffect_direction)

    mov channelSoA_panEffect_direction + X, A

    ; halfWavelength
    mov A, #0

    ; fallthrough
    .assert PC == __bc__read_pan_effect_offset__dp_true__
.endproc
.p0


; IN: A = halfWavelength
; IN: Y = subPan
; IN: X = channelIndex
; direct_page = true
; KEEP: X
; OUT: direct_page = false
.p1
.proc __bc__read_pan_effect_offset__dp_true__
    .assert inFirstpage(channelSoA_panEffect_halfWavelength)
    .assert inFirstpage(channelSoA_subPan)

    mov channelSoA_panEffect_halfWavelength + X, A
    mov channelSoA_subPan + X, Y

    clrp
.p0

    mov Y, #0

    mov A, [instructionPtr] + Y
    mov channelSoA_panEffect_offset_l + X, A
    incw instructionPtr

    mov A, [instructionPtr] + Y
    mov channelSoA_panEffect_offset_h + X, A
    incw instructionPtr

    jmp process_next_bytecode
.endproc


; IN: A = quarter waveLength
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__panbrello
    .assert inFirstpage(channelSoA_panEffect_counter)
    .assert inFirstpage(channelSoA_panEffect_direction)

    setp
.p1

    mov Y, #VOL_PAN_EFFECT_TRIANGLE_UP
    mov channelSoA_panEffect_direction + X, Y

    mov channelSoA_panEffect_counter + X, A

    ; halfWavelength
    asl A

    ; subPan
    mov Y, #$7f
    bra __bc__read_pan_effect_offset__dp_true__
.endproc
.p0


; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__reuse_temp_gain
    mov A, channelSoA_prevTempGain + X
    mov channelSoA_virtualChannels_tempGain + X, A

    jmp process_next_bytecode
.endproc


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__reuse_temp_gain_and_wait
    mov Y, A

    mov A, channelSoA_prevTempGain + X
    mov channelSoA_virtualChannels_tempGain + X, A

    mov A, Y
    bra bc__wait
.endproc


; IN: A = ticks
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__reuse_temp_gain_and_rest
    mov Y, A

    mov A, channelSoA_prevTempGain + X
    mov channelSoA_virtualChannels_tempGain + X, A

    mov A, Y

    bra bc__rest
.endproc


; IN: A = tempGain
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_temp_gain
    mov channelSoA_virtualChannels_tempGain + X, A
    mov channelSoA_prevTempGain + X, A

    jmp process_next_bytecode
.endproc


; IN: A = tempGain
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_temp_gain_and_wait
    mov channelSoA_virtualChannels_tempGain + X, A
    mov channelSoA_prevTempGain + X, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    bra bc__wait
.endproc


; IN: A = tempGain
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_temp_gain_and_rest
    mov channelSoA_virtualChannels_tempGain + X, A
    mov channelSoA_prevTempGain + X, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    ; fallthrough
    .assert PC == bc__rest
.endproc


; IN: A = length
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__rest
    ; Y is not 0

    ; Set key-off flag
    mov Y, #$80
    mov channelSoA_keyoffMsbFlag + X, Y

    ; fallthrough
    .assert PC == bc__wait
.endproc


; IN: A = length
; IN: X = channelIndex
; Y unknown
; KEEP: X
.proc bc__wait
    ; Y is not 0

    mov Y, A
    bne OneByteLength
        ; u16be length
        ; Y = 0
        mov A, [instructionPtr] + Y
        incw instructionPtr
        mov channelSoA_countdownTimer_h + X, A

        mov A, [instructionPtr] + Y
        incw instructionPtr
    OneByteLength:

    mov channelSoA_countdownTimer_l + X, A

    ; return (do not execute the next bytecode and sleep)
    ret
.endproc


; IN: A = echo volume
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_echo_volume
    and A, #$7f
    mov echo.echoVolume_l, A

    bra _bc__set_stereo_echo_volume_r
.endproc


; IN: A = left echo volume
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_stereo_echo_volume
    and A, #$7f
    mov echo.echoVolume_l, A

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    and A, #$7f

    ; fallthrough
    .assert PC == _bc__set_stereo_echo_volume_r
.endproc


; IN: A = right echo volume
; IN: X = channelIndex
; IN: Y = 0
; CAUTION: right is unchecked
; KEEP: X
.proc _bc__set_stereo_echo_volume_r
    mov echo.echoVolume_r, A

    set1 echoDirty, ECHO_DIRTY__VOLUME_BIT

    jmp process_next_bytecode
.endproc


; IN: A = adjust
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__adjust_echo_volume
    push A

    ; Y = 0
    call _bc__adjust_echo_volume_channel

    pop A
    bra _bc__adjust_stereo_echo_volume_r
.endproc


; IN: A = left adjust
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__adjust_stereo_echo_volume
    ; Y = 0
    call _bc__adjust_echo_volume_channel

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    ; fallthrough
    .assert PC == _bc__adjust_stereo_echo_volume_r
.endproc


; IN: A = right adjust
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__adjust_stereo_echo_volume_r
    inc Y
    call _bc__adjust_echo_volume_channel

    set1 echoDirty, ECHO_DIRTY__VOLUME_BIT

    jmp process_next_bytecode
.endproc


; IN: A = adjust
; IN: Y = echo volume channel
; IN: X = channelIndex
; KEEP: Y
; KEEP: X
.proc _bc__adjust_echo_volume_channel
    ; Assumes _echoVolume_l[y] is <= 127

    clrc
    adc A, echo.echoVolume_l + Y

    bpl EndIf
        mov A, #0
        bvc EndIf
            mov A, #127
    EndIf:

    mov echo.echoVolume_l + Y, A

    ; Return (NOT A BYTECODE INSTRUCTION)
    ret
.endproc


; IN: A = fir0
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_fir_filter
    mov echo.firFilter + 0, A

    mov Y, #6
    Loop:
        mov A, [instructionPtr] + Y
        mov echo.firFilter + 1 + Y, A
        dec Y
        bpl Loop

    inc Y
    mov A, #7
    ; Y = 0
    addw YA, instructionPtr

    or echoDirty, #(1 << ECHO_DIRTY__FIR_FILTER_BIT) | (1 << ECHO_DIRTY__CLEAR_FIR_BIT)

    jmp process_bytecode
.endproc


; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__set_or_adjust_echo_i8__out_of_bounds
    mov A, #MAX_SET_OR_ADJUST_ECHO_I8_PARAM

    ; fallthrough
    .assert PC == bc__set_or_adjust_echo_i8
.endproc


; IN: A = eIndexAndSetFlag
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__set_or_adjust_echo_i8
    .assert echo.firFilter + ECHO_I8_EFB_INDEX == echo.echoFeedback
    .assert MAX_SET_OR_ADJUST_ECHO_I8_PARAM >> 1 == ECHO_I8_EFB_INDEX

    cmp A, #MAX_SET_OR_ADJUST_ECHO_I8_PARAM + 1
    bcs _bc__set_or_adjust_echo_i8__out_of_bounds

    push X

    lsr A
    mov X, A

    ; carry = LSB of eIndexAndSetFlag

    ; Y = 0
    mov A, [instructionPtr] + Y
    incw instructionPtr

    bcs SetI8
        ; Adjust echo i8
        ; carry clear
        adc A, echo.firFilter + X
        bvc SetI8
            mov A, #I8_MIN & $ff
            bcs SetI8
                mov A, #I8_MAX
SetI8:
    mov echo.firFilter + X, A


    cmp X, #ECHO_I8_EFB_INDEX
    bcs FeedbackDirty
        set1 echoDirty, ECHO_DIRTY__FIR_FILTER_BIT

        pop X
        jmp process_next_bytecode


    FeedbackDirty:
        set1 echoDirty, ECHO_DIRTY__FEEDBACK_BIT

        pop X
        jmp process_next_bytecode
.endproc


; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc _bc__adjust_echo_i8_limit__out_of_bounds
    mov A, #ECHO_I8_EFB_INDEX

    ; fallthrough
    .assert PC == bc__adjust_echo_i8_limit
.endproc


; IN: A = echo index
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__adjust_echo_i8_limit
    .assert echo.firFilter + ECHO_I8_EFB_INDEX == echo.echoFeedback

    cmp A, #ECHO_I8_EFB_INDEX + 1
    bcs _bc__adjust_echo_i8_limit__out_of_bounds

    push X
    mov X, A

    ;   if adjust < 0 {
    ;      firFilter[x] = firFilter[x].saturating_add(adjust).max(limit)
    ;   } else {
    ;      firFilter[x] = firFilter[x].saturating_add(adjust).min(limit)
    ;   }

    ; Y = 0
    mov A, [instructionPtr] + Y       ; adjust
    bpl PositiveAdjust
        ; A is negative
        incw instructionPtr

        clrc
        adc A, echo.firFilter + X
        bvs WriteLimit

        mov echo.firFilter + X, A

        setc
        sbc A, [instructionPtr] + Y   ; limit
        bvc LimitNegativeTest
            eor A, #$ff
            bra LimitNegativeTest


    PositiveAdjust:
        ; A is positive
        incw instructionPtr

        clrc
        adc A, echo.firFilter + X
        bvs WriteLimit

        mov echo.firFilter + X, A

        ; using clc for a > comparison
        ; (SOURCE: http://6502.org/tutorials/compare_beyond.html#5.1)
        ;
        ; Using `bvs` to invert the negative flag and use the same negative branch instruction
        ; for both positive and negative adjust code paths.
        clrc
        sbc A, [instructionPtr] + Y     ; limit
        bvs LimitNegativeTest
            eor A, #$ff


    LimitNegativeTest:
        bpl End

        WriteLimit:
            mov A, [instructionPtr] + Y ; limit
            mov echo.firFilter + X, A
    End:

    incw instructionPtr


    cmp X, #ECHO_I8_EFB_INDEX
    bcs FeedbackDirty
        set1 echoDirty, ECHO_DIRTY__FIR_FILTER_BIT

        pop X
        jmp process_next_bytecode


    FeedbackDirty:
        set1 echoDirty, ECHO_DIRTY__FEEDBACK_BIT

        pop X
        jmp process_next_bytecode
.endproc


; IN: A = instruction argument
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.proc bc__miscellaneous
    lsr A

    bcc Bit0Clear
        lsr A

        ; Y = 0
        mov A, [instructionPtr] + Y
        incw instructionPtr

        bcs SetEchoDelay
            .assert SET_SONG_TICK_CLOCK_OPCODE == %01
            mbc__set_song_tick_clock
            jmp process_next_bytecode

        SetEchoDelay:
            .assert SET_ECHO_DELAY_OPCODE == %11
            mbc__set_echo_delay
            jmp process_next_bytecode


    Bit0Clear:
        .assert DISABLE_NOISE_OPCODE == 0
        bne PMod
            mbc__disable_noise__inline
            jmp process_next_bytecode

        PMod:
            .assert DISABLE_PMOD_OPCODE & 1 == 0
            .assert ENABLE_PMOD_OPCODE & 1 == 0
            lsr A
            mbc__enable_or_disable_pmod
            jmp process_next_bytecode
.endproc


; IN: A = edl
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.inline mbc__set_echo_delay
    cmp A, maxEdl
    beq EndIf
    bcc EndIf
        mov A, maxEdl
    EndIf:

    mov echo.edl, A

    set1 echoDirty, ECHO_DIRTY__EDL_BIT
.endinline


; Sets the timer for the music channels
;
; IN: A = timer clock
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.inline mbc__set_song_tick_clock
    ; if (a > 0 && a < MIN_TICK_CLOCK) a = MIN_TICK_CLOCK
    dec A
    cmp A, #MIN_TICK_CLOCK - 1
    bcs EndIf
        mov A, #MIN_TICK_CLOCK - 1
    EndIf:

    inc A

    mov T0TARGET, A
.endinline


; IN: C = enable if set
; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.inline mbc__enable_or_disable_pmod
    mov A, ChannelVoiceBit + X
    and A, #ENABLE_PMOD_MASK

    tclr1 pmonShadow

    bcc Skip
        tset1 pmonShadow
    Skip:
.endinline


; IN: X = channelIndex
; IN: Y = 0
; KEEP: X
.inline mbc__disable_noise__inline
    mov A, ChannelVoiceBit + X
    cmp X, #FIRST_SFX_CHANNEL
    bcc MusicChannel
        tclr1 pendingNon_sfx
        jmp process_next_bytecode

    MusicChannel:
        tclr1 pendingNon_music
        jmp process_next_bytecode
.endinline


bc_ReservedForCustomUse = bc__disable_channel


BytecodeInstructionTable:
    .functiontable INSTRUCTIONS_WITH_ARGUMENTS
BytecodeInstructionTable__NoArguments:
    .functiontable NO_ARGUMENT_INSTRUCTIONS
BytecodeInstructionTable_End:

.assert BytecodeInstructionTable__NoArguments - BytecodeInstructionTable == FIRST_NO_ARGUMENT_INSTRUCTION_OPCODE * 2
.assert BytecodeInstructionTable_End - BytecodeInstructionTable == FIRST_PLAY_NOTE_INSTRUCTION * 2



; Data Tables
; ===========

; The bit used for global DSP registers (KON, KOFF, NON, EON, etc)
ChannelVoiceBit:
    .db 1<<0, 1<<1, 1<<2, 1<<3, 1<<4, 1<<5, 1<<6, 1<<7, 1<<6, 1<<7
.assert PC - ChannelVoiceBit == N_CHANNELS


; The address of the last writeable voice DSP register for each channel
LastVoiceRegister:
    .db $07, $17, $27, $37, $47, $57, $67, $77, $67, $77
.assert PC - LastVoiceRegister == N_CHANNELS


; Channel Stack Pointer Offsets
BcStackIndexesEnd:
    .db  BC_CHANNEL_STACK_OFFSET
BcStackIndexesStart:
    .dbrepeat i in 0..N_CHANNELS, (i + 1) * BC_CHANNEL_STACK_SIZE + BC_CHANNEL_STACK_OFFSET
.assert PC - BcStackIndexesStart == N_CHANNELS
.assert BcStackIndexesEnd + 1 == BcStackIndexesStart


; Used to determine if `loopStackPointer` is valid.
; Equal to `BcStackIndexesStart[x] - 3 + 1` (+1 for < comparison)
ValidLoopStackPointerTable:
    .dbrepeat i in 0..N_CHANNELS, (i + 1) * BC_CHANNEL_STACK_SIZE + BC_CHANNEL_STACK_OFFSET - 3 + 1
.assert PC - ValidLoopStackPointerTable == N_CHANNELS

