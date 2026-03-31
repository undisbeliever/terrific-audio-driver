; Audio driver bytecode instruction definitions
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


MAX_PAN = 128

N_NOTE_OCTAVES = 8

N_PLAY_NOTE_INSTRUCTIONS = N_NOTE_OCTAVES * 12 * 2

FIRST_PLAY_NOTE_INSTRUCTION = 64
FIRST_NO_ARGUMENT_INSTRUCTION_OPCODE = 56

; The last not-note instruction is the `disable_channel` instruction
; `disable_channel` will also send a KEY-OFF event.
DISABLE_CHANNEL_BYTECODE = FIRST_PLAY_NOTE_INSTRUCTION - 1


; Opcodes >= FIRST_PLAY_NOTE_INSTRUCTION are a two-byte play note instruction.
;
;  nnnnnnnO <length>
;      nnnnnnn = note to play (number of semitones above c0 + FIRST_PLAY_NOTE_INSTRUCTION / 2)
;            O = key-off note
;
;  NOTE: This instruction will wait an extra tick if the `key-off` bit is set.
;
;  CAUTION: play-note instructions are offset by `FIRST_PLAY_NOTE_INSTRUCTION`.


; The <length> format is 1 or 3 bytes long.
;  * [ 1..=255 ] - 1 byte read  - 8 bit length (1-255 ticks)
;  * [ 0, h, l ] - 3 bytes read - 16 bit length, big-endian (1-65535 ticks)
;
; v0.2.0 BREAKING CHANGE: A instruction that will key-off no-longer sleeps for an extra tick.
;
; A length of 1 is invalid if the instruction has the keyOff flag set.
;
; [0, 0, 0] is an invalid length.


; These bytecodes do have parameters
.ftdef INSTRUCTIONS_WITH_ARGUMENTS
    ; This opcode is currently reserved for custom extensions.
    ; (It currently disables the channel)
    bc_ReservedForCustomUse

    ; Miscellaneous instructions with two byte opcodes.
    ; See `MISC_INSTRUCTIONS`.
    bc__miscellaneous

    ; parameters: nnnnnnnO ssssssss <length>
    ;      nnnnnnn = note to play (same note format as a play-note instruction)
    ;            O = key-off note
    ;     ssssssss = u8 speed (VxPITCH change per tick)
    ;
    ; NOTE: portamento does not emit a KON event
    bc__portamento_down
    bc__portamento_up

    ; parameters: nnnnnnnO ssssssss <length>
    ;      nnnnnnn = note to play (same note format as a play-note instruction)
    ;            O = key-off note
    ;     ssssssss = slide length in ticks
    ;
    ; Slide length can be > note length.
    ;
    ; NOTE: portamento does not emit a KON event
    bc__portamento_calc


    ; parameters: pppppppp pppppppD ssssssss <length>
    ;     -pppppp- pppppppp = target VxPITCH (lower bit in the high byte is not pitch)
    ;                     O = key-off note
    ;              ssssssss = u8 speed (VxPITCH change per tick)
    ;
    ; CAUTION: Detune has no effect on portamento_pitch instructions.
    bc__portamento_pitch_down
    bc__portamento_pitch_up

    ; parameters: pppppppp pppppppD ssssssss <length>
    ;     -pppppp- pppppppp = target VxPITCH (lower bit in the high byte is not pitch)
    ;                     O = key-off note
    ;              ssssssss = slide length in ticks
    ;
    ; Slide length can be > note length.
    ;
    ; CAUTION: Detune has no effect on portamento_pitch instructions.
    bc__portamento_pitch_calc


    ; parameters: pppppppp tttttttt
    ;      pppppppp = pitch offset per tick.
    ;      tttttttt = quarter wavelength ticks.  MUST BE >= 1 and <= 128.
    ;
    ; If pppppppp is 0, vibrato is disabled.
    ;
    ; NOTES:
    ;  * This instruction will set the vibrato delay to 0 ticks
    ;  * The total wavelength of the vibrato is `tttttttt * 4`.
    ;  * If `tttttttt` is > 128, the vibrato is unbalanced.
    ;  * Vibrato has no range checking and may overflow or underflow the PITCH register.
    ;  * Portamento overrides vibrato
    ;  * Vibrato is disabled when calling or returning from a subroutine
    bc__set_vibrato


    ; parameters: pppppppp dddddddd tttttttt
    ;      pppppppp = pitch offset per tick.
    ;      dddddddd = vibrato delay in ticks (delays vibrato on every note)
    ;      tttttttt = quarter wavelength ticks.  MUST BE >= 1 and <= 128.
    ;
    ; See `set_vibrato`
    bc__set_vibrato_with_delay


    ; parameters: pppppppp nnnnnnnO <length>
    ;               pppppppp = pitch offset per tick.
    ;      nnnnnnnO <length> = note to play (same format as play_note)
    ;
    ; If pppppppp is 0, vibrato is disabled.
    ;
    ; NOTES:
    ;  * No note_id range checking
    ;  * Assumes vibrato quarter wavelength is set by `set_vibrato`.
    ;  * Vibrato quarter wavelength is not initialised on channel reset.
    ;  * Vibrato is disabled when calling or returning from a subroutine
    bc__set_vibrato_depth_and_play_note


    ; parameters: pppppppp pppppppO <length>
    ;      ppppppp- pppppppp = VxPITCH register value (be aware of the key-off bit in the high byte)
    ;                      O = key-off note
    ;
    ; CAUTION: Detune has no effect on play_pitch instructions.
    bc__play_pitch


    ; parameters: --fffffO <length>
    ;         fffff = noise frequency
    ;             O = key-off note
    bc__play_noise


    ; parameter = <length>
    ;
    ; Does not key-off the channel.
    bc__wait

    ; parameter = <length>
    ;
    ; Will key-off the channel
    bc__rest


    ; parameter = u8 instrument_id
    ;
    ; NOTE: There is no bounds-checking on instrument_id
    bc__set_instrument

    ; parameter = u8 instrument_id, u8 adsr1, u8 adsr2OrGain
    ;
    ; ADSR/Gain is the same format as `InstrumentsSoa`.
    ;
    ; If bit 7 of `adsr1` is set, `adsr2OrGain` is written to the ADSR2 register
    ; If bit 7 of `adsr1` is clear, ADSR is disabled and `adsr2OrGain` is written to the GAIN register.
    ;
    ; NOTE: There is no bounds-checking on instrument_id
    bc__set_instrument_and_adsr_or_gain


    ; parameter = u16 value to send to the ADSR register
    bc__set_adsr

    ; parameter = u8 value to send to the GAIN register
    ; NOTE: Disables ADSR
    bc__set_gain


    ; parameter = u8 value to temporarily send to the GAIN register
    ; NOTE: a temp-gain of 0 disables temp-gain.
    ; NOTE: Disables ADSR and GAIN envelope
    ; The envelope will be restored on key-off.
    bc__set_temp_gain

    ; parameters: gggggggg tttttttt
    ;  gggggggg = temp GAIN value
    ;  tttttttt = number of ticks to wait.
    ;
    ; See `set_temp_gain` and `wait`
    bc__set_temp_gain_and_wait

    ; parameters: gggggggg tttttttt
    ;  gggggggg = temp GAIN value
    ;  tttttttt = number of ticks to sleep before key-off
    ;
    ; See `set_temp_gain` and `rest`
    bc__set_temp_gain_and_rest

    ; parameter: <length>
    ;
    ; Reuses the temp-GAIN in the last `set_temp_gain*` instruction.
    ;
    ; See `wait`
    bc__reuse_temp_gain_and_wait

    ; parameter: <length>
    ;
    ; Reuses the temp-GAIN in the last `set_temp_gain*` instruction.
    ;
    ; See `rest`
    bc__reuse_temp_gain_and_rest


    ; parameter: u8 cmp, u8 minTicks, u8 gain
    ;  * cmp = number of ticks to remove + 1
    ;  * minTicks = minimum number of ticks before release
    ;  * gain = GAIN envelope to use on early release
    ;
    ; If `gain` is 0, the note will be key-offed on early release.
    ; If `gain` is non-zero, the envelope will be temporarily set to GAIN(`gain`) at early release.
    ;
    ; Early release is when `nextEventIsKeyOff && ticksAfterNote >= min && countdownTimer < cmp`.
    ;
    ; Early release is disabled if `cmp` is 0.
    ;
    ; CAUTION: If a note has a length less-than `cmp`, it will only be played for a single tick.
    bc__set_early_release


    ; parameter: u8 cmp, u8 gain
    ;  * cmp = number of ticks to remove + 1
    ;  * gain = GAIN envelope to use on early release
    ;
    ; This instruction is `set_early_release cmp, 0, gain`.
    bc__set_early_release_no_minimum


    ; parameter = i8 transpose
    bc__set_transpose

    ; parameter = i8 adjust
    bc__adjust_transpose


    ; parameter: i16 VxPITCH offset
    bc__set_detune_i16

    ; parameter: low byte of VxPITCH offset
    ; High byte of pitch_offset will be 0
    bc__set_detune_p8

    ; parameter: low byte of VxPITCH offset
    ; High byte of pitch_offset will be $ff
    bc__set_detune_n8


    ; parameter = i8 pan adjustment
    bc__adjust_pan

    ; parameter = pan (0-MAX_PAN, MAX_PAN/2 is centered)
    ;
    ; NOTE: There are no range checks on the pan value. A pan value > MAX_PAN will not pan correctly.
    bc__set_pan

    ; parameter: pppppppp vvvvvvvv
    ;      pppppppp = pan (0 - MAX_PAN, MAX_PAN/2 is centered)
    ;      vvvvvvvv = volume
    ;
    ; NOTE: There are no range checks on the pan value. A pan value > MAX_PAN will not pan correctly.
    bc__set_pan_and_volume

    ; parameter = i8 volume adjustment
    bc__adjust_volume

    ; parameter = u8 volume (0-255)
    bc__set_volume


    ; parameter = crl0000m
    ;  c = channel invert if set, echo invert if clear
    ;  r = invert right channel
    ;  l = invert left channel
    ;  m = invert both channels in mono mode
    bc__set_channel_or_echo_invert


    ; parameters: tttttttt oooooooo oooooooo
    ;      tttttttt          = number of ticks (0 is 256 ticks)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__volume_slide_up

    ; parameters: tttttttt oooooooo oooooooo
    ;      tttttttt          = number of ticks (0 is 256 ticks)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__volume_slide_down

    ; parameters: qqqqqqqq oooooooo oooooooo
    ;      qqqqqqqq          = quarter-wavelength in ticks (1 - 127)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__tremolo

    ; parameters: tttttttt oooooooo oooooooo
    ;      tttttttt          = number of ticks (0 is 256 ticks)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__pan_slide_up

    ; parameters: tttttttt oooooooo oooooooo
    ;      tttttttt          = number of ticks (0 is 256 ticks)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__pan_slide_down

    ; parameters: qqqqqqqq oooooooo oooooooo
    ;      qqqqqqqq          = quarter-wavelength in ticks (1 - 127)
    ;      oooooooo oooooooo = offset per tick (8.8 unsigned fixed point)
    bc__panbrello


    ; parameter = u8 number of times to loop
    ;
    ; CAUTION: disables channels if the stack is full
    bc__start_loop


    ; Breaks a loop early if the loop is on the last cycle
    ;  Used to implement the `:` token in a MML loop.
    ;
    ; ie. Allows for `[ abc : de ]3` loops, which will play `abc de abc de abc`.
    ;
    ; parameter = number of bytes in the bytecode to skip if loop counter == 1.
    ;
    ; CAUTION: Must be used in a loop.
    bc__skip_last_loop_u8

    ; same as `skip_last_loop_u8`, except the parameter is a big endian u16
    bc__skip_last_loop_u16be


    ; parameter = subroutine ID
    ;
    ; NOTE: There is no bounds checking
    bc__call_subroutine_and_disable_vibrato

    ; parameter = subroutine ID
    ; NOTE: There is no bounds checking
    bc__call_subroutine

    ; parameter = i16le offset
    ;
    ; Goto `instructionPtr + offset + 2`
    ;
    ; CAUTION: Not bounds checked
    bc__goto_relative


    ; parameter = u8 value
    ; value will be masked and used for both channels
    bc__set_echo_volume

    ; parameter = u8 left, u8 right
    ; Both channels will be masked
    bc__set_stereo_echo_volume

    ; parameter = i8 adjust
    bc__adjust_echo_volume

    ; parameter = i8 left, i8 right
    bc__adjust_stereo_echo_volume

    ; parameter = [i8 ; 8] FIR coefficients
    bc__set_fir_filter

    ; parameters: ---iiiis vvvvvvvv
    ;      iiii = index (0-7 = FIR filter tap, 8 = echo feedback)
    ;         s = 0 = adjust value, 1 = set value
    ;  vvvvvvvv = value
    ;
    ; If the first parameter byte is invalid (`> MAX_SET_OR_ADJUST_ECHO_I8_PARAM`)
    ; `set_or_adjust_echo_i8` will set echo feedback.
    bc__set_or_adjust_echo_i8

    ; parameter = u8 index, i8 adjust, i8 limit
    ;   if index < 8: sets FIR filter tap
    ;   if index >= 8: sets echo feedback
    ; comparison used will depend on the sign of adjust
    bc__adjust_echo_i8_limit
.endftdef


; These bytecodes do not have parameters
.ftdef NO_ARGUMENT_INSTRUCTIONS
    ; Jump to `start_loop` if `--loop_count != 0`
    ;
    ; CAUTION: Must be used in a loop.
    bc__end_loop


    ; Return from a call instruction and disabled vibrato
    bc__return_from_subroutine_and_disable_vibrato

    ; Return from a call instruction
    bc__return_from_subroutine


    ; Enable echo on this channel
    bc__enable_echo

    ; Disable echo on this channel
    bc__disable_echo


    ; Reuses the temp-GAIN in the last `set_temp_gain*` instruction.
    bc__reuse_temp_gain


    ; Sets the keyOnMask bits for the channel, allowing key-on without key-off
    ; if the channel is currently outputting sound.
    ;
    ; CAUTION: This instruction has no effect if the previous note has been key-offed.
    bc__keyon_next_note


    ; Disables the channel
    bc__disable_channel
.endftdef


; The `miscellaneous` bytecode extends the opcode range, similar to the `ED` opcode on the Z80.
;
; The `miscellaneous` is implemented by bitshifts.
; A dash (`-`) in the bitfield is ignored and `xxxxxx` is a non-zero value.
;
; Some miscellaneous instructions have arguments.
    ; %00000000
    ;
    ; Disable noise on this channel
    DISABLE_NOISE_OPCODE = %000000_00

    ; %xxxxxx00
    ;
    ; Disable pitch modulation (xxxxxx is non-zero)
    DISABLE_PMOD_OPCODE = %000001_00

    ; %------10
    ;
    ; Enable pitch modulation
    ;
    ; PMOD is only available on channels B - F.
    ENABLE_PMOD_OPCODE = %000000_10

    ; %------01
    ; parameter: u8 new timer0 value
    ;
    ; NOTE: Only sets the song timer (tempo).  The timer for the sound-effect channels is unchanged.
    SET_SONG_TICK_CLOCK_OPCODE = %000000_01

    ; %------11
    ; parameter: u8 EDL echo delay value
    ;
    ; If the parameter > maxEdl, maxEdl will be used instead
    SET_ECHO_DELAY_OPCODE = %000000_11


; Do not enable PMOD on channels G & H to prevent conflicts with the sound-effect channels.
ENABLE_PMOD_MASK = %00111110

ECHO_I8_EFB_INDEX = 8

MAX_SET_OR_ADJUST_ECHO_I8_PARAM = (ECHO_I8_EFB_INDEX << 1) | 1

