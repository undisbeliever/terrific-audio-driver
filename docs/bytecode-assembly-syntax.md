Bytecode Assembly Syntax
========================

The audio-engine and bytecode syntax is still in development and **subject to change without notice**.


Instructions
============

 * `rest <duration>` - Sleep for *duration-1* ticks then key-off for 1 tick.
    * CAUTION: A wait-rest chain should end with `rest 257` to prevent conflict with early-release.
 * `wait <duration>` - Sleep for *duration* ticks (no key-off).

 * `play_note <note> <duration>` - Play *note* for *duration* ticks then key-off the note
 * `play_note <note> <ko> <duration>` - Play *note* for *duration* ticks
 * `play_pitch <pitch> [ko] <duration>` - Set `VxPITCH` to *pitch* for *duration* ticks
 * `portamento <note> <ko> <pitch_velocity> <duration>` - Extend a slur into a portamento
    * Portamento instructions do not emit a key-on event.  You will need to slur the previous note before a portamento instruction.
 * `portamento_pitch <pitch> <ko> <pitch_velocity> <duration>` - Extend a slur into a portamento with a target `VxPITCH` register value

 * `play_noise <0-31> [ko] <duration>` - play noise
    * Noise is disabled on the next `play_note` or `play_pitch` instruction
    * CAUTION: There is only 1 noise generator.  
      This instruction will change the noise frequency on all channels playing noise.
    * CAUTION: The instrument is used to determine the length of the noise.
       * If the instrument does not loop, the noise is played for the length of the instrument's sample.
       * If the instrument loops, the noise is played until key-off.
 * `disable_noise` - disable noise

 * `disable_vibrato` - disable vibrato
 * `set_vibrato <pitch_offset_per_tick> <quarter_wavelength>` - Enable vibrato
    * Vibrato will add or subtract *pitch_offset_per_tick* value to the S-DSP PITCH register in a sawtooth pattern on every tick.
    * *quarter_wavelength* controls the rate of the vibrato.
 * `set_vibrato_depth_and_play_note <pitch_offset_per_tick> <note> <ko> <duration>` - Set vibrato depth and play a note (vibrato rate is unchanged)

 * `set_instrument <name>` - Set the channel's instrument to *name*.
 * `set_instrument_and_adsr <name> <attack> <decay> <sustain_level> <sustain_rate>` - Set the channel's instrument to *name* and sets the channel's ADSR register (overriding the instrument's envelope).
 * `set_instrument_and_gain <name> <gain>` - Set the channel's instrument to *name* and sets the channel's GAIN register (overriding the instrument's envelope).

 * `set_adsr <attack> <decay> <sustain_level> <sustain_rate>` - Set the channel's ADSR register (disables GAIN).
 * `set_gain <gain>` - Set's the channel's GAIN register (disables ADSR).

 * `set_temp_gain <gain>` - Temporarily sets the channel's GAIN register to `<gain>`.
    * The envelope will be restored on key-off.
    * A *gain* value of `0` will disable temp-gain.
 * `set_temp_gain_and_rest <gain> <duration>` - Temporarily sets the channel's GAIN register to `<gain>` and rest.
    * Sleeps for *duration-1* ticks then key-off for 1 tick.
    * A *gain* value of `0` will disable temp-gain.
    * The envelope will be restored at the end of the instruction.
 * `set_temp_gain_and_wait <gain> <duration>` - Temporarily sets the channel's GAIN register to `<gain>` and wait (no key-off).
    * A *gain* value of `0` will disable temp-gain.
    * The envelope will be restored on key-off
 * `reuse_temp_gain` - Reuses the temporary-GAIN from the previous `set_temp_gain*` instruction.
 * `reuse_temp_gain_and_rest <duration>` - Reuses the temporary-GAIN from the previous `set_temp_gain*` instruction and rests.
 * `reuse_temp_gain_and_wait <duration>` - Reuses the temporary-GAIN from the previous `set_temp_gain*` instruction and waits.

 * `set_early_release <t> <min>` - When playing notes, key-off the channel *t* ticks before the note normally ends.
    * The note will play for a minimum *min* ticks, unless the note is shorter than *min*.
    * This instruction uses less audio-RAM if *min* is 1.
    * CAUTION: The rest instruction in a `play-note rest` or `play-note wait rest` chain should be 257 ticks to ensure long notes are cut at the correct time.
 * `set_early_release <t> <min> <gain>` - early release with custom GAIN envelope
    * When playing notes, the envelope is temporarily changed to GAIN(*gain*) *t* ticks before key-off.
    * The envelope is changed a minimum *min* ticks after the start of the last note.
    * This instruction uses less audio-RAM if *min* is 1.
    * Useful for setting a custom release envelope.
    * CAUTION: the GAIN rate may need to be changed if the song's tempo changes.
 * `disable_early_release` - disables early-release.

 * `set_detune <i16>` - Adds an offset to the `VxPITCH` register in play-note and portamento instructions.
    * CAUTION: `play_pitch` instructions are unaffected by detune.
    * CAUTION: detune is not disabled when the instrument is changed.
    * CAUTION: detune persists across subroutine calls
 * `disable_detune` - Resets detune to 0.

 * `set_volume <volume>` - Set the channel's volume (0-255).
 * `adjust_volume <i8>` - Adjusts the channel's volume.
 * `set_pan <pan>` - Set the channel's pan (0-128).  0 is 100% to the left, 64 is centered, 128 is 100% to the right.
 * `adjust_pan <i8>` - Adjusts the channel's panning.
 * `set_pan_and_volume <pan> <volume>` - Sets the channel's panning and volume.

 * `volume_slide <amount> <duration>` - Volume slide
    * This instruction does not sleep and the effect runs in the background.
    * Volume slide is disabled when the volume is set or adjusted.
    * Volume slide is disabled on overflows and underflows.
 * `tremolo <amplitude 1..127> <quarter_wavelength_in_ticks 1..127>` - Tremolo
    * Tremolo will amplitude modulate the channel volume in a sawtooth pattern every tick.
    * *amplitude* is the center-to-peak depth in volume units.
    * *quarter_wavelength_in_ticks* controls the rate (speed) of the tremolo.
    * Tremolo is disabled when the volume is set or adjusted.
    * Tremolo is disabled on overflows and underflows.

 * `pan_slide <amount> <duration>` - Pan slide
    * This instruction does not sleep and the effect runs in the background.
    * Pan slide is disabled when the pan is set or adjusted.
    * Pan slide is disabled on overflows and underflows.
 * `panbrello <amplitude 1..64> <quarter_wavelength_in_ticks 1..127>` - Tremolo
    * Panbrello will amplitude modulate the pan in a sawtooth pattern every tick.
    * The first parameter is the center-to-peak depth in pan units.
    * *amplitude* is the center-to-peak depth in pan units.
    * *quarter_wavelength_in_ticks* controls the rate (speed) of the tremolo.
    * Panbrello is disabled when the pan is set or adjusted.
    * Panbrello is disabled on overflows and underflows.

 * `enable_pmod` - Enable pitch modulation on this channel.
   * Only channels B to F can be pitch modulated.
   * Channels G & H cannot be pitch-modulated as they would conflict with sound-effect channel ducking.
 * `disable_pmod` - Disable pitch modulation on this channel.

 * `enable_echo` - Enable echo on this channel.
 * `disable_echo` - Disable echo on this channel.

 * `start_loop [loop_count]` - Start a loop block
    * The `loop_count` instruction is optional if the *loop_count* is set in `end_loop`.
 * `skip_last_loop` - All bytecode after `skip_last_loop` will be skipped on the last iteration of the loop.
 * `end_loop [loop_count]` - End the loop block
    * The `loop_count` instruction is optional if the *loop_count* is set in `start_loop`.

 * `call_subroutine <name>` - Call the subroutine *name*.
    * Not available in sound effects.

 * `call_subroutine_and_disable_vibrato <name>` - Disable vibrato and call the subroutine *name*.
    * Disables vibrato before the subroutine call.
    * Not available in sound effects.


Global instructions
===================
These instructions will affect all channels.

The echo instructions can be used in sound effects to control echo using S-CPU 65816 code.


 * `set_song_tick_clock <tick_clock>` - Set the tick TIMER0 clock register to *tick_clock*
    * `tick_clock` must be >= 64 and <= 255.
    * Not available in sound effects.

 * `set_echo_volume <0..127>` - Sets the left and right echo volume to the same value (`EVOL` registers)
 * `set_stereo_echo_volume <left 0..127> <right 0..127>` - Set the left and right echo volume (`EVOL` registers)
 * `adjust_echo_volume <-127..+127>` - Adds the parameter to the left and right echo volumes
 * `set_stereo_echo_volume <left -127..+127> <right -127..+127>` - Adds the stereo parameters the left and right echo volumes
 * `set_echo_feedback <-128..127>` - Sets the echo feedback (`EFB` register)
 * `adjust_echo_feedback <-128..127>` - Adds the parameter to the echo feedback


Parameters
==========

 * `<note>` - the note to play
    * Format `<pitch>[+-]<octave>`
      * `c4` will play C4 Middle C
      * `d+4` will play D♯4
      * `d-4` will play D♭4
    * Alternatively, a note id integer can be used.

 * `<duration>` - duration of the instruction in ticks

 * `<ko>` - key-off flag
    * `keyoff` - send a key-off event after the instruction has finished
        * One tick will be subtracted from the *duration* and used for the key-off event.
    * `slur_next`, `sn` or `no_keyoff` - do not send a key-off event at the end of the instruction.
        * Used to extend the length of a note beyond the limits of an instruction
        * Used to slur the current note with the next note.

 * `<volume>` - Channel volume (0 to 255)

 * `<pan>` - Channel pan value (0 to 128)
    * 0 is 100% to the left, 64 is centered, 128 is 100% to the right.

 * `<i8>` - signed 8 bit integer (-128 to 127)

 * ADSR settings:
    * `<attack>`: 0-15
    * `<decay>`: 0-7
    * `<sustain_level>`: 0-7
    * `<sustain_rate>`: 0-31

 * GAIN settings:
    * `<u8>` (no prefix) - the raw 8 bit value to write to the GAIN register
    * `F<value>` - fixed GAIN envelope (0-127)
    * `D<rate>` - linear decrease (0-31)
    * `E<rate>` - exponential decrease (0-31)
    * `I<rate>` - linear increase (0-31)
    * `B<rate>` - bent increase (0-31)

 * `<loop_count>` - loop count (2 - 256)


Opcodes and data formats
========================

The bytecode opcodes and parameter data formats can be found in `audio-driver/src/bytecode.wiz`


