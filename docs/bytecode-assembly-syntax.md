Bytecode Assembly Syntax
========================

The audio-engine and bytecode syntax is still in development and **subject to change without notice**.


Instructions
============

 * `rest <duration>` - Sleep for *duration-1* ticks then key-off for 1 tick.
 * `wait <duration>` - Sleep for *duration* ticks (no key-off).

 * `play_note <note> <duration>` - Play *note* for *duration* ticks then key-off the note
 * `play_note <note> <ko> <duration>` - Play *note* for *duration* ticks
 * `portamento <note> <ko> <pitch_velocity> <duration>` - Extend a slur into a portamento
    * Portamento instructions do not emit a key-on event.  You will need to slur the previous note before a portamento instruction.

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

 * `set_volume <volume>` - Set the channel's volume (0-255).
 * `adjust_volume <i8>` - Adjusts the channel's volume.
 * `set_pan <pan>` - Set the channel's pan (0-128).  0 is 100% to the left, 64 is centered, 128 is 100% to the right.
 * `adjust_pan <i8>` - Adjusts the channel's panning.
 * `set_pan_and_volume <pan> <volume>` - Sets the channel's panning and volume.

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

 * `set_song_tick_clock <tick_clock>` - Set the tick TIMER0 clock register to *tick_clock*
    * `tick_clock` must be >= 64 and <= 255.
    * Not available in sound effects.


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


