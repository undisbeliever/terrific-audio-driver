MML Syntax
==========

The audio-engine and MML syntax is still in development and **subject to change without notice**.


Header
======

Lines starting with `#` define metadata.

Song metadata:
 * `#Title`
 * `#Composer`
 * `#Author`
 * `#Copyright`
 * `#Date`
 * `#License`

<br/>

The song tempo can be set with either `#Tempo` or `#Timer`:
 * `#Tempo number` - Sets the number of times a 48-tick cycle will occur in a minute.
    * With a `#Zenlen` value of 96 (default), specifies the number of half-notes in a minute
    * With a `#Zenlen` value of 192, specifies the number of quarter-notes in a minute
 * `#Timer number` - Sets the tick clock.  Each tick is `Timer * 0.125` milliseconds (or `Timer / 8000Hz` seconds) long.

<br/>

The following adjust default values:
 * `#Zenlen number` - Set the default whole-note length (default 96)

<br/>

Echo values:
 * `#EchoLength` - Echo buffer size in milliseconds.
    * This value must be a multiple of 16
    * The echo buffer requires `EchoLenth / 16 * 2048` bytes of Audio-RAM.
    * The echo buffer is placed at the end of Audio-RAM.
    * An `EchoLength` of 0 does not disable the echo buffer.  When `EchoLength` is 0, 256 bytes of Audio-RAM will be allocated to the echo buffer and the echo buffer is 4 bytes in size.
    * The default `EchoLength` value is 0.
 * `#FirFilter` - FIR filter (`C0` - `C7` S-DSP registers)
    * The FIR filter must contain 8 space separated values.
    * Values prefixed with a `$` are hexadecimal ($00-$FF)
    * Values without a prefix are decimal (-128 to 127)
    * **WARNING:** These values are copied to the `C0` - `C7` S-DSP registers without any overflow checks.<br/>
      An overflowing FIR filter can pop.  Excessive filter feedback will continue to get louder and louder, exploding your song.<br/>
      Headphone users should turn down their volume when playing with echo filters/feedback.
 * `#EchoFeedback` - Echo feedback setting (-128 to 127) (`EFB` S-DSP register)
    * **WARNING:** This value is copied to the `EFB` S-DSP register without any overflow checks.<br/>
      Excessive echo feedback will continue to get louder and louder, exploding your song.<br/>
      Headphone users should turn down their volume when playing with echo filters/feedback.
 * `#EchoVolume` - Echo volume (-128 to 127) (`EVOL` S-DSP registers)



Instruments
===========

Lines starting with `@` contain instrument definitions.

Instrument ids can be decimal numbers or alphanumeric.

 * `@<id> <sample_name>` - Create an instrument called *id*, that references the instrument *sample_name* in the *samples.json* file.
 * `@<id> <sample_name> adsr <attack> <decay> <sustain_level> <sustain_rate>` - Creates an instrument that overrides the ADSR values.
 * `@<id> <sample_name> gain <gain_value>` - Creates an instrument that overrides will disable ADSR and sets the `GAIN` S-DSP register to *gain_value*.


Channels
========

Lines starting with `A` - `F` select which track the MML will be played on.

The channels must be separated from the MML by a space or tab.


Multi-Channels
--------------
A single MML line can be played on multiple channels.  Do not separate the channels with spaces.  If a channel is repeated, the line will only be played once.

For example:
```
A   c
CDE a b
```

Will play `c` on channel A and `a b` on channels C, D and E.

<br/>

**NOTE**: Playing a MML line on multiple channels duplicates the MML data for each channel.



Subroutines
===========

Lines starting with `!` are subroutines.

```
!<id> <MML>
```

Subroutines allow you to deduplicate MML code.

For example:
```
!1 a b c

A !1 d e !1
```
Would play `a b c  d e  a b c` on channel A.

<br/>
The subroutine id can be a decimal or alphanumeric.

Subroutines cannot be called inside subroutines.

Subroutines can expand across multiple lines:
```
!a a

!z d e
!z f g
```
Subroutine `!a` will output `a`.<br/>
Subroutine `!z` will output `d e f g`


<br/>

This is not a substitution macro, each subroutine/channel has the following independent settings:
 * Whole note length (zenlen)
 * Octave
 * Transpose (semitone offset)
 * Default length
 * Quantization value
 * MP Vibrato



MML Syntax
==========

An MML command cannot span multiple lines.  Loops can span multiple lines.

<br/>

`id` format:
 * If the id is a number, no space is required after the `id`.
 * If the id is an alphanumeric, a space is required after the `id`.

<br/>

`length` format: `[[%]duration][.]`
 * The note length is `whole-note/duration` (if there is no `%`).
 * The `%` prefix will specify `duration` in ticks (clock-value).
 * Adding one or more dots `.` will extent the note length by half.
 * Examples:
    * `a4` - Plays `a` for a quarter-note.
    * `a8` -  Plays `a` for an eighth-note.
    * `a4.` - Plays `a` for 1.5 × quarter-note length (`a4 & a8`).
    * `a4..` - Plays `a` for 1.75 × quarter-note length (`a4 & a8 & a16`).
    * `a%20` - Plays `a` for 20 ticks.
    * `a%20.` - Plays `a` for 30 ticks (20+10).

<br/>

Required parameters are enclosed in `<>`.<br/>
Optional parameters are enclosed in `[]`.

 * `c` `d` `e` `f` `g` `a` `b` - Play note
    * `+` adds a sharp.
    * `-` adds a flat.
    * Multiple `+` and `-` can be used to increase/decrease the pitch by a semitone.
    * A optional *length* can be added after the note.  If there is no length, the default length (`l`) will be used.
 * `n<number>` - Play the integer note id at the default length
 * `^[length]` - Tie, extends the duration of the previous note
 * `&` - Slur.  Used to tie two notes together without a key-off event
 * `&<length>` - Alternative Tie syntax
 * `r[length]` - Rest

<br/>

 * `@<id>` - Set instrument
    * The instrument should not be changed in the middle of a slur.
 * `o<0..7>` - Set octave
 * `>` - Increase octave
 * `<` - Decrease octave
 * `_<-128..+128>` - Sets transpose.  Changes the pitch of each note played by *param* semitones.  The default is 0.
 * `__<-128..+128>` - Relative transpose.  Adds *param* to the transpose setting.

<br/>

 * `Q<1-8>` - Quantize
    * Cuts the note by *param*/8.  The default value is 8.
    * For example: `Q4 c4` will cut the notes length in half (4/8) and play `c8 r8`
    * `Q8` (the default) will not cut notes.
 * `l<length>` - Set the default length
 * `C<4..256>` - Set the whole note length (in ticks).
    * Default is `#Zenlen` or 96 if `#Zenlen` not set.

<br/>

 * `v<0..16>` - Set coarse volume
 * `V<0..255>` - Set fine volume
 * `v-<1..8>` - Decrement volume (coarsely)
 * `v+<1..7>` - Increment volume (coarsely)
 * `V-<1..128>` - Decrement volume (finely)
 * `V+<1..127>` - Increment volume (finely)
 * `p<0..128>` - Set pan
    * A pan of 0 will output to the left speaker only.  A pan of 128 will output to the right speaker only.  The default pan is 128 (centered)
    * Panning is disabled if the stereo flag is false (mono output).
 * `p-<1..128>` - Decrement pan (pan to the left)
 * `p+<1..127>` - Increment pan (pan to the right)
 * `E` - Enable echo
 * `E1` - Enable echo
 * `E0` - Disable echo

<br/>

 * `[ <mml> ]<2..255>` - Loop
    * Repeats a section of MML a given number of times
    * You can nest a maximum of 3 loops, including the loops in any subroutines you call.
    * The broken chord command `{{ }}` consumes a loop.
 * `[ <mml1> : <mml2> ]<2..255>` - Loop
    * Repeats a section of MML a given number of times, skipping the MML after the `:` on the last loop.
    * For example: `[c : e r]4` will play `c e r   c e r   c e r   c`
 * `!<id>` - Call subroutine
    * A subroutine may change the instrument.
    * Calling a subroutine disables manual vibrato.
    * A subroutine cannot call a subroutine.
 * `L` - Set loop point
    * If a loop point is set, the channel will restart at the loop point when the channel has reached the end.
 * `;` - Comment.  The text between a `;` and a new line is ignored.
 * `|` - Divider.  Has no effect and can be used for aesthetic reasons (ie, splitting a line into bars).

<br/>

 * `{<pitch1> <pitch2>} [total_length] [, delay_length] [, portamento_speed]` - Portamento
    * Smoothly transition between two pitches.
    * If `delay_length` is set, there will be a delay between `pitch1` key-on and the pitch-shift.  For example: `{df}4,8` will delay the portamento by a eighth note (`d8 & {df}8`).
        * If `delay_length` is set, it must be less than `total_length`.
    * The S-DSP `PITCH` register will be incremented/decremented by a fixed value on every tick.  This value is calculated by the MML compiler and can be overridden by setting `portamento_spped`.
       * The MML compiler needs to know which instrument is playing the pitch.  If you are using portamento in a subroutine, you will need to either set an instrument (`@`) before playing a portamento or manually set `portamento_speed`.
       * `portamento_speed` can be set without setting `delay_length`, ie: `{df}4,,50`
    * Can be tied (`^`) and slurred (`&`) like a regular note
    * The octave can be changed inside the braces.  For example: `{a > c}2` and `{o3 c o4 c}2`

 * `{{<pitch list>}} [total_length] [, note_length] [, tie]` - Broken Chord
    * Quickly cycle through the pitches for a given duration (`length`).
    * For example, `{{ceg}}` will quickly alternate between `c`, `e` and `g` every tick with a single key-on event.
    * `total_length` the total length of the broken chord.
    * `note_length` is the length of each note played.
       * By default this value is `%1` if `tie` is 1 (default) or `%2` if `tie` is 0.
    * The `tie` flag indicates if the pitches are tied or not.
       * If `1` (default), the pitches will be tied, a single key-on event and key-off event will be emitted.
       * If `0`, There will be a key-on and key-off event for each pitch.
    * The octave can be changed inside the braces (like portamento).
    * This command consumes a loop.
    * Examples (Whole Note Length is 96, default `l` is 4):
        * `{{ceg}}` expands to `[c%1 & : e%1 & g%1 &]8 e%2`
        * `{{ce-g}}2,32` expands to `[c32 & e-32 & g32 &]5 c32`
        * `{{c}}4,,0` expands to `[c%2]12`
        * `{{de}}4,,0` expands to `[d%2 e%2]6`
        * `{{fg}}4,%3,0` expands to `[f%3 g%3]4`
        * `{{ab}}4,16,0` expands to `[a16 b16]2`

 * `~0` - Disable manual vibrato
 * `~<pitch_offset_per_tick>,<quarter_wavelength_in_ticks>` - Manual vibrato
    * Generates a `set_vibrato` audio instruction bytecode.
    * Vibrato will add or subtract a `pitch_offset_per_tick` value to the S-DSP `PITCH` register in a sawtooth pattern on every tick.
    * `quarter_wavelength_in_ticks` value controls the rate of vibrato.
    * Manual vibrato will disable MP Vibrato.
    * Calling a subroutine will disables manual vibrato.

 * `MP0` - Disable MP vibrato
 * `MP<depth_in_cents>,<quarter_wavelength_in_ticks>` - MP Vibrato
    * `depth_in_cents` controls the depth of the vibrato, in cents either side of the note (half-extent).
    * `quarter_wavelength_in_ticks` value controls the rate of vibrato.
    * When MP Vibrato is activated the MML compiler will calculate a `pitch_offset_per_tick` value for each subsequent note played.  This greatly simplifies and speedups the SPC-700 code, but requires the MML compiler to know which instrument is playing the note.
    * MP Vibrato does not take effect immediately.  The vibrato starts on the next note played.


<br/>

The Following commands affect **all channels**:
 * `t<bpm>` - Set song tempo (see `#Tempo` for the *bpm* format)
 * `T<64-255>` - Set song tick-clock  (see `#Timer` for the parameter format)


Engine Limitations
==================

This audio engine was designed to be simple, to that effect the following limitations apply:

 * Only 6 channels can be used for music.  2 channels are dedicated to sound effects.
 * To prevent popping, there needs to be a 8 millisecond delay between key-off and key-on events.  This is accomplished by:
    * Setting a minimum tick-clock of 8ms.
    * Subtracting a single tick from every non-slurred note and adding a 1-tick rest after a key-off event.
    * The minimum length of a non-slurred note is 2 ticks.
 * All songs and sound effects use the same samples (no sample swapping).
 * A subroutine cannot call a subroutine.
 * A maximum of 3 loops can be nested at once (including any subroutines that are called).
 * The echo buffer is always active, even if `#EchoLength` is 0.
 * A 250ms delay is required to initialize and clear the echo buffer before it can be written to.  All songs will be delayed a minimum of 250ms before the song starts, even if the song does not use echo.


To simplify and speedup the SPC-700 code:
 * All pitches and pitch-offsets are precalculated.
    * The pitches are stored in a 256 entry pitch table.
    * There is no overflow or underflow checks when accessing the pitch table.
    * The MML compiler will check for pitch-out-of-range errors, but only if it knows what instrument the note is play in.
 * Pitch effects (portamento and vibrato) are precalculated by the MML compiler.
    * To calculate the *pitch-offset-per-tick* value, the MML compiler needs to know which instrument is playing which portamento/vibrato note.


