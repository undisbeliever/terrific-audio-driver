MML Syntax
==========

The audio-engine and MML syntax is still in development and **subject to change without notice**.


Header
======

Lines starting with `#` define metadata.

Song metadata:
 * `#Title`
 * `#Game`
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
 * `#MaxEchoLength` - Maximum echo buffer size in milliseconds.
    * If `MaxEchoLength` is not supplied, `EchoLength` will be used.
 * `#FirFilter` - FIR filter (`C0` - `C7` S-DSP registers)
    * The FIR filter must contain 8 space separated values.
    * Values prefixed with a `$` are hexadecimal ($00-$FF)
    * Values without a prefix are decimal (-128 to 127)
    * **WARNING:** These values are copied to the `C0` - `C7` S-DSP registers without any overflow checks.<br/>
      An overflowing FIR filter can pop.  Excessive filter feedback will continue to get louder and louder, exploding your song.<br/>
      Headphone users should turn down their volume when playing with echo filters/feedback.
    * The compiler will reject an FIR filter whose absolute sum is > 128 (unless this limit is disabled).
 * `#DisableFirFilterLimit` - Disables the FIR filter absolute sum limit.
 * `#EchoFeedback` - Echo feedback setting (-128 to 127) (`EFB` S-DSP register)
    * **WARNING:** This value is copied to the `EFB` S-DSP register without any overflow checks.<br/>
      Excessive echo feedback will continue to get louder and louder, exploding your song.<br/>
      Headphone users should turn down their volume when playing with echo filters/feedback.
 * `#EchoVolume` - 1 or 2 echo volume arguments (0 to 127)
    * If only 1 value is given, it will used for both channels
 * `#EchoInvert` - Invert echo volume
    * `mono`, `left`, `right` - echo flags
    * `both` - invert both left and right channels in all modes
    * `none` - disable echo invert
    * See `\ei` command for more details


<br/>

The following will only affect exported .spc files:
 * `#SpcSongLength` - Override the .spc file song length (in seconds)
    * The maximum value is 999 seconds
 * `#SpcFadeout` - The fadeout length in milliseconds
    * The maximum value is 99999 milliseconds
    * The default fadeout is 0 (no fadeout)


Instruments
===========

Lines starting with `@` contain instrument definitions.

Instrument ids can be decimal numbers or alphanumeric.

 * `@<id> <sample_name>` - Create an instrument called *id*, that references the instrument *sample_name* in the *samples.json* file.
 * `@<id> <sample_name> adsr <attack> <decay> <sustain_level> <sustain_rate>` - Creates an instrument that overrides the ADSR values.
 * `@<id> <sample_name> gain <gain_value>` - Creates an instrument that overrides will disable ADSR and sets the `GAIN` S-DSP register to *gain_value*.


Channels
========

Lines starting with `A` - `H` select which track the MML will be played on.

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
 * MD automatic detune by cents



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

A `ticks` argument can be prefixed with an `l` to specify an MML length.
For example: `ql8` is the same as `q12` with a 96 ZenLen.

<br/>

Required parameters are enclosed in `<>`.<br/>
Optional parameters are enclosed in `[]`.<br/>
Numbers can be decimal, or hexadecimal when prefixed with `$` (ie, `$ff`)

 * `c` `d` `e` `f` `g` `a` `b` - Play note
    * `+` adds a sharp.
    * `-` adds a flat.
    * Multiple `+` and `-` can be used to increase/decrease the pitch by a semitone.
    * A optional *length* can be added after the note.  If there is no length, the default length (`l`) will be used.
 * `P<pitch>[,length]` - Play pitch
    * Sets the `VxPITCH` register to *pitch* and plays a note.  For example, `P$1000` will play the BRR sample at the native sample rate.
 * `PR<sample_rate>[,length]` - Play BRR sample at a given sample rate (in Hertz)
    * Sets the `VxPITCH` register to play the current BRR sample at the given sample rate.
    * For example: `PR32000` will play the BRR sample at the native 32KHz sample rate (`P$1000`)
      and `PR16000` will it at 16Khz sample rate (`P$0800`).
 * `PF<freq>[,length]` - Play instrument a given frequency (in Hertz)
    * Uses the instrument frequency to generate a play-pitch instruction
    * Cannot be used on samples
 * `s[number][,length]` - Play sample.
    * If this command is used with a sample, `number` is the sample-rate list index (default is 0).
    * If this command is used with an instrument, `number` is the note-id.
 * `n<number>` - Play the integer MIDI note at the default length
 * `^[length]` - Tie, extends the duration of the previous note or rest
 * `&` - Slur.  Used to tie two notes together without a key-off event
 * `&<length>` - Alternative Tie syntax
 * `r[length]` - Rest
    * `r` rest commands send a key-off event.
    * `r` rest commands can be tied, ie `r2 ^8`.
    * Three or more consecutive `r` rest commands will be merged by the MML parser.  For example, `r4 r8 r8` will be merged into a two `r4 r4` commands.
    * Very long `r` rests will be optimised into a loop.  For example, when Whole Note Length is 96, `r1 r1 r1 r1 r1 r1 r1 r1 r1 r1` will be converted to `r1 [r%216]4`.
 * `w[length]` - Wait
    * A `w` wait will not send a key-off event.  Useful for changing pan or volume in the middle of a slurred note.
    * `w` wait commands can be tied, ie `w2 ^8`.
    * Multiple `w` wait commands will be merged by the MML parser. For example, `w4 w8 w8` will be merged into a single `w2` command.
    * Very long `w` waits will be optimised into a loop.  For example, when Whole Note Length is 96, `w1 w1 w1 w1 w1 w1 w1 w1 w1 w1` will be converted to `[w%240]4`.

 * `N<0-31>[,length]` - play noise
    * Noise is disabled on the next note or `P` play-pitch command
    * CAUTION: There is only 1 noise generator.
       * In a song, this command will change the noise frequency on all song channels playing noise.
    * CAUTION: The instrument is used to determine the length of the noise.
       * If the instrument does not loop, the noise is played for the length of the instrument's sample.
       * If the instrument loops, the noise is played until key-off.
    * If a song channel plays noise at the same time a sound effect plays noise, the song noise will be muted.
    * If both sound effect channels play noise at the same time, an uninterruptible sound effect will have priority.
 * `N-` - disable noise

<br/>

 * `@<id>` - Set instrument
    * The instrument should not be changed in the middle of a slur.
 * `?@<id>` - Set subroutine instrument hint
    * Sets an instrument hint for `{}` portamento, `MP` and `MD` commands without emitting an
      `set_instrument` instruction.
    * The `?@` command can only be used in subroutines.
    * The `?@` hint will be used when previewing subroutines in the GUI.
    * CAUTION: A subroutine with a `?@` hint can only be called if the instrument source frequency
      matches the `?@` hint.
 * `A<attack>,<decay>,<sustain_level>,<sustain_rate>` - Set the channel's ADSR register
 * `G` - Set the channel's GAIN register (disables ADSR)
     * `G<gain>` - write the raw 8 bit value to the GAIN register
     * `GF<value>` - fixed envelope (0-127)
     * `GD<rate>` - linear decrease envelope (0-31)
     * `GE<rate>` - exponential decrease envelope (0-31)
     * `GI<rate>` - linear increase envelope (0-31)
     * `GB<rate>` - bent increase envelope (0-31)
 * `GT` - Temporarily sets the channel's GAIN register (the envelope will be restored on key-off)
     * `GT0` - disable temporary GAIN
     * `GT<gain>` - write the raw 8 bit value to the GAIN register
     * `GFT<value>` - fixed temporary envelope (0-127)
     * `GDT<rate>` - linear temporary decrease envelope (0-31)
     * `GET<rate>` - exponential decrease temporary envelope (0-31)
     * `GIT<rate>` - linear increase temporary envelope (0-31)
     * `GBT<rate>` - bent increase temporary envelope (0-31)
     * `GT` - reuse previous temporary GAIN

<br/>

 * `o<0..7>` - Set octave
 * `>` - Increase octave
 * `<` - Decrease octave
 * `_<-128..+128>` - Sets transpose.  Changes the pitch of each note played by *param* semitones.  The default is 0.
 * `__<-128..+128>` - Relative transpose.  Adds *param* to the transpose setting.

<br/>

 * `l<length>` - Set the default length
 * `C<4..256>` - Set the whole note length (in ticks).
    * Default is `#Zenlen` or 96 if `#Zenlen` not set.

<br/>

 * `v<0..16>` - Set coarse volume
 * `V<0..255>` - Set fine volume
 * `v-<1..16>` - Decrement volume (coarsely)
 * `v+<1..16>` - Increment volume (coarsely)
 * `V-<1..255>` - Decrement volume (finely)
 * `V+<1..255>` - Increment volume (finely)
 * `p<0..128>` - Set pan
    * A pan of 0 will output to the left speaker only.  A pan of 128 will output to the right speaker only.
    * The default pan is 64 (centered).
    * Panning is disabled if the stereo flag is false (mono output).
 * `px<-64..+64>` - Set pan
    * Negative `px` values pan to the left. `px-64` is 100% to the left.
    * Positive `px` values pan to the right. `px+64` is 100% to the right.
    * `px0` is centered
    * Panning is disabled if the stereo flag is false (mono output).
 * `p-<1..128>` - Decrement pan (pan to the left)
 * `p+<1..128>` - Increment pan (pan to the right)

<br/>

 * `vs+<1..16>,<ticks>` - Volume slide up (coarse volume)
    * Volume slide is disabled when the volume is set or adjusted.
    * Volume slide is disabled on overflows and underflows.
    * *ticks* can be prefixed with an `l` to specify an MML length  
      For example: `vs+4,l2` is the same as `vs+4,48` with a 96 ZenLen.
 * `vs-<1..16>,<ticks>` - Volume slide down (coarse volume)
 * `Vs+<1..255>,<ticks>` - Volume slide up (fine volume)
 * `Vs-<1..255>,<ticks>` - Volume slide down (fine volume)
 * `v~<1..8>,<quarter_wavelength_in_ticks>` - Tremolo (coarse volume)
    * Tremolo will amplitude modulate the channel volume in a sawtooth pattern every tick.
    * The first parameter is the center-to-peak depth in volume units.
    * *quarter_wavelength_in_ticks* controls the rate (speed) of the tremolo.
    * *quarter_wavelength_in_ticks* can be prefixed with `l` to specify MML length.  
      For example: `v~2,l32` is equal to `v~2,3` with a 96 ZenLen.
    * Tremolo is disabled when the volume is set or adjusted.
    * Tremolo is disabled on overflows and underflows.
 * `V~<1..127>,<quarter_wavelength_in_ticks>` - Tremolo (fine volume)

 * `ps+<1..128>,<ticks>` - Pan slide up
    * Pan slide is disabled when the pan is set or adjusted.
    * Pan slide is disabled on overflows and underflows.
 * `ps-<1..128>,<ticks>` - Pan slide down
 * `p~<1..64>,<quarter_wavelength_in_ticks>` - Panbrello
    * Panbrello will amplitude modulate the pan in a sawtooth pattern every tick.
    * The first parameter is the center-to-peak depth in pan units.
    * *quarter_wavelength_in_ticks* controls the rate (speed) of the panbrello.
    * `quarter_wavelength_in_ticks` can be prefixed with `l` to specify MML length.  
      For example: `p~15,l32` is equal to `p~15,3` with a 96 ZenLen.
    * Panbrello is disabled when the pan is set or adjusted.
    * Panbrello is disabled on overflows and underflows.

<br>

 * `i[L][R][M]` - Set channel phase invert
    * `L` - invert left channel if in surround mode
    * `R` - invert right channel if in surround mode
    * `M` - invert both channels if in mono or stereo mode
    * Multiple invert flags can be set at once.
      For example, `iLR` will invert the left and right channels in surround mode
      but no channels will be inverted in mono or stereo mode.
    * If no `L`/`R`/`M` is given, all 3 flags will be set.
    * CAUTION: There must be a space after the `i` command.
    * CAUTION: `L` and `R` are ignored in stereo mode.
 * `iB` - Phase invert with the left, right and mono invert-flags set
 * `i0` - Disable phase invert


<br/>

 * `E` - Enable echo
 * `E1` - Enable echo
 * `E0` - Disable echo

<br/>

 * `[ <mml> ]<2..255>` - Loop
    * Repeats a section of MML a given number of times
    * The broken chord command `{{ }}` consumes a loop.
 * `[ <mml1> : <mml2> ]<2..255>` - Loop
    * Repeats a section of MML a given number of times, skipping the MML after the `:` on the last loop.
    * For example: `[c : e r]4` will play `c e r   c e r   c e r   c`
 * `!<id>` - Call subroutine
    * A subroutine may change the instrument.
    * Calling a subroutine disables MP vibrato.
 * `L` - Set loop point
    * If a loop point is set, the channel will restart at the loop point when the channel has reached the end.
 * `;` - Comment.  The text between a `;` and a new line is ignored.
 * `|` - Divider.  Has no effect and can be used for aesthetic reasons (ie, splitting a line into bars).

<br/>

 * `{[pitch1] <pitch2>} [total_length] [, delay_length] [, portamento_speed]` - Portamento
    * Smoothly transition between two pitches.
    * If `delay_length` is set, there will be a delay between `pitch1` key-on and the pitch-shift.  For example: `{df}4,8` will delay the portamento by a eighth note (`d8 & {df}8`).
        * If `delay_length` is set, it must be less than `total_length`.
    * The S-DSP `PITCH` register will be incremented/decremented by a fixed value on every tick.  This value is calculated by the MML compiler and can be overridden by setting `portamento_spped`.
       * The MML compiler needs to know which instrument is playing the pitch.  If you are using portamento in a subroutine, you will need to either set an instrument (`@`) before playing a portamento or manually set `portamento_speed`.
       * `portamento_speed` can be set without setting `delay_length`, ie: `{df}4,,50`
    * Can be tied (`^`) and slurred (`&`) like a regular note
    * The octave can be changed inside the braces.  For example: `{a > c}2` and `{o3 c o4 c}2`
    * The pitches can be `P`, `PR` or `PF` commands
    * `pitch1` is optional if the previous note is slurred and known (ie, not at the start of a loop)

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
    * The pitches can be `P`, `PR` or `PF` play-pitch commands
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
    * `quarter_wavelength_in_ticks` can be prefixed with `l` to specify MML length.  
      For example: `~20,l32` is the same as `~20,3` with a 96 ZenLen.
    * Manual vibrato will disable MP Vibrato.
    * Manual vibrato will persist across subroutine calls.

 * `MP0` - Disable MP vibrato
    * MP Vibrato does not take effect immediately.  Vibrato will be disabled on the next note.
 * `MP<depth_in_cents>,<quarter_wavelength_in_ticks>` - MP Vibrato
    * `depth_in_cents` controls the depth of the vibrato, in cents either side of the note (half-extent).
    * `quarter_wavelength_in_ticks` value controls the rate of vibrato.
    * `quarter_wavelength_in_ticks` can be prefixed with `l` to specify MML length.  
      For example: `MP50,l32` is the same as `MP50,3` with a 96 ZenLen.
    * When MP Vibrato is activated the MML compiler will calculate a `pitch_offset_per_tick` value for each subsequent note played.  This greatly simplifies and speedups the SPC-700 code, but requires the MML compiler to know which instrument is playing the note.
    * MP Vibrato does not take effect immediately.  The vibrato starts on the next note played.
    * CAUTION: MP Vibrato does not persist across subroutine calls.
      When calling a `!` subroutine, MP vibrato is temporally disabled and MP vibrato resumes after the `!` mml subroutine returns.

<br/>

 * `Q<1-8>` - Quantize
    * Cuts the note by *param*/8.  The default value is 8.
    * Has no effect on slurred notes.
    * For example: `Q4 c4` will cut the notes length in half (4/8) and play `c8 r8`
    * `Q8` (the default) will not cut notes.
 * `Q%<0-255>` - Fine Quantize
    * Same as `Q`, except it cuts the notes by *param*/256.
 * `Q<1-7>,<temp-gain>` / `Q%<0-255>,<temp-gain>` - Quantize with Temp-GAIN
    * Instead of a hard 1 tick keyoff, this command will a set temp-GAIN, then rest.
    * For example: `Q4,D10 c4` will expand to `c8 & GDT10 r8`
    * `Q<n>,D<rate>` - Quantize with linear decrease envelope (0-31)
    * `Q<n>,E<rate>` - Quantize with exponential decrease envelope (0-31)
    * `Q<n>,I<rate>` - Quantize with linear increase envelope (0-31)
    * `Q<n>,B<rate>` - Quantize with bent increase envelope (0-31)
    * `Q<n>,F<value>` - Quantize with fixed envelope (1-127)
    * `Q<n>,<gain>` - Quantize with a raw GAIN value (1-255)
    * CAUTION: This command changes temporary GAIN on the next note.
 * `q<1-254>[,<1-255>][,<gain>]` - early release (sound cut)
    * First parameter: number of ticks to cut the note earlier then normal.
       * Can be prefixed with `l` to specify MML length.  (ie, `ql8` == `q12` with a 96 ZenLen)
    * Second parameter: minimum ticks before a note is cut (default 1 tick).
       * Can be prefixed with `l` to specify MML length.  (ie, `q10,l4` == `q10,24` with a 96 ZenLen)
    * Third parameter: temporally change the envelope to GAIN(*gain*) on early-release.
    * If there is no GAIN parameter, the note will be key-offed on early-release.
    * Examples:
       * `q4 c%24` is equivalent to `c%20 w%4`.
       * `q20,8 c%48` is equivalent to `c%28 w%20`.
       * `q20,8 c%24` is equivalent to `c%9 w%11` (8 ticks playing `c`, then key-off, then wait for 11 more ticks).
    * The *gain* parameter is useful for custom release envelopes (ie, `q10,D16` or `q8,E28`).
       * Of the various ways to write custom release envelopes, the `q` command uses the least audio-RAM.
    * The *minimum ticks* parameter can be skipped:
       * `q<n>,D<rate>` - linear decrease envelope (0-31)
       * `q<n>,E<rate>` - exponential decrease envelope (0-31)
       * `q<n>,I<rate>` - linear increase envelope (0-31)
       * `q<n>,B<rate>` - bent increase envelope (0-31)
       * `q<n>,F<rate>` - fixed envelope (1-127)
    * CAUTION: the GAIN rate may need to be changed if the song's tempo changes.
    * CAUTION: `q` persists across subroutine calls and `:` skip-last-loop commands.
 * `q0` - Disable early-release.

<br/>

 * `D<-16383..+16383>` - detune
    * Adds an offset to the `VxPITCH` register in play-note and portamento instructions.
    * CAUTION: `P` play pitch command is unaffected by detune.
    * CAUTION: detune is not disabled when the instrument is changed.
    * CAUTION: detune persists across subroutine calls
 * `D0` - disable detune
 * `MD<-600..+600>` - automatic detune by cents
    * Detunes play-note and portamento commands by a given number of cents.
    * `MD` automatically calculates the `VxPITCH` detune and adds a `D` detune command before each note.
      For example, `MD+20 c d e` would expand to  `D+25 c D+28 d D+31 e`.
    * CAUTION: `MD` increases the song data by 2 or 3 bytes per note.
      If you have a lot of notes with a fixed `MD` cents, you could clone and detune the instrument instead.
 * `MD0` - disable automatic detune
    * CAUTION: Also disables `D` detune


<br/>

 * `PM` - enable pitch modulation
   * The previous channel's OUTX (sample * envelope) value is used as the pitch modulation source.
   * Only channels B to F can be pitch modulated.
   * Channels G & H cannot be pitch-modulated as they would conflict with sound-effect channel ducking.
   * CAUTION: tad-gui's Play song from cursor feature cannot accurately emulate pitch-modulation.
      * The rust bytecode interpreter does not fully emulate key-on, portamento, vibrato or envelopes.
      * A pitch-modulation source will only key-on if pitch-modulation is on when playback starts.
 * `PM0` - disable pitch modulation

<br/>

 * `\asm { <asm_code> }` - bytecode assembly
    * Instructions can be separated by `|` dividers or newlines.
    * Loops inside `\asm` blocks must be self-contained.
      The `start_loop` and `end_loop` instructions must be within the same `\asm` block.
    * See [bytecode assembly documentation](bytecode-assembly-syntax.md).


<br/>

The Following commands affect **all channels**:
 * `t<bpm>` - Set song tempo (see `#Tempo` for the *bpm* format)
 * `T<64-255>` - Set song tick-clock  (see `#Timer` for the parameter format)

<br/>

Global echo commands:
 * CAUTION: hexadecimal numbers in echo commands are byte values.  For example `\efb $c0` is the same as `\efb -64`.
 * **WARNING:** These commands modify the echo S-DSP registers without any checks.
   * Excessive echo feedback will continue to get louder and louder, exploding your song.
   * An overflowing FIR filter can pop.  Excessive filter feedback will continue to get louder and louder, exploding your song.
   * Headphone users should turn down their volume when playing with echo filters/feedback.

<br/>

 * `\evol <0..127>` - Set the left and right global echo volume to the same value
 * `\evol <left 0..127>,<right 0..127>` - Set the left and right global echo volume
 * `\evol <-127..+127>` - Adds the parameter to the left and right global echo volume
 * `\evol <left -127..+127>,<right -127..+127>` - Adds the stereo parameters to the left and right global echo volumes
 * `\efb <-128..127>` - Sets the echo feedback
 * `\efb+ <1..127>[,max]` - Increment echo feedback
    * If `max` is set, the echo feedback be <= `max`.
 * `\efb- <1..128>[,min]` - Decrement echo feedback
    * If `min` is set, the echo feedback be >= `min`.
 * `\fir { c0 c1 c2 c3 c4 c5 c6 v7 }` - Set all 8 FIR filter tap coefficients
 * `\ftap <tap 0-7>,<value -128..127>` - Set a single FIR filter tap to the given coefficient value
 * `\ftap+ <tap 0-7>,<1..127>[,max]` - Increment a single FIR filter tap coefficient
    * If `max` is set, the FIR tap be <= `max`.
 * `\ftap- <tap 0-7>,<1..128>[,min]` - Decrement a single FIR filter tap coefficient
    * If `min` is set, the FIR tap be >= `min`.
 * `\ei <flags>` - Invert echo volume
    * `0` - disable echo invert
    * `B` - invert both left and right channels in all modes
    * `L` - invert left channel if in surround mode
    * `R` - invert right channel if in surround mode
    * `M` - invert both channels if in mono or stereo mode
    * Multiple `L`/`R`/`M` flags can be set at once.
      For example, `iLR` will invert the left and right channels in surround mode
      but no channels will be inverted in mono or stereo mode.
 * `\edl <delay>` - Change echo delay buffer length in milliseconds
    * This value must be a multiple of 16 and cannot be greater than `#MaxEchoLength`
    * CAUTION: changing the echo delay while echo is active will cause a glitch.
      * Shrinking the echo buffer delay will drop a section of the echo buffer.
      * Increasing the echo buffer delay will reveal and process uninitialized echo buffer memory.
    * CAUTION: this command does not take effect immediately.
      * `EDL` register writes only take effect when the S-DSP reaches the end of the echo buffer.
      * Echo volume and feedback should be 0 when changing echo delay.
      * Echo volume and feedback should be restored `old-EDL + new-EDL` milliseconds after the `\edl` command.
        This wait will also clear the uninitialized portion of echo buffer.
    * CAUTION: The play-from-cursor feature of the GUI cannot emulate the echo buffer position


Engine Limitations
==================

This audio engine was designed to be simple, to that effect the following limitations apply:

 * Song channels `G` and `H` are also used for sound effects.
    * When a sound effect is playing, song channels `G` and/or `H` will be ducked (temporally muted).
    * The sound effects window (SFX button or Menu > Audio > Sound Effects) can be used to test how channel ducking affects the song.
 * To prevent popping, there needs to be a 8 millisecond delay between key-off and key-on events.  This is accomplished by:
    * Setting a minimum tick-clock of 8ms.
    * Subtracting a single tick from every non-slurred note and adding a 1-tick rest after a key-off event.
    * The minimum length of a non-slurred note is 2 ticks.
 * All songs and sound effects use the same samples (no sample swapping).
 * Loops and subroutines share a stack.
    * The stack is 21 bytes per channel.
    * 3 bytes of stack are required for a loop.
    * 2 bytes of stack are required for a subroutine call.
    * The compiler will refuse to compile MML that would overflow the stack.
 * The echo buffer is always active, even if `#EchoLength` is 0.
 * A 250ms delay is required to initialize and clear the echo buffer before it can be written to.  All songs will be delayed a minimum of 250ms before the song starts, even if the song does not use echo.


To simplify and speedup the SPC-700 code:
 * All pitches and pitch-offsets are precalculated.
    * The pitches are stored in a 256 entry pitch table.
    * There is no overflow or underflow checks when accessing the pitch table.
    * The MML compiler will check for pitch-out-of-range errors, but only if it knows what instrument the note is play in.
 * Pitch effects (portamento and vibrato) are precalculated by the MML compiler.
    * To calculate the *pitch-offset-per-tick* value, the MML compiler needs to know which instrument is playing which portamento/vibrato note.


