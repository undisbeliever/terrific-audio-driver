Terrific Audio Driver Changelog
===============================


Version 0.0.14
==============

Audio driver changes:
 * SET_MUSIC_CHANNELS IO command now key-offs the disabled channels
 * Added volume slides
 * Added Tremolo


Bytecode assembly changes:
 * Fixed maximum vibrato quarter wavelength value
 * Added `$` hexadecimal number parsing to bytecode assembly
 * Instructions with signed arguments (`adjust_volume` and `adjust_pan`) now require a + or - sign


MML changes:
 * Fixed maximum vibrato quarter wavelength value
 * Added `px` set pan command
    * `px0` is centered
    * Negative `px` values pan to the left. `px-64` is 100% to the left.
    * Positive `px` values pan to the right. `px+64` is 100% to the right.
 * Out-of-range errors now show the value that was out-of-range
 * Added subroutine no-instrument note tracking.
   * The compiler now emits an error if a subroutine call would play an out-of-range note.
   * The GUI's subroutine playback will now use the first MML instrument that can play all of the
     subroutine's no-instrument notes.

BRR changes:
 * Added gaussian overflow glitch detector (3 maximum-negative values in a row)
 * Added a gaussian overflow glitch penalty to the BRR encoder (Optional - on by default)

Compiler changes:
 * Increased maximum common-audio-data size
 * Added `tad-compiler 64tass-enums` command
 * Added `tad-compiler 64tass-export` command
 * Added hexadecimal segment suffix to `tad-compiler ca65-export`
   (using the `-x` or `-X` command line options)
 * Moved `ca65-export` `LoadAudioData` to the start of the first segment.

65816 API changes:
 * Added a 64tass API (identical to the ca65 API)
 * Renamed `MAX_PAN` to `TAD_MAX_PAN`
 * Renamed `CENTER_PAN` to `TAD_CENTER_PAN`
 * Renamed `MIN_TICK_CLOCK` to `TAD_MIN_TICK_CLOCK`

GUI changes:
 * Fixed a panic in the Sample Analyser if the sample is longer than 16384 samples.
 * Sample Analyser now uses Hann window when calculating the spectrum.
 * Fixed unable to click on the comment textbox in the sample editor.
 * Fixed rust bytecode interpreter going off the rails after a channel ends
 * Added a way to set the instrument and settings when previewing song subroutines
    * Click on the `!` button in the song toolbar and a new textbox will appear
    * Input (or paste) the MML you want to execute before the subroutine is played
      (ie, `@Flute E` would set the instrument to Flute and turn on echo)
    * Preview the subroutine with the *Play from line start* (F6) or *Play from cursor* (F7) buttons.
    * Be aware, no notes in the "! preview prefix" textbox will not be played.
 * Added a song start field to the song tab
    * Sets the starting position when playing the song with the play-button or F5
    * Pressing "set" or `F9` while editing the song selects the start position
    * Useful for editing and previewing `!` MML subroutines.
      Press `F9` before the subroutines is called, edit macro, press `F5` to listen to the macro.
    * Pressing Shift + "set" or `Shift+F9` will also mute the other channels.
    * Pressing Shift + play-button or `Shift+F5` will play the song from the beginning
 * Shift clicking the channel buttons A-H will select only one channel (Ctrl+Shift 0-9).


**Known bugs**:
 * Sample Analyser spectrum can only analyse the first 16384 samples in a long BRR sample.


Version 0.0.13
==============

**CAUTION**: This release has breaking changes to portamento and early-release.
Please check all songs and sound effect that use portamento and early-release.


**BREAKING Audio Driver changes:**
 * Portamento effects are now processed immediately after bytecode.
    * The pitch now changes on the same tick as the `portamento` instruction.
    * Previously, the pitch changed 1 tick after the `portamento` instruction.
    * This fixes a bug in the MML `{}` portamento command,
      where (for example) `{c g}` played `c` note for 2 ticks, when it should have played `c` for 1 tick.


**BREAKING MML changes:**
 * Added optional minimum-ticks parameter to the `q` early release MML command.
    * two-parameter `q` early release is unaffected if the second parameter has a GAIN mode
      (`D` `E` `I` `B` `F`).
    * If the second `q` early release parameter is a number, it will be read as the minimum-ticks parameter.
 * Forbid raw GAIN in `q` early release MML command.


**BREAKING Bytecode changes:**
 * Added required minimum ticks parameter to the `set_early_release` instruction.
    * `set_early_release` instructions will need to be modified.
    * If the instruction used a GAIN mode (`D` `E` `I` `B` `F`) it will output an error
    * If the instruction used raw GAIN, it will erroneously read the 2nd argument as minimum-ticks.
 * Forbid raw GAIN in `set_early_release` instructions


MML Changes:
 * Fixed an off-by-one error in the portamento PITCH velocity calculation.
 * Added rounding to the portamento PITCH velocity calculation.



Version 0.0.12
==============

Bytecode changes:
 * Added note range tests to bytecode assembly.
 * Changed the bytecode format to increase the number of non play-note instructions from 32 to 64.
 * Added temporary GAIN instructions
 * Added `set_early_release` instruction
    * `set_early_release` can also be used to add a custom release envelope to channels.


MML changes:
 * Added bytecode assembly to MML
    * Syntax: `\asm {  }`
    * Bytecode instructions can be separated by `|` dividers or new lines.
 * Added temporary GAIN MML commands
    * `GT0` - disable temporary GAIN
    * `GT<gain>` - write the raw 8 bit value to the GAIN register
    * `GFT<value>` - fixed temporary envelope (0-127)
    * `GDT<rate>` - linear temporary decrease envelope (0-31)
    * `GET<rate>` - exponential decrease temporary envelope (0-31)
    * `GIT<rate>` - linear increase temporary envelope (0-31)
    * `GBT<rate>` - bent increase temporary envelope (0-31)
    * `GT` - reuse previous temporary GAIN
 * Added `Q%<0-255>` Fine Quantize MML command.
 * Added `Q` Quantize with temp-GAIN.
    * For example: `Q4,D10 c4` will expand to `c8 & GDT10 r8`
    * `Q<n>,D<rate>` - Quantize with linear decrease envelope (0-31)
    * `Q<n>,E<rate>` - Quantize with exponential decrease envelope (0-31)
    * `Q<n>,I<rate>` - Quantize with linear increase envelope (0-31)
    * `Q<n>,B<rate>` - Quantize with bent increase envelope (0-31)
    * `Q<n>,<gain>` - Quantize with a raw GAIN value (1-255)
 * Added `q` set early release command
    * The `q` command supports early-release with a custom GAIN envelope and is useful for custom-release envelopes.  
      For example, `q10,D16` will change the envelope to linear-decrease (`D16`) 10 ticks before a note's key-off.
 * Fixed portamento not quantized with the `Q` quantize MML command.


GUI changes:
 * Fix sound effects stopping at the first rest instruction.
   (This only effected the GUI, the spc700 audio driver was unaffected).


Version 0.0.11
==============

**CAUTION**: This release has breaking changes to the `rest` bytecode instruction, please check all sound effects.

**CAUTION**: This release has breaking changes to MML vibrato and subroutine calls, please check all songs.

<br/>

**BREAKING MML changes:**
 * `~` manual vibrato persists across subroutine calls.
    * `MP` vibrato does not persist across subroutine calls (unchanged from v0.0.10).

**BREAKING Bytecode changes:**
 * `call_subroutine` does not disable vibrato
 * `return_from_subroutine` does not disable vibrato
 * `rest` has been renamed to `wait` (to match the `w` MML command)
 * `rest_keyoff` has been renamed to `rest` (to match the `r` MML command)

Bytecode changes:
 * Loops and subroutines now use a traditional stack.
    * The maximum number of nested loops has increased.
 * Added `call_subroutine_and_disable_vibrato` instruction
 * Added `return_from_subroutine_and_disable_vibrato` instruction
 * Added `goto_relative` bytecode instruction.

MML changes:
 * MML subroutines can now call subroutines
 * Fixed slurred note tracking after a subroutine call.  
   A `{}` portamento after an MML subroutine that ends with a slurred note now has the same
   behaviour as a `{}` portamento after a loop that ends with a slurred note.
 * Fixed slurred note tracking at the start of a loop and after a `:` skip last loop command.
 * Fixed vibrato not disabled if a subroutine ends with `MP0`.
 * Fixed wrong MP vibrato state at the start of a loop (ie, `MP20,4 [a MP30,6 b]4`).
    * `~` manual vibrato loop behaviour is unchanged.
 * Fixed wrong MP vibrato state after a `L` set loop point MML command.
    * `~` manual vibrato loop behaviour is unchanged.

GUI changes:
 * Closing a song tab while the song is playing will stop audio.

Data format changes:
 * Removed loop point from song header.  Songs now loop using the `goto_relative` instruction.


Version 0.0.10
==============

**CAUTION**: This release changes the data formats and IO commands.
You must update the audio driver and ca65/PVSnesLib API.

TLDR:
 * 8 music channels.
 * New sound effect dropout behaviour [(documentation)](sound-effects.md).
 * Made the ca65/PVSnesLib sound effect queue public.
 * Added a `PAUSE_MUSIC_PLAY_SFX` IO command.
 * Lots of space saving optimisations.


Audio driver changes:
 * Increased the number of music channels to 8.
 * Rewrote the main loop.
    * Virtual channel valid/dirty state is stored in a bitset in preparation for channel ducking.
    * IO ports are read a lot more often.
    * Changed how disabled channels are handled.
 * Added sound effect dropout behaviour [(documentation)](sound-effects.md).
    * Added *interruptible* flag to sound effects.
    * Sound effects with the *one-channel* flag set will play on at most 1 sfx channel.  
      If there is a `play_sound_effect` command for a *one-channel* sound effect that is already playing:
       * The sound effect will be reset if the sound effect is *interruptible*.
       * The `play_sound_effect` command will be dropped if the sound effect is not *interruptible*.
    * High-priority sound effects will ignore the *interruptible* flag if both sfx channels are uninterruptible.
    * Low-priority sound effects will not play if both sfx channels are active.
 * The pitch table is now variable-sized.
 * Added a `channelSoA.volShadowValid` flag.
    * `vol_l` and `vol_r` are now calculated after the bytecode has been processed.
 * `return_from_subroutine` bytecode instruction disables the channel if it is not in a subroutine.
 * Moved common audio data Structure of Arrays pointers to common audio data.
    * The amount of available Audio-RAM is unchanged.
 * Lots of little space-saving optimisations.

MML Changes:
 * Fixed merged relative volume commands clamping to an i8 (-128..=+127).
    * `V+100 V+100` will now correctly merge into a single `V+200` command.
 * Support larger values for `v+`/`v-`/`V+`/`V-` relative volume commands.
 * Optimise saturating relative volume command into absolute pan commands.
   (For example, `V+255` is transformed into `V255` and `V-255` is transformed into `V0`).
 * Support larger values for `p+`/`p-` relative pan commands.
 * Optimise saturating relative pan command into absolute pan commands.
   (For example, `p+200` is transformed into `p128` and `p-100` is transformed into `p0`.)

IO Command changes:
 * Removed `SET_ENABLED_CHANNELS` IO command, replaced with `SET_MUSIC_CHANNELS`.
    * Masking channels 6 & 7 no-longer silences sound effects.
 * Added `PAUSE_MUSIC_PLAY_SFX` IO command.

ca65 API changes:
 * The sound effect queue variables are now public.

PVSnesLib API changes:
 * The sound effect queue variables are now public.

File formats:
 * Changed the format of the sound effects file.
    * Added attributes to the sound effect name line.
    * tad-compiler and tad-gui can still read sound effect files in the old format.

GUI changes:
 * Fixed table not scrolling when items are added or moved.
 * Fixed selected row not visible when editing table cells.
 * Rewrote the GUI's data storage, list messages and item selection:
    * Fixed instrument and sample tables button enabled/disabled state.
    * Fixed missing name deduplication in instrument and sample tables.
    * Fixed name deduplicator not detecting an already deduplicated name.
 * Added pitch table size to the sample sizes widget.
 * Fixed sound effects window not playing selected sound effect with spacebar.
 * Fixed sound effects window selection scrolling and clicking without moving the mouse cursor.
 * Fixed pan slider playing from both speakers when 100% to the right


Version 0.0.9
=============

BRR changes:
 * Added more invalid wave file error messages

GUI changes:
 * Songs can be played in the sound effect tab.
    * Select a song on the Sound Effect tab toolbar, click the rightmost play button (or press F7).
    * The textbox after the song combo-box is the starting offset in ticks.
 * Sound effects can now be played while a song is playing.
    * Click on the SFX button in a song tab or Menu > Audio > Sound Effects.
    * Double click on a Sound Effect name to play the sound effect.
    * This window will send PLAY_SOUND_EFFECT IO commands to the emulated audio driver,
      allowing the user to hear how the sound effects will sound in-game.
    * The sound effects window is disabled when the sound effects tab is open.
 * Added a sample sizes graph and table to the Samples tab.
    * Click on the large button above the Instruments list to view it.
    * If there is an error combining samples into Common Audio Data, the sample sizes widget will
      display an error message.
 * Added a pan slider to the Sound Effects tab.
 * Added custom toolbar icons.
 * Added *Clear sample cache and rebuild* to the File menu.
 * Fixed song note tracking not cleared when the audio thread plays a different song
 * Lots of improvements to the compiler thread:
    * The Common Audio Data is compiled whenever an instrument or sample is changed.
    * The "Dependency Error" bug has been fixed.
    * Improved inter-thread messaging.
    * Plus many more small optimisations.


Version 0.0.8
=============
 * Added PVSnesLib integration code
 * Added `tad-compiler pv-export` command
 * Added `tad-compiler pv-enums` command
 * Added include sound effects argument to `tad-compiler song2spc` (`-s` or `--sound-effects`)

MML Changes:
 * Fixed `@` set instrument, `A` set ADSR and/or `G` set gain MML commands not emitting a
   set instrument/ADSR/gain instruction after a `L` set loop point MML command.
 * Forbid the `L` set loop point MML command inside a `[ ]` MML loop.

Audio Driver Changes:
 * Fixed audio driver not clearing the echo buffer.

ca65 API Changes:
 * Added `Tad_LoadSongIfChanged` subroutine
 * Added `Tad_GetSong` subroutine
 * Fixed ca65 API tests not rebuilding when `tad-audio.s` changes
 * Fixed Illegal addressing mode error when compiling with ca65 v2.19
 * Fixed typos in ca65 API documentation


Version 0.0.7
=============
 * Added ca65 integration code
 * Added `tad-compiler ca65-export` command
 * Added `tad-compiler ca65-enums` command
 * Fixed missing key-off in the pause IO command
 * Added bounds checking to the play_sound_effect IO command


**Breaking changes**:
 * Moved Loader to audio-RAM address $200


Version 0.0.6
=============

MML Changes:
 * Fixed missing `set_instrument` instruction with the `@` set-instrument MML command
   when the envelope has been changed and the `@` id is unchanged (ie, `@1 A1,2,3,4 @1`).
 * Fixed missing set-instrument and/or set-envelope instructions at the start of a loop
   (ie, `@1 a [@1 b @2 c]10`).
 * Channels A-F now correspond to S-DSP voices 0-5


BRR Encoder changes:
 * Fixed BRR encoder outputting glitched BRR blocks
 * Improved the accuracy of the BRR encoder by using the filter and shift formulas from Anomie's S-DSP Doc
 * Improved the BRR encoder by testing 3 nibbles for each sample
 * BRR encoder can now read 8 bit mono PCM wave files


GUI Changes:
 * Show filename in the window title
 * Added close to song tabs
 * Added sample analyser (click on the `...` button on the Frequency field of the Instrument editor)
 * Added play channel from line start button to the song tab.
 * Added play channel from cursor button to the song tab.
 * Added toggle buttons to enable and disable channels in the song tab.
 * The play from cursor buttons can now play subroutines.<br/>
   (The first instrument in the MML file will be used when playing a subroutine.)
 * When opening a file that is outside of the project directory, the user is given the option to copy the file into the project directory.<br/>
   (The *copy file to project* feature will not override existing files.)


Audio Driver Changes:
 * Fixed `set_enabled_channels` IO command


Version 0.0.5
=============
 * Fixed a crash when creating a new project or opening the Save As dialog (Windows).


Version 0.0.4
=============
 * Added `#Game` to song metadata.
 * Fixed a volume popping bug by delaying all S-DSP register writes to the start of the next tick.
 * Started implementing virtual channels.
 * Added Samples
 * Added `s` play sample MML command
 * Fixed `n` play MIDI note MML command erroneously parsing note length
 * Added `$` hexadecimal number parsing
 * Added loop point filter overrides to the BRR encoder
 * Added GAIN register modes to MML, bytecode assembly and envelopes.
 * Fixed a race condition that can cause an audible glitch when changing envelope from ADSR to GAIN
   (or vice versa).
 * Removed RingBuffer is full panic from the GUI.


Version 0.0.3
=============

 * **Breaking change**: MML `r` rest commands now sends key-off events.
 * Added MML `w` wait command.
 * Quantized MML notes emit `rest_keyoff` bytecode instructions.
 * Long MML `r` rests and `w` waits are optimised into loops.
   For example, when Whole Note Length is 96, `r1 r1 r1 r1 r1 r1 r1 r1 r1 r1` will be converted to `r1 [r%216]4`.
 * Added `A` set ADSR MML command
 * Added `G` set GAIN MML command
 * Fixed MML `:` SkipLastLoopState tracking with nested loops.
 * Fixed play from line-start/cursor buttons not working when the cursor is at the start of a line or in a comment.
 * Highlight subroutine calls in MML note tracking.


Version 0.0.2
=============

Sample/Instrument changes:
 * The test sample widget no longer limits the octave spinner.
   The widget will not play the sample if the octave is too high for the sample source frequency.
 * Fixed loop point not 0 when changing the loop setting from Dupe Block Hack to Loop
 * Fixed wrong error message when last octave is invalid
 * Fixed `pitch < PITCH_REGISTER_FLOAT_LIMIT` assert panic when the sample frequency is not
   between *B* and *C*

Sound effect changes:
 * Added MML sound effects
 * Show sound effect tick count and duration in the GUI
 * Increased the maximum number of sound effects to 254
 * The sound effect file header can now be edited in the GUI
 * Fixed duplicate sound effect file headers when saving the sound effects file

MML changes:
 * Rewrote the MML parser and improved the song data generator
 * The MML parser can now merge commands across multiple lines
 * Added an MML cursor tracking status bar to the GUI
 * Added play from cursor/line-start to the audio driver GUI
 * Parse MML ties after rests (r4^16)
 * The set default length command (`l`) requires a length number
 * Added a basic FIR Filter overflow check
 * Improved portamento note tracking
 * Fixed invalid song data when number of subroutines is >= 128
 * Improved `#` header error messages
 * Fixed change whole note length MML command (`C`) not updating default length

Audio driver:
 * Fixed not setting S-DSP FLG register in spc export
 * Found a few small optimisations
 * Added `songTickCounter` to the audio driver (not visible to the S-CPU)

Other changes:
 * Changed project file extension to `.terrificaudio`
 * Added is_terminal check to `tad-compiler --stdout`, forbid writing output to a terminal
 * Lots of little fixes

