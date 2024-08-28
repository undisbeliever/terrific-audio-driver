Terrific Audio Driver Changelog
===============================

Version 0.0.11
==============

**BREAKING MML changes:**
 * `~` manual vibrato persists across subroutine calls.
    * `MP` vibrato does not persist across subroutine calls (unchanged from v0.0.10).

**BREAKING Bytecode changes:**
 * `call_subroutine` does not disable vibrato
 * `return_from_subroutine` does not disable vibrato

Bytecode changes:
 * Loops and subroutines now use a traditional stack.
    * The maximum number of nested loops has increased.
 * Added `call_subroutine_and_disable_vibrato` instruction
 * Added `return_from_subroutine_and_disable_vibrato` instruction

MML changes:
 * MML subroutines can now call subroutines
 * Fixed slurred note tracking after a subroutine call.  
   A `{}` portamento after an MML subroutine that ends with a slurred note now has the same
   behaviour as a `{}` portamento after a loop that ends with a slurred note.


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

