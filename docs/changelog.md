Terrific Audio Driver Changelog
===============================


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

