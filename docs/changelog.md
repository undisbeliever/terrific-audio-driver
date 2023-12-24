Terrific Audio Driver Changelog
===============================

Version 0.0.4
=============
 * Added `#Game` to song metadata.
 * Fixed a volume popping bug by delaying all S-DSP register writes to the start of the next tick.
 * Started implementing virtual channels.
 * Added Samples
 * Added `s` play sample MML command
 * Fixed `n` play MIDI note MML command erroneously parsing note length
 * Added `$` hexadecimal number parsing


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

