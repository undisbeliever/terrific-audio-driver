Sound Effects
=============

The Terrific Audio Driver supports sequenced sound effects that can be written using
[bytecode assembly](bytecode-assembly-syntax.md) or [MML](docs/mml-syntax.md).

Only 2 sound effects can play at a time and each sound effect will play on a single S-DSP channel.
When a sound effect is playing, music channels `G` and/or `H` will be ducked (temporally muted).


Limitations
===========

Sound effects have the following limitations:
 * Sound effects are played at a fixed tempo (125 ticks per second)
 * Sound effects cannot call subroutines
 * The echo buffer and FIR Filter settings are set by the song.  If a sound effect enables echo,
   it can have an inconsistent sound (depending on the songs echo and FIR settings).
 * The default sound effect queue can only hold 1 sound effect.
   This means only 1 sound effect can be played per frame.
   If the game requests two or more sound effects in a single frame, the one with the lowest index
   (as defined by the *Sound Effect Export Order*) will be prioritised.


Sound Effect Export Order
=========================

The export order is the order of the sound effects in the common-audio-data.  It is also used for
`play_sound_effect` `sfx_id`, `tad-compiler ca65-enums` and `tad-compiler pv-enums`.  It is defined
in the `.terrificaudio` project file and is separate from the sound effects file order.

The export order is split into three priorities; high, medium and low.  With all high-priority sound
effects having a lower `sfx_id` then normal-priority sound effects and normal-priority sound effects
having a lower `sfx_id` then low-priority sound effects.

The priority affects the sound effect dropout behaviour (see below).


Sound Effect Dropout Behaviour
==============================

The sound effect dropout behaviour is complex, but designed to handle a wide variety of sound
effects.

Each sound effect has two flags that affect how the sound effects are dropped:
 * Interruptible flag
    * If the *interruptible* flag is set, the sound effect can be interrupted by a
      `play_sound_effect` command. (Useful for collect coin, collision, splash sound effects.)
    * If the *interruptible* flag is clear (uninterruptible), the sound effect will not be dropped
      by a `play_sound_effect` command.
      (Useful for explosion, puzzle-complete ditty, low-health and vocal sound effects.)
    * Caution: The *interruptible* flag may be ignored by high priority sound effects.
 * One-channel flag: If set the sound effects will play on a maximum of 1 channel.<br/>
   What happens to a `play_sound_effect` command that uses the same sfx_id as a sound-effect
   channel, depends on the *interruptible* flag:
    * If the sound effect is interruptible, the sound effect will be restarted.
    * If the sound effect is uninterruptible, the `play_sound_effect` command will be dropped.


The ca65/PVSnesLib API contains a sound effect queue that can only hold a single sound effects.  If
there is a `Tad_QueueSoundEffect` subroutine call and the sfx-queue is occupied, the sound effect
one with the lower `sfx_id` (as defined by the *Sound Effect Export Order*) will be prioritised.
 * For example, if a frame calls `Tad_QueueSoundEffect(20)`, `Tad_QueueSoundEffect(10)` and
   `Tad_QueueSoundEffect(15)`; `Tad_Process` will send a `play_sound_effect` command with `sfx_id`
   of 10 (assuming a `play_sound_effect` command can be sent to the audio driver).
 * The sound-effect queue variables are public and a developer can override this behaviour by
   writing their own queue-sound-effect function.


When the audio driver receives a `play_sound_effect` command, it will first check to see if the
`sfx_id` has the *one-channel* flag set and the `sfx_id` is currently playing.
 * If the sound effect is interruptible, the sound effect will be restarted.
 * If the sound effect is uninterruptible, the `play_sound_effect` command will be dropped.

If the `play_sound_effect` `sfx_id` is not playing or the *one-channel* flag is clear, the sound
effect that is dropped is determined by the sound-effect priority and the *interruptible* flag:
 * High priority: Will always play (unless *one-channel* and uninterruptible)
    * If both sound effect channels are active and uninterruptible, then the *interruptible* flag is
      ignored and sound effect that will finish first is dropped.
    * If both sound effect channels are active and only 1 channel is interruptible, then the
      interruptible sound effect is dropped.
 * Normal priority:
    * If both sound effect channels are active and interruptible, then the sound effect that will
      finish first is dropped.
    * If both sound effects channels are active and uninterruptible, the `play_sound_effect` command
      is dropped.
 * Low priority: Will not play if both sound effect channels are active (ignoring the *one-channel* flag)
    * If both sound effects channels are in use, the `play_sound_effect` command is dropped.
    * If there is a free sound effect channel, the sound effect will be played.


Sound Effect File Format
========================

 * All sound effects are stored in a single text file.
 * Each sound effect is seperated with a line that starts with `===`
    * This line is in the format `=== <name> === [flags]`
    * Flags:
        * `mml`: the sound effect is MML, otherwise the sound effect is bytecode assembly
        * `interruptible`: Sets the *interruptible* flag
        * `uninterruptible`: Clears the *interruptible* flag
        * `one`: Sets the *one-channel* flag
        * `both`: Clears the *one-channel* flag
    * A line starting with `===` is not allowed in a sound effect file and will be silently
      converted to a comment by `tad-gui` when the sound effects are saved to disk.
 * The text before the first `===` line is the sound-effect subroutines
    * The sound-effects subroutines are written in MML and shared across all sound effects.

