Terrific Audio Driver
=====================

A homebrew audio driver for the Super Nintendo.


Features
========
 * A custom loader to speed-up data loading.
 * A bytecode-based song and sound effect data format and interpreter.
 * Songs written in Music Macro Language (MML). ([mml syntax](docs/mml-syntax.md))
 * 8 music channels.
 * [Sound effects](docs/sound-effects.md):
   * Sequenced sound effects, written in either bytecode assembly
     ([syntax](docs/bytecode-assembly-syntax.md)) or MML.
   * 2 sound effect channels, which will duck (temporally mute) song channels `G` and/or `H`.
   * Elaborate [sound effect dropout behaviour](docs/sound-effects.md#sound-effect-dropout-behaviour):
      * The *one-channel* flag set limit a sound effect to a single channel, preventing common and
        frequent sound effects from using both sound effect channels.
      * The *interruptible* flag controls if a sound effect can be interrupted (useful for short
        sound effects, like collect coin, collision and splash) or uninterruptable (useful for long
        or vocal sound effects).
 * Custom ADSR and GAIN envelopes in songs and sound effects.
 * Mono, stereo and surround audio modes.
 * Lots of effects:
    * Vibrato and portamento
    * Volume slides, tremolo, pan slides and panbrello
    * Detune
    * Left/right channel invert (if the audio mode is set to Surround)
    * Commands for editing the echo buffer parameters in the middle of a song
 * ca65, pvsneslib, 64tass and asar APIs
    * The API is asynchronous to minimise impact on the entity/mainloop code.
    * Audio data can be loaded across multiple frames (`Tad_Process` calls).
    * Audio data is loaded through a `LoadAudioData` callback, simplifying integration with
      existing resource subsystems.  Alternativly, the `tad-compiler` binary can create and export
      the `LoadAudioData` callback.
    * Audio data can wrap across bank boundaries.
 * A GUI for editing and previewing samples, sound effects and songs.


Engine Limitations and Deliberate Design Decisions
==================================================
 * There are only 2 sound effect channels.
    * When a sound effect is playing, music channels `G` and/or `H` will be ducked (temporally muted).
 * There is no sample swapping.
    * Samples and sound effect data are combined to form the *common audio data* and transferred to
      audio-RAM in a single block.
    * The loader has the ability to override the common audio data.  If you want to have a special
      set of songs that contain unique samples (ie, intro and credits), you will need to create a new
      project (with a different set of samples) and manually swap the common audio data in S-CPU
      code.
 * All pitches and most pitch offsets are precalculated by the compiler.
    * The `VxPITCH` values for each note are precalculated by the compiler inside a pitch table.
      * The pitch table holds a maximum 256 pitches (512 bytes) and is stored in the Common Audio Data.
      * Limiting an instruments first-last octave will reduce the number of pitch table entries
        required by the instrument.
    * Where possible, the portamento *pitch-offset per tick* value is precalculated by the MML
      compiler.  This requires the MML compiler to know what instrument is playing the portamento.
       * If the instrument of the portamento is unknown, the audio driver will calculate the
         portamento velocity.
       * The MML syntax provides a way to manually set the *pitch-offset per tick* value.
       * If the song contains a transpose command the compiler will not calculate the portamento
         *pitch-offset per tick* value.
    * `MP` vibrato is precalculated by the MML compiler, the instrument must be known and audio
      driver transpose must be disabled.
 * There is a minimum 1 tick delay between a key-off and key-on event.
    * Instructions that emit a key-off event will key-off at least 1 tick before the instruction ends.
      For example, a `play_note c+4 16` instruction will play the note, sleep for 15 ticks, emit a
      key-off event, and sleep for 1 final tick.
    * The key-off timing is controlled by the `q` early-release command.
    * This delay is required to prevent popping.
 * MML subroutines and loops share a bytecode stack.  The compiler will refuse to compile songs and
   sound effects that would cause a stack-overflow.
 * There are no overflow or underflow checks when playing notes.
    * The MML compiler will check for pitch-out-of-range errors, but only if it knows which
      instrument is playing the note.
 * There are no FIR filter overflow or echo feedback overflow checks in the compiler.
    * If the FIR filter or echo feedback overflows it can clip, pop or explode the song.
      Headphone users should turn down their volume when playing with the echo filter or feedback.
 * The CLI song compiler and audio-driver loader cannot detect audio-RAM overflow.
    * The GUI can check if songs will fit in audio-RAM.
    * `tad-compiler check` can be used to check if all songs in a project will fit in audio-RAM.
 * Additional limitations exist for sound effects:
    * Sound effects are played at a fixed tempo (125 ticks per second).
    * The echo buffer and FIR Filter settings are set by the song.  If a sound effect enables echo,
      it can have an inconsistent sound (depending on the songs echo and FIR settings).
    * The default 65816 sound effect queue can only hold 1 sound effect.
      This typically means a game can only play a single sound effect per frame.
      If the game requests two or more sound effects in a single frame, the one with the lowest
      index (as defined by the *Sound Effect Export Order*) will be prioritised.


<!-- START RELEASE CUT -->
Build Requirements
==================

 * Rust 1.85 or later
 * Cargo
 * A C++17 compiler, compatible with the [cxx](https://cxx.rs/) and
   [fltk-rs](https://crates.io/crates/fltk#user-content-build-dependencies) crates.
 * CMake
 * [fltk-rs build dependencies](https://github.com/fltk-rs/fltk-rs/blob/master/README.md#build-dependencies):
    * MSVC: Windows SDK
    * MacOS: MacOS SDK
    * Linux/BSD: X11, libpango and OpenGL development headers
 * Linux/BSD: ALSA development files


For Debian/Ubuntu/Mint distributions, the following dev packages are required:
```
    cmake libx11-dev libxext-dev libxft-dev libxinerama-dev libxcursor-dev libxrender-dev libxfixes-dev libpango1.0-dev libgl1-mesa-dev libglu1-mesa-dev libasound2-dev
```


To build the audio driver and examples, the following is also required and available in your `PATH`:

 * GNU Make
 * python 3.10 or higher
 * [ca65](https://cc65.github.io/)
 * [PVSnesLib](https://github.com/alekmaul/pvsneslib/)
 * [tass64](https://tass64.sourceforge.net/)
 * [asar](https://github.com/RPGHacker/asar)


Build Scripts
=============

This project uses three build scripts:
 * [crates/compiler/build.rs](crates/compiler/build.rs): Assembles the audio driver
   and generates a `symbols.rs` file containing audio-driver addresses and constants.
 * [crates/shvc-sound-emu/build.rs](crates/shvc-sound-emu/build.rs): Compiles c++ code using the `cxx_build` crate.
 * [crates/tad-gui/build.rs](crates/tad-gui/build.rs): Converts markdown files to HTML files so they
   can be embedded into the GUI application.


Build Instructions
==================

 * run `cargo build --release`
 * (optionally) run `make` in the `audio-driver` directory to assemble the audio driver binaries

<!-- END RELEASE CUT -->
Licensing
=========
The terrific audio driver is copyright (c) 2023, Marcus Rowe.

See [docs/licenses.md](docs/licenses.md) for full license text.

 * The audio driver (S-SMP and `.spc` code) is licensed under the [zlib License](audio-driver/LICENSE).
 * The spc700asm assembler is licensed under the [MIT License](crates/spc700asm/LICENSE).
 * The compiler and GUI are licensed under the [MIT License](crates/tad-compiler/LICENSE).
 * The audio emulator used by the GUI is licensed under the [ISC License](crates/shvc-sound-emu/LICENSE).

 * The compiler and GUI make use of multiple third party open source projects.
 * The GUI is based in part on the work of the FLTK project (https://www.fltk.org).


