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
 * ca65, 64tass and pvsneslib APIs
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
      set of songs that contain unique songs (ie, intro and credits), you will need to create a new
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
 * There is a fixed 1 tick delay after a key-off event.
    * 1 tick will be subtracted from any instruction that emits a key-off event.
      For example, a `play_note c+4 16` instruction will play the note, sleep for 15 ticks, emit a
      key-off event, and sleep for 1 final tick.
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

 * Rust
 * Cargo
 * Git
 * GNU Make
 * A C++17 compiler, compatible with the [cxx](https://cxx.rs/) crate.
 * CMake
 * Rust (version > 1.55), CMake (version > 3.11), Git and a C++17 compiler to compile fltk-rs.
   (See [fltk-rs build dependencies](https://github.com/fltk-rs/fltk-rs/blob/master/README.md#build-dependencies) for more details.)
    * MSVC: Windows SDK
    * Linux/BSD: X11 and OpenGL development headers
 * SDL 2 to compile the [rust-sdl2](https://github.com/Rust-SDL2/rust-sdl2) crate
    * Windows (MSVC):  Follow the instructions on the [rust-sdl2 README](https://github.com/Rust-SDL2/rust-sdl2/blob/master/README.md#windows-msvc) to install and setup SDL2.
    * Linux/BSD: Install the SDL2 development headers


Build Scripts
=============

This project uses two build scripts:
 * [crates/compiler/build.rs](crates/compiler/build.rs): Compiles the audio driver using wiz and
   extracts audio-driver addresses from a symbol file.
 * [crates/shvc-sound-emu/build.rs](crates/shvc-sound-emu/build.rs): Compiles c++ code using the `cxx_build` crate.
 * [crates/tad-gui/build.rs](crates/tad-gui/build.rs): Converts markdown files to HTML files so they
   can be embedded into the GUI application.


Build Instructions
==================

 * Import the `wiz` git submodule.
 * Compile `wiz`.
    * See [Wiz - Building Source](https://github.com/wiz-lang/wiz#building-source) for more details.
    * The [compiler build script](crates/compiler/build.rs) expects a binary called `wiz` or
      `wiz.exe` in the `wiz/bin/` directory.
 * run `cargo build --release`


<!-- END RELEASE CUT -->
Licensing
=========
The terrific audio driver is copyright (c) 2023, Marcus Rowe.

See [docs/licenses.md](docs/licenses.md) for full license text.

 * The audio driver (S-SMP and `.spc` code) is licensed under the [zlib License](audio-driver/LICENSE).
 * The compiler and GUI are licensed under the [MIT License](crates/tad-compiler/LICENSE).
 * The audio emulator used by the GUI is licensed under the [ISC License](crates/shvc-sound-emu/LICENSE).

 * The compiler and GUI make use of multiple third party open source projects.
 * The GUI is based in part on the work of the FLTK project (https://www.fltk.org).


