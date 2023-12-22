Terrific Audio Driver
=====================

A homebrew audio driver for the Super Nintendo.

**NOTICE:** This audio driver is still in development.
The samples, MML syntax and bytecodes are subject to change without notice


Features
========
 * A custom loader to speed-up data loading.
 * A bytecode-based song and sound effect data format and interpreter.
 * Sound effects.
 * Songs written in Music Macro Language (MML). ([mml syntax](docs/mml-syntax.md))
 * Vibrato.
 * Portamento.
 * A GUI for editing and previewing samples, sound effects and songs.


Engine Limitations and Deliberate Design Decisions
==================================================
 * 6 channels are dedicated to music, 2 channels are dedicated to sound effects.
    * This will change.  I am planning to enable music that can be played using all 8 channels.
      Virtual channels are only partially implemented at the moment and will be fully implemented in
      early 2024.
 * There is no sample swapping.
    * Samples and sound effect data are combined to form the *common audio data* and transferred to
      audio-RAM in a single block.
    * The loader has the ability to override the common audio data.  If you want to have a special
      set of songs that contain unique songs (ie, intro and credits), you will need to create a new
      project (with a different set of samples) and manually swap the common audio data in S-CPU
      code.
 * All pitches and pitch offsets are precalculated by the compiler
    * The pitches are stored in a fixed-size 256 entry pitch table.
    * All pitch offsets are precalculated by the MML compiler.  To calculate this *pitch-offset per
      tick* value, the MML compiler needs to know what instrument is playing the portamento/vibrato
      note.  The MML syntax provides a way to manually set the *pitch-offset per tick* value.
    * Sound effects written in bytecode must manually the *pitch-offset per tick* value.
 * There is a fixed 1 tick delay after a key-off event.
    * 1 tick will be subtracted from any instruction that emits a key-off event.
      For example, a `play_note c+4 16` instruction will play the note, sleep for 15 ticks, emit a
      key-off event, and sleep for 1 final tick.
    * This delay is required to prevent popping.
 * A MML subroutine cannot call a MML subroutine.
 * A maximum of 3 loops can be nested at once (including loops within subroutine calls).
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
    * Sound effects cannot call subroutines.
    * The echo buffer and FIR Filter settings are set by the song.  If a sound effect enables echo,
      it can have an inconsistent sound (depending on the songs echo and FIR settings).
    * If both sound effect channels are occupied, new sound effects will be ignored.
    * Only 1 sound effect can be played per frame.  If the game requests two or more sound effects
      in a single frame, the one with the lowest index (as defined by the *Sound Effect Export
      Order*) will be prioritised.


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


Licensing
=========
The terrific audio driver is copyright (c) 2023, Marcus Rowe.

See [docs/licences.md](docs/licences.md) for full license text.

 * The audio driver (S-SMP and `.spc` code) is licensed under the [zlib License](audio-driver/LICENSE).
 * The compiler and GUI are licensed under the [MIT License](crates/tad-compiler/LICENSE).
 * The audio emulator used by the GUI is licensed under the [ISC License](crates/shvc-sound-emu/LICENSE).

 * The compiler and GUI make use of multiple third party open source projects.
 * The GUI is based in part on the work of the FLTK project (https://www.fltk.org).


