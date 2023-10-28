SHVC-SOUND emulator
===================

Rust bindings for a SNES audio emulator created using modified [ares](https://ares-emu.net/) source
code.

**NOTE:** This crate is not designed to play `.spc` files.  It is designed to play music and sound
effects created using the Terrific Audio Driver.

Changes from ares:
 * Removed the Node, debugger, serialization and disassembler methods.
 * Added a sample buffer.
 * Removed the libco based scheduler and switched to a hard-coded smp/dsp scheduler that executes
   SPC instructions until the sample buffer is full.
 * Removed global variables.
 * Uses `std::array<uint8_t>` for memory arrays.
 * Added `DSP::resetEchoBuffer()`.
 * Added rust bindings using the [cxx](https://cxx.rs/) crate.


Build Requirements
==================
 * Rust
 * Cargo
 * A C++17 compiler, compatible with the [cxx](https://cxx.rs/) crate.


Licensing
=========
The `shvc-sound-emu` crate is released under the [ISC licence](LICENSE) (the same licence as ares).


