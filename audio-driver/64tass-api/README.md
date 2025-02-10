Terrific Audio Driver 64tass API
================================

Safety
======

The Terrific Audio Driver can deadlock if:

 * `Tad_Init` is called more than once.
 * A `Tad_*` subroutine is called inside an Interrupt Service Routine.
 * A `Tad_*` subroutine is called by multiple threads.
 * A `TadPrivate_*` subroutine or macro is called outside of the API.
 * A `TadPrivate_*` private variable is modified outside of the API.



Using Terrific Audio Driver in a 64tass Project
===============================================

The 64tass API is identical to the ca65 API.
Please see [ca65-api/tad-audio.inc](../ca65-api/tad-audio.inc) for the API documentation.

Private functions, macros and variables have been prefixed with `TadPrivate_`.
Do not use them outside of the TAD API.


Compiling the API
-----------------

Assign the string `"LOROM"` or `"HIROM"` to the `TAD_MEMORY_MAP` constant, depending on memory map.

Include the four include files into the 64tass assembly file.
 * **tad-zeropage.inc**: zeropage variables.
    * Contains the sound effect queue and should be placed if the zeropage section.
    * Must be placed somewhere in the lowram segment ($0000 - $1fff or $7e0000-$7e1fff).
 * **tad-lowram.inc**: lowram variables.
    * Must be placed somewhere in the lowram segment ($0000 - $1fff or $7e0000-$7e1fff).
 * **tad-code.inc**: Subroutines that update or read the queue and quickly return.
    * All public functions in this file are called with `JSR` addressing
 * **tad-process.inc**: Subroutines that communicate with audio-driver and process messages
    * All public functions in this file are called with `JSL` long addressing
    * `tad-process.inc` and `tad-code.inc` can be placed in separate banks


Simplified example:

``` asm
TAD_MEMORY_MAP = "LOROM"

.section Zeropage
    .include "{{path to API}}/tad-zeropage.inc"
.send

; Lowram is the mirrored RAM addresses at $0100 - $1fff
.section Lowram
    .include "{{path to API}}/tad-lowram.inc"
.send

.section Bank80
    .include "{{path to API}}/tad-process.inc"

    .include "{{path to API}}/tad-code.inc"
.send
```


Embedding Audio Data
--------------------

There are two ways to embed audio data into a ROM:

1. Automatically using `tad-compiler ca65-export`.
   `tad-compiler tass-export` will output:
    * a binary file that contains the audio-driver, common-audio data and song data.
    * an assembly file containing `.incbin` statements and a `LoadAudioData` subroutine.
    * an (optional) `.inc` file containing Song and SFX enums.

2. Manually, by creating an assembly file that:
   * Embeds and exports the audio driver binaries.
     See the `External Resources` section of [ca65-api tad-audio.inc](../ca65-api/tad-audio.inc) for more details.
   * Exports a `LoadAudioData` far subroutine.
     See the `CALLBACKS` section of [ca65-api tad-audio.inc](../ca65-api/tad-audio.inc) for more details.
      * The common-audio data can be compiled with the `tad-compiler common` command.
      * The song data can be compiled with the `tad-compiler song` command.
      * IMPORTANT NOTE: `tad-compiler song` does not test if the song can fit in Audio-RAM and
        there is no bounds checking in the loader.<br/>
        Please use the `tad-compiler check` command or `tad-gui` to verify the song data fits in
        Audio-RAM.
   * The `tad-compiler tass-enums` command can create an `.inc` file containing the Song and SFX enums.



Build Requirements
==================
 * [64tass](https://tass64.sourceforge.net/)

To compile the tests, you also need:
 * GNU Make


Testing
=======

Testing is done by compiling the 64tass and ca65 APIs in a minimal environment (with no SNES code)
and checking if they output identical binaries.

To run the test, run `make` in the [test](test) directory.


