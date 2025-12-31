Terrific Audio Driver asar API
==============================

This API is compatible with asar 1.91 and the asar 2 beta.


Safety
======

The Terrific Audio Driver can deadlock if:

 * `Tad_Init` is called more than once.
 * A `Tad_*` subroutine is called inside an Interrupt Service Routine.
 * A `Tad_*` subroutine is called by multiple threads.
 * A `TadPrivate_*` subroutine or macro is called outside of the API.
 * A `TadPrivate.*` private variable is modified outside of the API.


Using Terrific Audio Driver in an asar Project
==============================================

The asar API is near identical to the ca65 API. (The variable names have been changed to make them compatible with asar's struct variable allocation.)

Please see [ca65-api/tad-audio.inc](../ca65-api/tad-audio.inc) for the API documentation.

The following public variables are renamed from the ca65 API:
 * `Tad_flags` is `Tad.flags`
 * `Tad_audioMode` is `Tad.audioMode`
 * `Tad_sfxQueue_sfx` is `TadSfxQueue.sfx`
 * `Tad_sfxQueue_pan` is `TadSfxQueue.pan`


Private functions, macros and variables have been prefixed with `TadPrivate_`.
Do not use them outside of the TAD API.


Compiling the API
-----------------

 * Declare the memory map by defining a `LOROM` or `HIROM` define.
 * Include `tad-constants.inc`.
 * Declare the variables listed in the [tad-variables.inc](tad-variables.inc) file.
    * The variables MUST be declared in the lowram segment ($0000-$1fff region of Work-RAM)
    * The `TadSfxQueue` variables can be in lowram or zeropage.
    * The variables can be defined using structs (see, [tad-variables.inc](tad-variables.inc))
      or labels (see [example/src/variables.inc](example/src/variables.inc)).
 * Set the program counter and include the `tad-control.inc` file
    * All subroutines in `tad-control.inc` are called with `jsr` and should be
      in the same data bank as the mainloop code.
 * Set the program counter and include the `tad-process.inc` file
    * All public subroutines in `tad-process.inc` are called with `jsl` and
      can be included in a different bank to `tad-control.inc`

`tad-control.inc` and `tad-process.inc` can be loaded in any order.


Overriding the Default Configuration
------------------------------------

The following defines are compile-time configurable (see [\_example-lorom.asm](example/src/_example-lorom.asm) for an example) if they are set before `tad-constants.inc` is included:

 * `TAD_DEFAULT_FLAGS` - the initial TAD flags.
 * `TAD_DEFAULT_AUDIO_MODE` - the default audio mode.
 * `TAD_DEFAULT_TRANSFER_PER_FRAME` - The default bytes to transfer every `Tad_Process` call.



Embedding Audio Data
--------------------

There are two ways to embed audio data into a ROM:

1. Automatically using `tad-compiler asar-export`.  See the included example for details.  
   `tad-compiler asar-export` will output:
    * a binary file that contains the audio-driver, common-audio data and song data.
    * an assembly file containing `incbin` statements and a `LoadAudioData` subroutine.
    * an (optional) `.inc` file containing Song and SFX enums.

2. Manually, by:
   * Embedding the audio driver binaries into the ROM.
     See the `External Resources` section of [ca65-api tad-audio.inc](../ca65-api/tad-audio.inc) for more details.
   * `LoadAudioData` far subroutine.
     See the `CALLBACKS` section of [ca65-api tad-audio.inc](../ca65-api/tad-audio.inc) for more details.
      * The common-audio data can be compiled with the `tad-compiler common` command.
      * The song data can be compiled with the `tad-compiler song` command.
      * IMPORTANT NOTE: `tad-compiler song` does not test if the song can fit in Audio-RAM and
        there is no bounds checking in the loader.<br/>
        Please use the `tad-compiler check` command or `tad-gui` to verify the song data fits in
        Audio-RAM.
   * The `tad-compiler asar-enums` command can create an `.inc` file containing the Song and SFX enums.



Testing
=======

Testing is done by compiling the asar and 64tass APIs in a minimal environment (with no SNES code)
and checking if they output identical binaries.

To run the test, run `make` in the [test](test) directory.

