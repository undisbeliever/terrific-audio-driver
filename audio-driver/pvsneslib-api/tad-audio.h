/*
 * Terrific Audio Driver PVSnesLib API
 *
 * SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
 * SPDX-License-Identifier: Zlib
 *
 * Copyright © 2024 Marcus Rowe <undisbeliever@gmail.com>
 *
 * This software is provided 'as-is', without any express or implied warranty.  In
 * no event will the authors be held liable for any damages arising from the use of
 * this software.
 *
 * Permission is granted to anyone to use this software for any purpose, including
 * commercial applications, and to alter it and redistribute it freely, subject to
 * the following restrictions:
 *
 *      1. The origin of this software must not be misrepresented; you must not
 *         claim that you wrote the original software. If you use this software in
 *         a product, an acknowledgment in the product documentation would be
 *         appreciated but is not required.
 *
 *      2. Altered source versions must be plainly marked as such, and must not be
 *         misrepresented as being the original software.
 *
 *      3. This notice may not be removed or altered from any source distribution.
 */


/*!
 * \file tad-audio.h
 * \brief Terrific Audio Driver PVSnesLib API
 *
 *
 * Lifetimes and Safety
 * ====================
 *
 * These functions MUST NOT be called in an interrupt ISR.  They are not thread-safe.
 *
 * tad_init() MUST be called first, before any other `tad_*` functions.
 * tad_init() has additional restrictions, see the tad_init() documentation for more details.
 *
 * The audio-data loaded by the loadAudioData() callback have additional lifetime requirements
 * that must be met.  See the loadAudioData() documentation for more details.
 *
 * All functions and variables in `tad-audio.asm` ending in a double-underscore (__) are private.
 *  * Calling a private subroutine can cause a crash as they do not honor the PVSnesLib ABI.
 *  * Modifying a private variable can cause a state mismatch or deadlock.
 *
 * `tad-audio.asm` MUST BE the only code that accesses the $2140-$2143 APUIO registers.
 *
 *
 * tad-audio.asm conflicts with SNESMOD
 * ------------------------------------
 *
 * Since PVSnesLib is bundled with SNESMOD, you MUST NOT call any function in
 * the `<snes/sound.h>` library that comes with PVSnesLib.
 *
 * Luckily the wla-dx linker strips unused functions from the program, so it is easy
 * to test if a program calls a SNESMOD function by inspecting the symbol file.
 *
 * If the symbol file contains any of the following, the program contains SNESMOD, which will
 * cause SPC IO communication conflicts, resulting in deadlocks and/or crashes.
 *
 *  * `SECTIONSTART_.sm_spc`
 *  * `SECTIONSTART_.soundmod`
 *  * `SECTIONSTART_.reg_sounds`
 *  * a function starting with `spc`
 *
 *
 * The following command can be used to detect if SNESMOD is in your program.
 * If it outputs any lines, it is highly likely SNESMOD is exists in your program and the calls
 * to either SNESMOD or the *Terrific Audio Driver* API need to be removed to prevent conflicts:
 *
 *      grep -E 'SECTIONSTART_.sm_spc|SECTIONSTART_.soundmod|SECTIONSTART_.reg_sounds| spc\w+' FILE.sym
 *
 *
 *
 * Adding tad-audio.asm to a PVSnesLib project
 * ===========================================
 *
 * 1. Add either a `LOROM` or `HIROM` define to `hdr.asm`, depending on the memory map used.
 *    PVSnesLib version 4.2.0 and earlier only use LOROM mapping.
 *
 *    `tad-audio.asm` uses these defines to determine how to advance the audio-data pointer
 *    when it has crossed a bank boundary.
 *
 * 2. Add `tad-audio.asm` to the build.  Either by:
 *    * Creating a blank assembly file that contains a single `.include` to `tad-audio.asm`.  
 *      (See `pvsneslib-api/sound-test/tad-audio.asm` for an example).
 *    * Copying `tad-audio.asm` and `tad-audio.h` to your source directory.  
 *      This is not recommended as the IO protocol in `tad-audio.asm`, the audio-driver binaries
 *      and `tad-compiler` MUST match.
 *
 * 3. Embed the audio driver and audio data into your project.  There are two ways to do this:
 *     * Automatically using the `tad-compiler pv-export` command.  
 *       See the `pvsneslib-api/sound-test` example's Makefile and `audio-data.asm` for an example.
 *     * Manually embedding the data and creating a loadAudioData() callback.
 *       See below for more details.
 *
 * 4. Optionally create include header containing the Song and SFX enums using the
 *    `tad-compiler pv-enums` command.
 *
 *
 * External Resources
 * ==================
 *
 * To increase compatibility with existing resource subsystems, `tad-audio.asm` does not embed
 * the audio driver or audio data into the ROM.
 *
 * The audio driver is divided into 3 files (see `pvsneslib-api/api-tests/audio-data.asm`
 * for an example that adds these files to a program):
 *  * `loader.bin` - the custom spc700 loader.  Imported as `Tad_Loader_Bin`, size is read from `Tad_Loader_SIZE`.
 *  * `audio-driver.bin` - the spc700 audio driver.  Imported as `Tad_AudioDriver_Bin`, size is read from `Tad_AudioDriver_SIZE`.
 *  * `blank-song.bin` - a blank (silent) song.  Imported as `Tad_BlankSong_Bin`, size is read from `Tad_BlankSong_SIZE`.
 *
 * Audio data is loaded using the external callback loadAudioData().  loadAudioData() is
 * called when *Common Audio Data* or *Song Data* need to be loaded into Audio-RAM.
 * Please see `loadAudioData` documentation as the audio data has complex lifetime requirements.
 *
 * There is no bounds checking in `tad-audio.asm`.
 *  * loadAudioData() is responsible for determining if the input to tad_loadSong() is valid.
 *  * The audio driver (*Common Audio Data*) is responsible for determining if the *sfx id*
 *    is valid.
 *
 * The `tad-compiler pv-export` command can output a single uncompressed binary file that
 * contains the audio-driver and audio-data and an assembly file that contains audio driver
 * exports and loadAudioData() callback.
 *
 * Alternatively, the developer can create their own loadAudioData() callback subroutine and
 * populate the ROM with the output of the `tad-compiler common` and `tad-compiler song`
 * commands.
 *
 */

#ifndef TAD_AUDIO__INCLUDE_H__
#define TAD_AUDIO__INCLUDE_H__

#include <snes/snestypes.h>


/*! Maximum pan value (100% to the right) */
#define MAX_PAN 128

/*! Center pan value (default if pan is not specified) */
#define CENTER_PAN 64

/*! Minimum tick clock value for tad_queueCommand_setSongTempo() and tad_queueCommandOverride_setSongTempo() */
#define MIN_TICK_CLOCK 64


/*!
 * Initialises the audio driver:
 *
 *  * Loads the loader into Audio-RAM
 *  * Loads the audio driver into Audio-RAM
 *  * Sets the song to 0 (silence)
 *  * Resets variables
 *  * Sets the flags
 *      * Sets the *Reload Common Audio Data* flag
 *      * Sets the *Play Song Immediately* flag
 *      * Clears the *Stereo* flag (mono output)
 *  * Queues a common audio data transfer
 *
 * This function will require multiple frames of execution time.
 *
 * REQUIRES: S-SMP reset
 *
 * TIMING
 * ------
 *  * Should be called more than 40 scanlines after reset
 *  * MUST be called ONCE
 *     * Calling tad_init() more than once will hardlock.
 *  * MUST be called with INTERRUPTS DISABLED
 *      * Interrupts are enabled in consoleInit().
 *      * tad_init() should be called before consoleInit().
 *  * MUST be called while the S-SMP is running the IPL.
 *  * MUST be called after the loadAudioData() callback is setup (if necessary)
 *  * tad_init() MUST be called before any other TAD functions.
 */
void tad_init(void);

/*!
 * Processes the next queue.
 *
 * This function will do one of the following, depending on the state:
 *  * Transfer data to the Audio-RAM
 *  * Wait for the loader and call loadAudioData() callback when the loader is ready to receive new data
 *  * Send a command to the audio driver
 *  * Send a play-sound effect command to the audio driver
 *
 * NOTE: The command and sound-effect queues will be reset after a new song is loaded into Audio-RAM.
 *
 * TIMING:
 *  * MUST be called after tad_init().
 *  * Should be called once per frame.
 *  * MUST NOT be called in an interrupt ISR.
 */
void tad_process(void);

/*!
 * Finish loading the data into audio-RAM.
 *
 * tad_finishLoadingData() will not transfer data if the state is `WAITING_FOR_LOADER`.
 * It will only transfer data if the loader is in the middle of transferring data
 * (when tad_isLoaderActive() returns true).
 *
 * This function can be safely called by `loadAudioData`.
 *
 * This function may require multiple frames of execution time.
 */
void tad_finishLoadingData(void);


/*!
 * @name Queue IO Commands
 *
 * The following functions will add a command to the command queue if the queue is empty.
 * The command queue can only hold 1 command.
 *
 * @{
 */

/*!
 * Queue a pause command.
 *
 * Stops song and sound effect execution.
 * * IO Commands will still be executed when the audio-driver is paused.
 * * The audio driver starts paused unless the `PlaySongImmediately` flag is set.
 *
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_pause(void);

/*!
 * Queue an unpause command.
 *
 * * Resets the S-SMP timer counters,
 *   can cause issues if the S-CPU spams unpause commands.
 *
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_unpause(void);

/*!
 * Queue a stop-sound-effects command.
 *
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_stopSoundEffects(void);

/*!
 * Queue a set-main-volume command.
 *
 * NOTE: The main volume is reset whenever a new song is loaded.
 *
 * @param volume signed i8 main volume
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_setMainVolume(s8 volume);

/*!
 * Queue a set-enabled-channels command.
 *
 * Enables or disable channels.  Useful for disabling channels in a song.
 *
 * NOTE: The enabled channels bitmask is reset whenever a new song is loaded.
 *
 * @param mask A bitmask of the 8 channels that can send key-on events
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_setEnabledChannels(u8 mask);

/*!
 * Queues a set-song-tempo command.
 *
 * NOTE: The song can still change the tempo.
 *
 * @param tickClock The new S-DSP TIMER_0 register value (MUST be >= MIN_TICK_CLOCK 64, is bounds checked)
 * @return true if the command was added to the command queue.
 */
bool tad_queueCommand_setSongTempo(u8 tickClock);

/*!
 * @}
 */

/*!
 * @name Queue IO Command Override
 *
 * The following functions will add a command to the command queue, overriding
 * any unsent IO commands in the command queue.
 *
 * @{
 */

/*!
 * Queue a pause command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_pause()
 */
void tad_queueCommandOverride_pause(void);

/*!
 * Queue a unpause command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_unpause()
 */
void tad_queueCommandOverride_unpause(void);

/*!
 * Queue a stop-sound-effects command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_stopSoundEffects()
 */
void tad_queueCommandOverride_stopSoundEffects(void);

/*!
 * Queue a set-main-volume command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_setMainVolume()
 */
void tad_queueCommandOverride_setMainVolume(s8 volume);

/*!
 * Queue a set-enabled-channels command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_setEnabledChannels()
 */
void tad_queueCommandOverride_setEnabledChannels(u8 mask);

/*!
 * Queue a set-song-tempo command (overriding any unprocessed IO commands)
 * @see tad_queueCommand_setSongTempo()
 */
void tad_queueCommandOverride_setSongTempo(u8 tickClock);

/*!
 * @}
 */


/*!
 * Queue the next sound effect to play, with panning.
 *
 * NOTE: Only 1 sound effect can be played at a time
 * NOTE: Lower sound effect IDs take priority over higher sound effect IDs.
 *
 * @param sfx_id the sound effect to play
 * @param pan pan value. 0 = 100% to the left, MAX_PAN = 100% to the right.
 */
void tad_queuePannedSoundEffect(u8 sfx_id, u8 pan);

/*!
 * Queue the next sound effect to play with center pan (MAX_PAN/2).
 *
 * NOTE: Only 1 sound effect can be played at a time.
 * NOTE: Lower sound effect IDs take priority over higher sound effect IDs.
 *
 * @param sfx_id the sound effect to play
 */
void tad_queueSoundEffect(u8 sfx_id);

/*!
 * Disables the audio driver, starts the loader and queues a song transfer.
 *
 * This function will not restart the loader if the loader is loading common audio data.
 *
 * CAUTION: The audio driver starts in the paused state if the `PlaySongImmediately` flag is false.
 *
 * CAUTION: tad_loadSong() will switch the state to `WAITING_FOR_LOADER`.  loadAudioData() will
 * not be called until tad_process() is called **and** the audio-driver has switched to the loader.
 * While the state remains `WAITING_FOR_LOADER`, no audio data will be transferred and calling
 * tad_finishLoadingData() will not transfer any audio data.
 *
 * @param song_id the song id to play (0 = silence, first song starts at 1)
 */
void tad_loadSong(u8 song_id);


/*!
 * Calls tad_loadSong() if `song_id` != the `song_id` used in the last tad_loadSong() call.
 *
 * @param song_id the song id to play (0 = silence, first song starts at 1)
 * @return true if the song_id changed and tad_loadSong() was called.
 *
 * @see tad_loadSong() for details about how songs are loaded into the audio driver.
 */
bool tad_loadSongIfChanged(u8 song_id);


/*!
 * Returns the current song id.
 *
 * @return the song_id used in the last tad_loadSong() call.
 */
u8 tad_getSong();


/*!
 * @name Flags and setting functions
 *
 * @{
 */

/*!
 * If this subroutine is called, the *common audio data* will be reloaded into Audio-RAM.
 * This will not take effect until the next song is loaded into Audio-RAM.
 */
void tad_reloadCommonAudioData(void);

/*!
 * Clears the stereo flag.
 * This will not take effect until the next song is loaded into Audio-RAM.
 */
void tad_setMono(void);

/*!
 * Set the stereo flag.
 * This will not take effect until the next song is loaded into Audio-RAM.
 */
void tad_setStereo(void);

/*!
 * Reads the mono/stereo flag.
 * @return true if stereo, false if mono
 */
bool tad_getStereoFlag(void);

/*!
 * Sets the `PlaySongImmediately` flag
 */
void tad_songsStartImmediately(void);

/*!
 * Clears the `PlaySongImmediately` flag
 */
void tad_songsStartPaused(void);

/*!
 * Sets the number of bytes to transfer to Audio-RAM per `tad_process` call.
 *
 * The value will be clamped from `MIN_TRANSFER_PER_FRAME` to `MAX_TRANSFER_PER_FRAME`.
 */
void tad_setTransferSize(u16 transferSize);

/*!
 * @}
 */


/*!
 * @name State querying functions
 *
 * @{
 */

/*!
 * @return true if the loader is still using data returned by `loadAudioData` (state == `LOADING_*`)
 */
bool tad_isLoaderActive(void);

/*!
 * @return true if the song is loaded into audio-RAM (state is `PAUSED` or `PLAYING`)
 */
bool tad_isSongLoaded(void);

/*!
 * @return true if the song is playing (state is `PLAYING`)
 */
bool tad_isSongPlaying(void);

/*!
 * @}
 */


/*!
 * @name Callbacks
 *
 * @{
 */

/*! The data that is returned by loadAudioData() */
struct Tad_AudioData {
    /*! The address of the data.  Must be valid if `size != 0`. */
    u8* data;
    /*! The size of the data.  If `size` is 0, then there is no data. */
    u16 size;
};

/*! The variable that contains the loadAudioData() callback output */
extern struct Tad_AudioData loadAudioData_out;

/*!
 * This callback will be called by tad_process() when the common audio data or song
 * data need to be loaded into Audio-RAM.
 *
 * **loadAudioData() does not return a value**.  Instead this function will write
 * the data address and size to the @ref loadAudioData_out variable.
 *
 * If the input is *common audio data* (`id = 0`) this function MUST write a valid
 * address and non-zero size to @ref loadAudioData_out.
 *
 * loadAudioData() is also responsible for determining if the input is a valid song.
 * If @ref loadAudioData_out is not written to (or `loadAudioData_out.size` is 0),
 * a blank (silent) song will be used instead.
 *
 * loadAudioData() can be generated by `tad-compiler`. Alternatively, the developer
 * can create their own loadAudioData() function if they wish to use their own
 * resources subsystem or want to use compressed song data.
 *
 * The data is allowed to cross bank boundaries. When the data crosses a bank
 * boundary, the data pointer is advanced to start of the next bank (as determined
 * by the bank byte and memory map).
 *
 *
 * This callback is called using the PVSnesLib ABI.
 *
 *
 * @param id The audio resource to load
 *      * If `id` is 0, load *common audio data*.  
 *        This function must write a valid address and non-zero size to
 *        @ref loadAudioData_out if `id == 0`.
 *      * If `id >= 1`, load *song data*.  
 *        This function can skip the @ref loadAudioData_out write if `id` is not a
 *        valid song and `id >= 1`.
 *
 *
 * LIFETIME
 * --------
 *  * The data MUST remain in memory while it is being transferred to Audio-RAM
 *    (which may take several frames).
 *  * The data can be freed on the next loadAudioData() call.
 *  * The data can be freed when the state changes to PAUSED or PLAYING.
 *  * The data can be freed if the tad_isLoaderActive() function returns false.
 *  * tad_finishLoadingData() can be used to flush decompressed memory into Audio-RAM.
 *    The data can be freed immediately after a tad_finishLoadingData() call.
 *
 * loadAudioData() **MUST NOT** call tad_process() or tad_loadSong().
 *
 * loadAudioData() **is allowed** to call tad_finishLoadingData().
 */
extern void loadAudioData(u8 id);

/*!
 * @}
 */

#endif

