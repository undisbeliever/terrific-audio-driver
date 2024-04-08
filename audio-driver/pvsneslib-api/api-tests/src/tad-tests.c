/*
 * Terrific Audio Driver PVSnesLib unit tests
 *
 * These tests have been ported from the ca65 API tests to C.
 * If you modify these tests, please also modify the corresponding ca65 API test.
 *
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


#include "../../tad-audio.h"


#define ASSERT_EQ(t, v) if ((t) != (v)) { assert_failure(); }

#define N_ELEMENTS(a) ((sizeof(a) / sizeof((a)[0])))


// Values MUST MATCH `audio-data.asm`
#define DUMMY_COMMON_AUDIO_DATA_SIZE 3000
#define DUMMY_SONG_DATA_SIZE 2000

extern u8 DummyCommonAudioData_Part1;
extern u8 DummySongData_Part1;


// ::HACK manually accessing the sfxQueue to test if the values were added to the SFX queue::
extern u8 tad_sfxQueue__;
extern u8 tad_sfxQueue_pan__;


// from main.c
void assert_failure(void);


static void finishLoading(void);
static void wait(void);
static void waitForLoader(void);

static void queueSoundEffect_assertSuccess(u8 sfx_id);
static void queueSoundEffect_assertFail(u8 sfx_id);
static void queuePannedSoundEffect_assertSuccess(u8 sfx_id, u8 pan);
static void queuePannedSoundEffect_assertFail(u8 sfx_id, u8 pan);
static u16 countTransfers_song1(void);


void test_finishLoadingData(void) {
    tad_loadSong(1);
    waitForLoader();

    tad_finishLoadingData();

    ASSERT_EQ(tad_isLoaderActive(), false)
    ASSERT_EQ(tad_isSongLoaded(), true)
}

// also tests `tad_reloadCommonAudioData`
void test_finishLoadingData2(void) {
    tad_reloadCommonAudioData();

    tad_loadSong(1);
    waitForLoader();

    tad_finishLoadingData();

    // common audio data loaded
    // State = WAITING_FOR_LOADER
    ASSERT_EQ(tad_isLoaderActive(), false)
    ASSERT_EQ(tad_isSongLoaded(), false)

    waitForLoader();
    tad_finishLoadingData();

    ASSERT_EQ(tad_isLoaderActive(), false)
    ASSERT_EQ(tad_isSongLoaded(), true)
}

void test_loadSong(void) {
    ASSERT_EQ(tad_isSongLoaded(), true)

    tad_loadSong(0);

    ASSERT_EQ(tad_isSongLoaded(), false)

    waitForLoader();

    // Loading blank song, it should only require 1 Tad_Process call to load it
    tad_process();
    ASSERT_EQ(tad_isSongLoaded(), true)
    ASSERT_EQ(tad_isLoaderActive(), false)
}

void test_loadSongWhileLoaderActive(void) {
    ASSERT_EQ(tad_isSongLoaded(), true)

    tad_loadSong(1);

    ASSERT_EQ(tad_isSongLoaded(), false)

    waitForLoader();
    tad_process();
    tad_process();
    tad_process();

    ASSERT_EQ(tad_isLoaderActive(), true)
    tad_loadSong(0);
    ASSERT_EQ(tad_isLoaderActive(), false)

    waitForLoader();

    ASSERT_EQ(tad_isLoaderActive(), true)
    finishLoading();
}

void test_loadSongWhileLoadingCommonAudioData(void) {
    tad_reloadCommonAudioData();

    tad_loadSong(1);
    ASSERT_EQ(tad_isSongLoaded(), false)
    ASSERT_EQ(tad_isLoaderActive(), false)

    waitForLoader();
    tad_process();
    tad_process();
    tad_process();

    ASSERT_EQ(tad_isLoaderActive(), true)
    tad_loadSong(1);

    // Tests `tad_loadSong()` did not switch to the WAITING_FOR_LOADER state
    ASSERT_EQ(tad_isLoaderActive(), true)
}

void test_queueCommand(void) {
    bool r;

    ASSERT_EQ(tad_isSongPlaying(), true)

    r = tad_queueCommand_pause();
    ASSERT_EQ(r, true)

    wait();
    tad_process();

    // Assert PAUSE command changed state
    ASSERT_EQ(tad_isSongPlaying(), false)


    // Queue is empty
    r = tad_queueCommand_unpause();
    ASSERT_EQ(r, true)

    // Assert pause/playing state unchanged
    ASSERT_EQ(tad_isSongPlaying(), false)

    // Command queue contains an UNPAUSE command

    // Queue is full
    r = tad_queueCommand_stopSoundEffects();
    ASSERT_EQ(r, false)

    // Queue is full
    r = tad_queueCommand_setMainVolume(128);
    ASSERT_EQ(r, false)

    // Process command
    wait();
    tad_process();

    // Assert UNPAUSE command changed state
    ASSERT_EQ(tad_isSongPlaying(), true)


    // Queue is empty
    r = tad_queueCommand_setMainVolume(20);
    ASSERT_EQ(r, true)

    // Queue is full
    r = tad_queueCommand_setEnabledChannels(0);
    ASSERT_EQ(r, false)

    // Queue is full
    r = tad_queueCommand_pause();
    ASSERT_EQ(r, false)

    // process command
    wait();
    tad_process();

    // Command was SET_MAIN_VOLUME
    // Confirm playing state unchanged
    ASSERT_EQ(tad_isSongPlaying(), true)
}

void test_queueCommandOverride(void) {
    bool r;

    ASSERT_EQ(tad_isSongPlaying(), true)

    tad_queueCommandOverride_pause();

    wait();
    tad_process();

    // Assert PAUSE command changed state
    ASSERT_EQ(tad_isSongPlaying(), false)


    tad_queueCommandOverride_setMainVolume(20);

    // Test queueCommand will not fill the queue
    r = tad_queueCommand_pause();
    ASSERT_EQ(r, false)

    tad_queueCommandOverride_unpause();

    // Assert paused/plating state unchanged
    ASSERT_EQ(tad_isSongPlaying(), false)

    wait();
    tad_process();

    // Assert UNPAUSE command changed state
    ASSERT_EQ(tad_isSongPlaying(), true)


    tad_queueCommandOverride_setMainVolume(20);
    tad_queueCommandOverride_setEnabledChannels(0);
    tad_queueCommandOverride_stopSoundEffects();

    wait();
    tad_process();

    // Command was STOP_SOUND_EFFECTS
    // Confirm playing state unchanged
    ASSERT_EQ(tad_isSongPlaying(), true)
}

void test_queuePannedSoundEffect(void) {
    ASSERT_EQ(tad_isSongPlaying(), true);

    queuePannedSoundEffect_assertSuccess(0xfe, 1);
    queuePannedSoundEffect_assertSuccess(10, 2);
    queuePannedSoundEffect_assertFail(11, 3);
    queuePannedSoundEffect_assertSuccess(9, 4);
    queuePannedSoundEffect_assertFail(10, 5);

    queuePannedSoundEffect_assertFail(0xfe, 6);

    wait();
    tad_process();

    // PLAY_SOUND_EFFECT command sent to the audio driver

    queuePannedSoundEffect_assertSuccess(0xfe, 7);

    // Clear SFX queue so it doesn't interfere with the next test
    wait();
    tad_process();
}

void test_queueSoundEffect(void) {
    ASSERT_EQ(tad_isSongPlaying(), true);

    queueSoundEffect_assertSuccess(0xfe);
    queueSoundEffect_assertSuccess(10);
    queueSoundEffect_assertFail(11);
    queueSoundEffect_assertSuccess(9);
    queueSoundEffect_assertFail(10);

    queueSoundEffect_assertFail(0xfe);

    wait();
    tad_process();

    // PLAY_SOUND_EFFECT command sent to the audio driver

    queueSoundEffect_assertSuccess(0xfe);

    // Clear SFX queue so it doesn't interfere with the next test
    wait();
    tad_process();
}

void test_commandAndSfxQueuePriority(void) {
    bool r;

    ASSERT_EQ(tad_isSongPlaying(), true);

    queueSoundEffect_assertSuccess(0);

    wait();
    tad_process();

    // PLAY_SOUND_EFFECT command sent to the audio driver

    queueSoundEffect_assertSuccess(10);

    r = tad_queueCommand_stopSoundEffects();
    ASSERT_EQ(r, true);

    wait();
    tad_process();

    // STOP_SOUND_EFFECTS command sent to the audio driver
    // Sound effect queue is unchanged

    // Test sfx cannot be added to the queue
    queueSoundEffect_assertFail(20);

    wait();
    tad_process();

    // PLAY_SOUND_EFFECT command sent to the audio driver

    // Test sfx can be added to the queue
    queueSoundEffect_assertSuccess(100);

    // Clear SFX queue so it doesn't interfere with the next test
    wait();
    tad_process();
}

void test_commandAndSfxQueueEmptyAfterSongLoad(void) {
    bool r;

    queueSoundEffect_assertSuccess(0);

    r = tad_queueCommand_stopSoundEffects();
    ASSERT_EQ(r, true);

    tad_loadSong(1);
    waitForLoader();
    tad_process();
    tad_process();

    // Test SFX queue is unchanged while the loader is active
    queueSoundEffect_assertFail(100);

    // Test command queue is unchanged while the loader is active
    r = tad_queueCommand_stopSoundEffects();
    ASSERT_EQ(r, false);

    finishLoading();

    // Test the two queues are now empty by trying to populate them
    queueSoundEffect_assertSuccess(0xfe);

    r = tad_queueCommand_stopSoundEffects();
    ASSERT_EQ(r, true);

    // Clear the two queues so they do don't interfere with the next test
    wait();
    tad_process();

    wait();
    tad_process();
}

// Skipped TestQueuePannedSoundEffectKeepsXY16
// Skipped TestQueuePannedSoundEffectKeepsXY8
// Skipped TestQueueSoundEffectKeepsXY16
// Skipped TestQueueSoundEffectKeepsXY8

void test_monoStereo(void) {
    tad_setMono();
    ASSERT_EQ(tad_getStereoFlag(), false);

    tad_setStereo();
    ASSERT_EQ(tad_getStereoFlag(), true);
}

// Does NOT test if the PlaySongImmediately flag is sent to the loader
void test_songStartsImmediately(void) {
    tad_songsStartImmediately();

    tad_loadSong(0);
    finishLoading();

    ASSERT_EQ(tad_isSongPlaying(), true);
}

// Does NOT test if the PlaySongImmediately flag is sent to the loader
void test_songStartPaused(void) {
    tad_songsStartPaused();

    tad_loadSong(0);
    finishLoading();

    ASSERT_EQ(tad_isSongLoaded(), true);
    ASSERT_EQ(tad_isSongPlaying(), false);
}


// The minimum and maximum values for tad_setTransferSize(u16)
#define MIN_TRANSFER 32
#define MAX_TRANSFER 800

void test_setTransferSize(void) {
    ASSERT_EQ(DUMMY_SONG_DATA_SIZE, 2000);

    ASSERT_EQ(tad_isSongLoaded(), true);

    // Also tests `loader` rounds up transfer size to 100
    tad_setTransferSize(99);
    ASSERT_EQ(countTransfers_song1(), 20);

    tad_setTransferSize(250);
    ASSERT_EQ(countTransfers_song1(), 8);

    // Test `Tad_SetTransferSize` enforces minimum value
    tad_setTransferSize(10);
    ASSERT_EQ(countTransfers_song1(), (DUMMY_SONG_DATA_SIZE + MIN_TRANSFER + 1) / MIN_TRANSFER);

    // Test `Tad_SetTransferSize` enforces maximum value
    tad_setTransferSize(0x6000);
    ASSERT_EQ(countTransfers_song1(), (DUMMY_SONG_DATA_SIZE + MAX_TRANSFER + 1) / MAX_TRANSFER);
}


static const VoidFn TAD_TESTS[] = {
    test_finishLoadingData,
    test_finishLoadingData2,
    test_loadSong,
    test_loadSongWhileLoaderActive,
    test_loadSongWhileLoadingCommonAudioData,
    test_queueCommand,
    test_queueCommandOverride,
    test_queuePannedSoundEffect,
    test_queueSoundEffect,
    test_commandAndSfxQueuePriority,
    test_commandAndSfxQueueEmptyAfterSongLoad,
    // Skipped TestQueuePannedSoundEffectKeepsXY16
    // Skipped TestQueuePannedSoundEffectKeepsXY8
    // Skipped TestQueueSoundEffectKeepsXY16
    // Skipped TestQueueSoundEffectKeepsXY8
    test_monoStereo,
    test_songStartsImmediately,
    test_songStartPaused,
    test_setTransferSize,
};

void runTests(void) {
    // Not testing with different DB values
    u16 testIndex = 0;


    for (testIndex=0; testIndex < N_ELEMENTS(TAD_TESTS); testIndex++) {
        // Reset TAD state
        tad_setTransferSize(256);
        tad_songsStartImmediately();
        // Switch to a blank song
        tad_loadSong(0);

        finishLoading();
        wait();

        TAD_TESTS[testIndex]();
    }
}


static void finishLoading(void) {
    while (tad_isSongLoaded() == false) {
        tad_process();
    }

    ASSERT_EQ(tad_isSongLoaded(), true);
    ASSERT_EQ(tad_isLoaderActive(), false)
}


static void wait(void) {
    u16 counter;

    for (counter=0; counter < 5000; counter++) {
    }
}

static void waitForLoader(void) {
    ASSERT_EQ(tad_isSongLoaded(), false)
    ASSERT_EQ(tad_isLoaderActive(), false)

    do {
        tad_process();
    } while (tad_isLoaderActive() == false);

    // assert state is LOADING_COMMON_AUDIO_DATA
    ASSERT_EQ(tad_isLoaderActive(), true)
    ASSERT_EQ(tad_isSongLoaded(), false)
}

static void queueSoundEffect_assertSuccess(u8 sfx_id) {
    tad_queueSoundEffect(sfx_id);

    // Test the queue contains sfx_id
    ASSERT_EQ(tad_sfxQueue__, sfx_id);
    ASSERT_EQ(tad_sfxQueue_pan__, CENTER_PAN);
}

static void queueSoundEffect_assertFail(u8 sfx_id) {
    u8 oldSfxId = tad_sfxQueue__;
    u8 oldPan = tad_sfxQueue_pan__;

    tad_queueSoundEffect(sfx_id);

    // Test oldSfxId unchanged.
    ASSERT_EQ(tad_sfxQueue__, oldSfxId);
    ASSERT_EQ(tad_sfxQueue_pan__, oldPan);
}

static void queuePannedSoundEffect_assertSuccess(u8 sfx_id, u8 pan) {
    tad_queuePannedSoundEffect(sfx_id, pan);

    // Test the sfx queue contains sfx_id and pan
    ASSERT_EQ(tad_sfxQueue__, sfx_id);
    ASSERT_EQ(tad_sfxQueue_pan__, pan);
}

static void queuePannedSoundEffect_assertFail(u8 sfx_id, u8 pan) {
    u8 oldSfxId = tad_sfxQueue__;
    u8 oldPan = tad_sfxQueue_pan__;

    tad_queuePannedSoundEffect(sfx_id, pan);

    // Test sfx queue is unchanged
    ASSERT_EQ(tad_sfxQueue__, oldSfxId);
    ASSERT_EQ(tad_sfxQueue_pan__, oldPan);
}

//! Returns the number of `tad_process()` calls required to load song id 1 to audio-RAM
static u16 countTransfers_song1(void) {
    u16 counter = 0;

    tad_loadSong(1);
    waitForLoader();

    while (tad_isSongLoaded() == false) {
        tad_process();
        counter++;
    }

    return counter;
}

void loadAudioData(u8 id) {
    switch(id) {
    case 0:
        loadAudioData_out.data = &DummyCommonAudioData_Part1;
        loadAudioData_out.size = DUMMY_COMMON_AUDIO_DATA_SIZE;
        break;

    case 1:
        loadAudioData_out.data = &DummySongData_Part1;
        loadAudioData_out.size = DUMMY_SONG_DATA_SIZE;
        break;
    }
}

