/*
 * Terrific Audio Driver PVSnesLib sound test.
 *
 * This example has been ported from the ca65 sound test to C.
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

#include <snes.h>

#include "../../tad-audio.h"
#include "gen/audio.h"

#define VRAM_BG3_MAP_WADDR  0x0400
#define VRAM_BG3_TILE_WADDR 0x1000

#define N_2BPP_PALETTES 6

extern char Font_Tiles, Font_Palette;


//! The currently selected menu item
u8 menuPos;

//! Selected channel bitfield
//! A maximum of ONE bit must be set in this variable
u8 selectedChannelMask;

u8 menu_song;
u8 menu_sfx;
u8 menu_sfxPan;
u8 menu_mainVolume;
u8 menu_musicVolume;
u8 menu_sfxVolume;
u8 menu_timerOverride;
u8 menu_channelMask;
u8 menu_audioMode;
bool menu_songStartsFlag;
bool menu_resetVolumesFlag;


#define U8_MAX 255

#define MAX_VOLUME 127


#define CURSOR_XPOS         2

#define MENU_LABEL_XPOS     (CURSOR_XPOS + 2)
#define VAR_XPOS            25
#define CHANNEL_MASK_XPOS   (VAR_XPOS - 5)

#define STATE_XPOS          23
#define STATE_YPOS          2

#define MENU_YPOS           3


//! outputs an index into scr_txt_font_map[index]
#define MENU_TO_TXT_ATTR_INDEX(x_, y_) ((MenuItemYPos[y_] << 6) + ((x_) << 1) + 1)


#define PAL_NORMAL                        0
#define PAL_SELECTED                      1
#define PAL_STATE                         2
#define PAL_ENABLED_CHANNEL               PAL_NORMAL
#define PAL_DISABLED_CHANNEL              3
#define PAL_SELECTED_AND_ENABLED_CHANNEL  4
#define PAL_SELECTED_AND_DISABLED_CHANNEL 5


enum MenuItem {
    MENU__PLAY_SONG,
    MENU__PLAY_SFX,
    MENU__SFX_PAN,
    MENU__MAIN_VOLUME,
    MENU__MUSIC_VOLUME,
    MENU__SFX_VOLUME,
    MENU__OVERRIDE_TIMER,
    MENU__CHANNEL_MASK,
    MENU__AUDIO_MODE,
    MENU__SONG_STARTS_FLAG,
    MENU__RESET_VOLUMES_FLAG,
    MENU__STOP_SOUND_EFFECTS,
    MENU__PAUSE_UNPAUSE_MUSIC,
    MENU__PAUSE_MUSIC_AND_SFX,
    MENU__RELOAD_COMMON_AUDIO_DATA,
};
#define N_MENU_ITEMS 15

const char* const STATE_LABEL__UNKNOWN = ".......";
const char* const STATE_LABEL__PLAYING = "PLAYING";
const char* const STATE_LABEL__SFX     = "SFX    ";
const char* const STATE_LABEL__PAUSED  = "PAUSED ";
const char* const STATE_LABEL__LOADING = "LOADING";

const char* const AUDIO_MODE_LABELS[3] = {
    "MONO    ",
    "STEREO  ",
    "SURROUND",
};

const char* const SONG_STARTS_SET_LABEL   = "SONGS START IMMEDIATELY";
const char* const SONG_STARTS_CLEAR_LABEL = "SONGS START PAUSED     ";

const char* const RESET_VOLUMES_FLAG_SET_LABEL   = "RESET VOLUMES ON SONG LOAD";
const char* const RESET_VOLUMES_FLAG_CLEAR_LABEL = "GLOBAL VOLUMES PERSIST    ";

const char* const MenuLabels[N_MENU_ITEMS] = {
    "PLAY SONG",
    "PLAY SFX",
    "SFX PAN",
    "MAIN VOLUME",
    "MUSIC VOLUME",
    "SFX VOLUME",
    "OVERRIDE TIMER",
    "MUSIC CHANNELS",
    NULL,
    NULL,
    NULL,
    "STOP SOUND EFFECTS (X)",
    "PAUSE / UNPAUSE (START)",
    "PAUSE MUSIC AND SFX",
    "RELOAD COMMON AUDIO DATA",
};

const u16 MenuItemYPos[N_MENU_ITEMS] = {
    MENU_YPOS + 0,
    MENU_YPOS + 2,
    MENU_YPOS + 3,
    MENU_YPOS + 5,
    MENU_YPOS + 7,
    MENU_YPOS + 8,
    MENU_YPOS + 10,
    MENU_YPOS + 11,
    MENU_YPOS + 13,
    MENU_YPOS + 14,
    MENU_YPOS + 15,
    MENU_YPOS + 17,
    MENU_YPOS + 19,
    MENU_YPOS + 21,
    MENU_YPOS + 23,
};

void menu_init(void);
void menu_printState(void);
void menu_printU8(enum MenuItem item, u16 value);
void menu_setAudioMode(u8 mode);
void menu_setSongStartsFlag(bool f);
void menu_setResetVolumesFlag(bool f);
void menu_setPos(u8 newPos);
void menu_updateChannelMask(void);
void highlightLine(u8 menuItem, u8 palette);
void menu_pauseUnpauseMusic(void);
void menu_pauseMusicAndSfx(void);
void menu_process_action(void);
void menu_process_item(void);
u8 menu_adjustValue(u8 value, enum MenuItem item, u8 min, u8 max, u16 pad);
u8 menu_adjustValue_slow(u8 value, enum MenuItem item, u8 min, u8 max);
u8 menu_adjustValue_fast(u8 value, enum MenuItem item, u8 min, u8 max);
void menu_process(void);


void menu_init(void) {
    u16 i;

    menuPos = 0;

    selectedChannelMask = 1;

    menu_song = 0;
    menu_sfx = 0;
    menu_sfxPan = TAD_CENTER_PAN;
    menu_mainVolume = MAX_VOLUME;
    menu_musicVolume = U8_MAX;
    menu_sfxVolume = U8_MAX;
    menu_timerOverride = 100;
    menu_channelMask = 0xff;

    menu_setSongStartsFlag(true);
    menu_setResetVolumesFlag(false);
    menu_setAudioMode(TAD_SURROUND);

    for (i = 0; i < N_MENU_ITEMS; i++) {
        const char* label = MenuLabels[i];
        if (label) {
            consoleDrawText(MENU_LABEL_XPOS, MenuItemYPos[i], "%s", label);
        }
    }

    consoleDrawText(CHANNEL_MASK_XPOS, MenuItemYPos[MENU__CHANNEL_MASK], "01234567");

    menu_printU8(MENU__PLAY_SONG, menu_song);
    menu_printU8(MENU__PLAY_SFX, menu_sfx);
    menu_printU8(MENU__SFX_PAN, menu_sfxPan);
    menu_printU8(MENU__MAIN_VOLUME, menu_mainVolume);
    menu_printU8(MENU__MUSIC_VOLUME, menu_musicVolume);
    menu_printU8(MENU__SFX_VOLUME, menu_sfxVolume);
    menu_printU8(MENU__OVERRIDE_TIMER, menu_timerOverride);

    menu_setPos(0);
    menu_updateChannelMask();
}

void menu_printState(void) {
    const char* label = STATE_LABEL__UNKNOWN;

    if (tad_isSongPlaying()) {
        label = STATE_LABEL__PLAYING;
    }
    else if (tad_isSfxPlaying()) {
        label = STATE_LABEL__SFX;
    }
    else if (tad_isSongLoaded()) {
        label = STATE_LABEL__PAUSED;
    }
    else if (tad_isLoaderActive()) {
        label = STATE_LABEL__LOADING;
    }

    consoleSetTextOffset(PAL_STATE << 10);
    consoleDrawText(STATE_XPOS, STATE_YPOS, "%s", label);
    consoleSetTextOffset(0);
}

void menu_printU8(enum MenuItem item, u16 value) {
    if (item >= N_MENU_ITEMS) {
        return;
    }

    value = value & 0xff;

    if (menuPos == item) {
        consoleSetTextOffset(PAL_SELECTED << 10);
    }
    consoleDrawText(VAR_XPOS, MenuItemYPos[item], "%3u", value);

    consoleSetTextOffset(0);
}

void menu_setAudioMode(u8 mode) {
    if (mode >= 0x80) {
        mode = 2;
    }
    else if (mode >= 3) {
        mode = 0;
    }
    menu_audioMode = mode;

    tad_audioMode = mode;

    if (menuPos == MENU__AUDIO_MODE) {
        consoleSetTextOffset(PAL_SELECTED << 10);
    }
    consoleDrawText(MENU_LABEL_XPOS, MenuItemYPos[MENU__AUDIO_MODE], "%s", AUDIO_MODE_LABELS[mode]);

    consoleSetTextOffset(0);
}

void menu_setSongStartsFlag(bool f) {
    menu_songStartsFlag = f;

    if (menuPos == MENU__SONG_STARTS_FLAG) {
        consoleSetTextOffset(PAL_SELECTED << 10);
    }
    consoleDrawText(MENU_LABEL_XPOS, MenuItemYPos[MENU__SONG_STARTS_FLAG], "%s",
                    (f ? SONG_STARTS_SET_LABEL : SONG_STARTS_CLEAR_LABEL));

    consoleSetTextOffset(0);

    if (f) {
        tad_songsStartImmediately();
    }
    else {
        tad_songsStartPaused();
    }
}

void menu_setResetVolumesFlag(bool f) {
    menu_resetVolumesFlag = f;

    if (menuPos == MENU__RESET_VOLUMES_FLAG) {
        consoleSetTextOffset(PAL_SELECTED << 10);
    }
    consoleDrawText(MENU_LABEL_XPOS, MenuItemYPos[MENU__RESET_VOLUMES_FLAG], "%s",
                    (f ? RESET_VOLUMES_FLAG_SET_LABEL : RESET_VOLUMES_FLAG_CLEAR_LABEL));

    consoleSetTextOffset(0);

    if (f) {
        tad_globalVolumesResetOnSongStart();
    }
    else {
        tad_globalVolumesPersist();
    }
}

void menu_setPos(u8 newPos) {
    if (newPos >= 0x80) {
        // pos underflowed
        newPos = N_MENU_ITEMS - 1;
    }
    else if (newPos >= N_MENU_ITEMS) {
        newPos = 0;
    }

    // Safety
    if (menuPos >= N_MENU_ITEMS) {
        menuPos = 0;
    }

    consoleDrawText(CURSOR_XPOS, MenuItemYPos[menuPos], " ");
    consoleDrawText(CURSOR_XPOS, MenuItemYPos[newPos], ">");

    highlightLine(menuPos, PAL_NORMAL);
    highlightLine(newPos, PAL_SELECTED);

    menuPos = newPos;

    menu_updateChannelMask();
}

void menu_updateChannelMask(void) {
    u16 index = MENU_TO_TXT_ATTR_INDEX(CHANNEL_MASK_XPOS, MENU__CHANNEL_MASK);

    u16 sel = 0;
    if (menuPos == MENU__CHANNEL_MASK) {
        sel = selectedChannelMask;
    }
    u16 m = menu_channelMask;

    u8 attr = 0;
    u16 i;
    for (i = 0; i < 8; i++) {
        if (m & 1) {
            attr = (sel & 1) ? (PAL_SELECTED_AND_ENABLED_CHANNEL << 2) : (PAL_ENABLED_CHANNEL << 2);
        }
        else {
            attr = (sel & 1) ? (PAL_SELECTED_AND_DISABLED_CHANNEL << 2) : (PAL_DISABLED_CHANNEL << 2);
        }

        scr_txt_font_map[index] = attr;
        index += 2;

        m >>= 1;
        sel >>= 1;
    }

    scr_txt_dirty = true;
}

//! highlight a single line
void highlightLine(u8 menuItem, u8 palette) {
    if (menuItem >= N_MENU_ITEMS) {
        return;
    }

    u16 index = MENU_TO_TXT_ATTR_INDEX(0, menuItem);

    const u8 attr = (palette & 7) << 2;

    u16 i;
    for(i = 0; i < 32; i++) {
        scr_txt_font_map[index] = attr;

        index += 2;
    }

    scr_txt_dirty = true;
}

void menu_pauseUnpauseMusic(void) {
    if (tad_isSongPlaying()) {
        // Tests `tad_queueCommand_*(void)` (built using a macro)
        tad_queueCommand_pauseMusicPlaySfx();
    }
    else {
        // Tests `tad_queueCommandOverride_*(void)` (built using a macro)
        tad_queueCommandOverride_unpause();
    }
}

void menu_pauseMusicAndSfx(void) {
    tad_queueCommand_pause();
}

//! Called if an action button is pressed
void menu_process_action(void) {
    switch (menuPos) {
    case MENU__PLAY_SONG:
        tad_loadSong(menu_song);
        if (menu_resetVolumesFlag) {
            menu_musicVolume = U8_MAX;
            menu_sfxVolume = U8_MAX;
            menu_printU8(MENU__MUSIC_VOLUME, U8_MAX);
            menu_printU8(MENU__SFX_VOLUME, U8_MAX);
        }
        break;

    case MENU__PLAY_SFX:
    case MENU__SFX_PAN:
        tad_queuePannedSoundEffect(menu_sfx, menu_sfxPan);
        break;

    case MENU__MAIN_VOLUME:
        // Tests `tad_queueCommandOverride_*(u8)` (built using a macro)
        tad_queueCommandOverride_setMainVolume(menu_mainVolume);
        break;

    case MENU__MUSIC_VOLUME:
        // Test set-global-volumes IO command and two-parameter command macro
        menu_musicVolume = 128;
        menu_sfxVolume = 255;
        menu_printU8(MENU__MUSIC_VOLUME, menu_musicVolume);
        menu_printU8(MENU__SFX_VOLUME, menu_sfxVolume);

        tad_queueCommand_setGlobalVolumes(128, 255);
        break;

    case MENU__SFX_VOLUME:
        // Test set-global-volumes IO command and two-parameter override command macro
        menu_musicVolume = 255;
        menu_sfxVolume = 128;
        menu_printU8(MENU__MUSIC_VOLUME, menu_musicVolume);
        menu_printU8(MENU__SFX_VOLUME, menu_sfxVolume);

        tad_queueCommandOverride_setGlobalVolumes(255, 128);
        break;

    case MENU__OVERRIDE_TIMER:
        // Tests `tad_queueCommandOverride_*(u8)` (built using a macro)
        tad_queueCommandOverride_setSongTimer(menu_timerOverride);
        break;

    case MENU__CHANNEL_MASK:
        menu_channelMask ^= selectedChannelMask;
        menu_updateChannelMask();
        // Tests `tad_queueCommandOverride_*(u8)` (built using a macro)
        tad_queueCommandOverride_setMusicChannels(menu_channelMask);
        break;

    case MENU__AUDIO_MODE:
        menu_setAudioMode(menu_audioMode + 1);
        break;

    case MENU__SONG_STARTS_FLAG:
        menu_setSongStartsFlag(!menu_songStartsFlag);
        break;

    case MENU__RESET_VOLUMES_FLAG:
        menu_setResetVolumesFlag(!menu_resetVolumesFlag);
        break;

    case MENU__STOP_SOUND_EFFECTS:
        tad_queueCommandOverride_stopSoundEffects();
        break;

    case MENU__PAUSE_UNPAUSE_MUSIC:
        menu_pauseUnpauseMusic();
        break;

    case MENU__PAUSE_MUSIC_AND_SFX:
        menu_pauseMusicAndSfx();
        break;

    case MENU__RELOAD_COMMON_AUDIO_DATA:
        tad_reloadCommonAudioData();
        break;
    }
}

//! Called every frame if an action button or up/down is not pressed
void menu_process_item(void) {
    const u16 keyPressed = padsDown(0);

    switch (menuPos) {
    case MENU__PLAY_SONG:
        menu_song = menu_adjustValue_slow(menu_song, MENU__PLAY_SONG, 0, LAST_SONG_ID);
        break;

    case MENU__PLAY_SFX:
#if N_SOUND_EFFECTS > 0
        menu_sfx = menu_adjustValue_slow(menu_sfx, MENU__PLAY_SFX, 0, N_SOUND_EFFECTS - 1);
#endif
        break;

    case MENU__SFX_PAN:
        menu_sfxPan = menu_adjustValue_fast(menu_sfxPan, MENU__SFX_PAN, 0, TAD_MAX_PAN);
        break;

    case MENU__MAIN_VOLUME: {
        const u8 v = menu_adjustValue_fast(menu_mainVolume, MENU__MAIN_VOLUME, 0, MAX_VOLUME);
        if (v != menu_mainVolume) {
            menu_mainVolume = v;

            // Tests `tad_queueCommand_*(u8)` (built using a macro)
            tad_queueCommand_setMainVolume(menu_mainVolume);
        }
        break;
    }

    case MENU__MUSIC_VOLUME: {
        const u8 v = menu_adjustValue_fast(menu_musicVolume, MENU__MUSIC_VOLUME, 0, U8_MAX);
        if (v != menu_musicVolume) {
            menu_musicVolume = v;

            tad_queueCommand_setGlobalMusicVolume(menu_musicVolume);
        }
        break;
    }

    case MENU__SFX_VOLUME: {
        const u8 v = menu_adjustValue_fast(menu_sfxVolume, MENU__SFX_VOLUME, 0, U8_MAX);
        if (v != menu_sfxVolume) {
            menu_sfxVolume = v;

            tad_queueCommand_setGlobalSfxVolume(menu_sfxVolume);
        }
        break;
    }

    case MENU__OVERRIDE_TIMER: {
        const u16 pad = padsCurrent(0);

        if (pad & KEY_LEFT) {
            if (menu_timerOverride == 0 || menu_timerOverride > TAD_MIN_TICK_CLOCK) {
                menu_timerOverride--;
                menu_printU8(MENU__OVERRIDE_TIMER, menu_timerOverride);
            }
        }
        else if (pad & KEY_RIGHT) {
            if (menu_timerOverride >= TAD_MIN_TICK_CLOCK) {
                menu_timerOverride++;
                menu_printU8(MENU__OVERRIDE_TIMER, menu_timerOverride);
            }
        }
        break;
    }

    case MENU__CHANNEL_MASK:
        if (keyPressed & (KEY_LEFT | KEY_RIGHT)) {
            if (keyPressed & KEY_RIGHT) {
                selectedChannelMask <<= 1;
                if (selectedChannelMask == 0) {
                    selectedChannelMask = 1;
                }
            }
            else {
                selectedChannelMask >>= 1;
                if (selectedChannelMask == 0) {
                    selectedChannelMask = 0x80;
                }
            }
            menu_updateChannelMask();
        }
        break;

    case MENU__AUDIO_MODE:
        if (keyPressed & KEY_LEFT) {
            menu_setAudioMode(menu_audioMode - 1);
        }
        else if (keyPressed & KEY_RIGHT) {
            menu_setAudioMode(menu_audioMode + 1);
        }
        break;

    case MENU__SONG_STARTS_FLAG:
        if (keyPressed & (KEY_LEFT | KEY_RIGHT)) {
            menu_setSongStartsFlag(!menu_songStartsFlag);
        }
        break;

    case MENU__RESET_VOLUMES_FLAG:
        if (keyPressed & (KEY_LEFT | KEY_RIGHT)) {
            menu_setResetVolumesFlag(!menu_resetVolumesFlag);
        }
        break;

    case MENU__STOP_SOUND_EFFECTS:
    case MENU__PAUSE_UNPAUSE_MUSIC:
    case MENU__PAUSE_MUSIC_AND_SFX:
    case MENU__RELOAD_COMMON_AUDIO_DATA:
        break;
    }
}

u8 menu_adjustValue(u8 value, enum MenuItem item, u8 min, u8 max, u16 pad) {
    if (pad & KEY_LEFT) {
        if (value > min) {
            value--;
            menu_printU8(item, value);
        }
    }
    else if (pad & KEY_RIGHT) {
        if (value < max) {
            value++;
            menu_printU8(item, value);
        }
    }

    return value;
}

u8 menu_adjustValue_slow(u8 value, enum MenuItem item, u8 min, u8 max) {
    menu_adjustValue(value, item, min, max, padsDown(0));
}

u8 menu_adjustValue_fast(u8 value, enum MenuItem item, u8 min, u8 max) {
    menu_adjustValue(value, item, min, max, padsCurrent(0));
}

void menu_process(void) {
    menu_printState();

    // Reset variables when songs are loaded
    if (tad_isSongLoaded() == false) {
        menu_mainVolume = MAX_VOLUME;
        menu_printU8(MENU__MAIN_VOLUME, menu_mainVolume);

        menu_channelMask = 0xff;
        menu_updateChannelMask();
    }


    u16 joyPressed = padsDown(0);

    if (joyPressed & KEY_UP) {
        menu_setPos(menuPos - 1);
    }
    else if (joyPressed & KEY_DOWN) {
        menu_setPos(menuPos + 1);
    }
    else if (joyPressed & (KEY_B | KEY_A)) {
        menu_process_action();
    }
    else if (joyPressed & KEY_START) {
        menu_pauseUnpauseMusic();
    }
    else if (joyPressed & KEY_X) {
        tad_queueCommandOverride_stopSoundEffects();
    }
    else {
        menu_process_item();
    }
}

int main(void) {
    // `consoleInit()` enables interrupts, which need to be disabled when loading
    // the audio-driver to audio-RAM.
    tad_init();

    consoleInit();

    consoleSetTextVramBGAdr(VRAM_BG3_MAP_WADDR);
    consoleSetTextVramAdr(VRAM_BG3_TILE_WADDR);
    consoleSetTextOffset(0x0000);
    consoleInitText(0, N_2BPP_PALETTES * 8, &Font_Tiles, &Font_Palette);

    bgSetGfxPtr(2, VRAM_BG3_TILE_WADDR);
    bgSetMapPtr(2, VRAM_BG3_MAP_WADDR, SC_32x32);

    setMode(BG_MODE1, 0);
    bgSetDisable(0);
    bgSetDisable(1);

    menu_init();

    WaitForVBlank();
    setScreenOn();

    while (true) {
        menu_process();
        tad_process();

        WaitForVBlank();
    }
}

