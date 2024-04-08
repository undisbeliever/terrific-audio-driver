/*
 * Terrific Audio Driver PVSnesLib unit tests
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

#define TESTING_COLOR 0b000001111111111
#define SUCCESS_COLOR 0b000001111100000
#define FAIL_COLOR 0b000000000011111

void runTests(void);


int main(void) {
    // `tad_init()` should be called before `consoleInit()`.
    // `consoleInit()` enables interrupts, which need to be disabled when loading
    // the audio-driver to audio-RAM.
    tad_init();

    consoleInit();

    setPaletteColor(0, TESTING_COLOR);

    setScreenOn();


    while (true) {
        runTests();

        WaitForVBlank();
        // Should hopefully still be in VBlank
        setPaletteColor(0, SUCCESS_COLOR);
    }
}

void assert_failure(void) {
    consoleMesenBreakpoint();

    WaitForVBlank();
    // Should hopefully still be in VBlank
    setPaletteColor(0, FAIL_COLOR);

    while (true) {
        WaitForVBlank();
    }
}

