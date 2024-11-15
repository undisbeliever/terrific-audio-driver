;; Minimal SNES register definitions

; SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
; SPDX-License-Identifier: Zlib
;
; Copyright © 2024 Marcus Rowe <undisbeliever@gmail.com>
;
; This software is provided 'as-is', without any express or implied warranty.  In
; no event will the authors be held liable for any damages arising from the use of
; this software.
;
; Permission is granted to anyone to use this software for any purpose, including
; commercial applications, and to alter it and redistribute it freely, subject to
; the following restrictions:
;
;      1. The origin of this software must not be misrepresented; you must not
;         claim that you wrote the original software. If you use this software in
;         a product, an acknowledgment in the product documentation would be
;         appreciated but is not required.
;
;      2. Altered source versions must be plainly marked as such, and must not be
;         misrepresented as being the original software.
;
;      3. This notice may not be removed or altered from any source distribution.


INIDISP     = $2100
OBJSEL      = $2101
OAMADD      = $2102
OAMADDL     = $2102
OAMADDH     = $2103
OAMDATA     = $2104
BGMODE      = $2105
MOSAIC      = $2106
BG1SC       = $2107
BG2SC       = $2108
BG3SC       = $2109
BG4SC       = $210a
BG12NBA     = $210b
BG34NBA     = $210c
BG1HOFS     = $210d
BG1VOFS     = $210e
BG2HOFS     = $210f
BG2VOFS     = $2110
BG3HOFS     = $2111
BG3VOFS     = $2112
BG4HOFS     = $2113
BG4VOFS     = $2114
M7HOFS      = $210d
M7VOFS      = $210e
VMAIN       = $2115
VMADD       = $2116
VMADDL      = $2116
VMADDH      = $2117
VMDATA      = $2118
VMDATAL     = $2118
VMDATAH     = $2119
M7SEL       = $211a
M7A         = $211b
M7B         = $211c
M7C         = $211d
M7D         = $211e
M7X         = $211f
M7Y         = $2120
CGADD       = $2121
CGDATA      = $2122
W12SEL      = $2123
W34SEL      = $2124
WOBJSEL     = $2125
WH0         = $2126
WH1         = $2127
WH2         = $2128
WH3         = $2129
WBGLOG      = $212a
WOBJLOG     = $212b
TM          = $212c
TS          = $212d
TMW         = $212e
TSW         = $212f
CGWSEL      = $2130
CGADSUB     = $2131
COLDATA     = $2132
SETINI      = $2133
MPY         = $2134
MPYMH       = $2135
MPYL        = $2134
MPYM        = $2135
MPYH        = $2136
SLHV        = $2137
OAMDATAREAD = $2138
VMDATAREAD  = $2139
VMDATALREAD = $2139
VMDATAHREAD = $213a
CGDATAREAD  = $213b
OPHCT       = $213c
OPVCT       = $213d
STAT77      = $213e
STAT78      = $213f
APUIO_OUT_0 = $2140
APUIO_OUT_1 = $2141
APUIO_OUT_2 = $2142
APUIO_OUT_3 = $2143
APUIO_IN_0  = $2140
APUIO_IN_1  = $2141
APUIO_IN_2  = $2142
APUIO_IN_3  = $2143
WMDATA      = $2180
WMADD       = $2181
WMADDML     = $2181
WMADDL      = $2181
WMADDM      = $2182
WMADDH      = $2183

JOYOUT      = $4016
JOYSER0     = $4016
JOYSER1     = $4017

NMITIMEN    = $4200
WRIO        = $4201
HTIME       = $4207
HTIMEL      = $4207
HTIMEH      = $4208
VTIME       = $4209
VTIMEL      = $4209
VTIMEH      = $420a
MDMAEN      = $420b
HDMAEN      = $420c
MEMSEL      = $420d
RDNMI       = $4210
TIMEUP      = $4211
HVBJOY      = $4212

WRMPYA      = $4202
WRMPYB      = $4203
WRDIV       = $4204
WRDIVL      = $4204
WRDIVH      = $4205
WRDIVB      = $4206
RDDIV       = $4214
RDDIVL      = $4214
RDDIVH      = $4215
RDMPY       = $4216
RDMPYL      = $4216
RDMPYH      = $4217

RDIO        = $4213
JOY1        = $4218
JOY2        = $421a
JOY3        = $421c
JOY4        = $421e
JOY1L       = $4218
JOY2L       = $421a
JOY3L       = $421c
JOY4L       = $421e
JOY1H       = $4218
JOY2H       = $421a
JOY3H       = $421c
JOY4H       = $421e

DMAP0       = $4300
DMAP1       = $4310
DMAP2       = $4320
DMAP3       = $4330
DMAP4       = $4340
DMAP5       = $4350
DMAP6       = $4360
DMAP7       = $4370
BBAD0       = $4301
BBAD1       = $4311
BBAD2       = $4321
BBAD3       = $4331
BBAD4       = $4341
BBAD5       = $4351
BBAD6       = $4361
BBAD7       = $4371
A1T0        = $4302
A1T1        = $4312
A1T2        = $4322
A1T3        = $4332
A1T4        = $4342
A1T5        = $4352
A1T6        = $4362
A1T7        = $4372
A1T0L       = $4302
A1T1L       = $4312
A1T2L       = $4322
A1T3L       = $4332
A1T4L       = $4342
A1T5L       = $4352
A1T6L       = $4362
A1T7L       = $4372
A1T0H       = $4303
A1T1H       = $4313
A1T2H       = $4323
A1T3H       = $4333
A1T4H       = $4343
A1T5H       = $4353
A1T6H       = $4363
A1T7H       = $4373
A1B0        = $4304
A1B1        = $4314
A1B2        = $4324
A1B3        = $4334
A1B4        = $4344
A1B5        = $4354
A1B6        = $4364
A1B7        = $4374
DAS0        = $4305
DAS1        = $4315
DAS2        = $4325
DAS3        = $4335
DAS4        = $4345
DAS5        = $4355
DAS6        = $4365
DAS7        = $4375
DAS0L       = $4305
DAS1L       = $4315
DAS2L       = $4325
DAS3L       = $4335
DAS4L       = $4345
DAS5L       = $4355
DAS6L       = $4365
DAS7L       = $4375
DAS0H       = $4306
DAS1H       = $4316
DAS2H       = $4326
DAS3H       = $4336
DAS4H       = $4346
DAS5H       = $4356
DAS6H       = $4366
DAS7H       = $4376
DASB0       = $4307
DASB1       = $4317
DASB2       = $4327
DASB3       = $4337
DASB4       = $4347
DASB5       = $4357
DASB6       = $4367
DASB7       = $4377
A2A0        = $4308
A2A1        = $4318
A2A2        = $4328
A2A3        = $4338
A2A4        = $4348
A2A5        = $4358
A2A6        = $4368
A2A7        = $4378
A2A0L       = $4308
A2A1L       = $4318
A2A2L       = $4328
A2A3L       = $4338
A2A4L       = $4348
A2A5L       = $4358
A2A6L       = $4368
A2A7L       = $4378
A2A0H       = $4309
A2A1H       = $4319
A2A2H       = $4329
A2A3H       = $4339
A2A4H       = $4349
A2A5H       = $4359
A2A6H       = $4369
A2A7H       = $4379
NLTR0       = $430a
NLTR1       = $431a
NLTR2       = $432a
NLTR3       = $433a
NLTR4       = $434a
NLTR5       = $435a
NLTR6       = $436a
NLTR7       = $437a


DMAP_TRANSFER_ONE             = %000
DMAP_TRANSFER_TWO             = %001
DMAP_TRANSFER_WRITE_TWICE     = %010
DMAP_TRANSFER_TWO_WRITE_TWICE = %011
DMAP_TRANSFER_FOUR            = %100

DMAP_TRANSFER_READ_TWICE      = DMAP_TRANSFER_WRITE_TWICE
DMAP_TRANSFER_TWO_READ_TWICE  = DMAP_TRANSFER_TWO_WRITE_TWICE

DMAP_FIXED                    = $08
DMAP_DECREMENT                = $10
DMAP_INDIRECT                 = $40
DMAP_B_TO_A                   = $80


NMITIMEN_VBLANK = $80
NMITIMEN_JOYPAD = $01

JOYPAD_L_A      = %10000000
JOYPAD_L_X      = %01000000
JOYPAD_L_L      = %00100000
JOYPAD_L_R      = %00010000

JOYPAD_H_B      = %10000000
JOYPAD_H_Y      = %01000000
JOYPAD_H_SELECT = %00100000
JOYPAD_H_START  = %00010000
JOYPAD_H_UP     = %00001000
JOYPAD_H_DOWN   = %00000100
JOYPAD_H_LEFT   = %00000010
JOYPAD_H_RIGHT  = %00000001
JOYPAD_H_DPAD   = %00001111

JOYPAD_B        = %1000000000000000
JOYPAD_Y        = %0100000000000000
JOYPAD_SELECT   = %0010000000000000
JOYPAD_START    = %0001000000000000
JOYPAD_UP       = %0000100000000000
JOYPAD_DOWN     = %0000010000000000
JOYPAD_LEFT     = %0000001000000000
JOYPAD_RIGHT    = %0000000100000000
JOYPAD_A        = %0000000010000000
JOYPAD_X        = %0000000001000000
JOYPAD_L        = %0000000000100000
JOYPAD_R        = %0000000000010000

JOYPAD_BUTTONS  = %1100000011000000
JOYPAD_DPAD     = %0000111100000000
JOYPAD_SHOULDER = %0000000000110000

