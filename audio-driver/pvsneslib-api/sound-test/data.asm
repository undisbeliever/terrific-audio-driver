;; Resources and Data used by the program

.include "hdr.asm"

.function SnesRgb(r, g, b)  ((r) | ((g) << 5) | ((b) << 10))

.section "Resources_Font" SUPERFREE
    Font_Tiles:     .incbin "tiles/font-2bpp.pic"

    Font_Palette:
        .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb(25, 25, 25)
        .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb(31, 31,  0)
        .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb( 0, 28,  0)
        .word   0, 0, SnesRgb( 0,  0,  0), SnesRgb( 7,  7,  7)
        .word   0, 0, SnesRgb(15, 15,  0), SnesRgb(31, 31, 31)
        .word   0, 0, SnesRgb(15, 15,  0), SnesRgb( 0,  0,  0)
.ends


; Adding gen to the include directory list as wla-dx does not check the directory of the asm
; file in .incbin statements.
.incdir "gen/"

.include "gen/audio-data.asm"

