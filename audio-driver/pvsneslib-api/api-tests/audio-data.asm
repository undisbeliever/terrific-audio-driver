;; Audio driver binaries and dummy audio data


.include "hdr.asm"

.if defined(LOROM)
    BANK_SIZE  =  $8000
.elif defined(HIROM)
    BANK_SIZE  = $10000
.else
    .fail "Unknown memory map"
.endif


.export Tad_Loader_SIZE, Tad_AudioDriver_SIZE

.section "Tad_AudioDriverBin" SUPERFREE
    Tad_Loader_Bin: .incbin "../../loader.bin"
    Tad_AudioDriver_Bin: .incbin "../../audio-driver.bin"

    Tad_Loader_SIZE = _sizeof_Tad_Loader_Bin
    Tad_AudioDriver_SIZE = _sizeof_Tad_AudioDriver_Bin
.ends


; Sizes MUST match `tad-tests.c`
COMMON_AUDIO_DATA_SIZE = 3000
SONG_DATA_SIZE = 2000

_COMMON_AUDIO_DATA_PART1_SIZE = 1500
_COMMON_AUDIO_DATA_PART2_SIZE = COMMON_AUDIO_DATA_SIZE - _COMMON_AUDIO_DATA_PART1_SIZE

_SONG_DATA_PART1_SIZE =  999
_SONG_DATA_PART2_SIZE = SONG_DATA_SIZE - _SONG_DATA_PART1_SIZE

.section "AudioData_CAD_1" FORCE PRIORITY 1000 BANK 1 ORG (BANK_SIZE - _COMMON_AUDIO_DATA_PART1_SIZE)
    DummyCommonAudioData_Part1:
        .dsb    _COMMON_AUDIO_DATA_PART1_SIZE, $01
    DummyCommonAudioData_Part1End:
.ends

.section "AudioData_CAD_2" FORCE PRIORITY 1000 BANK 2 ORG 0
    DummyCommonAudioData_Part2:
        .dsb    _COMMON_AUDIO_DATA_PART2_SIZE, $01
    DummyCommonAudioData_Part2End:
.ends

.section "AudioData_SD_1" FORCE PRIORITY 1000 BANK 2 ORG (BANK_SIZE - _SONG_DATA_PART1_SIZE)
    DummySongData_Part1:
        .dsb    _SONG_DATA_PART1_SIZE, $02
    DummySongData_Part1End:
.ends

.section "AudioData_SD_2" FORCE PRIORITY 1000 BANK 3 ORG 0
    DummySongData_Part2:
        .dsb    _SONG_DATA_PART2_SIZE, $02
    DummySongData_Part2End:
.ends



