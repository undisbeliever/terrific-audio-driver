
.include "hdr.asm"


.section "loadAudioData_text" SUPERFREE

loadAudioData:
    ; Assert m and x flags are clear
    php
    sep     #$20
.accu 8
    lda     1,s
    and     #$30
    bne     @Fail

    ; Assert DB == $7e
    phb
    pla
    cmp     #$7e
    bne     @Fail

    plp
    jml     loadAudioData_c

@Fail:
    jml     assert_failure
.ends

