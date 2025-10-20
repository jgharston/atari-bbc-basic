
    org $2000

FONT:
    icl 'font.s'

main:
    jmp *

    run main

; ----------------------------------------------------------------------------

    TARGET_ATARI=1

    icl 'basic/basic.s'
