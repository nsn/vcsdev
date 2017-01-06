; playfield test

    processor 6502

    include "vcs.h"
    include "macro.h"

    SEG
    ORG $F000

Reset
;----------------------------
; Variables
;----------------------------


;----------------------------
; Start of program
;----------------------------
ClearRam

    ; use CLEAN_START from <macro.h>
    CLEAN_START

    ; set bg color to black ($0)
    lda #$00
    sta COLUBK
    ; set pf color to white
    lda #$0E
    sta COLUPF
    ; set pf behaviour
    lda #$00
    sta CTRLPF

    lda #%10101011
    sta PF1
;----------------------------
; Start of Framw
;----------------------------
FrameStart
    ; vblank
    lda #0
    sta VBLANK
    ; vsync
    lda #2
    sta VSYNC
    ; 3 scanlines of vsync
    sta WSYNC
    sta WSYNC
    sta WSYNC
    ; set timer for 37 scanlines
    ; 1 scanline = 76 CPU cycles
    ; 37*76 = 2812, 
    ; subtrct 14 cycles of timer overhead => 2798 cycles of wait
    ; TIMINT64 ticks once every 64 clock cycles
    ; 2798/64 = ~64
    lda #43
    sta TIM64T
    ; blank out vsync
    lda #0
    sta VSYNC

;----------------------------
; vertical blank wait
; put init logic here
;----------------------------

;----------------------------
; vertical blank wait
;----------------------------
VerticalBlank
    ; loop until timer reaches 0
    lda INTIM
    bne VerticalBlank 

    ; wait for end of line
    sta WSYNC
    ; lda is 0, so we can use it to end VBLANK period
    sta VBLANK

    ; wait one more line so that we are sure we line up...
    sta WSYNC

    ;-----------------
    ; activate movents here...
    ;-----------------



;----------------------------
; Scanline loop
;----------------------------
    ; y is our scanline counter
    ldy #191

Scanloop
    ; wait for prev line to finish
    sta WSYNC
    
    ; decrease line counter in Y
    ; lopp until y == 0
    dey
    bne Scanloop

    ; end of color lines
    ; 2 for VBLANK
    lda #2
    ; finish last line
    sta WSYNC
    ; make tia output invisible for overscan
    sta VBLANK


;----------------------------
; Overscan
;----------------------------
    ; wait for 30 lines
    ldx #30
OverScan
    sta WSYNC
    dex
    bne OverScan

    ; jmp to start of next frame
    jmp FrameStart

;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


