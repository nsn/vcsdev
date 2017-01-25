; blank vcs 2600 project

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------

; ntsc constants
VBLANK_WAIT = #43
NUM_SCANLINES = 191

;--- end Constants

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

tmp                 ds 2
scanline            ds 1    ; current scanline
block_no            ds 1    ; block number
section_idx         ds 1    ; section number
pf0_data            ds 1    ; Playfield register shadow vars
pf1_data            ds 1    ; Playfield register shadow vars
pf2_data            ds 1    ; Playfield register shadow vars
pf3_data            ds 1    ; Playfield register shadow vars
pf4_data            ds 1    ; Playfield register shadow vars
pf5_data            ds 1    ; Playfield register shadow vars


;--- end Variables 

    SEG code
    ORG $F000
Reset:
;----------------------------
; Start of program
;----------------------------

; clear RAM
    sei         ; disable interrupts
    cld         ; Clear BCD math flag
    ; set stack pointer to top of RAM
    ldx #$FF
    txs

    lda #0
ClearRam:
    sta 0,x
    dex
    bne ClearRam

; set TIA behaviour
    ; set bg color to black ($0)
    lda #$00
    sta COLUBK
    ; set pf color to white
    lda #$0E
    sta COLUPF
    ; set pf behaviour
    lda #%00000000
    sta CTRLPF

;----------------------------
; Main Loop
;----------------------------
MainLoop:
    jsr VerticalBlank
    jsr DrawScreen
    jsr OverScan
    jmp MainLoop ; loop forever

;----------------------------
; vertical blank wait
;----------------------------
VerticalBlank:
    ldx #0      ; ?
    lda #2
    sta WSYNC   ; ?
    sta WSYNC   ; ?
    sta WSYNC   ; ?
    ; begin vertical sync
    sta VSYNC
    ; first two lines of vsync
    sta WSYNC   
    sta WSYNC   
    ; use duration 3rd line of VSYNC 
    ; to set the vertical blank timer
    lda VBLANK_WAIT
    sta TIM64T
    ; clear collision latches
    lda #0
    sta CXCLR
    ; end vsync period
    sta WSYNC 
    sta VSYNC
    ;----------------
    ; free cycles!
    ;----------------

    ; insert per-frame initializaton code here

    ; wait until vertical blank period is over
VerticalBlankWait:
    lda INTIM
    bne VerticalBlankWait
    rts         ; return

;----------------------------
; Draw visible scanlines
;----------------------------
DrawScreen:
    ; wait for last line to finish
    sta WSYNC
    sta VBLANK ; since A = #0


    ; Y will be our scanline counter
    ldy #NUM_SCANLINES
ScanLoop:
    ; WSYNC is placed BEFORE calculations
    sta WSYNC
    ; save scanline counter
    sty scanline

    ; block no = y / 4
    tya                     ; +2
    lsr                     ; +2
    lsr                     ; +2
    ; transver block_no to y
    tay                     ; 
    ;sta block_no            ; +3
    ; section no = y/16
    lsr                     ; +2
    lsr                     ; +2
    ;sta section_idx         ; +3 = (16)
    ; section_idx to x
    tax

    ; jump to proper kernel subroutine
BREAK 
    lda SectionPointersHI,x
    pha
    lda SectionPointersLO,x
    pha
    rts

KernelEnd:

    ; work done, dec scanline counter, clean up
    ldy scanline
    DEY
    BNE ScanLoop    

    ; clear registers to prevent bleeding
    lda #2
    sta WSYNC   ; finish scanline
    sta VBLANK  ; make TIA output blank
    ; re-use Y which is still 0
    sty PF0
    sty PF1
    sty PF2  
    sty GRP0
    sty GRP1
    sty ENAM0
    sty ENAM1
    sty ENABL
    rts ; DrawScreen

;----------------------------
; Overscan
;----------------------------
OverScan:
    ; wait 30 scanline
    ldx #30
OverScanLineWait:
    sta WSYNC
    dex
    bne OverScanLineWait
    ; return
    rts

;----------------------------
; Kernel(s)
;----------------------------
; each kernel is responsible for (at least one) 'section'
; of the screen, a section most of the time consisting of
; 4 blocks of 4 scanlines each
; section | scanlines | desc
;    11   | 191-176   | top of tunnel, nearest
;    10   |           |
;     9   |           |
;     8   |           |
;     7   |           |
;     6   |           |
;     5   |           |
;     4   |           |
;     3   |           |
;     2   |           |
;     1   |           |
;     0   |           |
Section0:
    lda #>PFData_0_W
    sta tmp+1
    lda #<PFData_0_W
    sta tmp
    lda (tmp),y
    sta PF0
    jmp KernelEnd

; dummy section, just nops
SectionNOP:
    nop
    jmp KernelEnd


;----------------------------
; Data
;----------------------------

SectionPointersLO:
    .byte <(SectionNOP)  ; index  0
    .byte <SectionNOP  ; index  2
    .byte <SectionNOP  ; index  4
    .byte <SectionNOP  ; index  6
    .byte <SectionNOP  ; index  8
    .byte <SectionNOP  ; index 10
    .byte <SectionNOP  ; index 12
    .byte <SectionNOP  ; index 14
    .byte <SectionNOP  ; index 16
    .byte <SectionNOP  ; index 18
    .byte <SectionNOP  ; index 20
    .byte <(Section0-1)    ; index 22

SectionPointersHI:
    .byte >(SectionNOP)  ; index  0
    .byte >SectionNOP  ; index  2
    .byte >SectionNOP  ; index  4
    .byte >SectionNOP  ; index  6
    .byte >SectionNOP  ; index  8
    .byte >SectionNOP  ; index 10
    .byte >SectionNOP  ; index 12
    .byte >SectionNOP  ; index 14
    .byte >SectionNOP  ; index 16
    .byte >SectionNOP  ; index 18
    .byte >SectionNOP  ; index 20
    .byte >(Section0-1)    ; index 22

    include "pfdata.inc"




;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


