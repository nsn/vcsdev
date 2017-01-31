; blank vcs 2600 project

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------

; ntsc constants
VBLANK_WAIT = 42
NUM_SCANLINES = 191

BGCOL_DARK = $E4
BGCOL_LIGHT = $E8

;--- end Constants

;----------------------------
; Macros   
;----------------------------

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

tmp                 ds 2
Sec0_PF_l_ptr       ds 2
Sec0_PF_r_ptr       ds 2
Sec1_PF_l_ptr       ds 2
Sec1_PF_r_ptr       ds 2
BGCol_odd           ds 1
BGCol_even          ds 1


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
    jsr GameState
    jsr DrawScreen
    jsr OverScan
    jmp MainLoop ; loop forever

;----------------------------
; vertical blank init
;----------------------------
VerticalBlank:
    ldx #0      
    lda #2
    sta WSYNC   ; ?
    ; begin vertical sync
    sta VSYNC
    ; first two lines of vsync
    sta WSYNC   
    sta WSYNC   
    ; use duration 3rd line of VSYNC 
    ; to set the vertical blank timer
    lda #VBLANK_WAIT
    sta TIM64T

    lda #0
    ; end vsync period
    sta WSYNC 
    sta VSYNC
    rts ;--- VerticalBlank

;----------------------------
; calculate game state for this frame
;----------------------------
GameState:
    ; set playfield data pointers 
    ; according to position in maze
    SET_POINTER Sec0_PF_l_ptr, PF_1_0 
    SET_POINTER Sec0_PF_r_ptr, PF_1_1

    SET_POINTER Sec1_PF_l_ptr, PF_1_1 
    SET_POINTER Sec1_PF_r_ptr, PF_1_0

    ; set background color
    ; according to position in maze
    lda #BGCOL_LIGHT
    sta BGCol_odd
    lda #BGCOL_DARK
    sta BGCol_even
    sta COLUBK

    rts ;--- GameState

;----------------------------
; Draw visible scanlines
;----------------------------
DrawScreen:
    ; wait until vertical blank period is over
    lda INTIM
    bne DrawScreen
    sta VBLANK ; since A = #0


    ; Y will be our scanline counter
    ; ---
    ; 16 Scanlines of Section0
    ldy #15
Section0:
    sta WSYNC
    lda (Sec0_PF_l_ptr),y   ; +5
    and #%11110000          ; +2
    sta PF0                 ; +3 
    lda #0                  ; +2    
    sta PF1                 ; +3 
    sta PF2                 ; +3 (18) 
    ; wait for PF0 to finish drawing
    SLEEP 16
    lda #0                  ;    
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    ; wait for P23 to finish drawing
    SLEEP 8
    lda (Sec0_PF_r_ptr),y
    and #%11110000
    sta PF2
    ; bg color
    lda BGCol_even
    sta COLUBK
BREAK
    dey
    bpl Section0
    ; ---
    ; 16 Scanlines of Section0
    ldy #15
Section1:
    sta WSYNC
    lda #%11110000
    sta PF0

    lda (Sec1_PF_l_ptr),y   ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 
    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait 10 cycles
    SLEEP 16

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec1_PF_r_ptr),y
    sta PF1                 ; +3 (8)
    and #%11110000
    sta PF2

    ; bg color
    lda BGCol_odd
    sta COLUBK
    SLEEP 10 
    ;lda (PF3_data_ptr),y
    ;sta PF0
    dey
    bne Section1

    ldy #191-32
ScanLoop:
    lda #0
    sta PF0
    sta PF1
    sta PF2
    ; WSYNC is placed BEFORE calculations
    sta WSYNC

    ; work done, dec scanline counter, clean up
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
; Data
;----------------------------

    include "pfdata.inc"



;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


