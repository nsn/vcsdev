; VCS Crawl 
; (c) 2017 Michael Bayer
; 
;
; TODO
;

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------

; ntsc constants
VBLANK_WAIT = 42
NUM_SCANLINES = 192

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

Vb_tmp00                ds 1
Vb_tmp01                ds 1
Vb_tmp02                ds 1
Vb_tmp03                ds 1
Vb_tmp04                ds 1
Vb_tmp05                ds 1
Vb_tmp06                ds 1
Vb_tmp07                ds 1
Vb_tmp08                ds 1
Vb_tmp09                ds 1
Vb_tmp10                ds 1
Vb_tmp11                ds 1
Vb_tmp12                ds 1
Vb_tmp13                ds 1
Vb_tmp14                ds 1
Vb_tmp15                ds 1
Vb_tmp16                ds 1
Vb_tmp17                ds 1
Vb_tmp18                ds 1
Vb_tmp19                ds 1
; shadow registers
Vb_SWCHA_Shadow         ds 1
; player data
Vb_PlayerPosX           ds 1
Vb_PlayerPosY           ds 1
; in BCF
Vb_Score00              ds 1
Vb_Score01              ds 1
Vb_Score02              ds 1

    echo "----",($100 - *) , "bytes of RAM left"
;--- end Variables 

    SEG code
    ORG $F000
    echo "---- start code at ",(*)

Reset:
;----------------------------
; Start of program
;----------------------------

    CLEAN_START

;----------------------------
; any code to be executed before
; the game actually starts
; goes here
;----------------------------


; set TIA behaviour
    ; set bg color to black ($0)
    lda #$00
    sta COLUBK
    ; set pf color
    ;lda #COL_PF_SOLID
    ;sta COLUPF
    ; set pf behaviour
    ;lda #%00000001
    ;sta CTRLPF
    ; set player color
    ;lda #$38
    ;sta COLUP0
    ;sta COLUP1
    ; set Player size
    ;lda #7
    ;sta NUSIZ0
    ;sta NUSIZ1

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
; initializes TIM64T timer
;----------------------------
VerticalBlank:
    ldx #0      
    lda #2
    sta WSYNC   ;
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

;-- check input
    ; joystick input
    lda SWCHA
    ; break if nothing has changed
    cmp Vb_SWCHA_Shadow
    ; beq NoMovement
    beq NoMovement
    ; store new SWCHA state
    sta Vb_SWCHA_Shadow

; right?
CheckRightPressed:
    and #%10000000
    ; skip to CheckLeftPressed if not equal
    bne CheckLeftPressed
    ;-- TODO: impl right movement
; left?
CheckLeftPressed:
    lda Vb_SWCHA_Shadow
    and #%01000000
    ; skip to CheckDownPressed not equal
    bne CheckDownPressed
    ;-- TODO: impl right movement
; down? 
CheckDownPressed:
    ; check if down is pressed
    lda Vb_SWCHA_Shadow
    and #%00100000
    ; skip to CheckUpPressed if not pressed
    bne CheckUpPressed
    ;-- TODO: impl down movement
; up?
CheckUpPressed:
    ; check if up is pressed
    lda Vb_SWCHA_Shadow
    and #%00010000
    ; skip to NoMovement if not pressed
    bne NoMovement
    ; move forward one step, check if valid movement
    ;-- TODO: impl up movement
NoMovement:

    rts ;--- GameState

;----------------------------
; Draw visible scanlines
; https://alienbill.com/2600/cookbook/subpixel.html
;----------------------------
DrawScreen: SUBROUTINE
    lda #0 
BREAK:

    ; wait until vertical blank period is over
.vblankWait:
    sta WSYNC
    lda INTIM
    bne .vblankWait
    sta VBLANK ; since A = #0

    lda #$FF
    sta COLUPF 

    ldy #NUM_SCANLINES
.lineLoop:
    sta WSYNC

    sty COLUBK
    sty PF0
    sty PF1
    sty PF2
    
    dey
    bne .lineLoop

    ; clear registers to prevent bleeding
    lda #2
    sta WSYNC   ; finish scanline
    sta VBLANK  ; make TIA output blank
    ; re-use Y which is still 0
    ldy #0
    sty PF0
    sty PF1
    sty PF2  
    sty GRP0
    sty GRP1
    sty ENAM0
    sty ENAM1
    sty ENABL
    sty VDELP0
    sty VDELP1
    rts ; DrawScreen

;----------------------------
; Overscan
;----------------------------
OverScan:
    lda #35
    sta TIM64T
    ; calc stuff here
OverScanLineWait:
    lda INTIM
    bne OverScanLineWait
    ; return
    rts


;----------------------------
; Data
;----------------------------
DATA_Start ALIGN 256
    echo "---- start data at ",(*)
    
    include "hero.inc"

;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


