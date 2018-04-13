; Lunar Launcher
; (c) 2018 Michael Bayer
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

; Game constants
COL_BG_SCORE = $0
COL_BG_RESOURCES = $0
COL_BG_LIGHT = $b6
COL_BG_DARK =  $c4

COL_PF_HIGHLIGHT = $1e


SCANLINES_RESOURCE = 16
SCANLINES_LANE = 30
SCANLINES_SCORE = 16


;--- end Constants

;----------------------------
; Macros   
;----------------------------

    ;###################################################
    ;
    ; M_ACTION_LANE 0 LIGHT
    MAC M_HIGHLIGHT

    ldy Vb_lane_select
    lda PF_HIGHLIGHT_{1},y
    sta PF0
    sta PF1
    sta PF2

    ENDM

    ;###################################################
    ;
    ; M_ACTION_LANE 0 LIGHT
    MAC M_ACTION_LANE

    ; init BGCOLOR
    lda #COL_BG_{2}
    sta COLUBK

    ; highlight
    M_HIGHLIGHT {1}
BREAK{1}:
    ; init sunflower pointers
    ldy Vb_sunflowers_lane_{1}
    lda SunflowerP0LoTbl,y
    sta Vptr_sunflower_pf0
    lda SunflowerP1LoTbl,y
    sta Vptr_sunflower_pf1

    sta WSYNC
    ; re-set pf registers
    lda #0
    sta PF0
    sta PF1
    sta PF2
    ; P0 behaviour
    lda #%00000011
    sta NUSIZ0
    SLEEP 10
    sta RESP0
    ; init scanline counter
    ldy #SCANLINES_LANE
.actionLaneLoop{1}:
    ; PF0 (sunflower)
    ;lda PF_SUNFLOWER,y
    lda (Vptr_sunflower_pf0),y
    sta PF0
    lda (Vptr_sunflower_pf1),y
    sta PF1
    ; P0 (shooter)
    lda PLANT,y
    sta GRP0

    SLEEP 10
    ; re-set PF0/1
    lda #0
    sta PF0
    sta PF1

    sta WSYNC
    dey
    bne .actionLaneLoop{1}

    ; highlight
    M_HIGHLIGHT {1}
    sta WSYNC
;    lda #COL_PF_HIGHLIGHT
;    sta COLUPF
;    ldy Vb_lane_select
;    lda PF_HIGHLIGHT_{1},y
;    sta PF0
;    sta PF1
;    sta PF2
    ENDM

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

;Vb_tmp00                ds 1
; shadow registers
Vb_SWCHA_Shadow         ds 1
; lane select
Vb_lane_select          ds 1
; sunflowers
Vb_sunflowers_lane_0    ds 1
Vb_sunflowers_lane_1    ds 1
Vb_sunflowers_lane_2    ds 1
Vb_sunflowers_lane_3    ds 1
Vb_sunflowers_lane_4    ds 1
; sunflower grahic pointer
Vptr_sunflower_pf0       ds 2
Vptr_sunflower_pf1       ds 2
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
    ; set PF color
    lda #COL_PF_HIGHLIGHT
    sta COLUPF
    ; set pf behaviour
    lda #%00000001
    sta CTRLPF
    ; set player color
    ;lda #$0F
    ;sta COLUP0
    ;sta COLUP1
    ; set Player size
    ;lda #7
    ;sta NUSIZ0
    ;sta NUSIZ1
    
    ; sunflower pointer hi
    lda #>(PF_SUNFLOWER)
    sta Vptr_sunflower_pf0+1
    sta Vptr_sunflower_pf1+1

; TEST VALUES 
    ; initial player pos 
    lda #1
    sta Vb_sunflowers_lane_1
    sta Vb_sunflowers_lane_3
    lda #2
    sta Vb_sunflowers_lane_2
    ;sta Vw_PlayerPosY+1
    ;lda #90
    ;sta Vb_PlayerPosX

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
    beq NoMovement
    ;beq NoMovement
    ; store new SWCHA state
    sta Vb_SWCHA_Shadow

; right?
CheckRightPressed:
    and #%10000000
    ; skip to CheckLeftPressed if not equal
    bne CheckLeftPressed
    ; move right
; left?
CheckLeftPressed:
    lda Vb_SWCHA_Shadow
    and #%01000000
    ; skip to CheckDownPressed not equal
    bne CheckDownPressed
    ; move left
; down? 
CheckDownPressed:
    ; check if down is pressed
    lda Vb_SWCHA_Shadow
    and #%00100000
    ; skip to CheckUpPressed if not pressed
    bne CheckUpPressed
    ; move down
    lda #4
    cmp Vb_lane_select 
    beq WrapDownPressed
    inc Vb_lane_select
    jmp CheckUpPressed
WrapDownPressed:
    lda #0
    sta Vb_lane_select
; up?
CheckUpPressed:
    ; check if up is pressed
    lda Vb_SWCHA_Shadow
    and #%00010000
    ; skip to NoMovement if not pressed
    bne NoMovement
    lda #0
    cmp Vb_lane_select
    beq WrapUpPressed
    dec Vb_lane_select
    jmp NoMovement
WrapUpPressed:
    lda #4
    sta Vb_lane_select
NoMovement:

    rts ;--- GameState

;----------------------------
; Draw visible scanlines
; https://alienbill.com/2600/cookbook/subpixel.html
;----------------------------
DrawScreen: SUBROUTINE
    lda #0 

    ; wait until vertical blank period is over
.vblankWait:
    sta WSYNC
    lda INTIM
    bne .vblankWait

    ; y will be our scanline counter
    ldy #SCANLINES_RESOURCE

    sta WSYNC
    sta HMOVE
    sta VBLANK ; since A = #0

 ;------------------------
 ; Resource lane 
 ;------------------------
    lda #COL_BG_RESOURCES
    sta COLUBK
.resourceLane:
    
    sta WSYNC
    dey
    bne .resourceLane
    ; scala, TODO: remove
    lda #%10101010
    sta PF0
    sta PF2
    lsr
    sta PF1
    sta WSYNC
 ;------------------------
 ; Action lanes
 ;------------------------
    M_ACTION_LANE 0, LIGHT
    M_ACTION_LANE 1, DARK
    M_ACTION_LANE 2, LIGHT
    M_ACTION_LANE 3, DARK 
    M_ACTION_LANE 4, LIGHT

 ;------------------------
 ; Score lane 
 ;------------------------
    lda #0
    sta PF0
    sta PF1
    sta PF2
    ldy #SCANLINES_SCORE
    lda #COL_BG_SCORE
    sta COLUBK
.scoreLane:
    
    sta WSYNC
    dey
    bne .scoreLane

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
; BattleZone-style sprite repositioning
; see https://alienbill.com/2600/cookbook/subpixel.html
; set A = desired horizontal position, then X to object
; to be positioned (0->4 = P0->BALL)
;----------------------------
bzoneRepos: SUBROUTINE
    sta WSYNC                   ; 00     Sync to start of scanline.
    sec                         ; 02     Set the carry flag so no borrow will be applied during the division.
.divideby15
    sbc #15                     ; 04     Waste the necessary amount of time dividing X-pos by 15!
    bcs .divideby15             ; 06/07  11/16/21/26/31/36/41/46/51/56/61/66

    tay
    lda fineAdjustTable,y       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
    sta HMP0,x

    sta RESP0,x                 ; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.
    rts


;----------------------------
; Data
;----------------------------
DATA_Start ALIGN 256
    echo "---- start data at ",(*)

PF_HIGHLIGHT_0:
    .byte #%11111111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    
PF_HIGHLIGHT_1:
    .byte #%00000000
    .byte #%11111111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    
PF_HIGHLIGHT_2:
    .byte #%00000000
    .byte #%00000000
    .byte #%11111111
    .byte #%00000000
    .byte #%00000000
    
PF_HIGHLIGHT_3:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11111111
    .byte #%00000000
    
PF_HIGHLIGHT_4:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11111111

PF_SUNFLOWER:
    .byte #%00000000
    .byte #%00000000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01100000
    .byte #%00000000
    .byte #%00000000
PF_NOSUNFLOWER:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

SunflowerP0LoTbl:
    .byte <(PF_NOSUNFLOWER) ; 0 -> none
    .byte <(PF_SUNFLOWER)   ; 1 -> yes
    .byte <(PF_SUNFLOWER)   ; 2 -> yes
SunflowerP1LoTbl:
    .byte <(PF_NOSUNFLOWER) ; 0 -> none
    .byte <(PF_NOSUNFLOWER) ; 1 -> nope
    .byte <(PF_SUNFLOWER)   ; 2 -> yes

    include "sprites.inc"

;-----------------------------
; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write
    ORG $FE00
fineAdjustBegin 
            DC.B %01110000 ; Left 7
            DC.B %01100000 ; Left 6
            DC.B %01010000 ; Left 5
            DC.B %01000000 ; Left 4
            DC.B %00110000 ; Left 3
            DC.B %00100000 ; Left 2
            DC.B %00010000 ; Left 1
            DC.B %00000000 ; No movement.
            DC.B %11110000 ; Right 1
            DC.B %11100000 ; Right 2
            DC.B %11010000 ; Right 3
            DC.B %11000000 ; Right 4
            DC.B %10110000 ; Right 5
            DC.B %10100000 ; Right 6
            DC.B %10010000 ; Right 7

fineAdjustTable EQU fineAdjustBegin - %11110001 ; NOTE: %11110001 = -15


;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


