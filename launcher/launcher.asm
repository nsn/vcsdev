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
PLAY_SCANLINES = 160

; Game constants
PLAYER_SPEED = 300      ; subpixel speed, div by 256 for pixel speed
PLAYER_HEIGHT = 27      ; player sprite height in scanlines
PF_COLOR = $0a
PLAYER_COLOR_HEAD = $0e
PLAYER_COLOR_TORSO = $8a
PLAYER_COLOR_LEGS = $38
PLAYER_THRUST = 40     ; 2 byte constant, div by 256 for pixels/frame
PLAYER_VMAX_Y = 10     ; 
PLAYER_VMIN_Y = -10     ; msb only
GRAVITY = -20         ; 2 bytes again


;--- end Constants

;----------------------------
; Macros   
;----------------------------

    ;###################################################
    ; adds two two byte values
    ; M_ADD_CONSTANT <target> <constant>
    MAC M_ADD_CONSTANT
    clc
    lda {1}
    adc #<{2}
    sta {1}
    lda {1}+1
    adc #>{2}
    sta {1}+1
    ENDM

    ;###################################################
    ; adds two two byte values
    ; M_ADD_WORDS <target> <operand>
    MAC M_ADD_WORDS
    clc
    lda {1}
    adc {2}
    sta {1}
    lda {1}+1
    adc {2}+1
    sta {1}+1
    ENDM

    ;###################################################
    ; subtracts an 8bit constant from a 16bit variable
    ; M_SUB <target> <constant>
    MAC M_SUB
    sec
    lda {1}
    sbc {2}
    sta {1}
    bcs .subDone
.subDone:
    dec {1}+1
    ENDM

    MAC M_ADJUST_PFPTR

    ENDM

    MAC M_DRAW_STRIP
    lda (Vptr_S{1}_PF0),y
    sta PF0
    lda (Vptr_S{1}_PF1),y
    sta PF1
    lda (Vptr_S{1}_PF2),y
    sta PF2
    ENDM
;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

;Vb_tmp00                ds 1
; shadow registers
Vb_SWCHA_Shadow         ds 1
; -----------
; player data
; -----------
Vb_PlayerPosX           ds 1
; Player Y-pos is stored as 2 bytes,
; LO -> fractional part
; HI -> integer part
Vw_PlayerPosY           ds 2
Vw_PlayerVelX           ds 2
Vw_PlayerVelY           ds 2
Vb_PlayerY              ds 1 ; skipdraw
Vptr_PlayerSprite       ds 2 ; player sprite pointer
Vptr_PlayerColor        ds 2 ; player color pointer
Vb_PlayerSpriteIndex    ds 1
; -----------
; in BCD
Vb_Score00              ds 1
Vb_Score01              ds 1
Vb_Score02              ds 1
; pf pointers
Vptr_S0_PF0             ds 2
Vptr_S0_PF1             ds 2
Vptr_S0_PF2             ds 2
Vptr_S1_PF0             ds 2
Vptr_S1_PF1             ds 2
Vptr_S1_PF2             ds 2
Vptr_S2_PF0             ds 2
Vptr_S2_PF1             ds 2
Vptr_S2_PF2             ds 2
Vptr_S3_PF0             ds 2
Vptr_S3_PF1             ds 2
Vptr_S3_PF2             ds 2
Vptr_S4_PF0             ds 2
Vptr_S4_PF1             ds 2
Vptr_S4_PF2             ds 2
;
Vptr_jmp_target         ds 2

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
    lda #PF_COLOR
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
    ; initial player pos 
    lda #42 
    sta Vw_PlayerPosY+1
    lda #90
    sta Vb_PlayerPosX

    ; set high byte of player sprite pointer
    lda #>HERO_LEFT_F0
    sta Vptr_PlayerSprite+1
    ; set high byte of player color pointer
    lda #>PlayerColors
    sta Vptr_PlayerColor+1



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
    ;cmp Vb_SWCHA_Shadow
    ; beq NoMovement
    ;beq NoMovement
    ; store new SWCHA state
    sta Vb_SWCHA_Shadow

; right?
CheckRightPressed:
    and #%10000000
    ; skip to CheckLeftPressed if not equal
    bne CheckLeftPressed
    ; move right
    inc Vb_PlayerPosX
    ; player sprite index
    lda #1
    sta Vb_PlayerSpriteIndex
; left?
CheckLeftPressed:
    lda Vb_SWCHA_Shadow
    and #%01000000
    ; skip to CheckDownPressed not equal
    bne CheckDownPressed
    ; move left
    dec Vb_PlayerPosX
    ; player sprite index
    lda #0
    sta Vb_PlayerSpriteIndex
; down? 
CheckDownPressed:
    ; check if down is pressed
    lda Vb_SWCHA_Shadow
    and #%00100000
    ; skip to CheckUpPressed if not pressed
    bne CheckUpPressed
    ; move down
    ; TODO
    ;lda #0
    ;sta Vw_PlayerVelY
    ;sta Vw_PlayerVelY+1
    M_ADD_CONSTANT Vw_PlayerVelY, GRAVITY
; up?
CheckUpPressed:
    ; check if up is pressed
    lda Vb_SWCHA_Shadow
    and #%00010000
    ; skip to NoMovement if not pressed
    bne NoMovement

    ; 16bit math, adds both bytes
    ; of player_speed to the 2 bytes
    ; of Vw_PlayerPosY
    M_ADD_CONSTANT Vw_PlayerVelY, PLAYER_THRUST

    ; player sprite index changes when moving up 
    lda Vb_PlayerSpriteIndex
    ora #2
    sta Vb_PlayerSpriteIndex
NoMovement:
    ; apply gravity
    M_ADD_CONSTANT Vw_PlayerVelY, GRAVITY

    ;vmin?
    ; only compare hi byte
    lda #PLAYER_VMIN_Y
    cmp Vw_PlayerVelY+1
    bmi PVgtVMin
    ; here player velocity is < PLAYER_VMIN_Y
    sta Vw_PlayerVelY+1
PVgtVMin:
    ;vmax?
    lda #PLAYER_VMAX_Y
    cmp Vw_PlayerVelY+1
    bpl PVTestEnd
    ; here player vel > PLAYER_VMAX_Y
    sta Vw_PlayerVelY+1
PVTestEnd:

    ; apply veloity to position
    M_ADD_WORDS Vw_PlayerPosY, Vw_PlayerVelY 

    ;-- set player sprite pointer
    ldx Vb_PlayerSpriteIndex
    lda PlayerSpriteIndexTable,x
    sta Vptr_PlayerSprite ;store in LO byte
    ;-- .. and color
    lda #<(PlayerColors)
    sta Vptr_PlayerColor

    ; reposition P0
    lda Vb_PlayerPosX
    ldx #0
    jsr bzoneRepos

    ; set Vb_PlayerY to vertical position (0 = top)
    ; PlayerY = vertical position + Po height - 1
    lda #PLAY_SCANLINES + #PLAYER_HEIGHT - #1
    sec 
    sbc Vw_PlayerPosY+1 ;subtract integer part of position
    sta Vb_PlayerY

    ; adjust Vptr_PlayerSprite for skipDraw
    lda Vptr_PlayerSprite
    sec
    sbc Vw_PlayerPosY+1
    clc
    adc #PLAYER_HEIGHT-#1
    sta Vptr_PlayerSprite
    ; adjust Vptr_PlayerColor
    lda Vptr_PlayerColor
    sec 
    sbc Vw_PlayerPosY+1
    clc
    adc #PLAYER_HEIGHT-#1
    sta Vptr_PlayerColor

    ; PF pointers
    SET_POINTER Vptr_S0_PF0, PFData0
    SET_POINTER Vptr_S0_PF1, PFData1
    SET_POINTER Vptr_S0_PF2, PFData2
    SET_POINTER Vptr_S1_PF0, PFData0 
    SET_POINTER Vptr_S1_PF1, PFData1 
    SET_POINTER Vptr_S1_PF2, PFData2 
    SET_POINTER Vptr_S2_PF0, PFData0 
    SET_POINTER Vptr_S2_PF1, PFData1 
    SET_POINTER Vptr_S2_PF2, PFData2 
    SET_POINTER Vptr_S3_PF0, PFData0 
    SET_POINTER Vptr_S3_PF1, PFData1 
    SET_POINTER Vptr_S3_PF2, PFData2 
    SET_POINTER Vptr_S4_PF0, PFData0 
    SET_POINTER Vptr_S4_PF1, PFData1 
    SET_POINTER Vptr_S4_PF2, PFData2 



HERE:
    M_SUB Vptr_S0_PF0, #160
    M_SUB Vptr_S0_PF1, #160
    M_SUB Vptr_S0_PF2, #160
    M_SUB Vptr_S1_PF0, #160
    M_SUB Vptr_S1_PF1, #160
    M_SUB Vptr_S1_PF2, #160
;    sec
;    lda Vptr_S0_PF0
;    sbc #192
;    sta Vptr_S0_PF0
;    bcs ok
;    dec Vptr_S0_PF0+1
;ok

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

    ; y will be out scanline counter
    ldy #PLAY_SCANLINES

    sta WSYNC
    sta HMOVE
    sta VBLANK ; since A = #0

.lineLoop:

;    lda (Vptr_S0_PF0),y
;    sta PF0
;    lda (Vptr_S0_PF1),y
;    sta PF1
;    lda (Vptr_S0_PF2),y
;    sta PF2
    lda #>(.playerColor-1)
    pha
    lda #<(.playerColor-1)
    pha
    
    lda drawPFPtrHi
    sta Vptr_jmp_target+1
    lda drawPFPtrLoTbl,y
    sta Vptr_jmp_target
    jmp (Vptr_jmp_target)

.playerColor
    lda (Vptr_PlayerColor),y
    sta COLUP0
    ; skipDraw
    ; draw P0
    lda #PLAYER_HEIGHT-1
    dcp Vb_PlayerY
    bcs .doDraw
    lda #0
    .byte $2c    ;BIT ABS to skip 2 bytes
.doDraw:
    lda (Vptr_PlayerSprite),y
    sta GRP0
    
    sta WSYNC
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


drawStrip0: SUBROUTINE
    M_DRAW_STRIP 0
    rts
drawStrip1: SUBROUTINE
    M_DRAW_STRIP 1
    rts
drawStrip2: SUBROUTINE
    M_DRAW_STRIP 2
    rts
drawStrip3: SUBROUTINE
    M_DRAW_STRIP 3
    rts
drawStrip4: SUBROUTINE
    M_DRAW_STRIP 4
    rts


;----------------------------
; Data
;----------------------------
DATA_Start ALIGN 256
    echo "---- start data at ",(*)

drawPFPtrHi:
    .byte >(drawStrip0)
    ; maps y register to drawStrip subroutine
    include "draw_pf_tbl.inc" 


    ALIGN 256+PLAY_SCANLINES
    include "hero.inc"

; -- remember kids: it's inverted...
PlayerColors:
    .byte #$0e
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_LEGS
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_TORSO
    .byte #PLAYER_COLOR_HEAD
    .byte #PLAYER_COLOR_HEAD
    .byte #PLAYER_COLOR_HEAD
    .byte #PLAYER_COLOR_HEAD
    .byte #PLAYER_COLOR_HEAD
    .byte #$0e

PlayerSpriteIndexTable:
    .byte <(HERO_LEFT_F0)
    .byte <(HERO_RIGHT_F0)
    .byte <(HERO_LEFT_F1)
    .byte <(HERO_RIGHT_F1)

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

    include "pfdata.inc"


;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


