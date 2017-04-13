; VCS Crawl 
; (c) 2017 Michael Bayer
; 
;
; TODO
; - support Joy1 inputs as well as Joy0
; - last section (3) looks too large, 
;   maybe reduce to 12 scanlines instead of 16?
; - try to color the walls directly facing the player by setting PF registers
;   to #%00000000 and using a designated bgcol
;
; Loads of room for optimizations:
; - optimize M_Move to not need a return label (maybe abuse DASM's '*')?
; - only update wall pointers and Vb_DrawDist after movement actually happened
; - culling tests
;   these decide whether to use the current odd/even color or use
;   the playfield color to simulate a solid wall
;   these are performed each scanline instead if once per section 
;   or even per frame
; - wall pointers
;   the subroutine TestTile is executed many times per wall and for
;   culldistance calculations, could be optimized to fetch the 
;   relevant maze byte only once. This would require each maze
;   quadrant to also be stored in a transposed ([]^-1) format in 
;   addition to the current x-inverted format
; - Left/RightWall subroutines
;   should probably be converted to loops...
;

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
BGCOL_FAR = $00
PFCOL = $0E

C_MAX_DRAW_DIST = 5

;--- end Constants

;----------------------------
; Macros   
;----------------------------

    ; "walks" a step in the direction defined by Vb_PlayerOrientation
    ; basically just calls the MoveEast/South/West/North subroutine
    ; pointed to by Vb_tmp4 and Vb_tmp5, then returns to {1}
    ; expects Vb_tmp4 and Vb_tmp5 to point to the appropriate subroutine
    MAC M_CallWalkStepReturn

.TARGET SET {1}
    ; push target address to stack
    lda #>(.TARGET-1)
    pha
    lda #<(.TARGET-1)
    pha
    ; jump to walk* subroutine
    jmp (Vb_tmp4)

    ENDM ;--- M_CallWalkStepReturn


    ; movement macros
    ; calls appropriate Move* subroutines 
    ; destroys 
    ; x
    ; Vb_tmp4 and 5
    MAC M_Move

.RETURNTARGET SET {2}
    ; push return target address to stack
    lda #>(.RETURNTARGET-1)
    pha
    lda #<(.RETURNTARGET-1)
    pha

    ; load tmp4 and tmp5
    ldx Vb_PlayerOrientation
    lda MovePtrHI
    sta Vb_tmp5
    lda Move{1}PtrLOTable,x
    sta Vb_tmp4
    ; execute jump
    jmp (Vb_tmp4)

    ENDM ;--- M_Move


    ; sets COLUBK to {1} or PFCOL
    ; depending on X < / >= Vb_DrawDist
    ; expects X to contain the section's draw distance
    MAC M_CullBG
    ; compare X to Vb_DrawDist
    cpx Vb_DrawDist        ; +3
    bcc .nocull             ; +2/3
    ; X  >= Vb_DrawDist -> cull
    lda #PFCOL              ; +3 (6)
    jmp .setbg              ; +3 (8)
.nocull
    ; X < Vb_DrawDist -> nocull
    lda {1}                 ; +3 (5)
    nop                     ; +2 (8) nop to equalize branch cycle counts
.setbg 
    ; set bg color
    sta COLUBK

    ENDM ;--- M_CullBG

    
    ; load PosX/Y into Vb_tmp1/2
    MAC M_CopyPos2Tmp

    lda Vb_PlayerPosX
    sta Vb_tmp1
    lda Vb_PlayerPosY
    sta Vb_tmp2

    ENDM ;--- M_CopyPos2Tmp

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

Vb_tmp1                ds 1
Vb_tmp2                ds 1
Vb_tmp3                ds 1
Vb_tmp4                ds 1
Vb_tmp5                ds 1
Vb_tmp6                ds 1
; shadow registers
Vb_SWCHA_Shadow        ds 1
; player data
Vb_PlayerPosX        ds 1
Vb_PlayerPosY        ds 1
; 0 0  E
; 0 1  S
; 1 0  W
; 1 1  N
Vb_PlayerOrientation  ds 1
; maze data
; 4x4 = 16 bytes
Vb_MazeAA        ds 1 
Vb_MazeAB        ds 1
Vb_MazeAC        ds 1
Vb_MazeAD        ds 1
Vb_MazeBA        ds 1
Vb_MazeBB        ds 1
Vb_MazeBC        ds 1
Vb_MazeBD        ds 1
Vb_MazeCA        ds 1
Vb_MazeCB        ds 1
Vb_MazeCC        ds 1
Vb_MazeCD        ds 1
Vb_MazeDA        ds 1
Vb_MazeDB        ds 1
Vb_MazeDC        ds 1
Vb_MazeDD        ds 1
; wall section pointers:
; 4*2*2 = 16 bytes
Vptr_Sec0L       ds 2
Vptr_Sec0R       ds 2
Vptr_Sec1L       ds 2
Vptr_Sec1R       ds 2
Vptr_Sec2L       ds 2
Vptr_Sec2R       ds 2
Vptr_Sec3L       ds 2
Vptr_Sec3R       ds 2
; Wall states
Vb_LeftWall      ds 1
Vb_RightWall     ds 1
; basically how far to the end of the tunnel?
Vb_DrawDist      ds 1
; BGColor value, 2 bytes
Vb_BGColOdd           ds 1
Vb_BGColEven          ds 1

    echo "----",($100 - *) , "bytes of RAM left"
;--- end Variables 

    SEG code
    ORG $F000
    echo "---- start code at ",(*)
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

; init maze
    ldx #0
    lda #0
InitMaze:
    sta Vb_MazeAA,x
    clc
    adc #16
    inx
    cpx #16
    bne InitMaze
; init player pos
    lda #3
    sta Vb_PlayerPosX 
    lda #6
    sta Vb_PlayerPosY
    lda #3
    sta Vb_PlayerOrientation

; set TIA behaviour
    ; set bg color to black ($0)
    lda #$00
    sta COLUBK
    ; set pf color
    lda #PFCOL
    sta COLUPF
    ; set pf behaviour
    lda #%00000000
    sta CTRLPF
    ; set player color
    lda #$38
    sta COLUP0
    sta COLUP1
    ; set Player sie
    lda #7
    sta NUSIZ0
    sta NUSIZ1

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
; test tile in maze
; input: 
;   Vb_tmp1 - playerX
;   Vb_tmp2 - playerY
; output:
;   Z flag - set if tile is solid, unset otherwise
; destroys:
;   X, Y
;   Vb_tmp3
;----------------------------
TestTile: SUBROUTINE
    ; get byte for left corridor walls
    ; - calculate quadrant offset (0-F)  
    lda Vb_tmp1
    lsr
    lsr
    lsr 
    sta Vb_tmp3
    lda Vb_tmp2
    lsr
    and #%11111100
    clc
    adc Vb_tmp3
    ; store quadrant offset in x
    tax
    ; load value of quadrant pointer into a
    lda Vb_MazeAA,x

    ; store it
    sta Vb_tmp3
    ; - add y offset
    lda Vb_tmp2
    and #%00000111
    clc
    adc Vb_tmp3
    ; store in x
    tax
    ; load maze value and
    ; shift so that Vb_PlayerPosX is at lsb
    lda Vb_tmp1
    and #%00000111
    tay
    lda MAZEDATA_0,x
.shiftLoop
    dey
    bmi .doneShifting
    lsr
    jmp .shiftLoop
.doneShifting
    ; set Z flag
    and #1
    rts


;----------------------------
; calculate game state for this frame
;----------------------------
GameState:
    ; joystick input
    lda SWCHA
    ; break if nothing has changed
    cmp Vb_SWCHA_Shadow
    ; beq NoMovement
    beq NoMovement
    ; store new SWCHA state
    sta Vb_SWCHA_Shadow

; Player orientation, 
; joystick left/right
Check4Turn:

; turn right?
CheckRightPressed:
    and #%10000000
    ; skip to CheckLeftPressed if not equal
    bne CheckLeftPressed
    dec Vb_PlayerOrientation

; turn left?
CheckLeftPressed:
    lda Vb_SWCHA_Shadow
    and #%01000000
    ; skip to Check4Movement if not equal
    bne Check4Movement
    inc Vb_PlayerOrientation

; Player Position
; joystick up/down
Check4Movement:
    ; preparations:
    ; normalize Vb_PlayerOrientation
    lda #%00000011
    and Vb_PlayerOrientation
    sta Vb_PlayerOrientation
    ; load PosX/Y into Vb_tmp1/2
    M_CopyPos2Tmp

; check if down was pressed
CheckDownPressed: SUBROUTINE
    ; check if down is pressed
    lda Vb_SWCHA_Shadow
    and #%00100000
    ; skip to CheckUpPressed if not pressed
    bne CheckUpPressed
    ; move back one step, check if valid movement
    M_Move Back,CheckMovementValid

; check if up was pressed
CheckUpPressed: SUBROUTINE
    ; check if up is pressed
    lda Vb_SWCHA_Shadow
    and #%00010000
    ; skip to NoMovement if not pressed
    bne NoMovement
    ; move forward one step, check if valid movement
    M_Move Forward,CheckMovementValid

CheckMovementValid:
    ; test if move is valid
    jsr TestTile
    bne Collision
    ; TODO: implement collision sound
    ; copy Vb_tmp1/2 back to PosX/Y
    lda Vb_tmp1
    sta Vb_PlayerPosX
    lda Vb_tmp2
    sta Vb_PlayerPosY
    jmp NoMovement
Collision:
    nop
NoMovement:


; calculate wall and tunnel states
;
; tmp4,5 = player_pos
; for (i=0; i<C_MAX_DRAW_DIST; i++) {
;   move left
;   check tile
;   save state left
;   move right (to middle)
;   check tile
;   inc draw dist if not solid
;
; }
CalcTunnelState: SUBROUTINE
    ; initialize variables
    M_CopyPos2Tmp
    lda #0
    sta Vb_LeftWall
    sta Vb_RightWall
    sta Vb_DrawDist
    lda #C_MAX_DRAW_DIST
    ; loop C_MAX_DRAW_DIST times
    ; use Vb_tmp6 as loop counter
    ; for (tmp6 = C_MAX_DRAW_DIST; tmp6 > 0; tmp6--) {
    ; ...
    ; }
    sta Vb_tmp6
.DrawDistLoop:    
    ; we start at the left tile
    M_Move Left,CTS_LeftTile
CTS_LeftTile:
    ; TODO: create CTS_TestAndShift,[Left,Right] Macro
    jsr TestTile
    ; after TestTile: Z == A = 1 if solid
    ; store tile state in tmp5
    sta Vb_tmp5 
    ; load LeftWall state
    lda Vb_LeftWall
    ; shift left
    clc
    asl
    ; set lsb
    ora Vb_tmp5
    ; store new sate
    sta Vb_LeftWall
    ; move right -> center tile
    M_Move Right,CTS_CenterTile
CTS_CenterTile:
    jsr TestTile
DEBUG1:
    ; not solid - don't care
    beq .drawDistAlreadySet
    lda Vb_DrawDist
    ; DrawDist already set, don't overwrite
    bne .drawDistAlreadySet
    ; solid tile && DrawDist == 0
    ;    -> DrawDist := loop variable's value
    lda Vb_tmp6
    sta Vb_DrawDist
.drawDistAlreadySet
    ; move right -> right tile
    M_Move Right,CTS_RightTile
CTS_RightTile:
    jsr TestTile
    ; after TestTile: Z == A = 1 if solid
    ; store tile state in tmp5
    sta Vb_tmp5 
    ; load LeftWall state
    lda Vb_RightWall
    ; shift left
    clc
    asl
    ; set lsb
    ora Vb_tmp5
    ; store new sate
    sta Vb_RightWall
    
    ; move one step forward and reset to center tile
    ; happens at the end of the loop b/c we also need to check
    ; current position's tile state
    ; move forward
    M_Move Forward,CTS_MoveToCenter
    ; move Left, back to center
CTS_MoveToCenter
    M_Move Left,CTS_LoopCheck

    ; break loop?
CTS_LoopCheck:
    dec Vb_tmp6
    ; bne .DrawDistLoop is too far a jump...
    beq .breakLoop
    jmp .DrawDistLoop
.breakLoop:

DEBUG2:
    ; "normalize" tunnel state vars
    lda Vb_DrawDist
    beq .drawDistZero
    ; DrawDist := MAX - DrawDist
    lda #C_MAX_DRAW_DIST
    sec
    sbc Vb_DrawDist
    sta Vb_DrawDist
.drawDistZero:
    ; DrawDist cannot be 0, so we assume MAX then
    lda #C_MAX_DRAW_DIST
    sta Vb_DrawDist
    ; we don't care about the farthest tile as it's out of sight
    ; so we shift the state vars left
.finalizeLeftWall:
    lda Vb_LeftWall
    lsr
    sta Vb_LeftWall
    lda Vb_RightWall
    lsr
    sta Vb_RightWall

BREAKHERE:

    ; set playfield data pointers 
    ; according to position in maze
    ; first: set all to solid
    SET_POINTER Vptr_Sec0L, PF_1_0 
    SET_POINTER Vptr_Sec0R, PF_1_1

    SET_POINTER Vptr_Sec1L, PF_1_1 
    SET_POINTER Vptr_Sec1R, PF_1_1

    SET_POINTER Vptr_Sec2L, PF_1_1 
    SET_POINTER Vptr_Sec2R, PF_1_0

    SET_POINTER Vptr_Sec3L, PF_1_0 
    SET_POINTER Vptr_Sec3R, PF_1_0

    ; set up Vb_tmp4 and Vb_tmp5 as pointer to the correct walking subrouting
    ; D0 of Vb_PlayerOrientation: 0 -> E/W, 1 -> N,S
    ; D1 of Vb_PlayerOrientation: 0 -> inc, 1-> dec
    ; 0 0  E
    ; 0 1  S
    ; 1 0  W
    ; 1 1  N
    ; calc direction index and store in x
    ldx Vb_PlayerOrientation
    ; load appropriate subroutine location into Vb_tmp4 and Vb_tmp5
    lda WalkingTableHI,x
    sta Vb_tmp5
    lda WalkingTableLO,x
    sta Vb_tmp4

    ; far wall
    ; calculate Vb_DrawDist
    ; reset
    lda #0
    sta Vb_DrawDist
    ; set up Vb_tmp1 and Vb_tmp2
    M_CopyPos2Tmp
FarWall: SUBROUTINE
    M_CallWalkStepReturn FarWallRet
FarWallRet:
    inc Vb_DrawDist
    beq .done
    lda #5
    cmp Vb_DrawDist
    bcc .done
    jsr TestTile
    beq FarWall
.done

LeftWall: SUBROUTINE
    ; set up Vb_tmp1 and Vb_tmp2
    M_CopyPos2Tmp
    ; modify according to Vb_PlayerOrientation
    lda Vb_PlayerOrientation
    ; facing east?
    cmp #%00
    bne .notEast
    inc Vb_tmp2
    jmp .LeftWallStep0
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    dec Vb_tmp1
    jmp .LeftWallStep0
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    dec Vb_tmp2
    jmp .LeftWallStep0
.notWest
    ; facint north!
    inc Vb_tmp1
.LeftWallStep0:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid0
    SET_POINTER Vptr_Sec0L, PF_NONE
.solid0
    ; walk one step formward
    M_CallWalkStepReturn LeftWallStep1
LeftWallStep1:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid1
    SET_POINTER Vptr_Sec1L, PF_NONE
.solid1
    ; walk one step formward
    M_CallWalkStepReturn LeftWallStep2
LeftWallStep2:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid2
    SET_POINTER Vptr_Sec2L, PF_NONE
.solid2
    ; walk one step formward
    M_CallWalkStepReturn LeftWallStep3
LeftWallStep3:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid3
    SET_POINTER Vptr_Sec3L, PF_NONE
.solid3
    ; right corridor wall
RightWall: SUBROUTINE
    ; set up Vb_tmp1 and Vb_tmp2
    M_CopyPos2Tmp
    ; modify according to Vb_PlayerOrientation
    lda Vb_PlayerOrientation
    ; facing east?
    cmp #%00
    bne .notEast
    dec Vb_tmp2
    jmp .RightWallStep0
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    inc Vb_tmp1
    jmp .RightWallStep0
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    inc Vb_tmp2
    jmp .RightWallStep0
.notWest
    ; facint north!
    dec Vb_tmp1
.RightWallStep0:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid0
    SET_POINTER Vptr_Sec0R, PF_NONE
.solid0
    ; walk one step formward
    M_CallWalkStepReturn RightWallStep1
RightWallStep1:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid1
    SET_POINTER Vptr_Sec1R, PF_NONE
.solid1
    ; walk one step formward
    M_CallWalkStepReturn RightWallStep2
RightWallStep2:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid2
    SET_POINTER Vptr_Sec2R, PF_NONE
.solid2
    ; walk one step formward
    M_CallWalkStepReturn RightWallStep3
RightWallStep3:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid3
    SET_POINTER Vptr_Sec3R, PF_NONE
.solid3
     
    ; set background color
    ; according to position in maze
BackgroundColor: SUBROUTINE
    clc
    lda Vb_PlayerPosX
    adc Vb_PlayerPosY
    and #1
    beq .odd
    ldx #BGCOL_LIGHT
    ldy #BGCOL_DARK
    jmp .bgcolend
.odd
    ldx #BGCOL_DARK
    ldy #BGCOL_LIGHT
.bgcolend
    stx Vb_BGColOdd
    sty Vb_BGColEven
    sty COLUBK



    rts ;--- GameState

;----------------------------
; Draw visible scanlines
;----------------------------
DrawScreen:
    ; wait until vertical blank period is over
    lda INTIM
    bne DrawScreen
    sta VBLANK ; since A = #0

    include "tunnelkernel.inc"


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
; Walking subroutines
;----------------------------
; inc/dec Vb_tmp1 (== XCoord) or Vb_tmp2 (==YCoord)
MoveNorth: SUBROUTINE
    dec Vb_tmp2
    rts
MoveEast: SUBROUTINE
    inc Vb_tmp1
    rts
MoveSouth: SUBROUTINE
    inc Vb_tmp2
    rts
MoveWest: SUBROUTINE
    dec Vb_tmp1
    rts

;----------------------------
; Data
;----------------------------

    echo "---- start data at ",(*)

    ; movement soubroutine pointer table
    ; all Move* subroutines pointers share a single HI byte
MovePtrHI:
    .byte >(MoveNorth)
    ; low bytes, index /w Vb_PlayerOrientation
    ; forward
MoveForwardPtrLOTable:
    .byte <(MoveEast)   ; 00 -> facing east
    .byte <(MoveSouth)  ; 01 -> facing south
    .byte <(MoveWest)   ; 10 -> facing west
    .byte <(MoveNorth)  ; 11 -> facing north
    ; back
MoveBackPtrLOTable:
    .byte <(MoveWest)   ; 00 -> facing east
    .byte <(MoveNorth)  ; 01 -> facing south
    .byte <(MoveEast)   ; 10 -> facing west
    .byte <(MoveSouth)  ; 11 -> facing north
    ; left
MoveLeftPtrLOTable:
    .byte <(MoveNorth)   ; 00 -> facing east
    .byte <(MoveEast)    ; 01 -> facing south
    .byte <(MoveSouth)   ; 10 -> facing west
    .byte <(MoveWest)    ; 11 -> facing north
    ; right
MoveRightPtrLOTable:
    .byte <(MoveSouth)   ; 00 -> facing east
    .byte <(MoveWest)    ; 01 -> facing south
    .byte <(MoveNorth)   ; 10 -> facing west
    .byte <(MoveEast)    ; 11 -> facing north


    ; walk subroutine pointer table
WalkingTableHI:
    .byte >(MoveEast)
    .byte >(MoveSouth)
    .byte >(MoveWest)
    .byte >(MoveNorth)

WalkingTableLO:
    .byte <(MoveEast)
    .byte <(MoveSouth)
    .byte <(MoveWest)
    .byte <(MoveNorth)

    ; playfield data
    include "pfdata.inc"
    ; sprites
    include "mobdata.inc"

    echo "---- bytes left ",($fd00 - *)

    ; maze data needs to be page aligned...
    ORG $FD00
    include "mazedata.inc"

;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


