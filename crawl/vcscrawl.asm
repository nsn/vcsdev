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

COL_BG_DARK = $E4
COL_BG_LIGHT = $E8
COL_BG_EMPTY = $00
COL_BG_SOLID = $0C
COL_PF_SOLID = $08

C_MAX_DRAW_DIST = 5

;--- end Constants

;----------------------------
; Macros   
;----------------------------
    ;####################################################################
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
    ;####################################################################


    ;####################################################################
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
    ;####################################################################


    ;####################################################################
    ; sets COLUBK to {1} or COL_PF_SOLID
    ; depending on X < / >= Vb_DrawDist
    ; expects X to contain the section's draw distance
    MAC M_CullBG
    ; compare X to Vb_DrawDist
    cpx Vb_DrawDist        ; +3
    bcc .nocull             ; +2/3
    ; X  >= Vb_DrawDist -> cull
    lda #COL_PF_SOLID              ; +3 (6)
    jmp .setbg              ; +3 (8)
.nocull
    ; X < Vb_DrawDist -> nocull
    lda {1}                 ; +3 (5)
    nop                     ; +2 (8) nop to equalize branch cycle counts
.setbg 
    ; set bg color
    sta COLUBK

    ENDM ;--- M_CullBG
    ;####################################################################

    
    ;####################################################################
    ; load PosX/Y into Vb_tmp1/2
    MAC M_CopyPos2Tmp

    lda Vb_PlayerPosX
    sta Vb_tmp1
    lda Vb_PlayerPosY
    sta Vb_tmp2

    ENDM ;--- M_CopyPos2Tmp
    ;####################################################################


    ;####################################################################
    ; calculate wall state, uses Vb_tmp6 and Vb_tmp5
    ; param {1} Left/Right
    ; param {2} label to break to
    MAC M_CTS_LOOP
    ; initialize variables
    lda #C_MAX_DRAW_DIST-1
    sta Vb_tmp6
    lda #0
    sta Vb_{1}Wall
    ; init position
    M_CopyPos2Tmp
    M_Move {1}, CTS_{1}Loop
CTS_{1}Loop:
    ; test if tile is solid
    jsr TestTile
    ; after TestTile: Z == A = 1 if solid
    ; store tile state in tmp5
    sta Vb_tmp5 
    ; load wall state
    lda Vb_{1}Wall
    ; shift left
    clc
    asl
    ; set lsb
    ora Vb_tmp5
    ; store new sate
    sta Vb_{1}Wall
    ; dec loop var, break if 0
    dec Vb_tmp6
    beq {2}
    ; != 0, move forward
    M_Move Forward, CTS_{1}Loop
    ENDM ;--- M_CTS_LOOP
    ;####################################################################


    ;####################################################################
    ; M_SetSecPtr 0, Left, PF_1_0
    MAC M_SetSecPtr

    ;HI byte is always the same
    lda #>{3}
    sta Vptr_Sec{1}{2}+1
    ;check wall state
    lda Vb_{2}Wall
    IF {1} == 0
    and #%00001000
    ENDIF
    IF {1} == 1
    and #%00000100
    ENDIF
    IF {1} == 2
    and #%00000010
    ENDIF
    IF {1} == 3
    and #%00000001
    ENDIF
    ;set load appropriate LO byte
    bne .solid
    lda #<PF_NONE
    jmp .done
.solid:
    lda #<{3}
.done:
    ;store HI byte
    sta Vptr_Sec{1}{2}
    ENDM ;--- M_SetSecPtr 

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
Vptr_Sec0Left       ds 2
Vptr_Sec0Right       ds 2
Vptr_Sec1Left       ds 2
Vptr_Sec1Right       ds 2
Vptr_Sec2Left       ds 2
Vptr_Sec2Right       ds 2
Vptr_Sec3Left       ds 2
Vptr_Sec3Right       ds 2
; MOB Pointer, 2 bytes
Vptr_MOB         ds 2
; Wall states
Vb_LeftWall      ds 1
Vb_RightWall     ds 1
; basically how far to the end of the tunnel?
Vb_DrawDist      ds 1
; BGColor value, 3 bytes
Vb_BGColOdd      ds 1
Vb_BGColEven     ds 1
Vb_BGColFar      ds 1

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
    lda #COL_PF_SOLID
    sta COLUPF
    ; set pf behaviour
    lda #%00000001
    sta CTRLPF
    ; set player color
    lda #$38
    sta COLUP0
    sta COLUP1
    ; set Player size
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
CalcTunnelState: SUBROUTINE
; first: calc draw distance
; TODO: this is where we find possible map objects (sprites) to draw as well
; WARNING: runtime depends on effective draw distance,
;          performance should always be tested for worst
;          case: draw distance == 5 tiles
.CenterTile:
    ; initialize variables
    M_CopyPos2Tmp
    lda #0
    sta Vb_DrawDist
    lda #COL_BG_SOLID
    sta Vb_BGColFar
.centerLoop:
    ; inc draw distance
    inc Vb_DrawDist
    ; move one step forward
    M_Move Forward, CTS_CenterTest
CTS_CenterTest:
    jsr TestTile
    ; not solid - don't care
    beq .centerLoopCheck
    ; else: break loop
    jmp CTS_LeftTile
.centerLoopCheck
    ; break if C_MAX_DRAW_DIST is reached
    lda Vb_DrawDist
    cmp #C_MAX_DRAW_DIST
    bne .centerLoop

; calc left wall state
CTS_LeftTile:
    M_CTS_LOOP Left, CTS_RightTile
; calc right wall state
CTS_RightTile:
    M_CTS_LOOP Right, CTS_Finalize
    
CTS_Finalize:
    ; mask off wall states according to drawing distance
    ldx Vb_DrawDist
    lda WallStateMaskTable,x
    and Vb_LeftWall
    sta Vb_LeftWall
    lda WallStateMaskTable,x
    and Vb_RightWall
    sta Vb_RightWall
    ; set Vb_BGColFar to EMPTY if DrawDist >= MAX
    cpx #C_MAX_DRAW_DIST
    ; x < MAX? then Vb_BGColFar stays == SOLID
    bcc CTS_Done
    ; else Vb_BGColFar := EMPTY
    lda #COL_BG_EMPTY
    sta Vb_BGColFar
CTS_Done:


    ; set playfield data pointers 
    ; according to position in maze
    ;SET_POINTER Vptr_Sec0Left, PF_1_0 
    ;SET_POINTER Vptr_Sec0Right, PF_1_0
    ;SET_POINTER Vptr_Sec1Left, PF_1_1 
    ;SET_POINTER Vptr_Sec1Right, PF_1_1

    ;SET_POINTER Vptr_Sec2Left, PF_1_1 
    ;SET_POINTER Vptr_Sec2Right, PF_1_0

    ;SET_POINTER Vptr_Sec3Left, PF_1_0 
    ;SET_POINTER Vptr_Sec3Right, PF_1_0

    M_SetSecPtr 0, Left, PF_1_0
    M_SetSecPtr 0, Right, PF_1_0

    M_SetSecPtr 1, Left, PF_1_1
    M_SetSecPtr 1, Right, PF_1_1

    M_SetSecPtr 2, Left, PF_1_1
    M_SetSecPtr 2, Right, PF_1_0

    M_SetSecPtr 3, Left, PF_1_0
    M_SetSecPtr 3, Right, PF_1_0

    ; set background color
    ; according to position in maze
BackgroundColor: SUBROUTINE
    clc
    lda Vb_PlayerPosX
    adc Vb_PlayerPosY
    and #1
    beq .odd
    ldx #COL_BG_LIGHT
    ldy #COL_BG_DARK
    jmp .bgcolend
.odd
    ldx #COL_BG_DARK
    ldy #COL_BG_LIGHT
.bgcolend
    stx Vb_BGColOdd
    sty Vb_BGColEven
    sty COLUBK

    ; set MOB Pointer
    SET_POINTER Vptr_MOB, SKELETON_P0

    rts ;--- GameState

;----------------------------
; Draw visible scanlines
;----------------------------
DrawScreen:
    ; wait until vertical blank period is over
    lda INTIM
    bne DrawScreen
    sta VBLANK ; since A = #0

; --- ##########################
; 16 Scanlines of Section0Top
Section0Top: SUBROUTINE
    ldy #15
.lineLoop
    sta WSYNC
    ; bg color
    lda Vb_BGColEven
    sta COLUBK
    lda (Vptr_Sec0Left),y      ; +5
    sta PF0                 ; +3
    lda #0                  ; +2
    sta PF1                 ; +3
    sta PF2                 ; +3 (18)
    ; wait for PF0 to finish drawing
    SLEEP 16

    lda #0                  ;
    sta PF2                 ; +3 (8)
    sta PF1                 ; +3 (8)
    ; wait for P23 to finish drawing
    SLEEP 6
    lda (Vptr_Sec0Right),y
    and #%11110000
    sta PF0
    dey
    bpl .lineLoop


; --- ##########################
; 32 scanlines of TunnelCenter
TunnelCenter: SUBROUTINE
    ldy #32
.sectionLoop:
    sta WSYNC
    ; assume BG is solid
    lda #COL_BG_SOLID
    sta COLUBK
    ; load PF registers for left side
    ldx Vb_LeftWall
    lda PF_WALL_STATE_0,x
    sta PF0
    lda PF_WALL_STATE_1,x
    sta PF1
    lda PF_WALL_STATE_2,x
    sta PF2   
    ; 30 cycles - it's safe to re-set PF
    ldx Vb_RightWall
    ;32 - wait for PF-Pixel 15 to be drawn 
    SLEEP 3 
    ; set far end bg color
    lda Vb_BGColFar ; +3 
    sta COLUBK      ; +3
    ; set PF registers in reverse order
    lda PF_WALL_STATE_2,x
    sta PF2
    lda PF_WALL_STATE_1,x
    sta PF1
    ; re-set bg col to solid
    lda #COL_BG_SOLID
    sta COLUBK
    ; finish playfield  
    lda PF_WALL_STATE_0,x
    sta PF0   

;    lda (Vptr_MOB),y
;    sta GRP0
;    lda (Vptr_MOB),y
;    sta GRP1

    ; section loop
    dey
    bne .sectionLoop








    ; reset
    ; TODO: remove
    lda #$5C
    sta COLUBK
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC

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
    dec Vb_tmp1
    rts
MoveSouth: SUBROUTINE
    inc Vb_tmp2
    rts
MoveWest: SUBROUTINE
    inc Vb_tmp1
    rts

;----------------------------
; Data
;----------------------------

    echo "---- start data at ",(*)
    ; wall state mask table
    ; index is draw distance
WallStateMaskTable:
    .byte #%00000000    ; DrawDist = 0, should never happen
    .byte #%00001000    ; DrawDist = 1
    .byte #%00001100    ; DrawDist = 2
    .byte #%00001110    ; DrawDist = 3
    .byte #%00001111    ; DrawDist = 4
    .byte #%00001111    ; DrawDist = 5, should never happen


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
    ; TODO: remove!
WalkingTableHI:
    .byte >(MoveWest)
    .byte >(MoveSouth)
    .byte >(MoveEast)
    .byte >(MoveNorth)

WalkingTableLO:
    .byte <(MoveWest)
    .byte <(MoveSouth)
    .byte <(MoveEast)
    .byte <(MoveNorth)

    ; playfield data
    include "pfdata.inc"


PF_WALL_STATE_0:
        .byte #%00000000  ; 0000 
        .byte #%00000000  ; 0001 
        .byte #%00000000  ; 0010 
        .byte #%00000000  ; 0011 
        .byte #%00000000  ; 0100 
        .byte #%00000000  ; 0101 
        .byte #%00000000  ; 0110 
        .byte #%00000000  ; 0111 
        .byte #%11110000  ; 1000 
        .byte #%11110000  ; 1001 
        .byte #%11110000  ; 1010 
        .byte #%11110000  ; 1011 
        .byte #%11110000  ; 1100 
        .byte #%11110000  ; 1101 
        .byte #%11110000  ; 1110 
        .byte #%11110000  ; 1111 

PF_WALL_STATE_1:
        .byte #%00000000  ; 0000 
        .byte #%00000000  ; 0001 
        .byte #%00001111  ; 0010 
        .byte #%00001111  ; 0011 
        .byte #%11110000  ; 0100 
        .byte #%11110000  ; 0101 
        .byte #%11111111  ; 0110 
        .byte #%11111111  ; 0111 
        .byte #%00000000  ; 1000 
        .byte #%00000000  ; 1001 
        .byte #%00001111  ; 1010 
        .byte #%00001111  ; 1011 
        .byte #%11110000  ; 1100 
        .byte #%11110000  ; 1101 
        .byte #%11111111  ; 1110 
        .byte #%11111111  ; 1111 

PF_WALL_STATE_2:
        .byte #%00000000  ; 0000 
        .byte #%00001111  ; 0001 
        .byte #%00000000  ; 0010 
        .byte #%00001111  ; 0011 
        .byte #%00000000  ; 0100 
        .byte #%00001111  ; 0101 
        .byte #%00000000  ; 0110 
        .byte #%00001111  ; 0111 
        .byte #%00000000  ; 1000 
        .byte #%00001111  ; 1001 
        .byte #%00000000  ; 1010 
        .byte #%00001111  ; 1011 
        .byte #%00000000  ; 1100 
        .byte #%00001111  ; 1101 
        .byte #%00000000  ; 1110 
        .byte #%00001111  ; 1111 








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


