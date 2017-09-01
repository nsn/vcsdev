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
COL_BG_STATUSBAR = $08
;COL_PF_SOLID = $08
COL_PF_SOLID = $0C

COL_COMPASS = $9A

C_MAX_DRAW_DIST = 5

;--- end Constants

;----------------------------
; Macros   
;----------------------------
    ;####################################################################
    ; loads the maze quadrant offset for x/y coords into a
    ;
    ; M_LoadMapQuadrantRow <X> <Y> <TMP>
    ; destroys <TMP>
    MAC M_LoadMapQuadrant
    
    ; calc quadrant pointer (Vb_MazeXX) offset (0-F)
    ; quadrant_offset = quadrant_x + quadrant_y * 
    ; a quadrant is 8 tiles wide, the maze is 4x4 quadrants 
    ; quadrant_x = player_x/8
    ; quadrant_y = player_y/8
    ; offset = player_x/8 + (player_y*4)/8
    ; --> offset = px/8 + py/2

    lda {1}           ; A = player_x
    lsr
    lsr
    lsr               ; A = player_x/8
    sta {3}           ; {3} = A
    lda {2}           ; A = player_y          
    lsr               ; A = player_Y/2
    and #%11111100    ; erase 2 lowest bits, rounds down, quadrants are not divisible
    clc
    adc {3}           ; A = px/8 + py/2

    ENDM ;--- M_LoadMapQuadrant

    ;####################################################################
    ; loads a maze row for x/y coords into A
    ;
    ; M_LoadMapQuadrantRow <X> <Y> <TMP>
    MAC M_LoadMapQuadrantRow
   
    ; calc quadrant pointer (Vb_MazeXX) offset (0-F)
    ; quadrant_offset = quadrant_x + quadrant_y * 
    ; a quadrant is 8 tiles wide, the maze is 4x4 quadrants 
    ; quadrant_x = player_x/8
    ; quadrant_y = player_y/8
    ; offset = player_x/8 + (player_y*4)/8
    ; --> offset = px/8 + py/2

    lda {1}           ; A = player_x
    lsr
    lsr
    lsr               ; A = player_x/8
    sta {3}           ; {3} = A
    lda {2}           ; A = player_y          
    lsr               ; A = player_Y/2
    and #%11111100    ; erase 2 lowest bits, rounds down, quadrants are not divisible
    clc
    adc {3}           ; A = px/8 + py/2

    tax               ; store offset into X
    lda Vb_MazeAA,x   ; load maze offset
    sta {3}           ; store it in tmp var
    lda {2}           ; load py
    and #%00000111    ; lower 3 bits determine row
    clc
    adc {3}           ; offset + row
    tax               ; X = offset + row

    lda MAZEDATA_0,x

    ENDM ;--- M_LoadMapQuadrantRow


    ;####################################################################
    ;####################################################################
    ; "walks" a step in the direction defined by Vb_PlayerOrientation
    ; basically just calls the MoveEast/South/West/North subroutine
    ; pointed to by Vb_tmp03 and Vb_tmp04, then returns to {1}
    ; expects Vb_tmp03 and Vb_tmp04 to point to the appropriate subroutine
    MAC M_CallWalkStepReturn

.TARGET SET {1}
    ; push target address to stack
    lda #>(.TARGET-1)
    pha
    lda #<(.TARGET-1)
    pha
    ; jump to walk* subroutine
    jmp (Vb_tmp03)

    ENDM ;--- M_CallWalkStepReturn
    ;####################################################################


    ;####################################################################
    ; movement macros
    ; calls appropriate Move* subroutines 
    ; destroys 
    ; x
    ; Vb_tmp03 and 5
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
    sta Vb_tmp04
    lda Move{1}PtrLOTable,x
    sta Vb_tmp03
    ; execute jump
    jmp (Vb_tmp03)

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
    ; load PosX/Y into Vb_tmp00/2
    MAC M_CopyPos2Tmp

    lda Vb_PlayerPosX
    sta Vb_tmp00
    lda Vb_PlayerPosY
    sta Vb_tmp01

    ENDM ;--- M_CopyPos2Tmp
    ;####################################################################


    ;####################################################################
    ; calculate wall state, uses Vb_tmp05 and Vb_tmp04
    ; param {1} Left/Right
    ; param {2} label to break to
    MAC M_CTS_LOOP
    ; initialize variables
    lda #C_MAX_DRAW_DIST-1
    sta Vb_tmp05
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
    sta Vb_tmp04 
    ; load wall state
    lda Vb_{1}Wall
    ; shift left
    clc
    asl
    ; set lsb
    ora Vb_tmp04
    ; store new sate
    sta Vb_{1}Wall
    ; dec loop var, break if 0
    dec Vb_tmp05
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
    ;####################################################################


    ;####################################################################
    ; M_SetDigitPtr Vb_PlayerHPHi, Vb_tmp00, Vb_tmp02
    MAC M_SetDigitPtr
    
    ; HI nibble -> first digit
    lda {1}
    and #%11110000
    ror
    ror
    ror
    ror
    tax
    ; load digit memory address from table
    lda DigitsFrameTable,x
    ; store into lo part of pointer
    sta {2}
    ; 2nd digit
    lda {1}
    and #%00001111
    tax
    lda DigitsFrameTable,x
    sta {3}

    ENDM ;--- M_SetDigitPtr
    ;####################################################################


    ;####################################################################
    ; M_BuildDigitBfr Vb_tmp00, Vb_tmp02, Vb_tmp08, GRP0
    MAC M_BuildDigitBfr 

    lda ({1}),y
    and #%11110000
    sta {3}
    lda ({2}),y
    and #%00001111
    ora {3}
    sta {4} 

    ENDM ;--- M_SetDigitPtr

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
Vb_SWCHA_Shadow        ds 1
; player data
Vb_PlayerPosX        ds 1
Vb_PlayerPosY        ds 1
; in BCF
Vb_PlayerHPLo        ds 1
Vb_PlayerHPHi        ds 1
Vb_PlayerHPMaxLo     ds 1
Vb_PlayerHPMaxHi     ds 1
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
; compass pointer, 2 bytes
Vptr_Compass     ds 2
; Wall states
Vb_LeftWall      ds 1
Vb_RightWall     ds 1
; basically how far to the end of the tunnel?
Vb_DrawDist      ds 1
; BGColor value, 3 bytes
Vb_BGColOdd      ds 1
Vb_BGColEven     ds 1
Vb_BGColSec1     ds 1
Vb_BGColSec2     ds 1
Vb_BGColSec3     ds 1
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
;   Vb_tmp00 - playerX
;   Vb_tmp01 - playerY
; output:
;   Z flag - set if tile is solid, unset otherwise
; destroys:
;   X, Y
;   Vb_tmp02
;----------------------------
TestTile: SUBROUTINE
    ; get byte for left corridor walls
    ; - calculate quadrant offset (0-F)  
    lda Vb_tmp00
    lsr
    lsr
    lsr 
    sta Vb_tmp02
    lda Vb_tmp01
    lsr
    and #%11111100
    clc
    adc Vb_tmp02
    ; store quadrant offset in x
    tax
    ; load value of quadrant pointer into a
    lda Vb_MazeAA,x

    ; store it
    sta Vb_tmp02
    ; - add y offset
    lda Vb_tmp01
    and #%00000111
    clc
    adc Vb_tmp02
    ; store in x
    tax
    ; load maze value and
    ; shift so that Vb_PlayerPosX is at lsb
    lda Vb_tmp00
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
    ; load PosX/Y into Vb_tmp00/2
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
    ; copy Vb_tmp00/2 back to PosX/Y
    lda Vb_tmp00
    sta Vb_PlayerPosX
    lda Vb_tmp01
    sta Vb_PlayerPosY
    jmp NoMovement
Collision:
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

CTS_Done:


    ; set Section pointers
    M_SetSecPtr 0, Left, PF_1_0
    M_SetSecPtr 0, Right, PF_1_0

    M_SetSecPtr 1, Left, PF_1_1
    M_SetSecPtr 1, Right, PF_1_1

    M_SetSecPtr 2, Left, PF_1_1
    M_SetSecPtr 2, Right, PF_1_1

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
    jmp .bgcolset
.odd
    ldx #COL_BG_DARK
    ldy #COL_BG_LIGHT
.bgcolset
    stx Vb_BGColOdd
    sty Vb_BGColEven
    sty COLUBK

    ; set Vb_BGColFar to EMPTY if DrawDist >= MAX
    ; x < MAX? then Vb_BGColFar stays == SOLID
    ;bcc CTS_Done
    ; else Vb_BGColFar := EMPTY
    ;lda #COL_BG_EMPTY
    ;sta Vb_BGColFar




    ; set MOB Pointer
    SET_POINTER Vptr_MOB, SKELETON_P0
    ; set Compass Pointer depending on Vb_PlayerOrientation
    ldx Vb_PlayerOrientation
    lda CompassFrameTableHI
    sta Vptr_Compass+1
    lda CompassFrameTableLO,x
    sta Vptr_Compass
    ; set player HP
    lda #$99
    sta Vb_PlayerHPMaxLo
    sta Vb_PlayerHPMaxHi
    lda #%00110100
    sta Vb_PlayerHPLo
    lda #%00010010
    sta Vb_PlayerHPHi


    rts ;--- GameState

;----------------------------
; Draw visible scanlines
;----------------------------
DrawScreen:
    ; set P0 (compass) behaviour
    lda #%00000101
    sta NUSIZ0
    lda #COL_COMPASS
    sta COLUP0
    ; wait until vertical blank period is over
    lda INTIM
    bne DrawScreen
    sta VBLANK ; since A = #0

; --- ##########################
; 16 Scanlines of Section0Top
Section0Top: SUBROUTINE
    ldy #15
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-4
    bcc .solidbg
    lda Vb_BGColEven
.solidbg:
    sta COLUBK

    ; load PF registers for left side
    lda (Vptr_Sec0Left),y      ; +5
    sta PF0                 ; +3
    lda #0                  ; +2
    sta PF1                 ; +3
    sta PF2                 ; +3

    ; load compass
    lda (Vptr_Compass),y    ; +5
    sta GRP0                ; +3 (34)

    ; wait for beam to reach center 
    ; minus compass width, minus sta cycles (~= cycle 40)
    SLEEP 8

    ; draw compass
    sta RESP0

    ; wait for PF0 to finish drawing
    ; SLEEP 4
    
    lda (Vptr_Sec0Right),y
    and #%11110000
    sta PF0
    dey
    bpl .lineLoop

; --- ##########################
; 16 Scanlines of Section1Top
Section1Top: SUBROUTINE
    ldy #15
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-3
    bcc .solidbg
    lda Vb_BGColOdd
.solidbg:
    sta COLUBK

    ; PF0 
    ;ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0
    ; PF1 depends on wall state
    lda (Vptr_Sec1Left),y
    and #%11110000
    sta PF1
    ; PF0 finished rendering
    ; PF2 is fixed
    lda #0
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 4

    ; PF1 depends on wall state
    lda (Vptr_Sec1Right),y
    and #%11110000
    sta PF1

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    dey
    bpl .lineLoop

; --- ##########################
; 16 Scanlines of Section2Top
Section2Top: SUBROUTINE
    ldy #15
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-2
    bcc .solidbg
    lda Vb_BGColEven
.solidbg:
    sta COLUBK

    ; PF0 
    ;ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0
    ; PF1 depends on wall state
    lda (Vptr_Sec2Left),y
    ora #%11110000
    sta PF1
    ; PF0 finished rendering
    ; PF2 is fixed
    lda #0
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 4

    ; PF1 depends on wall state
    lda (Vptr_Sec2Right),y
    ora #%11110000
    sta PF1

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    dey
    bpl .lineLoop

; --- ##########################
; 16 Scanlines of Section3Top
Section3Top: SUBROUTINE
    ldy #15
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-1
    bcc .solidbg
    lda Vb_BGColOdd 
.solidbg:
    sta COLUBK
    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11111111
    sta PF0
    ; PF1 
    sta PF1
    ; PF0 finished rendering
    ; PF2 depends on wall state
    lda (Vptr_Sec3Left),y
    and #%00001111
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 3

    ; PF1 depends on wall state
    lda (Vptr_Sec3Right),y
    and #%00001111
    sta PF2

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    dey
    bpl .lineLoop

; --- ##########################
; 32 scanlines of TunnelCenter
TunnelCenter: SUBROUTINE
    ldy #31
.sectionLoop:
    sta WSYNC
    ; bg col
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    cpx #C_MAX_DRAW_DIST
    ; DrawDist < MAX?
    bcc .solidbg
    ; DrawDist >= MAX
    lda #COL_BG_EMPTY
.solidbg:
    sta COLUBK

    lda #%11111111
    sta PF0
    sta PF1
    and #%00001111
    sta PF2

    ;lda #COL_BG_SOLID
    ;sta COLUBK
    ; load PF registers for left side
    ;ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    ;sta PF0
    ;lda PF_WALL_STATE_1,x
    ;sta PF1
    ;lda PF_WALL_STATE_2,x
    ;sta PF2   
    ; 30 cycles - it's safe to re-set PF
    ;ldx Vb_RightWall
    ;32 - wait for PF-Pixel 15 to be drawn 
    ;SLEEP 3 
    ; set far end bg color
    ;lda Vb_BGColFar ; +3 
    ;sta COLUBK      ; +3
    ; set PF registers in reverse order
    ;lda PF_WALL_STATE_2,x
    ;sta PF2
    ;lda PF_WALL_STATE_1,x
    ;sta PF1
    ; re-set bg col to solid
    ;lda #COL_BG_SOLID
    ;sta COLUBK
    ; finish playfield  
    ;lda PF_WALL_STATE_0,x
    ;sta PF0   

;    lda (Vptr_MOB),y
;    sta GRP0
;    lda (Vptr_MOB),y
;    sta GRP1

    ;SLEEP 2
    ; section loop
    dey
    bne .sectionLoop


; --- ##########################
; 16 Scanlines of Section3Bottom
Section3Bottom: SUBROUTINE
    ldy #0 
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-1
    bcc .solidbg
    lda Vb_BGColOdd 
.solidbg:
    sta COLUBK
    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11111111
    sta PF0
    ; PF1 
    sta PF1
    ; PF0 finished rendering
    ; PF2 depends on wall state
    lda (Vptr_Sec3Left),y
    and #%00001111
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 3

    ; PF1 depends on wall state
    lda (Vptr_Sec3Right),y
    and #%00001111
    sta PF2

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    iny
    cpy #16
    bne .lineLoop


; --- ##########################
; 16 Scanlines of Section2Bottom
Section2Bottom: SUBROUTINE
    ldy #0 
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-2
    bcc .solidbg
    lda Vb_BGColEven
.solidbg:
    sta COLUBK

    ; PF0 
    ;ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0
    ; PF1 depends on wall state
    lda (Vptr_Sec2Left),y
    ora #%11110000
    sta PF1
    ; PF0 finished rendering
    ; PF2 is fixed
    lda #0
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 4

    ; PF1 depends on wall state
    lda (Vptr_Sec2Right),y
    ora #%11110000
    sta PF1

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    iny
    cpy #16
    bne .lineLoop

; --- ##########################
; 16 Scanlines of Section1Bottom
Section1Bottom: SUBROUTINE
    ldy #0
    ; cycle #68
.lineLoop
    ; set bg
    lda #COL_BG_SOLID
    ldx Vb_DrawDist
    sta WSYNC
    cpx #C_MAX_DRAW_DIST-3
    bcc .solidbg
    lda Vb_BGColOdd
.solidbg:
    sta COLUBK

    ; PF0 
    ;ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0
    ; PF1 depends on wall state
    lda (Vptr_Sec1Left),y
    and #%11110000
    sta PF1
    ; PF0 finished rendering
    ; PF2 is fixed
    lda #0
    sta PF2
    ; wait for first 8 PF pixels to finish
    SLEEP 4

    ; PF1 depends on wall state
    lda (Vptr_Sec1Right),y
    and #%11110000
    sta PF1

    ; PF0 
    ldx Vb_LeftWall
    ;lda PF_WALL_STATE_0,x
    lda #%11110000
    sta PF0

    iny
    cpy #16
    bne .lineLoop

; --- TODO: make cleaner cut betwenn section and status bar
    lda #0 
    sta WSYNC
    sta PF1
    sta PF2 
    sta PF0
    lda #COL_BG_STATUSBAR
    sta COLUBK
; --- ##########################
; 55 lines of status bar

StatusBar: SUBROUTINE
    ; set up TIA behaviour
    lda #$1E
    sta COLUP0
    sta COLUP1
    lda #0
    sta VDELP0
    sta VDELP1
    lda #0
    sta NUSIZ0
    sta NUSIZ1
    lda #0
    sta REFP0
    sta REFP1

    ; set up digit display buffers
    clc
    lda #>(DIGITS)
    ; hi part of pointer
    sta Vb_tmp01
    sta Vb_tmp03
    sta Vb_tmp05
    sta Vb_tmp07
    ; digits: hi byte of player hp
    M_SetDigitPtr Vb_PlayerHPHi, Vb_tmp00, Vb_tmp02
    ; digits: low bye of player hp
    M_SetDigitPtr Vb_PlayerHPLo, Vb_tmp04, Vb_tmp06

; ### HP Display
    ; digit height: 6px
    ldy #5
.hpLoop:
    sta WSYNC
    ; hi part of player hp: P0
    M_BuildDigitBfr Vb_tmp00, Vb_tmp02, Vb_tmp08, GRP0
    ; lo part of player hp: P1
    M_BuildDigitBfr Vb_tmp04, Vb_tmp06, Vb_tmp08, GRP1
   
    SLEEP 4
    sta RESP0
    sta RESP1

    dey 
    bpl .hpLoop

; ### clear
    lda #0 
    sta GRP0
    sta GRP1

; ### MiniMap
    ; set tia behaviour
    ;lda #5
    ;sta NUSIZ0
    ; load quadrant offset into x
    M_LoadMapQuadrant Vb_PlayerPosX, Vb_PlayerPosY, Vb_tmp00
    tax
    ldy #24
.MiniMapLoop:
    sta WSYNC

    lda Vb_MazeAA,x
    sta GRP0
    ;sta RESP0
    
    ; inc maze data index
    ;inx
    ; dec loop var
    dey
    bne .MiniMapLoop

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
; todo:
;   lda #35
;   sta TIM64T
; Loop:
;   lda INTIM
;   bne Loop
;   ; end of overscan
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
;----------------------------
; Walking subroutines
;----------------------------
; inc/dec Vb_tmp00 (== XCoord) or Vb_tmp01 (==YCoord)
MoveNorth: SUBROUTINE
    dec Vb_tmp01
    rts
MoveEast: SUBROUTINE
    dec Vb_tmp00
    rts
MoveSouth: SUBROUTINE
    inc Vb_tmp01
    rts
MoveWest: SUBROUTINE
    inc Vb_tmp00
    rts

; digit frames - 10 bytes
DigitsFrameTable:
    .byte <(DIGITS_F0)  ; 00000000 -> 0
    .byte <(DIGITS_F1)  ; 00000001 -> 1
    .byte <(DIGITS_F2)  ; 00000010 -> 2
    .byte <(DIGITS_F3)  ; 00000011 -> 3
    .byte <(DIGITS_F4)  ; 00000100 -> 4
    .byte <(DIGITS_F5)  ; 00000101 -> 5
    .byte <(DIGITS_F6)  ; 00000110 -> 6
    .byte <(DIGITS_F7)  ; 00000111 -> 7
    .byte <(DIGITS_F8)  ; 00001000 -> 8
    .byte <(DIGITS_F9)  ; 00001001 -> 9

    ; compass frame address table
    ; 5 bytes = 15
    ; low bytes only
CompassFrameTableHI:
    .byte >(COMPASS)
CompassFrameTableLO:
    .byte <(COMPASS_F0) ; 00 -> facing east
    .byte <(COMPASS_F1) ; 01 -> facing south
    .byte <(COMPASS_F2) ; 10 -> facing west
    .byte <(COMPASS_F3) ; 11 -> facing north

    ; wall state mask table
    ; 6 bytes = 21
    ; index is draw distance
WallStateMaskTable:
    .byte #%00000000    ; DrawDist = 0, should never happen
    .byte #%00001000    ; DrawDist = 1
    .byte #%00001100    ; DrawDist = 2
    .byte #%00001110    ; DrawDist = 3
    .byte #%00001111    ; DrawDist = 4
    .byte #%00001111    ; DrawDist = 5, should never happen


    ; movement soubroutine pointer table
    ; 17 bytes = 38
    ; all Move* subroutines pointers share a single HI byte
MovePtrHI:
    .byte >(MoveNorth)
    ; low bytes, index /w Vb_PlayerOrientation
    ; forward
MoveForwardPtrLOTable:
    .byte <(MoveWest)   ; 00 -> facing east
    .byte <(MoveSouth)  ; 01 -> facing south
    .byte <(MoveEast)   ; 10 -> facing west
    .byte <(MoveNorth)  ; 11 -> facing north
    ; back
MoveBackPtrLOTable:
    .byte <(MoveEast)   ; 00 -> facing east
    .byte <(MoveNorth)  ; 01 -> facing south
    .byte <(MoveWest)   ; 10 -> facing west
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


DATA_WallStates ALIGN 256
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
DATA_MOBS ALIGN 256
    include "mobdata.inc"
DATA_COMPASS ALIGN 256
    ; compass, needs to be on one page, 64 bytes
    include "compassdata.inc"
    ; digits, need to be on same page, 60 bytes 
    include "digitsdata.inc"
    ; playfield data, 64 bytes
    include "pfdata.inc"

    ; maze data needs to be page aligned...
    ;ORG $FD00
DATA_MAZE ALIGN 256
    include "mazedata.inc"

    echo "---- bytes left ",($fffc - *)
    ;echo "---- reset gap ",($fffc - *)
;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


