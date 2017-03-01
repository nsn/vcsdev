; VCS Crawl 
; (c) 2017 Michael Bayer
; 
;
; TODO
; - last section (3) looks too large, 
;   maybe reduce to 12 scanlines instead of 16?
; - try to color the walls directly facing the player by setting PF registers
;   to #%00000000 and using a designated bgcol
;
; Loads of room for optimizations:
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

;--- end Constants

;----------------------------
; Macros   
;----------------------------

    ; "walks" a step in the direction defined by Player_Orientation
    ; basically just calls the WalkEast/South/West/North subroutine
    ; pointed to by tmp4 and tmp5, then returns to {1}
    ; expects tmp4 and tmp5 to point to the appropriate subroutine
    MAC CallWalkStepReturn

.TARGET SET {1}
    ; push target address to stack
    lda #>(.TARGET-1)
    pha
    lda #<(.TARGET-1)
    pha
    ; jump to walk* subroutine
    jmp (tmp4)

    ENDM ;--- CallWalkStepReturn


    ; sets COLUBK to {1} or PFCOL
    ; depending on X < / >= CullDistance
    ; expects X to contain the section's draw distance
    MAC CullBG
    ; compare X to CullDistance
    cpx CullDistance        ; +3
    bcc .nocull             ; +2/3
    ; X  >= CullDistance -> cull
    lda PFCOL               ; +3 (6)
    jmp .setbg              ; +3 (8)
.nocull
    ; X < CullDistance -> nocull
    lda {1}                 ; +3 (5)
    nop                     ; +2 (8) nop to equalize branch cycle counts
.setbg 
    ; set bg color
    sta COLUBK

    ENDM ;--- CullBG

    
    ; load PosX/Y into tmp1/2
    MAC CopyPos2Tmp

    lda Player_Pos_X
    sta tmp1
    lda Player_Pos_Y
    sta tmp2

    ENDM ;--- CopyPos2Tmp

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

tmp1                ds 1
tmp2                ds 1
tmp3                ds 1
tmp4                ds 1
tmp5                ds 1
tmp6                ds 1
; shadow registers
SWCHA_Shadow        ds 1
; player data
Player_Pos_X        ds 1
Player_Pos_Y        ds 1
; 0 0  E
; 0 1  S
; 1 0  W
; 1 1  N
Player_Orientation  ds 1
; maze data
; 4x4 = 16 bytes
Maze_a_a        ds 1 
Maze_a_b        ds 1
Maze_a_c        ds 1
Maze_a_d        ds 1
Maze_b_a        ds 1
Maze_b_b        ds 1
Maze_b_c        ds 1
Maze_b_d        ds 1
Maze_c_a        ds 1
Maze_c_b        ds 1
Maze_c_c        ds 1
Maze_c_d        ds 1
Maze_d_a        ds 1
Maze_d_b        ds 1
Maze_d_c        ds 1
Maze_d_d        ds 1
; wall section pointers:
; 4*2*2 = 16 bytes
Sec0_l_ptr       ds 2
Sec0_r_ptr       ds 2
Sec1_l_ptr       ds 2
Sec1_r_ptr       ds 2
Sec2_l_ptr       ds 2
Sec2_r_ptr       ds 2
Sec3_l_ptr       ds 2
Sec3_r_ptr       ds 2
; basically how far away is the far wall?
CullDistance     ds 1
; BGColor value, 2 bytes
BGCol_odd           ds 1
BGCol_even          ds 1

    echo "----",($100 - *) , "bytes of RAM left"
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

; init maze
    ldx #0
    lda #0
InitMaze:
    sta Maze_a_a,x
    adc #16
    inx
    cpx #16
    bne InitMaze
; init player pos
    lda #1
    sta Player_Pos_X 
    lda #2
    sta Player_Pos_Y

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
;   tmp1 - playerX
;   tmp2 - playerY
; output:
;   Z flag - set if tile is solid, unset otherwise
; destroys:
;   X, Y
;   tmp2, tmp3
;----------------------------
TestTile: SUBROUTINE
    ; get byte for left corridor walls
    ; - calculate quadrant offset (0-F)  
    lda tmp1
    lsr
    lsr
    lsr 
    sta tmp3
    lda tmp2
    lsr
    and #%11111100
    clc
    adc tmp3
    ; store quadrant offset in x
    tax
    ; load value of quadrant pointer into a
    lda Maze_a_a,x

    ; store it
    sta tmp3
    ; - add y offset
    lda tmp2
    and #%00000111
    clc
    adc tmp3
    ; store in x
    tax
    ; load maze value and
    ; shift so that Player_Pos_X is at lsb
    lda tmp1
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
    cmp SWCHA_Shadow
    beq InputCheckEnd
    ; store new SWCHA state
    sta SWCHA_Shadow
    ; Player orientation, 
    ; joystick left/right
CheckRight:
    and #%10000000
    bne CheckLeft
    dec Player_Orientation
CheckLeft:
    lda SWCHA_Shadow
    and #%01000000
    bne CheckDown
    inc Player_Orientation
    ; Player Position
    ; joystick up/down
CheckDown: SUBROUTINE
    ; normalize Player_Orientation
    lda #%00000011
    and Player_Orientation
    sta Player_Orientation

    ; load PosX/Y into tmp1/2
    CopyPos2Tmp
    ;TODO use tmp1/2 for movement, check for collisions

    lda SWCHA_Shadow
    and #%00100000
    bne CheckUp
    ; down pressed!
    ; modify Player Pos according to Player_Orientation
    lda Player_Orientation
    ; facing east?
    cmp #%00
    bne .notEast
    dec Player_Pos_X
    jmp CheckUp
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    dec Player_Pos_Y
    jmp CheckUp
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    inc Player_Pos_X
    jmp CheckUp
.notWest
    ; facint north!
    inc Player_Pos_Y
CheckUp: SUBROUTINE
    lda SWCHA_Shadow
    and #%00010000
    bne InputCheckEnd
    ; Up pressed!
    ; modify Player Pos according to Player_Orientation
    lda Player_Orientation
    ; facing east?
    cmp #%00
    bne .notEast
    inc Player_Pos_X
    jmp InputCheckEnd
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    inc Player_Pos_Y
    jmp InputCheckEnd
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    dec Player_Pos_X
    jmp InputCheckEnd
.notWest
    ; facint north!
    dec Player_Pos_Y
InputCheckEnd:

    ; set playfield data pointers 
    ; according to position in maze
    ; first: set all to solid

    SET_POINTER Sec0_l_ptr, PF_1_0 
    SET_POINTER Sec0_r_ptr, PF_1_1

    SET_POINTER Sec1_l_ptr, PF_1_1 
    SET_POINTER Sec1_r_ptr, PF_1_1

    SET_POINTER Sec2_l_ptr, PF_1_1 
    SET_POINTER Sec2_r_ptr, PF_1_0

    SET_POINTER Sec3_l_ptr, PF_1_0 
    SET_POINTER Sec3_r_ptr, PF_1_0

    ; set up tmp4 and tmp5 as pointer to the correct walking subrouting
    ; D0 of Player_Orientation: 0 -> E/W, 1 -> N,S
    ; D1 of Player_Orientation: 0 -> inc, 1-> dec
    ; 0 0  E
    ; 0 1  S
    ; 1 0  W
    ; 1 1  N
    ; calc direction index and store in x
    ldx Player_Orientation
    ; load appropriate subroutine location into tmp4 and tmp5
    lda WalkingTableHI,x
    sta tmp5
    lda WalkingTableLO,x
    sta tmp4

    ; far wall
    ; calculate CullDistance
    ; reset
    lda #0
    sta CullDistance
    ; set up tmp1 and tmp2
    CopyPos2Tmp
FarWall: SUBROUTINE
    CallWalkStepReturn FarWallRet
FarWallRet:
    inc CullDistance
    beq .done
    jsr TestTile
    beq FarWall
.done

    ; left corridor wall
LeftWall: SUBROUTINE
    ; set up tmp1 and tmp2
    CopyPos2Tmp
    ; modify according to Player_Orientation
    lda Player_Orientation
    ; facing east?
    cmp #%00
    bne .notEast
    inc tmp2
    jmp .LeftWallStep0
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    dec tmp1
    jmp .LeftWallStep0
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    dec tmp2
    jmp .LeftWallStep0
.notWest
    ; facint north!
    inc tmp1
.LeftWallStep0:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid0
    SET_POINTER Sec0_l_ptr, PF_NONE
.solid0
    ; walk one step formward
    CallWalkStepReturn LeftWallStep1
LeftWallStep1:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid1
    SET_POINTER Sec1_l_ptr, PF_NONE
.solid1
    ; walk one step formward
    CallWalkStepReturn LeftWallStep2
LeftWallStep2:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid2
    SET_POINTER Sec2_l_ptr, PF_NONE
.solid2
    ; walk one step formward
    CallWalkStepReturn LeftWallStep3
LeftWallStep3:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid3
    SET_POINTER Sec3_l_ptr, PF_NONE
.solid3
    ; right corridor wall
RightWall: SUBROUTINE
    ; set up tmp1 and tmp2
    CopyPos2Tmp
    ; modify according to Player_Orientation
    lda Player_Orientation
    ; facing east?
    cmp #%00
    bne .notEast
    dec tmp2
    jmp .RightWallStep0
.notEast
    ; facing south?
    cmp #%01
    bne .notSouth
    inc tmp1
    jmp .RightWallStep0
.notSouth
    ; facing west?
    cmp #%10
    bne .notWest
    inc tmp2
    jmp .RightWallStep0
.notWest
    ; facint north!
    dec tmp1
.RightWallStep0:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid0
    SET_POINTER Sec0_r_ptr, PF_NONE
.solid0
    ; walk one step formward
    CallWalkStepReturn RightWallStep1
RightWallStep1:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid1
    SET_POINTER Sec1_r_ptr, PF_NONE
.solid1
    ; walk one step formward
    CallWalkStepReturn RightWallStep2
RightWallStep2:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid2
    SET_POINTER Sec2_r_ptr, PF_NONE
.solid2
    ; walk one step formward
    CallWalkStepReturn RightWallStep3
RightWallStep3:
    ; test tile, set wall pointer accordingly
    jsr TestTile
    bne .solid3
    SET_POINTER Sec3_r_ptr, PF_NONE
.solid3
     
    ; set background color
    ; according to position in maze
BackgroundColor: SUBROUTINE
    clc
    lda Player_Pos_X
    adc Player_Pos_Y
    and #1
    beq .odd
    ldx #BGCOL_LIGHT
    ldy #BGCOL_DARK
    jmp .bgcolend
.odd
    ldx #BGCOL_DARK
    ldy #BGCOL_LIGHT
.bgcolend
    stx BGCol_odd
    sty BGCol_even
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


    ; Y will be our scanline counter
    ; --- ##########################
    ; 16 Scanlines of Section0Top
Section0Top: SUBROUTINE
    ldy #15
.lineLoop
    sta WSYNC
    ; bg color
    lda BGCol_even
    sta COLUBK
    lda (Sec0_l_ptr),y   ; +5
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
    SLEEP 6
    lda (Sec0_r_ptr),y
    and #%11110000
    sta PF2
    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section1Top
Section1Top: SUBROUTINE
    ldy #15
.lineLoop
    ; prepare CullDistance comparison for later
    ldx #1                  ; +2
    ; prepare PF0 value
    lda #%11110000

    sta WSYNC

    sta PF0
    ; set up PF1
    lda (Sec1_l_ptr),y      ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 

    ; use PF0 and first half of PF1 to set 
    ; bg color: even/odd or playfield
    CullBG BGCol_odd

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF2 to finish drawing
    SLEEP 8

    ; A still is #0
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Sec1_r_ptr),y
    ora #%11110000
    sta PF2

    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section2Top
Section2Top: SUBROUTINE
    ldy #15
.lineLoop
    ; prepare CullDistance comparison for later
    ldx #2                  ; +2
    lda #%11110000
    sta WSYNC

    sta PF0
    lda (Sec2_l_ptr),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    CullBG BGCol_even

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec2_r_ptr),y
    and #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section3Top
Section3Top: SUBROUTINE
    ldy #15
.lineLoop
    ; ScanCycle 64 - 8 cycles left...
    ; prepare CullDistance comparison for later
    ldx #3                  ; +2
    lda #%11111111
    ; out of cycles - need to strobe WSYNC
    sta WSYNC
    ; PF0, Pf1 are solid
    sta PF0
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    CullBG BGCol_odd
    
    lda (Sec3_l_ptr),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec3_r_ptr),y
    ora #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl .lineLoop


    ; --- ##########################
    ; 32 Scanlines of far end of tunnel
FarEnd: SUBROUTINE
    ldy #31
.lineLoop
    ; ScanCycle 62 - 10 cycles left for checks
    ; test if far wall should be solid or BG
    lda #4              ; +2
    ; we only used 5 cycles, but WSYNC takes 3...
    sta WSYNC
    ; now we have ~22 cycles to set bgcol and PF0 
    cmp CullDistance    ; +3 (5)
    bcc .nocull         ; +2/3 
    lda #PFCOL          ; +2 (5)
    jmp .setbg
.nocull
    lda #BGCOL_FAR      ; +2 (4)
    nop                 ; +2 (7) nop to equalize branch cycles
.setbg    

    ; bg color 
    sta COLUBK          

    lda #$ff
    sta PF0
    sta PF1
    lda #%00001111
    sta PF2

    ; wait for PF1 to finish
    SLEEP 15

    sta PF0
    lda #$ff
    sta PF1
    sta PF2

    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section3Bottom
Section3Bottom: SUBROUTINE
    ldy #0
.lineLoop
    ; prepare CullDistance comparison for later
    ldx #3                  ; +2
    lda #%11111111
    ; plenty of cycles left, end line - need to strobe WSYNC
    sta WSYNC
    sta PF0
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    CullBG BGCol_odd

    lda (Sec3_l_ptr),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec3_r_ptr),y
    ora #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    iny
    cpy #16
    bne .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section2Bottom
Section2Bottom: SUBROUTINE
    ldy #0
.lineLoop
    ; prepare CullDistance comparison for later
    ldx #2                  ; +2
    lda #%11110000
    sta WSYNC

    sta PF0
    lda (Sec2_l_ptr),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    CullBG BGCol_even

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec2_r_ptr),y
    and #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    iny
    cpy #16
    bne .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section1Bottom
Section1Bottom: SUBROUTINE
    ldy #0
.lineLoop
    ; prepare CullDistance comparison for later
    ldx #1                  ; +2
    lda #%11110000
    sta WSYNC
    sta PF0
    lda (Sec1_l_ptr),y   ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    CullBG BGCol_odd

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4 

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Sec1_r_ptr),y
    ora #%11110000
    sta PF2

    iny
    cpy #16
    bne .lineLoop


    ; --- ##########################
    ; 16 Scanlines of Section0Bottom
Section0Bottom: SUBROUTINE
    ldy #0
.lineLoop
    sta WSYNC
    ; bg color
    lda BGCol_even
    sta COLUBK
    lda (Sec0_l_ptr),y   ; +5
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
    lda (Sec0_r_ptr),y
    and #%11110000
    sta PF2

    iny
    cpy #16
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
; inc/dec tmp1 (== XCoord) or tmp2 (==YCoord)
WalkNorth: SUBROUTINE
    dec tmp2
    rts
WalkEast: SUBROUTINE
    inc tmp1
    rts
WalkSouth: SUBROUTINE
    inc tmp2
    rts
WalkWest: SUBROUTINE
    dec tmp1
    rts

;----------------------------
; Data
;----------------------------
    ; walk subroutine pointer table
WalkingTableHI:
    .byte >(WalkEast)
    .byte >(WalkSouth)
    .byte >(WalkWest)
    .byte >(WalkNorth)

WalkingTableLO:
    .byte <(WalkEast)
    .byte <(WalkSouth)
    .byte <(WalkWest)
    .byte <(WalkNorth)

    ; playfield data
    include "pfdata.inc"
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


