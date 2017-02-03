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
BGCOL_FAR = $00

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
; shadow registers
SWCHA_Shadow        ds 1
; player data
Player_Pos_X        ds 1
Player_Pos_Y        ds 1
Player_Orientation  ds 1
; maze data
; 9*2 = 18 bytes
Maze_a_a_ptr        ds 2
Maze_a_b_ptr        ds 2
Maze_a_c_ptr        ds 2
Maze_b_a_ptr        ds 2
Maze_b_b_ptr        ds 2
Maze_b_c_ptr        ds 2
Maze_c_a_ptr        ds 2
Maze_c_b_ptr        ds 2
Maze_c_c_ptr        ds 2
; wall section pointers:
; 4*2*2 = 16 bytes
; TODO: remove the need for btm ptrs,
; as upper and lower part of section walls
; has to be equal, just inverted
; would also reduce pfdata ROM size
Sec0_top_l_ptr       ds 2
Sec0_top_r_ptr       ds 2
Sec0_btm_l_ptr       ds 2
Sec0_btm_r_ptr       ds 2
Sec1_top_l_ptr       ds 2
Sec1_top_r_ptr       ds 2
Sec1_btm_l_ptr       ds 2
Sec1_btm_r_ptr       ds 2
Sec2_top_l_ptr       ds 2
Sec2_top_r_ptr       ds 2
Sec2_btm_l_ptr       ds 2
Sec2_btm_r_ptr       ds 2
Sec3_top_l_ptr       ds 2
Sec3_top_r_ptr       ds 2
Sec3_btm_l_ptr       ds 2
Sec3_btm_r_ptr       ds 2
; BGColor value, 2 bytes
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

; init maze
    SET_POINTER Maze_a_a_ptr, MAZE_A_A_0
    SET_POINTER Maze_a_b_ptr, MAZE_A_A_0
    SET_POINTER Maze_a_c_ptr, MAZE_A_A_0
    SET_POINTER Maze_b_a_ptr, MAZE_A_A_0
    SET_POINTER Maze_b_b_ptr, MAZE_A_A_0
    SET_POINTER Maze_b_c_ptr, MAZE_A_A_0
    SET_POINTER Maze_c_a_ptr, MAZE_A_A_0
    SET_POINTER Maze_c_b_ptr, MAZE_A_A_0
    SET_POINTER Maze_c_c_ptr, MAZE_A_A_0

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
;----------------------------
TestMaze:
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
    inc Player_Orientation
CheckLeft:
    lda SWCHA_Shadow
    and #%01000000
    bne CheckUp
    dec Player_Orientation
    ; Player Position
    ; joystick up/down
CheckUp:
CheckDown:
    lda #1
    sta Player_Pos_X 
    lda #4
    sta Player_Pos_Y
InputCheckEnd:

    ldx Player_Pos_X
    ldy Player_Pos_Y
    jsr TestMaze

    ; set playfield data pointers 
    ; according to position in maze
    SET_POINTER Sec0_top_l_ptr, PF_1_0 
        SET_POINTER Sec0_top_l_ptr, PF_NONE
    SET_POINTER Sec0_top_r_ptr, PF_1_1
    SET_POINTER Sec0_btm_l_ptr, PF_0_0 
        SET_POINTER Sec0_btm_l_ptr, PF_NONE
    SET_POINTER Sec0_btm_r_ptr, PF_0_1

    SET_POINTER Sec1_top_l_ptr, PF_1_1 
    SET_POINTER Sec1_top_r_ptr, PF_1_1
    SET_POINTER Sec1_btm_l_ptr, PF_0_1 
    SET_POINTER Sec1_btm_r_ptr, PF_0_1

    SET_POINTER Sec2_top_l_ptr, PF_1_1 
    SET_POINTER Sec2_top_r_ptr, PF_1_0
        SET_POINTER Sec2_top_r_ptr, PF_NONE
    SET_POINTER Sec2_btm_l_ptr, PF_0_1 
    SET_POINTER Sec2_btm_r_ptr, PF_0_0
        SET_POINTER Sec2_btm_r_ptr, PF_NONE

    SET_POINTER Sec3_top_l_ptr, PF_1_0 
    SET_POINTER Sec3_top_r_ptr, PF_1_0
    SET_POINTER Sec3_btm_l_ptr, PF_0_0 
    SET_POINTER Sec3_btm_r_ptr, PF_0_0

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
    ; --- ##########################
    ; 16 Scanlines of Section0Top
    ldy #15
Section0Top:
    sta WSYNC
    ; bg color
    lda BGCol_even
    sta COLUBK
    lda (Sec0_top_l_ptr),y   ; +5
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
    lda (Sec0_top_r_ptr),y
    and #%11110000
    sta PF2
    dey
    bpl Section0Top

    ; --- ##########################
    ; 16 Scanlines of Section1Top
    ldy #15
Section1Top:
    sta WSYNC
    lda #%11110000
    sta PF0

    ; bg color
    lda BGCol_odd
    sta COLUBK

    lda (Sec1_top_l_ptr),y   ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 
    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Sec1_top_r_ptr),y
    ora #%11110000
    sta PF2

    dey
    bpl Section1Top

    ; --- ##########################
    ; 16 Scanlines of Section2Top
    ldy #15
Section2Top:
    sta WSYNC
    lda #%11110000
    sta PF0
    ; bg color
    lda BGCol_even
    sta COLUBK

    lda (Sec2_top_l_ptr),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 
    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec2_top_r_ptr),y
    and #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl Section2Top

    ; --- ##########################
    ; 16 Scanlines of Section3Top
    ldy #15
Section3Top:
    sta WSYNC
    lda #%11111111
    sta PF0
    sta PF1                 ; +3 
    ; bg color
    lda BGCol_odd
    sta COLUBK

    lda (Sec3_top_l_ptr),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec3_top_r_ptr),y
    ora #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl Section3Top


    ; --- ##########################
    ; 32 Scanlines of far end of tunnel
    ldy #31
FarEnd:
    sta WSYNC
    ; bg color 
    lda #BGCOL_FAR
    sta COLUBK

    lda #$ff
    sta PF0
    sta PF1
    lda #%00001111
    sta PF2

    ; wait for PF1 to finish
    SLEEP 22

    sta PF0
    lda #$ff
    sta PF1
    sta PF2

    dey
    bpl FarEnd

    ; --- ##########################
    ; 16 Scanlines of Section3Bottom
    ldy #15
Section3Bottom:
    sta WSYNC
    lda #%11111111
    sta PF0
    sta PF1                 ; +3 
    ; bg color
    lda BGCol_odd
    sta COLUBK

    lda (Sec3_btm_l_ptr),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec3_btm_r_ptr),y
    ora #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl Section3Bottom

    ; --- ##########################
    ; 16 Scanlines of Section2Bottom
    ldy #15
Section2Bottom:
    sta WSYNC
    lda #%11110000
    sta PF0
    ; bg color
    lda BGCol_even
    sta COLUBK

    lda (Sec2_btm_l_ptr),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 
    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Sec2_btm_r_ptr),y
    and #%00001111
    sta PF1                 ; +3 (8)
    lda #%11111111
    sta PF2

    dey
    bpl Section2Bottom

    ; --- ##########################
    ; 16 Scanlines of Section1Bottom
    ldy #15
Section1Bottom:
    sta WSYNC
    lda #%11110000
    sta PF0

    ; bg color
    lda BGCol_odd
    sta COLUBK

    lda (Sec1_btm_l_ptr),y   ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 
    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 12

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Sec1_btm_r_ptr),y
    ora #%11110000
    sta PF2

    dey
    bpl Section1Bottom


    ; --- ##########################
    ; 16 Scanlines of Section0Bottom
    ldy #15
Section0Bottom:
    sta WSYNC
    ; bg color
    lda BGCol_even
    sta COLUBK
    lda (Sec0_btm_l_ptr),y   ; +5
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
    lda (Sec0_btm_r_ptr),y
    and #%11110000
    sta PF2
    dey
    bpl Section0Bottom


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
; Data
;----------------------------

    include "pfdata.inc"

MAZE_A_A_0:
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%10110111
    .byte #%10000000
    .byte #%11101101
    .byte #%11111111
    .byte #%11111111


;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


