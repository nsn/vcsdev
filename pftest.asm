; playfield test

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------
MASK_UPPER_NIBBLE = %11110000
MASK_LOWER_NIBBLE = %00001111

PAL = 0
NTSC = 1

; ntsc constants
VBLANK_WAIT = #43
NUM_SCANLINES = 191

; colors
PFCOL_DARK  = $94
PFCOL_LIGHT = $98

; scanline heights for tunnel segments
TS0_CEIL_SCANLINES = NUM_SCANLINES-0
TS1_CEIL_SCANLINES = NUM_SCANLINES-15
TS2_CEIL_SCANLINES = NUM_SCANLINES-31
TS3_CEIL_SCANLINES = NUM_SCANLINES-47
TS4_CEIL_SCANLINES = NUM_SCANLINES-63

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

tmp         ds 2

PFData0     .byte   
PFData1     .byte  
PFData2     .byte 
PFData3     .byte 
PFData4     .byte 
PFData5     .byte 


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

    ; init PF variables
    lda #%00010000
    sta PFData0
    lda #%00000000
    sta PFData1
    sta PFData2
    sta PFData3
    sta PFData4
    lda #%10000000
    sta PFData5
    ; set background color
    lda #PFCOL_DARK
    sta COLUBK

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
    ; WSYNC is placed BEFORE all of this action takes place.
    sta WSYNC

    ;-----------
    ; Playfield
    ;-----------
    ; a section ( = one step) of tunnel is 4 blocks of 4 = 16 scanlines high
 
    ; store left side of Playfield
    lda PFData0
    sta PF0
    lda PFData1
    sta PF1
    lda PFData2
    sta PF2

    ; every 4th ScanLine
    tya
    and #%00000011
    bne PFDone
TunnelSection0:
    ; top 16 scanlines: 
    ; top left and top right sides of tunnel
    cpy #TS1_CEIL_SCANLINES
    bcc TunnelSection1
    beq TunnelSection1
    ; pf calculations
    jsr PFCalcTS0
    jmp PFDone
TunnelSection1:
    ; next 16 scanlines 
    ; 2nd tunnel section
    cpy #TS2_CEIL_SCANLINES
    bcc TunnelSection2
    beq TunnelSection2
    ; set bgcol
    lda #PFCOL_LIGHT
    sta COLUBK
    ; pf calculations
    jsr PFCalcTS1
    jmp PFDone
TunnelSection2:
    ; next 16 scanlines 
    ; 2nd tunnel section
    cpy #TS3_CEIL_SCANLINES
    bcc TunnelSection3
    beq TunnelSection3
    ; set bgcol
    lda #PFCOL_DARK
    sta COLUBK
    ; pf calculations
    jsr PFCalcTS2
    jmp PFDone
TunnelSection3:
    ; next 16 scanlines 
    ; 3nd tunnel section
    cpy #TS4_CEIL_SCANLINES
    bcc TunnelSection4
    beq TunnelSection4
    ; set bgcol
    lda #PFCOL_LIGHT
    sta COLUBK
    ; pf calculations
    jsr PFCalcTS3
    jmp PFDone
TunnelSection4:
    
PFDone:


    ;-----------
    ; End Playfield
    ;-----------

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
; Playfield Calculations
;----------------------------
; calc playfield for tunnel segment 0
PFCalcTS0:
    ; calc upper nibble of PF0
    lda PFData0
    and #MASK_UPPER_NIBBLE
    asl 
    ora #%00010000
    sta tmp
    lda PFData0
    and #MASK_LOWER_NIBBLE
    ora tmp
    sta PFData0
    ; TODO: calc upper nibble of PF5
    lda PFData5
    and #MASK_UPPER_NIBBLE
    lsr 
    ora #%10000000
    sta tmp
    lda PFData5
    and #MASK_LOWER_NIBBLE
    ora tmp
    sta PFData5
    rts ; PFCalcTS0
; calc playfield for tunnel segment 1
PFCalcTS1:
    ; calc upper nibble of PF1
    lda PFData1
    and #MASK_UPPER_NIBBLE
    lsr 
    ora #%10000000
    sta tmp
    lda PFData1
    and #MASK_LOWER_NIBBLE
    ora tmp
    sta PFData1
    ; calc lower nibble of PF5
    lda PFData5
    and #MASK_LOWER_NIBBLE
    lsr 
    ora #%00001000
    sta tmp
    lda PFData5
    and #MASK_LOWER_NIBBLE
    ora tmp
    sta PFData5
    rts ; PFCalcTS1
; calc playfield for tunnel segment 2
PFCalcTS2:
    ; calc lower nibble of PF1
    lda PFData1
    and #MASK_LOWER_NIBBLE
    lsr 
    ora #%00001000
    sta tmp
    lda PFData1
    and #MASK_UPPER_NIBBLE
    ora tmp
    sta PFData1
    ; calc lower nibble of PF4
    lda PFData4
    and #MASK_LOWER_NIBBLE
    asl 
    ora #%00000001
    sta tmp
    lda PFData4
    and #MASK_UPPER_NIBBLE
    ora tmp
    sta PFData4
    rts ; PFCalcTS2
; calc playfield for tunnel segment 3
PFCalcTS3:
    ; calc lower nibble of PF2
    lda PFData2
    and #MASK_LOWER_NIBBLE
    asl 
    ora #%00000001
    sta tmp
    lda PFData2
    and #MASK_UPPER_NIBBLE
    ora tmp
    sta PFData2
    ; calc upper nibble of PF4
    lda PFData4
    and #MASK_UPPER_NIBBLE
    asl 
    ora #%00010000
    sta tmp
    lda PFData4
    and #MASK_LOWER_NIBBLE
    ora tmp
    sta PFData4
    rts ; PFCalcTS3


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

PFData:

PFData_0_L:
    .byte #%00010000    
    .byte #%00110000     
    .byte #%01110000     
    .byte #%11110000     

PFData_1_L:
    .byte #%10000000    
    .byte #%11000000    
    .byte #%11100000    
    .byte #%11110000    
    .byte #%11111000    
    .byte #%11111100    
    .byte #%11111110    
    .byte #%11111111    

PFData_2_L:
    .byte #%00000001    
    .byte #%00000011    
    .byte #%00000111    
    .byte #%00001111    

;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


