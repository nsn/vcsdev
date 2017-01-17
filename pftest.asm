; playfield test

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------
VBLANK_WAIT = #43
PFCOL_DARK  = $94
PFCOL_LIGHT = $98

;############################
; Bank1
;############################

;----------------------------
; Variables
;----------------------------
    SEG.U variables
    ORG $80

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
    lda #%00011111
    sta PFData0
    lda #%00000000
    sta PFData1
    sta PFData2
    sta PFData3
    sta PFData4
    lda #%10000000
    sta PFData5

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
    ldy #191
ScanLoop:
    ; WSYNC is placed BEFORE all of this action takes place.
    sta WSYNC

    ;-----------
    ; Playfield
    ;-----------
  

    ; every 4th ScanLine
    tya
    and #%00000011
    bne PFDone
ScanLineGEQ0
    ; top 16 scanlines: -> PF0
    cpy #191-15
    bcc ScanLineGEQ16
    beq ScanLineGEQ16
    ; calc PF0
    lda PFData0
    asl 
    ora #1
    sta PFData0
    ; calc upper nibble of PF5
    jmp PFDone
ScanLineGEQ16:
    ; next 32 scanlines -> PF1
    cpy #191-47
    bcc ScanLineGEQ48
    beq ScanLineGEQ48
    lda PFData1
    lsr 
    ora #%10000000
    sta PFData1
    jmp PFDone
ScanLineGEQ48:
PFDone:

    lda PFData0
    sta PF0
    lda PFData1
    sta PF1
    lda PFData2
    sta PF2

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
    rts

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


