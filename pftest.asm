; playfield test

    processor 6502

    include "vcs.h"
    include "macro.h"

;----------------------------
; Constants
;----------------------------
VBLANK_WAIT = #43

;############################
; Bank1
;############################
    ORG $F000

;----------------------------
; Variables
;----------------------------
Temp       = $80
PlayfieldY = $90

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
    lda #%00000001
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
    lda #44
    sta TIM64T
    ; clear collision latches
    lda #0
    sta CXCLR
    ; end vsync period
    sta WSYNC 
    sta VSYNC
    ; wait until vertical blank period is over
    ; move to VerticalBlank?
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

    ; actual draw code
    lda #2
    sta CTRLPF

    ldy #191
ScanLoop:
    ; WSYNC is placed BEFORE all of this action takes place.
    STA WSYNC
    lda #$0e
    sta COLUPF
    sta COLUP0
    sta COLUP1
    lda #$ff
    sta PF0
    sta PF1
    sta PF2
    DEY
    BNE ScanLoop    

    ; clear registers to prevent bleeding
    lda #2
    sta WSYNC   ; finish scanline
    sta VBLANK  ; make TIA output blank
    ; re-use Y which is still 0
    sty PF0
    sty PF1
    sty PF1  ; ? PF2?
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
    ORG $FF00
    ;include "pftest.inc"
PFData0  ;H       4 5 6 7
       .byte $00,$f0,$00,$A0,$A0,$E0,$A0,$A0
PFData1  ;EL      7 6 5 4 3 2 1 0
       .byte $00,$FF,$00,$77,$44,$64,$44,$74
PFData2  ;LO      0 1 2 3 4 5 6 7
       .byte $00,$FF,$00,$EE,$A2,$A2,$A2,$E2
PFLColor ; Left side of screen
       .byte $00,$FF,$00,$22,$26,$2A,$2C,$2E
PFRColor ; Right side of screen
       .byte $00,$1F,$00,$6E,$6C,$6A,$66,$62


;----------------------------
; Reset/Break 
;----------------------------
    ORG $FFFC
    ; set Reset pointer (at $FFFC and $FFFD) to Reset label 
    .word Reset
    ; set BRK pointer (at $FFFE and $FFFF) to Reset label
    .word Reset


