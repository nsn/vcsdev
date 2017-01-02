; '2600 for Newbies
; Session 13 - Playfield

    processor 6502

    include "vcs.h"
    include "macro.h"

;------------------------------------------------------------------------------
;--- DEFINES

COL_BLACK               = #$00
COL_WHITE               = #$0E

;------------------------------------------------------------------------------


  SEG
  ORG $F000

Reset

;------------------------------------------------
; Clear RAM and all TIA registers

  ldx #0 
  lda #0

Clear 
  sta 0,x 
  inx 
  bne Clear

;------------------------------------------------
; Once-only initialization...

  lda COL_BLACK
  sta COLUBK             ; set the background color

  lda COL_WHITE
  sta COLUPF             ; set playfield color


  lda #0
  ldy #0                 
  ldx #0                 

;------------------------------------------------

StartOfFrame

; Start of new frame
; Start of vertical blank processing


  lda #0
  sta VBLANK

  lda #2
  sta VSYNC

  sta WSYNC
  sta WSYNC
  sta WSYNC               ; 3 scanlines of VSYNC signal

  lda #0
  sta VSYNC           

;------------------------------------------------
; 37 scanlines of vertical blank...

  ldx #0

VerticalBlank 
  
  sta WSYNC
  inx
  cpx #37
  bne VerticalBlank

;------------------------------------------------
; end of vertical blank


;------------------------------------------------
; Do 192 scanlines of color-changing (our picture)

  ldx #0                 ; this counts our scanline number

Picture
 

  sta WSYNC              ; wait till end of scanline

  inx
  cpx #192
  bne Picture

;------------------------------------------------

  lda #%01000010
  sta VBLANK          ; end of screen - enter blanking

; 30 scanlines of overscan...

  ldx #0

Overscan        
  sta WSYNC
  inx
  cpx #30
  bne Overscan

  jmp StartOfFrame

;------------------------------------------------------------------------------

  ORG $FFFA

InterruptVectors

  .word Reset          ; NMI
  .word Reset          ; RESET
  .word Reset          ; IRQ

  END
