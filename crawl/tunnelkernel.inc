    ; Y will be our scanline counter
    ; --- ##########################
    ; 16 Scanlines of Section0Top
Section0Top: SUBROUTINE
    ldy #15
.lineLoop
    sta WSYNC
    ; bg color
    lda Vb_BGColEven
    sta COLUBK
    lda (Vptr_Sec0L),y   ; +5
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
    lda (Vptr_Sec0R),y
    and #%11110000
    sta PF2
    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section1Top
Section1Top: SUBROUTINE
    ldy #15
.lineLoop
    ; prepare Vb_DrawDist comparison for later
    ldx #1                  ; +2
    ; prepare PF0 value
    lda #%11110000

    sta WSYNC

    sta PF0
    ; set up PF1
    lda (Vptr_Sec1L),y      ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 

    ; use PF0 and first half of PF1 to set 
    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColOdd

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF2 to finish drawing
    SLEEP 8

    ; A still is #0
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Vptr_Sec1R),y
    ora #%11110000
    sta PF2

    dey
    bpl .lineLoop

    ; --- ##########################
    ; 16 Scanlines of Section2Top
Section2Top: SUBROUTINE
    ldy #15
.lineLoop
    ; prepare Vb_DrawDist comparison for later
    ldx #2                  ; +2
    lda #%11110000
    sta WSYNC

    sta PF0
    lda (Vptr_Sec2L),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColEven

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Vptr_Sec2R),y
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
    ; prepare Vb_DrawDist comparison for later
    ldx #3                  ; +2
    lda #%11111111
    ; out of cycles - need to strobe WSYNC
    sta WSYNC
    ; PF0, Pf1 are solid
    sta PF0
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColOdd
    
    lda (Vptr_Sec3L),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Vptr_Sec3R),y
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
    ; load player gfx
    ldx SKELETON_P0,y
    stx GRP0
    ; test if far wall should be solid or BG
    lda #4              ; +2
    ; we only used 5 cycles, but WSYNC takes 3...
    sta WSYNC
    ; now we have ~22 cycles to set bgcol and PF0 
    cmp Vb_DrawDist    ; +3 (5)
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
    sta RESP0
    SLEEP 12

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
    ; prepare Vb_DrawDist comparison for later
    ldx #3                  ; +2
    lda #%11111111
    ; plenty of cycles left, end line - need to strobe WSYNC
    sta WSYNC
    sta PF0
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColOdd

    lda (Vptr_Sec3L),y   ; +5
    and #%00001111          ; +2
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Vptr_Sec3R),y
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
    ; prepare Vb_DrawDist comparison for later
    ldx #2                  ; +2
    lda #%11110000
    sta WSYNC

    sta PF0
    lda (Vptr_Sec2L),y   ; +5
    ora #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColEven

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    lda (Vptr_Sec2R),y
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
    ; prepare Vb_DrawDist comparison for later
    ldx #1                  ; +2
    lda #%11110000
    sta WSYNC
    sta PF0
    lda (Vptr_Sec1L),y   ; +5
    and #%11110000          ; +2
    sta PF1                 ; +3 

    ; bg color: even/odd or playfield
    M_CullBG Vb_BGColOdd

    lda #0                  ; +2    
    sta PF2                 ; +3 (18) 

    ; wait for PF1 to finish drawing
    SLEEP 4 

    lda #0                  ;    
    sta PF0                 ; +3 (8)
    sta PF1                 ; +3 (8)
    lda (Vptr_Sec1R),y
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
    lda Vb_BGColEven
    sta COLUBK
    lda (Vptr_Sec0L),y   ; +5
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
    lda (Vptr_Sec0R),y
    and #%11110000
    sta PF2

    iny
    cpy #16
    bne .lineLoop


