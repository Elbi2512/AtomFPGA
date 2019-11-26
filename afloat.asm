; da65 V2.13.3 - (C) Copyright 2000-2009,  Ullrich von Bassewitz
; Created:    2019-11-04 08:10:39
; Input file: afloat.rom
; Page:       1


        .setcpu "6502"

L0052           := $0052
L0600           := $0600
L474F           := $474F
L554F           := $554F
L6618           := $6618
L7F52           := $7F52
L9E9A           := $9E9A
LA002           := $A002
LAED3           := $AED3
LC231           := $C231
LC278   		:= $C278 
LC279           := $C279
LC372           := $C372
LC3C8           := $C3C8
LC3CB           := $C3CB
LC3CD           := $C3CD
LC434           := $C434
LC4D9           := $C4D9
LC4E1           := $C4E1
LC4E4           := $C4E4
LC558           := $C558
LC54A			:= $C54A
LC55B           := $C55B
LC569           := $C569
LC78B           := $C78B
LC8BC           := $C8BC
LC99F           := $C99F
LCA1B           := $CA1B
LCA4C           := $CA4C
LCCD5           := $CCD5
LCD09           := $CD09
LCEB1           := $CEB1
LCF3E           := $CF3E
LCF41           := $CF41
LCFED           := $CFED
LE005           := $E005
LE0D2           := $E0D2
LED6C           := $ED6C
LEE7C   :=     $EE7C 
LFFD1           := $FFD1
LFFD4           := $FFD4
 .byte 		$AA 
 .byte 		$55 
 .byte 		$0E, $d1 
 .byte 		$5e, $d1 
XD006: 
 .byte '(' 
 .byte INIT1/256 
 .byte INIT1 .MOD 256 
 .byte    "ACS" 
 .byte ACS/256 
 .byte ACS .MOD 256 
 .byte    "ASN" 
 .byte ASN/256 
 .byte ASN .MOD 256 
 .byte    "ATN" 
 .byte ATN/256 
 .byte ATN .MOD 256 
 .byte    "ABS" 
 .byte ABS/256 
 .byte ABS .MOD 256 
 .byte    "COS" 
 .byte COS/256 
 .byte COS .MOD 256 
        ;.byte     "FPINIT" 
        ;DFB     FPINIT/256 
        ;DFB     FPINIT .MOD 256 
 .byte    "EXP" 
 .byte EXP/256 
 .byte EXP .MOD 256 
 .byte    "HTN" 
 .byte HTN/256 
 .byte HTN .MOD 256 
 .byte    "LOG" 
 .byte LOG/256 
 .byte LOG .MOD 256 
 .byte    "PI" 
 .byte PI/256 
 .byte PI .MOD 256 
 .byte    "SIN" 
 .byte SIN/256 
 .byte SIN .MOD 256 
 .byte "SQR" 
 .byte SQR/256 
 .byte SQR .MOD 256 
 .byte    "TAN" 
 .byte TAN/256 
 .byte TAN .MOD 256 
 .byte    "DEG" 
 .byte DEG/256 
 .byte DEG .MOD 256 
 .byte    "RAD" 
 .byte RAD/256 
 .byte RAD .MOD 256 
 .byte "SGN" 
  .byte SGN/256 
 .byte SGN .MOD 256 
 .byte    "VAL" 
 .byte VAL/256 
 .byte VAL .MOD 256 
 .byte "FLT" 
 .byte FLT/256 
 .byte FLT .MOD 256 
 .byte    "FGET" 
 .byte FGET/256 
 .byte FGET .MOD 256 
 .byte PER1/256 
 .byte PER1 .MOD 256 
 .byte    "%" 
 .byte VAR/256 
 .byte VAR .MOD 256 
 .byte "FIF" 
 .byte FIF/256 
 .byte FIF .MOD 256 
 .byte    "FUNTIL" 
 .byte FUNTIL/256 
 .byte FUNTIL .MOD 256 
 .byte "COLOUR" 
 .byte LEE7C/256 
 .byte LEE7C .MOD 256 
 .byte "FDIM" 
 .byte FDIM/256 
 .byte FDIM .MOD 256 
 .byte "STR" 
 .byte STR/256 
 .byte STR .MOD 256 
 .byte "FPRINT" 
 .byte FPRINT/256 
 .byte FPRINT .MOD 256 
 .byte "FINPUT" 
 .byte FINPUT/256 
 .byte FINPUT .MOD 256 
 .byte "FPUT" 
 .byte FPUT/256 
 .byte FPUT .MOD 256 
 .byte OTHERROM/256 
 .byte OTHERROM .MOD 256 
 .byte "+" 
 .byte FP_PLS/256 
 .byte FP_PLS .MOD 256 
 .byte "-" 
 .byte FP_MIN/256 
 .byte FP_MIN .MOD 256 
 .byte $FE 
 .byte "*" 
 .byte FP_MAAL/256 
 .byte FP_MAAL .MOD 256 
 .byte "/" 
 .byte FP_DIV/256 
 .byte FP_DIV .MOD 256 
 .byte $FE 
 .byte "^" 
 .byte FP_MACH/256 
 .byte FP_MACH .MOD 256 
 .byte $FE 
 .byte "+" 
 .byte FPL_pl/256 
 .byte FPL_pl .MOD 256 
 .byte "-" 
 .byte FPL_min/256 
 .byte FPL_min .MOD 256 
 .byte FPL_pl/256 
 .byte FPL_pl .MOD 256 
 .byte ")" 
 .byte LC278/256 
 .byte LC278 .MOD 256 
 .byte $FF 
 .byte ';', LC54A/256, LC54A .MOD 256 
 .byte $0d, LC54A/256, LC54A .MOD 256 
 .byte ',', FPRINT/256, FPRINT .MOD 256 
 .byte LD339/256, LD339 .MOD 256 
 .byte ',', FINPUT/256, FINPUT .MOD 256, LC558/256, LC558 .MOD 256 
 .byte '=', EQUAL/256, EQUAL .MOD 256       
.byte "<>", NEQUAL/256, NEQUAL .MOD 256    
.byte "<=", LEQUAL/256, LEQUAL .MOD 256     
.byte '<', LT/256, LT .MOD 256             
.byte ">=", GEQUAL / 256, GEQUAL .MOD 256  
 .byte '>', GT/256, GT .MOD 256, $FF        
; 	
INIT1: JSR LD0FC 
		LDX #$B4 
		BNE LD10B 
BEP_ARG: 
		clc 
LD0EC: 
		ror $73 
		ldx     #$AC 
		bne     LD10B 
LD0F2: 	jsr LD85D 
LD0F5: 	jsr LD106  ; we doen wat recursie 
LD0F8: 	ldx #$A1 
		bne     LD10B 	
		;
LD0FC:  jsr     LD0F5
LD0FF:  ldx     #$9A
        bne     LD10B
LD103:  jsr     LD85D
LD106:  jsr     BEP_ARG
LD109:  ldx     #$A8
LD10B:  clc
        bcc     LD113
        ldx     #$5F
        sty     $03
LD112:  sec
LD113:  ror     $53
        ldy     $03
        dey
LD118:  iny
        lda     ($05),y
        cmp     #$20
        beq     LD118
        dey
        sty     L0052
        dex
LD123:  ldy     L0052
LD125:  inx
        iny
LD127:  lda     $D006,x
LD12A:  .byte   $30
LD12B:  .byte   $1A
        cmp     ($05),y
        beq     LD125
        dex
LD131:  inx
        lda     $D006,x
        bpl     LD131
        inx
        bit     $53
        bpl     LD123
        lda     ($05),y
        cmp     #$2E
        bne     LD123
        iny
        dex
        bcs     LD127
        cmp     #$FE
        bcs     LD15B
        sta     $53
        lda     $D007,x
        sta     L0052
        sty     $03
        ldx     $04
        jmp     (L0052)
LD158:  ldx     $04
        rts
LD15B:  beq     LD158
        brk
LD15E:  sty     $03
        jsr     LD0EC
        jsr     LD89A
        lda     $5A
        sta     $60
        lda     $5B
        sta     $5F
        lda     $5C
        sta     $5E
        ldy     #$5D
        jmp     LC99F
        jsr     LD0F2
        jsr     LD870
        jsr     LD93C
        jmp     LD0FF
        jsr     LD0F2
        jsr     LD870
        jsr     LD939
        jmp     LD0FF
        jsr     LD103
        jsr     LD870
        jsr     LDA45
        jmp     LD0F8
        jsr     LD103
        jsr     LD870
        jsr     LDAB6
        jmp     LD0F8
        jsr     LDBB6
        jsr     LD85D
        jsr     BEP_ARG
        jsr     LD870
        jsr     LDA45
        jsr     LDDD7
        jmp     LD109
        jsr     LD1CB
LD1BF:  jsr     LD686
        beq     LD1CA
LD1C4:  lda     $57
        eor     #$80
        sta     $57
LD1CA:  rts
LD1CB:  ldy     $03
        dey
LD1CE:  iny
        lda     ($05),y
        cmp     #$20
        beq     LD1CE
        cmp     #$25
        bne     LD20B
        inc     $03
        jsr     LD444
        bcc     LD20B
        ldy     #$6F
        jsr     LC3CD
LD1E5:  ldy     #$04
        lda     #$00
        sta     $5E
        sta     $58
        sta     $57
LD1EF:  lda     ($6F),y
        sta     $59,y
        ora     $57
        sta     $57
        dey
        bpl     LD1EF
        tax
        beq     LD207
        lda     $5A
        sta     $57
        ora     #$80
        sta     $5A
        txa
LD207:  rts
LD208:  sty     $03
        rts
LD20B:  jsr     LD5A5
        bcs     LD208
        ldx     #$00
        jmp     LD112
        jsr     BEP_ARG
        jsr     LD686
        bmi     LD1C4
        rts
        jsr     LD224
        jmp     LDC86
LD224:  jsr     BEP_ARG
        jsr     LD686
        bpl     LD236
        lda     #$00
        sta     $57
        jsr     LD236
        jmp     LDC75
LD236:  jsr     LD82C
        jsr     LDA45
        jsr     LD831
        jsr     LDE8D
        jsr     LD933
        jsr     LDB72
        jsr     LD686
        beq     LD25F
        jsr     LD831
        jsr     LDBA6
        jsr     LD1E5
        jsr     LDBAA
        jsr     LDADC
        jmp     LDC67
LD25F:  jsr     LDD93
        jmp     LD1E5
        jsr     BEP_ARG
        ldy     #$7C
        lda     #$D2
LD26C:  sty     $6F
        sta     $70
        jmp     LDA45
        jsr     BEP_ARG
        ldy     #$81
        lda     #$D2
        bne     LD26C
        .byte   $7B
        asl     $35FA
        .byte   $12
        stx     $65
        rol     LD3E0
        jsr     BEP_ARG
        jsr     LD686
        beq     LD295
        pha
        jsr     LDE8D
        pla
        sta     $57
LD295:  rts
        bit     $73
        bmi     LD2C0
        jsr     LC8BC
        ldy     #$5D
        jsr     LC3CD
        sta     $5A
        lda     $5F
        sta     $5B
        lda     $5E
        sta     $5C
LD2AC:  lda     #$A0
        sta     $59
        ldy     #$00
        sty     $5E
        lda     $5A
        sta     $57
        bpl     LD2BD
        jsr     LD8D5
LD2BD:  jmp     LD7C8
LD2C0:  jmp     LCA1B
        jsr     LDD93
        jsr     LD1E5
        inc     $59
        rts
        jsr     LCF3E
        ldx     #$04
LD2D1:  jsr     LFFD4
        sta     $03C5,x
        dex
        bpl     LD2D1
LD2DA:  jsr     LDBAA
        jmp     LD1E5
        jsr     LCEB1
        .byte   $A0
LD2E4:  brk
        jsr     LD304
        cmp     #$2B
        beq     LD2FB
        cmp     #$2D
        bne     LD2FE
        jsr     LD303
        sty     $54
        jsr     LD5B1
        jmp     LD1BF
LD2FB:  jsr     LD303
LD2FE:  sty     $54
        .byte   $4C
        .byte   $B1
LD302:  .byte   $D5
LD303:  iny
LD304:  lda     (L0052),y
        cmp     #$20
        beq     LD303
        rts
        jsr     LD444
        bcc     LD2E4
        jsr     LC279
        jsr     LD0FC
        jsr     LC4E4
        jsr     LD836
        jmp     LC55B
        jsr     LD0FC
        jsr     LC231
        jsr     LC4E1
        jsr     LC3CB
        .byte   $20
LD32C:  bne     LD302
        jmp     LC55B
LD331:  jsr     LC372
        ldx     #$B8
        jmp     LD10B
LD339:  jsr     LD0FC
        lda     #$C5
        sta     L0052
        lda     #$03
        sta     $53
        jsr     LD4D0
        dec     $6F
        lda     $0321
        sec
        sbc     $6F
        bcc     LD35C
        beq     LD35C
        tay
        lda     #$20
LD356:  jsr     LCA4C
        dey
        bne     LD356
LD35C:  ldy     #$00
LD35E:  lda     (L0052),y
        cmp     #$0D
        beq     LD331
        jsr     LCA4C
        iny
        bne     LD35E
        jsr     LC372
        lda     ($05),y
        cmp     #$25
        bne     LD37B
        iny
        sty     $03
        jsr     LD444
        bcs     LD380
LD37B:  ldx     #$C3
        jmp     LD10B
LD380:  jsr     LCD09
        tay
        lda     $05
        pha
        lda     $06
        pha
        lda     $03
        pha
        sty     $03
        iny
        sty     $06
        lda     #$40
        sta     $05
        jsr     LD0FC
        pla
        sta     $03
        pla
        sta     $06
        pla
        sta     $05
        jsr     LD836
        jmp     LD37B
        jsr     LD9EB
        jmp     LC569
        jsr     LD9EB
        jmp     LCCD5
        jsr     LD494
        jsr     LD0FC
        jsr     LC4E4
        jsr     LD831
        ldx     $04
        jsr     LCF41
        ldx     #$04
LD3C7:  lda     $03C5,x
        jsr     LFFD1
        dex
        bpl     LD3C7
        jmp     LC55B
LD3D3:  lda     $01
        ora     $02
        beq     LD443
        jsr     LC434
        bcs     LD443
        ldy     $03
LD3E0:  lda     ($05),y
        cmp     #$25
        bne     LD443
        iny
        lda     ($05),y
        iny
        cmp     ($05),y
        bne     LD443
        cmp     #$5B
        bcs     LD443
        sbc     #$3F
        bcc     LD443
        iny
        sty     $03
        pha
        jsr     LC78B
        inc     $15,x
        bne     LD403
        inc     $24,x
LD403:  jsr     LD49A
        pla
        tay
        clc
        lda     $23
        sta     $2887,y
        adc     $16
        sta     $23
        lda     $24
        sta     $28A2,y
        adc     $25
        sta     $24
        ldy     #$00
        sty     $04
        lda     #$AA
        sta     ($23),y
        cmp     ($23),y
        bne     LD443
        lsr     a
        sta     ($23),y
        cmp     ($23),y
        bne     LD443
        jsr     LC434
        bcs     LD443
        ldy     $03
        lda     ($05),y
        cmp     #$2C
        bne     LD440
        inc     $03
        jmp     LD3D3
LD440:  jmp     LC558
LD443:  brk
LD444:  jsr     LC434
        bcc     LD457
        lda     $15,x
        asl     a
        asl     a
        adc     $15,x
        sta     $15,x
        lda     #$28
        sta     $24,x
        sec
        rts
LD457:  ldy     $03
        lda     ($05),y
        cmp     #$21
        bne     LD466
        inc     $03
        jsr     LC8BC
LD464:  sec
        rts
LD466:  iny
        cmp     ($05),y
        bne     LD473
        cmp     #$5B
        bcs     LD473
        sbc     #$3F
        bcs     LD475
LD473:  clc
        rts
LD475:  iny
        sty     $03
        pha
        jsr     LC8BC
        jsr     LD49A
        pla
        tay
        bcs     LD493
        lda     $2887,y
        adc     $15,x
        sta     $15,x
        lda     $28A2,y
        adc     $24,x
        sta     $24,x
        bcc     LD464
LD493:  brk
LD494:  jsr     LC8BC
        jmp     LC231
LD49A:  ldy     $24,x
        lda     $15,x
        asl     a
        rol     $24,x
        asl     a
        rol     $24,x
        clc
        adc     $15,x
        sta     $15,x
        tya
        adc     $24,x
        sta     $24,x
        rts
        lda     $E004
        cmp     #$BF
        beq     LD4C0
        lda     $A000
        cmp     #$40
        bne     LD440
        jmp     LA002
LD4C0:  jmp     LE005
LD4C3:  cmp     #$3A
        bcs     LD4CE
        cmp     #$30
        bcc     LD4CD
        sbc     #$30
LD4CD:  rts
LD4CE:  clc
        rts
LD4D0:  lda     #$00
        sta     $6F
        jsr     LD686
        bne     LD4EB
        lda     #$30
        jsr     LD58D
        lda     #$2E
        jsr     LD58D
        lda     #$30
        jsr     LD58D
        jmp     LD571
LD4EB:  bpl     LD4F2
        lda     #$2D
        jsr     LD58D
LD4F2:  lda     #$00
        sta     $6D
LD4F6:  lda     $59
        cmp     #$81
        bcs     LD504
        jsr     LD6A0
        dec     $6D
        jmp     LD4F6
LD504:  cmp     #$84
        bcc     LD518
        bne     LD510
        lda     $5A
        cmp     #$A0
        bcc     LD518
LD510:  jsr     LD71B
        inc     $6D
        jmp     LD4F6
LD518:  lda     $59
        cmp     #$84
        bcs     LD525
        jsr     LD6D8
        inc     $59
        bne     LD518
LD525:  sec
        lda     #$FF
        jsr     LD636
        lda     $5A
        cmp     #$A0
        bcs     LD510
        lda     #$01
        ldy     $6D
        bmi     LD541
        cpy     #$08
        bcs     LD541
        iny
        lda     #$00
        sta     $6D
        tya
LD541:  sta     $70
        ldx     #$09
        stx     $54
LD547:  jsr     LD575
        dec     $70
        bne     LD553
        lda     #$2E
        jsr     LD58D
LD553:  dec     $54
        bne     LD547
        lda     $6D
        beq     LD571
        lda     #$45
        jsr     LD58D
        lda     $6D
        bpl     LD56E
        lda     #$2D
        jsr     LD58D
        sec
        lda     #$00
        sbc     $6D
LD56E:  jsr     LD587
LD571:  lda     #$0D
        bne     LD58D
LD575:  lda     $5A
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        jsr     LD58B
        lda     $5A
        and     #$0F
        sta     $5A
        jmp     LD64E
LD587:  cmp     #$0A
        bcs     LD594
LD58B:  ora     #$30
LD58D:  ldy     $6F
        sta     (L0052),y
        inc     $6F
        rts
LD594:  ldx     #$FF
LD596:  inx
        sbc     #$0A
        bcs     LD596
        adc     #$0A
        pha
        txa
        jsr     LD587
        pla
        bpl     LD58B
LD5A5:  lda     $03
        sta     $54
        lda     $05
        sta     L0052
        lda     $06
        sta     $53
LD5B1:  jsr     LDAA4
        sta     $6C
        sta     $6D
        jsr     LD67B
        cmp     #$2E
        beq     LD5CD
        jsr     LD4C3
        bcc     LD635
        sta     $5E
LD5C6:  jsr     LD67B
        cmp     #$2E
        bne     LD5D6
LD5CD:  lda     $6C
        clc
        bne     LD60C
        inc     $6C
        bne     LD5C6
LD5D6:  cmp     #$45
        beq     LD601
        jsr     LD4C3
        bcc     LD60C
        sta     $6E
        lda     $5A
        cmp     #$18
        bcc     LD5EF
        lda     $6C
        bne     LD5C6
        inc     $6D
        bcs     LD5C6
LD5EF:  lda     $6C
        beq     LD5F5
        dec     $6D
LD5F5:  jsr     LD64E
        clc
        lda     $6E
        jsr     LD636
        jmp     LD5C6
LD601:  jsr     LD67B
        jsr     LD778
        clc
        adc     $6D
        sta     $6D
LD60C:  lda     #$A8
        sta     $59
        jsr     LD686
        beq     LD631
        jsr     LD7C8
        lda     $6D
        bmi     LD627
        beq     LD62E
LD61E:  jsr     LD6A0
        dec     $6D
        bne     LD61E
        beq     LD62E
LD627:  jsr     LD71B
        inc     $6D
        bne     LD627
LD62E:  jsr     LDA9B
LD631:  sec
        ldy     $54
        dey
LD635:  rts
LD636:  ldx     #$05
LD638:  adc     $59,x
        sta     $59,x
        lda     #$00
        dex
        bne     LD638
        rts
LD642:  ldx     #$05
LD644:  lda     $59,x
        adc     $61,x
        sta     $59,x
        dex
        bne     LD644
        rts
LD64E:  ldx     #$05
        lda     #$00
        sta     $67
LD654:  lda     #$00
        sta     $68
        lda     $59,x
        asl     a
        rol     $68
        asl     a
        rol     $68
        clc
        adc     $59,x
        bcc     LD667
        inc     $68
LD667:  asl     a
        rol     $68
        clc
        adc     $67
        bcc     LD671
        inc     $68
LD671:  sta     $59,x
        lda     $68
        sta     $67
        dex
        bne     LD654
        rts
LD67B:  sty     $55
        ldy     $54
        lda     (L0052),y
        ldy     $55
        inc     $54
        rts
LD686:  lda     $5A
        ora     $5B
        ora     $5C
        ora     $5D
        ora     $5E
        beq     LD699
        lda     $57
        bne     LD69F
        lda     #$01
        rts
LD699:  sta     $57
        sta     $59
        sta     $58
LD69F:  rts
LD6A0:  clc
        lda     $59
        adc     #$03
        sta     $59
        bcc     LD6AB
        inc     $58
LD6AB:  jsr     LD6C3
        jsr     LD6FB
        jsr     LD6FB
LD6B4:  jsr     LD642
LD6B7:  bcc     LD6C2
        jsr     LD6D8
        inc     $59
        bne     LD6C2
        inc     $58
LD6C2:  rts
LD6C3:  ldx     #$08
LD6C5:  lda     $56,x
        sta     $5E,x
        dex
        bne     LD6C5
        rts
LD6CD:  asl     $5E
        rol     $5D
        rol     $5C
        rol     $5B
        rol     $5A
        rts
LD6D8:  ror     $5A
        ror     $5B
        ror     $5C
        ror     $5D
        ror     $5E
        rts
        lda     $5D
        sta     $5E
        lda     $5C
        sta     $5D
        lda     $5B
        sta     $5C
        lda     $5A
        sta     $5B
        lda     #$00
        sta     $5A
        rts
LD6F8:  jsr     LD6C3
LD6FB:  lsr     $62
        ror     $63
        ror     $64
        ror     $65
        ror     $66
        rts
        lda     $65
        sta     $66
        lda     $64
        sta     $65
        lda     $63
        sta     $64
        lda     $62
        sta     $63
        lda     #$00
        sta     $62
        rts
LD71B:  sec
        lda     $59
        sbc     #$04
        sta     $59
        bcs     LD726
        dec     $58
LD726:  jsr     LD6F8
        jsr     LD6B4
        jsr     LD6F8
        jsr     LD6FB
        jsr     LD6FB
        jsr     LD6FB
        jsr     LD6B4
        lda     #$00
        sta     $62
        lda     $5A
        sta     $63
        lda     $5B
        sta     $64
        lda     $5C
        sta     $65
        lda     $5D
        sta     $66
        lda     $5E
        rol     a
        jsr     LD6B4
        lda     #$00
        sta     $62
        sta     $63
        lda     $5A
        sta     $64
        lda     $5B
        sta     $65
        lda     $5C
        sta     $66
        lda     $5D
        rol     a
        jsr     LD6B4
        lda     $5B
        rol     a
        lda     $5A
LD772:  jsr     LD636
        jmp     LD6B7
LD778:  ldy     #$FF
        cmp     #$2B
        beq     LD783
        cmp     #$2D
        bne     LD786
        iny
LD783:  jsr     LD67B
LD786:  jsr     LD4C3
        bcc     LD7AF
        tax
        jsr     LD67B
        jsr     LD4C3
        bcc     LD7A4
        sta     $6E
        jsr     LD67B
        txa
        sta     $67
        asl     a
        asl     a
        adc     $67
        asl     a
        adc     $6E
        tax
LD7A4:  tya
        bne     LD7AD
        stx     $6E
        sec
        sbc     $6E
        rts
LD7AD:  txa
        rts
LD7AF:  lda     #$00
LD7B1:  rts
LD7B2:  pha
        jsr     LDAA4
        pla
        beq     LD7B1
        bpl     LD7C2
        sta     $57
        lda     #$00
        sec
        sbc     $57
LD7C2:  sta     $5A
        lda     #$88
        sta     $59
LD7C8:  jsr     LD686
        beq     LD7B1
LD7CD:  lda     $5A
        bne     LD7F2
        lda     $5B
        sta     $5A
        lda     $5C
        sta     $5B
        lda     $5D
        sta     $5C
        lda     $5E
        sta     $5D
        lda     #$00
        sta     $5E
        sec
        lda     $59
        sbc     #$08
        sta     $59
        bcs     LD7CD
        dec     $58
        bcc     LD7CD
LD7F2:  lda     $5A
        bmi     LD7B1
        jsr     LD6CD
        lda     $59
        bne     LD7FF
        dec     $58
LD7FF:  dec     $59
        jmp     LD7F2
LD804:  ldy     #$04
        lda     #$00
        sta     $66
        sta     $60
        sta     $5F
LD80E:  lda     ($6F),y
        sta     $61,y
        ora     $5F
        sta     $5F
        dey
        bpl     LD80E
        tax
        beq     LD826
        lda     $62
        sta     $5F
        ora     #$80
        sta     $62
        txa
LD826:  rts
LD827:  jsr     LDBA2
        bne     LD83D
LD82C:  jsr     LDBA6
        bne     LD83D
LD831:  jsr     LDBAA
        bne     LD83D
LD836:  ldx     $04
        ldy     #$6F
        jsr     LC3CD
LD83D:  ldy     #$00
        lda     $59
        sta     ($6F),y
        iny
        lda     $57
        and     #$80
        sta     $57
        lda     $5A
        and     #$7F
        ora     $57
        sta     ($6F),y
LD852:  iny
        lda     $59,y
        sta     ($6F),y
        cpy     #$04
        bne     LD852
        rts
LD85D:  ldy     #$52
        sty     $6F
        lda     #$00
        sta     $70
        jsr     LD83D
        jsr     LC4D9
        lda     $56
        sta     $73,x
        rts
LD870:  ldx     $04
        jsr     LC3CB
        lda     $74,x
        sta     $56
        sty     $6F
        lda     #$00
        sta     $70
        rts
LD880:  lda     $5E
        cmp     #$80
        bcc     LD88D
        beq     LD892
        lda     #$FF
        jsr     LD772
LD88D:  lda     #$00
        sta     $5E
        rts
LD892:  lda     $5D
        ora     #$01
        sta     $5D
        bne     LD88D
LD89A:  jsr     LD8C7
        beq     LD8A5
LD89F:  lda     $59
        cmp     #$A0
        bcs     LD8B9
LD8A5:  lsr     $5A
        ror     $5B
        ror     $5C
        ror     $5D
        ror     $62
        ror     $63
        ror     $64
        ror     $65
        inc     $59
        bne     LD89F
LD8B9:  beq     LD8D1
LD8BB:  lda     #$7F
        sta     $5A
        lda     #$FF
        sta     $5B
        sta     $5C
        sta     $5D
LD8C7:  ldx     #$08
        lda     #$00
LD8CB:  sta     $5F,x
        dex
        bne     LD8CB
        rts
LD8D1:  lda     $57
        bpl     LD8E1
LD8D5:  sec
        ldx     #$04
LD8D8:  lda     #$00
        sbc     $59,x
        sta     $59,x
        dex
        bne     LD8D8
LD8E1:  rts
LD8E2:  lda     $62
        bpl     LD909
        ldx     #$04
LD8E8:  lda     #$00
        sbc     $61,x
        sta     $61,x
        dex
        bne     LD8E8
        lda     $57
        eor     #$80
        sta     $57
        bpl     LD90A
LD8F9:  inc     $5D
        bne     LD909
        inc     $5C
        bne     LD909
        inc     $5B
        bne     LD909
        inc     $5A
        beq     LD8BB
LD909:  rts
LD90A:  jsr     LD8D5
        jsr     LD8F9
        jmp     LD8D5
LD913:  ldx     #$05
LD915:  lda     $61,x
        sta     $59,x
        dex
        bne     LD915
        lda     #$80
        sta     $59
        jmp     LD7C8
LD923:  jsr     LD804
        jsr     LD83D
LD929:  ldx     #$08
LD92B:  lda     $5E,x
        sta     $56,x
        dex
        bne     LD92B
LD932:  rts
LD933:  jsr     LD939
        jmp     LD1BF
LD939:  jsr     LD1BF
LD93C:  jsr     LD804
        beq     LD932
LD941:  jsr     LD686
        beq     LD929
        lda     $59
        cmp     $61
        beq     LD972
        bcc     LD95D
        sbc     $61
        cmp     #$21
        bcs     LD932
        tax
LD955:  jsr     LD6FB
        dex
        bne     LD955
        beq     LD972
LD95D:  sec
        lda     $61
        sbc     $59
        cmp     #$21
        bcs     LD929
        tax
LD967:  clc
        jsr     LD6D8
        dex
        bne     LD967
        lda     $61
        sta     $59
LD972:  lda     $57
        eor     $5F
        bpl     LD9C1
        lda     $5A
        cmp     $62
        bne     LD999
        lda     $5B
        cmp     $63
        bne     LD999
        lda     $5C
        cmp     $64
        bne     LD999
        lda     $5D
        cmp     $65
        bne     LD999
        lda     $5E
        cmp     $66
        bne     LD999
        jmp     LDAA4
LD999:  bcs     LD9C8
        sec
        lda     $66
        sbc     $5E
        sta     $5E
        lda     $65
        sbc     $5D
        sta     $5D
        lda     $64
        sbc     $5C
        sta     $5C
        lda     $63
        sbc     $5B
        sta     $5B
        lda     $62
        sbc     $5A
        sta     $5A
        lda     $5F
        sta     $57
        jmp     LDA98
LD9C1:  clc
        jsr     LD6B4
        jmp     LDA9B
LD9C8:  sec
        lda     $5E
        sbc     $66
        sta     $5E
        lda     $5D
        sbc     $65
        sta     $5D
        lda     $5C
        sbc     $64
        sta     $5C
        lda     $5B
        sbc     $63
        sta     $5B
        lda     $5A
        sbc     $62
        sta     $5A
        jmp     LDA98
        brk
LD9EB:  jsr     LD0FC
        lda     #$C7
        pha
        ldx     #$C8
        jmp     LD10B
        lda     #$5D
        bne     LDA0C
        lda     #$66
        bne     LDA0C
        lda     #$6F
        bne     LDA0C
        lda     #$76
        bne     LDA0C
        lda     #$7D
        bne     LDA0C
        lda     #$84
LDA0C:  pha
        jsr     LD85D
        jsr     LD0FC
        jsr     LD870
        inc     $04
        jsr     LD804
        lda     $5F
        and     #$80
        sta     $5F
        ldy     #$00
        lda     $57
        and     #$80
        cmp     $5F
        bne     LDA38
        ldx     #$00
LDA2D:  lda     $61,x
        cmp     $59,x
        bne     LDA3D
        inx
        cpx     #$05
        bne     LDA2D
LDA38:  php
        ldx     $04
        plp
LDA3C:  rts
LDA3D:  ror     a
        eor     $5F
        rol     a
        lda     #$01
        bne     LDA38
LDA45:  jsr     LD686
        beq     LDA3C
        jsr     LD804
        bne     LDA52
        jmp     LDAA4
LDA52:  clc
        lda     $59
        adc     $61
        sta     $59
        bcc     LDA5D
        inc     $58
LDA5D:  sec
        lda     $59
        sbc     #$80
        sta     $59
        bcs     LDA68
        dec     $58
LDA68:  ldx     #$05
        ldy     #$00
LDA6C:  lda     $59,x
        sta     $66,x
        sty     $59,x
        dex
        bne     LDA6C
        lda     $57
        eor     $5F
        sta     $57
        ldy     #$20
        jsr     LD6FB
LDA80:  lda     $67
        bpl     LDA88
        clc
        jsr     LD642
LDA88:  jsr     LD6FB
        asl     $6B
        rol     $6A
        rol     $69
        rol     $68
        rol     $67
        dey
        bne     LDA80
LDA98:  jsr     LD7C8
LDA9B:  jsr     LD880
        lda     $58
        beq     LDAAD
        bpl     LDAA7
LDAA4:  ldx     #$08
        .byte   $A9
LDAA7:  brk
LDAA8:  sta     $56,x
        dex
        bne     LDAA8
LDAAD:  rts
LDAAE:  jsr     LD831
        jsr     LDE8D
        bne     LDADC
LDAB6:  jsr     LD686
        beq     LDAA7
        jsr     LD6C3
        jsr     LD1E5
        bne     LDAE6
        rts
        jsr     BEP_ARG
        jsr     LDB9E
        jsr     LD83D
        jsr     LDCE6
        jsr     LDB9E
        jsr     LD923
        jsr     LDCF1
        jsr     LDB9E
LDADC:  jsr     LD686
        beq     LDAAD
        jsr     LD804
        beq     LDAA7
LDAE6:  lda     $57
        eor     $5F
        sta     $57
        sec
        lda     $59
        sbc     $61
        sta     $59
        bcs     LDAF7
        dec     $58
LDAF7:  clc
        lda     $59
        adc     #$81
        sta     $59
        bcc     LDB02
        inc     $58
LDB02:  ldx     #$05
LDB04:  lda     $59,x
        sta     $66,x
        dex
        bne     LDB04
        lsr     $67
        ror     $68
        ror     $69
        ror     $6A
        ror     $6B
        jsr     LD6FB
        ldx     #$27
LDB1A:  lda     $67
        cmp     $62
        bne     LDB36
        lda     $68
        cmp     $63
        bne     LDB36
        lda     $69
        cmp     $64
        bne     LDB36
        lda     $6A
        cmp     $65
        bne     LDB36
        lda     $6B
        cmp     $66
LDB36:  bcc     LDB5C
        lda     $6B
        sbc     $66
        sta     $6B
        lda     $6A
        sbc     $65
        sta     $6A
        lda     $69
        sbc     $64
        sta     $69
        lda     $68
        sbc     $63
        sta     $68
        lda     $67
        sbc     $62
        sta     $67
        lda     $5E
        ora     #$01
        sta     $5E
LDB5C:  jsr     LD6CD
        asl     $6B
        rol     $6A
        rol     $69
        rol     $68
        rol     $67
        dex
        bne     LDB1A
        jmp     LDA98
        jsr     BEP_ARG
LDB72:  jsr     LD686
        beq     LDB9D
        bpl     LDB7A
        brk
LDB7A:  jsr     LD831
        lda     $59
        lsr     a
        adc     #$40
        sta     $59
        lda     #$05
        sta     $6E
LDB88:  jsr     LD827
        jsr     LDBAA
        jsr     LDAB6
        jsr     LDBA2
        jsr     LD93C
        dec     $59
        dec     $6E
        bne     LDB88
LDB9D:  rts
LDB9E:  lda     #$D4
        bne     LDBAC
LDBA2:  lda     #$CA
        bne     LDBAC
LDBA6:  lda     #$CF
        bne     LDBAC
LDBAA:  lda     #$C5
LDBAC:  sta     $6F
        lda     #$03
        sta     $70
        rts
        jsr     BEP_ARG
LDBB6:  jsr     LD686
        beq     LDBBD
        bpl     LDBBE
LDBBD:  brk
LDBBE:  lda     $59
        pha
        lda     #$81
        sta     $59
        jsr     LD8C7
        lda     #$C0
        sta     $62
        lda     #$81
        sta     $61
        sta     $5F
        jsr     LD941
        inc     $59
        lda     #$FE
        ldy     #$DB
        jsr     LDC27
        jsr     LD831
        pla
        sec
        sbc     #$81
        jsr     LD7B2
        lda     #$F9
        sta     $6F
        lda     #$DB
        sta     $70
        jsr     LDA45
        jsr     LDBAA
        jmp     LD93C
        .byte   $80
        and     ($72),y
        .byte   $17
        sed
        .byte   $07
        sta     $17
        ror     $85D4
        .byte   $80
        plp
        .byte   $C7
        .byte   $12
        ldy     #$84
        bvs     LDC5A
        .byte   $5F
        .byte   $F2
        sta     ($00,x)
        brk
        inc     $84EF,x
        .byte   $0F
        .byte   $FF
        .byte   $DA
        sbc     ($81,x)
        .byte   $7F
        .byte   $FF
        .byte   $FF
        .byte   $93
        .byte   $82
        rti
        brk
        brk
        .byte   $0C
        .byte   $7F
        .byte   $4F
        sta     $651F,y
LDC27:  sta     $71
        sty     $72
        jsr     LD831
        ldy     #$00
        lda     ($71),y
        sta     $6C
        inc     $71
        bne     LDC3A
        inc     $72
LDC3A:  lda     $71
        sta     $6F
        lda     $72
        sta     $70
        jsr     LD1E5
LDC45:  jsr     LDBAA
        jsr     LDAB6
        clc
        lda     $71
        adc     #$05
        sta     $71
        sta     $6F
        lda     $72
        adc     #$00
        sta     $72
LDC5A:  sta     $70
        jsr     LD93C
        dec     $6C
        bne     LDC45
        rts
        jsr     BEP_ARG
LDC67:  jsr     LD686
        beq     LDC79
        bpl     LDC7A
        lda     #$00
        sta     $57
        jsr     LDC7A
LDC75:  lda     #$80
        sta     $57
LDC79:  rts
LDC7A:  lda     $59
        cmp     #$81
        bcc     LDC8C
        jsr     LDAAE
        jsr     LDC8C
LDC86:  jsr     LDD93
        jmp     LD939
LDC8C:  lda     $59
        cmp     #$73
        bcc     LDC79
        jsr     LD82C
        jsr     LD8C7
        lda     #$80
        sta     $61
        sta     $62
        sta     $5F
        jsr     LD941
        lda     #$B0
        ldy     #$DC
        jsr     LDC27
        jsr     LDBA6
        jmp     LDA45
        ora     #$85
        .byte   $A3
        eor     $67E8,y
        .byte   $80
        .byte   $1C
        sta     $3607,x
        .byte   $80
        .byte   $57
        .byte   $BB
        sei
        .byte   $DF
        .byte   $80
        dex
        txs
        asl     $8483
        sty     $CABB
        ror     $9581
        stx     $06,y
        dec     $0A81,x
        .byte   $C7
        jmp     (L7F52)
        adc     $90AD,x
        lda     ($82,x)
        .byte   $FB
        .byte   $62
        .byte   $57
        .byte   $2F
        .byte   $80
        adc     $3863
        bit     $EB20
        .byte   $D0
LDCE6:  jsr     LDD24
        inc     $6E
        jmp     LDCF4
        jsr     BEP_ARG
LDCF1:  jsr     LDD24
LDCF4:  lsr     $6E
        bcc     LDCFB
        jsr     LDC86
LDCFB:  lsr     $6E
        bcc     LDD05
        jsr     LDD05
        jmp     LD1BF
LDD05:  jsr     LD82C
        jsr     LDD93
        jsr     LD804
        dec     $61
        lda     #$80
        sta     $5F
        jsr     LD941
        lda     #$A6
        ldy     #$DD
        jsr     LDC27
        jsr     LDBA6
        jmp     LDA45
LDD24:  lda     $59
        cmp     #$98
        bcs     LDD7E
        jsr     LD831
        jsr     LDD93
        jsr     LDADC
        jsr     LD89A
        lda     $5D
        sta     $6E
        ora     $5C
        ora     $5B
        ora     $5A
        beq     LDD80
        jsr     LD2AC
        jsr     LD827
        jsr     LDD86
        jsr     LDA45
        jsr     LDBAA
        jsr     LD93C
        jsr     LD83D
        jsr     LDBA2
        jsr     LD1E5
        jsr     LDD8A
        jsr     LDA45
        jsr     LDBAA
        jsr     LD93C
LDD69:  lda     $57
        bpl     LDD7F
        jsr     LDD8A
        jsr     LD933
        jsr     LDD86
        jsr     LD933
        dec     $6E
        jmp     LDD69
LDD7E:  brk
LDD7F:  rts
LDD80:  jsr     LD2DA
        jmp     LDD69
LDD86:  lda     #$97
        bne     LDD8C
LDD8A:  lda     #$9C
LDD8C:  sta     $6F
        lda     #$DD
        sta     $70
        rts
LDD93:  lda     #$A1
        bne     LDD8C
        sta     ($C9,x)
        brk
        brk
        brk
        adc     $FD,x
        tax
        .byte   $22
        .byte   $17
        sta     ($49,x)
        .byte   $0F
        .byte   $DA
        ldx     #$08
        sty     $04
        .byte   $C7
        .byte   $3C
        .byte   $FB
        sta     ($E0,x)
        .byte   $4F
        eor     $82AD,x
        .byte   $80
        brk
        adc     #$B8
        .byte   $82
        .byte   $5B
        .byte   $CF
        ora     $82B5,x
        .byte   $BF
        dec     $1E82
        .byte   $82
        eor     $44
        .byte   $7F
        .byte   $32
        .byte   $7F
        .byte   $62
        .byte   $44
        .byte   $5A
        .byte   $D2
        .byte   $83
        .byte   $82
        .byte   $14
        txa
        .byte   $27
        .byte   $80
        ror     $7B
        and     ($4D,x)
        jsr     BEP_ARG
LDDD7:  lda     $59
        cmp     #$87
        bcc     LDDED
        bne     LDDE5
        lda     $5A
        cmp     #$B3
        bcc     LDDED
LDDE5:  lda     $57
        bpl     LDDEC
        jmp     LDAA4
LDDEC:  brk
LDDED:  lda     $59
        cmp     #$80
        bcc     LDE1C
        jsr     LD89A
        jsr     LD8E2
        lda     $5D
        sta     $6E
        jsr     LD913
        jsr     LDE1C
        jsr     LD82C
        lda     #$23
        sta     $6F
        lda     #$DE
        sta     $70
        jsr     LD1E5
        lda     $6E
        jsr     LDE51
        jsr     LDBA6
        jmp     LDA45
LDE1C:  lda     #$28
        ldy     #$DE
        jmp     LDC27
        .byte   $82
        and     $54F8
        cli
        .byte   $07
        .byte   $83
        cpx     #$20
        stx     $5B
        .byte   $82
        .byte   $80
        .byte   $53
        .byte   $93
        clv
        .byte   $83
        jsr     L0600
        lda     ($82,x)
        brk
        brk
        and     ($63,x)
        .byte   $82
        cpy     #$00
        brk
        .byte   $02
        .byte   $82
        .byte   $80
        brk
        brk
        .byte   $0C
        sta     ($00,x)
        brk
        brk
        brk
        sta     ($00,x)
        brk
        brk
        brk
LDE51:  tax
        bpl     LDE5D
        dex
        txa
        eor     #$FF
        pha
        jsr     LDAAE
        pla
LDE5D:  pha
        jsr     LD831
        jsr     LDE8D
LDE64:  pla
        beq     LDE71
        sec
        sbc     #$01
        pha
        jsr     LDA45
        jmp     LDE64
LDE71:  rts
        jsr     BEP_ARG
        lda     $57
        bpl     LDE83
        lda     #$00
        sta     $57
        jsr     LDE83
        jmp     LD1BF
LDE83:  lda     $59
        cmp     #$81
        bcc     LDEBC
        cmp     #$85
        bcc     LDE98
LDE8D:  jsr     LDAA4
        ldy     #$80
        sty     $5A
        iny
        sty     $59
        rts
LDE98:  inc     $59
        lda     #$80
        sta     $57
        jsr     LDDD7
        jsr     LD831
        jsr     LDE8D
        jsr     LD93C
        jsr     LD827
        jsr     LDE8D
        jsr     LDBAA
        jsr     LD933
        jsr     LDBA2
        jmp     LDADC
LDEBC:  jsr     LD82C
        jsr     LDE8D
        dec     $59
        jsr     LD933
        lda     #$D4
        ldy     #$DE
        .byte   $20
        .byte   $27
LDECD:  .byte   $DC
        jsr     LDBA6
        jmp     LDA45
        php
        ror     $5185,x
        .byte   $B3
        .byte   $0C
        stx     $DE
        bcs     LDF5B
        .byte   $73
        .byte   $7C
        .byte   $23
        cld
        sbc     #$9A
        .byte   $87
        .byte   $34
        .byte   $82
        ora     $8180,x
        txs
        jsr     LED6C
        sta     ($BD,x)
        .byte   $32
        .byte   $34
        rol     $5D7F
        lsr     $87
        ldy     $82,x
        pla
        rol     $F743,x
        .byte   $80
        jmp     (L9E9A)
        .byte   $BB
        jsr     LC3C8
        lda     L0052
        and     #$03
        tay
        lda     LDF4E,y
        sta     $03FD
        lda     $B000
        and     #$F0
        cmp     #$70
        bne     LDF25
        lda     #$00
        tay
LDF1C:  sta     $8600,y
        sta     $8700,y
        dey
        bne     LDF1C
LDF25:  lda     $B000
        and     #$DF
        sta     $B000
        rol     a
        rol     a
        rol     a
        and     #$03
        tay
        lda     LDF42,y
        sta     $03FE
        lda     LDF46,y
        sta     $03FF
        jmp     LC558
LDF42:  .byte   $52
        bvs     LDECD
        .byte   $A0
LDF46:  .byte   $DF
        .byte   $DF
        .byte   $DF
        .byte   $DF
LDF4A:  .byte   $3F
        .byte   $CF
        .byte   $F3
        .byte   $FC
LDF4E:  brk
        eor     $AA,x
        .byte   $FF
        lda     $5B
        ora     $5D
        bne     LDF9F
        lda     $5A
        .byte   $C9
LDF5B:  rti
        bcs     LDF9F
        lsr     a
        lsr     a
        sta     $5F
        ldy     #$00
        sty     $60
        lda     #$3F
        sec
        sbc     $5C
        cmp     #$40
        bcc     LDFBE
        rts
        lda     $5B
        ora     $5D
        bne     LDF9F
        lda     $5A
        bmi     LDF9F
        lsr     a
        lsr     a
        sta     $5F
        lda     #$3F
        sec
        sbc     $5C
        cmp     #$40
        bcc     LDFB7
        rts
        lda     $5B
        ora     $5D
        bne     LDF9F
        lda     $5A
        bmi     LDF9F
        lsr     a
        lsr     a
        sta     $5F
        lda     #$5F
        sec
        sbc     $5C
        cmp     #$60
        bcc     LDFB7
LDF9F:  rts
        lda     $5B
        ora     $5D
        bne     LDF9F
        lda     $5A
        bmi     LDF9F
        lsr     a
        lsr     a
        sta     $5F
        lda     #$BF
        sec
        sbc     $5C
        cmp     #$C0
        bcs     LDF9F
LDFB7:  ldy     #$00
        sty     $60
        asl     a
        rol     $60
LDFBE:  asl     a
        rol     $60
        asl     a
        rol     $60
        asl     a
        rol     $60
        asl     a
        rol     $60
        adc     $5F
        sta     $5F
        lda     $60
        adc     #$80
        sta     $60
        lda     $5A
        and     #$03
        tax
        lda     LDF4A,x
        ldx     $5E
        dex
        beq     LDFF0
        dex
        beq     LDFE9
        and     ($5F),y
        sta     ($5F),y
        rts
LDFE9:  eor     #$FF
        eor     ($5F),y
        sta     ($5F),y
        rts
LDFF0:  tax
        and     ($5F),y
        sta     ($5F),y
        txa
        eor     #$FF
        and     $03FD
        ora     ($5F),y
        sta     ($5F),y
        rts
