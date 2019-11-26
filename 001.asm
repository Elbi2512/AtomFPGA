; 
;  (c) Copyright 1998, 1998   BinGate Automation 
;  Version 0.01 (Apr 13 1998) 
;	updates 1.0 17 nov 2019
; 
; File: fp100.asm 
;       Date:   Mon Jan 26 15:59:04 1998 
;       CPU:    MOS Technology 6502 
; 
.P02	; good old 6502
; writeFPRL

FPROC      :=      0 
FPADD      :=      1 
FPSUB      :=      2 
FPMUL      :=      3 
FPDIV      :=      4 
FPABS      :=      5 
FPRNDINT   :=      6 
FPSQRT     :=      7 
FPTAN      :=      8 
FPSIN      :=      9 
FPCOS      :=      10 
FPDEG      :=      11 
FPRAD      :=      12 
FPOWER     :=      13 
FPHTN      :=      14 
FPLN       :=      15 
FPLOG      :=      16 
FPYTOX     :=      17 
FPEXP      :=      18 
FPTENTOX   :=      19 
FPACOT     :=      20 
FPACS      :=      21 
FPASN      :=      22 
FPSEC      :=      23 
FPCSC      :=      24 
FPATN      :=      25 
FPTWOTOX   :=      26 
FPATHH     :=      27 
FPCOMP     :=      28 
; hooks to other roms 


L1002   :=     $1002 
LA002   :=     $A002 
LC231   :=     $C231 
LC278   :=     $C278 
LC279   :=     $C279 
LC3CB   :=     $C3CB 
LC372   :=     $C372 
LC3CD   :=     $C3CD 
LC434   :=     $C434 
LC4D9   :=     $C4D9 
LC4E1   :=     $C4E1 
LC4E4   :=     $C4E4 
LC54A   :=     $C54A 
LC558   :=     $C558 
LC55B   :=     $C55B 
LC569   :=     $C569 
LC78B   :=     $C78B 
LC8BC   :=     $C8BC 
LC99F   :=     $C99F 
LCA1B   :=     $CA1B 
LCA4C   :=     $CA4C 
LCB29   :=     $CB29 
LCCD5   :=     $CCD5 
LCD09   :=     $CD09 
LCEB1   :=     $CEB1 
LCF3E   :=     $CF3E 
LCF41   :=     $CF41 
;D6C3 Copy the 8 bytes #57..#5E to #5F..66 subroutine
;D6CD Shift Left 1 Bit #5E..#5A subroutine
;D6D8 Shift Right 1 Bit #5A..#5E subroutine
;D6E3 Copy #5A..#5D to #5B..#5E and clear #5A subroutine
;D706 Copy #62..#65 to #63..#66 and clear #62 subroutine
LEE7C   :=     $EE7C 
LF802   :=     $F802 
LFFD1   :=     $FFD1 
LFFC5   :=     $FFC5 
LFFC8   :=     $FFC8 
LFFD4   :=     $FFD4 

;D0E4 Deal with '('
;D0EB Deal with '+' and '-'
;D0F2 Deal with '*' and '/'
;D0FC Deal with '+' and '-'
;D103 Deal with '^'
;D10E Floating Point interpreter
;D15E Floating Point interpreter
;D177 Deals with '+'
;D183 Deals with '-'
;D18F Deals with '*'
;D19B Deals with '/'
;D1A7 Deals with '^'
;D1BC Deals with '-'
;D1CB Deals with '+'
; 
 .org $D000 
; .byte 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ; 16 dummy's 
 .byte $AA 
 .byte $55 
 .byte $0E, $d1 
 .byte $5e, $d1 
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
 ;.byte    FPINIT/256 
 ;.byte    FPINIT .MOD 256 
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
 .byte FPINIT/256 
 .byte FPINIT .MOD 256 
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
;D0E4 Deal with '('

INIT1: 	JSR LD0FC 
		LDX #$B4 
		BNE LD10B 
BEP_ARG:clc 
LD0EC: 	ror $73 
		ldx     #$AC 
		bne     LD10B 	; spring altijd
		
LD0F2: 	jsr 	LD85D ; kopier stack naar werkruimte
LD0F5: 	jsr 	LD106  ; we doen wat recursie voor haakjes, functies etc
LD0F8: 	ldx 	#$A1 
		bne     LD10B ;	spring altijd
LD0FC: 	jsr 	LD0F5  ; nog meer recursie 
LD0FF: 	ldx #$9A 
		bne     LD10B 	; spring altijd
		
LD103: 	jsr 	LD85D  ; kopier stack naar werkruimte
LD106: 	jsr BEP_ARG 
LD109: 	ldx #$A8 

LD10B: 	clc 
		bcc LD113  ; geforceerde sprong in het duister 
		
		ldx     #$5F 
		sty $03 
LD112: 	sec 
LD113: 	ror $53 
		ldy $03 
		dey 
LD118: 	iny 
		lda ($05),y 
		cmp     #$20 
		beq     LD118 
		dey 
		sty $52 
		dex 
LD123: 	ldy $52 
LD125: 	inx 
		iny 
LD127: 	lda XD006,x 
		bmi LD146 
		cmp ($05),y 
		beq     LD125 
		dex 
LD131: 	inx 
		lda 	XD006,x 
		bpl     LD131 
		inx 
		bit 	$53 
		bpl     LD123 
		lda ($05),y 
		cmp #'.'      ; afgekort? 
		bne     LD123 
		iny 
		dex 
		bcs     LD127 
LD146: 	cmp #$FE      ; einde tabel? 
		bcs     LD15B 
		sta $53 
		lda     $D007,x 
		sta $52 
		sty $03 
		ldx $04 
		jmp ($0052)      ; jump naar de kanibalen. 
LD158: 	ldx $04 
		rts 
; 
LD15B: 	beq LD158 
		brk      ; valt geen chocola van te maken: fp fout.	
;D15E Floating Point interpreter 
INIT: 	sty $03 
		jsr     LD0EC 
		jsr     LD89A 
		lda 	$5A 
		sta 	$60 
		lda 	$5B 
		sta 	$5F 
		lda 	$5C 
		sta 	$5E 
		ldy     #$5D 
		jmp     LC99F 

; D177 Deals with '+'
FP_PLS:    ; eerste argument is beschikbaar als we hier zijn 
		jsr 	LD0F2  ; haal eerste en tweede argument op 
        jsr     LD870 
        jsr     LD804 
		jsr		PUTF2	; zet de atom waardes klaar voor de fpga ; eerste argument overpompen 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga ; HIER TWEEDE argument 
		lda 	#FPADD  ; afdeling optellen 
		jsr		procesfpga

		jmp 	LD0FF  ; staat nu op 0x59 en verder 

; D183 Deals with '-'
FP_MIN: 
        jsr     LD0F2           ; FP MIN 
        jsr     LD870 
        jsr     LD804 
		jsr		PUTF2	; zet de atom waardes klaar voor de fpga ; eerste argument overpompen 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga ; HIER TWEEDE argument 
        lda     #FPSUB 
		jsr		procesfpga

		jmp     LD0FF 
; 
FP_MAAL: 
        jsr     LD103           ; FP MAAL 
        jsr     LD870 
        jsr     LD804 
		jsr		PUTF2	; zet de atom waardes klaar voor de fpga ; eerste argument overpompen 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga ; HIER TWEEDE argument 
		lda 	#FPMUL 
		jsr		procesfpga

		jmp     LD0F8 
; 
FP_DIV: 
        jsr     LD103           ; FP GEDEELD 
        jsr     LD870 
        jsr     LD804 
		jsr		PUTF2	; zet de atom waardes klaar voor de fpga ; eerste argument overpompen 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga ; HIER TWEEDE argument 
        lda     #FPDIV 
		jsr		procesfpga

		jmp     LD0F8 
; 
FP_MACH: 
        jsr     LD103           ; FP GEDEELD 
		jsr 	BEP_NUL     ; bepaal waarde argument 
		beq 	END_MC     ; bij 0 doen we nix 
		bpl 	mch_verder  ; boven 0 kan wortel bepaald worden 
		brk      ; daaronder gaan we gillend op ons bek 
mch_verder: 
        jsr     LD870 
        jsr     LD804 
		jsr		PUTF2	; zet de atom waardes klaar voor de fpga ; eerste argument overpompen 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga ; HIER TWEEDE argument 
		lda 	#FPYTOX 
		jsr		procesfpga

        ;jsr     pc_opdracht2 
        ;jsr     naar_pc1 
		;jsr van_pc 
END_MC: jmp LD109 
; 
FPL_min: 	jsr     FPL_pl 
LD1BF:  	jsr  BEP_NUL 
			beq  FPL_end 
chng_sg: lda  $57  
		eor  #$80       ; verander teken 
		sta  $57 ; deze had ik op 5a gezet.. 
FPL_end: rts 
; 
FPL_pl: ldy  $03   ; FP plus? 
		dey 
skp_spc: iny 
		lda  ($05),y 
		cmp  #' '       ; spatie ? 
		beq  skp_spc      ; ja, dan skip 
		cmp  #'%'       ; fp. variabele? 
		bne  LD20B       ; nee, door naar .. 
		inc  $03     ; verhoog adres 
		jsr  waarde_uit_var      ; haal waarde uit var 
		bcc  LD20B 
		ldy  #$6F 
		jsr  LC3CD 
copy_waarde: 
		ldy  #$04      ; clear waarde 
		lda  #$0 
		sta  $5E 
		sta  $58 
		sta  $57 
copy_lus: 
		lda  ($6F),y 
		sta  $59,y 
		ora  $57 
		sta  $57 
		dey 
		bpl  	copy_lus 
		tax 
		beq  	LD207  ; hij, de laagste waarde 59 is 0 
		lda  	$5A 
		sta  	$57       ; zet koud in 57 
        ora     #$80           ; forceer bit 
		sta  	$5A       ; en terug in zijn hok (maar waarom??? 
		txa   ; een return waarde 59 in a 
LD207:  rts 
; 
LD208:  sty $03 
		rts 
; 
LD20B: 	jsr     LD5A5 
		bcs     LD208 
		ldx 	#$0 
		jmp     LD112 
; 
ABS: 	jsr 	BEP_ARG 
		jsr 	BEP_NUL 
		bmi 	chng_sg      ; voor <1 naar de 0 anders klaar 
		rts 
; 
ACS: 	jsr 	BEP_ARG      ; buurten bij argument routine 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPACS     ; afdeling arc cosinus 
		jmp		procesfpga
;		rts 

; 
ASN: 	jsr BEP_ARG 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPASN     ; afdeling arc sinus 
		jmp		procesfpga
;		 rts 
; 
RAD: 	jsr BEP_ARG 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPRAD     ; afdeling radiatoren 
		jmp		procesfpga
;		rts 
; 
DEG: 	jsr BEP_ARG 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPDEG     ; afdeling GRADEN 
		jmp		procesfpga
;		rts 
SGN: 	jsr 	BEP_ARG 
		jsr 	BEP_NUL 
		beq 	isalnul 
		pha 
		jsr 	LDE8D     ; een -1 een 0 of een 1 
		pla 
		sta $57 
isalnul: 
		rts 
; 
PER1: 	bit 	$73 
		bmi     LD2C0 
FLT: 	jsr 	LC8BC 
		ldy     #$5D 
		jsr     LC3CD 
		sta 	$5A 
		lda 	$5F 
		sta 	$5B 
		lda 	$5E 
		sta 	$5C 
		lda     #$A0 
		sta 	$59 
		ldy 	#$0 
		sty 	$5E 
		lda 	$5A 
		sta 	$57 
		bpl     LD2BD 
		jsr     LD8D5 
LD2BD: 	jmp     LD7C8 
LD2C0: 	jmp     LCA1B 
; 
PI:		lda     #$82 
		sta 	$59 
        lda     #$C9 
		sta 	$5a 
        lda     #$0f 
		sta 	$5b 
        lda     #$da 
		sta 	$5c 
        lda     #$a2 
		sta 	$5d 
		rts 
; 
FGET: 	jsr 	LCF3E 
		ldx 	#$04   ; haal 5 bytes uit geopende file 
LD2D1: 
		jsr 	LFFD4   ; get byte uit handle 
		sta 	$03C5,x   ; zet hem weg 
		dex 
		bpl 	LD2D1   ; totdat we er 5 hebben 
		jsr 	LDBAA   ; ? 
		jmp 	copy_waarde  ; zet hem op de gewenst plek 
LDbrk: 	BRK  ; inserted om ass. error te voorkomen 
; 
VAL: 	jsr 	LCEB1   ; neem het fatsoenlijk numeriek deel 
 ldy #$0 
LD2E4: 
 jsr     LD304 
 cmp #'+' 
 beq     LD2FB 
 cmp #'-' 
 bne     LD2FE 
 jsr     LD303 
 sty $54 
 jsr     LD5B1 
 jmp     LD1BF 
; 
LD2FB: 
 jsr     LD303 
LD2FE: 
 sty $54 
 jmp LD5B1   ; hier zijn we klaar 
; 
LD303:     ; skip space 
 iny 
LD304: 
 lda ($52),y 
 cmp #' '   ; is het een spatie 
 beq     LD303 
 rts 
; 
VAR: jsr waarde_uit_var 
 bcc LDbrk 
 jsr     LC279 
 jsr     LD0FC 
 jsr     LC4E4 
        jsr     Sla_op 
 jmp LC55B   ; volgende statement 
; 
STR: jsr LD0FC 
 jsr     LC231 
 jsr     LC4E1 
 jsr     LC3CB 
 jsr     LD4D0 
 jmp LC55B   ; volgende statement 
; 
FPRINT: jsr LC372 
 ldx     #$B8 
 jmp LD10B   ; bepaal deel van argument 
; 
LD339: jsr LD0FC 
 lda     #$C5 
 sta $52 
 lda     #$03 
 sta $53 
 jsr     LD4D0 
 dec $6F 
 lda $0321   ; neemde waarde in @ 
 sec 
 sbc $6F 
 bcc     LD35C 
 beq     LD35C 
 tay 
 lda     #$20 
pr_sp: 
 jsr LCA4C   ; print 'Y' spaties 
 dey 
 bne pr_sp 
LD35C: 
 ldy #$0 
LD35E: 
 lda ($52),y 
 cmp #$0D   ; einde van de opdracht 
 beq FPRINT 
 jsr     LCA4C 
 iny 
 bne     LD35E 
FINPUT: jsr LC372 
 lda ($05),y 
 cmp #'%'   ; fp. variabele in aantocht? 
 bne LD37B   ; nee, 
 iny 
 sty $03 
 jsr waarde_uit_var 
 bcs     LD380 
LD37B: 
 ldx     #$C3 
 jmp LD10B   ; bepaal deel van argument 
; 
LD380: 
 jsr     LCD09 
 tay 
 lda $05 
 pha 
 lda $06 
 pha 
 lda $03 
 pha 
 sty $03 
 iny 
 sty $06 
 lda     #$40 
 sta $05 
 jsr     LD0FC 
 pla 
 sta $03 
 pla 
 sta $06 
 pla 
 sta $05 
 jsr Sla_op 
 jmp     LD37B 
; 
FIF: jsr LD9EB     ; vergelijken; Zet Cxxx adres op stack 
 jmp     LC569 
; 
FUNTIL: jsr LD9EB     ; zet het return adres HIGH van Cxxx op stack 
 jmp LCCD5     ; ook vergelijken 
; 
FPUT: jsr LD494     ; plaats 5 bytes naar output 
 jsr     LD0FC 
 jsr     LC4E4 
 jsr     LD831 
 ldx $04   ; 5 bytes 
 jsr     LCF41 
 ldx #$04     ; 5 bytes 
LD3C7: 
 lda $03C5,x     ; haal hem op 
 jsr LFFD1     ; stuur hem weg 
 dex 
 bpl LD3C7     ; en dat 5 keer 
 jmp LC55B     ; klaar zijn we 
; 
FDIM: lda $01 
 ora $02 
 beq     LD443 
 jsr     LC434 
 bcs     LD443 
 ldy $03 
 lda ($05),y 
 cmp #'%'   ; praten we over fp. getallen 
 bne LD443   ; nee. 
 iny 
 lda ($05),y        ; haal waarde op 
 iny 
 cmp ($05),y        ; vergelijk met bovenbuurvrouw (AA-ZZ) 
 bne LD443   ; nee, niet gelijk. Helemaal fout dus. 
 cmp #'['   ; groter dan Z 
 bcs LD443   ; Ja, helemaal fout dus. 
 sbc #$3F   ; maak van ascii absolute waarde 
 bcc LD443   ; <0? (A), foute boel dus. Wegwezen. 
 iny 
 sty $03 
 pha 
 jsr LC78B   ; argument ophalen. 
 inc $15,x 
 bne     LD403 
 inc $24,x 
LD403: 
 jsr     LD49A 
 pla 
 tay 
 clc 
 lda $23        ; top 
 sta $0687,y   ; basisopstelling 
 adc $16 
 sta $23 
 lda $24 
 sta $06A2,y   ; basisopstelling high 
 adc $25 
 sta $24 
 ldy #$0 
 sty $04 
 lda     #$AA 
 sta ($23),y        ; kijk of hier ook ram zit 
 cmp ($23),y 
 bne LD443   ; helaas. Past dus niet meer. Fout. 
 lsr a   ; extra testje, ivm spiegels 
 sta ($23),y 
 cmp ($23),y 
 bne LD443   ; alsnog fout. Zonde, he? 
 jsr LC434   ; alles fris. 
 bcs     LD443 
 ldy $03 
 lda ($05),y 
 cmp #','   ; wordt er soms nog meer gedimmed? 
 bne LD440   ; nee, het volgende statement aande beurt 
 inc $03 
 jmp FDIM   ; ja, dan doen we het nog eens. 
LD440: 
 jmp     LC558 
; 
LD443: brk 
; 
waarde_uit_var: 
 jsr LC434  ; geldige variabele naam? 
 bcc LD457  ; neen. Dikke mik dus. Misschien direct? 
 lda $15,x 
 asl     a 
 asl     a 
 adc $15,x 
 sta $15,x 
 lda     #$28 ; base page?
 sta $24,x 
 sec 
 rts 
; 
LD457: 
 ldy $03 
 lda ($05),y     ; byte from commandline 
 cmp #'!'       ; is het de pling? 
 bne no_pling      ; nee. 
 inc $03 
 jsr     LC8BC 
LD464: 
 sec 
 rts 
; 
no_pling: 
 iny 
 cmp ($05),y 
 bne     LD473 
 cmp #'['   ; zitten we in de reeks van A-Z 
 bcs     LD473 
 sbc #$3F   ; maak absolute waarde van 
 bcs LD475   ; als geldig (>0) gaan we door 
LD473: clc    ; ziet er niet fris uit. 
 rts 
; 
LD475: iny 
 sty $03 
 pha 
 jsr     LC8BC 
 jsr LD49A   ; bepaal y-> als variabele 
 pla 
 tay 
 bcs     LD493 
 lda $0687,y   ; haal onze vrienden op uit page 6 
 adc $15,x 
 sta $15,x 
 lda     $06A2,y 
 adc $24,x 
 sta $24,x        ; en sla ze op 
 bcc     LD464 
LD493: 
 brk 
LD494: 
 jsr     LC8BC 
 jmp     LC231 
; 
LD49A: 
 ldy $24,x 
 lda $15,x 
 asl     a 
 rol $24,x 
 asl     a 
 rol $24,x 
 clc 
 adc $15,x 
 sta $15,x 
 tya 
 adc $24,x 
 sta $24,x 
 rts 
; 
OTHERROM: lda   $A000 
	cmp     #$40 
	beq     LD4C0 
	lda     $A001 
	cmp     #$BF 
	bne     LD440 
	jmp     LA002 
LD4C0: 
	JMP 	$C558 ;      Do interpreter post test
 ;jmp L1002   ; naar onze rom manager. 
; 
ASC2DEC: 
	cmp 	#$3A     ; HEX A-F 
	bcs 	A_error 
	cmp 	#$30     ; LAGER DAN 0 
	bcc 	A_ok 
	sbc     #$30 
A_ok: rts 
; 
A_error: 
	clc 
	rts 
; 
LD4D0: 
 lda #$0 
 sta $6F 
 jsr BEP_NUL 
 bne LD4EB  ; geen null, dus geen default print 0.0 
 lda #'0'  ; een null 
 jsr In_Buf   ; store waarde in buffer en verhoog adres 
 lda #'.'  ; een punt 
 jsr In_Buf   ; store waarde in buffer en verhoog adres 
 lda #'0'  ; een null 
 jsr In_Buf   ; store waarde in buffer en verhoog adres 
 jmp     LD571 
; 
LD4EB: 
 bpl LD4F2  ; groter dan 0? ja, 
 lda #'-'  ; een minnetje 
 jsr In_Buf   ; store waarde in buffer en verhoog adres 
LD4F2: 
 lda #$0 
 sta $6D 
LD4F6: 
 lda $59 
 cmp     #$81 
 bcs     LD504 
 jsr     LD6A0 
 dec $6D 
 jmp     LD4F6 
; 
LD504: 
 cmp     #$84 
 bcc     LD518 
 bne     LD510 
 lda $5A 
 cmp #$A0     ; groter dan ? 
 bcc LD518     ; nee 
LD510: 
 jsr     LD71B 
 inc $6D 
 jmp     LD4F6 
; 
LD518: 
 lda $59 
 cmp     #$84 
 bcs     LD525 
 jsr     LD6D8 
 inc $59 
 bne     LD518 
LD525: 
 sec 
 lda     #$FF 
 jsr     LD636 
 lda $5A 
 cmp     #$A0 
 bcs     LD510 
 lda     #$01 
 ldy $6D 
 bmi     LD541 
 cpy     #$08 
 bcs     LD541 
 iny 
 lda #$0 
 sta $6D 
 tya 
LD541: 
 sta $70 
 ldx     #$09 
 stx $54 
LD547: 
 jsr     LD575 
 dec $70 
 bne     LD553 
 lda #'.' 
 jsr In_Buf  ; store waarde in buffer en verhoog adres 
LD553: 
 dec $54 
 bne     LD547 
 lda $6D 
 beq     LD571 
 lda #'E'  ; pratend over e macht notatie 
 jsr In_Buf  ; store waarde in buffer en verhoog adres 
 lda $6D 
 bpl LD56E  ; niet te klein? 
 lda #'-' 
 jsr In_Buf 
 sec 
 lda #$0 
 sbc $6D 
LD56E: 
 jsr     LD587 
LD571: 
 lda #$0D  ; een CR als einde buffer 
 bne In_Buf  ; store waarde in buffer en verhoog adres 
LD575: 
 lda $5A 
 lsr     a 
 lsr     a 
 lsr     a 
 lsr     a 
 jsr     LD58B 
 lda $5A 
 and     #$0F 
 sta $5A 
 jmp     LD64E 
; 
LD587: 
 cmp     #$0A 
 bcs     LD594 
LD58B: 
 ora     #$30 
In_Buf: 
 ldy $6F        ; plaats de ASCII in buffer 
 sta ($52),y 
 inc $6F 
 rts 
; 
LD594: 
 ldx     #$FF 
LD596: 
 inx 
 sbc     #$0A 
 bcs     LD596 
 adc     #$0A 
 pha 
 txa 
 jsr     LD587 
 pla 
 bpl     LD58B 
LD5A5: 
 lda $03 
 sta $54 
 lda $05 
 sta $52 
 lda $06 
 sta $53 
LD5B1: 
 jsr CLR_WRK 
 sta $6C 
 sta $6D 
 jsr     LD67B 
 cmp #'.' 
 beq     LD5CD 
 jsr ASC2DEC 
 bcc     LD635 
 sta $5E 
LD5C6: 
 jsr     LD67B 
 cmp #'.' 
 bne     LD5D6 
LD5CD: 
 lda $6C 
 clc 
 bne     LD60C 
 inc $6C 
 bne     LD5C6 
LD5D6: 
 cmp #'E' 
 beq     LD601 
 jsr ASC2DEC 
 bcc     LD60C 
 sta $6E 
 lda $5A 
 cmp     #$18 
 bcc     LD5EF 
 lda $6C 
 bne     LD5C6 
 inc $6D 
 bcs     LD5C6 
LD5EF: 
 lda $6C 
 beq     LD5F5 
 dec $6D 
LD5F5: 
 jsr     LD64E 
 clc 
 lda $6E 
 jsr     LD636 
 jmp     LD5C6 
; 
LD601: 
 jsr     LD67B 
 jsr     LD778 
 clc 
 adc $6D 
 sta $6D 
LD60C: 
 lda     #$A8 
 sta $59 
 jsr BEP_NUL 
 beq     LD631 
 jsr     LD7C8 
 lda $6D 
 bmi     LD627 
 beq     LD62E 
LD61E: 
 jsr     LD6A0 
 dec $6D 
 bne     LD61E 
 beq     LD62E 
LD627: 
 jsr     LD71B 
 inc $6D 
 bne     LD627 
LD62E: 
 jsr     LDA9B 
LD631: 
 sec 
 ldy $54 
 dey 
LD635: 
 rts 
; 
LD636: 
 ldx     #$05 
LD638: 
 adc $59,x 
 sta $59,x 
 lda #$0 
 dex 
 bne     LD638 
 rts 
; 
LD642: 
 ldx     #$05 
LD644: 
 lda $59,x 
 adc $61,x 
 sta $59,x 
 dex 
 bne     LD644 
 rts 
; 
LD64E: 
 ldx     #$05 
 lda #$0 
 sta $67 
LD654: 
 lda #$0 
 sta $68 
 lda $59,x 
 asl     a 
 rol $68 
 asl     a 
 rol $68 
 clc 
 adc $59,x 
 bcc     LD667 
 inc $68 
LD667: 
 asl     a 
 rol $68 
 clc 
 adc $67 
 bcc     LD671 
 inc $68 
LD671: 
 sta $59,x 
 lda $68 
 sta $67 
 dex 
 bne     LD654 
 rts 
; 
LD67B: 
 sty $55 
 ldy $54 
 lda ($52),y 
 ldy $55 
 inc $54 
 rts 
; 
BEP_NUL:        ; test op nullen 
		lda $5A 
		ora $5B 
		ora $5C 
		ora $5D 
		ora $5E 
		beq     LD699 ; alles is 0, clear de overige
		lda $57 
		bne     LD69F ; return waarde in A
		lda     #$01 ; geforceerde 1 
		rts 
; 
LD699:       ; store waarde in fp. werkruimte 
		sta $57 
		sta $59 
		sta $58 
LD69F:  rts 
; 
LD6A0: 
 clc 
 lda $59 
 adc     #$03 
 sta $59 
 bcc     LD6AB 
 inc $58 
LD6AB: 
 jsr     LD6C3 
 jsr     LD6FB 
 jsr     LD6FB 
LD6B4: 
 jsr     LD642 
LD6B7: 
 bcc     LD6C2 
 jsr     LD6D8 
 inc $59 
 bne     LD6C2 
 inc $58 
LD6C2: 
 rts 
; 
;D6C3 Copy the 8 bytes #57..#5E to #5F..66 subroutine
LD6C3:    ; copy waarde in werkruimte 
		ldx     #$08 
LD6C5:	lda $56,x 
		sta $5E,x 
		dex 
		bne     LD6C5 
		rts 
; 
;D6CD Shift Left 1 Bit #5E..#5A subroutine

LD6CD:    ; * 2 routine 
 asl $5E 
 rol $5D 
 rol $5C 
 rol $5B 
 rol $5A 
 rts 
; 
;D6D8 Shift Right 1 Bit #5A..#5E subroutine

LD6D8:    ; / 2 routine 
 ror $5A 
 ror $5B 
 ror $5C 
 ror $5D 
 ror $5E 
 rts 
; 
;D6E3 Copy #5A..#5D to #5B..#5E and clear #5A subroutine

 lda $5D       ; schuif de zaak 1 byte op 
 sta $5E 
 lda $5C 
 sta $5D 
 lda $5B 
 sta $5C 
 lda $5A 
 sta $5B 
        lda     #$0         ; en schoon het hoogste byte 
 sta $5A 
 rts 
; 
LD6F8: 
 jsr     LD6C3 
LD6FB: 
		lsr 	$62   ; /2 routine 
		ror 	$63 
		ror 	$64 
		ror 	$65 
		ror 	$66 
		rts 
; 
;D706 Copy #62..#65 to #63..#66 and clear #62 subroutine

		lda 	$65       ; copieer wat bytes 
		sta 	$66 
		lda 	$64 
		sta 	$65 
		lda 	$63 
		sta 	$64 
		lda 	$62 
		sta 	$63 
        lda     #$0           ; en schoon de hoogste vriend 
		sta 	$62 
		rts 
; 
LD71B: 
 sec 
 lda $59 
 sbc     #$04 
 sta $59 
 bcs     LD726 
 dec $58 
LD726: 
		jsr     LD6F8 
		jsr     LD6B4 
		jsr     LD6F8 
		jsr     LD6FB 
		jsr     LD6FB 
		jsr     LD6FB 
		jsr     LD6B4 
		lda 	#$0 
		sta 	$62 
		lda 	$5A 
		sta 	$63 ; teken rakker?
 lda 	$5B 
 sta 	$64 
 lda 	$5C 
 sta 	$65 
 lda 	$5D 
 sta 	$66 
 lda 	$5E 
 rol     a 
 jsr     LD6B4 
 
		lda #$00 ; deze nog heel nadrukkelijk checken..
		sta $62 
		sta $63 
		lda $5A 
		sta $64 
		lda $5B 
		sta $65 
		lda $5C 
		sta $66 
		lda $5D 
		rol     a 
		jsr     LD6B4 
 lda $5B 
 rol     a 
 lda $5A 
LD772: 
 jsr     LD636 
 jmp     LD6B7 
; 
LD778: 
 ldy     #$FF 
 cmp     #$2B 
 beq     LD783 
 cmp     #$2D 
 bne     LD786 
 iny 
LD783: 
 jsr     LD67B 
LD786: 
 jsr ASC2DEC 
 bcc     LD7AF 
 tax 
 jsr     LD67B 
 jsr ASC2DEC 
 bcc     LD7A4 
 sta $6E 
 jsr     LD67B 
 txa 
 sta $67 
 asl     a 
 asl     a 
 adc $67 
 asl     a 
 adc $6E 
 tax 
LD7A4: 
 tya 
 bne     LD7AD 
 stx $6E 
 sec 
 sbc $6E 
 rts 
; 
LD7AD: 
 txa 
 rts 
; 
LD7AF: 
 lda #$0 
LD7B1: 
 rts 
LD7C8: 
 jsr BEP_NUL 
 beq LD7B1 
LD7CD: 
 lda $5A 
 bne LD7F2 
 lda $5B 
 sta $5A 
 lda $5C 
 sta $5B 
 lda $5D 
 sta $5C 
 lda $5E 
 sta $5D 
 lda #$0 
 sta $5E 
 sec 
 lda $59 
 sbc #$08 
 sta $59 
 bcs LD7CD 
 dec $58 
 bcc LD7CD 
LD7F2: 
 lda $5A 
 bmi LD7B1 
 jsr LD6CD 
 lda $59 
 bne LD7FF 
 dec $58 
LD7FF: 
 dec $59 
 jmp LD7F2 
LD804: 
 ldy     #$04 
 lda #$0 
 sta $66 
 sta $60 
 sta $5F 
LD80E: 
 lda ($6F),y 
 sta $61,y 
 ora $5F 
 sta $5F 
 dey 
 bpl     LD80E 
 tax 
 beq     LD826 
 lda $62 
 sta $5F 
        ora     #$80 
 sta $62 
 txa 
LD826: 
 rts 
; 
LD831: 
 jsr LDBAA 
 bne LD83D 
Sla_op: 
 ldx $04 
 ldy     #$6F 
 jsr LC3CD    ; zet in basic stack 
LD83D: ; sla tijdelijke gegevens op
		ldy #$0 
		lda $59 
		sta ($6F),y 
		iny 
		lda $57 
		and     #$80 
		sta $57 
		lda $5A 
		and     #$7F 
		ora $57 
		sta ($6F),y 
LD852: 	iny 
		lda $59,y 
		sta ($6F),y 
        cpy     #$04 
		bne     LD852 
		rts 
; 
LD85D:        ; kopier stack naar werkruimte 
	ldy     #$52 
	sty 	$6F 
	lda 	#$0 
	sta 	$70 
	jsr 	LD83D      ; kopier een deel 
	jsr 	LC4D9      ; basis rom doet de rest 
	lda 	$56 
	sta 	$73,x 
	rts 
; 
LD870:        ; plaats van stack naar werkruimte 
	ldx $04 
	jsr     LC3CB 
	lda 	$74,x 
	sta 	$56 
	sty 	$6F 
	lda 	#$0 
	sta 	$70 
	rts 
; 
LD880: 
 lda $5E 
 cmp     #$80 
 bcc     LD88D 
 beq     LD892 
 lda     #$FF 
 jsr     LD772 
LD88D: 
 lda #$0 
 sta $5E 
 rts 
; 
LD892: 
 lda $5D 
 ora     #$01 
 sta $5D 
 bne     LD88D 
LD89A: 
 jsr     LD8C7 
 beq     LD8A5 
LD89F: 
 lda $59 
 cmp     #$A0 
 bcs     LD8B9 
LD8A5: 
 lsr $5A 
 ror $5B 
 ror $5C 
 ror $5D 
 ror $62 
 ror $63 
 ror $64 
 ror $65 
 inc $59 
 bne     LD89F 
LD8B9: 
 beq     LD8D1 
LD8BB: 
 lda     #$7F 
 sta $5A 
 lda     #$FF 
 sta $5B 
 sta $5C 
 sta $5D 
LD8C7: 
 ldx     #$08 
 lda #$0 
LD8CB: 
 sta $5F,x 
 dex 
 bne     LD8CB 
 rts 
; 
LD8D1: 
 lda $57 
 bpl     LD8E1 
LD8D5: 
 sec 
 ldx     #$04 
LD8D8: 
 lda #$0 
 sbc $59,x 
 sta $59,x 
 dex 
 bne     LD8D8 
LD8E1: 
 rts 
; 
LD8E8: 
 lda #$0 
 sbc $61,x 
 sta $61,x 
 dex 
 bne     LD8E8 
 lda $57 
 eor     #$80 
 sta $57 
 bpl     LD90A 
LD8F9: 
 inc $5D 
 bne     LD909 
 inc $5C 
 bne     LD909 
 inc $5B 
 bne     LD909 
 inc $5A 
 beq     LD8BB 
LD909: 
 rts 
; 
LD90A: 
 jsr     LD8D5 
 jsr     LD8F9 
 jmp     LD8D5 
; 
LD913: 
 ldx     #$05 
LD915: 
 lda $61,x 
 sta $59,x 
 dex 
 bne     LD915 
 lda     #$80 
 sta $59 
 jmp     LD7C8 
; 
LD929: 
 ldx     #$08 
LD92B: 
 lda $5E,x 
 sta $56,x 
 dex 
 bne     LD92B 
LD932: 
 rts 
; 
LD939: 
 jsr     LD1BF 
 jsr LD804 
 beq LD932 
LD941: 
 jsr BEP_NUL 
 beq     LD929 
 lda $59 
 cmp $61 
 beq     LD972 
 bcc     LD95D 
 sbc $61 
 cmp     #$21 
 bcs     LD932 
 tax 
LD955: 
 jsr     LD6FB 
 dex 
 bne     LD955 
 beq     LD972 
LD95D: 
 sec 
 lda $61 
 sbc $59 
 cmp     #$21 
 bcs     LD929 
 tax 
LD967: 
 clc 
 jsr     LD6D8 
 dex 
 bne     LD967 
 lda $61 
 sta $59 
LD972: 
 lda $57 
 eor $5F 
 bpl     LD9C1 
 lda $5A 
 cmp $62 
 bne     LD999 
 lda $5B 
 cmp $63 
 bne     LD999 
 lda $5C 
 cmp $64 
 bne     LD999 
 lda $5D 
 cmp $65 
 bne     LD999 
 lda $5E 
 cmp $66 
 bne     LD999 
 jmp CLR_WRK 
; 
LD999: 
 bcs     LD9C8 
 sec 
 lda $66 
 sbc $5E 
 sta $5E 
 lda $65 
 sbc $5D 
 sta $5D 
 lda $64 
 sbc $5C 
 sta $5C 
 lda $63 
 sbc $5B 
 sta $5B 
 lda $62 
 sbc $5A 
 sta $5A 
 lda $5F 
 sta $57 
 jmp     LDA98 
; 
LD9C1: 
 clc 
 jsr     LD6B4 
 jmp     LDA9B 
; 
LD9C8: 
 sec 
 lda $5E 
 sbc $66 
 sta $5E 
 lda $5D 
 sbc $65 
 sta $5D 
 lda $5C 
 sbc $64 
 sta $5C 
 lda $5B 
 sbc $63 
 sta $5B 
 lda $5A 
 sbc $62 
 sta $5A 
 jmp     LDA98 
; 
 brk 
LD9EB: 
		jsr     LD0FC 
		lda 	#$C7   ; het high adres op de stack voor de latere return 
		pha 
		ldx     #$C8 
		jmp 	LD10B ; bepaal het argument 
; 
EQUAL: 
		jsr  	jsrcomp 
        lda     #$5D   ; Hier worden bijdehande geintjes uitgehaald 
		bne  	LDA0C ; het basic adres voor vergelijkingen uit de C000 
LEQUAL: 
		jsr  	jsrcomp 
        lda     #$66  ; rom wordt van stal gehaald en het low adres wordt 
        bne     LDA0C  ; op de stack gezet 
NEQUAL: 
		jsr  	jsrcomp 
        lda     #$6F 
        bne     LDA0C 
LT: 
		jsr  	jsrcomp 
        lda     #$76 
        bne     LDA0C 
GEQUAL: 
		jsr  	jsrcomp 
        lda     #$7D 
        bne     LDA0C 
GT: 
		jsr  	jsrcomp 
        lda     #$84 
LDA0C: 
        pha                        ; zet returadres low op de stack 
        jsr     LD0FC              ; als alles gelijk is. anders NE 
        jsr     naar_pc1           ; tweede argument naar de PC 
        jsr     LFFC5              ; haal byte op. Hierin het resultaat. 
		and  	#$03      ; hoogste 6 bits resetten 
		sta  	$59      ; tijdelijk opslaan 
		php       ; status op stack 
		pla       ; status in accu 
		and  	#$fc      ; bovenste 6 bits bewaren 
		ora  	#59      ; zero en carry erbij schrijven 
		pha       ; en accu via stack 
		plp       ; naar status register 
LDA3C: 	rts       ; en jump naar de basic rom 
; 
jsrcomp: lda 	#FPCOMP      ; om de zaak iets in te korten 
	;	jmp pc_opdracht1     ; via externe RTS terug naar caller 
LDA45: 	jsr 	BEP_NUL 
		beq     LDA3C 
		jsr     LD804 
		bne     LDA52 
		jmp 	CLR_WRK 
; 
LDA52: 	clc 
		lda 	$59 
		adc 	$61 
		sta 	$59 
		bcc     LDA5D 
		inc 	$58 
LDA5D: 
		sec 
		lda 	$59 
		sbc     #$80 
		sta 	$59 
		bcs     LDA68 
		dec 	$58 
LDA68: 	ldx     #$05 
		ldy 	#$0 
LDA6C: 	lda 	$59,x 
		sta 	$66,x 
		sty 	$59,x 
		dex 
		bne     LDA6C 
		lda 	$57 
		eor 	$5F 
		sta 	$57 
		ldy     #$20 
		jsr     LD6FB 
LDA80: 	lda 	$67 
		bpl     LDA88 
		clc 
		jsr     LD642 
LDA88: 	jsr     LD6FB 
		asl 	$6B 
		rol 	$6A 
		rol 	$69 
		rol 	$68 
		rol 	$67 
		dey 
		bne     LDA80 
LDA98: 	jsr     LD7C8 
LDA9B: 	jsr     LD880 
		lda 	$58 
		beq     LDAAD 
		bpl     LDAA7 
CLR_WRK: ldx     #$08 
; 
		lda #$0 
; 
LDAA8: 	sta 	$56,x 
		dex 
		bne     LDAA8 
LDAAD:  rts 
LDAA7:  brk 
; 
LDAAE: 	jsr     LD831 
		jsr     LDE8D 
LDAB6: 
		jsr 	BEP_NUL 
		beq     LDAA7 
		jsr     LD6C3 
		jsr 	copy_waarde 
		rts 
; 
TAN: 	jsr 	BEP_ARG 
		jsr 	BEP_NUL      ; als ie nul is houdt alles op 
		beq 	tan_exit 
  		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPTAN     ; afdeling tangers 
		jmp		procesfpga
tan_exit: 
		rts 
PUTF2:
		lda		$5F
		eor		#$80
		sta		$62			; eerste argument..
		ldx 	#$5      ; op 0 
LD02:   lda  	$60,x 
		sta 	$B904,x 	; eerste byte;
        dex 
        bne     LD02 
        rts 

;
PUTF:	ldx  #$5      ; op 0 (is het tweede argument..
		lda		$57
		eor		#$80
		sta		$5A
LD01:   lda     $58,x 
		sta 	$B8ff,x 	; eerste byte;
        dex 
        bne     LD01
		rts

GETF:	ldy  	#$0       ; clear waarde 
		sty  	$5E 
		sty  	$58 
		sty  	$57 
		ldy		#$5
LL01:  	lda		$B909,y
		sta  	$58,y   ; sla waarde op 
		ora  	$57   	; test op 0 
		sta  	$57 
		dey      		; index verhogen we 
		bne  	LL01 
		ldx  	$57 
		beq  	LLexit 
		lda  	$5a 
		sta  	$57 
        ORA     #$80 ; hier weer teken gedoe??
		sta  	$5a 
		txa 
LLexit: rts 
		
procesfpga:
		sta 	$B90F
wachtFPGA:
		lda		$B90F	; laadt status van de fpga
		bmi		wachtFPGA
		nop		; hier nog iets met de nan status doen..
		jsr		GETF
		rts		; commando is klaar
 
SQR: 	jsr 	BEP_ARG     ; bepaal waarde argument 
LDB72: 	jsr 	BEP_NUL     ; bepaal waarde argument 
		beq 	ENDSQR     ; bij 0 doen we nix 
		bpl 	sqr_verder  ; boven 0 kan wortel bepaald worden 
		brk      ; daaronder gaan we gillend op ons bek 
sqr_verder: 

		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPSQRT     ; afdeling wortels 
		jmp		procesfpga
ENDSQR:	rts		
; 
LDB9E:  lda     #$D4        ; hier weer een bijdehante waarde 
		bne     LDBAC 
LDBA2: 
		lda     #$CA 
		bne     LDBAC 
LDBA6: 
		lda    	#$CF 
		bne     LDBAC 
LDBAA: 
		lda     #$C5 
LDBAC: 
		sta 	$6F 
		lda     #$03 
		sta 	$70 
		rts 
; 
LOG: 	jsr 	BEP_ARG 
LDBB6:  jsr 	BEP_NUL 
		beq 	te_klein 
		bpl 	log_verder 
te_klein: 
		brk 
log_verder: 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPLOG     ; afdeling logge rakkers 
		jmp		procesfpga
;		rts 
; 
ATN: 	jsr 	BEP_ARG 
LDC67: 	jsr 	BEP_NUL 
		beq 	atn_exit ; bij 0 houdt alles op 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPATN     ; afdeling wortels 
		jmp		procesfpga
atn_exit: 
		rts 
; 
COS: 	JSR 	BEP_ARG 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPCOS     ; afdeling ko en sinas 
		jmp		procesfpga
; 
; .org $DBF0 ; EE
;.res 429, $ea 
 
SIN: 	jsr 	BEP_ARG 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPSIN     ; afdeling ko en sinas 
		jmp		procesfpga
EXP: 	jsr 	BEP_ARG 
LDDD7: 	lda 	$59 
		cmp     #$87 
		bcc     LDDED 
		bne     LDDE5 
		lda $5A 
		cmp     #$B3 
		bcc     LDDED 
LDDE5: 	lda 	$57 
		bpl 	L_tegroot 
		bmi 	LDDED 
		jmp 	CLR_WRK 
; 
L_tegroot: 
		brk 
LDDED: 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPEXP     ; afdeling exponentiele gevallen 
		jmp		procesfpga
;		rts 
; 
HTN: jsr BEP_ARG     ; bepaal hyperbolische tangens 
		jsr		PUTF	; zet de atom waardes klaar voor de fpga
		lda 	#FPHTN     ; afdeling exponentiele gevallen 
		jmp		procesfpga
;		rts 
; 
naar_pc1: 
        rts 
van_pc: 
		rts 
LDE8D: 
		jsr CLR_WRK 
		ldy     #$80 
		sty 	$5A 
		iny 
		sty 	$59 
		rts 
FPINIT: lda 	#$87 
		jsr 	LF802 
        rts 
        JMP     LC55B       ; einde init 
 ;.res 255, $ea 
 ;.res 255, $ea 
 ;.res 255, $ea 
 .res 51, $ea 
 ;fill $ea,255 
 ;fill $ea,255 
 ;fill $ea,51 
 nop 
 ; vanaf hier colour routines 
 lda $52 
 and     #$03 
 tay 
 lda XDF4E,y 
 sta     $03FD 
 lda     $03DA 
 and     #$F0 
 cmp     #$70 
 bne     LDF25 
 lda #$0 
 tay 
LDF1C: 
 sta     $8600,y 
 sta     $8700,y 
 dey 
 bne     LDF1C 
LDF25: 
 lda     $03DA 
 and     #$DF 
 sta     $03DA 
 rol     a 
 rol     a 
 rol     a 
 and     #$03 
 tay 
 lda XDF42,y 
 sta     $03FE 
 lda XDF46,y 
 sta     $03FF 
 jmp LC558 
XDF42: .byte colo1 .MOD 256, colo2 .MOD 256, colo3 .MOD 256, colo4 .MOD 256 
XDF46: .byte colo1/256, colo2/256, colo3/256, colo4/256 
XDF4A: .byte $3F, $cf, $f3, $FC 
XDF4E: .byte $0, $55, $aa, $ff 
colo1: lda $5B 
 ora $5D 
 bne TE_GROOT 
 lda $5A 
 cmp #$40 
 bcs TE_GROOT 
 lsr a 
 lsr a 
 sta $5F 
 ldy #$0 
 sty $60 
 lda #$3F 
 sec 
 sbc $5C 
 cmp     #$40 
 bcc     LDFBE 
 rts 
; 
colo2: lda $5B 
 ora $5D 
 bne TE_GROOT 
 lda $5A 
 bmi TE_GROOT 
 lsr     a 
 lsr     a 
 sta $5F 
 lda     #$3F 
 sec 
 sbc $5C 
 cmp     #$40 
 bcc     LDFB7 
 rts 
; 
colo3: lda $5B 
 ora $5D 
 bne TE_GROOT 
 lda $5A 
 bmi TE_GROOT 
 lsr     a 
 lsr     a 
 sta $5F 
 lda     #$5F 
 sec 
 sbc $5C 
 cmp     #$60 
 bcc     LDFB7 
TE_GROOT: 
 rts 
; 
colo4: lda $5B 
 ora $5D 
 bne TE_GROOT 
 lda $5A 
 bmi TE_GROOT 
 lsr     a 
 lsr     a 
 sta $5F 
 lda     #$BF 
 sec 
 sbc $5C 
 cmp     #$C0 
 bcs TE_GROOT 
LDFB7: 
 ldy #$0 
 sty $60 
 asl     a 
 rol $60 
LDFBE: 
 asl     a 
 rol $60 
 asl     a 
 rol $60 
 asl     a 
 rol $60 
 asl     a 
 rol $60 
 adc $5F 
 sta $5F 
 lda $60 
 adc     #$80 
 sta $60 
 lda $5A 
 and     #$03 
 tax 
 lda XDF4A,x 
 ldx $5E 
 dex 
 beq     LDFF0 
 dex 
 beq     LDFE9 
 and ($5F),y 
 sta ($5F),y 
 rts 
; 
LDFE9: 
 eor     #$FF 
 eor ($5F),y 
 sta ($5F),y 
 rts 
; 
LDFF0: 
 tax 
 and ($5F),y 
 sta ($5F),y 
 txa 
 eor     #$FF 
 and     $03FD 
 ora ($5F),y 
 sta ($5F),y 
 rts 
; 
  