; LC256 OPL3-Test
; for ACME assembling by Vossi 09/2024, last update 10/2024
; v1.0 initial
!cpu 65c02
!ct pet		; Standard text/char conversion table -> pet = petscii
!to "opl3test", cbm
;switches
FILL 	= $00
; ******************************************* INFO ************************************************
; OPL3 doesn't need a pause between reg. writes and only 0,23us after a data write - no NOPs needed!
;
;Music from - http://www.sheetmusic2print.com/Media/Bach/Invention-13-BWV-784.gif
;
;*** $B0 - $B8 - bits 3, 4, 5 
;* Set frequency block. $00 is lowest, $07 is highest. This determines the frequency interval between notes. ***
;* 0 - 0.048 Hz, Range: 0.047 Hz ->   48.503 Hz
;* 1 - 0.095 Hz, Range: 0.094 Hz ->   97.006 Hz
;* 2 - 0.190 Hz, Range: 0.189 Hz ->  194.013 Hz
;* 3 - 0.379 Hz, Range: 0.379 Hz ->  388.026 Hz
;* 4 - 0.759 Hz, Range: 0.758 Hz ->  776.053 Hz
;* 5 - 1.517 Hz, Range: 1.517 Hz -> 1552.107 Hz
;* 6 - 3.034 Hz, Range: 3.034 Hz -> 3104.215 Hz
;* 7 - 6.069 Hz, Range: 6.068 Hz -> 6208.431 Hz
;
;***  Chart for Note Data, etc  - Not Complete, but what is used in this test. ***
;Note	 Freq	Block	HEX     10 bit	Start HZ Frq Spacing 
;C3	     130.81	4	    AB	    171		0.759	 0.758
;C#3/Db3  138.59	4	    B6	    182		0.759	 0.758
;D3	     146.83	4	    C0	    192		0.759	 0.758
;D#3/Eb3  155.56	4	    CC	    204		0.759	 0.758
;E3	     164.81	4	    D8	    216		0.759	 0.758
;F3	     174.61	4	    E5	    229		0.759	 0.758
;F#3/Gb3  185	4	    F3	    243		0.759	 0.758
;G3	     196	4	   101	    257		0.759	 0.758
;G#3/Ab3  207.65	4	   111	    273		0.759	 0.758
;A3 	     220	4	   121	    289		0.759	 0.758
;A#3/Bb3  233.08	4	   132	    306		0.759	 0.758
;B3	     246.94	4	   144	    324		0.759	 0.758
;C4	     261.63	4	   158	    344		0.759	 0.758
;C#4/Db4  277.18	4	   16C	    364		0.759	 0.758
;D4	     293.66	4	   182	    386		0.759	 0.758
;D#4/Eb4  311.13	4	   199	    409		0.759	 0.758
;E4	     329.63	4	   1B1	    433		0.759	 0.758
;F4	     349.23	4	   1CB	    459		0.759	 0.758
;F#4/Gb4  369.99	4	   1E6	    486		0.759	 0.758
;G4	     392	4	   203	    515		0.759	 0.758
;G#4/Ab4  415.3	4	   222	    546		0.759	 0.758
;A4	     440	4	   243	    579		0.759	 0.758
;A#4/Bb4  466.16	4	   265	    613		0.759	 0.758
;B4	     493.88	4	   28A	    650		0.759	 0.758
;C5	     523.25	4	   2B0	    688		0.759	 0.758
;C#5/Db5  554.37	4	   2D9	    729		0.759	 0.758
;D5	     587.33	4	   305	    773		0.759	 0.758
;D#5/Eb5  622.25	4	   333	    819		0.759	 0.758
;E5	     659.25	4	   364	    868		0.759	 0.758
;F5	     698.46	4	   397	    919		0.759	 0.758
;F#5/Gb5  739.99	4	   3CE	    974		0.759	 0.758
;G5	     783.99	5	   204	    516		1.517	 1.517
;G#5/Ab5  830.61	5	   223	    547		1.517	 1.517
;A5	     880	5	   243	    579		1.517	 1.517
;A#5/Bb5	 932.33	5	   266	    614		1.517	 1.517
;B5	     987.77	5	   28A	    650		1.517	 1.517
;
; ****************************************** DEFINES **********************************************
NOTEON	= %00100000
NOTEOFF	= %11011111
; ***************************************** ZEROPAGE **********************************************
!addr counter	= $05
; ***************************************** ADDRESSES *********************************************
; ROM / RAM addresses
!addr	opl	= $df00
; **************************************** BASIC LOADER *******************************************
!initmem FILL
!zone basic
*= $0401
!byte $0c,$04,$0a,$00,$9e,$20,$31,$30,$33,$38,$00,$00,$00	; 10 SYS 1038
; ***************************************** ZONE MAIN *********************************************
!zone main
*= $040e
; main code
start:	sei
	lda #$05			; setup OPL3 mode
	sta opl+2
	nop
	lda #$01
	sta opl+1

	ldx #0
init:	lda chan01,x
	sta opl
	nop
	lda ins01,x
	sta opl+1
	inx
	cpx #ins01-chan01
	bne init

again:	lda #$c0			; channel 1
	sta opl
	nop
	lda #$10
	sta opl+1

	jsr play


	lda #$c0			; channel 2
	sta opl
	nop
	lda #$20
	sta opl+1

	jsr play

	lda #$c0			; channel 1+2
	sta opl
	nop
	lda #$30
	sta opl+1

	jsr play

	jmp again
; ******************************************** PLAY ***********************************************
play:
	ldx #0

loop:	lda #$a0
	sta opl
	nop
	lda snotedata,x
	sta opl+1
	
	lda #$b0
	nop
	sta opl
	lda nblock,x
	asl
	asl
	beq +
	ora #NOTEON			; note on
+	ora lnotedata,x
	sta opl+1

	lda nlength,x			; length
	ldy #0
	sty counter
-	nop
	nop
	dey
	bne -
	dec counter
	bne -
	sec
	sbc #1
	bne -

	ldy #$b0
	sty opl
	nop
	and #NOTEOFF
	sta opl+1

	inx
	cpx #lnotedata-snotedata
	bne loop

	rts
; ******************************************** DATA ***********************************************
; sound data
!zone data
chan01:	!byte $20,$40,$60,$80,$c0,$e0,$23,$43,$63,$83,$e3,$bd

ins01:	!byte $01,$10,$f0,$77,$00,$00,$01,$00,$f0,$77,$00,$00
;ins01:	!byte $31,$00,$f1,$85,$0a,$00,$31,$8b,$f4,$15,$00,$00
;ins01:	!byte $41,$80,$a3,$13,$0c,$00,$85,$59,$d8,$72,$00,$c0
;ins01:	!byte $41,$00,$a3,$13,$0c,$00,$85,$00,$d8,$72,$00,$00

snotedata:				; Bottom eight bits of note data
	!byte $00,$d8,$21,$58,$44,$d8,$44,$82,$58,$b1,$11,$b1,$21,$d8,$21,$58,$44,$d8,$44,$82,$58,$21,$00
lnotedata:				; Top two bits of note data
	!byte $00,$00,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01,$00
nblock:					; "octave" block
	!byte $00,$03,$03,$04,$03,$03,$03,$04,$04,$04,$03,$04,$03,$03,$03,$04,$03,$03,$03,$04,$04,$03,$00
nlength:				; 1/16th note base 2x for 1/8th etc...
	!byte $02,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$04
