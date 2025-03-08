; LC256 Sound-Test
; for ACME assembling by Vossi 09/2024, last update 10/2024
; v1.0 initial
!cpu 65c02
!ct pet		; Standard text/char conversion table -> pet = petscii
!to "dac4khz", cbm
;switches
FILL 	= $00
DELAY 	= 1		; 480Hz sinewave1, 4kHz sinewave8, 8kHz sinewave16
;DELAY	= (180-35)/2	; 40Hz (sinewave1 - 256 values)
;DELAY	= (180-35)/5	; 22kHz Samplingrate
; ***************************************** ZEROPAGE **********************************************
!addr pointer	= $05
; ***************************************** ADDRESSES *********************************************
; ROM / RAM addresses
!addr	dac	= $df40
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
	lda #<SoundData
	sta pointer
	lda #>SoundData
	sta pointer+1
	ldy #0

loop:	lda (pointer),y
	sta dac

	ldx #DELAY
-	dex
	bne -

	inc pointer
	bne +
	inc pointer+1
+	lda pointer+1
	cmp #>SoundDataEnd
	bne loop
	lda pointer
	cmp #<SoundDataEnd
	bne loop

;	lda #12
;-	dex
;	bne -
;	dey
;	bne -
;	sec
;	sbc #1
;	bne -
	beq start
; ******************************************** DATA ***********************************************
; sound data
!zone data
SoundData:
;	!set .t = 2000
;	!do while .t > 0{
;		!byte $80,$c0,$e8,$ff,$e8,$c0,$80,$40,$18,$00,$18,$40
;		!byte $80,$b0,$d0,$e8,$d0,$b0,$80,$50,$30,$18,$30,$50		; single supply >=$18
;		!set .t = .t -1}
;!binary "tada.raw"
;!source "sinewave1.b"
!source "sinewave8.b"
SoundDataEnd:
	!byte $00
