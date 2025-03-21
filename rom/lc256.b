; LC256 ROM
; for ACME assembling by Vossi 09/2024, last update 3/2025
; v1.0 initial
; v1.1 added yjk mode
!cpu 65c02
!ct pet		; Standard text/char conversion table -> pet = petscii
!to "kernal.bin", plain
; * switches
FILL	= $ff	; Fills free memory areas
PI	= $99
HZ = 64		; System interrupt frequency, at 4MHz clock is minimum 62 because of 16 bit timer!
		;   if other then 64 the 1/8 seconds for TI$ are not correct!
V10PCB = 0	; select first PCB v.1.0: only 2x2 bit RAM banks (max. 128kB chips)
V11PCB128K = 0	; select PCB v.1.1+ with 128kB RAM1 chip
NMI555 = 0	; enable for 555 Restore-key NMI, disable for VIA2 CA1 NMI
JIFFY = 1	; enable Jiffy-DOS 6.01 (Works at 1, 2, 3 or 4 MHz clock)
DISDEF	= 23	;bytes to disassemble by default

NORAM1 = 0	; Test option without RAM1: screen+color RAM in RAM0 range
; shared VDP sources
!source "lc256_vdpeq.b" 
!source "lc256_vdpmacro.b" 
; ########################################### INFO ################################################
; loop3 = Main loop - wait for key input
; Disable scrolling with control '-' ($82), enable '+' ($02)
;
; current memory map:
;allways bank0:	$0000-$03ff  RAM (kernal/basic system)
;		$0400-$7fff  RAM (banks 0-15)
;		-$7d00-$7dff programmable keys
;		-$7e00-$7fff Sprite colors
;               $8000-$bfff  ROM (language)
;		$c000-$c7ff  ROM (kernal/basic)
;		$c800-$cfff  ROM Character
;		$d000-$d7ff  kernal screen shadow ram
;		$d800-$dbff  kernal color shadow ram
;               $dc00-$dcff  i/o  VDP
;               $dd00-$dd7f  i/o  USB
;               $dd80-$ddff  i/o  unused
;		$de00-$de3f  i/o  6522 VIA1
;               $de40-$de7f  i/o  6522 VIA2
;               $de80-$debf  i/o  OPL3
;               $dec0-$deff  i/o  Audio DAC
;               $df00-$df3f  i/o  IO3
;               $df40-$df7f  i/o  IO4
;               $df80-$dfbf  i/o  IO5
;               $dfc0-$dfff  i/o  IO6
;		$e000-$ffff  ROM  kernal
; ***************************************** ZEROPAGE **********************************************
; $02-$8f BASIC zeropage
; $6f-$78 monitor buffer in Basic fac
!addr	hulp		= $6f ; 10by	; Monitor: assembler buffer
!addr	txtptr		= $83 ; 2by	; BASIC pointer to current term
; Kernal page zero variables
; Kernal indirect address variables
!addr	tick		= $01		; 1/64 time
!addr	fnadr		= $90 ; 2by	; Address of file name string
!addr	sal		= $92		; Current load/store address
!addr	sah		= $93		;   low, high
!addr	eal		= $94		; End of load/save
!addr	eah		= $95
!addr	stal		= $96		; Start of load/save
!addr	stah		= $97
; Frequently used kernal variables
!addr	time		= $98 ; 3by	; 24 hour clock in 1/64th seconds
!addr	status		= $9b		; I/O operation status
!addr	fnlen		= $9c		; File name length
!addr	la		= $9d		; Current logical index
!addr	fa		= $9e		; Current first address
!addr	sa		= $9f		; Current secondary address
!addr	dfltn		= $a0		; Default input device
!addr	dflto		= $a1		; Default output device
; Variables for kernal speed
!addr	stkey		= $a2		; Stop key flag
; IEC / jiffydos / Monitor
!addr	c3p0		= $a3		; IEC buffer flag
!addr	wrap		= $a3		; Monitor: assembler temp
!addr	r2d2		= $a4		; IEC serial bus usage eoi / jiffy device
!addr	lastchr		= $a4		; Monitor: last basin char 
!addr	bsour		= $a5		; IEC character buffer 
!addr	bsour1		= $a5		; IEC character input
!addr	sxreg		= $a5		; Monitor: temp xreg, div
!addr	count		= $a6		; IEC bit counter / Monitor: assembler
!if JIFFY = 1{
!addr	jdtemp		= $a6		; JIFFY DOS temp (only used in jdload)
}
; Monitor virtual registers - Place in these locations temporarly...
!addr	pch		= $a7		; counter
!addr	pcl		= $a8
!addr	flgs		= $a9		; Processor status
!addr	acc		= $aa		; Accumulator
!addr	xr		= $ab		; X register
!addr	yr		= $ac		; Y register
!addr	sp		= $ad		; Stack pointer
!addr	invh		= $ae		; User interrupt vector
!addr	invl		= $af
; Monitor indirect variables
!addr	tmp0		= $b0 ; 2by	; Temp pointer
!addr	tmp1		= $b2 ; 2by	; Temp pointer
!addr	tmp2		= $b4 ; 2by	; Temp pointer
; Other monitor variables
!addr	tmpc		= $b6		; Place to save last cmd
; *** defined in shared lc256_vdpeq.b
;!addr	ddisk		= $b7		; Default disk unit # for monitor/basic
; Monitor Assembler variables
!addr	msal		= $b8 ; 3by	; Monitor: assembler
!addr	format		= $bb		; Monitor: assembler
!addr	length		= $bc		; Monitor: assembler 
; Screen editor page zero variables
; Editor indirect variables
!addr	pkybuf		= $bd ; 2by	; Start adr of pgm key
!addr	keypnt		= $bf ; 2by	; Current pgm key buf
; *** defined in shared lc256_vdpeq.b
;!addr	sedsal		= $c1 ; 2by	; Scroll ptr / pgmkey ptr
;!addr	sedeal		= $c3 ; 2by	; Scroll ptr / pgmkey ptr
!addr	pnt		= $c5 ; 2by	; Current character pointer
; Screen editor usage
!addr	bitabl		= $c7 ; 4by	; Wrap bitmap
!addr	user		= $cb ; 2by	; Pointer to color RAM
!addr	tcolor		= $cd		; Temporary color
!addr	crssw		= $ce		; Cursor visibility switch
; *** defined in shared lc256_vdpeq.b
;!addr	color		= $cf		; Character color
!addr	gdcol		= $d0		; Color behind cursor
; Editor variables for speed & size
!addr	tblx		= $d1		; Cursor line
!addr	pntr		= $d2		; Cursor column
!addr	rptflg		= $d3		; Repeat flag
!addr	lstx		= $d4		; Last character index
!addr	lstp		= $d5		; Screen editor start position
!addr	lsxp		= $d6		; Screen editor start row
!addr	crsw		= $d7		; cr flag - cr pressed -> input from screen
!addr	ndx		= $d8		; Index to keyd queue
!addr	qtsw		= $d9		; Quote mode flag
!addr	insrt		= $da		; Insert mode flag
!addr	config		= $db		; Char before blink (petii)
!addr	indx		= $dc		; Last byte position on line (##234-02##244-02)
!addr	kyndx		= $dd		; Count of program key string
!addr	kount		= $de		; Delay between key repeats
!addr	delay		= $df		; Initial key repeat delay
!addr	sedt1		= $e0		; Frequently used temp variables
!addr	sedt2		= $e1
; Frequently used editor variables
!addr	data		= $e2		; Current print data
!addr	crson		= $e3		; Cursor on = 0
!addr	keytab		= $e4 ; 2by	; Keyscan table indirect
!addr	lstshf		= $e6		; Last shift pattern
!addr	shflag		= $e7		; Keyscanner shift/control flags ($ff-nokey)
!addr	sfdx		= $e8		; Keyscanner normal key number ($ff-nokey)
!addr	saver		= $e9		; Temp store for output char
; $ea - $ff VDP zero page
; *** defined in shared lc256_vdpeq.b
; ***************************************** ABSOLUTE **********************************************
; System stack area
!addr	stack		= $0100		; Stack
!addr	stackp		= $01ff		; System Stack pointer transx code
; -------------------------------------------------------------------------------------------------
; $200 - $256 Basic's ROM page work area
!addr	buf		= $0200		; Basic input buffer
; Basic RAM vectors
!addr	ierror		= $0280         ; Basic error indirect
; -------------------------------------------------------------------------------------------------
; System RAM vectors
!addr	cinv		= $0300		; IRQ vector
!addr	cbinv		= $0302		; BRK vector
!addr	nminv		= $0304		; NMI vector
!addr	iopen		= $0306		; Open file vector
!addr	iclose		= $0308		; Close file vector
!addr	ichkin		= $030a		; Open channel in vector
!addr	ickout		= $030c		; Open channel out vector
!addr	iclrch		= $030e		; Close channel vector
!addr	ibasin		= $0310		; Input from channel vector 
!addr	ibsout		= $0312		; Output to channel vector
!addr	istop		= $0314		; Check stop key vector
!addr	igetin		= $0316		; Get from queue vector
!addr	iclall		= $0318		; Close all files vector
!addr	iload		= $031a		; Load from file vector
!addr	isave		= $031c		; Save to file vector
!addr	usrcmd		= $031e		; Monitor extension vector
!addr	escvec		= $0320		; User ESC key vector
!addr	ctlvec		= $0322		; unused control key vector
!addr	isecnd		= $0324		; IEC listen secondary address
!addr	itksa		= $0326		; IEC talk secondary address
!addr	iacptr		= $0328		; IEC character in routine
!addr	iciout		= $032a		; IEC character out routine
!addr	iuntlk		= $032c		; IEC bus untalk
!addr	iunlsn		= $032e		; IEC bus unlisten
!addr	ilistn		= $0330		; IEC listen device primary address
!addr	italk		= $0332		; IEC talk device primary address
; Kernal absolute variables
!addr	lat		= $0334 ; 10by	; Logical file numbers / table
!addr	fat		= $033e ; 10by	; Device numbers / table
!addr	sat		= $0348 ; 10by	; Secondary addresses / table
;
!addr	lowadr		= $0352 ; 2by	; Start of system memory: low, high
!addr	hiadr		= $0354 ; 2by	; Top of system memory: low, high
!addr	memstr		= $0356 ; 2by	; Start of user memory: low, high
!addr	memsiz		= $0358 ; 2by	; Top of user memory: low, high
!addr	timout		= $035a		; IEC timeout enable
!addr	verck		= $035b		; load/verify flag
!addr	ldtnd		= $035c		; Device table index
!addr	msgflg		= $035d		; Message flag
!addr	relsal		= $035e		; moveable start load address
!addr	relsah		= $035f		; 
!addr	relsab		= $0360		;   bank
; *** defined in shared lc256_vdpeq.b
; $0361-$0380 VDP sprites
!addr	xcnt		= $0361 ; 32by	; Monitor: 32 bytes compare buffer
; Screen editor absolute
!addr	pkyend		= $0381 ; 2by	; Program key buffer end address
!addr	keysiz		= $0383 ; 8by	; Sizes of function key texts 8 bytes
; *** defined in shared lc256_vdpeq.b
; $038b-$0390 VDP absolute
; Kernal temporary (local) variables
!addr	t1		= $0391
!addr	t2		= $0392
!addr	xsav		= $0393
!addr	savx		= $0394
!addr	svxt		= $0395
!addr	temp		= $0396		; allround temp - not used by kernal
; Screen editor absolute
!addr	rvs		= $0397		; Reverse mode flag
!addr	lintmp		= $0398		; Line # between in and out 
!addr	lstchr		= $0399		; Last char printed
!addr	insflg		= $039a		; Insert mode flag
!addr	scrdis		= $039b		; Scroll disable flag
!addr	bitmsk		= $039c		; Temorary bitmask
!addr	fktmp		= $039c		;   also used for function key temporary
!addr	keyidx		= $039d		; Index to programmables
!addr	pagsav		= $039e		; Temp RAM page
!addr	keyd		= $039f ; 10by	; Keyboard buffer 10 bytes
!addr	funvec		= $03a8 ; 2by	; Vector: funktion key handler
*= $03aa
!addr	ldtb1		*=*+25	; 50by	; screen lines hi byte table
!addr	ldtb2				; screen lines lo byte table
; $03dc - $03f7 free
; System warm start variables and vectors
!addr	evect		= $03f8 ; 5by	; Warm start vector and flags
; ***************************************** EQUATES ***********************************************
; Equates
	warm	= $a5		; Warm start flag
	winit	= $5a		; Initialization complete flag
	collen	= 40		; color ram line length
	scymax	= nrows-1	; Max line number
	keymax	= 9		; Keyboard buffer size - 1
	pgmkys	= 8		; Number of progam keys
	cr	= $d		; Carriage return
	sperr	= $10		; verify error
; *** defined in shared lc256_vdpeq.b
;	mxbank	=15+1		; 1st RAM bank out of range
; **************************************** ADDRESSES **********************************************
; ROM / RAM addresses
!addr	zp	= $00		; zeropage start
!addr	basic	= $8000		; Start of ROM (language)
!addr	font6x8	= $c800		; c64 font 6x8
!if NORAM1 = 1{			; Test option without RAM1: screen+color RAM in RAM0 range
!addr	scnram	= $7000		; kernal Video shadow RAM
!addr	clrram	= $7800		; kernal Color shadow RAM
} else{
!addr	scnram	= $d000		; kernal Video shadow RAM
!addr	clrram	= $d800		; kernal Color shadow RAM
}
!addr	kernal	= $e000		; Start of ROM (kernal)
; USB
!addr	usb	= $dd00		; FTDI USB chip
; OPL3 ports
!addr 	OPL3Adr	= $de80		; Port#0 Reg, #1 Data, #2 Reg high
!addr	oplreg	= OPL3Adr
!addr	opldata	= OPL3Adr+1
!addr	oplregh	= OPL3Adr+2
; Audio DAC
!addr	dac	= $dec0	; write only

; 6522 VIA1 - keyboard, control ports
!addr	via1	= $de00
	; pra: kybd col out 0-7, joystick 2
	; prb: kybd row in 0-7 (inputs), joystick 1

; 6522 VIA2 - MMU, IEC, USB RXF/TXE, Restore
!addr	via2	= $de40
	; ca1: restore-key in
	; ca2: MMU ROML
	; cb2: MMU ROMH
	; pa0: USB #rxf
	; pa1: USB #txe
	; pa2: (27c512 ROM: 32K bank 0/1)
	; pa3: IEC atn out
	; pa4: IEC clk out
	; pa5: IEC data out
	; pa6: IEC clk in
	; pa7: IEC data in
	; pb0: RAM0 bank bit #0 
	; pb1: RAM0 bank bit #1 
	; pb2: RAM0 bank bit #2 
	; pb3: RAM0 bank bit #3
	; pb4: RAM1 bank bit #0 
	; pb5: RAM1 bank bit #1 
	; pb6: RAM1 bank bit #2 
	; pb7: RAM1 bank bit #3

; VIA register
	prb	= $0	; Port reg b
	prah	= $1	; Port reg a (with handshaking)
	ddrb	= $2	; Direction reg b
	ddra	= $3	; Direction reg a
	t1lo	= $4	; Timer 1 low  byte
	t1hi	= $5	; Timer 1 high byte
	t1ldlo	= $6	; Timer 1 low  byte to load
	t1ldhi	= $7	; Timer 1 high byte to load
	t2lo	= $8	; Timer 2 low  byte
	t2hi	= $9	; Timer 2 high byte
	sr	= $a	; shift register
	acr	= $b	; auxilary control register
	pcr	= $c	; peripheral control register
	ifr	= $d	; Interrupt flags register
	ier	= $e	; Interrupt enable register
	pra	= $f	; Port reg a
; ******************************************* MACROS **********************************************
; IEC macro
!macro IECDelay .c{		; *** insert .c nops's
	!do while .c > 0{
		nop
		!set .c = .c -1}
}
; ***************************************** ZONE VDP **********************************************
!initmem FILL			; All unused memory filled with $ff
!zone vdp
*= $c000
!binary "lc256_logo.rgb"
; ************************************* MONITOR EXTENSION *****************************************
; monitor extension
!zone monext
;  simple assembler
;  syntax: a 1111 lda ($00,x)
; 	   a 1111 dex:		(':' = terminator)
assem:	jsr parse		; read adr
	bcs aserr		; ...branch if missing sa
	jsr t2t2		; save sa

asspc:	ldx #0
	stx hulp+1		; clear left mnemonic
	stx count		; clear operand

asnxchr:jsr gnc			; get a char
	bne aspars		; check for eol
	cpx #0
	bne aspars
	rts			; if eol & no mnemonic, exit cleanly

aspars:	cmp #' '		; is it a space ?
	beq asspc		; yes - start again (passes over object code, if any)
	sta msal,x		; no - save char
	inx
	cpx #3	  		; got three chars ?
	bne asnxchr		; no - loop
; 3 char mnemonic in msal -> crunch to two bytes in hulp, hulp+1
asnxmch:dex			; squished all three ? (3x5bits in hulp,hulp+1 bit#15-1)
	bmi asstart		; yes
	lda msal,x		; no - first in last out
	sec	     		; no borrow
	sbc #$3f		; normalize
	ldy #5	  		; set for 5 shift rights

ashftmn:lsr
	ror hulp+1		; left mnemonic
	ror hulp		; right mnemonic
	dey	     		; done 5 shifts?
	bne ashftmn		; no-loop
	beq asnxmch		; always

aserr:	jmp erropr		; syntax error jump
; assemble
asstart:ldx #2			; move output buffer index past crunched mnemonic

asoplp:	lda count		; after first number copy everything else to output buffer
	bne asopend
	jsr eval		; evaluate next parameter, if number crunch it
	beq asoptxt		; ...branch if not a number

	lda #'$'
	sta hulp,x		; buffer a number, either '$00' or '$0000'
	inx
	ldy #4
	cpy count
	beq asop2by		; ...branch to force absolute	
asopchk:lda tmp0+1
	bne asop2by		; ...branch if 2-byte field
	ldy #2			; else set up  1-byte field

asop2by:lda #'0'
asopzlp:sta hulp,x
	inx
	dey
	bne asopzlp

asoptxt:jsr glc			; re-get last character
	jmp asopen1
asopend:jsr gnc			; copy rest of input buffer to output buffer
asopen1:beq asopeol		; ...branch if eol
	cmp #' '
	beq asoplp		; ...squish out spaces
	sta hulp,x		; hopefully it's of one of these:   #,()
	inx
	cpx #10
	bcc asoplp		; ...loop until eol or
	bcs aserr		; ...buffer overflow

asopeol:stx tmp1		; save input # of characters
	ldx #0
	stx wrap		; start trial at zero

as110:  ldx #0
	stx temp		; disa index=0
	lda wrap		; get trial byte
	jsr dset		; digest it
	ldx format		; save format for later
	stx tmp1+1
	tax	     		; index into mnemonic table
	lda mnemr,x		; get compressed
	jsr tstrx		; mnemonic and test
	lda mneml,x
	jsr tstrx
	ldx #6	  		; six format bits

as210:  cpx #3
	bne as230
	ldy length
	beq as230		; skip-single byte instr

as220:  lda format
	cmp #$e8		; a relative instr?
	lda #'0'		; test zeros
	bcs as250		; no-3 byte
	jsr tst2		; test a byte,2 chars
	dey
	bne as220

as230:  asl format
	bcc as240
	lda char1-1,x
	jsr tstrx		; test syntax
	lda char2-1,x
	beq as240
	jsr tstrx		; test more syntax

as240:  dex
	bne as210
	beq as300

as250:  jsr tst2		; test a word,4 chars
	jsr tst2

as300:  lda tmp1	  	; check # chars of both
	cmp temp
	beq as310		; match, skip
	jmp tst05		; fail

as310:  ldy length
	beq as500		; if only 1 byte instr skip
	lda tmp1+1		; get saved format
	cmp #$9d		; a relative instr?
	bne as400		; no-skip

	lda tmp0	  	; calculate a relative
	sbc tmp2	  	; (.c=1 already)
	tax			; save 'diff'
	lda tmp0+1
	sbc tmp2+1
	bcc as320		; ...taken if a negative branch

	bne aerr		; positive branch, out of range
	cpx #$82
	bcs aerr
	bcc as340

as320	tay			; negative branch
	iny	     		; out of range, y=$ff
	bne aerr
	cpx #$82
	bcc aerr
as340:  dex	     		; subtract 2 from 'diff' for instr
	dex
	txa
	ldy length		; set index to length
	bne as420		; branch always

as400:	lda tmp0-1,y		; no-put byte out there

as420:	sta (tmp2),y		; put byte
	dey
	bne as400

as500: 	lda wrap		; get good op code
	sta (tmp2),y		; put byte
	jsr crlf		; get ready to overstrike line
	lda #$91		; cursor up
	jsr bsout
	lda #'a'
	jsr bsout
	jsr space	
	jsr dis400		; disassemble one line

	inc length
	lda length
	jsr addt2		; update address

	lda #'a'		; set up next line with 'a bnnnn ' for convenience
	sta keyd		; put it in the keyboard buffer
	lda #' '
	sta keyd+1
	sta keyd+6
	lda tmp2+1		; next get mid byte of address
	jsr makhex
	sta keyd+2		; ..and put in buffer,
	stx keyd+3
	lda tmp2		; then get the low byte of address,
	jsr makhex
	sta keyd+4		; ..and put that in the buffer, too.
	stx keyd+5
	lda #7			; store 7 keys in buffer
	sta ndx
	rts

aerr:	jmp erropr		; syntax error jump

;  test char in .a with char in hulp
tst2:	jsr tstrx		; test for '00' (do two tests)

tstrx:	stx sxreg
	ldx temp		; get current position
	cmp hulp,x		; same char
	beq tst10		; yes-skip
	pla	     		; pull jsr off stack
	pla

tst05:	inc wrap		; try next trial
	beq aerr		; =0 tried all,sorry
	jmp as110

tst10:	inc temp
	ldx sxreg		; restore x
	rts
; -------------------------------------------------------------------------------------------------
; Mini disassembler
;   syntax: d ssss eeee = start end (optional)
;           d (only) continues 
disasm:	jsr parse		; get sa
	bcs dishpag		; use a default length from current sa
	jsr t2t2		; sa to tmp2
	jsr parse
	bcc disto		; got sa,ea. use 'em

dishpag:lda #DISDEF		; guess at 1/2 page
	sta tmp0
	lda #0
	sta tmp0+1
	beq dislp		; always		

disto:	jsr sub0m2    		; put ea-sa in tmp0
	bcc diserr		; ...branch if sa > ea

dislp:  jsr crlf		; print <cr>
	jsr stop
	beq disx		; ...branch if user requests abort
	jsr dis300    		; disassemble 1 line
	inc length
	lda length
	jsr addt2
	lda length
	jsr subt0
	bcs dislp

disx:	rts

diserr:	jmp erropr		; syntax error jump

dis300:	lda #'.'
	jsr bsout
	jsr space

dis400:	ldx tmp2+1
	ldy tmp2
	jsr putwrd		; write start address
	jsr space
	ldy #0
	lda (tmp2),y		; get a byte from memory
	jsr dset		; get instr & digest it

	pha			; dump (length+1) bytes
	ldx length		; (.y=0 from 'dset' above)
	inx

pradr0:	dex
	bpl pradrl		; pad non-printers
	jsr space		; print 3 spaces
	jsr space
	jsr space
	jmp pradrm

pradrl:	lda (tmp2),y
	jsr puthxs		; write hex byte

pradrm:	iny
	cpy #3
	bcc pradr0
	pla

	ldx #3
	jsr prmne		; print mnemonic
	ldx #6	  	 	; 6 format bits

pradr1:	cpx #$03
	bne pradr3     		; if x=3 print adr val
	ldy length
	beq pradr3    	 	; no print if len=0

pradr2:	lda format
	cmp #$e8 		; relative addressing mode?
	php			; save carry
	lda (tmp2),y
	plp
	bcs reladr
	jsr puthex		; write hex byte
	dey
	bne pradr2

pradr3:	asl format		; test next format bit
	bcc pradr4		; no print if=0
	lda char1-1,x
	jsr bsout
	lda char2-1,x
	beq pradr4
	jsr bsout

pradr4:	dex
	bne pradr1
	rts

reladr: jsr pcadj3		; pcl,h + disp + 1 into a,x
	clc	     	 	; add 1
	adc #1
	bne relad2
	inx

relad2:	pha
	txa
	jsr puthex		; print byte in .a as two hex digits
	pla
	jsr puthxs

pcadj3:	ldx tmp2+1
	tay
	bpl pcadj4
	dex

pcadj4:	adc tmp2
	bcc pcrts
	inx

pcrts:	rts

; disassembler digest routine
dset:	tay
	lsr			; even/odd test
	bcc ieven
	lsr			; test b1
	bcs err			; xxxxxx11 instr bad
	cmp #$22
	beq err			; 10001001 instr bad
	and #$07		; mask 3 bits for adr mode
	ora #$80 		; add indexing offset

ieven:	lsr			; left/right test
	tax
	lda nmode,x		; index into mode table
	bcs rtmode		; if carry set use lsb for
	lsr			; print format index
	lsr
	lsr			; if carry clr use msb
	lsr

rtmode:	and #$0f		; mask for 4-bit index
	bne getfmt		; $0 for bad opcodes

err:	ldy #$80		; sub $80 for bad opcode
	lda #0			; set format index to zero

getfmt:	tax
	lda nmode2,x		; index into prt format tab
	sta format		; save for adr field format
	and #3			; mask 2-bit length. 0=1byte
	sta length		; 1=2byte,2=3byte
	tya			; op code
	and #$8f		; mask for 1xxx1010 test
	tax			; save in x
	tya			; op code again
	ldy #3
	cpx #$8a
	beq mnndx3

mnndx1:	lsr
	bcc mnndx3		; form index into mnemonic tab
	lsr

mnndx2:	lsr			; 1xxx1010->00101xxx
	ora #$20		; xxxyyy01->00111xxx
	dey			; xxxyyy10->00110xxx
	bne mnndx2		; xxxyy100->00100xxx
	iny			; xxxxx000->000xxxxx

mnndx3:	dey
	bne mnndx1
	rts			; (.y=0 is assumed!)

; print mnemonic
; enter x=3 characters
prmne:	tay
	lda mneml,y		; fetch 3 char mnemonic
	sta tmp1
	lda mnemr,y
	sta tmp1+1

prmn1:	lda #0
	ldy #5

prmn2:	asl tmp1+1		; shift 5 bits of char
	rol tmp1		; into a
	rol			; clear carry
	dey
	bne prmn2
	adc #$3f		; add '?' offset
	jsr bsout
	dex
	bne prmn1
	jmp space		; finish with space

; assembler - disassembler decoding tables
nmode:	!byte $40,2,$45,3
	!byte $d0,8,$40,9
	!byte $30,$22,$45,$33
	!byte $d0,8,$40,9
	!byte $40,2,$45,$33
	!byte $d0,8,$40,9
	!byte $40,$02,$45,$b3
	!byte $d0,$08,$40,$09
	!byte 0,$22,$44,$33
	!byte $d0,$8c,$44,0
	!byte $11,$22,$44,$33
	!byte $d0,$8c,$44,$9a
	!byte $10,$22,$44,$33
	!byte $d0,8,$40,9
	!byte $10,$22,$44,$33
	!byte $d0,8,$40,9
	!byte $62,$13,$78,$a9

nmode2:	!byte 0,$21,$81,$82
	!byte 0,0,$59,$4d
	!byte $91,$92,$86,$4a
	!byte $85,$9d

char1:	!pet ",),#($"

char2:	!pet "y",0,"x$$",0

mneml:	!byte $1c,$8a,$1c,$23
	!byte $5d,$8b,$1b,$a1
	!byte $9d,$8a,$1d,$23
	!byte $9d,$8b,$1d,$a1
	!byte 0,$29,$19,$ae
	!byte $69,$a8,$19,$23
	!byte $24,$53,$1b,$23
	!byte $24,$53,$19,$a1
	!byte 0,$1a,$5b,$5b
	!byte $a5,$69,$24,$24
	!byte $ae,$ae,$a8,$ad
	!byte $29,0,$7c,0
	!byte $15,$9c,$6d,$9c
	!byte $a5,$69,$29,$53
	!byte $84,$13,$34,$11
	!byte $a5,$69,$23,$a0

mnemr:	!byte $d8,$62,$5a,$48
	!byte $26,$62,$94,$88
	!byte $54,$44,$c8,$54
	!byte $68,$44,$e8,$94
	!byte 0,$b4,8,$84
	!byte $74,$b4,$28,$6e
	!byte $74,$f4,$cc,$4a
	!byte $72,$f2,$a4,$8a
	!byte 0,$aa,$a2,$a2
	!byte $74,$74,$74,$72
	!byte $44,$68,$b2,$32
	!byte $b2,0,$22,0
	!byte $1a,$1a,$26,$26
	!byte $72,$72,$88,$c8
	!byte $c4,$ca,$26,$48
	!byte $44,$44,$a2,$c8
; -------------------------------------------------------------------------------------------------
; transfer memory
;   syntax: t ssss eeee tttt = start end target
compar:	lda #0		   	; flag 'compare'
	sta fnlen		; diff flag
	!byte $2c		; skip next

trnsfr:	lda #$80		; flag 'transfer'
	sta verck
	jsr range		; get sa in tmp2, calculate length, put in tmp1
	bcs errl		; ...none=error
	jsr parse		; target in tmp0
	bcs errl		; ...none=error
	bit verck
	bmi trnok		; -> transfer
; compare
	jsr crlf	

trnok:	ldy #0
trnlp:	lda (tmp2),y		; get source byte
	bit verck
	bpl trncmp		; -> compare
; transfer
	sta (tmp0),y		; transfer: store target
	jmp trninc		; next
; compare
trncmp: cmp (tmp0),y		; equal?
	beq trninc		; yes -> skip
; report diffs
	ldx tmp2+1
	ldy tmp2
	jsr putwrd		; write diff address source
	sta fnlen
	ldy #0

trninc:	inc tmp0
	bne trnnxt
	inc tmp0+1
	bne trnnxt
	jmp errl		; disallow wrapping around operations

trnnxt:	jsr inct2
	jsr dect1
	bcs trnlp

	bit verck
	bmi hunx		; transfer -> exit
; compare result
	lda fnlen
	bne hunx		; diffs
; ok
	jmp printok		; output ok

errl:	jmp erropr
; -------------------------------------------------------------------------------------------------
; hunt for bytes or string in memory
;   syntax: h ssss eeee "ascii... <or> h ssss eeee 11 22 33 ...   s=start, e=end
hunt:	jsr range		; get sa in tmp2, calculate length, put in tmp1
	bcs errl		; ...none=error
	ldy #0
huntspc:jsr gnc			; get first char
	beq errl		; ...branch if true eol (error)
	cmp #' '		; blank?
	beq huntspc		; span blanks...
	cmp #$22 ; "		; is it an <">
	bne hunhex		; no..  must be hex
	jsr gnc			; yes.. get first string chr
	cmp #0
	beq errl		; ...branch if true eol (error)
; search string
hunnxch:sta xcnt,y
	iny
	jsr gnc			; get next
	beq hunstrt		; yes-end of string
	cpy #32			; no-32 char yet?
	bne hunnxch		; no-get more
	beq hunstrt		; yes-go look for it
; search bytes
hunhex:	jsr pargot

hunnxhx:cmp #2+1
	bcs errl		; > 2 digits
	lda tmp0
	sta xcnt,y
	iny
	jsr parse		; get next byte
	bcs hunstrt		; no more -go look for bytes
	cpy #32			; 32 bytes yet?
	bne hunnxhx		; no-get more

hunstrt:sty verck		; yes-start search
	jsr crlf		; next line

hunlp:	ldy #0

hunstlp:lda (tmp2),y		; get a byte from memory
	cmp xcnt,y
	bne hundiff		; ...branch if no match
	iny
	cpy verck		; checked full string?
	bne hunstlp		; no-check on
; report found
	ldx tmp2+1
	ldy tmp2
	jsr putwrd		; print address found

hundiff:jsr stop
	beq hunx		; ...branch if user requests abort
	jsr inct2
	jsr dect1
	bcs hunlp		; loop if not done

hunx:	rts
; -------------------------------------------------------------------------------------------------
; read & display the disk directory
diskdir:lda #'$'
dirchlp:cpy #16			; max string length
	beq errl		; string too long
	sta (fnadr),y		; store string
	inc fnlen
	iny
	jsr gnc			; get character of string
	bne dirchlp		; no eol -> next char

dirstr:	lda #0			; set logical index
	ldy #$60		; sa for directory
	jsr setlfs
	clc
	jsr open		; open directory channel
	bcs dirdone		; ...branch on error
	ldx #0
	jsr chkin		; make it an input channel

	ldy #3			; first pass only- trash first two bytes read

dirlp:  sty tmp1		; loop counter
dirbklp:jsr basin
	sta tmp0		; get # blocks low
	lda status
	bne dirdone		; ...branch if error, possibly no device?
	jsr basin
	sta tmp0+1		; get # blocks high
	lda status
	bne dirdone		; ...branch if error, possibly no device?
	dec tmp1
	bne dirbklp		; ...loop until done
	jsr crlf		; start a new line
	jsr prtblks		; convert # blocks to decimal and print it
	jsr space		; print space  (to match loaded directory display)

dirfnlp:jsr basin		; read & print filename & filetype
	beq direol		; ...branch if eol
	ldx status
	bne dirdone		; ...branch if error
	jsr bsout
	bcc dirfnlp		; ...loop always

direol:
	jsr stop
	beq dirend		; ...branch if user hit STOP
	ldy #2
	bne dirlp		; ...loop always

dirdone:cpy #3			; checks if first dir byte is cr -> no device
	beq direrr		; no device?
dirend: jmp disk30		; close
direrr:	jmp disk29		; -> error
; -------------------------------------------------------------------------------------------------
; read a range - put sa in t2, count in t1   (save ea in 'temps')
; returns .c=0 if okay, .c=1 if error (missing parameter or sa < ea)
range:  jsr parse
	bcs rangex		; ...branch if missing sa
	jsr t2t2		; move sa from t0 to t2
	jsr parse		; get ea
	bcs rangex		; ...branch if missing ea

	jsr sub0m2		; calculate length = ea - sa  (.c=0 if ea<sa)

	lda tmp0	   	; move length from t0 to t1
	sta tmp1
	lda tmp0+1
	sta tmp1+1
	bcc rangex		; invert .c from subtraction above
	clc			; good stuff exits here
	!byte $24		; skip next
rangex:	sec			; bad  stuff exits here
	rts
; -------------------------------------------------------------------------------------------------
;  parse entry when 1st character has already been read
pargot:	jsr evalgot
	jmp parse1
;  parse next item & put its value into  tmp0, tmp0+1
;    .z=0 valid number
;    .z=1 if no value found (only delimiter)
;    .c=1 if eol & no value found!
;    .x & .y are preserved, .a contains # digits read.
;    if error, call is popped & 'jmp error' performed.
parse:  lda lastchr
	cmp #cr			; last char was EOL?
	beq pareol		; ...yes
	jsr eval		; evaluate ascii input as a number
parse1:	bcs parserr		; ...branch if error
	jsr glc
	bne parckdl		; ...branch if not eol
	lda count
	bne parok		; ...valid number input, treat eol as a delimiter
	beq pareol		; ...the well is very dry

parckdl	cmp #' '		; parse delimiters (only allow <space> or <comma>)
	beq parok
	cmp #','
	beq parok		; ...fall into error if bad delimiter

parserr:pla			; pop this call
	pla
	jmp erropr

pareol:	sec			; set .c=1 for eol
	!byte $24		; skip next

parok:	clc			; clear .c for not-eol
	lda count		; set .z=0 for valid number
	rts
; -------------------------------------------------------------------------------------------------
;  evaluate when 1st character has already been read
evalgot:lda #0
	sta tmp0		; clear value
	sta tmp0+1
	sta count		; reset digit counter (flags valid number vs. null input)
	txa
	pha			; preserve .x & .y
	tya
	pha
	jsr glc
	jmp eval1 
;  evaluate next item in buffer & put its value into  tmp0, tmp0+1
;	.c=0  normal return
;	.c=1  error  return
;	.z=1  null input or EOL
;	.x & .y are preserved.
; returns last character
eval:	lda #0
	sta tmp0		; clear value
	sta tmp0+1
	sta count		; reset digit counter (flags valid number vs. null input)
	txa
	pha			; preserve .x & .y
	tya
	pha

evlspc:	jsr gnc			; get next character
eval1:	bne evnoeol
	jmp eval_ok		; ...branch if end of line
evnoeol:cmp #' '
	beq evlspc		; ...branch & ignore leading spaces
	cmp #'$'
	beq evlspc		; ignore leading $
	bne evnxch1

evnxchr:jsr gnc			; get next character
evnxch1:beq eval_ok		; ...branch if eol
	sec
	sbc #'0'		; convert ascii digit to binary value
	bcc eval_ok		; ...branch if not a number (<$30) (assume delimiter)
	cmp #10
	bcc evnumbr		; ...number 0-9
	sbc #7
	cmp #16			; ...number a-f
	bcs eval_ng		; >$46 ('f') -> error

evnumbr:tay			; remember value of current digit
	inc count		; flag valid digit

	ldx #4			; 4bit hex number
evshflp:asl tmp0		; shift digit up
	rol tmp0+1
	bcs eval_ng		; ...branch if overflow error
	dex
	bne evshflp		; ...next shift

evaddig:tya			; get current digit
	ora tmp0		; ..and add
	sta tmp0
	lda #4			; max 4 digits allowed
	cmp count
	bcs evnxchr		; <=4 ...next character
; count >4 = error
eval_ng:sec
	!byte $24		; skip next

eval_ok:clc
	pla
	tay			; restore .x & .y
	pla
	tax
	lda count
	rts
; -------------------------------------------------------------------------------------------------
; convert blocks from binary to decimal (BCD) and print it
;   input : binary value (2 bytes) in tmp0
;   output: decimal value (3 bytes) in HULP as packed BCD
prtblks:lda #0
	ldx #5
bindlp: sta hulp,x		; initialize working registers
	dex
	bpl bindlp
	inc hulp+5		; seed value_of_bit with 1

	ldy #15			; loop index (2 bytes = 16 bits)
	php			; save caller's processor mode
	sei			; disable IRQ's (but beware NMI's!)
	sed			; put processor into decimal mode (for ADC's & SBC's)
; main loop. rotate bits right 1 at a time, and if set add the current value_of_bit to the sum.
binmlp:	lsr tmp0+1		; hi
	ror tmp0		; lo
	bcc binbit0		; ...branch if bit not set (its value is 0)

	clc
	ldx #2
binbtlp:lda hulp+3,x		; add current value_of_bit (decimal arithmetic)
	adc hulp,x
	sta hulp,x
	dex
	bpl binbtlp

binbit0:clc
	ldx #2
bincalp:lda hulp+3,x		; calculate value of next bit (decimal arithmetic)
	adc hulp+3,x
	sta hulp+3,x
	dex
	bpl bincalp
	dey
	bpl binmlp		; loop until done conversion

	plp			; restore processor mode (clear decimal mode, clear irq flag)
; unpack BCD encoded number in hulp, convert it to ascii & print it.
	lda #0			; no leading zeros
	sta count
	ldx #6			; max digits
; shift a nibble (BCD digit) to a
unplp:	ldy #4-1		; # bits per digit, .x is digit counter
	lda #0
unpshlp:asl hulp+2		; lo
	rol hulp+1		; mid
	rol hulp		; hi
	rol			; shift a digit into .a
	dey
	bpl unpshlp

	tay			; set flags for .a
	bne binzero
	cpx #1
	beq binzero		; ...print zero if it's the last digit
	ldy count		; check flag if a non-zero value was already printed?
	beq binskzr		; no...skip leading zeros

binzero:inc count		; flag a non-zero digit
	ora #'0'		; make it ascii
	jsr bsout		; print it

binskzr:dex
	bne unplp		; ...loop until all digits printed
	rts
; ******************************************* BASIC ***********************************************
; BASIC DOS command
!zone basic
; Send disk command, print directory (@$) or read status '@'
bdoscmd:lda #$40+$80
	sta msgflg		; i/o messages to screen
	ldy #0			; .y=0 to count string length
	sty status		; clear status @ i/o begin
	sty fnlen		; filename length of zero...
	ldx ddisk		; drive id

	jsr chget		; get basic char
	cmp #'$'
	bne doscmd
	jmp dosdir		; ...branch if directory read

doscmd:	tya			; la=0
	ldy #15			; open command channel
	jsr setlfs		; .a-0 temporary channel #
	clc
	jsr open		; open a real channel
	bcs dos30		; exit if bad return

	jsr chgot		; see if status check
	beq dos20		; yes

	pha
	ldx #0
	jsr ckout		; set up as output
	pla
	bcs dos30		; bad status return
	bcc dos15		; no...ok
; command
dos10:	jsr chget		; get basic char
dos15:	php			; save for later
	jsr bsout		; out to floppy
	lda status
	bne dos28		; bad status returned
	plp			; end?
	bne dos10		; no...continue
	beq dos30		; yes...floppy done
; status
dos20:	jsr clrch		; clear channel
	jsr crlf
	ldx #0
	jsr chkin		; tell floppy to speak
	bcs dos30		; bad device

dos25:	jsr basin		; get char
	cmp #cr
	php			; save test for later
	jsr bsout		; out to screen
	lda status		; check for bad basin
	and #$ff-$40		; remove eoi bit
	bne dos28		; report bad status
	plp			; end?
	bne dos25		; no...
	beq dos30		; yes...floppy done

dos28:	pla			; clean up...
dos29:	jsr error5		; report error #5 device not present
dos30:	jsr clrch		; clean up
	lda #0
	sec			; close device (c=1)
	jmp close
; get basic char
chget:	inc txtptr		; increment text pointer
	bne chgot		; if no carry into msb
	inc txtptr+1

chgot:	tya
	pha
	ldy #0
	lda (txtptr),y
	sta temp
	pla
	tay
	lda temp
	rts
doserr:	jsr outqst
	rts
; read & display the disk directory
dosdir:	lda #'$'
ddirclp:cpy #16			; max string length
	beq doserr		; string too long
	sta (fnadr),y		; store string
	inc fnlen
	iny
	jsr chget		; get character of string
	bne ddirclp		; no eol -> next char

ddirstr:lda #0			; set logical index
	ldy #$60		; sa for directory
	jsr setlfs
	clc
	jsr open		; open directory channel
	bcs ddirdon		; ...branch on error
	ldx #0
	jsr chkin		; make it an input channel

	ldy #3			; first pass only- trash first two bytes read

ddirlp:	sty tmp1		; loop counter
ddirblp:jsr basin
	sta tmp0		; get # blocks low
	lda status
	bne ddirdon		; ...branch if error, possibly no device?
	jsr basin
	sta tmp0+1		; get # blocks high
	lda status
	bne ddirdon		; ...branch if error, possibly no device?
	dec tmp1
	bne ddirblp		; ...loop until done
	jsr crlf		; start a new line
	jsr prtblks		; convert # blocks to decimal and print it
	jsr space		; print space  (to match loaded directory display)

ddirflp:jsr basin		; read & print filename & filetype
	beq ddireol		; ...branch if eol
	ldx status
	bne ddirdon		; ...branch if error
	jsr bsout
	bcc ddirflp		; ...loop always

ddireol:jsr stop
	beq ddirend		; ...branch if user hit STOP
	ldy #2
	bne ddirlp		; ...loop always

ddirdon:cpy #3			; checks if first dir byte is cr -> no device
	beq ddirerr		; no device?
ddirend:jmp dos30		; close
ddirerr:jmp dos29		; -> error
; ******************************************** FONT ***********************************************
; font 256 chars
!zone font
*= font6x8
font:
!binary "c64-6x8.fon"
; **************************************** COLD START *********************************************
!zone cold
*= kernal
jmoncld:jmp monoff		; Monitor cold start
	nop
; ****************************************** EDITOR ***********************************************
;***************************************
;*                                     *
;* EEEEE DDD   IIIII TTTTT  OOO  RRRR  *
;* E     D  D    I     T   O   O R   R *
;* E     D   D   I     T   O   O R   R *
;* EEE   D   D   I     T   O   O RRRR  *
;* E     D   D   I     T   O   O R R   *
;* E     D  D    I     T   O   O R  R  *
;* EEEE  DDD   IIIII   T    OOO  R   R *
;*                                     *
;***************************************
;***************************************
;*   CBM EDITOR FOR P-SERIES SYSTEMS   *
;*   KEYBOARD AND SCREEN EDIT ROUTINES *
;* DRIVING THE HARDWARE OF THE         *
;* FOLLOWING MODEL : LC256             *
;* COPYRIGHT (C) 1983 BY CBM           *
;* COPYRIGHT (C) 2024 Vossi, Baleares  *
;***************************************
!zone editor
*= kernal+4
;****************************************
;  40/80 column LC256 screen editor
;    with unlimited screen line wrap
;****************************************
; Jump vector table
jcint:  jmp cint		; Init Screen editor, VIC, F-keys
jlp2:	jmp lp2			; Read a key from keyboard to A
jloop5:	jmp loop5		; Read character from screen to A
jprt:	jmp prt			; Print character from A on screen
jscror:	jmp scrorg		; Return screen dimensions to X, Y
jkey:	jmp scnkey		; Keyboard scan
jmvcur: jmp nofunc		; not used in LC256/P500 - only for CRTC hardware cursor in b-series
jplot:  jmp plot		; Get/set the cursor position to/from X, Y
jiobas:	jmp nofunc		; not used in LC256 - Return CIA base address to X, Y
jescrt:	jmp nofunc		; not used in LC256 - Handle an escape sequence
jfunky:	jmp keyfun		; Get/set/list function keys
; -------------------------------------------------------------------------------------------------
; Get/set the cursor position
plot:   bcs rdplt		; if C=1 get cursor position
; set cursor
	stx tblx		; store line, last line 
	stx lsxp
	sty pntr		; store column, last column
	sty lstp
;	jsr stupt		; Change pointer to this new line
rdplt:  ldx tblx
	ldy pntr		; load column, row
nofunc: rts
; -------------------------------------------------------------------------------------------------
; Return screen dimensions
scrorg: ldx columns		; columns
	ldy #nrows		; rows
	rts
; -------------------------------------------------------------------------------------------------
; Screen editor init (editor, VDP)
; Clear editor variables
cint:   lda #0
	ldx #stack-keypnt-3	; $b8-$fd (prevents clearing $fe = allocated sprite_colors pointer)
cloop1: sta keypnt,x		; clear page 0 variables (above $f6 = allocated pgmkey buffer pt.)
	dex
	bpl cloop1

	ldx #evect-rvs-1	; $397-$3f7
cloop2: sta rvs,x		; clear absolute variables
	dex
	bpl cloop2
; init some variables
	lda #4
	sta kount		; delay between key repeats
	lda #16
	sta delay		; initial key repeat delay
	stx crson		; cursor off <> 0
; init sprite colors vector
	lda sprite_colors	; check if buffers are allocated
	ora sprite_colors+1
	bne keybuf		; yes... skip
	ldx #0	
	ldy #2			; 2 pages
	jsr alocat		; get 512 bytes at end of system memory
	bcs noroom		; no room found...just reset the screen
	inx
	stx sprite_colors	; save start address (returned .x+1)
	bne room00
	iny
room00:	sty sprite_colors+1
; init F-keys
keybuf:	lda pkybuf		; check if buffers are allocated
	ora pkybuf+1
	bne keycpy		; yes..just copy f-keys (erased with absolute vars)
	lda hiadr		; get end of key area
	sta pkyend
	lda hiadr+1
	sta pkyend+1
	ldx #0	
	ldy #1			; 1 page
	jsr alocat		; get 256 bytes at end of system memory
	bcs noroom		; no room found...just reset the screen
	inx
	stx pkybuf		; save start address (returned .x+1)
	bne room10
	iny
room10: sty pkybuf+1		; save start address
keycpy: ldy #keyend-keydef	; load size of F-key texts
kyset1: lda keydef-1,y
	dey
	sta (pkybuf),y		; copy key texts to buffer
	bne kyset1

	ldy #keydef-keylen	; 10 F-key length bytes
kyset2: lda keylen-1,y
	sta keysiz-1,y		; copy F-key text length to $38d
	dey
	bne kyset2
; init VDP, screen
noroom:
	jsr vinit

	lda #<dokeyf		; copy function key vector
	sta funvec
	lda #>dokeyf+1
	sta funvec+1

	lda #TEXTCOL
	sta color		; init color
; Clear screen, cursor home
clsr:	jsr vclear		; clear VDP screen and screen+color ram
; Cursor home
nxtd:   ldx #0			; move to top left
	stx tblx
	stx pntr
	stx lsxp		; for input after home or clear
	stx lstp
; Reset screen ptr to line begin
stupt:	ldx tblx		; get curent line index
; Set screen ptr to line X 
scrset: lda ldtb2,x		; load start of screen line low
	sta pnt			; and store to screen, color RAM ptr
	lda cldtb2,x		; load start of color line low
	sta user
	lda ldtb1,x		; load high
	sta pnt+1		; and store to char pointer
	lda cldtb1,x		; load high
	sta user+1		; and store to color pointer
	rts
; -------------------------------------------------------------------------------------------------
; *** Input routines ***
; Remove character from queue
lp2:  	ldx kyndx		; are there any pgm keys
	beq lp3			; branch if not
	ldy keyidx		; get index to current char
	lda (keypnt),y		; get current byte
	dec kyndx		; 1 byte down
	inc keyidx		; bump index to next char
	cli
	rts
; No F-key
lp3: 	ldy keyd		; get key from irq buffer
	ldx #0
lp1:  	lda keyd+1,x		; shift key buffer
	sta keyd,x
	inx
	cpx ndx			; shift till last key in buffer
	bne lp1
	dec ndx			; decrease key index
	tya			; return char in A
	cli
	rts
; -------------------------------------------------------------------------------------------------
; Screen input - Main loop
loop4:	jsr prt			; print the character
	jmp loop3
; wait for key input
loop3:  lda ndx			; check key and pgm-key index
	ora kyndx
	sta crson		; 0 = cursor on, off if index > 0
	beq loop3		; loop - wait for key input
; key available
	sei			; disable interrupts
	lda crssw
	beq lp21		; skip if cursor already switched off
	lda config		; load char behind cursor
	ldy #0
	sty crssw		; switch off cursor
	ldx gdcol		; load color behind cursor
	jsr dspp		; write char before cursor
; check key
lp21:	jsr lp2			; get key input
	cmp #cr
	bne loop4		; print char if not cr
; return recognized
	sta crsw		; set cr flag - we pass chars now
	jsr fndend		; check nxt line for cont (double line?)
	stx lintmp		; save last line number of sentence
	jsr fistrt		; find begining of line
	lda #0
	sta insrt		; clear insert flag *****
	sta qtsw		; clear quote mode
	ldy #0			; retrieve from line start if left it
	lda lsxp		; input started row
	bmi lp80		; flag we left start line
	cmp tblx
	bcc lp80
	ldy lstp		; input started column
	cmp lintmp		; on start line
	bne lp70
	cpy indx		; past start column
	beq lp75		; ok if the same
lp70:	bcs clp2		; yes - null input
lp75:	sta tblx		; start from here on input
lp80:	sty pntr
	jmp lop5		; input a line
; -------------------------------------------------------------------------------------------------
; Read character from screen
loop5:	tya
	pha
	txa
	pha
	lda crsw		; passing chars to input
	beq loop3		; no - buffer on screen
	bpl lop5		; not done - get next char
clp2:	lda #0			; input done clear flag
	sta crsw
	lda #cr			; pass a return
	bne clp7
lop5:	jsr stupt		; set pnt and user
	jsr get1ch		; get a screen char
; convert screencode to petscii
	sta data		; store screen code for bit#5,6,7 check temporary
	and #$3f		; clear bit#6,7 in A
	asl data		; check: scrcode bit#7->C
	bit data		; check: scrcode bit#6->N, #5->V (shiftet to left)
	bpl lop54		; skip if scrcode #6=0 x0x -> 00x
	ora #$80		; x1x -> 10x
lop54:	bcc lop52		; skip if scrcode #7=0 (not reverse)
	ldx qtsw
	bne lop53		; skip if bit#7=1 & quote on: 10x -> 00x, 11x -> 10x
				; if quote off or bit#7=0:
lop52:	bvs lop53		; skip if scrcode #5=1: 001 -> 001, 011 -> 101
	ora #$40		; 000 -> 010, 100 -> 110
lop53:	jsr qtswc
	ldy tblx		; on input end line ?
	cpy lintmp
	bcc clp00		; no
	ldy pntr		; on input end column ?
	cpy indx
	bcc clp00		; no
	ror crsw		; c=1 minus flags last char sent
	bmi clp1		; always

clp00:	jsr nxtchr		; at next char
clp1:	cmp #$de		; a pi ?
	bne clp7		; no
	lda #PI			; translate
clp7:	sta data
	pla
	tax
	pla
	tay
	lda data		; return petscii char in A
	rts
; -------------------------------------------------------------------------------------------------
; *** Test for quote mode ***
; Switch quote mode depending on in A
qtswc:	cmp #$22 ; "
	bne qtswl		; skip if no quote-char
	lda qtsw
	eor #$1			; toggle quoteswitch
	sta qtsw
	lda #$22 ; "		; restore quote in A
qtswl:	rts
; -------------------------------------------------------------------------------------------------
; *** Output chars ***
nxt3:	bit rvs
	bpl nvs
	ora #$80
nvs:	ldx insrt
	beq nvsa
	dec insrt
nvsa:	bit insflg		; are we in auto insert mode?
	bpl nvs1		; branch if not
	pha			; save the char
;	jsr insert		; make room for this char
	ldx #0
	stx insrt		; make sure we turn off insert mode.
	pla			; restore char
nvs1:	jsr dsppcc		; display the character
	jsr movchr		; move to next char pos
; -------------------------------------------------------------------------------------------------
; ********* exit from prt *********
loop2:	lda data		; copy last char
	sta lstchr
	pla
	tay
	lda insrt
	beq lop2
	lsr qtsw		; clear quote switch if in insert mode
lop2:	pla
	tax
	pla
	rts
; -------------------------------------------------------------------------------------------------
;********************************
; Display a character
;********************************
; Write blank ($20) at cusor position
doblnk: lda #' '		; load blank
; Write char A with color or tcolor if color bit#7=1
dsppcc: ldx color		; load char color
	jmp dspp
; Write char A with tcolor
dsptco: ldx tcolor
; Write char A with color X
dspp:   ldy pntr		; get char index
	sta (pnt),y		; store byte to screen line pointer + column x
	pha
	bit mode
	bpl dspmod6		; -> mode 6, 16 colors
; mode7
	txa			; move color to A
	sta (user),y		; store to color RAM
	pla
	jsr vchar		; print char on VDP
	ldy pntr		; restore y
	rts
; mode6
dspmod6:tya			; column in y
	lsr			; bit #0 in c, column / 2
	bcs dspodd		; -> odd char
; even char - color in high nibble
	tay
	txa			; get color
	asl			; shift to high nibble
	asl
	asl
	asl
	sta temp1		; remember color
	lda #$0f
	and (user),y		; get color low nibble
	ora temp1		; add color high nibble
	sta (user),y
	pla
	jsr vchar		; print char on VDP
	ldy pntr		; restore y
	rts
; odd  char - color in low nibble
dspodd:	stx temp1		; remember color
	tay
	lda #$f0
	and (user),y		; get color high nibble
	ora temp1		; add color low nibble
	sta (user),y
	pla
	jsr vchar		; print char on VDP
	ldy pntr		; restore y
	rts
; -------------------------------------------------------------------------------------------------
; Subroutine to clear line
;   entry: line in x, screen pointer to line
clrln:	jsr clrbit		; make sure non-continued line

	txa			; line to a
	asl			; *8 (FONTH) for y-position
	asl
	asl
	sta dy			; y position
	lda #FONTH
	sta temp1		; line height
;	lda sizex		; screen width
;	sta dx
;	lda sizex+1
;	sta dx+1
	bit mode
	bmi clrm7
	lda #>(FONTW*COLUMNS6)
	sta dx+1
	lda #<(FONTW*COLUMNS6)	; text area width in pixels
	sta dx
	bne clr00
clrm7:	lda #>(FONTW*COLUMNS7)	; text area width in pixels
	sta dx+1
	lda #<(FONTW*COLUMNS7)
	sta dx

clr00:	+VdpCommand zero, zero, zero, zero, zero, dy, dx, dx+1, temp1, bgcolor_del, zero, HMMV

	lda #' '		; load blank
	ldy scxmax		; last column
clr10:	sta (pnt),y		; store byte to screen line pointer + column x
	dey
	bpl clr10		; next char till whole line done

	lda color		; load char color
	bit mode
	bmi clr20		; -> skip mode7
; mode 6
	asl			; shift color to left nibble
	asl
	asl
	asl
	ora color		; and add color to low nibble

	ldy #collen-1		; color line end
clr20:	sta (user),y		; store color
	dey
	bpl clr20		; next char till whole line done

	jmp waitcmd		; wait for command execution
; -------------------------------------------------------------------------------------------------
; Clear screen ram
clrscr:	ldx #scymax
clsbtlp:jsr clrbit		; make sure non-continued line
	dex
	bpl clsbtlp

	lda #' '
	ldx #0
clsr6lp:sta scnram,x
	sta scnram+$100,x
	sta scnram+$200,x
	sta scnram+$300,x
	inx
	bne clsr6lp

	bit mode
	bmi clsmod7		; -> skip mode7
clsr7lp:sta scnram+$400,x
	sta scnram+$500,x
	sta scnram+$600,x
	sta scnram+$700,x
	inx
	bne clsr7lp

clsmod7:lda color		; load char color
	bit mode
	bmi clscolp		; -> skip mode7
; mode 6
	asl			; shift color to left nibble
	asl
	asl
	asl
	ora color		; and add color to low nibble

clscolp:sta clrram,x
	sta clrram+$100,x
	sta clrram+$200,x
	sta clrram+$300,x
	inx
	bne clscolp
	rts
; -------------------------------------------------------------------------------------------------
; Grab a character from screen
get1ch: ldy pntr		; get char/color index
; Get char from column Y
getych: lda (pnt),y		; get the character
	pha
	bit mode
	bpl getmod6		; -> mode 6, 16 colors
; mode7
	lda (user),y		; get color
	sta tcolor		; and store it to tcolor
	pla
	rts
; mode6
getmod6:tya
	pha
	lsr			; column/2, bit #0 in c
	bcs getodd		; -> odd char
; even char - color in high nibble
	tay
	lda (user),y		; get color
	lsr			; shift to low nibble
	lsr
	lsr
	lsr
	sta tcolor
	pla
	tay
	pla
	rts
; odd  char - color in low nibble
getodd:	tay
	lda (user),y		; get color
	and #$0f		; isolate high nibble
	sta tcolor
	pla
	tay
	pla
	rts
; -------------------------------------------------------------------------------------------------
; *** Print a char ***
prt:	pha
prt10:	sta data		; save char
	txa			; save regs
	pha
	tya
	pha
	lda #0			; clear cr flag
	sta crsw
	ldy pntr		; column we are in
	lda data
	and #$7f
	cmp #$20		; test if control character (< $20)
	bcc ntcn		; yes
njt2:	and #$3f		; no - make a screen char
njt20:	bit data
	bpl njt30		; skip ahead if normal set - 00 - 3f
	ora #$40		; convert a0 - bf to 60 - 7f & c0 - df to 40 - 5f
njt30:	jsr qtswc		; test for quote
	jmp nxt3		; put on screen
; ********* Control keys *********
ntcn:	cmp #$0d		; test if a return
	beq ntcn20		; no inverse if yes
	cmp #$14		; test if insert or delete
	beq ntcn20		; allow in insert or quote mode
	cmp #$1b		; test if escape key
	bne ntcn1
	bit data
	bmi ntcn1		; its a $9b
	lda qtsw		; test if in quote mode...
	ora insrt		; ...or insert mode
	beq ntcn20		; if not, go execute remaining code
	jsr toqm		; else go turn off all modes
	sta data		; and forget about this character
	beq ntcn20		; always
ntcn1	cmp #$03		; test if a run/load or stop
	beq ntcn20		; no inverse if yes
	ldy insrt		; test if in insert mode
	bne ntcn10		; go reverse - if yes
	ldy qtsw		; check for quote mode
	beq ntcn20		; do not reverse if not
ntcn10:	ora #$80		; make reverse
	bne njt20
ntcn20:	lda data
	asl			; set carry if shifted ctrl
	tax
	jsr ctdsp		; indirect jsr
	jmp loop2
; Control code dispatcher
ctdsp:	lda ctable+1,x		; hi byte
	pha
	lda ctable,x		; low byte
	pha
	lda data
	rts			; indirect jmp
; -------------------------------------------------------------------------------------------------
; User control code jump vector
cuser:	jmp (ctlvec)
; -------------------------------------------------------------------------------------------------
; Cursor down/up
cdnup:  bcs cup			; cursor up
; cursor down
cdwn:	jsr nxln
cdn10:	jsr getbit		; a wrapped line ?
	bcs cdrts		; skip if yes
	sec			; flag we left line
	ror lsxp

cdrts:  clc
	rts
; Cursor up
cup:	ldx #0			; cursor up
	cpx tblx		; at top of window ?
	bcs critgo		; yes - do nothing
cup10:	jsr cdn10		; about to wrap to a new line ?
	dec tblx		; up a line
	jmp stupt
; -------------------------------------------------------------------------------------------------
; Cursor right/left
crtlf:  bcs cleft		; cursor left
; cursor right
crit:	jsr nxtchr		; cursor right
	bcs cdn10		; yes - test for wrap

critgo: rts
; Cursor left
cleft:  jsr bakchr		; move back
	bcs critgo		; abort if at top left
	bne cdrts		; no - exit
	inc tblx
	bne cup10		; go set flag if needed
; -------------------------------------------------------------------------------------------------
; RVS on/off
rvsf:   eor #$80
	sta rvs
	rts
; -------------------------------------------------------------------------------------------------
; Home/clear
homclr:	bcc homes		; if C=0 home
	jmp clsr		; Clear screen, cursor home
; Cursor home
homes:	cmp lstchr		; last char a home ?
	bne hm110		; no
hm110:  jmp nxtd		; set to top left
; -------------------------------------------------------------------------------------------------
; Skip to next line
;   wrap to top if scroll disabled
nxln:	ldx tblx
	cpx #scymax		; of the bottom of window ?
	bcc nxln1		; no
	bit scrdis		; what if scrolling is disabled?
	bpl doscrl		; branch if scroll is enabled
	lda #0			; wrap to top
	sta tblx
	jsr scrkey		; check commodore and stop key
	bcs nowhop		; always

doscrl:	jsr scrup		; scroll it all
	clc			; indicate scroll ok
nxln1:	inc tblx
nowhop:	jmp stupt		; set line base adr
; -------------------------------------------------------------------------------------------------
; A return or shift return
nxt1:   jsr fndend		; find the end of the current line
	inx
	jsr clrbit		; set next line as non-continued
	ldy #0			; else point to start of next line
	sty pntr
	jsr nxln		; set up next line
	jmp toqm		; turn off all modes
; -------------------------------------------------------------------------------------------------
; ****** scroll routines ******
; Move one line (only used space and clears the rest of target line) - line x to pnt (y for VDP)
;   preserves x
movlin:	txa			; source line to a
	asl			; *8 (FONTH) for y-position
	asl
	asl
	sta temp1		; source start
	tya			; target line to a
	asl			; *8 (FONTH) for y-position
	asl
	asl
	sta temp2		; target start
	lda #FONTH
	sta dy			; line height

	lda ldtb2,x		; set pointer to source line address lo
	sta sedsal
	lda ldtb1,x
	sta sedsal+1		; set pointer hi to vram

; search last used char in source line
	ldy scxmax		; start at right		
movls0:	lda (sedsal),y
	cmp #' '		; space ?
	bne movls1		; no... -> copy line
	dey
	bpl movls0		; test next char
	lda #0
	sta x1
	sta x1+1
	jmp movlt		; empty line -> nothing to copy
; y holds last used char in source line
movls1:	iny			; include last used char
	lda #0
	sta x1+1
; calc column *6 for pixel pos
	tya
	asl			; *4
	asl
	rol x1+1
	sta x1
	tya
	asl			; *2
	clc
	adc x1			; add to get *6
	sta x1			; store source line end pixel lo
	lda x1+1
	adc #0
	sta x1+1

	lda #$04		; copy from the left edge to x1
	sta arg

	+VdpCommand zero, zero, temp1, x1, x1+1, temp2, zero, zero, dy, zero, arg, YMMM

; search last used char in target line
movlt:	ldy scxmax		; start at right		
movlt0:	lda (pnt),y
	cmp #' '		; space ?
	bne movlt1		; no... -> found end of chars in target line
	dey
	bpl movlt0		; test next char
; y holds last used char target in line
movlt1:	iny			; include last used char
	lda #0
	sta dx+1
; calc column *6 for pixel pos
	tya
	asl			; *4
	asl
	rol dx+1
	sta dx
	tya
	asl			; *2
	clc
	adc dx			; add to get *6
	sta dx			; store target end pixel lo
	lda dx+1
	adc #0
	sta dx+1

; copy line in screen ram
	ldy scxmax		
movl10: lda (sedsal),y
	sta (pnt),y		; store byte to screen line pointer + coulmn X
	dey
	bpl movl10		; next char till whole line done
; color
	lda cldtb2,x		; set pointer to line address lo
	sta sedsal
	lda cldtb1,x
	sta sedsal+1		; set pointer hi to color ram

	ldy #collen-1		
movl20: lda (sedsal),y
	sta (user),y		; store to color RAM
	dey
	bpl movl20		; next char till whole line done

; calc diff from source end to target end (line to overwrite)
	lda dx			; target end
	sec
	sbc x1			; substract source end
	sta dx			; store diff (width to delete)
	lda dx+1
	sbc x1+1
	sta dx+1
	bmi movlx		; target line is shorter than source line - nothing to delete there
	bne movlt2		; hi <> o -> clear diff
	lda dx
	beq movlx		; lo = 0 -> no diff - nothing to clear

movlt2:	jsr waitcmd
; delete the part from new line to old line end
	+VdpCommand zero, zero, zero, x1, x1+1, temp2, dx, dx+1, dy, bgcolor_del, zero, HMMV

movlx:	jmp waitcmd		; wait for command execution
; -------------------------------------------------------------------------------------------------
; ****** Scroll down ******
scrdwn: ldx lsxp
	bmi scd30		; skip if new line flag already set
	cpx tblx
	bcc scd30		; skip if old line is below scroll area
	inc lsxp		; else inc start line number
scd30:  ldx #scymax		; scroll down, start bottom

scd10:  jsr scrset		; set pnt to line
	cpx tblx		; test if at destination line
	beq scd20		; done if yes
	txa			; move target line to y
	tay
	dex			; point to previous line as source
	jsr getbt1
	inx
	jsr putbt1		; move continuation byte
	dex
	jsr movlin		; move one line
	bcc scd10		; always

scd20:	jsr clrln		; set line to blanks
	jmp setbit		; mark as continuation line
; -------------------------------------------------------------------------------------------------
; ****** Scroll up ******
scrup:  ldx #0
scru00: inx
	jsr getbt1		; find first non-continued line
	bcc scru15
	cpx #scymax		; is entire screen 1 line?
	bcc scru00		; do normal scroll if not

	ldx #0
	inx
	jsr clrbit		; clear to only scroll 1 line

scru15:	dec tblx
	bit lsxp
	bmi scru20		; no change if already new line
	dec lsxp		; move input up one
scru20:	ldx #0
	cpx sedt2
	bcs scru30
	dec sedt2		; in case doing insert
scru30:	jsr scr10		; scroll
	ldx #0
	jsr getbt1
	php
	jsr clrbit		; make sure top line is not continuation
	plp
	bcc scru10		; done if top line off
scru10: rts

scr10:	jsr scrset		; point to start of line
	cpx #scymax		; at last line ?
	bcs scr40		; yes
	txa			; move target line to y
	tay
	inx			; point to next line
	jsr getbt1
	dex
	jsr putbt1		; move continuation byte
	inx
	jsr movlin		; move one line (only used space)
	bcc scr10		; always
scr40:  jsr clrln		; make last line blank
; Scroll stop
scrkey:	ldx #$7f		; allow only output line 7
	jsr getlin		; get input lines key
	and #$20		; check for the commodore key
	bne scr75		; exit if not - no stop scroll

scr90:	jsr getlin		; get input lines
	and #$20		; check for the commodore key
	beq scr90		; wait until com.key not depressed

scr95:	ldx #0			; allow all output lines
	jsr getlin		; get inputs
	eor #$ff		; check for any input
	beq scr95		; wait
	stx ndx
	ldx #$7f		; setup pa7 default output
	stx via1+pra		; default key row
scr75:	rts
; Keyboard check for stop
getlin: php			; preserve the irq flag
	sei
	stx via1+pra		; set keyboard column output
getl10:	lda via1+prb		; get row
	cmp via1+prb		; debounce keyboard
	bne getl10
	plp
	rts
; -------------------------------------------------------------------------------------------------
; ****** wrap table subroutines *******
; Check for a double length line
getbit: ldx tblx		; load current line
; Check line X for double length
getbt1: jsr bitpos		; get byte & bit positions
	and bitabl,x		; check if bit for line is set in table
	cmp #1			; make carry clear if zero
	jmp bitout		; return 0 if not a double length line
; -------------------------------------------------------------------------------------------------
; Mark current line as double length C=1, unmark C=0
; putbit - set bit according to carry
putbit: ldx tblx		; load current line
; Mark line X
putbt1: bcs setbit		; go if to mark as wrappped line
; clrbit - clear wrap bit
clrbit: jsr bitpos		; get byte & bit positions
	eor #$ff		; invert bit position
	and bitabl,x		; clear bit
bitsav: sta bitabl,x		; and store it to table at byte position X
bitout: ldx bitmsk		; move byte table position to X
	rts
; setbit  -  set bit to mark as wrapped line
setbit: bit scrdis		; auto line link disable...
	bvs getbt1		; branch if scrolling is disabled
	jsr bitpos		; get byte & bit position
	ora bitabl,x		; set wrap bit
	bne bitsav		; always
; Find bit table position for line X
bitpos: stx bitmsk		; remember line
	txa
	and #$07		; get bit position
	tax
	lda bits,x		; get bit mask
	pha			; remember it
	lda bitmsk
	lsr
	lsr			; shift to get byte position (/8)
	lsr
	tax			; move byte pos to X
	pla			; return bit value in A
	rts
; -------------------------------------------------------------------------------------------------
; ****** Move to start of line
; Find line start/end
	bcc fndend		; if C=0 find line end - NOT USED
; cursor to line start
fndfst:	ldy #0
	sty pntr		; set to leftmost column
fistrt:	jsr getbit		; find start of current line
	bcc fnd0		; branch if found
	dec tblx		; up a line
	bpl fistrt		; always
	inc tblx		; whoops went too far
fnd0:	jmp stupt		; set line base adr

; -------------------------------------------------------------------------------------------------
; ****** Find last non-blank char of line
;   pntr= column #
;   tblx= line #
; cursor to end of line
fndend:	inc tblx
	jsr getbit		; is this line continued
	bcs fndend		; branch if so
	dec tblx		; found it - compensate for inc tblx
	jsr stupt		; reset screen pointer to line start
	ldy scxmax		; get right margin
	sty pntr		; point to right margin
	bpl eloup2		; always
eloup1:	jsr bakchr		; backup one char
	bcs endbye		; if at top left get out
eloup2: jsr get1ch		; get char from screen
	cmp #$20
	bne endbye		; yes, space
	cpy #0			; are we at the left margin?
	bne eloup1		; branch if not
	jsr getbit		; if we're on a wraped line
	bcs eloup1		; always scan the above line

endbye: sty indx		; remember this
	rts
; -------------------------------------------------------------------------------------------------
; ****** Move to next char
; scroll if enabled
; wrap to top if disabled
nxtchr:	pha
	ldy pntr
	cpy scxmax		; are we at the right margin?
	bcc bumpnt		; branch if not

	jsr nxln		; point to nextline
	ldy #0			; point to first char of 1st line
	dey
	sec			; set to show moved to new line
bumpnt:	iny			; increment char index
	sty pntr
	pla
	rts
; -------------------------------------------------------------------------------------------------
; Backup one char - Move one char left
; wrap up and stop a top left
bakchr:	ldy pntr
	dey
	bmi bakot1
	cpy #0			; are we at the left margin
	bcs bakout		; no - past it
bakot1:	ldy #0
	cpy tblx		; are we at top line last character?
	bcs bakot2		; leave with carry set
	dec tblx		; else backup a line
	pha
	jsr stupt		; set line base adr
	pla
	ldy scxmax		; move cursor to right side
bakout: sty pntr
	cpy scxmax		; set z-flag if moved to new line
	clc			; always clear
bakot2: rts
; -------------------------------------------------------------------------------------------------
; savpos - Save row & column position
savpos: ldy pntr
	sty sedt1
	ldx tblx
	stx sedt2
	rts
; -------------------------------------------------------------------------------------------------
; Delete or insert a character
delins: bcs insert		; C=1 is insert
; delete a character
deleet: jsr cleft		; move back 1 position
	jsr savpos		; save column & row positions
	bcs delout		; abort if at top left corner

deloop: cpy scxmax		; at right margin?
	bcc delop1		; no - skip ahaed
	ldx tblx
	inx
	jsr getbt1		; is next line a wrapped line?
	bcs delop1		; yes - continue with delete
	jsr doblnk		; no - blank last character

delout: lda sedt1		; restore column and row positions
	sta pntr
	lda sedt2
	sta tblx
	jmp stupt		; restore pnt and exit

delop1: jsr nxtchr
	jsr get1ch		; get next character
	jsr bakchr
	jsr dsptco		; move it back 1 position
	jsr nxtchr		; move up 1 position
	jmp deloop		; loop until at end of line
; Insert a character 
insert: jsr savpos		; save column & row positions
	jsr fndend		; move to last char on the line
	cpx sedt2		; last row equal to starting row?
	bne ins10		; no - skip ahead
	cpy sedt1		; is last position before starting position?
ins10:	bcc ins50		; yes - no need to move anything
	jsr movchr		; move to next char position
	bcs insout		; abort if scroll needed but disabled

ins30:	jsr bakchr
	jsr get1ch		; move char forward 1 position
	jsr nxtchr
	jsr dsptco
	jsr bakchr
	ldx tblx
	cpx sedt2		; at original position
	bne ins30
	cpy sedt1
	bne ins30		; no - loop till we are

	jsr doblnk		; insert a blank
ins50:	inc insrt		; inc insert count
	bne insout		; only allow up to 255
	dec insrt
insout:	jmp delout		; restore original position
; -------------------------------------------------------------------------------------------------
; Stop/run
stprun: bcc runrts		; exit if a stop code
	sei			; disable interrupts
	ldx #9
	stx ndx			; set keyboard queue size
runlop:	lda runtb-1,x
	sta keyd-1,x		; load run character sequence into kybd queue
	dex
	bne runlop

	cli			; enable interrupts
runrts: rts
; -------------------------------------------------------------------------------------------------
; movchr  -  Move to next char position
; insert blank line if at end of line
;   y = column position
;   on exit - carry set = abort - scroll disabled
movchr: cpy scxmax
	bcc movc10		; easy if not at end of line
	ldx tblx
	cpx #scymax
	bcc movc10		; skip if not last line of screen
	bit scrdis
	bmi movc30		; abort if scrolling disabled

movc10:	jsr stupt		; set pnt address
	jsr nxtchr		; move to next char position
	bcc movc30		; done if not move to new line
	jsr getbit		; check if on a continued line
	bcs movc20		; skip ahead if not
	ldx #scymax		; check
	cpx #0
	bne movc15		; no...pass through old code
	pla			; abort
	pla
movc15:	bit scrdis		; restore patched area (test for scrolling mode)
	sec			; prep for abort...
	bvs movc30
	jsr scrdwn		; else insert a blank line

movc20:	clc			; for clean exit
movc30: rts
; -------------------------------------------------------------------------------------------------
; Change color
chkcol:	ldy #16			; there's 16 colors
chk1a:	dey
	bmi chk1b
	cmp coltab,y
	bne chk1a
	bit mode		; check mode
	bmi chkcol7		; -> mode7
; mode 6
	sty color		; change the color
	rts
; mode 7
chkcol7:pha			; save char
	lda col8bit,y
	sta color
	pla
	rts

chk1b:	jmp cuser
; -------------------------------------------------------------------------------------------------
; Turn off all modes - Reset modes: insert, reverse, quote
;   expected to return zero
toqm:	lda #0
	sta insrt
	sta rvs
	sta qtsw
	rts
; -------------------------------------------------------------------------------------------------
; Enable / Disable scrolling
;   carry set = disable
scrsw:	lda #0
	ror
	sta scrdis
	rts
; -------------------------------------------------------------------------------------------------
; ********** Patched new f-key functions with sei/cli and chr$(141) for shift-return **********
keyfun: sei			; prevent fight over variables with keyscan...
	dey
	bmi listky		; do list if no parameters given
	jmp addkey		; - else go add a new key definition
; list key defintions
listky:	ldy #0			; initialize key counter

listlp:	iny
	sty sedt1
	dey			; minus 1 for indexing
	lda keysiz,y		; get key length
	beq nodefn		; no listing if no defintion
	sta keyidx		; save key length
	jsr findky		; get buffer start addr for function key
	sta keypnt
	stx keypnt+1		; save 2 byte address in temp loc
; print 'key ' preamble
	ldx #3
preamb:	lda keword,x
	jsr bsout
	dex
	bpl preamb
; convert to 1 digit ascii
	lda sedt1		; get key number
	clc
	adc #$30		; make ascii
; print key string
	jsr bsout		; print digit
	ldy #0			; init string position counter
	lda #','		; for comma print
lstk20:	jsr bsout		; print char - comma or plus-sign
	ldx #7			; for chr$ printing - no plus-sign or quote to preceed
txtprt:	lda (keypnt),y		; get byte
	cmp #13
	beq lstkcr		; print chr$(13) for return
	cmp #141
	beq lstksc		; print chr$(141) for shift-return
	cmp #34
	beq lstkqt		; print chr$(34) for quote
	cpx #9			; was a normal char printed last time
	beq lstk10		; yes - skip ahead
	pha			; save char
	lda #$22 ; "
	jsr bsout		; print a quote
	pla			; restore the char

lstk10:	jsr bsout		; print the char
	ldx #9			; for chr$ - print quote and plus next time
	iny
	cpy keyidx
	bne txtprt		; loop to end of string

	lda #$22 ; "
	jsr bsout		; print ending quote

lstk30:	lda #cr
	jsr bsout		; do a return

nodefn:	ldy sedt1		; get key number
	cpy #pgmkys
	bne listlp		; loop til all keys checked

	cli			; all done...clear the keyscan holdoff
	clc			; okay return always
	rts

lstkcr:	ldx #qtword-cdword-1	; index for return
	!byte $2c		; skip 2
lstksc:	ldx #addkey-cdword-1	; index for shifted-return
	!byte $2c		; skip 2
lstkqt:	ldx #scword-cdword-1	; index for quote

lstk:	txa			; save value index....
	pha			; save .x
	ldx #crword-cdword-1	; print chr$(
lstklp:	lda cdword,x		; print loop
	beq lstk40		; zero is end...
	jsr bsout
	dex
	bpl lstklp

	pla			; move number and repeat
	tax
	bne lstklp		; loop again for 'xxx)' ending part

lstk40:	iny
	cpy keyidx
	beq lstk30		; exit if all string printed
	lda #'+'		; set to print plus sign
	bne lstk20		; return to routine

keword:	!pet " yek"
cdword:	!pet "($rhc+",$22 ; "
crword:	!pet 0,")31"
qtword:	!pet 0,")43"
scword:	!pet 0,")141"

; insert a new key defintion
addkey:	pha			; save zero page address of params
	tax
	sty sedt1		; save key number in temp loc
	lda zp,x		; get new string length
	sec
	sbc keysiz,y		; subtract old length
	sta sedt2		; save difference in temp location
	ror fktmp		; save the carry
	iny
	jsr findky		; find start addr of next function key
	sta sedsal
	stx sedsal+1		; save 2 byte address in temp loc
	ldy #pgmkys
	jsr findky		; find end of last function key
	sta sedeal
	stx sedeal+1		; save next free byte addr in temp loc
	ldy fktmp		; check if new string is longer or shorter
	bpl keysho		; skip ahead if shorter
	clc
	sbc pkyend		; subtract last available adress
	tay
	txa
	sbc pkyend+1
	tax
	tya
	clc
	adc sedt2		; add difference
	txa
	adc #0
	bcs kyxit		; exit if no room, skip if memory not full

; expand or contract key area to make room for new key definition.
keysho:
kymove:	lda sedeal
	clc			; check if entire area expanded or contracted
	sbc sedsal
	lda sedeal+1
	sbc sedsal+1
	bcc keyins		; go insert new key defintion if yes
	ldy #0
	lda fktmp		; check if expand or contract
	bpl kshort		; skip if needs to be contracted

	lda sedeal
	bne newky4		; dec 1 from source addr
	dec sedeal+1		; sub 1 for borrow
newky4:	dec sedeal
	lda (sedeal),y		; move 1 byte up to expand
	ldy sedt2		; get offset = difference
	sta (sedeal),y		; move byte up
	jmp kymove		; loop until all bytes moved

kshort:	lda (sedsal),y		; get source byte
	ldy sedt2		; get offset = difference
	dec sedsal+1		; sub 1 to move down
	sta (sedsal),y		; move the byte down
	inc sedsal+1
	inc sedsal		; move source up 1 byte
	bne kymove
	inc sedsal+1		; add 1 for carry
	bne kymove		; always
; insert the new string defintion
keyins:	ldy sedt1		; get the key index
	jsr findky		; find buffer start address for this key
	sta sedsal
	stx sedsal+1		; save 2 byte address in temp loc
	ldy sedt1
	pla
	pha
	tax			; get zero page addr of params
	lda zp,x
	sta keysiz,y		; save key length
	tay
	beq kyinok		; equal to zero no keys...exit
	lda zp+1,x		; get & save low byte of string address
	sta sedeal
	lda zp+2,x		; get & save high byte of string address
	sta sedeal+1

kyinlp:	dey
	lda (sedeal),y		; get byte
	sta (sedsal),y		; store into buffer
	tya			; .y flags...end?
	bne kyinlp		; no... loop

kyinok:	clc			; for good exit carry clear
kyxit:	pla			; pop zero page address for params
	cli			; all done...release keyscan
	rts			; c-set is memory full error
; -------------------------------------------------------------------------------------------------
;*******************************
; Keyboard scanner
;*******************************
; Cursor routine
scnkey: lda crson
	bne key			; skip if cursor is off (run mode)
; solid cursor
	lda crssw
	bne key			; skip if cursor already visible
	
	inc crssw		; set visibility switch
	jsr get1ch		; get char and color under cursor
	sta config		; remember char under cursor
	ldx tcolor
	stx gdcol		; remember char color
	ldx color		; load actual color
	eor #$80		; inverse char
	jsr dspp		; print (reversed) char
; Keyboard scanner
key:	lda #$00
	sta shflag
	ldy #64			; last key index
	sty sfdx		; null key found
	sta via1+pra		; raise all lines
	ldx via1+prb		; check for a key down
	cpx #$ff		; no keys down?
	beq scnout		; branch if none
	tay			; .a=0 ldy #0
	lda #<mode1
	sta keytab
	lda #>mode1
	sta keytab+1
	lda #$fe		; start with 1st column
	sta via1+pra
scn20:	ldx #8			; 8 row keyboard
	pha			; save column output info
scn22:	lda via1+prb
	cmp via1+prb		; debounce keyboard
	bne scn22
scn30:	lsr			; look for key down
	bcs ckit		; none
	pha
	lda (keytab),y		; get char code
	cmp #$05
	bcs spck2		; if not special key go on
	cmp #$03		; could it be a stop key?
	beq spck2		; branch if so
	ora shflag
	sta shflag		; put shift bit in flag byte
	bpl ckut

spck2:	sty sfdx		; save key number
ckut:	pla
ckit:	iny
	cpy #65
	bcs ckit1		; branch if finished
	dex
	bne scn30
	sec
	pla			; reload column info
	rol
	sta via1+pra		; next column on keyboard
	bne scn20		; always branch
ckit1:	pla			; dump column output...all done
	jmp shflog		; evaluate shift functions
rekey:	ldy sfdx		; get key index
	lda (keytab),y		; get char code
	tax			; save the char
; check function key $85-$8c	
	cmp #$85		; check if function key
	bcc notfun		; skip - not a function key
	cmp #$8d
	bcs notfun		; skip - not a function key
	tya
	pha
	jsr funjmp		; do function key indirect
	pla
	tay
	ldx #$ff		; no key
	bcs keyxit		; done if carry flag set (always set in funjmp->dokeyf)
; Not a function key
notfun: cpy lstx		; same as prev char index?
	beq rpt10		; yes
	ldy #10			; no - reset delay before repeat
	sty delay
	bne ckit2		; always
rpt10:	and #$7f		; unshift it
	bit rptflg		; check for repeat disable
	bmi rpt20		; yes
	bvs scnrts
	cmp #$7f		; no keys ?
scnout:	beq ckit2		; yes - get out
	cmp #$14		; an inst/del key ?
	beq rpt20		; yes - repeat it
	cmp #$20		; a space key ?
	beq rpt20		; yes
	cmp #$1d		; a crsr left/right ?
	beq rpt20		; yes
	cmp #$11		; a crsr up/dwn ?
	bne scnrts		; no - exit
rpt20:	ldy delay		; time to repeat ?
	beq rpt40		; yes
	dec delay
	bne scnrts
rpt40:	dec kount		; time for next repeat ?
	bne scnrts		; no
	ldy #4			; yes - reset ctr
	sty kount
	ldy ndx			; no repeat if queue full
	dey
	bpl scnrts

ckit2:	ldy sfdx		; get index of key
keyxit:	sty lstx		; save this index to key found
	ldy shflag		; update shift status
	sty lstshf
ckit3:	cpx #$ff		; a null key or no key ?
	beq scnrts		; branch if so
	txa			; need x as index so...
	ldx ndx			; get # of chars in key queue
	cpx #10			; irq buffer full ?
	bcs scnrts		; yes - no more insert
putque:
	sta keyd,x		; put raw data here
	inx
	stx ndx			; update key queue count
scnrts:	lda #$7f		; setup pa7 for stop key sense
	sta via1+pra
	rts
; -------------------------------------------------------------------------------------------------
; shift logic
shflog:	lda shflag
	asl
	cmp #$08		; was it a control key
	bcc nctrl		; branch if not
	lda #6			; else use table #4
;
nctrl:
notkat:	tax
	lda keycod,x
	sta keytab
	lda keycod+1,x
	sta keytab+1

	jmp rekey
; rsr 12/08/81 modify for vic-40
; rsr  2/18/82 modify for 6526 input pad sense
; rsr  3/11/82 fix keyboard debounce, repair file
; rsr  3/11/82 modify for commodore 64
; -------------------------------------------------------------------------------------------------
; Jump vector: Function key indirect
funjmp: jmp (funvec)
; -------------------------------------------------------------------------------------------------
; Default function key handler
dokeyf: cpy lstx
	beq funrts		; exit not allowed to repeat
	lda ndx
	ora kyndx
	bne funrts		; exit - function queue not empty
	sta keyidx		; init pointer index into function area
	txa
	sec
	sbc #$85		; calc function key number 0-7
	clc
	tay
	lda keysiz,y		; get function key size
	sta kyndx		; - and store it for key scan
	jsr findky
	sta keypnt		; get function start addr
	stx keypnt+1		; - and save in keypnt

funrts: sec
	rts
; -------------------------------------------------------------------------------------------------
; Find address of function key given in y-reg
findky: lda pkybuf
	ldx pkybuf+1

findlp: clc
	dey			; found key yet?
	bmi fndout		; yes - done
	adc keysiz,y		; add function key size
	bcc findlp		; loop if no high byte carry-over
	inx
	bne findlp		; loop - always

fndout: rts
; -------------------------------------------------------------------------------------------------
; ***** 'keyboard tables'
keycod:				; keyboard mode 'dispatch'
	!word mode1
	!word mode2
	!word mode3
	!word contrl		; control keys
; -------------------------------------------------------------------------------------------------
; ***** 'editor.3' *****
; keyboard tables
mode1:										; normal keys
	!byte $14,$0d,$1d,$8b,$85,$87,$89,$11  ;del ret rt  f7  f1  f3  f5  dn
	!byte $33,$57,$41,$34,$5a,$53,$45,$01  ; 3   w   a   4   z   s   e  shf
	!byte $35,$52,$44,$36,$43,$46,$54,$58  ; 5   r   d   6   c   f   t   x
	!byte $37,$59,$47,$38,$42,$48,$55,$56  ; 7   y   g   8   b   h   u   v
	!byte $39,$49,$4a,$30,$4d,$4b,$4f,$4e  ; 9   i   j   0   m   k   o   n
	!byte $2b,$50,$4c,$2d,$2e,$3a,$40,$2c  ; +   p   l   -   .   :   @   ,
	!byte $5c,$2a,$3b,$13,$01,$3d,$5e,$2f  ;lb.  *   ;  hom shf  =   ^   /
	!byte $31,$5f,$04,$32,$20,$02,$51,$03  ; 1  <-- ctl  2  spc  C=  q stop
	!byte $ff	; end of table null
;
mode2:										; shift
	!byte $94,$8d,$9d,$8c,$86,$88,$8a,$91  ;ins RTN lft f8  f2  f4  f6  up
	!byte $23,$d7,$c1,$24,$da,$d3,$c5,$01  ; #   W   A   $   Z   S   E  shf
	!byte $25,$d2,$c4,$26,$c3,$c6,$d4,$d8  ; %   R   D   &   C   F   T   X
	!byte $27,$d9,$c7,$28,$c2,$c8,$d5,$d6  ; '   Y   G   (   B   H   U   V
	!byte $29,$c9,$ca,$30,$cd,$cb,$cf,$ce  ; )   I   J   0   M   K   O   N
	!byte $db,$d0,$cc,$dd,$3e,$5b,$ba,$3c  ;+gr  P   L  -gr  >   [  @gr  <
	!byte $a9,$c0,$5d,$93,$01,$3d,$de,$3f  ;lbg *gr  ]  clr shf  =  pi   ?
	!byte $21,$5f,$04,$22,$a0,$02,$d1,$83  ; !  <-- ctl  "  SPC  C=  Q  run
	!byte $ff	; end of table null
;
mode3:										; left window grahpics
	!byte $94,$8d,$9d,$8c,$86,$88,$8a,$91  ;ins RTN lft f8  f2  f4  f6  up
	!byte $96,$b3,$b0,$97,$ad,$ae,$b1,$01  ;red  W   A  cyn  Z   S   E  shf
	!byte $98,$b2,$ac,$99,$bc,$bb,$a3,$bd  ;pur  R   D  grn  C   F   T   X 
	!byte $9a,$b7,$a5,$9b,$bf,$b4,$b8,$be  ;blu  Y   G  yel  B   H   U   V
	!byte $29,$a2,$b5,$30,$a7,$a1,$b9,$aa  ; )   I   J   0   M   K   O   N
	!byte $a6,$af,$b6,$dc,$3e,$5b,$a4,$3c  ;+gr  P   L  -gr  >   [  @gr  <
	!byte $a8,$df,$5d,$93,$01,$3d,$de,$3f  ;lbg *gr  ]  clr shf  =  pi   ?
	!byte $81,$5f,$04,$95,$a0,$02,$ab,$83  ;blk <-- ctl wht spc  C=  Q  run
	!byte $ff	; end of table null
;
contrl:										; control keys
	!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff  ; ~   ~   ~   ~   ~   ~   ~   ~
	!byte $1c,$17,$01,$9f,$1a,$13,$05,$ff  ;red /w  /a  cyn /z  /s  /e   ~
	!byte $9c,$12,$04,$1e,$03,$06,$14,$18  ;pur /r  /d  grn /c  /f  /t  /x
	!byte $1f,$19,$07,$9e,$02,$08,$15,$16  ;yel /y  /g  yel /b  /h  /u  /v
	!byte $12,$09,$0a,$92,$0d,$0b,$0f,$0e  ;ron /i  /j  rof /m  /k  /o  /n
	!byte $02,$10,$0c,$82,$ff,$1b,$00,$ff  ;esc /p  /l  dsc  ~  /[  /@   ~	esc/dsc=e/d scrolling
	!byte $1c,$ff,$1d,$ff,$ff,$1f,$1e,$ff  ;/lb  ~  /]   ~   ~  /=  /pi  ~
	!byte $90,$06,$ff,$05,$ff,$ff,$11,$ff  ;blk /<-  ~  wht  ~   ~  /q   ~
	!byte $ff	; end of table null
; -------------------------------------------------------------------------------------------------
; <SHIFT> <RUN/STOP> String: DLOAD "*" + RUN
runtb:  !pet "d",$cc,$22,"*",cr		; dL"* <RETURN>
	!pet "run",cr	          	; run <RETURN>
; -------------------------------------------------------------------------------------------------
;****** address of color ram lines ******
linz0	= clrram
linz1	= linz0+collen
linz2	= linz1+collen
linz3	= linz2+collen
linz4	= linz3+collen
linz5	= linz4+collen
linz6	= linz5+collen
linz7	= linz6+collen
linz8	= linz7+collen
linz9	= linz8+collen
linz10	= linz9+collen
linz11	= linz10+collen
linz12	= linz11+collen
linz13	= linz12+collen
linz14	= linz13+collen
linz15	= linz14+collen
linz16	= linz15+collen
linz17	= linz16+collen
linz18	= linz17+collen
linz19	= linz18+collen
linz20	= linz19+collen
linz21	= linz20+collen
linz22	= linz21+collen
linz23	= linz22+collen
linz24	= linz23+collen

;****** color ram lines lo byte table ******
cldtb2:	!byte <linz0
	!byte <linz1
	!byte <linz2
	!byte <linz3
	!byte <linz4
	!byte <linz5
	!byte <linz6
	!byte <linz7
	!byte <linz8
	!byte <linz9
	!byte <linz10
	!byte <linz11
	!byte <linz12
	!byte <linz13
	!byte <linz14
	!byte <linz15
	!byte <linz16
	!byte <linz17
	!byte <linz18
	!byte <linz19
	!byte <linz20
	!byte <linz21
	!byte <linz22
	!byte <linz23
	!byte <linz24

;****** color ram lines hi byte table ******
cldtb1:	!byte >linz0
	!byte >linz1
	!byte >linz2
	!byte >linz3
	!byte >linz4
	!byte >linz5
	!byte >linz6
	!byte >linz7
	!byte >linz8
	!byte >linz9
	!byte >linz10
	!byte >linz11
	!byte >linz12
	!byte >linz13
	!byte >linz14
	!byte >linz15
	!byte >linz16
	!byte >linz17
	!byte >linz18
	!byte >linz19
	!byte >linz20
	!byte >linz21
	!byte >linz22
	!byte >linz23
	!byte >linz24
; -------------------------------------------------------------------------------------------------
; Dispatch table (control codes $00-$1F, $80-$9F)
ctable:	!word cuser-1
	!word chkcol-1		; -/orange
	!word scrsw-1		; enable/disable scrolling
	!word stprun-1		; stop/run
	!word cuser-1 
	!word chkcol-1		; white/F1
	!word cuser-1		; -/F3
	!word cuser-1		; -/F5
	!word cuser-1		; -/F7
	!word cuser-1		; -/F2
	!word cuser-1		; -/F4
	!word cuser-1		; -/F6
	!word cuser-1		; -/F8
	!word nxt1-1		; return or shifted return
	!word cuser-1
	!word cuser-1		; 
	!word chkcol-1		; -/black
	!word cdnup-1		; cursor down/up
	!word rvsf-1		; rvs on/off
	!word homclr-1		; home/clr
	!word delins-1		; delete/insert character
	!word chkcol-1		; -/brown
	!word chkcol-1		; -/lightred
	!word chkcol-1		; -/gray1
	!word chkcol-1		; -/gray2
	!word chkcol-1		; -/lightgreen
	!word chkcol-1		; -/lightblue
	!word chkcol-1		; -/gray3
	!word chkcol-1		; red/purple
	!word crtlf-1		; cursor right/left
	!word chkcol-1		; green/yellow
	!word chkcol-1		; blue/cyan
; -------------------------------------------------------------------------------------------------
; Length of function key texts
keylen: !byte key2-key1
	!byte key3-key2
	!byte key4-key3
	!byte key5-key4
	!byte key6-key5
	!byte key7-key6
	!byte key8-key7
	!byte keyend-key8

; Function key definitions
keydef:
key1:	!pet "run",cr					; F1
key2:	!pet "@cd:"					; F2
key3:	!pet "list",cr					; F3
key4:	!pet "oP8,8,15:pR8,",$22,"u0>",9,$22,":clO8",cr	; F4
key5:	!pet "dload",$22				; F5
key6:	!pet "dsave",$22				; F6
key7:	!pet "@$",cr					; F7
key8:	!pet "sys0",cr					; F8
keyend:
; -------------------------------------------------------------------------------------------------
; bits  -  bit position table
bits:	!byte $80,$40,$20,$10,$08,$04,$02,$01
; -------------------------------------------------------------------------------------------------
; Color control code table
; blk,wht,red,cyan,magenta,grn,blue,yellow
coltab:	!byte $90,$05,$1c,$9f,$9c,$1e,$1f,$9e
; org,brown,lred,gray1,gray2,lgreen,lblue,gray3
	!byte $81,$95,$96,$97,$98,$99,$9a,$9b
; rsr modify for vic-40 system			*** Just for fun from rev.1 c64-kernal rev.1 ;) ***
; rsr 12/31/81 add 8 more colors
; -------------------------------------------------------------------------------------------------
; 8-bit Colors for mode 6
col8bit:
	!byte $00,$ff,$3c,$e7,$57,$cd,$43,$fd	; black, white, red, cyan, violet, green, 6=blue, yellow
	!byte $7c,$58,$7d,$24,$6d,$ed,$8b,$b6	; orange, brown, ltred, dkgrey, grey, ltgreen, ltblue, ltgrey
; ***************************************** ZONE VDP **********************************************
!zone vdpeditor
; init vdp
vinit:	lda #0
	tax
	+VdpSetReg 17			; write VDP regs fast indirect
	+VdpWait WAIT23,7-1
vinilp:	lda VdpInitData,x
	sta VDPIndirect
	inx
	cpx #VdpInitDataEnd-VdpInitData
	+VdpWait WAIT23,14
	bne vinilp
	
	lda #VDPREG18
	+VdpWait WAIT23,11-1
	+VdpSetReg 18			; set register 18 V/H display adjust L 7-1,0,f-8 R
; clear 128kB VRAM (in mode 6/7 complete autoincrement)
	ldy #$00
	tya
	tax
	+VdpWait WAIT23,9-1
	+VdpWriteAddress		; set VRAM write address to $AAXX = $0000, Bank Reg already 0
	lda #$2				; 2x $10000 VRAM to clear
	sta temp1			; A16 counter
	tya				; VRAM init value =$00
	; x, y already 0
	+VdpWait WAITVRAM1,10
viramlp:sta VDPRamWrite
	inx
	+VdpWait WAITVRAM,8
	bne viramlp
	iny
	bne viramlp
	dec temp1			; dec A16 counter
	bne viramlp
; y already 0
	tya				; a=0
	sta lastvbank			; remember last vram bank
	+VdpWait WAIT23,23-1	
	+VdpSetReg 14			; set VRAM bank register to 0
	; x, y already 0
	tya				; a=0
	+VdpSetReg 16			; set VDP register 16 = palette pointer to $00 
	+VdpWait WAIT23,7-1	
vipallp:lda PaletteData,x		; load palette-color to write
	sta VDPPalette
	inx
	cpx #PaletteDataEnd-PaletteData	; finished ?
	+VdpWait WAIT23,14	
	bne vipallp			; ..no -> next color
; init vars
	ldy #<(!MODE<<7)		; shift startup mode to bit#7 and inverse to force mode set
	sty mode			; store mode flag
	lda #>font			; default font in ROM
	sta userfont_adr+1
	
	lda #0
	ldy #SPRITES
visprlp:sta sprite_flags,y		; clear all sprite flags
	dey
	bne visprlp

	sta sizex			; size x lo = zero
	sta x0
	sta y0
	sta tblx			; reset cursor
	sta pntr
	sta userfont_adr
	
	lda #TEXTCOL			; default color
	sta color
	lda #BGRCOL			; default background color
	sta bgcolor
	sta bgcolor_del

!if MODE=6 {				; default left colors mode 6
	lda #(BGRCOL<<4)
	sta bgcolor_left
	lda #(BGRCOL<<4)+BGRCOL		; double background color for vdp commands
	sta bgcolor_del
}	
	lda #EXTCOL			; a backdrop color
	ldy #((MODE&$01)<<2|PAL<<1)+1	; y = mode, bit#0=0 screen off
; set mode, backdrop bgcolor
vmode:	+VdpSetReg 7			; set reg 7 backdrop color
	tya
	pha				; remember mode
	and #$04			; isolate graphics 6/7 bit
	sta temp1			; remember
	asl				; shift to bit#7
	asl
	asl
	asl
	asl
	cmp mode			; check graphics mode
	beq vmpalnt			; ..no change -> skip
	
	sta mode			; store new mode
	lda #>SIZEX7			; default mode7 sizex
	ldx #COLUMNS7			; default mode7 columns
	bit mode			; check mode
	bmi vmode7			; -> skip if mode7
	ldx #COLUMNS6			; mode6 columns
	asl				; double size x for mode6
vmode7:	sta sizex+1			; store size x hi
	stx columns			; store screen columns
	dex
	stx scxmax			; store last screen column
	lda #VDPREG0			; base value
	ora temp1			; add graphics 6/7 bit
	+VdpSetReg 0			; set new graphics mode in reg 0

; calculate screen line table	
	ldy #>scnram			; screen base
	lda #<scnram
	ldx #0				; line index
vmlnlp:	sta ldtb2,x			; store low to table
	pha
	tya
	sta ldtb1,x			; store line high
	pla
	inx
	cpx #25				; last line?
	beq vmpalnt			; yes
	clc
	adc columns			; add columns
	bcc vmlnlp
	iny				; inc high
	bcs vmlnlp			; always

vmpalnt:pla				; get mode
	tay				; remember in y
	and #$02			; isolate NTSC/PAL bit
	ora #VDPREG9			; add lines bit
	+VdpWait WAIT23,39-1
	+VdpSetReg 9

	tya
	and #$08			; isolate yjk-bit
	+VdpWait WAIT23,7-1
	+VdpSetReg 25			; set yjk

	tya				; get mode again
	and #$01			; isolate on/off bit
	+VdpWait WAIT23,11-1
	bne vmscron			; -> screen on		
; off				; *** disable screen ***
	lda #VDPREG1 & $bf		; set mode reg 1 (M1+M2), bit#6 = 0 disables screen
	+VdpSetReg 1
	rts
vmscron:
; on				; *** enable screen ***
	lda #VDPREG1 | $40		; set mode reg 1 (M1+M2), bit#6 = 1 enables screen
	+VdpSetReg 1
	rts
; -------------------------------------------------------------------------------------------------
; clear screen with backgroundcolor
vclear:	lda #SIZEY
	sta dy
					; call from uprint clears only text screen area
	+VdpCommand zero, zero, zero, zero, zero, zero, sizex, sizex+1, dy, bgcolor_del, zero, HMMV
	
	jsr clrscr			; clear screen+color ram
	jmp waitcmd			; wait for command execution
; ----------------------------------------------------------------------------
; display a char on VDP screen
;   a = char, x = color
vchar:
; calc char start address in font
	pha				; remember char
	stx temp1			; remember color
	sta source_pointer		; store char to source pointer
	ldx #$00
	stx source_pointer+1
	asl				; *8 for char position in font
	rol source_pointer+1
	asl
	rol source_pointer+1
	asl
	rol source_pointer+1
	clc
	adc userfont_adr		; add userfont baseaddress
	sta source_pointer
	lda source_pointer+1
	adc userfont_adr+1
	sta source_pointer+1
; calc x
	lda pntr			; get column
	asl				; *3 (char byte width mode 6)
	clc
	adc pntr
	bit mode			; check mode
	bpl +				; skip mode 6
	asl				; *2 (*6 total) (width mode 7)
+	sta x1
; calc y	
	lda tblx			; get row
	asl				; *8 (userfont height)
	asl
	asl
	sta y1

	ldx #0
	stx temp2			; clear char line counter
; set vram address
	and #$c0			; isolate bit 6+7 (16k bank)
	cmp lastvbank			; same bank like last line ?
	beq vchlp			; ..yes -> skip set bank
	sta lastvbank			; remember bank
; set bank
	asl				; address bits 14+15 -> bit 0+1 (bank)
	rol
	rol
	+VdpSetReg 14			; set VRAM bank
	+VdpWait WAIT23,14
; set address (bank never changes inside a row)
vchlp:	lda y1				; get y line
	and #$3f			; remove VRAM address bit 14+15
	ldx x1
	ldy temp2			; get char line counter
	+VdpWriteAddress
	bit mode			; check mode
	bpl vchmod6			; write char mode 6
; write char mode 7
	ldx #FONTW			; set bit counter
	lda (source_pointer),y		; load data byte
	lsr				; shift one right for first shift left in loop
	ldy bgcolor			; load bgcolor as default
	+VdpWait WAITVRAM1,25-1
vchpxlp:asl
	bpl vchpix
	ldy temp1			; set pixel
vchpix:	sty VDPRamWrite			; write color
	ldy bgcolor			; default for clear pixel
	dex
	+VdpWait WAITVRAM,16
	bne vchpxlp			; next bit
; check last line of print char reached
	ldx temp2			; get char line counter
	inx				; inc line
	cpx #FONTH			; finished char ?
	beq vchx			; ..yes -> x to next column
; next line
	stx temp2			; store new char line counter
	inc y1				; next y
	bne vchlp			; always -> next char line
; write char mode 6
vchmod6:lda temp1			; get color
	asl				; shift to high nibble
	asl
	asl
	asl
	sta temp3			; remember left nibble color
	ldx #FONTW/2			; set 2pixel counter
	lda (source_pointer),y		; load data byte
	lsr				; shift one right for first shift left in loop
	ldy bgcolor_left		; load bgcolor as default
	+VdpWait WAITVRAM1,45-1
vch6plp:asl
	bpl vchpixl
	ldy temp3			; set pixel
vchpixl:sty temp4			; remember left pixel
; right pixel
	ldy bgcolor			; load bgcolor as default
	asl
	bpl vchpixr
	ldy temp1			; set pixel
vchpixr:pha				; remember source byte
	tya				; move to a
	ora temp4			; add left pixel
	sta VDPRamWrite			; write color
	pla				; restore source byte
	ldy bgcolor_left		; default for clear pixel
	dex
	+VdpWait WAITVRAM,39-1
	bne vch6plp			; next 2pixel
; check last line of print char reached
	ldx temp2			; char line counter
	inx				; inc line
	cpx #FONTH			; finished char ?
	beq vchx			; ..yes -> x to next column
; next line
	stx temp2			; store new char line counter
	inc y1				; next y
	bne vchlp			; always -> next char line
; both modes
; char finished
vchx:	pla				; restore char
	rts
; -------------------------------------------------------------------------------------------------
; wait for command execution finished
waitcmd:
	lda #2
	+VdpWait WAIT23,8
	+VdpSetReg 15			; reg 15 = 2 initiates read status-reg 2
	+VdpWait WAIT23,3-1		; wait for DVP
vrdylp:	lda VDPStatus			; read status
	lsr				; command execute bit#0 in c
	bcs vrdylp			; not finished -> wait
	rts
; **************************************** ZONE VDP-DATA ******************************************
!zone vdpdata
; vdp-data
VdpInitData:	; graphics7-mode
!byte VDPREG0|(MODE&$01)<<2,VDPREG1,$1f,$80,$00,$f7,$1e,$00,$28,VDPREG9|PAL<<1,$00,$01,$10,$f0,$00
	; reg  0: $0a mode control 0: bit#1-3=M3,M4,M5: mode 6=101, mode 7=111
	; reg  1: $00 mode control 1: mode 7 (bit#3 M2 = 0, bit#4 M1 = 0, bit#6 = 1, display enable)
	;             (bit#1 = 1 sprite size 16x16, bit#0 = 0 sprites not magnified, bit#5 = 0 virq off)
	; reg  2: $1f BitmapData table base address $0000 (bit#5 * $10000, bit#0-4 = 1)
	; reg  3: $ff color table base address $FFC0 (* $40)
	; reg  4: $00 pattern (character) generator table base address $0000 (* $800)
	; reg  5: $f7 sprite attribute table base address $FA00 (* $80) - Sprite color table = -200 -> $F800
	; reg  6: $1e sprite pattern (data) generator base address = $F000 (* $800)
	; reg  7: $00 8bit backdrop color 
	; reg  8: $28 mode control 2: refresh bit#3 = 1: 64k VRAM chips, bit#2 = 0 sprites not disabled
	;             bit#5=1 color 0 not transparent (mode6)
	; reg  9: $80 mode control 3: bit#1 = NTSC/PAL, #2 = EVEN/ODD, #3 = interlace, #7 = 192/212 lines
	; reg 10: $03 color table base address $F800 bit#0-2 = A14-A16
	; reg 11: $01 sprite attribute table base address $FA00 bit#0-1 = A15-A16
	; reg 12: $10 text/background blink color
	; reg 13: $f0 blink periods ON/OFF - f0 = blinking off
	; reg 14: $00 VRAM write addresss bit#0-2 = A14-A16
VdpInitDataEnd:
; ***** Color Palette - 16 colors, 2 byte/color: RB, 0G each 3bit -> C64 VICII-colors *****
PaletteData:
	!byte $00,$00,$77,$07,$70,$01,$17,$06	;	0=black		1=white		2=red		3=cyan
	!byte $56,$02,$32,$06,$06,$02,$72,$07	;	4=violet	5=green		6=blue		7=yellow
	!byte $70,$03,$60,$02,$72,$03,$11,$01	;	8=orange	9=brown		a=lightred	b=darkgrey
	!byte $33,$03,$52,$07,$27,$04,$55,$05	;	c=grey		d=litegreen	e=lightblue	f=lightgrey
PaletteDataEnd:
; ****************************************** KERNAL ***********************************************
;***************************************
;*                                     *
;* KK  K EEEEE RRRR  NN  N  AAA  LL    *
;* KK KK EE    RR  R NNN N AA  A LL    *
;* KKK   EE    RR  R NNN N AA  A LL    *
;* KKK   EEEE  RRRR  NNNNN AAAAA LL    *
;* KK K  EE    RR  R NN NN AA  A LL    *
;* KK KK EE    RR  R NN NN AA  A LL    *
;* KK KK EEEEE RR  R NN NN AA  A LLLLL *
;*                                     *
;***************************************
;***************************************
;* CBM KERNAL                          *
;*   MEMORY AND I/O DEPENDENT ROUTINES *
;* DRIVING THE HARDWARE OF THE         *
;* FOLLOWING MODEL : LC256             *
;* COPYRIGHT (C) 1983 BY CBM           *
;* COPYRIGHT (C) 2024 Vossi, Baleares  *
;***************************************
;
; ##### monitor #####
!zone kernal
;************************************************
;* kernal monitor                               *
;*                                              *
;* entry via call (jmp) or breakpoint (brk)     *
;* ---functions---                              *
;* <:>      alter memory                        *
;* <;>      alter registers                     *
;* <.>      alter assembly                      *
;* <r>      display registers                   *
;* <m>      display memory                      *
;* <a>      assemble                            *
;* <d>      disassemble                         *
;* <g>      start execution of code             *
;* <j>      start subroutine (jsr)              *
;* <t>      transfer memory                     *
;* <c>      compare memory                      *
;* <f>      fill memory                         *
;* <h>      hunt memory                         *
;* <l>      load                                *
;* <s>      save                                *
;* <v>      verify                              *
;* <b>      select RAM 0 bank (0-15)            *
;* <@>      disk command (@$ directory)         *
;* <x>      warm start basic                    *
;* <u>      set default disk unit               *
;*                                              *
;* for syntax & semantics see cbm kernal manual *
;* copyright (c) 1981 by cbm                    *
;************************************************
; Reset Entry
*= kernal+$f00

; ***** Warm start entry *******
monon:	jsr ioinit		; get i/o
	jsr restor		; vectors
	jsr jcint		; screen editor

; ***** Cold start entry ******
monoff:	jsr clrch		; clear channels
	lda #winit		; waste two bytes so timc=60950
	ldx #<monon		; point reset vectors at monitor on
	ldy #>monon
	jsr vreset
	cli			; release irq's

; ***** Call entry *****
timc:	lda #$40+$80
	sta msgflg		; error+messages on
	lda #ms34-ms1		; call entry
	sta tmpc
	bne b3			; branch always

; ***** Break entry *****
timb:	jsr clrch		; clr channels
	lda #ms36-ms1		; break entry
	sta tmpc
	cld 

; Save .y,.x,.a,flags, and pc
	ldx #5
; Pop registers from stack and save them
b1:	pla
	sta pch,x
	dex
	bpl b1
	
	sec
	lda pcl			; pc -1 for correct reentry address after break
	sbc #1
	sta pcl
	lda pch
	sbc #0
	sta pch


b3:	lda cinv
	sta invl		; save irq low
	lda cinv+1
	sta invh		; save irq high

	tsx
	stx sp			; save original sp
	cli			; clear ints
	lda #8			; set disk default to 8
	sta ddisk

b5:	ldy tmpc		; message code
	jsr spmsg		; print break/call

	lda #'r'		; display regs on entry
	bne s0			; branch always
; ***** Error entry *****

erropr:	jsr outqst
	pla
	pla

; ***** Command interpreter entry *****
strtm1=*-1
	lda #$40+$80
	sta msgflg		; i/o messages to screen
	lda #<buf		; put filename at bottom of basic buffer
	sta fnadr
	lda #>buf
	sta fnadr+1
	jsr crlf

st1:	jsr gnc			; read command

	cmp #' '
	beq st1			; span blanks
	jmp (usrcmd)		; user indirect for monitor
; -------------------------------------------------------------------------------------------------
; Command interpreter
s0:	ldx #0
	stx fnlen
	tay			; save current command

; Put return address for commands on stack
	lda #>strtm1
	pha
	lda #<strtm1
	pha

	tya			; current command in .a

s1:	cmp cmds,x		; is it this one?
	bne s2			; notit

	sta savx		; save current command

; Indirect jmp from table
	lda cmds+1,x
	sta tmp0
	lda cmds+2,x
	sta tmp0+1
	jmp (tmp0)

; Each table entry is 3 long---skip to next
s2:	inx
	inx
	inx
	cpx #cmdend-cmds
	bcc s1			; loop for all commands
; Command not in table
	jmp erropr		; syntax error
; -------------------------------------------------------------------------------------------------
; Command table
cmds:	!pet ':'		; alter memory
	!word altm
	!pet ";"		; alter registers
	!word altr
	!pet '.'		; alter assembly
	!word assem
	!pet 'r'		; display registers
	!word dsplyr
	!pet 'm'		; display memory
	!word dsplym
	!pet 'a'		; assemble
	!word assem
	!pet 'd'		; disassemble
	!word disasm
	!pet 'g'		; start execution
	!word go
	!pet 'j'		; jsr to subroutine
	!word gosub
	!pet 't'		; transfer memory
	!word trnsfr
	!pet 'c'		; compare memory
	!word compar
	!pet 'f'		; fill memory
	!word fill
	!pet 'h'		; hunt
	!word hunt
	!pet 'l'		; load
	!word ld
	!pet 's'		; save 
	!word ld
	!pet 'v'		; verify
	!word ld
	!pet 'b'		; bank RAM0
	!word bank
	!pet '@'		; disk command (alternate)
	!word disk
	!pet 'x'		; warm start basic
	!word xeit
	!pet 'u'		; default disk unit set
	!word unitd
cmdend:
; -------------------------------------------------------------------------------------------------
; Exit 'x'
xeit:	lda #0
	ldx #9
xcfaclp:sta hulp,x		; clear basic fac
	dex
	bpl xcfaclp

	pla			; remove command return from stack
	pla
	sei			; disable interrupts...all warm start code expects
	jmp (evect)		; go warmstart language
; -------------------------------------------------------------------------------------------------
; Move tmp0/tmp0+1 to PC memory location
putp:	lda tmp0		; move tmp0 to pch,pcl
	sta pcl
	lda tmp0+1
	sta pch
	rts
; -------------------------------------------------------------------------------------------------
; Set tmp2 to point to the saved regs in zero page
setr:	lda #<flgs		; set to access regs
	sta tmp0
	lda #>flgs
	sta tmp0+1
	lda #5
	rts
; -------------------------------------------------------------------------------------------------
; Prints '.:' or '.;' before data to permit alter after 'm' or 'r' command

altrit: pha			; preserve alter character
	jsr crlf
	pla
	jsr bsout

space:  lda #' '		; output a space
	!byte $2C		; skip two bytes

outqst: lda #'?'		; output question
	!byte $2C		; skip two bytes

crlf:   lda #cr			; do carriage return
	jmp bsout
; -------------------------------------------------------------------------------------------------
; Data for register display heading
regk:	!pet cr,"  "		; 3 spaces
	!pet " pc "," irq "," sr ac xr yr sp"
; -------------------------------------------------------------------------------------------------
; Display register function 'r'
dsplyr:	ldx #0
d2:	lda regk,x
	jsr bsout		; print heading
	inx
	cpx #dsplyr-regk	; max length
	bne d2
	lda #";"
	jsr altrit		; allow alter after display
	ldx pch
	ldy pcl
	jsr putwrd		; print program counter
	ldx invh
	ldy invl
	jsr putwrd		; print irq vector
	jsr setr		; set to print .p,.a,.x,.y,.s

; display memory subroutine
dm:	sta tmpc
	ldy #0			; indirect index
	sty fnlen		; fnlen is zero-page crossing flag...
dm1:	lda (tmp0),y
	jsr puthxs		; write byte of memory

dm2:	iny
	cpy tmpc		; bytes finished?
	bne dm1			; ..no -> next byte

	lda tmpc
	cmp #8
	bne dmx			; no char with register display

; display character
	ldy #0
dmchar:	lda (tmp0),y		; re-get byte from memory
	pha
	and #$7f		; mask control characters ($00-$1f and $80-$9f)
	cmp #$20
	pla
	bcs dmctl
	lda #'.'		; print control characters as '.'

dmctl:	jsr bsout
	iny
	cpy tmpc		; char finished?
	bne dmchar		; ..no -> next char
; increment indirect
	clc
	lda tmp0
	adc tmpc		; add bytes per line
	sta tmp0
	lda tmp0+1
	adc #0
	sta tmp0+1
	bcc dmx			; no zero page crossing
	dec fnlen

dmx:	rts
; -------------------------------------------------------------------------------------------------
; Display memory function 'm'
dsplym:	jsr parse		; read start adr
	bcs arrn		; ...err if no sa
	jsr t2t2		; sa to tmp2

; allow user to type just one address
	jsr parse		; read end adr
	bcc dsp123		; good...no default

	lda tmp2
	sta tmp0		; default low byte
	lda tmp2+1
	sta tmp0+1		; default hi byte

dsp123:	jsr t2t2		; sa to tmp0, ea to tmp2

dsp1:	jsr stop		; stop key?
	beq beqs1		; yes...break list

	lda #':'
	jsr altrit		; allow alter
	ldx tmp0+1
	ldy tmp0
	jsr putwrd		; write start address

	lda #8			; count of bytes
	jsr dm			; display bytes

	lda fnlen		; check for zero-crossing
	bne beqs1		; yup....
	sec
	lda tmp2
	sbc tmp0
	lda tmp2+1
	sbc tmp0+1
	bcs dsp1		; end >= start

beqs1:	rts			; a.o.k. exit

arrn:	jmp erropr		; syntax error jump
; -------------------------------------------------------------------------------------------------
; Alter register function ';'
altr:	jsr parse		; read new pc
	bcs arrn		; ...no address=error

	jsr putp		; alter pc

	jsr parse		; read new irq
	bcs arrn		; ...no address=error

	lda tmp0
	sta invl		; alter irq vector
	lda tmp0+1
	sta invh

	jsr setr		; set to alter r's
	jsr t2t2		; flags adr in t2
	jmp a4			; store regs
; -------------------------------------------------------------------------------------------------
; Select bank RAM 0 'b'
bank:	jsr parse		; get a byte
	bcs arrn		; ...if none...error
	cmp #2+1
	bcs arrn		; > 2 digits
	lda tmp0
	cmp #mxbank		; compare range
	bcs arrn		; too large
	sta temp		; store new bank
	jsr RestoreBank		; switch to RAM0 bank in temp	
	rts
; -------------------------------------------------------------------------------------------------
; Unit default for disk 'u'
unitd:	jsr parse		; get a byte
	bcs arrn		; ...if none...error
	cmp #2+1
	bcs arrn		; > 2 digits
	lda tmp0
	cmp #8			; range 8-31
	bcc arrn		; too small
	cmp #32
	bcs arrn		; too large
	sta ddisk
	rts
; -------------------------------------------------------------------------------------------------
; Alter memory - read adr and data ':'
altm:	jsr parse		; read alter adr
	bcs arrn		; ...if none...error
	jsr t2t2		; adr in t2

	lda #8			; allow 8 bytes change

; common code for ':' and ';'
a4:	sta tmpc		; number of bytes to change

a5:	jsr parse		; read byte
	bcs a9			; eol or error
	cmp #2+1
	bcs a9			; > 2 digits  
	lda tmp0

	ldy #0
	sta (tmp2),y		; store it away

; increment store address
	inc tmp2
	bne a6
	inc tmp2+1

a6:	dec tmpc		; count byte
	bne a5			; until zero
a9:	rts
; -------------------------------------------------------------------------------------------------
; Start execution function 'g'
go:	jsr parse		; get addr
	bcs g1			; none...pc is address
	jsr putp		; move addr to p.c.

g1:	ldx sp
	txs			; orig or new sp value to sp

	sei			; prevent disaster

	lda invh
	sta cinv+1		; set up irq vector
	lda invl
	sta cinv

; get flags,pch,pcl,.a,.x,.y
	ldx #0
g2:	lda pch,x
	pha			; everybody on stack
	inx
	cpx #6
	bne g2

; interrupt return sets everybody up from data on stack
	jmp prend
; -------------------------------------------------------------------------------------------------
; Jsr command 'j' - start executing at supplied address or (default) the current contents of the PC
gosub:	jsr parse		; get addr
	bcs gosub1		; none...pc is address
	jsr putp		; move addr to p.c.

; set flags,pc,.a,.x,.y
gosub1:	lda flgs		; flags
	pha
	lda pcl			; PC
	sta tmp0
	lda pch
	sta tmp0+1
	ldx xr			; regs
	ldy yr
	lda acc
	plp
	jmp (tmp0)
; -------------------------------------------------------------------------------------------------
; fill memory
;   syntax: f ssss eeee	vv = start eeee value
fill:	jsr range		; get sa in tmp2, calculate length, put in tmp1
	bcs errl1		; ...none=error
	jsr parse		; get fill value
	bcs errl1		; ...none=error
	cmp #2+1
	bcs errl1		; > 2 digits

	ldy #0
fillp:	lda tmp0
	sta (tmp2),y
	jsr inct2
	bcs errl1		; disallow bank-wrapping operations
	jsr dect1
	bcs fillp

	rts

errl1:	jmp erropr
; -------------------------------------------------------------------------------------------------
; Load ram function 'l' and 's'
ld:	ldy ddisk
	sty fa			; default device u
	ldy #0			; .y=0 to count name length
	sty fnlen		; reset file name length

l1:	jsr gnc			; get char
	beq errl1		; command only -> syntax error

	cmp #' '
	beq l1			; span blanks

	cmp #$22 ; "		; string next?
l2:	bne errl1		; no file name...

l3:	jsr gnc			; get character of name
	beq l5			; end...asssume load

	cmp #$22 ; "		; end of string?
	beq l7			; yes...could still be 'l' or 's'

	sta (fnadr),y		; store name
	inc fnlen
	iny
	cpy #16			; max file name length

l4:	beq errl1		; file name too long
	bne l3			; branch always
; default load address
l5	lda #$ff		; default no move load
	sta tmp0
	sta tmp0+1
; see if we got a load
l6:	lda savx		; get last command
	cmp #'v'		; check for verify?
	beq verify		; yes
	cmp #'l'		; load?
	bne l2			; no..not a load..error
	lda #0			; flag load
	!byte $2c		; skip next
verify:	lda #$80		; flag for verify
	sta verck		; store flag

	lda via2+prb		; get MMU reg
	and #%00001111		; isolate RAM0 bank
	ora verck		; set verify flag bit#7
	ldx tmp0
	ldy tmp0+1

	jsr load		; do load/verify

	bcs verifyx
	lda savx
	cmp #'v'		; check for verify
	beq verchk
verifyx:rts
; print verify result 
verchk:	jsr crlf
	lda status		; get verify state
	and #$10
	bne lssterr		; error detected
	jmp printok		; output ok

lssterr:lda #'b'
	jsr bsout
	lda #'a'
	jsr bsout
	lda #'d'
	jsr bsout
	rts

l7:	jsr gnc			; get next char
	beq l5			; no...default load
	cmp #','		; delimeter?
	beq l8			; yes
	cmp #' '
	bne l15			; no delimeter -> error

l8:	jsr parse		; get next parm
	bcs l5			; none...default load
	cmp #2+1
	bcs l15			; > 2 digits
	lda tmp0
	cmp #8			; range 8-31
	bcc l15			; too small
	cmp #32
	bcs l15			; too large
	sta fa

	jsr parse		; more parms?
	bcs l5			; no...default load
; set up start address
	lda tmp0
	sta stal
	lda tmp0+1
	sta stah

	jsr parse		; try to read end address
	bcs l6			; no... do load

; set up end save address
	lda tmp0
	sta eal
	lda tmp0+1
	sta eah

l14:	lda savx 		; was command save?
	cmp #'s'
	bne l15			; no...load can't have parms

	lda via2+prb		; get MMU reg
	and #%00001111		; isolate RAM0 bank
	ldx #<stal		; get addresses of params for save
	ldy #<eal
	jmp save

l15:	jmp erropr
; -------------------------------------------------------------------------------------------------
; Send disk command, print directory (@$) or read status '@'
disk:	ldy #0			; .y=0 to count string length
	sty status		; clear status @ i/o begin
	sty fnlen		; filename length of zero...
	ldx ddisk		; get default disk

	jsr gnc			; get char
	cmp #'$'
	bne diskcmd
	jmp diskdir		; ...branch if directory read

diskcmd:tya			; la=0
	ldy #15			; open command channel
	jsr setlfs		; .a-0 temporary channel #
	clc
	jsr open		; open a real channel
	bcs disk30		; exit if bad return

	jsr glc			; see if status check
	beq disk20		; yes

	pha
	ldx #0
	jsr ckout		; set up as output
	pla
	bcs disk30		; bad status return
	bcc disk15		; no...ok

disk10:	jsr basin		; get a character
disk15:	cmp #cr			; see if end
	php			; save for later
	jsr bsout		; out to floppy
	lda status
	bne disk28		; bad status returned
	plp			; end?
	bne disk10		; no...continue
	beq disk30		; yes...floppy done

disk20:	jsr crlf
	ldx #0
	jsr chkin		; tell floppy to speak
	bcs disk30		; bad device

disk25: jsr basin		; get a character
	cmp #cr
	php			; save test for later
	jsr bsout		; out to screen
	lda status		; check for bad basin
	and #$ff-$40		; remove eoi bit
	bne disk28		; report bad status
	plp			; end?
	bne disk25		; no...
	beq disk30		; yes...floppy done

disk28:	pla			; clean up...
disk29:	jsr error5		; report error #5 for bad device
disk30:	jsr clrch		; clean up
	lda #0
	sec			; close device (c=1)
	jmp close
; -------------------------------------------------------------------------------------------------
; Exchange temporaries
t2t2:	ldx #2
t2t21:	lda tmp0-1,x
	pha
	lda tmp2-1,x
	sta tmp0-1,x 
	pla
	sta tmp2-1,x
	dex
	bne t2t21
	rts
; -------------------------------------------------------------------------------------------------
; subtract tmp2 from tmp0, result in tmp0
sub0m2:	sec
	lda tmp0
	sbc tmp2
	sta tmp0
	lda tmp0+1
	sbc tmp2+1
	sta tmp0+1		; note .c=0 indicates tmp0 < tmp2, thus tmp0 is negative!
	rts
; -------------------------------------------------------------------------------------------------
; decrement t0
dect0:  lda #1
; subtract .a from tmp0
subt0:  sta sxreg
	sec
	lda tmp0
	sbc sxreg
	sta tmp0
	lda tmp0+1
	sbc #0
	sta tmp0+1
	rts
; -------------------------------------------------------------------------------------------------
; decrement t1
dect1:  sec
	lda tmp1
	sbc #1
	sta tmp1
	lda tmp1+1
	sbc #0
	sta tmp1+1
	rts
; -------------------------------------------------------------------------------------------------
; increment tmp2 - returns c=1 at overflow
inct2:	lda #1
; add .a to tmp2
addt2:  clc
	adc tmp2
	sta tmp2
	lda tmp2+1
	adc #0
	sta tmp2+1
addt2x:	rts
; -------------------------------------------------------------------------------------------------
; print address: x=hi, y=lo
putwrd:	txa
	jsr puthex
	tya

puthxs:	jsr puthex		; print byte in .a as two hex digits

putspc:	lda #' '		; print <space>
	jmp bsout

; print hex byte, preserve x
puthex:	stx sxreg
	jsr makhex
	jsr bsout
	txa
	ldx sxreg
	jmp bsout
; -------------------------------------------------------------------------------------------------
; convert .a to 2 hex digits & put msb in .a, lsb in .x
makhex:	pha
	jsr maknib		; convert nibble
	tax			; move low nibble to .x
	pla
	lsr			; sift high nibble right and convert it to .a
	lsr
	lsr
	lsr

maknib:	and #$0f
	cmp #$0a
	bcc mak0_9		; number 0-9
	adc #$06		; add 6+carry=7 for 'a-f
mak0_9	adc #'0'		; add petscii '0'
	rts
; -------------------------------------------------------------------------------------------------
; get last character: return in .a, z=1 if EOL, : (terminator) or ?
glc:	stx sxreg
	lda lastchr
	jmp gncchk
; get next character
gnc:	stx sxreg
	jsr basin
	sta lastchr		; save for glc
gncchk:	cmp #cr			; is it a cr
	beq gnceol		; eol-return with z=1
	cmp #':'
	beq gnceol		; eol-return with z=1
	cmp #'?'
gnceol:	php
	ldx sxreg
	plp
	rts
; -------------------------------------------------------------------------------------------------
; output "OK"
printok:lda #'o'
	jsr bsout
	lda #'k'
	jsr bsout
	rts
; -------------------------------------------------------------------------------------------------
; ##### messages #####
ms1:	!pet cr,"i/o error ",$a3
ms5:	!pet cr,"searching",$a0
ms6:	!pet "for",$a0
ms10:	!pet cr,"loadin",$c7
ms11:	!pet cr,"saving",$a0
ms21:	!pet cr,"verifyin",$c7
ms17:	!pet cr,"found",$a0
ms18:	!pet cr,"ok",$8d
ms34:	!pet cr,"** lc256 monitor 1.0 **",$8d
ms36:	!pet cr,"brea",$cb
; -------------------------------------------------------------------------------------------------
; Print message to screen only if output enabled
spmsg:	bit msgflg		; printing messages?
	bpl msg10		; no...
msg:	lda ms1,y
	php
	and #$7f
	jsr bsout
	iny
	plp
	bpl msg
msg10:	clc
	rts
; -------------------------------------------------------------------------------------------------
; ***** 'serial routines' *****
; command serial bus device to talk
ntalk:	ora #$40		; make a talk  adr
	!byte $2c		; skip two bytes
; command serial bus device to listen
nlistn:	ora #$20		; make a listen adr
list1:	pha
; check if last char in buffer? - If yes set EOI and send with EOI
	bit c3p0		; character left in buf?
	bpl list2		; no...
; send buffered character
	sec			; set eoi flag
	ror r2d2
;
!if JIFFY = 1{			; ##### JIFFY #####
	jsr jdSendByte	
} else{
	jsr isour		; send last character
}
;
	lsr c3p0		; buffer clear flag
	lsr r2d2		; clear eoi flag
;
list2:	pla			; talk/listen address
	sta bsour
	sei
!if JIFFY = 1{			; ##### JIFFY #####
	jsr jdIecDataH		; JD: set data 1, and clear serial bit count
} else{
	jsr datahi
}
	cmp #$3f		; clkhi only on unlisten
	bne list5
	jsr clkhi
;
list5:	lda via2+pra		; assert attention
	ora #$08
	sta via2+pra
;
isoura:	sei
	jsr clklo		; set clock line low
	jsr datahi
	jsr w1ms		; delay 1 ms

isour:	sei			; no irq's allowed
	jsr datahi		; make sure data is released
	jsr debvia		; data should be low

	bcc isr00		; device found -> skip

	jmp nodev		; no device found (data is after 1ms still high = no answer)

isr00:	jsr clkhi		; clock line high
	bit r2d2		; eoi flag test
	bpl noeoi
; do the eoi (leave clock high until receiver confirms with data high impulse)
isr02:	jsr debvia		; wait for data to go high
	bcc isr02
;
isr03:	jsr debvia		; wait for data to go low
	bcs isr03
;
noeoi:	jsr debvia		; wait for data high
	bcc noeoi
	jsr clklo		; set clock low
; set to send data
!if JIFFY = 1{			; ##### JIFFY #####
	txa
	pha

	ldx #8

jsr01:	lda via2+pra		; debounce the bus
	cmp via2+pra
	bne jsr01
	cmp via2+pra
	bne jsr01
	cmp via2+pra
	bne jsr01
	bit via2+pra		; time out?
	bmi jsr02		; no, -> skip
; time out
	pla
	tax
	jmp frmerr		; time out error

jsr02:	jsr datahi

	ror bsour		; bit set?
	bcs jsrhi		; yes, -> output a 1
; outpur a 0
	jsr datalo		; no, -> output a 0

jsrhi:	
; clock low 70us
!if CLOCK > 1{
	txa
	ldx #(CLOCK-1)*12	; 3*12 = 36 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax
}
	jsr clkhi
; clock high 26us
!if CLOCK > 1{
	txa
	ldx #(CLOCK-1)*5	; 3*5 = 15 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax
} 
!if CLOCK = 1{
	nop
	nop
	nop
	nop
}	
	lda via2+pra
	and #$df		; DATA line = L (5V)
	ora #$10		; CLK line = H (0V)
	php
	pha
	sta via2+pra

	and #$08		; Check ATN line?
	beq jsskip		; H (0V) -> skip
; ATN L (5V)
	lda bsour		; next bit to bit #7 (for Jiffy EOI flag if last bit)
	ror
	ror

; check if jiffy drive connected between bit 6 and 7
	cpx #2			; bit counter = 2 (second last bit?)
	bne jsskip		; no, -> skip

; wait 330us for data low = jiffy device connected
	ldx #$1e*CLOCK		; 30 * 11 cycles for each MHz
jssidlp:bit via2+pra		; DATA line (L)?
	bpl jifdev		; yes, -> jiffy device found

	dex
	bne jssidlp		; -> wait more
	beq jnojdev		; always -> no jiffy device found
; jiffy device found - wait for Data=H to continue
jifdev:	bit via2+pra		; DATA line (L)?
	bpl jifdev		; yes, -> wait until (H)
; set bit 6=jiffy, bit7=bit7 of byte to send
	ora #$40		; set bit 6 which means ...
	sta r2d2		; ... device is a JiffyDOS device

jnojdev:ldx #2			; restore bit counter

jsskip:	pla
	plp
	dex			; 8 bits done?
	bne jsr01		; no, -> send next bit

	pla
	tax
; now all eight bits have been sent it's up to the peripheral to signal the
; byte was received by pulling the serial data low. this should be done within
; one milisecond
} else{
	lda #8			; count 8 bits
	sta count
;
isr01:	lda via2+pra		; debounce the bus
	cmp via2+pra
	bne isr01
	cmp via2+pra
	bne isr01
	cmp via2+pra
	bne isr01
	asl			; set the flags
	bcc frmerr		; data must be hi
;
	ror bsour		; next bit into carry
	bcs isrhi
	jsr datalo
	bne isrclk
isrhi:	jsr datahi
isrclk:	
; clock low 70us
!if CLOCK > 1{
	txa
	ldx #(CLOCK-1)*12	; 3*12 = 36 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax
}
	jsr clkhi		; clock hi
; clock high 26us
!if CLOCK > 1{
	txa
	ldx #(CLOCK-1)*5	; 3*5 = 15 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax
}
!if CLOCK = 1{
	nop
	nop
	nop
	nop
}	
	lda via2+pra
	and #$ff-$20		; data high
	ora #$10		; clock low
	sta via2+pra
	dec count
	bne isr01
}
	lda #4*CLOCK		; set timer for 1ms
	sta via1+t2hi
isr04:	lda via1+ifr
	and #$20
	bne frmerr
	jsr debvia
	bcs isr04		; wait 1ms for data low

	cli			; let irq's continue
	rts
; -------------------------------------------------------------------------------------------------
; device not present error
nodev:	lda #$80
	!byte $2c
frmerr:	; framing error
	lda #$03
csberr:	jsr udst		; commodore serial bus error entry
	cli			; irq's were off...turn on
	clc			; make sure no kernal error returned
	bcc dlabye		; turn atn off ,release all lines
; -------------------------------------------------------------------------------------------------
; send secondary address after listen
nsecnd:	sta bsour		; buffer character
	jsr isoura		; send it
; release attention after listen
scatn:	lda via2+pra
	and #$ff-$08
	sta via2+pra		; release attention
	rts
; -------------------------------------------------------------------------------------------------
; talk second address
ntksa:	sta bsour		; buffer character
	jsr isoura		; send second addr
; shift over to listener
tkatn:	sei			; no irq's here
	jsr datalo		; data line low
	jsr scatn
	jsr clkhi		; clock line high jsr/rts
!if JIFFY = 1{			; ##### JIFFY #####
tkatn1:	bit via2+pra		; test bit6 and wait for CLK = H (0V)
	bvs tkatn1		; no, -> wait
} else{
tkatn1:	jsr debvia		; wait for clock to go low
	bmi tkatn1
}
	cli			; irq's okay now
	rts
; -------------------------------------------------------------------------------------------------
; buffered output to serial bus
nciout:	bit c3p0		; buffered char?
	bmi ci2			; yes...send last
; first char: set c3p0 buffered char flag and don't send char!
	sec			; no...
	ror c3p0		; set buffered char flag
	bne ci4			; branch always
; from second char: send buffered char in bsour - leave c3p0 flag untouched
ci2:	pha			; save current char
!if JIFFY = 1{			; ##### JIFFY #####
	jsr jdSendByte
} else{
	jsr isour		; send last char
}
	pla			; restore current char
ci4:	sta bsour		; buffer current char
	clc			; carry-good exit
	rts
; -------------------------------------------------------------------------------------------------
; send untalk command on serial bus
nuntlk:	sei
!if JIFFY = 1{			; ##### JIFFY #####
	lda via2+pra
	ora #$08
	sta via2+pra		; ATN line = L (0V)

	jsr clklo
} else{
	jsr clklo
	lda via2+pra		; pull atn
	ora #$08
	sta via2+pra
}
	lda #$5f		; untalk command
	!byte $2c		; skip two bytes
; -------------------------------------------------------------------------------------------------
; send unlisten command on serial bus
nunlsn:	lda #$3f		; unlisten command
	jsr list1		; send it
; release all lines
dlabye:	jsr scatn		; always release atn
; delay then release clock and data
dladlh:	txa			 ; delay approx 60 us
	ldx #10*CLOCK
dlad00:	dex
	bne dlad00
	tax
	jsr clkhi
	jmp datahi
; -------------------------------------------------------------------------------------------------
; input a byte from serial bus
;
; this routine reads a byte of data from the serial bus using full handshaking.
; the data is returned in the accumulator. before using this routine the TALK
; routine, talk/$FFB4, must have been called first to command the device on
; the serial bus to send data on the bus. if the input device needs a secondary
; command it must be sent by using the TKSA routine, $FF96, before calling this routine.
;
; errors are returned in the status word which can be read by calling the
; READST routine, ReadIoStatus.
nacptr:	
!if JIFFY = 1{			; ##### JIFFY #####
IecByteIn2:
	jmp jdIecByteIn

IecByteIn:			; save the serial bus bit count
}
 	sei			; no irq allowed
	lda #$00		; set eoi/error flag

	sta count
	jsr clkhi		; make sure clock line is released
acp00a:	jsr debvia		; wait for clock high
	bpl acp00a
	jsr datahi		; data line high
;
eoiacp:	lda #1*CLOCK		; set timer 2 for 256us
	sta via1+t2hi
acp00:	lda via1+ifr
	and #$20		; check the timer
	bne acp00b		; ran out.....
	jsr debvia		; check the clock line
	bmi acp00		; no not yet
	bpl acp01		; yes.....
;
acp00b:	lda count		; check for error (twice thru timeouts)
	beq acp00c
	lda #2
	jmp csberr		; st = 2 read timeout
; timer ran out do an eoi thing
acp00c:	jsr datalo		; data line low
	jsr dladlh		; delay and then set datahi (also clkhi) ************ VOSSI from vic kernal **********
	lda #$40
	jsr udst		; or an eoi bit into status
	inc count		; go around again for error check on eoi
	bne eoiacp
; do the byte transfer
acp01:	lda #8			; set up counter
	sta count
;
acp03:	lda via2+pra		; wait for clock high
	cmp via2+pra		; debounce
	bne acp03
	cmp via2+pra		; debounce
	bne acp03
	cmp via2+pra		; debounce
	bne acp03
	asl			; shift data into carry
	bpl acp03		; clock still low...
	ror bsour1		; rotate data in
;
acp03a:	lda via2+pra		; wait for clock low
	cmp via2+pra		; debounce
	bne acp03a
	cmp via2+pra		; debounce
	bne acp03a
	cmp via2+pra		; debounce
	bne acp03a
	asl
	bmi acp03a
	dec count
	bne acp03		; more bits.....
; ...exit...
	jsr datalo		; data low
	bit status		; check for eoi
	bvc acp04		; none...
;
	jsr dladlh		; delay then set data high
;
acp04:
!if JIFFY = 1 & CLOCK > 1{	; ##### JIFFY #####
; delay to fix errors with non-jiffy drives	
	txa
	ldx #(CLOCK-1)*2	
-	dex
	bne -
	tax
}
	lda bsour1
	cli			; irq is ok
	clc			; good exit
	rts
; -------------------------------------------------------------------------------------------------
; set clock line high (inverted)
clkhi:	lda via2+pra
	and #$ff-$10
	sta via2+pra
	rts
; -------------------------------------------------------------------------------------------------
; set clock line low  (inverted)
clklo:	lda via2+pra
	ora #$10
	sta via2+pra
	rts
; ########################################### JIFFY ###############################################
!if JIFFY = 1{
;**  This is a patch that clears the flag that indicates a JiffyDOS device, before setting data hi.
jdIecDataH:
	lda #$00
	sta r2d2		; clear JiffyDOS device flag
}
; set data line high (inverted)
datahi:
	lda via2+pra
	and #$ff-$20
	sta via2+pra
	rts
; -------------------------------------------------------------------------------------------------
; set data line low  (inverted)
datalo:	lda via2+pra
	ora #$20
	sta via2+pra
	rts
; -------------------------------------------------------------------------------------------------
; debounce the via
debvia:	lda via2+pra
	cmp via2+pra
	bne debvia
	cmp via2+pra
	bne debvia
	cmp via2+pra
	bne debvia
	asl			; shift the data bit into the carry...
	rts			; ...and the clock into neg flag
; -------------------------------------------------------------------------------------------------
; delay 1ms for isoura (vic uses t2, c64 uses loop)
w1ms:	lda #4*CLOCK		; set timer for 1ms
	sta via1+t2hi
w1ms1:	lda via1+ifr
	and #$20
	beq w1ms1
	rts
; *******************************
; written 8/11/80 bob fairbairn
; test serial0.6 8/12/80  rjf
; change i/o structure 8/21/80 rjf
; more i/o changes 8/24/80 rjf
; final release into kernal 8/26/80 rjf
; some clean up 9/8/80 rsr
; add irq protect on isour and tkatn 9/22/80 rsr
; fix untalk 10/7/80 rsr
; modify for vic-40 i/o system 12/08/81 rsr
; add sei to (untlk,isoura,list2) 12/14/81 rsr
; modify for 6526 flags fix errs 12/31/81 rsr
; modify for commodore 64 i/o  3/11/82 rsr
; change acptr eoi for better response 3/28/82 rsr
; change wait 1 ms routine for less code 4/8/82 rsr
; ******************************
; ########################################### JIFFY ###############################################
!if JIFFY = 1{
;**  This routine is a patch to the original load routine and tests if the
;    current device is a JiffyDOS device. If not, the routine jumps back to
;    the original loader. Some handshaking is done.
jdLOAD:
	jsr loding
	bit r2d2		; JiffyDOS device?
	bmi jload		; yes, -> continue
	jmp ld40		; original load routine

jload:	sei
!if CLOCK = 1{
	ldy #0
}
jload2:	jsr ud60		; <STOP> key pressed?
	bpl jlstop

	lda via2+pra
	and #$07		; all IEC lines L (5V)
	sta jdtemp
	sta via2+pra
; receiver ready to receive
	ora #$20		; DATA line = H (0V)
	tax			; X = %0010 0xxx

jlclkh:	bit via2+pra		; CLK line = L (5V)?
	bvc jlclkh		; no, -> wait
	bit via2+pra		; debounce
	bvc jlclkh		; no, -> wait
	bit via2+pra		; debounce
	bvc jlclkh		; no, -> wait
; sender ready to send
	bpl jldath		; if DATAline = (H), -> LOAD
; loops for CLK=0 in us = EOI
; $64*11 = 1100 cycles loop  -> $3f*17 cycles loop
	ldx #$37*CLOCK
jleoilp:bit via2+pra		; EOI?
	bvc jleoi		; yes, -> 
	bit via2+pra		; again to get 6 more cycles
	bvc jleoi		; needed for 4MHz clock loops below $ff! loops

	dex
	bne jleoilp		; loop again for EOI

	lda #$42		; EOI & READ TIME OUT
	!byte $2C		; skip next
jleoi:	lda #$40		; EOI found
	jsr udst		; update status

	clc
	!byte $24		; skip next
jlstop:	sec
	jsr RestoreBank		; restore RAM0 bank
	bcs jlbreak		; if carry set, stop detected

	jmp jdCloseFile

jlbreak:jmp break

jldath:	bit via2+pra		; DATA line = L (5V)?
	bpl jldath		; no, -> wait
	bit via2+pra		; DATA line = L (5V)?
	bpl jldath		; no, -> wait
	bit via2+pra		; DATA line = L (5V)?
	bpl jldath		; no, -> wait
; prepare transmission with DATA+CLK = L (5V)
jlread:	
!if CLOCK = 1{
	nop
	nop
	nop
	nop
}
; original = 18 cycles (via bit/bpl to via stx)
!if CLOCK > 1{
	ldy #3+(CLOCK-2)*4	; 3*4 = 12 * 5 extra cycles @ 4MHz
-	dey
	bne -
}
	lda jdtemp
	stx via2+pra		; store %0010 0xxx
				; DATA line = L (5V)
	bit via2+pra		; CLK line = L (5V)
	bvc jload2		; no, -> try again
; start transmission with DATA = L (5V)
!if CLOCK = 1{
	nop
}
; original = 17 cycles (via stx to ora) (incl. nop above / without 15 cy)
; optimized !!! 15+16=31 is perfect (extra 14-18 works)
!if CLOCK = 2{
	pha			; 16 cycles
	pla
	pha
	pla
	nop
}
!if CLOCK > 2{
; optimized !!!!
; 15+31=46 at 3MHz, 15+46=61 at 4MHz are perfect (at 4 MHz works extra 41-51)
	ldy #3+(CLOCK-2)*3	; 2*3 = 6 * 5 extra cycles @ 4MHz
-	dey
	bne -
}
	sta via2+pra		; store %0000 0xxx
; read 8 bits 
	ora via2+pra		; receive 2 bits
	lsr
	lsr
	nop
!if CLOCK = 2{
	nop
	nop
	nop
	nop
	nop
}
!if CLOCK > 2{
; original = 10 cycles via ora to ora)
	ldy #1+(CLOCK-2)*2	; 2*2 = 2 * 5 extra cycles @ 4MHz
-	dey
	bne -
	nop
	nop
}
	ora via2+pra		; receive 2 bits
	lsr
	lsr
	eor jdtemp
!if CLOCK = 2{
	pha			; 11 cycles
	pla
	nop
	nop
}
!if CLOCK > 2{
; original = 11 cycles via ora to eor)
	ldy #4+(CLOCK-3)*2	; 1*2 = 2 * 5 extra cycles @ 4MHz
-	dey
	bne -
	nop
}
	eor via2+pra		; receive 2 bits
	lsr
	lsr
	eor jdtemp
!if CLOCK = 2{
	pha			; 11 cycles
	pla
	nop
	nop
}
!if CLOCK > 2{
; original = 11 cycles via ora to eor)
	ldy #4+(CLOCK-3)*2	; 1*2 = 2 * 5 extra cycles @ 4MHz
-	dey
	bne -
	nop
}
	eor via2+pra		; receive 2 bits
!if CLOCK > 1{
; original = min. 20 cycles via eor to jlread)
; NECESSARY !
	ldy #3+(CLOCK-1)*6	; 3*6 = 18 * 5 extra cycles @ 4MHz
-	dey
	bne -
} 
!if CLOCK > 1{
	ldy #0
}
	bit verck		; performing verify?
	bmi jlveri		; yes, -> VERIFY

	sta (eal),y		; save read byte

jlvcont:inc eal
	bne jlread

	inc eal+1
	jmp jlread		; next byte
; VERIFY
jlveri:	cmp (eal),y		; compare OK?
	beq jlvcont		; yes, -> continue

	sec
	lda #$10		; VERIFY ERROR
	sta status
	bne jlvcont		; always ->
; ########################################### JIFFY ###############################################
 ; JIFFY DOS subroutines
;**  This is a routine used by JiffyDOS to untalk device (A), then TALK and
;    TKSA is executed to current device with current secondary address.
jdTalkTKSA:
	jsr untlk

	lda fa
	jsr talk

	lda sa
	jmp tksa
; ########################################### JIFFY ###############################################
;**  JiffyDOS: read an IEC byte 
jdIecByteIn:
	sei
	bit r2d2		; JiffyDOS device?
	bvs jbyin		; yes

	jmp IecByteIn		; no, -> normal read routine
;**  Read a byte from the IEC bus	
jbyin:	lda via2+pra
	cmp via2+pra		; debounce
	bne jbyin
	cmp via2+pra		; debounce
	bne jbyin
	cmp via2+pra		; debounce
	bne jbyin
	cmp #$40		; DATA = H (0V) + CLK = H (0V) 
	bcc jbyin		; yes, wait
; sender is ready = CLK gets L (5V) >=$40
;   receiver here still holds CLK H (0V)
	and #$07
	pha			; save .a
	
; original >= 14 cycles wait for VIC raster (orig. 31 via lda to sta)
	txa			; save .x
	ldx #1+(CLOCK-1)*10	; 3*10 = 30 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	nop
	nop

	pla			; restore .a
; send ready to receive with DATA = L (5V)
	sta via2+pra		; store %0000 0xxx = all lines L (5V)
	sta bsour1
; optimized: 14 is perfect (12-16 are OK)
!if CLOCK = 2{
	pha			; 14 cycles
	pla
	pha
	pla
}
!if CLOCK > 2{
; original = 16 cycles via sta to ora)
; optimized: 30 / 45 cycles are perfect (at 4MHz works 40-50)
	txa			; save .x
	ldx #1+(CLOCK-2)*3	; 2*3 = 6 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	nop
	bit zp
} 	
	lda bsour1		; optimized - one cycle less (orig. 2x nop) 
	ora #$20
	pha
; read 8 bits
	ora via2+pra		; receive 2 bits
	lsr
	lsr
	nop
!if CLOCK = 2{
	pha			; 10 cycles
	pla
	bit zp
}
!if CLOCK > 2{
; original = 10 cycles via ora to ora)
	pha
	txa			; save .x
	ldx #1+(CLOCK-3)*2	; 1*2 = 2 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
	bit zp			; 3 cycles
}
	ora via2+pra		; receive 2 bits
	lsr
	lsr
	eor bsour1
!if CLOCK = 2{
	pha			; 11 cycles
	pla
	nop
	nop
}
!if CLOCK > 2{
; original = 11 cycles via ora to eor)
	pha
	txa			; save .x
	ldx #1+(CLOCK-3)*2	; 1*2 = 2 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
	nop
	nop
	nop
}
	eor via2+pra		; receive 2 bits
	lsr
	lsr
	eor bsour1
!if CLOCK = 2{
	pha			; 11 cycles
	pla
	nop
	nop
}
!if CLOCK > 2{
; original = 11 cycles via eor to eor)
	pha
	txa			; save .x
	ldx #1+(CLOCK-3)*2	; 1*2 = 2 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
	nop
	nop
	nop
}
	eor via2+pra		; receive 2 bits
	sta bsour1

!if CLOCK > 1{
; original = 15 cycles via eor to sta)
	txa			; save .x
	ldx #1+(CLOCK-2)*3	; 2*3 = 6 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	nop
	bit zp			; 3 cycles
} 
	pla
	bit via2+pra
	sta via2+pra		; store %0010 0xxx, DATA line = H (0V)
; Check EOI?
	bvc jbyinx		; no EOI: CLK = H (0V) -> exit and return byte
; CLK = L (5V)
	bpl jseteoi		; EOI: CLK = L (5V), DATA = H (0V) -> set EOI
; DATA = L (5V)
	lda #$42		; EOI & READ TIME OUT
	jmp csberr
; ########################################### JIFFY ###############################################
jdIecByteIn2:
	jsr jdIecByteIn

	pha
	bit r2d2		; test bit6, device is a JD device?
	bvc jbyin2x		; no, ->

	ldy #0
	lda (fnadr),y		; current filename
	cmp #'$'		; directory?
	beq jbyin2x		; yes, -> exit

	inc sa			; why ???
	jsr jdTalkTKSA

	dec sa
	asl r2d2		; why ???

jbyin2x:pla
	rts
; ########################################### JIFFY ###############################################
;**  Set bit 4 of the Status
SetStatusBit4:
	lda #$10
	jmp udst
; ########################################### JIFFY ###############################################
jseteoi:lda #$40		; set EOI
	jsr udst		; update status

jbyinx:	lda bsour1		; read data byte from IEC bus

	cli			; enable interrrupts
	clc			; good exit
	rts
; ########################################### JIFFY ###############################################
;**  The following routine is used to send a byte to a device on ther serial bus.
;    The routine checks if the device is a JiffyDOS device by reading r2d2.
;    If it is not a JiffyDOS device, the routine jumps back to the original load routine.
jdSendByte:
	sei
	bit r2d2		; JiffyDOS device?
	bvs jsbyte		; yes -> send jiffy byte 
; second check?
	lda r2d2		; JiffyDOS device?
	cmp #$a0		; shifted in EOI bit#7 + jiffy bit#6 shifted to #5
	bcs jsbyte		; yes -> send jiffy byte 
; normal send
	jmp isour		; no jiffy dev -> send byte standard 

jsbyte:	txa			; save X
	pha

	lda bsour		; byte to be send
	and #$f0
	pha			; save upper four bits

	lda bsour
	and #$0f
	tax			; X = lower four bits of data byte


jsb2dah:lda via2+pra		; DATA line = H (0V)?
	bpl jsb2dah		; yes -> wait
; receiver is ready if sets data = L (5V)
	and #$07		; store via2 pra register value with cleared IEC bits
	sta bsour		; ..to byte to send
	sta via2+pra		; all lines L (5V)
; start transmission with setting all lines L
!if CLOCK > 1{
; original = 10 cycles via sta to ora)
	txa			; save .x
	ldx #1+(CLOCK-2)*2	; 2*2 = 4 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
}
; send 8 bits
	pla			; upper four bits
	ora bsour
	sta via2+pra		; send bits 4 (CLK) and 5 (DATA)
	lsr
	lsr
	and #$f0
	ora bsour
; tuned here +2cycles for the complete time of bits 4,5 + 6,7 transmission
!if CLOCK = 2{
	pha			; 14 cycles
	pla
	pha
	pla
}
!if CLOCK > 2{
; original = 12 cycles via sta to sta)
	pha
	txa			; save .x
	ldx #3+(CLOCK-3)*3	; 1+1*3 = (1 + 3) * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
	nop			; tuned here
}
	sta via2+pra		; send bits 6 and 7
; send lower nibble encoded from table
!if CLOCK > 1{
; original = 10 cycles via sta to sta)
	txa			; save .x
	ldx #1+(CLOCK-2)*2	; 2*2 = 4 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	bit zp			; tuned here 3cy
}
	lda jdLoNibbleEnc,x	; load lower four bits encoded
	ora bsour
	sta via2+pra		; send bits bits 4+5 of encoded value

	lsr
	lsr
	and #$f0
	ora bsour
!if CLOCK = 2{
	pha			; 13 cycles
	pla
	nop
	nop
	nop
}
!if CLOCK > 2{
; original = 12 cycles via sta to sta)
	pha
	txa			; save .x
	ldx #2+(CLOCK-3)*3	; 1+1*3 = (1 + 3) * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
	nop			; tuned here
}
	sta via2+pra		; send bits 6+7 of encoded value

; check if EOI needed?
	and #$0f
	bit r2d2		; EOI needed?
	bmi jsb2eoi		; yes, -> signal EOI: CLK = L (0V)
; no EOI
	ora #$10		; CLK = H (0V)
jsb2eoi:
!if CLOCK = 2{
	pha			; 13 cycles
	pla
	nop
	nop
	nop
}
!if CLOCK > 2{
; original = 12 cycles via sta to sta)
	pha
	txa			; save .x
	ldx #3+(CLOCK-3)*3	; 1*3 = 3 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	pla
}
	sta via2+pra		; CLK+DATA = H (CLK = L if EOI)
!if CLOCK > 1{
; original = 14 cycles via sta to sta)
	txa			; save .x
	ldx #1+(CLOCK-2)*3	; 2*3 = 6 * 5 extra cycles @ 4MHz
-	dex
	bne -
	tax			; restore .x
	nop
	nop
}
	pla
	tax			; restore X

	lda bsour
	ora #$10		; CLK = H (0V)
	sta via2+pra
; small delay to prevent false frame errors
	+IECDelay (CLOCK-1)*2

	bit via2+pra		; DATA line = H (0V)?
	bmi jsberr		; no, -> error

	cli			; enable interrrupts
	clc			; good exit
	rts
; error
jsberr:	jmp frmerr		; no -> framing error
; ########################################### JIFFY ###############################################
; Encoding table for lower nibble of the byte to be send to a JiffyDOS device.
jdLoNibbleEnc:
!byte $00, $80, $20, $A0, $40, $C0, $60, $E0
!byte $10, $90, $30, $B0, $50, $D0, $70, $F0
}
; -------------------------------------------------------------------------------------------------
; alocat - alocatate space
;  entry:
;    .x- low # of bytes needed
;    .y- high # of bytes needed
;  exit :
;    c-clr  no problem alocatating space
;     .x,.y is start address of alocatated space
;    c-set  problem with alocatation
;     if .a =$ff then alocatation refused (cannot cross segment boundrys)
;     if .a =$4x then top of memory needs to be changed
;     return to language
;-----------------------------------------------------------------------
alocat:
tttop:	txa			; calc new hiadr
	sec
	eor #$ff		; invert
	adc hiadr		; sub low from end of system RAM
	tax
	tya
	eor #$ff
	adc hiadr+1		; sub high
	tay
	bcs top010
refuse:	lda #$ff		; allocation refused...crossed boundry
topbad:	ora #$40		; want top of memory changed
	sec			; c=1 Not enough memory available 
	rts			; return unsuccessful

top010:	cpy memsiz+1		; compare new high address with user memory high
	bcc topbad		; branch if new high lower = not enough memory alocatatable
	bne topxit		; branch to memoryok if new high > 
	cpx memsiz		; if higbyte equal compare low
	bcc topbad		; branch if lower = not enough memory alocatatable
topxit:	stx hiadr		; store new end of system memory ($)
	sty hiadr+1
	clc
	rts
; -------------------------------------------------------------------------------------------------
; ##### channelio #####
;*****************************************
;* getin -- get character from channel   *
;*      channel is determined by dfltn.  *
;* if device is 0, keyboard queue is     *
;* examined and a character removed if   *
;* available.  devices 1,3-31 advance to *
;* basin.                                *
;*                                       *
;* exit:  .a = character                 *
;*        cy = 1, stop key error for cas-*
;*                cassetes and rs232     *
;*           = 0, otherwise.             *
;*        z  = 1, if kbd and queue empty.*
;*****************************************
; F444
ngetin:	lda dfltn		; check device
	bne gn10		; not keyboard

	lda ndx			; queue index
	ora kyndx		; check function key que
	beq gn20		; nobody there...exit

	sei
	jsr jlp2		; go remove a character
	clc
	rts

; Check for input from device 2 = RS232
gn10:
;	cmp #2			; is it rs-232
;	beq gn232
	jmp basin		; no...use basin

; getin RS232
gn232:
;	sty xsav		; save .y...
;	stx savx		; ..and .x
;	ldy ridbs		; get last byte address
;	cpy ridbe		; see if buffer emptyy
;	bne gn15		; rs232 buffer not empty...
;
;	lda acia+cdr		; make sure receiver is on
;	and #$FD
;	ora #$01		; bits(10) = 01 now
;	sta acia+cdr
;	lda rsstat		; set empty input buffer condition
;	ora #$10
;	sta rsstat
;	beq gnexit		; always

; Get one byte from RS232 input buffer
gn15:
;	lda rsstat		; clear empty buffer status
;	and #$ef
;	sta rsstat
;	ldx i6509
;	lda ribuf+2
;	sta i6509		; point at buffer
;	lda (ribuf),y		; get last char
;	stx i6509		; restore
;	inc ridbs		; inc to next posistion
;	bit sa			; check for ascii flag
;	bpl gnexit		; not on...
;	jsr tocbm		; convert to cbm code
gnexit:
;	ldy xsav		; restore .y
;	ldx savx
gn20:	clc			; good return
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* basin-- input character from channel*
;*     input differs from get on device*
;* #0 function which is keyboard. the  *
;* screen editor makes ready an entire *
;* line which is passed char by char   *
;* up to the carriage return.          *
;* other devices are:                  *
;*      0 -- keyboard                  *
;*      2 -- usb                       *
;*      3 -- screen                    *
;*   4-31 -- iec   bus                *
;*                                     *
;* exit: cy=1, error for  usb.         *
;*       cy=0, otherwise.              *
;*                                     *
;*       all other errors must be de-  *
;*       tected by checking status !   *
;***************************************
nbasin:	lda dfltn		; check device
	bne bn10		; is not keyboard...
; input from keyboard
	lda pntr		; save current...
	sta lstp		; ... cursor column
	lda tblx		; save current...
	sta lsxp		; ... line number
	jmp bn15		; blink cursor until return

bn10:	cmp #3			; is input from screen?
	bne bn20		; no...
; screen
	ora crsw
	sta crsw		; fake a carriage return
	lda #scymax
	sta lintmp		; remember bootom line no
	lda scxmax		; moved to patch to make space
	sta indx		; ...up on this line
bn15:	jsr jloop5		; pick up characters
	clc
	rts

bn20:	bcs bn30		; devices >3
	cmp #2			; usb?
	beq bn50

	jmp error5		; illegal device

; input from iec bus
bn30:	lda status		; status from last
	beq bn35		; was good
bn31:	lda #cr			; bad...all done
bn32:	clc			; valid data
bn33:	rts

!if JIFFY = 1{			; ##### JIFFY #####
bn35:	jmp jdIecByteIn
} else{
bn35:	jmp acptr		; good...handshake
}
; input from USB
bn50:	lda via2+pra		; get usb status
	lsr			; shift RXF bit#1 to carry
	bcs bn55		; branch if high = no data
	lda usb			; load data
	!byte $2c		; skip next
; no data
bn55:	lda #0			; return 0
	clc			; good return
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* bsout -- out character to channel   *
;*     determined by variable dflto:   *
;*     0 -- invalid                    *
;*     2 -- usb                        *
;*     3 -- screen                     *
;*  4-31 -- iec   bus                 *
;*                                     *
;* exit:  cy=1, error for usb.         *
;*        cy=0, otherwise.             *
;*                                     *
;*       note, other errors must be de-*
;*       tected by checking status !   *
;***************************************
nbsout:	pha			; preserve .a
	lda dflto		; check device
	cmp #3			; is it the screen?
	bne bo10		; no...
; print to crt
	pla			; restore data
	jsr jprt		; print on crt
	clc
	rts

bo10:	bcc bo20		; device 0, 1 or 2
; print to iec bus
	pla
	jmp ciout

bo20:	cmp #2			; is it usb?
	beq bo50
	pla
	jmp error5		; illegal device

bo50:	lda via2+pra		; get usb status
	lsr			; shift TXE bit#1 to carry
	lsr
	bcs bo50		; wait for TXE = low
	pla
	sta usb
	rts
; -------------------------------------------------------------------------------------------------
; ##### openchannel #####
;***************************************
;* nchkin -- open channel for input    *
;*                                     *
;* the number of the logical file to be*
;* opened for input is passed in .x.   *
;* chkin searches the logical file     *
;* to look up device and command info. *
;* errors are reported if the device   *
;* was not opened for input ,(e.g.     *
;* cassette write file), or the logical*
;* file has no reference in the tables.*
;* device 0, (keyboard), and device 3  *
;* (screen), require no table entries  *
;* and are handled separate.           *
;***************************************
nchkin:	jsr lookup		; see if file known
	beq jx310		; yup...

	jmp error3		; no...file not open

jx310:	jsr jz100		; extract file info
	lda fa
	beq jx320		; is keyboard...done.

; could be screen, keyboard, or serial
	cmp #3
	beq jx320		; is screen...done.
	bcs jx330		; is serial...address it
	cmp #2			; rs232?
	bne jx315		; no...

; rs232 channel
	lda sa
	and #02			; check for input
	beq jx316		; not input file
;	and acia+cdr		; check if running
	beq jx312		; is...done ?? (rceiver on => yes)
	eor #$FF		; flip all bits
;	and acia+cdr		; turn on...
	ora #$01		; turn on dtr ;bits(10)=01
	pha
;	jsr rst232		; reset rs232 status
	pla
;	sta acia+cdr		; set command
jx312:	lda #2			; device
	bne jx320		; bra...done

; some extra checks for tape
jx315:	jmp error5		; illegal device

jx316:	jmp error6		; not input file

jx320:	sta dfltn		; all input come from here

	clc			; good exit
	rts

; an serial device has to be a talker
jx330:	tax			; device # for dflto
	jsr talk		; tell him to talk

	lda sa			; a second?
	bpl jx340		; yes...send it
	jsr tkatn		; no...let go
	jmp jx350

jx340:	jsr tksa		; send second

jx350:	txa
	bit status		; did he listen?
	bpl jx320		; yes

	jmp error5		; device not present
; -------------------------------------------------------------------------------------------------
;***************************************
;* chkout -- open channel for output   *
;*                                     *
;* the number of the logical file to be*
;* opened for output is passed in .x.  *
;* chkout searches the logical file    *
;* to look up device and command info. *
;* errors are reported if the device   *
;* was not opened for input ,(e.g.     *
;* keyboard), or the logical file has  *
;* reference in the tables.            *
;* device 0, (keyboard), and device 3  *
;* (screen), require no table entries  *
;* and are handled separate.           *
;***************************************
nckout:	jsr lookup		; is file in table?
	beq ck5			; yes...

	jmp error3		; no...file not open

ck5:	jsr jz100		; extract table info
	lda fa			; is it keyboard?
	bne ck10		; no...something else.

ck20:	jmp error7		; yes...not output file

;could be screen,serial,or tapes
ck10:	cmp #3
	beq ck30		; is screen...done
	bcs ck40		; is serial...address it
	cmp #2			; rs232?
	bne ck15

; rs232 output
	lda sa			; check if output file
	lsr
	bcc ck20		; not so...
;	jsr rst232		; reset rs232 status
;	jsr xon232		; make sure transmit is on
	lda #2			; device#
	bne ck30		; bra...done

; special tape channel handling
ck15:	jmp error5		; illegal device

ck30:	sta dflto		; all output goes here

	clc			; good exit
	rts

ck40:	tax			; save device for dflto
	jsr listn		; tell him to listen

	lda sa			; is there a second?
	bpl ck50		; yes...

	jsr scatn		; no...release lines
	bne ck60		; branch always

ck50:	jsr secnd		; send second...

ck60:	txa
	bit status		; did he listen?
	bpl ck30		; yes...finish up

	jmp error5		; no...device not present
; -------------------------------------------------------------------------------------------------
; ##### close #####
;*************************************
;* nclose -- close logical file      *
;*                                   *
;* enter:                            *
;*     cy =1 ,transmit close to dev- *
;*            ice.                   *
;*     cy =0 ,only remove from kernal*
;*            tables.                *
;*                                   *
;*     the logical file number of the*
;* file to be closed is passed in .a.*
;* keyboard, screen, and files not   *
;* open pass straight through. tape  *
;* files open for write are closed by*
;* dumping the last buffer and       *
;* conditionally writing an end of   *
;* tape block.serial files are closed*
;* by sending a close file command if*
;* a secondary address was specified *
;* in its open command.              *
;*************************************
nclose:	php			; save cy flag
	jsr jltlk		; look file up
	beq jx110		; was open...continue
	plp
	clc			; was never open...no error
	rts

jx110:	jsr jz100		; extract table data
	plp			; retrieve cy flag
	txa			; save table index
	pha
	bcc jx150		; close out table entries only

	lda fa			; check device number
	beq jx150		; is keyboard...done
	cmp #3
	beq jx150		; is screen...done
	bcs jx120		; is iec...process
	cmp #2			; rs232?
	bne jx115		; no...

; close rs-232
cls232:	jmp jx150		; jmp...remove file

; close cassette file
jx115:	pla			; cassette now closes the channel...
	jsr jx151		; before transmitting out the final data
	jmp error5		; illegal device

; close an iec file
jx120:	jsr clsei

; entry to remove a give logical file from table of logical, primary, and secondary addresses
jx150:	pla			; get table index off stack
jx151:	tax			; entry for cassette special
	dec ldtnd
	cpx ldtnd		; is deleted file at end?
	beq jx160		; yes...done

; delete entry in middle by moving last entry to that position.
	ldy ldtnd
	lda lat,y
	sta lat,x
	lda fat,y
	sta fat,x
	lda sat,y
	sta sat,x
jx160:	clc
jx170:	rts			; close exit

; lookup tablized logical file data
lookup:	lda #0
	sta status
	txa
jltlk:	ldx ldtnd
jx600:	dex
	bmi lkups4
	cmp lat,x
	bne jx600
	clc
	rts

; routine to fetch table entries
jz100:	lda lat,x
	sta la
	lda fat,x
	sta fa
	lda sat,x
	sta sa
jz101:	rts

; sa is passed in .y
; routine looks for match in tables
; carry set if not present
; carry clear:
; .a=la,.x=fa,.y=sa
lkupsa:	tya
	ldx ldtnd
lkups2:	dex
	bmi lkups4
	cmp sat,x
	bne lkups2
	clc
lkups3:	jsr jz100		; get table data
	tay
	lda la
	ldx fa
	rts
lkups4:	sec
	rts			; not found exit

; la is passed in .a
; routine looks for match in tables
; carry set if not found
; carry clear:
; .a=la,.x=fa,.y=sa
lkupla:	tax
	jsr lookup
	bcc lkups3
	rts
; -------------------------------------------------------------------------------------------------
; ##### clall #####
;******************************************
;* nclall -- close all logical files      *
;*      deletes all table entries and     *
;* restores default i/o channels          *
;* and clears iec port devices           *
;******************************************
;------------------------------------------
; new ncall
;  closes all files untill done or an
;  error occurs.
;  entry:
;    c-clr => close all files
;    c-set => .a = fa (device to be closed)
;------------------------------------------
; Close all logical files
nclall:	ror xsav		; save carry
	sta savx		; save .a
ncl010:	ldx ldtnd		; scan index
ncl020:	dex
	bmi nclrch		; all done...clear channels (dclose patch 5/31/83)
	bit xsav		; check for fixed fa
	bpl ncl030		; none...
	lda savx
	cmp fat,x
	bne ncl020		; no match...
ncl030:	lda lat,x		; close this la
	sec			; c-set required to close
	jsr close
	bcc ncl010

ncl040:	lda #0			; original entry for nclall
	sta ldtnd		; forget all files
; -------------------------------------------------------------------------------------------------
;********************************************
;* nclrch -- clear channels                 *
;*   unlisten or untalk iec devices, but   *
;* leave others alone.  default channels    *
;* are restored.                            *
;********************************************
nclrch:	ldx #3
	cpx dflto		; is output channel iec?
	bcs jx750		; no...

	jsr unlsn		; yes...unlisten it

jx750:	cpx dfltn		; is input channel iec?
	bcs clall2		; no...

	jsr untlk		; yes...untalk it

; restore default values
clall2:	ldx #3
	stx dflto		; output chan=3=screen
	lda #0
	sta dfltn		; input chan=0=keyboard
	rts
; -------------------------------------------------------------------------------------------------
; ##### open #####
;***********************************
;*                                 *
;* open function                   *
;*                                 *
;* enter: cy=1, transmit command to*
;*              device.            *
;*        cy=0, perform open opera-*
;*              tion.              *
;*                                 *
;* la, fa, sa must be set up prior *
;* to the call to this routine, as *
;* well as the file name descript- *
;* tor.                            *
;*                                 *
;***********************************
; F6C6
nopen:	bcc     op000		; do open
	jmp     tranr		; do transmit

;***********************************
;*                                 *
;* create an entry in the logical  *
;* files tables consisting of      *
;* logical file number--la, device *
;* number--fa, and secondary cmd-- *
;* sa.                             *
;*                                 *
;* a file name descriptor, fnadr & *
;* fnlen, is passed to this routine*
;*                                 *
;***********************************
; F6CB
op000:	ldx la			; check file #

; bne op98 ;is not the keyboard
; jmp error6 ;not input file...

op98:	jsr lookup		; see if in table
	bne op100		; not found...o.k.

	jmp error2		; file open

op100:	ldx ldtnd		; logical device table end
	cpx #10			; maximum # of open files
	bcc op110		; less than 10...o.k.

	jmp error1		; too many files

op110:	inc ldtnd		; new file
	lda la
	sta lat,x		; store logical file #
	lda sa
	ora #$60		; make sa an iec command
	sta sa
	sta sat,x		; store command #
	lda fa
	sta fat,x		; store device #

; perform device specific open tasks
	beq op175		; is keyboard...done.
	cmp #3
	beq op175		; is screen...done.
	bcc op150		; are cassettes 1 & 2

	jsr openi		; is on iec...open it
	bcc op175		; branch always...done

; perform tape open stuff
op150:	cmp #2
	bne op152

; USB - nothing to do
	jmp op175

op152:	jmp error5		; illegal device

op175:	clc			; flag good open
op180:	rts			; exit in peace

openi:	lda sa
	bmi op50		; no sa...done

	ldy fnlen
	beq op50		; no file name...done

	lda fa
	jsr listn		; device la to listen

	lda sa
	ora #$f0
openib:	jsr secnd

	lda status		; anybody home? (set by nodev)
	bpl op35		; yes...continue

	pla
	pla
	jmp error5		; device not present

op35:	lda fnlen
	beq op45		; no name...done sequence

; send file name over iec
	ldy #0
op40:	lda (fnadr),y
	jsr ciout
	iny
	cpy fnlen
	bne op40

op45:	jsr unlsn

op50:	clc			; no  error
	rts
; -------------------------------------------------------------------------------------------------
;*****************************************
;*  transmit command to device           *
;*                                       *
;*   fnlen,fnadr must be set up already  *
;*   to contain the command string.      *
;*   fa must be set for the device.      *
;*****************************************
; F741
tranr:  lda fa
	jsr listn
	lda #$6F
	sta sa
	jmp openib
; -------------------------------------------------------------------------------------------------
; ##### load #####
;**************************************
;* load ram function     10/30/81     *
;*                                    *
;*  loads from iec bus devices        *
;*  >=4 to 31 as determined by        * 
;*  contents of variable fa.          *
;* entry:                             *
;*   .a(bit 7)=0 performs load        *
;*   .a(bit 7)=1 performs verify      *
;*   .a(bits 0-3)=RAM0 bank           *
;*   .x=start address low             *
;*   .y=start address high            *
;*   if .x=$ff & .y=$ff => fixed load *
;* exit:                              *
;*   .a(bits 0-3)=RAM0 bank           *
;*   .x=end address low               *
;*   .y=end address high              *
;*                                    *
;**************************************
nload:	stx relsal		; save alt address
	sty relsah
	sta verck		; set verify flag (n)
	and #%00001111		; isolate bank bits #0-3
	sta relsab		; save bank
	lda #0			; clear status
	sta status

	lda fa			; check device number
	cmp #4
	bcs ld20

	jmp error9		; bad device #

; load from cbm iec device
ld20:	lda #$60		; special load command
	sta sa

	ldy fnlen		; must have file name
	bne ld25		; yes...ok

	jmp error8		; missing file name

ld25:	jsr luking		; tell user looking
	jsr openi		; open the file

	lda fa
	jsr talk		; establish the channel
	lda sa
	jsr tksa		; tell it to load

	jsr acptr		; get first byte
	sta eal
	sta stal

	lda status		; test status for error
	lsr
	lsr
	bcc ld30		; file  found...

	jmp error4		; file not found error

ld30:
!if JIFFY = 1{			; ##### JIFFY #####
	jsr jdIecByteIn2
} else{
	jsr acptr
}
	sta eah
	sta stah

; test for fixed or moveable load
	lda relsal
	and relsah
	cmp #$ff
	beq ld35		; fixed load

	lda relsal
	sta eal
	sta stal
	lda relsah
	sta eah
	sta stah
ld35:	jsr SwitchBank		; switch to new bank
!if JIFFY = 1{			; ##### JIFFY #####
	jmp jdLOAD

ld40:	jsr stop		; STOP key?
	bne ld45		; no, -> continue

	jsr RestoreBank
	jmp break

ld45:	jsr jdIecByteIn
	
	lda status
	and #$fd
	cmp status		; Read timeout?
	sta status
	bne ld40		; yes -> repeat

	ldy #0
	ldx r2d2		; save JiffyDOS flag
	lda bsour1
	bit verck		; performing verify?
	bpl ld50		; no...load

	cmp (eal),y		; compare OK?
	beq ld55		; yes, -> continue
	jsr SetStatusBit4	; error
	!byte $2C		; skip next store

ld50:	sta (eal),y		; store in memory

ld55:	stx r2d2		; restore JiffyDOS flag
} else{
	jsr loding		; tell user loading

ld40:	lda #$fd		; mask off timeout
	and status
	sta status

	jsr stop		; stop key?
	bne ld45		; no...

	jsr RestoreBank
	jmp break		; stop key pressed

ld45:	jsr acptr		; get byte off iec
	tax
	lda status		; was there a timeout?
	lsr
	lsr
	bcs ld40		; yes...try again
	txa
	ldy #0
	bit verck		; performing verify?
	bpl ld50		; no...load
	sta sal			; use as a temp
	lda (eal),y
	cmp sal
	beq ld60		; okay
	lda #sperr		; no good...verify error
	jsr udst		; update status
	!byte $ad		; skip next store

ld50:	sta (eal),y
}
ld60:	inc eal			; increment store addr
	bne ld64
	inc eah
ld64:	bit status		; eoi?
	bvc ld40		; no...continue load

	jsr RestoreBank		; switch to old RAM0 bank
!if JIFFY = 1{			; ##### JIFFY #####
; close file and exit
jdCloseFile:
}
	jsr untlk		; close channel
	jsr clsei		; close the file
; exit iec load
	clc			; good exit
	lda relsab		; set up end load address
	ldx eal
	ldy eah
ld190:	rts

; subroutine to print to console: searching [for name]
luking:	bit msgflg		; supposed to print?
	bpl ld115
	ldy #ms5-ms1		; "searching"
	jsr spmsg
	lda fnlen
	beq ld115
	ldy #ms6-ms1		; "for"
	jsr spmsg

; subroutine to output file name
outfn:	ldy fnlen		; is there a name?
	beq ld115		; no...done
	ldy #0
ld110:	lda (fnadr),y
	jsr bsout
	iny
	cpy fnlen
	bne ld110
ld115:	rts

; subroutine to print: loading/verifing
loding:	ldy #ms10-ms1		; assume 'loading'
	lda verck		; check flag
	bpl ld410		; are doing load
	ldy #ms21-ms1		; are 'verifying'
ld410:	jmp spmsg

; rsr  fix segmentation 10/15/81
; rsr  6509 changes  10/15/81
; -------------------------------------------------------------------------------------------------
; ##### save #####
;***************************************
;* nsave              10/30/81         *
;*                                     *
;* saves to iec devices 4>=n>=31 as    *
;* selected by variable fa.            *
;*                                     *
;* .a = RAM0 bank                      *
;* .x => zpage address of start vector *
;* .y => zpage address of end vector   *
;***************************************
nsave:	sta relsab		; bank
	lda zp,x		; get start vector
	sta stal
	lda zp+1,x
	sta stah
	tya
	tax
	lda zp,x		; get end vector
	sta eal
	lda zp+1,x
	sta eah

	lda fa 			; check device number
	cmp #4
	bcs sv20

	jmp error9		; bad device #

sv20:	lda #$61		; special save command
	sta sa
	ldy fnlen
	bne sv25

	jmp error8		; missing file name

sv25:	jsr openi
	jsr saving
	lda fa
	jsr listn
	lda sa
	jsr secnd

	jsr SwitchBank		; switch to new bank

	jsr rd300
	lda sal
	jsr ciout
	lda sah
	jsr ciout

	ldy #0
sv30:	jsr cmpste		; compare start to end
	bcs sv50		; have reached end
	lda (sal),y
	jsr ciout
	jsr incsal
	jsr stop
	bne sv30

	jsr RestoreBank		; switch to old RAM0 bank
break:	jsr clsei
	lda #0
	sec
	rts

sv50:	jsr RestoreBank		; switch to old RAM0 bank
	jsr unlsn

clsei:	bit sa
	bmi clsei2
	lda fa
	jsr listn
	lda sa
	and #$ef
	ora #$e0
	jsr secnd
	jsr unlsn

clsei2:
sv110:	clc
sv115:	rts

; subroutine to output: 'saving <file name>'
saving:	lda msgflg
	bpl sv115		; no print

	ldy #ms11-ms1		; 'saving'
	jsr spmsg
	jmp outfn		; <file name>
; -------------------------------------------------------------------------------------------------
; switch to new RAM0 bank
SwitchBank:
	lda via2+prb		; get MMU reg
	and #%00001111		; isolate RAM0 bank
	sta temp		; and remember
	lda #%11110000		; clear RAM0 bank
	and via2+prb
	ora relsab		; set RAM0 bank
	sta via2+prb
	rts
; -------------------------------------------------------------------------------------------------
; Restore RAM0 Bank
RestoreBank:
	lda #%11110000		; clear RAM0 bank
	and via2+prb
	ora temp
	sta via2+prb		; restore old RAM bank
	rts
; -------------------------------------------------------------------------------------------------
; ##### errorhandler #####
;************************************
;* error handler                    *
;*  restores i/o channels to default*
;*  prints kernal error message if  *
;*  bit 6 of msgflg set.  returns   *
;*  with error # in .a and carry.   *
;************************************
error1:	lda #1			; too many files
	!byte $2c
error2:	lda #2			; file open
	!byte $2c
error3:	lda #3			; file not open
	!byte $2c
error4:	lda #4			; file not found
	!byte $2c
error5:	lda #5			; device not present
	!byte $2c
error6:	lda #6			; not input file
	!byte $2c
error7:	lda #7			; not output file
	!byte $2c
error8:	lda #8			; missing file name
	!byte $2c
error9:	lda #9			; bad device #

errorx:	pha			; error number on stack
	jsr clrch		; restore i/o channels

	ldy #ms1-ms1
	bit msgflg		; are we printing error?
	bvc erexit		; no...

	jsr msg			; print "cbm i/o error #"
	pla
	pha
	ora #$30		; make error # ascii
	jsr bsout		; print it

erexit:	pla
	sec
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* stop -- check stop key flag and     *
;* return z flag set if flag true.     *
;* also closes active channels and     *
;* flushes keyboard queue.             *
;* also returns key downs from last    *
;* keyboard row in .a.                 *
;***************************************
; Check the stop key
nstop:	lda stkey		; value of last row
	cmp #$7f		; check stop key position
	bne stop2		; not down
	php
	jsr clrch		; clear channels
	sta ndx			; flush queue (.a already 0 from clrch)
	plp
stop2:	rts
; -------------------------------------------------------------------------------------------------
;***********************************
;*     udtim-- update time.        *
;*     called every 64th second.   *
;*     update the stop key location*
;*     bit0 of stkey=0 for key down*
;***********************************
; interrupts are coming from the 6522 timer
udtim:	ldx #0			; pre-load for later
; here we proceed with an increment of the time register.
	inc tick		; inc 1/64th
	lda tick
	cmp #HZ			; one second full?
	bne ud60		; no
	stx tick
	sed			; set decimal mode
	lda time+2
	clc
	adc #1
	sta time+2
	cmp #$60		; next minute?
	bne ud50		; no
	stx time+2
	lda time+1
	clc
	adc #1
	sta time+1
	cmp #$60		; next hour?
	bne ud50		; no
	stx time+1
	lda time
	clc
	adc #1
	sta time
	and #$7f		; clear pm flag
	cmp #$12		; 12 hours full?
	bne ud50		; no
	lda time		; get hours again		
	and #$80		; clear hours except pm flag
	eor #$80		; toggle pm flag
	sta time
	stx time+1
	stx time+2
	stx tick
ud50:	cld
; set stop key flag here
ud60:	lda via1+prb		; wait for it to settle
	cmp via1+prb
	bne ud60		; still bouncing
	tax			; set flags...
	bmi ud80		; no stop key...exit  stop key=$7f
	ldx #$ff-$42		; check for a shift key (c64 keyboard)
	stx via1+pra
ud70:	ldx via1+prb		; wait to settle...
	cpx via1+prb
	bne ud70
	sta via1+pra		; !!!!!watch out...stop key .a=$7f...same as colms was...
	inx			; any key down aborts
	bne ud90		; leave same as before...
ud80:	sta stkey		; save for other routines
ud90:	rts
; read time
;  .y = (bit7=pm,bit6/5=t8/t4,bits4-0 hrs)
;  .x = (bit7=t2,bits6-0 minutes)
;  .a = (bit7=t1,bits6-0 seconds)
; last digit is 1/8 seconds because of 1/64 tick
rdtim:	sei			; keep time from rolling
	lda tick		; get 64th
	and #%00100000		; isolate t4 of 64th
	ora time
	tay			; hours
	lda tick		; get 64th
	asl			; t2 of 64th to bit#7
	asl
	asl
	pha			; remember
	and #$80		; clear except t2
	ora time+1
	tax			; minutes
	pla			; restore
	asl			; t1 of 64th to bit#7
	and #$80		; clear except t1
	ora time+2		; seconds
	cli
	rts
; set time
settim:	sei			; keep time from changing
	pha			; remember seconds
	and #$7f		; clear 8th bit#7
	sta time+2		; store seconds
	pla
	and #$80		; isolate t1
	sta tick		; store	64th
	txa
	and #$7f		; clear 8th bit#7
	sta time+1		; store minutes
	txa
	asl			; t2 to carry
	lda tick
	ror			; shift t2 to 64th 
	lsr			; 1/8 to correct position bit#3+4
	lsr
	lsr
	sta tick
	tya
	and #%10011111		; clear 8th bit#5+6
	sta time		; store hours incl. pm flag
	tya
	and #%00100000		; isolate t4
	ora tick
	sta tick
	cli
	rts
; rsr 8/21/80 remove crfac change stop
; rsr 3/29/82 add shit key check for commodore 64
; -------------------------------------------------------------------------------------------------
; ##### init #####
;------------------------------------------------
; start - system reset routine
;  kernal checks only at $8000 for ROM
;    if no occurance then $e000 is used for vector
;    $e000 => monitor start
;  kernal expects:
;    $x000 - jmp init  (cold start)
;    $x003 - jmp winit (warm start)
;    $x006 - 'c'(+$80)=> kernal cold start first
;    $x007 - 'b'+$80
;    $x008 - 'm'+$80
;    $x009 - 'x'  x=4k bank (1-8)
;------------------------------------------------
; test bytes for ROMs
patall: !byte $c2,$cd		; $x004 rom pattern
; reset entry
start:	ldx #$fe		; init stack
	sei
	txs
	cld
; check for warm start
	lda #warm
	cmp evect+2		; check warm flag ?
	bne scold		; no -> cold start
	lda evect+3
	cmp #winit		; check winit ?
	beq swarm		; yes -> warm start
; cold start
scold:	lda #6			; set up indirect to $0006 = position ROM ident bytes
	sta eal
	lda #$70		; start at $8000 ($10 added below)
	sta eah
	lda #0
	sta evect		; set low byte of vector warm start to $00
	ldx #$37		; existance flag 4. rom ident byte compare value to '0'
sloop0: ldy #3			; set counter to 4th ROM ident byte
	lda eah
	cmp #$c0		; last allowed rom position reached?
	beq sloop2		; no roms but this one... -> monitor cold boot
	clc			; calc new test point
	adc #$10                ; 4k steps
	sta eah
	inx			; next 4. byte compare value $31, $32, $33...
	txa
	cmp (eal),y		; compare if 4. byte $31 at address $1006+3, $32 at $2006...
	bne sloop0		; 4. byte does not mach - > next ROM pos. $2000, $3000...
	dey			; check next byte backwards if 4th byte matches
sloop1: lda (eal),y		; load 3., 2., 1. byte
	dey
	bmi sloop3		; all done...correctly - 2.+3. byte matches -> autostart ROM found!
	cmp patall,y		; compare test bytes 'M', 'B'
	beq sloop1		; 3. byte OK -> check 2. byte
	bne sloop0		; no good... 2. or 3. ident byte does not mach

; monitor (could be test for keydown ***)
sloop2: ldy #$e0                ; monitor vector
	!byte $2c		; skip two bytes
sloop3: ldy eah
	sty evect+1             ; set high byte of vector

	tax                     ; move 1. ident byte to x to set N-flag
	bpl swarm               ; don't use kernal initilization
				;   jump to warm start if value is positive ('c'=$43)

; kernal cold start
	jsr ioinit              ; initilize i/o, MMU -> (VIA1, VIA2)
	jsr ramtas              ; ram-test and set
	jsr restor              ; operationg system vectors (copies $0300 Vector Table)
	jsr jcint               ; screen editor init -> cint (editor, VDP)
	lda #warm		; Kernal initilize done flag
	sta evect+2             ; save first warm start flag $A5
; warm start entry
swarm:  jmp (evect)             ; start exit -> basic warm start
; -------------------------------------------------------------------------------------------------
; ioinit - initilize i/o system, MMU
;   must be entered with irq's disabled
;------------------------------------------
; 6522 VIA1, VIA2 initilization code
ioinit: lda #$7f		; kill interrupts
	sta via1+ier
	sta via2+ier
; timers, aux lines
	lda  #%01000000         ; via1+2: free running t1, t2 oneshot
	sta  via1+acr		;   via1 t1 = system IRQ (freerun), t2 = IEC (oneshot)
	sta  via2+acr
	lda  #%01110110         ; via1: ca2+cb2 independent input mode,ca1 neg,cb1 pos
	sta  via1+pcr
	lda  #%11111110         ; via2: ca2 high, cb2 high, ca1 neg,cb1 pos
	sta  via2+pcr		;   MMU: ca2+cb2 = ROM enabled, ca1 = restore key
; configure ports	
	ldx #$ff
	stx via1+pra		; keyboard out 0-7=1
	stx via1+ddra		; keyboard cols are outputs
	inx			; $00
	stx via1+ddrb		; keyboard rows are inputs
	stx via2+pra		; set IEC
	lda #%00111000		; set ddr IEC in/out, Busy in, USB RXF+TXF in
	sta via2+ddra
!if V11PCB128K = 1{
	ldx #%10000000		; PCB v.1.1 with 128KB RAM1 chip
}
	stx via2+prb		; set MMU RAM0+1 = bank 0
!if V10PCB = 1{
	ldx #%00001111		; PCB v.1.0: only 2x2 bit RAM banks (max. 128kB chips)
} else{
	ldx #%11111111		; >= PCB v.1.1: .x=$ff
}
	stx via2+ddrb		; set ddr MMU RAM bank = output
; restore key NMI
	lda  #$82		; enable panic button
	sta  via2+ier
; start timer
	lda  #$c0		; enable t1 interrupts
	sta  via1+ier
	lda  #<sixty		; ...at 64 hz rate
	sta  via1+t1lo
	lda  #>sixty
	sta  via1+t1hi
	rts

sixty    =  1000000*CLOCK/HZ	; sixty hertz value (64 Hz, because 16bit is max for 4 MHz clock)
; -------------------------------------------------------------------------------------------------
; ramtas - initilize lower ram with $00 and test all system ram
;   set ram limits
;   alocatate initial buffer space
;-----------------------------------------
; RAM-test / vector init
ramtas: lda #0			; init value a = $00, counter x = 0
	tax
px1:    sta zp,x		; clear ZP
	sta buf,x		; clear basic input buffer from $0200       
	sta evect-$100,x	; clear kernal RAM till evct $03f8
	inx
	bne px1			; clear next byte

; memory size check
	tay			; move 0 to y
	lda #$04		; memory start page
	sta memstr+1		; set bottom of user memory
	sta lowadr+1		; ...and system memory
	sta sah			; set high inital index
; memsiz,sal,lowadr are zeroed above
siz100: lda (sal),y
	tax			; save memory value in x 
	lda #$55		; test with $55
	sta (sal),y
	lda (sal),y
	cmp #$55		; check if $55 
	bne size		; end test if different
	asl			; test with $aa
	sta (sal),y
	cmp (sal),y		; check if $aa
	bne size		; end test if different
	txa
	sta (sal),y		; restore old memory value from x
	iny
	bne siz100		; test next byte
	inc sah
	bne siz100		; test next page
; set top of memory
;   sah,y = first bad RAM byte
size:   tya			; low to x
	tax
	ldy sah			; high in y
	dex			; 1 byte down
	cpx #$ff		; crossed page?
	bne siz110		; no...skip
	dey			; dec hi
siz110:	stx hiadr
	sty hiadr+1
; reserve 2 pages (512by) for sprite colors	
	dey
	dey
; allocate 1 page (256funcs)
	dey
	clc
	jsr memtop              ; set user top of memory
	rts
; -------------------------------------------------------------------------------------------------
; standard vector table - initialized at boot from restor sub to cinv $0300
jmptab:
	!word yirq		; cinv	
	!word timb		; cbinv....brk goes to monitor
	!word panic		; no.....nminv !!!!!
	!word nopen		; open file
	!word nclose		; close file
	!word nchkin		; open channel in
	!word nckout		; open channel out
	!word nclrch		; close channel
	!word nbasin		; input from channel
	!word nbsout		; output to channel
	!word nstop		; scan stop key
	!word ngetin		; scan keyboard
	!word nclall		; close all files
	!word nload		; load from file
	!word nsave		; save to file
	!word s0		; monitor command parser
	!word jescrt		; esc key vector
	!word jescrt		; user ctrl key vector
	!word nsecnd		; IEC listen secondary address
	!word ntksa		; IEC talk secondary address
	!word nacptr		; IEC character in
	!word nciout		; IEC character out
	!word nuntlk		; IEC untalk bus
	!word nunlsn		; IEC unlisten bus
	!word nlistn		; IEC listen a device
	!word ntalk		; IEC talk to a device
tabend:
; -------------------------------------------------------------------------------------------------
; FB3D NMI entry, jumps indirect to NMI routine
nmi:    jmp (nminv)             ; ($0304) default -> panic
; -------------------------------------------------------------------------------------------------
; Set file name address
; .a = filename length
; .x = zero page location of 3 byte address
setnam: sta fnlen		; store length
	lda zp,x		; load and store address
	sta fnadr
	lda zp+1,x
	sta fnadr+1
	lda zp+2,x
	sta fnadr+2
	rts
; -------------------------------------------------------------------------------------------------
; Set file paramaters
; .a = logical address
; .x = first address
; .y = secundary address
setlfs: sta la
	stx fa
	sty sa
	rts
; -------------------------------------------------------------------------------------------------
; Read/write status
; carry set -- read device status into .a
readst: bcc storst
	bcs readss
; Set the system message flag
setmsg: sta msgflg
readss: lda status		; read status
; set status bit
udst:   ora status		; set bit and store status
	sta status
	rts
; carry clear -- set device status with .a
storst: sta status		; store status
	rts
; -------------------------------------------------------------------------------------------------
; IEC timeout on/off
settmo: sta timout
	rts
; -------------------------------------------------------------------------------------------------
; Read/set top of memory
memtop: bcc settop

; carry set--read top of memory
	ldx memsiz		; load user memory top in .x.y
	ldy memsiz+1

; carry clear--set top of memory
settop: stx memsiz		; set user memory top
	sty memsiz+1
	rts
; -------------------------------------------------------------------------------------------------
; Manage bottom of memory
membot: bcc setbot

; carry set--read bottom of memory
	ldx memstr		; load bottom mem in .x.y
	ldy memstr+1

; carry clear--set bottom of memory
setbot: stx memstr		; set bottom mem
	sty memstr+1
	rts
; -------------------------------------------------------------------------------------------------
; Restore ram i/o vectors at $0300
restor: ldx #<jmptab		; load vector table address in kernal
	ldy #>jmptab
	clc

; Manage ram i/o vectors
vector: stx sal			; store address
	sty sah
	bcc vect50		; carry=0 -> set/restore table

; carry set--read vectors
	ldy #tabend-jmptab-1
vect20: lda cinv,y		; from ram table $F0300
	sta (sal),y		; into user area
	dey
	bpl vect20

; carry clear--set vectors
vect50: ldy #tabend-jmptab-1
vect60: lda (sal),y		; from user area
	sta cinv,y		; into ram table $0300
	dey
	bpl vect60

	rts
; -------------------------------------------------------------------------------------------------
; vreset - reset vector flags and control
;   .x - low vector address  .y - high vector address
vreset: stx evect
	sty evect+1
	lda #winit
	sta evect+3
	rts
; -------------------------------------------------------------------------------------------------
; ##### irq #####
;**********************************************
;* nirq - handler for:       10/30/81 rsr     *
;* 6522 irq's                                 *
;*   (timera, timerb)                         *
;* keyboard scan (50/60hz irq)                *
;**********************************************
; IRQ handler
nirq:	pha			; save registers
	txa
	pha
	tya
	pha
	tsx			; check for brk...
	lda stack+4,x		; get old p status
	and #$10		; break flag?
	bne brkirq		; yes...
	jmp (cinv)		; via vector -> yirq
brkirq: jmp (cbinv)		; yes...

; entry via indirect vector cinv
yirq:   jsr scnkey
	jsr udtim		; set stopkey flag
	bit via1+t1lo		; clear interupt flag

prend:	pla			; restore registers
	tay
	pla
	tax
	pla
	rti			; exit from irq routines

; Default NMI routine
panic:	sei			; no irq's allowed...
	pha			; save 6502 regs
	txa
	pha
	tya
	pha
!if NMI555 = 0{
	lda via2+ifr		; check if real nmi...
	bpl nmirti		; ...no

	and via2+ier		; show only enables
	tax			; save in .x for later
	and #$02		; check if restore key...
	beq nnmi20		; ...no
	bit  via2+prah		; clr ca1 flag
}
; check for stop key down
	jsr  ud60
	jsr  stop
	bne  nnmi20		; no stop key
; stop/restore detected
	jsr  restor		; restore system indirects
	jsr  ioinit		; restore i/o for basic
	jmp  (evect)		; ...no, so basic warm start
; not restore NMI
nnmi20:	lda #$7f		; kill all NMI's
	sta via2+ifr
nmirti:	cli
	bne prend		; always
; -------------------------------------------------------------------------------------------------
; some needed routines
; sta -> sa
rd300:	lda stah
	sta sah
	lda stal
	sta sal
	rts
; sa - ea
cmpste: sec
	lda sal
	sbc eal
	lda sah
	sbc eah
	rts
; increse sal
incsal: inc sal
	bne incr20
	inc sah
ffrts:
incr20:	rts
; -------------------------------------------------------------------------------------------------
; ##### vectors #####
; Jump table kernal functions
!zone vectors
*= $ff6f
; also used in basic! Equal to table at end of Basic 4+
	jmp vreset		; Power-on/off vector reset
ipcgov:	jmp vmode		; VDP: set mode, backdrop color
	jmp jfunky		; Function key vector
	jmp vclear		; VDP: clear screen with background color
	jmp ioinit		; I/O initialization
	jmp jcint		; Screen initialization
	jmp alocat		; Allocation routine
	jmp waitcmd		; VDP: Wait for VDP command
	jmp restor		; restore I/O vectors
	jmp lkupsa		; Match sa--return sa,fa
	jmp lkupla		; Match la--return sa,fa
	jmp setmsg		; Control o.s. messages
secnd:	jmp (isecnd)		; Send sa after listen
tksa:	jmp (itksa)		; Send sa after talk
	jmp memtop		; set/read top of memory
	jmp membot		; set/read bottom of memory
	jmp jkey		; Scan keyboard
	jmp settmo		; set timeout in IEC
acptr:	jmp (iacptr)		; Handshake IEC byte in
ciout:	jmp (iciout)		; Handshake IEC byte out
untlk:	jmp (iuntlk)		; Send untalk out IEC
unlsn:	jmp (iunlsn)		; Send unlisten out IEC
listn:	jmp (ilistn)		; Send listen out IEC
talk:	jmp (italk)		; Send talk out IEC
	jmp readst		; read/write I/O status byte
	jmp setlfs		; set la, fa, sa
	jmp setnam		; set length and fn adr
open:	jmp (iopen)		; Open logical file/transmit command
close:	jmp (iclose)		; Close logical file
chkin:	jmp (ichkin)		; Open channel in
ckout:	jmp (ickout)		; Open channel out
clrch:	jmp (iclrch)		; Close I/O channel
basin:	jmp (ibasin)		; Input from channel
bsout:	jmp (ibsout)		; Output to channel
load:	jmp (iload)		; Load from file
save:	jmp (isave)		; Save to file
	jmp settim		; Set internal clock
	jmp rdtim		; read internal clock
stop: 	jmp (istop)		; scan stop key
getin:	jmp (igetin)		; Get char from q
clall:	jmp (iclall)		; Close all files
	jmp udtim		; increment clock
	jmp jscror		; Screen org
	jmp jplot		; read/set x,y coord
	jmp bdoscmd		; BASIC DOS command

*= $fffa
; -------------------------------------------------------------------------------------------------
; Hardware vectors
!zone hardvectors
hwnmi:  !word nmi		; Program defineable
	!word start		; Initialization code
	!word nirq		; Interrupt handler
