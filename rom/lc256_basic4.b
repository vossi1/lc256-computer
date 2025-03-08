; Commodore CBM2 Basic 4 plus
; ported to LC256 by Vossi 09/2024, last update 10/2024
!cpu 65c02
!ct pet		; Standard text/char conversion table -> pet = petscii
!to "basic.bin", plain
;
; * constants
FILL	= $ff	; Fills free memory areas
; shared VDP sources
!source "lc256_vdpeq.b" 
!source "lc256_vdpmacro.b" 
; ########################################### INFO ################################################
; modded by vossi 11/24 for LC256, IEC/Jiffy, RAM0 bank switching 0-15
; TI$ with 1/8 seconds as last digit
; Workaround for THEN + ELSE:
;   Write ':' after each THEN and ELSE !!!
; USB receive only with GET# - INPUT# does not work!
; **************************************** DISCLAIMER *********************************************
; *****************************************
; *                                       *
; *       commodore pet basic             *
; *                                       *
; *           version 4.0                 *
; *     copyright 1979,  1980,  by        *
; *    commodore international limited    *
; *                                       *
; *****************************************
;
; version 1 - august 1977
; original microsoft release
;
; version 2 - august 1978
; fixed many bugs some are
; 1) arrays limited to 255
; 2) non-interruptable code
; 3) garbage collect strings
;
; version 3 - may 1979
; faster garbage collect
;
; version 4 - july 1979
; 1) expand reserved words
; 2) add disk commands
; 3) add disk status vars.
;
; version 4x - may, 1980
; 1) separate out kernal
; 2) set-up kernal interface
; 3) remove all kernal direct cell usage
;
; version 4.7 - october, 1981
; 1) bank related commands
; 2) copy literal strings to string space
; 3) indirect input buffer
; 4) copy fbuffr to strings
; 5) chrgot by indirect
; 6) print messages direct
;
;
; version 4.73 - november, 1981
; *** 64k version ***
; 1) floating point move & bank mapping routines
; 2) temp string descriptor pointers moved to
;    top of "user" memory.
; 3) ds$ string area allocated permanently at
;    top of "user" memory.
; 4) three byte pointers (third byte is bank#)
;    for:
;     string descriptor pointers(back links too)
;     string pointers
;     variable pointers
;    string descriptor pointers.
; 5) user memory copy of "zero" occupies 4 null
;    bytes preceding user text area.
;
;
; version 4.74 - december, 1981
; 1) user-defined functions: formal param.
;    ptr uses 3 bytes
; 2) ti$,ti with new kernal interface
;
; version 4.75 - january 7,1982
; 1) extensions for 64k,128k,192k,256k
; ****************************************** EQUATES **********************************************
	false	=0
	true	=255
; assembly constants and parameters
	cr	=$d			; carriage return
	lf	=$a			; line feed
	clrscr	=147			; clear pet screen
	home	=197			; home pet screen
	eom	=$80			; indicate end of message

	forsiz	=19
	numlev	=26			; maximum number of calls to getstk
	bufsiz	=161
	addprc	=1
	addpr2	=addprc+addprc
	addpr4	=addpr2+addpr2
	addpr8	=addpr4+addpr4
	clmwid	=10
	pi	=$ff
	strsiz	=4			; # of locs per str descriptor
	ptrsiz	=3			; # of locs per pointer
	numtmp	=3			; # of string temporaries
; dos interface equates.
	dosfnl	=16+2			; filename length
	dosdsk	=8			; disk device #
	doslfn	=14			; dos internal logical file number
	dosctl	=21			; dos directory lines before prompt
	doslst	=dosfnl+dosfnl+dosfnl+16; (dosfnl+2)*3+10 --transmit buffer length
; ******************************************** IO *************************************************
!addr	via2	= $de40		; via2 base
	prb	= $0		; Port reg b
!addr	logodata= $c000		; logo data
; ****************************************** BDEFINE **********************************************
; 'page zero storage definitions'
*=2
usrpok	*=*+3			; set up orgin by init
tmhour	*=*+1			; for ti$ calculations
tmmin	*=*+1
tmsec	*=*+1
tmten	*=*+1

form	*=*+ptrsiz		; format pointer

integr				; one-byte integer from qint
charac	*=*+1			; a delimiting char
endchr	*=*+1			; other delimiting char
count	*=*+1			; general counter
xcnt	*=*+1			; dos loop counter

; flags
; dimflg, valtyp and intflg must be
; consecutive locations.
dimflg	*=*+1			; in getting a pointer to a variable
; it is important to remember whether
; it is being done for dim or not.
valtyp	*=*+1			; the type indicator 0=numeric, 1=string
intflg	*=*+1			; tells if integer

garbfl				; whether to do garbage collection
dores	*=*+1			; whether can or can't crunch res'd word.
; turned on when data being scanned by
; crunch so unquoted strs won't be crunched.

subflg	*=*+1			; flag whether sub'd variable allowed.
; for and user-defined function pointer
; fetching turn this on before calling
; ptrget so arrays won't be detected.
; stkini and ptrget clear it.
; also disallows integers there.

inpflg	*=*+1			; flags whether we are doing input or read.

dsdesc	*=*+strsiz		; disk status string
channl	*=*+1			; holds channel number
poker
linnum	*=*+2			; location to store line # + ; 16bit return value of getpin

; pointers to temporary string descriptors.
temppt	*=*+1			; temppst relative offset to 1st free temp descr
lastpt	*=*+2			; pointer to last-used str temporary
tempst	*=*+2			; pointer to storage for 3 temporary descriptors.

index
index1	*=*+ptrsiz		; direct cells for 1st indexing usage
index2	*=*+ptrsiz		; direct cells for 2nd indexing usage

;
resho	*=*+1			; result of multiplier and divider
resmoh	*=*+1
addend				; temp used by umult
resmo	*=*+1
reslo	*=*+1
	*=*+1			; overflow previous cells

; pointers into dynamic data structures
; all are 2-byte offsets into fixed banks
; the following always mark the beginning of an area:
;    txttab, vartab
;    arytab, memtop
; these will have unchangeable values in versions
; where the areas they mark are equal to the "bottom"
; (or "top" for memtop) of a bank.
; additional variables:
;     txtend, varend, aryend
; are used to mark the end of an area, when the start
; of the "next" area is in a different bank (i.e.,
; the end isn't bordered by another area.)

; highst is used to store the offset value from a basic
; startup call to get the top of memory.

; the limit of growth in an area must also be kept.
; in the different versions, the following are used:

txttab	*=*+2			; pointer to beginning of text and
; doesn't change after being setup
; by init

txtend	*=*+2			; pointer to end of text (except 64k)

vartab	*=*+2			; pointer to start of simple variable space.

varend	*=*+2			; pointer to end of simple vars (256k only)

arytab	*=*+2			; pointer to start of array table

strend	*=*+2			; end of storage in use.

fretop	*=*+2			; top of str free space
frespc	*=*+2			; pointer to new str
memtop	*=*+ptrsiz		; highest location in memory

; line numbers and textual pointers
curlin	*=*+2			; current line number
oldlin	*=*+2			; old line number (setup by stop or
; end in a program)
oldtxt	*=*+ptrsiz		; old text pointer

datlin	*=*+2			; data line number
datptr	*=*+2			; pointer to data. initialized to point
; at the zero infront of (txttab) by
; clr command.
; updated by execution of a read.
inpptr	*=*+2			; remembers where input is coming from.

; stuff used in evaluations

varnam	*=*+2			; variable's name

fdecpt				; pointer into power of tens table.
varpnt	*=*+ptrsiz		; pointer to variable in memory

forpnt				; a variable's pointer for for loops
; and let statements (3 bytes).
lstpnt	*=*+ptrsiz		; pointer to list string (3 bytes).

vartxt				; save current txtptr on read.
opptr	*=*+ptrsiz		; pointer to current op's entry in optab.

opmask	*=*+1			; mask created by current operation.

; temporary floating result registers (5bytes each):
; tempf1,tempf2,tempf3

tempf3				; temp float reg
grbpnt				; pointer used in garbage collection.
defpnt	*=*+ptrsiz		; pointer used in function definition.

dscpnt	*=*+ptrsiz		; pointer to a string descriptor.

jmper	*=*+2			; three bytes long
oldov	*=*+1			; the old overflow.

tempf1				; temp float reg
ptarg1	=tempf1 			; multiply def'd for use by instr$
ptarg2	=tempf1+3
str1	=tempf1+6
str2	=tempf1+10
tmppos	=tempf1+14
positn	=tempf1+15
match	=tempf1+16
arypnt				; pointer used in array building.
highds	*=*+ptrsiz		; destination of highest element in blt.
hightr	*=*+ptrsiz		; source of highest element to move.

tempf2				; temp float reg (5bytes)
lowds	*=*+1			; location of last byte transfered (3 bytes).
deccnt	*=*+1			; number of places before decimal point.
tenexp	*=*+1			; base ten exponent

grbtop				; pointer used in garbage collection.(3 bytes)
lowtr	*=*+1			; last thing to move in blt (3 bytes).
dptflg	*=*+1			; has a dpt been input
expsgn	*=*+1			; sign of exponent

; the floating accumulator
dsctmp	*=*+1			; temporary descriptors are built here.
      				; dsctmp overlaps up to facmoh.
fac	;$6f			; fac#1
facexp	*=*+1			; exponent
facho	*=*+1			; most significant byte of mantissa.
facmoh	*=*+1
inth				; high byte of word from qint (used by VDP routines)
indice				; used by qint.
facmo	*=*+1
intl				; low byte of word from qint (used by VDP routines)
faclo	*=*+1
facsgn	*=*+1			; signum
degree				; count used by polynomials.
sgnflg	*=*+1
bits	*=*+1			; cell for shiftr to use.

; the floating argument (unpacked)
t1	=*			; temporaries --uses fp buffer
t2	=t1+1
t3	=t1+2
t4	=t1+3

argexp	*=*+1
argho	*=*+1
argmoh	*=*+1
argmo	*=*+1
arglo	*=*+1
argsgn	*=*+1
strng1
arisgn	*=*+1			; a sign reflecting the result
facov	*=*+1			; overflow byte of the fac
	*=*+1

strng2				; -> to str or desc
polypt				; -> to polynomial coefficients
curtol				; absolute linear index is formed here
fbufpt	*=*+ptrsiz		; -> into fbuffr used by fout
txtptr	*=*+ptrsiz ;$83		; pointer to current term
buffpt	*=*+ptrsiz		; ^input buffer

noze				; using's leading zero counter
parsts	*=*+1			; dos std parser word
point				; using's pointer to decimal point
parstx	*=*+1			; dos aux parser word

seedpt	*=*+2
errnum	*=*+1

; string area available for copy.  this area is used
; by fout as a buffer and must have dosspc contiguous
; bytes.
; 
; in addition this area is used to store temporaries
; used by the dos interface routines. note, declaration
; order of locations dosofl-dossa must be preserved.
; ****************************************** ABSOLUTE *********************************************
!addr	zp	= $00		; zeropage start
!addr	stack	= $0100		; Stack
!addr	memend	= $ffff
!addr	vecorg	= $ff6f		; kernal jump vector table
; Basic's ROM page work area
* =$200
fbuffr
vspbuf				; buffer used to interface with vsp
	*=*+16			; reserve 16 bytes for filename 1
dosf1l	*=*+1			; dos file name 1 length
dosds1	*=*+1			; dos disk drive 1
dosf1a	*=*+2			; dos file name 1 address
dosf1b	*=*+1			; dos file name 1 bank

dosf2l	*=*+1			; dos file name 2 length
dosds2	*=*+1			; dos disk drive 2
dosf2a	*=*+2			; dos file name 2 address
dosf2b	*=*+1			; dos file name 2 bank

dosbnk	*=*+1			; dos bank number
dosofl	*=*+2			; dos low offset  (bsave,bload)
dosofh	*=*+2			; dos high offset (bsave)

dosla	*=*+1			; dos logical address
dosfa	*=*+1			; dos physical address
dossa	*=*+1			; dos secondary address
dosrcl	*=*+1			; dos record length

dosdid	*=*+2			; dos disk identifier (2 chars)
didchk	*=*+1			; dos did flag

dosstr	*=*+1			; dos output string buffer
dosspc=*-fbuffr			; spaced used by dos routines

* =*+46
trmpos				; cursor column on crt
andmsk	*=*+1			; mask used by wait
eormsk	*=*+1			; mask used by wait

dfbank	*=*+1			; default bank number
dolu  	*=*+1			; default output lu (0=> not std output) keeps ds + dir ok

domask
tansgn	*=*+1			; used in determining sign of tan

ldaabs	*=*+1			; lda abs routine (see initat)
tttemp				; temporary store
ldaadr	*=*+2			; modifiable address
	*=*+1			; return opcode

;declarations for print using

hulp	*=*+1			; counter
bnr	*=*+1			; pointer to begin no
enr	*=*+1			; pointer to end no
dolr	*=*+1			; dollar flag
flag	*=*+1			; comma flag
swe	*=*+1			; counter
usgn	*=*+1			; sign exponent
uexp	*=*+1			; pointer to exponent
vn	*=*+1			; # digits before decimal point
chsn	*=*+1			; justify flag
vf	*=*+1			; # pos before dec point (field)
nf	*=*+1			; # pos after dec point (field)
posp	*=*+1			; +/- flag (field)
fesp	*=*+1			; exponent flag (field)
etof	*=*+1			; switch
cform	*=*+1			; char counter (field)
sno	*=*+1			; sign no
blfd	*=*+1			; blank/star flag
begfd	*=*+1			; pointer to begin of field
lfor	*=*+1			; length of for]at
endfd	*=*+1			; pointer to end of field
puchrs
pufill	*=*+1			; print using fill symbol
pucoma	*=*+1			; print using comma symbol
pudot	*=*+1			; print using decimal point symbol
pumony	*=*+1			; print using monetary symbol
; -------------------------------------------------------------------------------------------------
; Basic RAM vectors
*=$280
;    basic indirects

ierror	*=*+2			; error routine, output err in .x
imain	*=*+2			; main - interpreter main loop
icrnch	*=*+2			; cruncher - tokenization routine
iqplop	*=*+2			; qplop - token output expander routine
igone	*=*+2			; dispatcher
ieval	*=*+2			; eval routine
ifrmev	*=*+2			; frmevl routine
ichrgo	*=*+2			; chrgot routine
ichrge	*=*+2			; chrget routine
adray1	*=*+2			; convert float -> integer
adray2	*=*+2			; convert integer -> float

; error trapping declarations

trapno	*=*+2			; error trap vector
errlin	*=*+2			; holds line # of last error
errtxt	*=*+2			; text pointer at time of error
oldstk	*=*+1			; stack pointer before execution of last instruction
tmptrp	*=*+1			; used to save hi byte of trap line >trap & <resume
dsptmp	*=*+1			; temporary for dispose
oldtok	*=*+1			;     "      "     "
tmpdes	*=*+6			; temporary for instr$

highst	*=*+2			; max offset for any user bank

msiism	*=*+1			; used to save length of string to be added in garb collect
; ******************************************* TOKENS **********************************************
!initmem FILL			; All unused memory filled with $ff
; 'tables, reserved words, and error texts'
*=$8000
; entry name and entry point jump
bentry:	jmp init 		; entry point for start
	jmp warm 		; warm start re-entry point
	!byte $c3,$c2,$cd,'8'	; cmb8 ($80 or'd in 1st 3 chars)
; -------------------------------------------------------------------------------------------------
; statment dispatch table
stmdsp:	!word end-1
	!word for-1
	!word next-1
	!word data-1
	!word inputn-1
	!word input-1
	!word dim-1
	!word read-1
	!word let-1
	!word goto-1
	!word run-1
	!word if-1
	!word restor-1
	!word gosub-1
	!word return-1
	!word rem-1
	!word stop-1
	!word ongoto-1
	!word fnwait-1
	!word cload-1
	!word csave-1
	!word cverf-1
	!word defn-1
	!word poke-1
	!word printn-1
	!word print-1
	!word cont-1
	!word list-1
	!word clear-1
	!word cmd-1
	!word csys-1
	!word copen-1
	!word cclos-1
	!word get-1
	!word scrath-1
	!word go-1

; disk commands, added 79-june 28
	!word concat-1
	!word dopen-1
	!word dclose-1
	!word record-1
	!word format-1
	!word colect-1
	!word backup-1
	!word dcopy-1
	!word append-1
	!word dsave-1
	!word dload-1
	!word dunit-1
	!word rename-1
	!word scratc-1
	!word dcat-1			; directory == catalog

; disk clear command added for toi
	!word dclear-1

; multi-bank related commands
	!word chbank-1
	!word bload-1
	!word bsave-1

; function key command
	!word fkey-1
	!word delete-1
	!word rem-1			; else stmt
	!word trap-1
	!word resume-1
	!word dispos-1
	!word puctrl-1

fundsp:	!word sgn
	!word int
	!word abs

usrloc:	!word usrpok
	!word fre
	!word pos
	!word sqr
	!word rnd
	!word log
	!word exp
	!word cos
	!word sin
	!word tan
	!word atn
	!word peek
	!word len
	!word strd
	!word val
	!word asc
	!word chrd
	!word leftd
	!word rightd
	!word midd
; -------------------------------------------------------------------------------------------------
; floating point functions
optab:	!byte 121
	!word faddt-1

	!byte 121
	!word fsubt-1

	!byte 123
	!word fmultt-1

	!byte 123
	!word fdivt-1

	!byte 127
	!word fpwrt-1

	!byte 80
	!word andop-1

	!byte 70
	!word orop-1

negtab:	!byte 125
	!word negop-1

nottab:	!byte 90
	!word notop-1

ptdorl:	!byte 100
	!word dorel-1
; -------------------------------------------------------------------------------------------------
; reserved word list
reslst:
tkend=$80
	!scr "EN",$c4     		;  end
tkfor=tkend+1
	!scr "FO",$d2     		;  for
tknext=tkfor+1
	!scr "NEX",$d4    		;  next
tkdata=tknext+1
	!scr "DAT",$c1    		;  data
	!scr "INPUT",$a3  		;  input#
	!scr "INPU",$d4   		;  input
	!scr "DI",$cd     		;  dim
	!scr "REA",$c4    		;  read
	!scr "LE",$d4     		;  let
tkgoto=tkdata+6
	!scr "GOT",$cf    		;  goto
tkrun=tkgoto+1
	!scr "RU",$ce     		;  run
	!scr "I",$c6      		;  if
tkrest=tkrun+2
	!scr "RESTOR",$c5 		;  restore
tkgosu=tkrest+1
	!scr "GOSU",$c2   		;  gosub
	!scr "RETUR",$ce  		;  return
tkrem=tkgosu+2
	!scr "RE",$cd     		;  rem
	!scr "STO",$d0    		;  stop
tkon=tkrem+2
	!scr "O",$ce      		;  on
	!scr "WAI",$d4    		;  wait
	!scr "LOA",$c4    		;  load
	!scr "SAV",$c5    		;  save
	!scr "VERIF",$d9  		;  verify
	!scr "DE",$c6     		;  def
	!scr "POK",$c5    		;  poke
	!scr "PRINT",$a3  		;  print#
tkprin=tkon+8
	!scr "PRIN",$d4   		;  print
	!scr "CON",$d4    		;  cont
	!scr "LIS",$d4    		;  list
	!scr "CL",$d2     		;  clr
	!scr "CM",$c4     		;  cmd
	!scr "SY",$d3     		;  sys
	!scr "OPE",$ce    		;  open
	!scr "CLOS",$c5   		;  close
	!scr "GE",$d4     		;  get
tkscra=tkprin+9
	!scr "NE",$d7     		;  new
tktab=tkscra+1
	!scr "TAB",$a8    		;  tab(
tkto=tktab+1
	!scr "T",$cf      		;  to
tkfn=tkto+1
	!scr "F",$ce      		;  fn
tkspc=tkfn+1
	!scr "SPC",$a8    		;  spc(
tkthen=tkspc+1
	!scr "THE",$ce    		;  then
tknot=tkthen+1
	!scr "NO",$d4     		;  not
tkstep=tknot+1
	!scr "STE",$d0    		;  step
tkplus=tkstep+1
	!byte   $ab          		;  "+"
tkminu=tkplus+1
	!byte $ad          		;  "-"
	!byte $aa          		;  "*"
	!byte $af          		;  "/"
	!byte $de          		;  "^"
	!scr "AN",$c4     		;  and
	!scr "O",$d2      		;  or
tkgrea=tkminu+6
	!byte $be          		;  ">"
tkequl=tkgrea+1
	!byte $bd          		;  "="
tkless=tkequl+1
	!byte $bc          		;  "<"
tkonef=tkless+1
	!scr "SG",$ce     		;  sgn
	!scr "IN",$d4     		;  int
	!scr "AB",$d3     		;  abs
	!scr "US",$d2     		;  usr
	!scr "FR",$c5     		;  fre
	!scr "PO",$d3     		;  pos
	!scr "SQ",$d2     		;  sqr
	!scr "RN",$c4     		;  rnd
	!scr "LO",$c7     		;  log
	!scr "EX",$d0     		;  exp
	!scr "CO",$d3     		;  cos
	!scr "SI",$ce     		;  sin
	!scr "TA",$ce     		;  tan
	!scr "AT",$ce     		;  atn
	!scr "PEE",$cb    		;  peek
	!scr "LE",$ce     		;  len
	!scr "STR",$a4    		;  str$
	!scr "VA",$cc     		;  val
	!scr "AS",$c3     		;  asc
tklasn=tkonef+19
	!scr "CHR",$a4    		;  chr$
	!scr "LEFT",$a4   		;  left$
	!scr "RIGHT",$a4  		;  right$
	!scr "MID",$a4    		;  mid$
tkgo=tklasn+4
	!scr "G",$cf      		;  go
	!scr "CONCA",$d4  		;  concat
	!scr "DOPE",$ce   		;  dopen
	!scr "DCLOS",$c5  		;  dclose
	!scr "RECOR",$c4  		;  record
	!scr "HEADE",$d2  		;  header
	!scr "COLLEC",$d4 		;  collect
	!scr "BACKU",$d0  		;  backup
	!scr "COP",$d9    		;  copy
	!scr "APPEN",$c4  		;  apppend
	!scr "DSAV",$c5   		;  dsave
	!scr "DLOA",$c4   		;  dload
	!scr "UNI",$d4 			;  unit
	!scr "RENAM",$c5  		;  rename
	!scr "SCRATC",$c8 		;  scratch
	!scr "DIRECTOR",$d9		;  directory
	!scr "DCLEA",$d2  		;  dclear
	!scr "BAN",$cb    		;  bank
	!scr "BLOA",$c4   		;  bload
	!scr "BSAV",$c5   		;  bsave
	!scr "KE",$d9     		;  key
	!scr "DELET",$c5  		;  delete
tkelse=tkgo+22
	!scr "ELS",$c5    		;  else
tktrap=tkelse+1
	!scr "TRA",$d0    		;  trap
tkresu=tktrap+1
	!scr "RESUM",$c5  		;  resume
	!scr "DISPOS",$c5 		;  dispose
	!scr "PUDE",$c6   		;  pudef
tkusin=tkresu+3
	!scr "USIN",$c7   		;  using
tkerrd=tkusin+1
	!scr "ERR",$a4    		;  err$
tkinst=tkerrd+1
	!scr "INST",$d2   		;  instr
	!byte 0            		;  end of reserved word list
; -------------------------------------------------------------------------------------------------
; message vectors
ebase                   	;base for error messages
ms0 =*-ebase
	!word ams0
ms1 =*-ebase
	!word ams1
ms2 =*-ebase
	!word ams2
ms3 =*-ebase
	!word ams3
ms4 =*-ebase
	!word ams4
ms5 =*-ebase
	!word ams5
ms6 =*-ebase
	!word ams6
ms7 =*-ebase
	!word ams7
ms8 =*-ebase
	!word ams8
ms9 =*-ebase
	!word ams9
msg30 =*-ebase
	!word ams30
msg31 =*-ebase
	!word ams31

msrdy =*-ebase
	!word reddy
intxt =*-ebase
	!word aintxt
brktxt =*-ebase
	!word abrktx
exignt =*-ebase
	!word aexi
tryagn =*-ebase
	!word atry
fbptr =*-ebase
	!word fbuffr
mremsg =*-ebase
	!word aremsg
asigon =*-ebase
	!word signon
awords =*-ebase
	!word words

errnf =*-ebase
	!word aernf
errsn =*-ebase
	!word aersn
errrg =*-ebase
	!word aerrg
errod =*-ebase
	!word aerod
errfc =*-ebase
	!word aerfc
errov =*-ebase
	!word aerov
errom =*-ebase
	!word aerom
errus =*-ebase
	!word aerus
errbs =*-ebase
	!word aerbs
errdd =*-ebase
	!word aerdd
errdvo =*-ebase
	!word aerdvo
errid =*-ebase
	!word aerid
errtm =*-ebase
	!word aertm
errls =*-ebase
	!word aerls
errbd =*-ebase
	!word aerbd
errst =*-ebase
	!word aerst
errcn =*-ebase
	!word aercn
erruf =*-ebase
	!word aeruf
errld =*-ebase
	!word aerld
errvr =*-ebase
	!word aervr
erros =*-ebase
	!word aeros
errcr =*-ebase
	!word aercr
errdi =*-ebase
	!word aerdi
errbln=*-ebase
; -------------------------------------------------------------------------------------------------
; kernal error messages.
ams0:	!scr "STOP KEY DETECTED",0
ams1:	!scr "TOO MANY FILES",0
ams2:	!scr "FILE OPEN",0
ams3:	!scr "FILE NOT OPEN",0
ams4:	!scr "FILE NOT FOUND",0
ams5:	!scr "DEVICE NOT PRESENT",0
ams6:	!scr "NOT INPUT FILE",0
ams7:	!scr "NOT OUTPUT FILE",0
ams8:	!scr "MISSING FILE NAME",0
ams9:	!scr "ILLEGAL DEVICE "
	!scr "NUMBER",0

; basic error messages.
ams30:	!scr cr,"ARE YOU SURE ?",0
ams31:	!scr cr,"BAD DISK ",cr,0

aernf:	!scr "NEXT WITHOUT FOR",0
aersn:	!scr "SYNTAX ERROR",0
aerrg:	!scr "RETURN WITHOUT GOSUB",0
aerod:	!scr "OUT OF DATA",0
aerfc:	!scr "ILLEGAL QUANTITY",0
aerov:	!scr "OVERFLOW",0
aerom:	!scr "OUT OF MEMORY",0
aerus:	!scr "UNDEFINED STATEMENT",0
aerbs:	!scr "BAD SUBSCRIPT",0
aerdd:	!scr "REDIM",$27,"D ARRAY",0
aerdvo:	!scr "DIVISION BY ZERO",0
aerid:	!scr "ILLEGAL DIRECT",0
aertm:	!scr "TYPE MISMATCH",0
aerls:	!scr "STRING TOO LONG",0
aerbd:	!scr "FILE DATA",0
aerst:	!scr "FORMULA TOO COMPLEX",0
aercn:	!scr "CANNOT CONTINUE",0
aeruf:	!scr "UNDEFINED FUNCTION",0
aerld:	!scr cr,"?LOAD ERROR",cr,0
aervr:	!scr cr,"?VERIFY ERROR",cr,0

aintxt:	!scr " IN ",0
reddy:	!scr cr,"READY.",cr,0
abrktx:	!scr cr,"BREAK",0

aexi:	!scr "EXTRA IGNORED",cr,0
atry:	!scr "REDO FROM START",cr,0
aremsg:	!scr "MORE",cr,0
aeros:	!scr "OUT OF STACK",0
aercr:	!scr "UNABLE TO RESUME",0
aerdi:	!scr "UNABLE TO DISPOSE",0
; ******************************************* CONTRL **********************************************
; 'output error code and start again'
omerr:	ldx #errom		; out of memory

;   entry: x= offset from errtable
error:	jmp (ierror)

nerror:	stx errnum		; save error index
	cpx #errus		; don't trap undefined statement errors
	beq errisd
	jsr tstdir		; don't trap if in direct mode
	beq errisd
	ldy trapno+1		; is trap vector set?
	iny
	beq errisd		; not if $ff
	dey			; restore trapno+1
	sty linnum+1
	sty tmptrp		; save until a resume is executed
	ldy trapno		; finish vector
	sty linnum
	ldy #$ff
	sty trapno+1		; marks no more traps until a 'resume' or 'trap'
	ldx #1

sots:	lda curlin,x
	sta errlin,x
	lda oldtxt,x
	sta errtxt,x
	dex
	bpl sots
	ldx oldstk
	txs
	jsr luk4it		; set up pointers
	jmp nstt9		; and goto that line

errisd:	lda channl
	beq error2		; close non-terminal channel.
	jsr clrch		; close it
	lda #0
	sta channl
error2:	jsr ocrlf		; output new line
	jsr outqst		; output '?'
	ldx errnum		; restore offset

	jsr msg			; output message

	jsr stkini		; reset stk and flags

errfin:	ldy curlin+1
	cpy #$fa
	bcs ready		; if # = 64000, don't type line #
	ldx #intxt
	jsr msg
	lda curlin+1
	ldx curlin

	jsr linprt		; list line
ready:	ldx #msrdy
	jsr msg			; ready.
	lda #$80		; turn off messages
	jsr setmsg
; *************************************************************************************************
; main loop for basic
;
; LOAD -> fini
;
main:	jmp (imain)

nmain:	ldx #255
	stx curlin+1
	stx errlin
	stx errlin+1		; flag no errors yet
	stx errnum
	jsr inlin		; get a line from terminal
	sta txtptr		; txtptr:=^to input buffer
	sty txtptr+1
	jsr chrget
	tax
	beq main		; if end of line
	bcc main1		; if line number
	cmp #'@'		; check DOS command?
	bne main0
	jsr doscmd		; dos command
	jmp ready

main0:	jsr crunch
	jsr chrgot		; get command
	jmp xeqdir		; execute command

main1:	jsr linget		; read line # into linnum
	jsr crunch
	sty count		; retain char count
	jsr fndlin
	bcc nodel		; no match, so don't delete
	ldy #1
	lda (lowtr),y
	sta index1+1

	lda vartab		; text end (64k)
	sta index1
	lda lowtr+1		; set xfer to
	sta index2+1
	dey
	lda (lowtr),y		;  compute length
	clc
	sbc lowtr
	eor #$ff		;  make it negative
	clc
	adc vartab		; compute new text end
	sta vartab
	sta index2		; set low of xfer to
	lda vartab+1
	adc #255
	sta vartab+1		; compute high of text end
	sbc lowtr+1		; compute # of blocks to move
	tax
	sec
	lda lowtr
	sbc vartab		; compute offset
	tay
	bcs qdect1		; if vartab <= lowtr
	inx			; dec due to carry and
	dec index2+1		; dec store so carry works
qdect1:	clc
	adc index1
	bcc mloop
	dec index1+1
	clc
mloop:	lda (index1),y
	sta (index2),y
	iny
	bne mloop
	inc index1+1
	inc index2+1
	dex
	bne mloop
nodel:	jsr clearc
	jsr lnkprg
	ldy #0
	lda (txtptr),y		; delete line?
	bne nodele		; no...
	jmp main

nodele:	clc			; no...something to insert

	lda vartab
	ldy vartab+1
	sta hightr
	sty hightr+1		; top of block to move

	adc count		; length of characters in line
	bcc nodel1
	iny
nodel1:	clc
	adc #4 			; plus link and line #
	bcc nodelc
	iny
nodelc:	sta highds		; destination of top
	sty highds+1

; low block address is lowtr
; where it was left in the call to fndlin
	jsr bltu

; make links non-null to fool chead
	ldy #0
	lda #1
	sta (lowtr),y
	iny
	sta (lowtr),y

; put line number in text
	iny
	lda linnum
	sta (lowtr),y
	lda linnum+1
	iny
	sta (lowtr),y

; advance lowtr to start of line
	iny
	tya
	clc
	adc lowtr
	sta lowtr
	bcc main2
	inc lowtr+1

main2:	lda strend		; 64k only
	ldy strend+1
	sta vartab
	sty vartab+1

; block move line to text
	ldy count
	dey
stolop:	lda (txtptr),y
	sta (lowtr),y
	dey
	dec count
	bne stolop
fini:	jsr lnkprg
	jsr runc
	jmp main

lnkprg:	lda txttab
	ldy txttab+1
	sta index
	sty index+1
	clc 
chead:	ldy #0
	lda (index),y		; check for null link
	bne chea3
	iny
	lda (index),y
	beq lnkrts
chea3:	ldy #4
czloop:	iny
	lda (index),y
	bne czloop
	iny
	tya
	adc index
	tax
	ldy #0
	sta (index),y
	tya
	adc index+1
	iny
	sta (index),y
	stx index
	sta index+1
	bcc chead		; always
lnkrts:	clc
	lda index		; set pointer to end of text
	ldy index+1
	adc #2
	bcc *+3
	iny
	sta varend
	sty varend+1
	rts
; -------------------------------------------------------------------------------------------------
; line input routine
;   enter: buffpt is used as pointer to start of input buffer.
;   exit:  regs contain pointer to byte preceding buffer
;   index1 also has this pointer value.
;   .a, .y = offset

inlin:	lda buffpt
	ldy buffpt+1
	sta index1
	sty index1+1
	ldy #0
inlinc:	sty count
	jsr basin
	cmp #cr
	beq fininl
	ldy count
	sta (index1),y
	iny
	cpy #bufsiz
	bcc inlinc
	jmp errlen		; string too long
fininl:	ldy count
	lda #0
	sta (index1),y		; line terminator
	lda channl
	bne *+5
	jsr ocrlf
	ldy index1+1
	ldx index1
	bne *+3
	dey
	dex
	txa
	rts
; -------------------------------------------------------------------------------------------------
; searches program text for the line whose number is passed in 'linnum'.
;   exit:
;   cbit set: lowtr -> to the link field in the line which is the one searched for.
;   cbit clear: line not found. (lowtr) -> line in the program > the one sough after
;   always assume text bank should be used
fndlin:	lda txttab
	ldx txttab+1
fndlnc:	sta lowtr
	stx lowtr+1
	ldy #0
	lda (lowtr),y		; check for null link
	bne fndln3
	iny
	lda (lowtr),y
	beq flnrt
fndln3:	ldy #3
	lda (lowtr),y
	cmp linnum+1
	beq fndl20		; look at low bytes
	bcc affrts		; if not equal,try next
flnrt:	clc			; didn't find it
flnrts:	rts

fndl20:	dey
	lda (lowtr),y
	cmp linnum
	beq flnrts		; if number found
	bcs flnrt		; if above number
affrts:	ldy #1
	lda (lowtr),y
	tax
	dey
	lda (lowtr),y
	bcc fndlnc		; always
; -------------------------------------------------------------------------------------------------
; here for new statment.
;   character -> by txtptr is ':' or eol. the adr of this loc is left on the stack when a statement
;   is executed so that it can merely do a rts when it is done.
;   get char, exit via xeqcm3, and return to newstt
; MAIN LOOP -> xeqdir (now routed form new main loop)
; FOR -> newstt
; NERROR -> nstt9 
; GOSUB -> newstt
; IF THEN ELSE -> xeqcm3 (write a ':' after THEN and ELSE as workaround)
; ON GOTO -> xeqcm2
xeqcm:
gone:	jmp (igone)
ngone:	jsr chrget		; get statement type
xeqdir:	jsr xeqcm3

newstt:	jsr kstop
	bne nstt1		; if stop not requested
	sec			; brk msg desired
	jmp stopc		; if stop requested

sav42:	lda (txtptr),y		; make sure it's not end-of-text
	bne nstt4
	iny
	lda (txtptr),y		; end of text storage?
	bne nstt4		; no...go to next line
nstt3:	jmp ready			; yes...finished

nstt4:	ldy #3			; new line, update pointers
	lda (txtptr),y 		; extract line# lo byte
	sta curlin
	iny
	lda (txtptr),y		; extract line # hi byte
	sta curlin+1
ffrts1:	rts

nstt1:	jsr tstdir		; are we in direct mode?
	beq nstt2		; yes...

; in run mode...
; save txtptr for cont command
nstt9:  lda txtptr		; entry point for error trapping
	ldy txtptr+1
	sta oldtxt
	sty oldtxt+1
	tsx
	stx oldstk		; save in case of error

nstt2:	ldy #0
	lda (txtptr),y		; end of the line?
	bne morsts		; no...end of statement

	jsr tstdir		; in direct mode?
	beq nstt3		; yes...finished execution

; in run mode...
; look for start of next line
	ldy #1
	jsr sav42
	tya 			; y=4
	clc
	adc txtptr 		; point @ character before line start
	sta txtptr
	bcc xeqcm
	inc txtptr+1
	bne xeqcm		; always...execute new line

; set up for command processing and set processor address on stack, exit via jmp to chrget
xeqcm3:	beq ffrts1
xeqcm2:	cmp #pi
	beq snerr1
	sec
	sbc #tkend
	bcc glet
	cmp #tkscra-tkend+1
	bcc nstt6
	cmp #tkgo-tkend
	bcc snerr1
;
	cmp #TKVDP-tkend	; token >= $e9
	bcs VdpStmt		; -> vdp statement
;
	cmp #tkerrd-tkend
	bcs snerr1		; trap err$ & instr$
	sbc #tkgo-tkscra-2
nstt6:	asl
	tay
	lda stmdsp+1,y
	pha
	lda stmdsp,y
	pha			; process address on stack
	jmp chrget		; process command

glet:	jmp let

morsts:	cmp #':'
	bne snerr1
	jmp xeqcm		; if ':', continue statement
snerr1:	jmp snerr		; -> syntax error, ready
;
VdpStmt:
	sbc #TKVDP-tkend	; vdp commands 0-21 ($e9-$fe)
	cmp #(VdpStmdspEnd-VdpStmdsp)/2
	bcs snerr1		; > last token -> error
	asl			; table position
	tay
	lda VdpStmdsp+1,y
	pha
	lda VdpStmdsp,y
	pha
	jmp chrget
; -------------------------------------------------------------------------------------------------
; find a 'for' entry on the stack via varpnt
;   exit:
;   not found - z=0
;   found -  z=1, .x=stack offset to for entry
;   fndfor is also used by the return statement to clear off all loops activated during a gosub call. i.e.,
;   all for entries between tos and last gosub entry. register .a contains token value which ended the search.
;   when fndfor is used by a next statement for which no for-variable was given, forpnt shall contain values
;   impossible for actual variables.
fndfor:	tsx			 ; load x with stk ptr.
	inx
	inx
	inx
	inx		 	; ignore adr (newstt) and rts adr.

ffloop:	lda stack+1,x		; get stack entry
	cmp #tkfor
	bne ffrts		; if not 'for' token
	lda forpnt+2		; test for real bank#
	bpl cmpfor		; yes...
	lda stack+2,x		; no, so assume this one
	sta forpnt
	lda stack+3,x
	sta forpnt+1
	lda stack+4,x
	sta forpnt+2
cmpfor:	cmp stack+4,x
	bne addfrs
	lda forpnt+1
	cmp stack+3,x
	bne addfrs
	lda forpnt
	cmp stack+2,x
	beq ffrts
addfrs:	txa 
	clc
	adc #forsiz
	tax
	bne ffloop
ffrts:	rts
; -------------------------------------------------------------------------------------------------
; block transfer routine(s).
;   make space by shoving everything forward.

;   this is done for two purposes:
;   1. to make room for new statements
;   2. to make room for new simple variables.

;   for the 64k version, a single block transfer routine (bltu)
;   serves both purposes. for the 128k, 192k, and 256k versions,
;   separate routines, bltut (for text) and bltuv (for vars),
;   are used as well as a general version of bltu.

;   the allocation of new space requires that checks be made
;   to see if enough memory exists. for the 64k version, the
;   routine, reason, is called. for the 128k, 192k, and 256k
;   versions, reason is called for variables and reasnt is
;   called for text.

;   entry: y,a = (highds)
;   (highds)= destination of high address
;   (lowtr) = lowest adr to be xferred
;   (hightr)= highest adr to be xferred

;
;   exit: (lowtr) = unchanged
;   (hightr)= (lowtr)-80h
;   (highds)= lowest adr xferred into minus 80h
;    high boundary set to new value:
; bltu(64k): strend
; bltut: text end
; bltuv: variable end (strend or varend)
bltu:
	jsr reason		; 64k version
	sta strend
	sty strend+1
	sec			; (all versions) prepare to subract
	lda hightr
	sbc lowtr		; compute # of things to move and save
	sta index
	tay
	lda hightr+1
	sbc lowtr+1
	tax			; put it in a counter reg
	inx			; so that cntr algorithm works
	tya			; see if low part of cnt is 0
	beq decblt		; if 0, start moving blocks
	lda hightr		; must justify base adr
	sec
	sbc index
	sta hightr
	bcs blt1
	dec hightr+1
	sec
blt1:	lda highds
	sbc index
	sta highds
	bcs moren1
	dec highds+1
	bcc moren1
bltlp:	lda (hightr),y
	sta (highds),y
moren1:	dey
	bne bltlp
	lda (hightr),y
	sta (highds),y
decblt:	dec hightr+1
	dec highds+1
	dex
	bne moren1
	rts
; -------------------------------------------------------------------------------------------------
; ascertain that a given number of locs remain available for the stack.
;   entry: lda #number of word entries needed
;   jsr getstk must be called by any routine which puts an arbitrary amount of stuff on the stack.
;   note: routines that merely use and free up the guaranteed numlev locations need not call getstk.
;   exit: a and x have been modified.
getstk:	asl			; an entry is two bytes
	adc #numlev+numlev	; overhead required for maximum
	bcs oserr
	sta index		; this much memory must be left
	tsx
	cpx index 		; is it?
	bcs rearts		; yes...

oserr:	ldx #erros		; out of stack space error
	jmp error

reason:
	cpy fretop+1
	bcc rearts
	bne trymor
	cmp fretop
	bcc rearts
trymor:	pha
	ldx #8+addprc
	tya
reasav:	pha
	lda highds-1,x
	dex
	bpl reasav
	jsr garba2
	ldx #248-addprc
reasto:	pla
	sta highds+8+addprc,x
	inx
	bmi reasto
	pla
	tay
	pla
	cpy fretop+1
	bcc rearts
	bne omerrc
	cmp fretop
	bcc rearts
omerrc:	jmp omerr		; out of memory error

sav73:	jsr ptrget
sav74:	sta forpnt
	sty forpnt+1
	stx forpnt+2
rearts:	rts
; -------------------------------------------------------------------------------------------------
; crunch
;   entry:  txtptr points to start of text to crunch
;   exit:   txtptr points to start of crunched text
;   calls:  chrget, chrgot, reser, kloop, rem, data
;   collapses all reserved words to tokens.  does not alter data or rem.  removes all graphic
;   characters not in quoted strings

crunch:	jmp (icrnch)

ncrnch:	lda txtptr		; save old text loc
	pha
	lda txtptr+1
	pha
crun05:	jsr chrgot
	jmp crun20

crun10:	jsr chrget
crun20:	bcc crun10		; don't crunch numbers
	cmp #0			; end of line?
	beq crun90		; yes...
	cmp #':'		; multi-stmt char?
	beq crun10
	cmp #'?'		; print abreviation?
	bne crun30		; no...
	lda #tkprin		; substitute print token
	ldy #0
	sta (txtptr),y
	beq crun10		; branch always
crun30:	cmp #$80		; graphics?
	bcc crun40		; no...
	cmp #pi			; yes...pi?
	beq crun10		; o.k....leave alone
	ldy #1
	jsr kloop		; crunch out graphics
	beq crun05		; branch always
crun40:	cmp #$22 ; "		; quote string?
	bne crun60		; no...
crun50: jsr chrget
	cmp #$0			; end of line?
	beq crun90		; yes...
	cmp #$22 ; "		; close quote?
	beq crun10		; yes...
	bne crun50		; no...
crun60:	jsr reser		; reserved word?
	bcc crun10
	cpy #0			; anything to move?
	beq crun70		; no...
	jsr kloop		; crunch it out
crun70:	lda count		; put token...
	ldy #0
	sta (txtptr),y		; in text
	cmp #tkrem
	beq crun80
	cmp #tkdata
	bne crun10
	jsr chrget
	jsr data
	jmp crun05
crun80:	jsr chrget
	jsr rem

; no other statements can follow a rem
crun90:	ldx txtptr
	pla
	sta txtptr+1
	pla
	sta txtptr
	sec			; compute length of line
	txa
	sbc txtptr
	tay
	iny
	rts
; -------------------------------------------------------------------------------------------------
; kloop
;   crunch loop.  moves offset .y characters from txtptr to end of line
kloop:	clc			; compute source address
	tya
	adc txtptr
	sta index1
	lda txtptr+1
	adc #0
	sta index1+1
	ldy #0
kloop2:	lda (index1),y		; move source
	sta (txtptr),y		; to destination offset
	iny
	cmp #0			; end of line?
	bne kloop2		; no...
	rts
; -------------------------------------------------------------------------------------------------
; reser
;   search reserved word list for a match
;   entry:  (txtptr) is first char of word to match
;   exit:   .y=length of word matched
;   .c=success/fail (set/clear) flag
;   count=token value
reser:	lda #>reslst		; start search here
	ldy #<reslst
	sta index1+1
	sty index1
	ldy #0
	sty count
	dey
rese10:	iny
rese20:	lda (txtptr),y
	sec
	sbc (index1),y		; does letter match?
	beq rese10		; yes...continue
	cmp #eom		; no...end of word?
	beq rese60		; yes...c set...done

; find next word
rese30:	lda (index1),y
	bmi rese40		; found end of current
	iny
	bne rese30
rese40:	iny			 ; start of next
	inc count		; value of token
	clc
	tya
	jsr sav14
	ldy #0
	lda (index1),y		;  end of list?
	
	beq rese70		; 0 = yes..start with vdplst
	cmp #1			; 1 = end of vdplst ?

	bne rese20		; no...
	clc
; yes...carry clear...fail
rese60:	ora count		; .a=$80 if match
	sta count		; token is formed
	rts

rese70:	lda #<Vdplst		; set index to vdplst
	sta index
	lda #>Vdplst
	sta index+1
	bne rese20		; branch always
; ****************************************** BVERBS1 **********************************************
; LIST - process list verb

list:	jsr range		; set up line range
	bne lstend		; ok...
	rts			; some bad params, do nothing

lstend:	pla
	pla

list4:	ldy #1
	sty dores
	lda (lowtr),y
	bne list44
	dey
	lda (lowtr),y
	beq grody
list44:	ldy #1
	jsr kstop
	bne list5		; if no stop requested
	clc			; no brk msg desired for...
	jmp stopc		; ...process stop

list5:	jsr ocrlf		; new line
	iny
	lda (lowtr),y
	tax
	iny
	lda (lowtr),y
	cmp linnum+1
	bne tstdun
	cpx linnum
	beq typlin

tstdun:	bcs grody
typlin:	sty lstpnt
	jsr linprt
	lda #$20
prit4:	ldy lstpnt			; restore position y
	and #$7f			; clear bi7t from last char of word
ploop:	jsr ochr			; output char (first char after linenumber is space already in a)
	cmp #$22;"			; test for quote
	bne ploop1			; no.. skip
	lda dores			; toggle reverse flag (init at line start =$01)
	eor #$ff			; $fe or i6509
	sta dores
ploop1:	iny				; next char
	beq grody			; $00 ? -> end of line
	lda (lowtr),y			; load next char
	bne qplop			; -> check char/token
; pointer to next line
	tay				; y = 0
	lda (lowtr),y			; load address low of next line
	tax
	iny
	lda (lowtr),y			; load address high
	stx lowtr			; set lowtr pointer to next line address
	sta lowtr+1
	jmp list4			; -> list next line
grody:	jmp ready			; end of list

qplop:	jmp (iqplop)
; check char / token
nqplop:	bpl ploop			; print char if no token
	cmp #pi
	beq ploop			; print pi
	bit dores			; check id just switched to reverse mode
	bmi ploop			; print quote
	sty lstpnt			; remember y
;
	cmp #TKVDP			; vdp token ?
	bcc ldresl			; no.. -> select standard reslst
; vdp token
	ldy #>Vdplst			; set index to reserved word list
	sty index+1
	ldy #<Vdplst
	sty index
	sec
	sbc #TKVDP-tkend		; calc vdp token 0-21	
	bmi rescr0			; -> skip always
; basic token
ldresl:	ldy #>reslst			; set index to reserved word list
	sty index1+1
	ldy #<reslst
	sty index1

rescr0:	tax				; token in x
	ldy #0
plop20:	
	asl				; *2 (clears bit 7 for token)
	beq prit3b			; -> print word
; point index1 at start of word
resrch:	dex				; dec x wor next word in table
	bpl prit3			; -> print if word number found
rescr1:	inc index1			; increase pointer to reslst
	bne rescr2
	inc index1+1
rescr2:	lda (index1),y			; load char from table
	bpl rescr1
	bmi resrch
; list word indexing by y
prit3:	iny				; next char of word
prit3b:	lda (index1),y			; load char from reserved word list
	bmi prit4			; last char ? -> clear bit#7 and print 
	jsr ochr			; print char
	bne prit3			; always (returned char is never zero)
; -------------------------------------------------------------------------------------------------
; process new and clr commands.
;   scrtch resets text area.
;   clearc resets data areas.

scrath:	bne stkrts
scrtch:	lda #0
	tay
	sta (txttab),y
	iny
	sta (txttab),y
	clc
	lda txttab
	adc #2
	sta vartab
	lda txttab+1
	adc #0
	sta vartab+1

runc:	jsr stxtpt
	lda #0

; clearc:
;   strend:=arytab:=vartab
clear:	bne stkrts
clearc:	jsr settop			; set top of memory
	lda #$ff			; indicate no error trap vector
	sta trapno+1
	ldx #pumony-puchrs		; reset print using chars
clrpu:	lda pudefs,x
	sta puchrs,x
	dex
	bpl clrpu
	lda #0
	sta dsdesc			; clear ds$
	jsr clall
	lda vartab
	ldy vartab+1
	sta strend
	sty strend+1
	sta arytab
	sty arytab+1
fload:	jsr resto1

; reset stk, clear oltxt and current channel
stkini:	ldx #0
	stx temppt
	pla 
	tay
	pla			; get callers address
	ldx #$fe		; reset stack (leave $01ff free)
	txs
	pha
	tya
	pha			; restore caller's address
	lda #0
	sta oldtxt
	sta oldtxt+1
	sta channl		; clear out channel
	sta dolu
	sta subflg
stkrts:	rts

pudefs:	!scr " ,.$"		; default fill,comma,dec pt,money symbols
; -------------------------------------------------------------------------------------------------
; FOR
;    a 'for' entry on the stk has the following format using sp relative offsets:
;    (17-18) txtptr value to end of for-stmt
;    (15-16) line number of for-stmt
;    (10-14) limit value, in basic floating form
;    (04-09) step value, in fac floating form
;    (01-03) ptr to for-var's data area
;    (00)    tkfor token

;   this code creates a new for-entry on the stack.
;   note, return to the caller of "for" is cleared and control jumps to newstt. active for-loops between this
;   and an older loop of the same for-variable are cleared off.

for:	lda #128
	sta subflg		; integer for-vars illegal
	jsr let			; for-var <- initial value
	jsr fndfor		; pop off redundant entries
	bne notol
	txa
	adc #forsiz-3
	tax
	txs
notol:	pla
	pla
	lda #8+addprc
	jsr getstk
	jsr datan
	clc
	tya
	adc txtptr		; ptr to end-of-for-stmt
	pha
	lda txtptr+1
	adc #0
	pha
	lda curlin+1		; line number
	pha
	lda curlin
	pha
	lda #tkto
	jsr synchr		; check for 'to' token
	jsr chknum
	jsr frmnum		; compute limit, round
	lda facsgn
	ora #127
	and facho
	sta facho
	lda #<ldfone		; return from forpsh
	ldy #>ldfone
	sta index1
	sty index1+1
	jmp forpsh
ldfone:	lda #<fone		; "one",default step
	ldy #>fone
	jsr movfm
	jsr chrgot
	cmp #tkstep
	bne oneon		; no step given...
	jsr chrget
	jsr frmnum		; compute step value
oneon:	jsr sign
	jsr pushf
	lda forpnt+2		; for-var pointer
	pha
	lda forpnt+1
	pha
	lda forpnt
	pha
	lda #tkfor		; for token
	pha
	jmp newstt		; get new statement
; -------------------------------------------------------------------------------------------------
; NEXT

next:	bne getfor
	ldx #$ff		; garbage bank#
	bmi stxfor		; always
getfor:	jsr ptrget
stxfor:	jsr sav74
	jsr fndfor
	beq havfor
	ldx #errnf
	jmp error

havfor:	txs
	txa
	clc
	adc #2+ptrsiz		;  offset to step va lue
	pha
	adc #5+addprc
	sta index2		; offset to limit value
	pla
	ldy #1
	jsr movfm		; fac<-step value
	tsx
	lda stack+9+addprc,x
	sta facsgn
	lda forpnt
	ldy forpnt+1
	ldx forpnt+2
	jsr ucnupk		; acc <- for value
	jsr faddt
	jsr movvf		; for var=for var+step
	ldy #1
	jsr fcompn		; compare to limit
	tsx
	sec			; correct results for sign
	sbc stack+9+addprc,x
	beq loopdn
	lda stack+14+addprc+addprc,x
	sta curlin		; not done, repeat...
	lda stack+15+addprc+addprc,x
	sta curlin+1
	lda stack+17+addprc+addprc,x
	sta txtptr
	lda stack+16+addprc+addprc,x
	sta txtptr+1
newsgo:	jmp newstt

loopdn:	txa			; cy=1
	adc #16+addprc+addprc
	tax
	txs			; pop for-entry off stack
	jsr chrgot
	cmp #','
	bne newsgo
	jsr chrget
	jsr getfor
	jmp frmnum
; -------------------------------------------------------------------------------------------------
; RESTORE

restor:	beq resto1		; no argument..set pointer to start of text
	jsr getpin 		; get line #
	sty linnum 		; store < of arg
	sta linnum+1		; store > of arg
	jsr fndlin 		; point to link of arg. line #
	bcc reserr		; line not found
	lda lowtr		; fndlin's result
	sbc #1
	sta datptr
	lda lowtr+1
	sbc #0
	sta datptr+1
	rts
resto1:	sec
	lda txttab
	sbc #1
	sta datptr
	lda txttab+1
	sbc #0
	sta datptr+1
runm10:	rts
reserr:	jmp userr 		; undef'd statement
; -------------------------------------------------------------------------------------------------
; STOP - process stop verb

stop:	bcs stpz2
end:	clc
stpz2:	bne contx
	beq stopd		; always

; here if stop key depressed while running
; enter: cy=0   =>  no break message desired
stopc:	php        		; entered from list or newstt
	ldy trapno+1   		; test if error trapping on
	iny
	beq sa         		; branch if not

unstop: jsr kstop      		; don't go on till stop key no longer depressed
	beq unstop
	ldx #$1c       		; otherwise load 'break' error number
	jmp error      		; and error routine will goto error trapping
sa:	plp
	pha
	pha		 ; ....push dummy return to be discarded
stopd:	php		 ;  save cy flag
	jsr tstdir		; in direct mode?
	beq nsxx4		; yes...can't continue
	lda txtptr
	ldy txtptr+1
	sta oldtxt
	sty oldtxt+1
	lda curlin
	ldy curlin+1
	sta oldlin
	sty oldlin+1
nsxx4:	plp		 ; restore cy flag
	pla		 ; pop off return to newstt from xeqcm3
	pla
endcon:	bcc nsxx6		; if no break message
	ldx #brktxt
	jsr msg		 ; output message
	jmp errfin

nsxx6:	jmp ready		; output ready message

cont:	bne contx		; make sure there is a terminator
	ldy oldtxt+1
	bne contz3		; if there is code to continue
	lda oldtxt
	bne contz4
	ldx #errcn
	jmp error
contz3 lda oldtxt
contz4 sta txtptr
	sty txtptr+1
	lda oldlin
	ldy oldlin+1
	sta curlin
	sty curlin+1
contx:	rts
; -------------------------------------------------------------------------------------------------
; RUN - process run verb

run:	jsr runmod		; set run mode
	jsr chrgot		; is there a line # ?
	bne run2		; yes...
	jmp runc		; no...run from txttab and clear stack
run2 jsr clearc			; run from line #...only clear stack
	beq runc2		; common routine for goto and gosub ... always go

;initialize run mode
;can be started by run,goto, or gosub
runmod:	jsr tstdir
	bne runm10		; already running
	lda #$fe		; curlin>64999 and <> $ff00
; neither valid line or direct mode
	sta curlin+1
	lda #0
	jmp setmsg		; no loading messages, etc. during run
; -------------------------------------------------------------------------------------------------
; GOSUB -  process gosub verb
;   on entry has the following format
;   1. the gosutk one byte
;   2. the line # of the gosub statement - two bytes
;   3. a -> into the text of the gosub   - two bytes
;   total of 5 bytes

gosub:	lda #3
	jsr getstk		; make sure there is room
	lda txtptr+1
	pha
	lda txtptr
	pha			; push text ptr onto stk
	lda curlin+1
	pha
	lda curlin
	pha			; push current line #
	lda #tkgosu
	pha			; push on a gosub token
runc2:	jsr chrgot		; get chr and set codes for linget
	jsr goto
	jmp newstt
; -------------------------------------------------------------------------------------------------
; IF

if:	jsr frmevl
	bit valtyp
	bpl if50
	jsr frefac		; if string expr
if50:	jsr chrgot
	cmp #tkgoto
	beq okgoto
	lda #tkthen
	jsr synchr
okgoto:	lda facexp
	bne docond
lkelse:	jsr data		; skip to next ":" or end-of-line
	ldy #0
	lda (txtptr),y
	beq rem			; eol, no else-clause
	jsr chrget		; is stmt an "else" ?
	cmp #tkelse
	bne lkelse		; no, keep looking...
	jsr chrget		; skip over tkelse
docond:	jsr chrgot
	bcc goto
	jmp xeqcm3
; -------------------------------------------------------------------------------------------------
; REM

rem:	jsr remn		; scan to end of line
	beq addon 		; always

go:	jsr chrgot
	lda #tkto
	jsr synchr
; -------------------------------------------------------------------------------------------------
; GOTO - process goto verb

goto:	jsr linget
goto0:				; always goto0=goto+3
	jsr remn
goto1:	sec
	lda curlin
	sbc linnum
	lda curlin+1
	sbc linnum+1
	bcs luk4it
	tya
	sec
	adc txtptr
	ldx txtptr+1
	bcc lukall
	inx
	bcs lukall
luk4it:	lda txttab
	ldx txttab+1
lukall:	jsr fndlnc
	bcc userr
	lda lowtr
	sbc #1
	sta txtptr
	lda lowtr+1
	sbc #0
	sta txtptr+1
	jmp runmod         	; put into run mode
; -------------------------------------------------------------------------------------------------
; RETURN - process return verb

return bne addrts
	lda #$ff		; improbable offset(hi)
	sta forpnt+1
	jsr fndfor
	txs
	cmp #tkgosu
	beq retu1
	ldx #errrg
	!byte $2c		; eat the next two bytes
userr:	ldx #errus
	jmp error

retu1:	pla
	pla
	sta curlin
	pla
	sta curlin+1
	pla
	sta txtptr
	pla
	sta txtptr+1
; -------------------------------------------------------------------------------------------------
; DATA

data:	jsr datan
addon:	tya
	clc
	adc txtptr
	sta txtptr
	bcc addrts
	inc txtptr+1
addrts:	rts
; -------------------------------------------------------------------------------------------------
; datan, remn

datan:	ldx #':'
	!byte $2c		; eat the next two bytes
remn:	ldx #0
remn1:	stx charac
	ldy #0
	sty endchr
exchqt:	lda endchr
	ldx charac
	sta charac
	stx endchr
remer:	lda (txtptr),y
	beq remtxt
	cmp endchr
	beq remtxt
	iny
	cmp #$22 ; "
	bne remer
	beq exchqt		; always
remtxt:	rts
; -------------------------------------------------------------------------------------------------
; TRAP

trap:	jsr errdir		; no direct mode allowed
	jsr chrgot		; look for argument
	beq trap1		; null means no arg.
	jsr getpin		; get what should be a line number
	sty trapno		; lo byte of argument
	!byte $2c		; eat 2
trap1:	lda #$ff		; flag no trap
	sta trapno+1
	rts
; -------------------------------------------------------------------------------------------------
; ON GOTO

ongoto:	jsr getbyt
	pha
	cmp #tkgosu
	beq onglop
	cmp #tkgoto
	beq onglop		; if goto
snerr3:	jmp snerr		; -> syntax error, ready

onglop:	dec faclo
	bne onglp1
	pla
	jmp xeqcm2

onglp1:	jsr chrget
	jsr linget
	cmp #','
	beq onglop
	pla
ongrts:	rts
; -------------------------------------------------------------------------------------------------
; linget
linget:	ldx #0
	stx linnum
	stx linnum+1
morlin:	bcs ongrts
	sbc #$2f
	sta charac
	lda linnum+1
	sta index
	cmp #25
	bcs snerr3
	lda linnum
	asl
	rol index
	asl
	rol index
	adc linnum
	sta linnum
	lda index
	adc linnum+1
	sta linnum+1
	asl linnum
	rol linnum+1
	lda linnum
	adc charac
	sta linnum
	bcc nxtlgc
	inc linnum+1
nxtlgc:	jsr chrget
	jmp morlin

ressnr:	bne snerr3
; -------------------------------------------------------------------------------------------------
; LET
let:	jsr sav73
	lda #tkequl
	jsr synchr
	lda intflg
	pha
	lda valtyp
	pha
	jsr frmevl
	pla
	rol
	jsr chkval
	bne copstr
	pla
qintgr:	bpl copflt
	jsr round
	jsr ayint
	ldy #0
	lda facmo
	sta (forpnt),y
	iny
	lda faclo
	sta (forpnt),y
	rts

copflt:	jmp movvf

copstr:	pla
	jmp inpcom
; -------------------------------------------------------------------------------------------------
; RESUME
resume:	jsr errdir		; no direct mode
	ldx errlin+1		; is there an error to resume fron?
	inx
	beq rescnt		; can't resume!
	jsr chrgot		; look for arguments
	beq resswp		; no arg's...restart err'd line
	bcc resnum		; numeric argument
	cmp #tknext		; only other choice is 'next'
gooos:	bne ressnr		; if not, syntax error

	jsr resswp		; resume execution with next stm't
	ldy #0
	lda (txtptr),y
	bne resum2 		; must be a ':'
	iny			; must be a null,get next line
	jsr sav42
	jsr addon
resum2:	jsr chrget		; skip over this character, into body of statement
	jmp data		; advance until null or ':', then rts

resnum:	jsr getpin		; get address
	sta linnum+1
	jsr resend
	jmp luk4it

resswp:	ldx #1
resum0:	lda errlin,x
	sta curlin,x
	lda errtxt,x
	sta txtptr,x
	dex
	bpl resum0
resend:	ldx #$ff
	stx xcnt
	stx errlin
	stx errlin+1		; flag 'no further resumes until next error'
	ldx tmptrp		; restore trap line to allow traps again
	stx trapno+1
	rts

rescnt:	ldx #errcr
	jmp error
; -------------------------------------------------------------------------------------------------
; dispose

dispos:	cmp #tkfor
	beq dispo0
	cmp #tkgosu
	bne gooos 		; not next or gosub is syntax error
dispo0:	sta oldtok
	jsr errdir
	tsx
	inx
	inx  			; look past return to newstt
	txa
	tay
dispo1:	tya			; y==>x
	tax
	cpy #$ff		; stack empty?
	beq diserr		; yes...
	lda stack+1,x
	cmp #tkfor
	bne dispo2
	txa 			; stack entry is a for loop, 19 bytes.
	adc #18			; + 1 in carry
	jmp dispo3

dispo2:	txa			; stack entry is gosub, 5 bytes.
	clc
	adc #7

dispo3:	bcs diserr		; stack underflow...
	tay
	lda stack+1,x
	cmp oldtok
	bne dispo1		; keep looking
	beq dispo5
dispo4:	ldx dsptmp
dispo5:	lda stack,x 		; shift stack down
	sta stack,y
	dey
	dex
	stx dsptmp		; done when x=s
	tsx
	cpx dsptmp
	bne dispo4
	tya			; s <== y
	tax
	txs
	jmp chrget		; advance past modifier, do rts
diserr:	ldx #errdi
	jmp error
.end

; ****************************************** BVERBS2 **********************************************
; PRINT# - process print# verb

printn:	jsr cmd			; docmd
	jmp iodone		; release channel
; -------------------------------------------------------------------------------------------------
; CMD - process cmd verb

cmd:	jsr getbyt
	beq cmnd2		; if no more
	jsr chkcom		; comma after channel#
cmnd2:	php			; save status
	pha
	stx dolu		;  x=la
	jsr patch1		; check and open output chn*****clr dsk status********
	sta channl		; set chn returned from chkout
	pla
	plp			; get status back
	jmp print		; process reset of print stm

strdon:	jsr strprt
newchr:	jsr chrgot		; reget last char
; -------------------------------------------------------------------------------------------------
; PRINT - process print verb

print:	beq ocrlf		; terminator so type cr/lf
; here after seeing tab(x) or , or in which ase a terminator does not mean type a cr/lf but just rts.
printc:	beq ocrlfx
	cmp #tktab
	beq taber		; if tab function
	cmp #tkspc
	clc
	beq taber		; if space
	cmp #','
	beq comprt		; if comma
	cmp #';'
	beq notabr		; if semicolon
	jsr frmevl		; evaluate the formula
	bit valtyp
	bmi strdon		; if string
	jsr outfac
	jsr ospc		; output a space
	bne newchr		; always goes

; entry to process carriage return and optional line feed
ocrlf:	lda #cr
	jsr ochr
	lda dolu
	bpl crfin		; cr only
	lda #lf			; line feed
	jsr ochr
crfin:	eor #255
ocrlfx:	rts

comprt:	sec			; set flag to read cursor position
	jsr plot		; get cursor position
	tya
	sec
morco1:	sbc #clmwid
	bcs morco1
	eor #255
	adc #1
	bne aspac		; always

taber:	php			; spc or tab function
	sec
	jsr plot
	sty trmpos
	jsr gtbytc
	cmp #')'
	bne snerr2
	plp
	bcc xspac
	txa
	sbc trmpos
	bcc notabr
aspac:	tax
xspac:	inx
xspac2:	dex
	beq notabr		; if spacing complete
	jsr ospc
	bne xspac2

notabr:	jsr chrget
	jmp printc

snerr2:	jmp snerr		; -> syntax error, ready
; -------------------------------------------------------------------------------------------------
; GET command
;   get from keyboard or get# from channel.
;   the value of an ascii 0 for a string value is the null string.  the value of an ascii 0 for
;   a numeric value is also 0.  get is not allowed in direct mode.

get:	jsr errdir		; not allowed in direct mode
	jsr chrgot		; reget first char
	cmp #'#'		; was it a get# ?
	bne get010		; no...get from keyboard
	jsr gtbytc		; value for channel#...returned in .x
	jsr chkcom		; must be comma before variable list
	jsr oldclr		; clear disk status for inputn & get
	jsr chkin
	sta channl		; new active channel

; this routine puts a zero as the 2nd character in the buffer and points (x,y) at it.
; this forces a call to getin by inloop the first time and also acts as a terminator.
get010:	ldy buffpt+1 		; buffpt points to buffer
	ldx buffpt
	inx			; increment to point to 2nd element
	bne get020
	iny
get020:	stx index1		; use this as index
	sty index1+1
	ldy #0
	tya			; 0 in .a
	sta (index1),y		; to make buffer terminator
	ldy index1+1		; pass pointer in (.x,.y)
	lda #64			; inpflg will be get
	jsr inpco1		; get and fill variable list...
	ldx channl		; get from keyboard?
	bne iorele		; no...close channel
	rts			; yes...done
; -------------------------------------------------------------------------------------------------
; INPUT#
inputn:	jsr getbyt		; must have channel #
	jsr chkcom		; comma after channel #
	jsr oldclr		; clear disk status for inputn & get
	jsr chkin
	sta channl
	jsr notqti
; close current working channel
iodone:	lda channl
iorele:	jsr clrch
	ldx #0
	stx channl
	stx dolu
	rts
; -------------------------------------------------------------------------------------------------
; INPUT

input:	cmp #$22 ; "		; prompt string?
	bne notqti		; no...
	jsr strtxt		; yes...print it out
	lda #';'
	jsr synchr		; semi-c as delimeter
	jsr strprt
notqti:	jsr errdir

getagn:	jsr qinlin
	lda channl
	beq bufful
	jsr readst		; get i/o status byte
	and #3			; ***stop for timeouts***
	beq bufful		; good input
	jsr iodone		; bad input
	jmp data		; skip rest of this statement

bufful:	ldy #0
	lda (index1),y		; anything in buffer?
	beq buffu2

; input ready to process
; point at comma before buffer
buffu1:	jsr sav75
	jmp inpcon

; buffer was empty
; if from device, try again if not eoi
; otherwise default all variables
buffu2:	lda channl
	bne *+5
	jmp data		; from keyboard, skip rest of stmt
	jsr readst		; from device, check for eoi
	and #$40
	beq getagn		; no eoi, was only a cr
	bne buffu1		; eoi, fill with null vals

qinlin:	lda channl 		; keyboard/crt?
	bne ginlin		; no...
	jsr outqst		; yes..prompt
	jsr ospc
ginlin:	jmp inlin
; -------------------------------------------------------------------------------------------------
; find data
;   search is made by using the execution code for data	to skip over statments.  the start word of
;   each statement is compared with 'datatk'. each new line number is stored in 'datlin' so that if
;   an error occurs while reading data the error msg can give the line # of the ill-formatted data.

datlop:	jsr datan
	iny
	tax
	bne nowlin
	lda (txtptr),y		; look for null links
	bne dtlp0
	iny
	lda (txtptr),y
	bne dtlp1		; if no error
	ldx #errod
	jmp error

dtlp0:	iny
dtlp1:	iny			; point stored stmt to line no.
	lda (txtptr),y
	sta datlin
	iny
	lda (txtptr),y
	iny
	sta datlin+1

nowlin:	jsr addon
	jsr chrgot
	tax
	cpx #tkdata
	bne datlop
	beq datbk1
; -------------------------------------------------------------------------------------------------
; READ
;   in the processing of data and read statements
;   one -> points to the data (ie, the #s being fetched)
;   and another points to the list of variables.

;   the -> into the data always starts pointing to a terminator -- a ,  or eol
;   at this point txtptr points to listo of variables and (y,x) points to data or input line.

;   note, since both data statements and the input buffer are in the text bank, the bank for
;   indirection and the text pointer, txtptr, is always text.

read:	ldx datptr
	ldy datptr+1
	lda #$98
	!byte $2c		; eat the next two bytes
inpcon:	lda #0
inpco1:	sta inpflg
	stx inpptr
	sty inpptr+1
inloop:	jsr sav73
	lda txtptr
	ldy txtptr+1
	sta vartxt
	sty vartxt+1
	ldx inpptr
	ldy inpptr+1
	stx txtptr
	sty txtptr+1
	jsr chrgot
	bne datbk1
	bit inpflg
	bvc qdata
	jsr getin
	jsr sav75
	stx index1
	sty index1+1
	ldy #1
	sta (index1),y
	ldy index1+1
	jmp datbk
qdata:	bmi datlop
	lda channl
	bne getnth
	jsr outqst
getnth:	jsr qinlin
	tax
datbk:	stx txtptr
	sty txtptr+1
datbk1:	jsr chrget
	bit valtyp
	bpl numins
	bit inpflg
	bvc setqut
	inx
	stx txtptr
	lda #0
	sta charac
	beq resetc
setqut:	sta charac
	cmp #$22 ; "
	beq nowget
	lda #':'
	sta charac
	lda #','
resetc:	clc
nowget:	sta endchr
	jsr sav30
	jsr strlt2
	jsr st2txt
	jsr inpcom
	jmp strdn2
sav30:	lda txtptr
	ldy txtptr+1
	adc #0
	bcc nowge1
	iny
nowge1:	rts

numins:	jsr fin
	lda intflg
	jsr qintgr
strdn2:	jsr chrgot
	beq trmok
	cmp #','
	bne trmnok

trmok:	lda txtptr
	ldy txtptr+1
	sta inpptr
	sty inpptr+1
	lda vartxt
	ldy vartxt+1
	sta txtptr
	sty txtptr+1
	jsr chrgot
	beq endvar
	jsr chkcom
	jmp inloop

; here when the data that was typed in or in 'data' statements is improperly formatted.
; for 'input' we start again.
; for 'read' we give a syntax errort the data line.
trmnok:	lda inpflg
	beq trmno1		; if input try again
	bpl snerr6		; get stmt, issue syntax err
;	bmi getdtl
;	ldy #255		; make it look direct
;	bne stcurl		; always goes
getdtl:	lda datlin		; get data line number
	ldy datlin+1
stcurl:	sta curlin		; make it current line
	sty curlin+1
snerr6:	jmp snerr		; -> syntax error, ready

trmno1:	lda channl		; if not terminal, give bad data
	beq doagin
	ldx #errbd
	jmp error

doagin:	ldx #tryagn
	jsr msg			; print '?redo from start'
	lda oldtxt		; -> at start of this current line
	ldy oldtxt+1
	sta txtptr
	sty txtptr+1
	rts			; go to newstt

endvar:	lda inpptr
	ldy inpptr+1
	ldx inpflg
	bpl vary0
	sta datptr
	sty datptr+1
inprts:	rts

vary0:	ldy #0
	lda (inpptr),y
	beq inprts
	lda channl
	bne inprts
	ldx #exignt
	jmp msg
; -------------------------------------------------------------------------------------------------
; SYS command

csys:	jsr getpin		; get positive integer
	lda via2+prb		; get MMU reg
	and #%00001111		; isolate RAM0 bank
	pha			; save
	lda #%11110000		; clear RAM0 bank bits# 0-3
	and via2+prb
	ora dfbank		
	sta via2+prb 		; set RAM0 bank
	lda #>csysrz		; push return address
	pha
	lda #<csysrz
	pha
	jmp (poker)
csysrz=*-1
	pla
	sta tttemp
	lda #%11110000		; clear RAM0 bank bits# 0-3
	and via2+prb
	ora tttemp
	sta via2+prb		; restore RAM0 bank
	rts
; -------------------------------------------------------------------------------------------------
; DIM
dim3:	jsr chkcom

dim:	tax
	jsr ptrgt1
	jsr chrgot
	bne dim3
	rts
; -------------------------------------------------------------------------------------------------
; DEFFN

defn:	jsr getfnm
	jsr errdir
	jsr chkopn
	lda #128
	sta subflg
	jsr ptrget
	jsr chknum
	jsr chkcls
	lda #tkequl		; must have equal token
	jsr synchr
	lda varpnt+2
	pha
	lda varpnt+1
	pha
	lda varpnt
	pha
	lda txtptr+1
	pha
	lda txtptr
	pha
	jsr data
	jmp deffin
; -------------------------------------------------------------------------------------------------
; POKE

poke:	jsr getnum
	lda via2+prb		; get MMU reg
	pha			; save
	and #%11110000		; clear RAM0 bank bits# 0-3
	ora dfbank		
	sta via2+prb 		; set default RAM0 bank
	ldy #0
	txa
	sta (poker),y
	pla
	sta via2+prb		; restore RAM0 bank
	rts
; -------------------------------------------------------------------------------------------------
; WAIT

fnwait:	jsr getnum
	stx andmsk
	ldx #0
	jsr chrgot
	beq stordo
	jsr combyt
stordo:	stx eormsk
	lda via2+prb		; get MMU reg
	pha			; save
	and #%11110000		; clear RAM0 bank bits# 0-3
	ora dfbank		
	sta via2+prb 		; set default RAM0 bank
	ldy #0
waiter:	lda (poker),y
	eor eormsk
	and andmsk
	beq waiter
	pla
	sta via2+prb		; restore RAM0 bank
	rts
; -------------------------------------------------------------------------------------------------
; KEY processor:   key [ key number , string ]

fkey:	bne key20
	ldy #0			; no params => list all keys
	beq keygo		; always

key20:	jsr getbyt		; .x := positive byte integer
	txa
	bne key40		; must be non-zero
fcerr3:	jmp fcerr		; -> illegal quantity error, ready

key40:	cmp #21
	bcs fcerr3		; must be <= 20
	pha			; save key number
	jsr chkcom
	jsr sav13
	sta highds		; set up string descriptor
	lda index1
	ldx index1+1
	ldy index1+2
	sta highds+1
	stx highds+2
	sty highds+3
	pla
	tay			; .y := key number
	lda #<highds		; note hightr must follow highds

keygo:	jsr pgmkey		; do it!
	bcc bakrts		; if no problem
	ldx #52			; out of memory
	jmp (ierror)
bakrts:	rts
sav13:	jsr frmevl
	jmp frestr
; -------------------------------------------------------------------------------------------------
; VERIFY - process verify verb

cverf:	lda #errvr		; set message table pointer
	pha
	lda #$80		; set verify flag / basic bank 0
	jsr ldver		; load/verify routine
	beq cvf1		; if direct mode
	pla
	rts
; -------------------------------------------------------------------------------------------------
; LOAD - process load verb

cload:	lda #0			; set load flag / basic bank 0
	jsr ldver		; load/verify routine

; entry from dload
loadck:	beq clf1		; if direct mode
	jsr lnkprg		; relink
	jsr stxtpt		; point txtptr to null byte
	jmp fload		; set stack pointer

clf1:	stx vartab		; save new text end
	sty vartab+1
	lda #errld		; set message table pointer
	pha

cvf1:	jsr readst		; read status of load/verify
	and #$10
	bne cld2		; if error
	pla
	lda #msrdy
	pha

cld2:	pla
	tax

cld3:	jsr msg			; output message
	jmp fini		; relink, set end of text and goto main loop

cld4:
	ldx #errom		; 64k	**** PATCHED by Vossi - CBM had here absolute instead immediate! *** 
	bne cld3		; always

;   entry from dload
loadnp:	pha			; a = 0
	beq lvr1		; always

;   entry a = 0 if no verify, <>0 if verify
ldver:	pha			; save load/verify flag
	jsr plsv		; verify entry

lvr1:	jsr kstop
	beq lvr1		; debounce

	pla			; restore flag
	ldx txttab		; load into text table
	ldy txttab+1
	jsr load		; request load from kernal
	bcs ldsver		; if load/verify error
	jmp tdm        		; get direct mode
; -------------------------------------------------------------------------------------------------
; SAVE - process basic save command.

csave:	jsr plsv

savenp:	ldx txttab		; highds is start vector
	ldy txttab+1
	lda #0			; basic bank
	stx highds
	sty highds+1
	ldx vartab
	ldy vartab+1		; hightr is end vector
savenb:	stx hightr
	sty hightr+1
	ldx #<highds
	ldy #<hightr
	jsr save
	bcs ldsver		; load/save error
	rts

ldsver:	asl			; double error number
	tax			; transfer to x
	jmp error		; process error
; -------------------------------------------------------------------------------------------------
; OPEN - process open verb

copen:	ldx #00			; set default parameters
	stx parsts		; clear status
	stx dosf1l		; set length to zero
	stx dossa		; set secondary address to zero
	inx
	stx dosfa		; set device number to one
	jsr plsv32
	jsr getbyt
	stx dosla		; set logical file number
	jsr cops		; get next number if one
	stx dosfa		; save device number
	ldy #0
	cpx #3
	bcc cop1		; if not iec
	dey

cop1:	sty dossa		; set default secondary address
	jsr cops		; get next number if one
	stx dossa		; set secondary address
	jsr chrgot
	beq copx		; if end of statement
	jsr plsv30
	jsr sav77

copx:	jsr plsvx		; set file info

globtt:	jsr clrch		; unlisten
	jsr open
	jmp dcat0		; restore listener,if any

cops:	jsr chrgot
	bne copg
	pla			; if end of statement, scrap return...
	pla			; ...and go off to open it
	jmp copx
copg:	jsr plsv30		; else get next device param and continue
	jmp getbyt
; -------------------------------------------------------------------------------------------------
; CLOSE - process close verb

cclos:	jsr plsv32
	jsr getbyt
	txa
	jmp close
; ****************************************** BVERBS3 **********************************************
; 'basic/dos interface verbs.'
; this set of routines takes tokens and values after the following basic keywords

; dopen,  dclose, record, format, collect, backup, copy
; concat, dsave,  dload,  catlog, rename,  append, scrtch

; it then parses the following line and finds syntax errors, checks for out of range values, and sets
; variables in the zero-page to be passed to the disk message generator (dmg).

; CATALOG-DIRECTORY - catalog a device (==directory)

dcat:	jsr dospar		; parse the line
	lda parsts		; check options
	and #$e6
	beq *+5
	jmp snerr		; -> syntax error, ready
	ldy #tcat		; table offset
	ldx #1			; just $
	lda parsts		; chk for default
	and #$11		; no drive?
	beq dcat2
	lsr
	bcc dcat1		; just drive
	inx			; drive and filename
	inx
dcat1:	inx
dcat2:	txa			; a now has length
	jsr sendp		; build
	lda #dosctl
	sta xcnt		; max lines before prompt
	ldy #$60		; sa, load floppy
	jsr ochanl		; open special channel
	ldy #3			; loop counter
; get length in blocks
dcat3:	sty t3			; save counter
	ldx #doslfn
	jsr chkin		; open for input
	jsr basin		; get char
	sta t4
	jsr readst
	bne dcat10		; if bad status
	jsr basin		; get char
	sta t4+1
	jsr readst
	bne dcat10		; if bad status
	ldy t3
	dey
	bne dcat3		; if not done
; output blocks number
	jsr dcat0		; clear channel  restore output device
	ldx t4
	lda t4+1
	jsr linprt		; output number
	lda #' '
	jsr bsout		; output a space
; loop reading name and output
dcat4:	jsr clrch 		; clear channel  default input is disk
	ldx #doslfn
	jsr chkin
	jsr basin		; get char
	pha			; save char
	jsr readst		; get status
	bne dcat9		; if bad status
	jsr dcat0 		; clear channel  restore output device
	pla			; get back char
	beq dcat5		; if eol
	jsr bsout		; echo char
	bcc dcat4		; continue to process name

; here on end of name
dcat5:	dec xcnt		; decrement max lines counter
	lda #cr
	jsr bsout		; output new line
	jsr clrch		; close
; check for suspend or halt
	jsr kstop		; get status of stop key
	beq dcat10		; if stop request
	lda xcnt		; lines
	bne dcat8		; if not max lines
	lda #dosctl
	sta xcnt		; reset counter
	jsr tstdir    		; don't prompt unless in direct mode
	bne dcat8
	ldx channl    		; or if not writing to screen
	cpx #0
	bne dcat8
	ldx #mremsg
	jsr msg			; prompt user
; wait for go
dcat7:	jsr getin		; suspend waiting for key
	beq dcat7		; if no key - suspend
; process next
dcat8:	ldy #2			; perform 2 times
	bne dcat3		; jmp

dcat9:	pla			; clean up stk
dcat10:	jsr clrch
	lda #doslfn
	jsr close		; close special channel
dcat0:	jsr clrch
	ldx dolu
	beq goorat
	jmp chkout
; -------------------------------------------------------------------------------------------------
; open channel 14 for special purposes
;   enter:  .y=sa
;   doslfn = lu
;   dosfa = fa
;   assumes setnam previously called
ochanl:	ldx dosfa
	bne ochl10
	ldx #8			; use default of 8
ochl10:	lda #doslfn		; lfn
	jsr setlfs		; set file parameters
	jsr clrch		; unlisten default out
	jmp open		; open it...
; -------------------------------------------------------------------------------------------------
; DOPEN code dfn(,t(,r))

dopen:	lda #$22		; set error flag
	jsr sav9
	ldy #topn		; fcb format pointer
	ldx #4			; normal length
	bit parsts		; relative record
	bvc dop2		; if not random access
	ldx #8			; random access length
	bne dop2		; alway jump
; -------------------------------------------------------------------------------------------------
; APPEND code

append:	lda #$e2		; set error flags
	jsr sav9		; chk req'd parms
	ldy #tapn		; tabld index
	ldx #5			; length

dop2:	txa			; set length into a
	jsr sendp
	jmp globtt

sav9	jsr dosprs		; parse the line
	jsr chk6		; chk req'd parms
;  find an available secondary address
fndsca:	ldy #$61
fsca10:	iny
	cpy #$6f
	beq fsca20		; if none available
	jsr lkupsa		; get physical unit from secondary
	bcc fsca10		; if secondary address used
	sty dossa		; save secondary address

goorat:	rts			; return .y = sa

fsca20:	ldx #ms1		; too many files open
	jmp error
; -------------------------------------------------------------------------------------------------
; DCLOSE - close disk file

dclose:	lda #$f3		; set error flags
	jsr dosprs		; parse the line
	jsr oldclr
	lda parsts		; any la given?
	and #$04
	beq dclall		; no....
	lda dosla
	jmp close		; close file
; -------------------------------------------------------------------------------------------------
; dclall

dclall:	lda dosfa		; get disk #
	jmp patch3		; close all units*****device in .a only******
; -------------------------------------------------------------------------------------------------
; DSAVE dfn

dsave:	lda #$66		; set error flags
	jsr dosprs		; parse the line
	jsr sav20
	jmp savenp
; -------------------------------------------------------------------------------------------------
; DLOAD dfn

dload:	lda #$e6		; set error flags
	jsr dosprs		; parse the line
	jsr sav20
	lda #0			; set load flag
	jsr loadnp		; perform load no parse
	jmp loadck		; go to load checking
; -------------------------------------------------------------------------------------------------
; BANK dfn

chbank:	jsr getbyt
	cpx #mxbank		; too large?
	bcs xbkerr
	stx dfbank
bsvrts:	rts
xbkerr:	jmp    fcerr		; illegal quantity
; -------------------------------------------------------------------------------------------------
; BSAVE dfn

bsave:	lda #$66		; std error flag
	ldx #$f8		; auxiliary error flag
	jsr dosprx		; parse options
	jsr sav20		; check req params done

	lda dosbnk		; bank
	ldx dosofl		; start addr
	ldy dosofl+1
	stx highds
	sty highds+1
	ldx dosofh		; end addr
	ldy dosofh+1
	jmp savenb
; -------------------------------------------------------------------------------------------------
; BLOAD dfn

bload:	lda #$e6		; std error flag
	ldx #$fc		; aux error flag
	jsr dosprx		; parse options
	jsr sav20

	lda dosbnk		; bit7 is clear(=>load)
	clc
	ldx dosofl		; start addr
	ldy dosofl+1
	jsr load
	bcc bsvrts		; done
; -------------------------------------------------------------------------------------------------
; HEADER nddn (,id)

format:	jsr dospar		; parse the line
	jsr chk1		; check parameter errors
	and #$11
	cmp #$11
	bne rec5 		; if required parameters not present

frmt2:	jsr dclall		; close all files
	jsr rusure		; r-u-sure prpt
	bcs bsvrts 		; if no and direct mode
	ldy #thed		; tabld index
	lda #4			; length
	ldx dosdid		; check for diskid
	beq frmt6
	lda #6			; length with id
frmt6:	jsr trans			; build and send

	jsr errchl		; get error status
	jsr tdm			; test direct mode
	bne bsvrts 		; if direct mode

	ldy #0
	lda (dsdesc+1),y
	cmp #'2'
	bcs frmt8		; if error occured
	rts

frmt8:	ldx #msg31		; ? bad disk
	jmp error
; -------------------------------------------------------------------------------------------------
; SCRATCH dfn

scratc:	jsr dospar		; parse the line
	jsr chk1
	jsr rusure		; check direct and ask user
	bcs zxit		; if no and direct mode
	ldy #tscr		; offset
	lda #4			; length
	jsr trans
	jsr errchl		; read error channel
	jsr tdm			; test direct mode
	bne zxit		; if direct mode
	lda #cr
	jsr bsout		; output cr
	ldy #0			; clr to read errchl
sctc1:	lda (dsdesc+1),y	; get mesg
	beq sctc2		; if end of error message
	jsr bsout
	iny
	bne sctc1		; always

sctc2:	lda #cr
	jsr bsout		; output cr
zxit:	rts

rec5:	jmp snerr		; -> syntax error, ready
; -------------------------------------------------------------------------------------------------
; RECORD - random record access.

record:	lda #01			; default pos = 1
	sta dosrcl
	jsr chrgot
	lda #'#'
	jsr synchr		; next character = "#"
	jsr gtvl2		; get next value
	cpx #0
	beq rec4		; cannot be zero
	stx dosla		; save logical address
	jsr chkcom		; check for ","
	beq rec5		; if end of statement
	bcc rec1		; if numeric
	jsr chkopn		; check for "("
	jsr getpin		; get positive integer
	jsr chkcls
	jmp rec2

rec4:	jmp qtyerr

rec6:	ldx #ms3		; file not found err
	jmp error

rec1:	jsr getpin		; get positive integer

rec2:	jsr chrgot		; see what is next
	beq rec3		; if end of statement
	jsr chkcom		; check for ","
	beq rec5		; if end of statement
	jsr gtvl2
	cpx #0
	beq rec4		; if out of range
	cpx #$ff
	beq rec4		; if out of range
	stx dosrcl		; save byte position (pos)
	jsr chrgot
gaaa:	bne rec5		; if not end of statement

rec3:	lda dosla		; get logical address
	jsr lkupla		; logical to physical map
	bcs rec6		; if file not found
	sty dossa		; save secondary address
	stx dosfa
	jsr setlfs		; set up la, fa
	jsr oldclr		; clear disk status
	ldy #trec		; set pointer
	lda #4			; process five bytes
	bne trans		; transfer on channel 15
; -------------------------------------------------------------------------------------------------
; 950a DCLEAR

dclear:	jsr dospar		; parse the line
	ldy #tclr		; set code
	lda #2
	bne trans
; -------------------------------------------------------------------------------------------------
; COLLECT v<drive#}

colect:	jsr dospar		; parse the line
	jsr chk3		; chk opt parms
	jsr dclall		; close all files
	ldy #tcoll		; tabld offset
	ldx #1			; length
	lda parsts
	and #$10
	beq clct2
	inx			; include drive
clct2:	txa			; place in a
	bne trans		; always (x <> 0)
; -------------------------------------------------------------------------------------------------
; COPY - copy routines cdddfn=sdsfn

dcopy:	jsr dospar		; parse the line
	and #$30
	cmp #$30		; chk req'd parms
	bne dcpy2
	lda parsts
	and #$c7
	beq dcpy4
dcpy2:	lda parsts
	jsr chk4
	lda parsts
dcpy4:	ldy #tcopy		; tabld offset
	lda #8			; length
	bne trans		; go do it
; -------------------------------------------------------------------------------------------------
; CONCAT routines

concat:	jsr dospar		; parse the line
	jsr chk4
	ldy #tconc		; offset
	lda #12			; length
	bne trans		; go do it
; -------------------------------------------------------------------------------------------------
; RENAME rdddfn=sdsfn

rename:	lda #$e4		; set error flags
	jsr dosprs		; parse the line
	jsr chk5
	ldy #tren		; offset
	lda #8			; length
	bne trans		; go do it
; -------------------------------------------------------------------------------------------------
; BACKUP d<dd>=<sd}

backup:	lda #$c7		; set error flags
	jsr dosprs		; parse the line
	and #$30		; req'd parms
	cmp #$30
	bne gaaa		; if syntax error
	jsr rusure		; test for direct mode and send message
	bcc bup1  		; if run mode or not 'yes'
	rts

bup1:	jsr dclall		; close disk
	ldy #tbak
	lda #4			; length
; trans subroutine
trans:	jsr sendp		; build string to output
	jsr clrch
	sec
	jsr opn10 		; send it...
	jmp dcat0

; ****************************************** MATH1 ************************************************
; 'math cracking routines.'

forfix:               		; empty space for future fixes
	!byte $22		; check sum byte

patch1:	jsr oldclr		; clear disk status for printn & cmd
	jmp chkout


patch3:	sec			; close all files associated with device in .a
	jmp kclall

*=*+$2b
; -------------------------------------------------------------------------------------------------
; 95c1 frmevl
;   the formula evaluator starts with (txtptr) -> to the 1st char of the formula.
;   at the end (txtptr) -> to the terminator. the result is left in the fac.
;   on return (a) does not reflect the terminator.
; 
;   the formula evaluator uses the operator list (optab= to determine precedence and dispatch
;   addresses for each operator. a tem result on the stk has the following format address of the
;   operator routine fp temp result precedence of the operator

frmevl:	jmp (ifrmev)

nfrmev:	ldx txtptr
	bne frmev1
	dec txtptr+1
frmev1:	dec txtptr
	ldx #0
	!byte $24

lpoper:	pha
	txa
	pha
	lda #1
	jsr getstk
	jsr eval
	lda #0
	sta opmask
tstop:	jsr chrgot

loprel:	sec
	sbc #tkgrea
	bcc endrel
	cmp #tkless-tkgrea+1
	bcs endrel
	cmp #1
	rol
	eor #1
	eor opmask
	cmp opmask
	bcc snerr5
	sta opmask
	jsr chrget
	jmp loprel

endrel:	ldx opmask
	bne finrel
	bcs qop
	adc #tkgrea-tkplus
	bcc qop
	adc valtyp
	bne *+5
	jmp cat

	adc #$ff
	sta index1
	asl
	adc index1
	tay 
qprec:	pla
	cmp optab,y
	bcs qchnum
	jsr chknum
doprec:	pha
negprc:	jsr dopre1
	pla
	ldy opptr
	bpl qprec1
	tax
	beq qopgo
	bne pulstk
finrel:	lsr valtyp
	txa
	rol
	ldx txtptr
	bne finre2
	dec txtptr+1
finre2:	dec txtptr
	ldy #ptdorl-optab
	sta opmask
	bne qprec
qprec1:	cmp optab,y
	bcs pulstk
	bcc doprec

dopre1:	lda optab+2,y
	pha
	lda optab+1,y
	pha
	jsr pushf1
	lda opmask
	jmp lpoper

snerr5:	jmp snerr		; -> syntax error, ready
; -------------------------------------------------------------------------------------------------
; pushf1

pushf1:	lda facsgn
	ldx optab,y
pushf:	tay
	pla
	sta index1
	pla
	sta index1+1
	tya
	pha
	inc index1		; fix 10/19/79
	bne forpsh		; 
	inc index1+1
forpsh:	jsr round
	lda faclo
	pha
	lda facmo
	pha
	lda facmoh
	pha
	lda facho
	pha
	lda facexp
	pha
	jmp (index1)

qop:	ldy #$ff
	pla
qopgo:	beq qoprts
qchnum:	cmp #100
	beq unpstk
	jsr chknum
unpstk:	sty opptr
pulstk:	pla
	lsr
	sta domask
	pla
	sta argexp
	pla
	sta argho
	pla
	sta argmoh
	pla
	sta argmo
	pla
	sta arglo
	pla
	sta argsgn
	eor facsgn
	sta arisgn
qoprts:	lda facexp
	rts
; -------------------------------------------------------------------------------------------------
; eval

eval:	jmp (ieval)

neval:	lda #0
	sta valtyp

eval0:	jsr chrget
	bcs eval2
eval1:	jmp fin

eval22:	cmp #pi
	bne qdot
	lda #<pival
	ldy #>pival
	jsr movfm
	jmp chrget

pival:	!byte $82,$49,$0f,$da,$a1	; constant pi

; non numeric
eval2:	jsr isletc
	bcc eval22
;	bcs isvar			; isvar branch replaced with
	jmp isvar			; ..jmp isvar (too far for branch)

qdot:	cmp #'.'
	beq eval1
	cmp #tkminu
	beq domin
	cmp #tkplus
	beq eval0
	cmp #34
	bne eval3
strtxt:	jsr sav30
	jsr strlit
	jmp st2txt

eval3:	cmp #tknot
	bne eval4
	ldy #nottab-optab
	bne gonprc
; not
notop:	jsr ayint
	lda faclo
	eor #$ff
	tay
	lda facmo
	eor #$ff
	jmp givayf
; fn
eval4:	cmp #tkfn
	bne eval5
	jmp fndoer
; math functions
eval5:	cmp #tkonef
	bcc parchk
	cmp #tkerrd		; cover err$ & instr$
	bcs eval6
	cmp #tkgo		; can't be reserved word past str ftns
	bcs snerr
eval6:	jmp NewIsfun		; VDP: check for VDP function befere execute basic 4+ function
;	jmp isfun		; execute function

domin:	ldy #negtab-optab
gonprc:	pla
	pla
	jmp negprc

parchk:	jsr chkopn		; check '(' ')'
	jsr frmevl

chkcls:	lda #')'		; check for ')'
	!byte $2c		; eat the next two bytes

chkopn:	lda #'('		; check for '('
	!byte $2c		; eat the next two bytes

chkcom:	lda #','		; check ','
; -------------------------------------------------------------------------------------------------
; synchr
;   looks at the current char to make sure it is the specific thing loaded into acca just before
;   the call to synchk. if not it calls the syntax error routine.
;   otherwise it eats the next char and returns
synchr:	sta tttemp
	ldy #0
	lda (txtptr),y
	cmp tttemp
	bne snerr		; if not =, then error
	jmp chrget		; eat char
; -------------------------------------------------------------------------------------------------
; new basic4+ isfun routine start to detect vdp function
NewIsfun:
	cmp #TKTEST			; TEST ?
	beq IsTest
	jmp isfun			; isfun

IsTest:	jsr chrget
	jsr parchk
	jsr VDPTest
	jmp sngflt			; sngflt
; -------------------------------------------------------------------------------------------------
; snerr - output syntax error
snerr:	ldx #errsn
	jmp error
; -------------------------------------------------------------------------------------------------
; isvar
;   isvar sets up the value of the variable expression in the floating accumulator. if it is an
;   integer,the value is floated; if it is a string,fac holds a pointer to the string's descriptor.

;   note, symbol table space for uninitialized variables is never allocated. for these and basic
;   reserved vars, "ptrget" returns a pointer to "zero", stored in basic's rom area. "isvar" checks
;   the var name and type for these special cases. if not a reserved var, "isvar" uses "zero" to
;   create a numeric zero or null string value.

isvar:	jsr ptrget

isvret:	sta facmo
	sty facmo+1
	stx facmo+2
	ldx varnam
	ldy varnam+1
	lda valtyp
	beq goo
	lda #0
	sta facov
; all special variables in RAM	- modded by vossi
; check ti$
	cpx #'t'
	bne isvds		; if not 't'
	cpy #$c9 		; 'i'+$80
	bne isvds		; if 't' but not 'ti$'
	jmp gettim		; get time
; check ds$
isvds:	cpx #'d'
	bne isvrts		; if not 'd'
	cpy #$d3  		; 's'+$80
	bne isvrts		; if not 'ds$'
	jsr chkds
	lda dsdesc+1
	ldy dsdesc+2
	ldx dsdesc+3
	jmp strlit

strrts:	lda memtop		; facmo<-addr of user "zero"
	sta facmo
	lda memtop+1
	sta facmo+1
isvrts:	rts

chkds:	lda dsdesc
	bne isvrts
	jmp errchl		; get status

goo:	bit intflg		; test sign
	bpl gooo
	ldy #0
	lda (facmo),y
	tax
	iny
	lda (facmo),y
	tay
	txa
	jmp givayf

gooo:	cpx #'s'
	bne qerlin		; if not 's'
	cpy #'t'
	bne qerlin		; if 's' but not 'st'
	jsr readst		; get i/o status byte
	jmp float

qerlin:	cpx #'e'
	bne qdsav
	cpy #'r'
	beq gnumer
	cpy #'l'
	bne qdsav
	lda errlin+1
	ldy errlin
	jmp nosflt		; print 2 bytes
gnumer:	lda errnum
	lsr			; divide by 2
	jmp float 		; print 1 byte

qdsav:	cpx #'d'
	bne gomovf		; if not 'd'
	cpy #'s'
	bne gomovf		; if 'd' but not 'ds'
	jsr chkds
	ldy #0
	lda (dsdesc+1),y
	and #$f
	asl
	sta garbfl
	asl
	asl
	adc garbfl
	sta garbfl
	iny
	lda (dsdesc+1),y
	and #$f
	adc garbfl
	jmp float

gomovf:	lda facmo		;  fac<-rom mem (uninit. flt var)
	ldy facmo+1
	jmp movfm
; -------------------------------------------------------------------------------------------------
; isfun

isfun:	asl
	pha
	tax
	jsr chrget
	cpx #tklasn+tklasn-$ff
	bcc oknorm

tkerd2=tkerrd+tkerrd
	cpx #<tkerd2
	beq errjmp
	jsr chkopn
	jsr frmevl
	jsr chkcom
	jsr chkstr
	pla

tkint2=tkinst+tkinst
	cmp #<tkint2
	beq insjmp
	tax
	lda facmo+2		;  push descr ptr for opnd1
	pha
	lda facmo+1
	pha
	lda facmo
	pha
	txa
	pha
	jsr getbyt		; opnd2
	pla
	tay
	txa
	pha
	jmp fingo

oknorm:	jsr parchk
	pla
	tay
fingo:	lda fundsp-tkonef-tkonef+256,y
	sta jmper+1

	lda fundsp-tkonef-tkonef+257,y
	sta jmper+2

	jsr jmper
	jmp chknum
; -------------------------------------------------------------------------------------------------
; OR

orop:	ldy #$ff
	!byte $2c		; eat the next two bytes
; AND
andop:	ldy #0
	sty eormsk
	jsr ayint
	lda facmo
	eor eormsk
	sta integr
	lda faclo
	eor eormsk
	sta integr+1
	jsr movfa
	jsr ayint
	lda faclo
	eor eormsk
	and integr+1
	eor eormsk
	tay
	lda facmo
	eor eormsk
	and integr
	eor eormsk
	jmp givayf

insjmp:	jmp instrg

errjmp:	jmp errd
; -------------------------------------------------------------------------------------------------
; = - perform relational operator
;   (domask) contains the bits as to which relational operator it was. cbit set = string compare.

dorel:	jsr chkval
	bcs strcmp
	lda argsgn		; conv arg to basic internal form
	ora #127
	and argho
	sta argho
	lda #<argexp
	ldy #>argexp
	jsr fcomp		;  arg is 1st opnd
	tax
	jmp qcomp

; string =
strcmp:	lda #0
	sta valtyp
	dec opmask
	jsr frefac		; free string2
	sta dsctmp		;  dsctmp <- length2,
	ldx #2

grrk:	lda index,x
	sta dsctmp+1,x
	dex
	bpl grrk
	lda argmo
	ldy argmo+1
	ldx argmo+2
	jsr fretmp		; free string1
	ldx index
	ldy index+1
	stx argmo		; argmo<-ptr to string1
	sty argmo+1
	ldy index+2
	sty argmo+2
	tax			; .x <- length1
	sec
	sbc dsctmp
	beq stasgn		;  length1=length2
	lda #1
	bcc stasgn		;  length1<length2
	ldx dsctmp
	lda #$ff
; .a = o(equal), 1(len1<len2), -1(len1>len2)
; .x = length of shorter string
stasgn:	sta facsgn
	ldy #$ff
	inx
nxtcmp:	iny			; .y=inx to nxt chr, .x=#chrs to go
	dex
	bne getcmp
	ldx facsgn
; come here when strings are equal, but
; .x =  1, len1<len2, cy=1
;  0, len1=len2, cy=0
; -1, len1>len2, cy=0
qcomp:	bmi docmp
	clc
	bcc docmp
getcmp:	lda (dsctmp+1),y	;  chr from string2
	sta tttemp
	lda (argmo),y		;  chr from string1
	cmp tttemp
	beq nxtcmp
	ldx #$ff
	bcs docmp
	ldx #1
docmp:	inx
	txa
	rol
	and domask
	beq goflot
	lda #$ff
goflot:	jmp float
; -------------------------------------------------------------------------------------------------
; ptrget

ptrget:	ldx #0
	jsr chrgot
ptrgt1:	stx dimflg
ptrgt2:	sta varnam
	jsr chrgot
	jsr isletc
	bcs ptrgt3
interr:	jmp snerr		; -> syntax error, ready

ptrgt3:	ldx #0
	stx valtyp
	stx intflg
	jsr chrget
	bcc issec
	jsr isletc
	bcc nosec
issec:	tax
eatem:	jsr chrget
	bcc eatem
	jsr isletc
	bcs eatem
nosec:	cmp #'$'
	bne notstr
	lda #$ff
	sta valtyp
	bne turnon
notstr:	cmp #'%'
	bne strnam
	lda subflg
	bne interr
	lda #$80
	sta intflg
	ora varnam
	sta varnam
turnon:	txa
	ora #$80
	tax
	jsr chrget
strnam:	stx varnam+1
	sec
	ora subflg
	sbc #40
	bne strna1
	jmp isary

strna1:	ldy #0
	sty subflg
	lda vartab
	ldx vartab+1
stxfnd:	stx lowtr+1
lopfnd:	sta lowtr		; reached end of simple vars?

	cpx arytab+1
	bne lopfn
	cmp arytab
	beq notfns
lopfn:	lda (lowtr),y
	cmp varnam
	bne notit
	iny
	lda (lowtr),y
	cmp varnam+1
	bne nxtptr
	jmp finptr

gobadv:	beq interr

nxtptr:	dey
notit:	clc
	lda lowtr
	adc #6+addprc
	bcc lopfnd
	inx
	bne stxfnd
isletc:	cmp #'a'
	bcc islrts
	sbc #$5b
	sec
	sbc #$a5
islrts:	rts

notfns:	pla
	pha

isvrm1=isvret-1
	cmp #<isvrm1
	bne notevl
	tsx
	lda stack+2,x
	cmp #>isvrm1
	bne notevl
ldzr:	lda #<fzero
	ldy #>fzero
	rts

notevl:	lda varnam
	ldy varnam+1
	cmp #'t'
	bne qstavr
	cpy #$c9   		; 'i'+$80
	beq ldzr
;	cpy #'i'		; disabled by CBM Basic4+ only TI$
;	bne qstavr
;	gobadv jmp snerr	; -> syntax error, ready

qstavr:	cmp #'s'
	bne qeravr
	cpy #'t'
	beq gobadv
qeravr:	cmp #'e'
	bne qdsvar
	cpy #'r'
	beq gobadv
	cpy #'l'
	beq gobadv

qdsvar:	cmp #'d'		; disk status?
	bne varok
	cpy #'s'
	beq gobadv		; cannot modify ds
	cpy #$d3  		; 's'+$80
	beq gobadv		; cannot modify ds$
varok:
	lda arytab
	ldy arytab+1
	sta lowtr
	sty lowtr+1
	lda strend
	ldy strend+1
	sta hightr
	sty hightr+1
	clc
	adc #6+addprc
	bcc noteve
	iny
noteve:	sta highds
	sty highds+1
	jsr bltu
	lda highds
	ldy highds+1
	iny
	sta arytab
	sty arytab+1
	sta arypnt		; set pointer to arrays
	sty arypnt+1
aryva2:	lda arypnt
	ldx arypnt+1
aryva3:	cpx strend+1		; end of arrays ?
	bne aryvgo
	cmp strend
	bne aryvgo
	jmp arydon		; finished...
aryvgo:	sta index1		; parse the array
	stx index1+1
	ldy #1-addprc
	lda (index1),y		; array name
	tax
	iny
	lda (index1),y		; name 2nd char
	php			; save status reg
	iny
	lda (index1),y		; point to next array
	adc arypnt
	sta arypnt
	iny
	lda (index1),y
	adc arypnt+1
	sta arypnt+1
	plp			; restore status
	bpl aryva2		; not a string type
	txa
	bmi aryva2		; not a string array
	iny			; ok we have a string array
	lda (index1),y
	ldy #$00
	asl
	adc #5
	adc index1
	sta index1
	bcc aryget
	inc index1+1
aryget:	ldx index1+1		; parse array
	cpx arypnt+1		; done with this array ?
	bne gogo
	cmp arypnt
	beq aryva3		; yes...
gogo:	ldy #$00		; process string pointer
	lda (index1),y
	beq dvarts		; no string...go on...
	sta tttemp
	iny
	lda (index1),y
	clc
	adc tttemp
	sta hightr
	tax			; save is x for check
	iny
	lda (index1),y
	adc #$00		; adjust high byte
	sta hightr+1
	iny
; fix backwards pointer by adding
; the move length to it
	clc
	ldy #$00
	lda (hightr),y
	adc #6+addprc
	sta (hightr),y
	iny
	lda (hightr),y
	adc #$00
	sta (hightr),y		; done with this str.
; fix the next string in the array
dvarts:	lda #strsiz
	clc
	adc index1
	sta index1
	bcc aryget
	inc index1+1
	bne aryget		; branch always

arydon:
	ldy #0
	lda varnam
	sta (lowtr),y
	iny
	lda varnam+1
	sta (lowtr),y
	lda #0
	iny
	sta (lowtr),y
	iny
	sta (lowtr),y
	iny
	sta (lowtr),y
	iny
	sta (lowtr),y
	iny
	sta (lowtr),y

finptr:	lda lowtr
	clc
	adc #2
	ldy lowtr+1
	bcc finnow
	iny
finnow:	sta varpnt
	sty varpnt+1
	rts

; ******************************************* MATH2 ***********************************************
; fmaptr

fmaptr:	lda count
	asl
	adc #5			; -> entries, c clr'd by asl
	adc lowtr
	ldy lowtr+1
	bcc jsrgm
	iny
jsrgm:	sta arypnt
	sty arypnt+1
	rts
; -------------------------------------------------------------------------------------------------
; intidx
;   read a formula from the current pos and turns it into a positive integer.
;   leaving the result in facmo & lo. negative arguments are not allowed.

intidx:	jsr chrget
	jsr frmevl
posint:	jsr chknum
	lda facsgn
	bmi nonono
ayint:	lda facexp
	cmp #$90
	bcc qintgo
	lda #<n32768
	ldy #>n32768
	jsr fcomp
nonono:	bne fcerr8
qintgo:	jmp qint

fcerr8:	jmp fcerr		; -> illegal quantity error, ready
; -------------------------------------------------------------------------------------------------
; isary
;   format of arrays in ram.
;   descriptor:
;   lb = 1st char
;   hb = 2nd char (200 bit = str flag)
;   length of array in ram in bytes
;   number of dims
;   for each dim starting with the 1st list (2 bytes each) of the max indice+1 the values.

isary:	lda dimflg
	ora intflg
	pha
	lda valtyp
	pha
	lda #0
	pha
indlop:	lda varnam+1
	pha
	lda varnam
	pha
	jsr intidx
	pla
	
	sta varnam
	pla
	sta varnam+1
	pla
	tay
	tsx
	lda stack+2,x
	pha
	lda stack+1,x
	pha
	lda indice
	sta stack+2,x
	lda indice+1
	sta stack+1,x
	
	iny
	tya
	pha
	jsr chrgot
	cmp #44
	beq indlop
	pla
	sta count
	jsr chkcls
	pla
	sta valtyp
	pla
	sta intflg
	and #127
	sta dimflg
	ldx arytab
	lda arytab+1
lopfda:	stx lowtr
	sta lowtr+1
	cmp strend+1		; 64k,128k
	cpx strend		; 64k,128k
	beq notfdd
lopfdv:	ldy #0
	lda (lowtr),y
	iny
	cmp varnam
	bne nmary1
	lda (lowtr),y
	cmp varnam+1
	beq gotary
nmary1:	iny
	lda (lowtr),y
	clc
	adc lowtr
	tax
	iny
	lda (lowtr),y
	adc lowtr+1
	
	bcc lopfda
; -------------------------------------------------------------------------------------------------
; bserr - bad subscript error

bserr:	ldx #errbs
	!byte $2c
; illegal quantity error
fcerr:	ldx #errfc
errgo3:	jmp error
; -------------------------------------------------------------------------------------------------
; gotary

gotary:	ldx #errdd
	lda dimflg
	bne errgo3
	jsr fmaptr
	ldy #4
	lda (lowtr),y
	cmp count
	bne bserr
	jmp getdef
sav44:	bpl sav45		; float: size=5
	dex
	lda varnam
	bpl sav45		; string:size=4
	dex
	dex			; int  : size=2
sav45:	rts
; -------------------------------------------------------------------------------------------------
; 9bd0 notfdd - here when var is not found in the array table
;   building an entry
;   put down the descriptor
;   setup # of dims
;   make sure there is room for the new entry
;   remember 'varpnt'
;   tally =4
;   skip 2 locs for later fill in of size
;   loop get an indice
;   put down #+1 and inc varptr
;   tally= tally*#+1
;   decrement #-dims
;   bne loop
;   call 'reason' with y,a reflecting last loc of var
;   update strend
;   zero all
;   make tally include maxdims and descriptor
;   put down tally
;   if called by dim, return
;   otherwise
;   index into the var as if it were found on the
;   initial search

notfdd:	jsr fmaptr
	jsr reason
	ldy #0
	sty curtol+1
	lda varnam		; stor name, calc elt size
	sta (lowtr),y
	iny
	ldx #5
	lda varnam+1
	sta (lowtr),y
	jsr sav44
	stx curtol
	lda count
	iny
	iny
	iny
	sta (lowtr),y		; store #dimensions

loppta:	ldx #11			; default upper bound for subscr
	lda #0
	bit dimflg
	bvc notdim
	pla			; not using default, get size from stack...
	clc
	adc #1			; ...and add 1 to it for limit
	tax
	pla
	adc #0
notdim:	iny
	sta (lowtr),y		; store upper bound (reverse order)
	iny
	txa
	sta (lowtr),y
	jsr umult		; increase current size
	stx curtol
	sta curtol+1
	ldy index		; restore index saved by umult
	dec count
	bne loppta		; any more dims to do?
	adc arypnt+1		; no, add total size of array to ptr
	bcs omerr1
	sta arypnt+1
	tay
	txa
	adc arypnt
	bcc grease
	iny
	beq omerr1

grease:	jsr reason		; 64k or 128k versions
	sta strend
	sty strend+1
	lda #0
	inc curtol+1
	ldy curtol
	beq deccur
zerita:	dey
	sta (arypnt),y
	bne zerita

deccur:	dec arypnt+1
	dec curtol+1
	bne zerita
	inc arypnt+1
	sec
	lda strend
	sbc lowtr
	ldy #2
	sta (lowtr),y
	lda strend+1
	iny
	sbc lowtr+1
	sta (lowtr),y
	lda dimflg
	bne dimrts
	iny

getdef:	lda (lowtr),y
	sta count
	lda #0
	sta curtol
inlpnm:	sta curtol+1
	iny
	pla
	tax
	sta indice
	pla
	sta indice+1
	lda (lowtr),y
	iny
	cmp indice+1
	beq inlpn0
	bcs inlpn1
bserr7:	jmp bserr
omerr1:	jmp omerr

inlpn0:	lda (lowtr),y
	cmp indice
	beq bserr7
	bcc bserr7

inlpn1:	lda curtol+1
	ora curtol
	clc
	beq addind
	jsr umult
	txa
	adc indice
	tax
	tya
	ldy index1
addind:	adc indice+1
	stx curtol
	dec count
	bne inlpnm
	sta curtol+1
	ldx #5			; maximum elt size is:
	lda varnam+1
	jsr sav44
	stx addend
	lda #0
	jsr umultd
	txa
	adc arypnt
	sta varpnt
	tya
	adc arypnt+1
	sta varpnt+1
	tay
	lda varpnt
dimrts:	rts
; -------------------------------------------------------------------------------------------------
; umult
;   two byte unsigned integer multiply.
;   for multiply dimensioned arrays.
;   x,y= x,a= curtol*lowtr,y,y+1

umult:	sty index
	lda (lowtr),y
	sta addend
	dey
	lda (lowtr),y
umultd:	sta addend+1
	lda #16
	sta deccnt
	ldx #0
	ldy #0
umultc:	txa
	asl
	tax
	tya
	rol
	tay
	bcs omerr1
	asl curtol
	rol curtol+1
	bcc umlcnt
	clc
	txa
	adc addend
	tax
	tya
	adc addend+1
	tay
	bcs omerr1
umlcnt:	dec deccnt
	bne umultc
	rts
; -------------------------------------------------------------------------------------------------
; FRE
;   fre(expr) returns the number of available bytes in the specified bank. if 'expr' is string,
;   then the bank is assumed to be the string bank. if 'expr' is numeric, it must be a positive,
;   byte value to be used as the bank#. if the selected bank cannot be used for basic programs,
;   a zero is returned. garbage collection is always done whenever the string bank is selected.

fre:	lda valtyp		; numeric or string param?
	beq fref10		; numeric
	jsr frefac		; string... free temporary
	jmp frefst

fref10:	jsr conint		; .x = parameter
	bne fref50
frefst:	jsr garba2		; compress out garbage strings
	sec
	lda fretop
	sbc strend
	tay
	lda fretop+1
	sbc strend+1
	jmp nosflt

fref50:	lda #0
	tay
; unsigned double byte integer to float
nosflt:	jsr stoint
	sec			; sign is positive
	jmp floatc
; -------------------------------------------------------------------------------------------------
; POS
; function pos(x)
pos:	sec
	jsr plot		; get current position

; signed single byte integer to float
sngflt:	lda #0			; msd is 0, lsd in .y

; signed double byte integer to float
givayf:	jsr stoint
	jmp floats
; -------------------------------------------------------------------------------------------------
; stoint
;   mov int to fac and compute proper exponents
stoint:	ldx #0
	stx valtyp		; type is integer
	sta facho		; msd
	sty facho+1		; lsd
	ldx #$90		; exponent
storts:	rts
; -------------------------------------------------------------------------------------------------
; note
;   only single arguments are allowed to functions and functions must be of the single line form
;   def fna(x)= x^2+x-2
;   no strings can be involved with these functions
; 
; idea
;   create a simple variable entry whose 1st char has the 200 bit set.
;   the value will be
;   a text ptr to the formula
;   a ptr to the argument variable
; 
;   function names can be like 'fna4'.

; see if we are in direct mode.
; and complain if so.

; errdir - test direct? -> illegal direct error
errdir:	jsr tstdir		; in direct mode?
	bne storts		; no...
	ldx #errid		; yes...'illegal direct'
	!byte $2c

; errguf - undefined function error
errguf:	ldx #erruf		; udf never defined
	jmp error
; -------------------------------------------------------------------------------------------------
; tstdir
;   test if execution is in direct mode
;   return z=1 if direct, z=0 otherwise

tstdir:	lda curlin+1
	cmp #$ff
	rts
tdm =tstdir
; -------------------------------------------------------------------------------------------------
; getfnm - get a ptr to a function name

getfnm:	lda #tkfn		;  must start with fn
	jsr synchr
	ora #$80		;  .a<-1st char of fn name+$80
	sta subflg		;  indicates parsing fn
	jsr ptrgt2
	sta defpnt		;  defpnt<-ptr to ftn
	sty defpnt+1
	stx defpnt+2
	jmp chknum
; -------------------------------------------------------------------------------------------------
; fndoer

fndoer:	jsr getfnm
	lda defpnt+2		; stack <- ptr to ftn (defpnt)
	pha
	lda defpnt+1
	pha
	lda defpnt
	pha
	jsr parchk		; compute actual param, check for parens
	jsr chknum
	pla			;  defpnt <- stack
	sta defpnt
	pla
	sta defpnt+1
	pla
	ldy #2			;  varpnt <- ptr for formal param
	lda (defpnt),y
	sta varpnt
	tax
	iny
	lda (defpnt),y
	sta varpnt+1
	ora varpnt		; null ptr => undef'd
	beq errguf
	iny
	lda (defpnt),y
fndo50:	lda (varpnt),y		;  push old value of formal param
	pha
	dey
	bpl fndo50
	ldy varpnt+1		; init formal param with fac
	lda varpnt+2
	jsr movumf
	lda txtptr+1		;  stack <- text ptr
	pha
	lda txtptr
	pha
	lda (defpnt),y		; text ptr <- ptr to ftn body text
	sta txtptr
	iny
	lda (defpnt),y
	sta txtptr+1
	lda varpnt+1
	pha
	lda varpnt
	pha
	jsr frmnum		;  compute the ftn body
	pla			;  defpnt <- ptr to formal
	sta defpnt
	pla
	sta defpnt+1
	jsr chrgot
	beq deffi		;  no multi-stmt ftn bodies
	jmp snerr		; -> syntax error, ready

deffi:	pla			;  restore text ptr
	sta txtptr
	pla
	sta txtptr+1

; when entered from defstf, deffin is used to restore the pre-function call value of the formal
; parameter (5 bytes).
; when entered from defn, deffin is used to initialize the ftn's symbol table entry with a ptr to
; the text for the ftn body (2 bytes) and a ptr to the data area of the formal parameter (3 bytes)
deffin:	ldy #0
	pla
	sta (defpnt),y
	pla
	iny
	sta (defpnt),y
	pla
	iny
	sta (defpnt),y
	pla
	iny
	sta (defpnt),y
	pla
	iny
	sta (defpnt),y
	rts
; -------------------------------------------------------------------------------------------------
; PEEK

peek:	lda poker+1
	pha
	lda poker
	pha
	jsr getadr
	lda via2+prb		; get MMU reg
	tax			; save in .x
	and #%11110000		; clear RAM0 bank bits# 0-3
	ora dfbank		
	sta via2+prb 		; set default RAM0 bank
	ldy #0
	lda (poker),y
	stx via2+prb		; restore RAM0 bank
	tay
	pla
	sta poker
	pla
	sta poker+1
	jmp sngflt
; -------------------------------------------------------------------------------------------------
; faddh

faddh:	lda #<fhalf
	ldy #>fhalf
	bne fadd

fsub:	jsr conupk
; -------------------------------------------------------------------------------------------------
; MINUS

fsubt:	jsr sav41
	eor argsgn
	sta arisgn
	lda facexp
	jmp faddt

sav75:	ldy buffpt+1
	ldx buffpt
	bne sav76
	dey

sav76:	dex
zerrts:	rts

fadd5:	jsr shiftr
	bcc fadd4		; always
fadd:	jsr conupk
; -------------------------------------------------------------------------------------------------
; PLUS

faddt:	bne faddb
	jmp movfa

faddb:	ldx facov
	stx oldov
	ldx #<argexp
	lda argexp
faddc:	tay
	beq zerrts
	sec
	sbc facexp
	beq fadd4
	bcc fadda
	sty facexp
	ldy argsgn
	sty facsgn
	eor #$ff
	adc #0
	ldy #0
	sty oldov
	ldx #<fac
	bne fadd1
fadda:	ldy #0
	sty facov
fadd1:	cmp #$f9
	bmi fadd5
	tay
	lda facov
	lsr zp+1,x
	jsr rolshf
fadd4:	bit arisgn
	bpl fadd2
	ldy #<facexp
	cpx #<argexp
	beq subit
	ldy #<argexp
subit:	sec
	eor #$ff
	adc oldov
	sta facov
	lda zp+3+addprc,y
	sbc zp+3+addprc,x
	sta faclo
	lda zp+2+addprc,y
	sbc zp+2+addprc,x
	sta facmo
	lda zp+2,y
	sbc zp+2,x
	sta facmoh
	lda zp+1,y
	sbc zp+1,x
	sta facho
fadflt:	bcs normal
	jsr negfac
normal:	ldy #0
	tya
	clc
norm3:	ldx facho
	bne norm1
	ldx facho+1
	stx facho
	ldx facmoh+1
	stx facmoh
	ldx facmo+1
	stx facmo
	ldx facov
	stx faclo
	sty facov
	adc #$08
	cmp #$18+addpr8
	bne norm3
zerofc:	lda #0
zerof1:	sta facexp
zeroml:	sta facsgn
	rts
; -------------------------------------------------------------------------------------------------
; fadd2

fadd2:	adc oldov
	sta facov
	lda faclo
	adc arglo
	sta faclo
	lda facmo
	adc argmo
	sta facmo
	lda facmoh
	adc argmoh
	sta facmoh
	lda facho
	adc argho
	sta facho
	jmp squeez

norm2:	adc #1
	asl facov
	rol faclo
	rol facmo
	rol facmoh
	rol facho
norm1:	bpl norm2
	sec
	sbc facexp
	bcs zerofc
	eor #$ff
	adc #1
	sta facexp
squeez:	bcc rndrts
rndshf:	inc facexp
	beq overr
	ror facho
	ror facmoh
	ror facmo
	ror faclo
	ror facov
rndrts:	rts
; -------------------------------------------------------------------------------------------------
; negfac

negfac:	jsr sav41

negfch:	lda facho
	eor #$ff
	sta facho
	lda facmoh
	eor #$ff
	sta facmoh
	lda facmo
	eor #$ff
	sta facmo
	lda faclo
	eor #$ff
	sta faclo
	lda facov
	eor #$ff
	sta facov
	inc facov
	bne incfrt
incfac:	inc faclo
	bne incfrt
	inc facmo
	bne incfrt
	inc facmoh
	bne incfrt
	inc facho
incfrt:	rts
; -------------------------------------------------------------------------------------------------
; overflow error

overr:	ldx #errov
	jmp error
; -------------------------------------------------------------------------------------------------
; mulshf

mulshf:	ldx #<(resho-1)
shftr2:	ldy zp+3+addprc,x
	sty facov
	ldy zp+3,x
	sty zp+4,x
	ldy zp+2,x
	sty zp+3,x
	ldy zp+1,x
	sty zp+2,x
	ldy bits
	sty zp+1,x
shiftr:	adc #$08
	bmi shftr2
	beq shftr2
	sbc #$08
	tay
	lda facov
	bcs shftrt
shftr3:	asl zp+1,x
	bcc shftr4
	inc zp+1,x
shftr4:	ror zp+1,x
	ror zp+1,x
rolshf:	ror zp+2,x
	ror zp+3,x
	ror zp+4,x
	ror
	iny
	bne shftr3
shftrt:	clc
	rts

; constants
fone:	!byte $81,0,0,0,0		; 1
logcn2:	!byte $03			; 4 log constants (polynom 3)
	!byte $7f,$5e,$56,$cb,$79	; 0.434255942
	!byte $80,$13,$9b,$0b,$64	; 0.576584541
	!byte $80,$76,$38,$93,$16	; 0.961800759
	!byte $82,$38,$aa,$3b,$20	; 2.885390007
sqr05:	!byte $80,$35,$04,$f3,$34	; 1/2 sqr 2
sqr20:	!byte $81,$35,$04,$f3,$34	; sqr 2
neghlf:	!byte $80,$80,0,0,0		; -0.5
log2:	!byte $80,$31,$72,$17,$f8	; log 2
; ******************************************* MATH3 ***********************************************
; LOG

log:	jsr sign
	beq logerr
	bpl log1
logerr:	jmp fcerr		; -> illegal quantity error, ready

log1:	lda facexp
	sbc #$7f
	pha
	lda #$80
	sta facexp
	lda #<sqr05
	ldy #>sqr05
	jsr fadd
	lda #<sqr20
	ldy #>sqr20
	jsr fdiv
	lda #<fone
	ldy #>fone
	jsr fsub
	lda #<logcn2
	ldy #>logcn2
	jsr polyx
	lda #<neghlf
	ldy #>neghlf
	jsr fadd
	pla
	jsr finlog
	lda #<log2
	ldy #>log2

; -------------------------------------------------------------------------------------------------
; MULTIPLY
fmult:	jsr conupk

fmultt:	beq multrt

	jsr muldiv
	lda #0
	sta resho
	sta resmoh
	sta resmo
	sta reslo
	lda facov
	jsr mltply
	lda faclo
	jsr mltply
	lda facmo
	jsr mltply
	lda facmoh
	jsr mltply
	lda facho
	jsr mltpl1
	jmp movfr

mltply:	bne mltpl1
	jmp mulshf

mltpl1:	lsr
	ora #$80
mltpl2:	tay
	bcc mltpl3
	clc
	ldx #3

mltpl4:	lda resho,x
	adc argho,x
	sta resho,x
	dex
	bpl mltpl4
mltpl3:	ror resho
	ror resmoh
	ror resmo
	ror reslo
	ror facov
	tya
	lsr
	bne mltpl2
multrt:	rts
; -------------------------------------------------------------------------------------------------
; conupk
;    move, typically a basic rom constant, into fac
;    enter: .a,.y  pointer to value
;    .x     bank# (ucnupk)

conupk:
ucnupk:	sta index1
	sty index1+1
	ldy #4
	lda (index1),y
	sta arglo
	dey
	lda (index1),y
	sta argmo
	dey
	lda (index1),y
	sta argmoh
	dey
	lda (index1),y
	sta argsgn
	eor facsgn
	sta arisgn
	lda argsgn
	ora #$80
	sta argho
	dey
	lda (index1),y
	sta argexp
	lda facexp
	rts
; -------------------------------------------------------------------------------------------------
; muldiv

muldiv:	lda argexp
mldexp:	beq zeremv
	clc
	adc facexp
	bcc tryoff
	bmi goover
	clc
	!byte $2c
tryoff:	bpl zeremv
	adc #$80
	sta facexp
	bne *+5
	jmp zeroml

	lda arisgn
	sta facsgn
	rts
mldvex:	lda facsgn
	eor #$ff
	bmi goover
zeremv:	pla
	pla
	jmp zerofc

goover:	jmp overr
; -------------------------------------------------------------------------------------------------
; multiply by 10

mul10:	jsr movaf
	tax
	beq mul10r
	clc
	adc #2
	bcs goover
finml6:	ldx #0
	stx arisgn
	jsr faddc
	inc facexp
	beq goover
mul10r:	rts
; -------------------------------------------------------------------------------------------------
; constant 10
tenc	!byte $84,$20,0,0,0		
; -------------------------------------------------------------------------------------------------
; divide by 10

div10:	jsr movaf
	lda #<tenc
	ldy #>tenc
	ldx #0
fdivf:	stx arisgn
	jsr movfm
	jmp fdivt
; -------------------------------------------------------------------------------------------------
; DIVIDE
fdiv:	jsr conupk

fdivt:	beq dv0err
	jsr round
	lda #0
	sec
	sbc facexp
	sta facexp
	jsr muldiv
	inc facexp
	beq goover
	ldx #$fc
	lda #1
divide:	ldy argho
	cpy facho
	bne savquo
	ldy argmoh
	cpy facmoh
	bne savquo
	ldy argmo
	cpy facmo
	bne savquo
	ldy arglo
	cpy faclo
savquo:	php
	rol
	bcc qshft
	inx
	sta reslo,x
	beq ld100
	bpl divnrm
	lda #1
qshft:	plp
	bcs divsub
shfarg:	asl arglo
	rol argmo
	rol argmoh
	rol argho
	bcs savquo
	bmi divide
	bpl savquo
divsub:	tay
	txa
	pha
	ldx #3

divsb1:	lda argho,x
	sbc facho,x
	sta argho,x
	dex
	bpl divsb1
	pla
	tax
	tya
	jmp shfarg

dv0err:	ldx #errdvo		; division by zero error
	jmp error

ld100:	lda #$40
	bne qshft
divnrm:	asl
	asl
	asl
	asl
	asl
	asl
	sta facov
	plp
; -------------------------------------------------------------------------------------------------
; floating pt move routines
;   move data between memory and float "regs". letters following "mov" in each routine name tells
;   what that routine accomplishes: destination, source
;   fac <- res
;   movfr
movfr:	ldx #3

movfrz:	lda resho,x
	sta facho,x
	dex
	bpl movfrz
	jmp normal
; -------------------------------------------------------------------------------------------------
;   movfm:   fac <- system memory
;   movfum:  fac <- user memory
movfm:
movfum:	sta index1
	sty index1+1
	ldy #3+addprc
	lda (index1),y
	sta faclo
	dey
	lda (index1),y
	sta facmo
	dey
	lda (index1),y
	sta facmoh
	dey
	lda (index1),y
	sta facsgn
	ora #$80
	sta facho
	dey
	lda (index1),y
	sta facexp
	sty facov
	rts
; -------------------------------------------------------------------------------------------------
;   mov2f:  temp2f <- fac
;   mov1f:  temp1f <- fac
;   movmf:  system memory <- fac
;   movvf:  forpnt var  <- fac
;   movumf: user memory <- fac
mov2f:	ldx #<tempf2
	!byte $2c
mov1f:	ldx #<tempf1
	ldy #0
movmf:	jsr round
	jmp mov001

movvf:	ldx forpnt
	ldy forpnt+1
	lda forpnt+2

movumf:	jsr round
mov001:	sty index1+1
	stx index1
	ldy #3+addprc
	lda faclo
	sta (index),y
	dey
	lda facmo
	sta (index),y
	dey
	lda facmoh
	sta (index),y
	dey
	lda facsgn
	ora #$7f
	and facho
	sta (index),y
	dey
	lda facexp
	sta (index),y
	sty facov
	rts
; -------------------------------------------------------------------------------------------------
;   movfa:  fac <- arg register
;   movaf:  arg <- fac register
movfa:	lda argsgn
movfa1:	sta facsgn
	ldx #4+addprc
movfal:	lda argexp-1,x
	sta facexp-1,x
	dex
	bne movfal
	stx facov
	rts

movaf:	jsr round
movef:	ldx #5+addprc
movafl:	lda facexp-1,x
	sta argexp-1,x
	dex
	bne movafl
	stx facov
movrts:	rts
; -------------------------------------------------------------------------------------------------
; round

round:	lda facexp
	beq movrts
	asl facov
	bcc movrts

incrnd:	jsr incfac
	bne movrts
	jmp rndshf
sign:	lda facexp
	beq signrt
fcsign:	lda facsgn
fcomps:	rol
	lda #$ff
	bcs signrt
	lda #1
signrt:	rts
; -------------------------------------------------------------------------------------------------
; SGN

sgn:	jsr sign

float:	sta facho
	lda #0
	sta facho+1
	ldx #$88
floats:	lda facho
	eor #$ff
	rol
floatc:	lda #0
	sta faclo
	sta facmo
floatb:	stx facexp
	sta facov
	sta facsgn
	jmp fadflt
; -------------------------------------------------------------------------------------------------
; ABS

abs:	lsr facsgn
	rts
; -------------------------------------------------------------------------------------------------
; fcomp - compare fac with value stored in system memory
;   enter: .a,.y pointer to value

fcomp:	sta index2
fcompn:	sty index2+1		;  enter w/value on stack ("next")
	ldy #0
	jsr fcinx2 
	iny
	tax
	beq sign
	jsr fcinx2 
	eor facsgn
	bmi fcsign
	cpx facexp
	bne fcompc
	jsr fcinx2 
	ora #$80
	cmp facho
	bne fcompc
	iny
	jsr fcinx2 
	cmp facmoh
	bne fcompc
	iny
	jsr fcinx2 
	cmp facmo
	bne fcompc
	iny
	lda #$7f
	cmp facov
	jsr fcinx2 
	sbc faclo
	beq qintrt
fcompc:	lda facsgn
	bcc fcompd
	eor #$ff
fcompd:	jmp fcomps
; -------------------------------------------------------------------------------------------------
; fcinx2
fcinx2:	lda (index2),y
	rts
; ******************************************* MATH4 ***********************************************
; qint - quick greatest integer function.
;   leaves int(fac) in facho,mo,lo signed.
;   assumes fac < 2**23 = 8388608.

qint:	lda facexp
	beq clrfac
	sec
	sbc #addpr8+$98
	bit facsgn
	bpl qishft
	tax
	lda #$ff
	sta bits
	jsr negfch
	txa
qishft:	ldx #<fac
	cmp #$f9
	bpl qint1
	jsr shiftr
	sty bits
qintrt:	rts
qint1:	tay
	lda facsgn
	and #$80
	lsr facho
	ora facho
	sta facho
	jsr rolshf
	sty bits
	rts
; -------------------------------------------------------------------------------------------------
; INT

int:	lda facexp
	cmp #addpr8+$98
	bcs intrts
	jsr qint
	sty facov
	lda facsgn
	sty facsgn
	eor #$80
	rol
	lda #$98+8
	sta facexp
	lda faclo
	sta integr
	jmp fadflt
clrfac:	sta facho
	sta facmoh
	sta facmo
	sta faclo
	tay 
intrts:	rts
; -------------------------------------------------------------------------------------------------
; fin - number input is left in fac.
;   at entry txtptr -> to the 1st char in a text buf. the 1st char is also in acc.
;   fin packs the digits into the fac as an integer and keeps track of where the decimal point is.
;   dptflg tells whether a dp has been seen.  dccnt is the number of digits after the dp.
;   at the end dccnt and the exp are used to determine how many times to mult or div by 10 to
;   get the correct #.

fin:	ldy #$00
	ldx #sgnflg-deccnt
finzlp:	sty deccnt,x
	dex
	bpl finzlp
	bcc findgq
	cmp #'-'
	bne qplus
	stx sgnflg
	beq finc
qplus:	cmp #'+'
	bne fin1
finc:	jsr chrget
findgq:	bcc findig
fin1:	cmp #'.'
	beq findp
	cmp #'e'
	bne fine
	jsr chrget
	bcc fnedg1
	cmp #tkminu
	beq finec1
	cmp #'-'
	beq finec1
	cmp #tkplus
	beq finec
	cmp #'+'
	beq finec
	bne finec2
finec1:	ror expsgn
finec:	jsr chrget
fnedg1:	bcc finedg
finec2:	bit expsgn
	bpl fine
	lda #0
	sec
	sbc tenexp
	jmp fine1
; findp
findp:	ror dptflg
	bit dptflg
	bvc finc
fine:	lda tenexp
fine1:	sec
	sbc deccnt
	sta tenexp
	beq finqng
	bpl finmul
findiv:	jsr div10
	inc tenexp
	bne findiv
	beq finqng
finmul:	jsr mul10
	dec tenexp
	bne finmul
finqng:	lda sgnflg
	bmi negxqs
	rts

negxqs:	jmp negop
; findig
findig:	pha
	bit dptflg
	bpl findg1
	inc deccnt
findg1:	jsr mul10
	pla
	sec
	sbc #'0'
	jsr finlog
	jmp finc
; finlog
finlog:	pha
	jsr movaf
	pla
	jsr float
	lda argsgn
	eor facsgn
	sta arisgn
	ldx facexp
	jmp faddt
; finedg
finedg:	lda tenexp
	cmp #$0a
	bcc mlex10
	lda #$64
	bit expsgn
	bmi mlexmi
	jmp overr

mlex10:	asl
	asl
	clc
	adc tenexp
	asl
	clc
	ldy #0
	sta tttemp
	lda (txtptr),y
	adc tttemp
	sec
	sbc #'0'
mlexmi:	sta tenexp
	jmp finec
; -------------------------------------------------------------------------------------------------
; constants
n0999:	!byte $9b,$3e,$bc,$1f,$fd	; 99,999,999.0499
n9999:	!byte $9e,$6e,$6b,$27,$fd	; 999,999,999.499
nmil:	!byte $9e,$6e,$6b,$28,0  	; 10**9
; -------------------------------------------------------------------------------------------------
; linprt	
;   entry   a= < of number
;   x= >of number

linprt:	sta facho
	stx facho+1
	ldx #$90		; exponent is 2**16
	sec
	jsr floatc
outfac:	jsr fout
	ldx #fbptr
; -------------------------------------------------------------------------------------------------
; msg - print a message
;   can also print the contents of fbuffr
;   entry:  .x =index into ebase, a table of start addresses of messages
;   data =sequential ascii characters
;   terminated by a zero byte
;   calls:  ldaabs, ochr

msg:	lda ebase,x
	ldy ebase+1,x
	sta ldaadr
	sty ldaadr+1

msgprt:	jsr ldaabs
	beq msgrts		; zero byte terminator
	jsr ochr
	inc ldaadr
	bne msgprt
	inc ldaadr+1
	bne msgprt
msgrts:	rts
; -------------------------------------------------------------------------------------------------
; fout -  build string for formula result.

fout:	ldy #1
	lda #$20		; print space if positive
	bit facsgn
	bpl fout1
	lda #'-'
fout1:	sta fbuffr-1,y 		; store the char
	sta facsgn 		; make fac positive or qint.
	sty fbufpt 		; save for later
	iny
	lda #'0'		; get zero to type if fac=0
	ldx facexp
	bne fout10
	jmp fout19

fout10:	lda #0
	cpx #$80
	beq fout37 		; if number > 1.0
	bcs fout7
fout37:	lda #<nmil
	ldy #>nmil
	jsr fmult  		; * 10**6
	lda #250-addpr2-addprc
fout7:	sta deccnt 		; save count or zero it
fout4:	lda #<n9999
	ldy #>n9999
	jsr fcomp  		; if num > 999,999.999
	beq bigges
	bpl fout9
fout3:	lda #<n0999
	ldy #>n0999
	jsr fcomp
	beq fout38
	bpl fout5
fout38:	jsr mul10
	dec deccnt
	bne fout3
fout9:	jsr div10
	inc deccnt
	bne fout4
fout5:	jsr faddh
bigges:	jsr qint
	ldx #1
	lda deccnt
	clc
	adc #addpr2+addprc+7
	bmi foutpi
	cmp #addpr2+addprc+$08
	bcs fout6
	adc #$ff
	tax
	lda #2
foutpi:	sec
fout6:	sbc #2
	sta tenexp
	stx deccnt
	txa
	beq fout39
	bpl fout8
fout39:	ldy fbufpt
	lda #'.'
	iny
	sta fbuffr-1,y
	txa
	beq fout16
	lda #'0'
	iny
	sta fbuffr-1,y
fout16:	sty fbufpt

fout8:	ldy #0
	ldx #$80
fout2:	lda faclo
	clc
	adc foutbl+2+addprc,y
	sta faclo
	lda facmo
	adc foutbl+1+addprc,y
	sta facmo
	lda facmoh
	adc foutbl+1,y
	sta facmoh
	lda facho
	adc foutbl,y
	sta facho
	inx
	bcs fout41
	bpl fout2
	!byte $2c    		; hop
fout41:	bmi fout2
fout40:	txa
	bcc foutyp
	eor #$ff
	adc #$0a
foutyp:	adc #$2f
	iny
	iny
	iny
	iny
	sty fdecpt
	ldy fbufpt
	iny
	tax
	and #$7f
	sta fbuffr-1,y
	dec deccnt
	bne stxbuf
	lda #'.'
	iny
	sta fbuffr-1,y
stxbuf:	sty fbufpt
	ldy fdecpt
	txa
	eor #$ff
	and #$80
	tax
	cpy #fdcend-foutbl
	bne fout2
fouldy:	ldy fbufpt
fout11:	lda fbuffr-1,y
	dey
	cmp #'0'
	beq fout11
	cmp #'.'
	beq fout12
	iny
fout12:	lda #'+'
	ldx tenexp
	beq fout17
	bpl fout14
	lda #0
	sec
	sbc tenexp
	tax
	lda #'-'
fout14:	sta fbuffr+1,y
	lda #'e'
	sta fbuffr,y
	txa
	ldx #$2f
	sec
fout15:	inx
	sbc #$0a
	bcs fout15
	adc #$3a
	sta fbuffr+3,y
	txa
	sta fbuffr+2,y
	lda #0
	sta fbuffr+4,y
	beq fout20
fout19:	sta fbuffr-1,y
fout17:	lda #0
	sta fbuffr,y
fout20:	rts
; -------------------------------------------------------------------------------------------------
; constants

n32768:	!byte $90        	; 32768 - must stay with fhalf and zero
fhalf:	!byte $80,0		; 0.5 for sqr
fzero:	!byte 0,0,0
; -------------------------------------------------------------------------------------------------
; power of ten table

foutbl:	!byte $fa,$0a,$1f,$00   ; -100,000,000
	!byte $00,$98,$96,$80   ; 10,000,000
	!byte $ff,$f0,$bd,$c0   ; -1,000,000
	!byte $00,$01,$86,$a0   ; 100,000
	!byte $ff,$ff,$d8,$f0   ; -10,000
	!byte $00,$00,$03,$e8   ; 1000
	!byte $ff,$ff,$ff,$9c   ; -100
	!byte $00,$00,$00,$0a   ; 10
	!byte $ff,$ff,$ff,$ff   ; -1
fdcend:
;	!byte $ff,$df,$0a,$80   ; -216,000 for time converter
;	!byte $00,$03,$4b,$c0   ; 216,000
;	!byte $ff,$ff,$73,$60   ; -36000
;	!byte $00,$00,$0e,$10   ; 3600
;	!byte $ff,$ff,$fd,$a8   ; -600
;	!byte $00,$00,$00,$3c   ; 60
;timend
; -------------------------------------------------------------------------------------------------
; SQR - square root function  sqr(a)
;   uses sqr(a) == a**0.5
sqr:	jsr movaf
	lda #<fhalf
	ldy #>fhalf
	jsr movfm
;	jmp fpwrt  		; last thing fetched is facexp into accx
; -------------------------------------------------------------------------------------------------
; EXP - exponentation x**y
;   if y = 0 then results = 1.
;   if x = 0 then results = 0.
;   if x > 0, if not check that y is an integer.
;   if so, negate x, so that log doesn't give fcerr.
;   if x is negative and y is odd, negate the result.
;   returned by exp.
;   to compute the use x**y=exp((y*log(x)).

fpwrt:	beq exp			; if fac=0, just exponentiaaate that
	lda argexp
	bne fpwrt1 		; if x <> 0
	jmp zerof1

fpwrt1:	ldx #<tempf3
	ldy #>tempf3
	jsr movmf

; y=0 already.  good in case no one calls int.
	lda argsgn
	bpl fpwr1  		; no problems if x > 0
	jsr int			; integerize the fac
	lda #<tempf3		; get comperand adr
	ldy #>tempf3
	jsr fcomp
	bne fpwr1
	tya
	ldy integr
fpwr1:	jsr movfa1
	tya
	pha
	jsr log
	lda #<tempf3
	ldy #>tempf3
	jsr fmult
	jsr exp
	pla
	lsr
	bcc negrts
; -------------------------------------------------------------------------------------------------
; NEG

negop:	lda facexp
	beq negrts
; -------------------------------------------------------------------------------------------------
; sav41
sav41:	lda facsgn
	eor #$ff
	sta facsgn
negrts:	rts
; -------------------------------------------------------------------------------------------------
; constants

logeb2:	!byte $81,$38,$aa,$3b,$29	; log(e) base 2

expcon:	!byte 7				; 8 exp constants (polynom 7)
	!byte $71,$34,$58,$3e,$56		
	!byte $74,$16,$7e,$b3,$1b
	!byte $77,$2f,$ee,$e3,$85
	!byte $7a,$1d,$84,$1c,$2a
	!byte $7c,$63,$59,$58,$0a
	!byte $7e,$75,$fd,$e7,$c6
	!byte $80,$31,$72,$18,$10
	!byte $81,0,0,0,0		; 1
; -------------------------------------------------------------------------------------------------
; EXP - multiply by log(e) base 2.

exp:	lda #<logeb2
	ldy #>logeb2
	jsr fmult
	lda facov
	adc #$50
	bcc stold
	jsr incrnd

stold:	sta oldov
	jsr movef
	lda facexp
	cmp #$88
	bcc exp1
gomldv:	jsr mldvex
exp1:	jsr int
	lda integr
	clc
	adc #$81
	beq gomldv
	sec
	sbc #1
	pha
	ldx #4+addprc
swaplp:	lda argexp,x
	ldy facexp,x
	sta facexp,x
	sty argexp,x
	dex
	bpl swaplp
	lda oldov
	sta facov
	jsr fsubt
	jsr negop
	lda #<expcon
	ldy #>expcon
	jsr poly
	lda #0
	sta arisgn
	pla
	jsr mldexp
	rts
; -------------------------------------------------------------------------------------------------
; polyx - evaluate p(x**y)*x
;   pointer to degree is in y,a
;   the constants follow the degree for x=fac, compute
;   c0*x+c1*x**3+c2*x**5+c3*x**7... cn*x**(2*n-1)

polyx:	sta polypt
	sty polypt+1
	jsr mov1f
	lda #<tempf1
	jsr fmult
	jsr poly1
	lda #<tempf1
	ldy #>tempf1
	jmp fmult

; polynomial evaluator
; pointer to degree is in y,a
poly:	sta polypt
	sty polypt+1
poly1:	jsr mov2f
	lda #0
	ora (polypt),y		;  read system mem
	sta degree
	ldy polypt
	iny
	tya
	bne poly3
	inc polypt+1
poly3:	sta polypt
	ldy polypt+1
poly2:	jsr fmult
	lda polypt
	ldy polypt+1
	clc
	adc #4+addprc
	bcc poly4
	iny
poly4:	sta polypt
	sty polypt+1
	jsr fadd
	lda #<tempf2
	ldy #>tempf2
	dec degree
	bne poly2
	rts
; -------------------------------------------------------------------------------------------------
; constants for RND
rmulc:	!byte $98,$35,$44,$7a	; 11879546
raddc:	!byte $68,$28,$b1,$46	; 3.92767774 e-4
; -------------------------------------------------------------------------------------------------
; RND - pseudo-random number generator.

rnd:	jsr sign
	bmi rnd1
	ldx seedpt
	ldy seedpt+1
	inx
	bne rnd10
	iny

rnd10:	txa
	jsr movfum
	lda #<rmulc
	ldy #>rmulc
	jsr fmult
	lda #<raddc
	ldy #>raddc
	jsr fadd
rnd1:	ldx faclo
	lda facho
	sta faclo
	stx facho
	ldx facmoh
	lda facmo
	sta facmoh
	stx facmo
strnex:	lda #0
	sta facsgn
	lda facexp
	sta facov
	lda #$80
	sta facexp
	jsr normal
	ldx seedpt
	ldy seedpt+1
	inx
	bne rnd20    		; if no carry
	iny
rnd20:	jmp movumf
; -------------------------------------------------------------------------------------------------
; COS - cosinus function

cos:	lda #<pi2
	ldy #>pi2
	jsr fadd
; -------------------------------------------------------------------------------------------------
; SIN - sinus function

sin:	jsr movaf
	lda #<twopi
	ldy #>twopi
	ldx argsgn
	jsr fdivf
	jsr movaf
	jsr int
	lda #0
	sta arisgn
	jsr fsubt
	lda #<fr4
	ldy #>fr4
	jsr fsub
	lda facsgn
	pha
	bpl sin1
	jsr faddh
	lda facsgn
	bmi sin2
	lda tansgn
	eor #$ff
	sta tansgn
sin1:	jsr negop
sin2:	lda #<fr4
	ldy #>fr4
	jsr fadd
	pla
	bpl sin3
	jsr negop
sin3:	lda #<sincon
	ldy #>sincon
	jmp polyx
; -------------------------------------------------------------------------------------------------
; TAN - tangent function

tan:	jsr mov1f
	lda #0
	sta tansgn
	jsr sin
	ldx #<tempf3
	ldy #>tempf3
	jsr movmf
	lda #<tempf1
	ldy #>tempf1
	jsr movfm
	lda #0
	sta facsgn
	lda tansgn
	jsr cosc
	lda #<tempf3
	ldy #>tempf3
	jmp fdiv

cosc:	pha
	jmp sin1
; -------------------------------------------------------------------------------------------------
; constants

pi2:	!byte $81,$49,$0f,$da,$a2	; pi/2
twopi:	!byte $83,$49,$0f,$da,$a2	; 2pi
fr4:	!byte $7f,0,0,0,0		; 0.25

sincon:	!byte 5				; 6 sin+cos constants (polynom 5) 
	!byte $84,$e6,$1a,$2d,$1b
	!byte $86,$28,$07,$f6,$f8
	!byte $87,$99,$68,$89,1
	!byte $87,$23,$35,$df,$e1
	!byte $86,$a5,$5d,$e7,$28
	!byte $83,$49,$0f,$da,$a2

atncon:	!byte $0b			; 12 atn constants (polynom 11)
	!byte $76,$b3,$83,$bd,$d3
	!byte $79,$1e,$f4,$a6,$f5
	!byte $7b,$83,$fc,$b0,$10
	!byte $7c,$0c,$1f,$67,$ca
	!byte $7c,$de,$53,$cb,$c1
	!byte $7d,$14,$64,$70,$4c
	!byte $7d,$b7,$ea,$51,$7a
	!byte $7d,$63,$30,$88,$7e
	!byte $7e,$92,$44,$99,$3a
	!byte $7e,$4c,$cc,$91,$c7
	!byte $7f,$aa,$aa,$aa,$13
	!byte $81,0,0,0,0
; -------------------------------------------------------------------------------------------------
; ATN

atn:	lda facsgn
	pha
	bpl atn1
	jsr negop
atn1:	lda facexp
	pha
	cmp #$81
	bcc atn2
	lda #<fone
	ldy #>fone
	jsr fdiv
atn2:	lda #<atncon
	ldy #>atncon
	jsr polyx
	pla
	cmp #$81
	bcc atn3
	lda #<pi2
	ldy #>pi2
	jsr fsub
atn3:	pla
	bpl atn4
	jmp negop
; ******************************************* STRNG1 **********************************************
; 'string utility routines'
; PUDEF
;   puctrl takes assigns values for the print using characters (fill,comma,dot,dollar) from a
;   user string.

puctrl:	jsr sav13
	tay
	beq pucbye		; nothing,... done
	cpy #pumony-puchrs+1
	bcc puc50		; len < #pu chars
	ldy #pumony-puchrs+1	; len >= #pu chars
puc50:	dey
puc60:	lda (index),y
	sta puchrs,y
	dey
	bpl puc60
pucbye:	rts
; -------------------------------------------------------------------------------------------------
; STR
;   the str$ fnc takes a number and gives a string with the chars the output of the # would have
;   given.

strd:	jsr chknum
	jsr fout
	pla
	pla

; find length of number string
foutst:	ldy #0
str010:	lda fbuffr,y
	beq str020
	iny
	bne str010

str020:	tya			; length to .a
	jsr strspa
;	move from fbuffr to (dsctmp)
	ldy #0
str030:	lda fbuffr,y
	beq str040
	sta (dsctmp+1),y
	iny
	bne str030
str040:	jmp putnew
; -------------------------------------------------------------------------------------------------
; strini
;   get string space for the creation of a string and create a descriptor for it in dsctmp
;   enter:  strini -  facmo contains descriptor ptr
;   -  .a = length
;   strspa -  .a = length

strini:	ldx facmo
	ldy facmo+1
	stx dscpnt		; dscpnt <- descriptor ptr
	sty dscpnt+1
strspa:	jsr getspa		; allocate space
	sta dsctmp		; length
	stx dsctmp+1		; ptr to string
	sty dsctmp+2
atn4:	rts
; -------------------------------------------------------------------------------------------------
; strlit
;   get string space for the creation of a string.
;   space is allocated and a copy is made of this substring. a temporary descriptor is made.
;   exit:   strng1 = ptr to start of string
;   strng2 = ptr to 1st char past terminator
;   facmo  = descriptor ptr for copy

;   strlit,strlt2: a string is searched until the end is reached or a matching terminating char is
;   found. this substring is copied to a new temporary string.
;   enter: .a,.y = ptr to string
;          .x = string bank#
;   putnew: temp descriptor dsctmp is put on the descr "stack" and facmo is its desc ptr.

strlit:	pha
	lda #34			; set terminators
	sta charac
	sta endchr
	pla
strlt2:	sta strng1		; strng1<-ptr to string
	sty strng1+1
	sta dsctmp+1		; ditto..
	sty dsctmp+2
	ldy #$ff
strget:	iny			; skim over string
	lda (strng1),y
	beq strfi1
	cmp charac
	beq strfin
	cmp endchr
	bne strget
strfin:	cmp #34
	beq strfi2
strfi1:	clc
strfi2:	sty dsctmp
	tya
	adc strng1		; strng2 <- ptr to end of string
	sta strng2
	ldx strng1+1
	bcc strst2
	inx
strst2:	stx strng2+1
	tya			; .a <- length
	jsr strini		; allocate space
	ldx #2

sotl:	ldy strng1,x
	sty index,x
	dex
	bpl sotl
; entry point from leftd
glgr: 	jsr movdo		; copy it to end of string area

putnew:	ldy temppt
	cpy #strsiz+strsiz+strsiz
	bne putnw1
	ldx #errst
	jmp error

; compute address of this temp descriptor and temppt becomes offset to next descriptor.
; two descr pointers:
; facmo,lastpt <- tempst+temppt
; and:
; temppt <- temppt+strsiz
putnw1:	ldx #0			; move len,ptr,bank bytes

putnwl:	lda dsctmp,x
	sta (tempst),y		; length
	iny
	inx
	cpx #strsiz
	bne putnwl
	lda tempst+1
	sta facmo+1
	sta lastpt+1
	clc
	lda tempst
	adc temppt
	sta facmo
	sta lastpt
	bcc putnw2
	inc facmo+1
	inc lastpt+1

putnw2:	sty temppt
	ldy #0
	sty facov
	dey
	sty valtyp		; $ff is string type
	rts
; -------------------------------------------------------------------------------------------------
; movins - move a string to top of free space
;   enter: movins  - strng1 is src descriptor ptr
;   movdo  - index = ptr to src string
;         .a = length
;   exit: index = ptr to source
;   frespc = ptr to dst link bytes

movins:	ldy #0
	lda (strng1),y		; get length,ptr via descr ptr
	tax
	iny
	lda (strng1),y		; index<-ptr to string
	sta index
	iny
	lda (strng1),y
	sta index+1
	iny
	lda (strng1),y
	sta index+2
	txa
movdo:	tay
	beq mvdone
	pha
movlp:	dey			; move the string
	lda (index),y
	sta (frespc),y
	tya
	bne movlp
	pla
mvdone:	clc			; adjust frespc ptr
	adc frespc
	sta frespc
	bcc mvstrt
	inc frespc+1
mvstrt:	rts
; -------------------------------------------------------------------------------------------------
; frestr
;   given a string descriptor pointer, determine if it can be freed and do so.
;   exit:  .a = length
;   index = ptr to string

frestr:	jsr chkstr

frefac:	lda facmo		;  descptr ptr
	ldy facmo+1
fretmp:	jsr sav10
	bne fre02		; one then scratch it
	jsr stradj		; index points to link
	bcc fre02		; literal no fix
	jsr mkgarb		; mark it as garbage
	pha			; save length on stack

	eor #$ff		; put index back
	sec			; to first byte
	adc index
	ldy index+1
	bcs res00
	dey
res00:	sta index
	sty index+1
	tax			; lo into x
	pla			; pull length from stack
	ldy index+1
	cpy fretop+1		; ptr(hi)
	bne frerts
	cpx fretop		; ptr(lo)
	bne frerts

; string was last into string space
; save garbage collection some time by freeing up. (length + ptrsiz)
	pha			; save length on stack
	clc
	adc fretop
	bcc fre01
	inc fretop+1
fre01:	clc
	adc #ptrsiz
	sta fretop
	bcc frepla
	inc fretop+1
frepla:	pla			; pull length off stack
	rts

; index is descripto ptr. set up return values as if string had been marked as garbage, as in above
fre02:	ldy #$00		; set up .a and index
	lda (index),y		; length
	pha
	iny
	lda (index),y		; pointer lo
	pha
	iny
	lda (index),y		; pointer hi
	tax
	iny
	lda (index),y		; pointer bank
	sta index+2
	stx index+1
	pla
	sta index
	pla			; get back length
	rts
; -------------------------------------------------------------------------------------------------
; sav10
;   if the descriptor pointer is on the top of string descriptor stack, pop it off.
;   enter:  .a,.y = descriptor ptr

sav10:	sta index
	sty index+1

fretms:	cpy lastpt+1
	bne frerts
	cmp lastpt
	bne frerts
	sbc #strsiz
	sta lastpt
	bcs fret10
	dec lastpt+1
fret10:	sec
	lda temppt
	sbc #strsiz
	sta temppt
	ldy #0
frerts:	rts
; -------------------------------------------------------------------------------------------------
; string assignment
; inpcom perform the assignment of a string value to a string variable.
;   enter: forpnt = ptr to lhs var's data memory
;          facmo  = descriptor ptr to rhs string val
;   forpnt is checked to see if it ds$. special code handles ds$ assignments. also, if the rhs value
;   is not a temporary result (i.e., descriptor ptr points to the symbol tables), a copy of the
;   value is made.

inpcom:
; detect ti$ 			; modified by vossi
	lda varnam
	ldy varnam+1
	cmp #'t'
	bne getspt
	cpy #$c9   		; 'i'+$80
	bne getspt
	jmp mktime		; see ptrget, => ti$
; not ti$
getspt:	ldy facmo+1		; descr ptr for temp?
	cpy strend+1		; make a copy, if not...
	bcc copy
	bne dntcpy
	lda facmo
	cmp strend
	bcc copy
dntcpy: lda facmo
	ldy facmo+1
	sta dscpnt
	sty dscpnt+1
	jsr sav10
	jsr stradj		; set up index to new string
	bcc dcop02
	jsr bcklnk		; link new
dcop02:	jsr fixold		; fix old...
	ldy #strsiz-1
dcop01:	lda (dscpnt),y		; set the descriptor
	sta (forpnt),y
	dey
	bpl dcop01
	rts
; -------------------------------------------------------------------------------------------------
; copy - make a copy of the source value. do the assignment of the duplicate.

copy:	ldy #0
	lda (facmo),y
	jsr strini		; copy src value: make space for it
	ldx #2

soth:	ldy dscpnt,x
	sty strng1,x
	dex
	bpl soth
	jsr movins		; move src to temp
; fix to fre get strings
	lda strng1		; restore .a & .y
	ldy strng1+1
	ldx strng1+2
	jsr fretms		; fre the temp string

	jsr stradd		; set up index
	bcc copy02		; ?fix back links
	jsr bcklnk		; link new
copy02:	jsr fixold		;  fix old...
	ldy #strsiz-1		; fix var's descriptor
copy01:	lda dsctmp,y
	sta (forpnt),y
	dey
	bpl copy01
	rts
; -------------------------------------------------------------------------------------------------
; bcklnk
;   fix the new string by setting its back link bytes to point to the destination descriptor.
;   enter: forpnt = descr ptr for dest

bcklnk:	ldy #$00
	lda forpnt		; put in backwards link
	sta (index),y
	iny
	lda forpnt+1
	sta (index),y
	rts
; -------------------------------------------------------------------------------------------------
; fixold - if the destination has an old value, free it and mark it as garbage.
;   enter:  forpnt=dest descriptor ptr

fixold:	ldx #2
sump:	ldy forpnt,x
	sty index,x
	dex
	bpl sump
	jsr stradj		; point to old string
	bcc fnk05		; in text do not fix
;	jmp mkgarb		; mark as garbage
; -------------------------------------------------------------------------------------------------
; mkgarb - mark the string as garbage
;   enter:  index points to str link bytes
;   .x = length
;   exit:   .x,.a = length
;   .y = 0
;   index unchanged

mkgarb:	ldy #2
	lda #$ff
	sta (index),y		; mark bank#
	dey
	sta (index),y		; mark back link(hi) as junk
	dey
	txa
	sta (index),y		; back link(lo)=len
	rts
; -------------------------------------------------------------------------------------------------
; stradj, stradd	
;   takes the pointer index which points to a descriptor and indexes to the desciptors string data.
;   if no action to take, (ds$ string or zero length string) we return with carry clear;
;   otherwise we return with the pointer set to the link bytes in the string
;   the length in .a and the carry set.
;   stradj - use index as descriptor ptr
;   stradd - use dsctmp as descriptor

stradj:	ldy #$00
	lda (index),y		; push length on stack
	pha
	beq sadj8		; length 0 do nothing
	iny
	lda (index),y		; push lo byte
	pha
	iny
	lda (index),y		; high byte in .x
	tax
	iny
	lda (index),y		; bank in .y
	tay
	pla			; lo

sadj3:
	;cpx fretop+1
	;bcc sadj8
	;cmp fretop
	;bcc sadj8
	cpy dsdesc+3		; is it ds$ string?
	bne sadj4
	cpx dsdesc+2
	bne sadj4		; fix
	cmp dsdesc+1
	beq sadj8
sadj4:	sta index		; ok, set pointer
	stx index+1
	sty index+2
	pla			; get back length
	tax			; into x also
	jsr sav15
sadj6:	sec			; carry set
fnk05:	rts

sadj8:	pla			; clean up stack
	clc
	rts

stradd:	lda dsctmp
	pha			; length on stack
	beq sadj8		; do nothing
	lda dsctmp+1		; string ptr (lo)
	ldx dsctmp+2		; string ptr (hi)
	ldy dsctmp+3		; string ptr (bank)
	jmp sadj3		; etc
; ******************************************* STRNG2 **********************************************
; 'string functions'
; cat - concatenate two strings

cat:	lda facmo+1		; (ptr)
	pha
	lda facmo
	pha
	jsr eval		; do 2nd operand
	jsr chkstr		; must be string value
	pla
	sta strng1		; strng1 <- descr ptr for 1st opnd
	pla
	sta strng1+1
	ldy #0
	lda (strng1),y		; len1+len2<256 ?
	sta tttemp
	lda (facmo),y
	clc
	adc tttemp
	bcc sizeok
	jmp errlen

sizeok:	jsr strini		; allocate for result
	jsr movins		; copy 1st operand
	jsr sav47
	jsr movdo		; append 2nd string
	lda strng1		; descriptor ptr for result
	ldy strng1+1
	ldx strng1+2
	jsr fretmp
	jsr putnew
	jmp tstop
sav47:	lda dscpnt		; descr ptr 2nd opnd
	ldy dscpnt+1
	jmp fretmp		; sets up index1!
; -------------------------------------------------------------------------------------------------
; CHR$ function

chrd:	jsr conint
	txa			; .a <- parameter value
	pha
	lda #1
	jsr strspa		; space for 1 char
	pla
	ldy #0
	sta (dsctmp+1),y	; store the char
chrd2:	pla
	pla
	jmp putnew		; another tmp descr...
; -------------------------------------------------------------------------------------------------
; LEFT$

leftd:	jsr pream
	pha
	lda (dscpnt),y
	sta tttemp
	pla
	cmp tttemp
	tya
; if 2nd param>length, use length instead
rleft:	bcc rleft1
	lda (dscpnt),y
	tax
	tya
rleft1:	pha			; starting offset
rleft2:	txa			; length
rleft3:	pha
	jsr strspa		; get space for result
	jsr sav47
	pla
	tay
	pla
; add offset. index will point to 1st desired char.
	jsr sav15
pulmor:	tya
	jmp glgr 		; movdo followed by putnew
sav15:	clc
sav14:	adc index
	sta index
	bcc sav16
	inc index+1
sav16:	rts
; -------------------------------------------------------------------------------------------------
; RIGHT$

rightd:	jsr pream
	pha
	lda (dscpnt),y
	sta tttemp
	pla
	clc
	sbc tttemp		; places-length
	eor #$ff		; if cy, then length will be used
	jmp rleft

sav17:	jsr chrgot
	cmp #')'
	beq sav18
	jmp combyt     		; check for comma and get a byte
; -------------------------------------------------------------------------------------------------
; MID$

midd:	lda #$ff
	sta faclo
	jsr sav17

mid2:	jsr pream
	beq gofuc
	dex
	txa			; 1st pos = 1st opnd - 1
	pha
	pha
	clc
	ldx #0
	lda (dscpnt),y
	sta tttemp
	pla
	sbc tttemp
; length of result = 0, if 1st pos > len str opnd
;                  = min(len str opnd - 1st pos, 3rd opnd)
	bcs rleft2
	eor #$ff
	cmp faclo
	bcc rleft3
	lda faclo
	bcs rleft3
; -------------------------------------------------------------------------------------------------
; pream
;   exit pream: .x=.a=2nd param val
;   .y=0
;   dscpnt set up for 1st opnd

pream:	jsr chkcls		; ')'?
	pla
	tay
	pla
	sta jmper+1		; pop return

; pop off:  return from caller of pream (see isfun should go back to eval instead.)
; .x<-stack (2nd param, byte val)
; dscpnt<-stack (descrp ptr 1st opnd)
; push back return to caller of pream.
	pla
	pla
	pla
	tax
	pla
	sta dscpnt
	pla
	sta dscpnt+1
	pla
	lda jmper+1

	pha
	tya
	pha
	ldy #0
	txa
sav18:	rts
; -------------------------------------------------------------------------------------------------
; LEN

len:	jsr len1
	jmp sngflt

len1:	jsr frestr
	ldx #0
	stx valtyp
	tay
	rts
; -------------------------------------------------------------------------------------------------
; ASC

asc:	jsr len1
	beq gofuc
	ldy #0
	jsr sav12
	tay
	jmp sngflt

gofuc:	jmp fcerr		; -> illegal quantity error, ready
; -------------------------------------------------------------------------------------------------
; VAL

val:	jsr len1
	bne val1
	jmp zerofc

val1:	ldx #2    		; strng2 := txtptr

trug:	ldy txtptr,x
	sty strng2,x
	dex
	bpl trug
	ldx index1+2 
	ldx index1
	stx txtptr
	clc
	adc index1
	sta index2		; index2 := offset to end of str
	ldx index1+1
	stx txtptr+1
	bcc val2
	inx
val2:	stx index2+1
	ldy #0
	lda (index2),y
	pha
	tya
	sta (index2),y
	jsr chrgot
	jsr fin
	pla
	ldy #0
	sta (index2),y
st2txt:	ldx strng2		; restore text pointer
	ldy strng2+1
	stx txtptr
	sty txtptr+1
	rts
; trash old token from isfun
errd:	pla
	jsr parchk
	jsr chknum		; check for numeric argument
	jsr conint		; get integer arg. in x
	txa
	asl
	cmp #errbln		; check value against length of error pointer list
	bcs gofuc
	tay
	lda ebase,y 		; get address of message
	sta index2		; and create a temp descriptor pointing to it
	lda ebase+1,y
	sta index2+1
	ldy #$ff
	ldx #0
erflp1:	iny	 		; count # of characters in message
	lda (index2),y
	beq errd1  		; quit when the end is found
	cmp #$20   		; don't count non-printers
	bcc erflp1
	inx 			; it's a printer, so count it!
	bne erflp1		; always
errd1:	txa
	jsr strspa		; reserve a string
	ldy #$ff
	ldx #0
erflp2:	iny
	lda (index2),y
	beq errd2    		; done if null
	cmp #$20
	bcc erflp2
	sty tttemp 		; save .y
	pha
	txa        		; move .x to .y
	tay
	pla
	sta (dsctmp+1),y
	tya        		; move .y back to .x
	tax
	ldy tttemp  		; restore .y
	inx
	bne erflp2		; always
errd2:	jmp putnew
; -------------------------------------------------------------------------------------------------
; gettim
;   time is passed to and from the system in all three registers in the following way:
;   .a  (tenth bit 0) (seconds)
;   .x  (tenth bit 1) (minutes)
;   .y  (am/pm bit) (tenth bits 3,2) (hours)
;   the hours, minutes, and seconds are bcd digits and the tenths digit is stored in the unused
;   bits of them.
;   gettim: get the time in the registers and unpack into tmhour,tmmin,tmsec,tmten
;   puts unpacked time into a string.

gettim:	jsr rdtim
	pha			; save 10th bit 0
	and #$7f		; get seconds
	sta tmsec
	tya			; get hours
	and #$9f
	php			; save plus status
	and #$1f		; get rid of pm bit, if set
	cmp #$12		; treat 12 as 0
	bne *+4
	lda #0
	plp 			; test for pm
	bpl lkt50
	sei			; pm bit set, adjust hours
	sed
	clc
	adc #$12
	cld
	cli
lkt50:	sta tmhour
	lda #0			; do 10ths
	sta tmten
	tya
	rol			; pm bit
	rol			; 10th bit 3
	rol tmten
	rol			; 10th bit 2
	rol tmten
	txa
	rol			; 10th bit 1
	rol tmten
	lsr			; minutes
	sta tmmin
	pla
	rol			; 10th bit 0
	rol tmten

	lda #8			; string space for 7 chars & 1 null
	jsr getspa
	stx index1		; index1<- pointer to str space
	sty index1+1
	tay			;  .y = 8
; put in the null terminator
	dey
	lda #0
	sta (index1),y
	dey
; do tenths, convert to ascii
	lda tmten
	clc
	adc #'0'
	sta (index1),y
	dey
; .y index to place for next digit
; .x index to next byte containing 2 packed
; bcd digits(rel to tmhour)
	ldx #2

gti70:	lda tmhour,x		; do low digit first
	pha
	and #$0f
	clc
	adc #'0'
	sta (index1),y
	dey			; now do high digit
	pla
	and #$70
	lsr
	lsr
	lsr
	lsr
	adc #'0'
	sta (index1),y
	dey
	dex
	bpl gti70
	lda index1
	ldy index1+1
	ldx index1+2
	jmp strlit
; -------------------------------------------------------------------------------------------------
; ti$ routines - ti$ is assigned a value from a string whose descriptor is stored in fac.
; mktime

mktime:	jsr frefac
	pha
	cmp #6
	beq mktmb
	cmp #7			; must receive 7 digits
	bne fcerr2

mktmb:	ldy #0			; inx reg for packed digits
	sty fbufpt		; inx for unpacked chars

; loop around here three times to get hours, minutes and seconds. two bcd digits are packed into
; each byte indexed by "index2" and .y. expects tmhour, tmmin, tmsec to be stored consecutively.
mkti10:	jsr timnum		; get digit
	asl			; move digit into high nibble
	asl
	asl
	asl
	sta tmhour,y
	jsr timnum		; get and pack 2nd digit
	ora tmhour,y
	sta tmhour,y
	iny
	cpy #3
	bne mkti10
	pla
	cmp #6
	beq mktmc
	jsr timnum		; get tenths
	bne mktmd 		; always

mktmc:	lda #0
mktmd:	sta tmten
; adjust 24 hr clock, setting p.m. if needed
	lda tmhour
	cmp #$12		;  hr >= 12?
	bcc mkti50
	sei
	sed			; decimal math
	sbc #$12
	cld
	cli
	ora #$80		; p.m. bit
	sta tmhour

mkti50:	lda #0			; pack 10th bit 0
	ror tmten
	ror
	ora tmsec
	pha
	lda #0			; pack 10th bit 1
	ror tmten
	ror
	ora tmmin
	tax
	lda #0			; pack 10th bits 2,3
	ror tmten
	ror
	ror tmten
	ror
	lsr
	ora tmhour
	tay
	pla
	clc			; c-clr => set time-of-day
	jmp settim		; set time as specified
; -------------------------------------------------------------------------------------------------
; timnum gets next digit
;   uses index2 as base ptr, .y<=fbufpt to get ascii digit
;   uses fbufpt+1 as temp for preserving .y
;   exit: .y unchanged
;         .a = unasciied digit value
;   fbufpt=fbufpt+1

timnum:	sty fbufpt+1
	ldy fbufpt
	inc fbufpt
	jsr sav12
	jsr qnum
	bcs fcerr2
	sbc #$2f
	ldy fbufpt+1
	rts

fcerr2:	jmp fcerr		; -> illegal quantity error, ready
; -------------------------------------------------------------------------------------------------
; garbage collection
;   get space for char string.
;   note may force garbage collection
;   entry:   ac = # of chars
;   exit:    ptr in y,x otherwise
;   blows off to 'out of string space' error
;   also preserves ac and sets frespc=
;   y,x= ->at space.
; ad53 getspa

getspa:	lsr garbfl
tryag2:	tax			; save in x also
	beq getrts		; length of 0 no go...
	pha			; save a (length) on stack
	lda fretop		; lo byte
	sec			; for subtract
	sbc #ptrsiz		; minus 3 (link bytes)
	ldy fretop+1
	bcs tryag3
	beq garbag		; make sure no wraparound to $ff page
	dey
tryag3:	sta index1		; save for later
	sty index1+1
	txa
	eor #$ff
	sec
	adc index1
	bcs tryag4
	sta tttemp		; prevent wraparound to $ff page
	tya
	beq garbag
	lda tttemp
	dey
tryag4:	cpy strend+1
	bcc garbag
	bne strfre
	cmp strend
	bcc garbag
strfre:	sta frespc
	sty frespc+1
	ldy #$02		; flag string as garb.
	lda #$ff		; garbage flag
	sta (index1),y
	dey
	sta (index1),y		; flag
	dey
	pla			; length
	sta (index1),y
	ldx frespc
	ldy frespc+1
	stx fretop
	sty fretop+1

getrts:	rts

garbag:	lda garbfl
	bmi grbg99		; if out of memory
	jsr garba2
	sec
	ror garbfl
	pla
	bne tryag2  		; always

grbg99:	jmp omerr  		; out of memory
; -------------------------------------------------------------------------------------------------
; garba2 - routine looks for and squashes out any unused string space it finds.
;   thus returning the space for future use by the string routines.
;   garba2 is called only when basic needs space of a fre instruction is used.

garba2:	lda temppt      	; get # of temporary strings
	beq naa         	; skip if none
; create a back pointer on current temporary strings
la:	pha			; save # of temps
	jsr slr1
	beq la10
	jmp la20
la10	jmp la30
la20:	tya			; .y now points to which temporary descriptor
	         		; get location of temp descriptor
	clc
	adc tempst
	ldy #0
	sta (tempf2),y		; and create a back pointer to it
	lda tempst+1
	iny
	sta (tempf2),y

la30:	pla
	sec
	sbc #4
	bne la    		; repeat if this was not last temporary
; main body of garbage collect
naa:	ldy #$00		; set up flag
	sty highds
	lda memtop		; get top of memory
	ldy memtop+1
	sta grbtop		; set both pointers
	sta grbpnt
	sta frespc
	sty grbtop+1
	sty grbpnt+1
	sty frespc+1

; do while (grbpnt <> fretop)
gloop:	jsr chkgrb		; check garbage string
	bne col01		; if not garbage string
col00a:	ldy #0			; get length
	lda (grbpnt),y
	jsr movpnt		; move grbpnt to next
	sec
	ror highds		; indicate garbage string found
	bne gloop		; always

col01:	bit highds
	bpl col03		; if garbare string not found
	ldx #$00
	stx highds		; clear indicator

; move a string over garbage
col02:	ldy #$02		; move the link bytes

col02a:	lda (grbpnt),y
	sta (grbtop),y
	dey
	bpl col02a

	jsr sav7
	txa			; put length-1 in .y
	tay

glop1:	dey
	lda (grbpnt),y
	sta (grbtop),y
	dex
	bne glop1

	ldy #$02		; fix the descriptor
col02b:	lda grbtop-1,y
	sta (index1),y
	dey
	bne col02b
	lda grbpnt		; check pointer
	ldy grbpnt+1
	jsr chkgrb		; check garbage string
	beq col00a		; if garbage string found
	bne col02		; always

col03:	jsr sav7
	jmp gloop

sav7:	ldy #0 			; skip over string body
	lda (index1),y
	tax
	jsr movtop
	sta frespc
	sty frespc+1
	txa
	jmp movpnt
; -------------------------------------------------------------------------------------------------
; adb5 subroutines used for garbage collection
;   compare for y,a = fretop.
;   entry:   y,a = address of current string descriptor.
;   exit:    exit to caller if y,a = fretop.
;   else:    z flag set if garbage string.
;            z flag clear if not garbage string.
;   in either case pointers are setup for next loop and string movement.
;   exit to cfre4.
;   carry clear y,a <> fretop.

chkgrb:	cpy fretop+1		; end of strings ?
	bcc cfre4
	bne cfre1		; if not equal
	cmp fretop
	beq cfre4
	bcc cfre4

cfre1:	bit highds		; check flag
	bmi cfre2		; if empty string found
	lda #ptrsiz		; skip pointers past
	jsr movtop		; move top pointer
cfre2:	lda #ptrsiz		; skip pointers past
	jsr movpnt		; move pointers
	ldy #$02
	lda (grbpnt),y		; garbage ?
	cmp #$ff
	bne cfre3		; if not garbage string
	rts

cfre3:	lda (grbpnt),y		; to link bytes
	sta index1,y
	dey
	bpl cfre3		; if three bytes not moved
	rts
; mark temporary strings as garbage and exit
cfre4:	lda temppt  		; get # of temporary strings
	beq naa2    		; skip if none

; mark current temporary strings as garbage
fa:	pha     		; save # of temps
	jsr slr1
	beq fa10
	jmp fa20
fa10:	jmp fa30
fa20:	lda (tempst),y 		; get string length
	ldy #0     		; set up index
	sta (tempf2),y 		; replace back pointer
	iny
	lda #$ff
	sta (tempf2),y
	iny
	sta (tempf2),y

fa30:	pla
	sec
	sbc #4
	bne fa    		; repeat if this was not last temporary

naa2:	pla			; throw away return address
	pla
	lda frespc		; fix fretop and frespc
	ldy frespc+1
	sta fretop
	sty fretop+1
	rts
	
slr1:	tay
	dey
	dey
	lda (tempst),y  	; get high byte of address
	sta tempf2+1    	; and save it
	dey
	lda (tempst),y  	; get low byte of address
	sta tempf2    		; and save it
	dey
	lda (tempst),y  	; get length of string
	pha
	clc
	adc lowds
	sta lowds
	bcc slr10
	inc lowds+1
slr10:	pla
	rts
	sta tempf2  		; rather than body of the string
	bcc slr2
	inc tempf2+1
slr2:	rts

movpnt:	eor #$ff		; comp and add
	sec
	adc grbpnt
	ldy grbpnt+1
	bcs mov00
	dey
mov00:	sta grbpnt
	sty grbpnt+1
	rts
	
movtop:	eor #$ff		; comp and add
	sec
	adc grbtop
	ldy grbtop+1
	bcs mov01
	dey
mov01:	sta grbtop
	sty grbtop+1
	rts
; -------------------------------------------------------------------------------------------------
; instrg - instring

infcer:	jmp fcerr		; -> illegal quantity error, ready

instrg:	ldx #2
incop1:	lda facmo,x		; save pointer to temporary descriptor
	sta tmpdes,x
	dex
	bpl incop1
	jsr frmevl		; get next arg.
	jsr chkstr		; must be string
	ldx #2
incop2:	lda facmo,x		; and save it, too
	sta tmpdes+3,x
	dex
	bpl incop2

	ldx #1
	stx faclo		; if no starting position is given, 1 is assumed
	jsr sav17
inst1:	jsr chkcls
	ldx faclo
	beq infcer		; s.a. of 0 is an error
	dex
	stx positn

	ldx #5			; move pointers to temp descriptors to zero page
inst2:	lda tmpdes,x
	sta ptarg1,x
	dex
	bpl inst2

	ldy #3
inst3:	lda (ptarg1),y
	sta str1,y
	dey
	bpl inst3

	ldy #3
inst4:	lda (ptarg2),y
	sta str2,y
	dey
	bpl inst4

	lda str2		; check if string 2 is null
	beq instnf		; if so, return zero

inst5:	lda #0
	sta match
	clc
	lda str2		; length of string 2
	adc positn
	bcs instnf		; too long, not found
	cmp str1		; see if > length of string 1
	bcc inst6		; < len. string 1
	bne instnf		; must be >, not found
inst6:	ldy match
	cpy str2		; if match len. = str. len, then found
	beq instfd
	tya
	clc
	adc positn		; compare string1(s+p+m) with string2(m)
	tay
	lda (str1+1),y
	sta tmppos
	ldy match
	lda (str2+1),y
	cmp tmppos
	beq inst7
	inc positn		; not the same, start over from next position
	bne inst5		; always
inst7:	inc match		; count characters that match
	bne inst6		; always

instfd:	inc positn
	lda positn
	!byte $2c
instnf:	lda #0
	pha
	lda tmpdes+3		; free temp descriptors
	ldy tmpdes+4
	ldx tmpdes+5
	jsr fretmp
	lda tmpdes
	ldy tmpdes+1
	ldx tmpdes+2
	jsr fretmp
	pla 			; send result back as an integer
	tay
	jmp sngflt
; ******************************************* DELETE **********************************************
; DELETE -  delete a range of source
;   syntax: delete from#,to#

nrange:	jmp snerr		; -> syntax error, ready

delete:	beq nrange		; bad..no range parms.
	jsr range		; get line # range
	beq nrange		; some range errors
	lda lowtr		; save it
	ldx lowtr+1
	sta index2
	stx index2+1
	jsr fndlin		; find it
	bcc del300		; skip if not found
	ldy #1
	lda (lowtr),y   	; at end of source ?
	dey
	tax			; save it in case of swap
	bne noteos
	lda (lowtr),y
	beq del300 		; both zero means end-of-source
noteos:	lda (lowtr),y
	sta lowtr		; include to line #
	stx lowtr+1
del300:	lda index2		; check from#<to#
	sec
	sbc lowtr		; gen neg delta
	tax
	lda index2+1
	sbc lowtr+1
	tay
	bcs notdel		; no good
	txa
	clc
	adc txtend		; gen new end of source
	sta txtend
	tya
	adc txtend+1
	sta txtend+1
	ldy #0
del500:	lda (lowtr),y   	; move source down
	sta (index2),y
	iny
	bne del500
	inc lowtr+1
	inc index2+1
	lda txtend+1		; done one extra page
	cmp index2+1
	bcs del500		; no
notdel:	jmp fini		; relink and say ready
; -------------------------------------------------------------------------------------------------
; range - input range parms
;   exit: z=1, if range errors

range:	bcc rng100		; from chrgot a # ?
	beq rng100		; a terminator
	cmp #tkminu		; a dash
	beq rng100
rngerr:	lda #0			; z=1, syntax error
	rts

rng100:	jsr linget		; get #
	jsr fndlin		; find or set ptrs
	jsr chrgot		; get last char
	beq rng200		; skip done
	cmp #tkminu		; a dash
	bne rngerr		; syntax error
	jsr chrget		; yes - skip dash
	jsr linget		; get to #
	bne rngerr		; not a number err
rng200:	lda linnum
	ora linnum+1		; was a # to input ?
	bne rngrts		; yes
	lda #$ff		; no - make max
	sta linnum
	sta linnum+1
rngrts:	rts			; z=0, no errors
; -------------------------------------------------------------------------------------------------
sav12:	lda (index),y
	rts
; ******************************************* BUTES1 **********************************************
; 'basic utility routines'
; stxtpt

stxtpt:	clc
	lda txttab
	adc #255
	sta txtptr
	lda txttab+1
	adc #255
	sta txtptr+1
	rts
; -------------------------------------------------------------------------------------------------
; getnum - get positive integer

getnum:	jsr getpin
; check comma
combyt:	jsr chkcom
	jmp getbyt

gtbytc:	jsr chrget

getbyt:	jsr frmnum
conint:	jsr posint
	ldx facmo
	bne fcer1
	ldx faclo
	jmp chrgot
; -------------------------------------------------------------------------------------------------
; getpin - evaluate the formula

getpin:	jsr frmnum

getadr:	lda facsgn
	bmi fcer1
	lda facexp
	cmp #145
	bcs fcer1
	jsr qint
	lda facmo
	ldy facmo+1
	sty poker
	sta poker+1
	rts

bjmps:		  		; referenced in initialization routine
fcer1:	jmp fcerr		; -> illegal quantity error, ready
; -------------------------------------------------------------------------------------------------
; these routines check for certain 'valtyp'
;   (c) is not preserved.
; frmnum

frmnum:	jsr frmevl

chknum:	clc
	!byte $24
chkstr:	sec

chkval:	bit valtyp
	bmi docstr
	bcs chkerr
chkok:	rts

docstr:	bcs chkok
chkerr:	ldx #errtm
	jmp error
; -------------------------------------------------------------------------------------------------
; strprt - print the str whose descriptor is -> by facmo

strprt:	jsr frefac		; return temp -}
	tax			; put count into counter
	ldy #0
	inx

; output x characters to device
strp2:	dex
	beq strp3
	lda (index),y
	jsr ochr		; output one char
	iny
	jmp strp2		; next
strp3:	rts
; -------------------------------------------------------------------------------------------------
; output space
ospc:	lda channl
	beq crtskp
	lda #' '
	!byte $2c		; eat next two bytes
crtskp:	lda #29
	!byte $2c		; eat next two bytes
outqst:	lda #'?'
; output one character to device
ochr:	jsr bsout		; output character
	and #$ff
	rts
; -------------------------------------------------------------------------------------------------
; 'dos utility routines.' -mgm 7/23/79-
;   this is the dos parser routine which looks at lines passed to it and varifies that the syntax
;   is proper.
;   entry (dosprs): a = parsts bit which must be zero.
;   exit:    a = parsts as follows
;
; i-i-i-i-i-i-i-i-i
; i7+6+5+4+3+2+1+0i
; i-i-i-i-i-i-i-i-i
;  ^ ^ ^ ^ ^ ^ ^ ^ ====>  fn1 this bit is set when the
;  | | | | | | | |             first filename is parsed
;  | | | | | | | ======>  fn2 set for second filename
;  | | | | | |=========>  la set when #lfn parsed
;  | | | | |===========>  fa set for device number
;  | | | | ============>  d1 set for first disk unit
;  | | | ==============>  d2 set for second disk unit
;  | | ================>  dosrcl set for record size
;  | ==================>  @ set when @ encountered.
;
; the following are the vaild bit patterns for parsts after parsing for the various keywords
;
;      7 6 5 4   3 2 1 0
; (format)
;  header   0 0 0 1   * 0 0 1
;  colect   0 0 0 *   * 0 0 0
;  backup   0 0 1 1   * 0 0 0
;  copy     0 0 1 1   * 0 0 0
;    or..   0 0 * *   * 0 1 1
;  concat   0 0 * *   * 0 1 1
;  bsave    * 0 0 *   * 0 0 1
;  dsave    * 0 0 *   * 0 0 1
;  bload    0 0 0 *   * 0 0 1
;  dload    0 0 0 *   * 0 0 1
;  catlog   0 0 0 *   * 0 0 *
;  rename   0 0 0 *   * 0 1 1
;  append   0 0 0 *   * 1 0 1
;  scrtch   0 0 0 *   * 0 0 1
;  dopen    * * 0 *   * 1 0 1
;  dclose   0 0 0 0   * * 0 0
;      ^ ^ ^ ^   ^ ^ ^ ^
;      @ l d d   f l f f
;      r r 2 1   a a n n
;      p e           2 1
;      l l
;
;    "0" bits are required to be clear.
;    "1" bits are required to be set.
;    "*" bits are optional parameters.
;
; entry (dosprs):  parstx shall be set to prevent any auxiliary options to be specified.
; entry (dosprx):  x = parstx bits whic must be zero.
; exit: x = parstx as follows
;
; i-i-i-i-i-i-i-i-i
; i7+6+5+4+3+2+1+0i
; i-i-i-i-i-i-i-i-i
;  ^ ^ ^ ^ ^ ^ ^ ^ ====>  bnk is set for bank option
;  < < < < < < < ======>  offl set for 1st address
;  < < < < < <=========>  offh set for 2nd address
;  < < < < <===========>  unused
;  < < < < ============>  unused
;  < < < = ============>  unused
;  < < = = ============>  unused
;  < = = = ============>  unused

; the following are the vaild bit patterns for parstx after parsing for the various keywords
; only two stmts are allowed bits set in parstx.

;      7 6 5 4   3 2 1 0
; (format)
;  bsave    0 0 0 0   0 * * *
;  bload    0 0 0 0   0 0 * *
;      ^ ^ ^ ^   ^ ^ ^ ^
;      ? ? ? ?   ? o o b
;                  f f n
;                  h l k
;    "0" bits are required to be clear.
;    "1" bits are required to be set.
;    "*" bits are optional parameters.

dostbl:	!byte $ff,$ff,$ff,$ff,doslfn,dosdsk,$6f

; dospar

dospar:	lda #0
; special error flag entry
dosprs:	ldx    #$ff		; no aux options!
; spec aux error flag entry
dosprx:	pha			; save error flags
	txa
	pha
	lda #0
	sta parsts
	sta parstx

	ldx #dosspc-1		; clear dos scratch area
dos01:	sta fbuffr,x
	dex
	bne dos01
	ldx #dossa-dosofl	; set some defaults from table
dos02:	lda dostbl,x
	sta dosofl,x
	dex
	bpl dos02
	lda ddisk		; get default drive
	ldx #5			; disdsk
	sta dosofl,x		; store as default (overwrite fixed table value)
	ldx dfbank
	stx dosbnk

	jsr chrgot		; get current chr
	bne parse1		; if not end of statement

done:	pla			; get aux error flag
	and parstx		;  repeated,illegal params?
	bne dn20
	pla			; get error flags
	jsr prmrpt
	lda parsts
	ldx parstx
	rts

parse1:	cmp #'#'
	beq logadr		; if logical file number
	cmp #'w'
	beq reclen		; if record length
	cmp #'l'
	beq reclen		; if record length
	cmp #'r'
	bne dos5		; if not ????
	jsr chrget		; move on
	jmp delim1

on1:	jsr on
sav60:	jmp del1

unit1:	jsr unit		; do unit# parsing
	bne sav60    		; always

bank1:	jsr bank
	beq sav60    		; always

dos5:	cmp #'d'
	beq drv1
	cmp #tkon		; "on" token
	beq on1
	cmp #'b'
	beq bank1
	cmp #'u'
	beq unit1
	cmp #'p'
	beq doffl
	cmp #'i'
	bne dos10		; if not identifier
	beq ident
	
logadr:	lda #4
	jsr prmrpt		; check for repeated parameter
	jsr getval
	cpx #0
	beq qtyer2		; if illegal value
	stx dosla
	lda #4			; set logical address flag
	bne sav60		; get next parameter
dn20:	jmp snerr
	
reclen:	tax			; save char
	lda #$40
	jsr prmrpt		; check for repeated parameter
	cpx #'w'
	bne recoo
	jsr chrget
	jmp recon		; set parsts
recoo:	jsr getval
	cpx #0
	beq qtyer2		; zero illegal dosrcl
	cpx #255
	beq qtyer2		; illegal dosrcl
	stx dosrcl		; store parcel

recon:	lda #$40		; set dosrcl flag &
	bne tacky1

dos10:	cmp #$22;"
	beq name1
	cmp #'('
	beq name1
	bne dn20

drv1:	lda #$10
	jsr prmrpt		; check for repeated parameter
	jsr getval
	cpx #2
	bcs qtyer2		; illegal drv# if >1
	stx dosds1
	stx dosds2
	lda #$10
tacky1:	bne del1

qtyer2:	jmp qtyerr
	
ident:	lda didchk
	beq idcon		; only 1 dosdid allowed
	bne dn20

doffl:	lda #$02		; chk aux status
	jsr prxrpt
	jsr getoff		; get offset value
	sty dosofl
	sta dosofl+1
	lda #$02
dlimx1:	ora parstx		; set aux status bits
	sta parstx
	bne delim1		; try for nxt param

doffh:	lda #$04
	jsr prxrpt
	jsr getoff
	sty dosofh
	sta dosofh+1
	lda #$04
	bne dlimx1		; set aux status

idcon:	jsr chrget		; get next character
	sta dosdid		; m(txtptr => dosdid
	jsr chrget
	sta dosdid+1
	lda #$ff
	sta didchk		; set dosdid flag &
	jsr chrget		; continue
	jmp delim1
	
name1:	lda #1			; name1 allwd only once
	jsr newnam		; do name parsing
	sta dosf1l
	sta xcnt		; save temp value
	lda #<fbuffr
	sta dosf1a
	lda #>fbuffr
	sta dosf1a+1

	ldy #0
loop6:	lda (index),y
	sta fbuffr,y		;  set char in buffer
	iny
	cpy xcnt
	bcc loop6		; if not full name
	lda #1			; set name1 flag

del1:	ora parsts
	sta parsts

delim1:	jsr chrgot
	bne nxxx
done1:	jmp done		; <cr>/<>  => done

next6:	cmp #tkon
	bne next6a
	jmp on1
next6a:	cmp #tkto		; "to" token
	beq next6b
	bne sav61		; sntax error

; if "to" is not followed by an offset param, then do file2 params. otherwise, do high
; offset and continue with file0 options.
next6b:	jsr chrget
	cmp #'p'
	bne pars22
	beq doffh

nxxx:	cmp #','
	bne next6
	jsr chrget
	jmp parse1

parse2:	jsr chrget
pars22:	cmp #'d'
	beq drv2
	cmp #tkon		; "on" token
	beq on2
	cmp #'u'
	beq unit2
	cmp #$22;"
	beq name2
	cmp #'('
	beq name2

drv2:	lda #$20
	jsr prmrpt		; check for repeated parameter
	jsr getval
	cpx #2
	bcs qtyerr		; illegal drive #
	stx dosds2
	lda #$20
	bne del2

on2:	jsr on
	jmp del2

unit2:	jsr unit		; do unit# parsing
	bne del2  		; always

name2:	lda #2			; name2 allowed only once
	jsr newnam
	sta dosf2l
	stx dosf2a
	sty dosf2a+1
	lda #2			; set filename2 flag &

del2:	ora parsts		; set flag in status
	sta parsts
	jsr chrgot
	beq done1		; done on <cr>/<}
	cmp #','
	beq parse2
	cmp #tkon		; "on" token
	beq on2
	cmp #'u'
	beq unit2

sav61:	bne sner
	
qtyerr:	jmp fcerr		; "illegal quantity"

on:	jsr chrget
	cmp #'u'
	beq unit		; if character is a "u"
	cmp #'b'
	beq bank
	bne sner

unit:	jsr getval
	cpx #32
	bcs qtyerr		; error if >31
	cpx #3
	bcc qtyerr		; error if <3
	stx dosfa
	lda #8
	rts

bank:	lda #$01		; repeated param?
	jsr prxrpt
	jsr getval
	cpx #mxbank		; bank too large?
	bcs qtyerr
	stx dosbnk
	lda #$01
	ora parstx		; set bnk bit in aux status
	sta parstx
	lda #0			;  .a=std status wrd, no bits to set
	rts

sner:	jmp snerr		; jump syntax error

newnam:	jsr prmrpt		; check for repeated parameter
	jsr sav13
	tax		; save length of string
	beq qtyerr		; if length = 0
	ldy #0
	jsr sav12
	cmp #'@'
	bne lenchk
	lda #$80
	jsr prmrpt
	lda parsts
	ora #$80		; set "@" flag
	sta parsts
	dex			; decrement length
	inc index1		; increment past "@"
	bne lenchk		; if no carry
	inc index1+1

lenchk:	txa
	cmp #17
	bcs errlen		; if length < 17
	ldx index1
	ldy index1+1
	rts

errlen:	ldx #errls
	jmp error		; filename too long
; get nextvalue routine
getval:	jsr chrget		; get nxt chr
gtvl2:	beq sner		; if end of statement
	bcc gtvl5		; can be numeric
	jsr chkopn		; or a "("
	jsr getbyt		; anything else is an error
	jmp chkcls		; need closing ")"

gtvl5:	jmp getbyt		; evaluate it

; get next 2byte expr routine
;   exit:  .a,.y (high,low) value
getoff:	jsr chrget		; get nxt chr
	beq sner		; if end of statement
	bcc gtff5		; can be num. const
	jsr chkopn		; or a "("
	jsr getpin		; expr
	jsr chkcls		; need closing ")"
	ldy poker
	lda poker+1
	rts

gtff5:	jmp getpin		; evaluate it
	
; prmrpt  checks for a repeated parameter.
;   entry   a contains parsts flag to check.
prmrpt:	and parsts		; and with parsts
	bne sner		; if bit previously set
	rts

; prxrpt  checks for a repeated parameter.
;   entry   a contains parstx flag to check.
prxrpt:	and parstx		; and with parstx
	bne sner		; if bit previously set
	rts
; ******************************************* BUTES2 **********************************************
; cbm 2001 disk verb processors - author rsr 7-24-79
; tabfcb - table of fcb processing strings.

tabfcb:
tclr=*-tabfcb
	!byte <fclrm1		; clear
tcat=*-tabfcb
	!byte fcat-1		; catalog
topn=*-tabfcb
	!byte fopn-1		; dopen dsave dload
tapn=*-tabfcb
	!byte fapn-1		; append
thed=*-tabfcb
	!byte fhed-1		; header
tcoll=*-tabfcb
	!byte fcoll-1		; collect
tbak=*-tabfcb
	!byte fbak-1		; backup
tcopy=*-tabfcb
	!byte fcopy-1		; copy
tconc=*-tabfcb
	!byte fconc-1		; concat
tren=*-tabfcb
	!byte fren-1		; rename
tscr=*-tabfcb
	!byte fscr-1		; scratch
trec=*-tabfcb
	!byte frec-1		; record

xsca =$c2		; send dossca
xid =$d0		; sends disk id
xd1 =$d1		; sends dosds1
xd2 =$d2		; sends dosds2
xrec =$e0		; sends s for seq or dosrcl
xwrt =$e1		; sends w or l
xrcl =$e2		; send low ((poker))
xfat =$f0		; sends "@" if specified
xfn1 =$f1		; sends filename1
xfn2 =$f2		; sends filename2

; tabld - token table definitions

tabld				; used to build disk command strings
fclr =*-tabld
fclrm1 =fclr-1
	!byte 'i',xd1
fcat =*-tabld
	!byte '$',xd1,':',xfn1

fopn =*-tabld
	!byte xfat,xd1,':',xfn1,',',xwrt,',',xrec

fconc =*-tabld
	!byte 'c',xd2,':',xfn2,'=',xd2,':',xfn2,','
fapn =*-tabld
	!byte xd1,':',xfn1,',','a'

fhed =*-tabld
	!byte 'n',xd1,':',xfn1,',',xid

fcoll =*-tabld
	!byte 'v',xd1

fbak =*-tabld
	!byte 'd',xd2,'=',xd1

fcopy =*-tabld
	!byte 'c',xd2,':',xfn2,'=',xd1,':',xfn1

fren =*-tabld
	!byte 'r',xd1,':',xfn2,'=',xd1,':',xfn1

fscr =*-tabld
	!byte 's',xd1,':',xfn1

frec =*-tabld
	!byte 'p',xsca,xrcl,xrec
; -------------------------------------------------------------------------------------------------
; send parameters to device
;   entry  a number of bytes in format.
;   y --> tabld entry.

sav20:	jsr chk2
	ldy #topn		; table offset
sav21:	lda #4			; length

sendp:	sta xcnt		; save number of string bytes
	lda tabfcb,y		; get pointer into tabld
	pha
	jsr oldclr		; clear old status

	ldx #0

sdp1:	pla
	dec xcnt
	bmi tranr
	tay
	iny			; move down table
	tya
	pha
	lda tabld,y		; get next entry
	bpl sdp5		; if not escape code
	cmp #xsca		; if not secondary address
	beq rsca
	cmp #xid
	beq rid			; if disk id
	cmp #xrcl
	beq rdcn		; if record number
	cmp #xwrt
	beq rwrt		; if w or l
	cmp #xfat
	beq rfat		; if "@" symbol request
	cmp #xfn1
	beq rsfn		; if filename 1
	cmp #xfn2
	beq gordfn		; if filename 2
	cmp #xrec
	bne sdp2		; if not record type
	lda dosrcl		; get rec #
	bne sdp5		; always branch
sdp2:	cmp #xd1
	bne sdp3		; if not drive 1
	lda dosds1
	bpl sdp4		; always branch
sdp3:	cmp #xd2
	bne sdp1		; if not drive 2, continue
	lda dosds2
sdp4:	ora #'0' 		; change # to ascii

sdp5:	sta dosstr,x		; else into buffer
	inx
	bne sdp1		; always

gordfn:	beq rdfn

tranr:	txa			; length to a
	pha
	ldx #<dosstr		; set filename
	ldy #>dosstr
	stx highds		;  set up vector for filename
	sty highds+1
	jsr sav3
	pla
	rts

rsca:	lda dossa		; secondary address (record)
	bne sdp5		; always

rfat:	bit parsts
	bmi rfata
	bpl sdp1		; if @ not encountered
rfata:	lda #'@'
	bne sdp5		; always

; id subroutine
rid:	lda dosdid		; include id
	sta dosstr,x
	inx
	lda dosdid+1
	bne sdp5		; always

rwrt:	lda dosrcl		; chk for l or w
	beq rwrt1		; zero then write
	lda #'l'
	bne sdp5		; always

rwrt1:	lda #'s'		; send w,s
	sta dosrcl
	lda #'w'
	bne sdp5		; always

; move record number
rdcn:	lda poker
	sta dosstr,x
	lda poker+1
	inx
	bne sdp5		; always

; move file names.
rsfn:	lda dosf1a
	sta index1
	lda dosf1a+1
	sta index1+1
	ldy dosf1l
	beq rdrt0		; if null string
	bne xrfn		; always

rdfn:	lda dosf2a
	sta index1
	lda dosf2a+1
	sta index1+1
	ldy dosf2l
	beq rdrt0		; if null string

xrfn:	sty count
	ldy #0			; move name to dosstr
rdmov:	lda (index1),y
	sta dosstr,x
	inx
	iny
	cpy count
	bne rdmov		; if move not complete
	!byte $24		; hop
rdrt0:	dex			; case cdd=sd

rdrt1:	jmp sdp1		; get next symbol
; -------------------------------------------------------------------------------------------------
; syntax checker
;   routines for dos.write

chk1:	and #$e6		; for header,dload,scrtch
	beq chk2		; chk opt parms
chker1:	jmp snerr		; -> syntax error, ready

chk2:	lda parsts		; for dsave
	and #1
	cmp #1			; chk req'd parms
	bne chker1		; error if 1 missing
	lda parsts		; reload for return
	rts

chk3:	and #$e7		; for colect
	bne chker1		; chk opt parms
	rts

chk4:	and #$c4		; for copy,concat
	bne chker1		; chk opt parms
	lda parsts
chk5:	and #3			; for rename
	cmp #3			; chk req'd parms
	bne chker1
	lda parsts		; reload for return
	rts

chk6:	and #5			; for append,dopen
	cmp #5			; chk req'd parms
	bne chker1
	lda parsts		; reload for rts
	rts
; -------------------------------------------------------------------------------------------------
; error on channel read

errchl:	lda #0			; no filename
	jsr setnam
	ldy #$6f		; .y=sa
	jsr ochanl		; open special channel
	ldx #doslfn		; make it the input channel
	jsr chkin
	ldy #$ff		; for offset

loop1:	iny
	jsr basin
	cmp #cr			; check for end
	beq errend
	sta (dsdesc+1),y
	cpy #39
	bne loop1
errend lda #00
	sta (dsdesc+1),y
	lda #40			; get 40 char str
	sta dsdesc		; we have 40 chrs
	jsr clrch
	lda #doslfn		; close channel and restore default chnls
	clc
	jsr tclose
	jmp dcat0		; restore default channel ...

; r-u-sure subroutine
rusure:	jsr tstdir		; chk for direct
	bne ansyes		; z clr=not direct
	ldx #msg30
	jsr msg			; prompt user
	jsr clrch		; clear channel for basin
	jsr basin		; next char
	cmp #'y'
	bne ansno		; if 1st <> 'y'
	jsr basin		; next chr
	cmp #cr
	beq ansyes		; if short form of yes (y,cr)
	cmp #'e'
	bne ansno		; if not 'e'
	jsr basin
	cmp #'s'
	bne ansno		; if not 's'
	jsr basin
	cmp #cr
	beq ansyes		; if 'yes',cr

; if not yes, input until cr received
ansno:	cmp #cr
	sec			; carry set =no&direct
	beq ansbye		; if cr received, exit
	jsr basin
	bne ansno		; continue to ignore

; here if answer 'yes'
ansyes:	clc			; carry clr =not direct
ansbye:	rts

; oldclr subroutine
;   clears ds$: set string length to zero.
;   clears st
oldclr:	lda #0
	sta dsdesc		; kill ds$
	clc
	jmp storst		; kill st

sav77:	jsr sav13
	sta dosf1l
	lda index1		; save address of string
	ldy index1+1
	sta dosf1a
	sty dosf1a+1
	rts
; -------------------------------------------------------------------------------------------------
; parser for load,save and verify verbs
;   (file name option)
;   (opt device #) dflt=DEFDEV
;   (eot cmd) dflt=0=no

plsv:	ldx #0
	stx dosf1l		; set file name length to zero
	stx dossa		; set secondary address to zero
	stx dosla		; clear logical address
	ldx ddisk		; default device
	stx dosfa		; set physical address to 8 (disk)

	jsr chrgot
	beq plsvx		; if no paramters
	jsr sav77

	jsr plsv27
	stx dosfa		; save device channel

	jsr plsv27
	stx dossa		; set secondary address

plsvx:	lda dosf1a
	ldx dosf1a+1
	sta highds		; set up vector to filename
	stx highds+1
	lda dosf1l		; .a=filename length
sav3:	ldx #<highds		; page 0 addr of vector
	jsr setnam
	lda dosla		; set logical file info
	ldx dosfa
	ldy dossa
	jmp setlfs

plsv27:	jsr chrgot
	beq plsvx
	jmp copg

plsv30:	jsr chkcom
plsv32:	jsr chrgot
	bne plsrts		; if not end of statement
	jmp snerr		; -> syntax error, ready

plsrts:	rts

flpint:	jsr ayint
	lda facmo
	ldy faclo
	rts
; -------------------------------------------------------------------------------------------------
; 'chrget routine'
;   chrget is called to get the next character from the text buffer pointed to by txtptr which is
;   incremented before the access is made.
;
;   chrgot is called to get the current character from the buffer pointed to by txtptr.

;   qnum is called to test for a ascii numeric character.
;   entry:  txtptr points to the buffer.
;   exit:  .a = character from buffer.
;          .y = 0
;   txtptr is updated (chrget).
;   c bit = 0, character in .a is numeric.
;         = 1, character in .a is non-numeric.
;   z bit = 0, the not end of statement.
;         = 1, end of statement "" or end of line.

; chrget
chrget:	jmp (ichrge)

; chrgot
chrgot:	jmp (ichrgo)

nchrge:	inc txtptr		; increment text pointer
	bne nchrgo		; if no carry into msb
	inc txtptr+1

nchrgo:	ldy #0
	lda (txtptr),y
	cmp #$20		; span blanks
	beq chrget		; if blank character

; if the character in .a is numeric then the following code clears the carry bit.
qnum:	cmp #':'
	bcs qnrts
	sbc #$2f		; '0'-1
	sec
	sbc #$d0		; $100-'0'
qnrts:	rts
; ******************************************** INIT ***********************************************
; 'init'
; initop - set variabales based upon top of memory.
;
;   for the 64k conversion, several variables have been allocated at the top of user memory and the
;   top of the string area is set below them. these areas are:
;   input buffer
;   ds$ string space
;   temporary descriptor stack
;   random seed
;   "zero" value
;   for all other versions, the input buffer is allocated at the top of the text bank and the other
;   items are put at the top of the string bank.
;   note, the highest usable location is assumed to be identical in the two segments; after
;   allocation here, the highest usable location in text is pointed to by buffpt and memtop in the
;   string bank.

initop:	jsr clall		; restore default i/o channels
	sec
	jsr topmem		; get top of ram
	stx highst		; save highest offset
	sty highst+1

	sec
	txa
	sbc #<bufsiz
	sta buffpt
	tax
	tya
	sbc #>bufsiz
	sta buffpt+1
	tay
	sec
	txa
	sbc #40+ptrsiz		; 40 char ds$ string
	sta dsdesc+1
	tax
	bcs itp04
	dey
itp04:	sty dsdesc+2

	sec			; 3 temp descriptors
	txa
	sbc #strsiz+strsiz+strsiz
	sta tempst
	tax
	bcs itp03
	dey
itp03:	sty tempst+1

	sec
	txa
	sbc #7			;  5 seed, 1 comma, 1 unused byte
	sta seedpt
	tax
	bcs itp02
	dey
itp02:	sty seedpt+1

	sec
	txa
	sbc #strsiz		; "zero" descriptor
	tax
	bcs itp06
	dey

itp06:	stx memtop		; top of str bank
	sty memtop+1

; set some banks
	sta memtop+2
	sta dsdesc+3

	ldy #6
itp01:	lda rseed-1,y		; move seed to ram
	sta (seedpt),y
	dey
	bne itp01		; if move not complete

	tya			; zero out "zero", .a<-0
	ldx #strsiz
itp05:	sta (memtop),y
	iny
	dex
	bne itp05

	lda #40 		; do ds$ link bytes
	tay
	sta (dsdesc+1),y	; length
	iny
	lda #$ff		; garbage bytes
	sta (dsdesc+1),y
	iny
	sta (dsdesc+1),y

settop:	lda memtop		; reset top of strings
	ldy memtop+1
	sta fretop
	sty fretop+1
	rts

rseed:	!byte $80,$4f,$c7,$52,$58,','
; -------------------------------------------------------------------------------------------------
; init - this initializes the basic interpreter and should
;   be located where it will be wiped out in ram if code is all in ram.

init:	jsr initv		; init vectors
; always init0 after vector initialization
init0:	ldx #2			; init usr
init05:	lda bjmps,x
	sta usrpok,x
	dex
	bpl init05
	sta jmper		; always a jmp op

; move small routines to page 0
	ldx #initl		; length of routine
init10:	lda initat-1,x
	sta ldaabs-1,x		; move to ram
	dex
	bne init10		; if move not complete

	stx bits		; clear cells
	stx channl
	stx dsdesc
	stx dolu
	stx temppt
	dex
	stx lastpt
	stx lastpt+1

; set pointers to bottom of dynamic areas. (only for areas which start in a new bank.
; bottom and top limits will be the same in each bank.)
	sec			; get bottom of memory
	jsr membot
	stx txttab		; save for text entry
	sty txttab+1
	jsr initop

; set text pointer and output signon message
	ldy #0
	tya
	sta (txttab),y
	inc txttab
	bne init20
	inc txttab+1

init20:	lda #TITLECOL
	sta color
	ldx #asigon
	jsr msg			; output cbm sign-on

	lda #BYTESCOL
	sta color

	lda memtop		; calc free memory
	sec
	sbc txttab
	tax
	lda memtop+1
	sbc txttab+1
	jsr linprt		; print

	lda #TITLE2COL
	sta color

	ldx #awords
	jsr msg			; output bytes free, (c)

	lda #TEXTCOL
	sta color
	
	jsr logo		; print logo

	jsr scrtch		; set variables
	ldx #<warm 		; fix for bob russell
	ldy #>warm
	jsr vreset
	lda #8			; set disk default to 8
	sta ddisk
	cli
	jmp ready		; indicate ready, and start
; -------------------------------------------------------------------------------------------------
; signon

signon:	!scr clrscr,"*** LC256  COMMODORE BASIC V4.0 PLUS ***",cr,cr,0
words:	!scr " BASIC BYTES FREE (C)  2024 VOSSI",cr,cr,0

bvtrs:	!word nerror,nmain,ncrnch,nqplop,ngone
	!word neval,nfrmev,nchrgo,nchrge,flpint,givayf

initv:	ldx #initv-bvtrs-1		; init vectors

initv5:	lda bvtrs,x
	sta ierror,x
	dex
	bpl initv5
	rts
; -------------------------------------------------------------------------------------------------
; initat
;   this code is moved to page 0 ram during initialization.
;   this code gets altered (ldaadr, staadr) throughout execution. it is very fast this way.

initat:
	lda memend
	rts
initl =*-initat		; length of move
; -------------------------------------------------------------------------------------------------
; warm entry point
;   do some i/o initialization

warm:	sei			; disable interrupts
	jsr ioinit
	jsr clall
	jsr cint		; screen init
	cli			; enable ints
	jmp ready
; ******************************************* SYSCAL **********************************************
; 'interface system calls.'

readst:	sec

storst:	jmp krdst		; return i/o status byte

open:	clc
opn10:	jsr kopen		; open logical file
	bcs ioerr
	rts

getin:	jsr kgetin		; get a byte
	bcs ioerr
	rts

basin:	jsr kbasin		; get a byte
	bcs ioerr
	rts

bsout:	jsr kbsout		; send a byte
	bcs ioerr
	rts

chkin:	jsr kchkin		; open channel in
	bcs ioerr
	rts

chkout:	jsr kchout		; open   -     out
	bcs ioerr
	rts

load:	jsr kload		; load from file
	bcs ioerr
	rts

save:	jsr ksave		; save to    -
	bcs ioerr
logox:	rts

close:	sec
tclose:	jmp kclose

clall:	clc			; c-clr => close 'em all
	jmp kclall		; close all files

; ioerr - .a = error number
;   cy = 1
;   print error message and go to ready.
ioerr:	clc
	rol
	pha
	lda #doslfn		; close cmd channel in case
	clc
	jsr tclose
	pla
	tax
	jmp error
endprg:
; -------------------------------------------------------------------------------------------------
; print logo
logo:	bit mode
	bpl logox			; logo only in mode 7 / 256 colors
	
	lda #24
	sta x0				; store x
	lda #0
	sta y0				; store y
	sta temp1			; old bank
	lda #<(logodata)
	sta source_pointer		; store source address
	lda #>(logodata)
	sta source_pointer+1
	jmp imglogo
; -------------------------------------------------------------------------------------------------
; select default disk unit
dunit:
	jsr Getbyte			; get byte parameter
	cmp #8				; range 8-31
	bcc Verror1			; too small
	cmp #32
	bcs Verror1			; too large
	sta ddisk			; store new default unit
	rts
; ****************************************** ZONE VDP *********************************************
!zone vdp
; enable/disable screen, set backdrop color
;   parameter 1: 1/0= on/off +2= PAL +4= mode 7, +8 YJK (only mode 7), parameter 2: backdrop color
VDPMode:
	jsr Getbyte			; get byte parameter
	cmp #15+1
	bcs Verror1			; >15 -> error
	sta temp1			; remember
	and #$0c			; isolate yjk and mode bits
	cmp #$08
	beq Verror1			; yjk in mode 6 not allowed -> error

	jsr chkcom			; check comma
	jsr Getbyte			; get byte
	tax				; remember color
	lda temp1			; get new mode
	and #$04			; isolate graphics 6/7 bit
	bne vmcolor			; -> skip mode7
	cpx #COLORS6
	bcs Verror1			; >colors -> error
vmcolor:txa				; get color
	ldy temp1			; get mode
	jmp vmode
; -------------------------------------------------------------------------------------------------
; select draw/text color
VDPColor:
	jsr Getbyte			; get byte parameter
	bit mode
	bmi +				; -> skip mode7
	cmp #COLORS6
	bcs Verror1			; >colors -> error
+	sta color			; store new color
	rts
; -------------------------------------------------------------------------------------------------
; clear screen with backgroundcolor
VDPClear:
	jsr Getbyte			; get byte parameter

	bit mode
	bmi +				; -> skip mode7
; mode6
	cmp #COLORS6
	bcs Verror1			; >colors -> error
	sta bgcolor			; remember 4bit color
	asl				; shift to high nibble for left pixel
	asl
	asl
	asl
	sta bgcolor_left		; store left color for print subs
	ora bgcolor			; add color for right pixel
	sta bgcolor_del
	jmp vclear
+	sta bgcolor			; store new background color
	sta bgcolor_del
	jmp vclear
; -------------------------------------------------------------------------------------------------
; set dot x, y (preserves x, y)
VDPDot:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror1			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror1			; ..too large
	sta y0				; store y

	jmp SetPixel			; calc pixel address in VRAM
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror1:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; set graphics cursor x, y
VDPSet:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror1			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror1			; ..too large
	sta y0				; store y
	rts
; -------------------------------------------------------------------------------------------------
; line x, y to x1, y1 (x, y is set to target)
VDPLine:
	jsr Getint			; get 16bit integer
	lda inth
	cmp sizex+1
	bcs Verror1			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror1			; ..too large
	sta y0				; store y
; x1
	jsr chkcom
; draw line to x1, y1 (x, y is set to target)
VDPDraw:
	jsr Getint			; get 16bit integer
	lda inth
	cmp sizex+1
	bcs Verror1			; hi too large -> error
	sta x1+1
	lda intl
	sta x1				; store x1
; y1
	jsr chkcom
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror1			; ..too large
	sta y1				; store y1

vdraw:	jmp DrawLine			; draw line sub
; -------------------------------------------------------------------------------------------------
; frame x, y (lower left) to x1, y1 (upper right)
VDPFrame:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror1			; hi too large -> error
	sta x0+1			; store x
	sta temp2			; remember x0
	lda intl
	sta x0
	sta temp1
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror2			; ..too large
	sta y0				; store y
; x1
	jsr chkcom
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror2			; hi too large -> error
	sta x1+1			; store x1
	sta wtemp+1			; remember x1
	lda intl
	sta x1
	sta wtemp
; y1
	jsr chkcom
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror2			; ..too large
	sta y1				; store y1
	sta temp3			; remember y1
; check x < x1
	lda x1+1
	cmp x0+1
	bcc Verror2			; x1 hi < x0 hi -> error
	bne vfchky			; x1 hi > x0 hi -> ok
; hi equal - compare lo
	lda x1
	cmp x0
	bcc Verror2			; x1 > x -> error
; check y1 < y
vfchky:	lda y0
	pha				; remember y0
	cmp y1
	bcc Verror2			; y > y1 -> error
; draw
	lda y0
	sta y1
	jsr DrawLine			; draw ine sub
	lda wtemp			; get original x1
	sta x1
	lda wtemp+1
	sta x1+1
	lda temp3			; get original y1
	sta y1
	jsr DrawLine			; draw line sub
	lda temp1			; get original x0
	sta x1
	lda temp2
	sta x1+1
	lda temp3			; get original y1
	sta y1
	jsr DrawLine			; draw line sub
	lda temp1			; get original x0
	sta x1
	lda temp2
	sta x1+1
	pla				; get original y0
	sta y1
	jsr DrawLine			; draw line sub
	rts
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror2:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; block x, y (lower left) size dx, dy (dx, dy > 0) (mode 6 only even x size used!)
VDPBlock:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror2			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror2			; ..too large
	sta y0				; store y
; dx
	jsr chkcom
 	jsr Getint			; get 16bit parameter
	lda inth
	clc
	adc intl
	beq Verror2			; 0 not allowed -> error
	lda inth

	sta dx+1
	cmp sizex+1
	bcc vbdxok			; hi < sizex hi-> ok
	bne Verror2			; hi..too large -> error
	lda intl
	cmp sizex			; check lo
	bcc vbdxok			; lo < -> ok
	bne Verror2			; lo..too large -> error
vbdxok:	lda intl
	sta dx
; dy
	jsr chkcom
	jsr Getbyte			; get next byte
	beq Verror2			; 0 not allowed -> error
	cmp #SIZEY+1
	bcs Verror2			; ..too large
	sta dy
; check dx
	lda dx
	sec
	sbc #1				; calc dx - 1
	tax
	lda dx+1
	sbc #0
	tay
	txa
	clc
	adc x0				; calc x + dx
	tax
	tya
	adc x0+1
	cmp sizex+1
	bcs Verror3			; hi too large -> error
; calc upper left y, check dy
	lda y0
	clc
	adc #1				; calc y1 = y + 1 - dy
	sec				;         = y - (dy - 1)
	sbc dy
	bcc Verror3			; < 0 -> error
	sta y1

	+VdpCommand zero, zero, zero, x0, x0+1, y1, dx, dx+1, dy, color, zero, HMMV
	jmp waitcmd			; wait for command execution
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror3:jmp fcerr			; -> illegal quantity error
Verrors:jmp snerr			; -> syntax error
; -------------------------------------------------------------------------------------------------
; copy image to x, y from address, bank (size dx, dy of image in first two bytes)
; only in mode 7 available!
VDPImage:
	bit mode
	bpl Verrors			; in mode 6 invalid -> syntax error
	
	jsr Getbyte			; get byte parameter
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror3			; ..too large
	sta y0				; store y
; address
	jsr chkcom
	jsr Getint
	lda intl
	sta source_pointer		; store source address
	lda inth
	sta source_pointer+1
; bank
	jsr chkcom
	jsr Getbyte			; get byte
	cmp #mxbank
	bcs Verror3			; bank > max bank -> error
	sta temp2			; remember

	lda via2+prb			; get MMU reg
	and #%00001111			; isolate RAM0 bank
	sta temp1			; remember
	lda #%11110000			; clear RAM0 bank bits# 0-3
	and via2+prb
	ora temp2
	sta via2+prb 			; set RAM bank

imglogo:ldy #0				; x counter
	sty dx+1			; clear dx high
	lda (source_pointer),y		; load size x
	sta dx
	bne vimgdy
	lda #$01
	sta dx+1
vimgdy:	iny
	lda (source_pointer),y		; load size y
; calc y1
	sec
	sbc #1				; y1 = img size y - 1 + y
	clc
	adc y0
	bcs Verror3			; >255 -> error
	cmp #SIZEY
	bcs Verror3			; too large -> error
	sta y1

	lda source_pointer
	clc
	adc #2				; add 2 for first data byte
	sta source_pointer
	lda source_pointer+1
	adc #0
	sta source_pointer+1
; calc vram address
vimgylp:lda y0
	and #$c0			; isolate bit 6+7 (16k bank)
	cmp lastvbank			; same bank like last time ?
	beq vimgadr			; ..yes -> skip set bank
	sta lastvbank			; remember bank
; set bank
	asl				; address bits 14+15 -> bit 0+1
	rol
	rol
	+VdpSetReg 14			; set VRAM bank
; set address
vimgadr:ldy color
	lda y0				; get y again
	and #$3f			; remove VRAM address bit 14+15
	ldx x0
	+VdpWait WAIT23,16
	+VdpWriteAddress
	+VdpWait WAITVRAM1,10

	ldy #0				; x counter
vimglp:	lda (source_pointer),y		; load data byte
	sta VDPRamWrite			; write byte to VRAM
	iny
	cpy dx				; dx reached ?
	+VdpWait WAITVRAM,16
	bne vimglp			; ..no -> next byte

	ldy y0
	cpy y1				; last line reached ?
	beq vimgx			; ..yes -> exit

	iny				; next y
	sty y0
	lda source_pointer
	clc
	adc dx				; add dx for next source line
	sta source_pointer
	lda source_pointer+1
	adc dx+1
	sta source_pointer+1
	jmp vimgylp			; next line

vimgx:	
	lda #%11110000			; clear RAM0 bank bits# 0-3
	and via2+prb
	ora temp1
	sta via2+prb 			; restore RAM bank
	rts
; -------------------------------------------------------------------------------------------------
; test(x) function returns pixel color (set y with SET)
VDPTest:
	jsr facint			; fac to integer
	lda inth
	cmp sizex+1
	bcs Verror4			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; read vram
	lda y0
	and #$c0			; isolate bit 6+7 (16k bank)
	cmp lastvbank			; same bank like last line ?
	beq vtadr			; ..yes -> skip set bank
	sta lastvbank			; remember bank
; set bank
	asl				; address bits 14+15 -> bit 0+1
	rol
	rol
	+VdpSetReg 14			; set VRAM bank
; set address
vtadr:	lda y0				; get y again
	and #$3f			; remove VRAM address bit 14+15
	tax
	lda x0
	bit mode
	bmi +				; skip in mode 7
	lsr x0+1			; mode6: shift x right because two pixels in a byte
	ror
+	+VdpWait WAIT23,19-1
	sta VDPControl			; write address bits 0-7
	+VdpWait WAIT12,3-1	
	stx VDPControl
	+VdpWait WAITVRAM1,3-1
	ldy VDPRamRead
 	bit mode
 	bmi vtx				; skip in mode 7
; get mode6 pixel color
 	lda x0
 	lsr				; x bit#0 in carry
 	tya				; color in a
 	bcs vtm6			; right pixel -> skip
 	lsr				; shift left pixel to lo nibble
 	lsr
 	lsr
 	lsr
vtm6:	and #$0f			; clear upper nibble
	tay
vtx:	rts				; function return color in y
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror4:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; circle at x, y with radius r
; x1,y1 = centre, intl = radius, cy = alt. radius, temp2,4 calculated add/sub values (for opt. x2)
; x0,y0 calculated values for SetPixel
VDPCircle:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror4			; hi too large -> error
	sta x1+1
	lda intl
	sta x1				; store x1
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror4			; ..too large
	sta y1				; store y1
; r
	jsr chkcom			; check comma
	jsr Getbyte			; get byte
; calc x+r
	bit mode			; check mode
	bmi +				; skip mode 7
	asl				; double r for x check in mode 6
+	sta temp2			; remember check radius for active mode
	clc				; r already in a
	adc x1
	sta x0
	lda x1+1
	adc #0
; check > xmax
	cmp sizex+1			; check high
	bcc vcirxok			; ..smaller max -> ok
	bne Verror4			; ..too large -> error
	lda x0
	cmp sizex			; check low
	bcs Verror4			; ..too large
; calc x-r + check < 0
vcirxok:lda x1				; calc x - r
	sec
	sbc temp2			; check radius
	lda x1+1
	sbc #0
	bmi Verror4			; negative -> error
; calc y+r
	lda y1				; calc y + r
	clc
	adc intl			; intl = r
	cmp #SIZEY
	bcs Verror4			; ..too large
; calc y-r + check < 0
vciryok:lda y1				; calc y - r
	sec
	sbc intl			; intl = r
	bcc Verror4			; negative -> error

	lda #0
	sta cy
	lda intl
	sta tx
vcirlp:
; calc add/sub values for opt. mode 6 double x-radius
	lda intl
	ldx cy
	bit mode			; check mode
	bmi +				; skip mode 7
; mode 6 x values *2
	asl				; x *2
	tay
	txa
	asl				; y *2
	tax
	tya
+	sta temp2			; add/sub value for x
	stx temp4			; add/sub value for x
; dot 1
	lda x1
	clc
	adc temp2			; add radius
	sta x0
	lda x1+1
	adc #0
	sta x0+1
	lda y1
	clc
	adc cy				; add y
	sta y0
	jsr SetPixel			; calc pixel position
; dot 2
	lda y1
	sec
	sbc cy				; sub y
	sta y0
	jsr SetPixel			; calc pixel position
; dot 3
	lda x1
	sec
	sbc temp2			; sub radius
	sta x0
	lda x1+1
	sbc #0
	sta x0+1
	jsr SetPixel			; calc pixel position
; dot 4
	lda y1
	clc
	adc cy				; add y
	sta y0
	jsr SetPixel			; calc pixel position
; dot 5
	lda x1
	clc
	adc temp4			; add y
	sta x0
	lda x1+1
	adc #0
	sta x0+1
	lda y1
	clc
	adc intl			; add radius
	sta y0
	jsr SetPixel			; calc pixel position
; dot 6
	lda y1
	sec
	sbc intl			; sub radius
	sta y0
	jsr SetPixel			; calc pixel position
; dot 7
	lda x1
	sec
	sbc temp4			; sub y
	sta x0
	lda x1+1
	sbc #0
	sta x0+1
	jsr SetPixel			; calc pixel position
; dot 8
	lda y1
	clc
	adc intl			; add radius
	sta y0
	jsr SetPixel			; calc pixel position

	ldx cy
	inx				; y = y+1
	stx cy
; finished ?
	lda intl
	cmp cy
	bcc vcirx			; radius < y+1 -> end

	lda tx
	cmp cy				; tx < y ?
	bcs vcirtxy			; ..no -> tx = tx - y

	dec intl			; intl = intl-1
	lda intl
	clc
	adc tx
	sec
	sbc cy
	sta tx
	jmp vcirlp			; next dots

vcirtxy:sec
	sbc cy				; tx = tx - y
	sta tx
	jmp vcirlp			; next dots

vcirx:	lda x1				; resture centre
	sta x0
	lda x1+1
	sta x0+1
	lda y1
	sta y0
	rts
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror5:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; SPRITE n,p,f set sprite n=0-31 to pattern p=0-63 (incl. sprite_colors, flags f= bit#5-7) 
VDPSprite:
	jsr Getbyte			; get byte parameter
	cmp #SPRITES
	bcs Verror5			; >sprites -> error
	sta temp1			; remember

	jsr chkcom
	jsr Getbyte			; get byte parameter
	cmp #PATTERNS
	bcs Verror5			; >patterns -> error
	sta temp2			; remember

	jsr chkcom
	jsr Getbyte			; get next byte
	and #$e0			; isolate bit#5-7
	sta temp3

	+VdpSpriteBank			; switch to sprite bank
; set color table pointer to new pattern for the sprite
	lda temp2			; get pattern no
	asl				; *8 for color table position
	asl
	sta temp2			; store pattern no *4 for 16x16 pattern
	asl 
	rol source_pointer+1		; carry to hi bit#0
	clc
	adc sprite_colors		; add color table base lo
	sta source_pointer		; store color pointer lo
	lda source_pointer+1
	and #$01			; clear color pointer hi except bit #0
	adc sprite_colors+1		; add color table base hi
	sta source_pointer+1		; store color pointer hi
; set VRAM color table pointer to first pattern
	lda #$00
	sta pointer1+1			; clear VRAM color hi
	ldx temp1			; get sprite no
	lda temp3
	sta sprite_flags,x
	txa
	asl				; *16 for color table position
	asl
	asl 
	asl
	rol pointer1+1
; set VRAM address
	sta VDPControl			; store color table adr lo
	lda pointer1+1
	ora #(>SpriteColorTable&$3e)|$40; add color table base bit#9-13  +bit#6 for write VRAM
	+VdpWait WAIT12,8
	sta VDPControl
	+VdpWait WAITVRAM1,33-1
; set color+flags
	ldy #0				; clear source counter
sprlp:	lda (source_pointer),y
	tax				; remember
	lsr				; upper nibble color to bit#0-3
	lsr
	lsr
	lsr
	ora temp3			; add flags
	sta VDPRamWrite
	txa
	and #$0f			; isolate low nibble color
	ora temp3			; add flags
	+VdpWait WAITVRAM,10
	sta VDPRamWrite
	+VdpWait WAITVRAM,28
	iny
	cpy #8				; finished ?
	bne sprlp			; ..no -> next byte
; store new pattern no for sprite n
	lda temp1			; get no.
	asl				; 4 attribute bytes each sprite
	asl
	clc
	adc #2				; add 2 for pattern pos. in table
	+VdpWait WAIT23,20
	sta VDPControl
	lda #(>SpriteAttributeTable&$3f)|$40	; bit#8-13 attribute table + bit#6 for write VRAM
	+VdpWait WAIT12,5-1
	sta VDPControl

	lda temp2			; pattern no. *4
	+VdpWait WAITVRAM1,13-1
	sta VDPRamWrite			; store new pattern no
	rts
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror6:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; SCOLOR mode6: set platte color / mode7: set sprite color
VDPSColor:
	bit mode			; check mode
	bpl +				; mode 6 -> skip
	jmp VDPSpriteColor		; mode7 -> sprite color
; SCOLOR n,r,g,b set palette color n to red, green, blue (each 0-7)
+	jsr Getbyte			; get byte parameter
	cmp #COLORS6
	bcs Verror6			; >colors -> error
	sta temp1			; remember color no.
; red
	jsr chkcom
	jsr Getbyte
	cmp #8
	bcs Verror6			; >7 -> error
	asl				; shift to high nibble
	asl
	asl
	asl
	sta temp2			; remember red
; green
	jsr chkcom
	jsr Getbyte			; get byte parameter
	cmp #8
	bcs Verror6			; >7 -> error
	sta temp3			; remember green
; blue
	jsr chkcom
	jsr Getbyte
	cmp #8
	bcs Verror6			; >7 -> error
	ora temp2			; or red to high nibble
	tay				; remember in y
; write color to vdp
	lda temp1
	+VdpSetReg 16			; select pallete no
	+VdpWait WAIT23,3-1 
	sty VDPPalette			; store blue+red
	lda temp3			; get green	
	+VdpWait WAIT12,6
	sta VDPPalette			; store blue+red
	rts

; SCOLOR n,c set uni color sprite n=0-31 with color c + sprite_flags 
VDPSpriteColor:
	jsr Getbyte			; get byte parameter
	cmp #SPRITES
	bcs Verror6			; >sprites -> error
	sta temp1			; remember

	jsr chkcom
	jsr Getbyte			; get byte
	cmp #16
	bcs Verror6			; color > 15 -> error

	ldx temp1
	ora sprite_flags,x
	tay				; remember color+flags

	+VdpSpriteBank			; switch to sprite bank
; set VRAM color table pointer to first pattern
	lda #$00
	sta pointer1+1			; clear VRAM color hi
	lda temp1			; get sprite no
	asl				; *16 for color table position
	asl
	asl 
	asl
	rol pointer1+1
; set VRAM address
	sta VDPControl			; store color table adr lo
	lda pointer1+1
	ora #(>SpriteColorTable&$3e)|$40; add color table base bit#9-13  +bit#6 for write VRAM
	+VdpWait WAIT12,8
	sta VDPControl

	+VdpWait WAITVRAM1,5-1
; set color+flags
	ldx #16				; 16 color lines
scollp:	sty VDPRamWrite
	+VdpWait WAITVRAM,8
	dex				; color lines finished ?
	bne scollp			; ..no -> next byte

	rts
; -------------------------------------------------------------------------------------------------
; SPOS n,x,y move sprite n=0-31 to x,y position 
VDPSpritePos:
	jsr Getbyte			; get byte parameter
	cmp #SPRITES
	bcs Verror7			; >sprites -> error
	sta temp1			; remember
; x
	jsr chkcom			; check comma
	jsr Getbyte			; get byte
	sta x1				; remember x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	sta y1

	+VdpSpriteBank			; switch to sprite bank
	lda temp1			; get no.
	asl				; 4 attribute bytes each sprite
	asl
	+VdpWait WAIT23,17-1		; (5 cycles in VdpSpriteBank)
	sta VDPControl
	lda #(>SpriteAttributeTable&$3f)|$40	; bit#8-13 attribute table + bit#6 for write VRAM
	+VdpWait WAIT12,5-1
	sta VDPControl

	lda y1
	+VdpWait WAITVRAM1,6
	sta VDPRamWrite
	lda x1
	+VdpWait WAITVRAM,6
	sta VDPRamWrite
	rts
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror7:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; SDATA p,cnt,adr,b copy cnt=1-64 x 40 bytes to patterntable, sprite_colors=0-63 from address, bank 
VDPSpriteData:
	jsr Getbyte			; get byte parameter
	cmp #PATTERNS
	bcs Verror7			; >patterns -> error
	sta temp1			; remember
; count
	jsr chkcom			; check comma
	jsr Getbyte			; get byte
	beq Verror7			; 0 not allowed -> error
	sta temp3			; store pattern counter
	clc
	adc temp1
	bcs Verror7			; >patterns -> error
; address
	jsr chkcom			; check comma
	jsr Getint			; get 16bit int
	lda intl
	sta source_pointer		; set pointer to address
	lda inth
	sta source_pointer+1
; bank
	jsr chkcom
	jsr Getbyte			; get byte
	cmp #mxbank
	bcs Verror7			; bank > max bank -> error
	sta temp2

	+VdpSpriteBank			; switch to sprite bank
; set color table pointer to first pattern
	lda temp1			; get start pattern
	asl				; *8 for color table position
	asl
	asl 
	sta temp1			; pattern address lo
	rol pointer1+1			; carry to hi bit#0
	clc
	adc sprite_colors		; add color table base lo
	sta pointer1			; store color pointer lo
	lda pointer1+1			; get hi
	and #$01			; clear color pointer hi except bit #0
	tax				; remember
	adc sprite_colors+1		; add color table base hi
	sta pointer1+1			; store color pointer hi
; calc pattern table address
	txa
	asl temp1			; *4 (*32 total) for pattern start address in VRAM
	rol
	asl temp1
	rol
	ora #(>SpritePatternTable&$3f)|$40 	; add pattern table base bit#8-13 +bit#6 for write VRAM
	tax				; pattern address hi
; set VRAM address
	lda temp1			; get pattern table adr lo
	sta VDPControl
; switch to source bank while waiting for VDP
	lda via2+prb			; get MMU reg
	and #%00001111			; isolate RAM0 bank
	sta temp4			; remember
	lda #%11110000			; clear RAM0 bank bits# 0-3
	and via2+prb
	ora temp2
	sta via2+prb 			; set RAM bank

	+VdpWait WAIT12,15-1
	stx VDPControl

	+VdpWait WAITVRAM1,12
; copy pattern
sdcpylp:ldy #0
sdpatlp:lda (source_pointer),y		; get source byte
	iny
	sta VDPRamWrite
	+VdpWait WAITVRAM,15-1
	cpy #SPRX/8*SPRY		; pattern finished ?
	bne sdpatlp			; ..no -> next byte
; copy colors
	lda source_pointer
	clc
	adc #(SPRX/8*SPRY)		; set to source color data
	sta source_pointer
	lda source_pointer+1
	adc #0
	sta source_pointer+1

	ldy #0
sdcollp:lda (source_pointer),y		; get source byte
	tax				; remember data
	lda #%11110000			; clear RAM0 bank bits# 0-3
	and via2+prb
	sta via2+prb 			; set RAM0 bank for sprite color data
	txa
	sta (pointer1),y		; store to color table 
	lda #%11110000
	and via2+prb
	ora temp2
	sta via2+prb 			; set source RAM bank
	iny
	cpy #8				; 8 color-bytes (16 nibbles) finished ?
	bne sdcollp			; ..no -> next byte

	lda source_pointer
	clc
	adc #8				; next source pattern
	sta source_pointer
	lda source_pointer+1
	adc #0
	sta source_pointer+1

	lda pointer1
	clc
	adc #8				; next color table entry
	sta pointer1
	lda pointer1+1
	adc #0
	sta pointer1+1

	dec temp3			; dec pattern counter
	bne sdcpylp			; copy next pattern

	lda #%11110000			; clear RAM0 bank bits# 0-3
	and via2+prb
	ora temp4
	sta via2+prb 			; restore RAM bank
	rts
; -------------------------------------------------------------------------------------------------
; set userfont datasource to address
VDPUserfont:
	jsr Getint
	lda intl
	sta userfont_adr		; store userfont address
	lda inth
	sta userfont_adr+1
	rts
; -------------------------------------------------------------------------------------------------
; illegal quantity error
Verror8:jmp fcerr			; -> illegal quantity error
; -------------------------------------------------------------------------------------------------
; copy screen data from x,y size dx, dy to x1,y1 (dx, dy > 0) (mode 6 only even x size used!)
VDPCopy:
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror8			; hi too large -> error
	sta x0+1
	lda intl
	sta x0				; store x
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
	bcs Verror8			; ..too large
	sta y0				; store y
; dx
	jsr chkcom
 	jsr Getint			; get 16bit parameter
	lda inth
	clc
	adc intl
	beq Verror8			; 0 not allowed -> error
	lda inth

	sta dx+1
	cmp sizex+1
	bcc vcpdxok			; hi < sizex hi-> ok
	bne Verror8			; hi..too large -> error
	lda intl
	cmp sizex			; check lo
	bcc vcpdxok			; lo < -> ok
	bne Verror8			; lo..too large -> error
vcpdxok:lda intl
	sta dx
; dy
	jsr chkcom
	jsr Getbyte			; get next byte
	beq Verror8			; 0 not allowed -> error
	cmp #SIZEY+1
	bcs Verror8			; ..too large
	sta dy
; x1
	jsr chkcom
	jsr Getint			; get 16bit parameter
	lda inth
	cmp sizex+1
	bcs Verror8			; hi too large -> error
	sta x1+1
	lda intl
	sta x1				; store x1
; y
	jsr chkcom			; check comma
	jsr Getbyte			; get next byte
	cmp #SIZEY			; > ymax ?
fferra:	bcs Verror8			; ..too large
	sta y1				; store y1
; check x0+dx
	lda dx
	sec
	sbc #1				; calc dx - 1
	tax				; remember
	lda dx+1
	sbc #0
	tay				; remember dx hi
	txa				; get dx lo
	clc
	adc x0				; calc x + dx
	tya				; get hi
	adc x0+1
	cmp sizex+1
	bcs fferra			; hi too large -> error
; check x1+dx
	txa
	clc
	adc x1				; calc x1 + dx
	tya
	adc x1+1
	cmp sizex+1
	bcs fferra			; hi too large -> error
; check y0+dy
	lda dy
	sec
	sbc #1				; calc dy - 1
	tax
	clc
	adc y0				; calc y + dy
	cmp #SIZEY			; > ymax ?
	bcs fferra			; too large -> error
; check y1+dy
	txa
	clc
	adc y1				; calc y1 + dy
	cmp #SIZEY			; > ymax ?
	bcs fferra			; too large -> error

	+VdpCommand x0, x0+1, y0, x1, x1+1, y1, dx, dx+1, dy, zero, zero, HMMM
	jmp waitcmd		; wait for command execution
; -------------------------------------------------------------------------------------------------
; set pixel x, y (preserves x, y / no validity check)
;   subroutine for Dot and Circle
;   checks if same vram bank like last time and skips bank setting for more speed
SetPixel:
	lda y0
	and #$c0			; isolate bit 6+7 (16k bank)
	cmp lastvbank			; same bank like last time ?
	beq spchkm			; ..yes -> skip set bank
	sta lastvbank			; remember bank
; set bank
	asl				; address bits 14+15 -> bit 0+1
	rol
	rol
	+VdpSetReg 14			; set VRAM bank
; check mode
spchkm:	bit mode
	bpl spmode6			; -> set pixel mode 6
; mode 7: set address+set pixel
	lda y0				; get y again
	and #$3f			; remove VRAM address bit 14+15
	ldx x0
	+VdpWait WAIT23,17-1
	+VdpWriteAddress
	lda color
	+VdpWait WAITVRAM1,6
	sta VDPRamWrite
	rts
; mode 6 read byte from VRAM
spmode6:lda x0+1
	lsr				; mode6: shift x right because two pixels in a byte
	lda x0
	ror
	tax				; remember x/2
	+VdpWait WAIT23,21-1
	sta VDPControl			; write address bits 0-7
	lda y0				; get y again
	and #$3f			; remove VRAM address bit 14+15, bit#6=0 read
	+VdpWait WAIT12,8	
	sta VDPControl

	lda x0
 	lsr				; x bit#0 in carry
	+VdpWait WAITVRAM1,8
	lda VDPRamRead			; get byte
  	bcs sppix1			; -> right pixel
; left pixel
	and #$0f			;  clear left nibble	
	sta temp1
 	lda color
 	asl				; shift color to hi nibble for left pixel
 	asl
 	asl
 	asl
	ora temp1			; add old right pixel
	bcc spsetm6			; always (asl shifts hi nibble color=0 -> carry)
; right pixel
sppix1: and #$f0			; clear right nibble
	ora color			; add new pixel to lo nibble
; write new byte
spsetm6	tay				; new byte in y
	lda y0				; get y again
	and #$3f			; remove VRAM address bit 14+15
	+VdpWait WAIT23,18
	+VdpWriteAddress
	+VdpWait WAITVRAM1,3-1
	sty VDPRamWrite
	rts
; -------------------------------------------------------------------------------------------------
; draw line from x, y to x1, y1 (x, y is set to target, no validity check)
;   subroutine for line, draw,..
DrawLine:
	ldx #$00			; clear VDP LINE command  arg
	lda y1				; save target
	pha
	lda x1
	pha
	lda x1+1
	pha
; check x1 < x0 ?
	cmp x0+1
	bcc dlxchg			; x1 hi < x0 hi -> exchange
	bne dlchky			; x1 hi > x0 hi -> skip
	lda x1				; hi equal - compare lo
	cmp x0
	bcs dlchky			; x1 >= x0 -> skip
; x1 < x0 exchange points
dlxchg:	ldy x1+1			; exchang x1 - x0
	lda x0+1
	sta x1+1
	sty x0+1
	ldy x1
	lda x0
	sta x1
	sty x0
	ldy y1
	lda y0
	sta y1
	sty y0
; check y1 < y0 and calc dy
dlchky:	lda y1				; check y1 < y0
	cmp y0
	bcs dldy			; y1 >= y0 -> skip
; calc dy backwards
	lda y0
	sec
	sbc y1				; dy = y0 - y1
	sta dy
	ldx #$08			; set VDP LINE command -y flag
	bne dldx			; skip alway
; calc dy
dldy:	lda y1
	sec
	sbc y0				; dy = y1 - y0
	sta dy
; calc dx
dldx:	lda x1
	sec
	sbc x0				; dx = x1 - x0
	sta dx
	lda x1+1
	sbc x0+1
	sta dx+1
; check dx > dy (longer cathete)
	bne dlcmd			; dx hi > 0 -> >dy -> skip 
	lda dx				; dx hi = 0: compare lo bytes
	cmp dy
	bcs dlcmd			; dx >= dy -> skip
; dy > dx 
	inx				; set flag VDP LINE command longer cathete flag 
	tay
	lda dy				; exchange dx - dy
	sta dx
	sty dy

dlcmd:	stx arg				; store command arg

	+VdpCommand zero, zero, zero, x0, x0+1, y0, dx, dx+1, dy, color, arg, LINE
	
	pla				; restore target
	sta x0+1
	pla
	sta x0
	pla
	sta y0
	jmp waitcmd			; wait for command execution
; -------------------------------------------------------------------------------------------------
; basic4+ 16bit parameter to intl, inth (faclo, facmo)
;   returns only lo byte in a
Getbyte:jsr frmnum			; evaluate the formula
	lda facsgn
	bmi Verror9			; negative -> error
	lda facexp
	cmp #145
	bcs Verror9			; too big -> error
	jsr qint
	lda inth
	bne Verror9			; ..too large -> error
	lda intl
	rts
; -------------------------------------------------------------------------------------------------
; basic4+ 16bit parameter to intl, inth (faclo, facmo)
Getint:	jsr frmnum			; evaluate the formula
facint:	lda facsgn
	bmi Verror9			; negative -> error
	lda facexp
	cmp #145
	bcs Verror9			; too big -> error
	jsr qint
	rts
; -------------------------------------------------------------------------------------------------
Verror9:jmp fcerr
; -------------------------------------------------------------------------------------------------
; vdp statement dispatch table (startaddress -1)
; reslst
VdpStmdsp:
	!byte <(Verror9-1), >(Verror9-1)		; $e9
	!byte <(VDPMode-1), >(VDPMode-1)		; $ea
	!byte <(VDPColor-1), >(VDPColor-1)		; $eb
	!byte <(VDPClear-1), >(VDPClear-1)		; $ec
	!byte <(VDPDot-1), >(VDPDot-1)			; $ed
	!byte <(VDPLine-1), >(VDPLine-1)		; $ee
	!byte <(VDPSet-1), >(VDPSet-1)			; $ef
	!byte <(VDPDraw-1), >(VDPDraw-1)		; $f0
	!byte <(VDPFrame-1), >(VDPFrame-1)		; $f1
	!byte <(VDPBlock-1), >(VDPBlock-1)		; $f2
	!byte <(VDPImage-1), >(VDPImage-1)		; $f3
	!byte <(VDPTest-1), >(VDPTest-1)		; $f4
	!byte <(VDPCircle-1), >(VDPCircle-1)		; $f5
	!byte <(VDPSprite-1), >(VDPSprite-1)		; $f6
	!byte <(VDPSpriteData-1), >(VDPSpriteData-1)	; $f7
	!byte <(VDPSpritePos-1), >(VDPSpritePos-1)	; $f8
	!byte <(Verror9-1), >(Verror9-1)		; $f9
	!byte <(VDPUserfont-1), >(VDPUserfont-1)	; $fa
	!byte <(Verror9-1), >(Verror9-1)		; $fb
	!byte <(Verror9-1), >(Verror9-1)		; $fc
	!byte <(VDPSColor-1), >(VDPSColor-1)		; $fd
	!byte <(VDPCopy-1), >(VDPCopy-1)		; $fe
VdpStmdspEnd:
; -------------------------------------------------------------------------------------------------
; vdp statements (vdp reslst)
Vdplst:					; upper left of screen is 0, 0
TKVDP = $e9				; 16 colors from palette / 8bit color %RRRGGGBB
	!scr "INI", $d4		; INIT			init graphics extension
	!scr "MOD", $c5		; MODE g,c		g=0/1 on/off +2=PAL +4=Mode7, +8 YJK (only mode 7), frame color
	!scr "COLO", $d2	; COLOR c		select draw/text color
	!scr "CLEA", $d2	; CLEAR c		clear screen with backgroundcolor
	!scr "DO", $d4		; DOT x,y		set pixel x,y
	!scr "LIN", $c5		; LINE x,y,x1,y1	line from x,y to x1,y1
	!scr "SE", $d4		; SET x,y		set cursor to x,y
	!scr "DRA", $d7		; DRAW x1,y1		draw line to x1,y1
	!scr "FRAM", $c5	; FRAME x,y,x1,y1	frame x,y=lower left, x1,y1=upper right
	!scr "BLOC", $cb	; BLOCK x,y,dx,dy	block x,y=lower left, size dx,dy (min. 1)
	!scr "IMAG", $c5	; IMAGE x,y,adr,b	copy image (byte 0+1=dx,dy) from adr,bnk to x,y (only mode 6)
TKTEST = $f4
	!scr "TES", $d4		; TEST(x)		function test returns pixel color (y with SET)
	!scr "CIRCL", $c5	; CIRCLE x,y,r		circle at x,y with radius r (x doubled in mode 6)
	!scr "SPRIT", $c5	; SPRITE n,p,f		sprite n=0-31, pattern p=0-63, flags f= bit#5-7 
	!scr "SDAT", $c1	; SDATA p,cnt,adr,b	copy cnt=1-64 x 40 bytes to p=0-63 from adr, bank 
	!scr "SPO", $d3		; SPOS n,x,y		move sprite n=0-31 to x, y pos. (0=upper left) 
	!scr "VPRIN", $d4	; VPRINT x,y,""		print with userfont to x, y	 
	!scr "USERFON", $d4	; USERFONT adr		set userfont to adr=address
	!scr "UPRIN", $d4	; UPRINT ""		print with userfont to cursor pos 
	!scr "UPO", $d3		; UPOS r,c		set uprint cursor to row, column
	!scr "SCOLO", $d2	; SCOLOR n, c	mode6:	set palette color 0-15, color 0-15
				;		mode7:	set uni color sprite n=0-31 with color c+flags 
	!scr "VCOP", $d9	; SCOPY x,y,dx,dy,x1,y1	copy screen data from x,y size dx,dy to x1,y1 
	!byte $01		; end of vdp reserved word list
; -------------------------------------------------------------------------------------------------
; interface jump vectors.
*=vecorg 	; $ff6f kernal jump vector table
vreset	*=*+3		; Power-on/off vector reset
vmode	*=*+3		; VDP: set mode
pgmkey	*=*+3		; Function key vector
vclear	*=*+3		; VDP: clear screen with background color
ioinit	*=*+3		; i/o initialization
cint	*=*+3		; screen initialization
alloc	*=*+3		; allocate routine
waitcmd	*=*+3		; VDP: Wait for VDP command
krestr	*=*+3		; restore i/o vectors
lkupsa	*=*+3		; secondary to physical map
lkupla	*=*+3		; logical to physical map
setmsg	*=*+3		; control os messages
second	*=*+3		; send sa after listen
tksa	*=*+3		; send sa after talk
topmem	*=*+3		; read/set top of memory
membot	*=*+3		; read/set bottom of memory
kscnky	*=*+3		; scan keyboard
ksetmo	*=*+3		; set timeout in iec
acptr	*=*+3		; hand shake iec byte in
ciout	*=*+3		; hand shake iec byte out
untlk	*=*+3		; send untalk out
unlsn	*=*+3		; send unlisten out
listn	*=*+3		; send listen out
talk	*=*+3		; send talk out

krdst	*=*+3		; return i/o status byte
setlfs	*=*+3		; set la, fa and sa
setnam	*=*+3		; set length and filename adr
kopen	*=*+3		; open logical file
kclose	*=*+3		; close  -      -
kchkin	*=*+3		; open channel in
kchout	*=*+3		; open   -     out

clrch	*=*+3		; close i/o channel
kbasin	*=*+3		; input from  -
kbsout	*=*+3		; output -    -
kload	*=*+3		; load from file
ksave	*=*+3		; save to    -
settim	*=*+3		; set internal clock
rdtim	*=*+3		; read   -      -
kstop	*=*+3		; scan stop key (break)
kgetin	*=*+3		; get char from queue
kclall	*=*+3		; close all files
udtim	*=*+3		; increment clock
scrorg	*=*+3		; return screen origin
plot	*=*+3		; read/set .x,.y coords
doscmd	*=*+3		; dos command
; -------------------------------------------------------------------------------------------------
*=$bfff
	!byte $ff