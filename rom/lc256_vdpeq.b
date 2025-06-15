; * switches
CLOCK = 4		; CPU clock in MHz for VDP waits
; vdp
MODE = 7		; 6 = graphics 6: 512px/16colors / 7 = graphics 7: 256px/256colors
PAL = 1			; PAL=1 / NTSC=0 selects V9938/58 PAL RGB-output, NTSC has a higher picture
LINES = 212		; lines = 192 / 212
; VDP speed parameter - don't change!
WAIT12 = 1 ; 2		; us 1. - 2. byte VDP
WAIT23 = 2 ; 5		; us 2. - 3. byte VDP
WAITVRAM1 = 5		; us vram 1.access
WAITVRAM = 5		; us vram loop 
;
!if MODE=6{
TITLECOL		= 1	; Title message color
TITLE2COL		= 10	; Title message line 2 color
BYTESCOL		= 6	; Title bytes color
TEXTCOL			= 3	; Default text color:   0=black, 1=white, 10=lightred, 6=blue, 3=cyan
BGRCOL			= 0	; background color
EXTCOL			= 0	; exterior color
}else {
TITLECOL		= $ff	; Title message color
TITLE2COL		= $3e	; Title message line 2 color
BYTESCOL		= $43	; Title bytes color
TEXTCOL			= $e7	; Default text color: $00=black, $ff=white, $3e=pink, $22=darkblue, $43=blue, $e7=cyan
BGRCOL			= $00	; background color
EXTCOL			= $00	; exterior color
}
VDPREG0			= $0a	; VDP reg 0 base value mode 6/7
VDPREG1			= $02	; VDP reg 1 value mode 6+7 (mode M1+M2, screen disabled, sprites 16x16)
VDPREG18		= $0d	; VDP reg 18 value (V/H screen adjust, $0d = Sony PVM 9")
!if LINES=192 {VDPREG9	= $00	; VDP reg 9 value (bit #7 = 192 / 212 lines, bit#1 = NTSC / PAL)
	}else {VDPREG9	= $80}
SIZEX6			= 512		; x size mode6
SIZEX7			= 256		; x size mode7
SIZEY			= LINES		; y size
COLORS6			= 16		; mode6 colors
SPRITES			= 32		; number of sprites
SPRX			= 16		; sprite x size 
SPRY			= 16		; sprite y size
PATTERNS		= 64		; sprite patterns, color patterns
FONTH			= 8		; userfont height
FONTW			= 6		; userfont width
!if LINES=192 {nrows	= 24		; screen rows
	}else {nrows	= 25}
COLUMNS7		= 40		; screen columns mode 7
COLUMNS6		= 80		; screen columns mode 6
; vdp commands
HMMV			= $c0		; highspeed fill
HMMM			= $d0		; highspeed VRAM copy
YMMM			= $e0		; highspeed VRAM y copy
LINE			= $70		; line
; ***************************************** ZEROPAGE **********************************************
; kernal zero page / equates (outsourced)
!addr	ddisk		= $b7		; Default disk unit # for monitor/basic
!addr	color		= $cf		; Character color
!addr	sedsal		= $c1 ; 2by	; Scroll ptr
!addr	sedeal		= $c3 ; 2by	; Scroll ptr
	mxbank	=15+1			; 1st RAM bank out of range
;
; VDP zero page
*= $ea
!addr	mode		*=*+1		; graphics mode: bit#7=1: 7, bit#7=0: 6 (512px/16 colors)
!addr	lastvbank	*=*+1		; last vram bank
!addr	columns		*=*+1		; screen columns
!addr	scxmax		*=*+1		; Max column number
!addr	bgcolor		*=*+1		; background color
!addr	bgcolor_left	*=*+1		; background color for left pixel
!addr	temp1		*=*+1		; temp	
!addr	temp2		*=*+1		; temp
!addr	y0		*=*+1		; 8bit y
!addr	y1		*=*+1		; 8bit y1 (target)
!addr	cy				; cy (only in circle)
!addr	temp3		*=*+1		; temp
!addr	dy				; 8bit dy
!addr	temp4		*=*+1		; temp for circle, print, spritedata
; $f6 16bit
!addr	zero				; zero value for command macro parameter (lo of sizex = 0)
!addr	sizex		*=*+2		; 16bit screen size
!addr	x0		*=*+2		; 16bit x
!addr	x1		*=*+2		; 16bit x1 (target)
!addr	tx				; 16bit tx (only in circle)
!addr	dx		*=*+2		; 16bit dx
!addr	sprite_colors 	*=*+2		; pointer to $7e00 64x 8by with 16 spriteline color nibbles
; sedsal, sedeal only used in function keys, scroll routines
!addr	source_pointer	= sedsal	; pointer to source
!addr	pointer1	= sedeal	; pointer
; ***************************************** ABSOLUTE **********************************************
; VDP sprites
!addr	sprite_flags	= $0361 ; 32by	; 32x sprite flags EC, CC, IC
; VDP absolute
!addr	bgcolor_del	= $038b		; double background color for vdp commands
!addr	arg		= $038c		; command arg
!addr	userfont_adr	= $038d	; 2by	; userfont address
!addr	wtemp		= $038f	; 2by	; 16bit temp
; **************************************** ADDRESSES **********************************************
; I/O addresses
VDPWriteAdr	= $dc00		; Port#0 RamWrite, #1 Control, #2 Palette, #3 Indirect
VDPReadAdr	= $dc80		; Port#0 RamRead, #1 Status
; sprite_colors 200 bytes at $7e00 - $7fff 	64x 8 bytes with 16 spriteline color nibbles
; VDP ports
!addr	VDPRamWrite	= VDPWriteAdr
!addr	VDPControl	= VDPWriteAdr+1
!addr	VDPPalette	= VDPWriteAdr+2
!addr	VDPIndirect	= VDPWriteAdr+3
!addr	VDPRamRead	= VDPReadAdr
!addr	VDPStatus	= VDPReadAdr+1
SpritePatternTable	= $f000
SpriteAttributeTable	= $fa00
SpriteColorTable	= SpriteAttributeTable - $200	; always $200 below sprite attribute table

