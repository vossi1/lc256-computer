; ******************************************* MACROS **********************************************
; VDP macros
!macro VdpWait .u, .c{		; *** us wait - cycles already present - for VDP access
	!set .t = (.u*10-(.c*10/CLOCK))*CLOCK/20
	!do while .t > 0{
		nop			; each nop needs 2 cycles
		!set .t = .t -1}
}
!macro VdpSetReg .r{		; *** set VDP Register
	sta VDPControl			; first writes data in A to control port #1
	lda #.r | $80			; writes register no. with bit#7 = 1 to Port #1
	+VdpWait WAIT12,5-1
	sta VDPControl
}
!macro VdpWriteAddress{		; *** set VDP write vram address-pointer to AAXX
	stx VDPControl
	ora #$40			; bit#6 = 1 write
	+VdpWait WAIT12,5-1
	sta VDPControl
} 
			; *** execute VDP Command
!macro VdpCommand .sx, .sxhi, .sy, .dx, .dxhi, .dy, .nx, .nxhi, .ny, .clr, .arg, .cmr{
	lda #32				; first command register
	+VdpSetReg 17			; set register indirect
; 32
	lda .sx
	+VdpWait WAIT23,6		; wait for DVP
	sta VDPIndirect
	lda .sxhi
	+VdpWait WAIT23,5-1
	sta VDPIndirect
	lda .sy
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda #$00
	+VdpWait WAIT23,5-1
	sta VDPIndirect
; 36
	lda .dx
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda .dxhi
	+VdpWait WAIT23,5-1
	sta VDPIndirect
	lda .dy
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda #$00
	+VdpWait WAIT23,5-1
	sta VDPIndirect
; 40
	lda .nx
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda .nxhi
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda .ny
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda #$00
	+VdpWait WAIT23,5-1
	sta VDPIndirect
; 44
	lda .clr
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda .arg
	+VdpWait WAIT23,6
	sta VDPIndirect
	lda #.cmr
	+VdpWait WAIT23,5-1
	sta VDPIndirect
}
!macro VdpSpriteBank{		; *** Switch to bank $C000-$FFFF for fast sprite access
	bit lastvbank			; bit#6 -> V, bit#7 -> N
	bpl .j
	bvs .k
.j	lda #SpritePatternTable>>14	; A14+15 = VDP bank
	+VdpSetReg 14			; writes VRAM bank
	lda #>SpritePatternTable&$c0	; isolate A14+15
	sta lastvbank
.k} 