// LC256 v1.1 logic, MMU
// (c) Vossi 11/2024, Baleares
// Altera EPM7064SLC
// v1.0 initial for pcb v1.0
// v1.1 replaced exram with busy input (63c09)
// v1.1a 6309 OS (no screen/color shadow RAM)

module lc256(	input [15:7]A,
					input PHI2,
					input R_W,
					input SYNC,
					input _DMA,
					input ROML,
					input ROMH,
					input _EXTL,
					input _EXTH,
					input _BUSY,
					output RDY,
					output BA,
					output _KB0,
					output _RAM0,
					output _RAM1,
					output _ROM,
					output _CART,
					output _IO,
					output _CSR,
					output _CSW,
					output _RDUSB,
					output WRUSB,
					output _CS1,
					output _CS2
					);

reg be;			// bus enable register
wire cartl, carth;
wire roml, romh;
wire io;
					
assign _KB0= !(A[15:10] == 0);				// $0 - $3FF always in RAM Bank 0

always @(*)		// DMA register
begin
	if(_DMA) 						// clear DMA mode -> enable bus, run cpu
		be <= 1;
	else if(!_DMA & SYNC)		// start DMA mode at sync (sync only appears at phi1 / read)
		be <= 0;
end
//assign BA = (!_DMA & SYNC) | (!_DMA & BA);	// register cleared with DMA=1
assign RDY	= be;					// run cpu
assign BA	= !be;					// enable bus driver

assign _IO = !(be & (A[15:10] == 6'b110111));		// I/O = DC00-DFFF (external limited to DE00-DFFF)

assign cartl	= !A[14] & !_EXTL & R_W;				// CART 8000-BFFF
assign carth	=  A[14] & !_EXTH & R_W & _IO;		// CART C000-FFFF excl. I/O DC00-DFFF
assign _CART	= !(be & A[15] & (cartl | carth));

assign roml		= !A[14] & ROML;							// ROM 8000-BFFF
assign romh		=  A[14] & ROMH & _IO;					// ROM C000-FFFF excl. I/O DC00-DFFF
assign _ROM		= !(be & A[15] & _CART & R_W & (roml | romh));

assign _RAM0	= !(!A[15] & ((be & PHI2) | !be));
assign _RAM1	= !( A[15] & ((be & PHI2 & _IO & _CART & _ROM) | !be));

assign io	= be & PHI2 & !_IO;
assign _CSW	= !(io & A[9:7] == 3'b000 & !R_W);
assign _CSR	= !(io & A[9:7] == 3'b001 &  R_W);

assign  WRUSB	= io & A[9:7] == 3'b010 & !R_W;
assign _RDUSB	= !(io & A[9:7] == 3'b010 &  R_W);

assign _CS1	= !(io & A[9:7] == 3'b011 & !R_W);
assign _CS2	= !(io & A[9:7] == 3'b011 &  R_W);

endmodule