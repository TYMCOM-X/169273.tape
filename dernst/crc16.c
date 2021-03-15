/*			     Cyclic Redundancy Check
 
	CRC is a function that generates a 16-bit value from an 8-bit code.
	It is called by two functions, that compute CRC1 and CRC2.  They,
	in turn are called by two funtions that compute CRC1/2 for sending
	and for receiving.
 
	------ LINK LAYER ------     ----- PACKET LAYER -----	 --- DATA ----
	RECEIVER	 SENDER      RECEIVER	      SENDER	DEFINE	 CALL
	crc1_chk	crc1_gen     crc2_chk	     crc1_gen	*buf	  buf
	   |_______  _______|_____	|______    _____|___	ndx	&ndx
		   \/		  \	       \  /	|   |	bytes	 bytes
	       crc_loop ___________\____________\/	|   |
	     ______|______	    \  _________________|   |
	    |		  |	     \/ 		    |
	get_byte,      crc_add---->mirror		put_byte
 
 
	crc2_gen sets a buffer pointer (ccb->outp_buf) and index (ccb->outp_
	ndx) that are used by put_byte and get_byte.  Both must be able to
	change the caller's caller's buf and ndx, so that crc2_gen can subse-
	quently call put_byte to store the CRC that is produced.
 
	Thus, pointers to there variables are passed to CRCx and to xxt_byte.
	.____________________.	.____________________.	.____________________.
	|     ddddddddddddddd|	|  dddddddddddddddddc|	|c		     |
	crc_gen store needs:	 \buf (may change)   \ndx (may overflow)
 
	Common CRC routines
	mirror reverses the order of the bits (left to/from right) in a byte.
	crc_loop computes calls crc_add for every byte in link/packet header.
	get_byte provides each byte, using the caller's buffer address and
	   index (on the stack) into each bufferlet.
	put_byte stores each byte (of CRC2), using the caller's buffer address
	   and index (on the stack) into each bufferlet.
 
	Link Layer CRC Routines
	crc1_gen calls crc-loop for output, from the byte after STX to the byte
	   before the CRC1 field, and stores the total into the CRC1 field.
	crc1_chk calls crc_loop for input, from the byte after STX through the
	   CRC1 field, and compares the total with a constant.
 
	Packet Layer CRC Routines
	crc2_gen calls crc-loop for output, from the byte after CRC1 to the byte
	   before the crc2 field, and stores the total into the crc2 field.
	crc2_chk calls crc-loop for input, from the byte after CRC1 through the
	   crc2 field, and compares the total with a constant.
 
*/
#include "p_define.d"
#include "p_pktlyr.d"
#include "p_bufque.s"
#include "p_lcbccb.s"
#include "p_bqpool.e"
#include "p_bufque.e"
extern LCB_STR lcb;
unsigned int crctbl [256];
 
/* -----------------------------------------------------------------------
     mirror is called to invert the bit order of the resultant CRC
 -------------------------------------------------------------------*/
 
unsigned
mirror(data)
unsigned data; {
static short xlat[] =
		{ 0, 8, 4, 0xC, 2, 0xA, 6, 0xE, 1, 9, 5, 0xD, 3, 0xB, 7, 0xF};
return((xlat[data&0xF] << 4) | xlat[(data >> 4) & 0xF]);
}
 
 
/* --------------------------------------------------------------------------
   crc_add is called by crc_loop to "add" a 8-bit code to a total.
-------------------------------------------------------------------------- */
unsigned
crc_add (code, total)
char code;
unsigned total; {
 
unsigned temp;
 
/*get XOR of next byte and upper 8 bits of running CRC*/
temp = (total >> 8) ^ mirror(code);
 
return (((total << 8) & 0xFF00) ^ crctbl[temp]);
 
}   /* crc function */
 
 
 
/* --------------------------------------------------------------------------
   get_byte returns next byte in the bufferlet chain and sets fin, if last.
   If packet size is zero, do not call this routine!  Get next bufferlet before
   using it.  Caller supplies 3 variables that get_byte may write into.
   get_byte goes beyound the end of data bytes to get 2 CRC bytes for crcX_chk
-------------------------------------------------------------------------- */
get_byte (buf, index)
BUF_PTR *buf;
int *index; {
  BUF_PTR nbuf;
  int ret_val;
  ret_val = (*buf)->data[*index];	/* get the byte */
  (*index)++;
  if (*index >= bflt_data_size) {
    if ((*buf)->bnext == NULL) {
      if ((nbuf = get_buff()) == NULL)
	(*buf)->num_bytes--;		/* cause a CRC error */
      else {
	(*buf)->bnext = nbuf;		/* old points to new  */
	*buf = (*buf)->bnext;		/* new is current buf */
      }   /* else get_buf worked */
    }	/* if no next bufferlet */
    else
      *buf = (*buf)->bnext;		/* move forward in chain.  */
    *index = 0; 			/* start new bufferlet	   */
  }   /* if not end (of bufferlet) */
  return (ret_val);			/* get the byte */
}   /* get_byte function */
 
/* --------------------------------------------------------------------------
   put_byte is called by crc2_gen, after it has computed the CRC, to store the
   next byte in the bufferlet chain.  Caller supplies the bufferlet, index and
   buffer-size pointers that put_byte may write into, and the byte to store.
-------------------------------------------------------------------------- */
put_byte (buf, index, size, bits)
BUF_PTR *buf;
int *index, *size, bits; {
  BUF_PTR nbuf;
  (*buf)->data[*index] = bits;		/* store the byte */
  (*index)++;				/* increm caller's index */
  (*buf)->num_bytes++;			/* increm bufferlet count */
  (*size)++;				/* increment total buffer count */
  if (*index >= bflt_data_size) {	/* need to get another bufferlet */
    if ((nbuf = get_buff ()) == NULL)
      (*buf)->num_bytes--;		/* cause a CRC error */
    else {
      (*buf)->bnext = nbuf;		/* old points to new  */
      *buf = nbuf;			/* new is current buf */
    }  /* else get_buff OK */
    *index = 0;
  }   /* if index > bufferlet bytes */
  /* else let CRC fail, retransmit later --- don't store*/
  return;
}  /* put_byte function */
 
 
 
/* --------------------------------------------------------------------------
   crc_loop is called by crcX_chk or _gen (X=1 or 2) to loop through all
   bufferlets and compute the CRC total that link/packet layer sends/reveived.
-------------------------------------------------------------------------- */
unsigned
crc_loop (buf, ndx, bytes_max)
BUF_PTR *buf;
int *ndx, bytes_max; {
  unsigned total;
  int i;
  char ch;
  total = INIT_CRC;			/* initial CRC value */
  for (i=0; i<bytes_max; i++) {
    ch = get_byte (buf, ndx);	/* ndx is incrm by get_byte */
    total = crc_add (ch, total);
  }   /* for i < bytes_max */
  return (total);
}  /* crc2 function */
 
 
 
/* --------------------------------------------------------------------------
   crc1_gen computes the CRC total for link layer to send, and stores it.
-------------------------------------------------------------------------- */
crc1_gen (que)
QUE_PTR que; {
  static BUF_PTR buf;			/* IBM PC must be in data segment */
  static int ndx;			/* IBM PC must be in data segment */
  buf = que->u.cmd.bfirst;
  ndx = B_SIZE; 			/* start at offset B_SIZE */
  lcb.out_crc = ~ crc_loop  (&buf, &ndx, 4);	  /* total is in lcb.out_crc */
  buf->data[B_CRC] = mirror( lcb.out_crc >> 8 );	/* top of total */
  buf->data[B_CRC+1] = mirror( lcb.out_crc & 0xFF);	/* bottom total */
  return;
}  /* crc1_gen function */
 
/* --------------------------------------------------------------------------
   crc1_chk computes the CRC total that link layer reveived, including the two
   CRC bytes, and compares it to a constant.
-------------------------------------------------------------------------- */
crc1_chk (que)
QUE_PTR que; {
  static BUF_PTR buf;			/* IBM PC must be in data segment */
  static int ndx;			/* IBM PC must be in data segment */
  int ret_val;
  buf = que->u.cmd.bfirst;
  ndx = B_SIZE; 			/* start at offset B_SIZE */
  lcb.in_crc = crc_loop  (&buf, &ndx, 6);	  /* total is in lcb.in_crc */
  ret_val = (lcb.in_crc & 0xFFFF) == INPUT_CRC;
  ret_val = TRUE;			/* T E M P O R A R Y */
  return (ret_val);
}  /* crc1_chk function */
 
/* --------------------------------------------------------------------------
   crc2_gen computes the CRC total for link layer to send, and stores it.
-------------------------------------------------------------------------- */
crc2_gen(ccb,que)
CCB_PTR ccb;
QUE_PTR que; {
  static BUF_PTR buf;			/* IBM PC must be in data segment */
  static int ndx, bytes_max;		/* IBM PC must be in data segment */
  buf = que->u.cmd.bfirst;		/* initialize... */
  ndx = B_BYTE0 + 1;			/* start at offset B_BYTE0 +1 */
  bytes_max = buf->data[B_SIZE];	/* get_byte... */
  if (bytes_max > 0) {
    ccb->out_crc2 =  ~crc_loop (&buf, &ndx, bytes_max);      /* CRC incl CRC2 */
    put_byte (&buf, &ndx, &bytes_max, mirror(ccb->out_crc2 >> 8));  /* store */
    put_byte (&buf, &ndx, &bytes_max, mirror(ccb->out_crc2));	    /* CRC */
  }   /* if size > 0 */
  return;
}  /* crc2_gen function */
 
/* --------------------------------------------------------------------------
   crc2_chk computes the CRC total that link layer reveived, including the two
   CRC bytes, and compares it to a constant.
-------------------------------------------------------------------------- */
crc2_chk (ccb, que)
CCB_PTR ccb;
QUE_PTR que; {
  static BUF_PTR buf;			/* IBM PC must be in data segment */
  static int ndx, bytes_max;		/* IBM PC must be in data segment */
  int ret_val;
  buf = que->u.cmd.bfirst;
  ndx = B_BYTE0 + 1;			/* start at offset B_BYTE0 */
  bytes_max = buf->data[B_SIZE] + 2;	/* total CRC, including CRC2 */
  if (bytes_max > 2) {
    ccb->in_crc2 = crc_loop (&buf, &ndx, bytes_max);
    ret_val = (ccb->in_crc2 & 0xFFFF) == INPUT_CRC;  /* set the return value */
    ret_val = TRUE;			/* T E M P O R A R Y */
  }   /* if size > 0 */
  else
    ret_val = TRUE;			/* zero bytes to crc */
  return (ret_val);   /* top of total */
}  /* crc2_chk function */
 
 
crc_gen (polybits)
long int polybits; {
  /* This program generates the CRC table for a polynomial for Tymshare engine.
	   SEG A.DATA		;by Mark Akselrod
  PLNM	   WC  18005		;CODE POLYNOMIAL HERE
  TABLE    HS  256
 
	   SEG A.CODE
 
  START    XR  R1,R1
  LOOP	   EXHR R2,R1
  LOOP1    JFFO R2,LOOP2
	   J	LOOP3
  LOOP2    LIS	R5,0F
	   SR	R5,R3
	   JLFS LOOP3
	   L	R4,PLNM
	   SLL	R4,0,R5
	   XR	R2,R4
	   JBS	LOOP1
  LOOP3    STH	R2,TABLE,R1,R1
	   AIS	R1,1
	   CHI	R1,0FF
	   JLE	LOOP
	   JAL	R10,CRASH
	   HC	0,0
  */
long int r2,r3;
int r1,r5;
for(r1 = 0; r1 <= 255; r1++){
	r2 = r1;    r2 = r2 << 16;
	while(r2 != 0){
		r3 = r2;
		for(r5 = 7 ;(r3 & 0x800000) == 0; r5--)
			r3 = r3 << 1;
		if (r5 < 0) break;
		r2 = r2 ^ (polybits << r5);
		};
	crctbl[r1] = r2;
};
}  /* crc_gen function */
 