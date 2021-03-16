.	<<	EBUSIM.IO	>>

.SEC(|I/O and Buffer Logic|)

	EBUS contains code for both Engine DMA (i.e., ISIS Dispatcher-ring)
I/O, and for PDP-10 (i.e., Ebus) transfers.  It also contains a variant
of conventional buffer (and bufferlet) management (although buffers are
used ONLY on the PDP-input path, and then only for normal data and those
control signals which MUST maintain sequentiality with the user data).

	These routine are so fundamental to operation that they are coded
as self-contained routines, and in the body of the code are treated almost
as though they were primative extensions to the basic instruction-set.

.SS(|Engine DMA Access|)

	The (1-Meg address-space) memory of the engine is mapped directly into
the address-space of the 68K, consuming addresses in the range $E00000 to
$EFFFFF.  the only constraint on transfers is that Byte-transfers are not
permitted;  only word (W) and long (L) transfers are allowed.  This has an
impact only in the PUTCH routine, which must emulate a byte transfer by
manipulation of Engine Half-word (W) operands.

	For convenience and efficiency, certain of the 68K registers have
been renamed, and are reserved for the exclusive use of the ISIS-ring
routines:

.BEGIN BOX
typedef	struct	ISISRING
{	short	NFMI;
	short	CEI;
	char	ring[100];	/* the size "100" is a dummy */
}	isisring;

	isisring Rs;		/* pointer to ISIS input-ring */
	isisring Rd;		/* pointer to ISIS output-ring */
	short	Cs;		/* input-ring cursor */
	short	Cd;		/* output-ring cursor */
.END

.SSS(|ISIS-ring Output Routines|)

	There are nine routines dealing with ISIS-ring output:

.SSSS(|Room|)
.DTOPIC(|ROOM Routine|)

	Room is passed the number of bytes required.  It examines the output-
ring and determines if that amount of space is available, returning
TRUE if so, else FALSE:

.BEGIN BOX
ROOM(n)
int	n;
{	int	m;		/* used to compute space */
	Cd = Rd->NFMI;
	if ( ( m = Cd - Rd->CEI ) < 0 )
		m = ORSIZE - m;
	if ( m >= n ) return TRUE;
	else return FALSE;
}
.END

.SSSS(|WAITIS|)
.DTOPIC(|WAITIS Routine|)

	WAITIS is passed the number of bytes required.  It waits, calling
ROOM repeatedly, until ROOM returns TRUE.

.BEGIN BOX
WAITIS(n)
int	n;
{	While ( ROOM( n ) );
	return;
}
.END

.SSSS(|WAITISW|)
.DTOPIC(|WAITISW Routine|)

	WAITISW is defined for convenience;  it invokes WAITIS with parameter
value 4 (wait for 4 bytes):

.BEGIN BOX
WAITISW
{	WAITIS( 4 );
	return;
}
.END

.SSSS(|SLOR|)
.DTOPIC(|SLOR Routine|)

	Start_Logical_Output_Routine (SLOR) is used to initialize ISIS-ring
output.  Arguments are port number and message-type:

.BEGIN BOX
SLOR(p,t)
int	p, t;
{	Cd = Rd->NFMI;
	Rd->ring[Cd] = (short) p;	/* place port-number */
	Rd->ring[Cd+2] = (char) t;	/* place type */
	Cd += 3;
	return;
}
.END

.SSSS(|PUTCH|)
.DTOPIC(|PUTCH Routine|)

	PUTCH places a single character into the ring.  It is complicated by
the inability to address single bytes in the Engine.

.BEGIN BOX
PUTCH(c)
char	c;
short	HW;
{	HW = (short) Rd->ring[ Cd&(-2) ];
	if ( (Cd & 1) = 0)
		HW = (HW & $0FF) | (c << 8); /* left byte */
	else
		HW = (HW & $0FF00) | c;	/* right byte */
	(short) [ Cd&(-2) ] = HW;
	if ( ++Cd >= ORSIZE ) Cd = 0;
	return;
}
.END

.SSSS(|PUTH|)
.DTOPIC(|PUTH Routine|)

	PUTH places a single short into the ring.

.BEGIN BOX
PUTH(s)
short	s;
{
	(short) Rd->ring[ Cd ] = s;
	if ( ( Cd += 2 ) >= ORSIZE ) Cd = 0;
	return;
}
.END

.SSSS(|PUTW|)
.DTOPIC(|PUTW Routine|)

	PUTW places a single long into the ring.

.BEGIN BOX
PUTW(l)
long	l;
{
	(long) Rd->ring[ Cd ] = l;
	if ( ( Cd += 4 ) >= ORSIZE ) Cd = 0;
	return;
}
.END

.SSSS(|ELOR|)
.DTOPIC(|ELOR Routine|)

	End_Logical_Output_Record (ELOR) terminates the current message.

.BEGIN BOX
ELOR()
{	Cs += 3;
	Cs &= -4;		/* round up to next message boundary */
	if ( Cs >= ORSIZE ) Cs = 0;
	Rs->NFMI = Cs;		/* DONE! */
	return;
}
.END

.SSSS(|SENDQI|)

	SENDQI is defined for convenience, to send a quick message to
ISIS:

.BEGIN BOX
SENDQI(p, t)
int	p, t;			/* p is port, t is type */
{	WAITISW();
	SLOR( p, t );
	ELOR();
	return;
}
.END

.SSS(|ISIS-ring Input Routines|)

	ISIS-ring input is implemented through six routines:

.SSSS(LOOK)
.DTOPIC(LOOK Routine)

	LOOK sets up Cs, and returns TRUE if there is a message waiting in
the ISIS input-ring (and also PORTNM and INTYBT), else FALSE:

.BEGIN BOX
LOOK()
{	Cs = Rs->CEI;
	if ( Cs == Rs->NFMI ) return FALSE;	/* ring empty */
	PORTNM = (short) Rs->ring[Cs];
	INTYBT = (char) Rs->ring[Cs+2];
	Cs += 3;
	return TRUE;
}
.END

.SSSS(GETCH)
.DTOPIC(GETCH Routine)

	GETCH gets a single byte from the ring.  It is complicated by the
inability to fetch single bytes from the Engine memory:

.BEGIN BOX
char	GETCH()
{	int	HW;
	HW = (short) Rs->ring[Cs&(-2)];	/* get HW containing char */
	if ( (Cs&(-2)) == 0 ) HW >>= 8;	/* select byte to use */
	if ( ++Cs >= IRSIZE ) Cs = 0;
	return (char) (HW&$0FF);
}
.END

.SSSS(GETH)
.DTOPIC(GETH Routine)

	GETH gets a single short from the ring:

.BEGIN BOX
short	GETH()
{	short	HW;
	HW = (short) Rs->ring[Cs];
	if ( Cs += 2 >= IRSIZE ) Cs = 0;
	return HW;
}
.END

.SSSS(GETW)
.DTOPIC(GETW Routine)

	GETW gets a single long from the ring:

.BEGIN BOX
long	GETW()
{	long	l;
	l = (long) Rs->ring[Cs];
	if ( Cs += 4 >= IRSIZE ) Cs = 0;
	return l;
}
.END

.SSSS(ELIR)
.DTOPIC(ELIR Routine)

	End_Logical_Input_Record (ELIR) terminates and closes input:

.BEGIN BOX
ELIR()
{	Cs += 3;		/* bring cursor to next boundary */
	Cs &= -4;
	if ( Cs >= IRSIZE ) Cs -= IRSIZE;
	Rs->CEI = Cs;
	return;
}
.END

.SSSS(FLUSH)
.DTOPIC(FLUSH Routine)

	FLUSH "flushes" the specified number of characters from the input-ring,
and then closes the current message:

.BEGIN BOX
FLUSH(n)
int	n;
{	Cs += n;		/* advance the cursor */
	ELIR();			/* ...then close it */
	return;
}
.END

.SS(|PDP (Black-Box) I/O|)
.TOPIC(|Black-Box|)

	The Black-box can be viewed as having an extended register, of form:

.BEGIN BOX
________________._______________.___.___._.______________
|     Dleft	|     Dmid	|Ahi|Drt|C|    Alo	|
:---------------:---------------:-------:---------------:
      "DHI"           "DLO"       "AHI"       "ALO"
.END

	Where:
.BEGIN OFFSET
	Dleft###Data-left -- Left 16-bits of PDP data-word;
	Dmid####Data-middle -- Middle 16-bits of PDP data-word;
	Drt#####Data-right -- Right 4-bits of PDP data-word;
	Ahi#####Address-high -- Left 4-bits of PDP address;
	Alo#####Address-low -- Right 15-bits of PDP address;
	C#######Control-bit -- Set to 1 for Write; set to 0 for Read.
	"DHI"###The "name" of the register addressed by the 68K.
	"DLO"###ditto.
	"AHI"###ditto. (Note:  this is a single byte).
	"ALO"###ditto.
.END

	These are addressed by the 68K as four 16-bit registers, which map into
the 68K's memory.  Addresses for input are different than those for output:

.BEGIN BOX
Register	Input		Output
DHI		$D0403A		$D0803A
DLO		$D0403C		$D0803C
AHI		$D0403E		$D0803E
ALO		$D04038		$D08038
.END

	In addition, the following addresses are also used to manipulate the
Black-box:

.BEGIN BOX
Symbol		Address		Usage
.SKIP
DSENS		$D000B8		Sense if Busy (high-bit on)
DTMOT		$D001B8		Sense if Read Time-out (high-bit on)
DPER		$D00138		Sense Read Parity-error (high-bit on)
DEXC0		$D01038		Reset Black-box (by referencing)
.END

	Commonly used operations are expressed as functions:

.BEGIN BOX
SENSE() { ( (short) DSENS & $0001 ) }
PARITY() { ( (short) DPER & $0001 ) }
TIMEOUT() { ( (short) DTMOT & $0001 ) }
RESET() { ( if(DEXC0); ) }
.END

	Internal locations used globally include:

.DTOPIC(|P10DAH Variable|)
.DTOPIC(|P10DAL Variable|)
.DTOPIC(|P10DLL Variable|)
.DTOPIC(|P10ADR Variable|)
.BEGIN BOX
short	P10DAH;			/* Data-high */
short	P10DAL;			/* Data-low */
short	P10ADR;			/* Address (within PDP) */
char	P10DLL;			/* Ahi | Drt */
.END

.BEGIN BLOX
	NOTE: P10DAH | P10DAL are also regularly used as a "long" with
name "P10DAH".
.END

.SSS(|SETBLKA|)
.DTOPIC(|SETBLKA Routine|)

	SETBLKA is a utility function provided for both Block -input and
-output.  It formats the argument (an address within the PDP) and sets
BLK = TRUE:

.BEGIN BOX
SETBLKA(adr)
int	adr;			/* the address */
.TOPIC(|P10ADR Variable|)
{	P10ADR = adr;		/* set low-15 bits */
.TOPIC(|P10DLL Variable|)
	P10DLL = ( (adr>>15) << 4; /* set high-4 bits */
	BLK = TRUE;
	return;
}
.END

.TOPIC(|P10DLL Variable|)
.SSS(|Output To PDP|)

	The primary mode of output to the PDP is via the PDP input-ring.
The 68K retains the following variables related to this ring:

.DTOPIC(|PDPIRP Variable|)
.DTOPIC(|PDPISZ Variable|)
.DTOPIC(|PDPIFC Variable|)
.DTOPIC(|PDPIEC Variable|)
.BEGIN BOX
short	PDPIRP;			/* Pointer (to base of ring) */
short	PDPISZ;			/* size of ring */
short	PDPIFC;			/* fill-cursor */
short	PDPIEC;			/* empty-cursor */
.END
	There are a number of routines for output:

.SSSS(|WR10R|)
.DTOPIC(|WR10R Routine|)

	WR10R is the primative routine for all transfers from 68K to PDP:

.BEGIN BOX
WR10R()
{	short HW;		/* temp store */
	int	cnt;
	for ( cnt=0 ; cnt < 70 ; cnt++ )
	{	if (SENSE() ) continue;
.TOPIC(|P10DAH Variable|)
		*DHI = ~ P10DAH; /* MIC inverts bits */
.TOPIC(|P10DAL Variable|)
		*DLO = ~ P10DAL;
		HW = P10DLL;	/* for Drt...*/
		if ( BLK == 0 )	/* if NOT Block-IO... */
			HW = ( ~ HW ) & $0F;
		*AHI = HW;	/* no inversion for address */
.TOPIC(|P10ADR Variable|)
		*ALO = $08000 | P10ADR; /* ditto. */
		return;
	}
	BUSCRAS();
}
.END

.SSSS(|PUTNOT0|)
.DTOPIC(|PUTNOT0 Routine|)

	Certain special locations within the PDP cause the PDP to crash
if non-zero.  PUTNOT0 insures that a Write will force non-zero:

.BEGIN BOX
PUTNOT0(v)
long	v;			/* v is value */
{	(long) P10AH = v;
.TOPIC(|P10DLL Variable|)
	P10DLL = 0;		/* insure low 4 bits set */
.TOPIC(|WR10R Routine|)
	WR10R();
	return;
}
.END

	It is used exclusively by:

.SSSS(|HCRASH|)
.DTOPIC(|HCRASH Routine|)

	HCRASH is called to crash the PDP.  It places the 16-bit Crash-code
into the PDP-KEY cell (at o150), and packs the 16-bit PDPOEC (Output-
Empty-Cursor) with the 16-bit PDPIFC (Input-Fill-Cursor) into the PDP-CRASH
cell (at o30):

.BEGIN BOX
HCRASH()
.TOPIC(|P10ADR Variable|)
{	P10ADR = 0150; RESET(); PUTNOT0( CODCASH << 16 );
.TOPIC(|PDPIFC Variable|)
.TOPIC(|PDPOEC Variable|)
	P10ADR = 030; RESET(); PUTNOT0( PDPOEC << 16 | PDPIFC );
	return;
}
.END

.SSSS(|PUTPDPL|)
.DTOPIC(|PUTPDPL Routine|)

	PUTPDPL places the argument at the (already specified) P10ADR
Left-Justified:

.BEGIN BOX
PUTPDPL(v)
long	v;			/* v is the argument */
{	(long) P10AH = v;
.TOPIC(|P10DLL Variable|)
	P10DLL = $0F;		/* compensates for bit-inversion */
.TOPIC(|WR10R Routine|)
	WR10R();
	return;
}
.END

.SSSS(|PUTPDPR|)
.DTOPIC(|PUTPDPR Routine|)

	PUTPDPR places the argument at the (already specified) P10ADR
Right-Justified:

.BEGIN BOX
PUTPDPR(v)
long	v;			/* v is the argument */
{	(long) P10AH = v >> 4;
.TOPIC(|P10DLL Variable|)
	P10DLL = (char) v;	/* place low 4-bits into Drt */
.TOPIC(|WR10R Routine|)
	WR10R();
	return;
}
.END

.SSSS(|PUTIFC|)
.DTOPIC(|PUTIFC Routine|)

	PUTIFC is used to place a copy of the 68K's Input-Fill-Cursor into
the PDP's memory (at o156), Right-justified:

.BEGIN BLOX
	NOTE:###The PDP will not examine a message UNTIL the presence of the
message is indicated by the PDP receiving a copy of PDPIFC which extends
the input-ring data to include the message.  Call PUTIFC to "flush" the
message for the PDP's examination.  Conversely, do NOT call it after each
transfer, unless that call corresponds to an entire message;  otherwise
the PDP will attempt to process an incomplete message.
.END
.BEGIN BOX
PUTIFC()
	PUTPDPR places the argument at the (already specified) P10ADR
{	P10ADR = 0156;
.TOPIC(|PDPIFC Variable|)
	PUTPDPR( PDPIFC );
	return;
}
.END

.SSSS(|PUTOEC|)
.DTOPIC(|PUTOEC Routine|)

	PUTOEC is used to place a copy of the 68K's Output-Empty-Cursor into
the PDP's memory (at o162), Right-justified:

.BEGIN BOX
PUTOEC()
.TOPIC(|PDPOEC Variable|)
.TOPIC(|P10ADR Variable|)
{	P10ADR = 0162; PUTPDPR( PDPOEC );
	return;
}
.END

.SSSS(|FDRGSZ|)
.DTOPIC(|FDRGSZ Routine|)

	FDRGSZ is used to determine the space available in the PDP's
input-ring (in bytes), based upon the constraints imposed by certain
ISIS message types (from INTYBT);  in particular, data messages
input to the PDP are not permitted to wrap the ring.  The value is
returned in INRGSP:

.BEGIN BOX
FDRGSZ()
{	GETIEP();		/* get the current empty-cursor */
.TOPIC(|PDPIFC Variable|)
.TOPIC(|PDPIEC Variable|)
	if (  ( INRGSP = 4*(PDPIEC-PDPIFC-1) )  >= 0 )
		return;		/* empty behind fill */
.TOPIC(|PDPISZ Variable|)
	INRGSP = 4*PDPISZ-PDPIFC); /* fill behind empty */
	if ( ~DATRGZ ) return;	/* no wrap for BIO */
	if (  (INTYBT==0) !! (INTYBT>$9D) !! (PORTNM==0)  )
	 if ( PDPIEC>0 )
	  INRGSP += 4*(PDPIEC-1); /* non-data may wrap */
	return;
}
.END

.SSSS(|WAITPDP|)
.DTOPIC(|WAITPDP Routine|)

	WAITPDP simply waits until there is at least one PDP input-ring
word available:

.BEGIN BOX
WAITPDP
{	do FDRGSZ()		/* loop here... */
	while ( INRGSP == 0 );	/* until space becomes available */
	return;
}
.END

.SSSS(|WRPDP|)
.DTOPIC(|WRPDP Routine|)

	WRPDP places its argument into the PDP input ring and performs the
necessary operations to advance (and fold, if necessary) the fill-cursor:

.BEGIN BOX
WRPDP(v)
long	v;			/* v is the argument */
.TOPIC(|PDPIRP Variable|)
.TOPIC(|P10ADR Variable|)
.TOPIC(|PDPIFC Variable|)
.TOPIC(|P10ADR Variable|)
{	P10ADR = PDPIRP+PDPIFC;
	PUTPDPL(v);
.TOPIC(|PDPISZ Variable|)
	if ( ++PDPIFC >= PDPISZ ) /* need to wrap the ring
.TOPIC(|PDPIEC Variable|)
	{   for ( ; PDPIEC==0 ; )GETIEP(); /* wait */
	    PDPIFC = 0;		/* wrap once PDP not at 0 */
	}
	return;
}
.END

.SSSS(|WRDAT|)
.DTOPIC(|WRDAT Routine|)

	WRDAT is an alternate entry-point to WRPDP, which forces the use
of DAT as the argument:

.BEGIN BOX
WRDAT	{ WRPDP( (long) DAT); return; }
.END

.SSSS(|WAITDAT|)
.DTOPIC(|WAITDAT Routine|)

	WAITDAT is an alternate entry-point to WRDAT, which waits for space in
the ring first:

.BEGIN BOX
WAITDAT	{ WAITPDP();  WRDAT(); return; }
.END

.SSSS(|WRBLK|)
.DTOPIC(|WRBLK Routine|)

	WRBLK is used to transmit one word from DAT into the current Bin
Block/word, and advance the fill cursor:

.BEGIN BOX
WRBLK()
.TOPIC(|P10DAH Variable|)
{	P10DAH = (long) DAT;
.TOPIC(|SETBLKA Routine|)
.TOPIC(|BI_TAR - Block-input: (PDP) block start address|)
	SETBLKA( Rp->BI_TAR++ ); /* set address */
.TOPIC(|WR10R Routine|)
	WR10R();		/* write word */
	BLK = FALSE;
	return;
}
.END

.SSSS(|GETBLK|)
.DTOPIC(|GETBLK Routine|)

	GETBLK is used to fetch one word into DAT from the current Bin
Block/word:

.BEGIN BOX
GETBLK()
.TOPIC(|SETBLKA Routine|)
.TOPIC(|BI_TAR - Block-input: (PDP) block start address|)
{	SETBLKA( Rp->BI_TAR );	/* set address */
	RD10R();		/* read word */
	BLK = FALSE;
	return;
}
.END

.SSS(|Input From PDP|)

	The primary mode of input from the PDP is via the PDP output-ring.
The 68K retains the following variables related to this ring:

.DTOPIC(|PDPORP Variable|)
.DTOPIC(|PDPOSZ Variable|)
.DTOPIC(|PDPOFC Variable|)
.DTOPIC(|PDPOEC Variable|)
.BEGIN BOX
short	PDPORP;			/* Pointer (to base of ring) */
short	PDPOSZ;			/* size of ring */
short	PDPOFC;			/* fill-cursor */
short	PDPOEC;			/* empty-cursor */
.END
	There are a number of routines for input:

.SSSS(|RD10R|)
.DTOPIC(|RD10R Routine|)

	RD10R is the primative routine for all transfers from PDP to 68K:

.BEGIN BOX
RD10R()
{	short HW;		/* temp store */
	int	cnt, cnt1, tmoc; /* counters */
	for ( tmoc=0 ; tmoc < 16 ; { RESET(); tmoc++ )
	{   for ( cnt=0 ; cnt < 70 ; cnt++ )
	    {	if (SENSE() ) continue;
		HW = 0;		/* for Drt...*/
		if ( BLK == 0 )	/* if NOT Block-IO... */
.TOPIC(|P10DLL Variable|)
			HW = P10DLL;
		*AHI = HW;
.TOPIC(|P10ADR Variable|)
		*ALO = $7FFF & P10ADR; /* set read */
		for ( cnt1=0 ; cnt1 < 70 ;
			{ if ( TMOUT() ) break;
			if ( PARITY() ) PARERR();
			cnt1++; }  )
		{   if ( SENSE() ) continue;
.TOPIC(|P10DAH Variable|)
		    P10DAH = *DHI;
.TOPIC(|P10DAL Variable|)
		    P10DAL = *DLO;
.TOPIC(|P10DLL Variable|)
		    P10DLL = *AHI & $0F;
		    if ( PARITY() ) PARERR;
		    return;
		}
		BUSCRAS();
	    }
	    BUSCRAS();
	}
	TMOUT1();
}
.END

.SSSS(|GETPDPL|)
.DTOPIC(|GETPDPL Routine|)

	GETPDPL fetches the (left-justified) argument from the (already
specified) P10ADR:

.BEGIN BOX
GETPDPL(v)
.TOPIC(|RD10R Routine|)
{	RD10R();		/* do the read */
	return (long) P10AH;	/* get the argument */
}
.END

.SSSS(|GETPDPR|)
.DTOPIC(|GETPDPR Routine|)

	GETPDPR fetches the (right-justified) argument from the (already
specified) P10ADR:

.BEGIN BOX
GETPDPR(v)
{	long	v;		/* v is the argument */
.TOPIC(|RD10R Routine|)
	RD10R();		/* get the argument */
	v = ((long) P10AH) << 4;
.TOPIC(|P10DLL Variable|)
	v != P10DLL & $0F;	/* pack argument */
	return v;
}
.END

.SSSS(|GETOFC|)
.DTOPIC(|GETOFC Routine|)

	GETOFC is used to fetch a copy of the PDP's Output-Fill-Cursor (at
o161) into the 68K's memory:

.BEGIN BOX
GETOFC()
.TOPIC(|PDPOFC Variable|)
.TOPIC(|P10ADR Variable|)
{	P10ADR = 0161; PDPOFC = GETPDPR();
	return;
}
.END

.SSSS(|GETIEC|)
.DTOPIC(|GETIEC Routine|)

	GETIEC is used to fetch a copy of the PDP's Input-Empty-Cursor (at
o155) into the 68K's memory:

.BEGIN BOX
GETIEC()
.TOPIC(|PDPIEC Variable|)
{	P10ADR = 0155; PDPIEC = GETPDPR();
	return;
}
.END

.SSSS(|RDPDPRP|)
.DTOPIC(|RDPDPRP Routine|)

	RDPDPRP is used to fetch a copy of all of the PDP's Ring-parameters
(both input and output):

.BEGIN BOX
RDPDPRP()
.TOPIC(|P10ADR Variable|)
.TOPIC(|PDPORP Variable|)
{	P10ADR = 0157; PDPORP = GETPDPL();	/* Output-ring */
.TOPIC(|PDPOSZ Variable|)
	P10ADR = 0160; PDPOSZ = GETPDPL();
.TOPIC(|PDPOFC Variable|)
	GETOEC();
.TOPIC(|PDPOEC Variable|)
	P10ADR = 0162; C = GETPDPR();
.TOPIC(|PDPIRP Variable|)
	P10ADR = 0153; PDPIRP = GETPDPL();	/* Input-ring */
.TOPIC(|PDPISZ Variable|)
	P10ADR = 0154; PDPISZ = GETPDPL();
.TOPIC(|PDPIEC Variable|)
	GETIEC();
.TOPIC(|PDPIFC Variable|)
	P10ADR = 0156; PDPIFC = GETPDPR();
	return;
}
.END

.SSSS(|RDPDP|)
.DTOPIC(|RDPDP Routine|)

	RDPDP fetches its argument from the PDP output ring (into DAT) and
performs the necessary operations to advance (and fold, if necessary) the
empty-cursor:

.BEGIN BOX
RDPDP()
.TOPIC(|PDPORP Variable|)
.TOPIC(|PDPOEC Variable|)
.TOPIC(|P10ADR Variable|)
{	P10ADR = PDPORP+PDPOEC;
	DAT = GETPDPL();
.TOPIC(|PDPOSZ Variable|)
	if ( ++PDPOEC >= PDPOSZ ) /* need to wrap the ring
	    PDPOEC = 0;
	return;
}
.END

.SSSS(|RDBLK|)
.DTOPIC(|RDBLK Routine|)

	RDBLK is used to fetch one word into DAT from the current Bout
Block/word:

.BEGIN BOX
RDBLK()
.TOPIC(|SETBLKA Routine|)
.TOPIC(|BO_TAR - Block-output: (PDP) block start addr.|)
{	SETBLKA( Rp->BO_TAR++ ); /* set address */
.TOPIC(|RD10R Routine|)
	RD10R();		/* read word */
	BLK = FALSE;
	return;
}
.END

.SSS(|Buffer Routines|)

	Buffering may be performed on data and certain control-signals
passing into the PDP, either because of Block-input, or because the
PDP has applied back-pressure.

	Several routine provide the primative functions required:

.SSSS(|DGCI|)
.DTOPIC(DGCI Routine)

	DGCI (Get Character and Increment) is used to remove a character
from the buffer (as specified by Rp):

.BEGIN BOX
DGCI()
{	char	c;
	int	b;
	b = Rp->BB++;		/* copy and advance cursor */
	c = .BFLTS[b++];	/* get char */
	if ( -- Rp->BCT <= 0 )	/* count it gone...now empty? */
	{   if (Rp->BCT < 0) DGCIH(); /* crash if underflow */
	    b += BFLSIZ-2;	/* force b to boundary */
	    b &= -BFLSIZ;
	    (short) .BFLTS[b] = (short) .BFLTS;
	    (short) .BFLTS = b;
	}
	else if ( (Rp->BB & (BFLSIZ-1) ) == 0) /* Bfl't exhausted? */
	{   Rp->BB = (short) .BFLTS[b];
	    (short) .BFLTS[b] = (short) .BFLTS - (BFLSIZ-2);
	    (short) .BFLTS = b;
	}
	return c;
}
.END

.SSSS(DWCI)
.DTOPIC(DWCI Routine)

	DWCI (Write Character and Increment) is used to place a character into
the buffer described by Rp:

.BEGIN BOX
DWCI(c)
char	c;			/* the character to place */
{	int	b;
	if (Rp->BCT++ == 0)	/* if buffer was empty */
	{   if (Rp->BCT <= 0) DWCIH(); /* CRASH...count was negative */
	    if ( (b = (short) .BFLTS) == 0 ) WRE(); /* none left */
	    (short) .BFLTS = (short) .BFLTS[b];
	    Rp->BE = Rp->BB = b -= BFLSIZ-2;
	    .BFLTS[b] = c;
	    return;
	}
	b = ++ Rp->BE;		/* not empty...find next position */
	if ( b & (bflsiz-1) == 0 ) /* bufferlet exhausted? */
	{   if ( (b = (short) .BFLTS) == 0 ) WRE(); /* none left */
	    (short) .BFLTS = (short) .BFLTS[b];
	    (short) .BFLTS[Rp->BE] = b;
	    Rp->BE = b -= (BFLSIZ-2);
	}
	.BFLTS[b]=c;
	return;
}
.END

.SSSS(DEMPTY)
.DTOPIC(DEMPTY Routine)

	DEMPTY is used to empty a buffer:

.BEGIN BOX
DEMPTY()
{	int	b;
	if (Rp->BCT != 0)	/* nothing to do if already empty */
	{   Rp->BCT = 0;	/* must empty it */
	    b = Rp->BE + BFLSIZ-2;
	    b &= -BFLSIZ;
	    (short) .BFLTS[b] = (short) .BFLTS;
	    b = Rp->BB + BFLSIZ-2;
	    b &= -BFLSIZ;
	    (short) .BFLTS = b;
	}
	return;
}
.END
@ kï