	TTL	'E B U S  --  PDP-10 Base Code, Version 2.00'
	CHIP	68010

*			***** ****  *   *  ****
*			*     *   * *   * *
*			****  ****  *   *  ***
*			*     *   * *   *     *
*			***** ****   ***  ****

*************************************************************************
**	                PROPRIETARY INFORMATION                        **
**	                                                               **
**	This  source code listing constitutes the proprietary pro-     **
**	perty of TYMNET, Incorporated. The recipient, by receiving     **
**	this program listing, agrees that neither this listing nor     **
**	the   information  disclosed herein nor any  part  thereof     **
**	shall be  reproduced or transferred to other documents  or     **
**	used  or  disclosed to others for manufacturing or for any     **
**	other   purpose except as specifically authorized in  wri-     **
**	ting by  TYMNET, Incorporated.                                 **
*************************************************************************

	OPT	B,-M,-I

VERSION	EQU	$202			; VERSION NUMBER
PRODID	EQU	$91			; product-ID
DEBUG	EQU	0			; what to tell host when we come up
MAXPORT	EQU	256			; maximum number of ports supported
BFLSIZ	EQU	16			; bufferlet size (Power of 2!!)
ESC	EQU	$1B			; code used for escapes (in buffers)
YB_CODE	EQU	$AC			; code for Yellow-ball

*	Assembly and Debugging Switches
RTRACE	EQU	1			; Enable trace of PDP ring transfers
BTRACE	EQU	0			; Enable trace of PDP BIO transfers
ITRACE	EQU	0			; Enable trace of ISIS transfers
CTRACE	EQU	0			; Enable trace of Buffer transfers
TRACING	EQU	RTRACE!BTRACE!ITRACE!CTRACE	; Switch if ANY trace active


*	Special (RESERVED) Register Declarations
Rp	EQU	A4			; Pointer to port-descriptor
Rs	EQU	A5			; Pointer to Start of ISIS Input Ring
Rd	EQU	A6			; Pointer to Start of ISIS Output Ring
Cs	EQU	D7			; Cursor to position within Input Ring
Cd	EQU	D6			; Cursor to position within Output Ring


*	B A S I C   M E M O R Y   L A Y O U T

*	"Magic" addresses for addressing the Engine
ENGINE	EQU	$FFFFF901		; address to interrupt the engine

ENGVECT	EQU	$13C			; Engine-interrupt Vector Location
*		$900			  Bottom of stack
*		$1000			  Top-of-Stack;  Beginnings of tables
*					   and variable storage
PATCHR	EQU	$4000			; PATCH AREA
*		$4F00			  Beginning of Bootstrap
*		$5000			  Beginning of Code
*		$7C00			  STARTES -- Diagnostic Routine
*		$7D00			  CONWRT -- diagnostic routine
*		$7E00			  READ diagnostic routine
BUFER	EQU	$8000			; bufers
BUFEREND EQU	BUFER+$7000		; the end of the bufferlet area
 IFNE	TRACING
STRACE	EQU	$20000			; beginning of trace area
ETRACE	EQU	$3FFE0			; end of trace area
 ENDC
*		$????			  

*	Various Locations for doing Read and Write to PDP-10
DSENS	EQU	$D000B8			; D0=0-BUSY, 1-NOT BUSY
DPER	EQU	$D00138			; D0=1-ERROR, DO AFTER READ
DTMOT	EQU	$D001B8			; D0=1-TIMEOUT
DEXCO	EQU	$D01038			; RESET BOX SIGNAL ONLY
DEXC1	EQU	$D010B8			; NEXT WRT TO PDP10 WRITE WRONG PARITY

*	Locations for reading from PDP-10
RD101	EQU	$D0403A
RD104	EQU	$D0403C
RD102	EQU	$D0403E
RD103	EQU	$D04038

*	Locations for Writing to PDP-10
WT101	EQU	$D0803A
WT102	EQU	$D0803C
WT103	EQU	$D0803E
WT104	EQU	$D08038
	PAGE

*	Define data-structure for Port-Descriptor:
	OFFSET	0
P_NUM	DS.B	1			; spare copy of port-number
P_FLAGS	DS.B	1			; port status (flags)
 PF_ACT: EQU	0			; port active
 PF_OBP: EQU	1			; Output Back-pressure applied (by ISIS)
 PF_IBP: EQU	2			; Input Back-pressure applied (by us)
 PF_HBP: EQU	3			; Input Back-pressure applied (by Host)
 PF_BKO: EQU	4			; Block-Output active
 PF_BKI: EQU	5			; Block-Input active
 PF_BFD: EQU	6			; Block FilleD

XMITLMT	DS.B	1			; transmit limit
IBRATE	DS.B	1			; Input baud-rate
AUXKEY	EQU	IBRATE			; hold AUX-key here too

BO_TAR	DS.L	1			; Block-output -- (PDP) block start addr.
BO_BYT	DS.W	1			; Block-Output -- byte position within word
BO_TCN	DS.W	1			; Block-output -- output count (total left)

BI_TIM	DS.L	1			; Block-input -- time (FASTC) last activity
BI_TAR	DS.L	1			; Block-input -- (PDP) block start address
BI_TCN	DS.W	1			; Block-input -- input count (total)
BI_LCN	DS.W	1			; Block-input -- input count (so far)
BI_BYT	DS.W	1			; Block-input -- pos. within current word

BB	DS.W	1			; bufferlet-begin
BE	DS.W	1			; bufferlet-end
BCT	DS.W	1			; count of characters
PD_SIZ	DS.W	0			; size of descriptor
	PAGE

***	Define some useful Macros

*	Macro to Cause VCRASH with crash-code VAL
CRASH	MACRO	VAL
	MOVE.B	#VAL,CODCASH		; set crash-code
	BSR	VCRASH			; go do it
	ENDM

*	Macro to Cause (FATAL!) BCRASH with crash-code VAL
FCRASH	MACRO	VAL
	MOVE.B	#VAL,CODCASH		; set crash-code
	BSR	BCRASH			; go do it
	ENDM

*	Macro to PUSH a register onto the stack
PUSH	MACRO	REG
	MOVE.L	REG,-(SP)
	ENDM

*	Macro to POP a register from the stack
POP	MACRO	REG
	MOVE.L	(SP)+,REG
	ENDM

*	Macro to PUSH multiple registers onto the stack
PUSHM	MACRO	REGS
	MOVEM.L	REGS,-(SP)
	ENDM

*	Macro to POP multiple registers from the stack
POPM	MACRO	REGS
	MOVEM.L	(SP)+,REGS
	ENDM
	SPC	3
*	ORG	$900			; where we actually begin
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		V A R I A B L E S'
	PAGE

*	DS.L	490			: The Bottom of the Stack
*	DS.L	0
	ORG	$1000
STKTOP	DS.L	0			; the end (Top) of the stack

 IFNE	TRACING
.TRACE	DC.L	STRACE			; Current Trace Pointer
 ENDC

CODCASH	DC.L	0			; leave as Long, even if used as Byte
PCCASH	DC.L	0			; save PC here on crash
CASHW	DC.L	0			; information about message causing crash

 IFNE	TRACING
TR_PORT	DC.W	-1			; Trace ONLY this port if not <0
 ENDC

*	The following data is read from Engine and translated for subsequent
*	references by the base code.
ISTOME	DS.L	1			; Address to get info from ISIS
METOIS	DS.L	1			; Address to pass info to ISIS
SLTOME	DC.L	0			; Address to get parameters from slot
METOSL	DC.L	0			; Address to put crash info into slot
.SYNC	DS.L	1			; Address to perform slot-synchronization
ORING	DS.L	1			; Dispatcher Output Ring Start Address
IRING	DS.L	1			; Dispatcher Input Ring Start Address
.SLOWC	DS.L	1			; Address of SLOWC Time
.FASTC	DS.L	1			; Address of FASTC
IRSIZE	DS.W	1			; record ISIS input-ring size here
ORSIZE	DS.W	1			; record ISIS output-ring size here


P10DAH	DC.W	0			; High data
P10DAL	DC.W	0			; low data
P10DLL	DC.B	0			; high address | low data (4-bits each)
P10ADR	DC.W	0			; low (15-bit) read/write addr. in PDP


ZERO	DS.L	0			; beginning of area to zero-out on init.


HOSTN	DS.W	1			; Host Number
HOSTP	DS.W	1			; Number of Host Ports
HSTAT	DS.W	1			; Host status | Host key
					; 0=ANSW 1=DOWN 2=SHUT 3=GONE
HOSTTO	DS.W	1			; Host-specified timeout (for Key test)
TOHOST	DS.W	1			; Current Host-Key timeout

MXPORT	DS.W	1			; maximum number of active ports possible
NPORTS	DS.W	1			; current number of active ports

AUXQ	DS.W	1			; AUX circuit key (= requesting port number)
AUXTIM	DS.L	1			; timeout for AUX circuit request
AUXX	DS.W	1			; index into AUXC
AUXC	DS.B	40			; AUX logon-string


TYPBYT	DS.B	1			; message type
HTDWN	DS.B	1			; flag -- host down if NE 0

PDPORP	DS.W	1			; start address of PDP's output ring
PDPOSZ	DS.W	1			; Size of PDP's output ring
PDPOFC	DS.W	1			; PDP output ring Fill Cursor
PDPOEC	DS.W	1			; our PDP output ring Empty Cursor
PDPIRP	DS.W	1			; start address of PDP's input ring
PDPISZ	DS.W	1			; Size of PDP's input ring
PDPIEC	DS.W	1			; PDP input ring Empty Cursor
PDPIFC	DS.W	1			; our PDP input ring Fill Cursor
INRGSP	DS.W	1			; # of bytes of space in PDP input ring
INTYBT	DS.W	1			; Type byte of MSG input from ISIS
PORTNO	DS.W	1			; Current Port number
NORNODE	DS.W	1			; normal login origination node
NORPORT	DS.W	1			; normal login origination port
DAT	DS.L	0			; DAT1--4 are both LONG, and single Bytes
DAT1	DS.B	1			; for message type
DAT2	DS.B	1			; for port number
DAT3	DS.B	1
DAT4	DS.B	1
DAT5	DS.B	48			; extra space available here
NCCT	DS.B	1			; save CCT from needle here
NSIZ	DS.B	1			; save username size from needle here
BLK	DC.B	0			; flag: non BIO usage if = 0
SLOWC	DS.L	1			; Clock within Engine
HTM	DS.L	1			; Time of last Open/Shut MSG (FASTC)
LFASTC	DS.L	1			; Time of last 1/2 sec logic

LASCOU	DS.W	1			; count of good keys

BKPR	DS.W	1			; # of bytes before applying back-pressure
IRRN	DS.W	1			; ISIS-input "throttle"
ORRN	DS.W	1			; host-output "throttle"

.BFLTS	DS.L	1			; pointer to bufferlet area
NBFLTS	DS.W	1			; number of bufferlets
R_TANK	DS.W	1			; bufferlet reserve tank

BLKFREE	DS.W	1			; pointer to block-IO free-list
BKIHEAD	DS.W	1			; block-input list head (pointer or null)
BKOHEAD	DS.W	1			; block-output list head (pointer or null)
BUFHEAD	DS.W	1			; Buffer list head (pointer or null)
SAVE1	DS.W	1
TMOC	DS.B	1
PFULL	DS.B	1			; flag for ports full (if non-zero)
DIAG	DS.B	1			; flag for diagnostic in progress

FREEBLK	DS.L	2*MAXPORT		; Block-IO Free-list storage

PORTS	DS.B	PD_SIZ*MAXPORT		; storage for port-descriptors


ENDZERO	DS.L	0			; end of area to zero-out on init.
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		T A B L E S'
	PAGE


*	TRANSLATED HOST MSG LENGTH
TRHSTLN	DC.B	0			; 00
	DC.B	12			; 01
	DC.B	12			; 02
	DC.B	0			; 03
	DC.B	0			; 04
	DC.B	0			; 05
	DC.B	0			; 06
	DC.B	0			; 07
	DC.B	3			; 08
	DC.B	3			; 09
	DC.B	3			; 0A
	DC.B	3			; 0B
	DC.B	3			; 0C
	DC.B	3			; 0D
	DC.B	3			; 0E
	DC.B	3			; 0F
	DC.B	3			; 10
	DC.B	3			; 11
	DC.B	3			; 12
	DC.B	3			; 13
	DC.B	3			; 14
	DC.B	0			; 15
	DC.B	4			; 16 ****
	DC.B	0			; 17
	DC.B	52			; 18
	DC.B	6			; 19 ****
	DC.B	0			; 1A
	DC.B	0			; 1B
	DC.B	5			; 1C
	DC.B	5			; 1D
	DC.B	5			; 1E 
	DC.B	100			; 1F ****
	DC.B	0			; 20
	DC.B	12			; 21
	DC.B	0			; 22 ****
	DC.B	0			; 23
	DC.B	0			; 24
	DC.B	0			; 25
	DC.B	0			; 26
	DC.B	0			; 27
	DC.B	0			; 28
	DC.B	0			; 29
	DC.B	0			; 2A
	DC.B	0			; 2B
	DC.B	0			; 2C
	DC.B	0			; 2D
	DC.B	0			; 2E
	DC.B	0			; 2F
	DC.B	0			; 30
	DC.B	3			; 31
	DC.B	3			; 32
	DC.B	0			; 33
	DC.B	0			; 34
	DC.B	0			; 35
	DC.B	0			; 36
	DC.B	0			; 37
	DC.B	0			; 38
	DC.B	0			; 0

*	Table of Input Baud Rates in CCT order
CCTBD	DC.B	5,2,2,2,2,1,1,1		; GET BAUD RATE FROM CCT
	DC.B	6,7,0,5,5,5,5,0
	DC.B	10,5,5,11,12,5,5,5
	DC.B	5,5,5,5,5,5,5,5 
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		B O O T S T R A P'
	PAGE
	ORG	$4F00

ENTRY	MOVE.W	#BOBCODE,A0		; intercept subsequent Engine interrupts
	MOVE.L	A0,ENGVECT		; set up interrupt location
	CLR.B	BLK			; insure not expecting BIO
	MOVE.L	A1,ISTOME		; address of ISIS-to-us message area
	MOVE.L	A2,METOIS		; address of us-to-ISIS message area
	MOVE.L	#2,(A2)
	POPM	D0-D7/A0-A6
	BSR	INTRPT			; interrupt engine
	STOP	#$2000			; Halt, Enable Interrupts

*	Here on interrupt from engine
BOBCODE	MOVE	#$2700,SR		; Disable Interrupts
	PUSHM	D0-D7/A0-A6
	MOVEA.L	ISTOME,A0		; address of ISIS-to-us message area
	MOVE.W	(A0),D0			; get high HW from there
	ANDI.W	#$0F00,D0		; examining only second nibble
	CMPI.W	#$0400,D0
	BEQ	GETIOWA			; 4 is what we expect, and is OK
	CMPI.W	#$0500,D0
	BNE	FFCRA			; anything but 4 or 5 is reason to crash
	POPM	D0-D7/A0-A6
	MOVE	#$2000,SR		; Enable Interrupts
	RTE				; got 5...just return

*	Interrupt the Engine
INTRPT	MOVE.B	#$0C0,ENGINE		; Interrupt ENGINE
	MOVE.B	#$0C1,ENGINE
	RTS

FFCRA	CMPI.W	#$0300,D0		; additional check
*	BNE.L	BCRASH			; other than 3 causes us to crash (FATAL)
	beq.s	ffcra5
	cmpi.w	#$0100,D0		; is this a load command?
*	bne.l	bcrash			; no, die a horrible death
	beq.s	ffcra2			; yes, go eat it
	move.b	#$EF,codcash		; set a crash reason
	lsr.w	#8,D0
	move.b	D0,codcash+1		; and save what we got
	movem.l	(A7)+,D0-D7/A0-A6	; clean up stack
	bsr.l	bcrash			; and go crash nicely, without returning

ffcra2	move.l	metois,A0		; yes, get ISIS message area
	move.l	#2,(A0)			; acknowledge down-load
	movem.l	(A7)+,D0-D7/A0-A6	; clean up stack
	bsr	intrpt			; go interrupt engine
	stop	#$2000			; and wait for another interrupt

FFCRA5	MOVE.B	#$EC,CODCASH		; else set reason
*	BSR.L	REGDMP			; and dump our registers
	BSR.L	HCRASH			; then crash the host
	MOVE.B	#1,HTDWN		; set host down
        POPM	D0-D7/A0-A6
	STOP	#$2000			; Halt, Enable Interrupts

GETIOWA	MOVE.L	(A0),D0			; get ISIS-to-us message
	ANDI.L	#$00FFFFFF,D0
	ORI.L	#$00E00000,D0		; translate for our address-space
	MOVE.L	D0,SLTOME		; address of slot parameters
	MOVE.L	D0,METOSL		; same as Slot-to-me address
	MOVE.L	METOIS,A0		; get us-to-ISIS address
	MOVE.L	#4,(A0)			; tell ISIS "4"
	POPM	D0-D7/A0-A6		; restore registers

	BSR	INTRPT			; interrupt engine
	MOVE	#$2000,SR		; Enable Interrupts
	BRA	INITIAL			; go initialize

	ORG	$5000


*		I N I T I A L I Z A T I O N

INITIAL	LEA	STKTOP,SP		; Set up the stack pointer

*	Zero out some memory
	LEA	ZERO,A0
	MOVE.W	#ENDZERO-ZERO,D0
INIT0	CLR.W	(A0)+
	SUB.W	#2,D0
	BNE	INIT0


*	Get parameters from ISIS
	MOVE.L	SLTOME,A0		; pointer to slot parameters
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,ORING
	MOVE.W	(A0)+,ORSIZE
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,IRING
	MOVE.W	(A0)+,IRSIZE
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,.FASTC
	MOVE.L	D0,A1
	MOVE.L	(A1),LFASTC		; initialize LFASTC to now
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,.SLOWC
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,.SYNC		; used for Sync with Slot code
	MOVE.L	(A0)+,D0
	OR.L	#$00E00000,D0
	MOVE.L	D0,METOSL		; prefered place to dump registers
	MOVE.W	#VERSION,(A0)+		; return Version number
	MOVE.L	IRING,Rs		; Input Ring address into Rs
	MOVE.L	ORING,Rd		; Output Ring address into Rd

*	Initialize Bufferlet Storage
	LEA	BUFER,A0		; pointer to start of area
	MOVE.L	A0,.BFLTS		; Record pointer to start of area
	CLR.W	D0			; record cursors here
INIT1	ADD.W	#BFLSIZ,D0		; advance cursor
	LEA	BFLSIZ(A0),A0		; pointer to next buferlet
	MOVE.W	D0,-BFLSIZ(A0)		; set last forward-chain pointer
	ADD.W	#1,NBFLTS		; keep count of number of bufferlets
	CMPA.L	#BUFEREND,A0		; done yet?
	BLT	INIT1			; no...continue
	CLR.W	(A0)			; last one signaled by 0

	MOVE.B	#3,HSTAT		; Set Host status = Gone
	MOVE.B	#1,HTDWN		; set Host Down
	MOVE.W	#-1,AUXX		; Enable AUX circuit requests
	ST	CODCASH			; Initialize Crash-Code to FF
	MOVE.W	#200,BKPR		; value to apply back-pressure
 IFNE	TRACING
	MOVE.L	#STRACE,.TRACE		; PDP-10 Trace Start Address
 ENDC

	LEA	FREEBLK,A0
	MOVE.W	A0,BLKFREE		; initialize Block-IO free-list
	MOVE.W	#MAXPORT*2,D1		; one list for input and output
BLKMAP	ADD.W	#4,A0			; point to next
	MOVE.W	A0,-4(A0)		; point last at this
	SUB.W	#1,D1
	BGT	BLKMAP
	CLR.W	-4(A0)			; last has null next

	MOVE.W	#MAXPORT,D0
	LEA	PORTS,Rp		; initialize port descriptors
	CLR.W	D1			; we'll be counting port number here
PUTPORT	MOVE.B	#157,XMITLMT(Rp)	; transmit-limit
	MOVE.B	D1,P_NUM(Rp)		; set port number
	ADD.W	#1,D1			; advance port number
	ADDA.W	#PD_SIZ,Rp		; advance to next port
	SUB.W	#1,D0
	BNE	PUTPORT

	TST.W	DEXCO			; Reset EBUS
	BSR.L	DOKEY			; see if host is already up
	BNE.L	EXEC			; no
	ADD.L	#300,LFASTC		; yes...do next host check in 1/2 second
	BSR.L	RDPDPRP			; read PDP ring pointers
*	MOVE.W	#$6B,P10ADR		; from o153
*	BSR.L	GETPDPL
*	MOVE.W	D1,PDPIRP		; Input-ring Start-address
*	MOVE.W	#$6C,P10ADR		; from o154
*	BSR.L	GETPDPL
*	MOVE.W	D1,PDPISZ		; Input-ring size
*	BSR.L	GETIEC			; where PDP thinks his cursor is
*	MOVE.W	D1,PDPIFC		; ...we'll start there too
	MOVE.B	#3,DAT1			; set "RESET INTERFACE"
	MOVE.B	#DEBUG,DAT2
	MOVE.W	#VERSION,DAT3
	BSR.L	WAITDAT			; and send it
	BSR.L	WAITDAT			; twice...just in case
	BSR.L	PUTIFC			; let PDP know it's there
	BRA.L	EXEC			; then go begin
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		C R A S H E S'
	PAGE
*	CRASH Routines for the Base
*	Dump CODCASH, PCCASH, and all registers to slot's REGDMP area
*	...if we know where it is.  NO REGISTERS AFFLICTED
REGDMP	TST.L	METOSL			; do we have a place to dump to?
	BEQ	REGDMPX			; no...just exit
	PUSH	A0			; yes...save A0
	MOVE.L	METOSL,A0		; address to dump to
	MOVE.L	CODCASH,(A0)+		; dump crash reason
	MOVE.L	PCCASH,(A0)+		; PC saved
	MOVE.L	D0,(A0)+		; successive D-registers
	MOVE.L	D1,(A0)+
	MOVE.L	D2,(A0)+
	MOVE.L	D3,(A0)+
	MOVE.L	D4,(A0)+
	MOVE.L	D5,(A0)+
	MOVE.L	D6,(A0)+
	MOVE.L	D7,(A0)+
	MOVE.L	(SP),(A0)+		; Saved value of A0
	MOVE.L	A1,(A0)+		; successive A-registers
	MOVE.L	A2,(A0)+
	MOVE.L	A3,(A0)+
	MOVE.L	A3,(A0)+
	MOVE.L	A5,(A0)+
	MOVE.L	A6,(A0)+
	MOVE.L	SP,(A0)+
	POP	A0			; restore A0
REGDMPX	RTS				; exit


*	CRASH ROUTINE
*	Calling Sequence:	BSR	BCRASH
*	FATAL CRASH:	NO RETURN!!!!!!
BCRASH	MOVE.L	(SP)+,PCCASH		; Location that called this routine
	CLR.L	D0
	MOVE.W	CODCASH,D0		; left byte is crash-reason
	MOVE.B	#3,D0			; 3 in right byte means Base crashes
	MOVE.L	METOIS,A0		; ADR OF BASE-TO-ENGINE MSG AREA
	MOVE.L	D0,(A0)			; MOVE IT TO BASE-TO-ENGINE MSG AREA
	BSR	REGDMP
	BSR	INTRPT			; interrupt engine
	STOP	#$2000			; and HALT! (with Interrupts Enabled)

*	ISIS MSG-type FATAL CRASH!!
ISMGER	MOVE.W	PORTNO,CASHW		; save port, message-type
	MOVE.B	INTYBT+1,CASHW+2
	FCRASH	$50			; Fatal crash, no return!

*	BUFFER Fatal Crash..."split" (non-existant) escaped character
BAD_ESC	FCRASH	$ED			; crash if escape not followed

*	BUFFER Fatal Crash...GCI
DGCIH	FCRASH	18			; GCI crash

*	BUFFER Fatal Crash...WCI
DWCIH	FCRASH	20			; WCI crash
	SPC	3
 IFNE	TRACING
	TTL	'E B U S -- PDP-10 Base Code,		T R A C E'
	PAGE
*	Tracing is individually enabled for ISIS and PDP rings, and for
*	BIO and Buffering.

*	An attempt is made to present a uniform trace mechanism, to permit
*	the simultaneous tracing of any or all of the above.  Thus it is
*	important to define the standard used:

*	The trace consists of successive (time-ordered) trace-descriptors
*	in a Trace-ring.  This ring starts at STRACE, folds at ETRACE, and
*	the (next) descriptor address may be found in .TRACE.

*	A trace-descriptor is 8 bytes long, and has the common form:

*	| TR | dd | p# | ?? | V | A | L | U |

*	TR|dd is a pair of characters which specify the operation (TR)
*	being traced and the ction" (dd);  p# is (usually) the current
*	port;  ?? is additional information specific to the TR, and V|A|L|U
*	is the (long) value being transfered.

*	Possible forms are:
*     TR|dd	Usage			Notes
*	>R	PDP-ring input		?? is current fill-cursor
*	<R	PDP-ring output		?? is current empty-cursor
*	>B	Block-input		?? is current-address low byte
*	@B	Block-input fetch	?? is current-address low byte
*	<B	Block-output		?? is current-address low byte
*	>C	DWCI
*	<C	DGCI
*	#C	DEMPTY
*	lI	LOOK			word 3 = port, word 4 is type (right)
*	<I	ISIS input		?? is number of bytes
*	sI	SLOR			word 3 = port, word 4 is type (right)
*	>I	ISIS output		?? is number of bytes
*	Xi	FLUSH			?? is number of bytes


*	This is the heart of the Trace-code
*	It assumes one long has been filled already.
TRACE0	MOVE.L	D1,(A0)+		; second long = (D1)

*	(Almost) everything filled...should we abort?
TRACE1	MOVE.W	TR_PORT,D0		; should we trace this port only?
	BLT	TRACE2			; no
	CMP.B	P_NUM(Rp),D0		; perhaps...is this the one?
	BNE	TRACE4			; no...just abort
TRACE2	MOVE.B	P_NUM(Rp),-6(A0)	; third byte = port #
	CMPA.L	#ETRACE,A0
	BLT	TRACE3
	LEA	STRACE,A0
TRACE3	MOVE.L	A0,.TRACE
TRACE4	POPM	D0/A0
	RTS

  IFNE	ITRACE
*	TRACE ISIS Transfers

TR_IOC	PUSHM	D0/A0			; PUTCH
	MOVE.L	.TRACE,A0
	MOVE.W	#'>I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#1,(A0)+		; fourth byte = # bytes
	CLR.L	(A0)+
	MOVE.B	D0,-1(A0)		; eighth byte = char
	BRA	TRACE1

TR_IOH	PUSHM	D0/A0			; PUTH
	MOVE.L	.TRACE,A0
	MOVE.W	#'>I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#2,(A0)+		; fourth byte = # bytes
	CLR.W	(A0)+			; third word unused
	MOVE.W	D0,(A0)+		; fourth word = HW
	BRA	TRACE1

TR_IOW	PUSHM	D0/A0			; PUTW
	MOVE.L	.TRACE,A0
	MOVE.W	#'>I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#4,(A0)+		; fourth byte = # bytes
	MOVE.L	D0,(A0)+		; second long = W
	BRA	TRACE1

TR_IOS	PUSHM	D0/A0			; SLOR
	MOVE.L	.TRACE,A0
	MOVE.W	#'sI',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#3,(A0)+		; fourth byte = # bytes
	MOVE.W	D3,(A0)+		; third word = port#
	MOVE.W	D4,(A0)+		; fourth word = type
	BRA	TRACE1

TR_IIL	PUSHM	D0/A0			; LOOK
	MOVE.L	.TRACE,A0
	MOVE.W	#'lI',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#3,(A0)+		; fourth byte = # bytes
	MOVE.W	D3,(A0)+		; third word = port#
	MOVE.W	D4,(A0)+		; fourth word = type
	BRA	TRACE1

TR_IIC	PUSHM	D0/A0			; GETCH
	MOVE.L	.TRACE,A0
	MOVE.W	#'<I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#1,(A0)+		; fourth byte = # bytes
	CLR.L	(A0)+			; next long unused
	MOVE.B	D0,-1(A0)		; eighth byte = char
	BRA	TRACE1

TR_IIH	PUSHM	D0/A0			; GETH
	MOVE.L	.TRACE,A0
	MOVE.W	#'<I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#2,(A0)+		; fourth byte = # bytes
	CLR.W	(A0)+			; next word unused
	MOVE.W	D0,(A0)+		; fourth word = HW
	BRA	TRACE1

TR_IIW	PUSHM	D0/A0			; GETW
	MOVE.L	.TRACE,A0
	MOVE.W	#'<I',(A0)+		; first word = ISIS-trace flag
	MOVE.W	#4,(A0)+		; fourth byte = # bytes
	MOVE.L	D0,(A0)+		; second long = W
	BRA	TRACE1

TR_IIF	PUSHM	D0/A0			; FLUSH
	MOVE.L	.TRACE,A0
	MOVE.W	#'xI',(A0)+		; first word = ISIS-trace flag
	MOVE.W	D0,(A0)+		; fourth byte = # bytes
	CLR.L	(A0)+			; second long unused
	BRA	TRACE1

  ENDC

  IFNE	CTRACE
*	TRACE Buffer Transfers

TR_CG	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'<C',(A0)+		; first word = BUF-trace flag
	CLR.W	(A0)+			; fourth byte = unused
	CLR.L	(A0)+
	MOVE.B	D0,-1(A0)		; second long has char right-justified
	BRA	TRACE1

TR_CW	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'>C',(A0)+		; first word = BUF-trace flag
	CLR.W	(A0)+			; fourth byte = unused
	CLR.L	(A0)+
	MOVE.B	D0,-1(A0)		; second long has char right-justified
	BRA	TRACE1

TR_CE	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'@C',(A0)+		; first word = BUF-trace flag
	CLR.W	(A0)+			; fourth byte = unused
	CLR.L	(A0)+			; second long = unused
	BRA	TRACE1

  ENDC

  IFNE	BTRACE
*	TRACE Block-IO Transfers

TR_BOUT	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'<B',(A0)+		; first word = BIO-trace flag
	MOVE.W	BO_TAR+2(Rp),(A0)+	; fourth byte = low PDP address
	BRA	TRACE0

TR_BIN	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'>B',(A0)+		; first word = BIO-trace flag
	MOVE.W	BI_TAR+2(Rp),(A0)+	; fourth byte = low PDP address
	BRA	TRACE0

TR_BINX	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'@B',(A0)+		; first word = BIO-trace flag
	MOVE.W	BI_TAR+2(Rp),(A0)+	; fourth byte = low PDP address
	BRA	TRACE0

  ENDC

  IFNE	RTRACE
*	TRACE PDP-10 Transfers

TR_RIN	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'<R',(A0)+		; first word = ring-trace flag
	MOVE.W	PDPOEC,(A0)+		; fourth byte = PDP's OEC
	BRA	TRACE0

TR_ROUT	PUSHM	D0/A0
	MOVE.L	.TRACE,A0
	MOVE.W	#'>R',(A0)+		; first word = ring-trace flag
	MOVE.W	PDPIFC,(A0)+		; fourth byte = PDP's OEC
	BRA	TRACE0

  ENDC

 ENDC
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		I S I S   R I N G S'
	PAGE

*		I S I S   R I N G   R O U T I N E S


*	Conventions:
*	Routines use dedicated registers
*	A6	Output Ring Base Address
*	Rs	Input Ring Base Address
*	NOTE ....
*	   IRSIZE & ORSIZE MUST BE LESS THAN 32K


*		RING OUTPUT ROUTINES

*	Insure that there is room in ring
*	Expects	D0 = # characters of space required
*	Returns	Z FLAG = 0 means OK, 1 is no room
*	Smashes D1
ROOM	MOVE.W	2(Rd),D1		; CEI
	SUB.W	(Rd),D1			;  - NFMI
	BGT	RNOWRAP			; >0 = no wrap
	BEQ	ROOMG			; =0 = empty
	ADD.W	ORSIZE,D1		; <0 = ring-wrap, compute bias
RNOWRAP	SUB.W	#4,D1			; can't fill ring completely
	CMP.W	D0,D1			; is required space available?
	BLT	ROOMB			; no
	SUB.B	D1,D1			; yes..set Z=0
ROOMG	RTS

ROOMB	MOVE.B	#-1,D1			; no room...set Z=1
	RTS

*	Wait until there's a word available in ISIS output ring
*	All registers preserved
WAITISW	PUSHM	D0/D1
	MOVEQ	#4,D0
	BRA	WAIT

*	Wait until there's D0 bytes available in ISIS output ring
*	All registers preserved
WAITIS	PUSHM	D0/D1
	AND.W	#$00FF,D0
WAIT	BSR	ROOM
	BNE	WAIT
	POPM	D0/D1
	RTS



*	START LOGICAL OUTPUT RECORD
*	Provides function of ROOM, PUTH & PUTCH
*	for first 3 characters of message
*	D3 = PORT # (B)
*	D4 = Message type code (B)
SLOR	PUSHM	D0/D4			; save registers
	MOVE.W	(Rd),Cd			; set up cursor from NFMI
 IFNE	ITRACE
	BSR	TR_IOS			; record activity
 ENDC
	CLR.W	D0
	MOVE.B	D3,D0
	MOVE.W	D0,4(A6,D6.W)
	LSL.W	#8,D4
	MOVE.W	D4,6(A6,D6.W)
	ADD.W	#3,Cd			; advance cursor
	POPM	D0/D4
	RTS


*	Put a character into the ring
*	D0 = CHARACTER
PUTCH	PUSHM	D0/D2			; save registers
 IFNE	ITRACE
	BSR	TR_IOC			; record activity
 ENDC
	BTST	#0,Cd			; check even/odd boundary
	BEQ	PUEVEN
	MOVE.W	3(A6,D6.W),D2		; get HW to fill
	MOVE.B	D0,D2			; pack in new byte
	MOVE.W	D2,3(A6,D6.W)
	BRA	PUTODD

PUEVEN	LSL.W	#8,D0
	MOVE.W	D0,4(A6,D6.W)
PUTODD	ADD.W	#1,Cd
	CMP.W	ORSIZE,Cd		; check for folding
	BLT	PUTC1
	CLR.W	Cd
PUTC1	POPM	D0/D2
	RTS



*	Put a (short) half-word into the ring
*	D0 = HALF WORD
PUTH	MOVE.W	D0,4(A6,D6.W)
 IFNE	ITRACE
	BSR	TR_IOH			; record activity
 ENDC
	ADD.W	#2,Cd
	CMP.W	ORSIZE,Cd		; check for folding
	BLT	PUTH11
	CLR.W	Cd
PUTH11	RTS



*	Put a (long) word into the ring
*	D0 = WORD
PUTW	MOVE.L	D0,4(A6,D6.W)
 IFNE	ITRACE
	BSR	TR_IOW			; record activity
 ENDC
	ADD.W	#4,Cd
	CMP.W	ORSIZE,Cd		; check for folding
	BLT	PUTW11
	CLR.W	Cd
PUTW11	RTS


*	END LOGICAL OUTPUT RECORD
ELOR	ADD.W	#3,Cd
	AND.W	#-4,Cd
	CMP.W	ORSIZE,Cd		; check for ring-fold
	BLT	ELOR21
	CLR.W	Cd
ELOR21	MOVE.W	Cd,(Rd)			; advance NFMI, message now in ring
	RTS

*	Send a quick message to ISIS
*	Expects:	D3 = port #
*			D4 = Type-byte
*	Returns:	3-byte message in ring
SENDQI	BSR	WAITISW			; wait for ring-space
	BSR	SLOR			; start-logical-output-record
	BRA	ELOR			; exit via ELOR
	PAGE
*		INPUT ROUTINES

*	LOOK at an input ring entry
*	Returns:
*	If ring is empty, then Z FLAG = 0
*	Else Z .NE. 0, PORT returned in D3
*	Message Type returned in D4
*	All registers set up for transfer
*	Cursor is advanced past PORT #



LOOK	MOVE.W	2(Rs),Cs		; get Ring's CEI
	CMP.W	(Rs),Cs			; check if empty
	BEQ	NOIRING			; yes..exit Z=0
	MOVE.W	4(A5,D7.W),D3		; PORT #
	MOVE.W	6(A5,D7.W),D4		; MSG TYPE BYTE
	ADD.W	#3,IRRN
	LSR.W	#8,D4			; justify the type byte
	MOVE.W	D3,D0
	BSR.L	SETPORT			; set up port
 IFNE	ITRACE
	BSR	TR_IIL			; record activity
 ENDC
	ADD.W	#3,Cs			; (sets Z .NE. 0)
NOIRING	RTS				; return Z-Flag indication


*	Get a character from the Input Ring
*	Character returned in D0
GETCH	ADD.W	#1,IRRN			; count it
	BTST	#0,Cs			; getting even or odd byte?
	BEQ	EVENBON			; skip if even
	MOVE.W	3(A5,D7.W),D0		; get from odd boundary
	BRA	ODDBON

EVENBON	MOVE.W	4(A5,D7.W),D0		; get from even boundary
	LSR.W	#8,D0			; ...and justify the character
ODDBON	AND.W	#$00FF,D0		; pad out to word
 IFNE	ITRACE
	BSR	TR_IIC			; record activity
 ENDC
	ADD.W	#1,Cs			; advance and fold cursor
	CMP.W	IRSIZE,Cs
	BLT	GETCH1
	CLR.W	Cs
GETCH1	RTS				; and exit

*	Get a half-word from the Input-Ring
*	Assumed we're on correct (HW) Boundary
*	Half-word returned in D0

GETH	MOVE.W	4(A5,D7.W),D0		; get the value
 IFNE	ITRACE
	BSR	TR_IIH			; record activity
 ENDC
	ADD.W	#2,IRRN
	ADD.W	#2,Cs			; advance and fold cursor
	CMP.W	IRSIZE,Cs
	BLT	GETH1
	CLR.W	Cs
GETH1	RTS				; ...and exit

*	Get a (long) Word from the Input-ring
*	Assumed we're at appropriate boundary
*	Value returned in D0

GETW	MOVE.L	4(A5,D7.W),D0
 IFNE	ITRACE
	BSR	TR_IIW			; record activity
 ENDC
	ADD.W	#4,IRRN
	ADD.W	#4,Cs			; advance and fold cursor
	CMP.W	IRSIZE,Cs
	BLT	GETW1
	CLR.W	Cs
GETW1	RTS				; ...and exit

*	Flush an input record
*	Expects number of Chars to flush in D0
FLUSH	AND.W	#$00FF,D0		; expand number
	ADD.W	D0,Cs
 IFNE	ITRACE
	BSR	TR_IIF			; record activity
 ENDC

*	END LOGICAL INPUT RECORD
ELIR	ADD.W	#3,Cs			; round cursor up to next (long) word
	AND.W	#-4,Cs
	CMP.W	IRSIZE,Cs		; check for folding required
	BLT	ELIR2			; no
	SUB.W	IRSIZE,Cs
ELIR2	MOVE.W	Cs,2(Rs)		; now set new CEI back into Ring
	RTS				; and exit
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		P D P   I / O'
	PAGE
*		P D P - 1 0   I / O   R o u t i n e s

***	ROUTINES TO READ FROM PDP-10



*	Routine to Read a "Word" from PDP-10
*	Smashes registers:	D0-D3
RD10R	MOVE.B	#16,TMOC		; initialize counters

RD10AG	MOVEQ	#70,D1			; counter for BUSY try
RD10R1	MOVE.W	DSENS,D0		; Sense Busy
	BTST	#0,D0			; is Sense BUSY?
	BNE	RDSNB			; yes...go to BUSY code
	CLR.W	D2
	TST.B	BLK
	BEQ	BLKK2
	MOVE.B	P10DLL,D2
BLKK2	MOVE.W	D2,WT103		; place low address
	MOVE.W	P10ADR,D2
	AND.W	#$7FFF,D2		; high-bit = 0 specifies READ operation
	MOVE.W	D2,WT104		; send Low address
	MOVEQ	#70,D3			; counter for Busy loop
RD10R2	MOVE.W	DSENS,D0		; Sense Busy
	BTST	#0,D0			; Is Sense BUSY?
	BNE	RDSNB1			; yes...go to BUSY code
	MOVE.W	RD101,P10DAH		; Read high 16-bits of data from PDP
	MOVE.W	RD104,P10DAL		; Read "middle" 16-bits of data from PDP
	MOVE.W	RD102,D2		; Read low 4-bits of data from PDP
	AND.W	#$000F,D2		; zero out high-order bits
	MOVE.B	D2,P10DLL		; and save low 4-bits of data
	MOVE.W	DPER,D2			; Check if there is a parity error
	BTST	#0,D2
	BNE.L	PARERR			; yes..process parity error
	RTS				; ...else return

RDSNB	SUB.B	#1,D1
	BNE	RD10R1			; another try if < 70 times
	BRA.L	BUSCRAS

TMOUT	SUB.B	#1,TMOC
	BEQ.L	TMOUT1
	TST.W	DEXCO			; Reset EBUS
	BRA	RD10AG

RDSNB1	MOVE.W	DTMOT,D2		; test for time-out
	BTST	#0,D2
	BNE	TMOUT			; yes...go process timeout
	MOVE.W	DPER,D2			; check if Parity error
	BTST	#0,D2
	BNE.L	PARERR			; yes...go process parity error
	SUB.B	#1,D3
	BNE	RD10R2			; another try if < 70
	BRA.L	BUSCRAS

*	Read from PDP, get (left-most) high 32-bit message into D1
GETPDPL	PUSHM	D0/D2-D3
	BSR	RD10R			; read from PDP
	MOVE.L	P10DAH,D1		;  ...into D1
	POPM	D0/D2-D3
	RTS

*	Read from PDP, get low-order 32-bits into D1
GETPDPR	PUSHM	D0/D2-D3
	BSR	RD10R			; read from PDP
	MOVE.L	P10DAH,D1
	LSL.L	#4,D1			; left-justify 28-bits of data
	MOVE.B	P10DLL,D2		; low 4 bits
	AND.B	#$0F,D2			; clear high order bits
	OR.B	D2,D1			; merge with previous result
	POPM	D0/D2-D3
	RTS

*	Read Word indicated by PDPOEC & put left (high) 32 bits in D1
*	and in DAT.  Also advance and wrap PDPOEC
RDPDP	PUSHM	D0/D2
	ADD.W	#1,ORRN
	MOVE.W	PDPORP,P10ADR		; PDP output ring start address
	MOVE.W	PDPOEC,D2		; get output ring cursor
	ADD.W	D2,P10ADR		; address of current word in ring
	BSR	GETPDPL			; get the word from ring
	MOVE.L	D1,DAT
 IFNE	RTRACE
	BSR	TR_RIN			; record activity
 ENDC
	ADD.W	#1,D2			; advance cursor
	CMP.W	PDPOSZ,D2		; check if to fold it
	BNE	RDPDP1
	CLR.W	D2			; yes
RDPDP1	MOVE.W	D2,PDPOEC		; save new cursor
	POPM	D0/D2
	RTS


*	Read another word from a storage-block in the PDP-10 to DAT
*	and increment pointer
*	Returns:	DAT and D1 = full-word of data read
*			No other Registers Disturbed
RDBLK	PUSHM	D0/D2-D3
	CLR.L	D0
	MOVE.L	BO_TAR(Rp),D0
	BSR.L	SETBLKA			; set up BLOCK-IO address
	BSR	RD10R
	CLR.B	BLK
	MOVE.L	P10DAH,D1
	MOVE.L	D1,DAT1
 IFNE	BTRACE
	BSR	TR_BOUT			; record activity
 ENDC
	ADD.L	#1,BO_TAR(Rp)
	POPM	D0/D2-D3
	RTS

*	Read PDP input-ring Empty-Cursor
GETIEC	MOVE.W	#$6D,P10ADR		; from o155
	BSR	GETPDPR
	MOVE.W	D1,PDPIEC
	RTS

*	Read PDP Output-ring Fill Cursor
GETOFC	MOVE.W	#$71,P10ADR		; from o161
	BSR	GETPDPR
	MOVE.W	D1,PDPOFC
	RTS

*	Read PDP's Ring-parameters
*	First read output ring parms, then input ring parms

RDPDPRP	MOVE.W	#$6F,P10ADR
	BSR	GETPDPL
	MOVE.W	D1,PDPORP
	MOVE.W	#$70,P10ADR
	BSR	GETPDPL
	MOVE.W	D1,PDPOSZ
	BSR	GETOFC
	MOVE.W	#$72,P10ADR
	BSR	GETPDPR
	MOVE.W	D1,PDPOEC
	MOVE.W	#$6B,P10ADR
	BSR	GETPDPL
	MOVE.W	D1,PDPIRP
	MOVE.W	#$6C,P10ADR
	BSR	GETPDPL
	MOVE.W	D1,PDPISZ
	BSR	GETIEC
	MOVE.W	#$6E,P10ADR
	BSR	GETPDPR
	MOVE.W	D1,PDPIFC
	RTS
	SPC	6
*	Set up Block-IO address
*	Expects:	D0 = address (may be more than 15 bits) (L)
*	Returns:	P10ADR and P10DLL set up; BLK set
*			D0 smashed; No other Registers Disturbed
SETBLKA	MOVE.W	D0,P10ADR		; low-order 15-bits
	ROL.L	#1,D0			; now manipulate high-order bits
	SWAP	D0
	LSL.W	#4,D0
	MOVE.B	D0,P10DLL		; high-order 4-bits
	MOVE.B	#-1,BLK
	RTS
	PAGE
*	Routines to write to PDP-10


*	Routine to write to PDP-10
*	Smashes Registers:	D0-D2
WR10R	MOVEQ	#70,D1			; counter for Busy try

*	Come back here to retry operation
WR10R1	MOVE.W	DSENS,D0		; Busy Sense
	BTST	#0,D0			; Is Sense Busy?
	BNE	WRSNB			; yes, go to Busy routine
	NOT.W	P10DAH			; MIC will invert bits
	MOVE.W	P10DAH,WT101		; transmit 1st HW to write
	NOT.W	P10DAL			; MIC will invert bits
	MOVE.W	P10DAL,WT102		; transmit 2nd HW to write
	MOVE.B	P10DLL,D2		; transmit last 4 bits of data
	TST.B	BLK
	BNE	BLKK1
	NOT.B	D2			; MIC will invert bits
	AND.W	#$000F,D2		; clear high order bits
BLKK1	MOVE.W	D2,WT103		; move data and address to register 3
	MOVE.W	P10ADR,D2		; get address within PDP
	OR.W	#$8000,D2		; High-bit = 1 specifies WRITE
	MOVE.W	D2,WT104		; to register 3
	RTS

WRSNB	SUB.B	#1,D1
	BNE	WR10R1			; another try if < 70 times
	BRA.L	BUSCRAS


*	Write into PDP (high), insuring that low 4 bits are ON!
PUTNOT0	PUSHM	D0-D3
	MOVE.L	D1,P10DAH
	CLR.B	P10DLL			; all 1's to last 4 bits
	BSR	WR10R
	POPM	D0-D3
	RTS

*	Crash the Host
HCRASH	MOVE.L	CODCASH,D1		; get reason
	LSR.L	#8,D1			; code into high 16 bits
	MOVE.W	#$68,P10ADR		; put into KEY at o150
	TST.W	DEXCO			; Reset EBUS
	BSR	PUTNOT0
	MOVE.W	#$18,P10ADR		; address o30
	MOVE.L	PDPOEC,D1		; return: Output Empty Cursor |
	MOVE.W	PDPIFC,D1		;	  Input Fill Cursor
	TST.W	DEXCO			; Reset EBUS
	BSR	PUTNOT0			; send it, insure word != 0
	TST.W	DEXCO			; Reset EBUS
	RTS				; then return


*	Write D1 to high (left) 32 bits within PDP-10
PUTPDPL	PUSHM	D0/D2-D3
	MOVE.L	D1,P10DAH		; deposit data
	MOVE.B	#$0F,P10DLL		; all 0's to last 4 bits
	BSR	WR10R
	POPM	D0/D2-D3
	RTS

*	Write D1 to low (right) 32 bits within PDP-10
PUTPDPR	PUSHM	D0-D3
	MOVE.B	D1,P10DLL		; last 4 bits to P10DLL
	LSR.L	#4,D1			; right-justify value
	MOVE.L	D1,P10DAH
	BSR	WR10R			; call write routine
	POPM	D0-D3
	RTS

*	Write another word to a storage-block in the PDP-10
*	and increment pointer
*	Expects:	DAT = full-word of data to write
*	Returns:	No Registers Disturbed
WRBLK	PUSHM	D0-D3
	MOVE.L	DAT,D1
	MOVE.L	D1,P10DAH		; save value to write
 IFNE	BTRACE
	BSR.L	TR_BIN			; record activity
 ENDC
	CLR.L	D0			; compute PDP-address to write to
	MOVE.L	BI_TAR(Rp),D0
	BSR	SETBLKA			; set up the BLOCK-IO address
	BSR	WR10R
	CLR.B	BLK			; clear BIO flag
	ADD.L	#1,BI_TAR(Rp)
	POPM	D0-D3
	RTS

*	Get current output word from a storage-block in the PDP-10
*	Returns:	Word into DAT;	No Registers Disturbed
GETBLK	PUSHM	D0-D3
	CLR.L	D0			; compute PDP-address to read from
	MOVE.L	BI_TAR(Rp),D0
	BSR	SETBLKA			; set up the BLOCK-IO address
	BSR	GETPDPL			; get the word from the ring
	CLR.B	BLK			; clear BIO flag
	MOVE.L	D1,DAT			; save word read
 IFNE	BTRACE
	BSR.L	TR_BINX			; record activity
 ENDC
	POPM	D0-D3
	RTS


*	Wait for space in PDP ring, then write DAT to it
WAITDAT	BSR	WAITPDP			; wait, then go on

*	Write MSG (from DAT) to PDP ring & update base pointer
WRDAT	MOVE.L	DAT,D1			; load it and fall into WRPDP

*	Write MSG (in D1) to PDP ring & update base pointer
WRPDP	PUSHM	D1/D2
 IFNE	RTRACE
	BSR	TR_ROUT			; record activity in trace
 ENDC
	MOVE.W	PDPIRP,P10ADR		; Input ring Start Address
	MOVE.W	PDPIFC,D2		; our fill cursor
	ADD.W	D2,P10ADR		; address in ring
	BSR	PUTPDPL
	ADD.W	#1,D2			; advance cursor
	CMP.W	PDPISZ,D2		; does new cursor = ring size?
	BNE	WRIRTS			; no
WRINRG	BSR	GETIEC			; yes...locate current PDP empty cursor
	BEQ	WRINRG			; spin until it moves off 0
	CLR.W	D2			; ...then wrap our cursor
WRIRTS	MOVE.W	D2,PDPIFC		; save new fill cursor
	POPM	D1/D2
	RTS

*	Write Input-ring Fill Cursor
PUTIFC	MOVE.W	#$6E,P10ADR		; at o156
	CLR.L	D1
	MOVE.W	PDPIFC,D1
	BSR	PUTPDPR
	RTS

***	Write our Output-ring Empty cursor to PDP
PUTOEC	CLR.L	D1
	MOVE.W	PDPOEC,D1
	MOVE.W	#$72,P10ADR		; at o162
	BSR	PUTPDPR			; write into PDP
	RTS


*	Wait until (at least) 4 bytes available in PDP-10 ring
WAITPDP	BSR	FDRGSZ			; find ring size
	TST.W	INRGSP
	BEQ	WAITPDP			; wait until non-zero
	RTS


*	Find Cursor, then Calculate # of bytes of space left in PDP-10 Ring
FDRGSZ	PUSHM	D0/D1
	BSR	GETIEC			; first read cursor
	MOVE.W	PDPIEC,D1		; PDP empty pointer
	SUB.W	PDPIFC,D1		;  - our fill pointer
	BLE	CARWRAP			; ring wrap...skip
	SUB.W	#1,D1			; fill ahead of empty, insure no overflow
	LSL.W	#2,D1			; * 4 to get number of bytes
	MOVE.W	D1,INRGSP		; this is space available
	BRA	CARRTS

CARWRAP	MOVE.W	PDPISZ,D1		; empty ahead of fill...
	SUB.W	PDPIFC,D1		; ring size - our fill cursor
	LSL.W	#2,D1			; * 4 to get number of bytes
	MOVE.W	D1,INRGSP		; this is space (to end of ring)
	MOVE.W	INTYBT,D1		; check type
	BEQ	NODALN			; Skip if Needle
	CMP.W	#$9E,D1			; Data MSG?
	BGE	NODALN			; no...alignment not required
	TST.W	PORTNO			; port0 MSG?
	BNE	CARRTS			; no...must be data...just go exit
NODALN	MOVE.W	PDPIEC,D1		; non-data messages CAN wrap in ring!
	BEQ	CARRTS			; no more if null
	SUB.W	#1,D1			; insure no overrun
	LSL.W	#2,D1			; times 4
	ADD.W	D1,INRGSP		; actual space allows for wrap
CARRTS	POPM	D0/D1
	RTS
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		B U F F E R   S T U F F'
	PAGE
**********************************
**********************************
**				**
**	Bufferlet Routines	**
**				**
**********************************
**********************************

*	Character handlers.
*	  These routines pass buffer char in D0
*	  A0/D1 for working registers

*	Get char from buffer and increment forward thru the buffer.
DGCI	MOVEA.L	.BFLTS,A0		; get pointer to bufferlets
	MOVE.W	BB(Rp),D1		; Get index to character
	CLR.W	D0			; return char as word
	MOVE.B	0(A0,D1.W),D0		; Pick up char
 IFNE	CTRACE
	BSR	TR_CG			; record activity
 ENDC
	SUBQ.W	#1,BCT(Rp)		; Update count
	BLE	DGCI2			; Skip if no characters left
	ADDQ.W	#1,D1			; Else increment BB value
	MOVE.W	D1,BB(Rp)		; save pointer
	AND.W	#BFLSIZ-1,D1		; Test if at end of bufferlet
	BNE	DGCI1			; skip to exit if not
	MOVE.W	BB(Rp),D1		; bufferlet exhausted
	MOVE.W	0(A0,D1.W),BB(Rp)	; get pointer to next bufferlet
	SUB.W	#BFLSIZ-2,BB(Rp)	; back it up to beginning of bufferlet
	MOVE.W	(A0),0(A0,D1.W)		; Link last bufferlet to free list
	MOVE.W	D1,(A0)			; And put at the head of the free list

DGCI1	RTS				; Then exit

*	Last char in buffer
DGCI2	BLT.L	DGCIH			; CRASH if empty
	ADD.W	#BFLSIZ-2,D1		; Bump character pointer past
*						end of bufferlet
	AND.W	#-BFLSIZ,D1		; Make chain a multiple of BFLSIZ
	MOVE.W	(A0),0(A0,D1.W)		; chain free-list to this bufferlet
	MOVE.W	D1,(A0)			; Update free list
	RTS
	SPC	3
*	Peek at next char in buffer.
*	return:	CC set EQ if buffer empty
*	else	CC set NE, char in D0.L
*			Unless ESC'ed, then D0 contains -1.W | Char.W
DPEEK	TST.W	BCT(Rp)			; is buffer empty?
	BEQ	DPEEKX			; exit (CC EQ) if not
	MOVEA.L	.BFLTS,A0		; get pointer to bufferlets
	MOVE.W	BB(Rp),D1		; Get index to character
	CLR.L	D0			; return char as long
	MOVE.B	0(A0,D1.W),D0		; Pick up char
	CMP.B	#ESC,D0			; ESC?
	BNE	DPEEKX			; exit if not ESC
	ADDQ.W	#1,D1			; Else increment BB value
	AND.W	#BFLSIZ-1,D1		; Test if at end of bufferlet
	BNE	DPEEK1			; skip if not
	MOVE.W	BB(Rp),D1		; bufferlet exhausted
	MOVE.W	1(A0,D1.W),D1		; get pointer to next bufferlet
	SUB.W	#BFLSIZ-1,D1		; back it up to beginning of bufferlet
	BRA	DPEEK2			; and skip

DPEEK1	MOVE.W	BB(Rp),D1		; examine next char

DPEEK2	MOVE.L	#$FFFF0000,D0		; ESC'ed char...return -1 in high-word
	MOVE.B	1(A0,D1.W),D0		; ESC'ed char in low word
	MOVEQ	#1,D1			; insure CC NE

DPEEKX	RTS
	PAGE
*	Put a character into the buffer
DWCI	MOVEA.L	.BFLTS,A0		; get pointer to bufferlet area
 IFNE	CTRACE
	BSR	TR_CW			; record activity
 ENDC
	TST.W	BCT(Rp)			; Check count
	BLE	DWCI4			; If new buffer, set up attention flags
	ADDQ.W	#1,BCT(Rp)		; Else update count
	ADDQ.W	#1,BE(Rp)		; Advance BE
	MOVE.W	BE(Rp),D1		; Get pointer to new character position
	AND.W	#BFLSIZ-1,D1		; Test if bufferlet full
	BEQ	DWCI2			; End of bufferlet
	MOVE.W	BE(Rp),D1		; ok...get index again
	MOVE.B	D0,0(A0,D1.W)		; And save char in buffer
	RTS

*	Need a new bufferlet
DWCI2	MOVE.W	BE(Rp),D1
	TST.W	(A0)			; any more bufferlets?
	BGT	DWCI3			; Skip if there is another
	 BSR.L	WRE			; (Else open the reserve tank)

DWCI3	MOVE.W	(A0),0(A0,D1.W)		; Link new bufferlet to last one
	MOVE.W	(A0),D1			; And remove from free list
	MOVE.W	0(A0,D1.W),(A0)
	SUB.W	#BFLSIZ-2,D1		; Back up new bufferlet
	MOVE.W	D1,BE(Rp)		; Save as new BE
	MOVE.B	D0,0(A0,D1.W)		; Put the character there
	RTS

*	The buffer was empty
DWCI4	BLT.L	DWCIH			; count should NEVER be negative
	ADD.W	#1,BCT(Rp)		; Set count = 1
	TST.W	(A0)			; Any buffers on free-list?
	BGT	DWCI5			; Skip if there was one
	 BSR	WRE			; Else open the reserve tank

DWCI5	MOVE.W	(A0),D1			; Pop a bufferlet off free list
	MOVE.W	0(A0,D1.W),(A0)		; Update free list
	SUB.W	#BFLSIZ-2,D1		; Point it at first character
	MOVE.W	D1,BB(Rp)		; Set up BB
	MOVE.W	D1,BE(Rp)		; And BE
	MOVE.B	D0,0(A0,D1.W)		; Put the character there
	RTS
	PAGE
*	Move reserve storage supply to main free list
WRE	MOVE.W	R_TANK,(A0)		; Get pointer to reserve bufferlets
	BLE.L	WREH			; was empty...abort
	CLR.W	R_TANK			; Set reserve bufferlets empty (=0)
	RTS
	SPC	4
*	Return all the space used by the buffer

DEMPTY	TST.W	BCT(Rp)			; Check contents
	BEQ	C1			; empty...just exit
	CLR.W	BCT(Rp)			; not empty...set count = 0
 IFNE	CTRACE
	BSR	TR_CE			; record activity
 ENDC
	MOVEA.L	.BFLTS,A0		; get pointer to bufferlet area
	MOVE.W	BE(Rp),D0		; Get pointer to last character in buffer
	ADD.W	#BFLSIZ-2,D0		; Locate the pointer in this bufferlet
	AND.W	#-BFLSIZ,D0
	MOVE.W	(A0),0(A0,D0.W)		; Chain previous free list to this
*						bufferlet
	MOVE.W	BB(Rp),D0		; Pointer to first char in buffer
	ADD.W	#BFLSIZ-2,D0
	AND.W	#-BFLSIZ,D0
	MOVE.W	D0,(A0)			; Put the buffer's bufferlets
*						onto free list
C1	MOVEQ	#-1,D0			; "empty" flag
	RTS				; And return
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		E R R O R   H A N D L I N G'
	PAGE
*	ERROR CONDITION DETECTED, CRASH INTERFACE


NOKEY	CRASH	0			; key not refreshed by host

BADKEY	CRASH	3			; bad key read from host

*	Crash due to bad Message Type
MERROR	MOVE.L	DAT,CASHW		; save copy of DAT
	CRASH	4			; crash the interface

*	Crash due to MIC timeout
TMOUT1	CRASH	8

*	Crash due to MIC parity error
PARERR	CRASH	9

*	Crash due to  MIC too busy too long
BUSCRAS	CRASH	10

WREH	MOVE.B	#21,CODCASH		; WRE crash
	BSR.L	VCRASH			; should give us a PC

*	Routine that crashes the host for interface problems
VCRASH	MOVE.L	(SP)+,PCCASH		: save caller's address
	BSR.L	REGDMP			; dump registers down to Engine
	LEA	STKTOP,SP		; reset stack
	TST.B	DIAG
	BNE.L	DIAGERR
	TST.W	DEXCO			; Reset EBUS
	BSR	HODOWN			; set host down
	BSR	HCRASH			; crash the host
	BSR.L	EMPTY			; Empty ISIS ring
	BRA.L	EXEC			; and start over


*	HODOWN - Set HTDWN and report host down
HODOWN	MOVEQ	#1,D4
	MOVE.B	D4,HTDWN		; set down and fall through...

*	HDOWN -	Report Host Down or Gone
*	D4 = 1 for Down, 3 for Gone (B)
HDOWN	MOVE.B	HSTAT,D0		; going down and already down/gone?
	AND.B	D4,D0			; or going gone and already gone?
	EOR.B	D4,D0
	BEQ	HDN3			; yes...nothing to be done
	MOVE.B	D4,HSTAT		; no...change required
	BSR	HREPRT			; report it
	MOVE.W	#MAXPORT-1,D0		; for all ports...
HDN1	BSR	SETPORT			; set up port
	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	HDN2			; no
	BSR	RBTALL			; yes...reset everything
	BSR.L	DPORT			;  ...and remember to reduce the port-count
	MOVE.W	PORTNO,D3		; Port #
	MOVE.B	#$9F,D4			; ZAPPER
	BSR	SENDQI			; send Quick ISIS message
HDN2	MOVE.W	PORTNO,D0		; on to next port
	SUB.W	#1,D0
	BGT	HDN1
HDN3	RTS				; done...exit


*	set up Rp, PORTNO
*	Expect:	D0 = port number (W)
*	Return:	D0 smashed, Rp set up
SETPORT	MOVE.W	D0,PORTNO		; set up port number
	MULU	#PD_SIZ,D0		; compute offset into port-descriptor
	LEA	PORTS,Rp
	ADD.W	D0,Rp			; compute address of port-descriptor
	RTS



*	Special routine to manipulate bits-flags

*	Reset everything...
RBTALL	BSR.L	RBTBKO			; cancel block-output
	BSR.L	RBTBKI			; cancel block-input
	BSR	DEMPTY			; empty any buffer storage
	BSR.L	RBTBUF			; clean up buffer usage
	CLR.B	P_FLAGS(Rp)		; clear everything
	MOVE.B	#157,XMITLMT(Rp)	; initialize transmit limit
	RTS


*	Verify KEY-cell in PDP-10...
*	If expected (i.e., $CDFA68CB2), zero out key, return CC EQ
*	Else return CC NE
DOKEY	MOVE.W	#$68,P10ADR		; get KEY at o150
	BSR	GETPDPR			; get (right) bits
	CMP.L	#$DFA68CB2,D1
	BNE	DOKEYX			; no match...return with CC set NE
	CMP.B	#$CD,P10DAH		; check high bits
	BNE	DOKEYX			; no match...return with CC set NE
	CLR.L	D1			; match...zero out the key
	BSR	PUTPDPR
	SUB.W	D1,D1			; insure CC set EQ
DOKEYX	RTS				; return


*	HREPRT - Report Host status to Sup
HREPRT	MOVEQ	#14,D0			; we'll need 14 bytes of space in ring
	BSR	WAITIS			; wait until it's there
	CLR.W	D3			; for Port 0
	MOVEQ	#$10,D4			; Message Type 10
	BSR	SLOR
	CLR.B	D0			; padding (B)
	BSR	PUTCH
	MOVE.W	HOSTN,D0		; Host Number (HW)
	BEQ	HREPRTX			; abort if null host-number
	BSR	PUTH
	MOVE.W	HOSTP,D0		; Host Ports (HW)
	BSR	PUTH
	CLR.W	D0			; Relative Host# = 0 (HW)
	BSR	PUTH
	MOVE.W	HSTAT,D0		; Status | Hkey=0 (HW)
	BSR	PUTH
	MOVE.W	#PRODID,D0		; No IIX | ProductID (HW)
	BSR	PUTH
	BSR	ELOR
HREPRTX	RTS


*	EMPTY - EMPTY OUT ISIS INPUT RING
EMPTY	BSR	LOOK			; see what's next in ring
	BNE	EMP1
	RTS				; return..ring is empty

EMP1	TST.W	D3			; check port number
	BEQ	EMP2			; port0 = intra-node message
	MOVE.W	D4,D0			; Check for Needle
	BEQ	EMP5			; special handling for needles
	CMP.W	#$B2,D0			; Check for Pseudo-needle
	BEQ	EMP6			; special handling for P-needles
	CMP.W	#$9D,D0			; in range of data message?
	BLE	EMP4			; Just flush data
	CMP.W	#$C1,D0			; range check
	BGE	ISMGER
	SUB.W	#$9E,D4
	LEA	IMSSZ,A0
	BRA	EMP3

*	Flush port0 messages
EMP2	CMP.W	#$0E,D4			; range check
	BGE	ISMGER
	LEA	IPZSZ,A0

EMP3	MOVE.B	0(A0,D4.W),D0		; table lookup for message length

EMP4	BSR	FLUSH			; remove MSG from ring
	BRA	EMPTY			; ...and get next one

*	Needle
EMP5	MOVE.B	#$9F,D4			; ZAPPER
	BSR	SENDQI			; send Quick Message to ISIS
	BSR	GETCH			; get needle-length
	BRA	EMP4

*	Psuedo-needle
EMP6	MOVE.B	#$9F,D4			; ZAPPER
	BSR	SENDQI			; send Quick Message to ISIS
	MOVEQ	#3,D0			; set pseudo-needle length
	BRA	EMP4


*	ISIS INPUT MESSAGE LENGTHS (NOT COUNTING PORT NMBR)
IMSSZ	DC.B	0			; 9E
	DC.B	0			; 9F
	DC.B	0			; A0
	DC.B	0			; A1
	DC.B	0			; A2
	DC.B	0			; A3
	DC.B	0			; A4
	DC.B	0			; A5
	DC.B	0			; A6
	DC.B	0			; A7
	DC.B	0			; A8
	DC.B	0			; A9
	DC.B	0			; AA
	DC.B	0			; AB
	DC.B	0			; AC
	DC.B	0			; AD
	DC.B	0			; AE
	DC.B	0			; AF
	DC.B	1			; B0
	DC.B	2			; B1
	DC.B	3			; B2
	DC.B	1			; B3
	DC.B	1			; B4
	DC.B	1			; B5
	DC.B	7			; B6
	DC.B	5			; B7
	DC.B	0			; B8
	DC.B	1			; B9
	DC.B	0			; BA
	DC.B	0			; BB
	DC.B	0			; BC
	DC.B	0			; BD
	DC.B	1			; BE
	DC.B	0			; BF
	DC.B	0			; C0
	DC.B	5			; C1


*	ISIS INTRA-NODE MESSAGE LENGTHS
IPZSZ	DC.B	0			; 00
	DC.B	0			; 01
	DC.B	1			; 02
	DC.B	6			; 03
	DC.B	9			; 04
	DC.B	5			; 05
	DC.B	5			; 06
	DC.B	8			; 07
	DC.B	8			; 08
	DC.B	4			; 09
	DC.B	7			; 0A
	DC.B	4			; 0B
	DC.B	7			; 0C
	DC.B	0			; 0D
	SPC	3
*		Host-port Management (soft Shut/Answer)


*	IPORT - INCREMENT NUMBER OF PORTS
IPORT	ADD.W	#1,NPORTS		; increment number
	MOVE.W	NPORTS,D0
	CMP.W	MXPORT,D0
	BGT	IPORT2			; reached limit
IPORT1	RTS

IPORT2	MOVE.B	#1,PFULL		; full (shutable)
	BRA	DPORT1


*	DPORT - DECREMENT NUMBER OF PORTS
DPORT	SUB.W	#1,NPORTS		; decrement number
	MOVE.W	NPORTS,D0
	CMP.W	MXPORT,D0
	BNE	IPORT1			; not at limit
	CLR.B	PFULL			; not full (answerable)

DPORT1	MOVEQ	#10,D0			; need 10 bytes for Message
	BSR	WAITIS			; insure there is room there
	CLR.W	D3			; port 0,
	MOVEQ	#10,D4			; message type 10d
	BSR	SLOR
	CLR.B	D0			; 1 byte of padding
	BSR	PUTCH
	MOVE.W	HOSTN,D0		; Host number (HW)
	BSR	PUTH
	MOVEQ	#1,D0
	SUB.B	PFULL,D0		; soft shut/answer (HW)
	BSR	PUTH
	CLR.W	D0			; only host is 0 (HW)
	BSR	PUTH
	BSR	ELOR
	RTS
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		E X E C   L O O P'
	PAGE


EXEC	CMPA.L	#STKTOP,SP		; if stack is empty,
	BEQ	EXEC1			; then everything is OK...
	FCRASH	$60			; Else, something is dreadfully WRONG!

EXEC1	MOVE.L	.SYNC,A0		; get sync address
	MOVE.W	(A0),D0			; did slot get back before us?
	BLT	EXEC4			; yes
	SUB.W	D0,(A0)			; no...set to 0

EXEC2	MOVE.W	#100,D0			; wait-loop
EXEC3	SUB.W	#1,D0
	BGT	EXEC3			; insure we don't tie up DMA
	TST.W	(A0)			; waiting for slot to set it
	BGE	EXEC2

*	we set SYNC to 1 while we are executing, 0 when we're done
*	Slot sets it to -1, and uses the number to generate speedometer.
EXEC4	MOVE.W	#1,(A0)			; Syncronized...reset it and proceed
	MOVE.W	HOSTN,D0		; now get host #
	LSL.W	#8,D0
	OR.W	NPORTS,D0		; and get # ports
	MOVE.W	D0,2(A0)		; give slot <HOST# | #PORTS> to display
	MOVE.W	HSTAT,4(A0)		; give slot <HSTAT> to display
	CLR.W	IRRN			; clear "throttles"
	CLR.W	ORRN
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		H O S T   O U T P U T'
	PAGE
*		O U T P U T   F R O M   P D P - 1 0   T O   I S I S



HOSTOUT	TST.B	HTDWN			; check host status
	BNE.L	HOUTX			; Host Down..Skip
	BSR	PUTOEC
	CMP.W	#$100,ORRN
	BGT	HOUTX
	BSR	GETOFC			; Read PDP output Fill cursor
	MOVE.W	PDPOFC,D1		; PDP cursor == Our cursor?
	SUB.W	PDPOEC,D1
	BEQ	HOUTX			; Yes...ring empty, go to exit
	BSR	RDPDP			; no...read a message
	MOVE.B	DAT1,TYPBYT		; extract TYPE byte
	CLR.W	D0			; extract port-number
	MOVE.B	DAT2,D0
	BSR	SETPORT			; set up ports
	BSR.L	GEHTLN			; compute ISIS message-length needed
	BSR	ROOM			; enough room in ORING?
	BNE	HOUT2			; no...go do input
	MOVE.B	TYPBYT,D1		; check for messages which require response
	BLT	TEXTMSG			; TYP > 80 "looks" < 0, is data
	CMP.B	#$2E,D1			; Block IO
	BEQ	HOUT1
	CMP.B	#$1F,D1			; Probe (query) Terminal Characteristics?
	BEQ	HOUT1
	CMP.B	#$22,D1			; Sup Clock Request?
	BEQ	HOUT1
	CMP.B	#$16,D1			; Supervisor (Aux Circuit) request?
	BEQ	HOUT1
	CMP.B	#$19,D1			; Test Pattern Probe?
	BNE.L	OTFRHST			; none of the above
HOUT1	BSR	FDRGSZ			; find the PDP's input ring size
	MOVE.W	INRGSP,D1
	CMP.W	#8,D1
	BGE.L	OTFRHST			; more than 8 bytes available, go DO IT!

HOUT2	MOVE.W	PDPOEC,D0		; Can't process message at this time...
	SUB.W	#1,D0			; back up our cursor
	BGE	HOUT3
	MOVE.W	PDPOSZ,D0		; wrapped backward...re-cycle
	SUB.W	#1,D0
HOUT3	MOVE.W	D0,PDPOEC

HOUTX	BRA.L	ISISIN			; now try to do ISIS input


*	Process Output Data Messages
TEXTMSG	MOVE.W	PORTNO,D3		; for this port
	MOVE.B	TYPBYT,D4
	SUB.B	#$80,D4			; message type is length
	MOVE.B	D4,D2			; save copy for counter
	BSR	SLOR
	LEA	DAT3,A1			; pointer to first byte to transfer
TEXTLOP	MOVE.B	(A1)+,D0		; next byte
	BSR	PUTCH
	SUB.B	#1,D2			; Done yet?
	BEQ	TEXTEND			; yes
	CMPA.W	#DAT5,A1		; have we exhausted this DAT?
	BNE	TEXTLOP			; no
	BSR	RDPDP			; yes...get another
	LEA	DAT1,A1			; re-init pointer
	BRA	TEXTLOP			; and proceed

TEXTEND	BSR	ELOR			; Done!
	BRA	HOSTOUT			; go for next message


*	Get length of Message (translate PDP MSG type to ISIS length)
*	Returns:	length in D0, A0 smashed
GEHTLN	CLR.W	D0
	MOVE.B	TYPBYT,D0		; get type byte
	BLT	NOCAL			; skip if data message (x80+ "looks" <0 )
	LEA	TRHSTLN,A0		; start addr. of MSG-length Table
	ADD.W	D0,A0			; add offset
	MOVE.B	(A0),D0			; look up length
	BRA	NOCAL5

*	Data message is $80 + length
NOCAL	SUB.B	#$80-3,D0		; remove bias, allow for ISIS header
NOCAL5	RTS
	PAGE
*		PROCESS INDIVIDUAL HOST MESSAGES

*	PROCESS MESSAGE IN RECORD
OTFRHST	CLR.W	D2
	MOVE.B	TYPBYT,D2		; message-type
	CMP.W	#$33,D2			; range-check
	BGT	MERROR			; invalid message
	ADD.W	D2,D2			; *2 to get offset
	MOVE.W	OMTAB(PC,D2.W),A1	; get target address
	JMP	(A1)			; go there


*	HOST OUTPUT MESSAGE DISPATCH TABLE
*	(	"<" -	Legal from host
*		">" -	Legal to host	)
OMTAB	DC.W	MERROR			;    00 - Illegal
	DC.W	OPEN			; <  01 - Host Open
	DC.W	SHUT			; <  02 - Host Shut
	DC.W	RESET			; <> 03 - Reset Interface
	DC.W	HOSTOUT			; <> 04 - reset-Ack (ignor if rec'd)
	DC.W	MERROR			;  > 05 - Takeover/Sup-lost
	DC.W	MERROR			;  > 06 - Externally-init circuit
	DC.W	MERROR			;  > 07 - Internally-init circuit
	DC.W	ONBP			; <> 08 - Backpressure On
	DC.W	OFFBP			; <> 09 - Backpressure Off
	DC.W	SPEC			; <> 0A - GOBBLER
	DC.W	ZAP			; <> 0B - ZAPPER
	DC.W	SPEC			; <> OC - Enter DEM
	DC.W	SPEC			; <> 0D - Leave DEM
	DC.W	SPEC			; <> 0E - Green-ball
	DC.W	SPEC			; <> 0F - Red-ball
	DC.W	SPEC			; <> 10 - Yellow-ball
	DC.W	SPEC			; <> 11 - Orange-ball
	DC.W	SPEC			; <  12 - Hang-up, forward out only
	DC.W	SPEC			; <> 13 - Enter Transparency
	DC.W	SPEC			; <> 14 - Leave Transparency
	DC.W	MERROR			;  > 15 - Black/Gray-Ball
	DC.W	SUPREC			; <  16 - SUP (AUX-circuit) Request
	DC.W	MERROR			;  > 17 - response to 16
	DC.W	SUPLOG			; <  18 - SUP Login Char
	DC.W	TP			; <  19 - Test-pattern Probe
	DC.W	MERROR			;  > 1A - Test-pattern Response
	DC.W	HSTSAD			; <  1B - Host-sad
	DC.W	ECHOON			; <  1C - Echo On
	DC.W	ECHOFF			; <  1D - Echo Off
	DC.W	STERM			; <  1E - Set Terminal Characteristic
	DC.W	PTERM			; <  1F - Probe Terminal Characteristic
	DC.W	MERROR			;  > 20 - Response to Terminal Char.
	DC.W	SHNUM			; <  21 - Set Host Number (& # ports)
	DC.W	SCLKR			; <  22 - SUP Clock Request (ignored)
	DC.W	MERROR			;  > 23 - Sup Clock Data
	DC.W	BOK1			; <  24 - Block-output request
	DC.W	MERROR			;  > 25 - Block-output done
	DC.W	BOK2			; <  26 - Block-Input Request
	DC.W	MERROR			;  > 27 - Bin Done -- Block full
	DC.W	MERROR			;  > 28 - Bin done --EOT
	DC.W	MERROR			;  > 29 - Bin Timeout
	DC.W	BOK4			; <  2A - Terminate Block-input
	DC.W	MERROR			;  > 2B - Response to 2A
	DC.W	BOK3			; <  2C - Terminate Block-output
	DC.W	MERROR			;  > 2D - Response to 2C
	DC.W	BOK5			; <> 2E - Request/Response Block-IO ports
	DC.W	MERROR			;  > 2F - Break-begin
	DC.W	MERROR			;    30 - Illegal
	DC.W	SPEC			; <> 31 - Enter Alt. Dev. mode
	DC.W	SPEC			; <> 32 - Leave Alt. Dev. mode
	DC.W	SNTO			; <  33 - Set new timeout





*	OMSG 01 - Host Answered/open
OPEN	CLR.L	D0			; set OPEN status
	BRA	OPSHT


*	OMSG 02 - Host shut
SHUT	MOVE.L	#2,D0			; set SHUT status
OPSHT	MOVE.B	HSTAT,D1
	SUB.B	#1,D1
	BNE	HST2
	CLR.W	NPORTS
TIMCHK	BSR	TSLOWC
	MOVE.L	SLOWC,D1
*	DON'T FLOOD SUP WITH OPEN/SHUT MSGS
	SUB.L	HTM,D1
	BLT	HST1
	SUB.L	#8,D1
	BGT	HST1
	BRA	TIMCHK

HST1	BSR	TSLOWC
	MOVE.L	SLOWC,HTM
HST2	MOVE.B	D0,HSTAT		; set new status
	BSR	HREPRT			; report new host status to SUP
	BRA	HOSTOUT

*	ROUTINE TO GET TIME FROM SLOWC
TSLOWC	MOVE.L	.SLOWC,A0
	MOVE.L	(A0),SLOWC
	RTS


*	OMSG 03 - RESET INTERFACE
RESET	BSR	EMPTY			; empty ISIS input-ring
	BSR	HODOWN			; set host down
	MOVE.B	#4,DAT1			; set "RESET ACK"
	MOVE.B	#DEBUG,DAT2
	MOVE.W	#VERSION,DAT3
	BSR.L	WAITDAT			; and send it
	BRA	HOSTOUT


*	OMSG 08 - Apply Backpressure
ONBP	BSET	#PF_HBP,P_FLAGS(Rp)	; set host-applied Backpressure
	BNE	HOSTOUT			; ...already has
	BSET	#PF_IBP,P_FLAGS(Rp)	; set we've-applied Backpressure
	BNE	HOSTOUT			; ...already did
	BRA	SPEC			; SPEC does the hard work


*	OMSG 09 - Release Backpressure
OFFBP	BCLR	#PF_HBP,P_FLAGS(Rp)	; Clear host-applied Backpressure
	BEQ	HOSTOUT			; ...already did
	TST.W	BCT(Rp)			; anything in buffer?
	BNE	HOSTOUT			; yes...we'll do it when buffer flushed
	BCLR	#PF_IBP,P_FLAGS(Rp)	; Clear our-applied Backpressure
	BEQ	HOSTOUT			; ...already did
	BRA	SPEC			; SPEC does the hard work


*	OMSG 0B - ZAPPER
ZAP	TST.W	PORTNO			; Check port #
	BEQ	HOSTOUT			; ignor if port 0
	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	SPEC1			; no
	BSR	DPORT			; yes...reduce port-count
	BSR	RBTALL			; reset everything for port
	BRA	SPEC1


*	OMSGS 08-14,31-32 - VARIOUS ONE-BYTE MESSAGES
SPEC	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	HOSTOUT			; ignor if not

SPEC1	CLR.W	D4
	MOVE.B	TYPBYT,D4		; message-type
	MOVE.B	SPTAB(PC,D4.W),D4	; get ISIS message-type
	MOVE.W	PORTNO,D3		; for port
	BSR	SENDQI			; send Quick ISIS message
	BRA	HOSTOUT



*	CONVERSION TABLE FOR HOST MSGS TO ISIS MSGS
SPTAB	DC.B	0,0,0,0,0,0,0,0,$0A0,$0A1
	DC.B	$0A2,$9F,$0A6,$0A7,$0AA,$0AB,$0AC,$0AD,$0AF,$0A8
	DC.B	$0A9,0,0,0,0,0,0,0,0,0
	DC.B	0,0,0,0,0,0,0,0,0,0 
	DC.B	0,0,0,0,0,0,0,0,0,$0BC
	DC.B	$0BD,0,0,0,0,0,0,0



*	OMSG 16 - SUPERVISOR REQUEST (for AUX circuit)
SUPREC	TST.W	AUXX			; index into username array
	BLT	SUPR1			; No AUX circuit in progress
	BSR	TSLOWC
	MOVE.L	SLOWC,D0		; low long since last try?
	SUB.L	AUXTIM,D0
	ANDI.W	#-32,D0
	BNE	SUPR1
	MOVE.W	#$0900,D3		; last request still in progress
	BSR	SEND17
	BRA	HOSTOUT

SUPR1	CLR.W	AUXX			; give enable request
	MOVE.W	PORTNO,AUXQ		; ...for this port
	CLR.W	D3			; response = 0
	BSR	SEND17
	BSR	TSLOWC
	MOVE.L	SLOWC,D0
	MOVE.L	D0,AUXTIM
	BSR	PUTIFC
	BRA	HOSTOUT


SEND17	MOVE.B	#$17,DAT1		; send SUP response
	MOVE.B	PORTNO+1,DAT2
	MOVE.W	D3,DAT3
	BSR	WAITDAT			; wait, then write DAT into host
	RTS

*	OMSG 18 - SUPERVISOR LOGIN CHAR
SUPLOG	MOVE.W	PORTNO,D0		; port number
	CMP.W	AUXQ,D0			; request for valid port?
	BNE	HOSTOUT			; no...ignor
	LEA	AUXC,A0			; yes...set start-address of user-string
	MOVE.W	AUXX,D0			; get index for next char
	ADD.W	D0,A0
	MOVE.B	DAT3,D1
	OR.B	#$80,D1			; set high-order bit
	MOVE.B	D1,(A0)			; save the character
	ADD.W	#1,D0			; advance index
	AND.W	#$3F,D0			; wrap it if too long
	MOVE.W	D0,AUXX			; save copy
	CMPI.B	#$BB,D1			; SEMICOLON?
	BEQ	SUPOUT
	CMPI.B	#$8D,D1			; CARRIAGE RETURN?
	BNE	HOSTOUT			; no...not done yet
SUPOUT	ADD.W	#$0B,D0			; done...pass to Sup
	BSR	WAITIS			; wait for space to send message
	CLR.W	D3			; to Port 0
	MOVEQ	#8,D4			; Request AUX Circuit
	BSR	SLOR
	CLR.B	D0			; padding (B)
	BSR	PUTCH
	MOVE.W	PORTNO,D0		; Local Key = portnumber (HW)
	BSR	PUTH
	CLR.W	D0			; No IIX (HW)
	BSR	PUTH
	MOVE.W	HOSTN,D0		; Orig. Host (HW)
	BSR	PUTH
	MOVE.W	AUXX,D2			; size of name
	MOVE.B	D2,D0			; size (B)
	BSR	PUTCH
	LEA	AUXC,A1			; address of first byte
SUPLOP	MOVE.B	(A1)+,D0		; next byte of string
	BSR	PUTCH
	SUB.W	#1,D2
	BGT	SUPLOP
	BSR	ELOR			; end request
	BRA	HOSTOUT


*	OMSG 19 - TEST PATTERN PROBE
TP	MOVE.B	#$1A,DAT1		; Test-pattern Response
	CLR.B	DAT2
	BSR	WRDAT			; write DAT into host
	BSR	RDPDP
	BSR	WRPDP			; copy next word too
	BSR	PUTIFC			; mark it as done
	BRA	HOSTOUT


*	OMSGS 1B - HOST SAD
HSTSAD	BSR	RDPDP			; get next word
	SWAP	D1			; d3/d4 to low half
	MOVEQ	#10,D0			; need 10d bytes of space
	BSR	WAITIS			; wait for room in ring
	CLR.W	D3			; Port 0 --
	MOVEQ	#3,D4			; Type 3 (diagnostic)
	BSR	SLOR
	CLR.B	D0			; Padding
	BSR	PUTCH
	MOVE.W	D1,D0			; data2 (HW)
	BSR	PUTH
	MOVE.W	#$1000,D0		; Rtype 10 | 0 (B | B)
	BSR	PUTH
	MOVE.B	#PRODID,D0		; ProductID
	BSR	PUTCH
	BSR	ELOR			; send it on
	BRA	HOSTOUT


*	OMSGS 1C & 1D - ECHO ON/OFF
ECHOON	MOVE.W	#1,DAT3			; subtype = 0, value = 1
	BRA	STERM1

ECHOFF	CLR.W	DAT3			; subtype=0, value = 0
	BRA	STERM1


*	OMSG 1E - SET TERMINAL CHARACTERISTICS
STERM	TST.B	DAT3			; check message subtype
	BLT	MERROR			; range check

STERM1	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	HOSTOUT			; ignor request for inactive ports
	CMP.B	#$13,DAT4
	BGT	MERROR			; range check
	MOVEQ	#8,D0
	BSR	WAITIS			; wait for space in ring
	CMP.B	#5,DAT3			; special handling for input baud rate
	BNE	STERM2			; skip if not
	MOVE.B	DAT4,IBRATE(Rp)		; save new input baud-rate
	BSR.L	PTMSN1			; turn around and Acknowledge it
STERM2	MOVE.W	PORTNO,D3		; port #
	MOVE.B	#$B1,D4			; Type-byte
	BSR	SLOR
	MOVE.B	DAT3,D0			; sub-type
*	CMP.B	#$0F,D0
*	BNE	NOJAP1
*	ADD.B	#1,D0			; translate subtype 15d to subtype 16d
NOJAP1	BSR	PUTCH
	MOVE.B	DAT4,D0			; value
	BEQ	STERM3			; 0 is OK
	MOVE.L	#$0007719F,D1		; check for single-bit use
	MOVE.B	DAT3,D2			; subtype
	BTST	D2,D1
	BEQ	STERM3			; multi-bit..use value
	MOVE.B	#-1,D0			; set whole byte to 1's
STERM3	BSR	PUTCH
	BSR	ELOR
	BRA	HOSTOUT




*	OMSG 1F - PROBE TERMINAL CHARACTERISTICS
PTERM	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	HOSTOUT			; ignor for inactive ports
	MOVE.B	DAT3,D2			; get request sub-type
	BLT	HOSTOUT			; range-check
	CMP.B	#63,D2			; special "probe all"?
	BEQ	PTALL			; skip if so
	CMP.B	#$13,D2			; range-check
	BGT	HOSTOUT			; ignore out of range requests
	BSR	PTMSND			; else send request
	BSR	PUTIFC			; clean up
	BRA	HOSTOUT

*	Special case for "probe all"
PTALL	MOVEQ	#$13,D2			; set counter
PTAL2	BSR	PTMSND			; send another request
	SUB.B	#1,D2
	BGE	PTAL2			; ...until done
	BSR	PUTIFC			; clean up
	BRA	HOSTOUT

*	Process one request
*	D2 = sub-type
PTMSND	CMP.B	#5,D2			; subtype 5?
	BEQ	PTMSN1			; fake out input baud rate request
	BSR	WAITISW			; insure space in ring
	MOVE.W	PORTNO,D3		; port number
	MOVE.B	#$B0,D4			; type = query parameter
	BSR	SLOR
	MOVE.B	D2,D0
*	CMP.B	#$0F,D0
*	BNE	NOJAP2
*	ADD.B	#1,D0			; type 15d translated to type 16d
NOJAP2	BSR	PUTCH			; subtype
	BSR	ELOR
	RTS

*	Report input baud rate
PTMSN1	MOVE.B	#$20,DAT1
	MOVE.B	PORTNO+1,DAT2
	MOVE.B	#5,DAT3			; sub-type = input baud rate
	MOVE.B	IBRATE(Rp),DAT4		; rate
	BSR	WRDAT			; send DAT to host
	RTS



*	OMSG 21 - SET HOST NUMBER
SHNUM	MOVEQ	#1,D4			; assume host going down
	CLR.L	D1
	MOVE.B	DAT3,D1
	CMP.W	HOSTN,D1		; is host number being changed?
	BNE	SHNUM1			; Skip if so
	MOVE.W	PORTNO,D2		; port number = number of ports
	CMP.W	HOSTP,D2		; did it change?
	BEQ	SHNUM2			; skip if not

SHNUM1	MOVEQ	#3,D4			; host or number of ports changed
*					 ...Tell SUP Host is Gone
SHNUM2	BSR	HDOWN			; make host change
	CLR.L	D2
	MOVE.B	DAT3,D2
	MOVE.W	D2,HOSTN		; update host number
	MOVE.B	DAT2,D2			; port number = number of ports
	BNE	SHNUM3
	MOVEQ	#48,D2			; 0 defaults to 48
SHNUM3	CMP.W	#MAXPORT-2,D2
	BLE	SHNUM4
	MOVE.W	#MAXPORT-2,D2		; we do not use port 0 or $FF
SHNUM4	MOVE.W	D2,HOSTP		; number of host ports
	SUB.W	#3,D2
	MOVE.W	D2,MXPORT		; maximum port number
	MOVE.B	#2,HSTAT		; set host shut
	BSR	HREPRT			; and report it
	BRA	HOSTOUT


*	OMSG 22 - SUP CLOCK REQUEST
SCLKR	BRA	HOSTOUT			; ignor for now



*	OMSGS 24,26,2A,2C,2E (BLOCK I/O)

*	---24---	REQ FOR BLOCK OUTPUT
*	Block output is simple...each Bout request is independent.
*	We transmit the block of data, and report it's transmission,
*	thereby ending that request.
BOK1	MOVE.W	DAT3,SAVE1		; save TCN value
	BSR	RDPDP			; get address
	LSR.L	#8,D1
	BTST	#PF_ACT,P_FLAGS(Rp)	; test if port is active
	BEQ	HOSTOUT			; no...ignore request
	BSET	#PF_BKO,P_FLAGS(Rp)	; set and test block output in progress
	BNE	HOSTOUT			; was already set
	MOVE.W	BLKFREE,A0		; wasn't set...get an entry off free-list
	MOVE.W	(A0),BLKFREE		; unlink from free-list
	MOVE.W	BKOHEAD,(A0)		; link list-head to element
	MOVE.W	PORTNO,2(A0)		; second word is port-number
	MOVE.W	A0,BKOHEAD		; place this element as new list-head
	MOVE.L	D1,BO_TAR(Rp)		; TAR value
	MOVE.W	SAVE1,BO_TCN(Rp)	; TCN value
	CLR.W	BO_BYT(Rp)		; BYT = 0
	BRA	HOSTOUT


*	Reset Block-output
RBTBKO	BCLR	#PF_BKO,P_FLAGS(Rp)	; clear Block-Output
	BEQ	LNKEND			; exit if not set
	LEA	BKOHEAD,A1		; was set...search for entry in list

UNLINK	CLR.L	D0
LNKLOP	MOVE.L	A1,A0			; copy NEXT to Current
	MOVE.W	(A0),D0			; get address of next element
	BEQ	LNKEND			; null...list exhausted
	MOVE.L	D0,A1			; ok...set address
	MOVE.W	2(A1),D0		; get NEXT port
	CMP.W	PORTNO,D0		; is NEXT correct element?
	BNE	LNKLOP			; no
	MOVE.W	(A1),(A0)		; found match...unlink (NEXT) element
	MOVE.W	BLKFREE,(A1)		; link element to free-list
	MOVE.W	A1,BLKFREE		; ...and make it new free-list head
LNKEND	RTS


*	Reset Block-input
RBTBKI	BCLR	#PF_BKI,P_FLAGS(Rp)	; clear Block-input active
	BEQ	LNKEND			; not set...just exit
	LEA	BKIHEAD,A1		; set address of list-head
	BRA	UNLINK			; unlink entry from this list


*	Reset BUFfer
RBTBUF	LEA	BUFHEAD,A1		; set address of list-head
	BRA	UNLINK			; unlink entry from this list


*	---26---	BLOCK INPUT REQUEST
*	Block input is more complicated...each Bin request may start
*	a series, OR continue an already-started sequence.  It is terminated
*	by a Terminate-Block-Input request;  until that is received, we must
*	buffer any data received after the last Bin service.
BOK2	BSET	#PF_BKI,P_FLAGS(Rp)	; set Block-input active
	BNE	BOK22			; already set...don't do it again
	BSR	RBTBUF			; wasn't set...insure buffer not on list
	MOVE.W	BLKFREE,A0		; get an entry off free-list
	MOVE.W	(A0),BLKFREE		; unlink from free-list
	MOVE.W	BKIHEAD,(A0)		; link block-input list to this element
	MOVE.W	PORTNO,2(A0)		; install the port number
	MOVE.W	A0,BKIHEAD		; install element as new list-head

BOK22	BCLR	#PF_BFD,P_FLAGS(Rp)	; clear Block FilleD
	MOVE.W	DAT3,BI_TCN(Rp)
	BSR	RDPDP
	LSR.L	#8,D1
	MOVE.L	D1,BI_TAR(Rp)
	CLR.W	BI_LCN(Rp)
	CLR.W	BI_BYT(Rp)
	MOVE.L	.FASTC,A2		; record current time
	MOVE.L	(A2),BI_TIM(Rp)
	TST.B	BCT(Rp)			; anything already in buffer?
	BEQ	HOSTOUT			; no
	BSR.L	BU_BK			; yes...try to empty the buffer
	BRA	HOSTOUT			; then quit


*	---2A---	TERMINATE BLOCK OUTPUT
BOK3	MOVE.B	#$2D,DAT1		; acknowledge request
	BSR	BKOTERM			; send termination
	BRA	HOSTOUT


*	Terminate Block-output -- DAT1 = reason
BKOTERM	MOVE.B	PORTNO+1,DAT2		; build message
	BSR	WAITDAT			; wait, then write DAT into host
	BSR	PUTIFC
	BSR	RBTBKO			; cancel block-output
	RTS




*	---2C---	TERMINATE BLOCK INPUT
BOK4	MOVE.B	#$2B,DAT1
	BSR	BKITERM			; send termination-message
	BSR.L	RBTBKI			; reset Block-input
	BSR.L	BU_RG			; attempt to flush buffer
	TST.W	BCT(Rp)			; anything left there?
	BEQ	HOSTOUT			; no...we're done
	BSR.L	BU_ADD			; yes...insure it will be flushed
	BRA	HOSTOUT			; and quit


*	Terminate Block-input -- Reason in DAT1
BKITERM	MOVE.B	PORTNO+1,DAT2		; set up message
	MOVE.W	BI_LCN(Rp),DAT3		; # of characters sent
	CLR.W	BI_LCN(Rp)		; ...and reset count
	BSR	WAITDAT			; wait, then write DAT into host
	BSR	PUTIFC
	BSET	#PF_BFD,P_FLAGS(Rp)	; set Block FilleD
	RTS


*	---2E---	REQ FOR BLK I/O PORTS
BOK5	BSR	WAITDAT			; wait, then return DAT into host
	BSR	PUTIFC
	BRA	HOSTOUT


*	OMSG $33 - Set New timeout value
SNTO	MOVE.W	DAT3,HOSTTO		; set new value
	MOVE.W	DAT3,TOHOST		; ...and how long we'll wait
	BRA	HOSTOUT			; that's all there is to this one...
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		I S I S   I N P U T'
	PAGE
*		I S I S   R I N G   I N P U T




*	Convenient Exit routines

*	Place port number at DAT2 and send DAT to host
PUTPN	MOVE.B	PORTNO+1,DAT2		; place port number

*	send DAT to host
SENDDAT	BSR	WRDAT

*	all routines exit to here to clean up
CLEANUP	BSR	ELIR
	BRA	ISISIN


*	flush message...length in D2
INRGFL	MOVE.W	D2,D0			; copy length
	BSR	FLUSH			; ...and flush it

*	INPUT TO PDP RING FROM ISIS RING
ISISIN	TST.B	HTDWN
	BNE.L	VERIFY			; Host not up, try Half-second logic
	BSR	PUTIFC
	MOVE.W	IRRN,D1			; time to throttle input?
	CMP.W	IRSIZE,D1
	BGT.L	VERIFY			; yes
	BSR	LOOK			; no...anything in ISIS ring?
	BEQ.L	VERIFY			; No...go do Half-second stuff
	MOVE.W	D4,INTYBT		; yes...save input Type-byte
	BSR	FDRGSZ			; compute INRGSP

*	Distinguish between Needles, Port-0, data, and control
	MOVE.W	INTYBT,D1		; get Type byte
	BEQ.L	NEEDLE			; special processing for needles
	MOVE.W	PORTNO,D0		; port 0?
	BEQ.L	IPZ			; special processing for port 0
	CMP.W	#$9E,D1			; data?
	BGE.L	IMESS			; Special processing if not

*	We have a data-message to process
	TST.W	BCT(Rp)			; is anything already buffered?
	BNE	IS_BU			; add to buffer if so
	BTST	#PF_BKI,P_FLAGS(Rp)	; is Block-input active?
	BNE.L	BLOKIN			; skip if block-input in progress
	BTST	#PF_HBP,P_FLAGS(Rp)	; has host applied back-pressure?
	BNE	IS_BUF			; skip to place in buffer if so
	MOVE.W	INRGSP,D3		; space in ring
	BEQ	IS_BUF			; skip to place in buffer if none
	SUBQ	#2,D3			; allow for header

*	Limit ourself to 30-byte messages to host
	CMP.W	#30,D3			; ring-space > 30 chars?
	BLE	ISISIN1			; less...normal processing
	MOVE.W	#30,D3			; yes...make ring-space = 30

ISISIN1	CMP.W	D1,D3			; ring < MSG length?
	BLE	ISISIN2			; yes
	MOVE.W	D1,D3			; message-length is number to move

ISISIN2	MOVE.B	D3,DAT1			; New type-byte
	OR.B	#$80,DAT1		; Header is text length + $80
	MOVE.B	PORTNO+1,DAT2		; set output port number
	LEA	DAT3,A0			; where to begin placing the characters
ISISIN3	BSR	GETCH			; copy chars
	MOVE.B	D0,(A0)+
	SUB.W	#1,INTYBT
	SUB.W	#1,D3			; done yet?
	BGT	ISISIN3			; no
	LEA	DAT,A1			; set up transmit cursor

ISISIN4	MOVE.L	(A1)+,D1		; write DAT into host
	BSR	WRPDP
	CMPA	A1,A0
	BGT	ISISIN4			; repeat for entire message
	TST.W	INTYBT			; have we split data?
	BEQ	CLEANUP			; no...finish message and on to next
	BSR	PUTIFC			; yes...insure host sees it...buffer rest



*	Buffer data because host said to...or because we split a message
IS_BUF	TST.W	BCT(Rp)			; anything already there?
	BNE	IS_BU			; yes...just go do it
	BSR.L	BU_ADD			; no...add this port to list

*	Move ISIS message to buffer
IS_BU	MOVE.W	INTYBT,D0
	ADD.W	BCT(Rp),D0		; calculate amount to be in buffer
	CMP.W	BKPR,D0			; time to set back-pressure?
	BLT	IS_BU1			; not yet
	BSET	#PF_IBP,P_FLAGS(Rp)	; set back-pressure applied
	BNE	IS_BU1			; already was set
	MOVE.W	PORTNO,D3		; for port
	MOVE.B	#$A0,D4			; apply back-pressure
	BSR	SENDQI			; send Quick ISIS message
IS_BU1	BSR	GETCH			; copy char to buffer
	BSR	DWCI
	CMP.B	#ESC,D0			; escape code?
	BNE	IS_BU2			; no
	BSR	DWCI			; yes...escape it
IS_BU2	SUB.W	#1,INTYBT
	BGT	IS_BU1
	BRA	CLEANUP			; finish this message and do next


*	BLOCK-INPUT
BLOKIN	BTST	#PF_BFD,P_FLAGS(Rp)	; was Block FilleD?
	BNE	IS_BU			; yes...place data into buffer

*	Copy from ISIS-Ring to Block:  If terminated by Block-full
*	then report termination.
IS_BK	MOVE.W	BI_TCN(Rp),D4		; get total count for block
	BSR	GETBLK			; get the word
	MOVE.W	BI_BYT(Rp),A3		; cursor within DAT
	LEA	DAT(A3),A3		; position of next byte
IS_BK1	TST.W	INTYBT			; test if anything to get
	BEQ	IS_BK2			; skip if not
	CMP.W	BI_LCN(Rp),D4		; see if any space left
	BEQ	IS_BK2			; skip if not
	BSR	GETCH			; OK...get from ring
	SUB.W	#1,INTYBT		; decrement data-counter
	ADD.W	#1,BI_LCN(Rp)		; count another char input
	MOVE.B	D0,(A3)+		; and save char
	CMPA.W	#DAT5,A3		; have we exhausted DAT?
	BLT	IS_BK1			; no
	BSR	WRBLK			; yes
	LEA	DAT,A3			; reset word cursor
	BRA	IS_BK1

IS_BK2	SUB.W	#DAT,A3			; compute displacement
	MOVE.W	A3,BI_BYT(Rp)		; save it for next time
	BEQ	IS_BK3			; at beginning of word
	BSR	WRBLK			; no...write word into PDP
	SUB.L	#1,BI_TAR(Rp)		; back up word-cursor
IS_BK3	CMP.W	BI_LCN(Rp),D4		; have we filled block?
	BEQ	IS_BK4			; yes
	MOVE.L	.FASTC,A2		; no...record current time
	MOVE.L	(A2),BI_TIM(Rp)
	BRA	CLEANUP			; close this message and go do next

*	Block-input terminated
IS_BK4	MOVE.B	#$27,DAT1
	BSR	BKITERM			; terminate block-input

	TST.W	INTYBT			; see if more to do
	BNE	IS_BU			; yes...go copy rest of message to buffer
	BRA	CLEANUP			; go clean up
	PAGE
*		ROUTINES TO PROCESS INDIVIDUAL ISIS MESSAGES


*	A NEEDLE REQUEST IS PROCESSED HERE
NEEDLE	MOVEQ	#4,D0			; we'll need 4 bytes in the ISIS ring
	BSR	ROOM			;  ...for a response
	BNE.L	VERIFY			; Ignore this pass if not enough room there
	MOVE.W	PORTNO,D3		; port number
	MOVE.W	#$B9,D4			; type = set transmit limit
	BSR	SLOR
	MOVEQ	#30,D0			; ...to 30d
	BSR	PUTCH
	BSR	ELOR
	MOVE.W	INRGSP,D1		; Also assume we'll need at least 80 bytes
	CMP.W	#80,D1			;  in the PDP to process a needle
	BLT.L	VERIFY			; Ignore this pass if not enough room there
	BSR	GETCH			; key
	BSR	GETW			; Invoice
	BSR	GETH			; Dest. Host
	BSR	GETH			; Orig. Node
	MOVE.W	D0,NORNODE
	BSR	GETH			; Orig. Host
	BSR	GETH			; Orig. Port
	MOVE.W	D0,NORPORT
	BSR	GETW			; Extra I.N. | XXX | TPC
	BSR	GETH			; Dest. Node
	BSR	GETCH			; CCT
	MOVE.B	D0,NCCT
	BSR	GETCH			; size
	MOVE.B	D0,NSIZ
	TST.B	PFULL			; ports still available?
	BNE.L	NEE2			; no
	BSR	RBTALL			; reset everything for port
	BSET	#PF_ACT,P_FLAGS(Rp)	; set port active
	BSR	IPORT			; bump number of active ports
	MOVE.B	NCCT,D0			; translate CCT
	AND.W	#$1F,D0
	LEA	CCTBD,A0
	MOVE.B	0(A0,D0.W),IBRATE(Rp)	; record Input Baud rate in case host asks

	MOVE.B	#6,DAT1			; type 6...New External Logon
	MOVE.B	PORTNO+1,DAT2		; port #
	CLR.W	DAT3
	BSR	WAITDAT			; wait, then send DAT to host

	MOVE.B	#$82,DAT1		; 2-byte Data message
	MOVE.B	NCCT,DAT3		; CCT
	MOVE.W	NORNODE,D0
	LSR.W	#6,D0
	MOVE.B	D0,DAT4			; send high 8-bits of originating node
	BSR	WAITDAT			; wait, then send DAT to Host

	MOVE.W	NORNODE,D0
	AND.W	#$3F,D0			; low 6-bits of originating node
	MOVE.B	D0,DAT3
	MOVE.B	NORPORT,DAT4		; originating port
	BSR	WAITDAT			; wait, then send DAT to host

*	Move the username from needle to DAT
	MOVE.B	NSIZ,D2			; number of bytes of username
	LEA	DAT3,A1			; copy username into DAT3+
GETUN	BSR	GETCH
	MOVE.B	D0,(A1)+
	SUB.B	#1,D2
	BGT	GETUN			; continue until copied

*	Now move the username from DAT into host
	MOVE.B	NSIZ,D2			; number of bytes
	LEA	DAT5,A1			; where to copy from
PUTUN	CMP.B	#1,D2			; single byte?
	BNE	PUTUN1			; no
	SUB.B	#1,DAT1			; yes...reduce count
PUTUN1	BSR	WAITDAT			; wait for space, then send DAT
	MOVE.W	(A1)+,DAT3		; copy next pair of bytes
	SUB.B	#2,D2			; reduce count
	BGT	PUTUN			; continue until name sent
	BRA	CLEANUP			; Done

*	Out of ports...Must zap circuit for current needle
NEE2	BSR	WAITISW			; we'll need 1 word to return Zapper
	MOVE.W	PORTNO,D3		; port number
	MOVE.B	#$BE,D4			; Type = Zap with reason
	BSR	SLOR
	CLR.W	D0			; reason = 0 -- out of ports
	BSR	PUTCH
	BSR	ELOR
	CLR.W	D2
	MOVE.B	NSIZ,D2			; number of bytes left in needle
	BRA	INRGFL			; go flush rest of needle
	PAGE
*	Determine handler for ISIS message -- D1 = INTYBT
IMESS	CMP.W	#$C1,D1			; range check on type
	BGE	ISMGER			; Bad ISIS input MSG
	SUB.W	#$9E,D1			; bias type-code to tables
	BLT	ISMGER			; Bad ISIS input MSG
	CLR.W	D0
	MOVE.B	TRISLN(PC,D1.W),D0	; is there space to proceed?
	CMP.W	INRGSP,D0		; space in ring
	BGE.L	VERIFY			; abort this go-roundie if not
	ADD.W	D1,D1			; *2
	MOVE.W	IMSTAB(PC,D1.W),A1	; look up handler address
	JMP	(A1)			; ...and go there


*	TRANSLATED ISIS MSG LENGTH
TRISLN	DC.B	0			; 9E
	DC.B	2			; 9F
	DC.B	2			; A0
	DC.B	2			; A1
	DC.B	2			; A2
	DC.B	0			; A3
	DC.B	3			; A4
	DC.B	3			; A5
	DC.B	2			; A6
	DC.B	2			; A7
	DC.B	0			; A8
	DC.B	0			; A9
	DC.B	2			; AA
	DC.B	2			; AB
	DC.B	2			; AC
	DC.B	2			; AD
	DC.B	2			; AE
	DC.B	0			; AF
	DC.B	0			; B0
	DC.B	4			; B1
	DC.B	0			; B2
	DC.B	0			; B3
	DC.B	0			; B4
	DC.B	3			; B5
	DC.B	6			; B6
	DC.B	0			; B7
	DC.B	0			; B8
	DC.B	0			; B9
	DC.B	0			; BA
	DC.B	0			; BB
	DC.B	0			; BC
	DC.B	0			; BD
	DC.B	0			; BE
	DC.B	0			; BF
	DC.B	0			; C0
	DC.B	0			; C1


*	ISIS INPUT MESSAGE DISPATCH TABLE
IMSTAB	DC.W	CLEANUP			; 9E - DETACH, IGNORE
	DC.W	ZAPPER			; 9F - ZAPPER, process and send on
	DC.W	BPON			; A0 - SET BKPR, SEND ON
	DC.W	BPOFF			; A1 - RELEASE BKPR, SEND ON
	DC.W	GOBBLER			; A2 - GOBBLER, Send on
	DC.W	CLEANUP			; A3 - FLUSH INPUT, IGNORE
	DC.W	BLACK			; A4 - BLACK BALL
	DC.W	GRAY			; A5 - GRAY BALL
	DC.W	STREAM			; A6 - ENTER DEM, Send on
	DC.W	STREAM			; A7 - LEAVE DEM, Send on
	DC.W	CLEANUP			; A8 - ENTER TRANSPARENCY, IGNORE
	DC.W	CLEANUP			; A9 - LEAVE TRANSPARENCY, IGNORE
	DC.W	STREAM			; AA - GREEN BALL, Send on
	DC.W	STREAM			; AB - RED BALL, Send on
	DC.W	YELLOW			; AC - YELLOW BALL, SEND ON
	DC.W	ISPM			; AD - ORANGE BALL, Send on immediately
	DC.W	STREAM			; AE - BREAK BEGIN, Send on
	DC.W	CLEANUP			; AF - HANG UP, IGNORE
	DC.W	CLEANUP			; B0 - QUERY TERM CHAR, IGNORE
	DC.W	ITERM			; B1 - SET TERM CHAR
	DC.W	PNEDL			; B2 - PSEUDO NEEDLE
	DC.W	ISMGER			; B3 - LOGON CHAR (we don't expect this)
	DC.W	CLEANUP			; B4 - LOGON STATUS, IGNORE
	DC.W	LOGFAL			; B5 - LOGON FAILED
	DC.W	ILOG			; B6 - LOGON OKAY
	DC.W	ISMGER			; B7 - ACCOUNTING (we don't expect this)
	DC.W	CLEANUP			; B8 - SUPER HANG, IGNORE
	DC.W	SMTLMT			; B9 - SET XMT LIMIT
	DC.W	CLEANUP			; BA - BREAK END, IGNORE
	DC.W	CLEANUP			; BB - ZAP ACKNOWLEDGE, IGNORE
	DC.W	CLEANUP			; BC - ENTER ALT DEV MODE, IGNORE
	DC.W	CLEANUP			; BD - LEAVE ALT DEV MODE, IGNORE
	DC.W	CLEANUP			; BE - ZAP WITH REASON,IGNORE
	DC.W	CLEANUP			; BF - SIIX COMMAND, IGNORE
	DC.W	CLEANUP			; C0 - TIIX COMMAND, IGNORE
	DC.W	ISMGER			; C1 - UUO (we don't expect this)


*	9F -- Zapper
ZAPPER	BCLR	#PF_ACT,P_FLAGS(Rp)	; test if port active (and reset it)
	BEQ	ISPM			; no...just proceed
	BSR	DPORT			; yes...count it gone
	BSR	RBTALL			; reset everything
	BRA	ISPM			; ...and proceed


*	A0 -- apply back-pressure
BPON	BSET	#PF_OBP,P_FLAGS(Rp)	; set output back-pressure
	BRA	ISPM

*	A1 -- release back-pressure
BPOFF	BCLR	#PF_OBP,P_FLAGS(Rp)	; clear output back-pressure
	BRA	ISPM

*	A2 -- Gobbler
GOBBLER	BTST	#PF_BKI,P_FLAGS(Rp)	; block-input active?
	BNE	ISPM			; yes...just pass it on
	BSR	DEMPTY			; no...empty the buffer
	BSR.L	BU_EMP			; release back-pressure
	BRA	ISPM			; ...then pass it on


*	A5 -- Gray ball
GRAY	CLR.B	DAT3			; d3 = 0 for grey ball
	BRA	ISPM			; then process as special message

*	A4 -- Black Ball
BLACK	MOVE.B	#-1,DAT3		; special parameter
	BRA	ISPM


*	AC - Yellow Ball
*	Special handling for Block-input active, since Yellow-ball
*	serves to indicate EOT.
YELLOW	BTST	#PF_BKI,P_FLAGS(Rp)	; is block-input active?
	BEQ	STREAM			; no...process as normal in-stream code
	BTST	#PF_BFD,P_FLAGS(Rp)	; yes...test Block-FilleD
	BNE	STREAM1			; was set...place yellow-ball into buffer
	MOVE.B	#$28,DAT1		; no...set Bin terminated by EOT
	BSR	BKITERM			; terminate block-input
					; ...then proceed with in-stream usage


*	Special Message...retain in-stream
STREAM	TST.W	BCT(Rp)			; anything in buffer?
	BEQ	ISPM			; no...just pass it on

STREAM1	MOVEQ	#ESC,D0			; Yes...place escape
	BSR	DWCI			;  ...into buffer
	MOVE.W	INTYBT,D0		;   ...followed by ISIS Code
	BSR	DWCI
	BRA	CLEANUP			; done...escaped code will be removed later


*	Single-byte ISIS message
*	MSG types 9F, A0, A1, A2, A6, A7, AA, AB, AC, AD, AE
ISPM	BSR	LOOKUP			; go translate message-type
	BRA	PUTPN			; go send it

*	Translate ISIS message-type to PDP type, place into DAT1
LOOKUP	MOVE.W	INTYBT,D1
	SUB.W	#$9F,D1
	MOVE.B	ISPTAB(PC,D1.W),DAT1	; translated message-type
	RTS

*	Translation table for ISIS special ( > $9E ) messages
ISPTAB	DC.B	11			; 9F - Zapper
	DC.B	8			; A0 - Apply backpressure
	DC.B	9			; A1 - Release backpressure
	DC.B	10			; A2 - Gobbler
	DC.B	0			; A3 - 
	DC.B	21			; A4 - Black Ball
	DC.B	21			; A5 - Gray Ball
	DC.B	12			; A6 - Enter DEM
	DC.B	13			; A7 - Leave DEM
	DC.B	0			; A8 - 
	DC.B	0			; A9 - 
	DC.B	14			; AA - Green Ball
	DC.B	15			; AB - Red Ball
	DC.B	16			; AC - Yellow Ball
	DC.B	17			; AD - Orange Ball
	DC.B	47			; AE - Break Begin


*	B1 -- Terminal-characteristics response
ITERM	MOVE.B	#$20,DAT1		; type
	BSR	GETCH			; parameter number
*	CMP.B	#$10,D0
*	BNE	NOJAP
*	SUB.B	#1,D0			; translate 16d to 15d
NOJAP	MOVE.B	D0,DAT3			; parameter code
	BSR	GETCH
	MOVE.B	D0,DAT4			; value
	BRA	PUTPN			; send it


*	B2 -- Pseudo-Needle
PNEDL	BSR	RBTALL			; first, clean up port
	BSR	GETCH			; padding
	BSR	GETH			; local key
	MOVE.B	D0,AUXKEY(Rp)		; save it
	TST.B	PFULL			; room for another circuit?
	BNE	PFULL1			; skip if not
	BSR	RBTALL			; reset everything for port
	BSET	#PF_ACT,P_FLAGS(Rp)	; set port active
	BSR	IPORT
	MOVE.W	#-1,AUXX		; enable another aux-request
	BRA	CLEANUP			; go clean up

*	Out of ports...Must zap circuit for B2
PFULL1	BSR	WAITISW			; we'll need 1 word to return Zapper
	MOVE.W	PORTNO,D3		; port number
	MOVE.B	#$BE,D4			; Type = Zap with reason
	BSR	SLOR
	CLR.W	D0			; reason = 0 -- out of ports
	BSR	PUTCH
	BSR	ELOR
	MOVEQ	#8,D0			; fake reason to "out of orig. ports"
	BRA	LOGFAL1			; tell host that Aux-circuit attempt failed


*	B5 -- Logon failure
LOGFAL	BSR	GETCH			; get reason
LOGFAL1	MOVE.W	#-1,AUXX		; enable another AUX attempt
	MOVE.B	#$17,DAT1		; message type
	MOVE.B	AUXQ+1,DAT2		; AUX circuit key
	MOVE.B	D0,DAT3			; error code
	BNE	SENDDAT			; send to PDP (unless 0)
	MOVE.B	#7,DAT3			; code 0 -> 7
	BRA	SENDDAT


*	B6 -- Logon OK
ILOG	BSR	GETCH			; padding
	BSR	GETW			; Invoice number
	BSR	GETH			; orig. host
	MOVE.W	PORTNO,D3		; for this port,
	MOVE.W	#$B9,D4			; ...set transmit limit
	BSR	SLOR
	MOVEQ	#30,D0			; ...to 30d
	BSR	PUTCH
	BSR	ELOR
	MOVE.B	#7,DAT1
	MOVE.B	PORTNO+1,DAT2
	MOVE.B	AUXKEY(Rp),DAT3		; orig port
	BSR	WRDAT			; send DAT on to PDP
	MOVE.B	#$17,DAT1		; tell PDP which AUX succeeded
	MOVE.B	AUXKEY(Rp),DAT2		; orig port
	CLR.W	DAT3			; set success-code
	BRA	SENDDAT			; send DAT to host


*	B9 - SET XMT LIMIT
SMTLMT	BSR	GETCH			; get limit
	SUB.B	#5,D0			; round down
	AND.B	#$FC,D0
	ADD.B	#2,D0			; round back up
	MOVE.B	D0,XMITLMT(Rp)		; save transmit-limit
	BRA	CLEANUP			; nothing to tell host
	PAGE
*	PROCESS PORT-ZERO MESSAGES


*	D0 = 0;  D1 = INTYBT
IPZ	CMP.W	#$10,D1			; is this valid Port 0 message?
	BGT	ISMGER			; not in range -- crash
	MOVE.B	INISLN(PC,D1),D0	; will MSG fit?
	CMP.W	INRGSP,D0		; space in ring
	BGE.L	VERIFY			; no...go to VERIFY
	ADD.W	D1,D1			; 2*message type
	MOVE.W	IPZTAB(PC,D1),A1	; get the service address
	JMP	(A1)			; go service port0 message

*	INTRA-ISIS MSG LENGTH
INISLN	DC.B	0			; 00
	DC.B	0			; 01
	DC.B	2			; 02
	DC.B	0			; 03
	DC.B	0			; 04
	DC.B	0			; 05
	DC.B	6			; 06
	DC.B	0			; 07
	DC.B	0			; 08
	DC.B	3			; 09
	DC.B	0			; 0A
	DC.B	0			; 0B
	DC.B	0			; 0C
	DC.B	2			; 0D
	DC.B	0			; 0E
	DC.B	0			; 0F
	DC.B	0			; 10


*	ISIS INTRA-NODE MESSAGE DISPATCH TABLE
IPZTAB	DC.W	ISMGER			; 00 RESTRT REQ -- Not Expected
	DC.W	ISMGER			; 01 RESTRT RESP -- Not Expected
	DC.W	SUPTAK			; 02 SUPERVISOR TAKEOVER
	DC.W	ISMGER			; 03 REPORT TO SUP -- Not Expected
	DC.W	ISMGER			; 04 HOST STATUS -- Not Expected
	DC.W	ISMGER			; 05 ACCNTNG -- Not Expected
	DC.W	IPTIME			; 06 SUP TIME
	DC.W	ISMGER			; 07 NORM CIR REQ -- Not Expected
	DC.W	ISMGER			; 08 AUX CIR REQ -- Not Expected
	DC.W	AUXERR			; 09 AUX CIRCUIT ERROR
	DC.W	ISMGER			; 0A HOST PORT AVAIL -- Not Expected
	DC.W	HSTUNA			; 0B HOST UNACCEPTABLE
	DC.W	ISMGER			; 0C HOST COST -- Not Expected
	DC.W	SUPTAK			; 0D SUPERVISOR LOST
	DC.W	ISMGER			; 0E 1-DOWN -- Not Expected
	DC.W	ISMGER			; 0F REPORT ALL HOSTS -- Not Expected
	DC.W	ISMGER			; 10 HOST STATUS -- Not Expected



*	0.2 -- Takeover (or 0.D -- Sup. Lost)
SUPTAK	MOVE.W	#-1,AUXX		; cancel outstanding AUX requests
	MOVE.B	#5,DAT1			; type 5
	CLR.B	DAT2			; port 0
	BRA	SENDDAT			; send DAT to PDP


*	0.6 -- GMT report
IPTIME	MOVE.B	#$23,DAT1
	CLR.B	DAT2			; for port 0...
	BSR	GETCH			; skip padding
	BSR	GETW
	MOVE.L	D0,DAT3			; save all bytes
	BSR	WRDAT			; send to PDP
	MOVE.L	DAT5,D1			; get next word
	CLR.W	D1			; pad it out with 0
	BSR	WRPDP			; and send it too
	BRA	CLEANUP			; then exit


*	0.9 -- AUX circuit error
AUXERR	BSR	GETCH			; padding
	BSR	GETH			; local key
	MOVE.W	#-1,AUXX		; re-enable Aux circuits
	MOVE.B	#$17,DAT1		; type 23d
	MOVE.B	D0,DAT2			; local key (port # - who made request)
	BSR	GETCH			; error code
	MOVE.B	D0,DAT3
	BNE	SENDDAT			; send DAT (unless error=0)
	MOVE.B	#7,DAT3			; 0 -> 7
	BRA	SENDDAT			; send DAT


*	0.0B -- HOST UNACCEPTABLE
HSTUNA	BSR	GETCH			; IGNORE FOR NOW *****
	BSR	GETH
	BSR	GETCH
	BRA	CLEANUP
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		H O S T   S T A T U S'
	PAGE
*		V E R I F Y   H O S T ,   C H E C K   C O N S I S T A N C Y


VERIFY	TST.B	HTDWN			; is host up?
	BEQ	HALFSEC			; yes
	BSR	HODOWN			; no, tel SUP Host is Down


*	Half-second logic.  Check if Host is up
HALFSEC	MOVE.L	.FASTC,A0
	MOVE.L	(A0),D0
	CMP.L	LFASTC,D0		; LFASTC specifies NEXT time to do it!
	BMI.L	HALFSEX			; beware of signed arithmetic
*	BLT.L	HALFSEX			; not time yet
	ADD.L	#300,D0			; we do this every half-second
	MOVE.L	D0,LFASTC		; set time for NEXT test
	TST.B	HTDWN			; check current state...
	BNE	HDOWNR			; was down..go to host-down code

*	Host is believed up...read key
*	If good, just reset timeout
*	Else if 0, timeout is in effect
*	Else, crash for bad key
HUPR	MOVEQ	#8,D2			; try to get key eight times
HUPR1	BSR	DOKEY			; verify Key usage
	BEQ	HUPR2			; got match...all is OK
	TST.L	D1			; did we read 0?
	BNE	BADKEY
	TST.B	P10DAH			; check high-bits
	BNE	BADKEY			; wasn't zero (from last try)
	SUB.B	#1,D2			; try again?
	BGE	HUPR1			; yes
	SUB.W	#1,TOHOST		; no...decrement current timeout
	BGE.L	HALFSEX			; not yet
	BRA	NOKEY			; timeout expired...go crash host

HUPR2	MOVE.W	HOSTTO,TOHOST		; reset timeout period
	BRA.L	HALFSEX			; ...and exit

*	Host believed down...read key
*	if good, get parameters, begin talking to host
HDOWNR	BSR	DOKEY			; do the key-stuff
	BNE.L	MNGOOD			; skip if no good
	ADD.W	#1,LASCOU
	MOVE.W	LASCOU,D2
	CMP.W	#4,D2
	BLT.L	HALFSEX			; wait 2 seconds before believe host
*	MOVE.W	#$73,P10ADR		; "DUMP" flag at o163
*	CLR.L	D1			; set "DUMP" finished
*	TST.W	DEXCO			; Reset EBUS
*	BSR	PUTPDPR


*	Output-ring parameters
*	MOVE.W	#$6F,P10ADR		; Read from o157
*	BSR	GETPDPL
*	MOVE.W	D1,PDPORP		; Output-ring start-address
*	MOVE.W	#$70,P10ADR		; from o160
*	BSR	GETPDPL
*	MOVE.W	D1,PDPOSZ		; Output-ring Size
*	CLR.W	PDPOEC			; initialize our output-ring empty cursor
*	BSR	PUTOEC			; ..and tell the PDP
*	BSR	GETOFC			; Get output-ring fill cursor

*	Input-ring parameters
*	MOVE.W	#$6B,P10ADR		; from o153
*	BSR	GETPDPL
*	MOVE.W	D1,PDPIRP		; Input-ring Start-address
*	MOVE.W	#$6C,P10ADR		; from o154
*	BSR	GETPDPL
*	MOVE.W	D1,PDPISZ		; Input-ring size
*	CLR.W	PDPIFC			; initialize out input-ring fill cursor
*	BSR	PUTIFC			; ...and tell the PDP
*	BSR	GETIEC			; Get input-ring empty cursor

	BSR.L	RDPDPRP			; read PDP ring pointers
	MOVE.W	#$180+DEBUG,DAT1	; set "INTERFACE ANSWERED"
	MOVE.W	#VERSION,DAT3
	BSR.L	WAITDAT			; and send it twice
*	BSR.L	WAITDAT
	CLR.B	HTDWN			; 0 = host is up
	MOVE.W	#1,TOHOST		; cancel timeouts in progress
	MOVE.W	#1,HOSTTO

MNGOOD	CLR.W	LASCOU
	BSR	EMPTY			; empty ISIS input ring

HALFSEX	EQU	*			; exit to here...
*					 ...and fall through to DOBKOUT
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		B L O C K   O U T P U T'
	PAGE
*		BLOCK OUTPUT

*	BLOCK OUTPUT IS BEING DONE HERE
DOBKOUT	TST.B	HTDWN			; quit if host down
	BNE	EXEC
	MOVE.W	BKOHEAD,D0		; any Block-output in progress?
	BEQ.L	INBKTIM			; no...just skip

BOUT	MOVE.W	D0,A0			; unchain pointer
	MOVE.W	(A0)+,SAVE1		; get NEXT
	MOVE.W	(A0),D0			; fetch port number
	BSR	SETPORT			; set up port
	BTST	#PF_OBP,P_FLAGS(Rp)	; check output back-pressure
	BNE	BOUTX			; set...don't output this time
	CLR.W	D1			; no back-pressure...set up to transmit
	MOVE.B	XMITLMT(Rp),D1		; extract transmit-limit
	MOVE.W	BO_TCN(Rp),D0		; number of characters left
	CMP.W	D1,D0			; more than we can send?
	BLE	BOUT1			; no
	MOVE.W	D1,D0			; yes...use limit instead
BOUT1	MOVE.W	D0,D4			; count = message-type
	ADD.W	#3,D0			; ...and this is how much space we'll need
	BSR	ROOM
	BNE	BOUTX			; quit if not enough room in ring
	MOVE.W	PORTNO,D3		; port number
	BSR	SLOR
	MOVE.W	BO_BYT(Rp),D3		; byte position
	LEA	DAT,A3			; compute address within DAT
	ADD.W	D3,A3

BOUT2	BSR	RDBLK			; get another word of input
BOUT3	MOVE.B	(A3)+,D0		; move another byte
	ADD.W	#1,D3			; advance cursor
	SUB.W	#1,BO_TCN(Rp)		; number of characters left
	BSR	PUTCH
	SUB.W	#1,D4			; done yet?
	BEQ	BOUT4			; yes...clean up and quit
	CMPA.W	#DAT5,A3		; is word exhausted?
	BNE	BOUT3			; not yet
	LEA	DAT1,A3			; pointer to initial byte
	CLR.W	D3			; start at byte 0 again
	BRA	BOUT2

BOUT4	BSR	ELOR			; done...send it on it's way
	CLR.W	BO_BYT(Rp)		; assume next byte is byte 0
	CMP.W	#4,D3			; is current word exhausted?
	BGE	BOUT5			; yes
	SUB.L	#1,BO_TAR(Rp)		; no...back up pointer to read again
	MOVE.W	D3,BO_BYT(Rp)		; save position of next byte
BOUT5	TST.W	BO_TCN(Rp)		; total number left..Done?
	BNE	BOUTX			; no
	MOVE.B	#$25,DAT1		; yes...report Bout done
	BSR	BKOTERM			; terminate block-output

BOUTX	MOVE.W	SAVE1,D0		; any more ports?
	BNE	BOUT			; yes...service another
*	...and fall through to INBKTIM
	SPC	3
	TTL	'E B U S  --  PDP-10 Base Code,		C O D E'
	PAGE
*		CHECK BLOCK INPUT TIMEOUT

*	Check to see if 16-seconds has elapsed since last Block-in Data
INBKTIM	MOVE.W	BKIHEAD,D0		; anything to do?
	BEQ	FL_BUF			; no...skip to service buffers
IBM	MOVE.W	D0,A0			; chain down list
	MOVE.W	(A0)+,SAVE1		; save NEXT
	MOVE.W	(A0),D0			; get port-number
	BSR	SETPORT
	MOVE.L	.FASTC,A0
	MOVE.L	(A0),D0
	SUB.L	BI_TIM(Rp),D0		; compute time elapsed since last input
	CMP.L	#9600,D0		; has it exceeded 16 seconds?
	BLT	IBK1			; no
	BTST	#PF_BFD,P_FLAGS(Rp)	; set Block FilleD
	BNE	IBK1			; already was set
	MOVE.B	#$29,DAT1		; not set previously...
	BSR	BKITERM			; terminate Block-input

IBK1	MOVE.W	SAVE1,D0
	BNE	IBM


*	Routine to put data from buffer to Host block
	MOVE.W	BKIHEAD,D0		; check block-input head
	BEQ	FL_BUF			; nothing to do here
BBLNK	MOVE.W	D0,A0			; address of next entry
	MOVE.W	(A0)+,SAVE1		; save NEXT of this block
	MOVE.W	(A0),D0			; port number
	BSR	SETPORT			; set up port
	BTST	#PF_BFD,P_FLAGS(Rp)	; was Block FilleD?
	BNE	MAD20			; yes
	TST.W	BCT(Rp)			; no...any of chars in buffer?
	BEQ	MAD20			; no
	BSR.L	BU_BK			; yes...proceed to move to block

MAD20	MOVE.W	SAVE1,D0		; done...is list exhausted?
	BNE	BBLNK			; no...do another
*					  ...then fall through to next test

*	Try to flush buffers to PDP's input-ring
FL_BUF	MOVE.W	BUFHEAD,D0		; anything to do?
	BEQ	EXEC			; no...back to beginning of EXEC loop
FL_BUF1	MOVE.W	D0,A0			; chain down list
	MOVE.W	(A0)+,SAVE1		; save NEXT
	MOVE.W	(A0),D0			; get port-number
	BSR	SETPORT
	BSR	BU_RG			; try to flush some stuff
	TST.W	BCT(Rp)			; is buffer now empty?
	BNE	FL_BUF2			; no
	BSR	RBTBUF			; yes...remove buffer from list

FL_BUF2	MOVE.W	SAVE1,D0		; any more in list?
	BNE	FL_BUF1			; proceed if more to do
	BRA	EXEC			; done...back to start of EXEC loop


*	Copy data from BUffer to PDP's input RinG for this port
BU_RG	TST.W	BCT(Rp)			; anything in buffer?
	BEQ.L	BU_EMP			; no, empty...quit
	BTST	#PF_HBP,P_FLAGS(Rp)	; has PDP applied back-pressure?
	BNE.L	BU_XIT			; yes...quit this time around
	MOVE.W	#30,INTYBT		; no...pretend we're doing data
	BSR	FDRGSZ			; find space available
	SUB.W	#2,INRGSP		; allow for header
	BLE.L	BU_XIT			; ...but quit if no room
	CMP.W	#30,INRGSP
	BLE	BU_RG1
	MOVE.W	#30,INRGSP		; don't send more than 30 bytes of data
BU_RG1	MOVE.B	PORTNO+1,DAT2
	CLR.W	D3			; clear data-counter
	CLR.W	INTYBT			; used to record escaped chars
	LEA	DAT3,A3			; place to (begin to) put data
BU_RG2	TST.W	BCT(Rp)			; anything left?
	BEQ	BU_RG4			; skip if no more
	BSR	DGCI			; else get the data
	CMP.B	#ESC,D0			; see if escape
	BNE	BU_RG3			; skip if data
	TST.W	BCT(Rp)			; still data left?
	BEQ	BAD_ESC			; crash if none
	BSR	DGCI			; else get it
	CMP.B	#ESC,D0			; see if escaped data
	BEQ	BU_RG3			; skip if so
	MOVE.W	D0,INTYBT		; else record it
	BRA	BU_RG4			; and skip

BU_RG3	MOVE.B	D0,(A3)+		; save in temp area
	ADD.W	#1,D3			; increment data-counter
	CMP.W	INRGSP,D3		; see if that's all that fits
	BNE	BU_RG2			; continue if not

BU_RG4	MOVE.B	D3,DAT1			; save data-count
	BEQ	BU_COD			; skip if no data found
	OR.B	#$80,DAT1		; make host data-MSG type
        ADD.W   #2,D3
	LEA	DAT,A3
BU_RG5	MOVE.L	(A3)+,D1		; move data to host-ring
	BSR	WRPDP			; send it (no need to wait)
	SUB.W	#4,D3
	BGT	BU_RG5			; send it all
	BSR	PUTIFC			; make sure host sees it

BU_COD	TST.W	INTYBT			; see if terminated by escaped char
	BEQ	BU_RGX			; we're done if not
	BSR	LOOKUP			; else go translate it
	MOVE.B	PORTNO+1,DAT2		; install port #
	BSR	WAITDAT			; wait, then write DAT into host
	BSR	PUTIFC			; make sure host sees it

BU_RGX	TST.W	BCT(Rp)			; is buffer now empty?
	BNE	BU_XIT			; no

BU_EMP	BCLR	#PF_IBP,P_FLAGS(Rp)	; clear back-pressure
	BEQ	BU_XIT			; wasn't set
	MOVE.W	PORTNO,D3		; was set, must clear...for port,
	MOVE.B	#$A1,D4			; release back-pressure
	BSR	SENDQI			; send Quick ISIS message

BU_XIT	RTS				; return


*	Put the current port onto the buffer-list
BU_ADD	MOVE.W	BLKFREE,A0		; get an entry off free-list
	MOVE.W	(A0),BLKFREE		; unlink from free-list
	MOVE.W	BUFHEAD,(A0)		; link list-head to element
	MOVE.W	PORTNO,2(A0)		; second word is port-number
	MOVE.W	A0,BUFHEAD		; place this element as new list-head
	RTS				; done...return


*	Copy from Buffer to Block:  If terminated by Block-full
*	or Yellow-Ball (EOT), then report termination;  if by EOT, then
*	report Yellow-ball also.
BU_BK	MOVE.W	BI_TCN(Rp),D4		; copy of # of chars expected
	BSR	GETBLK			; get the current word
	MOVE.W	BI_BYT(Rp),A3		; cursor within DAT
	LEA	DAT(A3),A3		; address of current byte
	CLR.W	INTYBT			; used to record control-codes

*	Copy until buffer-empty, block-full, or found control-code
BU_BK1	TST.W	BCT(Rp)			; anything there?
	BEQ	BU_BK3			; no, buffer empty...skip
	CMP.W	BI_LCN(Rp),D4		; block full?
	BEQ	BU_BK3			; no...skip
	BSR	DGCI			; get the char
	CMP.B	#ESC,D0			; ESC?
	BNE	BU_BK2			; no..skip to place the data
	TST.W	BCT(Rp)			; yes...anything else there?
	BEQ	BAD_ESC			; no, buffer now empty...fatal error
	BSR	DGCI			; get the code
	CMP.B	#ESC,D0			; ESC?
	BEQ	BU_BK2			; yes, data...skip to place it in block
	MOVE.W	D0,INTYBT		; no...save as termination-code
	BRA	BU_BK3			; and skip to terminate copy

BU_BK2	MOVE.B	D0,(A3)+		; save data char
	ADD.W	#1,BI_LCN(Rp)		; count it
	CMPA.W	#DAT5,A3		; have we exhausted DAT?
	BLT	BU_BK1			; no
	BSR	WRBLK			; yes
	LEA	DAT,A3			; reset word cursor
	BRA	BU_BK1

*	done with copy (for this pass)
BU_BK3	SUB.W	#DAT,A3			; compute displacement
	MOVE.W	A3,BI_BYT(Rp)		; save it for next time
	BEQ	BU_BK4			; at beginning of word?
	BSR	WRBLK			; no...write partial word into PDP
	SUB.L	#1,BI_TAR(Rp)		; back up word-cursor
BU_BK4	MOVE.B	#$27,DAT1		; assume terminated by FULL
	CMP.W	BI_LCN(Rp),D4
	BEQ	BU_BK5			; skip if that's reason
	ADD.B	#1,DAT1			; no...assume terminated by EOT
	CMP.W	#YB_CODE,INTYBT		; see if Yellow-ball found
	BEQ	BU_BK5			; skip if so
	MOVE.L	.FASTC,A2		; else record current time
	MOVE.L	(A2),BI_TIM(Rp)
	TST.W	INTYBT			; terminated by stream-code?
	BEQ	BU_RGX			; no...just exit (but check for BP)
	BSR	BU_COD			; yes...send it to host
	BRA	BU_BK			; then continue copying

*	Block-input terminated
BU_BK5	BSR	BKITERM			; terminate block-input
	BRA	BU_COD			; send code if encountered, and exit
	PAGE



	ORG	$7C00

*	D0 is the high order 32 bits to write to pdp-10
*	D1 last 4 bits are the low order 4 bits to write to 10
*	D2 is the start addr of the test
*	D3 is the ending addr of the test
*	D4 is the counter of mismatch between read and write
*	D5 is the high order 32 bits read from 10's memory
*	D6 last 4 bits are the low order 4 bits read from 10

STARTES	TST.W	DEXCO			; Reset EBUS
	CLR.B	BLK
	CLR.L	D4			; Initialize the mismatch counter
STATE12	PUSHM	D0-D4
	MOVE.W	D2,P10ADR		; move in addr(only half-word long)
	MOVE.L	D0,P10DAH		; first 32 bits of data
	AND.B	#$0F,D1			; only need last 4 bits
	MOVE.B	D1,P10DLL		; last 4 bits of data
	BSR	WR10R			; write it to 10. memory
	CLR.L	P10DAH			; clear out read area
	CLR.B	P10DLL
	BSR	RD10R			; read it back from 10's memory
	POPM	D0-D4
	CLR.L	D5
	CLR.L	D6
	MOVE.L	P10DAH,D5		; first 32 bits read rom 10
	MOVE.B	P10DLL,D6
	AND.B	#$0F,D1
	CMP.L	D5,D0
	BNE	SUB7			; go mismatch routine
	CMP.B	D6,D1
	BNE	SUB7			; go mismatch routine if not =
	CLR.L	D7			; no mismatch add 0 to counter(d4)
	BRA	NO77

SUB7	MOVE.L	#1,D7			; add 1 to counter D4 on mismatch
NO77	ADD.L	D7,D4			; add to mismatch counter
	ADD.W	#1,D2			; bump to next addr in 10 memory
	CMP.L	D2,D3			; if biggert than end addr
	BGE	STATE12			; no go another loop
NOMLOP	MOVE.L	A1,A1
	BRA	NOMLOP			; just dummy loop
	PAGE
*	Special diagnostics for manual operation and testing




	ORG	$7D00
CONWRT	MOVE.W	D2,P10ADR
	MOVE.B	#1,DIAG			; set DIAG > 0
	MOVE.L	D0,P10DAH
	AND.B	#$0F,D1
	MOVE.B	D1,P10DLL
WRTERR	TST.W	DEXCO			; Reset EBUS
	BSR	WR10R
	MOVE.W	#100,D5
WRERL	SUB.W	#1,D5
	BNE	WRERL
	BRA	WRTERR





	ORG	$7E00
	MOVE.B	#-1,DIAG		; set DIAG < 0
	MOVE.W	D2,P10ADR
REDERR	TST.W	DEXCO			; Reset EBUS
	BSR	RD10R
	MOVE.W	#100,D5
RELOP	SUB.W	#1,D5
	BNE	RELOP
	BRA	REDERR


*	Return here on diagnostic error
DIAGERR	TST.B	DIAG			; which diagnostic are we doing?
	BLT	REDERR			; < 0...read diagnostic
	BRA	WRTERR			; > 0...write diagnostic


	TTL	'E B U S  --  PDP-10 Base Code'
	END	ENTRY
 TÖ