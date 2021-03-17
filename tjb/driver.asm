	TITLE	'Device driver for parallel printer'
	STL	'Version 1.1	26-Jun-83  Tim Brengle'

*  Pull in needed XTEXTs

	XTEXT	DDDEF		Device driver communication flags
	XTEXT	DEVDEF		Capability flags, etc.
	XTEXT	ECDEF		Error code mnemonics
	XTEXT	PICDEF		PIC format
$TYPTX	EQU	31136A		H17 ROM routine to type text to console
$TBRA	EQU	31076A		H17 ROM routine to jump through a table
	XTEXT	HOSEQU		HDOS definitions needed for SETCAL.ACM
	XTEXT	SETCAL		The routines in SET.ABS
	XTEXT	DVDDEF		Device driver header

***  Assembly constants

DEBUG$	EQU	0		If 1 then print escape sequences
LP.OUT	EQU	320Q		Printer output port
LP.IN	EQU	LP.OUT+1	Printer status port
LP.CTRL EQU	LP.OUT+7	Printer control port
MODE	EQU	10101010B	Mode word: A output, B input
RESET0	EQU	00001000B	Reset line to 0
RESET1	EQU	00001001B	Reset line to 1
BUSY	EQU	00000001B	The BUSY bit
PAPER	EQU	00000010B	The PAPER OUT bit
SELECT	EQU	00000100B	The SELECT bit
FAULT	EQU	00001000B	The FAULT* bit
F.FORM	EQU	00000001B	Form feed at close?
	STL	'Code header'
	EJECT
***  Code header

	CODE	PIC

	DB	DVDFLV		Device driver flag value
	DB	DT.CW		Device capability: WRITE only
	DB	00000001B	Mounted unit mask
	DB	1		Only one unit
	DB	DT.CW		Unit 0: WRITE only
	DS	7		Units 1-7: ignored
	DB	DVDFLV		DVDFLV *if* will take set options
	DW	0		Pointer to INIT code

***  Get to the right place for the SET operations

.	SET	025Q
	ERRNZ	*-.
	DS	DVD.STE-.

***  SET entry point
*
*	ENTRY:	(A)	= Unit number
*		(DE)	= Command line buffer address
*
*	EXIT:	(PSW)	= NOT CARRY if no error
*			= CARRY if error; error code in (A)
*
SETN	EQU	*
	ERRNZ	*-DVD.STE
	MOV	B,D		Move (DE) to (BC) for $SOP
	MOV	C,E
	LXI	D,PRCTAB	Address of process table
	LXI	H,OPTTAB	Address of option table
	CALL	$SOP		Let $SOP do the processing and dispatch
	RC
	CALL	$SNA
	RZ			At end of line
	MVI	A,EC.ILO	Return "ILLEGAL OPTION"
	STC
	RET

*** Processors

HELP	CALL	$TYPTX
	DB	12Q,'Set options available are:',12Q,12Q
	DB	'[NO]FORM	Form-feed at close',12Q
	DB	'LMARGIN n      Left margin',12Q
	DB	'PAGELEN n      Number of lines per page',12Q
	DB	'PITCH	 n      Print pitch [10,12,15]',12Q
	DB	'RMARGIN n      Right margin',12Q
	DB	'SPACING n      Interline spacing [1,2,3]',12Q
	DB	'TABS	 n      Tab interval',12Q
	DB	200Q+12Q
	XRA	A
	RET

PITCH	CALL	$PBV		Read the value into L.PITCH
	LDA	L.PITCH
	CPI	10
	RZ
	CPI	12
	RZ
	CPI	15
	RZ
	MVI	A,EC.ILV
	STC
	RET

OPTTAB	DW	OPTTABE 	End of table address
	DB	6		Number of data bytes following option

	DB	'FOR','M'+200Q,FLAGI,F.FORM,F.FORM
	DW	L.FLAG
	DB	0

	DB	'LMARGI','N'+200Q,VALI,10,1,122
	DW	L.LMAR

	DB	'NOFOR','M'+200Q,FLAGI,F.FORM,0
	DW	L.FLAG
	DB	0

	DB	'PAGELE','N'+200Q,VALI,10,1,255
	DW	L.LEN

	DB	'PITC','H'+200Q,PITCHI,10,10,15
	DW	L.PITCH

	DB	'RMARGI','N'+200Q,VALI,10,10,132
	DW	L.RMAR

	DB	'SPACIN','G'+200Q,VALI,10,1,3
	DW	L.SPACE

	DB	'TAB','S'+200Q,VALI,10,0,132
	DW	L.TABS

	DB	'HEL','P'+200Q,HELPI,0,0,0,0,0
OPTTABE DB	0

PRCTAB	DS	0		$SOP process table

FLAGI	EQU	*-PRCTAB/2
	DW	$PBF

VALI	EQU	*-PRCTAB/2
	DW	$PBV

PITCHI	EQU	*-PRCTAB/2
	DW	PITCH

HELPI	EQU	*-PRCTAB/2
	DW	HELP
	STL	'Process entry point'
	EJECT
***  Get to the right place for the driver entry

	CODE	-REL
.	SET	*
	CODE	+REL
	DS	DVD.ENT-.

***  LP.DVD Process entry point
*
*	ENTRY:	(A)	= Function code to be performed
*		(BC)	= Byte count
*		(DE)	= Data buffer address
*
*	EXIT:	(PSW)	= NOT CARRY if no error
*			= CARRY if error; error code in (A)
*
*	USES:	Depends upon function called
*
*
*
START	EQU	*		HDOS comes here every time LP: is called for
	ERRNZ	START-DVD.ENT
	CPI	DC.MAX		See if requested function is in range
	JNC	ILLEGAL 	If out of range, treat as illegal function
	CALL	$TBRA		If ok, use (A) to jump through table
	DB	ILLEGAL-*	FCN 0: READ from device
	DB	WRITE-* 	FCN 1: WRITE to device
	DB	ILLEGAL-*	FCN 2: READ regardless
	DB	ILLEGAL-*	FCN 3: OPEN for READ
	DB	OPWRITE-*	FCN 4: OPEN for WRITE
	DB	ILLEGAL-*	FCN 5: OPEN for UPDATE
	DB	CLOSE-* 	FCN 6: CLOSE channel to device
	DB	ABORT-* 	FCN 7: ABORT operation
	DB	IGNORE-*	FCN 8: MOUNT device
	DB	LOAD-*		FCN 9: LOAD device driver
	DB	READY-* 	FCN 10: EXAMINE device ready status
	STL	'Driver processors'
	EJECT
ILLEGAL EQU	*		Here when an illegal function was requested
	MVI	A,EC.ILR	Put "ILLEGAL REQUEST" error code in (A)
	STC			Signal error to HDOS
	RET

IGNORE	EQU	*		Here to ignore the requested function
	XRA	A		Clear (A) and CARRY
	RET

OPWRITE EQU	*		Here to open the printer for writing
	LDA	WSTAT		Get the OPEN FOR WRITES flag
	ORA	A		Already open?
	JZ	OPWRIT1 	No: flag the new open
	MVI	A,EC.FAO	Yes: signal "FILE ALREADY OPEN"
	STC			Signal error to HDOS
	RET
OPWRIT1 MVI	A,-1		Here to flag that we are opened
	STA	WSTAT		Set the OPEN FOR WRITES flag
	MVI	A,RESET0	Start the printer resetting
	OUT	LP.CTRL
	MVI	A,RESET1	Stop the printer resetting
	OUT	LP.CTRL
	CALL	INITLP		Go initialize the printer
	XRA	A
	RET

CLOSE	LDA	L.FLAG		Here to close channel to the printer
	ANI	F.FORM		Output a form-feed?
	RZ			No: just return
	IN	LP.IN		Yes: get the printer status
	STA	STATUS		Save in case we need it later
	ANI	BUSY		Busy?
	JNZ	CLOSECK 	Yes: check for a fault
	MVI	A,14Q		Get a form-feed
	OUT	LP.OUT		Print it
	XRA	A		Clear (A) and CARRY
	STA	WSTAT		Reset the OPEN FOR WRITES flag
	RET
CLOSECK LDA	STATUS		Get back the saved status
	ANI	FAULT		Do we have a fault?
	CZ	WAIT		Yes: go wait for the fault to clear
	JMP	CLOSE		Try it again

ABORT	XRA	A		Here to abort the operation
	STA	WSTAT		Clear the OPEN FOR WRITES flag
	MVI	A,EC.DDA	Signal "DEVICE DRIVER ABORT"
	STC			And tell HDOS there is an error
	RET

LOAD	CALL	$TYPTX		Here to load the driver and init the printer
	DB	'Printer driver loaded.',200Q+12Q
	MVI	A,MODE		Set up the USART
	OUT	LP.CTRL
	MVI	A,RESET0	Reset the printer to power-up
	OUT	LP.CTRL
	MVI	A,RESET1	Stop resetting
	OUT	LP.CTRL
	XRA	A
	RET

READY	EQU	*		Here to check if the printer is ready
	IN	LP.IN		Read the printer status byte
	ANI	BUSY		Busy?
	RZ			No: just return
	STC			Yes: signal HDOS
	RET
	STL	'WRITE processor'
	EJECT
***  LP.DVD WRITE processor
*
*	ENTRY:	(BC)	= Byte count to be written
*		(DE)	= Buffer address
*
*	EXIT:	(PSW)	= NOT CARRY if no error
*			= CARRY if error; error code in (A)
*
*	USES:	ALL
*
*
WRITE	LDA	WSTAT		Here when want to write to the printer
	ORA	A		Look at the OPEN FOR WRITES flag
	MVI	A,EC.FNO	Prepare for "CHANNEL NOT OPEN" error
	STC
	RZ			Return if channel never opened
WRITE1	MOV	A,B		Check for byte count = 0
	ORA	C
	RZ			Return with NOT CARRY when all done
WRITE2	IN	LP.IN		Get printer status
	STA	STATUS		Save in case we need it later
	ANI	BUSY		Busy?
	JNZ	FAULTCK 	Yes: check for a fault
	LDAX	D		No: get the character to be output
	OUT	LP.OUT		Print it
	INX	D		Bump buffer pointer
	DCX	B		Decrement byte count
	JMP	WRITE1

FAULTCK LDA	STATUS		Get back the printer status
	ANI	FAULT		Do we have a fault?
	CZ	WAIT		Yes: tell him and wait for it to clear
	JMP	WRITE2

WAIT	LDA	STATUS		Get back the printer status
	ANI	PAPER		Out of paper?
	JZ	WAIT0		No: see if just not selected
	CALL	$TYPTX
	DB	33Q,'K',7Q,'The printer is out of paper!',15Q,200Q+7Q
	JMP	WAIT2		And go wait for it to clear
WAIT0	LDA	STATUS
	ANI	SELECT		Selected?
	JNZ	WAIT1		No: just announce a random fault
	CALL	$TYPTX
	DB	33Q,'K',7Q,'The printer is not selected!',15Q,200Q+7Q
	JMP	WAIT2
WAIT1	CALL	$TYPTX
	DB	33Q,'K',7Q,'The printer has a fault!',15Q,200Q+7Q
WAIT2	IN	LP.IN		Read the status again
	ANI	FAULT		Still have a fault?
	JZ	WAIT2		Yes: wait for it to clear
	CALL	$TYPTX
	DB	33Q,200Q+'K'	Clear out the error message
	RET

INITLP	LDA	L.LEN		Get the page length
	LXI	H,T.LEN
	CALL	TWO.HEX 	Put the two characters out
	LDA	L.SPACE 	Get the line spacing
	CPI	1		Single spacing?
	JNZ	INIT1
	MVI	A,'6'
	STA	T.SPACE
	JMP	INIT3
INIT1	CPI	2
	JNZ	INIT2
	MVI	A,'4'
	STA	T.SPACE
	JMP	INIT3
INIT2	MVI	A,'3'
	STA	T.SPACE
INIT3	LDA	L.PITCH 	Get the pitch
	CPI	10
	JNZ	INIT4
	MVI	A,'P'
	STA	T.PITCH
	JMP	INIT6
INIT4	CPI	12
	JNZ	INIT5
	MVI	A,'E'
	STA	T.PITCH
	JMP	INIT6
INIT5	MVI	A,'M'
	STA	T.PITCH
INIT6	LDA	L.LMAR		Get the left margin
	LXI	H,T.LMAR
	CALL	TWO.HEX
	LDA	L.RMAR		Get the right margin
	LXI	H,T.RMAR
	CALL	TWO.HEX 	Now should be all ready; set up for write
	LXI	B,INITLEN
	LXI	D,INITSTR
	CALL	WRITE1		Put out the initialization string
	LDA	L.LMAR		Do the tabs -- starting at the margin
	DCR	A
	STA	T.STOP
INIT7	LDA	L.TABS		(B) = increment
	MOV	B,A
	LDA	L.RMAR		(C) = limit
	MOV	C,A
	LDA	T.STOP
	ADD	B		Find the next tab stop
	CMP	C		Past the margin?
	RNC			Yes: done
	STA	T.STOP
	LXI	H,T.TABS	No: prep to write
	CALL	TWO.HEX
	LXI	B,TABLEN
	LXI	D,TABSTR
	CALL	WRITE1
	JMP	INIT7

TWO.HEX PUSH	PSW		Convert the (A) to two hex characters
	RRC
	RRC
	RRC
	RRC
	ANI	00001111B	Look at the first digit
	CPI	10		Normal digit?
	JNC	HEX1
	ADI	'0'		Yes:
	JMP	HEX2
HEX1	ADI	'A'-10		No: put up in 'A'-'F'
HEX2	MOV	M,A
	INX	H
	POP	PSW		Get (A) back
	ANI	00001111B
	CPI	10
	JNC	HEX3
	ADI	'0'
	JMP	HEX4
HEX3	ADI	'A'-10
HEX4	MOV	M,A
	RET
	STL	'Storage area'
	EJECT
***  Storage area

WSTAT	DB	0		OPEN FOR WRITE flag
STATUS	DB	0		Temp for printer status
T.STOP	DB	0		Temp for current tab stop
L.FLAG	DB	F.FORM		Flag for formfeed at close
L.LMAR	DB	1		Left margin
L.RMAR	DB	132		Right margin
L.LEN	DB	66		Page length
L.PITCH DB	15		Print pitch
L.SPACE DB	1		Interline spacing
L.TABS	DB	8		Tab spacing

	IF	DEBUG$
FF	EQU	14Q
ESC	EQU	33Q
TAB	EQU	11Q
	ELSE
FF	EQU	'|'
ESC	EQU	'&'
TAB	EQU	'>'
	ENDIF

INITSTR DB	ESC,'1' 	Clear all tabs and margins
	DB	ESC,'2' 	Clear VT and pagelength settings
	DB	ESC,FF,0,0	Page length
T.LEN	EQU	*-2
	DB	ESC,0		Line spacing
T.SPACE EQU	*-1
	DB	ESC,0		Print pitch
T.PITCH EQU	*-1
	DB	ESC,'9',0,0	Left margin
T.LMAR	EQU	*-2
	DB	ESC,'0',0,0	Right margin
T.RMAR	EQU	*-2
INITLEN EQU	*-INITSTR

TABSTR	DB	ESC,TAB,0,0	Set a tab stop
T.TABS	EQU	*-2
TABLEN	EQU	*-TABSTR

	EJECT
	LON	G		Turn on Pic table lister
	END
 