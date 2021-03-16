:
:	Carl A Baltrunas,   TTE 442	Lab #4
:	Source file for SLOT #1
:

	RA	0			: Reset radix (some libraries don't)

MAXBIT	EQ	(MAXPRT+20-1)/20	: Max ports as bits for words
WRDSIZ	EQ	4			: Bytes per word

CmBELL	EQ	07			: Bell
CmLF	EQ	0A			: Line-feed
CmCR	EQ	0D			: Carriage return
CmESC	EQ	1B			: Escape
CmSPAC	EQ	20			: Space

CmQMRK	EQ	3F			: "?" Help
CmCONN	EQ	43			: "C"onnect
CmHELP	EQ	48			: "H"elp
CmMSGS	EQ	4D			: "M"essage
CmQUIT	EQ	51			: "Q"uit
CmTIME	EQ	54			: "T"ime of day
CmWHAT	EQ	57			: "W"hat

	SEG	A.DATA

: Constant data storage

	BC	0			: Dummy
HUPMSG	BC	0E			: Msg byte count
	HC	1000,HST0,HPRT0,0,0	: Host up message (type 10)
	HC	PRODID,0		: Prod ID & filler for msg
	BC	0			: Dummy
HSTOOP	BC	8			: Msg byte count
	HC	0A00,HST0,0,0		: Host port availability (none)
	BC	0			: Dummy
HSTOKP	BC	8			: Msg byte count
	HC	0A00,HST0,HPRT0,0	: Host port availability

WELMSG	SC	\"0A"0D"0DG'Day Bubba!"0A"0DWelcome to Carl's Host"0A"0D"0D\
UIDMSG	SC	\"0A"0DUserid: \
PMTMSG	SC	\"0A"0D=> \
MxBYE	SC	\"0A"0DGo Away!!!"0A"0D"0A"0D\
MxALQ	SC	\"0A"0DQuit already in progress! Go Away!!!"0A"0D"0A"0D\
MxTIME	SC	\"0A"0DTime now is \
MxBELL	SC	\"07!\			: Ding!
MxCRLF	SC	\"0A"0D\


MxDET	HC	019E			: Detach message
MxZAP	HC	019F			: Zap message
MxEBP	HC	01A0			: Enable back-pressure message
MxDBP	HC	01A1			: Disable back-pressure message
MxGOB	HC	01A2			: Character gobbler message
MxFINP	HC	01A3			: Flush input message
MxBLB	HC	01A4			: Black ball message
MxGRB	HC	01A5			: Grey ball message
MxSDEM	HC	01A6			: Start Deferred Echo Mode
MxFDEM	HC	01A7			: Finish Deferred Echo Mode
MxGB	HC	01AA			: Green ball message
MxRB	HC	01AB			: Red ball message
MxYB	HC	01AC			: Yellow ball message
MxOB	HC	01AD			: Orange ball message
M1TP00	HC	03B1,0001		: B1 message echo-control ON
M0TP00	HC	03B1,0000		: B1 message echo-control OFF


UIDSIZ	HC	4*WRDSIZ		: Words for user id entry

CCCTAB	BC	2,2,2,2,2,2,2,1		: NUL,SOH,STX,ETX,EOT,ENQ,ACK,BEL
	BC	1,1,1,1,1,2,2,2		: BS ,HT ,LF ,VT ,FF ,CR ,SO ,SI
	BC	2,2,2,2,2,2,2,2		: DLE,DC1,DC2,DC3,DC4,NAK,SYN,ETB
	BC	2,2,2,1,2,2,2,2		: CAN,EM ,SUB,ESC,FS ,GS ,RS ,US

MxHELP	SC	\Cmd   Function"0A"0D\
MhHELP	SC	\ X    Type this message\
:MhCONN	SC	\ X    Connect a port to another user-port\
:MhMSGS	SC	\ X    Message a another port\
MhQUIT	SC	\ X    Leave the system (LOG OFF)\
:MhSTAT	SC	\ X    Type system statistics\
MhTIME	SC	\ X    Type current time of day\
MhWHAT	SC	\ X    Type user-port information\

CmdMAC	MACRO(Chr,Fnc,Hlp,ExtHlp) [	WC	Chr*01000000+Fnc
					HC	Hlp,ExtHlp	   ]
	WS	0	: Make sure word boundary
CmdTAB						: Command table
CmdChr	EQ	(.-CmdTAB)			:   Offset for Character
CmdFnc	EQ	(.-CmdTAB)			:   Offset for Dispatch
	WC	CmQMRK*01000000+CxHELP		:   Function address
CmdHlp	EQ	(.-CmdTAB)			:   Offset for Help text
	HC	MhHELP,0			:   Help message address
CmdSiz	EQ	(.-CmdTAB)			:   Entry size

	CmdMAC(	CmHELP,CxHELP,MhHELP,MhHELP )	: Help
:	CmdMAC(	CmCONN,CxCONN,MhCONN,MhCONN )	: Connect
:	CmdMAC(	CmMSGS,CxMSGS,MhMSGS,MhMSGS )	: Message
	CmdMAC(	CmQUIT,CxQUIT,MhQUIT,MhQUIT )	: Quit
:	CmdMAC(	CmSTAT,CxSTAT,MhSTAT,MhSTAT )	: Statistics
	CmdMAC(	CmTIME,CxTIME,MhTIME,MhTIME )	: Time
	CmdMAC(	CmWHAT,CxWHAT,MhWHAT,MhWHAT )	: What (Who)
CmdLEN	EQ	(.-CmdTAB)/CmdSiz
: Variable data

TCLASS	BS	MAXPRT			: Throughput class of port
CCTYPE	BS	MAXPRT			: CCT for each port
NFROM	HS	MAXPRT			: Node of origin for each port
HFROM	HS	MAXPRT			: Host of origin for each port
PORTID	HS	MAXPRT			: Address of each Port Id string

MsMSGC	BS	1			: Byte count for single char string
MsCHAR	BS	1			: Current character
MsPORT	HS	1			: Current message port
MsTYPE	HS	1			: Current message type
MsINDX	HS	1			: Index to save
CHRCNT	HS	1			: Place to hold character count
LOGINS	WS	1			: Count number of active logins

: Flags

WAITB1	WS	MAXBIT			: Bit array - Waiting for B1 return
ECHOME	WS	MAXBIT			: Bit array - Can echo characters
ECHOHT	WS	MAXBIT			: Bit array - Can echo HT
ECHOCR	WS	MAXBIT			: Bit array - Can echo CR (RUB on LF)
ECHOBS	WS	MAXBIT			: Bit array - Can echo BS
ECHOES	WS	MAXBIT			: Bit array - Can echo ESC
LOGGED	WS	MAXBIT			: Bit array - Logged-in
LOGDET	WS	MAXBIT			: Bit array - DET sent
NEEDID	WS	MAXBIT			: Bit array - need Id
NEEDCR	WS	MAXBIT			: Bit array - need CR

PORTSC	WS	MAXPRT*4		: Storage for port id's

SvTIME	SC	\HH:MM:SS"0A"0D\	: Time of day
MxWHAT	SC	\  Port Username      ST    Node    Host  CCT TPC "0A"0D\
SvWHAT	SC	\  *PP  ------------  --  NNNNNN  HHHHHH  CCT TPC "0A"0D\
WxMINE	HC	0120+00,SvWHAT+03	: * = My port
WxPORT	HC	0220+10,SvWHAT+04	: Port mask
WxUSER	HC	0C20+00,SvWHAT+08	: User ID
WxSTAT	HC	0220+00,SvWHAT+16	: Status code
WxNODE	HC	0620+08,SvWHAT+1A	: Origin Node
WxHOST	HC	0620+0A,SvWHAT+22	: Origin Host
WxCCT	HC	0320+08,SvWHAT+2A	: CCT
WxTPC	HC	0320+08,SvWHAT+2E	: TPC

	SEG	A.CODE

: Program begins here, entry defined by START.LIB
: Output initial host-up message to the SUP

START	LIS	R2,0			: RPORT = 0
	LA	R3,HUPMSG,,		: Setup msg address
	JAL	R5,OCM,,		: Send message

: Main message input loop, all routines return here one way or another

NOMSGS	SVC	DISMIS			: Done
ISMSG	JAL	R4,LOOK,,		: Look for a message (FRING)
	  J	NOMSGS			: No messages

: R1 condition codes setup. for message port (by FRING)
: Messages with RPORT=0 can be tested at this point

	JE	ZFLUSH			: RPORT 0 Msg!  Flush it

: Save RPORT and message TYPE for this message for later

	STH	R1,MsPORT		: Save current port
	STH	R2,MsTYPE		: Save current type
	LR	R2,R2			: Set condition codes for type
	JE	ISNDL			: Got a needle?

: Decide whether this is control or data

	CHI	R2,09E			: Control or data message?
	JL	MsDATA			: Data! -- Process the data
	CHI	R2,09F			: ZAP? (received on port)
	JE	MsIZAP			: Yes, incoming zap!

	CHI	R2,0AA			: GB (Green ball)?
	JE	MsIGB			: Yes, be polite, reflect GB
	CHI	R2,0AB			: RB (Red ball)?
	JE	MsIRB			: Yes, be polite, reflect RB
	CHI	R2,0AC			: YB (Yellow ball)?
	JE	MsIYB			: Yes, be polite, reflect OB

	CHI	R2,0A6			: Enter deferred echo mode
	JE	MsEDEF			: Yes, Set flags and return
	CHI	R2,0A7			: Leave deferred echo mode
	JE	MsLDEF			: Yes, Set flags and return

	CHI	R2,0B1			: Terminal parameters?
	JE	MsITPM			: Yes, take care of it


: Come here to flush non-rport=0 messages
: before returning to main loop

MFLUSH	LH	R2,MsTYPE		: Remember type
	LB	R0,LENGTH,R2,		: Get RPORT=N index byte
XFLUSH	JAL	R4,FLUSH,,		: Flush message
	J	ISMSG			: Any more messages

: Come here to flush rport=0 messages
: before returning to main loop

ZFLUSH	LH	R6,LP0LST,,		: Get RPORT=0 index table
	LB	R0,NRATE,R6,R2		: NRATE+TABLE(R2) Get message size
	J	XFLUSH			: Flush above

: Control message code:  Handle Circuit-ZAP


MsIZAP	LH	R2,MsPORT		: Get current port
	TBT	R2,LOGDET		: Check DET bit for this port
	  JN	MsEZAP			: Yes, expected zap, don't reflect
	LA	R3,MxZAP		: Get zap message
	JAL	R5,OCM,,		: Send it <Whack>
	LIS	R2,0			: Tell the supervisor
	LA	R3,HSTOKP		: host no longer
	JAL	R5,OCM,,		: out of ports
	LH	R0,LOGINS		: Decrement login count
	SIS	R0,1			:  by 1
	STH	R0,LOGINS		: to keep us honest
MsEZAP	LH	R2,MsPORT		: Get current port
	RBT	R2,LOGGED		: Clear logged-in bit
	RBT	R2,LOGDET		: Clear DET bit for this port
	J	MFLUSH			: Return through flushing code

: Terminal parameter message - for now, just mark we were waiting for it
:  Later, we need to check each type and keep track of parameter values

MsITPM	LH	R2,MsPORT		: Get current port
	RBT	R2,WAITB1		: Clear bit waiting
	J	MFLUSH			: and flush message

: Control message code:  Handle Yellow-Ball
: by reflecting an Orange-Ball on the same port

MsIYB	LA	R3,MxOB			: Get OB message
MsPMSG	LH	R2,MsPORT		: Get current port
	JAL	R5,OCM,,		: Send it <Whack>
	J	MFLUSH			: And flush message

MsIGB	LA	R3,MxGB			: Get GB message
	J	MsPMSG			: Send message

MsIRB	LA	R3,MxRB			: Get RB message
	J	MsPMSG			: And flush message

MsEDEF	LH	R2,MsPORT		: Get current port
	RBT	R2,ECHOME		: Clear echo flag (HOST ECHO)
	J	MFLUSH			: and clear input message

MsLDEF	LH	R2,MsPORT		: Get current port
	SBT	R2,ECHOME		: Set echo flag (CONSAT ECHO)
	J	MFLUSH			: and clear input message

: We have a needle, print startup (welcome) message to port and fall through

ISNDL	LH	R2,MsPORT		: Get RPORT
	RBT	R2,LOGDET		: Clear DET sent bit
	RBT	R2,NEEDCR		:  and CR needed bit
	JAL	R4,GETH,,		: Get 00+Messagelength
	LR	R6,R0			: Remember length (bytes)
	JAL	R4,GETW,,		: Grab invoice Number (to throw away)
	JAL	R4,GETW,,		: Grab Dest-host, Orig Node
	STH	R0,NFROM,R2,		: Store NODE of origin
	JAL	R4,GETH,,		: Grab HOST of origin
	STH	R0,HFROM,R2,		: Store it
	JAL	R4,GETH,,		: Get port of origin (logical)
	JAL	R4,GETW,,		: Get xxxx, xx Throughput class
	STB	R0,TCLASS,R2,		: Store it (byte)
	JAL	R4,GETH,,		: IIX + Destination node
	JAL	R4,GETCH,,		: Get Circuit Control Type
	STB	R0,CCTYPE,R2,		: Store for later
	JAL	R4,GETCH,,		: Finish word
	SHI	R6,5*4			: Decrement count by 5 full words
	LR	R0,R6			: Get remaining byte count
	JAL	R4,FLUSH,,		: Flush needle

: Instead of prompting for Userid, later just use the username from the needle

	LH	R2,MsPORT		: Get current port
	RBT	R2,ECHOME		: Clear who's echoing
	SBT	R2,WAITB1		: Waiting for a B1 return
	LA	R3,M1TP00		: Send my initial B1 message
	JAL	R5,OCM,,		: to the other end

	LH	R2,MsPORT		: Get current port
	LA	R3,WELMSG		: Get my greeting message
	JAL	R5,OCS,,		: Print message
	LH	R1,LOGINS		: Get login count
	AIS	R1,1			:   increment
	STH	R1,LOGINS		:   and store
	CHI	R1,HPRT0		: Reached port limit?
	  JL	WANTID			: No, keep going
	LIS	R2,0			: Tell the supervisor
	LA	R3,HSTOOP		: host out of ports
	JAL	R5,OCM,,		: so no more circuits
: Ask the user for an ID - This need not be unique

WANTID	LH	R2,MsPORT		: Get current port
	SBT	R2,NEEDID		: Set need ID bit for this port
	SBT	R2,NEEDCR		:  and wait for a CR too
	LR	R3,R2			: Copy for offset
	SIS	R3,1			: Decrement for 0 offset
	MH	R3,UIDSIZ		: Multiply by entry size
	LIS	R1,0			: Set to initial constant
	STH	R1,PORTSC,R3,		:  for user id
	JFS	ASKID			: And skip to asking message
ASKUID	TBT	R2,NEEDCR		: Need a CR before re-prompt?
	  JN	ISMSG			: Yes, don't say a thing
ASKID	SBT	R2,WAITB1		: Waiting for a B1 return
	LA	R3,M0TP00		: Send my no-echo B1 message
	JAL	R5,OCM,,		: to the other end
	LH	R2,MsPORT		: Get current port
	LA	R3,UIDMSG		: Get user id message
	JAL	R5,OCS,,		: Print request
	J	ISMSG			: Any others

: Send a prompt to the currently selected port

PRMPT	LH	R2,MsPORT		: Get RPORT
	TBT	R2,NEEDID		: Waiting for user id?
	  JN	ASKUID			: Yes, ask instead of prompt
	LA	R3,PMTMSG		: Get prompt message
	JAL	R5,OCS,,		: Print message
	J	ISMSG			: Any more messages
: Received a data message.  For this exercise, we are looking for
: a particular command, sans case, sans parity:
:
:	C = Connect to another line (echo on both)
:	H = Help, (or ?)
:	M = Message another line (send one time message)
:	Q = Quit, teardown circuit and wait for a new login
:	S = Status message about slot
:	T = Print daytime in local time zone
:	W = What lines are active and Who is on-line
:
:	any other character causes a bell
:

MsDATA	JAL	R4,GETCH,,		: Get count of characters (MsType)
	STH	R0,CHRCNT		: Copy byte count to loop register
	LH	R2,MsPORT		: Get current port number
	TBT	R2,NEEDID		: Waiting for UserId?
	  JN	MuDATA			: Yes, get user Id
MsLOOP	JAL	R4,GETCH,,		: Get data byte
	JAL	R4,CSEVEN,,		: Clip it to 7 bits (upper case)
	JAL	R13,RxECHO,,		: See if we need to echo R0
	LIS	R2,0			: Start at first entry
	LHI	R3,CmdLEN		: Get length
MsLOOK	CLB	R0,CmdTAB+CmdChr,R2,	: Is it this character?
	  JNFS	MsFIND			: No, try next
	L	R0,CmdTAB+CmdFnc,R2,	: Get function
	NI	R0,00FFFFFF		:  and remove high byte
	JR	R0			: Then dispatch ??

MsFIND	AHI	R2,CmdSiz		: Point to next entry
	SIS	R3,1			: Are we done?
	JG	MsLOOK			: No, try next command
	J	MsUNK			: No, unknown command -- beep

: All commands return to MsNEXT to complete reading input
: and to flush the remainder of the message if needed!

MsNEXT	LH	R0,CHRCNT		: Get characters left
	SIS	R0,1			: Decrement loop counter
	STH	R0,CHRCNT		: And remember the result
	JG	MsLOOP			: More characters, go get em
	JAL	R4,ELIR			: None left, fixup iring index
	J	PRMPT			: New prompt and go

: All messages reaching here contain unknown characters
: Ignore the rest of the message (later until we see a CR)

MsUNK	LH	R2,MsPORT		: Get current port
	LA	R3,MxBELL		: Get bell message
	JAL	R5,OCS,,		: Send message
	J	MsNEXT			: We are done, try for more

: All messages here are for user id characters
: Once the buffer pointer is setup, up to 12 bytes are stored
: as String Constants for use with OCS.

MuDATA	LH	R2,MsPORT		: Get current port
	LR	R8,R2			: Copy for offset
	SIS	R8,1			: Decrement for 0 offset
	MH	R8,UIDSIZ		: Multiply by entry size
	LB	R7,PORTSC,R8,		: Get user id address size
MuLOOP	JAL	R4,GETCH,,		: Get a byte
	NHI	R0,07F			: Clip it to 7 bits (upper case)
	CHI	R0,CmESC		: An ESCape
	  JE	WANTID			: Yes, reset and try again
	CHI	R0,CmCR			: A Return?
	  JE	MuTEND			: Yes, finish name
	CHI	R7,$A12			: Allow up to 12
	  JLFS	MuSAVE			: Ok, go save it
	LHI	R0,CmBELL		: Overflow, make believe a bell
	JFS	MuECHO			: and just echo it
MuSAVE	AIS	R7,1			: Increment byte (count) pointer
	STB	R0,PORTSC,R8,R7		: Store next byte
MuECHO	JAL	R13,RxECHO,,		: See if we need to echo R0
MuNEXT	LH	R0,CHRCNT		: Get characters left
	SIS	R0,1			: Decrement loop counter
	STH	R0,CHRCNT		: and remember it for later
	JG	MuLOOP			: More characters, go get em
	LH	R8,MsPORT		: Get current port
	SIS	R8,1			: Decrement for 0 offset
	MH	R8,UIDSIZ		: Multiply by entry size
	STB	R7,PORTSC,R8		: Store counter
	JAL	R4,ELIR,,		: None left, fixup iring index
	J	PRMPT			: New prompt and go


MuTEND	RBT	R2,NEEDID		: Set Id is complete
	RBT	R2,NEEDCR		:  and CR was seen
	SBT	R2,LOGGED		: We are logged completely in
	SIS	R2,1			: Decrement for 0 offset
	MH	R2,UIDSIZ		: Multiply by entry size
	STB	R7,PORTSC,R2		: Store counter
: Send B10001 message here
	J	MsNEXT			: All done, any more are commands

: HELP - Print a help message about the slot
:

	SEG	A.DATA
CxHPTR	HS	1			: Place for pointer offset
CxHLEN	HS	1			: Length of command list
	SEG	A.CODE

CxHELP	LH	R2,MsPORT		: Get current port
	LA	R3,MxHELP		: and message to print
	JAL	R5,OCS,,		: Then print it
	LIS	R1,0			: Start at first entry
	STH	R1,CxHPTR		: Remember the pointer offset
	LHI	R1,CmdLEN		: Get length
CxHLOP	STH	R1,CxHLEN		: Remember it
	LH	R1,CxHPTR		: Get offset
	LB	R0,CmdTAB+CmdChr,R1,	: Get character
	LH	R3,CmdTAB+CmdHlp,R1,	: Get help message
	STB	R0,0+2,R3,		: Store character in text
	AHI	R1,CmdSIZ		: Point to next entry
	STH	R1,CxHPTR		: Remember the pointer offset
	LH	R2,MsPORT		: Get the port
	JAL	R5,OCS,,		: and print the line
	JAL	R13,TCRLF		: Finish with a CRLF
	LH	R1,CxHLEN		: Get commands left	
	SIS	R1,1			: Any left?
	JG	CxHLOP			: Yes, print them
	JAL	R13,TCRLF		: Finish with a CRLF
	J	MsNEXT			: Return to command loop

: QUIT - Mark we are in progress (check if already in progress)

CxQUIT	LH	R2,MsPORT		: Get current port
	TBT	R2,LOGDET		: Check in array (in progress)?
	  JN	CxALQ			: Quit in progress
	LA	R3,MxBYE		: Get Goodbye message
	JAL	R5,OCS,,		: Send it
	LH	R2,MsPORT		: Get port again
	LA	R3,MxDET		: Get detach message
	SBT	R2,LOGDET		: Set in detach array
	RBT	R2,LOGGED		: Clear logged-in bit
	JAL	R5,OCM,,		: Send detach
	LIS	R2,0			: Tell the supervisor
	LA	R3,HSTOKP		: host no longer
	JAL	R5,OCM,,		: out of ports
	J	MsNEXT			: Get next character

: %OOPS - Already waiting for OB from this port

CxALQ	LA	R3,MxALQ		: Get message text
	JAL	R5,OCS,,		: Print it
	J	MsNEXT



: TIME - Obtain current daytime and print t for the user

CxTIME	L	R3,GMT,,		: Get current daytime
	LIS	R4,8			: for HH:MM:SS format
	LA	R5,SvTIME+1,,		: Indicate storage
	JAL	R6,TMSTMP,,		: Get date string

	LH	R2,MsPORT		: Get current port
	LA	R3,MxTIME,,		: Get time message
	JAL	R5,OCS,,		: Print it
	LH	R2,MsPORT		: Get current port
	LA	R3,SvTIME,,		: Get actual time string
	JAL	R5,OCS,,		: Print it
	J	MsNEXT			: Return to loop


: WHAT - Tell what lines are available

CxWHAT	JAL	R13,TCRLF		: Free CRLF
	LH	R2,MsPORT		: My port
	LA	R3,MxWHAT		: Print header message
	JAL	R5,OCS,,		: for this command
	LIS	R9,1			: Start with first port
CxWLOP	TBT	R9,LOGGED		: Is it logged?
	  JE	CxWNXT			: No, try next
	LH	R0,WxMINE		: Get -myself- mask
	LH	R1,WxMINE+2		:  and position
	LHI	R2,20			:  and value  (default = SP)
	CH	R9,MsPORT		: Is it my port?
	  JN	CxWNME			: No, not me
	LHI	R2,2A			: Yes, Me!  Indicate with "*"
CxWNME	STB	R2,0,R1			: Store appropriate value
	LH	R0,WxPORT		: Get port mask
	LH	R1,WxPORT+2		:  and position
	LR	R2,R9			:  and value
	JAL	R6,OUTNUM,,		: Convert it for printing
	LR	R8,R9			: Copy for offset
	SIS	R8,1			: Decrement for 0 offset
	MH	R8,UIDSIZ		: Multiply by entry size
	LH	R0,WxUSER		: Get user mask
	LH	R1,WxUSER+2		:  and position
	LA	R2,PORTSC,R8,		:  and address of SC
	JAL	R6,MOUTSC,,	
	LH	R0,WxNODE		: Get NODE mask
	LH	R1,WxNODE+2		:  and position
	LR	R2,R9			:  and value
	LH	R2,NFROM,R2,		:  from table
	JAL	R6,OUTNUM,,		: Convert it for printing
	LH	R0,WxHOST		: Get HOST mask
	LH	R1,WxHOST+2		:  and position
	LR	R2,R9			:  and value
	LH	R2,HFROM,R2,		:  from table
	JAL	R6,OUTNUM,,		: Convert it for printing
	LH	R0,WxCCT		: Get CCT mask
	LH	R1,WxCCT+2		:  and position
	LR	R2,R9			:  and value
	LH	R2,CCTYPE,R2,		:  from table
	JAL	R6,OUTNUM,,		: Convert it for printing
	LH	R0,WxTPC		: Get TPC mask
	LH	R1,WxTPC+2		:  and position
	LR	R2,R9			:  and value
	LH	R2,TCLASS,R2,		:  from table
	JAL	R6,OUTNUM,,		: Convert it for printing
	LH	R2,MsPORT		: Get my port
	LA	R3,SvWHAT		: and the right message
	JAL	R5,OCS,,		: then print it
CxWNXT	AIS	R9,1			: Try next port
	CHI	R9,MAXPRT		: Reached the limit?
	JLE	CxWLOP			: No, check it
	JAL	R13,TCRLF		: Print extra crlf
	J	M			: And return


: Utility routines: CSEVEN, TCRLF, MOUTSC

CSEVEN	NHI	R0,7F			: Only look at 7 bits
	CHI	R0,60			: Check for lower-case range
	JL	C7CASE			: No, skip subtraction
	  SHI	R0,20			: Convert to upper case
C7CASE	JR	R4			: Return


TCRLF	LH	R2,MsPORT		: Get current port
	LA	R3,MxCRLF		: Get message
	JAL	R5,OCS,,		: Print it
	JR	R13			: Return


MOUTSC	NHI	R0,3FFF			: Check field width
	CHI	R0,100
	JLR	R6			: Nothing to do?
	LR	R3,R0			: Extract padd character
	NHI	R3,7F			: (can be anything) SP = 20
	  JNFS	MOUTS0			: But must NOT be null
	LHI	R3,20			: If so, make it a space
MOUTS0	LB	R4,0,R2			: Get bytes in string
	SRL	R0,08			: Normalize field width
	CR	R0,R4			: Do we have too many?
	  JLEFS	MOUTS1			: No, no change
	LR	R4,R0			: Use the field width
MOUTS1	AIS	R2,1			: Adjust character pointer
	LB	R5,0,R2			: Get byte
	  JNFS	MOUTS2			: Non-null so keep going
	LR	R5,R3			: Oops! use filler instead
MOUTS2	STB	R5,0,R1			: Store byte
	AIS	R1,1			: Adjust pointers (output)
	SIS	R0,1			:  and counters (field)
	SIS	R4,1			:  to suit (input)
	JG	MOUTS1			: More? Yes, transfer them
	CR	R0,R4			: Anything left to fill?
	JLER	R6			: No, all done -- Return
MOUTS3	STB	R3,0,R1			: Add fill character
	AIS	R1,1			: Adjust output pointer
	SIS	R0,1			: Loop till all
	JG	MOUTS3			:  positions filled
	JR	R6			: All done, Return

: Echo character routine

RxECHO	STB	R0,MsCHAR		: Save byte for return
	LH	R0,MsPORT		: Get current port number
	TBT	R0,ECHOME		: See if we need to echo
	  JN	RxENOD			: No, skip this rot
	LB	R0,MsCHAR		: Get byte back
	LIS	R2,1			: One data character
	CHI	R0,CmSPAC		: Is it printable
	  JGEFS	RxOCH2			: Yes, just print one
	LB	R2,CCCTAB,R0		: No, get message length from table
RxOCH2	STB	R2,MsMSGC
	LR	R0,R2			: Copy for room needed
	LH	R1,MsPORT		: Get my port
	JAL	R4,SLOR			: Setup record
	LB	R0,MsCHAR		: Retrieve original byte
	CHI	R0,CmSPAC		: Printable?
	  JGE	RxOCHx			: Yes, do it
	CHI	R0,CmESC		: Escape?
	  JNFS	RxOCH4			: No, skip this
	LHI	R0,24			: Yes, change character to "$"
	J	RxOCHx			: Output character and return
RxOCH4	CHI	R0,CmCR			: Check CR
	  JGFS	RxOCH8			: No, Too high
	  JLFS	RxOCH6			: No, See if formatting char
	JAL	R4,PUTCH,,		: Yes, output the CR
	LHI	R0,CmLF			: Load a LF
RxOCH6	CHI	R0,CmBELL		: Check bell	
	  JGEFS	RxOCHx			: Yes!  BEL,BS,HT,LF,VT,FF to print
RxOCH8	LHI	R0,5E			: Get a "^"
	JAL	R4,PUTCH,,		: And put it away
	LB	R0,MsCHAR		: Get byte back
	AHI	R0,40			: Make it printable
RxOCHx	JAL	R4,PUTCH,,		: Put it away
	JAL	R4,ELOR,,		: Finish up
RxENOD	LB	R0,MsCHAR		: Get the original character
	JR	R13			: Return

 0 q½