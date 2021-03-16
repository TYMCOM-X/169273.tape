	SUBTTL	FMAIN - JS2033.F03 - This is the foreground for Joe's slot
	
	GL	FSTART,FCHECK		: Entry points (called from background)
	GL	SCOUNT,FCOUNT		: Global variables (defined here)
	GL	LUASYN,LUSYNC,LUSIO	: Defined in slot's TYM file

	SEG	A.CODE
	MO	.,FMAIN		: Start of module FMAIN


: Start up the foreground process

FSTART	LIS	R13,0		: Reset counter
	ST	R13,FCOUNT
	JAL	R13,INIASY	: Start up async
	JAL	R13,INISYN	: Start up sync
	JAL	R13,INISIO	: Start up SIO
	SVC	DISMIS,0	: Fall into FMAIN

: Main loop for foreground process

FMAIN	LIS	R0,0		: Main foreground loop starts here
	STH	R0,BUSY		: Clear flag (nonzero means don't dismiss)
	JAL	R13,DOASYI	: Handle async input
	JAL	R13,DOASYO	: Handle async output
	JAL	R13,DOSYNI	: Handle sync input
	JAL	R13,DOSYNO	: Handle sync output
	JAL	R13,DOSIOI	: Handle SIO input
	JAL	R13,DOSIOO	: Handle SIO output
	LIS	R0,1
	AM	R0,FCOUNT	: Count times thru here
	LH	R0,BUSY		: Did any routine indicate it did some I/O?
	JN	FMAIN		: Yes, loop back immediately
	SVC	DISMIS,0	: No, don't be a foreground hog
	J	FMAIN

: Background routine.  Called from BMAIN whenever background runs.

FCHECK	JAL	R12,CHKASY	: Check for async work
	JAL	R12,CHKSYN	: Check for sync work
	JAL	R12,CHKSIO	: Check for SIO work
	JR	R13		: End of FCHECK

	SEG	A.DATA
FCOUNT	WC	0		: Foreground execution count
BUSY	HC	0		: Nonzero if more to do in foreground loop
	SEG	A.CODE
	SUBTTL	SIO routines
:-----------------------------------------------------------------------
:  Start of SIO LAB assignment.  10-Nov-88
:  Send X.GK handshake messages, then get back the secret message.
:-----------------------------------------------------------------------

GKHEAD	EQ	0AAAA		: X.GK records start with this header
GKHERE	EQ	0B1B1		: X.GK "I'm here and want to talk"
GKHEAR	EQ	0C2C2		: X.GK "I hear you"
GKSEND	EQ	0D3D3		: X.GK "Send me a line"
GKLINE	EQ	0E4E4		: X.GK "Here it is", length in next byte
GKSIZE	EQ	100		: Size of each input record (256 bytes)

	SEG	A.DATA		: SIO data
SIOBEG	BND	100		: Start on a page boundary
SIOSTS	HS	7		: 6 bytes SIO, 4 bytes status, 4 bytes time

	BND	10
SIORES	HC 3,0018 : WR0, channel reset
	HC 3,1420 : reset ext st, WR4, 1xClock + SDLC + 8bit sync + no parity
	HC 3,1100 : reset ext st, WR1, disable all interrupts
	HC 3,03D8 : WR3, 8bit Rx, RxCRC enable, (Rx disabled)
	HC 3,05EB : WR5, 8bit Tx, TxCRC enable, Tx enable, DTR, RTS
	HC 3,877E : reset TxCRC, WR7, set sync byte to 7E (SDLC)
	HC 3,13D9 : reset ext int, WR3, Rx enable, enter hunt mode
	HC 0,0    : End of SIO programming

	BND	10
SIOSIN	HC	3,SIOREC*8+1	: Number of halfwords in buffer
	HC	1,SIOBUF/10	: Address of SIO buffer
	HC	0,0

	BND	10
SIOCMD	WS	10		: Room for building output command list

	BND	10
TXHERE	HC	4		: 4 bytes in this output record
	HC	GKHEAD,GKHERE	: AAAA,B1B1
				: 5 halfwords between here and next BND 10
FRGKMT	HC	0		: Last record type from X.GK
FROMGK	WC	0		: Pointer to record received from X.GK

	BND	10
TXSEND	HC	4		: 4 bytes in this output record
	HC	GKHEAD,GKSEND	: AAAA,D3D3
				: 5 halfwords between here and next BND 10
TOGKMT	HC	0		: Last record type sent to X.GK	

SIOREC	EQ	1+GKSIZE/2	: Halfwords in a record (including count)
	BND	10		: Must start on a quadword boundary
SIOBUF	HS	SIOREC*8+1	: Room for 8 records with counts
SIOPTR	HC	0		: Pointer to where we left off

SIOEND	BND	10		: End of SIO data
	SEG	A.CODE

: Initialize and start SIO

INISIO	LIS	R1,LUSIO	: Logical unit number
	LA	R2,SIOBEG	: Beginning of SIO window
	LA	R3,SIOEND	: End of SIO window
	LA	R4,SIOSTS	: Where to store status
	SVC	IO,090+R1	: Connect SIO motherboard
	  JAL	R10,CRASH,,	: Should not fail

: Reset the SIO chip and reprogram it

ISIO2	LIS	R1,LUSIO
	LA	R2,SIORES	: Address of command list
	SVC	IO,0A0+R1	: Force SIO output
	  J	ISIO2X		: It might be busy

: Tell SIO where the input buffer is

ISIO3	LIS	R1,LUSIO
	LA	R2,SIOSIN	: Address of command list
	SVC	IO,0B0+R1	: Connect SIO input
	  J	ISIO3X		: It might be busy

	LIS	R1,0
	ST	R1,SIOCMD	: Mark command list as being available
	ST	R1,SIOPTR	: Reset pointer into SIOBUF
	JR	R13		: SIO is ready

ISIO2X	SVC	DISMIS,0	: Dismiss in case it was busy
	CI	R1,0003000+LUSIO
	JE	ISIO2		: Try again if motherboard was busy
	JAL	R10,CRASH	: Die if any other error

ISIO3X	SVC	DISMIS,0	: Dismiss in case it was busy
	CI	R1,0003000+LUSIO
	JE	ISIO3		: Try again if motherboard was busy
	JAL	R10,CRASH	: Die if any other error


DOSIOI	JR	R13		: No SIO input yet
DOSIOO	JR	R13		: No SIO output yet
CHKSIO	JR	R12		: No SIO background

: Routine to get next GK message.  Returns pointer in R3.  Link on R4
	GL	GETGKM
GETGKM	LH	R1,TOGKMT	: Look at last GK message we sent
	JN	GETGK1		: Jump if not first time
	LA	R1,TXHERE	: B1B1
	JAL	R3,SENDGK	: Send this message out
	JR	R4		: Nonskip return means come back later

GETGK1	CHI	R1,GKHERE	: Did we send "I am here"
	JN	GETGK2		: Continue if not
	LH	R1,FRGKMT	: Look at last from-GK message type
	CHI	R1,GKHEAR	: Did GK say "I hear you"? (C2C2)
	JNR	R4		: Nonskip return until it arrives
	LA	R1,TXSEND	: D3D3
	JAL	R3,SENDGK	: Send this message out
	JR	R4		: Nonskip return to come back later

GETGK2	CHI	R1,GKSEND	: Did we ask for a line?
	JN	GETGK3		: Error if not
	LH	R1,FRGKMT	: Look at last from-GK message type
	CHI	R1,GKLINE	: Is it a line?
	JNR	R4		: Nonskip return until it arrives
	LA	R1,TXSEND	: Ask for the next line now
	JAL	R3,SENDGK
	L	R3,FROMGK	: Point to the message (in SC format)
	J	4,R4		: Skip return indicates message in R3

GETGK3	LA	R3,GETGK4	: Return error message
	J	4,R4
GETGK4	SC	/== Error in X.GK handshake =="8D"8A/

: Routine to send an X.GK message.  R1 has addr of buffer, Link on R3.

SENDGK	LH	R0,4,R1		: Get message type being sent
	STH	R0,TOGKMT	: Remember this to-GK message type
	SRLS	R1,4		: Divide by 16 (get quadword address)
	AI	R1,00010000	: Make it an output command
	ST	R1,SIOCMD+0	: Point to output buffer
	LIS	R0,0
	ST	R0,SIOCMD+1	: End of command list
	STH	R0,FRGKMT	: Zero this so we can see it come in
SENDG0	LIS	R1,LUSIO
	LA	R2,SIOCMD	: Address of command list
	SVC	IO,0A0+R1	: Force SIO output
	  J	SENDG1		: Check for busy
	JR	R3		: OK

SENDG1	SVC	DISMIS,0	: Dismiss in case of busy
	CI	R1,00030000+LUSIO
	JE	SENDG0		: Try again if busy
	JAL	R10,CRASH	: Die otherwise
	  BC	0,0,4*R3,80	: Crash type 80, R3 has the link address
	SUBTTL	Sync routines
:-----------------------------------------------------------------------
:  Start of SYNC LAB assignment.  7-Nov-88
:  Send a constant stream of data out the sync port.  A loop-back
:  connector will cause the data to be seen in the input buffer.
:  The data being sent is 16 bits from the count of times thru here.
:-----------------------------------------------------------------------

	SEG	A.DATA		: SYNC buffers

SYNIPT	HC	0		: SYNIBF empty pointer
SYNISZ	EQ	200		: 512 input bytes (does not include index)
SYNIBF	HS	1+(SYNISZ/2)	: Sync input buffer, halfword aligned
	HC	5555		: (to verify buffer does not overflow)

SYNOSZ	EQ	200		: 512 bytes for each output buffer
SYNBF1	HS	SYNOSZ/2	: 1st output buffer
SYNBF2	HS	SYNOSZ/2	: 2nd output buffer
SYNBF3	HS	SYNOSZ/2	: 3rd output buffer
SYNBF4	HS	SYNOSZ/2	: 4th output buffer

CMD	EQ	0		: Offsets into rotor list below
CNT	EQ	2
ADR	EQ	4
	BND	4		: This must start on a word boundary
RTLST1	HC	1		: Output command
	HC	SYNOSZ		: Byte count
	WC	SYNBF1		: Address of buffer
RTLST2	HC	1		: Output command
	HC	SYNOSZ		: Byte count
	WC	SYNBF2		: Address of buffer
RTLST3	HC	2		: Jump command
	HC	0
	WC	RTLST1		: Where to jump to

: Addresses of buffers to be put into the rotor list.
: Foreground: put old buffer in EMPTY, grab new one from FULL.
: Background: grab buffer from EMPTY, put in FILL and zero FILLPT.
: Background: when FILLPT is full, put buffer in FULL.

SCOUNT	WC	0		: Counter for sending sync data
FULL	WC	0		: Next buffer to send
FILL	WC	SYNBF3		: Buffer being filled
EMPTY	WC	SYNBF4		: Next buffer to fill
FILLPT	HC	0		: Pointer into FILL buffer

	SEG	A.CODE
: Start up SYNC line

INISYN	LIS	R1,LUSYNC	: Logical unit number
	LA	R2,SYNIBF,,	: Input buffer
	LHI	R3,SYNISZ	: Size (including index)
	SVC	IO,CSYNI.*10+R1	: Connect sync input
	  J	CRASH		: Should never fail
	SVC	DISMIS,0
	LI	R0,1*10000+SYNOSZ : Make sure the rotor list
	ST	R0,RTLST1+CMD,,	:  is properly set up,
	ST	R0,RTLST2+CMD,,	:  incase slot is restarted
	LIS	R0,2
	STH	R0,RTLST3+CMD,,
	LI	R0,SYNBF1	: Put buffer address in the right places
	ST	R0,RTLST1+ADR,,
	LI	R0,SYNBF2
	ST	R0,RTLST2+ADR,,
	LI	R0,SYNBF3
	ST	R0,FILL,,
	LI	R0,SYNBF4
	ST	R0,EMPTY,,
	LIS	R0,0
	ST	R0,FULL		: No buffer is full yet
	ST	R0,FILLPT	: Start at beginning of FILL buffer
	ST	R0,SCOUNT	: Clear background count
	
	LIS	R1,LUSYNC	: Logical unit number
	LA	R2,RTLST1,,	: Address of rotor list
	SVC	IO,CSYNO.*10+R1	: Connect sync output
	  J	CRASH		: Should never fail
	JR	R13		: End of INISYN

: Sync input routine

DOSYNI	LH	R0,SYNIBF	: Get the index
	STH	R0,SYNIPT	: Pretend that we read all the data
	JR	R13		: End of DOSYNI
: Resume sync output.  Link on R13

DOSYNO	LH	R0,RTLST1+CMD,,	: Check if we are caught up
	AH	R0,RTLST2+CMD,,
	AH	R0,RTLST3+CMD,,
	CHI	R0,1+1+2	: All values correct?
	JER	R13		: Yes, all caught up, go dismiss

: At least one of the commands in the rotor list needs to be reset

	LIS	R1,2
	STH	R1,BUSY		: Set the "more to do" flag
	STH	R1,RTLST3,,	: Fix the transfer command
	LA	R1,RTLST1,,	: Check the first command
	JAL	R2,SWAPSN	: Swap pointers
	LA	R1,RTLST2,,	: Check the second command
	JAL	R2,SWAPSN	: Swap pointers
	J	DOSYNO		: Double check all 3 again before dismissing

: Here to swap pointers between an empty pointer and a full one, link on R2.

SWAPSN	LH	R0,CMD,R1	: Check the command HW
	JNR	R2		: Proceed only if command is zero
	L	R0,FULL		: Is there a full buffer read to go?
	JE	SWAPNO		: Zero means no new buffer
	L	R3,ADR,R1	: Get old buffer address
	ST	R3,EMPTY	: This buffer is now empty
	ST	R0,ADR,R1	: Store new buffer address
	LIS	R0,0		: Zero means that
	ST	R0,FULL		:  the full buffer has been taken
SWAPOK	LIS	R0,1		: Reset the command code
	STH	R0,CMD,R1
	JR	R2		: Swap is done

SWAPNO	LHI	R0,0FFFF	: No new buffer, reuse the old one
	LHI	R3,SYNOSZ	:  after writing FF in all bytes
	L	R4,ADR,R1	:  of the buffer
SWAPFL	SIS	R3,2
	JL	SWAPOK		: Reset command code when done
	STH	R0,0,R4,R3	: Store 2 bytes of FF
	JBS	SWAPFL
: Enter here to do processing the in background on behalf of the foreground.
: Called from the background's main loop, link on R12.

CHKSYN	JAL	R3,CSROOM	: Check if room to do sync output
	  J	0,R12		: Can't do anything at this time

	LIS	R0,1		: Count number of times thru here
	A	R0,SCOUNT
	ST	R0,SCOUNT
	JAL	R4,STOR0	: Store this number in the output buffer
	J	CHKSYN		:*HACK* CONSTANTLY FILL UP OUTPUT BUFFERS

: Check if room to do sync output.  Link on R3, skip if OK.
: There are 4 buffers and 5 positions, RTLST1, RTLST2, FULL, FILL, EMPTY.
: Exactly 1 of FULL, FILL, and EMPTY will always be zero.

CSROOM	LH	R0,FILLPT	: Get current fill pointer
	CHI	R0,SYNOSZ	: Check if current buffer is full
	JL	4,R3		: Not full, ok to proceed (skip return)
	L	R0,FULL		: Current buffer is full, check previous buf
	JNR	R3		: Nonskip return if it is also full
	L	R1,FILL		: Take the just filled buffer
	ST	R1,FULL		:  and put it in the FULL position
	L	R1,EMPTY	: Take the EMPTY buffer
	ST	R1,FILL		:  and make it be the one to be filled
	JE	CRASH		: EMTPY should not be zero when FULL is zero
	STH	R0,FILLPT	: R0 still has a zero in it
	ST	R0,EMPTY	: This buffer is taken
	J	4,R3		: OK to proceed (skip return)

: Put 2 bytes of R0 into output buffer.  Link on R4

STOR0	L	R1,FILL		: Point to buffer to be filled
	LH	R2,FILLPT	: Get the fill pointer
	STH	R0,0,R1,R2	: Store in buffer that R1 points to
	AIS	R2,2		: 2 bytes per halfword
	STH	R2,FILLPT
	JAL	R3,CSROOM	: Send buffer now if it is full
	  NOP			: Don't care if no room
	JR	R4		: Return from STOR0
	SUBTTL	Async routines
:-----------------------------------------------------------------------
:  Start of ASYNC LAB assignment.  9-Nov-88
:  Set port 0 to 1200 baud, enable it, and output " OK ".  (Note: there
:  is a delay of about 5 seconds between startup and when ISIS sets DTR.)
:  Ignore all input from that port until a break is received.  Once the
:  break signal has ended, output a welcome message and then enable echo.
:  Be sure to echo CR as CR+LF.
:-----------------------------------------------------------------------

	GL	IZASYN,AGCI,AWCI,AWCP	: Routines in ARING.LIB
	GL	NGRP,RBS,NAPORT		: Equates from ARING.LIB
	GL	DSR,DTR,CP,TAM,TOPORT,FPORT,ARING	: Variables from ARING
LUASYN	EQ	0		: Logical unit for 1st async group

	SEG	A.DATA
ASECHO	HS	NGRP		: Async echo needed when bit is set
APORT	HC	0		: Port number that needs output
ACOUNT	HC	0		: Count of bytes left to be output
APOINT	WC	0		: Address of bytes left to be output
	SEG	A.CODE

: Start of program

START	JAL	R13,INIASY	: Initialize and start async
LOOP	SVC	DISMIS,0	: Give other slots a chance to run
	JAL	R13,DOASYI	: Do async input
	JAL	R13,DOASYO	: Do async output
	J	LOOP

: Messages to be sent

ASINIT	SC /"00"95"00"B5"00"E0 OK / :IBAUD=1200, OBAUD=1200, connect
WELCOM	SC /"8D"8AWelcome to Joe's ASYNC test slot.  This message is more than 14 bytes long."8D"8A/

: Initialize ASYNC and start it.  Link on R13.

INIASY	JAL	R0,IZASYN	: Init (zero) async variables
	LIS	R1,0
	STH	R1,ASECHO	: Mark port as not needing echo yet

: Set up registers for the "connect async" SVC

	LIS	R0,LUASYN	: Group #
	LA	R1,ARING,,	: Array of rings
	LA	R2,CP,,		: Bits of Carrier Present
	LA	R3,DSR,,	: Bits of Data Set Ready (modem on-line)
	LA	R4,DTR,,	: Bits of Data Term Ready (our ports)
	LA	R5,TOPORT,,	: Bits of ports with output ready (set by us)
	LA	R6,FPORT,,	: Bits of ports with input ready (set by ISIS)
	LA	R7,TAM,,	: Bits of transmit activity
	SVC	IO,030+R0	: Connect async (OP=3)
	  JAL	R10,CRASH	: Die if no async defined in node's TYM file

: Send an initialization message to set baud rates and enable I/O

	LIS	R5,0		: Set port number
	STH	R5,APORT
	RBT	R5,DTR,,	: Clear DTR bit (which turns on the signal)
	LB	R1,ASINIT	: Set byte count
	STH	R1,ACOUNT
	LA	R2,ASINIT+1	: Set message address
	ST	R2,APOINT
	JR	R13		: End of INIASY

:--------------------------------------------------------------------------

: Read input from the port.  Link on R13.

DOASYI	LH	R5,APORT	: Check only port 0 in this assignment
	JAL	R4,AGCI,,	: Get next byte (if any)
	  J	0,R13		: Nothing there
	CHI	R0,0		: Is it an escaped message?
	JE	ESCIN		: Process escape on input
	TBT	R5,ASECHO	: Are we supposed to echo on this port?
	JER	R13		: Do nothing if BREAK has not been seen

: Echo this input character.  Echo CR as CR+LF.  Since characters come in
: relatively slowly, we do not expect the echo to fill the output ring.

WAIT4	TBT	R5,TAM,,	: Is there room in the output ring?
	JEBS	WAIT4		: No, spin here until ISIS sets the bit
: The TAM bit gets set when there is room for 4 or more bytes in output ring
	CHI	R0,0		: Test for null
	JE	ECHNUL		: Because it has to be handled special
	JAL	R4,AWCI,,	: Echo character by writing it to the oring
	NHI	R0,7F
	CHI	R0,0D		: Check for the carriage return character
	JNR	R13		: Done if not
	LHI	R0,8A		: Get a linefeed
	JAL	R4,AWCI,,	: Echo this as well
	JR	R13		: Done 

ECHNUL	JAL	R4,AWCP,,	: Write NULL as an escaped character
	JR	R13		: Done

: Here to handle an escaped input character

ESCIN	JAL	R4,AGCI,,	: Get char after the escape
	  J	ESCIN		: Spin here until it arrives
	CHI	R0,91		: End of break?
	JNR	R13		: Ignore all other escaped characters
	CBT	R5,ASECHO	: Toggle echo-enable at break end
:Set up to output a welcome messsage
	LB	R1,WELCOM	: Get byte count
	LA	R2,WELCOM+1	: Address of message
	STH	R1,ACOUNT	: Store this for DOASYO
	ST	R2,APOINT	: (which will do it a few bytes at a time)
	JR	R13		: End of DOASYI

:--------------------------------------------------------------------------

: Do async output

DOASYO	LH	R5,APORT	: Do only port 0 in this assignment
	TBT	R5,TAM,,	: Is there room do to anything?
	JER	R13		: Wait until room for at least 4 bytes
	LH	R1,ACOUNT	: Check if anything left to output
	JER	R13		: Nothing waiting if count is zero
	L	R2,APOINT	: Address where we left off
	LB	R0,0,R2		: Get the next byte
	SIS	R1,1		: Decrement count
	STH	R1,ACOUNT
	AIS	R2,1		: Increment pointer
	ST	R2,APOINT
	JAL	R4,AWCI,,	: Write normal character
	J	DOASYO		: Check if OK to do more

: Check for async work.  Called from background

CHKASY	JR	R12		: Nothing to do

: End of ASYNC lab problem.

	EM			: End of module FMAIN
   