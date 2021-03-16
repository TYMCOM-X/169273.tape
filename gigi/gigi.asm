;.TITLE	GIGI - Interfaces a GIGI (VK100) to CPM 2.2
;.SBTTL	Definitions,  4-Nov-84

;Written by:	Joe Smith	[CSM Computing Center (303)273-3448]
;		12221 West 2nd Place #12-104
;		Lakewood, CO  80228


	ORG	100h
GIGI:	JMP	START		;Skip over constants
USEPORT:DB	UC1		;Assume the GIGI is connected to UC1:

VERSION:DB	'GIGI version 1.0, November 1984',CR,LF,'$',0
	DB	'< Copyright 1984 by Joe Smith >',CR,LF,0

	   ;1234567890123456789012345678901234567890123456789012345678901234
HLPTXT:	DB 'GIGI.COM acts as an interface between CP/M and a VK100',CR,LF,LF
	DB 'A>GIGI SHOW LOGO.PIC will enable ReGIS and show the picture file'
	DB CR,LF,'A>GIGI LOAD WOCKA.GGB will load the GIGI-BASIC program',CR,LF
	DB 'A>GIGI RUN WOCKA.GGB will load and run the program.',CR,LF,LF
	DB 'After a LOAD or a RUN, use SAVE "FILENAME" to save the program'
        DB ' currently',CR,LF,'in memory, LOAD "FILENAME" to get a new one,'
	DB 'or HOST to return to CPM.',CR,LF,LF
	DB 'Type "A>GIGI PORT" for more information.',CR,LF,0

TTY	EQU	0	;GIGI on TTY: (printer port on VT180)
CRT	EQU	1	;VT125 portion of VT185
MDM	EQU	2	;GIGI on PTP:/PTR: (communications port on VT180)
UC1	EQU	3	;GIGI on UC1: (general purpose port on VT180)

	   ;1234567890123456789012345678901234567890123456789012345678901234
PORTXT:	DB 'The GIGI program assumes that the GIGI terminal is connected to'
	DB ' UC1:, the',CR,LF,'general-purpose port on the VT180.  To change'
	DB ' this, use DDT to modify the byte',CR,LF,'at location 103.  Set it'
	DB ' to 0 to use the TTY: port, 1 for CRT:, 2 for PTP:/PTR:,',CR,LF
	DB 'or 3 for UC1:.  Use "SAVE ',PAGES+'0',' GIGI.COM" after exiting DDT.'
	DB CR,LF,0

;******************************************************************************

  ;Addresses
IOBYTE	EQU	03h	;Current setting of CON: in bits 0 and 1
BDOS	EQU	05h	;Call to Operating System
FCB	EQU	5Ch	;First formatted FCB
FCB2	EQU	6Ch	;Second formatted FCB
 DRV	EQU	00h	;Offset to drive number
 FIL	EQU	01h	;Offset to file name
 EXT	EQU	09h	;Offset to file extension (type)
DMABUF	EQU	80h	;Command tail stored here

  ;BDOS functions
CONIN	EQU	01	;Console input
CONOUT	EQU	02	;Console output
MDMIN	EQU	03	;Modem (reader) input
MDMOUT	EQU	04	;Modem (punch) output
LSTOUT	EQU	05	;List output
DCONIO	EQU	06	;Direct console I/O (no echo)
PRINTS	EQU	09	;Print string
OPENF	EQU	15	;Open file for input
READF	EQU	20	;Read sequential

  ;Characters
NUL	EQU	00h	;Null
CTRLC	EQU	03h	;Control-C
BS	EQU	08h	;Backspace
TAB	EQU	09h	;Horizontal Tab
LF	EQU	0Ah	;Linefeed
CR	EQU	0Dh	;Carriage Return
SO	EQU	0Eh	;Shift-Out (to line-drawing graphics set)
SI	EQU	0Fh	;Shift-In (to normal text set)
CTRLZ	EQU	1AH	;CONTROL-Z MARKS END OF FILE
ESC	EQU	1Bh	;ESCape character
DEL	EQU	7Fh	;Delete (RUBOUT)

;******************************************************************************

;.SBTTL	START and EXIT routines

START:	LDA	IOBYTE		;Get current setting of IOBYTE
	STA	OLDIOB		;Save for later
	MOV	C,A
	ANI	0FCh		;Clear the CON: bits
	MOV	B,A
	LDA	USEPORT		;Get the CON: bits to be used
	ORA	B		;Combine the rest of the bits
	STA	NEWIOB		;New IOBYTE
	XRA	C		;Same as previous?
	STA	DIFIOB		;Non-zero if different

	LXI	D,VERSION	;Point to sign-on message
	MVI	C,PRINTS	;String function
	CALL	BDOS

	LDA	FCB+FIL		;This byte non-blank if command tail is present
	STA	CMD1		;Don't exit at NEWCMD if blank
	CPI	' '		;Is it blank?
	JZ	START1		;Yes, output the prompt and wait for a command
	LDA	DMABUF+0	;Get byte count
	LXI	H,DMABUF+1	;Get starting address
	JMP	MAIN		;Process command

NEWCMD:	LDA	CMD1		;Was there a command tail?
	CPI	' '
	JNZ	EXIT		;Yes, exit after one command

START1:	LXI	H,PROMPT	;'Type "?" for help'
	CALL	OUTSTR
	CALL	ECHO		;Set flag to echo characters

RESTART:LXI	D,LINBUF	;Point to line buffer
	MVI	A,128		;Size of buffer
	STAX	D
	CALL	RDLINE		;Read a line of input
	JMP	MAIN		;Process it now



;Reset everything and exit back to CPM

EXIT:	CALL	SETCON		;Reset IOBYTE
	RET			;Return to the CCP

ABORT:	CALL	SETCON		;Reset IOBYTE
	JMP	0000		;Do a warm boot on Control-C

;*****************************************************************************

;Set up to use right port (UC1)

SETPORT:LDA	NEWIOB		;Set up to access the GIGI
	STA	IOBYTE
	RET

;Reset to previous console

SETCON:	LDA	OLDIOB		;Set up to access the VT180
	STA	IOBYTE
	RET

;Enable echoing of input characters

ECHO:	MVI	A,0FFh
	STA	ECHOI
	RET

;Disable echoing of input characters

NOECHO:	XRA	A
	STA	ECHOI
	RET

;Enable duplicate output to the CRT

DUPOUT:	MVI	A,0FFh
	STA	ECHOO
	RET

;Disable duplicate output to the CRT

NODUP:	XRA	A
	STA	ECHOO
	RET

;******************************************************************************

REM:	LDA	CMDBYTE		;Check for command after "REM"
	LHLD	LASTCMD		;Fall into MAIN

;Here if there is a command to be processed
;HL points to user's input, A has byte count

MAIN:	LXI	D,COMMAND	;Where to put result
	MVI	C,COMLEN	;Size
	CALL	GETWORD		;Skip leading blanks, get 1st word of command
	SHLD	LASTCMD		;Save pointer to start of possible file name
	STA	CMDBYTE		;Save byte count

	LXI	H,COMTAB	;Start at beginning of command table
NEXT:	MOV	A,M		;Check if end of command table
	CPI	0
	JZ	UNKNOWN		;User gave an unknown command
	LXI	D,COMMAND	;Point to the word
	MVI	C,COMLEN	;Size
	CALL	MATCH		;See if this one matches
	JNZ	NOFIND
	MOV	E,M		;Get low-order part of addr
	INX	H		;Point to hi-order
	MOV	D,M
	XCHG			;Copy to HL pair
GOTOHL:	PCHL			;Jump to subroutine

NOFIND:	INX	H		;Skip over the two-byte address
	INX	H
	JMP	NEXT

;Here if there was no match

UNKNOWN:CALL	SETCON
	LXI	H,ERRMSG	;Print '?Unknown GIGI command'
	CALL	OUTSTR
	JMP	NEWCMD

;******************************************************************************

;.SBTTL	Main routines


COMLEN	EQU	4	;Length of commands
COMTAB:
	DB	'?   '
	DW	 HELP
	DB	'DUMP'
	DW	 DUMP
	DB	'EXIT'
	DW	 EXIT
	DB	'HELP'
	DW	 HELP
	DB	'HOST'
	DW	 HOST
	DB	'LOAD'
	DW	 LOAD
	DB	'PORT'
	DW	 PORT
	DB	'REM '
	DW	 REM
	DB	'RUN '
	DW	 RUN
	DB	'SAVE'
	DW	 SAVE
	DB	'SHOW'
	DW	 SHOW
	DB	0		;End of command table


HELP:	LXI	H,HLPTXT	;Point to the help message
	CALL	OUTSTR		;Output it
	JMP	NEWCMD

PORT:	LXI	H,PORTXT	;Point to message describing ports
	CALL	OUTSTR
	JMP	NEWCMD

HOST:	CALL	INLMES
	DB	'Now returning to CP/M',CR,LF,0
	JMP	EXIT

SAVE:	CALL	SETPORT		;Set up to talk to GIGI
	CALL	SETCON	;*HACK*
	CALL	INLMES
	DB	'SAVE command not yet implemented',CR,LF,0
	JMP	NEWCMD

RUN:	CALL	LOADIT		;First load program into the GIGI
	CALL	SETPORT
	CALL	INLMES		;Switch from HOST-BASIC to LOCAL-BASIC
	DB	'PRINT #1,ESC$;"PrBA1";GOFF$; : RUN',CR,LF,0
	CALL	NOECHO		;Don't echo commands coming back from the GIGI
	JMP	RESTART		;Wait for REM HOST or REM SAVE FILENAME

LOAD:	CALL	LOADIT		;Load the program
	CALL	SETPORT
	CALL	INLMES		;Switch from HOST-BASIC to LOCAL-BASIC
	DB	'PRINT #1,ESC$;"PrBA1";GOFF$;"Program loaded"',CR,LF,0
	CALL	NOECHO		;Don't echo commands coming back from the GIGI
	JMP	RESTART		;Wait for REM HOST or REM SAVE FILENAME

LOADIT:	LDA	'G'		;Default file extension is 'GGB'
	LXI	H,'GB'
	CALL	GETFILE		;Get file name, put it in FCB
	JM	NOFILE		;Abort if 0FFh returned
	CALL	SETCON		;Output next message to the VT180
	CALL	INLMES		;Set jump scrolling
	DB	ESC,'[?4l	[Program being loaded into the GIGI]',CR,LF,0
	CALL	SETPORT		;Set up to talk to GIGI
	CALL	INLMES		;Enable "host BASIC", clear old program
	DB	ESC,'PrBA2',ESC,'\',CR,LF,'NEW',CR,LF,0
	CALL	DUPOUT		;Output to the VT180 as well
	CALL	TYPFILE		;Send to the GIGI
	CALL	NODUP		;Disable duplicate output
	CALL	SETCON
	CALL	INLMES
	DB	ESC,'[?4h	[End of file]',CR,LF
	DB	'Waiting for "REM HOST" command from the GIGI',CR,LF,0
	CALL	SETPORT		;End with GIGI selected
	RET

NOFILE:	CALL	INLMES
	DB	'PRINT "?No such file"',CR,LF,0
	RET

SHOW:	CALL	SETCON		;Make sure error message go to the console
	LDA	'P'		;Default file extension is 'PIC'
	LXI	H,'IC'
	CALL	GETFILE		;Get file name, put it in FCB
	JM	NOFILE		;Abort if 0FFh returned
	CALL	SETPORT		;Set up to talk to GIGI
	CALL	INLMES		;Enable ReGIS, reset parameters, erase screen
	DB	ESC,'Pp;S(I0,N0,A,E)W(V,I7,A0,S0,M1,N0,P1(M2))T(I0,A0,D0,S1)',0
	CALL	DUPOUT		;Output to the VT180 as well
	CALL	TYPFILE		;Send it to the GIGI
	CALL	NODUP		;Disable duplicate output
	CALL	INLMES
	DB	ESC,'\',ESC,'[H',0 ;Turn off ReGIS, home cursor
	CALL	SETCON
	JMP	NEWCMD

DUMP:	CALL	SETCON		;Output file to the console
	LDA	'T'		;Default file extension is 'TXT'
	LXI	H,'XT'
	CALL	GETFILE		;Get file name, put it in FCB
	JM	NOFILE		;Abort if 0FFh returned
	CALL	TYPVT100	;Output with VT100 special characters
	JMP	NEWCMD

;*****************************************************************************

;.SBTTL Subroutines

INCHR:	PUSH	B
	PUSH	D
	PUSH	H
	MVI	C,DCONIO	;Function for direct console I/O
	MVI	E,0FFh		;Input with no echo
	CALL	BDOS		;Get a char (if any)
	POP	H
	POP	D
	POP	B
	RET


;Routine to output an ASCIZ string.  Call with address of string in HL

OUTSTR:	MOV	A,M		;Get current byte
	CPI	0		;End of string?
	RZ			;Yes
	CALL	OUTCHR		;Output character to right device
	INX	H		;Point to next byte
	JMP	OUTSTR		;Loop til end of ASCIZ


;Routine to output an in-line message

INLMES:	POP	H		;Get addr of byte
	MOV	A,M		;Get byte
	INX	H		;Point to next one
	ORA	A		;Check for zero
	JZ	GOTOHL		;Return if so
	PUSH	H		;Save pointer
	CALL	OUTCHR		;Output character
	JMP	INLMES		;Loop till 0


;Routine to output character in A to the currently selected port

OUTCHR:	PUSH	B
	PUSH	D
	PUSH	H
	MOV	E,A		;Put in right reg
	MVI	C,CONOUT	;Assume TTY:, CRT:, or UC1:
	LDA	IOBYTE		;Get current port number
	ANI	3		;Look at only the lowest 2 bits
	CPI	MDM		;Check for modem port
	JNZ	OUTCH1		;Not
	MVI	C,MDMOUT	;Modem connected to reader/punch
OUTCH1:	CALL	BDOS
	POP	H
	POP	D
	POP	B
	RET


CRLF:	MVI	A,CR		;Carriage return
	CALL	OUTCHR
	MVI	A,LF		;Linefeed
	CALL	OUTCHR
	RET

;******************************************************************************

;Routine to send a file to the GIGI and type it on the VT180 CRT as well

TYPFILE:MVI	C,READF		;Read next sector from file
	LXI	D,FCB
	CALL	BDOS
	ORA	A		;Test return code
	RNZ			;Return at EOF
	LXI	H,DMABUF	;Point to the data
	MVI	C,80h		;Number of bytes

TYPFL1:	CALL	SETPORT		;Set IOBYTE to point to the port
	MOV	A,M		;Get byte
	CPI	CTRLZ		;Check for Control-Z
	RZ			;Done if so
	CPI	0		;Ignore null bytes
	JZ	TYPFL3		;(found in files orginating from TOPS-10)
	CALL	OUTCHR		;Send char to the GIGI
	CALL	INCHR		;Eat any echo from the GIGI
	CPI	CTRLC		;Control-C?
	JZ	ABORT		;Abort if so
	CALL	SETCON		;Set IOBYTE to point to the console
	LDA	ECHOO		;Echo on output?
	ORA	A
	JZ	TYPFL2		;No
	LDA	DIFIOB		;Is the port different from the console?
	ORA	A
	JZ	TYPFL2		;If same, don't honor ECHOO
	MOV	A,M		;Get the character again
	CALL	OUTCHR		;Send a duplicate to the console
TYPFL2:	CALL	INCHR		;Check for Control-C at the console
	CPI	CTRLC
	JZ	ABORT
TYPFL3:	INX	H		;Point to next one
	DCR	C		;Do all 128 characters
	JNZ	TYPFL1
	JMP	TYPFILE		;Get another record


;Routine to output file with displayable control characters
;If the parity bit is on, the character will be underlined.
;The characters HT,LF,VT,FF,CR are output using those mnemonics.
;NUL shows up as a centered dot, DEL as a splotch.

TYPVT100:CALL	INLMES		;Set up so SO goes to line-drawing set
	DB	ESC,')0',ESC,'(B',SI,0
	MVI	B,80		;80 characters per line
TYP100:	MVI	C,READF		;Read next sector from file
	LXI	D,FCB
	PUSH	B
	CALL	BDOS
	POP	B
	ORA	A		;Test return code
	RNZ			;Return at EOF

	MVI	C,128		;Number of bytes
	LXI	H,DMABUF	;Point to the data
TYP101:	SHLD	HLSAVE		;Save pointer
	MOV	A,M		;Get byte
	ORA	A		;Test the parity bit
	JP	TYP102		;No underline if off
	CALL	INLMES
	DB	ESC,'[4m',0	;Set underline attribute
	LHLD	HLSAVE
	MOV	A,M
TYP102:	ANI	7Fh		;Ignore parity bit now
	JNZ	TYP103		;Check for null
	CALL	INLMES
	DB	SO,'~',SI,0	;Output a centered dot, not inverse video
	JMP	TYP106
TYP103:	CPI	DEL		;Check for delete
	JNZ	TYP104
	CALL	INLMES
	DB	SO,'a',SI,0	;Output a splotch
	JMP	TYP106
TYP104:	CPI	20h		;Control character?
	JP	TYP105
	CPI	1Fh		;Control-Underline?
	JNZ	TYP104A
	CALL	INLMES
	DB	ESC,'[7m_',0	;Cannot use VT100 line-drawing set for this
	JMP	TYP104B
TYP104A:PUSH	PSW
	CALL	INLMES
	DB	ESC,'[7m',SO,0	;Yes, set inverse video
	LXI	D,TYPCNV	;Point to the conversion table
	POP	PSW
	ADD	E		;Index into table
	MOV	E,A
	MVI	A,0		;Now add in any carry
	ADC	D
	MOV	D,A
	LDAX	D		;Get translated byte
	CALL	OUTCHR		;Display printing character
TYP104B:CALL	INLMES
	DB	ESC,'[0m',SI,0	;Normal video
	JMP	TYP107
TYP105:	CALL	OUTCHR		;Send char to the VT180
TYP106:	LHLD	HLSAVE
	MOV	A,M		;Get original again
	ORA	A		;Test parity bit
	JP	TYP107
	CALL	INLMES		;Hi bit set, therfore
	DB	ESC,'[0m',0	; turn off underline
TYP107:	DCR	B		;Count down 80 characters per line
	JNZ	TYP108
	CALL	CRLF		;New line every 80
	MVI	B,80
TYP108:	CALL	INCHR		;Check for Control-C at the console
	CPI	CTRLC
	JZ	ABORT
	LHLD	HLSAVE
	INX	H		;Point to next one
	DCR	C		;Do all 128 characters
	JNZ	TYP101
	JMP	TYP100		;Get another record

;Null=centered-dot, Tab=HT, LF=LF, VT=VT, FF=FF, CR=CR, Delete=splotch
TYPCNV:	DB	'~ABCDEFGHbeicdNOPQRSTUVWXYZ[\]^r' ;Lowercase is special

;*****************************************************************************

;Routine to get a command, skipping leading blanks
;Command is terminated by a trailing blank, a null, or byte count exhausted
;Call with pointer to possible command in HL and byte count in A.
;DE points to where the command will be stored, C has count of bytes
;Returns updated pointer in HL and new byte count in A.

GETWORD:MOV	B,A		;Copy byte count
	PUSH	D		;Save pointer for return
	JMP	GETCM1		;Don't skip over first byte

;Find first non-blank

GETCM0:	INX	H		;Point to next char
	DCR	B		;Check the count
	JM	GETCM5		;End of command - nothing or all blanks
GETCM1:	MOV	A,M		;Get command from command line
	CPI	' '		;Blank?
	JZ	GETCM0		;Yes, skip it
	CPI	TAB		;Control-I?
	JZ	GETCM0		;Yes, skip it also
	JMP	GETCM3		;Found a non-blank

;Input, converting tabs to blanks

GETCM2:	INX	H		;Point to next char
	DCR	B		;Check the count
	JZ	GETCM5		;End of command
	MOV	A,M		;Get char from command line
	CPI	TAB		;Control-I?
	JNZ	GETCM3		;No, keep it
	MVI	A,' '		;Yes, convert it to a blank

;Store chars in COMMAND if there is room

GETCM3:	DCR	C		;Check if more room in COMMAND
	JM	GETCM4		;Skip STAX if negative
	STAX	D		;Copy into COMMAND
	INX	D		;Point to next position in COMMAND
GETCM4:	CPI	' '		;Check if blank
	JZ	GETCM5		;Stop if so
	CPI	'='
	JZ	GETCM5
	CPI	','
	JNZ	GETCM2		;Loop for rest of command

;End of input, fill COMMAND with trailing blanks for the error message

GETCM5:	MVI	A,' '		;Get a space
	DCR	C		;See if all 4 filled up
	JM	GETCM6		;Done if negative
	STAX	D		;Blank fill
	INX	D
	JMP	GETCM5

GETCM6:	POP	D		;Point to the extracted command
	MOV	A,B		;A has remaing count, HL point to last char
	RET			;End of GETCMD

;*****************************************************************************

;Routine to check if COMMAND and (HL) match
;Call with addr of COMMAND in DE and its size in C

MATCH:	MVI	B,0		;Set flag to good
MATCH1:	LDAX	D		;Get a char from user
	CPI	' '		;Blank?
	JZ	MATCH2		;Yes, accept first abbreviation
	CMP	M		;Match?
	JZ	MATCH2		;So far so good
	INR	B		;Set B nonzero when mismatch occurs
MATCH2:	INX	H		;Next one to match
	INX	D		;Next one to check
	DCR	C		;Decrement count
	JNZ	MATCH1		;Check all 4
	MOV	A,B		;Get flag
	ORA	A		;Set Z bit if it matched
	RET			;Return from MATCH

;Routine to input a line from the selected port with no echo
;This routine duplicates READ-CONSOLE-BUFFER, but no echo to the GIGI

RDLINE:	XRA	A		;Clear count of bytes read
	MOV	H,D		;Reset address
	MOV	L,E
	INX	H		;Skip over the size byte
	PUSH	H		;Save for return

RDLN1:	PUSH	PSW		;Save count
RDLN2:	CALL	INCHR		;Get character (if any) from new console
	ORA	A		;Check if null
	JZ	RDLN2		;Nothing there, try again
	CPI	CTRLC		;Control-C ?
	JZ	ABORT		;Yes, do a warm boot
	MOV	M,A		;Store character
	LDA	ECHOI		;Check if input should be echoed
	ORA	A
	JZ	RDLN3		;No echo
	MOV	A,M		;Get character
	CALL	OUTCHR		;Echo it
	MOV	A,M		;Get the char again
	CPI	CR
	JNZ	RDLN3		;Not CR
	MVI	A,LF		;Is, so echo a linefeed
	CALL	OUTCHR
RDLN3:	MOV	A,M		;Get char back
	CPI	CR		;RETURN character?
	JZ	RDLN4		;Yes, all done
	INX	H
	POP	PSW		;Get count
	INR	A		;Increment it
	JMP	RDLN1

RDLN4:	POP	PSW		;Restore count
	POP	H		;Point to start of buffer
	RET

;******************************************************************************

;Routine to parse file name and format FCB
;copy FCB2 to FCB1 (*HACK* for now)

GETFILE:LXI	H,FCB2		;Source
	LXI	D,FCB		;Destination
	LXI	B,1+8+3		;Number of bytes
	CALL	MOVER		;Copy data
	MVI	C,OPENF
	LXI	D,FCB
	CALL	BDOS		;Open file for input
	ORA	A		;Test for success
	RET			;Return OK if not 0FFh

MOVER:	MOV	A,M		;Get byte
	STAX	D		;Store it
	INX	H
	INX	D
	DCR	C
	JNZ	MOVER
	RET

;******************************************************************************

;.SBTTL	Data section (variables)

;The lines for ERRMSG,COMMAND,PROMPT must be in order!
ERRMSG:	DB	'?Unknown GIGI command - '
COMMAND:DB	'****'	;4 bytes
	DB	CR,LF,'Type "HELP" for help',CR,LF,0
PROMPT:	DB	'GIGI>',0
ENDBYTE:DB	'$'

PAGES EQU (ENDBYTE-GIGI+0FFh)/100h ;Size of this program in 256-byte pages

HLSAVE:	DS	2	;Save HL
ECHOI:	DS	1	;Nonzero to echo input
ECHOO:	DS	1	;Nonzero to echo output
CMD1:	DS	1	;Loop at NEWCMD if this is a blank
CMDBYTE:DS	1	;Number of bytes of command
LASTCMD:DS	2	;Pointer to last command parsed
OLDIOB:	DS	1	;Original value of IOBYTE
NEWIOB:	DS	1	;New value
DIFIOB:	DS	1	;Nonzero if NEWIOB not equal OLDIOB
LINBUF:	DS	128	;Line buffer

	END	START
    