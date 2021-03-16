File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

1)1		TTL	'E B U S  --  PDP-10 Base Code, Version 2.00'
1)	*			***** ****  *   *  ****
****
2)1	* EBUS02.JMS = EBUS02.J01 + modifications from Joe Smith
2)		TTL	'E B U S  --  PDP-10 Base Code, Version 2.02'
2)	*			***** ****  *   *  ****
**************
1)1	VERSION	EQU	$201			; VERSION NUMBER
1)	PRODID	EQU	$91			; product-ID
****
2)1	* Version 2.02 created by Joe Smith 19-Apr-88.
2)	* Includes fixes for bugs found by reading the code and observations
2)	* by Paul Krumviede.  This is NOT an official version!   /JMS
2)	VERSION	EQU	$202			; VERSION NUMBER
2)	PRODID	EQU	$91			; product-ID
**************
1)1		BRA	VCRASH			; go do it
1)		ENDM
****
2)1		BSR	VCRASH			; crash host (68K keeps running)
2)		ENDM
**************
1)1		BSR.\0	BCRASH			; go do it
1)		ENDM
****
2)1		BSR.\0	BCRASH			; tell ISIS then halt
2)		ENDM
**************
1)1	        ORG	$900			; where we actually begin
1)		SPC	3
1)		TTL	'E B U S  --  PDP-10 Base Code,		V A R I A B L E S'
1)		PAGE
1)		DS.L	490			: The Bottom of the Stack
1)		DS.L	0
1)		ORG	$1000
****
2)1		TTL	'E B U S  --  PDP-10 Base Code,		V A R I A B L E S'
2)		PAGE
2)	        ORG	$900			; where we actually begin
2)		SPC	3
2)	STKBOT  DS.L    0			: The Bottom of the Stack
2)		ORG	$1000
**************
1)1	ENTRY	MOVE.L	A1,ISTOME		; address of ISIS-to-us message area
****
2)1	*	Enter here from MACSbug with the following registers set up:
2)	*	A1 has address of kernel-to-68000 message area.
2)	*	A2 has address of 68000-to-kernel message area.
2)	*	A7 set up as stack pointer with D0-D7/A0-A6 pushed on it.
2)	ENTRY	MOVE.L	A1,ISTOME		; address of ISIS-to-us message area
**************
1)1		MOVE.L	#2,(A2)
1)		POPM	D0-D7/A0-A6
****
File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

2)1		MOVE.L	#IPDLOK,(A2)		; #2 = download OK
2)		POPM	D0-D7/A0-A6
**************
1)1	BOBCODE	MOVE	#$2700,SR		; Disable Interrupts
****
2)1	* The following kernel-to-68000 messages are defined in I2IS07.R03:
2)	*	EIPDLD EQ 01000000 :DOWN-LOAD AND START
2)	*	EIPCRA EQ 03000000 :ENGINE CRASH - GO CRASH HOST
2)	*	EIPIOW EQ 04000000 :START ADDR OF I/O WINDOW (SLOT TO IPI DATA)
2)	*	EIPRIN EQ 05000000 :TEST MESSAGE (RE-INIT), SENT ONCE PER SECOND
2)	*	SLTINT EQ 06000000 :SLOT WANTS TO INTERRUPT THE IPI
2)	*	EIPGMB EQ 07000000 :GO TO MACSBUG
2)	* The following 68000-to-kernel responses are defined:
2)	IPANMA	EQU	1	   ;Acknowledge new message area
2)	IPDLOK	EQU	2	   ;Down-line load OK (implies IPANMA as well)
2)	IPCRSH	EQU	3	   ;IPI is requesting a crash (68K detected problem)
2)	IPAIOW	EQU	4	   ;Acknowlege I/O window addr
2)	IPASI	EQU	6	   ;Acknowlege slot interrupt request
2)	BOBCODE	MOVE	#$2700,SR		; Disable Interrupts
**************
1)1		BNE	FFCRA			; anything but 4 or 5 is reason to crash
1)		POPM	D0-D7/A0-A6
****
2)1		BNE	FFCRA			; if not 4 or 5, go crash
2)	* Message 5 (EIPRIN) is sent once a second.  Ignore it.
2)		POPM	D0-D7/A0-A6
**************
1)1	FFCRA	MOVE.B	#$EF,CODCASH		: value in case Fatal Crash
1)		CMPI.W	#$0300,D0		; additional check
1)		BNE.L	BCRASH			; other than 3 causes us to crash (FATAL)
1)		MOVE.B	#$EC,CODCASH		; else set reason
1)		BSR.L	REGDMP			; and dump our registers
****
2)1	* Message 3 (EIPCRA) means slot is halted, go crash host and halt 68K
2)	FFCRA	CMPI.W	#$0300,D0		; EIPCRA means slot has crashed
2)		BNE.L	FFCRA1			; illegal message
2)		MOVE.B	#$EC,CODCASH		; else set reason (KEY=354 octal)
2)		BSR.L	REGDMP			; and dump our registers
**************
1)1	GETIOWA	MOVE.L	(A0),D0			; get ISIS-to-us message
****
2)1	* Message 1 (EIPDLD) should never get here since we give the interrupt
2)	* vector back to MACSBUG.  If it does, just do a warm start.  (4/19/88 /JMS)
2)	FFCRA1	CMPI.W	#$0100,D0		; Check for reload command
2)		BEQ	GETIOWB			; Use previous pointers
2)	        POPM	D0-D7/A0-A6		; Don't create stack overflow
2)		FCRASH.L  $EF			; Illegal message from ISIS
2)	* Message 4 (EIPIOW) has addr of I/O window to access slot's memory
2)	GETIOWA	MOVE.L	(A0),D0			; get ISIS-to-us message
**************
1)1		MOVE.L	METOIS,A0		; get us-to-ISIS address
1)		MOVE.L	#4,(A0)			; tell ISIS "4"
1)		POPM	D0-D7/A0-A6		; restore registers
****
File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

2)1	GETIOWB	MOVE.L	METOIS,A0		; get us-to-ISIS address
2)		MOVE.L	#IPAIOW,(A0)		; tell ISIS "4"
2)		POPM	D0-D7/A0-A6		; restore registers
**************
1)1		MOVEA.L	.MDUMP,A0		; Engine destination
1)	MEMDMP1	MOVE.W	(A0)+,(A1)+		; dump memory
****
2)1	*bug*	MOVEA.L	.MDUMP,A0	*bug* noticed 2-Jul-86 by JMS
2)		MOVEA.L	.MDUMP,A1		; Engine destination
2)	MEMDMP1	MOVE.W	(A0)+,(A1)+		; dump memory
**************
1)1	*	Routine that crashes the host for interface problems
1)	VCRASH	LEA	STKTOP,SP		; reset stack
1)		TST.B	DIAG
****
2)1	*[JMS] Save PC at time of slot crash
2)	*	Routine that crashes the host for interface problems
2)	VCRASH	MOVE.L	(SP)+,PCCASH		; save caller's address
2)		LEA	STKTOP,SP		; reset stack
2)		TST.B	DIAG
**************
1)1		DC.W	OPEN			; <  01 - Host Open
1)		DC.W	SHUT			; <  02 - Host Shut
****
2)1		DC.W	OPEN			; <> 01 - Host Open
2)		DC.W	SHUT			; <  02 - Host Shut
**************
1)1		CMP.B	#$0F,D0			;				<2.2
1)		BNE	NOJAP1			;				<2.2
1)		ADD.B	#1,D0			; translate subtype 15d to subtype 16d <2.2
1)	NOJAP1	BSR	PUTCH
****
2)1	*[JMS] Removed translation of PARM-D to KATAKANA, PDP-10 code fixed
2)	*	CMP.B	#$0F,D0			; *hack* remove this test when
2)	*	BNE	NOJAP1			; the PDP-10 code is fixed.
2)	*	ADD.B	#1,D0			; translate subtype 15d to 16d
2)	NOJAP1	BSR	PUTCH
**************
1)1		CMP.B	#$0F,D0
1)		BNE	NOJAP2
1)		ADD.B	#1,D0			; type 15d translated to type 16d
1)	NOJAP2	BSR	PUTCH			; subtype
****
2)1	*[JMS] Removed translation of PARM-D to KATAKANA, PDP-10 code fixed
2)	*	CMP.B	#$0F,D0
2)	*	BNE	NOJAP2
2)	*	ADD.B	#1,D0			; type 15d translated to type 16d
2)	NOJAP2	BSR	PUTCH			; subtype
**************
1)1		MOVE.B	NCCT,DAT3		; CCT
1)		MOVE.W	NORNODE,D0
1)		LSR.W	#6,D0
1)		MOVE.B	D0,DAT4			; high 6-bits of originating node
1)		BSR	WRDAT			; send DAT to Host
File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

1)		MOVE.W	NORNODE,D0
1)		AND.W	#$3F,D0			; low 6-bits of originating host
1)		MOVE.B	D0,DAT3
****
2)1		MOVE.B	NCCT,DAT3		; CCT = $C0 + circuit characteristics
2)		MOVE.W	NORNODE,D0
2)		LSR.W	#6,D0
2)		MOVE.B	D0,DAT4			; high 8-bits of originating node
2)		BSR	WRDAT			; send DAT to Host
2)		MOVE.W	NORNODE,D0
2)		AND.W	#$3F,D0			; low 6-bits of originating node
2)		MOVE.B	D0,DAT3
**************
1)1	NEE1	LEA	OOPMSG,A0		; set pointer
1)		BSR	SENDTEXT		; tell reason (with bells)
****
2)1	NEE1	LEA	OOPMSG,A0		; set pointer to [host out of ports]
2)		BSR	SENDTEXT		; tell reason (with bells)
**************
1)1	NEE2	LEA	HDNMSG,A0		; set pointer
1)	NEE3	BSR	SENDTEXT		; tell user
****
2)1	NEE2	LEA	HDNMSG,A0		; set pointer to [host down]
2)	NEE3	BSR	SENDTEXT		; tell user
**************
1)1	NEE4	LEA	HSHMSG,A0		; set pointer
1)		BRA	NEE3			; and go clean up
****
2)1	NEE4	LEA	HSHMSG,A0		; set pointer to [host shut]
2)		BRA	NEE3			; and go clean up
**************
1)1	*	ISIS INPUT MESSAGE DISPATCH TABLE
****
2)1	*[JMS] Changed ISP to ISPM because ISP is a reserved word on SUN assembler
2)	*	ISIS INPUT MESSAGE DISPATCH TABLE
**************
1)1		DC.W	ISP			; AD - ORANGE BALL, Send on immediately
1)		DC.W	STREAM			; AE - BREAK BEGIN, Send on
****
2)1		DC.W	ISPM			; AD - ORANGE BALL, Send on immediately
2)		DC.W	STREAM			; AE - BREAK BEGIN, Send on
**************
1)1		BEQ	ISP			; no...just proceed
1)		BSR	DPORT			; yes...count it gone
1)		BSR	RBTALL			; reset everything
1)		BRA	ISP			; ...and proceed
1)	*	A0 -- apply back-pressure
1)	BPON	BSET	#PF_OBP,P_FLAGS(Rp)	; set output back-pressure
1)		BRA	ISP
1)	*	A1 -- release back-pressure
1)	BPOFF	BCLR	#PF_OBP,P_FLAGS(Rp)	; clear output back-pressure
1)		BRA	ISP
1)	*	A2 -- Gobbler
1)	GOBBLER	BTST	#PF_BKI,P_FLAGS(Rp)	; block-input active?
File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

1)		BNE	ISP			; yes...just pass it on
1)		BSR	DEMPTY			; no...empty the buffer
1)		BSR.L	BU_EMP			; release back-pressure
1)		BRA	ISP			; ...then pass it on
1)	*	A5 -- Gray ball
1)	GRAY	CLR.B	DAT3			; d3 = 0 for grey ball
1)		BRA	ISP			; then process as special message
1)	*	A4 -- Black Ball
1)	BLACK	MOVE.B	#-1,DAT3		; special parameter
1)		BRA	ISP
1)	*	AC - Yellow Ball
****
2)1		BEQ	ISPM			; no...just proceed
2)		BSR	DPORT			; yes...count it gone
2)		BSR	RBTALL			; reset everything
2)		BRA	ISPM			; ...and proceed
2)	*	A0 -- apply back-pressure
2)	BPON	BSET	#PF_OBP,P_FLAGS(Rp)	; set output back-pressure
2)		BRA	ISPM
2)	*	A1 -- release back-pressure
2)	BPOFF	BCLR	#PF_OBP,P_FLAGS(Rp)	; clear output back-pressure
2)		BRA	ISPM
2)	*	A2 -- Gobbler
2)	GOBBLER	BTST	#PF_BKI,P_FLAGS(Rp)	; block-input active?
2)		BNE	ISPM			; yes...just pass it on
2)		BSR	DEMPTY			; no...empty the buffer
2)		BSR.L	BU_EMP			; release back-pressure
2)		BRA	ISPM			; ...then pass it on
2)	*	A5 -- Gray ball
2)	GRAY	CLR.B	DAT3			; d3 = 0 for grey ball
2)		BRA	ISPM			; then process as special message
2)	*	A4 -- Black Ball
2)	BLACK	MOVE.B	#-1,DAT3		; special parameter
2)		BRA	ISPM
2)	*	AC - Yellow Ball
**************
1)1		BEQ	ISP			; no...just pass it on
1)	STREAM1	MOVEQ	#ESC,D0			; Yes...place escape
****
2)1		BEQ	ISPM			; no...just pass it on
2)	STREAM1	MOVEQ	#ESC,D0			; Yes...place escape
**************
1)1	ISP	BSR	LOOKUP			; go translate message-type
1)		BRA	PUTPN			; go send it
****
2)1	ISPM	BSR	LOOKUP			; go translate message-type
2)		BRA	PUTPN			; go send it
**************
1)1		CMP.B	#$10,D0			;				<2.2
1)		BNE	NOJAP			;				<2.2
1)		SUB.B	#1,D0			; translate 16d to 15d		<2.2
1)	NOJAP	MOVE.B	D0,DAT3			; parameter code
****
2)1	*[JMS] Removed translation of KATAKANA to PARM-D, PDP-10 code fixed
File 1)	DSK:EBUS02.J01	created: 0959 02-JUL-86
File 2)	DSK:EBUS02.JMS	created: 1555 12-JUL-88

2)	*	CMP.B	#$10,D0			; *hack* remove this test when
2)	*	BNE	NOJAP			; the PDP-10 code is fixed.
2)	*	SUB.B	#1,D0			; translate 16d to 15d
2)	NOJAP	MOVE.B	D0,DAT3			; parameter code
**************
1)1		CMP.L	LFASTC,D0		; LFASTC specifies NEXT time to do it!
1)		BLT.L	HALFSEX			; not time yet
1)		ADD.L	#300,D0			; we do this every half-second
****
2)1	*[JMS] Watch out for signed arithmetic.  Use BMI, not BLE or BLS
2)		CMP.L	LFASTC,D0		; LFASTC specifies NEXT time to do it!
2)		BMI.L	HALFSEX			; not time yet
2)		ADD.L	#300,D0			; we do this every half-second
**************
1)1		MOVE.W	#$380+DEBUG,DAT1	; set "RESET INTERFACE"
1)		MOVE.W	#VERSION,DAT3
1)		BSR.L	WAITDAT			; and send it
1)		BSR.L	WAITDAT			;  ...twice...just in case
1)		CLR.B	HTDWN			; 0 = host is up
****
2)1		MOVE.W	#$180+DEBUG,DAT1	; send "INTERFACE ANSWERED"
2)		MOVE.W	#VERSION,DAT3
2)		BSR.L	WAITDAT			; and send it
2)		CLR.B	HTDWN			; 0 = host is up
**************
1)1	HALFSEX	*				; exit to here...
1)	*					 ...and fall through to DOBKOUT
****
2)1	HALFSEX	EQU	*			; exit to here...
2)	*					 ...and fall through to DOBKOUT
**************
     