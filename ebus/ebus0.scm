File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

1)1		TTL	'E B U S  --  PDP-10 Base Code, Version 2.00'
****
2)1	*  EBUS02.JPK = EBUS02.J00 + modifications from Paul Krumviede
2)		TTL	'E B U S  --  PDP-10 Base Code, Version 2.00'
**************
1)1	VERSION	EQU	$200			; VERSION NUMBER
1)	PRODID	EQU	$91			; product-ID
****
2)1	*[PK] This is the unofficial version 2.02
2)	VERSION	EQU	$202			; VERSION NUMBER
2)	PRODID	EQU	$91			; product-ID
**************
1)1		MOVE.B	#VAL,CODCASH		; set crash-code
1)		BRA	VCRASH			; go do it
1)		ENDM
****
2)1	*[PK] Do a BSR so PC of CRASH macro gets stored
2)		MOVE.B	#VAL,CODCASH		; set crash-code
2)		BSR	VCRASH			; go do it
2)		ENDM
**************
1)1	        ORG	$900			; where we actually begin
1)		SPC	3
1)		TTL	'E B U S  --  PDP-10 Base Code,		V A R I A B L E S'
1)		PAGE
1)		DS.L	490			: The Bottom of the Stack
1)		DS.L	0
1)		ORG	$1000
1)	STKTOP	DS.L	0			; the end (Top) of the stack
1)	 IF	TRACING
****
2)1		TTL	'E B U S  --  PDP-10 Base Code,		V A R I A B L E S'
2)		PAGE
2)		ORG	$0900			; Bottom of stack
2)		ORG	$1000			; Top of stack
2)	STKTOP	DS.L	0			; Initial value of the stack
2)	 IF	TRACING
**************
1)1	NORNODE	DS.W	1			; normal login origination node
****
2)1	NORHOST	DS.W	1			; normal login origination host
2)	NORNODE	DS.W	1			; normal login origination node
**************
1)1	FFCRA	CMPI.W	#$0300,D0		; additional check
1)		BNE.L	BCRASH			; other than 3 causes us to crash (FATAL)
1)		MOVE.B	#$EC,CODCASH		; else set reason
1)		BSR.L	REGDMP			; and dump our registers
1)		BSR.L	HCRASH			; then crash the host
****
2)1	*[PK] Because we have stolen the interrupt vector from MACSbug, we must
2)	*[PK] be able to handle the "go reload yourself" command.  (This problem
2)	*[PK] does not occur in EBUS02.J01 because it restores the vector.)
2)	FFCRA	CMPI.W	#$0300,D0		; additional check
2)		beq.s	ffcra5			; code 3 = ISIS wants us to crash host
File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

2)		cmpi.w	#$0100,D0		; is this a load command?
2)		beq.s	ffcra2			; yes, go eat it
2)		move.b	#$EF,codcash		; set a crash reason
2)		lsr.w	#8,D0
2)		move.b	D0,codcash+1		; and save what we got
2)		movem.l	(A7)+,D0-D7/A0-A6	; clean up stack
2)		bsr.l	bcrash			; and go crash nicely, without returning
2)	ffcra2	move.l	metois,A0		; yes, get ISIS message area
2)		move.l	#2,(A0)			; acknowledge down-load
2)		movem.l	(A7)+,D0-D7/A0-A6	; clean up stack
2)		bsr	intrpt			; go interrupt engine
2)		stop	#$2000			; and wait for another interrupt
2)	FFCRA5	MOVE.B	#$EC,CODCASH		; else set reason
2)	*	BSR.L	REGDMP			; and dump our registers
2)		BSR.L	HCRASH			; then crash the host
**************
1)1		MOVE.W	#$6B,P10ADR		; from o153
1)		BSR.L	GETPDPL
1)		MOVE.W	D1,PDPIRP		; Input-ring Start-address
1)		MOVE.W	#$6C,P10ADR		; from o154
1)		BSR.L	GETPDPL
1)		MOVE.W	D1,PDPISZ		; Input-ring size
1)		BSR.L	GETIEC			; where PDP thinks his cursor is
1)		MOVE.W	D1,PDPIFC		; ...we'll start there too
1)		MOVE.B	#3,DAT1			; set "RESET INTERFACE"
****
2)1	*[PK] Use routine RDPDPRP (which was extracted from EBUS02.J01)
2)		BSR.L	RDPDPRP			; read PDP ring pointers
2)		MOVE.B	#3,DAT1			; set "RESET INTERFACE"
**************
1)1		SPC	6
****
2)1	*[PK] This routine was copied verbatim from EBUS02.J01
2)	*	Read PDP's Ring-parameters
2)	*	First, read Output-ring parameters
2)	RDPDPRP	MOVE.W	#$6F,P10ADR		; Read from o157 (Left)
2)		BSR	GETPDPL
2)		MOVE.W	D1,PDPORP		;  ...Output-ring start-address
2)		MOVE.W	#$70,P10ADR		; from o160 (Left)
2)		BSR	GETPDPL
2)		MOVE.W	D1,PDPOSZ		;  ...Output-ring Size
2)		BSR	GETOFC			; Get output-ring fill cursor (from o161)
2)		MOVE.W	#72,P10ADR		; from o162 (Right)
2)		BSR	GETPDPR
2)		MOVE.W	D1,PDPOEC		;  ...output-ring empty cursor
2)	*	Next, read Input-ring parameters
2)		MOVE.W	#$6B,P10ADR		; from o153 (Left)
2)		BSR	GETPDPL
2)		MOVE.W	D1,PDPIRP		;  ...Input-ring Start-address
2)		MOVE.W	#$6C,P10ADR		; from o154 (Left)
2)		BSR	GETPDPL
2)		MOVE.W	D1,PDPISZ		;  ...Input-ring size
2)		BSR	GETIEC			; Get input-ring empty cursor (from o155)
2)		MOVE.W	#$6E,P10ADR		; from o156 (Right)
File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

2)		BSR	GETPDPR
2)		MOVE.W	D1,PDPIFC		;  ...Input-ring fill cursor
2)		RTS				; Done...return
2)		SPC	6
2)	*	Set up Block-IO address
2)		SPC	6
**************
1)1	VCRASH	LEA	STKTOP,SP		; reset stack
1)		TST.B	DIAG
****
2)1	*[PK] Save PC at time of crash
2)	VCRASH	MOVE.L	(SP)+,PCCASH		; save caller's address
2)		LEA	STKTOP,SP		; reset stack
2)		TST.B	DIAG
**************
1)1		CMP.B	#$0F,D0
1)		BNE	NOJAP1
1)		ADD.B	#1,D0			; translate subtype 15d to subtype 16d
1)	NOJAP1	BSR	PUTCH
****
2)1	*[PK] Removed translation from PARM-D to KATAKANA; the PDP-10 does it right
2)	*	CMP.B	#$0F,D0
2)	*	BNE	NOJAP1
2)	*	ADD.B	#1,D0			; translate subtype 15d to subtype 16d
2)	NOJAP1	BSR	PUTCH
**************
1)1		CMP.B	#$0F,D0
1)		BNE	NOJAP2
1)		ADD.B	#1,D0			; type 15d translated to type 16d
1)	NOJAP2	BSR	PUTCH			; subtype
****
2)1	*[PK] Removed translation from PARM-D to KATAKANA; the PDP-10 does it right
2)	*	CMP.B	#$0F,D0
2)	*	BNE	NOJAP2
2)	*	ADD.B	#1,D0			; type 15d translated to type 16d
2)	NOJAP2	BSR	PUTCH			; subtype
**************
1)1		BSR	GETH			; Orig. Port
****
2)1		MOVE.W	D0,NORHOST
2)		BSR	GETH			; Orig. Port
**************
1)1		MOVE.B	#6,DAT1			; type 6...New External Logon
1)		MOVE.B	PORTNO+1,DAT2		; port #
1)		BSR	WAITDAT			; wait, then send DAT to host
1)		MOVE.B	#$82,DAT1		; 2-byte Data message
1)		MOVE.B	NCCT,DAT3		; CCT
1)		MOVE.W	NORNODE,D0
1)		LSR.W	#6,D0
1)		MOVE.B	D0,DAT4			; high 6-bits of originating node
1)		BSR	WAITDAT			; wait, then send DAT to Host
1)		MOVE.W	NORNODE,D0
1)		AND.W	#$3F,D0			; low 6-bits of originating host
1)		MOVE.B	D0,DAT3
File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

1)		MOVE.B	NORPORT,DAT4		; originating port
1)		BSR	WAITDAT			; wait, then send DAT to host
****
2)1	*[PK] Send origination host in login message (from EBUS02.J01)
2)		MOVE.B	#6,DAT1			; type 6...New External Logon
2)		MOVE.B	PORTNO+1,DAT2		; port #
2)		MOVE.W	NORHOST,DAT3		; origination host (MUX, gateway)
2)		BSR	WAITDAT			; wait, then send DAT to host
2)		MOVE.B	#$82,DAT1		; 2-byte Data message
2)		MOVE.B	NCCT,DAT3		; CCT = $C0 + circuit type
2)		MOVE.W	NORNODE,D0
2)		LSR.W	#6,D0
2)		MOVE.B	D0,DAT4			; high 8-bits of originating node
2)		BSR	WAITDAT			; wait, then send DAT to Host
2)		MOVE.W	NORNODE,D0		; DAT1 still set for 2-bytes
2)		AND.W	#$3F,D0			; low 6-bits of originating node
2)		MOVE.B	D0,DAT3
2)		MOVE.B	NORPORT,DAT4		; originating port (GARBAGE)
2)		BSR	WAITDAT			; wait, then send DAT to host
**************
1)1	*	ISIS INPUT MESSAGE DISPATCH TABLE
****
2)1	*[PK] Changed ISP to ISPM (ISP is a reserved word on some assemblers)
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
File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

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
1)1		CMP.B	#$10,D0
1)		BNE	NOJAP
1)		SUB.B	#1,D0			; translate 16d to 15d
1)	NOJAP	MOVE.B	D0,DAT3			; parameter code
****
2)1	*[PK] Removed translation from PARM-D to KATAKANA; the PDP-10 does it right
2)	*	CMP.B	#$10,D0
2)	*	BNE	NOJAP
2)	*	SUB.B	#1,D0			; translate 16d to 15d
2)	NOJAP	MOVE.B	D0,DAT3			; parameter code
**************
1)1		CMP.L	LFASTC,D0		; LFASTC specifies NEXT time to do it!
1)		BLT.L	HALFSEX			; not time yet
1)		ADD.L	#300,D0			; we do this every half-second
****
2)1	*[PK] Changed BLT to BMI to avoid 41 day 10 hour problem
File 1)	DSK:EBUS02.J00	created: 1816 14-APR-86
File 2)	DSK:EBUS02.JPK	created: 1526 12-JUL-88

2)		CMP.L	LFASTC,D0		; LFASTC specifies NEXT time to do it!
2)		BMI.L	HALFSEX			; not time yet
2)		ADD.L	#300,D0			; we do this every half-second
**************
1)1		MOVE.W	#$73,P10ADR		; "DUMP" flag at o163
1)		CLR.L	D1			; set "DUMP" finished
1)		TST.W	DEXCO			; Reset EBUS
1)		BSR	PUTPDPR
1)	*	Output-ring parameters
1)		MOVE.W	#$6F,P10ADR		; Read from o157
1)		BSR	GETPDPL
1)		MOVE.W	D1,PDPORP		; Output-ring start-address
1)		MOVE.W	#$70,P10ADR		; from o160
1)		BSR	GETPDPL
1)		MOVE.W	D1,PDPOSZ		; Output-ring Size
1)		CLR.W	PDPOEC			; initialize our output-ring empty cursor
1)		BSR	PUTOEC			; ..and tell the PDP
1)		BSR	GETOFC			; Get output-ring fill cursor
1)	*	Input-ring parameters
1)		MOVE.W	#$6B,P10ADR		; from o153
1)		BSR	GETPDPL
1)		MOVE.W	D1,PDPIRP		; Input-ring Start-address
1)		MOVE.W	#$6C,P10ADR		; from o154
1)		BSR	GETPDPL
1)		MOVE.W	D1,PDPISZ		; Input-ring size
1)		CLR.W	PDPIFC			; initialize out input-ring fill cursor
1)		BSR	PUTIFC			; ...and tell the PDP
1)		BSR	GETIEC			; Get input-ring empty cursor
1)		CLR.B	HTDWN			; 0 = host is up
****
2)1	*[PK] Use routine RDPDPRP to read parameters when host comes up
2)		BSR.L	RDPDPRP			; Read PDP ring pointers
2)		MOVE.W	#$0180+DEBUG,DAT1	; set "INTERFACE ANSWERED"
2)		MOVE.W	#VERSION,DAT3
2)		BSR.L	WAITDAT			; Tell host we see it as up
2)		CLR.B	HTDWN			; 0 = host is up
**************
1)1	HALFSEX	*				; exit to here...
1)	*					 ...and fall through to DOBKOUT
****
2)1	HALFSEX	EQU	*			; exit to here...
2)	*					 ...and fall through to DOBKOUT
**************
      