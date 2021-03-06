File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

1)1	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
****
2)1	;[CSM] Fix a bug introduced by MCO 10924 at DSCSLP:+10
2)		CSMEDT	03	;Initialize CSM edit 03 - DCA comm gear
2)		CSMEDT	06	;Initialize CSM edit 06 - Raise/lower DTR
2)		CSMEDT	14	;Initialize CSM edit 14 - ESC for PF keys
2)		CSMEDT	15	;Initialize CSM edit 15 - BYE and KJOB changes
2)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
**************
1)7	LDRRMT==:004000		;REMOTE NON-DATASET LINE
1)	XP LDRDSR,LDRDSD+LDRRMT	;REMOTE OR DATA SET LINE (FOR PATCH)
****
2)7	LDRRMT==:004000		;REMOTE NON-DATASET LINE (meaningful to LOGIN only)
2)	XP LDRDSR,LDRDSD+LDRRMT	;REMOTE OR DATA SET LINE (FOR PATCH)
**************
1)7	LDRREM==:002000		;TERMINAL AT REMOTE STATION
1)	LDRSHC==:001000		;SUPPRESS HUNG CHECK -I.E. DON'T FORCE CHAR'S OUT
****
2)7	LDRREM==:002000		;TERMINAL AT REMOTE STATION (controlled by NETSER)
2)	LDRSHC==:001000		;SUPPRESS HUNG CHECK -I.E. DON'T FORCE CHAR'S OUT
**************
1)14	DEPHASE
****
2)14		CSMEDT	03,5	;DCA terminals, part 5 after LDBBKM::
2)	IFN CSM03$,<	;Extra words in the LDB
2)	LDBCSM::0		;Change in SCNSER Mode (UDT when the phone rang)
2)	IFN FTKL10,<	;The 1091 uses the DCA-250 as its communications front end
2)	LDBSMT::0		;Word used by SMTINT to manage PDP-8 lines
2)	>>  ;End of IFN CSM03$
2)	DEPHASE
**************
1)41	IFN FTNET,<
****
2)41		CSMEDT	03,6	;DCA comm gear, part 6 at RECINA:+3
2)	IFN CSM03$&FTKL10,<	;Echo chars properly
2)		MOVE	T2,LDBSMT(U)	;Get SMTINT flags
2)		TRNE	T2,LSRREO##	;Is remote echo on?
2)		 TRZ	T3,200		;Yes, do not echo char again
2)	>  ;End of CSM03$
2)	IFN FTNET,<
**************
1)41	>
1)	RECINB:	MOVE	T2,LDBTIC(U)	;COUNT CHARACTERS LINE HAS INPUT
****
2)41	>  ;End of IFN FTNET
2)	RECINB:	MOVE	T2,LDBTIC(U)	;COUNT CHARACTERS LINE HAS INPUT
**************
1)50	RICR:	LDB	T2,LDPRTC	;RTCOMPATABILITY IN EFFECT?
1)		JUMPN	T2,RECINA	;JUMP IF SO, STORE CHARACTER IN THE INPUT BUFFER
****
2)50	RICR:	PUSHJ	P,CHKLDB	;[CSM] Check for valid U
2)		LDB	T2,LDPRTC	;RTCOMPATABILITY IN EFFECT?
2)		JUMPN	T2,RECINA	;JUMP IF SO, STORE CHARACTER IN THE INPUT BUFFER
**************
1)50		SKIPGE	(U)		;IS THERE ALREADY AN OUTSTANDING COMMAND?
****
File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

2)50		PUSHJ	P,CHKLDB	;[CSM] Check for valid U
2)		SKIPGE	(U)		;IS THE RE ALREADY AN OUTSTANDING COMMAND?
**************
1)50		JUMPN	T3,CPOPJ##	;ALL DONE IF NOT A HARDCOPY TERMINAL
****
2)50		PUSHJ	P,CHKLDB	;[CSM] Check for valid U
2)		JUMPN	T3,CPOPJ##	;ALL DONE IF NOT A HARDCOPY TERMINAL
**************
1)51	;HERE ON RECEIVE INTERRUPT OF CONTROL S (XOFF)
****
2)50	;[CSM] We have seen some KAF stopcodes where SCNSER was looping in this
2)	;[CSM] part of the code with the value 2 in U - illegal LDB pointer.
2)	CHKLDB:	PUSH	P,T1		;[CSM] Save an AC
2)		HRRZ	T1,LINTAB	;[CSM] Get addr of first LDB
2)		CAILE	T1,(U)		;[CSM] Is U lower than this address?
2)		 STOPCD	.+1,DEBUG,LDB	;++ LDB pointer invalid [CSM]
2)		POP	P,T1		;[CSM]
2)		POPJ	P,		;[CSM]
2)51	;HERE ON RECEIVE INTERRUPT OF CONTROL S (XOFF)
**************
1)53	REPIM:
****
2)53	IFN FTTPAG,<	;[CSM] Routine to process XON/XOFF and optionally pass
2)			;[CSM] it to the user's program, added just before REPIM
2)	RICQX:	PUSH	P,T3		;Save ^Q character
2)		PUSHJ	P,RICQ		;Process XON and resume output
2)		JRST    RICQX1
2)	RICSX:	PUSH	P,T3		;Save ^S character
2)		PUSHJ	P,RICS		;Process XOFF and pause output
2)	RICQX1:	POP	P,T3		;Get the XON or XOFF back
2)		TLNN	U,LDLFCS	;Full-character set (caused by IO.LEM) ?
2)		 POPJ	P,		;No, absorb char at interrupt level
2)		JRST	REPIM3		;Yes, pass ^Q and ^S to the program
2)	>  ;[CSM] End of modification to SCNSER.
2)	REPIM:
**************
1)53		JRST	RICQ		;YES, TREAT AS NON PIM XON
1)		CAIE	T3,223		;OR IS IT AN XOFF
1)		CAIN	T3,23		;
1)		JRST	RICS		;TREAT AS NON PIM XOFF IF SO
1)	>;END OF FTTPAG
****
2)53		JRST	RICQX		;[CSM] Yes, go process it
2)		CAIE	T3,223		;OR IS IT AN XOFF
2)		CAIN	T3,23		;
2)		JRST	RICSX		;[CSM] Yes, go process it
2)	>;END OF FTTPAG
**************
1)55	RICU:	PUSHJ	P,FULLCQ	;BREAK ON ALL CHARACTERS?
1)		  JRST RECINA		;YES. STORE THE ^U IN BUFFER
****
2)55	RICU:	PUSHJ	P,CHKLDB	;[CSM] Check for valid LDB
2)		PUSHJ	P,FULLCQ	;BREAK ON ALL CHARACTERS?
2)		  JRST RECINA		;YES. STORE THE ^U IN BUFFER
**************
1)55		SKIPL	LDBDCH(U)	;LINE IDLE?
File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

****
2)55		PUSHJ	P,CHKLDB	;[CSM] Check for valid LDB
2)		SKIPL	LDBDCH(U)	;LINE IDLE?
**************
1)55		MOVSI	T4,LDLUAR	;CONTROL R BIT
****
2)55		PUSHJ	P,CHKLDB	;[CSM] Check for valid LDB
2)		MOVSI	T4,LDLUAR	;CONTROL R BIT
**************
1)55		JRST	RIDEL3		;SPACE OVER PROMPT CHARACTER
****
2)55		PUSHJ	P,CHKLDB	;[CSM] Check for valid LDB
2)		JRST	RIDEL3		;SPACE OVER PROMPT CHARACTER
**************
1)56	DELCHR:	SOSGE	T3,LDBECC(U)	;ANY LEFT TO ECHO?
1)		JRST	[SETZB	T3,LDBECC(U)	;CLEAR ECHO COUNT
****
2)56	DELCHR:	PUSHJ	P,CHKLDB	;[CSM] Check for valid LDB
2)		SOSGE	T3,LDBECC(U)	;ANY LEFT TO ECHO?
2)		JRST	[SETZB	T3,LDBECC(U)	;CLEAR ECHO COUNT
**************
1)58	RIDELB:	PUSHJ	P,FULLCQ	;BREAK ON ALL CHARS, OR FCS?
****
2)58	;[CSM] Jump to RIDELB on backspace when not in APL mode
2)	RIDELB:	PUSHJ	P,FULLCQ	;BREAK ON ALL CHARS, OR FCS?
**************
1)59	RIDEL5:	PUSHJ	P,TTVID
****
2)59	;[CSM] Set P3 nonzero if on a video display terminal
2)	RIDEL5:	PUSHJ	P,TTVID
**************
1)59		TLNN	P1,CHDEL	;RUBOUT?
****
2)59	;[CSM] At this point, backspace or delete has just erased a character
2)	;[CSM] from the input buffer.  Make Control-H act exactly like a RUBOUT
2)	;[CSM] by going backspace-space-backspace.
2)		SKIPE	P3		;[CSM] Video terminal?
2)		 TLO	P1,CHDEL	;[CSM] Yes.  Treat ^H like DEL
2)		TLNN	P1,CHDEL	;RUBOUT?
**************
1)66		MOVEI	T1,TTFCXH	;GET HELLO COMMAND INDEX
****
2)66		CSMEDT	03,7	;DCA comm gear, part 7 at DSRON2:+10 octal
2)	IFN CSM03$,<	;Take notice of when TTY gets attached
2)		MOVE	T1,DATE##	;Get the universal date/time
2)		MOVEM	T1,LDBCSM(U)	;Store it for CSMCPD/TTYMON
2)		MOVE	T1,STATES##	;Get the SCHED bits
2)		TRNE	T1,ST.NRT	;Stand-alone? (SET SCHED 10)
2)		 SKIPA	T1,[TTFCXR]	;.RESTA types time and CONFIG, but no INITIA
2)	>  ;End of IFN CSM03$
2)		MOVEI	T1,TTFCXH	;GET HELLO COMMAND INDEX
**************
1)67		TRNE	T1,ST.NRL	;CHECK THE SCHEDULE COMMAND WORD
1)		POPJ	P,0		;NOT ALLOWED. IGNORE.
1)		PUSH	P,U		;SAVE PTR TO DSCTAB
****
File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

2)67		CSMEDT	03,8	;DCA gear, part 8 at DSRRNG:+1
2)	IFE CSM03$&FTKL10,< ;Talk to the DCA even when SET SCHED 2 = no remote LOGINs
2)		TRNE	T1,ST.NRL	;CHECK THE SCHEDULE COMMAND WORD
2)		POPJ	P,0		;NOT ALLOWED. IGNORE.
2)	>  ;End of IFE CSM03$&FTKL10
2)		PUSH	P,U		;SAVE PTR TO DSCTAB
**************
1)85		LDB	T3,DSTIMP	;GET THE TIME SINCE RING OR CARRIER FAILURE
1)		JUMPN	T3,DSCSL1	;JUMP IF STILL TIMING
****
2)85		HRRZ	U,LINTAB##(T1)	;GET LDB ADDRESS
2)		HRRZ	F,LDBDDB(U)	;GET DDB ADDRESS
2)	;[CSM] Bug in MCO 10924 at DSCSLP:+10
2)		TLNN	T1,DSCFAI	;[CSM] Always do timing if carrier failed.
2)		SKIPN	F		;IS THERE A DDB?
2)		JRST	DSCSL0		;NO, CHECK TIMING
2)		MOVE	T1,DEVMOD(F)	;YES, GET MODE WORD
2)		TRNE	T1,ASSCON!ASSPRG ;THIS TTY ASSIGNED?
2)		JRST	[SETZM T3	;YES, CLEAR TIMER,
2)			 DPB T3,DSTIMP	; STORE NEW VALUE,
2)			 SCNON		; ALLOW INTERRUPTS
2)			 JRST DSCSNY]	; AND DON'T TURN OFF DATASET
2)		MOVE	T1,DSCTAB##(J)	;NO, GET BACK TABLE ENTRY
2)		JRST	DSCSL1		; AND CONTINUE TIMING
2)	DSCSL0:	LDB	T3,DSTIMP	;GET THE TIME SINCE RING OR CARRIER FAILURE
2)		JUMPN	T3,DSCSL1	;JUMP IF STILL TIMING
**************
1)137		JRST	TOPX4		;NO. ERROR CODE 4
1)		LDB	U,LDPDSC	;GET THE TABLE OFFSET INTO U
****
2)136		CSMEDT	06,1	;Raise/lower DTR changes, part 1 at TOPDSE+3
2)	IFE CSM06$,<JRST  TOPX4	>	;NO. ERROR CODE 4
2)	IFN CSM06$,<JRST  TPDSE1>	;No, just raise DTR
2)		LDB	U,LDPDSC	;GET THE TABLE OFFSET INTO U
**************
1)137	TOPDSF:	MOVE	T1,LDBDCH(U)	;GET DEV CHARACTERISTICS
****
2)136	IFN CSM06$,<	;Routines to change the status of the DTR lead
2)	TPDSE1:	SKIPA	T3,[DSTON]	;Dataset transaction ON  = raise DTR
2)	TPDSF1:	 MOVEI	T3,DSTOFF	;Dataset transaction OFF = lower DTR
2)		LDB	U,LDPDSC	;Get the table offset into U
2)		PUSHJ	P,DSCCAL	;Call dataset control
2)		JRST	CPOPJ1##	;And skip return
2)	>  ;End of CSM06$
2)	TOPDSF:	MOVE	T1,LDBDCH(U)	;GET DEV CHARACTERISTICS
**************
1)137		JRST	TOPX4		;NO. CAN'T DO THIS.
1)		PUSHJ	P,LDBCLR	;CLEAR OUT LDB
****
2)136	IFE CSM06$,<JRST  TOPX4	>	;NO. CAN'T DO THIS.
2)	IFN CSM06$,<JRST  TPDSF1>	;No, just drop DTR
2)		PUSHJ	P,LDBCLR	;CLEAR OUT LDB
**************
1)137	TOPDSS:	MOVE	T1,LDBDCH(U)	;CHARACTERISTICS
****
2)136		CSMEDT	15,6	;BYE/KJOB changes, part 6 after TOPDSF
File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

2)	IFN CSM15$,<	;Routine to hang up datasets, line number in U
2)	HANGUP::MOVE	T1,LDBDCH(U)	;Get dev char
2)		TRNN	T1,LDRDSD	;Dataset?
2)		 POPJ	P,		;No, give error return to COMCON
2)		PUSH	P,U		;Yes, preserve U
2)		LDB	U,LDPDSC	;Get index into DSCTAB
2)		MOVEI	T1,1		;Set idle timer to 1 second
2)		DPB	T1,DSTMPL	;Hang up during SCNSEC code
2)		JRST	UPOPJ1##	;Restore U and give skip return
2)	>  ;End of IFN CSM15$
2)	TOPDSS:	MOVE	T1,LDBDCH(U)	;CHARACTERISTICS
**************
1)166	IFN FTMODM,<
1)		MOVE	U,-1(P)		;GET LDB ADDRESS, FOR DS POINTER
1)		MOVSI	T1,LDBCMF
1)		TDNE	T1,LDBDDB(U)
1)		JRST	TTYAT3
1)		MOVE	T1,LDBDCH(U)	;LOOK AT DATA SET BIT
1)		TRNN	T1,LDRREM	;IF NETWORK OR
1)		TRNN	T1,LDRDSD	;IF NOT A DATA SET
1)		JRST	TTYAT3		;NO DSCTAB ENTRY
****
2)165	IFN FTMODM,<	;Reset 60 second timer
2)		MOVE	U,-1(P)		;GET LDB ADDRESS, FOR DS POINTER
2)		MOVSI	T1,LDBCMF	;Check for forced .HELLO
2)		TDNE	T1,LDBDDB(U)
2)		 JRST	TTYAT3		;Don't reset timer, INITIA responding to .HELLO
2)		MOVE	T1,LDBDCH(U)	;LOOK AT DATA SET BIT
2)		TRNN	T1,LDRREM	;If network or
2)		TRNN	T1,LDRDSD	; if not a dataset
2)		 JRST	TTYAT3		;NO DSCTAB ENTRY
**************
1)174		TLNE	T1,CHBRK	;BREAK?
1)		MOVEI	T3,12		;YES--MAKE INTO LINE FEED
1)		TLNE	T1,CHCNC	;IS THIS A CONTROL-C?
****
2)173		CSMEDT	14,2		;ESC for PF keys, part 2 at COMTI1:+6
2)	IFE CSM14$,<	;Convert all break chars at monitor level to linefeed
2)		TLNE	T1,CHBRK	;BREAK?
2)		MOVEI	T3,12		;YES--MAKE INTO LINE FEED
2)	>  ;End of CSM14$
2)	IFN CSM14$,<	;Preserve the identity of ESC (Program-Function keys)
2)		TLNN	T1,CHBRK	;Break?
2)		 JRST	CSM14Z		;Not a break, skip this
2)		CAIE	T3,33		;ESCape?
2)		 MOVEI	T3,12		;No, convert other break chars to linefeed
2)	CSM14Z:	>  ;End of IFN CSM14$
2)		TLNE	T1,CHCNC	;IS THIS A CONTROL-C?
**************
1)176		MOVSI	T1,LDBCMR+LDBCMF;CLEAR COMMAND REQUEST BITS
****
2)175	;[CSM] Bug in MCO 10885 at TTYCMR+21 octal
2)		MOVSI	T1,LDBCMR+LDBCMF;CLEAR COMMAND REQUEST BITS
**************
1)176		TLZ	T1,LDBCMF	;YES, LEAVE LDBCMF ALONE
1)	>
File 1)	DSKU:SCNSER.MAC[702,10]	created: 1035 24-Jan-84
File 2)	DSKU:SCNSER.CSM[702,10]	created: 1735 15-Dec-84

1)		ANDCAB	T1,LDBDDB(U)	;IN BASE WORD OF LDB
1)		MOVE	T1,LDBDCH(U)	;GET THE LINE'S CHARACTERISTIC
****
2)175	;[CSM]	TLZ	T1,LDBCMF	;YES, LEAVE LDBCMF ALONE
2)		TLC	T1,LDBCMF!LDBFDX;Yes, don't clear LDBCMF but do clear LDBFDX
2)	>
2)		ANDCAM	T1,LDBDDB(U)	;IN BASE WORD OF LDB
2)		MOVE	T1,LDBDCH(U)	;GET THE LINE'S CHARACTERISTIC
**************
1)203		DPB	T2,LDPSTB	;SET TTY STOP NN
****
2)202		LDB	T3,LDPSTB	;[CSM] See if SET TTY STOP used to be in effect
2)		SKIPN	T3		;[CSM] If was zero,
2)		 MOVEI	T2,0		;[CSM]  then don't make it nonzero
2)		DPB	T2,LDPSTB	;SET TTY STOP NN
**************
    