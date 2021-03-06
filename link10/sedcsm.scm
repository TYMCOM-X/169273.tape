File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)1	UNIVERSAL SEDSYM - SYMBOLS USED BY THE EDITOR AND ITS TERMINAL TABLES
1)	SUBTTL	  A CHRISTOPHER HALL FECIT
1)	;THE ACTUAL CRT EDITOR PROGRAM FOLLOWS THIS UNIVERSAL FILE
1)	CUSTVR==0	;DEC DEVELOPMENT
1)	HALVER==1	;MAJOR VERSION
1)	HALMVR==2	;MINOR VERSION
1)	HALEVR==161	;EDIT NUMBER
1)	TOPS10==0	;ON IF TOPS10; OFF IF TOPS20
1)	FTKA10==0	;ON IF PROCESSOR IS A KA-10
1)	FTSFD== 1	;SFD SUPPORT DESIRED
1)	SFDLVL==1	;MAXIMUM SUPPORTED SFD LEVEL
1)	NEWTAB==1	;SET UP NEW KEYBOARD ARRANGEMEN T TABLE
1)	SAVETY==0	;SAVE STREAM OF USER COMMANDS
1)	FTDDT== 0	;DEBUG - PUT SYMBOLS IN HIGHSEG
1)	;EDITOR PARAMETERS (CAN BE DIDDLED IF DESIRED)
1)	MAXSIZ==^D1200	;MAXIMUM FILE SIZE (BLOCKS) THAT CAN BE EDITED
1)	PCBSIZ==600	;SIZE OF PICK AND CLOSE BUFFER
1)	NOBYTE==140	;LENGTH OF LOOK-AHEAD WHEN SEARCHING FOR NULLS
1)	SQZVAL==100	;NUMBER OF DELETE COMMANDS BETWEEN SQUEEZES
1)	XBFNUM==10	;NUMBER OF EXECUTE BUFFERS
1)	XBFSIZ==14	;SIZE OF EACH EXECUTE BUFFER
1)	TYPSIZ==40	;SIZE OF TYPE BUFFER
1)	IFE TOPS10,<
1)	BUFSTT==20000	;START OF FILE BUFFER UNDER TOPS20
1)	BUFBLK==20	;BLOCK OF START OF FILE BUFFER
1)	>
1)	;AC DEFINITIONS
1)	T0=0		;USED HERE AND THERE ONLY WHEN ABSOLUTELY NEEDED
1)	T1=1		;THE USUAL SCRATCH ACS
1)	T2=2
1)	T3=3
1)	T4=4
1)	F=5		;FLAGS IN BOTH HALVES
1)	TM=7		;USER'S TERMINAL INDEX
1)	TY=10		;POINTER INTO TYPE BUFFER
1)	SL=11		;OFFSET FROM LEFT MARGIN (SET BY SLIDES)
1)	DO=12		;COMMAND THAT USER TYPED
1)	PT=13		;HOLDS VARIOUS POINTERS
1)	EN=14		;POINTER TO END OF BUFFER IN USE
1)	RW=15		;ROW CURSOR IS ON
1)	CM=16		;COLUMN CURSOR IS ON
1)	P=17		;STACK POINTER
1)	;FLAGS IN LH OF F
1)	FLG==1		;TEMPORARY FLAG - EACH ROUTINE MAY SET, AND MUST CLEAR
1)	ENT==2		;ENTER HAS BEEN TYPED
1)	XPL==4		;LINPTR IS INVALID
1)	XPC==10		;CHRPTR IS INVALID
1)	XPB==20		;BOTPTR IS INVALID
1)	FNC==40		;FENCE IS ON THE SCREEN
1)	CCH==100	;NEXT CHARACTER SHOULD BE A CONTROL CHARACTER
1)	INT==200	;AT SOFTWARE INTERRUPT LEVEL (TOPS-20)
1)	RDO==400	;FILE IS READ-ONLY - NO MODIFICATIONS ALLOWED
1)	CHG==1000	;FILE HAS BEEN MODIFIED
1)	SCN==2000	;IN SCAN MODE
1)	WRH==4000	;FILL HOLE (FROM MAKSPC) WITH CONTENTS OF PICK BUFFER
1)	LFF==10000	;LOOKING FOR A LINEFEED (DISPLAY ROUTINES)
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)	ERF==20000	;JUST HAD AN ERROR
1)	INS==40000	;IF POINTING BEYOND LINE OR FILE, INSERT LINES OR SPACES
1)	SMF==100000	;FILE AND ALTERNATE FILE ARE THE SAME
1)	FBL==200000	;BOTTOM LINE OF SCREEN IS FRAGGED - REWRITE
1)	PCM==400000	;MARK HAS BEEN MADE (FOR PICK OR CLOSE-LINES)
1)	;FLAGS IN RH OF F
1)	POV==1		;PICK BUFFER HAS OVERFLOWED
1)	COV==2		;CLOSE BUFFER HAS OVERFLOWED
1)	CWT==4		;CHARACTER ALREADY TYPED AND WAITING
1)	CMV==10		;DOING CURSOR MOVEMENT DURING ENTER
1)	XCT==20		;WANT TO TAKE COMMANDS FROM THE EXECUTE BUFFER
1)	XSV==40		;WANT TO SAVE COMMANDS IN THE EXECUTE BUFFER
1)	CRE==100	;WANT TO CREATE A FILE, IF NOT FOUND (SETFIL)
1)	IMD==200	;INSERT MODE TOGGLE FLAG
1)	WTB==400	;ON IF WANT WORDWISE TABS
1)	NRC==1000	;CRLF AT BOTTOM OF SCREEN WILL CAUSE A ROLL
1)	NLC==2000	;MAKE SEARCHES CASE-INDEPENDENT
1)	GFL==4000	;FILE SET UP - CHEERY MESSAGE IS NOT IN THE BUFFER
1)	DTB==10000	;DISPLAY TABS AS PROTECTED I'S AND SPACES
1)	NHP==20000	;NO HELP IS WANTED - EXTRA ENTERS ARE IGNORED
1)	NCR==40000	;CR DURING INSERT MODE DOES NOT INSERT CRLF
1)	RST==100000	;DO NOT RESET NOMINAL AFTER EACH COMMAND EXECUTION
1)	;FLAGS DEFINED IN TERMINAL BLOCK (AND SET UP IN LH OF TM)
1)	BEP==200	;BEEP ON INSERT MODE, DON'T PUT UP MESSAGE
1)	NPG==400	;DON'T SET TTY NO PAGE (THUS ^S ^Q ARE XOFF AND XON)
1)	SLW==1000	;TERMINAL IS SLOW - WRITE ERRORS ON BOTTOM OF SCREEN
1)	NLP==2000	;SEND NULLS AFTER CURSOR POSITIONING
1)	NEL==4000	;RESERVE LAST LINE FOR ARGS ETC; DON'T REWRITE FROM FILE
1)	NRD==10000	;NO SEQUENCE FOR ROLL DOWN; REWRITE ENTIRE SCREEN
1)	NRU==20000	;LIKEWISE NONE FOR ROLL UP (LINEFEED WON'T DO IT)
1)	MRK==40000	;NO PROTECTED FIELDS; THUS, MARK NEEDED ON ENTER
1)	WRP==100000	;LONG LINES WRAP AROUND TO NEXT LINE OF SCREEN
1)	TBS==200000	;HARDWARE TABS EXIST (AND ARE PRE-SET)
1)	LSD==400000	;LINEFEED AND CURSOR DOWN ARE THE SAME CHARACTER
1)	;DEFINITIONS OF WORDS IN TERMINAL OUTPUT TABLE
1)	CUP==0		;CURSOR UP
1)	CDN==1		;DOWN
1)	CRG==2		;RIGHT
1)	CLF==3		;LEFT
1)	CHM==4		;HOME
1)	CPG==5		;CLEAR TO END OF PAGE
1)	CLN==6		;CLEAR TO END OF LINE
1)	RUP==7		;ROLL SCREEN UP AND CLEAR NEW LINE
1)	RLD==10		;ROLL SCREEN DOWN, DITTO
1)	PON==11		;PROTECT ON
1)	POF==12		;PROTECT OFF
1)	ILN==13		;INSERT A BLANK LINE
1)	ISP==14		;INSERT A SPACE
1)	DLN==15		;DELETE A LINE
1)	DSP==16		;DELETE A SPACE
1)	MVB==17		;MOVE TO BOTTOM OF PAGE
1)	HCP==20		;HOME AND CLEAR PAGE
1)	PSC==21		;SEQUENCE TO START CURSOR POSITIONING
1)	PSL==22		;TYPE OF POSITIONING ALGORITHM TO USE
1)	LPP==23		;LINES PER PAGE
1)	CPL==24		;CHARACTERS PER LINE
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)	TCH==25		;TERMINAL CHARACTERISTICS FLAGS
1)	RTE==26		;ROUTINE TO CALL WHEN EDITOR IS STARTED
1)	RTX==27		;ROUTINE TO CALL WHEN EDITOR IS EXITED
1)	NUL==30		;NUMBER OF NULLS TO OUTPUT,,NULL CHARACTER
1)	ITB==31		;ADDRESS OF INPUT CHARACTER TABLE
1)	MAR==32		;MARK TO PUT UP ON ENTER (IF NO PROTECTED FIELDS)
1)	PRGEND
1)	TITLE	SED - SCREEN EDITOR
1)	SUBTTL	A CHRISTOPHER HALL FECIT
1)		SEARCH	SEDSYM
1)	IF1,<
1)	IFN TOPS10,<PRINTX ASSEMBLING SED FOR TOPS10>
1)	IFE TOPS10,<PRINTX ASSEMBLING SED FOR TOPS20>
1)	IFE NEWTAB,<PRINTX USING OLD COMMAND TABLES>
1)	IFN FTDDT,<PRINTX DDT SWITCH IS ON>
1)	>
1)	;TREATMENT BY TOPS10:
****
2)1	TITLE	SED - Screen Editor
2)	SUBTTL	A CHRISTOPHER HALL FECIT
2)		SEARCH	SEDSYM
2)	;[CSM] Minor modifications made by Joe Smith @ Colorado School of Mines
2)	;To create SED.EXE:
2)	;	.LOAD SETSYM.MAC/COMPIL,SEDTTY.MAC,SED.MAC   ;SED.REL must be last
2)		.DIRECTIVE FLBLST	;[CSM] List first line binary of ASCIZ
2)	;TREATMENT BY TOPS10:
**************
1)1	;	CODES >= 040 ECHOED NORMALLY, GO TO ROUTINE ALPNUM
1)	;	<H> <J> <\> <]> <UP> <_> ECHOED NORMALLY
****
2)1	;	CODES .GT. 040 ECHOED NORMALLY, GO TO ROUTINE ALPNUM
2)	;	<H> <J> <\> <]> <UP> <_> ECHOED NORMALLY
**************
1)1		LOC	137
1)		BYTE	(3)CUSTVR (9)HALVER (6)HALMVR (18)HALEVR
1)		LOC	134
****
2)1		LOC	134
**************
1)1	IFE TOPS10,<SEARCH MONSYM>
1)		EXTERN	TERMNL
1)		INTERN	CPOPJ,PUTSTG,PUTNUM,PUTSQ1,PUTTYP,TRMNAM,TYPBUF
****
2)1	IFN TOPS10,<SEARCH UUOSYM>
2)	IFE TOPS10,<SEARCH MONSYM>
2)		EXTERN	TERMNL		;Address of dispatch table
2)		INTERN	CPOPJ,PUTSTG,PUTNUM,PUTSQ1,PUTTYP,TRMNAM,TYPBUF
**************
1)1	START:	JFCL			;(TO ACCOUNT FOR CCL ENTRY)
****
2)1	;         Table of Contents for SED - Full screen editor
2)	;
2)	;
2)	;			   Section			      Page
2)	;
2)	;    1. Startup  . . . . . . . . . . . . . . . . . . . . . . .   2
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)	;    2. Main input loop  . . . . . . . . . . . . . . . . . . .   3
2)	;    3. Decode ESCape-sequence sub-table . . . . . . . . . . .   4
2)	;    4. ALPNUM - Store non-control character on the screen . .   5
2)	;    5. Process cursor movements . . . . . . . . . . . . . . .   6
2)	;    6. Search for string  . . . . . . . . . . . . . . . . . .   7
2)	;    7. Set INSERT mode  . . . . . . . . . . . . . . . . . . .   8
2)	;    8. PERCEN - Goto position in file . . . . . . . . . . . .   9
2)	;    9. EXIT and ABORT commands  . . . . . . . . . . . . . . .  10
2)	;   10. Insert line  . . . . . . . . . . . . . . . . . . . . .  11
2)	;   11. Delete lines . . . . . . . . . . . . . . . . . . . . .  12
2)	;   12. Copy lines into buffer . . . . . . . . . . . . . . . .  13
2)	;   13. Insert spaces  . . . . . . . . . . . . . . . . . . . .  14
2)	;   14. Delete characters  . . . . . . . . . . . . . . . . . .  15
2)	;   15. PICK command . . . . . . . . . . . . . . . . . . . . .  16
2)	;   16. PUT command  . . . . . . . . . . . . . . . . . . . . .  17
2)	;   17. SLIDE left and right . . . . . . . . . . . . . . . . .  18
2)	;   18. EXECUTE command  . . . . . . . . . . . . . . . . . . .  19
2)	;   19. Select input file  . . . . . . . . . . . . . . . . . .  20
2)	;   20. The ENTER command  . . . . . . . . . . . . . . . . . .  21
2)	;   21. Roll forward/backward pages  . . . . . . . . . . . . .  22
2)	;   22. RESET command  . . . . . . . . . . . . . . . . . . . .  23
2)	;   23. Change switch settings . . . . . . . . . . . . . . . .  24
2)	;   24. RECALL command . . . . . . . . . . . . . . . . . . . .  25
2)	;   25. REAL TAB, MARK, and COUNTER  . . . . . . . . . . . . .  26
2)	;   26. Utility subroutines  . . . . . . . . . . . . . . . . .  27
2)	;   27. Terminal output routines . . . . . . . . . . . . . . .  28
2)	;   28. HISEG data . . . . . . . . . . . . . . . . . . . . . .  29
2)	;   29. Command dispatch table . . . . . . . . . . . . . . . .  30
2)	;   30. LOWSEG data  . . . . . . . . . . . . . . . . . . . . .  31
2)	;   31. End of LOWSEG, start of text buffer  . . . . . . . . .  32
2)2	SUBTTL	Startup
2)	START:	JFCL			;(TO ACCOUNT FOR CCL ENTRY)
**************
1)1	IFN FTSFD,<
1)		MOVE	T1,[SFDLVL+4,,DEFPTH]
1)		PATH.	T1,		;READ STARTING PATH
1)		  JFCL
1)		MOVEI	T1,SFDLVL
1)		MOVE	T2,DEFPTH+2(T1)	;SET UP AS DEFAULT FILE PATH
1)		MOVEM	T2,FILPTH+2(T1)
1)		MOVEM	T2,OLDPTH+2(T1)
1)		SOJGE	T1,.-3
1)	>
1)		GETPPN	T1,		;GET USER'S PPN
****
2)2		GETPPN	T1,		;GET USER'S PPN
**************
1)1		  HALT
1)		MOVEM	T1,BRKADR+1	;SET IT UP IN VARIOUS LOCATIONS
****
2)2		  HALT	.
2)		MOVEM	T1,BRKADR+1	;SET IT UP IN VARIOUS LOCATIONS
**************
1)1		  HALT
1)		MOVE	T1,[XWD 3,BRKADR]
1)		TRMOP.	T1,		;SET UP TO BREAK ON ALL CHARACTERS
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)		  HALT
1)	>
****
2)2		  HALT	.
2)		MOVE	T1,[XWD 3,BRKADR]
2)		TRMOP.	T1,		;SET UP TO BREAK ON ALL CHARACTERS
2)		  HALT	.
2)	>
**************
1)1		HRR	TM,TERMNL	;SET UP THE RIGHT TYPE OF TERMINAL
1)		PUSHJ	P,@RTE(TM)	;CALL USER'S ENTRY ROUTINE
1)		HRLZ	T1,TCH(TM)	;GET TERMINAL-SPECIFIC FLAGS
****
2)2		HRR	TM,TERMNL	;Set pointer to default terminal characteristics
2)		MOVE	T1,TERMNL	;[CSM] Get flags or instruction
2)		TLNE	T1,777000	;[CSM] Is TERMNL the start of a subroutine?
2)		 PUSHJ	P,TERMNL	;[CSM] Yes, go ask monitor for terminal type
2)		PUSHJ	P,@RTE(TM)	;CALL USER'S ENTRY ROUTINE
2)		MOVEI	T1,^D500	;[CSM] Pause 1/2 second
2)	IFN TOPS10,<	HIBER	T1,	;[CSM] For .SET WATCH FILES messages
2)			  JFCL	>	;[CSM]
2)	IFE TOPS10,<	DISMS	>	;[CSM]
2)		HRLZ	T1,TCH(TM)	;GET TERMINAL-SPECIFIC FLAGS
**************
1)1		  HALT
1)		MOVEI	T1,REEERR	;SET UP RE-ENTRY ADDRESS
****
2)2		  HALT	.
2)		MOVEI	T1,REEERR	;SET UP RE-ENTRY ADDRESS
**************
1)1	;NOW ACCEPT CHARACTERS AND DO THINGS
****
2)3	SUBTTL	Main input loop
2)	;NOW ACCEPT CHARACTERS AND DO THINGS
**************
1)1		MOVE	T1,[XWD 2,CHRIN]
1)		TRMOP.	T1,		;READ CHARACTER FROM TERMINAL
1)		  HALT
1)	>
****
2)3		INCHRW	T1		;[CSM] Read character from terminal
2)	>
**************
1)1	;SUBROUTINE TO REFERENCE A TERMINAL'S SUBTABLES
1)	SUBTAB:	MOVE	T4,T1		;SET UP ADDRESS OF SUBTABLE
1)	IFN TOPS10,<
1)		MOVE	T1,[XWD 2,CHRIN]
1)		TRMOP.	T1,		;GET NEXT CHARACTER FROM TERMINAL
1)		  HALT
1)	>
****
2)4	SUBTTL	Decode ESCape-sequence sub-table
2)	;SUBROUTINE TO REFERENCE A TERMINAL'S SUBTABLES
2)	SUBTAB:	MOVE	T4,T1		;SET UP ADDRESS OF SUBTABLE
2)	SUBTB1:
2)	IFN TOPS10,<
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)		INCHRW	T1		;[CSM] Get next character from terminal
2)	>
**************
1)1		HRLI	T4,-^D20
1)		JRST	SUBTAB+1	;READ ANOTHER CHARACTER FROM TERMINAL
1)	;SUBROUTINE TO RESTORE NOMINALS, IF RST FLAG IS SET
****
2)4		HRLI	T4,-^D128	;[CSM] Set length to max (terminated by zero)
2)		JRST	SUBTB1		;READ ANOTHER CHARACTER FROM TERMINAL
2)	;SUBROUTINE TO RESTORE NOMINALS, IF RST FLAG IS SET
**************
1)1	;**********************************************************************
1)	;HERE IF A NON-CONTROL CHARACTER WAS TYPED - PUT IT IN FILE OR
****
2)5	SUBTTL	ALPNUM - Store non-control character on the screen
2)	;HERE IF A NON-CONTROL CHARACTER WAS TYPED - PUT IT IN FILE OR
**************
1)1	;**********************************************************************
1)	;DISPATCH AREAS FOR CURSOR CONTROL CHARACTERS
****
2)6	SUBTTL	Process cursor movements
2)	;DISPATCH AREAS FOR CURSOR CONTROL CHARACTERS
**************
1)2	;**********************************************************************
1)	;HERE ON LINEFEED, WHICH ERASES THE LINE THAT THE CURSOR GOES TO
****
2)6	;HERE ON LINEFEED, WHICH ERASES THE LINE THAT THE CURSOR GOES TO
**************
1)2	;**********************************************************************
1)	;HERE FOR THE SEARCH BACKWARD COMMAND
****
2)7	SUBTTL	Search for string
2)	;HERE FOR THE SEARCH BACKWARD COMMAND
**************
1)2		  HALT
1)		CAIN	T1,177		;IS IT A RUBOUT?
****
2)7		  HALT	.
2)		CAIN	T1,177		;IS IT A RUBOUT?
**************
1)2		MOVEI	T1,[ASCIZ /SEARCH FOR: /]
1)		PUSHJ	P,PUTSTG	;DISPLAY SEARCH KEY
****
2)7		MOVEI	T1,[ASCIZ /Search for: /]
2)		PUSHJ	P,PUTSTG	;DISPLAY SEARCH KEY
**************
1)2	;**********************************************************************
1)	;HERE ON ENTER-CONTROL-CHARACTER COMMAND. SET FLAG SO IF NEXT
****
2)8	SUBTTL	Set INSERT mode
2)	;HERE ON ENTER-CONTROL-CHARACTER COMMAND. SET FLAG SO IF NEXT
**************
1)2	;**********************************************************************
1)	;HERE ON INSERT MODE TOGGLE COMMAND. SET FLAG SO CHARACTERS TYPED
****
2)8	;HERE ON INSERT MODE TOGGLE COMMAND. SET FLAG SO CHARACTERS TYPED
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

**************
1)2		PUSHJ	P,POSCUR	;REPOSITION THE CURSOR
****
2)8		TLNE	TM,NEL		;[CSM] No end line from file?
2)		 PUSHJ	P,CBOTOM	;[CSM] Yes, clear it from screen
2)		PUSHJ	P,POSCUR	;REPOSITION THE CURSOR
**************
1)2	;**********************************************************************
1)	;HERE TO GO TO SOME GIVEN PERCENT OF THE FILE
****
2)9	SUBTTL	PERCEN - Goto position in file
2)	;HERE TO GO TO SOME GIVEN PERCENT OF THE FILE
**************
1)2	;**********************************************************************
1)	;HERE TO HANDLE AN EXIT COMMAND
1)	EXIPAR:	TLO	F,FLG		;SET FLAG TO DO A RUN ON COMPIL
1)	EEXIT:	PUSHJ	P,CLRALL	;HOME AND CLEAR SCREEN
1)		PUSHJ	P,PUTTYP	;OUTPUT IT NOW
1)	IFN TOPS10,<
1)		RELEAS	1,		;GET RID OF THE TERMINAL
1)	>
1)	IFN SAVETY,<
1)		OPEN	5,DEBBLK	;SAVE OFF THE DEBUG COMMAND FILE
1)		  HALT
1)		ENTER	5,DEBFIL
1)		  HALT
1)		OUTPUT	5,DEBCCL
****
2)10	SUBTTL	EXIT and ABORT commands
2)	EXIPAR:	TLO	F,FLG		;SET FLAG TO DO A RUN ON COMPIL
2)	EEXIT:
2)	IFN TOPS10,<
2)		CLOSE	1,		;[CSM] Close to clear PIM mode
2)		OPEN	1,[EXP 1,'TTY   ',0] ;[CSM] Open in ASCII LINE mode
2)		  HALT	.		;[CSM] So that typeahead is echoed properly
2)	;[CSM] There is a problem with SET TTY DEFER in the 7.01 monitor.  Only the 1st
2)	;[CSM] character of typeahead gets echoed, the rest of the line doesn't appear
2)	;[CSM] until the job hits monitor level, which can take minutes on ENTER ^Z.
2)		SKPINL			;[CSM] This TTCALL tells monitor to allow
2)		 JFCL			;[CSM]  the deferred echo of a full line
2)	>
2)		PUSHJ	P,CLRALL	;HOME AND CLEAR SCREEN
2)		PUSHJ	P,PUTTYP	;OUTPUT IT NOW
2)	IFN SAVETY,<
2)		OPEN	5,DEBBLK	;SAVE OFF THE DEBUG COMMAND FILE
2)		  HALT	.
2)		ENTER	5,DEBFIL
2)		  HALT	.
2)		OUTPUT	5,DEBCCL
**************
1)2		HRROI	T2,STTFIL	;SET UP nnnSTT.TMP FILE FOR OUTPUT
****
2)10	IFN TOPS10,<	;[CSM] Try writing TMP:SED before using DSK:000SST.TMP
2)		MOVN	T1,TY		;[CSM] Get negative word count
2)		HRLM	T1,STTCCL	;[CSM] Store in IOWD
2)		MOVE	T1,[3,,STTCCL-1];[CSM] Code to write file
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)		TMPCOR	T1,		;[CSM] Write TMP:SED
2)		  SKIPA			;[CSM] Can't, write to disk
2)		 JRST	EEXIT1		;[CSM] Did it
2)	>
2)		HRROI	T2,STTFIL	;SET UP nnnSTT.TMP FILE FOR OUTPUT
**************
1)2		MOVNS	TY		;SAVE NEGATIVE WORDCOUNT IN COMMAND BLOCK
1)		HRLM	TY,STTCCL
1)		OUTPUT	5,STTCCL	;OUTPUT THE STATUS STUFF
1)		RELEAS	5,		;MAKE IT SOUP (YET)
1)		MOVE	TY,[POINT 7,TYPBUF]
1)	ABORT1:	MOVEI	T1,1		;SET UP TTY PAGE
****
2)10		OUTPUT	5,STTCCL	;OUTPUT THE STATUS STUFF
2)		RELEAS	5,		;MAKE IT SOUP (YET)
2)	EEXIT1:	MOVE	TY,[POINT 7,TYPBUF]
2)	ABORT1:	MOVEI	T1,1		;SET UP TTY PAGE
**************
1)2		  HALT
1)	IFN FTSFD,<
1)		SOS	DEFPTH		;SET USER'S STARTING PATH BACK UP
1)		MOVE	T1,[SFDLVL+4,,DEFPTH]
1)		PATH.	T1,
1)		  JFCL
1)		SETOM	DEFPTH
1)	>
1)		MOVE	T1,[5,,SAVEAC]	;SAVE ACS IN CASE OF REENTER
****
2)10		  HALT	.
2)		MOVE	T1,[5,,SAVEAC]	;SAVE ACS IN CASE OF REENTER
**************
1)2		TLZE	F,FLG		;WANT TO EXIT AND GO?
****
2)10		RELEAS	1,		;[CSM] Finished with TTY after RTX routine
2)		TLZE	F,FLG		;WANT TO EXIT AND GO?
**************
1)2		JRST	START		;IS HE CONTINUES, CONTINUE
1)	EXITGO:	MOVE	T1,[1,,GOBLK]	;CODE TO RUN COMPIL
1)		RUN	T1,		;LET COMPIL TAKE OVER
1)		JRST	START		;IF IT DOESN'T WORK, ACT INNOCENT
1)	>
****
2)10		JRST	REEERR		;[CSM] Treat ".CONTINUE" as ".REENTER"
2)	EXITGO:	MOVE	T1,[1,,GOBLK]	;CODE TO RUN COMPIL
2)		RUN	T1,		;LET COMPIL TAKE OVER
2)		  HALT	START		;IF IT DOESN'T WORK, ACT INNOCENT
2)	>
**************
1)2	ABORT:	MOVEI	T4,FILSPC	;TELL USER THAT FILE IS NOT CHANGED
1)		PUSHJ	P,SAVMGN
****
2)10	ABORT:
2)	IFN TOPS10,<
2)		CLOSE	1,		;[CSM] Close to clear PIM mode
2)		OPEN	1,[EXP 1,'TTY   ',0] ;[CSM] Open in ASCII LINE mode
2)		  HALT	.		;[CSM] So thaeahead is echoed properly
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)	>
2)		MOVEI	T4,FILSPC	;TELL USER THAT FILE IS NOT CHANGED
2)		PUSHJ	P,SAVMGN
**************
1)2	;**********************************************************************
1)	;HERE TO ADD BLANK LINES TO THE BUFFER
****
2)11	SUBTTL	Insert line
2)	;HERE TO ADD BLANK LINES TO THE BUFFER
**************
1)2		JUMPE	CM,OPNLD1+1	;WITHIN A LINE?
1)		PUSHJ	P,CLRLNR	;YES - CLEAR TO END OF LINE
****
2)11		TLNN	T3,774000	;[CSM] ASCII string?
2)		TLNN	T3,IDARGS	;[CSM] No, does subroutine take INS/DEL args?
2)		 JRST	OPNDLA		;[CSM] No, do it one line at a time
2)		MOVE	T3,RW		;[CSM] Where to do it
2)		JUMPE	CM,OPNDLB	;[CSM] At beginning of line?
2)		PUSHJ	P,CLRLNR	;[CSM] No, clear partial line
2)		MOVEI	T1,CR		;[CSM] Some terminals don't work right
2)		IDPB	T1,TY		;[CSM]  unless in column 1
2)		PUSHJ	P,CDOWN		;[CSM] Go to where new line belongs
2)		MOVEI	T3,1(RW)	;[CSM] Start scrolling below the current line
2)	OPNDLB:	PJRST	@ILN(TM)	;[CSM] Insert T4 lines at row T3
2)	OPNDLA:	JUMPE	CM,OPNLD1+1	;WITHIN A LINE?
2)		PUSHJ	P,CLRLNR	;YES - CLEAR TO END OF LINE
**************
1)2	;HERE TO REMOVE LINES FROM THE BUFFER (NULL THEM OVER, REALLY)
****
2)12	SUBTTL	Delete lines
2)	;HERE TO REMOVE LINES FROM THE BUFFER (NULL THEM OVER, REALLY)
**************
1)2		TLNE	F,FNC		;IS FENCE ON SCREEN?
1)		PUSHJ	P,CBOTOM	;YES - ERASE IT
****
2)12	;[CSM]	TLNE	F,FNC		;IS FENCE ON SCREEN?
2)		TLNE	F,FNC!FBL	;[CSM] Is FENCE or INSERT-MODE on screen?
2)		PUSHJ	P,CBOTOM	;YES - ERASE IT
**************
1)2		MOVE	T1,T3		;GET CODE TO CLOSE LINES
1)		PUSHJ	P,PUTSEQ	;CLOSE A LINE
1)		SOJG	T2,.-2		;LOOP THROUGH ALL LINES
1)		SKIPN	ADDLSP
1)		JUMPE	CM,.+3		;JUMP IF AT START OF A LINE
****
2)12		TLNN	T3,771000	;[CSM] String of ASCII?
2)		TLNN	T3,IDARGS	;[CSM] No, does subroutine take INS/DEL args?
2)		 JRST	CLSLN5		;[CSM] No, do one line at a time
2)		MOVEI	T3,(RW)		;[CSM] Where to do it
2)		MOVE	T4,T2		;[CSM] How many to do
2)		PUSHJ	P,@DLN(TM)	;[CSM] Delete T4 lines starting at row T3
2)		JRST	CLSLN6		;[CSM] Continue
2)	CLSLN5:	MOVE	T1,T3		;GET CODE TO CLOSE LINES
2)		PUSHJ	P,PUTSEQ	;CLOSE A LINE
2)		SOJG	T2,CLSLN5	;LOOP THROUGH ALL LINES
2)	CLSLN6:	SKIPN	ADDLSP
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)		JUMPE	CM,.+3		;JUMP IF AT START OF A LINE
**************
1)2	;SUBROUTINE TO FIND (T4) REAL CHARACTERS (NOT NULLS) AT (PT), WHICH IS FRAGGED
****
2)13	SUBTTL	Copy lines into buffer
2)	;SUBROUTINE TO FIND (T4) REAL CHARACTERS (NOT NULLS) AT (PT), WHICH IS FRAGGED
**************
1)3	;**********************************************************************
1)	;HERE TO ADD SPACES TO THE BUFFER
****
2)14	SUBTTL	Insert spaces
2)	;HERE TO ADD SPACES TO THE BUFFER
**************
1)3		MOVE	T1,T3		;GET CODE TO DO THE OPEN
1)		PUSHJ	P,PUTSEQ	;OPEN ONE SPACE
****
2)14		TLNN	T3,771000	;[CSM] String of ASCII?
2)		TLNN	T3,IDARGS	;[CSM] No, does subroutine take INS/DEL args?
2)		 JRST	OPNSI3		;[CSM] No, do one space at a time
2)		PUSHJ	P,(T3)		;[CSM] INS/DEL T4 spaces at current position
2)		PJRST	PUTTYP		;[CSM] Continue
2)	OPNSI3:	MOVE	T1,T3		;GET CODE TO DO THE OPEN
2)		PUSHJ	P,PUTSEQ	;OPEN ONE SPACE
**************
1)3	;HERE TO REMOVE THE PREVIOUS CHARACTER FROM THE BUFFER
****
2)15	SUBTTL	Delete characters
2)	;HERE TO REMOVE THE PREVIOUS CHARACTER FROM THE BUFFER
**************
1)3	;**********************************************************************
1)	;HERE TO TAKE LINES FROM THE BUFFER AND PUT THEM IN THE PICK BUFFER
****
2)16	SUBTTL	PICK command
2)	;HERE TO TAKE LINES FROM THE BUFFER AND PUT THEM IN THE PICK BUFFER
**************
1)3	;**********************************************************************
1)	;HERE TO PUT THE CONTENTS OF THE PICK BUFFER INTO THE BUFFER
****
2)17	SUBTTL	PUT command
2)	;HERE TO PUT THE CONTENTS OF THE PICK BUFFER INTO THE BUFFER
**************
1)3	;**********************************************************************
1)	;HERE TO SLIDE THE VIEWING WINDOW TO THE LEFT
****
2)18	SUBTTL	SLIDE left and right
2)	;HERE TO SLIDE THE VIEWING WINDOW TO THE LEFT
**************
1)3	;**********************************************************************
1)	;HERE TO OPEN OR CLOSE THE EXECUTE BUFFER OR SET UP TO ITERATE IT A FEW TIMES
****
2)19	SUBTTL	EXECUTE command
2)	;HERE TO OPEN OR CLOSE THE EXECUTE BUFFER OR SET UP TO ITERATE IT A FEW TIMES
**************
1)3		  HALT
1)		CAIN	T1,177		;IS IT A RUBOUT?
****
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)19		  HALT	.
2)		CAIN	T1,177		;IS IT A RUBOUT?
**************
1)3	;**********************************************************************
1)	;HERE TO SET UP A NEW FILE FOR EDITING
****
2)20	SUBTTL	Select input file
2)	;HERE TO SET UP A NEW FILE FOR EDITING
**************
1)4	IFN FTSFD,<
1)		MOVE	T1,[SFDLVL+4,,FILPTH]
1)		PATH.	T1,
1)		  JRST	SETERR
1)	>
1)		OPEN	2,FILBLK	;OPEN FILE FOR EDITING
1)		  JRST	SETERR
1)		LOOKUP	2,FILFIL	;SET UP FILE FOR READING AND WRITING
1)		  JRST	SETERR
1)		LDB	T1,[POINT 9,FILFIL+4,8] ;GET PROTECTION CODE
1)		HRLI	T1,5		;CHECK READ ACCESS
1)		MOVEM	T1,REDACC
1)		HRLI	T1,2		;SET FOR CHECKING WRITE ACCESS
1)		MOVEM	T1,WRTACC
1)		MOVE	T2,USRPPN	;GET RUNNER'S PPN
1)		SKIPN	T1,FILPPN	;GET PPN OF FILE - ANY?
1)		MOVE	T1,T2		;NO - USE RUNNER'S PPN
1)	IFE FTKA10,<
1)		DMOVEM	T1,REDACC+1	;SAVE PPNS IN ACCESS BLOCKS
1)		DMOVEM	T1,WRTACC+1
1)	>
1)	IFN FTKA10,<
1)		MOVEM	T1,REDACC+1	;SAVE PPNS IN ACCESS BLOCKS
1)		MOVEM	T2,REDACC+2
1)		MOVEM	T1,WRTACC+1
1)		MOVEM	T2,WRTACC+2
1)	>
1)		MOVEI	T1,REDACC	;SEE IF THE FILE CAN BE READ
1)		CHKACC	T1,
1)		  JFCL			;(NOT IMPLEMENTED)
1)		JUMPL	T1,SETERR	;NO READ ACCESS - ERROR
1)		MOVEI	T1,WRTACC	;SEE IF FILE CAN BE WRITTEN
1)		CHKACC	T1,
1)		  JFCL
1)		JUMPGE	T1,.+2		;CAN FILE BE WRITTEN?
1)		TLOA	F,RDO		;NO - READ ONLY
1)		TLZ	F,RDO		;YES - ENABLE FOR WRITING
1)		SETOM	INJFN		;SAY FILE HAS BEEN SET UP
****
2)20		OPEN	2,FILBLK	;OPEN FILE FOR EDITING
2)		  JRST	SETERR
2)	IFN FTSFD,<	;[CSM] Do not change default path, just point to correct one
2)		SETZM	FILFIL+1	;[CSM] Monitor does not like pointer to 0 PPN
2)		MOVEI	T1,FILPTH	;[CSM] Get pointer to path
2)		SKIPE	T2,FILPPN	;[CSM] If default, leave alone
2)		 MOVEM	T1,FILFIL+1	;[CSM] Store in PPN word
2)	>
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)		LOOKUP	2,FILFIL	;SET UP FILE FOR READING AND WRITING
2)		  JRST	SETERR
2)	IFN FTSFD,<
2)		MOVEI	T1,FILPTH	;[CSM] Make sure pointer to path is there
2)		MOVEM	T1,FILFIL+1	;[CSM]  in case SKIPE T2,FILPPN skipped
2)		MOVEI	T1,2		;[CSM] Get channel number
2)		MOVEM	T1,FILPTH	;[CSM] Put in PATH block
2)		MOVE	T1,[SFDLVL+4,,FILPTH]
2)		PATH.	T1,		;[CSM] Find out where the file really is
2)		  JRST	SETERR
2)		MOVE	T1,FILPTH+0	;[CSM] Get device it was found on
2)		MOVEM	T1,FILBLK+1	;[CSM] Create new file on same disk
2)		SKIPN	T2		;[CSM] Was the old PPN zero?
2)		 PUSHJ	P,UNPARS	;[CSM] Yes, file may be in LIB:, so check it
2)	>
2)	;[CSM] Check for write access by trying to ENTER the file
2)		TLNE	F,RDO		;[CSM] Want file read-only?
2)		 JRST	SETFL2		;[CSM] Yes
2)		OPEN	5,FILBLK	;[CSM] Open scratch channel
2)		  HALT	.		;[CSM] Cannot fail
2)		MOVE	T1,FILFIL+2	;[CSM] Get file name
2)		MOVE	T2,FILFIL+3	;[CSM] Extension
2)		MOVE	T3,FILFIL+4	;[CSM] Protection
2)		MOVE	T4,FILFIL+1	;[CSM] Actual path
2)		ENTER	5,T1		;[CSM] Try to create
2)		  TLOA	F,RDO		;NO - READ ONLY
2)		TLZ	F,RDO		;YES - ENABLE FOR WRITING
2)		MOVEI	T1,5		;[CSM] Channel number
2)		RESDV.	T1,		;[CSM] Reset channel, without superseding file
2)		  RELEAS 5,		;[CSM] RESDV. not implemented
2)	SETFL2:				;[CSM]
2)		SETOM	INJFN		;SAY FILE HAS BEEN SET UP
**************
1)4		CAILE	T1,MAXSIZ*200	;IS FILE TOO BIG?
1)		JRST	SSZERR		;YES - ERROR
****
2)20		CAILE	T1,MAXSIZ	;[CSM] Is file too big?
2)		 JRST	SSZERR		;YES - ERROR
**************
1)4	IFN FTSFD,<
1)		SETZM	FILFIL+1
1)	>
1)	IFE FTSFD,<
****
2)20	IFE FTSFD,<
**************
1)4		  HALT
1)		MOVE	T1,FILSIZ	;EXPAND CORE TO ACCOMMODATE NEW FILE
****
2)20		  HALT	.
2)		MOVE	T1,FILSIZ	;EXPAND CORE TO ACCOMMODATE NEW FILE
**************
1)4		  HALT
1)		MOVN	T1,FILSIZ
1)		HRLM	T1,FILCCL	;READ ENTIRE FILE IN
****
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)20		  HALT	.
2)		MOVN	T1,FILSIZ
2)		SKIPN	T1		;[CSM] Zero length file?
2)		 MOVNI	T1,1		;[CSM] Yes, use 1 word
2)		HRLM	T1,FILCCL	;READ ENTIRE FILE IN
**************
1)4		  HALT			;ON CHANNEL 5
1)		ENTER	5,FILFIL
1)		  HALT
1)		HLLZS	FILFIL+3
****
2)20		  HALT	.		;ON CHANNEL 5
2)		ENTER	5,FILFIL
2)		  HALT	.
2)		HLLZS	FILFIL+3
**************
1)4	IFN FTSFD,<
1)		SETZM	FILFIL+1
1)	>
1)	IFE FTSFD,<
****
2)20	IFE FTSFD,<
**************
1)4		HRROI	T2,STTFIL
****
2)20	IFN TOPS10,<	;[CSM] Try TMP:SED before DSK:000SST.TMP (should be there already)
2)		MOVE	T1,[1,,STTCCL-1];[CSM] Code to read a file
2)		TMPCOR	T1,		;[CSM] Read TMP:SED
2)		  SKIPA			;[CSM] Error, try disk
2)		 JRST	SETMF1		;[CSM] Got it
2)	>
2)		HRROI	T2,STTFIL
**************
1)4	>
1)		TRZ	F,GFL		;CURRENT FILE IS NO LONGER AROUND
****
2)20	SETMF1: >
2)		TRZ	F,GFL		;CURRENT FILE IS NO LONGER AROUND
**************
1)4	;**********************************************************************
1)	;HERE TO ENTER AN ARGUMENT TO A COMMAND
****
2)21	SUBTTL	The ENTER command
2)	;HERE TO ENTER AN ARGUMENT TO A COMMAND
**************
1)4		MOVEI	T1,">"		;PUT UP ENTER MARK
****
2)21			   ;<
2)		MOVEI	T1,">"		;PUT UP ENTER MARK
**************
1)4		  HALT
1)	>
****
2)21		  HALT	.
2)	>
**************
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)4	;**********************************************************************
1)	;HERE TO ROLL FORWARD A GIVEN NUMBER OF PAGES (THE EASY WAY)
****
2)22	SUBTTL	Roll forward/backward pages
2)	;HERE TO ROLL FORWARD A GIVEN NUMBER OF PAGES (THE EASY WAY)
**************
1)4	;**********************************************************************
1)	;HERE ON RESET (ALIAS RUBOUT) RESETS ENTER MODE, OR RE-WRITES SCREEN
****
2)23	SUBTTL	RESET command
2)	;HERE ON RESET (ALIAS RUBOUT) RESETS ENTER MODE, OR RE-WRITES SCREEN
**************
1)4	;**********************************************************************
1)	;HERE FOR THE COMMAND TO SET A NUMBER OF SWITCHES
****
2)24	SUBTTL	Change switch settings
2)	;HERE FOR THE COMMAND TO SET A NUMBER OF SWITCHES
**************
1)4		MOVEI	T1,[ASCIZ /FILE: /]
1)		PUSHJ	P,PUTSTG
****
2)24		MOVEI	T1,[ASCIZ /File: /]
2)		PUSHJ	P,PUTSTG
**************
1)4		MOVEI	T1,[ASCIZ / PAGE: /]
1)		PUSHJ	P,PUTSTG
****
2)24		MOVEI	T1,[ASCIZ / Page: /]
2)		PUSHJ	P,PUTSTG
**************
1)4	SWHNP1:	MOVEI	T1,[ASCIZ / LINE: /]
1)		PUSHJ	P,PUTSTG
****
2)24	SWHNP1:	MOVEI	T1,[ASCIZ / Line: /]
2)		PUSHJ	P,PUTSTG
**************
1)4		MOVEI	T1,[ASCIZ /%) POS: /]
1)		PUSHJ	P,PUTSTG
****
2)24		MOVEI	T1,[ASCIZ /%) Pos: /]
2)		PUSHJ	P,PUTSTG
**************
1)4		MOVEI	T1,[ASCIZ / ALT: /]
1)		PUSHJ	P,PUTSTG
****
2)24		MOVEI	T1,[ASCIZ / Alt: /]
2)		PUSHJ	P,PUTSTG
**************
1)4		MOVEI	T1,[ASCIZ /; KEY: /]
1)		PUSHJ	P,PUTSTG
****
2)24		MOVEI	T1,[ASCIZ /; Key: /]
2)		PUSHJ	P,PUTSTG
**************
1)4	;**********************************************************************
1)	;HERE TO DO A RECALL COMMAND -
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

****
2)25	SUBTTL	RECALL command
2)	;HERE TO DO A RECALL COMMAND -
**************
1)4	;**********************************************************************
1)	;HERE TO INSERT A REAL TAB IN THE FILE - SAME AS IF USER TYPED E-C-C I
****
2)26	SUBTTL	REAL TAB, MARK, and COUNTER
2)	;HERE TO INSERT A REAL TAB IN THE FILE - SAME AS IF USER TYPED E-C-C I
**************
1)4	;**********************************************************************
****
2)27	SUBTTL	Utility subroutines
2)	;**********************************************************************
**************
1)5		  HALT
1)	>
****
2)27		  HALT	.
2)	>
**************
1)5		  HALT
1)	IFE FTKA10,<
****
2)27		  HALT	.
2)	IFE FTKA10,<
**************
1)5		  HALT
1)		LOOKUP	5,BAKFIL
****
2)27		  HALT	.
2)		LOOKUP	5,BAKFIL
**************
1)5		  HALT			;  (IT BETTER WORK)
1)	IFE FTKA10,<
****
2)27		  HALT	.		;  (IT BETTER WORK)
2)	IFE FTKA10,<
**************
1)5		MOVEI	T1,[ASCIZ /NOT MODIFIED: /]
1)		PUSHJ	P,PUTSTG
****
2)27		MOVEI	T1,[ASCIZ /Not modified: /]
2)		PUSHJ	P,PUTSTG
**************
1)5		MOVEI	T1,[ASCIZ /SAVING FILE: /]
1)		PUSHJ	P,PUTSTG
****
2)27		MOVEI	T1,[ASCIZ /Saving file: /]
2)		PUSHJ	P,PUTSTG
**************
1)5		MOVEI	T1,[ASCIZ / (NO BACKUP)/]
1)		SKIPN	BAKFLG		;IS A BACKUP WANTED?
****
2)27		MOVEI	T1,[ASCIZ / (no .BAK)/]
2)		SKIPN	BAKFLG		;IS A BACKUP WANTED?
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

**************
1)5		  HALT			;ON CHANNEL 5
1)		ENTER	5,(T2)
1)		  HALT
1)		HLLZS	1(T2)
****
2)27		  HALT	.		;ON CHANNEL 5
2)		ENTER	5,(T2)
2)		  HALT	.
2)		HLLZS	1(T2)
**************
1)5		  HALT			;ON CHANNEL 5
1)		SETZ	T1,		;ASSUME FILE'S NOT THERE
****
2)27		  HALT	.		;ON CHANNEL 5
2)		SETZ	T1,		;ASSUME FILE'S NOT THERE
**************
1)5		  HALT			;ON CHANNEL 5
1)		LOOKUP	5,(T2)
****
2)27		  HALT	.		;ON CHANNEL 5
2)		LOOKUP	5,(T2)
**************
1)5		JRST	ERSPM1		;YES - DON'T BOTHER TO ERASE IT
1)		TRNE	F,IMD		;IN INSERT MODE?
****
2)27		JRST	ERSPM0		;[CSM] Yes, don't update INSERT message
2)		TRNE	F,IMD		;IN INSERT MODE?
**************
1)5		TLZ	F,FBL		;BOTTOM LINE IS NOW O.K.
1)		PUSHJ	P,CBOTOM	;MOVE TO BOTTOM OF SCREEN
1)		TLZE	F,XPB		;IS POINTER TO LAST LINE VALID?
****
2)27	ERSPM0:	TLZ	F,FBL		;BOTTOM LINE IS NOW O.K.
2)		PUSHJ	P,CBOTOM	;MOVE TO BOTTOM OF SCREEN
2)		TLNE	TM,NEL		;[CSM] No bottom line?
2)		 JRST	ERSPM1		;[CSM] Just keep it blank
2)		TLZE	F,XPB		;IS POINTER TO LAST LINE VALID?
**************
1)6	SWHWRT:	JUMPE	T4,.+2		;SET READ-ONLY FLAG?
****
2)27	SWHRDO:	JUMPN	T4,.+2		;[CSM] Set READONLY flag?
2)		TLZA	F,RDO		;[CSM] No
2)		TLO	F,RDO		;[CSM] Yes
2)		POPJ	P,
2)	SWHWRT:	JUMPE	T4,.+2		;SET READ-ONLY FLAG?
**************
1)6		JRST	PARSF0		;GO GET FILE NAME AGAIN
****
2)27		MOVEM	T1,FILPTH	;[CSM] Put device name in PATH. block
2)		MOVE	T1,[SFDLVL+4,,FILPTH]
2)		PATH.	T1,		;[CSM] Get the impled path for this device
2)		  SETZM	FILPPN		;[CSM] Cannot fail for disk devices
2)		JRST	PARSF0		;GO GET FILE NAME AGAIN
**************
1)7		MOVEI	T1,SFDLVL	;SET UP DEFAULT PATH AS CURRENT ONE
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)	SETSFD:	MOVE	T2,DEFPTH+2(T1)
1)		MOVEM	T2,FILPTH+2(T1)
1)		SOJGE	T1,SETSFD
1)		JRST	SETFLC		;GO DO THAT SET-FILE
****
2)27		JRST	SETFLC		;GO DO THAT SET-FILE
**************
1)7	REDTMP:	HRROI	T2,STTFIL
1)		PUSHJ	P,SETIN		;GO FIND THE TEMPORARY FILE
****
2)27	REDTMP:
2)	IFN TOPS10,<	;[CSM] Try TMP:SED before DSK:000STT.TMP
2)		MOVEI	T1,-400		;[CSM] Set to MAX size
2)		HRLM	T1,STTCCL	;[CSM]  in IOWD
2)		MOVE	T1,[1,,STTCCL-1];[CSM] Code to read
2)		TMPCOR	T1,		;[CSM] Read TMP:SED
2)		  SKIPA			;[CSM] Not there, try disk
2)		 JRST   RDTMP1		;[CSM] Got it
2)	>
2)		HRROI	T2,STTFIL
2)		PUSHJ	P,SETIN		;GO FIND THE TEMPORARY FILE
**************
1)7	>
1)	IFE TOPS10,<
****
2)27	RDTMP1:	>
2)	IFE TOPS10,<
**************
1)7		JRST	T1,.-3
1)		DPB	T2,T4		;NULL OUT THE CARRIAGE RETURN
****
2)27		 JUMPN	T1,TMPGET	;[CSM] Loop unless null
2)		DPB	T2,T4		;NULL OUT THE CARRIAGE RETURN
**************
1)7	;****************************************************************
1)	;TERMINAL DEPENDENT OUTPUT SECTION
****
2)28	SUBTTL	Terminal output routines
2)	;TERMINAL DEPENDENT OUTPUT SECTION
**************
1)7	PUTSEQ:	TLNN	T1,777777	;GOT A ROUTINE ADDRESS?
1)		JRST	@T1		;YES - DISPATCH TO THAT ROUTINE
****
2)28	PUTSEQ:	TLNN	T1,774000+37	;[CSM] Nothing but flag bits? (003740)
2)		JRST	@T1		;YES - DISPATCH TO THAT ROUTINE
**************
1)7		  HALT
1)		MOVEM	T1,BRKADR+1	;SET IT UP IN VARIOUS LOCATIONS
****
2)28		  HALT	.
2)		MOVEM	T1,BRKADR+1	;SET IT UP IN VARIOUS LOCATIONS
**************
1)7		  HALT
1)		MOVE	T1,[XWD 3,BRKADR]
1)		TRMOP.	T1,		;SET UP TO BREAK ON ALL CHARACTERS
1)		  HALT
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)		MOVE	T1,[XWD 3,PAGADR]
1)		TRMOP.	T1,		;SET UP TTY NO PAGE
1)		  HALT
1)		SETZM	INTBLK+2	;RE-ENABLE CONTROL-C INTERRUPTS
****
2)28		  HALT	.
2)		MOVE	T1,[XWD 3,BRKADR]
2)		TRMOP.	T1,		;SET UP TO BREAK ON ALL CHARACTERS
2)		  HALT	.
2)		PUSHJ	P,@RTE(TM)	;[CSM] Set up terminal (enable keypad)
2)		TLNE	TM,NPG		;[CSM] Leave XON and XOFF alone?
2)		JRST	.+4		;[CSM] Yes
2)		MOVE	T1,[XWD 3,PAGADR]
2)		TRMOP.	T1,		;SET UP TTY NO PAGE
2)		  HALT	.
2)		SETZM	INTBLK+2	;RE-ENABLE CONTROL-C INTERRUPTS
**************
1)7		MOVEI	T3,13+11+4+SFDLVL
1)	>
****
2)28		MOVEI	T3,13+11+4+SFDLVL+2	;[CSM] Include .RBVER and .RBSPL
2)	>
**************
1)7		MOVEI	T2,5		;RESTORE THE LENGTH OF THE LOOKUP BLOCK
1)		MOVEM	T2,OLDFIL
****
2)28		MOVEI	T2,7		;[CSM] Restore the length of the LOOKUP block
2)		MOVEM	T2,OLDFIL
**************
1)7		HIBER	T1,
****
2)28		HRLI	T1,(HB.RTL)	;Wake on line of input (Control-C)
2)		HIBER	T1,
**************
1)7		POPJ	P,		;DONE
****
2)28		PUSHJ	P,DISTST	;[CSM] Process ^C at this time
2)		POPJ	P,		;DONE
**************
1)7	;************************************************************************
1)	;HISEG DATA
1)	CMVTBL:	CDOWN		;CURSOR MOVE TABLE -
****
2)29	SUBTTL	HISEG data
2)	CMVTBL:	CDOWN		;CURSOR MOVE TABLE -
**************
1)7	SWHNUM=.-SWCHES
****
2)29	ASCII /READO/		;[CSM] READONLY (=NOWRITE)
2)	SWHNUM=.-SWCHES
**************
1)7	;COMMAND DISPATCH TABLE
1)	CMDTBL:	RESET,,RESNPM	;	RESET (LOOKS LIKE A NULL)
****
2)29		SWHRDO		;[CSM] READONLY
2)30	SUBTTL	Command dispatch table
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)	CMDTBL:	RESET,,RESNPM	;	RESET (LOOKS LIKE A NULL)
**************
1)7	DEFINE TEXT1,<
1)	;COMMAND NAMES (FOR EXECUTE, MAINLY)
1)	CMDNAM:	ASCII	/^RS/	; @ RESET
1)		ASCII	/^IS/	; A INSERT SPACES
1)		ASCII	/^FL/	; B SET UP A FILE FOR EDITING
1)		ASCII	/^AB/	; C ABORT
1)		ASCII	/^IL/	; D INSERT LINES
1)		ASCII	/^SB/	; E SEARCH BACKWARD
1)		ASCII	/^DL/	; F DELETE LINES
1)		ASCII	/^PT/	; G PUT
1)		ASCII	/^CL/	; H CURSOR LEFT
1)		ASCII	/^TB/	; I TAB
1)		ASCII	/^LF/	; J LINEFEED
1)	IFN NEWTAB,<
1)		ASCII	/^SL/	; K SLIDE LEFT
1)		ASCII	/^SR/	; L SLIDE RIGHT
1)		ASCII	/^RT/	; M CARRIAGE RETURN
1)		ASCII	/^SW/	; N SET SWITCHES
1)	>
1)	IFE NEWTAB,<
1)		ASCII	/^PK/	; K PICK
1)		ASCII	/^SL/	; L SLIDE LEFT
1)		ASCII	/^RT/	; M CARRIAGE RETURN
1)		ASCII	/^BT/	; N BACK-TAB
1)	>
1)		ASCII	/^EC/	; O ENTER CONTROL CHARACTER
1)		ASCII	/^GO/	; P PERCENT GOTO
1)		ASCII	/^RBP/	; Q ROLL BACK PAGES
1)		ASCII	/^SF/	; R SEARCH FORWARD
1)		ASCII	/^DS/	; S DELETE SPACES
1)		ASCII	/^RFL/	; T ROLL FORWARD LINES
1)	IFN NEWTAB,<
1)		ASCII	/^BT/	; U BACK-TAB
1)		ASCII	/^PK/	; V PICK
1)	>
1)	IFE NEWTAB,<
1)		ASCII	/^SR/	; U SLIDE RIGHT
1)		ASCII	/^SW/	; V SET SWITCHES
1)	>
1)		ASCII	/^RBL/	; W ROLL BACK LINES
1)		ASCII	/^EX/	; X DO COMMAND SEQUENCE
1)		ASCII	/^RFP/	; Y ROLL FORWARD PAGES
1)		ASCII	/^XT/	; Z NORMAL EXIT
1)		ASCII	/$/	; $ ENTER PARAMS
1)		ASCII	/^CD/	; \ CURSOR DOWN
1)		ASCII	/^CR/	; ] CURSOR RIGHT
1)		ASCII	/^CU/	; ^ CURSOR UP
1)		ASCII	/^CH/	; _ HOME
1)		ASCII	/^RC/	;   RECALL
1)		ASCII	/^IN/	;   INSERT MODE
1)		ASCII	/^DC/	;   DELETE CHAR
1)		ASCII	/^TA/	;   REAL TAB
1)		ASCII	/^MK/	;   MARK POSITION FOR PICK OR CLOSE
1)		ASCII	/^CT/	;   USE INCREMENTING COUNTER
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)	FENCE:	ASCII	/   *** This FENCE marks the last page of the file /
1)		ASCII	/but is NOT a part of it */
1)		BYTE	(7)"*","*"," "," "
1)	STARS:	BYTE	(7) 15,12,15,12,40
1)		BYTE	(7) 40,40,40,40,40
1)		BYTE	(7) 40,40,40,40,40
1)		BYTE	(7) 40,"*","*","*","*"
1)		ASCII  /******************************/
1)		BYTE	(7) "*","*",15,12,15
1)		BYTE	(7) 12,40,40,40,40
1)		BYTE	(7) 40,40,40,40,40
1)		BYTE	(7) 40,40,40,0
1)	>
1)		XLIST
1)		TEXT1
1)		LIT
1)		LIST		;DUMP THE LITERALS IN THE HISEG
1)		RELOC	0	;*** LOWSEG STARTS HERE ***
****
2)30	;COMMAND NAMES (FOR EXECUTE, MAINLY)
2)	DEFINE	X(ARG),<IFIDN <ARG>,<$>,<ASCII /ARG/;>	ASCII	/^ARG/>
2)	CMDNAM:	CMDS
2)	FENCE:	ASCII	/   *** This FENCE marks the last page of the file /
2)		ASCIZ	/but is NOT a part of it ***  /
2)	STARS:	ASCIZ /
2)	            ************************************
2)	            /
2)	LITS:	XLIST           ;LIT
2)		LIT
2)		LIST		;DUMP THE LITERALS IN THE HISEG
2)31	SUBTTL	LOWSEG data
2)		RELOC	0	;*** LOWSEG STARTS HERE ***
**************
1)7	TTYBLK:	2
1)		SIXBIT	/TTY/
1)		0
1)	BRKADR:	2037
1)		0
1)		0
1)	CHRIN:	20
1)		0
1)	TSTIN:	1
1)		0
1)	PAGADR:	2021		;SET UP TTY NO PAGE
1)		0
****
2)31	TTYBLK:	.IOPIM		;Packed Image Mode
2)		SIXBIT	/TTY/
2)		0
2)	BRKADR:	.TOPBS+.TOSET	;PIM break set
2)		0
2)		0
2)	CHRIN:	.TOISC		;Input single character
2)		0
2)	TSTIN:	.TOSIP		;Skip if input in progress
2)		0
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

2)	PAGADR:	.TOPAG+.TOSET	;Set TTY NO PAGE
2)		0
**************
1)7	FILFIL:	5		;EXTENDED LOOKUP BLOCK FOR CURRENT FILE
1)		BLOCK	5
1)	IFN FTSFD,<
****
2)31	FILFIL:	7		;[CSM] Extended LOOKUP block for current file
2)		BLOCK	7	;[CSM] Preserve version number and spooling name
2)	IFN FTSFD,<
**************
1)7	OLDFIL:	5		;EXTENDED LOOKUP BLOCK FOR DITTO
1)		BLOCK	5
1)	IFN FTSFD,<
****
2)31	OLDFIL:	7		;[CSM] Extended LOOKUP block for ditto
2)		BLOCK	7	;[CSM] Include .RBVER and .RBSPL
2)	IFN FTSFD,<
**************
1)8	STTCCL:	IOWD	1,PIKBUF+PCBSIZ-400
****
2)31		SIXBIT	/SED/		;[CSM] Name of TMPCOR file, at STTCCL-1
2)	STTCCL:	IOWD	1,PIKBUF+PCBSIZ-400
**************
1)8	XCTNAM:	516530,,200001	;PRE-SET SUBSTITUTE SEQUENCE (ASCII /SUB/ ! BIT 35)
1)		BLOCK	XBFNUM-2
****
2)31	XCTNAM:	<ASCII /SUB/>+1	;PRE-SET SUBSTITUTE SEQUENCE
2)		BLOCK	XBFNUM-2
**************
1)8	IFN SAVETY,<
1)	DEBPTR:	POINT	7,DEBBUF
****
2)31	IFN SAVETY,<	;Put text in DSK:EDEBUG.TMP for debugging
2)	DEBPTR:	POINT	7,DEBBUF
**************
1)8	>
1)	IFN TOPS10,<
****
2)31	>  ;End of IFN SAVETY
2)	IFN TOPS10,<
**************
1)8	DEFINE TEXT2,<
1)	ASCIZ	/**************************************************************************
****
2)31	ASCIZ	/**************************************************************************
**************
1)8	     For a tutorial manual see the file 	       DOC:SED.MAN.
1)	     For complete editor documentation see the file    DOC:SED.DOC.
****
2)31	     For a tutorial manual see the file                MAN:SED.MAN.
2)	     For complete editor documentation see the file    DOC:SED.DOC.
**************
1)8	>
1)		XLIST
1)		TEXT2
File 1)	DSK:SED.MAC[14,10,DECUS0,SED]     	created: 0748 01-May-80
File 2)	DSKA:SED.MAC[14,10,DECUS0,SED,CSM]	created: 1330 14-Jun-83

1)		LIST	;OUTPUT TEXT2 (SAVING LISTING PAPER)
1)	BUFFEN==.
****
2)31	BUFFEN==.
**************
1)8		0
1)	>
1)	>
1)		END	START
1)	;EXPENDABLE STUFF AT THE END
****
2)31		0               ;Allocate this page
2)	>
2)	>  ;End of IFN FTDDT
2)32	SUBTTL	End of LOWSEG, start of text buffer
2)	SEDEND:	END	START	;EXPENDABLE STUFF AT THE END
**************
  #y;D?