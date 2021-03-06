File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

1)1	SUBTTL	BOWERING/DMN/TWE/DMN/LCR/LLN/ILG/MFB/MS/PY/BAH	3-Jun-85
**** ;At top of file +1L
2)1		IFNDEF TYMCOM,<TYMCOM==-1>	;[JMS] With TYMCOM-X hacks
2)	SUBTTL	BOWERING/DMN/TWE/DMN/LCR/LLN/ILG/MFB/MS/PY/BAH	3-Jun-85
**************
1)1		VCUSTOM==0
1)	INTERNAL .JBVER
**** ;At top of file +30L
2)1		VCUSTOM==2		;Modified by JMS at TYMSHARE
2)	INTERNAL .JBVER
**************
1)1		.REQUEST REL:HELPER		;[72] Add the request for HELPER
1)		.DIRECTIVE	FLBLST		;[100]
**** ;At top of file +41L
2)1	IFE TYMCOM,<	.REQUEST REL:HELPER >	 ;[72] Add the request for HELPER
2)	IFN TYMCOM,<	.REQUEST SYS:HELPER >	;[TYM1] No device REL: yet
2)		.DIRECTIVE	FLBLST		;[100]
**************
1)2	W1=1
**** ;At top of file +60L
2)2	CCL==3		;[TYM3] To read DSK:nnnFCM.TMP
2)	W1=1
**************
1)2	LPDL==50	;LENGTH OF PUSH DOWN LIST(HELPER NEEDS MORE THAN 20)
1)3	NOFORM==1		;DON'T OUTPUT FORM FEED
1)	PAGSW==2
1)	ENDSW==4		;END SWITCH (SUPPRESS FORM-FEED  IF FILES IDENTICAL)
**** ;At top of file +76L
2)2	%TMLEN==^D60	;Size of TMPCOR buffer
2)	LPDL==50	;LENGTH OF PUSH DOWN LIST(HELPER NEEDS MORE THAN 20)
2)3	;Flag bits in LH of FR
2)	NOFORM==     1		;DON'T OUTPUT FORM FEED
2)	;[TYM2];PAGSW==2	;OUTPUT A PAGE HEADER IF MORE DIFFERENCES
2)	KSWBIT==     2		;[TYM2] /K SWITCH - Ignore upper/lower case differences
2)	ENDSW==      4		;END SWITCH (SUPPRESS FORM-FEED  IF FILES IDENTICAL)
**************
1)3	USWBIT==200000		;LIST ENTIRE 2ND FILE, SHOWING CHANGES
1)	EXEBIT==400000		;COMPARING EXE FILES IN PROGRESS
1)	ASCSW==ASWBIT!SSWBIT!CSWBIT!ALLSW!USWBIT	;ASCII SWITCHES
1)	BINSW==WSWBIT!XSWBIT!WDFBIT!XDFBIT!HSGBIT!EXEBIT	;BINARY SWITCHES
**** ;At top of file +93L
2)3	USWBIT==200000		;/U LIST ENTIRE 2ND FILE, SHOWING CHANGES
2)	EXEBIT==400000		;COMPARING EXE FILES IN PROGRESS
2)	ASCSW==ASWBIT!SSWBIT!CSWBIT!ALLSW!USWBIT!KSWBIT	;[TYM2] ASCII SWITCHES
2)	BINSW==WSWBIT!XSWBIT!WDFBIT!XDFBIT!HSGBIT!EXEBIT	;BINARY SWITCHES
**************
1)3	MAXSFD==7		;MAXIMUM NUMBER OF SFD'S ALLOWED
**** ;At top of file +102L
2)3	LPPS==^D37		;[TYM4] Lines per page for small paper
2)	MAXSFD==7		;MAXIMUM NUMBER OF SFD'S ALLOWED
**************
1)8	;	END OF REVISION HISTORY.
**** ;At top of file +336L
2)8	;
2)	;TYM1	JMS		10-Jan-86
2)	;	Convert V22(111) to recognize TYMCOM-X dates
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

2)	;
2)	;TYM2	JMS		10-Jan-86
2)	;	Add /K to ignore upper/lower case differences
2)	;
2)	;TYM3	JMS		13-Jan-86
2)	;	Check for TMP:FCM on CCL entry, return to RPG if /R seen.
2)	;
2)	;TYM4	JMS		13-Jan-86
2)	;	Add /N for narrow paper, 37 line per page.  Add to /T (which forces
2)	;	output file even if same) to output header only once.  Make /O be
2)	;	the default for *.MAC compares.
2)	;
2)	;suggestions:   Make /T output the file names only once, no formfeeds in SCM
2)	;               Add (USERNAME) parsing
2)	;
2)	;	END OF REVISION HISTORY.
**************
1)9	FILCOM:	JFCL			;[100] IN CASE OF CCL RUN
1)		SETZM	XITFLG		;[45] ^Z HAS NOT BEEN TYPED
1)		JRST	COMPGO		;[45]   SO GO PROCESS A COMMAND.
1)	COMP:	CLOSE	CTL,
**** ;At COMPGO+8L
2)9	FILCOM:	TDZA	T,T		;[TYM3] Normal start
2)		 SETO	T,		;[TYM3] CCL entry
2)		SETZM	CCLCNT		;[TYM3] Clear byte count
2)		SETZM	XITFLG		;[45] ^Z HAS NOT BEEN TYPED
2)		JUMPE	T,COMPGO	;[TYM3] SO GO PROCESS A COMMAND.
2)		PUSHJ	PP,REDCCL	;[TYM3] Get CCL commands
2)		  OUTSTR [ASCIZ /%Could not find TMP:FCM command file/]
2)		JRST	COMPGO		;[TYM3] Process command
2)	COMP:	CLOSE	CTL,
**************
1)9	COMPGO:	SKIPE	XITFLG		;[45] EXIT BECAUSE OF ^Z?
1)		EXIT	1,		;[45] YES, A CONTINUE WILL RESTART
**** ;At COMPGO+8L
2)9	COMPGO:
2)	IFN TYMCOM,<	;Return to RPG on /R switch
2)		SKIPE	RPGFLG		;[TYM3] /R ?
2)		 JRST	RPG		;[TYM3] Yes, go run SYS:RPG
2)	>  ;End of IFN TYMCOM
2)		SKIPE	XITFLG		;[45] EXIT BECAUSE OF ^Z?
2)		EXIT	1,		;[45] YES, A CONTINUE WILL RESTART
**************
1)9		CAML	.JBREL		;[31] IS THIS NECESARY?
1)		JRST	CMPGO1		;[31] NO, CONTINUE INITIALICING REST
1)		HRL	.JBFF		;[31] SET LH FOR 'BLT'
**** ;At COMPGO+17L
2)9		CAML	.JBREL		;[31] IS THIS NECESSARY?
2)		JRST	CMPGO1		;[31] NO, CONTINUE INITIALIZING REST
2)		HRL	.JBFF		;[31] SET LH FOR 'BLT'
**************
1)9		MOVSI	FR,PAGSW+NOFORM	;INIT FR FOR NEW PAGE HEADING
1)10		PUSHJ	PP,INITTY	;INITIALIZE TTY
**** ;At CMPGO1+9L
2)9		MOVEI	0,LPP		;[TYM4] Reset lines per page
2)		MOVEM	0,LPPX		;[TYM4]
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

2)		MOVSI	FR,NOFORM	;[TYM2] INIT FR FOR NEW PAGE HEADING
2)		SETOM	PAGSW		;[TYM2]
2)10		PUSHJ	PP,INITTY	;INITIALIZE TTY
**************
1)10		PUSHJ	PP,TYO		;OUTPUT THE *
**** ;At CMPGO1+13L
2)10		SKIPG	CCLCNT		;[TYM3] CRLF but no * in CCL mode
2)		 PUSHJ	PP,TYO		;OUTPUT THE *
**************
1)31		CAIE	W1,";"		;COMMENT IN FILE 1?
**** ;At COMPL4+13L
2)31	;Here when the characters in W1 and W2 don't match.  Could be lowercase.
2)		CAIE	W1,";"		;COMMENT IN FILE 1?
**************
1)31		JRST	CPOPJ1		;NO, FILES DON'T MATCH, SKIP RETURN
1)		JUMPE	W1,CPOPJ	;YES, OTHER CHAR MUST BE NULL OR ELSE ONE
**** ;At COMPL4+16L
2)31		 JRST	COMPL8		;[TYM2] No, other differences
2)		JUMPE	W1,CPOPJ	;YES, OTHER CHAR MUST BE NULL OR ELSE ONE
**************
1)32	;WHEN WE GET TO THIS POINT WE HAVE FOUND 
**** ;At CPOPJ+1L
2)31	COMPL8:	XOR	W1,W2		;[TYM2] Get the difference bits of the two
2)		CAIN	W1,40		;[TYM2] Was the difference simply letter case?
2)		TLNN	FR,KSWBIT	;[TYM2]  and are we supposed to ignore that?
2)		 JRST	CPOPJ1		;[TYM2] No, this must be a mismatch
2)		TRZ	W2,40		;[TYM2] Check to make sure they were alphabetic
2)		CAIL	W2,"A"		;[TYM2] The bit clear above forced to UC range
2)		CAILE	W2,"Z"		;[TYM2]  and we know that was one of the chars
2)		 JRST	CPOPJ1		;[TYM2] Not alphabetic: mismatch
2)		JRST	COMPL1		;[TYM2] Aha! alphabetic, go on to next char
2)32	;WHEN WE GET TO THIS POINT WE HAVE FOUND
**************
1)35		TLO	FR,PAGSW	;THIS MEANS WE GET A NEW HEADING
1)		POPJ	PP,
**** ;At PCRLF+5L
2)35		SETOM	PAGSW		;[TYM2] THIS MEANS WE GET A NEW HEADING
2)		POPJ	PP,
**************
1)36		TLZN	FR,PAGSW	;DO WE NEED A NEW HEADING
1)		JRST	TYO		;NO--SIMPLE CHARACTER OUTPUT
1)		SETOM	LINCNT		;YES
**** ;At PCHAR+1L
2)36		SKIPN	PAGSW		;[TYM2] DO WE NEED A NEW HEADING
2)		JRST	TYO		;NO--SIMPLE CHARACTER OUTPUT
2)		SETZM	PAGSW		;[TYM2]
2)		SKIPE	OPTSWT		;[TYM4] /T typed?
2)		 JRST	TYO		;[TYM4] Yes, only one header
2)		SETOM	LINCNT		;YES
**************
1)37		MOVEI	LPP		;RESET LINES/PAGE COUNT
1)		MOVEM	LINCNT
1)		MOVSI	16,SAVEXS	;AND RESTORE ACS
**** ;At PCHAR1+2L
2)37		MOVE	0,LPPX		;[TYM4] Reset lines per page count
2)		MOVEM	0,LINCNT	;[TYM4]
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

2)		MOVSI	16,SAVEXS	;AND RESTORE ACS
**************
1)42		INIT	CTL,1
1)		SIXBIT	/TTY/
1)		XWD	CTOBUF,CTIBUF
1)		HALT
**** ;At INITTY+2L
2)42		OPEN	CTL,[.IOASL	;[TYM1]
2)			SIXBIT	/TTY/
2)			XWD	CTOBUF,CTIBUF]
2)		  HALT
**************
1)42	;HELP MESSAGE TEXT
**** ;At INITTY+10L
2)42	REDCCL:	MOVE	T,[.TCRDF,,W2]	;[TYM3] Read and delete file
2)		MOVSI	W2,'FCM'	;[TYM3] TMPCOR name
2)		MOVE	W3,[IOWD %TMLEN,TMPBUF] ;[TYM3] Pointer to buffer
2)		TMPCOR	T,		;[TYM3] Read TMP:FCM
2)		  JRST	CCLDSK		;[TYM3] Not there, try DSK:
2)	REDCC1:	MOVE	C,[BYTE(7).CHCNZ,.CHCNZ,.CHCNZ,.CHCNZ,0] ;[TYM3]
2)		MOVEM	C,TMPBUF(T)	;[TYM3] Store ^Z at end of input
2)		ADDI	T,1		;[TYM3] Include the control-Z's
2)		IMULI	T,5		;[TYM3] Convert to character count
2)		MOVEM	T,CCLCNT	;[TYM3]
2)		MOVE	T,[POINT 7,TMPBUF] ;[TYM3]
2)		MOVEM	T,CCLPTR	;[TYM3] Byte pointer
2)		SKIPLE	CCLCNT		;[TYM3] If nonzero,
2)		 AOS	(PP)		;[TYM3] Give skip return
2)		POPJ	PP,		;[TYM3] Success
2)	CCLDSK:	OPEN	CCL,[.IODMP	;[TYM3] INIT DSK: in dump mode
2)			 SIXBIT	/DSK/	;[TYM3]
2)			 XWD	0,0]	;[TYM3] No buffers in dump mode
2)		  POPJ	PP,		;[TYM3] Error return
2)		PJOB	W1,		;[TYM3] Job number
2)		IDIVI	W1,^D100	;[TYM3] 1st digit in W1
2)		IDIVI	W2,^D10		;[TYM3] 2nd in W2, 3rd in W3
2)		MOVE	C,['000FCM']	;[TYM3] File name
2)		DPB	W1,[POINT 4,C,5];[TYM3] Set bits in number field
2)		DPB	W2,[POINT 4,C,11];[TYM3]
2)		DPB	W3,[POINT 4,C,17];[TYM3]
2)		MOVEM	C,TMPBUF+0	;[TYM3] File name
2)		MOVSI	C,'TMP'		;[TYM3] Extension
2)		MOVEM	C,TMPBUF+1	;[TYM3]
2)		SETZM	C,TMPBUF+3	;[TYM3] PPN
2)		LOOKUP	CCL,TMPBUF	;[TYM3] Look for file
2)		  JRST	NOCCL		;[TYM3] File not found
2)		HLRE	T,TMPBUF+3	;[TYM3] Get minus word count
2)		MOVNS	T		;[TYM3] Positive word count
2)		MOVE	W1,[IOWD %TMLEN,TMPBUF];[TYM3]
2)		SETZ	W2,		;[TYM3] Dump mode IOWD list
2)		INPUT	CCL,W1		;[TYM3] Read in entire file
2)		SETZB	W1,W2		;[TYM3] Zero name to delete it
2)		RENAME	CCL,W1		;[TYM3] Delete file
2)		  OUTSTR [ASCIZ /%Could not delete FCM.TMP/]
2)	NOCCL:	RELEAS	CCL,		;[TYM3]	
2)		JRST	REDCC1		;[TYM3] Set up buffers
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

2)	;HELP MESSAGE TEXT
**************
1)46	TTYIN:	SOSG	CTIBUF+2	;DECREMENT CHARACTER COUNT, ANY LEFT?
1)		INPUT	CTL,		;NO, GET A BUFFER FULL
1)		ILDB	C,CTIBUF+1	;GET CHARACTER
1)		JUMPE	C,TTYIN		;FLUSH NULLS
1)		CAIE	C,.CHAL2	;[100]
**** ;At TTYIN
2)46	SUBTTL	TTY input routine
2)	TTYIN:	SOSGE	CCLCNT		;[TYM3] Any CCL command?
2)		 JRST	TTYIN1		;[TYM3] No, get it from the TTY
2)		ILDB	C,CCLPTR	;[TYM3] Yes, get a character
2)		JRST	TTYIN2		;[TYM3]
2)	TTYIN1:	SOSG	CTIBUF+2	;DECREMENT CHARACTER COUNT, ANY LEFT?
2)		INPUT	CTL,		;NO, GET A BUFFER FULL
2)		ILDB	C,CTIBUF+1	;GET CHARACTER
2)	TTYIN2:	JUMPE	C,TTYIN		;FLUSH NULLS
2)		CAIE	C,.CHAL2	;[100]
**************
1)46		POPJ	PP,		;NO, EXIT
**** ;At TTYIN+16L
2)46		CAIN	C,.CHCRT	;[TYM3] Ignore CR, go for LF
2)		 JRST	TTYIN
2)		POPJ	PP,		;NO, EXIT
**************
1)47		POPJ	PP,		;EXIT
**** ;At TERM+9L
2)47		CAIE	0,'MAC'		;[TYM4] MACRO-10 file?
2)		CAIN	0,'P11'		;[TYM4] or PDP-11 macro?
2)		 SETOM	OFSSWT		;[TYM4] Yes, make /O be default
2)		POPJ	PP,		;EXIT
**************
1)47		HSGBIT,,'SHR'
**** ;At EXTBL+3L
2)47		HSGBIT,,'SWR'		;[TYM2] Writeable SHR file
2)		HSGBIT,,'SHR'
**************
1)49		CAIN	C,"Q"		;/Q?
**** ;At GETSW1+2L
2)49		CAIN	C,"N"		;[TYM4] /N for /SMALL paper?
2)		 JRST	[MOVEI	C,LPPS	;[TYM4] Yes, get small lines per page
2)			 MOVEM	C,LPPX	;[TYM4]
2)			 JRST	GETIOC]	;[TYM4]
2)	IFN TYMCOM,<
2)		CAIN	C,"R"		;[TYM3] /R to return to RPG?
2)		 JRST	[SETOM	RPGFLG	;[TYM3] Yes, set flag
2)			 JRST	GETIOC]	;[TYM3]
2)	>  ;End IFN TYMCOM
2)		CAIN	C,"Q"		;/Q?
**************
1)49		CSWBIT!SSWBIT,,"C"	;/C DON'T COMP. COMMENTS OR SPACING
**** ;At SWTBL+1L
2)49		KSWBIT,,"K"		;[TYM2] /K ignore upper/lower case differences
2)		CSWBIT!SSWBIT,,"C"	;/C DON'T COMP. COMMENTS OR SPACING
**************
1)54		IDIVI	T,^D60		;CHANGE TO HOURS AND MINUTES
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

**** ;At HEAD4+16L
2)54	IFN TYMCOM,<;[TYM1] Need to convert TYMCOM-X format to DEC format
2)		IMULI	T,^D<60*60>	;60 ticks per second, 60 seconds per minute
2)		MOVEM	T,DATBLK+1	;Store ticks past midnight GMT
2)		LDB	T,[POINT 12,INDIR1+2,35]	;Get low 12 bits of date
2)		LDB	T+1,[POINT 2,INDIR1+1,21]	;Get high 2 bits of date
2)		DPB	T+1,[POINT 2,T,23]		;Merge the two parts
2)		MOVEM	T,DATBLK+0	;Store days since 1-Jan-64 (365 days per year)
2)		MOVSI	T,400020	;400000 = date is in TYMCOM-X format, 20 = GMT
2)		MOVEM	T,DATBLK+2	;0 in RH = convert to DEC, this time zone
2)		MOVEI	T,DATBLK	;Point to args
2)			OPDEF	DATUUO[CALLI -55]
2)	DAT:	DATUUO	T,		;Do conversion
2)		  JFCL			;Not implemtented
2)		MOVE	T,DATBLK+1	;Get new jiffies
2)		IDIVI	T,^D<60*60>	;Back into minutes
2)	>  ;End of IFN TYMCOM
2)		IDIVI	T,^D60		;CHANGE TO HOURS AND MINUTES
**************
1)54	HEDDAT:	LDB	T,[POINT 12,INDIR1+2,35]	;GET LOW 12 BITS OF DATE
1)		LDB	T+1,[POINT 3,INDIR1+1,20]	;GET HIGH 3 BITS OF DATE
1)		DPB	T+1,[POINT 3,T,23]		;MERGE THE TWO PARTS
1)		IDIVI	T,^D31		;STRIP OFF DAY OF MONTH
**** ;At HEDDAT+9L
2)54	IFE TYMCOM,<;[TYM1] DEC format (12+3 bits)
2)		LDB	T,[POINT 12,INDIR1+2,35]	;GET LOW 12 BITS OF DATE
2)		LDB	T+1,[POINT 3,INDIR1+1,20]	;GET HIGH 3 BITS OF DATE
2)		DPB	T+1,[POINT 3,T,23]		;MERGE THE TWO PARTS
2)	>  ;End IFE TYMCOM
2)	IFN TYMCOM,<;[TYM1] TYMCOM format
2)		MOVE	T,DATBLK+0	;Get converted date
2)	>  ;End IFN TYMCOM
2)		IDIVI	T,^D31		;STRIP OFF DAY OF MONTH
**************
1)58		TLZ	FR,PAGSW	;FAKE AS NOT NEW PAGE
1)					;SORT OUT CONFLICTING SWITCHES AND EXTENSIONS
**** ;At BINCOM+1L
2)58		SETZM	PAGSW		;[TYM2] FAKE AS NOT NEW PAGE
2)					;SORT OUT CONFLICTING SWITCHES AND EXTENSIONS
**************
1)60		TLO	FR,PAGSW	;[63] THIS MEANS WE GET A NEW HEADING
1)		SETOM	FWA1ST		;[63] SET IT ONE FOR NEXT CHECK
**** ;At COMP14+1L
2)60		SETOM	PAGSW		;[TYM2] THIS MEANS WE GET A NEW HEADING
2)		SETOM	FWA1ST		;[63] SET IT ONE FOR NEXT CHECK
**************
1)70	SYMPTR:	POINT	7,LSTSYM+SYMBOL	;[76] [104] POINTER TO LAST SYMBOL
**** ;At SYMPTR
2)69	IFN TYMCOM,<	;Here to return to SYS:RPG
2)	RPG:	MOVE	0,[1,,RPGBLK]	;[TYM3] Point to args
2)		RUN	0,		;[TYM3] Go back to SYS:RPG
2)		  HALT			;[TYM3] Let monitor output error message
2)	RPGBLK:	SIXBIT	/SYS/		;[TYM3] Device
2)		SIXBIT	/RPG/		;[TYM3] File name
2)		EXP	0,0,0,0		;[TYM3] Ext, prot, PPN, core
2)	>  ;End of IFN TYMCOM
File 1)	DSK:FILCOM.111	created: 1147 03-Jun-85
File 2)	DSK:FILCOM.MAC	created: 1611 15-Jan-86

2)70	SYMPTR:	POINT	7,LSTSYM+SYMBOL	;[76] [104] POINTER TO LAST SYMBOL
**************
1)72	VAR		;JUST IN CASE
1)	ENDP:
1)	RELOC		;DUMP LITS IN HIGH SEGMENT
1)		END	FILCOM
**** ;At PSYMB+1L
2)72	IFN TYMCOM,<DATBLK: BLOCK 3 >	;[TYM1] For converting TYMCOM-X dates
2)	PAGSW:	BLOCK	1		;[TYM2] No room in LH of FR, move bit here
2)	IFN TYMCOM,<RPGFLG: BLOCK 1 >	;[TYM3] Return to SYS:RPG if set
2)	LPPX:	BLOCK	1		;[TYM4] Lines per page, /N sets to ^D37
2)		VAR		;JUST IN CASE
2)	ENDP:		;End of CORE zeroed out by BLT at COMPGO
2)	TMPBUF:	BLOCK	%TMLEN		;[TYM3] TMPCOR buffer
2)	CCLCNT:	BLOCK	1		;[TYM3] Count of bytes in TMPBUF
2)	CCLPTR:	BLOCK	1		;[TYM3] Byte pointer into TMPBUF
2)		RELOC		;DUMP LITS IN HIGH SEGMENT
2)		END	FILCOM
**************
  