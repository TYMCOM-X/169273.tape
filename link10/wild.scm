File 1)	DSK:WILD.345	created: 1343 25-Sep-83
File 2)	DSK:WILD.NEW	created: 1530 25-Sep-83

1)1		SEARCH	MACTEN,UUOSYM,SCNMAC
1)		SALL
****
2)1		SEARCH	UUOTYM(UUOTYM.UNV[26007,244321])	; [A.BLAM 15-10-80]
2)		SEARCH	C(C.UNV[26007,244321])			; [A.BLAM 15-10-80]
2)		SEARCH	SCNMAC(SCNMAC.UNV[26007,244321])	; [A.BLAM 15-10-80]
2)		SALL
**************
1)3	;BLOCK+0:	ADDRESS OF SCAN FORMAT FILE SPEC (AT LEAST 32 WORDS)
1)	;      1:	OPEN BLOCK (3 WORDS)
****
2)3	;BLOCK+0:	ADDRESS OF SCAN FORMAT FILE SPEC (AT LEAST 35 WORDS)
2)	;      1:	OPEN BLOCK (3 WORDS)
**************
1)21		JUMPN	T3,WILDJ	;SEE IF UFD
1)		SKIPE	T1,.FXDIM (T4)	;YES--SEE IF [*,NOT*]
1)		TLNE	T1,-1		; ..
1)		JRST	WILDJ		;NO--OK TO PROCEED
1)		MOVE	T1,MYPPN	;YES--GET OUR NUMBER
1)		MOVE	T2,UFDPPN	;AND TARGET NUMBER
1)		TLNE	T1,777770	;SEE IF WE ARE GT PROJ 7
1)		TLNE	T2,777770	;AND TARGET IS LE PROJ 7
1)		JRST	WILDJ		;NO--PROCEED
1)		JRST	WILDN		;YES--SKIP MATCH SINCE THIS
1)					; IS TO A SYSTEM FILE FROM USER
1)	WILDJ:	AOS	T4,DEPTH	;LOOKS GOOD--ADVANCE DEPTH OF SEARCH
****
2)21	;	JUMPN	T3,WILDJ	;SEE IF UFD [A.BLAM 15-10-80]
2)	;	SKIPE	T1,.FXDIM(T4)	;YES--SEE IF [*,NOT*]
2)	;	TLNE	T1,-1		; ..
2)	;	JRST	WILDJ		;NO--OK TO PROCEED
2)	;	MOVE	T1,MYPPN	;YES--GET OUR NUMBER
2)	;	MOVE	T2,UFDPPN	;AND TARGET NUMBER
2)	;	TLNE	T1,777770	;SEE IF WE ARE GT PROJ 7
2)	;	TLNE	T2,777770	;AND TARGET IS LE PROJ 7
2)	;	JRST	WILDJ		;NO--PROCEED
2)	;	JRST	WILDN		;YES--SKIP MATCH SINCE THIS
2)					; IS TO A SYSTEM FILE FROM USER [END A.BLAM]
2)	WILDJ:	AOS	T4,DEPTH	;LOOKS GOOD--ADVANCE DEPTH OF SEARCH
**************
1)22	;HERE WHEN TIME TO READ NEXT BLOCK OF A DIRECTORY
1)	WILDR:	MOVEI	T1,100		;SET FOR 100
1)		SKIPE	FLDTA		;SEE IF DECTAPE
1)		MOVEI	T1,^D22		;YES--ONLY 22 FILES
1)		MOVEM	T1,BUFCNT(T4)	;  FILES IN A BLOCK
1)		SETZM	BUFPOS(T4)	;CLEAR POSITION IN BLOCK
1)	WILDRR:	PUSHJ	P,SETOPN	;SETUP OPEN BLOCK
****
2)22		setzm	astnam		;clear savefilename	[a.blam 15-10-80]
2)		setzm	astext		;clear saveext		[a.blam 15-10-80]
2)		setzm	afgch		;clear saveflag uuo chanio [a.blam 15-10-80]
2)	;HERE WHEN TIME TO READ NEXT BLOCK OF A DIRECTORY
2)	WILDR:	MOVEI	T1,100		;SET FOR 100
2)		SKIPE	FLDTA		;SEE IF DECTAPE
2)		MOVEI	T1,^D22		;YES--ONLY 22 FILES
2)		MOVEM	T1,BUFCNT(T4)	;  FILES IN A BLOCK
File 1)	DSK:WILD.345	created: 1343 25-Sep-83
File 2)	DSK:WILD.NEW	created: 1530 25-Sep-83

2)		SETZM	BUFPOS(T4)	;CLEAR POSITION IN BLOCK
2)	;save startname startext count flag for uuo chanio	[a.blam 15-10-80]
2)		skipe	fldta		;sauf dta
2)		jrst	wildrr		;and go read for files
2)		move	t1,astnam	;save astnam
2)		movem	t1,sstnam(t4)	;
2)		move	t1,astext	;save astext
2)		movem	t1,sstext(t4)	;
2)		move	t1,afgch	;save flag uuo chanio
2)		movem	t1,sfgch(t4)	;
2)		setom	firsrd(t4)		;set first read block
2)	;end [a.blam]
2)	WILDRR:	PUSHJ	P,SETOPN	;SETUP OPEN BLOCK
**************
1)23		CAILE	T1,1
1)	WILDRI:	USETI	WC,(T1)		;POSITION FILE
1)		MOVE	T1,[-200,,.WLDBF-1]
1)		MOVEI	T2,0		;SETUP DUMP LIST
1)		IN	WC,T1		;READ BUFFER
1)		  JRST	WILDRK		;OK
1)		STATZ	WC,IO.ERR	;SEE IF ANY ERRORS
1)		  PUSHJ	P,E.UFE		;YES--GO TELL USER
****
2)23	;;;;;	CAILE	T1,1
2)		
2)	;here where read directory (only dsk)		[a.blam 15-10-80]
2)		move	t1,sstnam(t4)	;restore astnam
2)		movem	t1,astnam	;
2)		move	t1,sstext(t4)	;restore astext
2)		movem	t1,astext	;
2)		move	t1,sfgch(t4)	;restore flag uuo de chanio
2)		movem	t1,afgch	;
2)		movsi	t1,'*  '	;scan *.*
2)		movem	t1,achani+1	;
2)		movem	t1,achani+2	;
2)		move	t1,[-200,,.wldbf]
2)		movem	t1,achani	;setup buffer
2)		move	t1,[.chufd,,wc]
2)		chanio	t1,achani	;read directory
2)		jrst	wldrie		;error
2)		jrst	wildrk		;ok
2)	;end [a.blam]
2)	WILDRI:	USETI	WC,(T1)		;POSITION FILE (only dta)
2)		MOVE	T1,[-200,,.WLDBF-1]
2)		MOVEI	T2,0		;SETUP DUMP LIST
2)		IN	WC,T1		;READ BUFFER
2)		  JRST	WILDRK		;OK
2)	wldrie:	STATZ	WC,IO.ERR	;SEE IF ANY ERRORS
2)		  PUSHJ	P,E.UFE		;YES--GO TELL USER
**************
1)24		JRST	WILDN		;AND GO READ FOR FILES
****
2)23		move	t1,achcpt	;# of entries block	[a.blam 15-10-80]
2)		aosg	firsrd(t4)	;first			[a.blam 15-10-80] 
2)		movem	t1,bufcnt(t4)	;yes			[a.blam 15-10-80]
2)		JRST	WILDN		;AND GO READ FOR FILES
File 1)	DSK:WILD.345	created: 1343 25-Sep-83
File 2)	DSK:WILD.NEW	created: 1530 25-Sep-83

**************
1)25		PUSHJ	P,.CNVDT##	;CONVERT TO INTERNAL FORMAT
1)		MOVEM	T1,FLCRDT	;STORE FOR LOOP			[300]
1)		MOVE	T1,AGLOOK	;POINT TO LOOKUP BLOCK		[300]
1)		MOVE	T2,.RBEXT(T1)	;GET ACCESS DATE		[161]
1)		ANDX	T2,RB.ACD	;REMOVE JUNK			[161]
1)		MOVEI	T1,0		;CLEAR TIME			[161]
1)		PUSHJ	P,.CNVDT##	;CONVERT TO INTERNAL FORMAT	[161]
****
2)24		PUSHJ	P,.CNDCT##	;CONVERT TO DEC FORMAT		[A.BLAM 15-10-80]
2)		PUSHJ	P,.CNVDT##	;CONVERT TO INTERNAL FORMAT
2)		MOVEM	T1,FLCRDT	;STORE FOR LOOP			[300]
2)		MOVE	T1,AGLOOK	;POINT TO LOOKUP BLOCK		[300]
2)		MOVE	T2,.RBEXT(T1)	;GET ACCESS DATE		[161]
2)		ANDX	T2,RB.ACD	;REMOVE JUNK			[161]
2)		MOVEI	T1,0		;CLEAR TIME			[161]
2)		PUSHJ	P,.CNDCT##	;CONVERT TO DEC FORMAT		[A.BLAM 15-10-80]
2)		PUSHJ	P,.CNVDT##	;CONVERT TO INTERNAL FORMAT	[161]
**************
1)57	ENDERR==.-1
****
2)56	FUSN:	BLOCK	2		;CURRENT USERNAME 			[A.BLAM 15-10-80]
2)	FHOS:	BLOCK	1		;CURRENT HOST				[A.BLAM 15-10-80]
2)	ENDERR==.-1
**************
1)58	.WLDBF::BLOCK	200		;BUFFER FOR READING DIRECTORIES
1)	DVCH:	BLOCK	1		;LAST DEVICE CHARACTERISTICS
1)	LWAZER==.-1	;END OF CLEARED AREA
****
2)57	sstnam:	block	.fxlnd+1	;save du startname [a.blam 15-10-80]
2)	sstext:	block	.fxlnd+1	;save du startext [a.blam 15-10-80]
2)	sfgch:	block	.fxlnd+1	;save du flag uuo chanio [a.blam 15-10-80]
2)	.WLDBF::BLOCK	200		;BUFFER FOR READING DIRECTORIES
2)	DVCH:	BLOCK	1		;LAST DEVICE CHARACTERISTICS
2)	firsrd:	block	.fxlnd+1	;flag first read of block of directry [a.blam 15-10-80]
2)	;area for chanio		[a.blam 15-10-80]
2)	achani:	block	4		; xwd -200,.wldbf  sixbit/*/ sixbit/*/ z
2)	afgch:	block	1		;
2)	astnam:	block	1		;
2)	astext:	block	1		;
2)	achcpt:	block	1		;
2)	LWAZER==.-1	;END OF CLEARED AREA
**************
    