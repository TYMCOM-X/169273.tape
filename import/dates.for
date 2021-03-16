	SUBROUTINE DATES
	IMPLICIT INTEGER(A-Z)

	PARAMETER	MAXTAB = 500
	PARAMETER	LINK   = 1		! LINK POINTER
	PARAMETER	LENGTH = 2		! LENGTH OF ITEM
	PARAMETER	LINNUM = 3		! WHAT LINE TO PUT IT ON
	PARAMETER	NSTART = 4		! WHERE THE NAME STARTS
	PARAMETER	NSIZE  = 18		! NSTART + 14 = 15 FOR NAME

	INTEGER MMAX(12),MADD(12),MONTH(12)
	INTEGER WEEK,WEEKLY(7)
	INTEGER TABENT,INF(NSIZE,MAXTAB)
	INTEGER IFILES(16),DDEV
	INTEGER DD,MM,YY,NAME(15),SET(366)
	INTEGER PASS,LEAP,ILEAP,L0,L1
	INTEGER INDEX,IDIX
	INTEGER DAY,FDAY,GDAY,IDAY,JDAY

	COMMON /DAY/DAY/GDAY/GDAY/FDAY/FDAY
	COMMON /NAME/NAME/SET/SET
	COMMON /TABENT/TABENT/INF/INF
	COMMON /LEAP/LEAP
	COMMON /IO/CAL,SYS,LIST
	COMMON /MADD/MADD/MMAX/MMAX
	COMMON /IYEAR/IYEAR,YEAR
	COMMON /WEEKLY/WEEKLY

	DATA DDEV/'DSK'/
	DATA MMAX/31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31/
	DATA MADD/00,31,59,90,120,151,181,212,243,273,304,334/
	DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
	1	   'JUL','AUG','SEP','OCT','NOV','DEC'/
	DATA WEEKLY/'SUN','MON','TUE','WED','THU','FRI','SAT'/
	TYPE 10,IYEAR				! TYPE A REMINDER
10	FORMAT(/1X,'Current Year:',I5)

70	TABENT = 1				! INITIAL TAB ENTRY

	DO 75 I = 1,365 + LEAP			! CLEAR SET ARRAY
75	SET(I) = 0
	DO 80 I = 1,MAXTAB			! CLEAR DATA ARRAY
	INF(LINK,  I) = 0			! CLEAR INITIAL LINK
	INF(LENGTH,I) = 0			! SET LENGTH OF NAME TO 0
	INF(LINNUM,I) = 0			! SET LINE FOR DATE TO 0
	DO 80 J = NSTART,NSIZE
80	INF(J,I) = ' '

	PASS = 0				! SETUP FOR INITIAL PASS
90	TYPE 91
91	FORMAT(1X,'What date file? '$)
	READ(5,92)IFILES
92	FORMAT(16A5)
	IF (IFILES(1).EQ.'none') IFILES(1) = 'NONE'
	IF (IFILES(1).EQ.'done') IFILES(1) = 'DONE'
	IF (IFILES(1).NE.'NONE'  .AND.  IFILES(1).NE.'DONE') GO TO 95
	IF (IFILES(1).EQ.'NONE') TYPE 93
	IF (IFILES(1).EQ.'DONE') TYPE 94
93	FORMAT(1X,'No holidays will be marked on this calendar')
94	FORMAT(1X,'Thank you, I will now create your calendar')
	RETURN

95	IF (PASS.NE.0  .AND.  IFILES(1).EQ.' ') RETURN
	DDEV = 'DSK'				! FIRST, RESET ASSUME DSK:
	IF (IFILES(1).EQ.' ') DDEV='PUB'	! IF BLANK, PUB: IS DEFAULT
	OPEN(UNIT=SYS,DEVICE=DDEV,FILE='BIGCAL.INF',
	1	ACCESS='SEQIN',DIALOGUE=IFILES)
	PASS = PASS +1				! COUNT THE PASSES THROUGH



100	READ(SYS,101,END=1000)DD,MM,YY,LIN,NAME
101	FORMAT(I,1A3,I,I,15A1)

	IF (DD.EQ.0 .AND. MM.EQ.' ' .AND. YY.EQ.0) GO TO 100	![11]
!	IF (DD.EQ.0) GO TO 100			![11 REMOVE] IGNORE BLANK LINES

	L0  = 0
	L1  = 0
	MM = MM .AND. "576773777776		! CLEAR 3 LOWERCASE BITS

	IF (MM.EQ.'DAY') GO TO 150		![13] CYCLICAL FUNCTIONS

	Do 105 IDIX = 1,12			![12] Check months 1-12
105	IF (MM.EQ.MONTH(IDIX)) GOTO 110		![12] If found?  Fantastic
						![12]    else try again

	DO 106 IDIX=1,7				![11] SEE IF A WEEKLY FUNCTION
106	IF (MM.EQ.WEEKLY(IDIX)) GOTO 120	![11] YES, SO WE'RE FINE

	TYPE 107,DD,MM,YY
107	FORMAT(1X,'Date ',I2,'-',A3,'-',I4,
	1' is not in the form DD-MMM-YYYY')
	GO TO 100


110	IF (	YY .NE. 0	.AND.		![12] ANY YEAR??
	1	YY .NE. IYEAR 	.AND.		![12] THIS YEAR??
	2	-1*YY .GT. IYEAR) GO TO 100	![12][5] EARLIER? NO? SO LOOP!

	IDIX = GREGOR(DD,IDIX,IYEAR)		![12] GET THE DATE.
	IF (IDIX .GT. 0) CALL SETDAY(IDIX-GDAY+1, LIN)	![12] SETUP LINE
	GO TO 100



120	IF (YY.GT.0 .AND. YY.LE.12) GO TO 135	![11] A SINGLE MONTH ???
	DO 140 YY = 1,12			![11] MONTHS 1-12
135	L0  = 0					![11] Set FEB days = +
	L1  = 0					![11] Set Days after Feb = +
	IF (YY .EQ. 2) L0 = LEAP		![11] IF FEBRUARY, MAYBE 1 MORE
	IF (YY .GT. 2) L1 = LEAP		![11] IF AFTER FEB, ADD 1 DAY ?
	IDAY = FDAY + MADD(YY) + L1		![11] ADD MONTH OFFSET ? LEAPS
	JDAY = IDAY - (IDAY / 7) * 7		![11] MAKE IT A WEEKDAY
	IF (JDAY .LE. 0) JDAY = 7		![11] RANGE 1-7
	INDEX = IDIX				![11] COPY
	IF (IDIX .LT. JDAY) INDEX = IDIX + 7	![11] ADD A WEEK FOR SHORT DAYS

	NWEEKS = ((MMAX(YY) + L0 - (INDEX + 1 - JDAY)) / 7) + 1
	IDD = DD				![11] SAVE NTH WHATEVER
	IF (DD .LT. 0) IDD = NWEEKS + DD + 1	![11] IF -NTH WEEK, MAKE IT +++

	WEEK = 0				![11] INITIALIZE WEEK COUNTER
	DO 140 JULIAN=INDEX-JDAY+1+MADD(YY)+L1,MADD(YY)+L1+MMAX(YY)+L0,7
	WEEK = WEEK + 1				![11] BUMP WEEKS AS WE DO THEM
	IF (IDD .NE. WEEK   .AND.
	1	DD .NE. 0) GO TO 140		![11] SKIP THIS STUFF IF BAD

	CALL SETDAY(JULIAN,LIN)			![11] PRINT THE MESSAGE

D	type 139,JULIAN,idix,jday,madd(yy),mmax(yy),week,dd
D139	format(1x,'JULIAN:',I3,' D:',i3,' JD:',i1,
D	1	' MA:',i3,' MX:',i2,' w:',i1,' dd:',i3)

140	CONTINUE
	GO TO 100


150	READ(SYS,101,END=165,ERR=165)BDD,BMM,BYY
	READ(SYS,101,END=165,ERR=165)EDD,EMM,EYY

	IF (Byy .EQ. 0) Byy = IYEAR	! Zero year means Every (This)
	IF (Byy .LT. 0) Byy = Byy * -1	! No such thing as - years
	IF (Eyy .EQ. 0) Eyy = IYEAR	! Zero year means Every (This)
	IF (Eyy .LT. 0) Eyy = Eyy * -1	! No such thing as - years

	Bmm = Bmm .and. "576773777776	! Correct for Lower case
	Emm = Emm .and. "576773777776	! Correct for Lower case

	Do 151 Bdix = 1,12		! Find the Month name
151	If (BMM .EQ. MONTH(BDIX)) GO TO 152
	GO TO 160
152	Do 153 Edix = 1,12		! Try the ending one
153	If (EMM .EQ. MONTH(EDIX)) GO TO 154
	GO TO 160
154	Bdate = Gregor(Bdd,Bdix,Byy)	! Get an absolute date (BEGIN)
	If (Bdate .GT. Gday+365+Leap) Go to 100	! No work if Done!
	Edate = Gregor(Edd,Edix,Eyy)	! For both the BEGIN and END

	Sdate = DD			! Document step size
!
!	Next 3 lines makeup this statement which compiles wrong
!
!	If (Bdate.LT.Gday) Bdate = Bdate + ((Gday-Bdate)/Sdate)*Sdate + Sdate
!
	Ydate = ((Gday-Bdate)/Sdate) * Sdate
	Zdate = Bdate + Sdate + Ydate
	If (Bdate .LT. Gday) Bdate = Zdate

	If (Edate.GT.Gday+365+Leap) Edate = Gday+365+leap

	Do 155 Idix = Bdate,Edate,Sdate	! Make a loop Begin==>End
155	Call SETDAY(Idix-Gday+1,LIN)	!     Ok! Then Set it up!
	GO TO 100			! Done!


160	Type 161,DD,MM,YY,LIN,NAME,BDD,BMM,BYY,EDD,EMM,EYY
161	FORMAT(/1x,'Illegal format for "DAY" appointment entry'
	1	/1x,I3,1x,a5,1x,i5,1x,i5,5x,15A1,
	2	2(/2x,i3,1x,a5,1x,i5))
	Go To 100

165	Type 166
166	Format(/1x,'End of File encountered while reading "DAY"',
	1	/1x,'appointment entry.  Please Check Data file.')
	Go To 1000			! Take normal end of file


1000	CLOSE(UNIT=SYS)
	IF (IFILES(1).NE.' ') GO TO 90		! WHEN BLANK WE'RE DONE
						! ELSE GET A NEW FILE???
	CONTINUE
D	DO 1100 I=1,365 + LEAP
D	IF (SET(I).EQ.0) GO TO 1100
D	J = SET(I)
D	GOTO 1020				! SKIP NEXT INSTRUCTION
D1010	J = INF(LINK,J)				! GET NEXT ADDRESS TO PRINT
D1020	TYPE 1030,I,INF(LENGTH,J),
D	1	(INF(N,J),N=NSTART,INF(LENGTH,J)+NSTART-1)
D1030	FORMAT(1X,'The ',I4,'th day (',I3,') is: ',15A1)
D	IF (INF(LINK,J).NE.0) GO TO 1010	! IF MORE, CONTINUE
D1100	CONTINUE
D	RETURN					! GO BACK TO CALLER
D	CALL EXIT
	END


