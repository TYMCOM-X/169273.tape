	SUBROUTINE	SETWEEK

	PARAMETER	MAXTAB = 500	! CO-ORDINATE WITH DATES.FOR
	PARAMETER	LINK   = 1	!    LINK ITEM
	PARAMETER	LENGTH = 2	!    LENGTH OF LIST
	PARAMETER	LINNUM = 3	!    WHAT LINE TO USE
	PARAMETER	NSTART = 4	!    WHERE TEXT STARTS
	PARAMETER	NSIZE  = 18	! NSTART + 15 - 1

	INTEGER	DAY,JDAY,DATE,DAYS
	Integer L0,L1
	INTEGER SET(366),INF(NSIZE,MAXTAB)
	INTEGER ID(15,7,7)
	LOGICAL	L,LINFLG,IUSE(7,7)

	COMMON/JDAY/JDAY/SET/SET/INF/INF/ID/ID
	COMMON/FDAY/FDAY/DAY/DAY/DATE/DATE/DAYS/DAYS
	COMMON/NUMBER/NUMBER(0/9)

	DATA (NUMBER(I),I=0,9)/'0','1','2','3','4','5','6','7','8','9'/



	DO 10 I=1,7				! CLEAR ID ARRAY
	DO 10 J=1,7
	IUSE(J,I) = 0
	DO 10 K=1,15
10	ID(K,J,I)=' '

	IF (DATE.GT.DAYS)RETURN			! TOO MANY, GOOD BYE!

20	IF (DAY.GT.7) GO TO 60			! IF AT END OF WEEK, BYE!
	IF (DATE.GT.DAYS) RETURN		! IF AT END OF MONTH BYE!!!
	ID(1,DAY,1)=NUMBER(DATE/10)		! SET HIGH ORDER DATE
	ID(2,DAY,1)=NUMBER(DATE-(DATE/10)*10)	! SET LOW ORDER DATE
	IF (ID(1,DAY,1).EQ.'0')ID(1,DAY,1)=' '	! FIX 0 HIGH ORDER TO A SPACE
	IF (SET(JDAY).EQ.0) GO TO 50		! IF NO MESSAGE SKIP!

	SETUP = SET(JDAY)			! GET INF ENTRY
30	IF (INF(LENGTH,SETUP).GT.0 . AND. INF(LINNUM,SETUP).NE.0) GO TO 35
31	SETUP = INF(LINK,SETUP)			!  NO, TRY NEXT.
	IF (SETUP.EQ.0) GO TO 40		! IF ZERO... IGNORE
	GO TO 30				! ...

35	LINFLG = .FALSE.			![14] Default flag to false
	L0 = INF(LINNUM,SETUP)			![14] Copy Line number for test
	If (L0 .LT. 0) LINFLG = .TRUE.		![14] IF Minus then Center it
	If (IABS(L0) .GT. 7) L0 = 0		![14] If out of bounds Set = 0
	INF(LINNUM,SETUP) = IABS(L0)		![14] Make Positive

	IF (IUSE(DAY,INF(LINNUM,SETUP))) GO TO 31 ! OOPS, MUST CHANGE TO A 0
	IUSE(DAY,INF(LINNUM,SETUP)) = .TRUE.	! TELL ME WE ARE IN USE
	L0 = 0					! OFFSET = O
	L1 = 15					![14] Size max to 15
	L  = .FALSE.				! .NOT. LINE 1
	IF (INF(LINNUM,SETUP).EQ.1) L = .TRUE.	!   TO TRUE!!! ?
	IF (L) L0 = 3				! OFFSET FOR RUN # 1
	IF (L) L1 = 12				![14] Shrink maximum size
	IF (L .AND. INF(LENGTH,SETUP).GT.12) INF(LENGTH,SETUP) = 12
	If (LINFLG) L0=L0+(L1-INF(LENGTH,SETUP))/2	![14] # Spaces
	DO 36 I=1,INF(LENGTH,SETUP)		! COPY ARRAY OVER
36	ID(I+L0,DAY,INF(LINNUM,SETUP))=INF(I+NSTART-1,SETUP)
D	TYPE 37,JDAY,DAY,INF(LINNUM,SETUP),
D	1	(ID(I,DAY,INF(LINNUM,SETUP)),I=1,15)
D37	FORMAT(1X,I3,'th day, (',I1,') Line',I2,2X,15A1)
	INF(LENGTH,SETUP) = 0			! SET STRING TO 0
	GO TO 30				! LOOP THROUGH ALL ENTRIES

40	SETUP = SET(JDAY)			! GET INF ENTRY
41	IF (INF(LENGTH,SETUP).GT.0) GO TO 45	! IF NON-ZERO LENGTH
	SETUP = INF(LINK,SETUP)			!    TEST IT, ELSE NEXT!
	IF (SETUP.EQ.0) GO TO 50		! DONE!
	GO TO 41				! LOOP!

45	DO 47 I=1,7				! SEE WHICH LINE TO USE
	IF (I.EQ.1 .AND. INF(LENGTH,SETUP).GT.12) GO TO 47
	IF (.NOT.IUSE(DAY,I)) GO TO 48		! SKIP THIS IF FREE!
47	CONTINUE
	TYPE 46,JDAY
46	FORMAT(1X,'Error: Attempt to have 8 holidays on',I4)
	GO TO 50				!CANCEL

48	IUSE(DAY,I) = .TRUE.			! GOT ONE!
	LINFLG = .FALSE.			![14] Default flag to false
	If (INF(LINNUM,SETUP).LT.0) LINFLG = .TRUE.	![14] IF Minus then Center it
	L0 = 0					! 0 IS NORMAL
	IF (I.EQ.1) L0 = 3			! OFFSET
	L1 = 15					![14] Size max to 15
	If (I.EQ.1) L1 = 12			![14] Shrink spacing size
	If (LINFLG) L0=L0+(L1-INF(LENGTH,SETUP))/2	![14] # Spaces
	DO 49 J=1,15-L0
49	ID(J+L0,DAY,I) = INF(J+NSTART-1,SETUP)	! COPY THE STRING
D	TYPE 37,JDAY,DAY,I,(ID(J,DAY,I),J=1,15)
	INF(LENGTH,SETUP) = 0			! CLEAR THE STRING SIZE
	GO TO 41				! NOW, LOOP

50	DATE=DATE+1				! INCREMENT DATE
	DAY=DAY+1				! INCREMENT DAY OF WEEK
	JDAY=JDAY+1				! INCREMENT JULIAN DATE
D	TYPE 51,DATE,DAY,JDAY
D51	FORMAT(3I)
	GO TO 20

60	DAY=1					! END OF WEEK, RESET NEXT
	RETURN
	END
    