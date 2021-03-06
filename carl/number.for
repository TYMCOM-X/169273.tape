      Program Number
      Real    TEMPS
      Integer JHUN,JTEN,JDIG,TEMP
      Integer STRING(132),SCOUNT
      Integer ONETAB(0/8,0/19),TENTAB(0/12,0/9)
      Integer MINUS(0/5),CENT(0/4),ET(0/2),POINT(0/7)

      Data ONETAB/
     *  4,'Z','e','r','o',4*' ',
     *	2,'U','n',6*' ',
     *	4,'D','e','u','x',4*' ',
     *	5,'T','r','o','i','s',3*' ',
     *	6,'Q','u','a','t','r','e',2*' ',
     *	4,'C','i','n','q',4*' ',
     *	3,'S','i','x',5*' ',
     *	4,'S','e','p','t',4*' ',
     *	4,'H','u','i','t',4*' ',
     *	4,'N','e','u','f',4*' ',
     *	3,'D','i','x',5*' ',
     *	5,'O','n','z','e','s',3*' ',
     *	6,'D','o','u','z','e','s',2*' ',
     *	6,'T','r','e','i','z','e',2*' ',
     *	8,'Q','u','a','t','o','r','z','e',
     *	6,'Q','u','i','n','z','e',2*' ',
     *	5,'S','e','i','z','e',3*' ',
     *	8,'D','i','x','-','S','e','p','t',
     *	8,'D','i','x','-','H','u','i','t',
     *	8,'D','i','x','-','N','e','u','f'/

      Data TENTAB/
     *	 4,'Z','e','r','o',8*' ',
     *	 3,'D','i','x',9*' ',
     *	 5,'V','i','n','g','t',7*' ',
     *	 5,'T','r','e','n','t',7*' ',
     *	 8,'Q','u','a','r','a','n','t','e',4*' ',
     *	 9,'C','i','n','q','u','a','n','t','e',3*' ',
     *	 7,'S','o','i','x','a','n','t',5*' ',
     *	 7,'S','o','i','x','a','n','t',5*' ',
     *	12,'Q','u','a','t','r','e','-','V','i','n','g','t',
     *	12,'Q','u','a','t','r','e','-','V','i','n','g','t'/

      Data MINUS/5,'M','o','i','n','s'/
      Data CENT/4,'C','e','n','t'/
      Data ET/2,'e','t'/
      Data POINT/7,'v','i','r','g','u','l','e'/
   75 READ (5,100,END=500)TEMPS			! READ IN THE NUMBER
      IPASS = 0					! MARK THIS FIRST TIME
      TEMP = IFIX(TEMPS)			! COPY NUMBER FOR WORKING
      FRACT = ABS(TEMPS - FLOAT(TEMP))		! COPY FRACTION FOR FINIS
      IF (ABS(FRACT + SIGN(.009,TEMPS)).LT.1.0) GO TO 95
      TEMP = TEMP + 1
      FRACT = FRACT - 1.0
  100 FORMAT (F12.2)

   95 SCOUNT = 0				! INITIALIZE POINTER TO 0
      DO 110 I=1,132				! INITIALIZE ARRAY TO SPACES
      STRING(I)=' '
  110 CONTINUE

      IF (TEMPS .NE. 0.0) GO TO 120		! Catch ZERO!!!
      CALL STRSET(SCOUNT,STRING,TENTAB,' ')	! Say 'Zero'
      GO TO 160					! Skip to FRACTION part

  120 IF (IPASS .NE. 0) GO TO 130		! Catch NEGATIVE!!!
      IF (TEMPS .GE. 0) GO TO 130		! But only on first pass
      TEMP = TEMP * -1				! CHANGE SIGN OF NUMBER
      CALL STRSET(SCOUNT,STRING,MINUS,' ')	! PUT 'Minus' INTO STRING

  130 JHUN = TEMP / 100				! HUNDREDS
      IF (JHUN .GT. 1) CALL STRSET(SCOUNT,STRING,ONETAB(0,JHUN),' ')
      IF (JHUN .GE. 1) CALL STRSET(SCOUNT,STRING,CENT,' ')
      JTEN = (TEMP - (JHUN * 100)) / 10		! TENS
      JDIG = TEMP - JHUN * 100 - JTEN * 10	! DIGITS

      SPACE = ' '				! Default to SPACE
      IF (JTEN.NE.1 .AND. JTEN.NE.7 .AND. JTEN.NE.9) GO TO 150
      SPACE = '-'				! Change to Dash
      JDIG = JDIG + 10				! Make Zero-Neuf > Dix etc.
  150 IF (JDIG .GT. 1) SPACE = '-'		! Pas de '-' si le numeral Un ou Zero
      IF (JTEN .GT. 1) CALL STRSET(SCOUNT,STRING,TENTAB(0,JTEN),SPACE)
      IF (JTEN.GT.1 .AND. JDIG.EQ.1) CALL STRSET(SCOUNT,STRING,ET,' ')
      IF(IPASS.GT.0.AND.JTEN.LT.1)CALL STRSET(SCOUNT,STRING,TENTAB,' ')
      CALL STRSET(SCOUNT,STRING,ONETAB(0,JDIG),' ')

  160 IF (FRACT .EQ. 0.) GO TO 200
      TEMP = IFIX( (FRACT + .0099) * 100.0) ! Multiply fraction by 100.00
      FRACT = 0.0
      IPASS = 1
      CALL STRSET(SCOUNT,STRING,POINT,' ')	! Add 'point' string to text
      GO TO 120

  200 WRITE (3,205)TEMPS,(STRING(I),I=1,SCOUNT)	! Write it out
      WRITE (5,205)TEMPS,(STRING(I),I=1,SCOUNT)	! Write it out
  205 FORMAT (1x,F8.2,5X,80A1)
      GO TO 75

  500 STOP
      END
	SUBROUTINE STRSET(SCOUNT,STRING,CHARS,SPACE)
	IMPLICIT INTEGER (A-Z)
	INTEGER STRING(132),CHARS(0/20)

	DO 10 I = 1,CHARS(0)
	SCOUNT = SCOUNT + 1
	STRING(SCOUNT) = CHARS(I)
10	CONTINUE
	SCOUNT = SCOUNT + 1
	STRING(SCOUNT) = SPACE
	RETURN
	END
  