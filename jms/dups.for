	PROGRAM DUPS ! Read INPUT.DAT, write DUPS.DAT and UNIQUE.DAT
	DIMENSION CURR(80),PREV(80)	!FINDIT uses 80 columns

	ISTART = 54 ; IEND = ISTART+6	!FINDIT's checksum in cols 54-59

	OPEN(UNIT=1,FILE='INPUT.DAT',ACCESS='SEQIN')
	OPEN(UNIT=2,FILE='DUPS.DAT',ACCESS='SEQOUT')
	OPEN(UNIT=3,FILE='UNIQUE.DAT',ACCESS='SEQOUT')
	MULTI = .FALSE.
	NDUP = 0

100	DO 105 I=1,132
105	PREV(I) = CURR(I)		!Remember previous line
	READ(1,110,END=200)CURR		!Get new line
110	FORMAT(132A1)

	DO 120 I=ISTART,IEND
	IF(CURR(I).NE.PREV(I)) GOTO 150	!If different
120	CONTINUE

130	IF(.NOT.MULTI) WRITE(2,110)PREV	!Match, output previous line
	WRITE(2,110)CURR		!Output 2nd line of match
	IF(.NOT.MULTI) NDUP = NDUP + 1	!1st in a set of matches
	NDUP = NDUP + 1			!2nd or succeeding match
	MULTI = .TRUE.			!Don't output PREV again
	GOTO 100

150	IF(.NOT.MULTI) WRITE(3,110)PREV	!Previous line was unique
	MULTI = .FALSE.
	GOTO 100

200	IF(.NOT.MULTI) WRITE(3,110)PREV	!Previous line was unique
	TYPE 210,NDUP
210	FORMAT(I6,' duplicates found')
	CLOSE(UNIT=1)
	CLOSE(UNIT=2)
	CLOSE(UNIT=3)
	END
   