	INTEGER	I,J,ERR,BIOBUF(101)

	CALL AXEBM(ERR)
	IF (ERR) STOP 1

C		SET MODE ="10 FOR TIMEOUT, ="14 FOR NO TIMEOUT
C		NOTE, NO TIMEOUT MEANS YOU'LL NEVER GET OUT!
	CALL AXCFS("10)

2	BIOBUF(1)=400
	CALL AXIBW(BIOBUF,ERR)
	IF (ERR.NE.0) GOTO 3
	J=(BIOBUF(1)+7)/4
	TYPE 21,(BIOBUF(I),I=1,J,1)
21	FORMAT(1H+6O13/)
	GOTO 2

3	CALL AXTBI
	CALL AXLBM

	END
   