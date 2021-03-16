	SUBROUTINE GETYR(IY,IOK,YEAR,OK)
	IMPLICIT INTEGER (A-Z)

	INTEGER IY,IOK,YEAR,OK
	DOUBLE PRECISION MON,MTABLE(0:12)
	COMMON/MTABLE/MTABLE
	DATA (MTABLE(I),I=0,12)/
	1	'Help  ','January','February','March ',
	2	'April ','May   ','June  ','July  ','August',
	3	'September','October','November','December'/


	GO TO 20				! Initially skip HELP message


10	TYPE  11				! ELSE, A <CR> GIVE INSTRUCT
	TYPE 12,MTABLE				! AND A LIST OF NAMES

11	FORMAT(/1X'Acceptable calendar years are positive numbers in'
	1/1X,'the range: 1-9999.  An optional Month-name may also be'
	2/1X,'entered on the same line.  If just the Month-name is'
	3/1X,'entered, the current year is assumed.  Month-names may be'
	4/1X,'shortened to any unique abbreviation, whereas "JA" will'
	5/1X,'work for January, but "JU" is ambiguous for either of'
	6/1X,'of June or July.  A list of legal Month-names follows:'/)

12	FORMAT(5X,A10,1X,A10,1X,A10,1X,A10,1X,A10)

20	TYPE	21
21	FORMAT(/1X,'Calendar Year? '$)
	CALL GETNAM(IY,MON,10)			! Get Year and Month

	IOK = MATCH(MON,MTABLE,13) - 1		! 0-12 READ AS 1-13
	IF (IOK .GT. 0) GO TO 50		! Good month.. go ahead
	IF (IOK.EQ.0 .OR.
	1  (IOK.EQ.-1 .AND. IY.EQ.0)) GO TO 10	! Give some help
	IF (IOK.EQ.-3)	TYPE 30,MON		! AMBIGUOUS ... TRY AGAIN
	IF (IOK.EQ.-2)	TYPE 31,MON		! NO MATCH ... ILLEGAL
	IF (IOK.LT.-1)  GO TO 20		! Fall through if blank
	IOK = 1					! ENTIRE YEAR - Jan = 1
	OK  = 12				! Through Dec = 12
	YEAR = IY				! Use the same years
	RETURN					! Done!

30	FORMAT(1X,'You have entered an Ambiguous Month-name, ',A10
	1/1X,'matches more than ONE entry.  Type "H" for Help')

31	FORMAT(1X,'You have entered an illegal Month-name, ',A10
	1/1X,'does not match any legal entries.  Type "H" for Help')


50	IF (IY .EQ. 0) IY = 1980		! Use current year
	BYR = IY * 100 + IOK			! MAKE YEAR.IOK A VALUE

60	TYPE	21
	CALL GETNAM(YEAR,MON,10)

	OK = MATCH(MON,MTABLE,13) - 1		! 0-12 READ AS 1-13
	IF (OK .GT. 0) GO TO 70			! Good month.. go ahead
	IF (OK.EQ.0) GO TO 10			! Give some help
	IF (OK.EQ.-3)	TYPE 30,MON		! AMBIGUOUS ... TRY AGAIN
	IF (OK.EQ.-2)	TYPE 31,MON		! NO MATCH ... ILLEGAL
	IF (OK.LT.-1)  GO TO 60			! Fall through if blank

70	IF (YEAR .GE. IY  .AND.
	1   OK .EQ. -1)  OK = 12		! USE DEC IF ONLY YEAR...
	IF (YEAR .EQ. 0) YEAR = IY		! DEFAULT TO CURRENT YEAR
	IF (OK   .EQ.-1) OK   = IOK		! IF STILL BLANK, USE THIS ONE
	EYR = YEAR * 100 + OK			! MAKE YEAR.OK A VALUE
	IF (EYR .GE. BYR) RETURN		! ALL RIGHT!! RETURN
	TYPE 80					! ELSE, WARN OF ILLEGAL DATE
	GO TO 60				! AND TRY AGAIN
80	FORMAT(1X,'You have entered a month/year prior to the start
	1 year.')

	End
