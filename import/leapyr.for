	Integer Function LEAPYR(YEAR)
	Implicit Integer (A-Z)

	Leapyr = 0				! Initially non-leap year
	IF ((YEAR/4)  *  4 .EQ. YEAR) Leapyr=1	! LEAP EVERY 4 YEARS
	IF ((YEAR/100)*100 .EQ. YEAR) Leapyr=0	! EXCEPT EVERY 100
	IF ((YEAR/400)*400 .EQ. YEAR) Leapry=1	! BUT Leap EVERY 400
	Return
	End
    