
	Program	CDT

	Integer Month, Day, Year, Days
	Integer StartM, StartD, StartY
	Integer StopM,  StopD,  StopY, StopDy

	Real Bal, Avg, AvgBal, Trans

	Type 1
1	Format( ' Start date (M,D,Y): '$ )
	Accept 2, StartM, StartD, StartY
2	Format( 3I )
	Type 3
3	Format( ' Stop date (M,D,Y):  '$ )
	Accept 2, StopM, StopD, StopY
	Type 4
4	Format( /' Beginning Balance:  '$ )
	Accept 5,Bal
5	Format( F )

	AvgBal = 0.0
	Month = StartM
	Day = StartD
	Year = StartY
	StopDy = ((StopY*10000)+(StopM*100)+StopD)
	Days = 0

10	Type 11, Month, Day, Year
11	Format( /' Today: ',2(I2,'/'),I4,/ )

12	Type 13
13	Format( ' Transaction: '$ )
	Accept 5,Trans

	Bal = Bal + Trans
	If ( Trans .ne. 0.0 ) Go To 12
	AvgBal = AvgBal + Bal

	Days = Days + 1
	Avg = AvgBal / Float(Days)

	Type 15, Bal, Avg, AvgBal, Days
15	Format( ' Balance: ',F8.2,' Average: ',F8.2,' T: ',F10.2,I4 )

	If ( ((Year*10000)+(Month*100)+Day) .ge. StopDy ) Go To 16
	Call IncDay( Month, Day, Year )
	Go To 10

16	Type 17
17	Format( /' End of SAIL Execution.'// )

	End

	Subroutine IncDay( M, D, Y )
	Implicit Integer (A-Z)

	Integer MMax(12)
	Data MMax/ 31,28,31,30,31,30,31,31,30,31,30,31 /

	D = D + 1
	L = 0
	If ( M .ne. 2 ) Go To 10
	If ( Y-((Y/4)*4) .ne. 0 ) Go To 10
	L = 1
	If ( Y-((Y/100)*100) .ne. 0 ) Go To 10
	L = 0
	If ( Y-((Y/400)*400) .ne. 0 ) Go To 10
	L = 1

10	If ( D-L .le. MMax( M ) ) Return
	D = 1
	M = M + 1
	If ( M .le. 12 ) Return
	M = 1
	Y = Y + 1
	Return
	End
    