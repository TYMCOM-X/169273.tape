1	extend
 
010 DIM Z%(512)
090	ON ERROR GOTO 8000
100 PRINT "SAV file"; \ INPUT LINE F$ \ F$=CVT$$(F$,4%)
 	\ F$=F$+".SAV" IF INSTR(1%,F$,".")=0%
110 OPEN F$+"/RONLY" FOR INPUT AS FILE 1%
120 x%=0%
130 open "A.ROM" for output as file 2%
135 open "B.ROM" for output as file 3%
150 print #2, "2716,2048,8,000,FFF,0,xxx"
160 print #3, "2716,2048,8,000,FFF,0,xxx"
200 for i%=2% to 9%
210 GET #1%,RECORD i%
212 field #1%, 512% as z$
215 change z$ to z%
216 for j%=1% to 509% step 2%
220   x% = x% + z%(j%) + z%(j%+1%) and 255%
225   print #2, fnh$(z%(j%),2%)
230   print #3, fnh$(z%(j%+1%),2%)
240   next j%
250 x% = x% + z%(511%)
255 if i% = 9% then z%(512%)=x% else x%=x%+z%(512%)
260 print #2, fnh$(z%(511%),2%)
265 print #3, fnh$(z%(512%),2%)
280 NEXT i%
310 close 1%,2%,3%
320 goto 32767
7000	!
 		FNH$ - Hexidecimal conversion function
 
7010	def fnh$(t%,t1%) &
\	t$ = string$(t1%,ascii("0")) &
\	rset t$ = t$+mid("0123456789ABCDEF",((t%/16%^t2%) and 15%)+1%,1%) &
		for t2% = t1%-1% to 0% step -1% &
\	fnh$ = t$ &

7020	fnend
 
8000	IF ERL<>210 OR ERR<>11% THEN ON ERROR GOTO 0
8010	RESUME 8020
8020	FOR J%=i% TO 9%
8025	  print #2, fnh$(0%,2%) for q%=1% to 256%
8030	  print #3, fnh$(0%,2%) for q%=1% to 255%
8032	  if j%=9% then print #3, fnh$(x%,2%) else print #3, fnh$(0%,2%)
8040	NEXT J%
8050	goto 310
32767	end
                                                                                                                                                                         