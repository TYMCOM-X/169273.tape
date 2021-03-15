1	extend
 
010 DIM Z%(512)
090	ON ERROR GOTO 8000
100 PRINT "SAV file"; \ INPUT LINE F$ \ F$=CVT$$(F$,4%)
 	\ F$=F$+".SAV" IF INSTR(1%,F$,".")=0%
110 OPEN F$+"/RONLY" FOR INPUT AS FILE 1%
125 for k%=1% to 2%
130 F$="A" \ F$="B" if K%=2% \ F$=F$+".ROM"
135 open F$ for output as file 2%
160 PRINT #2%,"2716,2048,8,000,FFF,0,xxx"
200 FOR I%=2% TO 9%
210 GET #1%,RECORD I%
215 FIELD #1%,512% AS Z$
220 CHANGE Z$ TO Z%
260 PRINT #2%,FNH$(Z%(J%+K%),2%) FOR J%=0% TO 510% STEP 2%
280 NEXT I%
290 close 2%
300 next k%
310 close 1%
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
8020	FOR J%=I% TO 9%
8030	PRINT #2%,FNH$(0%,2%) FOR Q%=1% TO 256%
8040	NEXT J%
8050	GOTO 290
32767	end
                                  