File 1)	DSK:MAILIB.OLD	created: 0445 09-JUL-87
File 2)	DSK:MAILIB.SAI	created: 1847 26-SEP-88

1)1	!	 9-Jul-87 WRS	enforce 100,000 character size limit in maiFEM
****
2)1	!	26-SEP-88 JMS	Set maiHost to "TRW" if running on "S33-P035".
2)	!	 9-Jul-87 WRS	enforce 100,000 character size limit in maiFEM
**************
1)6	    maiHost := adr:Host[maiMyAddress] :=
1)		cvstr(calli(!xwd(0,'11),'41))[1 to 1]&
****
2)6						! *HACK* so USER@TRW works ;
2)	    maiHost := adr:Host[maiMyAddress] :=
2)	        if(kequ(cvstr(calli(!xwd(0,'11),'41)),"S33-P")) then "TRW" else
2)		cvstr(calli(!xwd(0,'11),'41))[1 to 1]&
**************
1)21		    if kequ(adr:Host[a],adr:Host[maiMyAddress][2 to inf])
1)			then return( CheckLocal( adr:User[a] ))
****
2)21							! Allow for TRW here ;
2)		    if kequ(adr:Host[a],adr:Host[maiMyAddress][2 to inf])
2)		    or kequ(adr:Host[a],adr:Host[maiMyAddress])
2)			then return( CheckLocal( adr:User[a] ))
**************
      