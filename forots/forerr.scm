File 1)	DSK:FORERR.DEC	created: 2027 01-May-85
File 2)	DSK:FORERR.MAC	created: 1714 20-May-85

1)1	;***COPYRIGHT 1972,1973,1974,1975,1976,1977 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***
****
2)1	;[JMS] Changed CPU TIME message to TRUs.
2)	;***COPYRIGHT 1972,1973,1974,1975,1976,1977 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***
**************
1)1		PAGE
1)		SUBTTL FORERR ENTRY POINTS DEFINED BY ERRDIR IN (FORRM)
****
2)2	SUBTTL FORERR ENTRY POINTS DEFINED BY ERRDIR IN (FORRM)
**************
1)1		PAGE
1)		SUBTTL	TY%XXX GENERAL PURPOSE OUTPUT ROUTINES TO THE TTY
****
2)3	SUBTTL	TY%XXX GENERAL PURPOSE OUTPUT ROUTINES TO THE  TTY
**************
1)1		PAGE
1)		SUBTTL TY%DDB ROUTINE TO DUMP THE DEVICE BLOCK INFO.
****
2)4	SUBTTL TY%DDB ROUTINE TO DUMP THE DEVICE BLOCK INFO.
**************
1)1		PAGE
1)		SUBTTL ERROR MESSAGE PROCESSOR
****
2)5	SUBTTL ERROR MESSAGE PROCESSOR
**************
1)1		PAGE
1)		SUBTTL SYS ERROR PROCESSOR
****
2)6	SUBTTL SYS ERROR PROCESSOR
**************
1)1		OUTSTR	[ASCIZ /
1)	END OF EXECUTION
1)	CPU TIME: /]
1)		SETZ	T0,		;ASK FOR OUT RUNTIME
1)		RUNTIME	T0,		;GET THE TOTAL RUNTIME
1)		SUB	T0,RUN.TM(P4)	;MINUS THE STARTING TIME
1)		PUSHJ	P,TY%TIM	;TYPE THE TIME OUT
1)		OUTSTR	[ASCIZ /	ELAPSED TIME: /]
1)		MSTIME	T0,		;GET THE TIME OF DAY
****
2)6	IFE FT$TYM,<
2)		OUTSTR	[ASCIZ /
2)	END OF EXECUTION
2)	CPU TIME: /]
2)	>  ;End of IFE FT$TYM
2)	IFN FT$TYM,<
2)		OUTSTR	[ASCIZ /
2)	End of execution
2)	TRUs: /]
2)	>  ;End of IFN FT$TYM
2)		SETZ	T0,		;ASK FOR OUT RUNTIME
2)		RUNTIME	T0,		;GET THE TOTAL RUNTIME
2)		SUB	T0,RUN.TM(P4)	;MINUS THE STARTING TIME
2)		PUSHJ	P,TY%TIM	;TYPE THE TIME OUT
2)		OUTSTR	[ASCIZ /	Elapsed time: /]
2)		MSTIME	T0,		;GET THE TIME OF DAY
File 1)	DSK:FORERR.DEC	created: 2027 01-May-85
File 2)	DSK:FORERR.MAC	created: 1714 20-May-85

**************
1)1		PAGE
1)		SUBTTL OPN ERROR PROCESSOR
****
2)7	SUBTTL OPN ERROR PROCESSOR
**************
1)1		PAGE
1)		SUBTTL APR ARITHMETIC FAULT ERROR PROCESSOR
****
2)8	SUBTTL APR ARITHMETIC FAULT ERROR PROCESSOR
**************
1)1		PAGE
1)		SUBTTL LIB LIBRARY ERROR FAULT PROCESSOR
****
2)9	SUBTTL LIB LIBRARY ERROR FAULT PROCESSOR
**************
1)1		PAGE
1)		SUBTTL DAT DATA ERROR FAULT PROCESSOR
****
2)10	SUBTTL DAT DATA ERROR FAULT PROCESSOR
**************
1)1		PAGE
1)		SUBTTL	SPECIAL DATA ERROR FAULT PROCESSING
****
2)11	SUBTTL	SPECIAL DATA ERROR FAULT PROCESSING
**************
1)1		PAGE
1)		SUBTTL DEV DEVICE ERROR FAULT PROCESSOR
****
2)12	SUBTTL DEV DEVICE ERROR FAULT PROCESSOR
**************
1)1		PAGE
1)		SUBTTL MSG TYPE A MESSAGE OUT
****
2)13	SUBTTL MSG TYPE A MESSAGE OUT
**************
1)1		PAGE
1)		SUBTTL	ERROR MESSAGE FOR ARRAY OUT OF BOUNDS
****
2)14	SUBTTL	ERROR MESSAGE FOR ARRAY OUT OF BOUNDS
**************
1)1		PAGE
1)	SUBTTL	FORTRAN TRACE ROUTINES
****
2)14	SUBTTL	FORTRAN TRACE ROUTINES
**************
1)1		PAGE
1)		SUBTTL ERROR RECOVERY ROUTINES
****
2)15	SUBTTL ERROR RECOVERY ROUTINES
**************
 