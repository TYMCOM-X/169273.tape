File 1)	DSKU:TAPUUO.DEC[702,10]	created: 1034 04-Jan-84
File 2)	DSKU:TAPUUO.MAC[702,10]	created: 1715 15-Dec-84

1)1		SEARCH	F,S
****
2)1	;Includes DPM's patch for PSI on reel switch
2)		SEARCH	F,S
**************
1)38	TPMLSU:	MOVE	P2,T2		;SAVE PREDECESSOR
1)		PUSHJ	P,TPSTAT		;REPORT STATISTICS
1)		HLRZ	T1,P4		;GET USERS LENGTH
****
2)38	;[DPM] PSI on reel switch
2)	TPMLSU:	MOVE	P2,T2		;SAVE PREDECESSOR
2)		PUSHJ	P,TPSTAT	;REPORT STATISTICS
2)	IFN FTPI,<
2)		PUSH	P,F		;[DPM]
2)		HLRZ	T1,TDVUDB##(F)	;[DPM] Point to the TUB
2)		HRRZ	F,TUBDDB##(T1)	;[DPM] And now the real DDB
2)		PUSHJ	P,PSIRSW##	;[DPM] Signal the reel switch
2)		POP	P,F		;[DPM]
2)	>  ;End of IFN FTPI
2)		HLRZ	T1,P4		;GET USERS LENGTH
**************
1)38		MOVSI	T1,DVMTA	;GRNTEE IT IS A MTA
****
2)38		CAMN	P1,F		;[DPM] Check if PULSAR switched to same drive
2)		 JRST	CPOPJ1		;[DPM] Yes, call to PSIRSW accomplished
2)		MOVSI	T1,DVMTA	;GRNTEE IT IS A MTA
**************
  