File 1)	DSKU:TD2KON.DEC[702,10]	created: 1046 04-Jan-84
File 2)	DSKU:TD2KON.MAC[702,10]	created: 1820 15-Dec-84

1)1	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
****
2)1	;Includes MCO [11528], returning write-locked status to LPTSPL
2)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
**************
1)19	CHKWLK:	PUSHJ	P,SNSANL	;ANALYZE SENSE BYTES
1)		TLNE	P2,(S1.FPR)	;IS UNIT WRITE PROTECTED?
1)		TLO	T1,RB.SLK	;YES, TELL TAPUUO
1)		JRST	DONE		;EXIT THROUGH COMMON CODE
****
2)19	;MCO 11528, 1-Dec-84 Software Dispatch
2)	CHKWLK:	PUSHJ	P,SNSANL	;ANALYZE SENSE BYTES
2)		TLNE	P2,(S1.FPR)	;IS UNIT WRITE PROTECTED?
2)	;[11528]TLO	T1,RB.SLK	;YES, TELL TAPUUO
2)		HRLI	T1,RB.SLK	;[11528] YES, TELL TAPUUO
2)		JRST	DONE		;EXIT THROUGH COMMON CODE
**************
1)20	 
1)	SBRINT:	PUSHJ	P,ADJREC	;ADJUST TUBREC AS APPROPRIATE
****
2)20	SBRINT:	PUSHJ	P,ADJREC	;ADJUST TUBREC AS APPROPRIATE
**************
1)22	DONE:	PUSHJ	P,CHKCHN	;CHECK FOR CHANNEL ERRORS
1)		PUSHJ	P,TSTERR	;READ ERROR REGISTERS IF NECESSARY
****
2)22	;MCO 11528, 1-Dec-84 Software Dispatch
2)	DONE:	TLNN	T1,RB.SLK	;[11528] Channel errors meaningless on WLK errs
2)		PUSHJ	P,CHKCHN	;CHECK FOR CHANNEL ERRORS
2)		PUSHJ	P,TSTERR	;READ ERROR REGISTERS IF NECESSARY
**************
   