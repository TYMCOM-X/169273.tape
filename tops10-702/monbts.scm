File 1)	DSKU:MONBTS.MAC[702,10]	created: 1901 26-Sep-83
File 2)	DSKU:MONBTS.CSM[702,10]	created: 2258 24-May-84

1)1	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
****
2)1		CSMEDT	40	;Initialize CSM edit 40 - Type out contents of 30
2)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
**************
1)5	    
1)	VMAP(.MBOOT,.VBOOT)	;LEFT HALF WORD IN EPT MAPPING .VBOOT
****
2)5	VMAP(.MBOOT,.VBOOT)	;LEFT HALF WORD IN EPT MAPPING .VBOOT
**************
1)19					;<MATCH ANGLE BRACKET ON NEXT LINE
****
2)19		CSMEDT	40,2	;MONBTS change, part 2
2)	IFN CSM40$,<	;Type the contents of location 30
2)		SKIPN	T4,CRSHWD##	;Non-zero in 30?
2)		 JRST	RLDMO3		;No
2)		MOVEI	T1,[ASCIZ /[Location 30 contains /]
2)		PUSHJ	P,PRMSG		;Type header
2)		PUSHJ	P,PRHWD		;Type PC or POKEd octal value
2)		PUSHJ	P,PRRBCR	;Finish "]<CRLF>"
2)	>  ;End of IFN CSM40$
2)					;<MATCH ANGLE BRACKET ON NEXT LINE
**************
