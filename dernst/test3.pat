PATCH(850321,1800,DRE,CON040+4,,6)
	JFS	CON040+0E
CONPATCH(CON040+18,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,1E)
	LB	R0,WFR.NO,R5,	:ARE THERE ANY REMAINING COMANDS OUT THERE
	JN	CON041+50,,	:YES, SKIP THIS PROCESSING
	TBT	R1,ITP.4,,	:is the original state off
	JE	CON041,,	:yes, skip this message
	J	CON040+22,,	:CONTINUE NORMAL PROCESSING
  IF	CENALX
CONPATCH(CON042,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,1A)
	LHL	R1,DPORT
	TBT	R1,TURKEY	:TURKEY CALL ?
	JN	CON050,,	:YES, SKIP CANNED Q-BIT MESSAGES
	LHL	R1,IEDBUF
	LR	R5,R7
	J	CON042+6,,
  EI
ENDPATCH(Don't not try to restore PAD params after TKSUP if none were received)


PATCH(840413,1230,DRE,ESC050+26,,0A)
:	this patch and the one at esd010+16 wil normally skip around
:	this code to strip national pad parameters.  It does not work
:	at all if the pad parameters come in as M-bit iix
:	messages. A much larger fix is needed.  If someone really needs
:	this code then the BCUG(CLEAR) statement (which is not used now)
:	will turn on this code.
	TBT	RL,BCC.F,,
	JE	ESC110
ENDPATCH(Eliminate bad test against call processing national facility flag)


PATCH(840413,1230,DRE,ESD010+16,,0a)
	TBT	RL,BCC.F,,
	JE	ESD012
ENDPATCH(Eliminate bad test against call processing national facility flag)
 