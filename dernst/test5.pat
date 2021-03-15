  IF	PVC
:This patch fixes the problem of resets causing gobblers being sent
:while the PVC setup IIX messages are still in transit.  If the
:gobbler catches the setup message, it will be destroyed and the PVC
:will be left in the initializing state.  A similar thing can happen to
:the PVC build acknowledgement (which can also happen to the call
:connect IIX message) that this patch does NOT fix.  No NSR.  DRE 2/6/86
PATCH(850206,1800,DRE,SNDR20+0E,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,1C)
	LIS	R2,PWPVCR					:DRE 6-FEB-86
	CLB	R2,PCKSTE,R1,		:ARE WE IN PVC BUILDING STATE :DRE 6-FEB-86
	JER	R8			:YES, DONT SEND GOBBLER BECAUSE IT WILL :DRE 6-FEB-86
					:INTERFERE WITH THE PVC SETUP MESSAGES  :DRE 6-FEB-86
	LHL	R2,EPORT
	TBT	R2,PVCOUT,,		:IS OTHER DTE 'DOWN' ??
	JNR	R8			:YES, RETURN
	J	SNDR20+1A,,
ENDPATCH(KEEP RESET GOBBLERS FROM DESTROYING PVC SETUP MESSAGES)
  EI	PVC

::This patch prevents RR packets form being queued up for a channel
:in the PVC building state or any of othe other inappropriate states.
: No NSR.  DRE 2/7/86
PATCH(850207,1300,DRE,ICBKG-0A,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,20)
	RBT	R1,DFLUSH,,
	LHL	R4,IPORT					:DRE 7-FEB-86
	LB	R0,PCKSTE,R4,		:GET THE STATE OF THE CHANNEL :DRE 7-FEB-86
	CLHI	R0,PSRESC		:IN A NORMAL STATE?	:DRE 7-FEB-86
	JG	MMFRA,,			:NO, SKIP WINDOW UPDATE	:DRE 7-FEB-86
	J	CWROT,,			:GO SEE IF WINDOW NEED TO BE ROTATED
ENDPATCH(DON'T QUEUE UP RR PACKETS IN WRONG STATES)

  IF	PVC
:If the PVC build message contrains a link number that is not set up
:in the PVC link table then it reads a pointer off the end of the
:table and a illegal memory reference occurs.  This patch adds a
:check for correct range of link number in PVC building IIX message.
: No NSR.   DRE 3/3/86.
PATCH(860303,1800,DRE,ESP19+24,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,16)
	CLHI	R0,NLINKS		:IS IT IN RANGE?	:DRE 3-MAR-86
	JGE	ESP198,,		:NO,REPORT ERROR TO OTHER END :DRE 3-MAR-86
	LR	RL,R0			:DEST. LINK NUMBER
	LR	RL2,RL
	SLLS	RL2,1			:FORM *2 FOR NDLST2
	J	ESP19+2A,,
ENDPATCH(CHECK PVC BUILD MESSAGE FOR LINK NUMBER IN RANGE)
  EI	PVC