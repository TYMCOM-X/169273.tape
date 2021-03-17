File 1)	DSKU:COMDEV.MAC[702,10]	created: 1008 04-Oct-83
File 2)	DSKU:COMDEV.CSM[702,10]	created: 2054 16-Dec-84

1)1	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
****
2)1		SALL
2)		CSMEDT	02	;Initialize CSM edit 02 - LP11 printer DDB
2)		CSMEDT	03	;Initialize CSM edit 03 - Start DCA terminal output
2)		CSMEDT	13	;Initialize CSM edit 13 - TTY erase-to-end-of-line
2)		XALL		;List macro expansions
2)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
**************
1)21	SC'N'TIC::TTSTRT(M'N'TTG0,DL0TQ'N,DL0STO)
1)		TTSTRT(M'N'TTG1,DL1TQ'N,DL1STO)
1)		TTSTRT(M'N'D70N,D70TQ'N,D76STO)
****
2)21		CSMEDT	03,1	;DCA terminals, part 1
2)	SC'N'TIC::TTSTRT(M'N'TTG0,DL0TQ'N,DL0STO)
2)		TTSTRT(M'N'TTG1,DL1TQ'N,DL1STO)
2)	IFN CSM03$,<	;Start TTY output on the PDP-8 (DCA looks like a 680I)
2)		TTSTRT(M'N'68L0,CC0TQ'N,CC0STO)
2)	>  ;End of CSM03$
2)		TTSTRT(M'N'D70N,D70TQ'N,D76STO)
**************
1)24	TTPXNF::POINT	1,TCRTAB(T1),20	;ON IF TERMINAL HAS ^S/^Q
1)	TTPFLC::POINT	2,TCRTAB(T1),35	;POINTER TO DEFAULT FILL CLASS
1)	DEFINE CUSTRM,<		;CUSTOMER TERMINAL TYPES
1)		SPCTRM		;MONGEN CUSTOMER TERMINAL TYPES
1)	TERMCR	LA34,  132,  0, NOFF,   TAB,   LC, NOALT, NODIS, NOCRLF,   XON,  0,  0,  0
1)	>
1)	DEFINE DECTRM,<    ;DEC TERMINAL TYPES
1)	TERMCR  TTY,    72,  0, NOFF, NOTAB, NOLC,   ALT, NODIS,   CRLF,   XON,  1,  0,  0
1)	TERMCR  VT05,   72, 20, NOFF,   TAB, NOLC, NOALT,   DIS,   CRLF,   XON,  2, VT06EP, VTXXBP
****
2)24	TTPXNF::POINT	1,TCRTAB(T1),20	;On if terminal sends ^S/^Q by itself (LA120),
2)					; or if video terminal (user sends ^S/^Q).
2)	TTPFLC::POINT	2,TCRTAB(T1),35	;POINTER TO DEFAULT FILL CLASS
2)	;[CSM] DEC has LA34 and 2741 in the wrong places
2)	DEFINE CUSTRM,<		;CUSTOMER TERMINAL TYPES
2)	TERMCR 2741,  128, 0,NOFF,NOTAB,  LC,NOALT,NODIS,NOCRLF,NOXON,3,  0,  0
2)		SPCTRM		;MONGEN CUSTOMER TERMINAL TYPES
2)	;[CSM] Swapped places of LA34 and 2741
2)	>
2)	DEFINE DECTRM,<    ;DEC TERMINAL TYPES
2)	;[CSM] Put in alphabetic order, and added the following DEC terminals:
2)	;[CSM]	LA12	LA50	LA80	LA100	LQP01	LQP02	LQP8F	VT55
2)	;[CSM]	VT71T	VT72T	VT78	VT80	VT105	VT110	VT131	VT132	VT278
2)	;[CSM]	VT220	VT240	VT241 (new terminals)
2)	;[CSM] Set NOXON for all hardcopy terminals that don't generate ^S/^Q
2)	;[CSM] Set FF for VK100 and VT102 (because of the printer port)
2)	;[CSM] Also removed extra spaces so lines will fit within 80 columns for editor
2)	  ;
2)	TERMCR TTY,    72, 0,NOFF,NOTAB,NOLC,  ALT,NODIS,  CRLF,NOXON,1,  0,  0
2)	TERMCR  DAS21,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR LA12,   80, 0,NOFF,  TAB,  LC,NOALT,NODIS,  CRLF,  XON,0,  0,  0
2)	TERMCR LA30,   72, 0,NOFF,NOTAB,NOLC,NOALT,NODIS,  CRLF,NOXON,1,  0,  0
2)	TERMCR LA34,  132, 0,NOFF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LA36,  132, 0,NOFF,NOTAB,  LC,NOALT,NODIS,NOCRLF,NOXON,0,  0,  0
2)	TERMCR LA38,  132, 0,NOFF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
File 1)	DSKU:COMDEV.MAC[702,10]	created: 1008 04-Oct-83
File 2)	DSKU:COMDEV.CSM[702,10]	created: 2054 16-Dec-84

2)	TERMCR LA50,  132, 0,  FF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LA80,  132, 0,  FF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LA100, 132, 0,  FF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LA120, 132, 0,  FF,  TAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LQP01, 132, 0,  FF,NOTAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LQP02, 132, 0,  FF,NOTAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR LQP8F, 132, 0,  FF,NOTAB,  LC,NOALT,NODIS,NOCRLF,  XON,0,  0,  0
2)	TERMCR TTY33,  72, 0,NOFF,NOTAB,NOLC,  ALT,NODIS,  CRLF,NOXON,1,  0,  0
2)	TERMCR TTY35,  72, 0,  FF,  TAB,NOLC,  ALT,NODIS,  CRLF,NOXON,1,  0,  0
2)	TERMCR TTY37,  72, 0,  FF,  TAB,  LC,  ALT,NODIS,  CRLF,NOXON,1,  0,  0
2)	TERMCR VT05,   72,20,NOFF,  TAB,NOLC,NOALT,  DIS,  CRLF,  XON,2,VT06EP,VTXXBP
**************
1)24	TERMCR  VT61,   80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, VTXXEP, VTXXBP
1)	TERMCR  DAS21,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, VTXXEP, VTXXBP
1)	TERMCR  VT100,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  TTY33,  72,  0, NOFF, NOTAB, NOLC,   ALT, NODIS,   CRLF,   XON,  1,   0,  0
1)	TERMCR  TTY35,  72,  0,   FF,   TAB, NOLC,   ALT, NODIS,   CRLF,   XON,  1,   0,  0
1)	TERMCR  LA30,   72,  0, NOFF, NOTAB, NOLC, NOALT, NODIS,   CRLF,   XON,  1,   0,  0
1)	TERMCR  LA36,  132,  0, NOFF, NOTAB,   LC, NOALT, NODIS, NOCRLF,   XON,  0,   0,  0
1)	TERMCR  2741,  128,  0, NOFF, NOTAB,   LC, NOALT, NODIS, NOCRLF,   XON,  3,   0,  0
1)	TERMCR  LA38,  132,  0, NOFF,   TAB,   LC, NOALT, NODIS, NOCRLF,   XON,  0,   0,  0
1)	TERMCR  LA120, 132,  0,   FF,   TAB,   LC, NOALT, NODIS, NOCRLF,   XON,  0,   0,  0
1)	TERMCR  VT125,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VK100,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VT101,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VT102,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VT103,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VT180,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	TERMCR  VT185,  80, 24, NOFF,   TAB,   LC, NOALT,   DIS,   CRLF,   XON,  0, V100EP, VTXXBP
1)	>
****
2)24	TERMCR VT55,   80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VT61,   80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VT71t,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VT72t,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VT78,   80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VT80,   80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	TERMCR VK100,  80,24,  FF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT100,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT101,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT102,  80,24,  FF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT103,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT105,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT110,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT125,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT131,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT132,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT180,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT185,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT220,  80,24,  FF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT240,  80,24,  FF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT241,  80,24,  FF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,V100EP,VTXXBP
2)	TERMCR VT278,  80,24,NOFF,  TAB,  LC,NOALT,  DIS,  CRLF,  XON,0,VTXXEP,VTXXBP
2)	>
**************
1)25	VT06BP:	POINT	9,VT06TB
File 1)	DSKU:COMDEV.MAC[702,10]	created: 1008 04-Oct-83
File 2)	DSKU:COMDEV.CSM[702,10]	created: 2054 16-Dec-84

****
2)25		CSMEDT	13,1	;TTY changes, part 1 before VT06EP:
2)	IFN CSM13$,<	;Define backspace-space-backspace fillers
2)	;    ADM5/TV950, APPLE, CONCEP-100,  CHROMATICS, DASHER-200, DATAMEDIA,
2)	;    HAZELTINE-2000, HAZELTINE-1420
2)	 ;
2)	ADM5EP:	POINT	9,VTXXEL	;ADM-5 (ADM-3A with keypad and erase commands)
2)		POINT	9,ADM5E1
2)		POINT	9,ADM5E2
2)		POINT	9,ADM5E3
2)	;For the ADM-5, ^^=HOME, ^K=UP, ^L=RIGHT, ^H=LEFT, ^J=DOWN, $T=EOL, $Y=EOP
2)	ADM5E1:	BYTE	(9) 14,0		;Move right (FF)
2)	ADM5E2:	BYTE	(9) 14,33,324,0		;Move right, then erase to end of line
2)	ADM5E3:	BYTE	(9) 215,33,324,0	;Return, erase entire line
2)	 ;
2)	APPLEP:	POINT	9,VTXXEL	;APPLE IIe with 80-column card
2)		POINT	9,APPLE1
2)		POINT	9,APPLE2
2)		POINT	9,APPLE3
2)	;For the APPLE //e, $@=HOME, $A=UP, $B=RIGHT, $C=LEFT, $D=DOWN, $E=EOL, $F=EOP
2)	APPLE1:	BYTE	(9) 33,102,0		;Move right
2)	APPLE2:	BYTE	(9) 33,102,33,105,0	;Move right, then erase to end of line
2)	APPLE3:	BYTE	(9) 215,33,105,0	;Return, erase entire line
2)	 ;
2)	C100EP:	POINT	9,VTXXEL	;concept-100 terminal
2)		POINT	9,C100E1
2)		POINT	9,C100E2
2)		POINT	9,C100E3
2)	;For the 100 and 108, $?=HOME, $;=UP, $<=DOWN, $==RIGHT, $>=LEFT, $<^S>=EOL
2)	C100E1:	BYTE	(9)	033,275,0		;ESC,"=" = right cursor
2)	C100E2:	BYTE	(9)	033,275,033,223,0	;ESC,^S = erase line
2)	C100E3:	BYTE	(9)	215,033,223,0		;CR, EOL
2)	  ;
2)	CHROEP:	POINT	9,VTXXEL	;Chromatics graphics display
2)		POINT	9,CHROE1
2)		POINT	9,CHROE2
2)		POINT	9,CHROE3
2)	;For the Chromatics, ^\=HOME, ^K=UP, ^J=DOWN, ^]=RIGHT, ^H=LEFT, ^A3=EOL, ^L=CLEAR
2)	CHROE1:	BYTE	(9)	035,0			;^] = right cursor
2)	CHROE2:	BYTE	(9)	035,201,063,0		;^A,3 = erase line
2)	CHROE3:	BYTE	(9)	215,201,063,0		;CR, EOL
2)	  ;
2)	D200BP:	POINT	9,D200TB	;Dasher 200 (uses ^Y as backspace)
2)		POINT	9,D200TB,26
2)		POINT	9,D200TB+1,17
2)		POINT	9,D200TB+2,8
2)		POINT	9,D200TB+2,35
2)		POINT	9,D200TB+3,26
2)		POINT	9,D200TB+4,17
2)		POINT	9,D200TB+5,8
2)	D200TB:	BYTE	(9)	231,240,231,231,240,231,231,240
2)		BYTE	(9)	231,231,240,231,231,204,231,231
2)		BYTE	(9)	204,231,231,240,231,231,240,231
2)		BYTE	(9)	0
2)	  ;
2)	D200EP:	POINT	9,VTXXEL	;Dasher 200
File 1)	DSKU:COMDEV.MAC[702,10]	created: 1008 04-Oct-83
File 2)	DSKU:COMDEV.CSM[702,10]	created: 2054 16-Dec-84

2)		POINT	9,D200E1
2)		POINT	9,D200E2
2)		POINT	9,D200E3
2)	;For the D200, ^H=HOME, ^W=UP, ^X=RIGHT, ^Y=LEFT, ^Z=DOWN, ^K=EOL, ^L=EOP
2)	D200E1:	BYTE	(9) 30,0		;Move right
2)	D200E2:	BYTE	(9) 30,213,0		;Move right, then erase to end of line
2)	D200E3:	BYTE	(9) 215,213,0		;Return, erase entire line
2)	 ;
2)	DM15EP:	POINT	9,VTXXEL	;Datamedia 1521
2)		POINT	9,DM15E1
2)		POINT	9,DM15E2
2)		POINT	9,DM15E3
2)	DM15E1:	BYTE	(9)	234,0
2)	DM15E2:	BYTE	(9)	234,035,0
2)	DM15E3:	BYTE	(9)	215,035,0
2)	  ;
2)	HAZ2BP:	POINT	9,HAZ2TB	;Hazeltine 2000-A (uses "_" as backspace)
2)		POINT	9,HAZ2TB,26
2)		POINT	9,HAZ2TB+1,17
2)		POINT	9,HAZ2TB+2,8
2)		POINT	9,HAZ2TB+2,35
2)		POINT	9,HAZ2TB+3,26
2)		POINT	9,HAZ2TB+4,17
2)		POINT	9,HAZ2TB+5,8
2)	HAZ2TB:	BYTE	(9)	137,240,137,137,240,137,137,240
2)		BYTE	(9)	137,137,240,137,137,204,137,137
2)		BYTE	(9)	204,137,137,240,137,137,240,137
2)		BYTE	(9)	0
2)	  ;
2)	H142EP:	POINT	9,VTXXEL	;Hazeltine 1420
2)		POINT	9,H142E1
2)		POINT	9,H142E2
2)		POINT	9,H142E3
2)	;For the HZ1420, $^S=HOME, $^L=UP, ^P=RIGHT, ^H=LEFT, $^K=DOWN, $^O=EOL, $^X=EOP
2)	H142E1:	BYTE	(9) 33,220,0		;Move right
2)	H142E2:	BYTE	(9) 33,220,33,17,0	;Move right, then erase to end of line
2)	H142E3:	BYTE	(9) 215,33,17,0		;Return, erase entire line
2)	 ;
2)	>  ;End of IFN CSM13$
2)	PAGE
2)	VT06BP:	POINT	9,VT06TB
**************
1)63	LPTPAG::!0				;PAGE COUNTER WORD
1)	LPTVEC::!LP11IV				;-11 STYLE INTERRUPT VECTOR ADDRESS
1)	LPTBAS::!LP11CA				;LP20 BASE DEVICE ADDRESS
1)	LPTIVI::!JSR	LP'N'INT		;INTERRUPT INSTRUCTION FOR THIS LPT
****
2)63		CSMEDT	02,2	;Lineprinter changes, part 2 (part 1 is LP1SER.MAC)
2)	IFN CSM02$,<	;Tab simulation is done in software for LP11 controller
2)	LPTCC::! 0				;Column counter for LP1SER.MAC
2)	>  ;End of IFN CSM02$
2)	LPTPAG::!0				;PAGE COUNTER WORD
2)	LPTVEC::!LP11IV-<N*4>		;[LP20]	;Interrupt vectors 754 and 750 or 200
2)	LPTBAS::!LP11CA+<N*20>		;[LP20]	;Base addr 775400 and 775420 or 777514
2)	LPTIVI::!JSR	LP'N'INT		;INTERRUPT INSTRUCTION FOR THIS LPT
**************
File 1)	DSKU:COMDEV.MAC[702,10]	created: 1008 04-Oct-83
File 2)	DSKU:COMDEV.CSM[702,10]	created: 2054 16-Dec-84

1)63	LPTCDB::! CHNMAC(\M.LP'N'C)		;POINTER TO CHANNEL DATA BLOCK
1)	LPTBUF::!BLOCK <<MAXLBZ*5>/4>+1		;ENOUGH ROOM FOR LPTSPL 7 TO 8 BIT BYTE CONVERSION
****
2)63	LPTCDB::! CHNMAC(\M.LP0C)	;[LP20]	;Use same channel data block for both
2)	LPTBUF::!BLOCK <<MAXLBZ*5>/4>+1		;ENOUGH ROOM FOR LPTSPL 7 TO 8 BIT BYTE CONVERSION
**************
1)64			EXTERNAL LP2SER		;HAUL IN THE SERVICE ROUTINE
1)			$LPNUM==0		;LPT NUMBER SYMBOL
1)	IFG LPTN-1,<PRINTX KS10 MONITOR DOES NOT SUPPORT MORE THAN ONE LPT>
1)			REPEAT LPTN, <
****
2)63	IFN <LP11IV-200>*<LP11IV-754>,<PRINTX	?Wrong definitions for LP11IV and LP11CA>
2)	IFE LP11CA-3775400,<	IF2,<PRINTX	[Requesting LP2SER for LP20]>
2)		EXTERNAL LP2SER	>	;HAUL IN THE SERVICE ROUTINE
2)	IFE LP11CA-3777514,<	IF2,<PRINTX	[Requesting LP1SER for LP11]>
2)		EXTERNAL LP1SER	>	;LP11 interface instead of LP20
2)64			$LPNUM==0		;LPT NUMBER SYMBOL
2)	;[LP20] <PRINTX KS10 MONITOR DOES NOT SUPPORT MORE THAN ONE LPT>
2)			REPEAT LPTN, <
**************
1)65		CONI	PL'N,PL'N'STS
****
2)65	PLTSTS==:PL'N'STS-PL'N'DDB	;[CSM] ;Most recent CONI status in PLTSTS##(F)
2)	PLTCNI==:.-PL'N'DDB		;[CSM] ;For XCT PLTCNI##(F) in PLTSER
2)		CONI	PL'N,PL'N'STS
**************
  