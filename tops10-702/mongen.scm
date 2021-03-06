File 1)	DSKU:MONGEN.MAC[702,10]	created: 0851 25-Oct-83
File 2)	DSKU:MONGEN.CSM[702,10]	created: 2259 24-May-84

1)1	VWHO==0			;WHO LAST EDITED
1)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
****
2)1	VWHO==2			;WHO LAST EDITED - Joe Smith @ CSM
2)	FTDC68==1	;[CSM] Ask about 680I (but not other hardware)
2)			;[CSM] Changed spelling in TERMINAL TYPES dialog
2)			;[CSM] Changed to max of 64 lines on a 2020
2)	;THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY ONLY BE USED
**************
1)61	ASKTRA:	BTHOUT	<Type "terminal-type,WIDTH,LENGTH,FF,TAB,LC,ALT,DISPLAY,CRLF,XON,FILL,erase-to-EOL,backspace-space-backspac
1)	For customer defined terminal types.
1)	Terminal-type is the SIXBIT name of the terminal.
1)	WIDTH is the width of the terminal
1)	LENGTH is the length/pagesize of the terminal
1)	FF/NOFF is whether or not the terminal has hardware form feeds
1)	TAB/NOTAB is wheither or not the terminal has hardware tabs
1)	LC/NOLC is wheither or not the terminal is capabile of lower case
1)	ALT/NOALT is wheither or not the terminal generates old altmodes
1)	DIS/NODIS is wheither or not the terminal is a display terminal
1)	CRLF/NOCRLF is wheither or not the terminal needs a CRLF at right margin
1)	XON/NOXON is wheither or not ^S/^Q should pause the terminal
1)	FILL is the number of fill characters required for the terminal
1)	Erase-to-EOL is address of sequences to cursor right and erase to EOL
1)	Backspace-space-backspace is address of sequences to backspace, space, backspace
1)	Type extra carriage return when through.]@>
****
2)61	;[CSM] Corrected spelling of WHEITHER, CAPABILE.
2)	;[CSM] Change DISPLAY to DIS, XONXOFF to XON, clarify LENGTH and XON.
2)	;[CSM] Mention VTXXEP, V100EP, and VTXXBP.
2)	ASKTRA:	BTHOUT	<Type "terminal-type,WIDTH,LENGTH,FF,TAB,LC,ALT,DIS,CRLF,XON,FILL,erase-to-EOL,backspace-space-backspace"[
2)	For customer defined terminal types.
2)	TERMINAL-TYPE is the SIXBIT name of the terminal.
2)	WIDTH is the width of the terminal.
2)	LENGTH is the length/pagesize of the terminal (0 for hardcopy).
2)	FF/NOFF is whether or not the terminal has hardware form feeds.
2)	TAB/NOTAB is whether or not the terminal has hardware tabs.
2)	LC/NOLC is whether or not the terminal is capable of lower case.
2)	ALT/NOALT is whether or not the terminal generates old altmodes.
2)	DIS/NODIS is whether or not the terminal is a video display terminal.
2)	CRLF/NOCRLF is whether or not the terminal needs a CRLF at right margin.
2)	XON/NOXON should be set for all video terminals.
2)	FILL is the number of fill characters required for the terminal.
2)	Erase-to-EOL is address of sequences to cursor right and erase to EOL,
2)	  usually VTXXEP, V100EP, or 0.
2)	Backspace-space-backspace is address of sequences erase one character
2)	  from the screen, usually VTXXBP or 0.
2)	Type extra carriage return when through.]@>
**************
1)67	IFN FTUNSUPPORTED,<
1)		ASKDEC	<How many DC68s on CPU*(1,0-2)[
****
2)67	IFN FTUNSUPPORTED!FTDC68,<	;Needed at [CSM]
2)		ASKDEC	<How many DC68s on CPU*(1,0-2)[
**************
1)67	IFE FTUNSUPPORTED,<
1)		MOVEI	N,0		;NO DC68S
File 1)	DSKU:MONGEN.MAC[702,10]	created: 0851 25-Oct-83
File 2)	DSKU:MONGEN.CSM[702,10]	created: 2259 24-May-84

1)	>
1)		SKIPN	CPUN
****
2)67	IFE FTDC68,<	;FTDC68 is on at [CSM]
2)	IFE FTUNSUPPORTED,<
2)		MOVEI	N,0		;NO DC68S
2)	>>  ;End of IFE FTDC68
2)		SKIPN	CPUN
**************
1)69		ASKDEC	<TTY lines(0-32)[Total number of TTY lines]>,N
1)		MOVEM	N,M.DZNL
****
2)69	;[CSM] Need to have at least 48 lines on our KS2020
2)	;[CSM]	ASKDEC	<TTY lines(0-32)[Total number of TTY lines]>,N ;2020
2)		ASKDEC	<TTY lines(0-64)[Total number of TTY lines]>,N	;[CSM]
2)		MOVEM	N,M.DZNL
**************
    