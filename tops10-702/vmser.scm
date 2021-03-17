File 1)	DSKU:VMSER.DEC[702,10]	created: 1157 04-Jan-84
File 2)	DSKU:VMSER.MAC[702,10]	created: 1721 25-Feb-84

1)1		SEARCH	F,S
****
2)1	;[CSM] Includes fix from KIMO YAP to prevent PAO stopcodes when QUASAR dies.
2)	;[CSM] Remove the TRNE T2,400000 at IPCRMV+many (IPCPAG-6).
2)		SEARCH	F,S
**************
1)85		TRNE	T2,400000
1)		PUSHJ	P,DCVMT		;ADJUST VM COUNTERS
****
2)85	;[CSM] The fix to QAR:125714 (CSM#836) is not in the 4th tape and has no MCO #.
2)	;[CSM] Patch is at IPCRMV+many, at IPCRMV-6.
2)	;[CSM]	TRNE	T2,400000
2)		PUSHJ	P,DCVMT		;ADJUST VM COUNTERS
**************
