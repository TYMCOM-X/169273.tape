File 1)	DSKB:KERSYS.MAC[14,10,KERMIT,TOPS10]    	created: 1715 25-May-84
File 2)	DSKB:KERSYS.MAC[14,10,KERMIT,TOPS10,CSM]	created: 1721 05-Jun-84

1)1	; Universals
****
2)1	;[CSM] Make SY%LOGOUT detach job to implement ".KJOB/NOMESSAGES".
2)	;[CSM]   This is needed for VT180 with ZCPR so as not to fill up the
2)	;[CSM]   buffer with the "Job logged off" messages.
2)	; Universals
**************
1)1		SYSVER==2			; Major version number
1)		SYSMIN==0			; Minor version number
****
2)1	;[CSM] According to KERV3.MEM, this is KERMIT version 3(123)
2)		SYSVER==3		;[CSM]	; Major version number
2)		SYSMIN==0			; Minor version number
**************
1)6	TOPS10<
1)		MOVEI	S1,S2			; Build arguments in the registers
****
2)6	;[CSM] Change at BLSRTN(SY%LOGOUT)
2)	TOPS10<
2)		MOVSI	S1,-1           ;[CSM]  ; -1 in LH means current terminal
2)		ATTACH	S1,		;[CSM]	; 0 in RH means DETACH
2)		  JFCL			;[CSM]
2)		MOVEI	S1,S2			; Build arguments in the registers
**************
 