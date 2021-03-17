:
:		; r (ftsys)decmac	;
:		; rtsym=rtsym 		; compiled with
:		; iosym=iosym 		;    CMLMAC.COM
:		; passym=passym		;
:		; 			;
:
r (pasproc)dtmpfl
nlbmac.cmd
mac
:
r (ftsys)link
/run:dsk:decmac[1,315372]/runoff
