:
:			; r (ftsys)decmac
:			; RTMTH=RTMTH
:			; PASSYM=PASSYM
:			; RTPFID=RTPFID
:			; BUF0=BUF0
:			; 
:
r (pasproc)dtmpfl
ilbmac.cmd
mac
:
r (ftsys)link
/run:dsk:decmac[1,315372]/runoff
 