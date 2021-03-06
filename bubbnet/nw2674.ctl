:LOGFILE NW2674.LOG
:PARAMETERS NODE=NW2674
; Note: This file is NW2674.CTL created by JMS.
;       BB2674.COG is the CTL file to create BB2674.BND

SYSNO
DAYTIME
; This CTL file rebuilds the BND file for an EBUS base from scratch.
; The following files must exist:
DIRECT \NODE\.TYM,\NODE\.T01,\NODE\.CMD,\NODE\.C00,\NODE\.C01
DIRECT (TIIDEV)GOODGY.XRY
DIRECT (ISIS2)GOODGY.LAB

; Create the ISIS Kernel from NDxxxx.TYM
R (BETATEST)NAD
;X\NODE\.CMD

; Create the node code for slot 0 from NDxxxx.TYM
R (BETATEST)NAD
;X\NODE\.C00

; Create the Engine code that runs in Slot 1 from NDxxxx.T01
R (BETATEST)NAD
;X\NODE\.C01

; The S-RECORDS for the IPI 68000 are in EBUS02.O00 and load into slot 2
; Build BND file.  Note that the 68K code is the same in all bases.
R (ISIS2)MERLIN
\NODE\.BND=\NODE\.NIB,\NODE\.N00,\NODE\.N01,(SOURCE)EBUS02.O00/S

; The NIB files are not needed once the code has been bound
DELETE \NODE\.NIB,\NODE\.N00,\NODE\.N01
DIRECT \NODE\.BND
;[End of NW2674.CTL]
  