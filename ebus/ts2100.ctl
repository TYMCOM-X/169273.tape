:LOGFILE TS2100.LOG
:$MAIL=$FALSE
SYSNO
DAYTIME

R (SYS)NAD      ; Create the ISIS Kernel from NDxxxx.TYM
;XND2100.CMD
R (SYS)NAD      ; Create the node code for slot 0 from NDxxxx.TYM
;XND2100.C00
R (SYS)NAD      ; Create the Engine code that runs in Slot 1 from NDxxxx.T01
;XND2100.C01
; Slot 2 comes from EBUS02.O02
R (SYS)NAD      ; Create Joe's test code that runs in Slot 3 from NDxxxx.T03
;XND2100.C03
R (SYS)NAD      ; Create Carl's test code that runs in Slot 4 from NDxxxx.T04
;XND2100.C04
R (SYS)NAD      ; Create Osman's test code that runs in Slot 5 from NDxxxx.T05
;XND2100.C05
R (SYS)NAD      ; Create the big slot code that runs in Slot 6 from NDxxxx.T06
;XND2100.C06

; Build BND file.  Note that the 68K code is the same in all bases.
R (SYS)MERLIN
TS2100.BND=ND2100.NIB,ND2100.N00,ND2100.N01,(EBUS)EBUS02.O02/S,ND2100.N03,ND2100.N04,ND2100.N05,ND2100.N06

DIRECT ND2100.*,TS2100.*
    