:PARAMETERS NODE=ND2100
SYSNO
DAYTIME
; This CTL file rebuilds the BND file for an EBUS base from scratch.
; The following files must exist:
DIRECT \NODE\.TYM,\NODE\.T01,\NODE\.CMD,\NODE\.C00,\NODE\.C01
DIRECT ISXRAY.GGY,GOODGY.NQA

; The following files in SOURCE are EBUS specific:
;EBUS02.T00 = example of what is needed in NDxxxx.T01
;EBUS02.C00 = example of what is needed in NDxxxx.C01
;EBUS02.I00 = Initialization file
;EBUS02.R00 = Release code (?STAT=EBUSOM, front-panel display code)
;EBUS02.O00 = Object code for 68K, MERLIN puts this in slot 2
;EBUS02.J00 = Source code for EBUS02.O00, for reference only

; Create the ISIS Kernel from NDxxxx.TYM
R (SYS)NAD
;X\NODE\.CMD

; Create the node code for slot 0 from NDxxxx.TYM
R (SYS)NAD
;X\NODE\.C00

; Create the Engine code that runs in Slot 1 from NDxxxx.T01
R (SYS)NAD
;X\NODE\.C01

; This is how to create the 68000 code that runs in slot 2
;; RUN (MPL)ASM68K
;; EBUS02.LIS=(SOURCE)EBUS02.J00
;; DIFFER EBUS02.SRE,(SOURCE)EBUS02.O00
; Use "/X EBUS02.LIS=(SOURCE)EBUS02.J00" to get a CREF listing

; Build BND file.  Note that the 68K code is the same in all bases.
R (SYS)MERLIN
\NODE\.BND=\NODE\.NIB,\NODE\.N00,\NODE\.N01,(SOURCE)EBUS02.O00/S

DIRECT \NODE\.BND
