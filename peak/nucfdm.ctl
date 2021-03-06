:logfile nucfdm
;
;              Build an FDM file containing sources for the
;                        Concurrent Pascal Nucleus
;                                (V11.00)
da
del pnuc11.f00

;
;                     First the present .CTL file and
;                  the Nucleus Product Release documents
;
r(ken)fdm
o pnuc11.f00
R NUCFDM.CTL
R (BETATEST)PNUC11.X00
R (BETATEST)PNUC11.Y00
q

;
;                  Next, the Command, Init and Tym files
;
r(ken)fdm
o pnuc11.f00
R (BETATEST)PNUC11.C00
R (BETATEST)PNUC11.I00
R (BETATEST)PNUC11.T00
q

;
;          And finally, the files making up the .Rxx source file
;                                (Part 1)
;
r(ken)fdm
o pnuc11.f00
R (EPASCAL1)NUCLUS.G90
R (EPASCAL1)MACROS.NAD
R (ISIS2)START.LIB
R (EPASCAL1)VECTOR.LIB
R (ISIS2)TRACE.LIB
R (ISIS2)CRASH.LIB
R (ISIS2)BID.LIB
R (ISIS2)TIMOUT.LIB
R (EPASCAL1)FRING.NAD
R (ISIS2)OUTNUM.LIB
q

;
;          And finally, the files making up the .Rxx source file
;                                (Part 2)
;
r(ken)fdm
o pnuc11.f00
R (ISIS2)MBUFER.LIB
R (EPASCAL1)FRONT.NAD
R (EPASCAL1)NDBMAC.NAD
R (EPASCAL1)COMMON.NAD
R (EPASCAL1)STATUS.NAD
R (EPASCAL1)FORGND.NAD
R (EPASCAL1)DSKDRV.NAD
R (EPASCAL1)LPRDRV.NAD
R (EPASCAL1)PSEUDO.NAD
q

;
;          And finally, the files making up the .Rxx source file
;                                (Part 3)
;
r(ken)fdm
o pnuc11.f00
R (EPASCAL1)TTYDRV.NAD
R (EPASCAL1)NUCIO.NAD
R (EPASCAL1)NUCLUS.NAD
R (EPASCAL1)TTYDHI.NAD
R (EPASCAL1)PIPDHI.NAD
R (EPASCAL1)PALLOC.NAD
R (EPASCAL1)FINISH.NAD
q

;
;            Before leaving, we do a directory of our findings
;
r(ken)fdm
o pnuc11.f00
dir
q

da
  