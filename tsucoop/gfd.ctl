:LOGFILE GFD.LOG
;	[GFD BUILD INSTRUCTIONS]
;
;	Use the latest MACRO-10 and LINK-10:
CTEST SETPROC MACRO=(FTSYS)MACRO,LOADER=(SPL)LINKER
;
;	Compile, load and save GFD:
LOAD/COMPILE GFD.MAC
SAVE GFD
;
;	Clean up unneeded file:
DELETE GFD.REL
;
;	Log checksums and version:
R CKSUM
GFD.CTL
GFD.INF
GFD.MAC
GFD.SAV

;
GET GFD
VERSION
;
;	Transmit:
;	GFD.SAV to (SYS) with protection ALL RUN RUN
;			 with license    ST JL RF
