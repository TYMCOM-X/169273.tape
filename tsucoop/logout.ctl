:LOGFILE LOGOUT.LOG
;	[LOGOUT BUILD INSTRUCTIONS]
;
;	Use the latest MACRO-10 and LINK-10:
CTEST SETPROC MACRO=(FTSYS)MACRO,LOADER=(SPL)LINKER
;
;	Compile, load and save LOGOUT:
LOAD/COMPILE LOGOUT.MAC
SSAVE LOGOUT
;
;	Clean up unneeded file:
DELETE LOGOUT.REL
;
;	Log checksums and version:
R CKSUM
LOGOUT.CTL
LOGOUT.MAC
LOGOUT.SHR

;
GET LOGOUT
VERSION
;
;	Transmit:
;	LOGOUT.SAV to (SYS) with protection ALL RUN RUN
;			    with license    WC OP SY ST HF JL
   