;[DEFSYS.CTL] Control file to build DEFSYS
;             DEFSYS manages the perp master controller file
;             newly released with version 7.26 of PERP and
;             1.26 of DEFER.  DEFSYS.SAV resides on (SPPOPER).
;
;	Use PDP mode so all commands work
PDP
;
;	Use (FTSYS)SAIL and (SYS)LINK via (SPL)LINKER
CTE SETPROC SAIL=(FTSYS)SAIL,LOADER=(SPL)LINKER
;
;	Record start time for tracking
DAYTIME
;
;	Build DEFSYS
LOAD/SAVE:DEFSYS DEFSYS.SAI
;
;	Get Checksums
R CKSUM
DEFSYS.CTL
DEFSYS.SAI
DEFSYS.SAV
PERP.SYS

;	Record time
DAYTIME
;
;	This is an "ARCHIVE ONLY" transmittal
;
;[End of DEFSYS.CTL]
