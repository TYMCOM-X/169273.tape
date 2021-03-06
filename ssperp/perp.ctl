;[PERP.CTL] Control file to build PERP and LPERP
;	Use PDP mode so all commands work
PDP
;
;	Use (FTSYS)MACRO and (SYS)LINK via (SPL)LINKER
CTE SETPROC MACRO=(FTSYS)MACRO,LOADER=(SPL)LINKER
;	Record start time for tracking
DAYTIME
;	Build LPERP	Lockheed version
LOAD @LPERP.CMD
;	Build PERP	Normal version	(last leaves normal REL files)
LOAD/CROSS (MONITOR) @PERP.CMD
;	Create listings
CROSS
;	...and master cref
DELETE PERP.LST,PERP.SWM
R SWEET
PERP
Y
PERP/A
;	Get Checksums
R CKSUM
@CKPERP.CMD

;	Record time
DAYTIME
;
;	Don't forget to set license and protection
;
;	(SYS) PERP.SAV    RUN RUN RUN  AC
;	(SYS) PERP.SYS    RD  RD  RD   --None--
;
;[End of PERP.CTL]
    