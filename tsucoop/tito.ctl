:LOGFILE TITO.LOG
;[Begin TITO.CTL]
;	Command file to build TITO %16 and later
;
;Necessary files:
;	COM.MAC		Source file
;	CHR.MAC		Source file
;	TAP.MAC		Source file
;	CMD.MAC		Source file
;	UUOFLG.MAC	Source file
;	UUO.MAC		Source file
;	GETVER.MAC	Source file
;	TITO.CTL	This file
;	TITO.CMD	Assembly and Build file
;	TITO.CKS	List of files for transmittal
;	TITO.INF	Information file about changes
;
;Files produced:
;	TITO.SAV	Executable program image
;	TITO.LOG	Output from the build procedure
;
;System Processors:
;
;	MACRO % 53B	from	(FTSYS)MACRO.SHR
;	LINK  % 4A	from	(SYS)LINK.SHR via (SPL)LINKER.SAV
;	DIRIT		from	(SYS)DIRIT.SHR
;	CKSUM		from	(SYS)CKSUM.SAV
;
;	Use these Processors instead of the standard ones
;
CTE SETPROC MACRO=(FTSYS)MACRO,LOADER=(SPL)LINKER
;	Create TAP.REL, CMD.REL, UUO.REL, GETVER.REL and TITO.SAV
;
LOAD/COMPILE/CROSS (MON) @TITO.CMD
;	Show version of TITO just created
GET TITO
VERSION
;	Delete excess REL files 
;
DELETE TAP.REL,CMD.REL,UUO.REL,GETVER.REL
;	Checksum the files for the transmittal
;
R CKSUM
@TITO.CKS

;	Clear special processors
;
CTE SETPROC
;
;	Declare proper protection for this copy
;
DECLARE ALL RUN RUN TITO.SAV
;	Remember to Give TITO proper license and protection
;
;	License:    GD ST JL XC WF
;	Protection: ALL RUN RUN
;
;[End TITO.CTL]
 