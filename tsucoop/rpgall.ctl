:logfile RPG.LOG
;		[RPG BUILD INSTRUCTIONS]
;
;	Establish desired mode and tty characteristics:
PDP
TTY WIDTH 80
TTY FORM
;
;---------------------------------------------------------------
;	Use the latest MACRO-10 and LINK-10			)
;	(LINK-10 APPEARS TO HAVE A BUG!!!! USE LOADER)		)
; CTEST SETPROC MACRO=(FTSYS)MACRO,LOADER=(SPL)LINKER		)
;---------------------------------------------------------------
;
;	Use the latest MACRO-10:
CTEST SETPROC MACRO=(FTSYS)MACRO
;
;	Compile, load and save RPG and modules:
LOAD/COMPILE/SSAVE:RPG RPGUNV.MAC,RPG.MAC,RPGINI.MAC,RPGMAI.MAC

;
;	Delete unnecessary files:
DELETE RPG.REL,RPGUNV.REL,RPGINI.REL,RPGMAI.REL
;
;	Get checksums, for the record:
R CKSUM
@RPG.FIL

;
;	Get version number of executable created:
GET RPG
VERSION
;
;
;	Rename RPG.SHR to (SYS) directory, giving it ALL RUN RUN protection
;	and HF license.
;
;
;[END of RPG.CTL]

