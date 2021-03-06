! This is BACKUP.CMD, it saves Colorado CUSPS to 1600 bpi tape
!
!	Joe Smith
!	Colorado School of Mines
!	Computer Center
!	1500 Illinois
!	Golden, Colo 80401
!	(303) 279-0300 ext 2448  (Night line 279-0310)
!
NOMULTIREEL
DENSITY 1600
SORT FILES NONE
FILES
SSNAME "'READ.ME - file descriptions'"
SAVE DSK:READ.ME=BACKUP.CMD[13,10]
SSNAME "Non-SFD files"
SAVE DSK:=*.*[13,10]
SORT FILES ALPHA
SSNAME "Colorado School of Mines CUSPS"
SAVE -		! This file is an indirect command file for BACKUP
[13,10,CRF],-	! Tells CREF to do *.CRF in default path
[13,10,CSM],-	! General CSM stuff
[13,10,CSMCPD],-! CSM Police Department, watches idle lines, timed memos
[13,10,DAILY],- ! Types messages on specific dates, and 7K of fortune cookies
[13,10,DDBDPY],-! Displays disk DDB's for 603A or 701
[13,10,DSKCPY],-! Disk to disk utility, with full SCAN and WILD interface
[13,10,FACLST],-! Lists selected portions of FACT.SYS
[13,10,FOR],-	! FORTRAN-callable subroutines
[13,10,INFO],-  ! SYSTAT replacement with full SCAN interface
[13,10,JOBDPY],-! Compressed job display for 603A
[13,10,MACRO],- ! Rex Shadrick's modified MACRO
[13,10,MCR],-	! Monitor Command Recognizer - user mode COMCON
[13,10,MISC],-	! Miscellaneous
[13,10,MTCOPY],-! Modified for TU70 and 701
[13,10,PALXXX],-! Cross-assembler for PDP-8 and Rockwell 6502 (APPLE & PET)
[13,10,PLOT,*],-! CSM plotting package
[13,10,SYSUNV],-! Macro definitions for TYPER.REL
[13,10,TAPSPL],-! Puts GALAXY 2 queues in BACKUP format (no PLT: on 2020)
[13,10,TECO,*],-! Version 124 with local mods
[13,10,TECMAC],-! Handy TECO macros.  Label BACKUP listing, submit SPR
[13,10,VERSIO]  ! Sets .RBVER of source and REL files
SSNAME "LUG software"
SAVE -		! Software from outside Colorado
[14,10,RIPOFF],-! Disk utility
[14,10,SED],-	! Full-screen editor
[14,10,TYPE]	! CUA replacement for PIP TYPE and LIST commands
   