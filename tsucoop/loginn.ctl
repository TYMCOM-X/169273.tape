:LOGFILE loginn.log
;			[LOGINN build file]
;
;	Use (FTSYS)MACRO to assemble.  Assemble, load and save LOGINN:
ctest setproc macro=(ftsys)macro
load /com loginn.mac
ssave loginn
;
;	Delete unneeded .REL file:
delete loginn.rel
;
;	Display checksums and other info about files, and
;	version number of executable, for the record:
r cksum
^loginn.cks
y
@loginn.fil

dir /ev @loginn.fil
typ loginn.cks

get loginn
ver
;
;
;	To install:
;	Give executable file ALL RUN RUN protection 
;	and OP SY ST HF JL license:
declare all run run, loginn.shr

r setlic
loginn.shr,op sy st hf jl

;	Save last version of executable as LOGINN.BAK, then
;	rename new version to SYS directory, thereby 
;	saving previous version and preserving new version's
;	established license and protection:
;		delete (sys) loginn.bak
;		ren (sys) loginn.shr, (sys) loginn.bak
;		ren loginn.shr, (sys) loginn.shr
;
;
;
;[END of LOGINN.CTL]
