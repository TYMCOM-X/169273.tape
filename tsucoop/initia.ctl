:LOGFILE initia.log
;----------------------------------------------------------------
;	control file to build INITIA
;----------------------------------------------------------------
;
;	Use regular versions of MACRO & LOADER.  Assemble, link & 
;	load, and save INITIA.SAV:
ctest setproc
load /com initia.mac /save:initia
;
;	Get checksum and version number:
r cksum
initia.sav

get initia
ver
;
;	Delete unneeded file:
delete initia.rel
;
;	Give executable program file proper license and protection:
r setlic
initia.sav,op sy st jl

decl all run run
initia.sav
;
;
;	To install:
;	Save last version of executable as INITIA.BAK, then
;	rename new version to SYS directory, thereby 
;	preserving established license and protection:
;		delete (sys) initia.bak
;		ren (sys) initia.sav, (sys) initia.bak
;		ren initia.sav, (sys) initia.sav
;		dir (sys) initia.sav/lic/prot
;
;
  