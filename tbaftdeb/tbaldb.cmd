; Load debug-version of TBALDR, switch descriptions:
;  file%-4Y       specifies /HOTSTART:file
;  %-5Y           specifies deletion of "EXECUTION" message when loading done
TBALDR/FORWARD/REL%-4Y,%-5Y,(SYS)JOBDAT,RUNDAT,DATAR,TBARUN, /RUN(SPUNKDEV)LOADER
