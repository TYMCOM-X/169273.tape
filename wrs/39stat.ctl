:LOGFILE 39STAT.LOG
:TIME 300
R (XEXEC)MINIT
TRSTAT
SORT TRSTAT.DAT=TRSTAT.DAT/REV
TUMS feldman@tymix,seltzer@tymix
Usage this month by program name. (system 39)
~R TRSTAT.DAT

USSTAT
SORT USSTAT.DAT=USSTAT.DAT/REV
TUMS feldman@tymix,seltzer@tymix
Usage this month by user name. (system 39)
~R USSTAT.DAT

  