:logfile CPMUD.LOG
:time 600
:parameters restart
run lsmud1
\restart
ftmagnum mud
run sort.merge
quit
del mud.dat,mud.dir
run ldmud
   