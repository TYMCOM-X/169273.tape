:LOGFILE ENTER.LOG
;[ COMMAND FILE TEST MAGNUM ENTER.ITEM ]
:$SUBSTITUTION=1
;  Time = \$DATE\ \$DAYTIME\
;
DO DIRECT
*Q*/S
:ESCAPE
.PJOB
;  Time = \$DATE\ \$DAYTIME\
;
.DIR/TODAY
   