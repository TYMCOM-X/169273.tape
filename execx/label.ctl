;[ COMMAND FILE TEST ]
:$SUBSTITUTION=1
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
.DO DIRECT
*TEST.*/S
:ESCAPE
; Line just before turning off logging for the directory
:$LOGGING=$FALSE
; This line should not appear in the log file under any circumstances!
;  Time = \$DATE\ \$DAYTIME\
.DIRECTORY
;  Time = \$DATE\ \$DAYTIME\
; This line should not appear in the log file
:$LOGGING=$TRUE
; *** Back to the logging grind ***
; This had better be in the log file
;  Time = \$DATE\ \$DAYTIME\
;
; line before GOTO 2nd
:GOTO SECOND
;
; Skip this line.
FIRST::
; 1st comment line
.PJOB
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
;
; line before GOTO end
:GOTO END
;
SECOND::
; 2nd after GOTO
.WHO .
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
; line before backto first
:BACKTO FIRST
;
;
END::
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
;[End of TEST.CTL]
