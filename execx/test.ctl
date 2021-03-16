;[ Command file torture test ]
:$SUBSTITUTION=1
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
.DO DIRECT
*TEST.*/S
:ESCAPE
:CONTINUE
:REMARK now we will see if logging works
; Line just before turning off logging for the directory
:$LOGGING=$FALSE
; This line should not appear in the log file under any circumstances!
:REVIVE
;  now = \$DATE\ \$DAYTIME\ you see me...
:SILENCE
;  now you don't
.DIRECTORY
;  Time = \$DATE\ \$DAYTIME\
; This line should not appear in the log file
:REMARK this is not in the log file
:$LOGGING=$TRUE
; *** Back to the logging grind *** we are back!
:REMARK this is in the log file
:CONTINUE
; This had better be in the log file
;  Time = \$DATE\ \$DAYTIME\
;
; line before GOTO test
:GOTO FORWRD
;
; Skip this line.
BAKWRD::
; line after backto test
.PJOB
:GOTO SHORT
; line never sent to job
STOP:::STOP
; should never see this line either
::PERP
; test of perp-style labels
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
::IGNORE
:REMARK
:CONTINUE
; we can fall through labels also
IGNORE::.CORE
; what other commands do not run a program?
:GOTO MORE
; never send this line to job
QUIT:::REMARK isn't this remarkable!
; see what happens if user has :QUIT
:QUIT
;
SHORT::.SYSNO
:REMARK line follows SHORT label and same-line command
:CONTINUE
; ...
; we are not slipping
:BACKTO PERP
; line always skipped
::MORE
; come here for more tests
;
:REMARK
:REMARK		********************************
:REMARK		**                            **
:REMARK		**    just                    **
:REMARK		**         fooling            **
:REMARK		**                 around     **
:REMARK		**                            **
:REMARK		********************************
;
; testing is done for now.
; line before GOTO end
:GOTO END
;
FORWRD::
; line after GOTO test
.WHO .
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
; line before BACKTO test
:BACKTO BAKWRD
;
;
END::
;  Time = \$DATE\ \$DAYTIME\
.DAYTIME
;[End of TEST.CTL]
