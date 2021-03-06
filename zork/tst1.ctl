;[ Command file test ]

;The :SILENCEd line runs .INIT
:SILENCE
.init
:REVIVE

:PARAMETERS stuff PARAMA PARAMB

;value in parameter 'stuff' = "\stuff\"
;value in parameter 'paramA' = "\paramA\"
;value in parameter 'paramB' = "\paramB\"

;test of :DEFINE:  assigning new values to 3 params.:
:DEFINE stuff = "firstword", paramA = 'valueA', paramB = 'valueB'
;value in parameter 'stuff' = "\stuff\"
;value in parameter 'paramA' = "\paramA\"
;value in parameter 'paramB' = "\paramB\"

;test of colon-assignment command
:stuff = "newword", paramA="valueA second time",paramB="valueB second time"
;value in parameter 'stuff' = "\stuff\"
;value in parameter 'paramA' = "\paramA\"
;value in parameter 'paramB' = "\paramB\"

:DEFINE stuff = 'a string of stuff'
;value in parameter 'stuff' = "\stuff\"

:REMARK  an "illegal" PERP-type command, to see if it will abort the job:
:ILLEGAL

:REMARK  Keywords' time = \$DATE\ \$DAYTIME\
.DAYTIME

:STOP
 