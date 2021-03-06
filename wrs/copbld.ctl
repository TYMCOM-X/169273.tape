:logfile copbld.log
;
; Build of (SPL)COPY, a telecopy wildcarding program.
;
; Requires license RF JL.
; Requires protection ALL RUN RUN.
;

daytime

cte setp sail=(ftsys)sail
cte addp loader=(spl)linker

com/com copy
load (ftsys)sailow,copy
save copy

r cksum
^copcks.cks
y
@copcks.cmd

del copy.rel,copy.fdm

r(qasys)fdm
O COPY.FDM
Y
R @COPFDM.CMD
Q

type copcks.cks

;
; End of build of COPY.
;
 