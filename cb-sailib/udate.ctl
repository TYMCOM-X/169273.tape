; UDATE.CTL 14-Feb-79
; Compile and add module UDATE.
;
.path sai:
.com/com udate
.r maklib
*libary=libary/master:udate,udate/replace:udate
*libary=libary/index

; Now for high-segment version.
.com/com udateh=udate(h)
.r maklib
*libarh=libarh/master:udateh,udateh/replace:udateh
*libarh=libarh/index
