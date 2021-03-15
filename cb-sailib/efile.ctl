; EFILE.CTL 14-Feb-79
; Compile and add module EFILE.
;
.path sai:
.com/com efile
.r maklib
*libary=libary/master:efile,efile/replace:efile
*libary=libary/index

; Now for high-segment version.
.com/com efileh=efile(h)
.r maklib
*libarh=libarh/master:efileh,efileh/replace:efileh
*libarh=libarh/index
