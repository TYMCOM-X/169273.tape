; CCL.CTL 14-Feb-79
; Compile and add module CCL.
;
.path sai:
.com/com ccl
.r maklib
*libary=libary/master:ccl,ccl/replace:ccl
*libary=libary/index

; Now for high-segment version.
.com/com cclh=ccl(h)
.r maklib
*libarh=libarh/master:cclh,cclh/replace:cclh
*libarh=libarh/index
  