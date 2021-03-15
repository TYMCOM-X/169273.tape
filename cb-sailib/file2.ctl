; FILE2.CTL 14-Feb-79
; Compile and add module FILE2.
;
.path sai:
.com/com file2
.r maklib
*libary=libary/master:file2,file2/replace:file2
*libary=libary/index

; Now for high-segment version.
.com/com file2h=file2(h)
.r maklib
*libarh=libarh/master:file2h,file2h/replace:file2h
*libarh=libarh/index
