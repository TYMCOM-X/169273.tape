; FILE.CTL 14-Feb-79
; Compile and add module FILE.
;
.path sai:
.com/com file
.r maklib
*libary=libary/master:file,file/replace:file
*libary=libary/index

; Now for high-segment version.
.com/com fileh=file(h)
.r maklib
*libarh=libarh/master:fileh,fileh/replace:fileh
*libarh=libarh/index
 