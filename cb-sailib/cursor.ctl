; CURSOR.CTL 14-Feb-79
; Compile and add module CURSOR.
;
.path sai:
.com/com cursor
.r maklib
*libary=libary/master:cursor,cursor/replace:cursor
*libary=libary/index

; Now for high-segment version.
.com/com cursoh=cursor(h)
.r maklib
*libarh=libarh/master:cursoh,cursoh/replace:cursoh
*libarh=libarh/index
   