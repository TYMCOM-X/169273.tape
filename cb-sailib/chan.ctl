; CHAN.CTL 14-Feb-79
; Compile and add module CHAN.
;
.path sai:
.com/com chan
.r maklib
*libary=libary/master:chan,chan/replace:chan
*libary=libary/index

; Now for high-segment version.
.com/com chanh=chan(h)
.r maklib
*libarh=libarh/master:chanh,chanh/replace:chanh
*libarh=libarh/index
 