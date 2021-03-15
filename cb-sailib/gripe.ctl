; GRIPE.CTL 14-Feb-79
; Compile and add module GRIPE.
;
.path sai:
.com/com gripe
.r maklib
*libary=libary/master:gripe,gripe/replace:gripe
*libary=libary/index

; Now for high-segment version.
.com/com gripeh=gripe(h)
.r maklib
*libarh=libarh/master:gripeh,gripeh/replace:gripeh
*libarh=libarh/index
