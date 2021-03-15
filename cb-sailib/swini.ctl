; SWINI.CTL 14-Feb-79
; Compile and add module SWINI.
;
.path sai:
.com/com swini
.r maklib
*libary=libary/master:swini,swini/replace:swini
*libary=libary/index

; Now for high-segment version.
.com/com swinih=swini(h)
.r maklib
*libarh=libarh/master:swinih,swinih/replace:swinih
*libarh=libarh/index
