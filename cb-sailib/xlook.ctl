; XLOOK.CTL 14-Feb-79
; Compile and add module XLOOK.
;
.path sai:
.com/com xlook
.r maklib
*libary=libary/master:xlook,xlook/replace:xlook
*libary=libary/index

; Now for high-segment version.
.com/com xlookh=xlook(h)
.r maklib
*libarh=libarh/master:xlookh,xlookh/replace:xlookh
*libarh=libarh/index
