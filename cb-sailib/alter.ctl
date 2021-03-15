; ALTER.CTL 14-Feb-79
; Compile and add module ALTER.
;
.path sai:
.com/com alter
.r maklib
*libary=libary/master:alter,alter/replace:alter
*libary=libary/index

; Now for high-segment version.
.com/com alterh=alter(h)
.r maklib
*libarh=libarh/master:alterh,alterh/replace:alterh
*libarh=libarh/index
