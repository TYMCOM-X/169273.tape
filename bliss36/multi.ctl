;	MULTI.CTL file to create SITBOL %5
;
;	Files needed:
;		SNO???.MAC & STBERR.MAC
;		SITBOL.CMD & MULTI.CCL
;		MACTEN %2(26)-6 (on release tape)
;		(SIT'S version of MACTEN is REQUIRED for the
;		correct assembly of ABV'NME macro in SNODAT)
;
;
;	Files created:
;		SNOBOL.EXE & STBERR.EXE
;		SNOBOL.MAP & SNOBOL.GLB
;
;Copyright (c) 1977,1978  Stevens Institute of Technology, Hoboken, N.J.
;All rights reserved.
;
.set w ver
.r setsrc
*scan
; Check for macten
.r pip
*macten.unv=macten.unv
; Compile sources
.ty sitbol.cmd
.com/com @sitbol
; Load .rel's to form SITBOL
.ty multi.ccl
.r link
*@multi
.ssave
; Make STBERR (error file)
.r link
*stberr/g
.ssave
; Make GLOB listing
.R GLOB
*SNOFIO,SNOSYN,SNOINT,SNOERR,SNOSTR,SNOSMR,SNODFD,SNOEXT,SNODFF
*SNOARY,SNOKEY,SNOTBL,SNOSYM,SNOGC,SNOTRC,SNOCVT,SNOPG,SNOPL,SNONUM
*SNOLEX,SNODEB,SNOPAV,SNODAT,STBERR
*SNOBOL.GLB=
; Check whats there
.dir/noop/slow
;[end of SITBOL.CTL]
 