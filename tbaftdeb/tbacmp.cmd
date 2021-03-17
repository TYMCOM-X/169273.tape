; See (spunkdev)build.mem if you don't know what you are doing.
; This loads and saves (not compile!) the code generator TBACMP

; to load and save a code generator with a profile in it, make a copy of this
; command file with a '/PROFIL:TBACMP' after PCTRAP  and remove the '/SSAVE:'
; at the end.
(SYS)JOBDAT.REL
RUNDAT.REL
DATAC.REL
TBLDAT.REL
PARDAT.REL
PCTRAP.REL
;/PROFIL:TBACMP ;add this line for a profile
DECLAR.REL
COMPIL.REL
CSUBS.REL
LSUBS.REL
RFGEN.REL
TABLES.REL
PAGER.REL
LNDRY.REL
OPEN.REL
DOECHO.REL
TERCMD.REL
TERINP.REL
TEROUT.REL
TIE.REL, TIEMAC.REL
PARSE9.REL
LOOKMD.REL
LODSTR.REL
COMLOD.REL
TRNSUT.REL
xchrg.rel
/SSAVE:TBACMP  ; remove for profile
