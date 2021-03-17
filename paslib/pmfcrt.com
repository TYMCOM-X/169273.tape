;pmfcrt.pco, last modified 4/2/83, zw
;this command file creates the PASCAL MACRO FACILITY
;
;compile the PASCAL modules
r (pasnew)pascal
/optimize
pmf
pmfcmd
pmfinp
pmfput
pmfdef
pmfexp
pmfscn
pmferr
/exit
;
;link with utility library
r link
pmf/ssave=
pmf,pmfcmd,pmfinp,pmfput,pmfdef,pmfexp,pmfscn,pmferr
/s utl
/go
