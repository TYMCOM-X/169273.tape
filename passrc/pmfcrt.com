;pmfcrt.com, last modified 5/31/83, zw
;create PMF
:
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
:
;make backup copies
copy pmf.low, oldpmf.low
copy pmf.shr, oldpmf.shr
:
;link with utility library
r link
pmf/ssave=
pmf,pmfcmd,pmfinp,pmfput,pmfdef,pmfexp,pmfscn,pmferr
rdnlib/s
/go
  