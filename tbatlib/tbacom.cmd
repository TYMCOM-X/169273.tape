echo
no debug
!
! Here be files for which we only need a .REL file.
! Do a GET and a CDE 0.
get prosub
cde 0
get daynum
cde 0
get exitpe
cde 0
get sort1
cde 0
get forpak
cde 0
get gets
cde 0
get stperf
cde 0
get tmpcor
cde 0
!
! Here be files for which we need a .SHR image.  Do whatever
! compilation is required and a SAVE BINARY.
get cnest
save binary
y
get format
save binary
y
get pertst
library (tbatlib)sbclib
save binary
y
get tca
do tca
save binary
y
  