;UTLCRT.COM, last modified 4/11/84, zw
;TYM-Pascal utility libary
;This command file creates the utility library
;PDP10/TYMCOM-10

;compile the PASCAL modules
r (pasnew)pascal
runutl
tcrutl
timutl
flgutl
fioutl
keyutl
/exit

;assemble the MACRO modules
r macro
runmac=runmac
tcrmac=tcrmac
timmac=timmac

    