;UTLTST.COM, last modified 4/11/84, zw
;TYM-Pascal utility library
;This command file tests the utility library.
;PDP10/TYMCOM-10

;test RUNUTL
r (pasnew)pascal
runtst
/exit
r link
runtst
runutl
runmac
/save runtst
/go
runtst
del runtst.low
del runtst.hgh
del runtst.rel

;test TCRUTL
r (pasnew)pascal
tcrtst
/exit
r link
tcrtst
tcrutl
tcrmac
/save tcrtst
/go
tcrtst
del tcrtst.low
del tcrtst.hgh
del tcrtst.rel

;test TIMUTL
r (pasnew)pascal
timtst
/exit
r link
timtst
timutl
timmac
/save timtst
/go
timtst
del timtst.low
del timtst.hgh
del timtst.rel

;test FLGUTL
r (pasnew)pascal
flgtst
/exit
r link
flgtst
flgutl
/save flgtst
/go
flgtst
del flgtst.low
del flgtst.hgh
del flgtst.rel

;test FIOUTL
r (pasnew)pascal
fiotst
/exit
r link
fiotst
fioutl
/save fiotst
/go
fiotst
del fiotst.low
del fiotst.hgh
del fiotst.rel

;test KEYUTL
r (pasnew)pascal
keytst
/exit
r link
keytst
keyutl
/save keytst
/go
keytst
del keytst.low
del keytst.hgh
del keytst.rel
    