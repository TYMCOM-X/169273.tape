r (pasnew)pascal
pt0dp0
/exit
;
r decmac
pt0dpa=pt0dpb
pt0dps=pt0dpt
pt0dpm=pt0dpn
pt0dpd=pt0dpe

;
r link
@pt0dpa
;
ru pt0dpa
pt0dpa.dat
pt0dpa.rs0
;
ru pt0dps
pt0dps.dat
pt0dps.rs0
;
ru pt0dpm
pt0dpm.dat
pt0dpm.rs0
;
ru pt0dpd
pt0dpd.dat
pt0dpd.rs0
;
dif pt0dpa.res,pt0dpa.rs0
dif pt0dps.res,pt0dps.rs0
dif pt0dpm.res,pt0dpm.rs0
dif pt0dpd.res,pt0dpd.rs0
del pt0dp#.rel,pt0dp#.low,pt0dp#.hgh,pt0dp#.rs#
    