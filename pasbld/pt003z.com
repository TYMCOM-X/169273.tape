r link
pt0030/save=
pt0030/go
:
ru pt0030
pt0030.dat
pt0030.rs0
:
ru pt0030
pt0030.da1
pt0030.rs1
:
ru pt0030
pt0030.da2
pt0030.rs2
:
ru pt0030
pt0030.da3
pt0030.rs3
:
ru pt0030
pt0030.da4
pt0030.rs4
:
dif pt0030.res,pt0030.rs0
dif pt0030.re1,pt0030.rs1
dif pt0030.re2,pt0030.rs2
dif pt0030.re3,pt0030.rs3
dif pt0030.re4,pt0030.rs4
del pt0030.rel,pt0030.low,pt0030.hgh,pt0030.rs#
