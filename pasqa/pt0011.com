:
r (pasgsrc)newpas
pt001e
@ct0010
:
:com pt001z.com
:
dif pt0010.res,pt0010.rs0
dif pt0010.res,pt0012.rs2
del pt001#.rel,pt001#.hgh,pt001#.low,pt001#.rs#
:
r (pasgsrc)newpas
/opt
@ct0010
:
:com pt001z.com
:
dif pt0010.reo,pt0010.rs0
dif pt0010.reo,pt0012.rs2
del pt001#.rel,pt001#.hgh,pt001#.low,pt001#.rs#
:
r (pasgsrc)newpas
/ki
@ct0010
:
:com pt001z.com
:
dif pt001i.res,pt0010.rs0
dif pt001i.res,pt0012.rs2
del pt001#.rel,pt001#.hgh,pt001#.low,pt001#.rs#
:
r (pasgsrc)newpas
/opt/ki
@ct0010
:
:com pt001z.com
:
dif pt001i.reo,pt0010.rs0
dif pt001i.reo,pt0012.rs2
del pt001#.rel,pt001#.hgh,pt001#.low,pt001#.rs#,pt001e.env
:
