;utlcrt.com, last modified 5/31/83, zw
;create UTLLIB.REL and UTILIB.REL
:
;assemble UTLLIB routines
:
r decmac
runmac=runmac
infpac=infpac 
dirlok=dirlok 
timmac=timmac
douuo=douuo 
prgdir=prgdir 
jobnum=jobnum 
mmmfls=mmmfls 
blokio=blokio 
corout=corout
tmpcor=tmpcor
pasasm=pasasm

:
;assemble UTILIB routines
:
r decmac
dirlki=dirlki 
mflski=mflski
blioki=blioki
infpki=infpki

:
;compile UTLLIB routines
:
run (pasnew)pascal
/noquick
runutl
timutl
envutl
lookup
filutl
query
pasdir
diratr
cmdutl
tempfi/enable(P10)
fio
versio
swini
/exit
:
;compile UTILIB routines
:
run (pasnew)pascal
/kicode
/noquick
rd05ki=timutl
rd06ki=runutl
rd00ki=lookup
rd01ki=filutl
rd02ki=query
rd03ki=pasdir
rd04ki=diratr
rd07ki=cmdutl
rd08ki=tempfi/enable(P10)
rd09ki=fio
rd10ki=versio
rd11ki=swini
rd12ki=envutl
/exit
:
;link UTLLIB.REL
:
r (upl)fudge2
utllib= fio, versio, lookup, filutl, query, infpac, pasdir, diratr, dirlok,
 cmdutl, douuo, prgdir, tempfi, jobnum, envutl, mmmfls, tmpcor, corout,
 runutl, runmac, timutl, timmac, swini, pasasm, blokio/a
utllib= utllib/x

:
;link UTILIB.REL
:
r (upl)fudge2
utilib= rd09ki, rd10ki, rd00ki, rd01ki, rd02ki, infpki, rd03ki, rd04ki, dirlki,
 rd07ki, douuo, prgdir, rd08ki, jobnum, rd12ki, mflski, tmpcor, corout,
 rd06ki, runmac, rd05ki, timmac, rd11ki, pasasm, blioki/a
utilib= utilib/x

  