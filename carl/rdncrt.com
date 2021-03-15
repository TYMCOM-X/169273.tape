:
r decmac
run=run 
autoru=autoru 
infpac=infpac 
dirlok=dirlok 
daytim=daytim 
dtime=dtime 
douuo=douuo 
prgdir=prgdir 
jobnum=jobnum 
mmmfls=mmmfls 
blokio=blokio 

:
ru pascal
@rdnpas
:
:com rdnlib.com
:
ru pascal
/kicode
@rdipas
:
r decmac
infpki=infpki 
dirlki=dirlki 
mflski=mflski
blioki=blioki

:
:com rdilib.com
