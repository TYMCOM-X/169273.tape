;This COM file puts together the SCRMAC User's Manual
do pmf
scrmac.tmp=scrmac.doc/nopascal/lib:scrmac[31024,320162]
scrmac.ex2=scrmac.ex1/nopascal/lib:scrmac[31024,320162]

do scribe
scrmac.txt=scrmac.tmp
scrmac.ex3=scrmac.ex2

do ped
l scrmac.txt
1,$ s /%/#/a
$d
$a scrmac.ex3
w scrmac.txt

q
del scrmac.ex2,scrmac.tmp
 