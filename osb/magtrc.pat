"/patch to catch zero pages in MAGNUM relations/
mapio:
pat!push p,t1
pat+1!move t1,devfil(f)
pat+2!came T1,100
pat+3!jrst pat+12
pat+4!hllz t1,devext(f)
pat+5!came t1,101
pat+6!jrst pat+12
pat+7!move t1,devppn(f)
pat+10!camn t1,102
pat+11!jrst pat+14
pat+12!pop p,t1
pat+13!popj p,

pat+14!pushj p,unlfil
errcon:
pat+15!pushj p,errset
pat+16!jrst kjstop
pat+17!pushj p,errtty
pat+20!pushj p,inlmes
pat+21!"/BINGO/
pat+22!"/!!! -/
pat+23!"/ Unde/
pat+24!"/sirab/
pat+25!"/le Fi/
pat+26!"/le Op/
pat+27!"/erati/
pat+30!"/on/
pat+31!pushj p,uuopcp
pat+32!jrst kjstop

filrib:
pat+33!pushj p,pat
pat+34!move t1,t2
pat+35!jrst dlttci+1

dlttci/jrst pat+33

mapio:
pat+36!push p,t1
pat+37!jumpl t1,pat+44
pat+40!subi t1,(t4)
pat+41!caig t1,1
pat+42!jrst pat+44
pat+43!pushj p,pat
pat+44!pop p,t1
pat+45!move m,t1
pat+46!jrst kfcre2+2

kfcre2+1/jrst pat+36

pat+47!pat:
patsiz/pat
  