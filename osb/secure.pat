"/SECURE.PAT - System security patch #2/
filuuo:
pat!move t1,devfil(f)
pat+1!came t1,pat+26
pat+2!jrst ulook3
pat+3!hlrz t1,devext(f)
pat+4!caie t1,"/   DAT/
pat+5!jrst ulook3
pat+6!hlrz t1,jbtfpn(j)
pat+7!cain t1,1
pat+10!jrst ulook3
pat+11!hrrz t1,ttytab(j)
pat+12!hrrz t1,ddbldb(t1)
pat+13!jumpe t1,pat+22
scnser:
pat+14!move t1,ldblog(t1)
pat+15!tlnn t1,001700
filuuo:
pat+16!jrst pat+22
pat+17!move t1,jbtaun(j)
pat+20!tlnn t1,777770
pat+21!jrst ulook3
pat+22!jrst logoff
pat+23!jfcl
pat+24!jfcl
pat+25!jfcl
pat+26!"/EMPIRE/

ulook2+1/jrst pat
ulook2+4/jrst pat
pat+27!pat:
patsiz/pat
   