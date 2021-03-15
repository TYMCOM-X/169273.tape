
"/BIOBAS.PAT - Check BLOCK-IO errors connected with the Base/


pat!252525252525
pat+1!0
pat+2!0
pat+3!0
pat+4!0
pat+5!0
pat+6!0
pat+7!0
pat+10!0
pat+11!0
pat+12!0
pat+13!0
pat+14!PUSHJ P,NODIE
pat+15!"/BIOBAS/
pat+16!400000+S$INFO,,0
errcon:
pat+17!WHATJB
scnser:

pat+20!hrrz t3,ldbbio(u)
pat+21!move t1,pat
pat+22!movem t1,biodat(t3)
pat+23!movsi t1,biodat(t3)
pat+24!hrri t1,biodat+1(t3)
pat+25!blt t1,biodat+100.-1(t3)
pat+26!move t1,biocsh(t3)
pat+27!pushj p,csval
pat+30!camn t1,csvcnt
pat+31!jrst .-1
pat+32!popj p,
pat+33!jfcl
pat+34!jfcl
pat+35!jfcl
pat+36!jfcl

pat+37!jumpe t2,pat+71
pat+40!hrri t3,biodat(t1)
pat+41!push p,t2
pat+42!movn t2,t2
pat+43!hrl t3,t2
pat+44!move t1,pat
pat+45!andi t1,17
pat+46!move t2,(t3)
pat+47!andi t2,17
pat+50!camn t1,t2
pat+51!jrst pat+54
pat+52!aobjn t3,.-4
pat+53!jrst pat+70

pat+54!aos pat+1
pat+55!move t2,(t3)
pat+56!movem t2,pat+2
pat+57!movem t3,pat+3
pat+60!hrlz t1,ldbbio(u)
pat+61!movem t1,pat+4
pat+62!hrri t1,pat+5
pat+63!blt t1,pat+13
pat+64!xct pat+14
pat+65!jrst pat+70
pat+66!jfcl
pat+67!jfcl

pat+70!pop p,t2
pat+71!hrrz t1,ldbbio(u)
pat+72!hrli t3,biochr(t1)
pat+73!popj p,

gobin+2!pushj p,pat+20
gotbuf+6!pushj p,pat+37

pat+100!pat:
patsiz/pat

