"/BIOBAS.PAT - Check BLOCK-IO errors connected with the Base/

scnser:

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
pat+14!0
pat+15!0
pat+16!0
pat+17!0
pat+20!hrrz t3,ldbbio(u)
pat+21!move t1,pat
pat+22!movem t1,biodat(t1)
pat+23!move t1,biocsh(t3)
pat+24!pushj p,csval
pat+25!camn t1,csvcnt
pat+26!jrst .-1
pat+27!popj p,

pat+30!hrrz t1,ldbbio(u)
pat+31!move t1,biodat(t1)
pat+32!came t1,pat
pat+33!jrst pat+42
pat+34!aos pat+1
pat+35!move t1,uptime
pat+36!movem t1,pat+2
pat+37!hrrz t1,ldbbio(u)
pat+40!movei t1,biochr(t1)
pat+41!movem t1,pat+3
pat+42!hrrz t1,ldbbio(u)
pat+43!popj p,

pat+44!hrrzi t3,biochr(t1)
pat+45!came t3,pat+3
pat+46!jrst pat+66
pat+47!hrli t3,biochr(t1)
pat+50!hrri t3,pat+10
pat+51!blt t3,pat+14
pat+52!move t3,jbtppn(j)
pat+53!movem t3,pat+4
pat+54!move t3,jbtaun(j)
pat+55!movem t3,pat+5
pat+56!move t3,jbtfpn(j)
pat+57!movem t3,pat+6
pat+60!move t3,jbtnam(j)
pat+61!movem t3,pat+7
pat+62!jfcl
pat+63!jfcl
pat+64!jfcl
pat+65!jfcl
pat+66!hrli t3,biochr(t1)
pat+67!popj p,

gobin+2!pushj p,pat+20
blkib1!pushj p,pat+30
gotbuf+6!pushj p,pat+44

pat+70!pat:
patsiz/pat

  