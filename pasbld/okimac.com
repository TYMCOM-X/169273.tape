;okimac.com, last modified 5/31/83, zw
;assemble ODM stuff on a KI10
;
run dtmpfl
odmmac.cmd
mac
:
r link
/run:dsk:decmac[1,4]/runoff
:
copy ovlmki.rel+infpki.rel, ovlmki.rel
    