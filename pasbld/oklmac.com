;oklmac.com, last modified 5/31/83, zw
;assemble ODM stuff on a KL10
:
run dtmpfl
odmmac.cmd
mac
:
r link
/run:dsk:decmac[1,4]/runoff
:
copy mmovlm.rel+infpac.rel, mmovlm.rel
copy ovlmki.rel+infpki.rel, ovlmki.rel
    