;okllsd.com, last modified 5/31/83, zw
;do something for ODMS on a KL10
:
run mmdebm
lsgdeb
debug
debio
deblex
debbrk
debref
debasm
debdmp
debscp
debsym
debprt
debbol
dbsup

:
run mmdebm
lsdbki
dbugki
dbioki
dlexki
dbrkki
drefki
dasmki
ddmpki
dscpki
dsymki
dprtki
dbolki
dsupki

:
copy lsdbki.rel+lsdins.rel, lsdbki.rel
    