;okilsd.com, last modified 5/31/83, zw
;do something for ODMS on a KI10
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
  