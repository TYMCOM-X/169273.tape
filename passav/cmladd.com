;cmladd.com, last modified 5/31/83, zw
;compile/assemble additional things for PASCAL
:
run pascal
versio
fio

r decmac
tmpcor=tmpcor
pasasm=pasasm
corout=corout

    