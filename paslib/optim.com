do pascal
optim,optim=optim/enable:trace/deb/stat

do link
clustr
build
print
cost
optim
dispos
/g
ssave clustr
do clustr
