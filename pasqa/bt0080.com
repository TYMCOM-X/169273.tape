r (pasnew)odms
compile pt0080
build resident using /s paslib[,327106],forlib[,327106]
build main using pt0080
build pt008i using pt008i
build pt008k using pt008k
build pt008j using pt008j
build pt008a using pt008a
build pt008c using pt008c,pt008b
build pt008d using pt008d
exit
 