r (pasnew)odms
compile pt007o
build resident using /s paslib[,327106],forlib[,327106]
build main using pt0070
build pt007i using pt007i
build pt007j using pt007j
build pt007a using pt007a
build pt007c using pt007c,pt007b
build pt007d using pt007d
exit
   