r (pasnew)odms
compile ptd080
build resident using /s paslib[,327106],forlib[,327106]
build main using ptd080
build ptd08i using ptd08i
build ptd08k using ptd08k
build ptd08j using ptd08j
build ptd08a using ptd08a
build ptd08c using ptd08c,ptd08b
build ptd08d using ptd08d
exit
 