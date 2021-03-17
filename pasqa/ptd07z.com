:
ru ptd070
.b 31
.p
pt0030.dat
ptd070.dbg
.ss
.b 159 '.if sclr_vr<>three then .p'
.b 30 '.if chrt=''+'' then begin .d sc; .d sta; .k 30 end else .p'
.p
sclr_vr
.w ptr2^.elmt
rlnum
srlnum
numb
wrd
wrd:='debug!'
wrd
stsclr
lngst
.w subrcd
intar
.ss
.k 160
.ss
.ss
.ss
.d sc
.d sta
.k 123
.ss
.b 123 '.if rcdp.numb<>three then .p'
.p
chrt
ip
.ss
drl_vr
.p
.w rcdp
rlnum
srlnum
numb
wrd
stsclr
lngst
subrcd.intar
.d sta
.o 2
.k 18
.k 19
.k 20
.b 20
.p
.ss
.b 18 '.if rcdp.numb<>three then .p'
.k 50
.b 50
.p
.ss
blindp
.b 15 '.if blindp<>1 then .p'
.d sta
.o 1
.k 39
.b 39
.c 6
.p
rcdp
.d sta
.p
blindp
mesgp
.d sta
.p
blar_pr_mem_ct
.p
