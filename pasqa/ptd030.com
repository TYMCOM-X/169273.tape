:
ru ptd030
.b 194 '.if sclr_vr<>three then .p'
.b 100 '.if chrt=''+'' then begin .d sc; .d sta; .k 100 end else .p'
.b 222 '.if rcdp.numb<>three then .p'
.b 64
.p
pt0030.dat
ptd030.dbg
sclr_vr
.w ptr2^
.w elmt
rlnum
numb
wrd
wrd := 'debug!'
wrd
stsclr
lngst
.w subrcd
intar
.ss
.d loc
.k 195
.c 0
.p
chrt
ip
.ss
drl_vr
.p
.k 222
rcd_num
.w rcdp
rlnum
numb
wrd
stsclr
lngst
.w subrcd
intar
.d sc
.d sta
.o 2
flnmp
.ss
.k 227
.o pt0030@tp_fl_ops:
flnmp
.p
.k 64
mesgp
.p
  