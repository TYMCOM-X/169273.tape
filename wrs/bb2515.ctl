:logfile BB2515.LOG
:$logging=$true
:parameters sa="ALL",sb="X",sc="X",sd="X",se="X",sf="X",sg="X",sh="X"
set logout
::NEXT
:slot=sa,sa=sb,sb=sc,sc=sd,sd=se,se=sf,sf=sg,sg=sh,sh="X"
:goto \slot\

::?
::HELP
;Valid parameters:
;  I  - assemble isis
;  0  - assemble node code
;  1  - assemble slot 1 (consat)
;  M  - run merlin
; ALL - all of above
;  L  - load .bnd file
;  L1 - load .n01 file (consat)
:stop

::X
:quit

::ALL
:sa="I",sb="0",sc="1",sd="M"
::I
r nad
;xBB2515.CMD
:goto NEXT

::0
r nad
;xBB2515.C00
:goto NEXT

::1
r nad
;xBB2515.C01
:goto NEXT

::M
r merlin
@BB2515.MER
r (xexec)unbnd; bb2515
:goto NEXT

::L
r (wrs)rtyfix
r loadii
@bb2515.lm1
:goto NEXT

::L1
r (wrs)rtyfix
r loadii
s
2353
1
bb2515.n01
:goto NEXT

::L2
r (wrs)rtyfix
r loadii
s
2515
2
bb2515.n02
:goto NEXT
