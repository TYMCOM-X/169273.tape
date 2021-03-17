:logfile BB2521.LOG
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
;  2  - assemble slot 2 (mux)
;  3  - assemble slot 3 (xpc)
;  M  - run merlin
; ALL - all of above
;  L  - load .bnd file
;  L1 - load .n01 file (consat)
;  L2 - load .n02 file (mux)
;  L3 - load .n03 file (xpc)
:stop

::X
:quit

::ALL
:sa="I",sb="0",sc="1",sd="2",se="3",sf="M"
::I
r nad
;xBB2521.CMD
:goto NEXT

::0
r nad
;xBB2521.C00
:goto NEXT

::1
r nad
;xBB2521.C01
:goto NEXT

::2
r nad
;xBB2521.C02
:goto NEXT

::3
r nad
;xBB2521.C03
:goto NEXT

::M
r merlin
@BB2521.MER
r (xexec)unbnd; bb2521
:goto NEXT

::L
r (wrs)rtyfix
r loadii
@bb2521.lm1
:goto NEXT

::L1
r (wrs)rtyfix
r loadii
s
2521
1
bb2521.n01
:goto NEXT

::L2
r (wrs)rtyfix
r loadii
s
2521
2
bb2521.n02
:goto NEXT

::L3
r (wrs)rtyfix
r loadii
s
2521
3
bb2521.n03
:goto NEXT
