dif pt0080.res,pt0080.rs0
dif pt0080.rrx,pt0080.rx0
dif pt0070.re1,pt0080.rs1
dif pt0080.re2,pt0080.rs2
dif pt0080.rr2,pt0080.rx2
dif pt0070.re3,pt0080.rs3
dif pt0080.re4,pt0080.rs4
dif pt0080.rr4,pt0080.rx4
;
del pt008#.rel,pt008#.srl,pt008#.cmd,pt008#.mdo,pt008#.mod,*.tmp
del pt008#.exe,pt008#.low,pt008#.shr,pt008#.rs#,pt008#.rx#,pt008#.hgh
    