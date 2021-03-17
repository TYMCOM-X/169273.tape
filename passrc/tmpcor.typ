(*TMPCOR.TYP, last modified 12/20/83, zw*)
$IFNOT tmpcortyp

TYPE
tmpcor_name = PACKED ARRAY [1..3] OF CHAR;
tmpcor_opcode = (tmpcor_fs,tmpcor_rf,tmpcor_df,tmpcor_wf,tmpcor_rd,tmpcor_dd);
tmpcor_addr = INTEGER; (*0 .. 777777b;*)
tmpcor_length = INTEGER; (*0 .. 777777b;*)

$ENABLE tmpcortyp
$ENDIF
 