$PAGE TCRUTL.TYP, last modified 4/9/84, zw
$IFNOT tcrutltyp
(*TYM-Pascal temp-core utility*)

(*HEADER TCRUTL.HDR*)

TYPE
tcr_buffer = ^PACKED ARRAY [1..*] OF CHAR;
tcr_name = PACKED ARRAY [1..3] OF CHAR;
tcr_opcode = (tcr_fs,tcr_rf,tcr_df,tcr_wf,tcr_rd,tcr_dd);
tcr_address = INTEGER; (*0 .. 777777b;*)
tcr_length = INTEGER; (*0 .. 777777b;*)

$ENABLE tcrutltyp
$ENDIF
