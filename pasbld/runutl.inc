$PAGE RUNUTL.INC, last modified 4/9/84, zw
$IFNOT runutlinc
(*TYM-Pascal program run utility*)

(*HEADER RUNUTL.HDR*)

$SYSTEM UTLLIB.TYP
$SYSTEM RUNUTL.TYP

EXTERNAL FUNCTION runoff: run_offset;
EXTERNAL FUNCTION runprg(file_name; run_offset): yes_no;

$ENABLE runutlinc
$ENDIF    