$PAGE TCRUTL.INC, last modified 4/9/84, zw
$IFNOT tcrutlinc
(*TYM-Pascal temp-core utility*)

(*HEADER TCRUTL.HDR*)

$SYSTEM TYPUTL.TYP
$SYSTEM TCRUTL.TYP

EXTERNAL FUNCTION tcrfun
  (tcr_name; tcr_opcode; tcr_address; VAR tcr_length): yes_no;

$ENABLE tcrutlinc
$ENDIF
 