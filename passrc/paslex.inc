$PAGE PASLEX.INC, last modified 3/27/84, zw
$IFNOT paslexinc

(*SYSTEM PASPT.TYP*)

EXTERNAL VAR
token: token_type;
lsttkn: token_type;

EXTERNAL FUNCTION scan: symbols;
EXTERNAL PROCEDURE lexbgn;

$ENABLE paslexinc
$ENDIF
   