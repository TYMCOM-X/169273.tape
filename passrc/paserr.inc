$PAGE PASERR.INC, last modified 3/26/84, zw
$IFNOT paserrinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLSRC.TYP
$SYSTEM PASCAL.TYP

$SYSTEM PASERR.TYP

EXTERNAL EXCEPTION abort;

EXTERNAL PROCEDURE errsrc
  (error_code; source_position; generic_string; source_line_cursor);
EXTERNAL PROCEDURE errtxt(error_code; generic_string);
EXTERNAL PROCEDURE error(error_code);
EXTERNAL PROCEDURE elfopn;
EXTERNAL PROCEDURE elfcls;

$ENABLE paserrinc
$ENDIF
