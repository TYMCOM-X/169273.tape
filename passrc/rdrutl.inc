$PAGE UTLRDR.INC, last modified 3/21/84, zw
$IFNOT utlrdrinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLRDR.TYP

EXTERNAL VAR dirflg: CHAR;

EXTERNAL PROCEDURE endrdr;
EXTERNAL PROCEDURE bgnrdr
  (read_line_procedure; error_procedure; error_procedure; reader_list);
EXTERNAL PROCEDURE srcrdr;

$ENABLE utlrdrinc
$ENDIF
   