(*TMPNAM.INC, last modified 12/20/83, zw*)
$IFNOT tmpnaminc

$SYSTEM TMPNAM.TYP

(*defined in PASASM.MAC*)

EXTERNAL FUNCTION tempname(name_code): FILE_NAME;

$ENABLE tmpnaminc
$ENDIF

   