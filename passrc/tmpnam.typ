(*TMPNAM.TYP, last modified 12/20/83, zw*)
$IFNOT tmpnamtyp

TYPE name_code = PACKED ARRAY [1..3] OF CHAR;

$ENABLE tmpnamtyp
$ENDIF

   