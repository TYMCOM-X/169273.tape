(*RUN.INC, last modified 12/20/83, zw*)
$IFNOT runinc

(*defined in RUN.MAC*)

EXTERNAL PROCEDURE run(PACKED ARRAY [1..*] OF CHAR; INTEGER);

$ENABLE runinc
$ENDIF
  