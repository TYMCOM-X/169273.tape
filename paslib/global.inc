(*PASDAT.INC, last modified 11/7/83, zw*)
$IFNOT pasdatinc

$SYSTEM PASDAT.TYP

EXTERNAL PROCEDURE datsav(FILE_NAME);
EXTERNAL PROCEDURE datget(FILE_NAME; BOOLEAN);

$ENABLE pasdatinc
$ENDIF
    