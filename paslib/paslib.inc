(*PASLIB.INC, last modified 11/7/83, zw*)
$IFNOT paslibinc

$SYSTEM paslib.TYP

EXTERNAL function hpwrite(FILE_NAME; heap_area): BOOLEAN;
EXTERNAL function hpread(FILE_NAME; BOOLEAN): heap_area;

$ENABLE paslibinc
$ENDIF
    