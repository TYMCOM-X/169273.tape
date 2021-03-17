(*PASENV.INC, last modified 12/27/83, zw*)
$IFNOT pasenvinc

$SYSTEM UTLLIB.TYP

EXTERNAL FUNCTION wrpas(file_name): yes_no;
EXTERNAL FUNCTION rdpas(file_name; yes_no): yes_no;

$ENABLE pasenvinc
$ENDIF

