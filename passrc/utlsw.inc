$PAGE UTLSW.INC, last modified 3/21/84, zw
$IFNOT utlswinc

$INCLUDE UTLTYP.TYP
$INCLUDE UTLSW.TYP

EXTERNAL FUNCTION sw(switch_pointer; switch_name): yes_no;
EXTERNAL FUNCTION enasw(switch_pointer; switch_name; yes_no): switch_pointer;
EXTERNAL PROCEDURE popsw(VAR switch_pointer; switch_pointer);
EXTERNAL PROCEDURE wrsw(switch_pointer; VAR binary_file);
EXTERNAL FUNCTION rdsw(VAR binary_file): switch_pointer;

$ENABLE utlswinc
$ENDIF

