$PAGE UTLENV.INC, last modified 1/16/84, zw
$IFNOT utlenvinc

$SYSTEM UTLENV.TYP

EXTERNAL PROCEDURE putenv(INTEGER; INTEGER; VAR env);
EXTERNAL PROCEDURE getenv(INTEGER; INTEGER; env);
EXTERNAL FUNCTION wrenv(FILE_NAME; env): BOOLEAN;
EXTERNAL FUNCTION rdenv(FILE_NAME; BOOLEAN; VAR env): BOOLEAN;

$ENABLE utlenvinc
$ENDIF

 