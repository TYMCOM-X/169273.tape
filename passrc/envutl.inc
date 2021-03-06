$PAGE ENVUTL.INC, last modified 1/16/84, zw
$IFNOT envutlinc
(*TYM-Pascal heap environment manipulation utilities*)

(*HEADER ENVUTL.HDR*)

$SYSTEM TYPUTL.TYP
$SYSTEM ENVUTL.TYP

EXTERNAL PROCEDURE putenv(INTEGER; INTEGER; VAR environment);
EXTERNAL PROCEDURE getenv(INTEGER; INTEGER; environment);
EXTERNAL FUNCTION wrenv(file_name; environment): yes_no;
EXTERNAL FUNCTION rdenv(file_name; yes_no; VAR environment): yes_no;

$ENABLE envutlinc
$ENDIF

 