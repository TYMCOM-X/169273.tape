$PAGE FLGUTL.INC, last modified 4/3/84, zw
$IFNOT flgutlinc
(*TYM-Pascal flag manipulation utility*)

(*HEADER FLGUTL.HDR*)

$SYSTEM TYPUTL.TYP
$SYSTEM FLGUTL.TYP

EXTERNAL FUNCTION flag(flag_pointer; flag_name): yes_no;
EXTERNAL PROCEDURE setflg(VAR flag_pointer; flag_name; yes_no);
EXTERNAL PROCEDURE delflg(VAR flag_pointer; flag_pointer);
EXTERNAL PROCEDURE ldflgs(VAR flag_pointer; VAR binary_file);
EXTERNAL PROCEDURE stflgs(VAR flag_pointer; VAR binary_file);

$ENABLE flgutlinc
$ENDIF

  