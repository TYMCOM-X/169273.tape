$PAGE KEYUTL.INC, last modified 4/9/84, zw
$IFNOT keyutlinc
(*TYM-Pascal key word utility*)

(*HEADER KEYUTL.HDR*)

$SYSTEM TYPUTL.TYP
$SYSTEM KEYUTL.TYP

EXTERNAL FUNCTION scnkey
  (generic_string; VAR positive_integer; char_set; VAR generic_string): yes_no;
EXTERNAL FUNCTION lkpkey(generic_string; key_list; VAR integer): yes_no;
EXTERNAL FUNCTION keystr(key_list; integer): string_argument;
EXTERNAL PROCEDURE srtkey(VAR key_list);

$ENABLE keyutlinc
$ENDIF
 