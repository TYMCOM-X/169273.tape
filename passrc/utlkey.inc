$PAGE UTLKEY.INC, last modified 3/21/84, zw
$IFNOT utlkeyinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLKEY.TYP

EXTERNAL FUNCTION scnkey
  (generic_string; VAR positive_integer; char_set; VAR generic_string): yes_no;
EXTERNAL FUNCTION lkpkey(generic_string; key_record_list; VAR integer): yes_no;
EXTERNAL PROCEDURE srtkey(VAR key_record_list);

$ENABLE utlkeyinc
$ENDIF
  