$PAGE UTLKEY.TYP, last modified 3/21/84, zw
$IFNOT utlkeytyp

(*SYSTEM UTLTYP.TYP*)

TYPE
key_record = RECORD
  key: string_argument;
  abbreviate: 1 .. UPPERBOUND(string_argument);
  code: integer
END;
key_record_list = ARRAY [1 .. *] OF key_record;

$ENABLE utlkeytyp
$ENDIF
   