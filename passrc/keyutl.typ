$PAGE KEYUTL.TYP, last modified 4/9/84, zw
$IFNOT keyutltyp
(*TYM-Pascal key word utility*)

(*HEADER KEYUTL.HDR*)

(*SYSTEM TYPUTL.TYP*)

TYPE
key_record = RECORD
  key: string_argument;
  abbreviate: 1 .. UPPERBOUND(string_argument);
  code: integer
END;
key_list = ARRAY [1 .. *] OF key_record;

$ENABLE keyutltyp
$ENDIF
   