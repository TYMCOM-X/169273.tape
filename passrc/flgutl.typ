$PAGE FLGUTL.TYP, last modified 4/11/84, zw
$IFNOT flgutltyp
(*TYM-Pascal flag manipulation utility*)

(*HEADER FLGUTL.HDR*)

(*SYSTEM TYPUTL.TYP*)

TYPE
flag_name = STRING[32];
flag_pointer = ^flag_record;
flag_record = PACKED RECORD
  next: flag_pointer;
  enabled: yes_no;
  name: PACKED ARRAY [1 .. *] OF CHAR
END;

$ENABLE flgutltyp
$ENDIF
 