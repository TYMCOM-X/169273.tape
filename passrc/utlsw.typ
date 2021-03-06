$PAGE UTLSW.TYP, last modified 3/21/84, zw
$IFNOT utlswtyp

(*SYSTEM UTLTYP.TYP*)

TYPE
switch_name = STRING[32];
switch_pointer = ^switch_record;
switch_record = PACKED RECORD (*note flex array*)
  next_switch: switch_pointer;
  enabled: yes_no;
  name: PACKED ARRAY [1 .. *] OF CHAR
END;

$ENABLE utlswtyp
$ENDIF
    