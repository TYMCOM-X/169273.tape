$PAGE UTLCMD.TYP, last modified 4/3/84, zw
$IFNOT utlcmdtyp

(*SYSTEM UTLTYP.TYP*)

TYPE
command_line_string = STRING[80];
command_line_cursor = 0 .. 81;
command_list = ^command_record;
command_record = RECORD
  next: command_list;
  eof: yes_no;
  too_long: yes_no;
  text: PACKED ARRAY [1..*] OF CHAR
end;

$ENABLE utlcmdtyp
$ENDIF