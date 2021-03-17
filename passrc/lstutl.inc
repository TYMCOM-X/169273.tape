$PAGE LSTUTL.INC, last modified 3/21/84, zw
$IFNOT lstutlinc
(*TYM-Pascal listing utility*)

(*HEADER LSTUTL.HDR*)

$SYSTEM TYPUTL.TYP
$SYSTEM LSTUTL.TYP

EXTERNAL PROCEDURE lstrdy(VAR list_file);
EXTERNAL PROCEDURE lstnop(VAR list_file);
EXTERNAL PROCEDURE lstff(VAR list_file);
EXTERNAL PROCEDURE lstatt(VAR list_file; text_file);
EXTERNAL PROCEDURE lstopn(VAR list_file; file_name);
EXTERNAL PROCEDURE lstcls(VAR list_file);
EXTERNAL PROCEDURE lstpag(VAR list_file);
EXTERNAL PROCEDURE lstskp(VAR list_file);
EXTERNAL PROCEDURE lstnskp
  (VAR list_file; list_length_range; list_length_range );
EXTERNAL PROCEDURE lsttab(VAR list_file; list_width_range);
EXTERNAL PROCEDURE lststr(VAR list_file; generic_string);
EXTERNAL PROCEDURE lstlin(VAR list_file; generic_string);
EXTERNAL PROCEDURE lstspc(VAR list_file; positive_integer);

$ENABLE lstutlinc
$ENDIF
