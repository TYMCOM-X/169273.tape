$PAGE UTLRDR.TYP, last modified 2/3/84, zw
$IFNOT utlrdrtyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM UTLSRC.TYP*)

TYPE
read_line_procedure = FUNCTION(VAR generic_string): yes_no;
error_procedure = PROCEDURE(source_line_cursor);
reader_action_procedure = PROCEDURE(source_line_cursor);
reader_record = RECORD
  key: string_argument;
  abbrev: 1 .. UPPERBOUND(string_argument);
  action: reader_action_procedure
END;
reader_list = ARRAY [1 .. *] OF reader_record;

$ENABLE utlrdrtyp
$ENDIF
 