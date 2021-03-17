$PAGE LSTUTL.TYP, last modified 4/3/84, zw
$IFNOT lstutltyp
(*TYM-Pascal listing utility*)

(*HEADER LSTUTL.HDR*)

(*SYSTEM UTLTYP.TYP*)

CONST
maximum_page_width = 254; (*maximum line width*)
maximum_page_length= 254; (*maximum page length*)

TYPE
list_width_range = 0 .. maximum_page_width; (*valid page width*)
list_length_range = 0 .. maximum_page_length; (*valid page length*)
list_line = STRING[maximum_page_width]; (*line which can be listed*)
list_file = RECORD
  output_file: text_file; (*file variable to operate on*)
  name: file_name; (*true name of file, set on open*)
  page_number: positive_integer; (*page number of current page*)
  line_number: 0 .. maximum_page_length+ 1; (*line number within page (local)*)
  column: 1 .. maximum_page_width; (*current printing column position.*)
  continue_column: 0 .. maximum_page_width; (*continuation col: 0 is truncate*)
  page_width: list_width_range; (*maximum width of output line*)
  page_length: list_length_range; (*maximum length of output page*)
  footing: PROCEDURE(VAR list_file); (*subroutine to write page footing*)
  eject: PROCEDURE(VAR list_file); (*subroutine to perform page eject*)
  heading: PROCEDURE(VAR list_file) (*subroutine to write page heading*)
END;

$ENABLE lstutltyp
$ENDIF
