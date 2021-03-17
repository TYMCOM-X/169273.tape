$PAGE UTLSRC.TYP, last modified 3/21/84, zw
$IFNOT utlsrctyp

(*SYSTEM UTLTYP.TYP*)

CONST source_line_size = 254;

TYPE
source_line_cursor = 0 .. source_line_size + 1;
source_line_string = STRING[source_line_size];
source_search_list = ^source_search_record;
source_search_record = RECORD
  next_in_list: source_search_list;
  name: file_name (*directory name*)
END;
source_range = 0 .. #o277; (*7 bit value*)
source_page_range = 0 .. #o17777; (*13 bit value*)
source_line_range = 0 .. #o177777; (*16 bit value*)
source_position = PACKED RECORD (*36 bit PDP10 word*)
  source_number: source_range; (*source file number*)
  page_number: source_page_range; (*source page number in file*)
  line_number: source_line_range (*source line number in page*)
END;

CONST
null_source_position: source_position :=
  (MINIMUM(source_range),
   MINIMUM(source_page_range),
   MINIMUM(source_line_range));
last_source_position: source_position :=
  (MAXIMUM(source_range),
   MAXIMUM(source_page_range),
   MAXIMUM(source_line_range));

$ENABLE utlsrctyp
$ENDIF
