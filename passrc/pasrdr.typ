$PAGE PASRDR.TYP, last modified 3/21/84, zw
$IFNOT pasrdrtyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM UTLSRC.TYP*)

TYPE
title_string = STRING[80]; (*for page titles*)
page_pointer = ^page_record; (*for pages*)
source_pointer = ^source_record; (*for source files*)
source_record = PACKED RECORD (*note flex-array*)
  number: source_range; (*number assigned to file*)
  page_list: page_pointer; (*links pages in file*)
  next_source: source_pointer; (*sequential list of files*)
  previous_source: source_pointer; (*stack of included files*)
  include_level: positive_integer; (*inclusion level, zero is main file*)
  physical_page: positive_integer; (*physical page on which included*)
  is_system: yes_no; (*is this a system file?*)
  include_line_count: positive_integer; (*saved ilncnt when stacked*)
  name: PACKED ARRAY [1..*] OF CHAR (*full name of source file*)
END;
page_record = PACKED RECORD (*note flex_array*)
  number: source_page_range; (*number assigned to this page*)
  source_file: source_pointer; (*link to enclosing file*)
  next_in_file: page_pointer; (*sequential list of pages in a file*)
  next_in_list: page_pointer; (*next page in listing*)
  physical_page: positive_integer; (*physical page number*)
  left_page, right_page: page_pointer; (*for xref*)
  subtitle: PACKED ARRAY [1..*] OF CHAR (*page subtitle, if any*)
END;

$ENABLE pasrdrtyp
$ENDIF
 