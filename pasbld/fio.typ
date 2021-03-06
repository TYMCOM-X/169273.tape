$PAGE FIO.TYP, last modified 5/11/84, zw

CONST
max_width = 254; (* maximum line width *)
max_plength = 254; (* maximum page length *)

TYPE
fio_width = 0..max_width;
fio_string = STRING[max_width];
file_block = RECORD
  file_var: TEXT; (* file variable to operate on *)
  file_title: FILE_NAME; (* true name of file, set on open *)
  pageno: 0..999999; (* page number of current page *)
  lineno: 0..999999; (* line number within page (local) *)
  column: 1..max_width; (* current printing column position. *)
  width: 0..max_width; (* maximum width of output line *)
  c_column: 0..max_width; (* continuation column; 0 => truncate *)
  plength: 0..max_plength; (* maximum length of output page; 0 => no limit *)
  new_page: PROCEDURE (VAR file_block); (* subroutine to call to perform a "page eject" *)
  page_header: PROCEDURE (VAR file_block) (* subroutine to call to write a page header *)
END;
    