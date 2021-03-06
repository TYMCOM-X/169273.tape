$PAGE PASFIL.INC, last modified 5/11/84, zw
$INCLUDE fio.inc

TYPE
title_string = STRING[80];
(* Lists of files used in compilation and logical sections within those files. *)

TYPE
source_ptr = ^src_id;
page_ptr = ^page_id;
src_id = PACKED RECORD
  file_no: file_range; (* number assigned to file *)
  pages: page_ptr; (* links pages in file *)
  next_file: source_ptr; (* sequential list *)
  incl_level: 0..max_include_level; (* inclusion level *)
  incl_page: page_range; (* physical page on which included *)
  system_file: BOOLEAN;
  FILE_NAME: PACKED ARRAY [1..*] OF CHAR (* from filename *)
END;
page_id = PACKED RECORD
  left_page, right_page: page_ptr; (* for xref after compilation *)
  next_page: page_ptr; (* sequential list of pages in a file *)
  following_page: page_ptr; (* next page in listing *)
  in_file: source_ptr; (* link to enclosing file *)
  page_number: page_range; (* "section" number *)
  incl_page: page_range; (* physical page no for xref *)
  subtitle: PACKED ARRAY [1..*] OF CHAR (* page subtitle if any *)
END;
(* Defines states of output files which are "open" across passes. *)

TYPE
file_status = ( unopened, (* has never been opened *)
prev_opened, (* opened in a previous pass, but not in this one *)
now_open ); (* file is now open *)
EXTERNAL VAR
main_file: FILE_NAME; (* names of important files *)
list_file: FILE_NAME;
rel_file: FILE_NAME;
list_explicit: BOOLEAN; (* list file name from ",list=source" *)
file_list: source_ptr; (* start of list of files referenced *)
no_files: file_range; (* number of current file *)
fin_source: source_id; (* last line read prior to abort *)
line: line_string; (* uppercased input line + 2 blanks *)
literal_line: line_string; (* input line as read *)
src_on: BOOLEAN; (* line should be listed *)
ln_enabled: BOOLEAN; (* line is enabled for compilation *)
end_of_file: BOOLEAN; (* true => last line has been read *)
src_selected: BOOLEAN; (* true => source on somewhere in program *)
main_title: title_string; (* first title which appears in input *)
global_title: title_string; (* current title *)
page_title: title_string; (* currently applicable $PAGE title *)
df_status: file_status; (* indicates current state of dump file *)
lf_status: file_status; (* ... of list file *)
elf_status: file_status; (* ... of error log file *)
dumpfb: file_block; (* FIO control blocks *)
listfb: file_block; (* .width is current page_width
					   .plength is current page_length *)
 