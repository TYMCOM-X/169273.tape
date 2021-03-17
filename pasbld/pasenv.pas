$TITLE PASENV.PAS, last modified 1/10/84, zw
MODULE pasenv OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- global environment "public" definitions*)
$HEADER PASENV.HDR
$SYSTEM ENVUTL.INC
CONST version_save = 10;
VAR
start_data: BOOLEAN; (*mark beginning of saved data block*)
save_version: 0 .. 1000;
$PAGE PASCAL.INC template
$INCLUDE pasadr.typ
(* Options and related declarations *)
TYPE
optionlist = ( chk_ass_opt, (* check assertions *)
chk_cas_opt, (* check case statements *)
chk_com_opt, (* check array compatibility *)
chk_fld_opt, (* check field variant tags *)
chk_fil_opt, (* check file references and dereferences *)
chk_inp_opt, (* check input integers and reals *)
chk_poi_opt, (* check nil pointer dereferences *)
chk_str_opt, (* check substring bounds *)
chk_sub_opt, (* check array subscripts *)
chk_val_opt, (* check scalar assignments *)
sp_coe_opt, (* allow ADDRESS and pointer coercion functions *)
sp_ptr_opt, (* allow use of type PTR *)
sp_wor_opt, (* allow use of type MACHINE_WORD *)
map_opt, (* generate line/object code mapping *)
symbols_opt, (* list declared symbols *)
calls_opt, (* list called procedures *)
assembly_opt, (* list generated code *)
xref_opt, (* generate cross-reference *)
trace_opt, (* emit trace information *)
qblocks_opt, (* allow quick block analysis *)
optimize_opt); (* optimize code, overridden by debug *)
set_of_options = SET OF optionlist;
switch_ptr = ^ switch_node;
search_ptr = ^ search_node;
option_mode = ( opt_is_off, opt_is_auto, opt_is_on );
command_options = RECORD
  semantic_options: set_of_options;
  banner_opt: BOOLEAN; (* if banner pages desired *)
  code_opt: BOOLEAN; (* if code generation *)
  debug_opt: BOOLEAN; (* compile for use with debugger *)
  errors_opt: BOOLEAN; (* print an errors listing *)
  finish_opt: BOOLEAN; (* finish compilations after warnings *)
  global_opt: BOOLEAN; (* if .XRF global cross reference desired *)
  lsys_opt: BOOLEAN; (* list $SYSTEM files *)
  mainseg_opt: BOOLEAN; (* code is for main sharable overlay *)
  masking_opt: BOOLEAN; (* allow MASK(ATTENTION) to work *)
  names_opt: BOOLEAN; (* print file name at start of compilation *)
  overlay_opt: BOOLEAN; (* code will be overlaid *)
  quick_opt: option_mode; (* use checkout code generator *)
  source_opt: option_mode; (* if listing desired *)
  standard_opt: BOOLEAN; (* enforce standard Pascal *)
  statistics_opt: BOOLEAN; (* if stats desired after compilation *)
  terse_opt: BOOLEAN; (* if short error messages *)
  underflow_opt: BOOLEAN; (* signal MATHERROR on floating underflows *)
  page_length: 0..255; (* zero => pagination *)
  page_width: 0..255; (* columns per listing line *)
  alloc_mode: 0..99; (* dynamic allocation strategy code *)
  switches: switch_ptr; (* from enable/disable *)
  dump_switches: switch_ptr; (* from dump ( ) *)
  search_list: search_ptr; (* from search ( ) *)
  storage: 0..777777b; (* stack size *)
  ki_code_opt: BOOLEAN (* produce code runnable on a KI *)
END;
switch_string = STRING[32];
switch_node = PACKED RECORD
  next_switch: switch_ptr; (* singly linked *)
  enabled: BOOLEAN;
  name: PACKED ARRAY [1..*] OF CHAR
END;
search_node = PACKED RECORD
  next: search_ptr; (* singly linked *)
  name: PACKED ARRAY [1..*] OF CHAR
END;
(* Source line description. *)
CONST
line_length = 254;
TYPE
line_index = 0..255 (* line_length + 1 *);
line_string = STRING [line_length];
(* Source id record gives file, page, and line no of a statement *)
CONST
max_include_level = 3; (* permits 4 levels of files *)
max_file_no = 255; (* source_id parameters *)
max_page_no = 4095;
max_line_no = 65535;
TYPE
file_range = 0..max_file_no;
page_range = 0..max_page_no;
line_range = 0..max_line_no;
source_id = PACKED RECORD
  file_no: file_range; (* 0 is main source file *)
  page_no: page_range;
  line_no: line_range
END;
CONST
null_source: source_id := (0, 0, 0);
last_source: source_id := (max_file_no, max_page_no, max_line_no);
(* Error/warning message severity levels. *)
TYPE
severity_level = 0..3; (* no error, warning, error, fatal error *)
(* Generic types used by utilities *)
TYPE
int_type = INTEGER;
real_type = MINIMUM (REAL) .. MAXIMUM (REAL) PREC 16;
pos_int = 0 .. MAXIMUM (INTEGER);
parm_string = STRING[32]; (* large enough for most things *)
parm_number = 0..262144;
(* Global data *)
PUBLIC VAR
cur_source: source_id; (* current file/page/line *)
linect: INTEGER; (* total source lines read *)
inclct: INTEGER; (* source lines read from include files *)
max_severity: severity_level; (* highest severity error detected *)
err_count: INTEGER; (* number of errors in compilation *)
warnings: INTEGER; (* number of warnings in compilation *)
finish: BOOLEAN; (* go on and generate code? *)
quick: BOOLEAN; (* use quick code generator? *)
default_options: command_options; (* default global options *)
prog_options: command_options; (* global options--initialized to default_options,
				    augmented with program/module statement options *)
all_opts: set_of_options; (* all options specified on any block *)
opts_listing: BOOLEAN; (* true if any listing options specified *)
allc_required: BOOLEAN; (* true if ALLCONDITIONS ever specified *)
$PAGE PASIST.INC template
$INCLUDE TIMUTL.typ
$INCLUDE passpf.typ
$INCLUDE pasist.typ
PUBLIC VAR
root_name: nam; (* base of unbalanced binary name tree *)
env_name: nam; (* set in envmodule compilation *)
env_dtime: dtime_int; (* date/time of envmodule compilation *)
root_block: blk; (* imaginary outermost block containing standard names *)
cur_block: blk; (* innermost block in scope *)
ext_block: blk; (* block to represent all external routines *)
lex_block: blk; (* start of the lex_thread chain, ordered by level *)
blk_number: 0..2047; (* value given to cur_block^ *)
max_level: level_index; (* highest block level in the program *)
heap_chain: sym; (* chain of heap class var symbols *)
file_chain: sym; (* chain of file class var symbols *)
io_opsym: sym; (* input/output optimizer symbol *)
cdatesym: sym; (* COMPDATE predefined symbol *)
ctimesym: sym; (* COMPTIME predifined symbol *)
sym_vl_number: id_range; (* numbering for var and label symbols *)
vl_base: id_range; (* first free after symbol table initialization *)
sym_nvl_number: id_range; (* numbering for other kinds of symbols *)
nvl_base: id_range; (* first free after symbol table initialization *)
vl_list, (* chain of var and label symbols *)
vll_base: vl_link; (* chain after symbol table initialization *)
(*  Pointers to Standard Types  *)
PUBLIC VAR
type_int: typ; (* INTEGER *)
type_fullword: typ; (* MACHINE_WORD *)
type_non_neg: typ; (* non-negative integers *)
type_bool: typ; (* BOOLEAN *)
type_char: typ; (* CHAR *)
type_real: typ; (* REAL *)
type_ptr: typ; (* PTR *)
type_text: typ; (* TEXT *)
type_options: typ; (* IO_OPTIONS *)
stat_io: typ; (* IO_STATUS *)
stat_program: typ; (* PROGRAM_STATUS *)
stat_math: typ; (* MATH_STATUS *)
stat_special: typ; (* SPECIAL_STATUS *)
(*  Pointers to Standard File Symbols  *)
PUBLIC VAR
file_input: sym; (* INPUT *)
file_output: sym; (* OUTPUT *)
file_tty: sym; (* TTY *)
filettyoutput: sym; (* TTYOUTPUT *)
(*  Address Information for Code Generation  *)
PUBLIC VAR
size_init: unit_range; (* initialized static variable area *)
size_uninit: unit_range; (* uninitialized static variable area *)
size_cond: unit_range; (* condition cell area *)
$PAGE PASFIL.INC template
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
PUBLIC VAR
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
$PAGE PASLOG.INC template
$INCLUDE paslog.typ
PUBLIC VAR
log_record: log_file_record;
EXTERNAL
PROCEDURE log_write;
$PAGE PTMCON.INC template
(*********     Target Machine Customization     *********)
TYPE
fstr_3 = PACKED ARRAY [1..3] OF CHAR;
vstr_40 = STRING [40];
radix_type = ( hex_radix, octal_radix );
PUBLIC VAR
(*  TMPREFIX is a three-letter target machine prefix which is used in
    constructing the names of the individual passes of the compiler.  *)
tmprefix: fstr_3;
(*  TTYINAME and TTYONAME are the external file names to be associated with the
    terminal input and terminal output files.  *)
ttyiname: vstr_40;
ttyoname: vstr_40;
(*  REL EXTENSION is the default extension for the compiler's relocatable
    binary output file.  *)
rel_extension: fstr_3;
(*  HAVE CHECKOUT and HAVE OPTIMIZER indicate whether checkout and optimizing
    code generators are available for this compiler.  *)
have_checkout: BOOLEAN;
have_optimizer: BOOLEAN;
(*  RADIX is HexRadix or OctalRadix to indicate the output mode for target
    machine numeric constants.  *)
radix: radix_type;
(*  ADR WIDTH is the number of digits (in the target machine radix) to be
    printed for a target machine address value.  *)
adr_width: INTEGER;
(*  SREALPREC is the number of significant digits which the target machine
    will make available in a variable of type "Real" (i.e., a "single-
    precision" real number.  *)
srealprec: prec_type;
(*  ARR LEN SIZE and STR LEN SIZE are the number of bits required by the
    length words for a flexible array and a flexible string, respectively,
    in a dynamically allocated variable.  *)
arr_len_size: INTEGER;
str_len_size: INTEGER;
(*  SET LWB LIMIT and SET UPB LIMIT are the smallest and largest possible
    set elements.  SET SIZE LIMIT is the largest allowable base size for a
    set type (the base size of a set type is computed by AllocType).  *)
set_lwb_limit: INTEGER;
set_upb_limit: INTEGER;
set_size_limit: INTEGER;
(*  Extra elements may be allocated at the beginning or end of a set, if
    this will improve the efficiency of set operations.  When allocating
    a set, its first element will be rounded down to a multiple of
    SET LQUANTUM, or to SET LBASE if that is smaller.  The last element
    will be rounded up to one less than a multiple of SET UQUANTUM, or to
    SET UBASE if that is larger.  *)
set_lquantum: INTEGER;
set_uquantum: INTEGER;
set_lbase: INTEGER;
set_ubase: INTEGER;
(*  BYTE SIZE is the number of bits in the smallest addressable unit on the
    target machine.  *)
byte_size: INTEGER;
(*  INT PREC LIMIT is the maximum number of bits which can be used in an
    integer computation.  (Generally, this will be the same as the word
    size.  It needn't be larger than the number of bits required to
    represent a MachineWord variable.  *)
int_prec_limit: bit_range;
(*  QBL ALLOWED is a flag which indicates whether quick blocks may be
    constructed for this target machine.  *)
qbl_allowed: BOOLEAN;
$PAGE hp_put, hp_get
(* HP PUT is used to store blocks of static variables on the heap, so they can
   be saved between passes with Hpsave.  HP GET retrieves static variables from
   the heap after they have been read in with Hpload.  Note that the coding of
   these routines is extremely dependent on the implementation.
   HP PUT and HP GET are each called with the address of the first variable in
   a block and the address of the first variable past the end of the block.
   HP PUT creates a block on the heap, copies the variables to it, and returns
   a pointer to it.  HP GET takes a pointer to a block on the heap and copies
   the variables from it. *)
TYPE
save_pointer = ^ save_record;
save_record = ARRAY [1..*] OF machine_word;
PUBLIC
PROCEDURE hp_put ( start_addr, end_addr: INTEGER;
  VAR block_ptr: save_pointer ) OPTIONS special(coercions), nocheck;
VAR
i: INTEGER;
d: save_pointer;
BEGIN
  NEW (block_ptr, end_addr - start_addr);
  d := PTR (start_addr);
  FOR i := 1 TO end_addr - start_addr DO block_ptr^ [i] := d^ [i];
END;
PUBLIC
PROCEDURE hp_get ( start_addr, end_addr: INTEGER; block_ptr: save_pointer )
  OPTIONS special(coercions);
VAR
i: INTEGER;
d: save_pointer;
BEGIN
  d := PTR (start_addr);
  FOR i := 1 TO end_addr - start_addr DO d^ [i] := block_ptr^ [i];
END;
$PAGE miscellaneous
(*  These variables are only used in PASIFU.  They are included here only so
    that they can be preserved between passes.  *)
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasifu.typ
PUBLIC VAR
ch_index, ch_last: if_file_index;
ch_counter: CHAR;
(*  These variables are associated with the command file processor.  *)
$INCLUDE pascfm.typ
PUBLIC VAR
cmd_name_stack: cmd_stack;
cmd_list: pending_command;
last_cmd: pending_command;
auto_startup: BOOLEAN; (* set by NEWPAS *)
(*  These variables keep the addresses on the heap of the saved type and storage
    allocation tables.  *)
PUBLIC VAR
tal_tables: save_pointer;
alc_tables: save_pointer;
$PAGE end_data, getpas, putpas
VAR end_data: BOOLEAN; (*marks the end of above data*)
PUBLIC FUNCTION rdpas(fil: file_name; del: BOOLEAN): BOOLEAN
  OPTIONS SPECIAL(COERCIONS);
(*try to read TYM-Pascal environment from file, maybe delete file*)
VAR e: environment;
BEGIN
  CLOSE;
  rdpas := rdenv(fil, del, e);
  IF rdpas THEN BEGIN
    getenv(ORD(ADDRESS(start_data)), ORD(ADDRESS(end_data)), e);
    DISPOSE(e)
  END;
  rdpas := rdpas ANDIF (version_save = save_version)
END;
PUBLIC FUNCTION wrpas(fil: file_name): BOOLEAN OPTIONS SPECIAL(COERCIONS);
(*try to write TYM-Pascal environment to file, close all files*)
VAR e: environment;
BEGIN
  CLOSE; (*do not want to save file blocks*)
  save_version := version_save;
  putenv(ORD(ADDRESS(start_data)), ORD(ADDRESS(end_data)), e);
  wrpas := wrenv(fil, e);
  DISPOSE(e)
END.
