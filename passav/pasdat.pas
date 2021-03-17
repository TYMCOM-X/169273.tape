$TITLE PASDAT - global data for the Pascal compiler
$LENGTH 42

module pasdat;

var start_data: boolean;      (* see dat_save, dat_get *)

var save_version: 0 .. 1000;
$PAGE PASCAL.INC template

$INCLUDE pasadr.typ

(* Options and related declarations *)

type
  optionlist = (
        chk_ass_opt,            (* check assertions *)
        chk_cas_opt,            (* check case statements *)
        chk_com_opt,            (* check array compatibility *)
        chk_fld_opt,            (* check field variant tags *)
        chk_fil_opt,            (* check file references and dereferences *)
        chk_inp_opt,            (* check input integers and reals *)
        chk_poi_opt,            (* check nil pointer dereferences *)
        chk_str_opt,            (* check substring bounds *)
        chk_sub_opt,            (* check array subscripts *)
        chk_val_opt,            (* check scalar assignments *)
        sp_coe_opt,             (* allow ADDRESS and pointer coercion functions *)
        sp_ptr_opt,             (* allow use of type PTR *)
        sp_wor_opt,             (* allow use of type MACHINE_WORD *)
        map_opt,                (* generate line/object code mapping *)
        symbols_opt,            (* list declared symbols *)
        calls_opt,              (* list called procedures *)
        assembly_opt,           (* list generated code *)
        xref_opt,               (* generate cross-reference *)
        trace_opt,              (* emit trace information *)
        qblocks_opt,            (* allow quick block analysis *)
        optimize_opt);          (* optimize code, overridden by debug *)

  set_of_options = set of optionlist;

  switch_ptr = ^ switch_node;
  search_ptr = ^ search_node;

  option_mode = ( opt_is_off, opt_is_auto, opt_is_on );

  command_options = 
    record
      semantic_options: set_of_options;
      banner_opt: boolean;                      (* if banner pages desired *)
      code_opt: boolean;                   (* if code generation *)
      debug_opt: boolean;              (* compile for use with debugger *)
      errors_opt: boolean;              (* print an errors listing *)
      finish_opt: boolean;                 (* finish compilations after warnings *)
      global_opt: boolean;                      (* if .XRF global cross reference desired *)
      lsys_opt: boolean;                        (* list $SYSTEM files *)
      mainseg_opt: boolean;			(* code is for main sharable overlay *)
      masking_opt: boolean;		(* allow MASK(ATTENTION) to work *)
      names_opt: boolean; (* print file name at start of compilation *)
      overlay_opt: boolean;                 (* code will be overlaid *)
      quick_opt: option_mode;               (* use checkout code generator *)
      source_opt: option_mode;                  (* if listing desired *)
      standard_opt: boolean;               (* enforce standard Pascal *)
      statistics_opt: boolean;             (* if stats desired after compilation *)
      terse_opt: boolean;                  (* if short error messages *)
      underflow_opt: boolean;		(* signal MATHERROR on floating underflows *)
      page_length: 0..255;        (* zero => pagination *)
      page_width: 0..255;         (* columns per listing line *)
      alloc_mode: 0..99;        (* dynamic allocation strategy code *)
      switches: switch_ptr;       (* from enable/disable *)
      dump_switches: switch_ptr;  (* from dump ( ) *)
      search_list: search_ptr; (* from search ( ) *)
      storage: 0..777777b;        (* stack size *)
      ki_code_opt: boolean	  (* produce code runnable on a KI *)
    end;

  switch_string = string[32];
  switch_node = 
    packed record
      next_switch: switch_ptr;    (* singly linked *)
      enabled: boolean;
      name: packed array [1..*] of char
    end;

  search_node = 
    packed record
      next: search_ptr;    (* singly linked *)
      name: packed array [1..*] of char
    end;

(* Source line description. *)

const line_length = 254;
type  line_index = 0..255 (* line_length + 1 *);
      line_string = string [line_length];


(* Source id record gives file, page, and line no of a statement *)

const
  max_include_level = 3;        (* permits 4 levels of files *)
  max_file_no = 255;                    (* source_id parameters *)
  max_page_no = 4095;
  max_line_no = 65535;

type
  file_range = 0..max_file_no;
  page_range = 0..max_page_no;
  line_range = 0..max_line_no;
  source_id =
      packed record
        file_no: file_range;    (* 0 is main source file *)
        page_no: page_range;
        line_no: line_range
      end;

const
   null_source: source_id := (0, 0, 0);
   last_source: source_id := (max_file_no, max_page_no, max_line_no);

(* Error/warning message severity levels. *)

type
  severity_level = 0..3;                (* no error, warning, error, fatal error *)


(* Generic types used by utilities *)

type
  int_type = integer;
  real_type = minimum (real) .. maximum (real) prec 16;
  pos_int = 0 .. maximum (integer);
  parm_string = string[32];             (* large enough for most things *)
  parm_number = 0..262144;


(* Global data *)

public var
  cur_source: source_id;                (* current file/page/line *)
  linect: integer; (* total source lines read *)
  inclct: integer; (* source lines read from include files *)

  max_severity: severity_level;         (* highest severity error detected *)
  err_count: integer;                   (* number of errors in compilation *)
  warnings: integer; (* number of warnings in compilation *)
  finish: boolean;                      (* go on and generate code? *)
  quick: boolean;                       (* use quick code generator? *)

  default_options: command_options;     (* default global options *)

  prog_options: command_options; (* global options--initialized to default_options,
                                    augmented with program/module statement options *)
  all_opts: set_of_options;             (* all options specified on any block *)

  opts_listing: boolean; (* true if any listing options specified *)

  allc_required: boolean; (* true if ALLCONDITIONS ever specified *)
$PAGE PASIST.INC template
$INCLUDE dtime.typ
$INCLUDE passpf.typ
$INCLUDE pasist.typ

public var
  root_name: nam;                               (* base of unbalanced binary name tree *)
  env_name: nam;                                (* set in envmodule compilation *)
  env_dtime: dtime_int;                         (* date/time of envmodule compilation *)

  root_block: blk;                              (* imaginary outermost block containing standard names *)
  cur_block: blk;                               (* innermost block in scope *)
  ext_block: blk; (* block to represent all external routines *)
  lex_block: blk; (* start of the lex_thread chain, ordered by level *)
  blk_number: 0..2047;                  (* value given to cur_block^ *)
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

public var
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

public var
  file_input: sym; (* INPUT *)
  file_output: sym; (* OUTPUT *)
  file_tty: sym; (* TTY *)
  filettyoutput: sym; (* TTYOUTPUT *)

(*  Address Information for Code Generation  *)

public var
  size_init: unit_range; (* initialized static variable area *)
  size_uninit: unit_range; (* uninitialized static variable area *)
  size_cond: unit_range; (* condition cell area *)
$PAGE PASFIL.INC template
$INCLUDE fio.inc

type title_string = string[80];

(* Lists of files used in compilation and logical sections within those files. *)

type
  source_ptr = ^src_id;
  page_ptr = ^page_id;
  src_id =
      packed record
        file_no: file_range;            (* number assigned to file *)
        pages: page_ptr;                    (* links pages in file *)
        next_file: source_ptr;              (* sequential list *)
        incl_level: 0..max_include_level;   (* inclusion level *)
        incl_page: page_range;              (* physical page on which included *)
        system_file: boolean;
        file_name: packed array [1..*] of char   (* from filename *)
      end;

  page_id =
    packed record
      left_page, right_page: page_ptr;    (* for xref after compilation *)
      next_page: page_ptr;                (* sequential list of pages in a file *)
      following_page: page_ptr;         (* next page in listing *)
      in_file: source_ptr;                (* link to enclosing file *)
      page_number: page_range;            (* "section" number *)
      incl_page: page_range;              (* physical page no for xref *)
        subtitle: packed array [1..*] of char  (* page subtitle if any *)
    end;

(* Defines states of output files which are "open" across passes. *)

type
  file_status =
     (  unopened, (* has never been opened *)
        prev_opened, (* opened in a previous pass, but not in this one *)
        now_open  ); (* file is now open *)

public var
  main_file: file_name;   (* names of important files *)
  list_file: file_name;
  rel_file: file_name;

  list_explicit: boolean; (* list file name from ",list=source" *)

  file_list: source_ptr;                (* start of list of files referenced *)
  no_files: file_range;                 (* number of current file *)

  fin_source: source_id;                (* last line read prior to abort *)

  line: line_string;                    (* uppercased input line + 2 blanks *)
  literal_line: line_string;            (* input line as read *)
  src_on: boolean;                      (* line should be listed *)
  ln_enabled: boolean;                     (* line is enabled for compilation *)
  end_of_file: boolean;                 (* true => last line has been read *)

  src_selected: boolean;                (* true => source on somewhere in program *)
  main_title: title_string;             (* first title which appears in input *)
  global_title: title_string;           (* current title *)
  page_title: title_string;             (* currently applicable $PAGE title *)

  df_status: file_status;               (* indicates current state of dump file *)
  lf_status: file_status;               (* ... of list file *)
  elf_status: file_status;              (* ... of error log file *)

  dumpfb: file_block;                   (* FIO control blocks *)
  listfb: file_block;                   (* .width is current page_width
                                           .plength is current page_length *)
$PAGE PASLOG.INC template

$INCLUDE paslog.typ

public var
    log_record: log_file_record;

external procedure log_write;
$PAGE PTMCON.INC template
(*********     Target Machine Customization     *********)

type
    fstr_3 = packed array [1..3] of char;
    vstr_40 = string [40];
    radix_type = ( hex_radix, octal_radix );


public var

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

    have_checkout: boolean;
    have_optimizer: boolean;


(*  RADIX is HexRadix or OctalRadix to indicate the output mode for target
    machine numeric constants.  *)

    radix: radix_type;


(*  ADR WIDTH is the number of digits (in the target machine radix) to be
    printed for a target machine address value.  *)

    adr_width: integer;


(*  SREALPREC is the number of significant digits which the target machine
    will make available in a variable of type "Real" (i.e., a "single-
    precision" real number.  *)

    srealprec: prec_type;


(*  ARR LEN SIZE and STR LEN SIZE are the number of bits required by the
    length words for a flexible array and a flexible string, respectively,
    in a dynamically allocated variable.  *)

    arr_len_size: integer;
    str_len_size: integer;


(*  SET LWB LIMIT and SET UPB LIMIT are the smallest and largest possible
    set elements.  SET SIZE LIMIT is the largest allowable base size for a
    set type (the base size of a set type is computed by AllocType).  *)

    set_lwb_limit: integer;
    set_upb_limit: integer;
    set_size_limit: integer;


(*  Extra elements may be allocated at the beginning or end of a set, if
    this will improve the efficiency of set operations.  When allocating
    a set, its first element will be rounded down to a multiple of
    SET LQUANTUM, or to SET LBASE if that is smaller.  The last element
    will be rounded up to one less than a multiple of SET UQUANTUM, or to
    SET UBASE if that is larger.  *)

    set_lquantum: integer;
    set_uquantum: integer;
    set_lbase: integer;
    set_ubase: integer;


(*  BYTE SIZE is the number of bits in the smallest addressable unit on the
    target machine.  *)

    byte_size: integer;


(*  INT PREC LIMIT is the maximum number of bits which can be used in an
    integer computation.  (Generally, this will be the same as the word
    size.  It needn't be larger than the number of bits required to
    represent a MachineWord variable.  *)

    int_prec_limit: bit_range;


(*  QBL ALLOWED is a flag which indicates whether quick blocks may be
    constructed for this target machine.  *)

    qbl_allowed: boolean;
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

type
    save_pointer = ^ save_record;
    save_record = array [1..*] of machine_word;

public procedure hp_put ( start_addr, end_addr: integer; var block_ptr: save_pointer )
  options special(coercions);

var i: integer;
    d: save_pointer;

begin
  new (block_ptr, end_addr - start_addr);
  d := ptr (start_addr);
  for i := 1 to end_addr - start_addr do
    block_ptr^ [i] := d^ [i];
end;


public procedure hp_get ( start_addr, end_addr: integer; block_ptr: save_pointer )
  options special(coercions);

var i: integer;
    d: save_pointer;

begin
  d := ptr (start_addr);
  for i := 1 to end_addr - start_addr do
    d^ [i] := block_ptr^ [i];
end;
$PAGE miscellaneous

(*  These variables are only used in PASIFU.  They are included here only so
    that they can be preserved between passes.  *)

$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasifu.typ

public var
  ch_index, ch_last: if_file_index;
  ch_counter: char;

(*  These variables are associated with the command file processor.  *)

$INCLUDE pascfm.typ

public var
    cmd_name_stack: cmd_stack;
    cmd_list: pending_command;
    last_cmd: pending_command;
    auto_startup: boolean; (* set by NEWPAS *)

(*  These variables keep the addresses on the heap of the saved type and storage
    allocation tables.  *)

public var
    tal_tables: save_pointer;
    alc_tables: save_pointer;
$PAGE dat_save, dat_get
(* DAT SAVE and DAT GET are used to save the global variables between passes.  *)

var end_data: boolean; (* marks the end of data *)

const
  heap_version = 10;

external function hpwrite ( file_name; save_pointer ): boolean;
external function hpread ( file_name; boolean ): save_pointer;


public procedure dat_save ( fname: file_name )
  options special(coercions);
  var d: save_pointer;
begin
  close; (* Don't go heap-saving file blocks. *)
  save_version := heap_version;
  hp_put (ord (address (start_data)), ord (address (end_data)), d);
  if not hpwrite (fname, d) then begin
    rewrite (tty);
    writeln (tty, '?Unable to save heap to ', fname);
    stop;
  end;
end (* dat_save *);


public function dat_get ( fname: file_name; delete: boolean ): boolean
  options special(coercions);
  var d: save_pointer;
begin
  close;
  d := hpread (fname, delete);
  if d = nil then begin
    dat_get := false;
    return;
  end;
  hp_get (ord (address (start_data)), ord (address (end_data)), d);
  dispose (d);
  dat_get := (save_version = version);
  if not dat_get then begin
    rewrite (tty);
    writeln (tty, '?Obsolete environment loaded from ', fname);
  end;
end (* dat_get *).
  7@t