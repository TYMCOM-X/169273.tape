(*   +--------------------------------------------------------------+
     |                                                              |
     |                        DEBUG.TYP                             |
     |                                                              |
     +--------------------------------------------------------------+
  
     Debugger type definitions.                                       *)

(**********************************************************************
 *                                                                    *
 *     The following is actually PASCAL.INC - the Debugger            *
 *     temporarily has its own copy until the compiler is             *
 *     converted to be compiled by itself.                            *
 *                                                                    *
 **********************************************************************)

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
      names_opt: boolean; (* print file name at start of compilation *)
      overlay_opt: boolean;                 (* code will be overlaid *)
      quick_opt: option_mode;               (* use checkout code generator *)
      source_opt: option_mode;                  (* if listing desired *)
      standard_opt: boolean;               (* enforce standard Pascal *)
      statistics_opt: boolean;             (* if stats desired after compilation *)
      terse_opt: boolean;                  (* if short error messages *)
      ki_code_opt: boolean;	  (* ki-code generation *)
      page_length: 0..255;        (* zero => pagination *)
      page_width: 0..255;         (* columns per listing line *)
      alloc_mode: 0..99;        (* dynamic allocation strategy code *)
      switches: switch_ptr;       (* from enable/disable *)
      dump_switches: switch_ptr;  (* from dump ( ) *)
      storage: 0..777777b         (* stack size *)
    end;

  switch_string = string[32];
  switch_node = 
    packed record
      next_switch: switch_ptr;    (* singly linked *)
      enabled: boolean;
      case boolean of
        true: (name: switch_string);
        false: (name_len: -1..upperbound(switch_string);
                name_str: packed array[1..upperbound(switch_string)] of char)
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
  int_type = machine_word;
  real_type = minimum(real)..maximum(real) prec maximum (prec_type);
  pos_int = 0 .. maximum (int_type);
  parm_string = string[32];             (* large enough for most things *)
  parm_number = 0..262144;


(* Global data *)

external var
  cur_source: source_id;                (* current file/page/line *)

  max_severity: severity_level;         (* highest severity error detected *)
  err_count: int_type;                   (* number of errors in compilation *)
  warnings: int_type; (* number of warnings in compilation *)
  finish: boolean;                      (* go on and generate code? *)
  quick: boolean;                       (* use quick code generator? *)

  default_options: command_options;     (* default global options *)

  prog_options: command_options; (* global options--initialized to default_options,
                                    augmented with program/module statement options *)
  all_opts: set_of_options;             (* all options specified on any block *)

  opts_listing: boolean; (* true if any listing options specified *)
  
(********* end of copy of PASCAL.INC *********************************)
(* $INCLUDE pascal.inc  (* and therefore pasadr.typ *) *)
$INCLUDE debrel.typ
$INCLUDE debst.typ
$PAGE miscellaneous definitions
const
  max_cmd_line_length := 150;
  char_size := 7;				(* character size in bits *)
  single_real_prec := 7;  (* number of decimal digits of precision accommodated
			     by a single precision real *)
  block_size := 128;	(* number of words in a disk block *)
  
type
  disk_block = array [1..block_size] of int_type; 
						(* for reading .deb files *)
  lexic_lvl_range = 0..11;	(* range of static nesting levels the Debugger is
				   prepared to deal with *)
  display_lvl_range = 0..maximum (lexic_lvl_range) + 1;
						(* range for display of scope levels - one greater than
						   range of lexical levels to allow inclusion
						   of a main or module level *)
  cmd_line_index = 1..max_cmd_line_length + 2;	(* DEB$LEX can temporarily have an index
						   2 greater than string length *)
  cmd_str = string[max_cmd_line_length];
  
  id_string = string[40];			(* identifiers entered are arbitrarily 
						   limited to this size *)
  stack_level = 0..177777b;			(* stack frame numbers *)
  ovl_mod_no = 0..377777b;			(* overlay module number *)
  int_type_ptr = ^int_type;	(* for coercing addresses to be pointers to
				   machine words *)
  
  (* templates used in set and string assignments, displays, and comparisons *)
  
  string_overlay_ptr = ^packed array [1..maximum (char_range)] of char;
  set_overlay_ptr =    ^packed array [0..max_set_length] of boolean;
$PAGE status_code declaration
(* Error codes used throughout the debugger. *)

type
  status_code = (success, execute,

                 cmd_too_long, no_str_delim, not_terminated,
                 not_assignable, ill_command, lineno_missing,
                 illegal_brkpt_num, no_such_brkpt, too_many_brkpts, display_keyword_expected,
                 then_expected, end_expected, stmt_expected, rparen_expected,
                 int_cons_expected, not_addressible, illegal_cmd,
                 exec_must, scope_ref_expected,
                 noscope, notdebug, notdefined, notpascal, badnest, wronginvocation,
                 ill_frame_no, mod_not_found, mod_not_debug, file_not_found,
                 no_instead, page_not_found, dtime_mismatch,
                 format_code_expected, not_record, too_many_withs,
                 ill_sym_type, routine_inactive, undef_external, 
                 ident_expected, ill_set_elem_type, illegal_range,
                 set_too_big, rbrack_expected, cons_ref_expected,
                 type_incompatability, id_undefined, func_inactive,
                 below_range, above_range, field_id_expected, no_such_field,
                 not_file_or_ptr, nil_pointer, ptr_uninitialized,
                 not_array_string, not_string, ill_idx_type,
                 too_many_subscripts, illegal_value, undiscriminated_union,
                 oct_hex_invalid, cannot_print, decimal_invalid,
		 relop_invalid, not_boolean, no_file_block, target_unknown,
		 bad_length_word, cur_mod_not_debug, bad_iostatus,
  
                 bad_reason);
  
const
  severe: set of status_code := [cmd_too_long..bad_reason];
$PAGE token declarations
type
  token_kind = (lparent, rparent, asterisk, plus, 
                comma, minus, period, slash, colon, semicolon, 
                lbrack, rbrack, arrow, atsign, otherssy, elipsis, becomes,
                eqop, neop, leop, ltop, geop, gtop, inop,	(* relops - maintain this ordering! *)
                andop, beginsy, elsesy, endsy, ifsy, notsy, orop, thensy,
                ident, stringconst, intconst, realconst, 
                eofsy);

  relops = eqop..inop;

  token_set = set of token_kind;

  token_record = packed record
    tkind: token_kind;				(* kind of token *)
    id_text: id_string;				(* if identifier then text *)
    octal_value: int_type;			(* if intconst then value when interpreted
						   as octal, 0 if '8' or '9' scanned *)
    cons_node: value_node			(* value if constant *)
  end;
  
  lex_scan_rec_type = record
    cursor:		  cmd_line_index;  (* position of next available char in cmd_line *)
    cmd_line:		  cmd_str;	(* current command line *)
    cursor_at_next_token: 0..max_cmd_line_length; (* for determining if we already have
						     the next token (as result of earlier
						     lookahead) *)
    cursor_after_token:   cmd_line_index;
    next_token:		  token_record  (* to store scanned token to avoid repetitive
					   scanning caused by lookahead *)
  end;
$PAGE descriptor declaration
(* Descriptor records are a means of uniformly describing data objects -
   whether they are program variables, program constants, or typed in constants.  *)

type
  addr_record = packed record
    wordoffset: unit_range;			(* address of an object *)
    is_packed: boolean;				(* TRUE => object packed *)
    bitoffset: bit_offset			(* if IS_PACKED is true, bit offset within word *)
  end;

  desc_kinds = (scalar_dt, int_dt, char_dt, real_dt, set_dt, pointer_dt,
		array_dt, file_dt, string_dt, record_dt, subr_dt, substr_dt,
		slice_dt, unknown_dt);

  descriptor = packed record
    assignable: boolean;			(* object may be assigned a new value *)
    user_const: boolean;			(* true => constant keyed in by user *)
    type_ptr: inttyp;				(* symbol table offset of type node; occasionally
						   we need pointer rather than actual node *)
    octal_val: int_type;			(* if integer typed at tty, then value when inter-
						   preted as octal; 0 if '8' or '9' was typed *)
    addr: addr_record;				(* address of object *)
    dtype: type_node;				(* type node for object *)
    value: value_node;				(* will contain object if user constant or
						   expression *)
    case dkind: desc_kinds of

      set_dt:
        (  set_min: bit_range;			(* ord of minimim(set element type), ord of least
						   element for const set *)
           set_max: bit_range);			(* ord of maximum(set element type), ord of largest
						   element for const set *)
      substr_dt:				(* addr is full string address *)
        (  substr_start: char_range;
           substr_len: char_range);

      slice_dt,
      array_dt:
        (  lower_bnd: int_type;			(* lower bound of array ( not available from
						   index type node if flexible or generic) *)
           upper_bnd: int_type)			(* upper bound of array or upper element
						   of slice range *)
  end;
$PAGE with_table declarations
(* A with_table record within the Debugger's static area is used to keep track of WITHs
   currently in effect. *)

const
  max_with := 11;

type
  with_level = 1..max_with;

  with_tab = packed record
    active_withs: 0..max_with;
    withs: array [ with_level ] of
	     packed record
	       rec_inttyp:  inttyp;
	       rec_base:    addr_record;
	       first_field: intsym
	     end
  end;
$PAGE source_id declarations
(* Source program references (as used in the BREAKPOINT or KIND commands)
   are translated into source_id records.  *)

type
  ext_file_range = -1..max_file_no;	(* extend pascal.inc's file_range to include -1 *)
  ext_page_range = -1..max_page_no; (* extend pascal.inc's page_range to include -1 *)

  mod_string = string[6];
  file_string = string[6];
  page_string = id_string;

  source_id_record = record
    module_name: mod_string;			(* module name or null string *)
    file_name: file_string;			(* file name or null string *)
    file_num: ext_file_range;			(* file number or -1 *)
    page_name: page_string;			(* page name or null string *)
    page_num: ext_page_range;			(* page number or -1  *)
    line_num: line_range			(* line number or 0 *)
  end;

  (* Augmented source id records are used to insure that overlaid
     programs can use source references to refer to the default
     module.  The module name is not available, but the program
     block is.  *)

  augmntd_source_id = record
    program_block: ^prog_block;			(* if not nil, then this is used to find
						   the file block *)
    source_id: source_id_record			(* module name field ignored if PROGRAM_BLOCK
						   field is not NIL *)
  end;
$PAGE scope_id_record declarations
(* Scope references are translated into scope_id_records. *)

type
  scope_id_record = record
    name_count: lexic_lvl_range;		(* number of identifiers in <scope ref> *)
    names:  array [lexic_lvl_range] of id_string; (* text of id's *)
    delim: char;				(* '@' if first id was module, else ':' *)
    stack_frm_no: stack_level			(* instantiation number for recursive routines *)
  end;
$PAGE breakpoint table declarations
const
  nil_ovl_modno := 200000b;			(* indicates overlay module number in brkpt
						   table is unknown *)
  
type
  brkpt_range = 0..9;			(* lowest to highest legal breakpoint numbers *)

  brkpt_entry = packed record			(* breakpoint table entry *)
    in_core: boolean;				(* true if always resident or overlay module
						   which is in core *)
    mod_no: ovl_mod_no;				(* overlay module number, 377777b if unknown
						   or not an overlay *)
    stmt_blk_addr: stmt_block_ptr			(* Nil if this table entry is available *)
  end;
$PAGE stack frame declaration
type
  stack_frame = packed record
    rtn_flags: half_word;
    rtn_addr: half_word;				(* proc return address *)
    caller_basis: ^stack_frame;			(* ptr to callers stackframe *)
    parent_basis: ^stack_frame;			(* ptr to parents stackframe *)
    stack_size: half_word;			(* negative count of words remaining *)
    caller_top: ^stack_frame;			(* 17b(RH) on entry to proc *)
    link_flags: half_word;
    link_addr: ^procblklink			(* address of word following jsp 1,pntry.,
						   contains ptr to proc block *)
  end;
$PAGE display declarations
type

  (* A display record is used to describe each level of the currently
     open scope.  Normally each display record corresponds to one
     (in-scope) stack frame.  However if the procedures are within
     a module then a display record is also created which corresponds
     to the outermost level of the module (which does not have a
     corresponding stack frame).  *)

  display = packed record
    prog_blk: ^prog_block;			(* prog block for module, NIL if not debug *)
    blk_node: intblk;				(* symbol table block node for proc,
						   NIL if not DEBUG *)
    stackbase: ^stack_frame;			(* corresponding stack frame or NIL if display
						   record corresponding to a module *)
    staticbase: half_word			(* base of static storage, 0 if unknown *)
  end;

  (* A scope_type record is used to describe the currently open scope. *)

  scope_type = record
    display_levels: display_lvl_range;		(* number of entries in the following array
						   which are currently meaningful, i.e.,
						   static nesting level of current scope *)
    displays: array [1..maximum (display_lvl_range)] of display
						(* each entry describes one level of scope *)
  end;
$PAGE link symbol table
const
   modtype = 0;					(* link sym tab entry type *)

type
   linkentry = packed record
     case link_symtab_type: 0..17b of
       0:      (modname:   r50word;
		stsize:    half_word;   (* neg size of local st for this mod *)
		firstword: ^module_record);
       1..17b: (symname:   r50word;
		filler:    half_word;
		symaddr:   half_word)
   end;
$PAGE db_static_record declaration
(* A record of type DB_STATIC_RECORD contains static variables needed
   by DEB*)

type
  db_static_record = packed record
    cur_stmt_in: boolean;			(* true => overlay module containing current
						   stmt is currently in-core *)
    cur_ovl_mod: ovl_mod_no;			(* number of overlay module containing current
						   current stmt, NIL_OVL_MODNO if unknown *)
    cur_stmt_addr: unit_range;			(* any addr in the overlay module containing the
						   current stmt; used in tracking whether the
						   module is in-core *)
    def_mod_in: boolean;			(* true => overlay module containing the
						   current scope is in core *)
    def_ovl_mod: ovl_mod_no;			(* overlay module number of module containing
						   the current scope *)
    def_mod_addr: unit_range;			(* any address in the overlay module containing
						   the current scope *)
    with_table: with_tab;			(* table of WITHs currently in effect *)
    prev_display_levels: display_lvl_range;	(* The number of display levels and the topmost *)
    dummy2: 0..37777777777b;
    prev_top_display: display;			(* display are preserved across entries to the
						   debugger to permit checking if WITHs should
						   be preserved. *)
    brk_strings: array [brkpt_range] of cmd_str;  (* breakpoint strings table *)
    debugger_active: boolean			(* set true after debugger entered, used *)
						(* to detect recursive calls *)
    ;dummy3: 0..377777777777b
  end;
$PAGE rt_static_record declaration
(* A record of type RT_STATIC_RECORD contains static variables needed
   by the runtime ( some are also accessed by other debugger routines). *)

type
  reason_code = ( (* describes reason debug called *)
    badreason, (* Debugger called w/o run-time mediating - a bug,
		  or user has called it at wrong time via DDT *)
    init,  (* Call at beginning of execution *)
    trap,  (* Debugger entered (via DDT) after trap, e.g., ill
	      mem ref. Continuation of execution impossible. *)
    rterr, (* Debugger entered (via DDT) after error detected
	      and diagnosed by run-time. Continuing execution
	      afterwards impossible. *)
    step,  (* Step count set by debugger on last return to
	      program exhausted. *)
    brkpt, (* Debugger breakpoint has been encountered *)
    intrp); (* Debugger entered (via DDT) after break (^C) hit.
	       Execution may be continued afterwards.*)


  rt_static_record = record
    cstmt: stmt_block_ptr;			(* Pointer to last statement executed.  On break-
						   point and step entries, is current statement; 
						   0 if execution has not yet begun *)
    in_debugger: boolean;			(* set true while debugger running *)
    reason: reason_code;			(* reason debugger was entered *)
    step_count: int_type;				(* STEP and SSTEP cmds stmt count *)
    proc_skip: boolean;				(* true => skip procs when stepping *)
    brk_skip: int_type;				(* breakpoint proceed count *)
    brkpt_num: brkpt_range;			(* when brkpt occurs this is set to brkpt number *)
    max_active_brkpt: -1..maximum (brkpt_range);  (* number of largest active breakpoint;
						     -1 if none set *)
    brk_table:
      array [brkpt_range] of brkpt_entry;	(* breakpoint table *)
    regsave: array [1..16b] of int_type;		(* reg save area when debugger entered *)
    basis_rec: packed record
      main_flag: half_word;			
      basis: ^stack_frame			(* ptr to current stack frame *)
    end
  end;
 * \Ž