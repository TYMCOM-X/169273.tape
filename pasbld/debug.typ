(*   +--------------------------------------------------------------+
     |                                                              |
     |                        DEBUG.TYP                             |
     |                                                              |
     +--------------------------------------------------------------+
  
     Debugger type definitions.                                       *)

$INCLUDE pascal.inc  (* and therefore pasadr.typ *)
$INCLUDE debrel.typ
$INCLUDE debst.typ
$PAGE miscellaneous definitions
const
  max_cmd_line_length := 150;
  block_size := 128;	(* number of words in a disk block *)
$IF P10
  single_real_prec := 7;  (* number of decimal digits of precision accommodated
			     by a single precision real *)
  max_octal_width := 12;	(* chars reqd to represent word in octal *)
  max_hex_width := 9;		(* chars reqd to represent word in hex *)
  bits_per_addressible_unit := 36;
  str_lword_size := 36;
  char_size := 7;
  max_atomic_data_width := 36;
$ENDIF
$IF VAX
  single_read_prec := 6;
  max_octal_width := 11;
  max_hex_width := 8;
  bits_per_addressible_unit := 8;
  str_lword_size := 16;
  char_size := 8;
  max_atomic_data_width := 32;
$ENDIF
  
type
  elem_sizes = 0..max_atomic_data_width;
  disk_block = array [1..block_size] of machine_word; 
						(* for reading .deb files *)
  lexic_lvl_range = 0..11;	(* range of static nesting levels the Debugger is
				   prepared to deal with *)
  display_lvl_range = 0..maximum (lexic_lvl_range) + 1;
						(* range for display of scope levels - one greater than
						   range of lexical levels to allow inclusion
						   of a main or module level *)
  cursor_range = 1..max_cmd_line_length + 2;	(* LEX$SCAN can temporarily have a cursor
						   2 greater than string length *)
  cmd_str = string[max_cmd_line_length];
  
  id_string = string[40];			(* identifiers entered are arbitrarily 
						   limited to this size *)
  stack_level = 0..177777b;			(* stack frame numbers *)
  ovl_mod_no = 0..377777b;			(* overlay module number *)
  int_type_ptr = ^int_type;	(* for coercing addresses to be pointers to
				   machine words *)
  radix_type = (octal_radix, decimal_radix, hex_radix);
						(* supported radices *)

const
$IF P10 address_radix := octal_radix;
$IF VAX address_radix := hex_radix;
$PAGE status_code declaration
(* Error codes used throughout the debugger. *)

type
  status_code = (success, execute,

                 cmd_too_long, no_str_delim, bad_radix, bad_digits, too_many_bits,
		 not_terminated, not_assignable, ill_command, lineno_missing,
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
                 below_range, above_range, bad_real, field_id_expected, no_such_field,
                 not_file_or_ptr, nil_pointer, ptr_uninitialized,
                 not_array_string, not_string, ill_idx_type,
                 too_many_subscripts, illegal_value, undiscriminated_union,
                 oct_hex_invalid, cannot_print, addr_radix_expected,
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
                ident, stringconst, intconst, realconst, eofsy);

  relops = eqop..inop;
  token_set = set of token_kind; (* for follow-sets in recursive descent parsing *)

  token_record = record
    tkind: token_kind;				(* kind of token described by this record *)
    id_text: id_string;				(* if identifier then text *)
    address_value: machine_word;			(* if intconst then value when interpreted
							   in machine's address radix.  Note -
							   if user explicitly specified radix, value
							   is same as cons_node.scalar_val  *)
    (* If an integer constant doesn't have an explicit radix specified,
       it is evaluated in decimal (cons_node) and in the machine's address
       radix, and the corresponding statuses merely stored.  When it later
       is known which version is desired the appropriate status must be
       invoked. *)
    status_in_given_radix,
    status_in_addr_radix: status_code;
    cons_node: value_node			(* value if constant *)
  end;
  
  (* There will only be one of these records, local to DEBUG$, constituting
     "static" storage (for duration of an entry into Debugger) to preserve
     the current state of the lexical scanning. *)

  lex_scan_rec_type = record
    cursor:		  cursor_range;  (* position of next available char in cmd_line *)
    cmd_line:		  cmd_str;	(* current command line *)
    cursor_at_next_token: 0..max_cmd_line_length;
						(* for determining if we already have the
						   next token (as result of earlier lookahead) *)
    cursor_after_token:   cursor_range;
    tkind:		  token_kind; (* kind of last token scanned *)
    next_token:		  token_record  (* to store more info about a scanned token  *)
  end;
$PAGE descriptor declaration
(* Descriptor records are a means of uniformly describing data objects -
   whether they are program variables, program constants, or typed in constants.  *)

type
  addr_record = packed record
    wordoffset: unit_range;			(* address of an object *)
    is_packed: boolean;				(* TRUE => object packed *)
    bitoffset: 0..bits_per_addressible_unit-1		(* if IS_PACKED is true, bit offset within word *)
  end;

  desc_kinds = (scalar_dt, int_dt, char_dt, real_dt, set_dt, pointer_dt,
		array_dt, file_dt, string_dt, record_dt, subr_dt, substr_dt,
		slice_dt, unknown_dt);

  descriptor = packed record
    assignable: boolean;			(* object may be assigned a new value *)
    user_const: boolean;			(* true => constant keyed in by user *)
    type_ptr: inttyp;				(* symbol table offset of type node; occasionally
						   we need pointer rather than actual node *)
    addr: addr_record;				(* address of object *)
    dtype: type_node;				(* type node for object *)
    value: value_node;				(* will contain object if user constant or
						   expression *)
    case dkind: desc_kinds of

      int_dt:
        (  address_value: machine_word;
	   status_in_given_radix,
	   status_in_addr_radix: status_code);

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
    parent_basis: ^stack_frame;			(* ptr to parents stackframe *)
    caller_basis: ^stack_frame;			(* ptr to callers stackframe *)
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
   by DEBUG$. *)

type
  db_static_record = packed record
    cur_stmt_in: boolean;			(* true => overlay module containing current
						   stmt is currently in-core *)
    cur_ovl_mod: ovl_mod_no;			(* number of overlay module containing current
						   current stmt, NIL_OVL_MODNO if unknown *)
    cur_stmt_addr: int_type_ptr;		(* any addr in the overlay module containing the
						   current stmt; used in tracking whether the
						   module is in-core *)
    def_mod_in: boolean;			(* true => overlay module containing the
						   current scope is in core *)
    def_ovl_mod: ovl_mod_no;			(* overlay module number of module containing
						   the current scope *)
    def_mod_addr: int_type_ptr;			(* any address in the overlay module containing
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
    regsave: array [1..16b] of machine_word;		(* reg save area when debugger entered *)
    basis_rec: packed record
      undefined: half_word;			
      basis: ^stack_frame			(* ptr to current stack frame *)
    end
  end;
