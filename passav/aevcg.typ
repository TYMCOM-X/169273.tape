(* This defines the types used by the Eclipse code generator.  It defines all
   formats specific to the machine, the linker, etc. *)

type		(* ranges of basic operand types *)
  byte = -#H80..#H7F;	(* 8 bits *)
  word = -#H8000..#H7FFF;	(* 16 bits *)
  longword = -#H80000000..#H7FFFFFFF; (* 32 bits *)
  uns_byte = 0..#Hff;
  uns_word = 0..#Hffff;
  uns_longword = 0..#Hffffffff;

type
  registers = 0..4;
  set_of_registers = set of registers;
  reg_set_vector = array [0..*] of set_of_registers;
  reg_selector = 0..5;		(* registers + special values, see consts below *)
  opc_range = int_type;                  (* binary range of opcode values *)
  code_address = uns_word;	(* Eclipse address range *)
  virtual_address = uns_longword;
  bit_offset = 0..15;	(* bit offsets within unit *)
  elem_sizes = 0..16;	(* bit width of atomic data *)

const
  str_lw_width = 16;			(* string length word length *)
  r3 = 3;		(* general registers *)
  r2 = 2;
  r1 = 1;
  r0 = 0;
  fp3 = 3;		(* floating point registers *)
  fp2 = 2;
  fp1 = 1;
  fp0 = 0;
  fp = r3;				(* frame pointer *)
  ix = r2;	(* available index register *)
  noreg (* : reg_selector *) := 4;		(* no register assigned, no indexing, etc. *)
  anyreg (* : reg_selector *) := 5;		(* invites any register assignment *)

  publics_length := 10;				(* significant chars in public symbols *)
  bits_per_unit := 16;
  bits_per_word = 16;
  bits_per_byte = 8;
  bytes_per_unit = 2;
  bits_per_address = 16;
  bits_per_virtual_address = 32;
  bits_per_reg = 16;
  bits_per_integer = 16;
  bits_per_float = 32;
  bits_per_double = 64;
  char_size = 8;
  max_str_length = 177777b;			(* maximum string length *)
  int_nil: code_address := #Hffff;	(* nil defined with base type integer *)
  int_nilf: code_address := #Hffff;	(* ord ( nilf ) for target machine *)
  min_literal = 1;			(* smallest literal mode value *)
  max_literal = 4;			(* largest literal mode value *)
  max_disp_size = 15;			(* size of largest disp mode offset in bits *)

type
  code = ^ code_record;			(* pointers to types declared below *)
  def = ^ definition;
$PAGE runtime symbols
  (* INCOMPLETE *)

  (* The following defines the set of runtime symbols (runtime subroutines,
     constants) which may be referenced by the code generator.  The rel file
     generator maps these to actual names when building the rel file.  Note:
     (1) double precision routines *must* follow the corresponding single
     precision routines, and (2) extra symbols are defined to allow for
     expansion *)

  rt_symbol =
      (	rt_first,
 	rt_main_frame,			(* for addr of main's stack frame *)
	rt_uw_rtn,			(**** stack unwind to specified frame ****)
	rt_uw_prg,			(**** stack unwind to main program ****)
	rt_exp_ii,	(* exponentiation *)
	rt_exp_ri,
	rt_exp_di,
	rt_exp_rr,
	rt_exp_dd,
	rt_r_rnd2,	rt_d_rnd2,
	rt_r_sqrt,	rt_d_sqrt,	(* math functions *)
	rt_r_ln,	rt_d_ln,
	rt_r_log,	rt_d_log,
	rt_r_exp,	rt_d_exp,
	rt_r_sin,	rt_d_sin,
	rt_r_asin,	rt_d_asin,
	rt_r_sinh,	rt_d_sinh,
	rt_r_cos,	rt_d_cos,
	rt_r_acos,	rt_d_acos,
	rt_r_cosh,	rt_d_cosh,
	rt_r_tan,	rt_d_tan,
	rt_r_tanh,	rt_d_tanh,
	rt_r_ctan,	rt_d_ctan,
	rt_r_atan,	rt_d_atan,
	rt_r_atn2,	rt_d_atn2,
	rt_rand_set,		(* one and zero operand forms *)
	rt_rand,
	rt_new,	(* allocation routines *)
	rt_dispose,
	rt_extent,
	rt_sub_chk,		(* error check reporting *)
	rt_str_chk,
	rt_ovl_chk,
	rt_val_chk,
	rt_ptr_chk,
	rt_fil_chk,
	rt_fld_chk,
	rt_cmp_chk,
	rt_ass_chk,
	rt_case_chk,

	(* Only required search and verify runtime routines are for
	   upper/no casing for strings on
	   l and o format sets.	*)

	rt_sr_fl,
	rt_sru_fl,
	rt_sr_fo,
	rt_sru_fo,
	rt_vf_fl,
	rt_vfu_fl,
	rt_vf_fo,
	rt_vfu_fo,

	rt_genset,
	rt_stmt, (* for debugger *)
	rt_open,	rt_rewrite,	rt_reset, 	(****  input / output  ****)
	rt_open_typed,	rt_open_binary,
	rt_get,
	rt_get_char,
	rt_put,
	rt_put_char,
	rt_read_binary, rt_write_binary,
	rt_read_image,	rt_write_image,
	rt_close,	rt_scratch,	rt_close_all,
	rt_file_size,
	rt_break,
	rt_empty_text,
	rt_page,
	rt_clear,
	rt_iostatus,
	rt_iostat_last,
	rt_extstatus,
	rt_read,
	rt_write,
	rt_getstring,
	rt_put_fixed_string,
	rt_put_varying_string,
	rt_put_last_string,
	rt_readln,
	rt_writeln,
	rt_seek,
	rt_stop,
        rt_upper_table,			(* uppercase conversion table xfer vector *)
	rt_lower_table,			(* lowercase conversion table xfer vector *)
	rt_eof_eoln,
	rt_date,
	rt_time,
	rt_fname,
	rt_runtime,			(* CPU time in millisecs *)
	rt_last_used,                   (**** above is last actual symbol ***)

	rt2, rt3, rt4, rt5, rt6, rt7, rt8, rt9  ); (* spares *)
$PAGE definition and reference records
  (* Definition records define internal symbols, generated during code generation.
     They are used to label and resolve subroutine entry points, nonlocal labels,
     references to tables in the code, constant references, etc.  Identifying
     numbers are assigned depending on the type.  For symbols and subroutine defs,
     the id_number of their corresponding symbols is used.  In this way, the
     number is determined before forward references are made.  Numbers for local
     and label defs are recycled at the end of each block.  Numbers for other
     types are assigned as needed. *)

  def_types =
     (  sym_def,			(* user declared symbol *)
	constant_def,                   (* loc in constant area *)
	subr_def,                       (* block id *)
	deb_sym_def,			(* debug symbol table indices from trace blocks *)
	temp_size_def,			(* total stack frame size *)
	code_def,			(* control block in code, e.g. for debugger *)
	local_def,			(* definition local to code area of block *)
        label_def,			(* label node reference local to a block *)
	extern_def, (* external symbol reference *)
	offset_def	);		(* denotes offset relative to one of the above *)

  (* "references" are only used for unresolved forward references
     across procedure bodies (non-local goto's, constant refs, etc. *)

  ref = ^ reference;

  reference = packed record
    next: ref;	(* to same definition *)
    addr: code_address;	(* offset within code psect *)
    ref_size: elem_sizes;	(* size in bits of field to be resolved *)
    displacement: boolean;  (* true if displacement is for a displacement code
				record, not an instruction operand. *)
    forward_ref: boolean; (* true: disp. is (ref'ed symbol - from_addr)
			     false:         (from_addr - ref'ed symbol)  *)
    from_addr_or_offset: unit_range
  end;

  definition =
      packed record
	next: def;                     (* next definition of same type *)
	defined: boolean; (* set true when the address is set *)
	relocatable: boolean;
	refchain: ref;	(* chain of unresolved references to this def *)
	case deftype: def_types of

	  sym_def..extern_def:
	    (  first_offset: def; (* chain thru def^.next of offset defs for symbol *)
	       case def_types of
		 sym_def..label_def:
		   (  addr: code_address; (* offset within above class, else
					     if TEMP_SIZE_DEF, value of def *)
		      defnumber: id_range  ); (* logical name, see above for numbering scheme *)
		 extern_def:
		   (  ext_name: sym ) );	(* indicates name of external *)

	  offset_def:
	    (  reldef: def;            (* definition to which this is relative *)
	       offset: int_type       ) (* offset from above *)
      end;
$PAGE relocation record
  (* Relocation syllables define how to relocate fields of code records, which
     are defined below. *)

  rel_syllable =
      packed record
	case kind: storage_class of

(* 	  register_sc,                          (* field is register number *)
	  absolute_sc:  ();                     (* no relocation required *)	*)

	  local_sc, parameter_sc, static_sc,    (* user symbols defined in corresponding areas *)
	  external_sc:                          (* external name *)
	    ( relsym: sym );                    (* add value of this symbol to the offset *)

	  runtime_sc:                           (* add in value of runtime symbol to field *)
	    ( relrtsym: rt_symbol );

	  def_sc:				(* reference to internal label *)
	    ( reldef: def )			(* to label defined *)

      end;
$PAGE address descriptor
  addr_ptr = ^addr_desc;
  aev_type = 		(* operand/displacement sizes *)
    (	aev_byte,
	aev_word,
	aev_long,
	aev_quad,
	aev_float,
	aev_double );

  addressing_mode = 
     (	branch_displacement,	(* PC relative branches *)
	other  );	(* everything else *)

  addr_desc =
      packed record
	byte_size: aev_type;	(* operand/displacement size *)
	addr_mode: addressing_mode;
	immediate: boolean;
	indirect: boolean;
        index: registers;
	register: registers;	(* general register usage *)
	offset: code_address;
	reloc: rel_syllable		(* relocation for offset *)
      end;

  (* Following PSECT definitions are preliminary. *)

  (* Three types of PSECTs are of import. *)

  psect_type = (
	direct_static_psect,	(* direct static/public var area *)
	virtual_static_psect,	(* virtual static/public var area *)
	code_psect);	(* code and constants *)

  (* PSECT attributes for the linker to be supplied. *)
$PAGE code record
  (* Code records are an ordered list of code and data words, tagged with relocation
     information.  In effect, they constitute assembly language statements.
     During code generation, three lists are maintained.  The first is the code
     for the current block.  The second is the image of the static area as
     initialized.  The third is the constant area which is pooled for all blocks. *)

  code_types =
	(instruction, bytelen, wordlen, fullword, setword, stringword, quadword,
	 pstringword, defmark, origin, source, comment, deftemp, nullcode,
	 realword, doubleword , indirect_word );

  code_record =
	packed record
	  next: code;                             (* to word with next higher address *)
	  prev: code;				  (* to word with next lower address *)
	  branch_chain: code;	(* for resolution of branch displacements *)
	  case kind: code_types of

	    instruction:
	      ( opcode: opc_range;
		opreg: registers;	(* use implied by opcode *)
		operand: addr_desc);	(* use implied by opcode *)

	    bytelen:
	      (	 byte_value: byte  );

	    wordlen:
	      (  word_value: word  );

	    fullword:
	      (  fwd: longword  );

	    quadword:
	      (  qvalue1: longword;
		 qvalue2: longword  );

	    realword:
	      (  rvalue: real  );
	    
	    doubleword:
	      (  dvalue: real_type  );

	    stringword, pstringword:
	      (  strvalue: packed array[1..*] of char);	(* short allocated *)

	    setword:
	      ( setval: packed array[0..15] of boolean );

	    defmark:                        (* define a local symbol *)
	      ( defname: def ;          (* gives 'name', records references *)
		defspan: unit_range);

	    origin:                         (* sets relocation counter of containing area *)
	      (	psect: psect_type );	(* starts relocation area *)

	    source:                         (* commentary: defines where code for statement starts *)
	      ( stmtid: source_id );

	    comment:	(* arbitrary commentary *)
	      ( ctext: packed array [1..*] of char );	(* short allocated *)

	    indirect_word:	(* For publics with the overlay option *)
	      ( pub_sym : sym )
	end;

  code_list =
      record
	first, last: code		(* list through code^.next of code_records *)
      end;
$PAGE value descriptor
(* VALUE DESCriptors are used to track values across the course of
   code emission. *)

value_ptr = ^ value_desc;

value_desc = packed record

  loc: addr_desc;	(* location and type of value *)

  uses_remaining: usage_range;

  eval_point: code	(* point of evaluation in code stream.
			   NIL if already displaced to temporary of if
			   no registers used. *)
end;
$PAGE register descriptor
  (* Register descriptors define the machine registers.  Each descriptor denotes
     a single register.  Groups of descriptors are formed for multiword values. *)

  reg_status =
      packed record
	associate: registers;	(* non-zero if tagged to another register *)
	contents: val_ptr
      end;

  reg_descriptor = array[registers] of reg_status;

$PAGE string types
  (* String accessing information. *)

  str_translation =
      (	no_trans,			(* no case translation *)
	upper_trans,			(* translate to uppercase *)
	lower_trans  );			(* translate to lowercase *)




(* Data accessing information *)

data_alignment =
     (	unsigned_value,			(* right justified, zero extended *)
	signed_value,				(* right justified, sign extended *)
	right_aligned  );		(* right justified, zero padded (non arithmetic) *)

(* Types required by the string routines. *)

  str_len_context = ( no_length, actual_length, max_length );

  (* String descriptor format is INCOMPLETE. *)

  str_desc = packed record
    base_addr: addr_desc;		(* base address of string. address of bounds word *)
						(* for flex strings, address of length word for *)
						(* non-flex varying strings *)
    base_is_bound: boolean;		(* TRUE if BASE_ADDR is address of flex string *)
						  (* bounds word *)
    len_context: str_len_context;		(* indicates whether LEN_ADDR is invalid or actual *)
						  (* length or maximum length *)
    len_addr: addr_desc;			(* address of length of string, not valid if *)
						(* LEN_CONTEXT = no_length *)
    max_len: char_range;			(* compile time upperbound on maximum *)
						  (* length of string *)
    text_valid: boolean;			(* TRUE if TEXT_ADDR field is valid *)
    text_addr: addr_desc;			(* address of first char of string *)
    type_desc: expr_type_desc;		(* type info about string *)
    trans_code: str_translation		(* indicates any case conversions pending *)
						  (* for the string *)
  end;

$PAGE misc types and constants
  free_procedure = procedure;



(* Pass 4 error levels *)

error_levels = (no_errors, warning, severe_errors, fatals);

(* Useful constants of above types *)

const
  flex_arr_desc_size = 16;
  flex_str_desc_size = 16;

  none: rel_syllable := (absolute_sc);
  register: rel_syllable := (register_sc);
  dot: rel_syllable := (self_rel_sc);
  reg_addr_desc: addr_desc := (aev_word, other, false, false, noreg, noreg, 0, (register_sc));
  null_location: addr_desc := (aev_word, other, false, false, noreg, noreg, 0, (unallocated));
  immediate_reference: addr_desc := (aev_word, other, true, false, noreg, noreg, 0, (absolute_sc));
  absolute_reference: addr_desc := (aev_word, other, false, false, noreg, noreg, 0, (absolute_sc));
  temp_reference: addr_desc := (aev_word, other, false, false, noreg, fp, 0, (temp_sc));
  parm_reference: addr_desc := (aev_word, other, false, false, noreg, fp, 0, (parameter_sc, nil));
  no_preference: addr_desc := (aev_word, other, false, false, noreg, noreg, 0, (unallocated) );
						(* immediate address for fill character ( ' ' )  *)

(* Address descriptors for the more commonly used registers. *)

  r0_addr: addr_desc := (aev_word, other, false, false, noreg, r0, 0, (register_sc));
  r1_addr: addr_desc := (aev_word, other, false, false, noreg, r1, 0, (register_sc));
  r2_addr: addr_desc := (aev_word, other, false, false, noreg, r2, 0, (register_sc));
  r3_addr: addr_desc := (aev_word, other, false, false, noreg, r3, 0, (register_sc));
  fp3_addr: addr_desc := (aev_float, other, false, false, noreg, fp3, 0, (register_sc));
  fp2_addr: addr_desc := (aev_float, other, false, false, noreg, fp2, 0, (register_sc));
  fp1_addr: addr_desc := (aev_float, other, false, false, noreg, fp1, 0, (register_sc));
  fp0_addr: addr_desc := (aev_float, other, false, false, noreg, fp0, 0, (register_sc));
  fp_addr: addr_desc := (aev_word, other, false, false, noreg, fp, 0, (register_sc));
  ix_addr: addr_desc := (aev_word, other, false, false, noreg, ix, 0, (register_sc));
$PAGE assert
  
(* ASSERT is defined here in liew of the new compiler's assertion checking. *)
  
external procedure assert (boolean);
