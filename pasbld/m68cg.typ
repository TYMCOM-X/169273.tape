(*  This file contains definitions of types and constants used by the
    Motorola 68000 code generator.  It defines formats for the machine,
    the linker, etc.  Many of these definitions are discussed in greater
    detail in the document "Pascal Motorola 68000 -- Code Generator
    Design Notes".  *)
$PAGE basic types and constants
type
    byte = -#h80 .. #h7f;			(* 8 bits *)
    word = -#h8000 .. #h7fff;			(* 16 bits *)
    longword = integer;				(* 32 bits *)
    uns_byte = 0 .. #hff;
    uns_word = 0 .. #hffff;
    code_address = longword;

const
    bits_per_byte = 8;
    bits_per_word = 16;
    bits_per_long = 32;
    bits_per_address = 32;
    bits_per_reg = 32;
    bits_per_integer = 32;
    bits_per_float = 32;
    bits_per_double = 64;
    char_size = 8;
    str_lw_width = 16;
    str_upb_width = 16;
    arr_upb_width = 32;
    str_lw_bytes = str_lw_width div bits_per_byte;
    str_upb_bytes = str_upb_width div bits_per_byte;
    arr_upb_bytes = arr_upb_width div bits_per_byte;
    max_str_length = #hffff;

    int_nil = 0;				(* bit pattern for nil *)
    int_nilf = 0;				(* bit pattern for nilf *)

(*  Forward references.  *)

type
    code = ^ code_record;
    def = ^ def_record;
    fixup = ^ fixup_record;
    guard_key = ^ guard_record;			(* guard_record is internal to M68EXP *)
$PAGE registers
type
    registers =
      ( d0, d1, d2, d3, d4, d5, d6, d7,
	a0, a1, a2, a3, a4, a5, a6, a7 );

    data_regs = d0 .. d7;
    addr_regs = a0 .. a7;

    reg_set = set of registers;

const
    bp = a5;					(* static base pointer *)
    fp = a6;					(* frame pointer *)
    sp = a7;					(* stack pointer *)

    data_reg_set: reg_set = [minimum (data_regs) .. maximum (data_regs)];
    addr_reg_set: reg_set = [minimum (addr_regs) .. maximum (addr_regs)];

    reserved_registers: reg_set = [sp, fp, bp];
$PAGE runtime symbols
(*  Every symbol in the Pascal runtime library which may be referred to by
    compiler-generated code has a corresponding element in the type
    RT_SYMBOLS.  The mapping from elements of this type into the actual
    external symbol names must be defined in the code emitter.

    ***  NOTE  ***
    The code generator logic has been simplified with the assumption that
    if there are single and double precision versions of a floating-point
    runtime routine, the symbol for the double precision version will
    immediately follow the symbol for the single precision version in
    this list.  *)

type
    rt_symbol =
      ( rt_entry,
	rt_dyntemp_b,
	rt_dyntemp_w,
	rt_stop,
	rt_ass_chk,
	rt_case_chk,
	rt_val_chk,
	rt_fil_chk,
	rt_ptr_chk,
	rt_sub_chk,
	rt_str_chk,
	rt_cmp_chk,
        rt_open_text,
        rt_reset_text,
        rt_rewrite_text,
	rt_get_text,
	rt_put_text,
	rt_read_text,
	rt_write_text,
	rt_readln,
	rt_writeln,
	rt_page,
	rt_eoln,
	rt_eopage,
	rt_clear,
	rt_break_text,
        rt_empty_text,
        rt_open_typed,
        rt_get_typed,
        rt_put_typed,
	rt_break_typed,
        rt_empty_typed,
        rt_open_binary,
        rt_read_binary,
        rt_write_binary,
	rt_break_binary,
        rt_empty_binary,
	rt_eof,
	rt_cursor,
	rt_close,
	rt_scratch,
	rt_close_all,
        rt_seek,
	rt_filesize,
	rt_filename,
	rt_filestatus,
	rt_genstatus,
	rt_extstatus,
        rt_getstring,
        rt_putstring_varying,
        rt_putstring_fixed,
        rt_putstring_pad,
	rt_mask,
	rt_unmask,
	rt_masked,
	rt_pending,
	rt_mathstatus,
	rt_exiostatus,
	rt_progstatus,
	rt_spclstatus,
	rt_signal,
	rt_resignal,
	rt_set_handler,
	rt_restore_handler,
	rt_uw_prg,
	rt_return,
	rt_imul,
	rt_idiv,
	rt_imod,
	rt_ipwr,
	rt_move_block_bcnt,
	rt_move_block_wcnt,
	rt_time,
	rt_runtime,
	rt_clear_block,
	rt_setmove_same_lwb,
	rt_setmove_diff_lwb,
	rt_set_singleton_zero_lwb,
	rt_set_singleton_nonzero_lwb,
	rt_set_range_zero_lwb,
	rt_set_range_nonzero_lwb,
	rt_set_diff2,
	rt_set_diff3,
	rt_set_inter2,
	rt_set_inter3,
	rt_set_union2,
	rt_set_union3,
	rt_set_inclusion,
	rt_set_equality,
	rt_fadd,	rt_dadd,
	rt_fsub,	rt_dsub,
	rt_fmul,	rt_dmul,
	rt_fdiv,	rt_ddiv,
	rt_fcmp,	rt_dcmp,
	rt_fsqt,	rt_dsqt,
	rt_fsin,	rt_dsin,
	rt_fcos,	rt_dcos,
	rt_ftan,	rt_dtan,
	rt_fctn,	rt_dctn,
	rt_fasin,	rt_dasin,
	rt_facos,	rt_dacos,
	rt_fatan,	rt_datan,
	rt_fatan2,	rt_datan2,
	rt_fln,		rt_dln,
	rt_flog,	rt_dlog,
	rt_fsinh,	rt_dsinh,
	rt_fcosh,	rt_dcosh,
	rt_ftanh,	rt_dtanh,
	rt_fexp,	rt_dexp,
	rt_ftrunc,	rt_dtrunc,
	rt_fround,	rt_dround,
	rt_fround2,	rt_dround2,
	rt_ffloat,	rt_dfloat,
	rt_fpwr,	rt_dpwr,
	rt_fpwri,	rt_dpwri,
	rt_frandom,	rt_drandom,
	rt_franset,	rt_dranset,
	rt_fdouble,
	rt_dsingle,
	rt_new,
	rt_dispose,
        rt_extent,
        rt_date,
	rt_mv_cf,
	rt_mv_cfu,
	rt_mv_cfl,
	rt_mv_cr,
	rt_mv_cru,
	rt_mv_crl,
	rt_mv_ff,
	rt_mv_ffu,
	rt_mv_ffl,
	rt_mv_fr,
	rt_mv_fru,
	rt_mv_frl,
	rt_mv_cfp,
	rt_mv_cfpu,
	rt_mv_cfpl,
	rt_mv_crp,
	rt_mv_crpu,
	rt_mv_crpl,
	rt_mv_ffp,
	rt_mv_ffpu,
	rt_mv_ffpl,
	rt_mv_frp,
	rt_mv_frpu,
	rt_mv_frpl,
	rt_ct_cf,
	rt_ct_cfu,
	rt_ct_cfl,
	rt_ct_ff,
	rt_ct_ffu,
	rt_ct_ffl,
	rt_ct_cfp,
	rt_ct_cfpu,
	rt_ct_cfpl,
	rt_ct_ffp,
	rt_ct_ffpu,
	rt_ct_ffpl,
	rt_cp_cf,
	rt_cp_fc,
	rt_cp_ff,
	rt_ix_cf,
	rt_ix_fc,
	rt_ix_ff,
	rt_sr_co,
	rt_sr_cl,
	rt_sr_cou,
	rt_sr_clu,
	rt_sr_fo,
	rt_sr_fl,
	rt_sr_fou,
	rt_sr_flu,
	rt_vf_co,
	rt_vf_cl,
	rt_vf_cou,
	rt_vf_clu,
	rt_vf_fo,
	rt_vf_fl,
	rt_vf_fou,
	rt_vf_flu );
$PAGE relocatable sections
(*  REL SECTIONS contains an element for each relocatable section in which
    the code generator can generate code.  *)

type
    rel_sections =
      ( static_section,				(* static variables and condition cells *)
	code_section );				(* executable code and constants *)

const
    section_numbers: array [rel_sections] of integer = (2, 10);
$PAGE relocatable values
(*  A relocatable value record specifies a value which can be determined at
    compile time or at link time.  *)

type
    reloc_value = packed record
	offset: integer;			(* offset from base value *)
	case kind: storage_class of		(* kind of relocation *)
	  absolute_sc:
	    ( );				(* no base value *)
	  code_sc:
	    ( relsect: rel_sections );		(* base is section relocation *)
	  external_sc,
	  static_sc,
	  local_sc,
	  parameter_sc:
	    ( relsym: sym );			(* base is symbol address *)
	  def_sc:
	    ( reldef: def );			(* base is definition value *)
	  runtime_sc:
	    ( relrt: rt_symbol );		(* base is runtime symbol value *)
    end;

const
    abs_zero: reloc_value = (0, absolute_sc);
$PAGE symbolic definitions
type
    def_class =
      ( sym_def,				(* user declared symbol *)
	subr_def,				(* block id *)
	temp_size_def,				(* total stack frame size *)
	stackptr_save_def,			(* frame offset where SP may be saved *)
	with_save_def,				(* frame offset where WITHs may be saved *)
	code_def,				(* control block in code, e.g. for debugger *)
	local_def,				(* definition local to code area of block *)
	label_def,				(* label node reference local to a block *)
	const_def,				(* generated constants *)
	hnd_tab_def );				(* handler branch table *)

const
    indexed_def_classes: set of def_class =
      [sym_def, subr_def, temp_size_def, stackptr_save_def, with_save_def, label_def, hnd_tab_def];
    local_def_classes: set of def_class =
      [local_def, label_def, hnd_tab_def];
    comment_def_classes: set of def_class =
      [subr_def, temp_size_def, stackptr_save_def, with_save_def, const_def];

type

    (*  A definition record associates a relocatable value with a definition
	class and an index within that class.  *)

    def_record = packed record
	next: def;				(* next definition in list *)
	kind: def_class;			(* class of this definition *)
	index: id_range;			(* index of this definition in its class *)
	case defined: boolean of		(* value defined yet? *)
	  true:
	    ( defval: reloc_value );		(* value of the definition *)
	  false:
	    ( fixup_list: fixup;		(* locations to fix up when defined *)
	      label_code_rec: code; (* pointer back to defining def_code code
				       record, for "cross-jumping" optimization *)
	      provisional_address: integer); (* for resolving span dependent instructions *)
    end;

    (*  A fixup record specifies a location which must be patched up when a
	symbolic definition record is assigned a value.  *)

    fixup_record = packed record
	next: fixup;				(* next record in list *)
	fixup_long: boolean;			(* true => fixup longword, false => word *)
	fixup_addr: code_address;		(* address in code section of word to be fixed *)
	patch_value: integer;			(* add to definition value *)
    end;
$PAGE addressing modes
(*  The type ADDR MODE represents the possible addressing modes of the
    M68000 architecture.  *)

type
    addr_mode =
      ( areg_mode,				(* An *)
	dreg_mode,				(* Dn *)
	indirect_mode,				(* (An) *)
	predecrement_mode,			(* -(An) *)
	postincrement_mode,			(* (An)+ *)
	displacement_mode,			(* d16(An) *)
	index_w_mode,				(* d8(An,Ri.W) *)
	index_l_mode,				(* d8(An,Ri.L) *)
	abs_w_mode,				(* d16 *)
	abs_l_mode,				(* d32 *)
	pc_displacement_mode,			(* d16(PC) *)
	pc_index_w_mode,			(* d8(PC,Ri.W) *)
	pc_index_l_mode,			(* d8(PC,Ri.L) *)
	immediate_mode,				(* #xxxx *)
	null_mode );				(* non-existent operand *)

    addr_mode_set = set of addr_mode;

const
    all_modes: addr_mode_set =
	[minimum (addr_mode)..maximum (addr_mode)] - [null_mode];
    nonstack_modes: addr_mode_set =
	all_modes - [predecrement_mode, postincrement_mode];

    data_modes: addr_mode_set =
	nonstack_modes - [areg_mode];
    memory_modes: addr_mode_set =
	data_modes - [dreg_mode];
    control_modes: addr_mode_set =
	memory_modes - [immediate_mode];
    alterable_modes: addr_mode_set =
	[areg_mode..abs_l_mode] - [predecrement_mode, postincrement_mode];
    data_alterable_modes: addr_mode_set =
	data_modes * alterable_modes;
    memory_alterable_modes: addr_mode_set =
	memory_modes * alterable_modes;

    register_modes: addr_mode_set =
	[areg_mode, dreg_mode];
    stack_modes: addr_mode_set =
	[predecrement_mode, postincrement_mode];

    areg_relative_modes: addr_mode_set =
	[indirect_mode, displacement_mode, index_w_mode, index_l_mode];
    pc_relative_modes: addr_mode_set =
	[pc_displacement_mode, pc_index_w_mode, pc_index_l_mode];

    reg_use_modes: addr_mode_set =
	register_modes + areg_relative_modes + stack_modes;
    indexed_modes: addr_mode_set =
	[index_w_mode, index_l_mode, pc_index_w_mode, pc_index_l_mode];
    cst_use_modes: addr_mode_set =
	all_modes - register_modes - stack_modes - [indirect_mode];
$PAGE operand sizes
type
    op_sizes =
      ( size_byte, size_word, size_long, size_double, no_size );

    op_size_set = set of op_sizes;

const
    byte_only: op_size_set = [size_byte];
    word_only: op_size_set = [size_word];
    long_only: op_size_set = [size_long];
    double_only : op_size_set = [size_double];
    any_size: op_size_set = [size_byte, size_word, size_long, size_double, no_size];
    reg_sizes: op_size_set = [size_byte, size_word, size_long];
    word_min: op_size_set = [size_word, size_long];
    word_max: op_size_set = [size_byte, size_word];
    at_least : array [op_sizes] of op_size_set =
      ( reg_sizes, word_min, long_only, double_only, [] );
$PAGE operand descriptors
(*  A record of type OP DESC specifies an addressing mode and sufficient
    additional information to determine how some value can be accessed.  *)

type
    op_desc = packed record
	mode: addr_mode;			(* addressing mode *)
	value_size: op_sizes;			(* size *)
	extended_size: op_sizes;		(* maximum size of operand *)
	signed_value: boolean;			(* is ms bit a sign bit? *)
	known_positive: boolean;		(* is value certainly positive? *)
	reg: registers;				(* "n" in An or Dn *)
	index_reg: registers;			(* "i" in Ri.W or Ri.L *)
	cst_part: reloc_value;			(* d8, d16, d32, or xxxx *)
    end;

const
  descriptors: array[addr_mode] of op_desc =
      ( (areg_mode, no_size, size_long, true, false, d0, d0, abs_zero),
	(dreg_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(indirect_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(predecrement_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(postincrement_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(displacement_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(index_w_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(index_l_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(abs_w_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(abs_l_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(pc_displacement_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(pc_index_w_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(pc_index_l_mode, no_size, no_size, true, false, d0, d0, abs_zero),
	(immediate_mode, no_size, size_long, true, false, d0, d0, abs_zero),
	(null_mode, no_size, no_size, true, false, d0, d0, abs_zero));

  def_descriptor: op_desc =
	(pc_displacement_mode, no_size, no_size, true, false, d0, d0, (0, def_sc, nil));
  runtime_descriptor: op_desc =
	(abs_l_mode, no_size, no_size, true, false, d0, d0, (0, runtime_sc, minimum (rt_symbol)));
  pop_word: op_desc =
	(postincrement_mode, size_word, size_word, true, false, sp, d0, abs_zero);
  pop_long: op_desc =
	(postincrement_mode, size_long, size_long, true, false, sp, d0, abs_zero);
  pop_bool : op_desc =
	(postincrement_mode, size_byte, size_byte, true, true, sp, d0, abs_zero);
  stack_top_word: op_desc =
	(indirect_mode, size_word, size_word, true, false, sp, d0, abs_zero);
  stack_top_long: op_desc =
	(indirect_mode, size_long, size_long, true, false, sp, d0, abs_zero);
$PAGE saved operand descriptors
(*  SAVED OP DESC records are stored on the heap when multiple-use operands
    are processed.  Saved operand descriptors simply consist of an operand
    descriptor and a flag indicating whether the saved value must be deref-
    erenced to access the actual saved operand (used when the address of a
    "with" record is fetched and saved in a local temporary).  If the saved
    descriptor is indirect, then the SIZE and SIGNED fields give those fields
    for the actual operand.  *)

type
    saved_op_ptr = ^ saved_op_desc;

    saved_op_desc = record
	op: op_desc;
	case indirect: boolean of
	  false:
	    ( );
	  true:
	    ( size: op_sizes;
	      signed: boolean );
    end;
$PAGE instructions
type

    (*  GENERIC OPCODES represents all the interesting kinds of operations which
	are available on the M68000.  Similar operations with different sizes or
	addressing modes are not distinguished. *)

    generic_opcodes =
      ( add_opc,	and_opc,	asl_opc,	asr_opc,
	bcc_opc,	bchg_opc,	bclr_opc,	bset_opc,
	btst_opc,	clr_opc,	cmp_opc,	dbcc_opc,
	divs_opc,	divu_opc,	eor_opc,	exg_opc,
	ext_opc,	jmp_opc,	jsr_opc,	lea_opc,
	link_opc,	lsl_opc,	lsr_opc,	move_opc,
	movem_opc,	muls_opc,	mulu_opc,	neg_opc,
	nop_opc,	not_opc,	or_opc,		pea_opc,
	rol_opc,	ror_opc,	rts_opc,	scc_opc,
	sub_opc,	swap_opc,	tst_opc,	unlk_opc );

    (*  SPECIFIC OPCODES distinguish similar operations with different
	addressing modes.  *)

    specific_opcodes =
      ( add_dm_opc,	add_im_opc,	add_ma_opc,	add_md_opc,
	add_qm_opc,	and_dm_opc,	and_im_opc,	and_md_opc,
	asl_dd_opc,	asl_m_opc,	asl_qd_opc,	asr_dd_opc,
	asr_m_opc,	asr_qd_opc,	bcc_p_opc,	bchg_dd_opc,
	bchg_dm_opc,	bchg_wd_opc,	bchg_wm_opc,	bclr_dd_opc,
	bclr_dm_opc,	bclr_wd_opc,	bclr_wm_opc,	bset_dd_opc,
	bset_dm_opc,	bset_wd_opc,	bset_wm_opc,	bsr_p_opc,
	btst_dd_opc,	btst_dm_opc,	btst_wd_opc,	btst_wm_opc,
	clr_m_opc,	cmp_im_opc,	cmp_ma_opc,	cmp_md_opc,
	cmp_mm_opc,	dbcc_dw_opc,	divs_md_opc,	divu_md_opc,
	eor_dm_opc,	eor_im_opc,	exg_aa_opc,	exg_da_opc,
	exg_dd_opc,	ext_d_opc,	jmp_m_opc,	jsr_m_opc,
	lea_ma_opc,	link_aw_opc,	lsl_dd_opc,	lsl_m_opc,
	lsl_qd_opc,	lsr_dd_opc,	lsr_m_opc,	lsr_qd_opc,
	move_bd_opc,	move_ma_opc,	move_mm_opc,	movem_mw_opc,
	movem_wm_opc,	muls_md_opc,	mulu_md_opc,	neg_m_opc,
	nop_x_opc,	not_m_opc,	or_dm_opc,	or_im_opc,
	or_md_opc,	pea_m_opc,	rol_dd_opc,	rol_m_opc,
	rol_qd_opc,	ror_dd_opc,	ror_m_opc,	ror_qd_opc,
	rts_x_opc,	scc_m_opc,	sub_dm_opc,	sub_im_opc,
	sub_ma_opc,	sub_md_opc,	sub_qm_opc,	swap_d_opc,
	tst_m_opc,	unlk_a_opc );
$PAGE
    (*  CONDITION CODES represents the possible conditions for a BCC_OP,
	DBCC_OP, or SCC_OP.  *)

    condition_codes =
      ( t_cc,		f_cc,		hi_cc,		ls_cc,
	cc_cc,		cs_cc,		ne_cc,		eq_cc,
	vc_cc,		vs_cc,		pl_cc,		mi_cc,
	ge_cc,		lt_cc,		gt_cc,		le_cc );

    (*  An INSTRUCTION represents one M68000 machine instruction.  *)

    instruction = packed record
	opcode: specific_opcodes;
	ccode: condition_codes;
	label_ref: ^label_reference_record;
	operands: array [1..2] of op_desc;
    end;
$PAGE code list
type
    code_list = packed record
	first: code;
	last: code;
    end;

    code_record_kind =
      ( inst_code,				(* machine instruction *)
	byte_code,				(* 8-bit relocatable value *)
	word_code,				(* 16-bit relocatable value *)
	long_code,				(* 32-bit relocatable value *)
	string_code,				(* sequence of characters (bytes) *)
	len_code,				(* 16-bit string length word *)
	set_code,				(* sequence of set words *)
	sreal_code,				(* single precision real longword *)
	dreal_code,				(* double precision real doubleword *)
	dtime_code,				(* date/time longword *)
	block_code,				(* allocate uninitialized space *)
	fill_code,				(* one fill byte for alignment *)
	align_code,				(* force alignment of next data record *)
	def_code,				(* symbolic definition *)
	pc_assert_code,				(* assert location counter value *)
	source_code,				(* start of statement (commentary *)
	comment_code,				(* a comment *)
	asm_label_code );			(* assembly listing label *)
$PAGE
    (*  A code record is one item of information for the emitter.  *)

    code_record = packed record
	next: code;				(* next code record in the list *)
	prev: code;				(* previous code record in the *)
	addr: code_address;			(* offset in section *)
	active: boolean;			(* normally true; false if record's to be ignored *)
	case kind: code_record_kind of
	  inst_code:
	    ( next_sdi: code;
	      inst: instruction );
	  byte_code,
	  word_code,
	  long_code:
	    ( pc_relative: boolean;
	      label_ref: ^label_reference_record;
	      relval: reloc_value );
	  string_code:
	    ( strval: packed array [1..*] of char );
	  len_code:
	    ( lenval: integer );
	  set_code:
	    ( setval: packed array [0..*] of boolean );
	  sreal_code,
	  dreal_code:
	    ( realval: real_type );
	  dtime_code:
	    ( dtimeval: dtime_int);
	  block_code:
	    ( blocksize: unit_range );
	  fill_code:
	    ( );
	  align_code:
	    ( );
	  def_code:
	    ( defname: def;
	      ref_list: ^label_reference_record);
	  pc_assert_code:
	    ( loc: code_address );
	  source_code:
	    ( stmtindex: 0 .. 63;
	      stmtid: source_id  );
	  comment_code:
	    ( cmtext: packed array [1..*] of char );
	  asm_label_code:
	    ( labtext: packed array [1..8] of char;
	      defd_by_pos: boolean;
	      static_offset: code_address )
    end;
$PAGE set descriptors

(* There are three formats: 
  
  "z" format: null set.
  
      nargs = 0
      other fields UNUSED
  
  "o" format: singleton or range set.
  
      nargs = 2
      arg[1] = addr descriptor of lower bound
      arg[2] = addr descriptor of upper bound
      lwb_exp = pointer to integral-valued expression tuple for lowerbound
                  (must be operand of a genset tuple).
      upb_exp = pointer to similar tuple for upper bound.
      in_temp = UNUSED.
  
  "l" format:
  
      nargs = 3
      arg[1] = addr descriptor for location of set
      arg[2] = addr descriptor for lower bound in WORDS
      arg[3] = addr descriptor for length in WORDS
      lwb_exp,
      upb_exp = UNUSED.
      in_temp = true if set is in a temporary, false otherwise.  *)

type
  set_desc = packed record
    nargs: 0..3;
    in_temp: boolean;
    lwb_exp,
    upb_exp: expr;
    arg: array [1..3] of op_desc
  end;
$PAGE string descriptors
(* The string descriptors are patterned after those used by the other
   checkout code generators for the PDP-10 and VAX. *)

type
  str_len_context = ( no_length, actual_length, max_length );

  string_descriptor_format =
      (	c_format,			(* address of aligned char *)
	f_format,			(* address of aligned string + length *)
	r_format  );			(* denotes remainder of last assigned string *)

  str_translation =
      (	no_trans,			(* no case translation *)
	upper_trans,			(* translate to uppercase *)
	lower_trans  );			(* translate to lowercase *)

  str_desc = ^ string_descriptor;

  string_descriptor = packed record
    base_addr: op_desc; (* base address of string, address of bounds word
			     for flex strings, address of length word for
			     non-flex verying strings *)
    base_is_bound: boolean; (* TRUE if BASE_ADDR is address of flex string
			       bounds word *)
    len_context: str_len_context; (* indicates whether LEN_ADDR is invalid or
				     actual length or maximum length *)
    len_addr: op_desc; (* addrress of length of string, not valid if
			    LEN_CONTEXT = NO_LENGTH *)
    max_len: char_range; (* compile-time upper bound on length of string *)
    text_valid: boolean; (* TRUE if TEXT_ADDR field is valid *)
    text_addr: op_desc; (* address of first char of string *)
    type_desc: expr_type_desc; (* type info about string *)
    trans_code: str_translation; (* indicates any case conversions pending
				    for the string *)
  end;

  free_procedure = procedure;
$PAGE boolean expression module types

type
    relations = ( ltc, lec, eqc, nec, gec, gtc, fc, tc );

(*  An ACTION PROC is a procedural parameter to some boolean expression
    routine.  Boolean expressions generate code to set the condition codes,
    and then call an action routine with a particular condition code value;
    the action routine will generate code which will depend on the condition
    code settings and the particular value passed to it.

    There are basically two kinds of action procedures.  A branch procedure
    generates a conditional branch, using the condition code passed to it.
    A value procedure allocates a data register and loads a zero or a one
    into it, using the condition code.  *)

type
    action_proc = procedure ( condition_codes );
$PAGE peephole optimizer types
(* The following record type is used in the peephole optimization process
   to construct a data structure showing all of the distinct references
   to labels marking positions in the code stream.  *)

type
  label_reference_record = packed record
    label_code_rec, (* ptr to the def_code code record *)
    inst_code_rec: code; (* ptr to a referencing instruction, word, or longword *)
    pc_op: 0..2; (* for a referencing instruction: which operand *)
    prev_ref, (* to chain the references to the same label *)
    next_ref: ^label_reference_record
  end;
    m:2m