$PAGE P10CG.TYP, last modified 1/16/84, zw
$IFNOT p10cgtyp

(*data structures used by the PDP10 code generators, formats
  specific to the machine, the linker, etc. are defined here*)

CONST
  bits_per_unit = 36; (*word size*)
  chars_per_unit = 5; (*characters/word*)

TYPE
  opc_range = 0 .. #o777; (*binary range of opcode values*)
  registers = 0 .. #o17; (*register numbers*)
  reg_selector = 0 .. #o20; (*registers + special values - see below*)
  set_of_registers = SET OF registers;
  bit_offset = 0 .. bits_per_unit - 1; (*bit offset within word*)
  elem_sizes = 0 .. bits_per_unit; (*bit widths of atomic data*)
  char_position = 1 .. chars_per_unit; (*character positions in a word*)
  code_address = 0 .. #o777777; (*address range*)
  code_offset = -MAXIMUM(code_address) .. MAXIMUM(code_address);
  (*note that a code offset may be negative -- the HALFWORD function must
    be used to convert all offsets into addresses*)

CONST
  noreg: reg_selector = 0; (*no register assigned, no indexing, etc.*)
  anyreg: reg_selector = #o20; (*any register assignment is ok*)
  int_nil: code_address = #o377777; (*NIL, defined with base type integer*)

TYPE
  code = ^code_record;
  def = ^definition;
  reg_desc = ^register_descriptor;
  val_desc = ^value_descriptor; (*not used by the checkout code generator*)
  addr_ptr = ^addr_desc; (*not used by the optimizing code generator*)

$PAGE

(*PDP-10 words -- an undiscrimated union of words of all useful formats
  used in code records and rel files*)

  pdp10word = PACKED RECORD
    CASE CHAR OF
      'I': (opcode: opc_range; (*instruction*)
	 acc: registers;
	 indirect: BOOLEAN;
	 index: registers;
	 offset: code_address);
      'B': (p: 0..#o77; (*byte pointer*)
	 s: 0..#o77;
	 bp1: 0..1; (*unused: mbz*)
	 bpindirect: BOOLEAN; (*instruction field names used*)
	 bpindex: registers;
	 bpoffset: code_address);
      'F': (value: MACHINE_WORD); (*full word integer*)
      'R': (rvalue: REAL); (*single-precision real*)
      'S': (bits: PACKED ARRAY[bit_offset] OF BOOLEAN); (*full word set*)
      'X': (lh: code_address; (*halfwords*)
	 rh: code_address  );
      'C': (str: PACKED ARRAY[char_position] OF CHAR); (*character string*)
      '6': (sixbit: PACKED ARRAY[1..6] OF 0..#o77); (*six bit characters*)
      '5': (code50: 0..17b; (*radix-50 code/symbol word*)
	 sym50: 0..37777777777b);
      'L': (rel_byte: PACKED ARRAY [1..18] of 0..3); (*Link-10 reloc bytes*)
      'T': (dtime_value: dtime_int) (*so P10DEB can write comp_dtime's*)
  END;

$PAGE

(*The following defines the set of runtime symbols (runtime subroutines,
  constants) which may be referenced by the code generator.  The rel file
  generator maps these to actual names when building the rel file.  Note:
  (1) double precision routines *must* follow the corresponding single
  precision routines, and (2) extra symbols are defined to allow for
  expansion*)

  rt_symbol =
    (rt_init, (*initialize the environment*)
    rt_entry, (*subroutine entry*)
    rt_return, (*subroutine return*)
    rt_stop, (*halt routine*)
    rt_uw_rtn, (*stack unwind to specified frame*)
    rt_uw_prg, (*stack unwind to main program*)
    rt_exp_ii, (*exponentiation*)
    rt_exp_ri,
    rt_exp_di,
    rt_exp_rr,
    rt_exp_dd,
    rt_d_float, rt_d_trunc, rt_d_round,
    rt_r_rnd2, rt_d_rnd2,
    rt_r_sqrt, rt_d_sqrt, (*math functions*)
    rt_r_ln, rt_d_ln,
    rt_r_log, rt_d_log,
    rt_r_exp, rt_d_exp,
    rt_r_sin, rt_d_sin,
    rt_r_asin, rt_d_asin,
    rt_r_sinh, rt_d_sinh,
    rt_r_cos, rt_d_cos,
    rt_r_acos, rt_d_acos,
    rt_r_cosh, rt_d_cosh,
    rt_r_tan, rt_d_tan,
    rt_r_tanh, rt_d_tanh,
    rt_r_ctan, rt_d_ctan,
    rt_r_atan, rt_d_atan,
    rt_r_atn2, rt_d_atn2,
    rt_rand_set, (*one and zero operand forms*)
    rt_rand,
    rt_new, (*allocation routines*)
    rt_dispose,
    rt_extent,

$PAGE

    rt_sub_chk, (*error check reporting*)
    rt_str_chk,
    rt_val_chk,
    rt_ptr_chk,
    rt_fil_chk,
    rt_fld_chk,
    rt_cmp_chk,
    rt_ass_chk,
    rt_cas_chk,
    rt_cmp_fc, rt_cmpu_fc, rt_cmpl_fc,
    rt_cmp_ff, rt_cmpu_ff, rt_cmpl_ff,
    rt_cmp_fx, rt_cmpu_fx, rt_cmpl_fx,
    rt_cmp_xc, rt_cmpu_xc, rt_cmpl_xc,
    rt_cmp_xf, rt_cmpu_xf, rt_cmpl_xf,
    rt_cmp_xx, rt_cmpu_xx, rt_cmpl_xx,
    rt_cmp_rc, rt_cmpu_rc, rt_cmpl_rc,
    rt_cmp_rf, rt_cmpu_rf, rt_cmpl_rf,
    rt_cmp_rx, rt_cmpu_rx, rt_cmpl_rx,
    rt_cm_fc, rt_cmu_fc, rt_cml_fc,
    rt_cm_ff, rt_cmu_ff, rt_cml_ff,
    rt_cm_fx, rt_cmu_fx, rt_cml_fx,
    rt_cm_xc, rt_cmu_xc, rt_cml_xc,
    rt_cm_xf, rt_cmu_xf, rt_cml_xf,
    rt_cm_xx, rt_cmu_xx, rt_cml_xx,
    rt_cm_rc, rt_cmu_rc, rt_cml_rc,
    rt_cm_rf, rt_cmu_rf, rt_cml_rf,
    rt_cm_rx, rt_cmu_rx, rt_cml_rx,
    rt_ccp_fc, rt_ccpu_fc, rt_ccpl_fc,
    rt_ccp_ff, rt_ccpu_ff, rt_ccpl_ff,
    rt_ccp_fx, rt_ccpu_fx, rt_ccpl_fx,
    rt_ccp_xc, rt_ccpu_xc, rt_ccpl_xc,
    rt_ccp_xf, rt_ccpu_xf, rt_ccpl_xf,
    rt_ccp_xx, rt_ccpu_xx, rt_ccpl_xx,
    rt_cc_fc, rt_ccu_fc, rt_ccl_fc,
    rt_cc_ff, rt_ccu_ff, rt_ccl_ff,
    rt_cc_fx, rt_ccu_fx, rt_ccl_fx,
    rt_cc_xc, rt_ccu_xc, rt_ccl_xc,
    rt_cc_xf, rt_ccu_xf, rt_ccl_xf,
    rt_cc_xx, rt_ccu_xx, rt_ccl_xx,
    rt_cp_cf, rt_cp_cx, (*string compares, _cc is scalar op*)
    rt_cp_fc, rt_cp_ff, rt_cp_fx,
    rt_cp_xc, rt_cp_xf, rt_cp_xx,
    rt_cix_cc, rt_cix_cf, rt_cix_cx, (*string index operators*)
    rt_cix_fc, rt_cix_ff, rt_cix_fx,
    rt_cix_xc, rt_cix_xf, rt_cix_xx,

$PAGE

    rt_mask_ss, (*set masks*)
    rt_mask_sd1,
    rt_smv_ll, rt_smv_lo, rt_smv_lz, (*set moves*)
    rt_sun_ll, rt_sin_ll, rt_sdf_ll, (*set operations*)
    rt_sle_ll, rt_seq_ll, (*set comparisions*)
    rt_in_vl, (*in operators*)
    rt_sr_cl, rt_sr_fl, rt_sr_xl, (*search operators*)
    rt_sru_cl, rt_sru_fl, rt_sru_xl,
    rt_sr_co, rt_sr_fo, rt_sr_xo,
    rt_sru_co, rt_sru_fo, rt_sru_xo,
    rt_sr_cd, rt_sr_fd, rt_sr_xd,
    rt_sru_cd, rt_sru_fd, rt_sru_xd,
    rt_vf_cl, rt_vf_fl, rt_vf_xl, (*verify operators*)
    rt_vfu_cl, rt_vfu_fl, rt_vfu_xl,
    rt_vf_co, rt_vf_fo, rt_vf_xo,
    rt_vfu_co, rt_vfu_fo, rt_vfu_xo,
    rt_vf_cd, rt_vf_fd, rt_vf_xd,
    rt_vfu_cd, rt_vfu_fd, rt_vfu_xd,
    rt_stmt, (*for debugger*)
    rt_open, rt_rewrite, rt_reset, (*input / output*)
    rt_open_typed, rt_open_binary,
    rt_get,
    rt_get_char, rt_getch_rem,
    rt_put,
    rt_put_char, rt_putch_rem,
    rt_read_binary, rt_write_binary,
    rt_read_image, rt_write_image,
    rt_close, rt_scratch, rt_close_all,
    rt_file_size,
    rt_break,
    rt_empty_text, rt_empty,
    rt_page,
    rt_clear,
    rt_iostatus,
    rt_iostat_last,
    rt_extstatus,
    rt_rd_fdn, rt_rd_fdr,
    rt_wr_fdn, rt_wr_fdr,
    rt_wr_svn, rt_wr_svr,
    rt_rd_ssn, rt_rd_ssr,
    rt_wr_ssn, rt_wr_ssr,
    rt_rd_lnn, rt_rd_lnr,
    rt_wr_lnn, rt_wr_lnr,
    rt_wr_dnn, rt_wr_dnr,

$PAGE

    rt_seek,
    rt_int_read, rt_int_write,
    rt_real_read, rt_real_write,
    rt_xstr_read, rt_xstr_write,
    rt_fstr_read, rt_fstr_write,
    rt_cstr_read, rt_cstr_write,
    rt_strv_read,
    rt_bool_write,
    rt_set_handler, rt_rst_handler,
    rt_mask, rt_unmask, rt_masked, rt_pending,
    rt_signal, rt_resignal,
    rt_stat_math, rt_stat_program,
    rt_stat_special, rt_stat_exio,
    rt_exmessage,
    rt_inst_sml,
    rt_date, rt_time, rt_runtime,
    rt_d_to_s); (*double-to-single precision real rounding*)

rts_name_array = ARRAY[rt_symbol] OF PACKED ARRAY [1..6] OF CHAR;

$PAGE

(*Definition records define internal symbols, generated during code generation.
  They are used to label and resolve subroutine entry points, nonlocal labels,
  references to tables in the code, constant references, etc.  Identifying
  numbers are assigned depending on the type.  For symbols and subroutine defs,
  the id_number of their corresponding symbols is used.  In this way, the
  number is determined before forward references are made.  Numbers for local
  and label defs are recycled at the end of each block.  Numbers for other
  types are assigned as needed.*)

  def_types =
    (sym_def, (*user declared symbol*)
    constant_def, (*loc in constant area*)
    hnd_tab_def, (*handler table in constant area*)
    subr_def, (*block id*)
    deb_sym_def, (*debug symbol table indices from trace blocks*)
    temp_size_def, (*total stack frame size*)
    code_def, (*control block in code, e.g. for debugger*)
    local_def, (*definition local to code area of block*)
    label_def, (*label node reference local to a block*)
    extern_def, (*external symbol reference*)
    offset_def); (*denotes offset relative to one of the above*)

  definition = PACKED RECORD
    next: def; (*next definition of same type*)
    rbacklink: code_address; (*for generating linker requests*)
    lbacklink: code_address;
    defined: BOOLEAN; (*set true when the address is set*)
    relocatable: BOOLEAN; (*true if the address is relocatable*)
    fixup_required: BOOLEAN; (*true if Polish fixup needed for definition*)
    CASE deftype: def_types OF
      sym_def..extern_def:
        (first_offset: def; (*chain thru def^.next of offset defs for symbol*)
        CASE def_types OF
          sym_def..label_def:
	    (addr: code_address; (*offset within above class*)
	    defnumber: id_range); (*logical name see above numbering scheme*)
          extern_def:
	    (ext_name: pdp10word)); (*radix-50 external symbol name*)
      offset_def:
	(reldef: def; (*definition to which this is relative*)
	offset: int_type) (*offset from above*)
  END;

$PAGE

(*Relocation syllables define how to relocate fields of code records, which
  are defined below.*)

  rel_syllable = PACKED RECORD
    CASE kind: storage_class OF
      register_sc, (*field is register number*)
      absolute_sc: (*no relocation required*)
        ( );
      local_sc, parameter_sc, static_sc, (*user syms defined in areas*)
      external_sc: (*external name*)
        (relsym: sym); (*add value of this symbol to the offset*)
      runtime_sc: (*add in value of runtime symbol to field*)
        (relrtsym: rt_symbol);
      def_sc: (*reference to internal label*)
         (reldef: def); (*to label defined*)
      temp_sc:
        (relval: val_desc) (*unused by quick pass*)
  END;

$PAGE

(*Code records are an ordered list of code and data words, tagged with
  relocation information.  In effect, they constitute assembly language
  statements. During code generation, three lists are maintained.  The
  first is the code for the current block.  The second is the image of
  the static area as initialized.  The third is the constant area which
  is pooled for all blocks.*)

  code_types = (instruction, bytepointer, fullword, stringword, setword,
    realword, drealword, drealword2, halfwords, defmark, origin, source,
    comment, deftemp, asm_label, nullcode);

  code_record = PACKED RECORD
    next: code; (*to word with next higher address*)
    CASE kind: code_types OF
      instruction:
        (inst: pdp10word; (*instruction word*)
        reloc: rel_syllable); (*relocation for inst.offset*)
      bytepointer:
        (bptr: pdp10word;
        bpreloc: rel_syllable); (*for bptr.offset*)
      fullword, stringword, setword, realword, drealword, drealword2:
        (fwd: pdp10word);
      halfwords:
        (xwd: pdp10word;
        lreloc: rel_syllable; (*for xwd.lh*)
        rreloc: rel_syllable); (*for xwd.rh*)
      defmark: (*define a local symbol*)
        (defname: def); (*gives 'name', records references*)
      deftemp: (*notes introduction of non-global temporary*)
        (tempname: val_desc);
      origin: (*sets relocation counter of containing area*)
        (location: code_address); (*new offset*)
      source: (*commentary: defines where code for statement starts*)
        (stmtid: source_id;
        stmtindx: source_index);
      comment: (*arbitrary commentary*)
        (ctext: PACKED ARRAY [1..*] OF CHAR);
      asm_label: (*arbitrary label to be put into assembly listing*)
        (ltext: STRING[6])
  END;

  code_list = RECORD
    first, last: code (*list through code^.next of code_records*)
  END;

$PAGE

(*An addr desc is used to describe a memory location.  The mode denotes how
  to access the data.*)

  addr_mode =
    (fw, (*data occupies 1 or move fullwords*)
    lhw, (*data in left half of addressed word*)
    rhw, (*data in right half*)
    byte, (*contents (address) is byte ptr to data*)
    slice); (*data is in addressed word, see slice_size and slice_offset*)

  addr_desc = PACKED RECORD
    mode: addr_mode;
    has_sign: BOOLEAN;
    immediate: BOOLEAN;
    indirect: BOOLEAN;
    index: registers;
    offset: code_offset;
    slice_size: 0 .. bits_per_unit;
    slice_offset: 0 .. bits_per_unit; (*number of bits to left of slice*)
    reloc: rel_syllable (*relocation for offset*)
  END;

$PAGE

(*A value descriptor is assigned to each value/temporary in the program.
  (1) As an expr is encountered during code generation, a value descriptor
  is associated with it.  The descriptor serves to locate the result of the
  computation by identifying the permanent location (stack temporary or
  register) assigned to it, and any register currently holding a copy of
  the value.  (2) These temporaries are also used to describe stack temporaries
  which are developed internally (such as parameter lists).   The descriptors
  are managed by a reference count, which indicates when the descriptor is
  no longer needed.  If the value is never assigned a memory location and is
  not live on exit to a basic block, then its descriptor is discarded when
  the reference count reaches zero;  otherwise, it may be retained until the
  end of the current procedure block so that its actual temporary address may
  be assigned.
  Value descriptors are used only by the optimizing code generator, and not
  by the checkout code generator.*)

  value_descriptor = PACKED RECORD
    next, prev: val_desc; (*doubly linked list of all value descriptors*)
    size: unit_range; (*size of temporary required*)
    reg: reg_desc; (*register holding copy of value*)
    loc_index: val_desc; (*value required to complete address below*)
    loc: addr_desc; (*memory address assigned to value;
                      loc.reloc.kind = unallocated implies not assigned*)
    uses: 0..4095; (*number of remaining references to value*)
    allocate: BOOLEAN; (*initially false, set true if temp used*)
    global: BOOLEAN; (*set true if value live at exit to a basic block*)
    generated: BOOLEAN; (*implies computation has been performed*)
    free_at_stmt: BOOLEAN; (*implies temp should be freed at end of statement*)
    is_dirty: BOOLEAN; (*value is not to be used -- it is dirty*)
    CASE internal: BOOLEAN OF
      true: (temp_id: id_range); (*internally developed, id for macro listing*)
      false: (op: expr) (*expr node corresponding to this value*)
  END;

$PAGE

(*Register descriptors define the machine registers.  Each descriptor denotes
  a single register.  Groups of descriptors are formed for multiword values.*)

  reg_status = PACKED RECORD (*used by checkout code generator*)
    associate: registers; (*non-zero if tagged to another register*)
    uses_remaining: usage_range
  END;
  reg_descriptor = ARRAY[registers] OF reg_status;

  register_descriptor = PACKED RECORD (*used by optimizing code generator*)
    group_size: unit_range; (*size of register allocated, e.g. 2 => reg pair*)
    value_size: unit_range; (*size of value in register*)
    contents: val_desc; (*value loaded in location*)
    free: BOOLEAN; (*true => location not in use*)
    locked: BOOLEAN; (*true => prevents allocation or displacement*)
    succ_locked: BOOLEAN; (*true => reg+1 is also locked*)
    signed: BOOLEAN; (*true => signed value in register*)
    loc: addr_desc (*location; (loc.mode = rhw) => lhw garbage*)
  END;

(*A register state describes the contents of a register file at a certain point
  in time.  Register states are saved at the end of each basic block and the
  states of predecessor blocks are merged when a basic block is entered.*)

  reg_state_ptr = ^reg_state;
  reg_state = ARRAY [registers] OF PACKED RECORD
    size: unit_range;
    contents: expr
  END;

$PAGE

(*String accessing information.*)

  str_desc_format =
    (c_format, (*address of aligned char*)
    f_format, (*address of aligned string + length*)
    x_format, (*address of byte ptr + length*)
    r_format); (*denotes remainder of last assigned string*)
  str_translation =
    (no_trans, (*no case translation*)
    upper_trans, (*translate to uppercase*)
    lower_trans); (*translate to lowercase*)

(*Set accessing information.*)

  set_desc_format =
    (l_format, (*long aligned string*)
    o_format, (*range of ones: [op1..op2]*)
    z_format); (*null set: []*)

(*Data accessing information*)

  data_alignment =
    (unsigned_value, (*right justified, zero extended*)
    signed_value, (*right justified, sign extended*)
    left_aligned, (*left justified, zero padded*)
    left_unpadded, (*left justified, no padding*)
    right_aligned); (*right justified, zero padded (non arithmetic*)

$PAGE

(*miscellaneous standard constants -- useful constants of above types*)

CONST
  none: rel_syllable := (absolute_sc);
  register: rel_syllable := (register_sc);
  dot: rel_syllable := (self_rel_sc);
  reg_addr_desc: addr_desc :=
    (fw, false, false, false, 0, 0, 0, 0, (register_sc));
  null_location: addr_desc :=
    (fw, false, false, false, 0, 0, 0, 0, (unallocated));
  immediate_reference: addr_desc :=
    (fw, false, true, false, 0, 0, 0, 0, (absolute_sc));
  absolute_reference: addr_desc :=
    (fw, false, false, false, 0, 0, 0, 0, (absolute_sc));
  temp_reference: addr_desc :=
    (fw, false, false, false, 0, 0, 0, 0, (temp_sc, NIL));
  parm_reference: addr_desc :=
    (fw, false, false, false, #o16, 0, 0, 0, (parameter_sc, NIL));

$ENABLE p10cgtyp
$ENDIF
  