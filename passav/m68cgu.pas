$TITLE M68CGU - code generator utilities
module m68cgu options check, special (word);
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM ptmcon.inc
$SYSTEM pastal.inc
$SYSTEM m68cg.typ
$SYSTEM m68utl.inc
$SYSTEM m68emt.inc
$SYSTEM pasmth.inc
$PAGE public, static variables
public var
  code_area, (* chain of code records for code area *)
  cst_area: code_list; (* chain for constant area *)

static var
  def_lists: array[def_class] of def;
  def_num: array[def_class] of id_range;
$PAGE cdl_init
(* CDL INIT initializes a specified code list. *)

public procedure cdl_init (var area: code_list);

begin
  area := (nil, nil);
end (* cdl_init *);
$PAGE gen_emit
(* GEN EMIT appends a code record to a given code list. *)

procedure gen_emit (var area: code_list; cr: code);

begin
  with area, cr^ do begin
    if first = nil then
      first := cr;
    if last <> nil then
      last^.next := cr;
    prev := last;
    last := cr;
    next := nil;
    active := true;
  end;
end (* gen_emit *);
$PAGE gen_inst
(* GEN INST emits an instruction record in a specified area (actually the
   code area, though this routine makes no such assumptions).  The instruction
   is exhaustively checked to insure its legality. *)

public procedure gen_inst (
	var area: code_list;
	operation: generic_opcodes;
	condition: condition_codes;
	source: op_desc;
	destination: op_desc );
$PAGE is_quick - in gen_inst
(*  IS QUICK tests whether an operand is a legal "quick" mode operand.  *)

function is_quick ( oper : op_desc ) : boolean;

begin
  with oper, cst_part do
    is_quick := (mode = immediate_mode) and
		(kind = absolute_sc) and
		(offset >= 1) and
		(offset <= 8);
end (* is_quick *);
$PAGE validate_operand - in gen_inst
(*  VALIDATE OPERAND checks the fields of an operand descriptor for consistency.
    Errors such as displacements that exceed 16 bits cannot be caught here,
    since relocation has not yet been performed, but the more glaring errors
    can be detected.  *)

function validate_operand ( oper : op_desc ) : op_desc;

begin
  validate_operand := oper;
  with validate_operand do begin

    if not (mode in indexed_modes) then
      assert (index_reg = d0);

    if not (mode in cst_use_modes) then
      assert ( (cst_part.kind = absolute_sc) and (cst_part.offset = 0) );

    if mode in areg_relative_modes + stack_modes + [areg_mode] then
      assert (reg in addr_reg_set)
    else if mode = dreg_mode then
      assert (reg in data_reg_set)
    else
      assert (reg = d0);

    if not (mode in control_modes + [null_mode]) then
      assert (value_size in reg_sizes);

    if mode = areg_mode then
      assert (value_size <> size_byte);

    if extended_size = no_size then
      extended_size := value_size;

    assert (value_size <= extended_size);

    if mode = areg_mode then
      assert (extended_size = size_long)
    else if not (mode in [dreg_mode, immediate_mode]) then
      assert (extended_size = value_size);

    if (mode = immediate_mode) and (cst_part.kind = absolute_sc) then begin
      if signed_value then
	assert ( (cst_part.offset >= sgn_min [value_size]) and
		 (cst_part.offset <= sgn_max [value_size]) )
      else
	assert ( (cst_part.offset >= uns_min [value_size]) and
		 (cst_part.offset <= uns_max [value_size]) );
    end;

  end (* with validate_operand *);
end (* validate_operand *);
$PAGE translate_opcode - in gen_inst
(*  TRANSLATE OPCODE selects a specific opcode for a particular generic
    opcode, source operand, and destination operand.  The selected operand
    is not guaranteed to be valid for the operands (that is for VALIDATE_
    INSTRUCTION to determine), but it will be as valid as any other.  *)

function translate_opcode ( operation : generic_opcodes;
			    source, destination : op_desc
							  ) : specific_opcodes;

const
    choice_table : array [generic_opcodes] of specific_opcodes =
      ( add_dm_opc,	and_dm_opc,	asl_dd_opc,	asr_dd_opc,
	bcc_p_opc,	bchg_dd_opc,	bclr_dd_opc,	bset_dd_opc,
	btst_dd_opc,	clr_m_opc,	cmp_im_opc,	dbcc_dw_opc,
	divs_md_opc,	divu_md_opc,	eor_dm_opc,	exg_aa_opc,
	ext_d_opc,	jmp_m_opc,	jsr_m_opc,	lea_ma_opc,
	link_aw_opc,	lsl_dd_opc,	lsr_dd_opc,	move_bd_opc,
	movem_mw_opc,	muls_md_opc,	mulu_md_opc,	neg_m_opc,
	nop_x_opc,	not_m_opc,	or_dm_opc,	pea_m_opc,
	rol_dd_opc,	ror_dd_opc,	rts_x_opc,	scc_m_opc,
	sub_dm_opc,	swap_d_opc,	tst_m_opc,	unlk_a_opc );
begin
  translate_opcode := choice_table [operation];
  case operation of

    add_opc,
    sub_opc :
      if is_quick (source) then				(* op_qm *)
	translate_opcode := succ (succ (succ (succ (translate_opcode))))
      else if destination.mode = dreg_mode then		(* op_md *)
	translate_opcode := succ (succ (succ (translate_opcode)))
      else if destination.mode = areg_mode then		(* op_ma *)
	translate_opcode := succ (succ (translate_opcode))
      else if source.mode = dreg_mode then		(* op_dm *)
	(* base opcode *)
      else						(* op_im *)
	translate_opcode := succ (translate_opcode);

    and_opc,
    or_opc :
      if destination.mode = dreg_mode then		(* op_md *)
	translate_opcode := succ (succ (translate_opcode))
      else if source.mode = dreg_mode then		(* op_dm *)
	(* base opcode *)
      else						(* op_im *)
	translate_opcode := succ (translate_opcode);

    asl_opc,
    asr_opc,
    lsl_opc,
    lsr_opc,
    rol_opc,
    ror_opc :
      if source.mode = dreg_mode then			(* op_dd *)
	(* base opcode *)
      else if source.mode = null_mode then		(* op_m *)
	translate_opcode := succ (translate_opcode)
      else						(* op_qd *)
	translate_opcode := succ (succ (translate_opcode));

    bchg_opc,
    bclr_opc,
    bset_opc,
    btst_opc :
      if source.mode = dreg_mode then begin
	if destination.mode = dreg_mode then		(* op_dd *)
	  (* base opcode *)
	else						(* op_dm *)
	  translate_opcode := succ (translate_opcode);
      end
      else begin
	if destination.mode = dreg_mode then		(* op_wd *)
	  translate_opcode := succ (succ (translate_opcode))
	else						(* op_wm *)
	  translate_opcode := succ (succ (succ (translate_opcode)));
      end;

    cmp_opc :
      if source.mode = immediate_mode then
	translate_opcode := cmp_im_opc
      else if destination.mode = areg_mode then
	translate_opcode := cmp_ma_opc
      else if destination.mode = dreg_mode then
	translate_opcode := cmp_md_opc
      else
	translate_opcode := cmp_mm_opc;

    eor_opc :
      if source.mode = immediate_mode then
	translate_opcode := eor_im_opc
      else
	translate_opcode := eor_dm_opc;

    exg_opc :
      if source.mode = areg_mode then
	translate_opcode := exg_aa_opc
      else if destination.mode = areg_mode then
	translate_opcode := exg_da_opc
      else
	translate_opcode := exg_dd_opc;

    jmp_opc :
      assert (destination.mode <> pc_displacement_mode); (* assumption about jmp vs. bcc usage *)

    jsr_opc :
      if destination.mode = pc_displacement_mode then
	translate_opcode := bsr_p_opc;

    move_opc :
      if destination.mode = areg_mode then
	translate_opcode := move_ma_opc
      else if (source.mode = immediate_mode) and
	      (source.value_size = size_byte) and
	      (source.signed_value) and
	      (destination.mode = dreg_mode) and
	      (destination.value_size = size_long) then
	translate_opcode := move_bd_opc
      else
	translate_opcode := move_mm_opc;

    movem_opc :
      if source.mode = immediate_mode then
	translate_opcode := movem_wm_opc
      else
	translate_opcode := movem_mw_opc;

    others :
      (* only one specific opcode *);

  end;
end (* translate_opcode *);
$PAGE validate_instruction - in gen_inst
(*  VALIDATE_INSTRUCTION checks whether the operand modes, sizes, and
    sign attributes are acceptable for the instruction, and whether the
    operands are compatible with each other.  *)

procedure validate_instruction ( var instr : instruction );

type

    (*  An OP_CHECK_RECORD specifies what addressing modes, sizes, and
	sign attributes are allowable for an operand.  *)

    op_check_record = packed record
	valid_modes : addr_mode_set;
	valid_sizes : op_size_set;
	valid_signs : set of boolean;
    end;

    (*  An INST_CHECK_RECORD specifies allowable attributes for each operand
	of an instruction, whether its operands must have compatible sizes,
	and whether its first operand is quick (1..8).  *)

    inst_check_record = packed record
	op_checks : array [1..2] of op_check_record;
	attributes : set of (len_compat, quick_mode);
    end;

const
    loadm_modes : addr_mode_set = control_modes + [postincrement_mode];
    storem_modes: addr_mode_set = (control_modes * alterable_modes) + [predecrement_mode];

    uns : set of boolean = [false];
    sgn : set of boolean = [true];
    s_u : set of boolean = [false, true];
$PAGE
const
    als_op   : op_check_record = ( [areg_mode], long_only, sgn );
    as_op    : op_check_record = ( [areg_mode], reg_sizes, sgn );
    d_op     : op_check_record = ( [dreg_mode], reg_sizes, s_u );
    dls_op   : op_check_record = ( [dreg_mode], long_only, sgn );
    dw_op    : op_check_record = ( [dreg_mode], word_only, s_u );
    dwls_op  : op_check_record = ( [dreg_mode], word_min, sgn );
    dws_op   : op_check_record = ( [dreg_mode], word_only, sgn );
    e_op     : op_check_record = ( all_modes, any_size, s_u );
    ea_op    : op_check_record = ( alterable_modes + stack_modes, any_size, s_u );
    ec_op    : op_check_record = ( control_modes, any_size, s_u );
    ed_op    : op_check_record = ( data_modes + stack_modes, any_size, s_u );
    eda_op   : op_check_record = ( data_alterable_modes + stack_modes, any_size, s_u );
    edab_op  : op_check_record = ( data_alterable_modes + stack_modes, byte_only, s_u );
    edb_op   : op_check_record = ( data_modes + stack_modes, byte_only, s_u );
    edw_op   : op_check_record = ( data_modes + stack_modes, word_only, s_u );
    ewls_op  : op_check_record = ( all_modes, word_min, s_u );
    ema_op   : op_check_record = (memory_alterable_modes + stack_modes, any_size, s_u );
    emaw_op  : op_check_record = ( memory_alterable_modes + stack_modes, word_only, s_u );
    i_op     : op_check_record = ( [immediate_mode], reg_sizes, s_u );
    ibs_op   : op_check_record = ( [immediate_mode], byte_only, sgn );
    is_op    : op_check_record = ( [immediate_mode], reg_sizes, sgn );
    iw_op    : op_check_record = ( [immediate_mode], word_only, s_u );
    iws_op   : op_check_record = ( [immediate_mode], word_only, sgn );
    p_op     : op_check_record = ( [pc_displacement_mode], any_size, s_u );
    x_op     : op_check_record = ( [null_mode], [no_size], sgn );
    y1_op    : op_check_record = ( loadm_modes, word_min, s_u );
    y2_op    : op_check_record = ( storem_modes, word_min, s_u );
    zs_op    : op_check_record = ( [postincrement_mode], reg_sizes, sgn );
$PAGE
const
    inst_check_table : array [specific_opcodes] of inst_check_record =
      ( (* add_dm *)	( (d_op,     ema_op),   [len_compat] ),
	(* add_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* add_ma *)	( (ewls_op,  as_op),    [ ]          ),
	(* add_md *)	( (ed_op,    d_op),     [len_compat] ),
	(* add_qm *)	( (is_op,    ea_op),    [quick_mode] ),
	(* and_dm *)	( (d_op,     ema_op),   [len_compat] ),
	(* and_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* and_md *)	( (ed_op,    d_op),     [len_compat] ),
	(* asl_dd *)	( (d_op,     d_op),     [ ]          ),
	(* asl_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* asl_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* asr_dd *)	( (d_op,     d_op),     [ ]          ),
	(* asr_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* asr_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* bcc_p *)	( (x_op,     p_op),     [ ]          ),
	(* bchg_dd *)	( (d_op,     dls_op),   [ ]          ),
	(* bchg_dm *)	( (d_op,     edab_op),  [ ]          ),
	(* bchg_wd *)	( (iws_op,   dls_op),   [ ]          ),
	(* bchg_wm *)	( (iws_op,   edab_op),  [ ]          ),
	(* bclr_dd *)	( (d_op,     dls_op),   [ ]          ),
	(* bclr_dm *)	( (d_op,     edab_op),  [ ]          ),
	(* bclr_wd *)	( (iws_op,   dls_op),   [ ]          ),
	(* bclr_wm *)	( (iws_op,   edab_op),  [ ]          ),
	(* bset_dd *)	( (d_op,     dls_op),   [ ]          ),
	(* bset_dm *)	( (d_op,     edab_op),  [ ]          ),
	(* bset_wd *)	( (iws_op,   dls_op),   [ ]          ),
	(* bset_wm *)	( (iws_op,   edab_op),  [ ]          ),
	(* bsr_p *)	( (x_op,     p_op),     [ ]          ),
	(* btst_dd *)	( (d_op,     dls_op),   [ ]          ),
	(* btst_dm *)	( (d_op,     edb_op),   [ ]          ),
	(* btst_wd *)	( (iws_op,   dls_op),   [ ]          ),
	(* btst_wm *)	( (iws_op,   edb_op),   [ ]          ),
	(* clr_m *)	( (x_op,     eda_op),   [ ]          ),
	(* cmp_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* cmp_ma *)	( (ewls_op,  as_op),    [ ]          ),
	(* cmp_md *)	( (e_op,     d_op),     [len_compat] ),
	(* cmp_mm *)	( (zs_op,    zs_op),    [len_compat] ),
	(* dbcc_dw *)	( (dws_op,   p_op),     [ ]          ),
	(* divs_md *)	( (edw_op,   dls_op),   [ ]          ),
	(* divu_md *)	( (edw_op,   dls_op),   [ ]          ),
	(* eor_dm *)	( (d_op,     eda_op),   [len_compat] ),
	(* eor_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* exg_aa *)	( (als_op,   als_op),   [ ]          ),
	(* exg_da *)	( (als_op,   dls_op),   [ ]          ),
	(* exg_dd *)	( (dls_op,   dls_op),   [ ]          ),
	(* ext_d *)	( (x_op,     dwls_op),  [ ]          ),
	(* jmp_m *)	( (x_op,     ec_op),    [ ]          ),
	(* jsr_m *)	( (x_op,     ec_op),    [ ]          ),
	(* lea_ma *)	( (ec_op,    als_op),   [ ]          ),
	(* link_aw *)	( (als_op,   iws_op),   [ ]          ),
	(* lsl_dd *)	( (d_op,     d_op),     [ ]          ),
	(* lsl_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* lsl_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* lsr_dd *)	( (d_op,     d_op),     [ ]          ),
	(* lsr_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* lsr_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* move_bd *)	( (ibs_op,   dls_op),   [ ]          ),
	(* move_ma *)	( (ewls_op,  as_op),    [ ]          ),
	(* move_mm *)	( (e_op,     eda_op),   [len_compat] ),
	(* movem_mw *)	( (y1_op,    iw_op),    [ ]          ),
	(* movem_wm *)	( (iw_op,    y2_op),    [ ]          ),
	(* muls_md *)	( (edw_op,   dw_op),    [ ]          ),
	(* mulu_md *)	( (edw_op,   dw_op),    [ ]          ),
	(* neg_m *)	( (x_op,     eda_op),   [ ]          ),
	(* nop *)	( (x_op,     x_op),     [ ]          ),
	(* not_m *)	( (x_op,     eda_op),   [ ]          ),
	(* or_dm *)	( (d_op,     ema_op),   [len_compat] ),
	(* or_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* or_md *)	( (ed_op,    d_op),     [len_compat] ),
	(* pea_m *)	( (x_op,     ec_op),    [ ]          ),
	(* rol_dd *)	( (d_op,     d_op),     [ ]          ),
	(* rol_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* rol_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* ror_dd *)	( (d_op,     d_op),     [ ]          ),
	(* ror_m *)	( (x_op,     emaw_op),  [ ]          ),
	(* ror_qd *)	( (is_op,    d_op),     [quick_mode] ),
	(* rts *)	( (x_op,     x_op),     [ ]          ),
	(* scc_m *)	( (x_op,     edab_op),  [ ]          ),
	(* sub_dm *)	( (d_op,     ema_op),   [len_compat] ),
	(* sub_im *)	( (i_op,     eda_op),   [len_compat] ),
	(* sub_ma *)	( (ewls_op,  as_op),    [ ]          ),
	(* sub_md *)	( (ed_op,    d_op),     [len_compat] ),
	(* sub_qm *)	( (is_op,    ea_op),    [quick_mode] ),
	(* swap_d *)	( (x_op,     dw_op),    [ ]          ),
	(* tst_m *)	( (x_op,     eda_op),   [ ]          ),
	(* unlk_a *)	( (x_op,     als_op),   [ ]          ) );
$PAGE
var i : 1 .. 2;

begin
  with instr, inst_check_table [opcode] do begin

    (*  First, check for operand size compatibility if it is required,
	doing length extension if it is allowed and will help.  *)

    if len_compat in attributes then begin
      if operands [1].value_size < operands [2].value_size then begin
	assert (operands [1].extended_size >= operands [2].value_size);
	operands [1].value_size := operands [2].value_size;
	operands [1].signed_value := true;
      end
      else if operands [2].value_size < operands [1].value_size then begin
	assert (operands [2].extended_size >= operands [1].value_size);
	operands [2].value_size := operands [1].value_size;
	operands [2].signed_value := true;
      end;
    end;

    (*  Check each of the operands to against the allowable operands modes
	for this instruction.  *)

    for i := 1 to 2 do begin
      with operands [i], op_checks [i] do begin
	if (mode = pc_displacement_mode) and
	   not (pc_displacement_mode in valid_modes) and
	   (abs_l_mode in valid_modes) then
	  mode := abs_l_mode;
	assert (mode in valid_modes);
	if not (value_size in valid_sizes) then begin
	  assert ([value_size..extended_size] * valid_sizes <> []);
	  while not (value_size in valid_sizes) do
	    value_size := succ (value_size);
	  signed_value := true;
	end;
	assert (signed_value in valid_signs);
      end;
    end;

    (*  If the first operands is a quick mode operands, make sure that
	its value is between one and eight.  *)

    if quick_mode in attributes then
      assert (is_quick (operands [1]));

  end (* with *);
end (* validate_instruction *);
$PAGE simplify_instruction - in gen_inst
(*  SIMPLIFY INSTRUCTION attempts to do just that: perform special case analysis
    to replace this instruction with one faster and/or shorter.  *)

procedure simplify_instruction ( var instr : instruction );

var i : 1 .. 2;

begin
  with instr do begin

    (* Replace "CMP #0,X" bye "TST X". *)

    if ( opcode = cmp_im_opc ) and
       ( operands [1].cst_part.kind = absolute_sc ) and
       ( operands [1].cst_part.offset = 0 ) then begin
      operands [1] := descriptors [null_mode];
      opcode := tst_m_opc;
    end;

    (* Replace <0(An)> by <(An)>. *)

    for i := 1 to 2 do begin
      with operands [i], cst_part do begin
	if (mode = displacement_mode) and (kind = absolute_sc (offset = 0) then
	  mode := indirect_mode;
      end;
    end;

    (* Replace "MOVE #0,X" by "CLR X". *)

    if ( opcode = move_mm_opc) and
       ( operands [1].mode = immediate_mode ) and
       ( operands [1].cst_part.kind = absolute_sc ) and
       ( operands [1].cst_part.offset = 0 ) then begin
      operands [1] := descriptors [null_mode];
      opcode := clr_m_opc;
    end;

  end (* with instr *);
end (* simplify_instruction *);
$PAGE gen_inst - main routine
var instr : instruction;
    cr : code;

begin
  if condition <> t_cc then begin
    assert (operation in [bcc_opc, scc_opc, dbcc_opc]);
    if condition = f_cc then
      assert (operation <> bcc_opc);
  end;

  instr := ( translate_opcode (operation, source, destination), condition, nil,
	     (validate_operand (source), validate_operand (destination)) );

  validate_instruction (instr);

  simplify_instruction (instr);

  new (cr, inst_code);
  cr^.inst := instr;
  gen_emit (area, cr);
end (* gen_inst *);
$PAGE gen_opcode
(* GEN OPCODE emits an instruction which has no operands. *)

public procedure gen_opcode (
	opc: generic_opcodes );

begin
  gen_inst (code_area, opc, t_cc, descriptors[null_mode], descriptors[null_mode]);
end;
$PAGE gen_r, gen_rr
(* GEN R emits an instruction with a single operand in a register. *)

public procedure gen_r (
	opc: generic_opcodes;
	register: registers;
	operand_size: op_sizes );

begin
  gen_inst (code_area, opc, t_cc, descriptors[null_mode], reg_desc (register, operand_size, true));
end;

(* GEN RR emits an instruction with two register operands. *)

public procedure gen_rr (
	opc: generic_opcodes;
	source: registers;
	destination: registers;
	operand_size: op_sizes );

begin
  gen_inst (code_area, opc, t_cc, reg_desc (source, operand_size, true), reg_desc (destination, operand_size, true));
end;
$PAGE gen_m, gen_mm
(* GEN M emits an instruction with a single operand specified by a descriptor. *)

public procedure gen_m (
	opc: generic_opcodes;
	operand: op_desc );

begin
  gen_inst (code_area, opc, t_cc, descriptors[null_mode], operand);
end;

(* GEN MM emits an instruction with two operands specified by descriptors *)

public procedure gen_mm (
	opc: generic_opcodes;
	source: op_desc;
	destination: op_desc );

begin
  gen_inst (code_area, opc, t_cc, source, destination);
end;
$PAGE gen_rm, gen_mr
(* GEN RM emits an instruction whose first operand is in a register and whose
   second operand is specified by a descriptor. *)

public procedure gen_rm (
	opc: generic_opcodes;
	source: registers;
	destination: op_desc );

begin
  gen_inst (code_area, opc, t_cc, reg_desc (source, destination.value_size, destination.signed_value), destination);
end;

(* GEN MR emits an instruction whose source operand is specified by a descriptor
   and whose second operand is in a register. *)

public procedure gen_mr (
	opc: generic_opcodes;
	source: op_desc;
	destination: registers );

begin
  gen_inst (code_area, opc, t_cc, source, reg_desc (destination, source.value_size, source.signed_value));
end;
$PAGE gen_ir, gen_im
(* GEN IR emits an immediate instruction whose operand is in a register. *)

public procedure gen_ir (
	opc: generic_opcodes;
	value: integer;
	destination: registers;
	operand_size: op_sizes );

begin
  gen_inst (code_area, opc, t_cc, int_desc (value, no_size, false), reg_desc (destination, operand_size, true));
end;

(* GEN IM emits an immediate instruction whose operand is specified by a descriptor. *)

public procedure gen_im (
	opc: generic_opcodes;
	value: integer;
	destination: op_desc );

begin
  gen_inst (code_area, opc, t_cc, int_desc (value, no_size, false), destination);
end;
$PAGE gen_rt, gen_jump
(* GEN RT generates a call to a specified runtime entry point. *)

public procedure gen_rt (
	routine: rt_symbol );

var destination: op_desc;

begin
  destination := descriptors[abs_l_mode];
  destination.cst_part := rt_reloc (routine);
  gen_inst (code_area, jsr_opc, t_cc, descriptors[null_mode], destination);
end;

(* GEN JUMP emits a jump instruction to a specified location. *)

public procedure gen_jump (
	target: def );

var destination: op_desc;

begin
  assert (not (target^.kind in local_def_classes)); (* assumption about jmp vs. bcc usage *)
  destination := descriptors[abs_l_mode];
  destination.cst_part := def_reloc (target);
  gen_inst ( code_area, jmp_opc, t_cc, descriptors[null_mode], destination );
end;
$PAGE gen_bcc, gen_scc
(* GEN BCC emits a conditional branch to a specified location. *)

public procedure gen_bcc (
	condition: condition_codes;
	target: def );

var destination: op_desc;

begin
  assert (target^.kind in local_def_classes); (* assumption about bcc vs. jmp usage *)
  destination := descriptors[pc_displacement_mode];
  destination.cst_part := def_reloc (target);
  gen_inst (code_area, bcc_opc, condition, descriptors[null_mode], destination);
end;

(* GEN SCC emits a "set conditionally" instruction with an operand specified
   by a descriptor. *)

public procedure gen_scc (
	condition: condition_codes;
	destination: op_desc );

begin
  gen_inst (code_area, scc_opc, condition, descriptors[null_mode], destination);
end;
$PAGE gen_call

public procedure gen_call (target: def);

var destination : op_desc;

begin
  destination := descriptors[pc_displacement_mode];
  destination.cst_part := def_reloc (target);
  gen_inst (code_area, jsr_opc, t_cc, descriptors[null_mode], destination);
end;
$PAGE gen_dbcc
(* GEN DBCC emits a decrement and branch conditional instruction. *)

public procedure gen_dbcc (
	condition: condition_codes;
	register: registers;
	target: def );

var destination: op_desc;

begin
  destination := descriptors[pc_displacement_mode];
  destination.cst_part := def_reloc (target);
  gen_inst (code_area, dbcc_opc, condition, reg_desc (register, size_word, true), destination);
end;
$PAGE gen_byte, gen_word, gen_long
(* GEN BYTE emits a (relocatable) byte value in a specified area. *)

public procedure gen_byte (
	var area: code_list;
	value: reloc_value;
	relative: boolean );

var cr: code;

begin
  (* Gen_byte, word, and long have all been given the flexibility to utilize
     general relocatable values, as well as specifying pc-relative.  Especially
     in the case of a byte value, however, it would be SURPRISING to see anything
     other than simple absolute values. *)
  assert ((value.kind = absolute_sc) and not relative);

  new (cr, byte_code);
  with cr^ do begin
    pc_relative := relative;
    label_ref := nil;
    relval := value;
  end;
  gen_emit (area, cr);
end (* gen_byte *);

(* GEN WORD emits a (relocatable) word value in a specified area. *)

public procedure gen_word (
	var area: code_list;
	value: reloc_value;
	relative: boolean );

var cr: code;

begin
  (* Verify an assumed relation between the reloc_value and use of the pc-relative
     specification. *)
  assert (relative = ((value.kind = def_sc) andif (value.reldef^.kind in local_def_classes)) );

  new (cr, word_code);
  with cr^ do begin
    pc_relative := relative;
    label_ref := nil;
    relval := value;
  end;
  gen_emit (area, cr);
end (* gen_word *);


(* GEN LONG emits a (relocatable) longword value in a specified area. *)

public procedure gen_long (
	var area: code_list;
	value: reloc_value;
	relative: boolean );

var cr: code;

begin
  (* It would be surprising to see pc-relative specified on a longword.  Lets
     be sure to have a look at it if it ever occurs:  *)
  assert (not relative);

  new (cr, long_code);
  with cr^ do begin
    pc_relative := relative;
    label_ref := nil;
    relval := value;
  end;
  gen_emit (area, cr);
end (* gen_long *);
$PAGE gen_len, gen_string, gen_set
(* GEN LEN emits a string length word in a specified area. *)

public procedure gen_len (
	var area: code_list;
	len: integer );

var cr: code;

begin
  new (cr, len_code);
  cr^.lenval := len;
  gen_emit (area, cr);
end;

(* GEN STRING emits a string of characters in a specified area. *)

public procedure gen_string (
	var area: code_list;
	str: packed array[1..*] of char );

var cr: code;

begin
  new (cr, string_code, length (str));
  cr^.strval := str;
  gen_emit (area, cr);
end;

(* GEN SET emits a set constant in a specified area. *)

public procedure gen_set (
	var area: code_list;
	set_const: packed array[0..*] of boolean );

var cr: code;

begin
  assert ((dimension (set_const) mod 16) = 0);
  new (cr, set_code, upperbound (set_const));
  cr^.setval := set_const;
  gen_emit (area, cr);
end;
$PAGE gen_sreal, gen_dreal
(* GEN SREAL emits a single precision real constant in a specified area. *)

public procedure gen_sreal (
	var area: code_list;
	real_value: real_type );

var cr: code;

begin
  new (cr, sreal_code);
  cr^.realval := real_value;
  gen_emit (area, cr);
end;

(* GEN DREAL emits a double precision real constant in a specified area. *)

public procedure gen_dreal (
	var area: code_list;
	real_value: real_type );

var cr: code;

begin
  new (cr, dreal_code);
  cr^.realval := real_value;
  gen_emit (area, cr);
end;
$PAGE gen_def, gen_source
(* GEN DEF emits an definition in a specified area. *)

public procedure gen_def (
	var area: code_list;
	def_value: def );

var cr: code;

begin
  new (cr, def_code);
  cr^.defname := def_value;
  cr^.ref_list := nil;
  assert (not def_value^.defined);
  def_value^.label_code_rec := cr; (* ptr back to code rec. for cross-jumping *)
  gen_emit (area, cr);
end (* gen_def *);

(* GEN SOURCE emits a "source id" record in a specified area. *)

public procedure gen_source (
	var area: code_list;
	source_loc: source_id;
	statement_index: 0..63 );

var cr: code;

begin
  new (cr, source_code);
  cr^.stmtid := source_loc;
  cr^.stmtindex := statement_index;
  gen_emit (area, cr);
end (* gen_source *);
$PAGE gen_cmt, gen_asm_label
(* GEN CMT emits a comment for the assembly listing. *)

public procedure gen_cmt (
	var area: code_list;
	comment: packed array[1..*] of char );

var cr: code;

begin
  new (cr, comment_code, length (comment));
  cr^.cmtext := comment;
  gen_emit (area, cr);
end;

(* GEN ASM LABEL merely emits a label to appear on the assembly listing. *)

public procedure gen_asm_label (
	var area: code_list;
	name: packed array[1..8] of char;
	defined_by_pos: boolean;
	stat_offset: code_address );

var cr: code;

begin
  new (cr, asm_label_code);
  cr^.labtext := name;
  cr^.defd_by_pos := defined_by_pos;
  cr^.static_offset := stat_offset;
  gen_emit (area, cr);
end;
$PAGE gen_dtime
(* GEN DTIME emits a date and time record in a specified area. *)

public procedure gen_dtime (
	var area: code_list;
	dt: dtime_int );

var cr: code;

begin
  new (cr, dtime_code);
  cr^.dtimeval := dt;
  gen_emit (area, cr);
end;
$PAGE gen_block, gen_pc_assert, gen_align
(* GEN BLOCK reserves a block of storage in a specified area. *)

public procedure gen_block (
	var area: code_list;
	size: unit_range );

var cr: code;

begin
  assert (size >= 0);
  new (cr, block_code);
  cr^.blocksize := size;
  gen_emit (area, cr);
end;

(* GEN PC ASSERT emits a record in a specified area to insure the value of the
   "location counter" within that area. *)

public procedure gen_pc_assert (
	var area: code_list;
	location: code_address );

var cr: code;

begin
  new (cr, pc_assert_code);
  cr^.loc := location;
  gen_emit (area, cr);
end;

(* GEN ALIGN emits a record in a specified area to force the word alignment
   of the next generated data. *)

public procedure gen_align (
	var area: code_list );

var cr: code;

begin
  new (cr, align_code);
  gen_emit (area, cr);
end;
$PAGE def_init
(* DEF INIT initializes the data structures used in the manipulation of
   defintion records. *)

public procedure def_init;

var class: def_class;

begin
  for class := minimum (def_class) to maximum (def_class) do begin
    def_lists[class] := nil;
    def_num[class] := 1;
  end;
end;
$PAGE def_delete, def_purge, def_term
(* DEF DELETE deletes any allocated definition records for a given class
   and reinitializes the data structures for that class. *)

procedure def_delete (
	class: def_class );

var d1, d2: def;

begin
  d1 := def_lists[class];
  while d1 <> nil do begin
    d2 := d1^.next;

    (* It is an error to delete an undefined definition with unresolved references. *)

    assert (d1^.defined or (d1^.fixup_list = nil));
    dispose (d1);
    d1 := d2;
  end;
  def_lists[class] := nil;
  def_num[class] := 1;
end;

(* DEF PURGE deletes the definition records for those classes which are
   renumbered for each routine. *)

public procedure def_purge;

var class: def_class;

begin
  for class := minimum (def_class) to maximum (def_class) do
    if class in local_def_classes then
      def_delete (class);
end;

(* DEF TERM deletes the definition records allocated for ALL classes. *)

public procedure def_term;

var class: def_class;

begin
  for class := minimum (def_class) to maximum (def_class) do
    def_delete (class);
end;
$PAGE def_create
(* DEF CREATE allocates and initializes a definition record for a given
   class. *)

public function def_create (
	class: def_class ): def;

begin
  assert (not (class in indexed_def_classes));
  new (def_create);
  def_create^ := (def_lists[class], class, def_num[class], false, nil, nil, -1);
  def_lists[class] := def_create;
  def_num[class] := def_num[class] + 1;
end;
$PAGE def_lookup
(* DEF LOOKUP searches the list of definitions in a given class for one with
   a given index, returning it if found and creating one to return if not. *)

public function def_lookup (
	class: def_class;
	index: id_range ): def;

begin
  assert (class in indexed_def_classes);
  def_lookup := def_lists[class];
  while def_lookup <> nil do begin
    if def_lookup^.index = index then
      return;
    def_lookup := def_lookup^.next;
  end;
  new (def_lookup);
  def_lookup^ := (def_lists[class], class, index, false, nil, nil, -1);
  def_lists[class] := def_lookup;
end;
$PAGE def_assign
(* DEF ASSIGN sets the value of a definition to a specified relocatable value. *)

public procedure def_assign (
	defn: def;
	val: reloc_value );

var
  f1, f2: fixup;

begin
  with defn^ do begin
    assert (not defined);
    f1 := fixup_list;
    while f1 <> nil do with f1^ do begin	(* emit fixup records as necessary *)
      f2 := next;
      emt_fixup (fixup_addr, fixup_long, val, patch_value);
      dispose (f1);
      f1 := f2;
    end;
    fixup_list := nil;
    defined := true;
    defval := val;
  end;
end;
$PAGE def_forward
(* DEF FORWARD adds a fixup record for the specified definition. *)

public procedure def_forward (
	defn: def;
	loc: integer;
	long: boolean;
	patch: integer );

var f: fixup;

begin
  assert (not defn^.defined);
  new (f);
  f^ := (defn^.fixup_list, long, loc, patch);
  defn^.fixup_list := f;
end;
$PAGE gen_vnode
(* GEN VNODE generates a sequence of code records in the specified area to
   represent the value of an arbitrarily complex constant.  *)

public procedure gen_vnode (var area_list: code_list; value: val_ptr; values_type: typ);
$PAGE gen_component
(* GEN COMPONENT generates a sequence of code records to represent the given
   value.  For simple objects, it does this directly.  Aggregates are broken
   down into their components, in which case this routine may recurse.  BASE_WIDTH
   is the width in bits of the constant represented by VALUE, so that the value
   can be generated with size appropriate to the context, i.e. packed or not.  *)

procedure gen_component (value: val_ptr;
			 required_width: bit_range);



  (* GEN SUBVAL generates a value for an array element or a record field, given
     a val record and required width.  If the value isn't a simple scalar or
     pointer constant, GEN_COMPONENT is called recursively.  *)

  procedure gen_subval (val_record: val; required_width: bit_range);

    begin
      case val_record.kind of

	scalar_cst:
	  case required_width of
	    bits_per_byte: gen_byte (area_list, (val_record.ival, absolute_sc), false);
	    bits_per_word: gen_word (area_list, (val_record.ival, absolute_sc), false);
	    bits_per_long: gen_long (area_list, (val_record.ival, absolute_sc), false)
	  end;

	real_cst,
	string_cst,
	set_cst,
	array_cst,
	record_cst:
	  gen_component (val_record.valp, required_width);

	ptr_cst: begin
	  gen_long (area_list, (int_nil, absolute_sc), false);
	  assert (required_width = bits_per_long)
	end;

      end
    end (* gen_subval *);
$PAGE find_variant - in gen_vnode
(* FIND VARIANT is given a type node of kind RECORDS or VARIANTS and
   an integer representing a tag field value.  It returns the type node
   of the variant corresponding to the tag value.  NIL is returned if no
   such variant is found.  The record or variant whose type is passed
   in must contain a variant part. *)

function find_variant (recvar: typ; tag_val: integer): typ;

var
  cur_variant: typ;

begin
  find_variant := nil;				(* updated if variant found or
						   if others case found *)
  cur_variant := recvar^.variant_tag^.first_variant;
  while cur_variant <> nil do begin
    with cur_variant^ do begin
      if others_var 
        then find_variant := cur_variant
      else if (minlab <= tag_val) and (tag_val <= maxlab)
        then begin
          find_variant := cur_variant;
          return				(* <--- exit here if specific variant found *)
        end;
      cur_variant := next_variant
    end (* with *);
  end (* while *);
end;
$PAGE gen_component - body

var
  i, padding, width_generated: bit_range;
  set_array: ^packed array [0..*] of boolean;
  cur_recvar: typ;
  field_sym: sym;
  field_number: unit_range;

begin
  with value^ do
    case kind of

      scalar_cst:
	case required_width of
	  bits_per_byte: gen_byte (area_list, (scalar_val, absolute_sc), false);
	  bits_per_word: gen_word (area_list, (scalar_val, absolute_sc), false);
	  bits_per_long: gen_long (area_list, (scalar_val, absolute_sc), false)
	end;

      real_cst:
	if real_prec > srealprec then begin
	  gen_dreal (area_list, real_val);
	  assert (required_width = bits_per_double)
        end
	else begin
	  gen_sreal (area_list, real_val);
	  assert (required_width = bits_per_float)
	end;

      string_cst: begin
	padding := required_width - char_size * length (str_val);
	if str_varying_ref then begin
	  gen_len (area_list, length (str_val));
	  padding := padding - str_lw_width
	end;
	gen_string (area_list, str_val);
	gen_block (area_list, padding div bits_per_byte)
      end;

      set_cst: begin
	new (set_array, required_width - 1);
	if required_width > 0 then begin
	  padding := set_origin mod bits_per_word; (* number of leading pad bits *)
	  for i := 0 to (padding) - 1 do
	    set_array^[i] := false;
	  for i := padding to upperbound (set_val) + padding do
	    set_array^[i] := set_val [i - padding];
	  for i := dimension (set_val) + padding to required_width - 1 do
	    set_array^[i] := false
	end;
	gen_set (area_list, set_array^)
      end;

      array_cst:
	with struc_type^ do begin
	  gen_align (area_list);
	  padding := required_width;
	  for i := 1 to min (upperbound (elem_vals),
			     index_type^.maxval - index_type^.minval + 1) do begin
	    gen_subval (elem_vals [i], element_size);
	    padding := padding - element_size
	  end;
	  if padding > 0 then
	    gen_block (area_list, padding div bits_per_byte)
	end;

      record_cst: begin
	gen_align (area_list);
        (* Initialize pointer to record/variant whose fields are currently
	   being traversed, and pointer to do the traversing. *)
	cur_recvar := struc_type;
	field_sym := struc_type^.field_list;
	field_number := 0; (* position in elem_vals array *)
	width_generated := 0; (* in case we have to pad out *)

	(* Traverse the fields of the record, descending into the variants,
	   subvariants, etc., as required according to the provided tag values. *)

	repeat

	  (* If starting new variant, then find correct variant type record
	     and its first field. *)

	  while ( (cur_recvar <> nil) andif
	          (cur_recvar^.field_list = nil) andif
		  (cur_recvar^.variant_tag <> nil) ) orif (* no fixed part, but there is a variant? *)
		( (field_sym <> nil) andif
		  (cur_recvar <> field_sym^.fld_variant) ) do begin (* gone off end of current variant? *)
	    field_number := field_number + 1; (* advance to the tag value's pos in elem_vals *)
	   exit if field_number > upperbound (elem_vals); (* not enough values provided? *)
	    cur_recvar := find_variant (cur_recvar, elem_vals[field_number].ival);
	    if cur_recvar <> nil then
	      field_sym := cur_recvar^.field_list
	    else
	      field_sym := nil;
	    (* Make sure we have first of the variant records labelling this
	       variant, otherwise tests for end-of-variant won't work.  *)
	    if field_sym <> nil then cur_recvar := field_sym^.fld_variant
	  end;
	 
	 exit if field_sym = nil;

	  (* Process the current field. *)

	  field_number := field_number + 1;
	  if field_number <= upperbound (elem_vals) then begin

	    (* Code to support constant records containing flex strings,
	       and/or flex arrays *)

	    with field_sym^.type_desc^ do
	      if flexible and (kind in [arrays, strings]) then
		with value^.elem_vals [ field_number ].valp^ do
		  if kind = array_cst then
		    gen_long (area_list, (struc_type^.index_type^.maxval, absolute_sc), false)
		  else
		    gen_word (area_list, (length (str_val), absolute_sc), false);

	    assert (field_sym^.fld_number = field_number);
	    gen_subval (value^.elem_vals[field_number], field_sym^.fld_width);
	    width_generated := field_sym^.fld_offset + field_sym^.fld_width
	  end;
	  (* If we just generated a tag field, then back off the field_number by
	     one.  Note that a tag may not have an actual field.  The loop (up
	     above) that will take the tag's value and find the matching variant
	     must work in either case, and this is the nicest way to accomplish that. *)
	  if (cur_recvar^.variant_tag <> nil) andif
		    (field_sym = cur_recvar^.variant_tag^.tag_field) then
	    field_number := field_number - 1;

	  (* Get next field and check whether or not we are done. *)

	  field_sym := field_sym^.next;
	  if (field_sym <> nil) andif
	     (field_sym^.fld_variant <> cur_recvar) (* gone off end of current variant? *)
		      andif
	     (cur_recvar^.variant_tag = nil) then (* and no subvariant to descend into? *)
	    field_sym := nil (* then quit! *)
	until field_sym = nil;
	
	gen_block (area_list, (required_width - width_generated) div bits_per_byte)
      end
    end
end (* gen_component *);
$PAGE bits_required in gen_vnode
(* BITS REQUIRED returns the size in bits of the constant described by "value".  *)

function bits_required (value: val_ptr; values_type: typ): bit_range;

  begin
    case value^.kind of

      scalar_cst:
	if values_type^.kind in [bools, chars] then
	  bits_required := bits_per_byte
	else if values_type^.kind = scalars then begin
	  if values_type^.base_type^.base_size <= bits_per_byte then
	    bits_required := bits_per_byte
	  else if values_type^.base_type^.base_size <= bits_per_word then
	    bits_required := bits_per_word
	  else
	    assert (false) (* should never required longword *)
	end
	else if values_type^.packable then begin
	  assert (values_type^.kind = ints);
	  if values_type^.base_size <= bits_per_byte then
	    bits_required := bits_per_byte
	  else if values_type^.base_size <= bits_per_word then
	    bits_required := bits_per_word
	  else
	    bits_required := bits_per_integer
	end
	else (* ints, but not packable *)
	  bits_required := bits_per_integer;

      real_cst:
	if value^.real_prec > srealprec  then
	  bits_required := bits_per_double
	else
	  bits_required := bits_per_float;

      string_cst:
	begin
	  bits_required := ngm (length (value^.str_val) * char_size, bits_per_word);
	  if value^.str_varying_ref then
	    bits_required := bits_required + str_lw_width
	end;

      set_cst:
	if dimension (value^.set_val) = 0 then
	  bits_required := 0
	else
	  bits_required := ngm (dimension (value^.set_val) + value^.set_origin mod bits_per_word,
				bits_per_word);

      array_cst,
      record_cst:
	bits_required := value^.struc_type^.base_size

    end
  end (* bits_required *);
$PAGE gen_vnode - body

begin
  gen_component (value, bits_required (value, values_type))
end (* gen_vnode *);
$PAGE gen_val
(* GEN VAL generates a sequence of code records in the specified area to
   represent the given value.  It is similar to GEN_VNODE, except that
   it takes a VAL record rather than a VAL_PTR.  *)

public procedure gen_val (var area_list: code_list; value: val; values_type: typ);

var
  datum_width: bit_range;
  datum_alignment: align_range;

begin
  case value.kind of

    scalar_cst: begin
      alc_data (values_type, datum_width, datum_alignment); (* ask the type allocation module *)
      if datum_alignment > bits_per_byte then
	gen_align (area_list);
      if datum_width <= bits_per_byte then
	gen_byte (area_list, (value.ival, absolute_sc), false)
      else if datum_width <= bits_per_word then
	gen_word (area_list, (value.ival, absolute_sc), false)
      else
	gen_long (area_list, (value.ival, absolute_sc), false)
    end;

    real_cst,
    string_cst,
    set_cst,
    array_cst,
    record_cst:
      gen_vnode (area_list, value.valp, values_type);

    ptr_cst:
      gen_long (area_list, (int_nil, absolute_sc), false)
  end
end (* gen_val *);
$PAGE gen_cnode
(* GEN CNODE allocates the specified value in the constant area, and
   returns a DEF_SC reloc value referring to the location of the constant.  *)

public function gen_cnode (value: val_ptr; values_type: typ): reloc_value
							      options special (coercions);

begin
  if value^.def_addr = nil then begin
    gen_cnode := def_reloc (def_create (const_def));
    gen_def (cst_area, gen_cnode.reldef); (* define the label's value *)
    gen_vnode (cst_area, value, values_type); (* generate the content of the constant *)
    value^.def_addr := val_ptr (gen_cnode.reldef) (* remember for later users *)
  end
  else
    gen_cnode := def_reloc (def (value^.def_addr)) (* already generated *)
end;
$PAGE gen_cval
(* GEN CVAL allocates the specified value in the constant area, and
   returns a DEF_SC reloc value referring to the location of the constant.
   This routine is similar to GEN CNODE, except that it takes a VAL
   record rather than a VAL_PTR.  *)

public function gen_cval (value: val; values_type: typ): reloc_value
							   options special (coercions);

const
  has_ptr: set of value_kind = [real_cst, string_cst, set_cst, array_cst, record_cst];

begin

  (* If previously generated, pick up DEF saved at that time. *)

  if value.kind = alloc_cst then
    gen_cval := def_reloc (def (value.defp)) (* previously generated *)
  else if (value.kind in has_ptr) andif (value.valp^.def_addr <> nil) then
    gen_cval := def_reloc (def (value.valp^.def_addr)) (* previously generated *)

  (* Generate it now. *)

  else begin
    gen_cval := def_reloc (def_create (const_def));
    gen_def (cst_area, gen_cval.reldef); (* define the label's value *)
    gen_val (cst_area, value, values_type); (* generate the content of the constant *)
    if value.kind in has_ptr then
      value.valp^.def_addr := val_ptr (gen_cval.reldef) (* remember for later users *)
  end
end (* gen_cval *);
$PAGE gen_cst
(* GEN CST generates a scalar value in the constant area and returns an
   op_desc for its location.  The size of the field in which the constant
   is generated is specified by parameter SIZE.  *)

public function gen_cst (value: integer; size: op_sizes): op_desc;

begin
  gen_cst := def_descriptor; (* pc relative, with def_sc relocation *)
  gen_cst.cst_part.reldef := def_create (const_def);
  gen_def (cst_area, gen_cst.cst_part.reldef); (* define the label's value *)

  assert (size in reg_sizes); (* thats all we're prepared for here *)
  gen_cst.value_size := size;

  (* Generate the data. *)

  if size = size_byte then
    gen_byte (cst_area, (value, absolute_sc), false)
  else if size = size_word then
    gen_word (cst_area, (value, absolute_sc), false)
  else
    gen_long (cst_area, (value, absolute_sc), false)
end (* gen_cst *).
z:IÇ