$WIDTH=100
$LENGTH=55
$TITLE Q10GEN.PAS, last modified 1/18/84, zw
MODULE q10gen OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- PDP10 quick code generator*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PTMCON.INC
$SYSTEM PASFIL.INC
$SYSTEM PASIST.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM P10CGU.INC
$SYSTEM P10MAC.INC
$SYSTEM P10OPC.INC
$SYSTEM PASIFU.INC
$SYSTEM Q10EXP.INC
$SYSTEM Q10CLL.INC
$SYSTEM Q10CMP.INC
$SYSTEM P10REL.INC
$SYSTEM PASMAP.INC
$SYSTEM P10CSP.INC
$SYSTEM P10DEB.INC
$SYSTEM PASJMP.INC
$SYSTEM PASTAL.INC
$SYSTEM UTLSW.INC
$SYSTEM PA2DMP.INC
$SYSTEM PASDMP.INC
$SYSTEM Q10SET.INC
$SYSTEM Q10DSC.TYP
$SYSTEM Q10DSC.INC
$SYSTEM PASAST.INC

$PAGE definitions

PUBLIC VAR
  low_base: def; (* relocatable 0 (static area) *)
  high_base: def; (* relocatable 400000b (code area) *)
  prg_blk_def: def;     (* location of program block in debug *)
  reset_needed: boolean;        (* true while pending dynamic temps *)

CONST
  prg_blk_size := 6;    (* 5 words data + module word *)

STATIC VAR
  temp_start: unit_range;       (* start of temps in local stack frame *)
  temp_base: unit_range;        (* top of temps in local stack frame *)
  temp_max: unit_range;         (* max temp location used *)
  save17addr: addr_desc;        (* addr of saved 17 for free of dynamic temps *)
  save17loc: def;               (* for definition of offset *)
  save17inst: code;             (* instruction which does the save *)
  hsb_addr: addr_desc;	(* descriptor for statically allocated temporary area in 
			   block's stack frame where handler state blocks will be located. *)
  overlaid: boolean; (* true if compilation under /OVERLAY *)

$PAGE get_temp
(* GET TEMP allocates static temporaries in the current stack frame.  These temps
   have a duration of one source statement - KILL TEMPS will release the space
   allocated here when the next start_statement tuple is encountered.  *)

public function get_temp (temp_size: unit_range): addr_desc;
begin
  get_temp := temp_reference;
  get_temp.index := sp;
  get_temp.offset := temp_base;
  temp_base := temp_base + temp_size;
  assert (temp_base <= 777777b); (* must fit into stack_end field of block node *)
  temp_max := max (temp_max, temp_base);
end;
$PAGE kill_temps
public procedure kill_temps;
begin
  temp_base := temp_start;
end;
$PAGE reset_stack, bb_end

(* RESET STACK is called to free dynamic temporaries by restoring the
   saved stack pointer. *)

procedure reset_stack;

begin
  if save17loc = nil then begin (* first use *)
    save17loc := make_def (local_def);
    save17addr.reloc.reldef := save17loc;
    save17inst^.reloc.reldef := save17loc;
  end;
  gen_rm (move, sb, save17addr);
  reset_needed := false;
end;

(* BB END determines if a stack reset is necessary and, if so, calls
   reset_stack to perform it. *)

public procedure bb_end;

begin
  if reset_needed then
    reset_stack;
end;
$PAGE emit_code
(* EMIT CODE takes a code list.  It will write it to the rel file, and to
   the macro listing if assembly_opt is in the options_list parameter. *)

public procedure emit_code ( var area_list: code_list; (* the code area to be written *)
		      var ic: unit_range; (* the address to write the code at *)
		      options_list: set_of_options ); (* to control listing *)

 begin
  if assembly_opt in options_list then
    mac_list (area_list, ic );
  wr_code (area_list, ic, true);
 end;
$PAGE init_static
(* INIT STATIC generates code for the initialized part of the static area, and
   for the declared public constants.  Symbols are processed in the order:
	Symbols within the id list of a block
	Blocks in downward_call_thread order
   This is the order in which they are processed during storage allocation. *)

procedure init_static  options special(coercions);

 var
   block: blk;
   symbols: sym;
   reld: rel_syllable;
   cr: code;

 begin
  gen_origin (static_area, loc_static);

  gen_asm_label (static_area, 'STATC.');
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
	with symbols^ do begin
	  if (kind = vars) andif (dcl_class = static_sc) andif
	     (init_value.kind <> no_value) then begin
	    gen_origin (static_area, item_addr);
	    gen_val (static_area, init_value);
	  end
	  else if (kind = consts) andif (init_value.kind <> subr_cst) andif
		  ( public_dcl or
		    ( prog_options.debug_opt and
		      (dcl_class <> external_sc) ) ) then begin
	    reld := gen_cval (init_value);
	    init_value.kind := alloc_cst;
	    init_value.defp := val_ptr (reld.reldef);
	  end
	  else if (kind = conditions) andif (dcl_class = static_sc) then begin
	    gen_origin (static_area, item_addr + size_init + size_uninit);
	    new (cr, halfwords);
	    with cr^ do begin
	      xwd.lh := 0;
	      xwd.rh := 0;
	      lreloc := none;
	      rreloc.kind := static_sc;
	      rreloc.relsym := symbols (* word's own address *)
	    end;
	    gen_emit (static_area, cr)
	  end;
	  symbols := next;
	end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  gen_origin (static_area, loc_static + size_init); (* start of uninit. static *)
  emit_code (static_area, loc_static, prog_options.semantic_options);
  if (size_uninit > 0) and (assembly_opt in prog_options.semantic_options) then
    mac_pad (size_uninit) (* put BLOCK for uninit. static into macro listing *)
 end;
$PAGE store
(* STORE moves the contents of a register or register pair ("reg") into
   a memory location ("maddr").  The precision "p" denotes whether a single
   register or a pair is to be moved.  "Alignment" gives the data alignment
   requirements of the value in the register;  it is assumed to be compatible
   with that of the target. *)

public procedure store (reg: registers; maddr: addr_desc; p: bit_range; alignment: data_alignment);
 begin
  case maddr.mode of
    fw:     if p > 36
	      then gen_rm (dmovem, reg, maddr)
	      else gen_rm (movem, reg, maddr);

    lhw:    if alignment = left_aligned
	      then gen_rm (hllm, reg, maddr)
	      else gen_rm (hrlm, reg, maddr);

    rhw:    if alignment = left_aligned
	      then gen_rm (hlrm, reg, maddr)
	      else gen_rm (hrrm, reg, maddr);

    byte:   begin
	      if alignment = left_aligned then begin
		right_shift (reg, 36, 36 - maddr.slice_size);
	      end;
	      gen_rm (dpb, reg, maddr);
	    end;

    slice:  begin
	      if alignment = left_aligned then begin
		right_shift (reg, 36, 36 - maddr.slice_size);
	      end;
	      gen (dpb, reg, 0, 0, gen_bptr (maddr));
	    end
  end;
 end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   r: registers;
   laddr, raddr: addr_desc;
   scalar_val: int_type;
   opc: opc_range;

 begin
  with tpl^ do begin
    raddr := fetch (rhs);
    laddr := fetch (lhs);
    if (laddr.mode in [fw, lhw, rhw]) andif aconstp (raddr, scalar_val) andif
      ((scalar_val = 0) orif (laddr.mode = fw) andif (scalar_val = -1)
	orif (laddr.mode <> fw) andif (scalar_val mod 1000000b = 777777b)) then begin
	case laddr.mode of
	  fw:
	    if scalar_val = 0
	      then opc := setzm
	      else opc := setom;
	  lhw:
	    if scalar_val = 0
	      then opc := hrrzs
	      else opc := hrros;
	  rhw:
	    if scalar_val = 0
	      then opc := hllzs
	      else opc := hllos
	end;
	gen_rm (opc, 0, laddr);
	free (raddr);
    end
    else begin
      r := load_addr (raddr, alignment (rhs), rhs^.desc.int_prec);
      store (r, laddr, 36, signed_value);
      decr_reg_usages (r);
    end;
    free (laddr);
  end (* with *) ;
 end;
$PAGE real_assignment
(* REAL ASSIGNMENT generates code for assignments of real values. *)

procedure real_assignment (tpl: tuple);

 var
   r: registers;
   laddr, raddr: addr_desc;
   regsize: bit_range;

 begin
  with tpl^ do begin
    raddr := fetch (rhs);
    laddr := fetch (lhs);
    if rhs^.desc.precision > srealprec
      then regsize := 72
      else regsize := 36;
    r := load_addr (raddr, right_aligned, regsize);
    store (r, laddr, regsize, right_aligned);
    free (laddr);
    decr_reg_usages (r);
  end (* with *) ;
 end;
$PAGE proc_func_assignment

procedure proc_func_assignment (target, source: tuple);

var
  reg: registers;
  taddr: addr_desc;

begin
  pf_access (source, reg, false); (* get address and frame pointer *)
  taddr := fetch (target);
  gen_rm (movem, reg, taddr);
  free (taddr);
  decr_reg_usages (reg);
end;
$PAGE do_blt
(* DO BLT generates the code necessary to move multiple memory words with a BLT instruction.
   It takes addr_descs representing the source and destination addresses for the move, and an
   addr_desc representing the number of words to be moved.  DO BLT will free all three of these
   addr_descs.  It also assumes that that the width addr_desc is either immediate, or the
   value is in a register. *)

(* CST ADDR returns true if the address is known at compile time. *)

public function cst_addr (mem: addr_desc): boolean;
  begin
    with mem do
      cst_addr := (reloc.kind = static_sc) andif (not indirect) andif (index = noreg)
  end;

public procedure do_blt (src_addr, dest_addr, width_addr: addr_desc);

var 
  r, r1: registers;
  temp_addr: addr_desc;

begin
  if width_addr.immediate then begin
    if width_addr.offset = 0 then begin
      free (width_addr);
      free (src_addr);
      free (dest_addr);
      return (* <----  no code for zero-length copy *)
    end;
  end
  else
    assert (is_register (width_addr));

  (*  If both the source and destination addresses are known at compile time, the BLT
      control word may be generated as a constant and loaded.  Otherwise, it must be constructed. *)

  if not (cst_addr (dest_addr) orif (width_addr.immediate and not dest_addr.indirect) ) then
    free (dest_addr);  (* only case 2 below would still need it *)
  r := get_reg (36);
  if cst_addr (dest_addr) andif cst_addr (src_addr) then
    gen (move, r, 0, 0, gen_blt (src_addr.offset, src_addr.reloc,
				 dest_addr.offset, dest_addr.reloc))
  else begin
    gen_rm (movei, r, dest_addr);
    gen_rm (hrli, r, src_addr)  
  end;
  free (src_addr);

  (*  Once the BLT control word is in register r, several different code
      sequences may be generated:

      (1) If the length and the destination address are both known (at compile time):
	    BLT    r, dest+len-1

      (2) If the length is known, and the destination address is "nd(rd)":
	    BLT  r, nd+len-1(rd)

      (3) If the length is known, and the destination address is totally unknown:
	    HRRZI  r1, 0(r)
	    BLT    r, len-1(r1)

      (4) If neither the length nor the destination address is known:
	    JUMPLE len_reg, .+3
	    ADDI   len_reg, -1(r)
	    BLT    r, 0(len_reg)                                                *)

  if cst_addr (dest_addr) orif
	   (width_addr.immediate and not dest_addr.indirect) then begin (* cases 1 and 2 *)
    temp_addr := dest_addr;
    temp_addr.offset := temp_addr.offset + width_addr.offset - 1;
    gen_rm (blt, r, temp_addr);
    free (dest_addr)
  end
  else if width_addr.immediate (* and desc_addr.indirect *) then begin (* case 3 *)
    r1 := get_reg (36);
    gen_rx (hrrzi, r1, r);
    decr_reg_usages (r1);
    gen (blt, r, r1, width_addr.offset - 1, none)
  end
  else begin
    r1 := width_addr.offset;
    gen (jump+lec, r1, 0, 3, dot); (* case 4 *)
    gen (addi, r1, r, -1, none);
    gen_rx (blt, r, r1)
  end;

  decr_reg_usages (r);
  free (width_addr)
end;
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code for assignments of array and record values. *)

procedure agg_assignment (tpl: tuple);

var
  raddr, laddr,
  best_addr, width_addr, temp_addr: addr_desc;
  reg: registers;
  field: sym;
  best_side: expr;
  elem_size: bit_range;
  const_lwb, field_offset: unit_range;

procedure calc_width (agg_type: typ);
  var
    a, b, c, d: unit_range;
    grotesque: boolean;
  begin
    size_of (agg_type, false, a,b,c,d, grotesque);
    assert (not grotesque);
    if a <> 1 then
      gen_ri (imuli, reg, a);
    if b > 0 then
      gen_ri (addi, reg, b)
    else if b < 0 then
      gen_ri (subi, reg, -b);
    if c <> 1 then begin
      reg := coerce_to_double (reg_addr (reg), signed_value);
      gen_ri (idivi, reg, c);
      free_and_disassociate (reg + 1)
    end;
  end;

procedure move_it (laddr, raddr, width_addr: addr_desc);
  begin
    if width_addr.immediate andif (width_addr.offset <= 2) then begin
      free (raddr);
      free (width_addr);
      if width_addr.offset = 1 then begin
	reg := get_reg (36);
	gen_rm (move, reg, raddr);
	gen_rm (movem, reg, laddr);
	decr_reg_usages (reg)
      end
      else if width_addr.offset = 2 then begin
	reg := get_reg (72);
	gen_rm (dmove, reg, raddr);
	gen_rm (dmovem, reg, laddr);
	decr_reg_usages (reg)
      end;
      free (laddr)
    end
    else (* width is greater than 2 words, or isn't known *)
      do_blt (raddr, laddr, width_addr)
  end;

begin
  with tpl^ do
    if rhs^.opcode = func_call_op then
      if not overlaps then
	pas_call (rhs, expr_return_value, lhs, temp_addr)
      else begin
	temp_addr := get_temp ((rhs^.desc.base^.base_size + 35) div 36);
	pas_call (rhs, addr_return_value, nil, temp_addr);
	laddr := fetch (lhs);
	width_addr := int_value ((rhs^.desc.base^.base_size + 35) div 36);
	move_it (laddr, temp_addr, width_addr)
      end

    else (* rhs not a function call *) begin
      raddr := fetch (rhs);
      laddr := fetch (lhs);

      (* determine number of words to be copied, taking advantage of fact that both
	 sides must have the same length.  *)

      if not rhs^.desc.base^.flexible then (* rhs has fixed size *)
	width_addr := int_value ((rhs^.desc.base^.base_size + 35) div 36)
      else if not lhs^.desc.base^.flexible then (* lhs has fixed size *)
	width_addr := int_value ((lhs^.desc.base^.base_size + 35) div 36)

      else if rhs^.desc.kind = records then begin (* (flex) record assignment *)
	field := lhs^.desc.base^.field_list;
	while field^.next <> nil do
	  field := field^.next;
	field_offset := field^.fld_offset div 36;
	reg := load_addr (increment_addr (duplicate_addr (laddr), field_offset, 0, 36),
			  unsigned_value, 36);
	calc_width (lhs^.desc.base);
	width_addr := reg_addr (reg)
      end 

      else (* rhs^.desc.kind = arrays *) begin (* (flex or generic) array assignment *)
	if not rhs^.desc.base^.generic then begin
	  best_side := rhs;  (* flex *)
	  best_addr := raddr
	end
	else begin
	  best_side := lhs;  (* flex or generic *)
	  best_addr := laddr
	end;
	reg := load_addr ( upper_bound (best_side, best_addr), unsigned_value, 36);
	if best_side^.desc.base^.generic then 
	  gen_rm (sub, reg, lower_bound (best_side, best_addr));
	calc_width (best_side^.desc.base);
	width_addr := reg_addr (reg)
      end; 

      if rhs^.desc.kind = arrays then begin
	if dynamic_flex (lhs) then
	  laddr := increment_addr (laddr, 1, 0, 36);
	if dynamic_flex (rhs) then
	  raddr := increment_addr (raddr, 1, 0, 36)
      end;

      move_it (laddr, raddr, width_addr)
    end

end (* agg_assignment *);
$PAGE expand_agg_constructor
(* EXPAND_AGG_CONSTRUCTOR inserts tuples after an agg_val expr tuple to
   assign the given fields or elements into a statically allocated temporary.
   The result field of the agg_val is left pointing to a copy of the addr_desc 
   for the temp (left on the heap by FETCH).  When the agg_val is eventually
   fetched, the temp's addr_desc will be returned.  The temp will have the desired
   contents at runtime, as the assign_op stmt tuples emitted by this routine will
   precede the reference to the agg_val, and therefore the code generated by
   those assign_ops will be executed before the code for the agg_val's user.  *)


procedure expand_agg_constructor (var agg_val_tpl: expr);

var
  previous_tpl,
  alc_temp_tpl,
  component_tpl,
  index_tpl,
  lhs_upb_tpl, rhs_upb_tpl: expr;
  temp_addr: addr_desc;
  i: oper_range;
  index_type,
  comp_type,
  cur_var,
  var_list: typ;
  tag_val: int_type;
  fields,
  cur_tag: sym;

procedure init_expr (var node: expr; node_type: typ);

  begin
    with node^ do begin
      desc.base := node_type;
      with node_type^ do begin
	desc.kind := kind;
	case kind of
	  bools, chars, ints, scalars: begin
	    desc.signed := minval < 0;
	    desc.int_prec := base_size
	  end;
	  reals:
	    desc.precision := precision;
	  strings: begin
	    desc.str_kind := str_kind;
	    desc.str_length := str_length;
	    desc.str_flex := flexible
	  end;
	  sets: begin
	    desc.set_cst_lwb := true;
	    desc.set_lwb := set_element_type^.minval;
	    desc.set_cst_len := true;
	    desc.set_length := base_size
	  end
	end
      end;
      result := nil;
      ref_fre := 0;
      usage_count := 1
    end
  end (* init_expr *);

procedure emit_assignment (lhs_tpl, rhs_tpl: expr);
  var
    assign_tpl: tuple;
  begin
    new (assign_tpl, assign_op);
    with assign_tpl^ do begin
      must_store := true;
      lrecursive := false;
      rrecursive := false;
      overlaps := false;
      lhs := lhs_tpl;
      rhs := rhs_tpl
    end;
    emit (assign_tpl)
  end;

begin (* expand_agg_constructor *)
  t_set (agg_val_tpl); (* tuples will be inserted after the agg_val *)

  new (alc_temp_tpl, alc_temp_op, alc_temp_op,0); (* tuple to allocate target temp *)
  with alc_temp_tpl^ do begin
    desc := agg_val_tpl^.desc;
    result := nil;
    ref_fre := 0;
    usage_count := upperbound (agg_val_tpl^.operand) + 2 (* n opers + agg_val + below *)
  end;
  emit (alc_temp_tpl); (* chain tuple into I/F *)
  temp_addr := fetch (alc_temp_tpl); (* let fetch obtain addr_desc for temp,
					and put copy on heap *)
  agg_val_tpl^.result := alc_temp_tpl^.t; (* this "invisible" connection won't show in I/F dump *)

  if agg_val_tpl^.desc.kind = arrays then begin
    index_type := agg_val_tpl^.desc.base^.index_type;
    comp_type := agg_val_tpl^.desc.base^.element_type;
    for i := 1 to upperbound (agg_val_tpl^.operand) do begin
      new (index_tpl, cst_ref, cst_ref); (* constant index of element i *)
      init_expr (index_tpl, index_type);
      index_tpl^.cst_val.ival := index_type^.minval + i - 1;
      emit (index_tpl);
      new (component_tpl, array_ref, array_ref);
      init_expr (component_tpl, comp_type);
      with component_tpl^ do begin
	base_array := alc_temp_tpl;
	index_val := index_tpl
      end;
      emit (component_tpl);
      emit_assignment (component_tpl, agg_val_tpl^.operand[i]) (* tuple to assign element into temp *)
    end
  end

  else (* kind = records *) begin
    cur_var := agg_val_tpl^.desc.base;
    if cur_var^.variant_tag = nil then
      cur_tag := nil
    else
      cur_tag := cur_var^.variant_tag^.tag_field;
    fields := cur_var^.field_list;

    for i := 1 to upperbound (agg_val_tpl^.operand) do begin
     exit if fields = nil;
      if fields^.fld_variant = cur_var then begin
	new (component_tpl, field_ref, field_ref);
	init_expr (component_tpl, fields^.type_desc);
	with component_tpl^ do begin
	  base_rec := alc_temp_tpl;
	  field_sym := fields
	end;
	emit (component_tpl); (* reference field of temp *)
	if (fields^.type_desc^.kind in [arrays, strings]) and fields^.type_desc^.flexible then begin
	  assert (fields^.type_desc^.kind <> strings); (* can't handle strings properly *)
	  index_type := fields^.type_desc^.index_type;
	  new (lhs_upb_tpl, upb_op, upb_op,1);
	  init_expr (lhs_upb_tpl, index_type);
	  lhs_upb_tpl^.operand[1] := component_tpl;
	  component_tpl^.usage_count := component_tpl^.usage_count + 1;
	  emit (lhs_upb_tpl);
	  new (rhs_upb_tpl, upb_op, upb_op,1);
	  init_expr (rhs_upb_tpl, index_type);
	  rhs_upb_tpl^.operand[1] := agg_val_tpl^.operand[i];
	  with agg_val_tpl^.operand[i]^ do
	    usage_count := usage_count + 1;
	  emit (rhs_upb_tpl);
	  emit_assignment (lhs_upb_tpl, rhs_upb_tpl) (* assign field's upb into temp *)
	end;
	emit_assignment (component_tpl, agg_val_tpl^.operand[i]); (* assign field into temp *)
      end;
      if (fields = cur_tag) or (fields^.fld_variant <> cur_var) then begin
	tag_val := agg_val_tpl^.operand[i]^.cst_val.ival; (* must be a scalar constant *)
	var_list := cur_var^.variant_tag^.first_variant; (* the variant list *)
	while (var_list <> nil) andif not ( var_list^.others_var orif
	      ( (var_list^.minlab <= tag_val) and (tag_val <= var_list^.maxlab) ) ) do
	  var_list := var_list^.next_variant;
	if var_list = nil then (* no variant selected - no more fields *)
	  fields := nil
	else begin (* select the new variant *)
	  fields := var_list^.field_list;
	  if fields <> nil then
	    cur_var := fields^.fld_variant; (* if fields = nil, it doesn't matter *)
	  if cur_var^.variant_tag = nil
	    then cur_tag := nil (* no sub-variants *)
	    else cur_tag := cur_var^.variant_tag^.tag_field; (* the sub-variant tag field *)
	end;
      end
      else
	fields := fields^.next;
    end (* for i *);
  end

end (* expand_agg_constructor *);

$PAGE prologue

PROCEDURE prologue;
(* saves parameters on entry to a routine *)
VAR
parm: sym;
reg, rv_reg: registers;
instr_count: 0..6;
index: 1..7; (*5 is < 6, 5+2 = 7*)
stack_offset: ARRAY[1 .. 6] OF int_type;
next_link: call_link;
BEGIN
  WITH cur_block^ DO BEGIN
    IF (kind = subr_blk) ANDIF (subr_sym^.type_desc^.parmlist_size > 6)
    THEN BEGIN gen(movem, 2, sp, parm_list_base, none); rv_reg := 3 END
    ELSE BEGIN (* save individual parameters *)
      (*First, determine where on the stack Pass 1 has decided each successive
	parameter register should be stored.  Actually, the regs will go into
	successive locations, but we explicitly do not make that assumption.*)
      instr_count := 0; parm := parm_list.first;
      WHILE parm <> NIL DO BEGIN
	IF parm^.type_desc^.flexible THEN BEGIN
	  IF parm^.type_desc^.generic THEN BEGIN
	    stack_offset[instr_count + 1] := parm^.item_addr - 2;
	    stack_offset[instr_count + 2] := parm^.item_addr - 1;
	    stack_offset[instr_count + 3] := parm^.item_addr;
	    instr_count := instr_count + 3
	  END
	  ELSE BEGIN
	    stack_offset[instr_count + 1] := parm^.item_addr - 1;
	    stack_offset[instr_count + 2] := parm^.item_addr;
	    instr_count := instr_count + 2
	  END
	END
	ELSE BEGIN
	  stack_offset[instr_count + 1] := parm^.item_addr;
	  instr_count := instr_count + 1;
	  IF NOT passed_by_address(parm) ANDIF
  	  (parm^.type_desc^.base_size > 36)
	  THEN BEGIN
	    stack_offset[instr_count + 1] := parm^.item_addr + 1;
	    instr_count := instr_count + 1
	  END
	END;
	parm := parm^.next
      END;

$PAGE

      (*Traverse the list of offsets emitting the movem instructions.  Where
	possible use dmovem's for adjacent pairs.  Actually it always is,
	but again we explicitly do not make that assumption here.*)
      reg := 2; index := 1;
      WHILE index < instr_count DO BEGIN
	IF stack_offset[index] = stack_offset[index + 1] - 1 THEN BEGIN
	  gen(dmovem, reg, sp, stack_offset[index], none);
	  reg := reg + 2;
	  index := index + 2 (*index could become 7 here*)
	END
	ELSE BEGIN
	  gen(movem, reg, sp, stack_offset[index], none);
	  reg := reg + 1;
	  index := index + 1
	END
      END;
      IF index = instr_count THEN BEGIN (* odd one left? *)
	gen(movem, reg, sp, stack_offset[index], none);
	reg := reg + 1
      END;
      rv_reg := reg
    END;
    IF (return_sym <> NIL) ANDIF passed_by_address (return_sym)
    THEN gen(movem, rv_reg, sp, return_sym^.item_addr, none)
  END;
  reg_init; (* mark all registers as free *)
  (*temp_start  records where we start allocating temps for this block
    multi-statement duration temps (for saving with-regs) are allocated
    by bumping up temp_start so that statements within a WITH start with
    a higher temp_base
    temp_base  is advanced as temps are needed within a statement, and is
    reset to temp_start at start of each statement
    temp_max  keeps track of highest value rached by temp_base  i.e.
    requirements of "worst" statement.*)
  temp_start := cur_block^.pos_stk_end; next_link := cur_block^.calls;
  WHILE next_link <> NIL DO BEGIN
    IF cur_block^.owner = next_link^.called_subr^.owner
    THEN temp_start := max (temp_start, next_link^.called_subr^.pos_stk_end);
    next_link := next_link^.rlink
  END;
  temp_base := temp_start; temp_max := temp_start;

$PAGE

  (*Emit instruction to save 17 so that is can be restored after statements
    that have allocated dynamic temps by changin it.  If there never are any
    such statements, this instruction will be deleted at the end of
    COMPILE_BODY.  The location in which to store 17 is left undefined at
    this point.  If its actually required, RESET_STACK will obtain a
    symbol, and COMPILE_BODY will ultimately give the symbol a value.*)
  reset_needed := false; save17loc := nil; save17addr := absolute_reference;
  save17addr.index := sp; save17addr.reloc.kind := def_sc;
  gen_rm(movem, sb, save17addr); save17inst := code_area.last;
  (*Initialize descriptor for stack locations where handler state blocks
    will reside.*)
  hsb_addr := absolute_reference;
  WITH hsb_addr DO BEGIN
    index := sp; reloc.kind := def_sc; reloc.reldef := make_def(local_def)
  END
END;

$PAGE case_jump

(* CASE JUMP generates code for a case jump operator and following jump in operators.
   The parameter "tpl" points to the case operator on entry; on return, it is
   set to the last jump in operator. *)

procedure case_jump (var tpl: tuple);

 var
   reg: registers;
   caddr: addr_desc;
   jmp: tuple;
   i: int_type;

 begin
  with tpl^ do begin
    caddr := fetch (cond);
    if reset_needed then
      reset_stack;
    if next^.opcode <> jump_in_op then begin    (* no cases *)
      free (caddr);
    end
    else begin
      reg := load_addr (caddr, alignment (cond), 36);
      gen_rm (cam+ltc, reg, int_value (low_cntrl));
      gen_rm (cam+lec, reg, int_value (high_cntrl));
      gen_rl (jrst, 0, jump_to);

      genind (jrst, 0, reg, 1 - low_cntrl, dot);

      jmp := next;
      loop              (* over jump_in_op's, assume at least one *)
	for i := jmp^.low_cntrl to jmp^.high_cntrl do
	  gen_rl (arg, 0, jmp^.jump_to);
      exit if jmp^.next^.opcode <> jump_in_op;
	jmp := jmp^.next;
	for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
	  gen_rl (arg, 0, tpl^.jump_to);        (* goto 'others' for unspecified cases *)
      end;
      tpl := jmp;                       (* advance to last jump in op *)
      decr_reg_usages (reg);
    end;
  end (* with *) ;
 end;
$PAGE cond_handler_stmts
(* COND HNDLR STMTS generates code for the statement tuples associated with
   condition handling.  *)

procedure cond_handler_stmts (var tpl: tuple);

var
  cr: code;
  cond_addr: addr_desc;
  jmp: tuple;
  area: code_list;

procedure cond_cell_arg (rtsym: rt_symbol);
  begin
    cond_addr := fetch (tpl^.cond_parm);
    gen_rt (pushj, sb, rtsym);
    gen_rm (arg, 0, cond_addr);
    free (cond_addr)
  end;

begin
  with tpl^ do
    case opcode of

      set_handler_op, rst_handler_op:
	begin
	  if opcode = set_handler_op
	    then gen_rt (pushj, sb, rt_set_handler)
	    else gen_rt (pushj, sb, rt_rst_handler);
	  if hndlr_tuple = nil then
	    gen_ri (arg, 0, 0)
	  else
	    gen (arg, 0, 0, 0, reldef (get_def (hnd_tab_def, hndlr_tuple^.block_order_no)))
	end;

      signal_op:
	cond_cell_arg (rt_signal);

      mask_op:
	cond_cell_arg (rt_mask);

      unmask_op:
	cond_cell_arg (rt_unmask);

      resignal_op:
	gen_rt (pushj, sb, rt_resignal);

      exmessage_op:
	gen_rt (pushj, sb, rt_exmessage);

      hndlr_jump_op: begin
  
	(* handler common entry code *)
  
	with_restore;
	reset_stack;
	genind (jrst, 0, 0, 0, none);  (* jrst @0 *)
	if overlaid
	  then area := hbt_area
	  else area := code_area;
	 
	(* handler branch table *)
 
	mark_def (area, get_def (hnd_tab_def, jump_from^.block_order_no)); (* mark with H.nn label *)

	new (cr, halfwords); (* XWD L.m,H.n where L.m is common entry code, and
					          H.n is encompassing hbt  *)
	with cr^ do begin
	  xwd.lh := 0;
	  xwd.rh := 0;
	  lreloc := reldef (get_def (label_def, jump_from^.block_order_no));
	  if jump_from^.in_handler = nil then
	    rreloc := none
	  else
	    rreloc := reldef (get_def (hnd_tab_def, jump_from^.in_handler^.block_order_no))
	end;
	gen_emit (area, cr);

	new (cr, halfwords);	(* XWD 0,n  where n is hsb offset in stack frame *)
	with cr^ do begin
	  xwd.lh := 0;
	  lreloc := none;
	  xwd.rh := (high_cntrl - 1) * 2;
	  rreloc := hsb_addr.reloc
	end;
	gen_emit (area, cr);

	(* body of table: one word per condition *)
 
	jmp := tpl;
	while jmp^.next^.opcode = jump_cond_op do begin
	  jmp := jmp^.next;
	  new (cr, halfwords); (* XWD  <condition>,L.n  where L.n is label of handler *)
	  cond_addr := fetch (jmp^.cond);
	  free (cond_addr);
	  with cr^ do begin
	    xwd.lh := 0;
	    xwd.rh := 0;
	    lreloc := cond_addr.reloc;
	    rreloc := reldef (get_def (label_def, jmp^.jump_to^.block_order_no))
	  end;
	  gen_emit (area, cr)
	end;

	new (cr, halfwords);	(* last word of handler branch table *)
	with cr^ do begin
	  lreloc := none;
	  if jump_to^.next^.opcode = resignal_op then begin
	    xwd.lh := 777777b; (* others = -1 *)
	    xwd.rh := 0;
	    rreloc := none
	  end
	  else begin
	    if low_cntrl = 0 then
	      xwd.lh := 777777b (* others = -1 *)
	    else
	      xwd.lh := 777776b; (* allconditions = -2 *)
	    assert (jump_to^.opcode = label_node);
	    rreloc := reldef (get_def (label_def, jump_to^.block_order_no))
	  end
	end;
	gen_emit (area, cr);
 
	tpl := jmp;  (* advance to last jump_cond_op *)
	if overlaid
	  then hbt_area := area
	  else code_area := area;
      end;

      others:
	assert (false)

    end (* case opcode *);
end (* cond_handler_stmts *);
$PAGE rt_io_call
(* RT IO CALL generates a runtime routine call for one of the simple i/o
   routines, given "op", the runtime statement tuple, and "rts", the runtime
   routine code.  If there are both explicit and implicit argument forms of
   a call, it is assumed that the code for the implicit call is the successor
   of the code for the explicit call. *)

procedure rt_io_call ( op: tuple; rts: rt_symbol );

 var
    f: expr;
    mem: addr_desc;

 begin
  with op^ do begin
    if old_file then
      gen_rt (pushj, sb, succ (rts))

    else begin
      f := file_arg;
      if (f^.opcode = io_var_str_op) or (f^.opcode = io_fix_str_op) then
	f := f^.operand[1];
      mem := argument (f);
      gen_rt (pushj, sb, rts);
      gen_rm (arg, 0, arg_addr (mem));
      free (mem);
    end;
  end;
 end;
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine,
   using the specified file and index arguments. *)

procedure rt_seek_call ( fil, ind: expr );

var
  fil_mem, ind_mem: addr_desc;
  cval: int_type;

begin
  fil_mem := argument (fil);
  ind_mem := argument (ind);
  if aconstp (ind_mem, cval) then
    ind_mem := gen_cst (cval);
  gen_rt (pushj, sb, rt_seek);
  gen_rm (arg, 0, arg_addr (fil_mem));
  gen_rm (arg, 0, ind_mem);
  free (fil_mem);
  free (ind_mem);
end;
$PAGE value_check

(* VALUE CHECK generates code for value and subscript range check tuples.  *)

procedure value_check (value_chk_op: tuple; rts: rt_symbol);

var
  reg:	registers;
  low_addr, high_addr: addr_desc;

begin
  with value_chk_op^ do begin
    (* Fetch addresses of bounds. *)

    if operand[2] <> nil then
      low_addr := fetch_fullword (operand[2]);
    if operand[3] <> nil then
      high_addr := fetch_fullword (operand[3]);

    (* might as well load up the value now - the check tuple wouldn't exist if it was detectable
       as useless at compile time (but note that it might be guaranteed to fail!)  *)

    reg := nond_load (operand[1], 36);

    if operand[2] <> nil then
      if operand[3] <> nil then
	gen_rm (cam+ltc, reg, low_addr)
      else
	gen_rm (cam+gec, reg, low_addr);
    if operand[3] <> nil then
      gen_rm (cam+lec, reg, high_addr);
    gen_rt (jsp, 1, rts);

    decr_reg_usages (reg);
    if operand[2] <> nil then
      free (low_addr);
    if operand[3] <> nil then
      free (high_addr)
  end (* with value_chk_op^ *);
end;
$PAGE compatibility_check

(* COMPATIBILITY CHECK generates code for compatibility check tuples.  *)

procedure compatibility_check (compat_chk_tpl: tuple);

var
  reg: registers;

begin
  reg := do_binary_op (cam + eqc, compat_chk_tpl^.operand[1], compat_chk_tpl^.operand[2], [commutative]);
  gen_rt (jsp, 1, rt_cmp_chk);
  decr_reg_usages (reg)
end;
$PAGE pointer_check

(* POINTER CHECK generates code for a pointer or file check tuple.  Tests
   for NIL and zero are generated.  *)

procedure pointer_check (ptr_check_tpl: tuple; rts: rt_symbol);

var 
  reg: registers;

begin
  reg := nond_load (ptr_check_tpl^.operand[1], 36);
  gen (jump + eqc, reg, 0, 2, dot);
  gen_ri (cai + nec, reg, 377777B);
  gen_rt (jsp, 1, rts);
  decr_reg_usages (reg)
end;
$PAGE substring_check

(* SUBSTRING CHECK generates code for string range check tuples.  It compiles
   str_range_chk (sidx, slen, rlen) to check that:

	    1 <= sidx <= sidx + slen <= rlen + 1

    The basic instruction sequence is:

	1) move	sidx_reg, <sidx>
	2) move	slen_reg, <slen>
	3) jumpl	slen_reg, .+5
	4) jumple	sidx_reg, .+4
	5) movei	1, -1(sidx_reg)
	6) add	1, slen_reg
	7) camle	1, <rlen>
	8) jsp	1, strer.

    If sidx and/or slen are known at compile-time, from two to five of the above eight
    instructions will be eliminated.  No attempt has been made yet to take advantage of
    rlen being constant, not that that would be a bad thing to do.  Note that the check
    tuple should not have been generated if it is known to be useless at compile_time,
    but it will be generated if its known at compile time that the check will fail.  *)

procedure substring_check (str_chk_tpl: tuple);

var
  sidx_reg,
  slen_reg: registers;
  rlen_addr: addr_desc;
  sidx_const, slen_const: boolean;
  sidx_value, slen_value: int_type;

begin
  sidx_const := iconstp (str_chk_tpl^.operand[1], sidx_value) andif (sidx_value > 0);
  slen_const := iconstp (str_chk_tpl^.operand[2], slen_value) andif (slen_value >= 0);

  if sidx_const then
    free (fetch (str_chk_tpl^.operand[1]))
  else
    sidx_reg := nond_load (str_chk_tpl^.operand[1], 36);
  if slen_const then
    free (fetch (str_chk_tpl^.operand[2]))
  else
    slen_reg := nond_load (str_chk_tpl^.operand[2], 36);
  rlen_addr := fetch (str_chk_tpl^.operand[3]);
  free (rlen_addr);  (* any register allocation is already done *)

  if not slen_const then
    if sidx_const then
      gen (jump + ltc, slen_reg, 0, 4, dot)
    else
      gen (jump + ltc, slen_reg, 0, 5, dot);
  if not sidx_const then
    gen (jump + lec, sidx_reg, 0, 4, dot);

  if not sidx_const then
    gen (movei, 1, sidx_reg, -1, none)
  else if not slen_const then
    gen_ri (movei, 1, sidx_value - 1)
  else (* if both constant *)
    gen_ri (movei, 1, sidx_value + slen_value - 1);

  if not slen_const then
    gen_rr (add, 1, slen_reg)
  else if not sidx_const then
    gen_ri (addi, 1, slen_value);
  (* else both constant and the movei above did it all *)

  gen_rm (cam + lec, 1, rlen_addr);
  gen_rt (jsp, 1, rt_str_chk);

  if not sidx_const then
    decr_reg_usages (sidx_reg);
  if not slen_const then
    decr_reg_usages (slen_reg)
end;
$PAGE perform_check
(* PERFORM CHECK merely dispatches a check tuple to the appropriate routine. *)

procedure perform_check (check_tuple: tuple);

begin
  case check_tuple^.opcode of

    val_range_chk:
      value_check (check_tuple, rt_val_chk);

    file_chk:
      pointer_check (check_tuple, rt_fil_chk);

    ptr_chk:
      pointer_check (check_tuple, rt_ptr_chk);

    sub_range_chk:
      value_check (check_tuple, rt_sub_chk);

    str_range_chk:
      substring_check (check_tuple);

    compat_chk:
      compatibility_check (check_tuple);

    others:
      assert (false)

  end;
end;
$PAGE do_check
(* DO CHECK forces evaluation of a delayed check tuple, given one of its
   operands. The result field of the operand indicates its check tuple. *)

public procedure do_check (exp: expr);

var
  tpl: tuple;

begin
  tpl := exp^.result;
  exp^.result := nil;   (* to allow evaluation *)
  exp^.ref_fre := 0;
  perform_check (tpl);
end;

$PAGE attach_check_op

PROCEDURE attach_check_op(check_tpl: tuple; operand: expr);
(*links a check tuple and one of its operands for evaluation
  when the operand is to be used. If the operand has already
  been evaluated, the check is performed now.*)
BEGIN
  ASSERT(operand <> NIL);
  IF operand^.result <> NIL (*already evaluated, do the check*)
  THEN perform_check(check_tpl)
  ELSE BEGIN
    operand^.result := check_tpl;
    operand^.ref_fre := 1; (*mark as linked*)
    ASSERT(operand^.usage_count > 1)
    (*verify that we're setting our hook on a tuple that has a use
      other than the check, so the check WILL be invoked*)
  END
END;

$PAGE get_next_action

FUNCTION get_next_action(tpl: tuple): tuple;
(*scans the IF from a given tuple and returns the first tuple
  which is not a "start_stmt" operator*)
BEGIN
  ASSERT(tpl <> NIL); ASSERT(tpl^.next <> NIL);
  get_next_action := tpl^.next;
  WHILE get_next_action^.opcode = start_stmt DO BEGIN
    get_next_action := get_next_action^.next;
    ASSERT(get_next_action <> NIL);
  END
END;

$PAGE compile_body

PROCEDURE compile_body(stack_frame: def) OPTIONS SPECIAL(COERCIONS);
(*compiles the body of the current block.  "Stack_frame" is the
  definition node (if any) which must be defined in alc_temps as the stack
  frame size of the block.*)
VAR
tpl, lab: tuple; d: def; reg: registers; i: int_type;
mem: addr_desc; addr_ptr: ^addr_desc;
prune_ok: BOOLEAN; prune_tpl: tuple;
BEGIN
  (*fetch the intermediate form*)
  rd_tuples;
  IF sw(cur_block^.dump_switches, 'IFM0')
  THEN dmptuples('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');
  (*explode booleans and supply labels*)
  make_basic_blocks;
  IF sw(cur_block^.dump_switches, 'IFM')
  THEN dmptuples('INTERMEDIATE FORM AFTER MAKE_BASIC_BLOCKS FOR BLOCK $');
  (*fetch parameters and compile tuples*)
  prologue; tpl := t_chain^.first_label;
  WHILE tpl <> NIL DO BEGIN
    prune_tpl := tpl; prune_ok := FALSE;
    WITH tpl^ DO BEGIN
      CASE opcode OF
	assign_op: BEGIN
	  CASE lhs^.desc.kind OF
	    bools, ints, scalars, pointers, files: scalar_assignment(tpl);
	    chars: BEGIN
	      IF rhs^.desc.kind = strings THEN str_assignment(tpl)
	      ELSE scalar_assignment(tpl)
	    END;
	    reals: real_assignment(tpl);
	    strings: str_assignment(tpl);
	    sets: set_assignment(tpl);
	    procs, funcs: proc_func_assignment(lhs, rhs);
	    arrays, records: agg_assignment(tpl);
	    OTHERS: ASSERT(FALSE)
	  END;
	  IF reset_needed THEN reset_stack
	END;
	start_with: BEGIN
	  prune_ok := TRUE; with_start(with_rec);
	  IF reset_needed THEN reset_stack
	END;
	end_with: BEGIN prune_ok := TRUE; with_end(with_rec) END;
	call_op: BEGIN
	  prune_ok := true; procedure_call(tpl);
	  IF reset_needed THEN reset_stack
	END;

$PAGE

	label_node: BEGIN
	  d := get_def(label_def, block_order_no);
	  mark_def(code_area, d);
	  IF (label_sym <> NIL) ANDIF (label_sym^.lab_nonlocal_use) THEN BEGIN
	    d := get_def(sym_def, label_sym^.id_number);
	    mark_def(code_area, d);
	    reset_stack (*free any dynamic temps*)
	  END
	END;
	jump_op: BEGIN
	  prune_ok := true;
	  IF jump_to <> get_next_action(tpl) (*generate jump only if useful*)
	  THEN gen_rl(jrst, 0, jump_to)
	END;
	jump_t_op, jump_f_op: BEGIN
	  prune_ok := TRUE;
	  (*skip to alternate jump op, may be label node*)
	  tpl := tpl^.next; ASSERT(tpl <> NIL); lab := get_next_action(tpl);
	  IF opcode = jump_t_op
	  THEN test_and_jump(cond, jump_to, tpl^.jump_to, lab)
	  ELSE test_and_jump(cond, tpl^.jump_to, jump_to, lab)
	END;
	case_jump_op: BEGIN prune_ok := TRUE; case_jump(tpl) END;
	goto_op: BEGIN
	  prune_ok := TRUE;
	  d := get_def(sym_def, target_lab^.id_number); (*get label def*)
	  IF target_lab^.block^.owner = cur_block^.owner
	  THEN gen(jrst, 0, 0, 0, reldef(d))
	  ELSE IF target_lab^.block^.kind = program_blk THEN BEGIN (*in main*)
	    gen_rt(pushj, sb, rt_uw_prg); gen(jrst, 0, 0, 0, reldef(d))
	  END
	  ELSE BEGIN (*label is in a containing routine*)
	    (*get owners stack frame pointer*)
	    reg := get_reg(36); gen(hlrz, reg, sp, 1, none);
	    FOR i := 2 TO
	      (cur_block^.apparent_level - target_lab^.block^.apparent_level)
	    DO gen(hlrz, reg, reg, 1, none);
	    gen_rt(pushj, sb, rt_uw_rtn); (*unwind and jump*)
	    gen(jump+awc, reg, 0, 0, reldef(d)); (*reg is arg to unwind*)
	    decr_reg_usages(reg)
	  END
	END;

$PAGE

	gen_jump_op: BEGIN
	  prune_ok := TRUE; bb_end; (*clean up temps, etc.*)
	  (*presumably the jump_to label, remember label node*)
	  tpl := tpl^.next; ASSERT(tpl <> NIL); lab := tpl;
	  tpl := tpl^.next; ASSERT(tpl <> NIL); (*should be gen_xxxif oper*)
	  reg := get_reg(36); (*get register in which to load value*)
	  CASE tpl^.opcode OF
	    gen_andif_op: gen(skip+awc, reg, 0, 0, gen_cint(1));
	    gen_orif_op: gen_ri(tdza, reg, reg);
	    OTHERS: ASSERT(FALSE)
	  END;
	  d := get_def(label_def, lab^.block_order_no); mark_def(code_area, d);
	  CASE tpl^.opcode OF
	    gen_andif_op: gen_ri(setz, reg, 0);
	    gen_orif_op: gen_ri(movei, reg, 1);
	    OTHERS: ASSERT(FALSE)
	  END;
	  NEW(addr_ptr); addr_ptr^ := get_temp(1);
	  gen_rm(movem, reg, addr_ptr^);
	  tpl^.result := PTR(ORD(addr_ptr)); (*chain to orif/andif node*)
	  decr_reg_usages(reg)
	END;
	dispose_op: BEGIN
	  prune_ok := TRUE; mem := argument(dptrarg);
	  gen_rt(pushj, sb, rt_dispose); gen_rm(arg, 0, arg_addr(mem));
	  free(mem); IF reset_needed THEN reset_stack
	END;
	start_stmt: BEGIN
	  prune_ok := TRUE; kill_temps; (*reset stack if required*)
	  cur_source := stmt_source; (*for debugging*)
	  gen_source(stmt_source, stmt_index); (*comment for asm listing*)
	  IF (stmt_index = 1) ANDIF prog_options.debug_opt
	  THEN stmt_block(stmt_source, stmt_kind)
	END;
	stop_op: BEGIN prune_ok := TRUE; gen_rt(jrst, 0, rt_stop) END;
	return_op: BEGIN
	  prune_ok := TRUE;
	  IF cur_block <> cur_block^.owner
	  THEN gen_ri(popj, sb, 0) (*if quick*)
	  ELSE gen_rt(jrst, 0, rt_return)
	END;
	abort_op: BEGIN prune_ok := TRUE; gen_rt(jsp, 1, rt_ass_chk) END;
	case_abort_op: BEGIN  prune_ok := TRUE; gen_rt(jsp, 1, rt_cas_chk) END;
	set_handler_op, rst_handler_op, signal_op, mask_op,
	unmask_op, resignal_op, exmessage_op, hndlr_jump_op,
	jump_cond_op: cond_handler_stmts(tpl);

$PAGE

	start_io_op: io_begins(tpl);
	end_io_op: BEGIN io_ends(tpl); IF reset_needed THEN reset_stack END;
	get_op: BEGIN
	  IF file_arg^.desc.base^.file_kind = textfile
	  THEN rt_io_call(tpl, rt_get_char)
	  ELSE rt_io_call(tpl, rt_get)
	END;
	put_op: BEGIN
	  IF file_arg^.desc.base^.file_kind = textfile
	  THEN rt_io_call(tpl, rt_put_char)
	  ELSE rt_io_call(tpl, rt_put)
	END;
	readln_op: rt_io_call(tpl, rt_rd_lnn);
	writeln_op: rt_io_call(tpl, rt_wr_lnn);
	page_op: rt_io_call(tpl, rt_page);
	clear_op: rt_io_call(tpl, rt_clear);
	break_op: rt_io_call(tpl, rt_break);
	empty_op: BEGIN
	  IF file_arg^.desc.base^.file_kind = textfile
	  THEN rt_io_call(tpl, rt_empty_text)
	  ELSE rt_io_call(tpl, rt_empty)
	END;
	close_op: rt_io_call(tpl, rt_close);
	scratch_op: rt_io_call(tpl, rt_scratch);
	read_op, write_op: read_write_call(tpl);
	seek_op: rt_seek_call(seek_file, seek_index);
	close_all_op: gen_rt(pushj, sb, rt_close_all);
	val_range_chk, file_chk, ptr_chk, sub_range_chk,
	str_range_chk: attach_check_op(tpl, operand[1]);
	compat_chk: BEGIN
	  lab := operand[1]^.operand[1]; (*back over dim_op, lwb_op, upb_op*)
	  ASSERT(lab <> NIL);
	  IF NOT (lab^.usage_count > 1) THEN BEGIN
	    ASSERT(lab^.opcode = field_ref); (*must be for flex record*)
	    lab := lab^.base_rec; ASSERT(lab <> NIL)
	  END;
	  attach_check_op(tpl, lab)
	END;
	agg_val: expand_agg_constructor(tpl); (*insert assignments into temp*)
	OTHERS: (*do nothing for non-code instructions*)
      END (*case*)
    END; (*with*)
    tpl := tpl^.next;
    (*prune the tuple chain of compiled tuples*)
    IF prune_ok THEN LOOP
      prune_tpl := prune_tpl^.next; EXIT IF prune_tpl = NIL;
      dechain(prune_tpl^.prev); EXIT IF prune_tpl = tpl
    END
  END; (*while*)

$PAGE

  (*clean up debug stuff*)
  IF prog_options.debug_opt THEN blk_end;
  (*if any dynamic temps allocated, supply temp location, otherwise
    delete the save of the stack pointer*)
  IF save17loc = NIL THEN save17inst^.kind := nullcode
  ELSE BEGIN (*reset_stack obtained the symbol*)
    def_value(save17loc, temp_max, FALSE); temp_max := temp_max + 1
  END;
  (*if saving of with-registers was required, allocate the temp locations and
    aim the symbol definition at the high end (the block of temps is used from
    the high end down, just as with-regs are allocated from #o14 down)*)
  IF maxwith_regs > 0 THEN BEGIN
    def_value(savewithaddr.reloc.reldef, temp_max + maxwith_regs - 1, FALSE);
    temp_max := temp_max + maxwith_regs
  END;
  (*if condition handlers in procedure, allocate space for state blocks*)
  IF cur_block^.hndlr_depth > 0 THEN BEGIN
    def_value(hsb_addr.reloc.reldef, temp_max, FALSE);
    temp_max := temp_max + 2 * cur_block^.hndlr_depth
  END;
  cur_block^.pos_stk_end := temp_max;
  (*if not a quick routine then fill in ADJSP's value*)
  IF stack_frame <> NIL THEN def_value(stack_frame, temp_max, FALSE);
  (*verify that all registers were freed*)
  FOR reg := 2 TO #o14 DO ASSERT(regdesc[reg].uses_remaining = 0);
  ASSERT(with_base = #o15);
  IF hbt_area.first <> NIL THEN BEGIN
    set_origin(hbt_area, loc_hbt); set_origin(code_area, loc_code);
    wr_code (hbt_area, loc_hbt, FALSE); (*define the handler addresses*)
  END;
  emit_code(code_area, loc_code, cur_block^.semantic_options);
  IF hbt_area.first <> NIL
  THEN emit_code(hbt_area, loc_hbt, cur_block^.semantic_options);
  IF sw(cur_block^.dump_switches, 'FINAL') THEN BEGIN
    reclaim; dmptuples('FINAL INTERMEDIATE FORM FOR BLOCK $')
  END;
  del_def_list(label_def); del_def_list(local_def);
  del_tuples; (*get rid of IF for this block*)
END;

$PAGE compile_subr

PROCEDURE compile_subr;
(*generates code for the body of a procedure or function.  It is assumed
  that cur_block points to the block node for the subroutine on entry.*)
VAR tb, entryaddr: def; lev: level_index; quick: BOOLEAN; stack_frame: def;
BEGIN
  gen_cmt(code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);
  (*entry sequence differs for quick/nonquick*)
  quick := (cur_block^.owner <> cur_block);
  IF NOT quick THEN BEGIN
    IF prog_options.debug_opt OR (trace_opt IN cur_block^.semantic_options)
    THEN tb := trace_block(cur_block) (*emit the trace control block*)
    ELSE tb := NIL;
    IF (NOT quick) AND (cur_block^.level > 2) (*may be called by parent*)
    THEN gen(hrli, sp, sp, 0, none) (*set static = dynamic link*)
  END;
  WITH cur_block^.subr_sym^ DO IF public_dcl THEN
    gen_asm_label(code_area, name^.text);
  entryaddr := get_def(subr_def, cur_block^.number); (*define the entry point*)
  mark_def(code_area, entryaddr);
  IF NOT quick THEN BEGIN (*generate full entry sequence*)
    gen_rt(jsp, 1, rt_entry);
    IF tb <> NIL (*trace or debug in effect*)
    THEN gen_xwd(0, none, 0, reldef (tb))
    ELSE gen_xwd(0, none, int_nil, none);
    stack_frame := get_def(temp_size_def, cur_block^.number);
    IF prog_options.ki_code_opt THEN BEGIN
      gen_rt(jsr,0,rt_inst_sml); gen(adj_sp, sb, 0, 0, reldef(stack_frame))
    END
    ELSE gen(adjsp, sb, 0, 0, reldef(stack_frame)) 
  END
  ELSE stack_frame := nil;
  IF cur_block^.return_sym <> NIL (*if function, mark return symbol*)
  THEN cur_block^.return_sym^.allocated := TRUE; (*no del of ref'ing cod rcds*)
  compile_body(stack_frame) (*compile the body of the subroutine*)
END;

$PAGE compile_main

PROCEDURE compile_main(VAR startaddr: def);
(*generates code for the body of a main program.  It is assumed that
  cur_block points to the program's block node at entry.*)
VAR tb: def; stack_frame: def;
BEGIN
  gen_cmt(code_area, 'Begin program ' || cur_block^.id^.text);
  IF prog_options.debug_opt OR (trace_opt IN cur_block^.semantic_options)
  THEN tb := trace_block (cur_block)
  ELSE tb := NIL;
  startaddr := get_def(subr_def, cur_block^.number);
  mark_def(code_area, startaddr);
  gen_ri(jfcl, 0, 0);
  gen_ri(movei, sp, prog_options.storage);
  gen_rt(jsp, 7, rt_init);
  IF tb <> NIL (*trace or debug*)
  THEN gen_xwd(0, none, 0, reldef(tb))
  ELSE gen_xwd(0, none, int_nil, none);
  stack_frame := get_def(temp_size_def, cur_block^.number);
  IF prog_options.ki_code_opt THEN BEGIN
    gen_rt(jsr,0,rt_inst_sml); gen(adj_sp, sb, 0, 0, reldef(stack_frame)) 
  END
  ELSE gen(adjsp, sb, 0, 0, reldef(stack_frame));
  compile_body(stack_frame) (*generate code for body of block*)
END;

$PAGE gen_code

PUBLIC PROCEDURE gen_code(VAR code_size, const_size, static_size: unit_range);
(*The driving program for the code generator.  It directs initialization,
  generation, and termination.*)
VAR
loc_fp: code_address; lowseg_break: unit_range; temp_file: TEXT;
startaddr: def; (*start address of main program, nil otherwise*)
BEGIN
  (*set immediately in case of assert failure*)
  cur_block := root_block^.children; cur_source := (0, 0, 0);
  lowseg_break := size_init + size_uninit + size_cond;
  overlaid := prog_options.overlay_opt or prog_options.mainseg_opt;
  startaddr := nil; (*init globals in this module*)
  gen_init; (*and others*)
  low_base := make_def(code_def); def_value(low_base, loc_static, TRUE);
  high_base := make_def(code_def); def_value(high_base, loc_code, TRUE);
  loc_hbt := lowseg_break;
  IF assembly_opt in all_opts THEN mac_header('Quick Code Generator');
  IF map_opt IN all_opts THEN map_init;
  rel_init; init_static;
  (*The first word is the module word for the debugger.  Emit it here even
    if this is a data module and doesn't contain any procedures.*)
  gen_origin(code_area, loc_code);
  deb_init; (*reserves program block*)
  emit_code(code_area, loc_code, prog_options.semantic_options);
  (*Compile the constituent blocks in reverse depth first order.*)
  cur_block := root_block; (*look for end of call chain*)
  WHILE cur_block^.downward_call_thread <> NIL
  DO cur_block := cur_block^.downward_call_thread;
  WHILE cur_block <> root_block DO BEGIN (*compile in reverse order*)
    CASE cur_block^.kind OF 
      program_blk: compile_main(startaddr);
      subr_blk: compile_subr;
      OTHERS: (*do nothing for non-code blocks*)
    END;
    cur_block := cur_block^.upward_call_thread
  END;
  cur_block := root_block^.children; (*in case of assertion failure*)
  (*final cleanup*)
  pool_constants;
  (*consts go at end of code area*)
  IF NOT overlaid THEN loc_cst := loc_code
  ELSE BEGIN (*emit consts in lowseg for overlay compilation*)
    loc_cst := loc_hbt;
    set_origin(cst_area, loc_cst)
  END;
  emit_code(cst_area, loc_cst, prog_options.semantic_options);
  emit_code(bptr_area, loc_cst, prog_options.semantic_options);
  emit_code(blt_area, loc_cst, prog_options.semantic_options);
  IF prog_options.debug_opt THEN BEGIN
    IF overlaid THEN loc_fp := loc_code ELSE loc_fp := loc_cst;
    gen_origin(code_area, loc_fp);
    fp_blocks; (*dump page and file blocks in code area*)
    emit_code(code_area, loc_fp, prog_options.semantic_options);
    deb_stable; (*dump symbol table*)
    prog_block
  END
  ELSE IF rel_file <> '' THEN BEGIN (*delete any previous debug file*)
    RESET(temp_file, rel_file || '.DEB[,]'); SCRATCH(temp_file)
  END;
  IF prog_options.debug_opt THEN BEGIN
    IF overlaid THEN code_size := loc_fp - #o400000
    ELSE code_size := (loc_fp - loc_cst) + (loc_code - #o400000);
  END
  ELSE code_size := loc_code - #o400000;
  IF overlaid THEN const_size := loc_cst - lowseg_break
  ELSE const_size := loc_cst - loc_code;
  static_size := lowseg_break;
  IF assembly_opt IN all_opts (*terminate assembly listing*)
  THEN mac_end(startaddr, code_size, const_size, static_size);
  IF overlaid THEN BEGIN
    IF prog_options.debug_opt
    THEN rel_end(startaddr, loc_cst, loc_fp)
    ELSE rel_end(startaddr, loc_cst, loc_code)
  END
  ELSE BEGIN
    IF prog_options.debug_opt
    THEN rel_end(startaddr, lowseg_break, loc_fp)
    ELSE rel_end(startaddr, lowseg_break, loc_code)
  END;
  gen_term; dmp_close;
  IF map_opt IN all_opts THEN map_print
END.
 n  q,